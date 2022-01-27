//! # Grezi
//!
//! This library offers a parser and a solver for the `GRZ` presentation format.
//!
//! ## Parser
//!
//! If you just need to access the AST of a `.grz` file, or you want to implement your own solver, you can parse `.grz` documents into an AST like this
//!
//! ```
//! let file = "Viewbox: Size[0] ^ 1:2, 1:2 ]; Title: Header(value: \"Grezi is cool!\"); { Title: Viewbox[0];__.., };";
//! let tokens = grezi::tokenizer(file.as_ref());
//! ```
//!
//! ## Solver
//!
//! Grezi also has the capability of solving `.grz` documents into slideshows very easily. However,
//! you must provide your own objects. This is to account for varying implementations and allow for
//! associated data in each objcet. Here is an example of a simple slideshow program
//!
//! ```
//! #[derive(Debug, Clone)]
//! struct CustomObject {
//!     name: String,
//!     type_: String,
//!     values: grezi::AHashMap<String, String>
//! }
//!
//! impl grezi::Object for CustomObject {
//!     type Error = std::convert::Infallible;
//!
//!     fn bounds(&mut self, w: f64, h: f64) -> Result<(f64, f64), Self::Error> {
//!         Ok((50.0, 50.0))
//!     }
//!     fn construct(
//!         name: String,
//!         type_: String,
//!         values: &mut grezi::AHashMap<String, String>,
//!         _: &grezi::AHashMap<String, String>
//!     ) -> Result<Self, Self::Error> {
//!         Ok(Self {
//!             name,
//!             type_,
//!             values: values.clone()
//!         })
//!     }
//! }
//!
//! const SIZE: grezi::layout::Rect = grezi::layout::Rect {
//!     left: 0.0,
//!     top: 0.0,
//!     right: 1920.0,
//!     bottom: 1080.0,
//! };
//!
//! let file = "Viewbox: Size[0] ^ 1:2, 1:2 ]; Title: Header(value: \"Grezi is cool!\"); { Title: Viewbox[0];__.., };";
//!
//! let slideshow: Result<Vec<grezi::Slide<CustomObject, grezi::easing::CubicPointInOut, grezi::easing::CubicSingleInOut>>, _> = grezi::file_to_slideshow(
//!     file.as_ref(),
//!     SIZE,
//!     grezi::easing::cubic_rect_inout,
//!     grezi::easing::cubic_single_inout,
//!     1.0,
//!     60.0 * 0.5
//! );
//! ```
//! The above code will solve a slideshow with a 1920x1080 screen size at 60 FPS, where each slide
//! would take 0.5 seconds to transition.
#![warn(missing_docs)]
//--> Use statements

/// Access to the raw constraint solver and associated types modified from the `tui` crate.
pub mod layout;

/// Various easing mechanisms for moving objects, controlling opacities, and transitioning slides
pub mod easing;

use std::{iter::Zip, ops::Range};

use ahash::RandomState;
/// A type alias for a [`std::collections::HashMap`] which uses [`ahash`] as it's hasher.
pub type AHashMap<K, V> = std::collections::HashMap<K, V, RandomState>;
use chumsky::prelude::*;
use layout::{Constraint, Layout, Rect};
use vek::Vec4;

//--> Macros

#[macro_export]
/// A macro for converting a lineup value, a viewbox and an object to a pair of x, y coordinates.
macro_rules! get_pos {
    ($line_up:expr, $vbx:expr, $obj:expr) => {
        match $line_up {
            LineUp::TopLeft => ($vbx.left, $vbx.top),
            LineUp::TopRight => ($vbx.right - $obj.0, $vbx.top),
            LineUp::BottomLeft => ($vbx.left, $vbx.bottom - $obj.1),
            LineUp::BottomRight => ($vbx.right - $obj.0, $vbx.bottom - $obj.1),
            LineUp::CenterTop => (($vbx.left + $vbx.right) / 2.0 - ($obj.0 / 2.0), $vbx.top),
            LineUp::CenterBottom => (
                ($vbx.left + $vbx.right) / 2.0 - ($obj.0 / 2.0),
                $vbx.bottom - $obj.1,
            ),
            LineUp::CenterLeft => ($vbx.left, ($vbx.top + $vbx.bottom) / 2.0 - ($obj.1 / 2.0)),
            LineUp::CenterRight => (
                $vbx.right - $obj.0,
                ($vbx.top + $vbx.bottom) / 2.0 - ($obj.1 / 2.0),
            ),
            LineUp::CenterCenter => (
                ($vbx.left + $vbx.right) / 2.0 - ($obj.0 / 2.0),
                ($vbx.top + $vbx.bottom) / 2.0 - ($obj.1 / 2.0),
            ),
        }
    };
}

//--> Enums

/// A method of splitting the screen a certain way
type Viewbox = (
    (((String, String), usize), layout::Direction),
    Vec<Constraint>,
);

type SlideType = Vec<(
    ((String, String), usize),
    (
        (LineUpGeneral, LineUpGeneral),
        (LineUpGeneral, LineUpGeneral),
    ),
)>;

/// This is the AST of the "grz" format
#[derive(Debug, PartialEq)]
pub enum Token {
    /// A method of splitting the screen a certain way
    Viewbox(
        /// The Viewbox itself
        Viewbox,
        Range<usize>,
    ),
    /// Objects on the slide
    Obj(
        /// The name of the object
        ((String, String), Vec<(String, String)>),
        Range<usize>,
    ),
    /// The slide itself
    Slide(
        /// A Vector containing all the objects in the slide
        SlideType,
        Range<usize>,
    ),
    /// Register
    Register((String, String)),
}

#[derive(Debug, Clone, Copy, PartialEq)]
/// A corner or an edge are made up of 2 ['LineUpGeneral'] values.
pub enum LineUpGeneral {
    /// To the left
    Left,
    /// Take it back now y'all
    Right,
    /// One hop this time
    Top,
    /// Reverse, reverse
    Bottom,
    /// Chacha real smooth.
    Center,
}

impl std::fmt::Display for LineUpGeneral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            LineUpGeneral::Top => write!(f, "^"),
            LineUpGeneral::Left => write!(f, "<"),
            LineUpGeneral::Right => write!(f, ">"),
            LineUpGeneral::Center => write!(f, "."),
            LineUpGeneral::Bottom => write!(f, "_"),
        }
    }
}

#[derive(Debug, Clone)]
/// Where the object lines up to
pub enum LineUp {
    /// .. or <> or ^_
    CenterCenter,
    /// ^< or <^
    TopLeft,
    /// << or .< or <.
    CenterLeft,
    /// _< or <_
    BottomLeft,
    /// __ or ._ or _.
    CenterBottom,
    /// _< or <_
    BottomRight,
    /// << or .< or <.
    CenterRight,
    /// ^< or <^
    TopRight,
    /// ^^ or .^ or ^.
    CenterTop,
}

/// Custom objects are used in grezi to account for varying implementations in drawing, and
/// bounds checking. Objects should have at least a `Header` type, and a `Paragraph` type. Here is
/// an example of what an [`Object`] could look like.
/// ```
/// #[derive(Debug, Clone)]
/// enum SlideObj {
///     Text {
///         value: String,
///         font_size: f32,
///         alignment: String
///     }
/// }
/// ```
pub trait Object: std::fmt::Debug + Sized {
    /// Errors will be displayed with the span of code that was in error.
    type Error: std::fmt::Display;

    /// Returns the width and height of the object
    fn bounds(&mut self, w: f64, h: f64) -> Result<(f64, f64), Self::Error>;

    /// Constructs an object based on it's name, type, and associated keys/values.
    fn construct(
        name: String,
        type_: String,
        values: &mut AHashMap<String, String>,
        registers: &AHashMap<String, String>,
    ) -> Result<Self, Self::Error>;
}

//--> Structs

/// A parsed and complete object
#[derive(Debug, Clone)]
pub struct SlideObject<T: Object + Clone> {
    /// The type of the object
    pub obj_type: T,
    /// The current position of the object
    pub position: Vec4<f64>,
    /// The current opacity of the object
    pub opacity: f64,
}

/// A slide
pub struct Slide<T: Object + Clone, I: Iterator<Item = Vec4<f64>>, O: Iterator<Item = f64>>(
    pub Vec<Cmd<T, I, O>>,
);

impl<T: Object + Clone, I: Iterator<Item = Vec4<f64>>, O: Iterator<Item = f64>> Slide<T, I, O> {
    /// Steps the entire slide forward, returning if anything changed.
    pub fn step(&mut self) -> bool {
        for f in self.0.iter_mut().map(|f| f.step()) {
            if !f {
                return false;
            }
        }
        true
    }
}

/// Commands are used to draw and move objects on the screen
pub struct Cmd<T: Object + Clone, I: Iterator<Item = Vec4<f64>>, O: Iterator<Item = f64>>(
    /// The object to add
    pub SlideObject<T>,
    /// Easing for (x, y, opacity)
    pub Zip<I, O>,
);

impl<T: Object + Clone, I: Iterator<Item = Vec4<f64>>, O: Iterator<Item = f64>> Cmd<T, I, O> {
    /// Step the given command forward
    pub fn step(&mut self) -> bool {
        if let Some(e) = self.1.next() {
            self.0.position = e.0;
            self.0.opacity = e.1;
            true
        } else {
            false
        }
    }
}

//--> Private Functions

fn fold_lineup(line_up: (LineUpGeneral, LineUpGeneral)) -> LineUp {
    match line_up.0 {
        LineUpGeneral::Center => match line_up.1 {
            LineUpGeneral::Center => LineUp::CenterCenter,
            LineUpGeneral::Top => LineUp::CenterTop,
            LineUpGeneral::Bottom => LineUp::CenterBottom,
            LineUpGeneral::Left => LineUp::CenterLeft,
            LineUpGeneral::Right => LineUp::CenterRight,
        },
        LineUpGeneral::Top => match line_up.1 {
            LineUpGeneral::Center => LineUp::CenterTop,
            LineUpGeneral::Top => LineUp::CenterTop,
            LineUpGeneral::Bottom => LineUp::CenterCenter,
            LineUpGeneral::Left => LineUp::TopLeft,
            LineUpGeneral::Right => LineUp::TopRight,
        },
        LineUpGeneral::Bottom => match line_up.1 {
            LineUpGeneral::Center => LineUp::CenterBottom,
            LineUpGeneral::Top => LineUp::CenterCenter,
            LineUpGeneral::Bottom => LineUp::CenterBottom,
            LineUpGeneral::Left => LineUp::BottomLeft,
            LineUpGeneral::Right => LineUp::BottomRight,
        },
        LineUpGeneral::Left => match line_up.1 {
            LineUpGeneral::Center => LineUp::CenterLeft,
            LineUpGeneral::Top => LineUp::TopLeft,
            LineUpGeneral::Bottom => LineUp::BottomLeft,
            LineUpGeneral::Left => LineUp::CenterLeft,
            LineUpGeneral::Right => LineUp::CenterCenter,
        },
        LineUpGeneral::Right => match line_up.1 {
            LineUpGeneral::Center => LineUp::CenterRight,
            LineUpGeneral::Top => LineUp::TopRight,
            LineUpGeneral::Bottom => LineUp::BottomRight,
            LineUpGeneral::Left => LineUp::CenterCenter,
            LineUpGeneral::Right => LineUp::CenterRight,
        },
    }
}

//--> Public Functions

/// The main tokenizer for grezi it takes a `.grz` file and returns an AST + All the errors
/// generated during parsing.
pub fn tokenizer(data: &[u8]) -> (Option<Vec<Token>>, Vec<Simple<u8>>) {
    let str_parser = filter(|c| *c != b'"')
        .repeated()
        .padded_by(just(b'"'))
        .collect::<Vec<u8>>()
        .map(|f| unsafe { String::from_utf8_unchecked(f) })
        .labelled("String Parser");

    let text_ident = choice((text::ident(), text::int(10)))
        .map(|f| unsafe { String::from_utf8_unchecked(f) })
        .labelled("Text Ident Parser");

    let int_parser = text::int(10).try_map(|f, span| match String::from_utf8(f) {
        Ok(string) => Ok(string),
        Err(_) => Err(chumsky::Error::expected_input_found(span, [None], None)),
    });

    let ident_parser = str_parser
        .or(text_ident)
        .padded()
        .then_ignore(just(b':'))
        .padded()
        .then(str_parser.or(text_ident).padded())
        .labelled("Identifier Parser");
    let index_parser = int_parser
        .from_str::<usize>()
        .unwrapped()
        .delimited_by(b'[', b']')
        .recover_with(nested_delimiters(b'[', b']', [], |_| 0));

    let edge_parser = choice((
        just(b'>').to(LineUpGeneral::Right),
        just(b'<').to(LineUpGeneral::Left),
        just(b'^').to(LineUpGeneral::Top),
        just(b'_').to(LineUpGeneral::Bottom),
        just(b'.').to(LineUpGeneral::Center),
    ));

    choice((
        ident_parser
            .then(index_parser)
            .then_ignore(just(b';'))
            .then(
                edge_parser
                    .then(edge_parser)
                    .then(edge_parser.then(edge_parser)),
            )
            .separated_by(just(b','))
            .allow_trailing()
            .padded()
            .recover_with(skip_until([b','], |_| Vec::new()))
            .delimited_by(b'{', b'}')
            .recover_with(nested_delimiters(b'{', b'}', [], |_| Vec::new()))
            .map_with_span(Token::Slide),
        ident_parser
            .then(index_parser.padded())
            .then(
                one_of(b"^_")
                    .to(layout::Direction::Vertical)
                    .or(one_of(b"<>").to(layout::Direction::Horizontal))
                    .padded(),
            )
            .then(
                choice((
                    int_parser
                        .from_str()
                        .unwrapped()
                        .then_ignore(just(b':'))
                        .then(int_parser.from_str().unwrapped())
                        .map(|(f, g)| Constraint::Ratio(f, g)),
                    int_parser
                        .from_str()
                        .unwrapped()
                        .then_ignore(just(b'%'))
                        .map(Constraint::Percentage),
                    int_parser
                        .from_str()
                        .unwrapped()
                        .then_ignore(just(b'+'))
                        .map(Constraint::Max),
                    int_parser
                        .from_str()
                        .unwrapped()
                        .then_ignore(just(b'-'))
                        .map(Constraint::Min),
                    int_parser.from_str().unwrapped().map(Constraint::Length),
                ))
                .padded()
                .separated_by(just(b','))
                .allow_trailing()
                .padded()
                .recover_with(skip_until([b','], |_| Vec::new())),
            )
            .then_ignore(just(b']'))
            .map_with_span(Token::Viewbox),
        ident_parser
            .then(
                ident_parser
                    .padded()
                    .separated_by(just(b','))
                    .allow_trailing()
                    .padded()
                    .recover_with(skip_until([b','], |_| Vec::new()))
                    .delimited_by(b'(', b')')
                    .recover_with(nested_delimiters(b'(', b')', [], |_| Vec::new())),
            )
            .map_with_span(Token::Obj),
        ident_parser.map(Token::Register),
    ))
    .padded()
    .separated_by(just(b';'))
    .allow_trailing()
    .padded()
    .then_ignore(end())
    .parse_recovery(data)
}

/// Converts a `.grz` file into a full slide show made up of commands. Commands contain the object
/// to be drawn as well as the iterator deciding where objects actually go to.
pub fn file_to_slideshow<
    U: Iterator<Item = Vec4<f64>>,
    C: Iterator<Item = f64>,
    K: Object + Clone,
>(
    file: &[u8],
    size: Rect,
    easing_fn: impl Fn(Vec4<f64>, Vec4<f64>, f64) -> U,
    opacity_fn: impl Fn(f64, f64, f64) -> C,
    opacity_steps: f64,
    delay: f64,
) -> Result<Vec<Slide<K, U, C>>, Vec<Simple<u8>>> {
    let mut layouts: AHashMap<String, Vec<Rect>> = AHashMap::default();
    let mut unused_objects: AHashMap<String, SlideObject<K>> = AHashMap::default();
    let mut objects_in_view: AHashMap<String, (*mut SlideObject<K>, Rect)> = AHashMap::default();
    let mut slides: Vec<Slide<K, U, C>> = Vec::new();
    let mut registers: AHashMap<String, String> = AHashMap::default();
    registers.insert(String::from("FONT_SIZE"), String::from("48"));
    registers.insert(String::from("HEADER_FONT_SIZE_ADD"), String::from("24"));
    registers.insert(String::from("FONT_FAMILY"), String::from("Helvetica"));
    registers.insert(
        String::from("HEADER_FONT_FAMILY"),
        String::from("Helvetica"),
    );
    let mut errors = Vec::new();

    let tokens = match tokenizer(file) {
        (Some(tokens), e) => {
            errors.extend(e);
            tokens
        }
        (None, e) => return Err(e),
    };

    for i in tokens {
        match i {
            Token::Slide(cmds, span) => unsafe {
                let mut new_slide = Slide(Vec::with_capacity(cmds.len()));
                let mut modified_names: Vec<String> = Vec::with_capacity(cmds.len());
                for (((name, split), split_index), (from, goto)) in cmds.into_iter() {
                    let vbx = if let Some(rect) = layouts.get(&split) {
                        rect[split_index]
                    } else if split == "Size" {
                        size
                    } else {
                        errors.push(Simple::custom(
                            span.clone(),
                            format!("Could not find viewbox {split}"),
                        ));
                        continue;
                    };
                    if let Some(obj) = objects_in_view.get_mut(&name) {
                        let obj_slide = (*(obj.0)).clone();
                        let w = obj_slide.position.z - obj_slide.position.x;
                        let h = obj_slide.position.w - obj_slide.position.y;
                        let pos_rect_from_xy = get_pos!(fold_lineup(from), obj.1, (w, h));
                        let pos_rect_goto_xy = get_pos!(fold_lineup(goto), vbx, (w, h));
                        let pos_rect_from = Vec4::new(
                            pos_rect_from_xy.0,
                            pos_rect_from_xy.1,
                            pos_rect_from_xy.0 + w,
                            pos_rect_from_xy.1 + h,
                        );
                        let pos_rect_goto = Vec4::new(
                            pos_rect_goto_xy.0,
                            pos_rect_goto_xy.1,
                            pos_rect_goto_xy.0 + w,
                            pos_rect_goto_xy.1 + h,
                        );
                        obj.1 = vbx;
                        new_slide.0.push(Cmd(
                            obj_slide,
                            easing_fn(pos_rect_from, pos_rect_goto, delay).zip(opacity_fn(
                                opacity_steps,
                                opacity_steps,
                                delay,
                            )),
                        ));
                        modified_names.push(name);
                    } else {
                        let obj = match unused_objects.get(&name) {
                            Some(obj) => obj,
                            None => {
                                errors.push(Simple::custom(
                                    span.clone(),
                                    format!("Could not find object {name}"),
                                ));
                                continue;
                            }
                        };
                        let mut obj_slide = obj.clone();
                        let bounds = match obj_slide.obj_type.bounds(vbx.width(), vbx.height()) {
                            Ok(b) => b,
                            Err(e) => {
                                errors.push(Simple::custom(span.clone(), e));
                                continue;
                            }
                        };
                        obj_slide.position.z = bounds.0;
                        obj_slide.position.w = bounds.1;
                        let pos_rect_from_xy = get_pos!(fold_lineup(from), vbx, bounds);
                        let pos_rect_goto_xy = get_pos!(fold_lineup(goto), vbx, bounds);
                        let pos_rect_from = Vec4::new(
                            pos_rect_from_xy.0,
                            pos_rect_from_xy.1,
                            pos_rect_from_xy.0 + bounds.0,
                            pos_rect_from_xy.1 + bounds.1,
                        );
                        let pos_rect_goto = Vec4::new(
                            pos_rect_goto_xy.0,
                            pos_rect_goto_xy.1,
                            pos_rect_goto_xy.0 + bounds.0,
                            pos_rect_goto_xy.1 + bounds.1,
                        );
                        new_slide.0.push(Cmd(
                            obj_slide,
                            easing_fn(pos_rect_from, pos_rect_goto, delay).zip(opacity_fn(
                                0.0,
                                opacity_steps,
                                delay,
                            )),
                        ));
                        modified_names.push(name.clone());
                        match new_slide.0.last_mut() {
                            Some(Cmd(obj, _)) => {
                                objects_in_view.insert(name, (obj, vbx));
                            }
                            _ => core::hint::unreachable_unchecked(),
                        }
                    }
                }
                objects_in_view.retain(|k, _| modified_names.contains(&k));
                slides.push(new_slide);
            },
            Token::Viewbox(((((name, split), split_index), direction), constraints), span) => {
                let rects = Layout::default()
                    .margin(50.0)
                    .direction(direction)
                    .constraints(constraints.as_ref())
                    .split(if let Some(rect) = layouts.get(&split) {
                        rect[split_index]
                    } else if split == "Size" {
                        size
                    } else {
                        errors.push(Simple::custom(
                            span,
                            format!("Could not find viewbox {split}"),
                        ));
                        continue;
                    });
                layouts.insert(name, rects.into_iter().collect());
            }
            Token::Obj(((name, type_), values), span) => {
                let mut values_map = values.into_iter().collect::<AHashMap<String, String>>();
                let obj = match K::construct(name.clone(), type_, &mut values_map, &registers) {
                    Ok(o) => o,
                    Err(e) => {
                        errors.push(Simple::custom(span, e));
                        continue;
                    }
                };
                unused_objects.insert(
                    name,
                    SlideObject {
                        obj_type: obj,
                        position: Vec4::zero(),
                        opacity: 0.0,
                    },
                );
            }
            Token::Register((name, value)) => {
                registers.insert(name, value);
            }
        }
    }
    if !errors.is_empty() {
        Err(errors)
    } else {
        Ok(slides)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const SIZE: layout::Rect = layout::Rect {
        left: 0.0,
        top: 0.0,
        right: 1920.0,
        bottom: 1080.0,
    };

    #[test]
    fn line_up() {
        assert_eq!(get_pos!(LineUp::TopLeft, SIZE, (50.0, 20.0)), (0.0, 0.0));
        assert_eq!(
            get_pos!(LineUp::TopRight, SIZE, (50.0, 20.0)),
            (1870.0, 0.0)
        );
        assert_eq!(
            get_pos!(LineUp::BottomLeft, SIZE, (50.0, 20.0)),
            (0.0, 1060.0)
        );
        assert_eq!(
            get_pos!(LineUp::BottomRight, SIZE, (50.0, 20.0)),
            (1870.0, 1060.0)
        );
        assert_eq!(
            get_pos!(LineUp::CenterTop, SIZE, (50.0, 20.0)),
            (935.0, 0.0)
        );
        assert_eq!(
            get_pos!(LineUp::CenterBottom, SIZE, (50.0, 20.0)),
            (935.0, 1060.0)
        );
        assert_eq!(
            get_pos!(LineUp::CenterRight, SIZE, (50.0, 20.0)),
            (1870.0, 530.0)
        );
        assert_eq!(
            get_pos!(LineUp::CenterLeft, SIZE, (50.0, 20.0)),
            (0.0, 530.0)
        );
        assert_eq!(
            get_pos!(LineUp::CenterCenter, SIZE, (50.0, 20.0)),
            (935.0, 530.0)
        );
    }

    #[test]
    fn parse_registers() {
        let register_lowercase = tokenizer("font_size: 24;".as_ref());
        let register_uppercase = tokenizer("FONT_SIZE: 24;".as_ref());
        let register_string_uppercase = tokenizer(r#"FONT_SIZE: "24";"#.as_ref());
        let register_cammel_case = tokenizer(r#"CustomVal: "ewr";"#.as_ref());
        assert!(register_lowercase.1.is_empty());
        assert_eq!(
            register_lowercase.0,
            Some(vec![Token::Register((
                String::from("font_size"),
                String::from("24")
            ))])
        );
        assert!(register_string_uppercase.1.is_empty());
        assert_eq!(
            register_string_uppercase.0,
            Some(vec![Token::Register((
                String::from("FONT_SIZE"),
                String::from("24")
            ))])
        );
        assert!(register_uppercase.1.is_empty());
        assert_eq!(
            register_uppercase.0,
            Some(vec![Token::Register((
                String::from("FONT_SIZE"),
                String::from("24")
            ))])
        );
        assert!(register_cammel_case.1.is_empty());
        assert_eq!(
            register_cammel_case.0,
            Some(vec![Token::Register((
                String::from("CustomVal"),
                String::from("ewr")
            ))])
        );
    }

    #[test]
    fn parse_viewboxes() {
        let viewboxes_ratios = [
            tokenizer("HorizontalBox:Size[0]^1:3,1:3,1:3,];".as_ref()),
            tokenizer("HorizontalBox: Size[0] ^ 1:3, 1:3, 1:3, ];".as_ref()),
            tokenizer(
                r#"HorizontalBox: Size[0] ^
    1:3,
    1:3,
    1:3,
];"#
                .as_ref(),
            ),
        ];
        for i in viewboxes_ratios {
            if let Some(ref viewbox) = i.0 {
                assert!(viewbox.len() == 1);
                assert_eq!(
                    match &viewbox[0] {
                        Token::Viewbox(arg, _) => arg,
                        t => panic!("Token is not a viewbox {:?}", t),
                    },
                    &(
                        (
                            ((String::from("HorizontalBox"), String::from("Size")), 0),
                            layout::Direction::Vertical
                        ),
                        vec![layout::Constraint::Ratio(1.0, 3.0); 3]
                    )
                );
            }
        }
        let viewboxes_percentages = [
            tokenizer("HorizontalBox:Size[0]^33%,33%,33%,];".as_ref()),
            tokenizer("HorizontalBox: Size[0] ^ 33%, 33%, 33%, ];".as_ref()),
            tokenizer(
                r#"HorizontalBox: Size[0] ^
    33%,
    33%,
    33%,
];"#
                .as_ref(),
            ),
        ];
        for i in viewboxes_percentages {
            if let Some(ref viewbox) = i.0 {
                assert!(viewbox.len() == 1);
                assert_eq!(
                    match &viewbox[0] {
                        Token::Viewbox(arg, _) => arg,
                        t => panic!("Token is not a viewbox {:?}", t),
                    },
                    &(
                        (
                            ((String::from("HorizontalBox"), String::from("Size")), 0),
                            layout::Direction::Vertical
                        ),
                        vec![layout::Constraint::Percentage(33.0); 3]
                    )
                );
            }
        }
        let viewboxes_mixed = [
            tokenizer("HorizontalBox:Size[0]^76,33-,40+,];".as_ref()),
            tokenizer("HorizontalBox: Size[0] ^ 76, 33-, 40+, ];".as_ref()),
            tokenizer(
                r#"HorizontalBox: Size[0] ^
    76,
    33-,
    40+,
];"#
                .as_ref(),
            ),
        ];
        for i in viewboxes_mixed {
            if let Some(ref viewbox) = i.0 {
                assert!(viewbox.len() == 1);
                assert_eq!(
                    match &viewbox[0] {
                        Token::Viewbox(arg, _) => arg,
                        t => panic!("Token is not a viewbox {:?}", t),
                    },
                    &(
                        (
                            ((String::from("HorizontalBox"), String::from("Size")), 0),
                            layout::Direction::Vertical
                        ),
                        vec![
                            layout::Constraint::Length(76.0),
                            layout::Constraint::Min(33.0),
                            layout::Constraint::Max(40.0)
                        ]
                    )
                );
            }
        }
    }

    #[test]
    fn parse_objs() {
        let objs_paragraph = [tokenizer(
            r#"SubtitleParagraph:Paragraph(value:"Neat",alignment:"left",font:"Fira Code",);"#
            .as_ref(),
            ),
            tokenizer(
                r#"SubtitleParagraph: Paragraph(value:"Neat", alignment:"left", font:"Fira Code", );"#
                .as_ref(),
                ),
                tokenizer(
                    r#"SubtitleParagraph: Paragraph(
    value:"Neat",
    alignment:"left",
    font:"Fira Code",
);"#
                    .as_ref(),
                    )];
        for i in objs_paragraph {
            if let Some(ref objs) = i.0 {
                assert!(objs.len() == 1);
                assert_eq!(
                    match &objs[0] {
                        Token::Obj(arg, _) => arg,
                        t => panic!("Token is not an object {:?}", t),
                    },
                    &(
                        (String::from("SubtitleParagraph"), String::from("Paragraph")),
                        vec![
                            (String::from("value"), String::from("Neat")),
                            (String::from("alignment"), String::from("left")),
                            (String::from("font"), String::from("Fira Code"))
                        ]
                    )
                );
            }
        }
    }

    #[test]
    fn parse_slides() {
        let slides = [
            tokenizer(r#"{Title:VertVbx[0];_<..,Subtitle:VertVbx[1];^>..,};"#.as_ref()),
            tokenizer(r#"{ Title: VertVbx[0];_<.., Subtitle: VertVbx[1];^>.., };"#.as_ref()),
            tokenizer(
                r#"{
Title: VertVbx[0];_<..,
Subtitle: VertVbx[1];^>..,
};"#
                .as_ref(),
            ),
        ];
        for i in slides {
            if let Some(ref slide) = i.0 {
                assert!(slide.len() == 1);
                assert_eq!(
                    match &slide[0] {
                        Token::Slide(arg, _) => arg,
                        t => panic!("Token is not an object {:?}", t),
                    },
                    &vec![
                        (
                            ((String::from("Title"), String::from("VertVbx")), 0),
                            (
                                (LineUpGeneral::Bottom, LineUpGeneral::Left),
                                (LineUpGeneral::Center, LineUpGeneral::Center)
                            )
                        ),
                        (
                            ((String::from("Subtitle"), String::from("VertVbx")), 1),
                            (
                                (LineUpGeneral::Top, LineUpGeneral::Right),
                                (LineUpGeneral::Center, LineUpGeneral::Center)
                            )
                        )
                    ]
                );
            }
        }
    }
}
