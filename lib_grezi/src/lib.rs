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
//!     fn bounds(&mut self, w: f32, h: f32) -> Result<(f32, f32), Self::Error> {
//!         Ok((50.0, 50.0))
//!     }
//!     fn construct(
//!         name: &str,
//!         type_: &str,
//!         values: &mut grezi::AHashMap<String, String>,
//!         _: &grezi::AHashMap<&str, &str>
//!     ) -> Result<Self, Self::Error> {
//!         Ok(Self {
//!             name: name.to_string(),
//!             type_: type_.to_string(),
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

use std::{borrow::Cow, ops::Range};

use ahash::RandomState;
/// A type alias for a [`std::collections::HashMap`] which uses [`ahash`] as it's hasher.
pub type AHashMap<K, V> = std::collections::HashMap<K, V, RandomState>;
use chumsky::prelude::*;
use glam::{const_vec4, Vec4};
use layout::{Constraint, Layout, Rect};
use rayon::iter::{IntoParallelRefMutIterator, ParallelIterator};

//--> Macros

#[macro_export]
/// A macro for converting a lineup value, a viewbox and an object to a pair of x, y coordinates.
macro_rules! get_pos {
    ($line_up:expr, $vbx:expr, $obj:expr) => {
        match $line_up {
            LineUp::TopLeft => Some(($vbx.left, $vbx.top)),
            LineUp::TopRight => Some(($vbx.right - $obj.0, $vbx.top)),
            LineUp::BottomLeft => Some(($vbx.left, $vbx.bottom - $obj.1)),
            LineUp::BottomRight => Some(($vbx.right - $obj.0, $vbx.bottom - $obj.1)),
            LineUp::CenterTop => Some((($vbx.left + $vbx.right) / 2.0 - ($obj.0 / 2.0), $vbx.top)),
            LineUp::CenterBottom => Some((
                ($vbx.left + $vbx.right) / 2.0 - ($obj.0 / 2.0),
                $vbx.bottom - $obj.1,
            )),
            LineUp::CenterLeft => {
                Some(($vbx.left, ($vbx.top + $vbx.bottom) / 2.0 - ($obj.1 / 2.0)))
            }
            LineUp::CenterRight => Some((
                $vbx.right - $obj.0,
                ($vbx.top + $vbx.bottom) / 2.0 - ($obj.1 / 2.0),
            )),
            LineUp::CenterCenter => Some((
                ($vbx.left + $vbx.right) / 2.0 - ($obj.0 / 2.0),
                ($vbx.top + $vbx.bottom) / 2.0 - ($obj.1 / 2.0),
            )),
            LineUp::None => None,
        }
    };
}

//--> Types

/// A method of splitting the screen a certain way
type Viewbox = (
    (((String, String), usize), layout::Direction),
    Vec<Constraint>,
);

type SlideType = (
    Vec<(
        ((String, String), usize),
        (
            (LineUpGeneral, LineUpGeneral),
            (LineUpGeneral, LineUpGeneral),
        ),
    )>,
    Vec<(String, String)>,
);

type Slideshow<K, F> = Result<Vec<Slide<K, F>>, Vec<Simple<u8>>>;

//--> Enums

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
        ((String, String), AHashMap<String, String>),
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
    /// Command
    Command(((String, String), String), Range<usize>),
}

#[derive(Debug, Clone, Copy, PartialEq)]
/// A corner or an edge are made up of 2 [`LineUpGeneral`] values.
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
    /// Reverse, reverse
    None,
}

impl std::fmt::Display for LineUpGeneral {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            LineUpGeneral::Top => write!(f, "^"),
            LineUpGeneral::Left => write!(f, "<"),
            LineUpGeneral::Right => write!(f, ">"),
            LineUpGeneral::Center => write!(f, "."),
            LineUpGeneral::Bottom => write!(f, "_"),
            LineUpGeneral::None => Ok(()),
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
    /// None, not specified
    None,
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
    fn bounds(&mut self, w: f32, h: f32) -> Result<(f32, f32), Self::Error>;

    /// Constructs an object based on it's name, type, and associated keys/values.
    fn construct(
        name: &str,
        type_: String,
        values: AHashMap<String, String>,
        registers: &AHashMap<Cow<str>, Cow<str>>,
    ) -> Result<Self, Self::Error>;
}

pub trait Functions: Sized {
    type Error: std::fmt::Display;

    fn construct(name: String, arg: String) -> Result<Self, Self::Error>;
}

//--> Structs

/// The parameters of the object
#[derive(Debug, Clone, Copy)]
pub struct Parameters {
    /// The current position of the object
    pub position: Vec4,
    /// The current opacity of the object
    pub opacity: f32,
}

/// A parsed and complete object
#[derive(Debug, Clone)]
pub struct SlideObject<T: Object + Clone> {
    /// The type of the object
    pub obj_type: T,
    /// The parameters of the object
    pub parameters: Parameters,
}

/// A slide
pub struct Slide<T: Object + Clone, F: Functions> {
    /// The commands inside the slide
    pub cmds: Vec<Cmd<T>>,
    pub calls: Vec<F>,
    /// The current frame the slide is on between 0.0-1.0
    pub step: f32,
    bg_from: Vec4,
    bg_to: Vec4,
}

impl<T: Object + Clone, F: Functions> Slide<T, F> {
    /// Steps the entire slide forward, returning if anything changed.
    #[inline]
    pub fn step(&mut self) -> Vec4 {
        let ease = if self.step < 0.5 {
            4.0 * self.step.powi(3)
        } else {
            (self.step - 1.0).mul_add((2.0 * self.step - 2.0).powi(2), 1.0)
        };
        self.cmds.par_iter_mut().for_each(|f| f.step(ease));
        self.bg_from.lerp(self.bg_to, ease)
    }
}

/// Commands are used to draw and move objects on the screen
pub struct Cmd<T: Object + Clone> {
    /// The object to add
    pub obj: SlideObject<T>,
    /// The original state of the object before the command
    pub iter_from: Parameters,
    /// The end state of the object
    pub iter_to: Parameters,
}

unsafe impl<T: Object + Clone> Send for Cmd<T> {}

unsafe impl<T: Object + Clone> Sync for Cmd<T> {}

impl<T: Object + Clone> Cmd<T> {
    /// Step the given command forward
    #[inline]
    pub fn step(&mut self, ease: f32) {
        self.obj.parameters.position = self.iter_from.position.lerp(self.iter_to.position, ease);
        self.obj.parameters.opacity =
            self.iter_from.opacity + (self.iter_to.opacity - self.iter_from.opacity) * ease;
    }
}

//--> Private Functions

#[inline]
fn fold_lineup(line_up: &(LineUpGeneral, LineUpGeneral)) -> LineUp {
    match line_up.0 {
        LineUpGeneral::Center => match line_up.1 {
            LineUpGeneral::Center => LineUp::CenterCenter,
            LineUpGeneral::Top => LineUp::CenterTop,
            LineUpGeneral::Bottom => LineUp::CenterBottom,
            LineUpGeneral::Left => LineUp::CenterLeft,
            LineUpGeneral::Right => LineUp::CenterRight,
            _ => LineUp::None,
        },
        LineUpGeneral::Top => match line_up.1 {
            LineUpGeneral::Center => LineUp::CenterTop,
            LineUpGeneral::Top => LineUp::CenterTop,
            LineUpGeneral::Bottom => LineUp::CenterCenter,
            LineUpGeneral::Left => LineUp::TopLeft,
            LineUpGeneral::Right => LineUp::TopRight,
            _ => LineUp::None,
        },
        LineUpGeneral::Bottom => match line_up.1 {
            LineUpGeneral::Center => LineUp::CenterBottom,
            LineUpGeneral::Top => LineUp::CenterCenter,
            LineUpGeneral::Bottom => LineUp::CenterBottom,
            LineUpGeneral::Left => LineUp::BottomLeft,
            LineUpGeneral::Right => LineUp::BottomRight,
            _ => LineUp::None,
        },
        LineUpGeneral::Left => match line_up.1 {
            LineUpGeneral::Center => LineUp::CenterLeft,
            LineUpGeneral::Top => LineUp::TopLeft,
            LineUpGeneral::Bottom => LineUp::BottomLeft,
            LineUpGeneral::Left => LineUp::CenterLeft,
            LineUpGeneral::Right => LineUp::CenterCenter,
            _ => LineUp::None,
        },
        LineUpGeneral::Right => match line_up.1 {
            LineUpGeneral::Center => LineUp::CenterRight,
            LineUpGeneral::Top => LineUp::TopRight,
            LineUpGeneral::Bottom => LineUp::BottomRight,
            LineUpGeneral::Left => LineUp::CenterCenter,
            LineUpGeneral::Right => LineUp::CenterRight,
            _ => LineUp::None,
        },
        LineUpGeneral::None => LineUp::None,
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
        .delimited_by(just(b'['), just(b']'))
        .recover_with(nested_delimiters(b'[', b']', [], |_| 0));

    let edge_parser = choice((
        just(b'>').to(LineUpGeneral::Right),
        just(b'<').to(LineUpGeneral::Left),
        just(b'^').to(LineUpGeneral::Top),
        just(b'_').to(LineUpGeneral::Bottom),
        just(b'.').to(LineUpGeneral::Center),
        any().rewind().to(LineUpGeneral::None),
    ));

    choice((
        ident_parser
            .then(index_parser)
            .then(
                edge_parser
                    .then(edge_parser)
                    .then(edge_parser.then(edge_parser)),
            )
            .separated_by(just(b','))
            .allow_trailing()
            .padded()
            .recover_with(skip_until([b','], |_| Vec::new()))
            .delimited_by(just(b'{'), just(b'}'))
            .recover_with(nested_delimiters(b'{', b'}', [], |_| Vec::new()))
            .then(
                str_parser
                    .or(text_ident)
                    .padded()
                    .then(
                        str_parser
                            .or(text_ident)
                            .padded()
                            .delimited_by(just(b'('), just(b')'))
                            .recover_with(nested_delimiters(b'(', b')', [], |_| String::new())),
                    )
                    .separated_by(just(b','))
                    .allow_trailing()
                    .padded()
                    .recover_with(skip_until([b','], |_| Vec::new()))
                    .delimited_by(just(b'['), just(b']'))
                    .recover_with(nested_delimiters(b'[', b']', [], |_| Vec::new())),
            )
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
                    .delimited_by(just(b'('), just(b')'))
                    .recover_with(nested_delimiters(b'(', b')', [], |_| Vec::new()))
                    .collect::<AHashMap<String, String>>(),
            )
            .map_with_span(Token::Obj),
        ident_parser.map(Token::Register),
        str_parser
            .or(text_ident)
            .then_ignore(just(b'.'))
            .then(str_parser.or(text_ident).padded())
            .then_ignore(just(b':'))
            .padded()
            .then(str_parser.or(text_ident).padded())
            .map_with_span(Token::Command),
    ))
    .padded()
    .separated_by(just(b';'))
    .allow_trailing()
    .padded()
    .then_ignore(end())
    .parse_recovery(data)
}

/// Converts a `.grz` file into a full slide show made up of commands. Commands contain the object
/// to be drawn as wellwas the iterator deciding where objects actually go to.
pub fn file_to_slideshow<K: Object + Clone, F: Functions>(
    file: &[u8],
    size: Rect,
    opacity_steps: f32,
) -> Slideshow<K, F> {
    let mut layouts: AHashMap<String, Vec<Rect>> = AHashMap::default();
    let mut unused_objects: AHashMap<String, SlideObject<K>> = AHashMap::default();
    let mut objects_in_view: AHashMap<String, (*const SlideObject<K>, Rect, Vec4, LineUp)> =
        AHashMap::default();
    let mut slides: Vec<Slide<K, F>> = Vec::new();
    let mut registers: AHashMap<Cow<str>, Cow<str>> = AHashMap::default();
    registers.insert(Cow::Borrowed("FONT_SIZE"), Cow::Borrowed("48"));
    registers.insert(Cow::Borrowed("HEADER_FONT_SIZE_ADD"), Cow::Borrowed("24"));
    registers.insert(Cow::Borrowed("FONT_FAMILY"), Cow::Borrowed("Helvetica"));
    registers.insert(
        Cow::Borrowed("HEADER_FONT_FAMILY"),
        Cow::Borrowed("Helvetica"),
    );
    registers.insert(Cow::Borrowed("VIEWBOX_MARGIN"), Cow::Borrowed("25"));
    registers.insert(
        Cow::Borrowed("BG_COLOR"),
        Cow::Borrowed("0.39, 0.39, 0.39, 1.0"),
    );
    let mut errors = Vec::new();

    let mut bg = const_vec4!([0.39, 0.39, 0.39, 1.0]);
    let mut last_bg = bg;
    for i in match tokenizer(file) {
        (Some(tokens), e) => {
            errors.extend(e);
            tokens
        }
        (None, e) => return Err(e),
    } {
        match i {
            Token::Slide(cmds, span) => unsafe {
                let mut bg_parser = registers.get("BG_COLOR").unwrap_unchecked().split(",");
                bg.x = if let Some(bgx) = bg_parser.next() {
                    if let Ok(b) = bgx.trim().parse() {
                        b
                    } else {
                        errors.push(Simple::custom(span.clone(), "Error parsing BG_COLOR"));
                        continue;
                    }
                } else {
                    errors.push(Simple::custom(span.clone(), "Error parsing BG_COLOR"));
                    continue;
                };
                bg.y = if let Some(bgx) = bg_parser.next() {
                    if let Ok(b) = bgx.trim().parse() {
                        b
                    } else {
                        errors.push(Simple::custom(span.clone(), "Error parsing BG_COLOR"));
                        continue;
                    }
                } else {
                    errors.push(Simple::custom(span.clone(), "Error parsing BG_COLOR"));
                    continue;
                };
                bg.z = if let Some(bgx) = bg_parser.next() {
                    if let Ok(b) = bgx.trim().parse() {
                        b
                    } else {
                        errors.push(Simple::custom(span.clone(), "Error parsing BG_COLOR"));
                        continue;
                    }
                } else {
                    errors.push(Simple::custom(span.clone(), "Error parsing BG_COLOR"));
                    continue;
                };
                bg.w = if let Some(bgx) = bg_parser.next() {
                    if let Ok(b) = bgx.trim().parse() {
                        b
                    } else {
                        errors.push(Simple::custom(span.clone(), "Error parsing BG_COLOR"));
                        continue;
                    }
                } else {
                    errors.push(Simple::custom(span.clone(), "Error parsing BG_COLOR"));
                    continue;
                };
                let mut new_slide = Slide {
                    cmds: Vec::with_capacity(cmds.0.len()),
                    calls: Vec::with_capacity(cmds.1.len()),
                    step: 0.0,
                    bg_from: last_bg,
                    bg_to: bg,
                };
                last_bg = bg;
                let mut modified_names: Vec<String> = Vec::with_capacity(cmds.0.len());
                for (((name, split), split_index), (from, goto)) in cmds.0 {
                    let vbx = if let Some(rect) = layouts.get(split.as_str()) {
                        rect[split_index]
                    } else if split == "Size" {
                        let vbx_margin = registers.get("VIEWBOX_MARGIN").unwrap_unchecked();
                        let margin = if let Ok(num) = vbx_margin.parse::<f32>() {
                            num
                        } else {
                            errors.push(Simple::custom(
                                span.clone(),
                                format!("\"{}\" is not a valid floating point number", vbx_margin),
                            ));
                            continue;
                        };
                        size.inner(&layout::Margin {
                            vertical: margin,
                            horizontal: margin,
                        })
                    } else {
                        errors.push(Simple::custom(
                            span.clone(),
                            format!("Could not find viewbox {split}"),
                        ));
                        continue;
                    };
                    if let Some(obj) = objects_in_view.get_mut(&name) {
                        let mut obj_slide = (*(obj.0)).clone();
                        let w = obj.2.z - obj.2.x;
                        let h = obj.2.w - obj.2.y;
                        let pos_rect_from_xy = get_pos!(fold_lineup(&from), obj.1, (w, h));
                        let bounds = match obj_slide.obj_type.bounds(vbx.width(), vbx.height()) {
                            Ok(b) => b,
                            Err(e) => {
                                errors.push(Simple::custom(span.clone(), e));
                                continue;
                            }
                        };
                        let goto_folded = fold_lineup(&goto);
                        let pos_rect_goto_xy = get_pos!(goto_folded, vbx, bounds);
                        let pos_rect_from;
                        let pos_rect_goto = if let Some(pos) = pos_rect_goto_xy {
                            obj.3 = goto_folded;
                            let p = pos_rect_from_xy.unwrap_unchecked();
                            pos_rect_from = Vec4::new(p.0, p.1, p.0 + w, p.1 + h);
                            Vec4::new(pos.0, pos.1, pos.0 + bounds.0, pos.1 + bounds.1)
                        } else {
                            let pos = get_pos!(obj.3, vbx, bounds).unwrap_unchecked();
                            pos_rect_from = obj.2;
                            Vec4::new(pos.0, pos.1, pos.0 + bounds.0, pos.1 + bounds.1)
                        };
                        obj.1 = vbx;
                        obj.2 = pos_rect_goto;
                        new_slide.cmds.push(Cmd {
                            obj: obj_slide,
                            iter_from: Parameters {
                                position: pos_rect_from.into(),
                                opacity: opacity_steps,
                            },
                            iter_to: Parameters {
                                position: pos_rect_goto.into(),
                                opacity: opacity_steps,
                            },
                        });
                        modified_names.push(name);
                    } else {
                        let obj = match unused_objects.get(name.as_str()) {
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
                        let pos_rect_from_xy = match get_pos!(fold_lineup(&from), vbx, bounds) {
                            Some(b) => b,
                            None => {
                                errors.push(Simple::custom(
                                    span.clone(),
                                    format!("Object must have a known location to specify None"),
                                ));
                                continue;
                            }
                        };
                        let goto_folded = fold_lineup(&goto);
                        let pos_rect_goto_xy = match get_pos!(goto_folded, vbx, bounds) {
                            Some(b) => b,
                            None => {
                                errors.push(Simple::custom(
                                    span.clone(),
                                    format!("Object must have a known location to specify None"),
                                ));
                                continue;
                            }
                        };
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
                        new_slide.cmds.push(Cmd {
                            iter_to: Parameters {
                                position: pos_rect_goto.into(),
                                opacity: opacity_steps,
                            },
                            obj: obj_slide,
                            iter_from: Parameters {
                                position: pos_rect_from.into(),
                                opacity: 0.0,
                            },
                        });
                        match new_slide.cmds.last() {
                            Some(Cmd { obj, .. }) => {
                                objects_in_view
                                    .insert(name.clone(), (obj, vbx, pos_rect_goto, goto_folded));
                            }
                            _ => core::hint::unreachable_unchecked(),
                        }
                        modified_names.push(name);
                    }
                }
                objects_in_view.retain(|k, _| modified_names.contains(k));
                for k in cmds.1 {
                    new_slide.calls.push(match F::construct(k.0, k.1) {
                        Ok(b) => b,
                        Err(e) => {
                            errors.push(Simple::custom(span.clone(), format!("Error: {}", e)));
                            continue;
                        }
                    });
                }
                slides.push(new_slide);
            },
            Token::Viewbox(((((name, split), split_index), direction), constraints), span) => {
                let vbx_margin = unsafe { registers.get("VIEWBOX_MARGIN").unwrap_unchecked() };
                let margin = if let Ok(num) = vbx_margin.parse::<f32>() {
                    num
                } else {
                    errors.push(Simple::custom(
                        span.clone(),
                        format!("\"{}\" is not a valid floating point number", vbx_margin),
                    ));
                    continue;
                };
                let mut rects = Layout::default()
                    .direction(&direction)
                    .constraints(&constraints)
                    .split(if let Some(rect) = layouts.get(split.as_str()) {
                        rect[split_index]
                    } else if split == "Size" {
                        let vbx_margin =
                            unsafe { registers.get("VIEWBOX_MARGIN").unwrap_unchecked() };
                        let margin = if let Ok(num) = vbx_margin.parse::<f32>() {
                            num
                        } else {
                            errors.push(Simple::custom(
                                span.clone(),
                                format!("\"{}\" is not a valid floating point number", vbx_margin),
                            ));
                            continue;
                        };
                        size.inner(&layout::Margin {
                            vertical: margin,
                            horizontal: margin,
                        })
                    } else {
                        errors.push(Simple::custom(
                            span.clone(),
                            format!("Could not find viewbox {split}"),
                        ));
                        continue;
                    });
                rects.iter_mut().for_each(|f| {
                    *f = f.inner(&layout::Margin {
                        vertical: margin,
                        horizontal: margin,
                    })
                });
                layouts.insert(name, rects);
            }
            Token::Obj(((name, type_), values), span) => {
                let obj = match K::construct(&name, type_, values, &registers) {
                    Ok(o) => o,
                    Err(e) => {
                        errors.push(Simple::custom(span.clone(), e));
                        continue;
                    }
                };
                unused_objects.insert(
                    name,
                    SlideObject {
                        obj_type: obj,
                        parameters: Parameters {
                            position: Vec4::ZERO,
                            opacity: 0.0,
                        },
                    },
                );
            }
            Token::Register((name, value)) => {
                registers.insert(Cow::Owned(name), Cow::Owned(value));
            }
            Token::Command(((_, _), _), _) => {} /*
                                                 Token::Command(((name, field), value), span) => {
                                                 let obj = match unused_objects.get_mut(name.as_str()) {
                                                 Some(obj) => obj,
                                                 None => {
                                                 errors.push(Simple::custom(
                                                 span.clone(),
                                                 format!("Could not find object {name}"),
                                                 ));
                                                 continue;
                                                 }
                                                 };
                                                 obj.obj_type.change_field(field, value);
                                                 }
                                                 */
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
        assert_eq!(
            get_pos!(LineUp::TopLeft, SIZE, (50.0, 20.0)),
            Some((0.0, 0.0))
        );
        assert_eq!(
            get_pos!(LineUp::TopRight, SIZE, (50.0, 20.0)),
            Some((1870.0, 0.0))
        );
        assert_eq!(
            get_pos!(LineUp::BottomLeft, SIZE, (50.0, 20.0)),
            Some((0.0, 1060.0))
        );
        assert_eq!(
            get_pos!(LineUp::BottomRight, SIZE, (50.0, 20.0)),
            Some((1870.0, 1060.0))
        );
        assert_eq!(
            get_pos!(LineUp::CenterTop, SIZE, (50.0, 20.0)),
            Some((935.0, 0.0))
        );
        assert_eq!(
            get_pos!(LineUp::CenterBottom, SIZE, (50.0, 20.0)),
            Some((935.0, 1060.0))
        );
        assert_eq!(
            get_pos!(LineUp::CenterRight, SIZE, (50.0, 20.0)),
            Some((1870.0, 530.0))
        );
        assert_eq!(
            get_pos!(LineUp::CenterLeft, SIZE, (50.0, 20.0)),
            Some((0.0, 530.0))
        );
        assert_eq!(
            get_pos!(LineUp::CenterCenter, SIZE, (50.0, 20.0)),
            Some((935.0, 530.0))
        );
        assert_eq!(get_pos!(LineUp::None, SIZE, (50.0, 20.0)), None);
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
        let mut hashmap_eq = AHashMap::default();
        hashmap_eq.insert(String::from("value"), String::from("Neat"));
        hashmap_eq.insert(String::from("alignment"), String::from("left"));
        hashmap_eq.insert(String::from("font"), String::from("Fira Code"));
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
                        hashmap_eq.clone()
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
