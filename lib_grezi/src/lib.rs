//--> Use statements

pub mod layout;

use std::{iter::Zip, ops::Range};

use ahash::RandomState;
pub type AHashMap<K, V> = std::collections::HashMap<K, V, RandomState>;
use chumsky::prelude::*;
use layout::{Constraint, Layout, Rect};
use num_traits::{NumAssignOps, One, ToPrimitive, Zero};

//--> Macros

#[macro_export]
/// A macro for converting a lineup value, a viewbox and an object to a pair of x, y coordinates.
/// Equivalent to
/// ```
/// fn get_pos(line_up: grezi::LineUp, vbx: grezi::layout::Rect, obj: grezi::Object) -> (u16, u16) {}
/// ```
macro_rules! get_pos {
    ($line_up:expr, $vbx:expr, $obj:expr) => {
        match $line_up {
            LineUp::TopLeft => ($vbx.left, $vbx.top),
            LineUp::TopRight => ($vbx.right - $obj.position.width(), $vbx.top),
            LineUp::BottomLeft => ($vbx.left, $vbx.bottom - $obj.position.height()),
            LineUp::BottomRight => (
                $vbx.right - $obj.position.width(),
                $vbx.bottom - $obj.position.height(),
            ),
            LineUp::CenterTop => (
                ($vbx.left + $vbx.right) / 2.0 - ($obj.position.width() / 2.0),
                $vbx.top,
            ),
            LineUp::CenterBottom => (
                ($vbx.left + $vbx.right) / 2.0 - ($obj.position.width() / 2.0),
                $vbx.bottom - $obj.position.height(),
            ),
            LineUp::CenterLeft => (
                $vbx.left,
                ($vbx.top + $vbx.bottom) / 2.0 - ($obj.position.height() / 2.0),
            ),
            LineUp::CenterRight => (
                $vbx.right - $obj.position.width(),
                ($vbx.top + $vbx.bottom) / 2.0 - ($obj.position.height() / 2.0),
            ),
            LineUp::CenterCenter => (
                ($vbx.left + $vbx.right) / 2.0 - ($obj.position.width() / 2.0),
                ($vbx.top + $vbx.bottom) / 2.0 - ($obj.position.height() / 2.0),
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

#[derive(Debug, Clone, Copy)]
pub enum LineUpGeneral {
    Center,
    Left,
    Right,
    Top,
    Bottom,
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

pub trait Object: std::fmt::Debug + Sized {
    type Error: std::fmt::Display;

    fn bounds(&mut self, w: f64, h: f64) -> Result<(f64, f64), Self::Error>;
    fn construct(
        name: String,
        type_: String,
        values: &mut AHashMap<String, String>,
    ) -> Result<Self, Self::Error>;
}

//--> Structs

/// A parsed and complete object
#[derive(Debug, Clone)]
pub struct SlideObject<T: Object + Clone> {
    /// The type of the object
    pub obj_type: T,
    /// The current position of the object
    pub position: Rect,
    /// The current opacity of the object
    pub opacity: f32,
}

/// A slide
pub struct Slide<T: Object + Clone, I: Iterator<Item = f64>, O: Iterator<Item = f32>>(
    pub Vec<Cmd<T, I, O>>,
);

impl<T: Object + Clone, I: Iterator<Item = f64>, O: Iterator<Item = f32>> Slide<T, I, O> {
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
pub struct Cmd<T: Object + Clone, I: Iterator<Item = f64>, O: Iterator<Item = f32>>(
    /// The object to add
    pub SlideObject<T>,
    /// Easing for (x, y, opacity)
    pub Zip<Zip<I, I>, O>,
);

impl<T: Object + Clone, I: Iterator<Item = f64>, O: Iterator<Item = f32>> Cmd<T, I, O> {
    /// Advance the object's easing functions
    pub fn step(&mut self) -> bool {
        if let Some(e) = self.1.next() {
            let width = self.0.position.width();
            let height = self.0.position.height();
            self.0.position.left = e.0 .0;
            self.0.position.top = e.0 .1;
            self.0.position.right = e.0 .0 + width;
            self.0.position.bottom = e.0 .1 + height;
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
            .recover_with(skip_until([b','], |_| Vec::new()))
            .padded()
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
                .recover_with(skip_until([b','], |_| Vec::new()))
                .padded(),
            )
            .then_ignore(just(b']'))
            .map_with_span(Token::Viewbox),
        ident_parser
            .then(
                ident_parser
                    .padded()
                    .separated_by(just(b','))
                    .allow_trailing()
                    .recover_with(skip_until([b','], |_| Vec::new()))
                    .padded()
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

pub fn file_to_tokens<
    F: ToPrimitive + One + Zero + NumAssignOps + PartialOrd + Copy,
    U: Iterator<Item = f64>,
    C: Iterator<Item = f32>,
    K: Object + Clone,
>(
    file: &[u8],
    size: Rect,
    opacity_fn: impl Fn(f32, f32, F) -> C,
    easing_fn: impl Fn(f64, f64, F) -> U,
    delay: F,
) -> Result<Vec<Slide<K, U, C>>, Vec<Simple<u8>>> {
    let mut layouts: AHashMap<String, Vec<Rect>> = AHashMap::default();
    let mut unused_objects: AHashMap<String, SlideObject<K>> = AHashMap::default();
    let mut objects_in_view: AHashMap<String, (*mut SlideObject<K>, Rect)> = AHashMap::default();
    let mut slides: Vec<Slide<K, U, C>> = Vec::new();
    let mut registers: AHashMap<String, String> = AHashMap::default();
    registers.insert("FONT_SIZE".to_string(), "48".to_string());
    registers.insert("HEADER_FONT_SIZE_ADD".to_string(), "24".to_string());
    registers.insert("FONT_FAMILY".to_string(), "Helvetica".to_string());
    registers.insert("HEADER_FONT_FAMILY".to_string(), "Helvetica".to_string());
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
                        let mut obj_slide = (*(obj.0)).clone();
                        let bounds = match obj_slide.obj_type.bounds(vbx.width(), vbx.height()) {
                            Ok(b) => b,
                            Err(e) => {
                                errors.push(Simple::custom(span.clone(), e));
                                continue;
                            }
                        };
                        obj_slide.position.right = bounds.0;
                        obj_slide.position.bottom = bounds.1;
                        let pos_rect_from = get_pos!(fold_lineup(from), obj.1, obj_slide);
                        let pos_rect_goto = get_pos!(fold_lineup(goto), vbx, obj_slide);
                        obj.1 = vbx;
                        new_slide.0.push(Cmd(
                            obj_slide,
                            easing_fn(pos_rect_from.0, pos_rect_goto.0, delay)
                                .zip(easing_fn(pos_rect_from.1, pos_rect_goto.1, delay))
                                .zip(opacity_fn(1.0f32, 1.0f32, delay)),
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
                        obj_slide.position.right = bounds.0;
                        obj_slide.position.bottom = bounds.1;
                        let pos_rect_from = get_pos!(fold_lineup(from), vbx, obj_slide);
                        let pos_rect_goto = get_pos!(fold_lineup(goto), vbx, obj_slide);
                        new_slide.0.push(Cmd(
                            obj_slide,
                            easing_fn(pos_rect_from.0, pos_rect_goto.0, delay)
                                .zip(easing_fn(pos_rect_from.1, pos_rect_goto.1, delay))
                                .zip(opacity_fn(0.0f32, 1.0f32, delay)),
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
                values_map.extend(registers.clone());
                let obj = match K::construct(name.clone(), type_, &mut values_map) {
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
                        position: Rect {
                            left: 0.0,
                            right: 0.0,
                            top: 0.0,
                            bottom: 0.0,
                        },
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
