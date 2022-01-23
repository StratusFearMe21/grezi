//--> Use statements

pub mod layout;

use std::iter::Zip;

use ahash::RandomState;
pub type AHashMap<K, V> = std::collections::HashMap<K, V, RandomState>;
use anyhow::bail;
use ariadne::{Label, Source};
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
    ($line_up:expr,$vbx:expr,$obj:expr) => {
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

type SlideType = Vec<(((String, String), usize), (LineUp, LineUp))>;

/// This is the AST of the "grz" format
pub enum Token<T: Object + Clone> {
    /// A method of splitting the screen a certain way
    Viewbox(
        /// The Viewbox itself
        Viewbox,
    ),
    /// Objects on the slide
    Obj(
        /// The name of the object
        String,
        /// The object itself
        SlideObject<T>,
    ),
    /// The slide itself
    Slide(
        /// A Vector containing all the objects in the slide
        SlideType,
    ),
}

#[derive(Debug, Clone, Copy)]
enum LineUpGeneral {
    Center,
    Left,
    Right,
    Top,
    Bottom,
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
    fn bounds(&mut self, w: f64, h: f64) -> anyhow::Result<(f64, f64)>;
    fn construct(
        name: String,
        type_: String,
        values: &mut AHashMap<String, String>,
    ) -> anyhow::Result<Self>;
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

pub fn file_to_tokens<
    U: ToPrimitive + One + Zero + NumAssignOps + PartialOrd + Copy,
    I: Iterator<Item = f64>,
    O: Iterator<Item = f32>,
    E: Fn(f64, f64, U) -> I,
    G: Fn(f32, f32, U) -> O,
    T: Object + Clone,
>(
    file: &[u8],
    size: Rect,
    opacity_fn: G,
    easing_fn: E,
    delay: U,
) -> anyhow::Result<Vec<Slide<T, I, O>>> {
    let mut layouts_raw: AHashMap<String, Vec<Rect>> = AHashMap::default();
    let layouts: *mut AHashMap<String, Vec<Rect>> = &mut layouts_raw as *mut _;
    let mut unused_objects_raw: AHashMap<String, SlideObject<T>> = AHashMap::default();
    let unused_objects: *mut AHashMap<String, SlideObject<T>> = &mut unused_objects_raw as *mut _;
    let mut objects_in_view_raw: AHashMap<String, (*mut SlideObject<T>, Rect)> =
        AHashMap::default();
    let objects_in_view: *mut AHashMap<String, (*mut SlideObject<T>, Rect)> =
        &mut objects_in_view_raw as *mut _;
    let mut slides: Vec<Slide<T, I, O>> = Vec::new();
    let slides_ptr: *mut Vec<Slide<T, I, O>> = &mut slides as *mut _;

    let str_parser = filter(|c| *c != b'"')
        .repeated()
        .padded_by(just(b'\"'))
        .collect::<Vec<u8>>()
        .map(|f| unsafe { String::from_utf8_unchecked(f) })
        .labelled("String Parser");

    let int_parser = text::int(10).try_map(|f, span| match String::from_utf8(f) {
        Ok(string) => Ok(string),
        Err(_) => Err(chumsky::Error::expected_input_found(span, [None], None)),
    });

    let ident_parser = str_parser
        .or(text::ident().map(|f| unsafe { String::from_utf8_unchecked(f) }))
        .padded()
        .then_ignore(just(b':'))
        .then(
            str_parser
                .or(text::ident().map(|f| unsafe { String::from_utf8_unchecked(f) }))
                .padded(),
        )
        .labelled("Identifier Parser");
    let index_parser = int_parser
        .from_str::<usize>()
        .unwrapped()
        .delimited_by(b'[', b']');

    let edge_parser = choice((
        just(b'>').to(LineUpGeneral::Right),
        just(b'<').to(LineUpGeneral::Left),
        just(b'^').to(LineUpGeneral::Top),
        just(b'_').to(LineUpGeneral::Bottom),
        just(b'.').to(LineUpGeneral::Center),
    ));

    let obj_viewbox_parser = ident_parser
        .then(index_parser.padded())
        .then(
            just(b"^")
                .to(layout::Direction::Vertical)
                .or(just(b">").to(layout::Direction::Horizontal))
                .padded(),
        )
        .then(
            int_parser
                .from_str()
                .unwrapped()
                .then_ignore(just(b':'))
                .then(int_parser.from_str().unwrapped())
                .map(|(f, g)| Constraint::Ratio(f, g))
                .or(int_parser
                    .from_str()
                    .unwrapped()
                    .then_ignore(just(b'%'))
                    .map(Constraint::Percentage))
                .or(int_parser
                    .from_str()
                    .unwrapped()
                    .then_ignore(just(b'+'))
                    .map(Constraint::Max))
                .or(int_parser
                    .from_str()
                    .unwrapped()
                    .then_ignore(just(b'-'))
                    .map(Constraint::Min))
                .or(int_parser.from_str().unwrapped().map(Constraint::Length))
                .padded()
                .separated_by(just(b','))
                .allow_trailing(),
        )
        .then_ignore(just(b']'))
        .try_map(
            |((((name, split), split_index), direction), constraints), span| {
                let rects = Layout::default()
                    .margin(50.0)
                    .direction(direction)
                    .constraints(constraints.as_ref())
                    .split(if let Some(rect) = unsafe { &mut *layouts }.get(&split) {
                        rect[split_index]
                    } else if split == "Size" {
                        size
                    } else {
                        return Err(Simple::custom(
                            span,
                            format!("Could not find viewbox {split}"),
                        ));
                    });
                unsafe { &mut *layouts }.insert(name, rects.into_iter().collect());
                Ok(())
            },
        )
        .or(ident_parser
            .then(
                ident_parser
                    .padded()
                    .separated_by(just(b','))
                    .collect::<AHashMap<String, String>>()
                    .delimited_by(b'(', b')'),
            )
            .try_map(|((name, type_), mut values), span| {
                let obj = match T::construct(name.clone(), type_, &mut values) {
                    Ok(o) => o,
                    Err(e) => return Err(Simple::custom(span, e)),
                };
                unsafe { &mut *unused_objects }.insert(
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
                Ok(())
            })
            .ignored());

    let lexer = ident_parser
        .then(index_parser)
        .then_ignore(just(b';'))
        .then(
            edge_parser
                .then(edge_parser)
                .map(fold_lineup)
                .then(edge_parser.then(edge_parser).map(fold_lineup)),
        )
        .separated_by(just(b','))
        .allow_trailing()
        .padded()
        .delimited_by(b'{', b'}')
        .try_map(|cmds, span| unsafe {
            let mut new_slide = Slide(Vec::with_capacity(cmds.len()));
            let mut modified_names: Vec<String> = Vec::with_capacity(cmds.len());
            for (((name, split), split_index), (from, goto)) in cmds.into_iter() {
                let vbx = if let Some(rect) = (&mut *layouts).get(&split) {
                    rect[split_index]
                } else if split == "Size" {
                    size
                } else {
                    return Err(Simple::custom(
                        span,
                        format!("Could not find viewbox {split}"),
                    ));
                };
                if let Some(obj) = (&mut *objects_in_view).get_mut(&name) {
                    let mut obj_slide = (*(obj.0)).clone();
                    let bounds = match obj_slide.obj_type.bounds(vbx.width(), vbx.height()) {
                        Ok(b) => b,
                        Err(e) => return Err(Simple::custom(span, e)),
                    };
                    obj_slide.position.right = bounds.0;
                    obj_slide.position.bottom = bounds.1;
                    let pos_rect_from = get_pos!(from, obj.1, obj_slide);
                    let pos_rect_goto = get_pos!(goto, vbx, obj_slide);
                    obj.1 = vbx;
                    new_slide.0.push(Cmd(
                        obj_slide,
                        easing_fn(pos_rect_from.0, pos_rect_goto.0, delay)
                            .zip(easing_fn(pos_rect_from.1, pos_rect_goto.1, delay))
                            .zip(opacity_fn(1.0f32, 1.0f32, delay)),
                    ));
                    modified_names.push(name);
                } else {
                    let obj = match (*unused_objects).get(&name) {
                        Some(obj) => obj,
                        None => {
                            return Err(Simple::custom(
                                span,
                                format!("Could not find object {name}"),
                            ));
                        }
                    };
                    let mut obj_slide = obj.clone();
                    let bounds = match obj_slide.obj_type.bounds(vbx.width(), vbx.height()) {
                        Ok(b) => b,
                        Err(e) => return Err(Simple::custom(span, e)),
                    };
                    obj_slide.position.right = bounds.0;
                    obj_slide.position.bottom = bounds.1;
                    let pos_rect_from = get_pos!(from, vbx, obj_slide);
                    let pos_rect_goto = get_pos!(goto, vbx, obj_slide);
                    new_slide.0.push(Cmd(
                        obj_slide,
                        easing_fn(pos_rect_from.0, pos_rect_goto.0, delay)
                            .zip(easing_fn(pos_rect_from.1, pos_rect_goto.1, delay))
                            .zip(opacity_fn(0.0f32, 1.0f32, delay)),
                    ));
                    modified_names.push(name.clone());
                    match new_slide.0.last_mut() {
                        Some(Cmd(obj, _)) => {
                            (&mut *objects_in_view).insert(name, (obj, vbx));
                        }
                        _ => core::hint::unreachable_unchecked(),
                    }
                }
            }
            (&mut *objects_in_view).retain(|k, _| modified_names.contains(&k));
            (&mut *slides_ptr).push(new_slide);
            Ok(())
        })
        .or(obj_viewbox_parser)
        .padded()
        .separated_by(just(b';'))
        .allow_trailing()
        .padded()
        .ignored()
        .then_ignore(end());
    match lexer.parse(file) {
        Ok(_) => Ok(slides),
        Err(parse_errs) => {
            parse_errs.into_iter().for_each(|e: Simple<u8>| {
                ariadne::Report::build(ariadne::ReportKind::Error, (), 0)
                    .with_label(Label::new(e.span()).with_message(format!("{:?}", e.reason())))
                    .with_message(e.label().unwrap_or("Unkown error"))
                    .finish()
                    .eprint(Source::from(unsafe { std::str::from_utf8_unchecked(file) }))
                    .unwrap();
            });
            bail!("Failed to compile presentation")
        }
    }
}
