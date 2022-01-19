//--> Use statements

use std::{collections::HashMap, iter::Zip};

use anyhow::{bail, Context};
use ariadne::{Label, Source};
use chumsky::prelude::*;
use skia_safe::{
    textlayout::{FontCollection, ParagraphStyle, TextStyle},
    Canvas, FontStyle, Rect, Typeface,
};
use tui::layout::{Constraint, Layout};

//--> Macros

#[macro_export]
/// A macro for converting a lineup value, a viewbox and an object to a pair of x, y coordinates.
/// Equivalent to
/// ```
/// fn get_pos(line_up: LineUp, vbx: tui::Rect, obj: Object) -> (u16, u16)
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
    (((String, String), usize), tui::layout::Direction),
    Vec<Constraint>,
);

type SlideType = Vec<(((String, String), usize), (LineUp, LineUp))>;

/// This is the AST of the "grz" format
pub enum Token {
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
        Object,
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

/// Commands are used to draw and move objects on the screen
pub struct Cmd(
    /// The object to add
    Object,
    /// Easing for (x, y, opacity)
    Zip<Zip<easing::CubicInOut, easing::CubicInOut>, easing::CubicInOut>,
);

impl Cmd {
    /// Advance the object's easing functions
    pub fn step(&mut self) -> bool {
        if let Some(e) = self.1.next() {
            self.0.position.set_xywh(
                e.0 .0,
                e.0 .1,
                self.0.position.width(),
                self.0.position.height(),
            );
            self.0.opacity = e.1;
            true
        } else {
            false
        }
    }
}

#[derive(Debug, Clone)]
/// The type of the objcet in question
pub enum ObjectType {
    /// Draw text to the screen
    Text {
        /// The text to draw, each index in the vector is a newline.
        value: String,
        /// Font size of the text
        font_size: f32,
        /// Font family of the text
        font_family: String,
        /// Alignment of the text in question
        alignment: skia_safe::textlayout::TextAlign,
        /// Max width of the text block (for internal use)
        max_width: f32,
    },
    /// Draw a circle to the screen
    Point,
}

//--> Structs

/// A parsed and complete object
#[derive(Debug, Clone)]
pub struct Object {
    /// The type of the object
    pub obj_type: ObjectType,
    /// The current position of the object
    pub position: Rect,
    /// The current opacity of the object
    pub opacity: f32,
}

/// A slide
pub struct Slide(pub Vec<Cmd>);

impl Slide {
    pub fn step(&mut self) -> bool {
        for f in self.0.iter_mut().map(|f| f.step()) {
            if !f {
                return false;
            }
        }
        true
    }
    pub fn draw(&self, canvas: &mut Canvas, collection: &FontCollection) {
        for cmd in self.0.iter() {
            match cmd {
                Cmd(obj, _) => match &obj.obj_type {
                    ObjectType::Text {
                        value,
                        font_size,
                        font_family,
                        alignment,
                        max_width,
                    } => {
                        let typeface = Typeface::new(font_family, FontStyle::normal());
                        let mut text_style = TextStyle::new();
                        text_style
                            .set_font_size(*font_size)
                            .set_font_families(&[font_family])
                            .set_typeface(typeface)
                            .set_color(skia_safe::Color::from_argb(
                                (obj.opacity * 255.0) as u8,
                                255,
                                255,
                                255,
                            ));
                        let mut style = ParagraphStyle::new();
                        style.set_text_align(*alignment).set_text_style(&text_style);
                        let mut paragraph =
                            skia_safe::textlayout::ParagraphBuilder::new(&style, collection);
                        paragraph.add_text(value);
                        let mut built = paragraph.build();
                        built.layout(*max_width);
                        built.paint(canvas, (obj.position.left, obj.position.top));
                        #[cfg(debug_assertions)]
                        {
                            let paint = skia_safe::Paint::new(
                                skia_safe::Color4f::new(1.0, 1.0, 0.5, 0.5),
                                None,
                            );
                            canvas.draw_rect(obj.position, &paint);
                            canvas.draw_circle((obj.position.left, obj.position.top), 20.0, &paint);
                        }
                    }
                    _ => unimplemented!(),
                },
            }
        }
        #[cfg(debug_assertions)]
        {
            let paint = skia_safe::Paint::new(skia_safe::Color4f::new(1.0, 1.0, 0.5, 0.5), None);
            canvas.draw_rect(skia_safe::Rect::from_xywh(955.0, 0.0, 10.0, 1080.0), &paint);
            canvas.draw_rect(skia_safe::Rect::from_xywh(0.0, 535.0, 1920.0, 10.0), &paint);
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

pub fn file_to_tokens<F>(
    file: &[u8],
    size_tui: tui::layout::Rect,
    bounds_fn: F,
    fps: u64,
) -> anyhow::Result<Vec<Slide>>
where
    F: Fn(&String, f32, &String, skia_safe::textlayout::TextAlign) -> anyhow::Result<(f32, f32)>,
{
    let mut layouts_raw: HashMap<String, Vec<Rect>> = HashMap::new();
    let layouts: *mut HashMap<String, Vec<Rect>> = &mut layouts_raw as *mut _;
    let mut unused_objects_raw: HashMap<String, Object> = HashMap::new();
    let unused_objects: *mut HashMap<String, Object> = &mut unused_objects_raw as *mut _;
    let mut objects_in_view_raw: HashMap<String, (*mut Object, Rect)> = HashMap::new();
    let objects_in_view: *mut HashMap<String, (*mut Object, Rect)> =
        &mut objects_in_view_raw as *mut _;
    let mut slides: Vec<Slide> = Vec::new();
    let slides_ptr: *mut Vec<Slide> = &mut slides as *mut _;
    let size = Rect {
        left: size_tui.left() as f32,
        right: size_tui.right() as f32,
        top: size_tui.top() as f32,
        bottom: size_tui.bottom() as f32,
    };
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
            .to(tui::layout::Direction::Vertical)
            .or(just(b">").to(tui::layout::Direction::Horizontal))
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
                            .margin(25)
                            .direction(direction)
                            .constraints(constraints.as_ref())
                            .split(if let Some(rect) = unsafe { &mut *layouts }.get(&split) {
                                tui::layout::Rect {
                                    x: rect[split_index].left as u16,
                                    y: rect[split_index].top as u16,
                                    width: rect[split_index].width() as u16,
                                    height: rect[split_index].height() as u16,
                                }
                            } else if split == "Size" {
                                size_tui
                            } else {
                                return Err(Simple::custom(
                                        span,
                                        format!("Could not find viewbox {split}"),
                                        ));
                            });
                        unsafe { &mut *layouts }.insert(
                            name,
                            rects
                            .into_iter()
                            .map(|f| Rect {
                                left: f.left() as f32,
                                top: f.top() as f32,
                                right: f.right() as f32,
                                bottom: f.bottom() as f32,
                            })
                            .collect(),
                            );
                        Ok(())
                    },
                    )
                        .or(ident_parser
                            .then(
                                ident_parser
                                .padded()
                                .separated_by(just(b','))
                                .collect::<HashMap<String, String>>()
                                .delimited_by(b'(', b')'),
                                )
                            .try_map(|((name, type_), mut values), span| {
                                let obj = match type_.as_str() {
                                    "Paragraph" | "paragraph" | "PP" | "pp" | "P" | "p" => {
                                        let value =
                                            values
                                            .remove("value")
                                            .with_context(|| format!("A value is required for your text, try adding one\n{}: {}(value: \"Your Text\");", name, type_))
                                            .unwrap()
                                            ;
                                        let font_family = values
                                            .remove("font_family")
                                            .unwrap_or(String::from("Helvetica"));
                                        let alignment = match match values.remove("alignment") {
                                            Some(a) => Some(a),
                                            None => values.remove("align"),
                                        }
                                        .as_deref()
                                        {
                                            Some("left") | Some("Left") => skia_safe::textlayout::TextAlign::Left,
                                            Some("right") | Some("Right") => skia_safe::textlayout::TextAlign::Right,
                                            _ => skia_safe::textlayout::TextAlign::Center,
                                        };
                                        let font_size = values
                                            .remove("font_size")
                                            .map_or_else(|| 48.0, |k| k.parse().unwrap());
                                        let bounds = match bounds_fn(&value, font_size, &font_family, alignment) {
                                            Ok(bounds) => bounds,
                                            Err(e) => return Err(Simple::custom(span, e.to_string())),
                                        };
                                        Object {
                                            obj_type: ObjectType::Text {
                                                value,
                                                font_size,
                                                font_family,
                                                alignment,
                                                max_width: bounds.0,
                                            },
                                            position: Rect {
                                                left: 0.0,
                                                top: 0.0,
                                                right: bounds.0,
                                                bottom: bounds.1,
                                            },
                                            opacity: 0.0,
                                        }
                                    }
                                    "Header" | "header" | "NH" | "nh" | "SH" | "sh" | "H" | "h" => {
                                        let value =
                                            values
                                            .remove("value")
                                            .with_context(|| format!("A value is required for your text, try adding one\n{}: {}(value: \"Your Text\");", name, type_))
                                            .unwrap();
                                        let font_family =
                                            values
                                            .remove("font_family")
                                            .unwrap_or(String::from("Helvetica"));
                                        let alignment = match match values.remove("alignment") {
                                            Some(a) => Some(a),
                                            None => values.remove("align"),
                                        }
                                        .as_deref()
                                        {
                                            Some("left") | Some("Left") => skia_safe::textlayout::TextAlign::Left,
                                            Some("right") | Some("Right") => skia_safe::textlayout::TextAlign::Right,
                                            _ => skia_safe::textlayout::TextAlign::Center,
                                        };
                                        let font_size =
                                            values
                                            .remove("font_size")
                                            .map_or_else(|| 72.0, |k| k.parse().unwrap());
                                        let bounds = match bounds_fn(&value, font_size, &font_family, alignment) {
                                            Ok(bounds) => bounds,
                                            Err(e) => return Err(Simple::custom(span, e.to_string())),
                                        };
                                        Object {
                                            obj_type: ObjectType::Text {
                                                value,
                                                font_size,
                                                font_family,
                                                alignment,
                                                max_width: bounds.0,
                                            },
                                            position: Rect {
                                                left: 0.0,
                                                top: 0.0,
                                                right: bounds.0,
                                                bottom: bounds.1,
                                            },
                                            opacity: 0.0,
                                        }
                                    }
                                    "Point" | "point" | "Circle" | "circle" => {
                                        Object {
                                            obj_type: ObjectType::Point,
                                            position: Rect {
                                                left: 0.0,
                                                right: 0.0,
                                                top: 0.0,
                                                bottom: 0.0,
                                            },
                                            opacity: 0.0,
                                        }
                                    }
                                    e => panic!("Object cannot have type {}", e),
                                };
                                unsafe { &mut *unused_objects }.insert(
                                    name,
                                    obj
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
        .map(|cmds| unsafe {
            let mut new_slide = Slide(Vec::with_capacity(cmds.len()));
            let mut modified_names: Vec<String> = Vec::with_capacity(cmds.len());
            for (((name, split), split_index), (from, goto)) in cmds.into_iter() {
                let vbx = if let Some(rect) = (&mut *layouts).get(&split) {
                    rect[split_index]
                } else if split == "Size" {
                    size
                } else {
                    panic!("Could not find viewbox {split}")
                };
                if let Some(obj) = (&mut *objects_in_view).get_mut(&name) {
                    let pos_rect_from = get_pos!(from, obj.1, *obj.0);
                    let pos_rect_goto = get_pos!(goto, vbx, *obj.0);
                    obj.1 = vbx;
                    new_slide.0.push(Cmd(
                        (*(obj.0)).clone(),
                        easing::cubic_inout(pos_rect_from.0, pos_rect_goto.0, fps / 2)
                            .zip(easing::cubic_inout(
                                pos_rect_from.1,
                                pos_rect_goto.1,
                                fps / 2,
                            ))
                            .zip(easing::cubic_inout(1.0, 1.0, fps / 2)),
                    ));
                    modified_names.push(name);
                } else {
                    let obj = (*unused_objects)
                        .get(&name)
                        .with_context(|| format!("Could not find object {name}"))
                        .unwrap();
                    let pos_rect_from = get_pos!(from, vbx, obj);
                    let pos_rect_goto = get_pos!(goto, vbx, obj);
                    new_slide.0.push(Cmd(
                        obj.clone(),
                        easing::cubic_inout(pos_rect_from.0, pos_rect_goto.0, fps / 2)
                            .zip(easing::cubic_inout(
                                pos_rect_from.1,
                                pos_rect_goto.1,
                                fps / 2,
                            ))
                            .zip(easing::cubic_inout(0.0, 1.0, fps / 2)),
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
        })
        .or(obj_viewbox_parser)
        .padded()
        .separated_by(just(b';'))
        .allow_trailing()
        .padded()
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
