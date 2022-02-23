use std::{mem::MaybeUninit, time::Instant};

use anyhow::{bail, Context};
use ariadne::Label;
use grezi::{layout::Rect, AHashMap, Slide};
use skulpin::{
    rafx::api::RafxExtents2D,
    skia_safe::{
        textlayout::{FontCollection, ParagraphBuilder, ParagraphStyle, TextAlign, TextStyle},
        Color4f, Font, FontMgr, FontStyle, Typeface,
    },
    winit::{
        event::{ElementState, Event, KeyboardInput, VirtualKeyCode, WindowEvent},
        event_loop::{ControlFlow, EventLoop},
        window::{Fullscreen, WindowBuilder},
    },
};
use structopt::StructOpt;
static mut FONT_CACHE: MaybeUninit<FontCollection> = MaybeUninit::uninit();

#[derive(StructOpt)]
struct Opts {
    #[structopt(short, long, default_value = "0.5")]
    /// The delay between slides in seconds
    delay: f64,
    #[structopt(long)]
    /// Deactivate VSYNC
    no_vsync: bool,
    /// The file for Grezi to open
    file: String,
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
        alignment: TextAlign,
        max_width: f32,
    },
    /// Draw a circle to the screen
    Point,
    Image(
        /// The image itself
        skulpin::skia_safe::Image,
    ),
}

impl grezi::Object for ObjectType {
    type Error = anyhow::Error;

    fn construct(
        _: &str,
        type_: &str,
        values: &mut AHashMap<String, String>,
        registers: &AHashMap<&str, &str>,
    ) -> anyhow::Result<Self> {
        match type_ {
            "Paragraph" | "paragraph" | "PP" | "pp" | "P" | "p" => {
                let value = match values.remove("value") {
                    Some(val) => val,
                    None => {
                        bail!("A value is required for your text");
                    }
                };
                let font_family = values.remove("font_family").unwrap_or_else(|| unsafe {
                    registers.get("FONT_FAMILY").unwrap_unchecked().to_string()
                });
                let alignment = match match values.remove("alignment") {
                    Some(a) => Some(a),
                    None => values.remove("align"),
                }
                .as_deref()
                {
                    Some("left") | Some("Left") => TextAlign::Left,
                    Some("right") | Some("Right") => TextAlign::Right,
                    _ => TextAlign::Center,
                };
                let font_size = values.remove("font_size").map_or_else(
                    || unsafe { registers.get("FONT_SIZE").unwrap_unchecked().parse::<f32>() },
                    |k| k.parse(),
                )?;
                Ok(ObjectType::Text {
                    value,
                    font_size,
                    font_family,
                    alignment,
                    max_width: 0.0,
                })
            }
            "Header" | "header" | "NH" | "nh" | "SH" | "sh" | "H" | "h" => {
                let value = match values.remove("value") {
                    Some(val) => val,
                    None => {
                        bail!("A value is required for your text")
                    }
                };
                let font_family = values.remove("font_family").unwrap_or_else(|| unsafe {
                    registers
                        .get("HEADER_FONT_FAMILY")
                        .unwrap_unchecked()
                        .to_string()
                });
                let alignment = match match values.remove("alignment") {
                    Some(a) => Some(a),
                    None => values.remove("align"),
                }
                .as_deref()
                {
                    Some("left") | Some("Left") => TextAlign::Left,
                    Some("right") | Some("Right") => TextAlign::Right,
                    _ => TextAlign::Center,
                };
                let font_size = values.remove("font_size").map_or_else(
                    || {
                        Ok(unsafe {
                            registers
                                .get("HEADER_FONT_SIZE_ADD")
                                .unwrap_unchecked()
                                .parse::<f32>()?
                                + registers
                                    .get("FONT_SIZE")
                                    .unwrap_unchecked()
                                    .parse::<f32>()?
                        })
                    },
                    |k| k.parse(),
                )?;
                Ok(ObjectType::Text {
                    value,
                    font_size,
                    font_family,
                    alignment,
                    max_width: 0.0,
                })
            }
            "Point" | "point" | "Circle" | "circle" => Ok(ObjectType::Point),
            /*
            "Image" | "image" | "Picture" | "picture" => {
            let image = skulpin::skia_safe::Bitmap::new();
            Ok(ObjectType::Image {
            data: skulpin::skia_safe::Bitmap::
            })
            }
            */
            "Image" | "image" | "img" | "IMG" | "pic" | "Pic" | "Picture" | "picture" => {
                let image = unsafe {
                    memmap::Mmap::map(&std::fs::File::open(
                        values.remove("path").context("Images require a path")?,
                    )?)?
                };
                let image = skulpin::skia_safe::image::Image::from_encoded(
                    skulpin::skia_safe::Data::new_copy(&image),
                )
                .context("Invalid image passed")?;
                Ok(ObjectType::Image(image))
            }
            e => {
                bail!("Object cannot have type {}", e)
            }
        }
    }

    fn bounds(&mut self, w: f64, h: f64) -> anyhow::Result<(f64, f64)> {
        let w = w as f32;
        let h = h as f32;
        match self {
            ObjectType::Text {
                value,
                font_size,
                font_family,
                alignment,
                max_width,
            } => {
                let typeface =
                    Typeface::new(&font_family, FontStyle::normal()).context("Invalid typeface")?;
                let mut text_style = TextStyle::new();
                let font = Font::new(&typeface, *font_size);
                text_style
                    .set_font_size(*font_size)
                    .set_typeface(typeface)
                    .set_font_families(&[font_family]);
                let mut style = ParagraphStyle::new();
                style.set_text_align(*alignment).set_text_style(&text_style);
                let mut paragraph =
                    ParagraphBuilder::new(&style, unsafe { FONT_CACHE.assume_init_ref() });
                paragraph.add_text(&value);
                let mut built = paragraph.build();
                let width = value
                    .lines()
                    .map(|f| font.measure_str(f, None).1.width())
                    .max_by(|f, g| f.partial_cmp(g).unwrap())
                    .unwrap()
                    + 10.0;
                *max_width = if width > w { w } else { width };
                built.layout(*max_width);
                if built.height() > h {
                    bail!("This text overflows the slide. text should be less than {h} pixels high")
                } else {
                    Ok((built.max_width() as f64, built.height() as f64))
                }
            }
            ObjectType::Image(image) => {
                let width = image.width() as f32;
                let height = image.height() as f32;
                let nw = if width > w { w } else { width };
                let nh = if height > h { h } else { height };
                let ds = nw.min(nh);
                let ss = width.max(height);

                Ok(((ds * width / ss) as f64, (ds * height / ss) as f64))
            }
            ObjectType::Point => unimplemented!(),
        }
    }
}

fn main() -> anyhow::Result<()> {
    rayon::ThreadPoolBuilder::new().build_global()?;
    let opts = Opts::from_args();
    let map = unsafe { memmap::Mmap::map(&std::fs::File::open(opts.file)?)? };
    let logical_size = skulpin::winit::dpi::LogicalSize::new(1920.0, 1080.0);
    let visible_range = skulpin::skia_safe::Rect {
        left: 0.0,
        right: logical_size.width,
        top: 0.0,
        bottom: logical_size.height,
    };
    let size = Rect {
        left: 0.0,
        top: 0.0,
        // windowed_context.window().inner_size().width as u16
        right: logical_size.width as f64,
        // dbg!(windowed_context.window().inner_size().height) as u16
        bottom: logical_size.height as f64,
    };
    let font_mgr = unsafe { FONT_CACHE.write(FontCollection::new()) };
    font_mgr.set_default_font_manager(FontMgr::default(), None);
    let mut slideshow = match grezi::file_to_slideshow(&map, size, 255.0) {
        Ok(slides) => slides,
        Err(errors) => {
            let mut report = ariadne::Report::build(ariadne::ReportKind::Error, (), 0);
            for e in errors {
                report = report
                    .with_label(Label::new(e.span()).with_message(&e))
                    .with_message(e.label().unwrap_or("Unknown Error"))
            }
            report.finish().eprint(ariadne::Source::from(unsafe {
                std::str::from_utf8_unchecked(map.as_ref())
            }))?;
            bail!("Failed to compile presentation")
        }
    };
    let event_loop = EventLoop::new();
    let winit_window = WindowBuilder::new()
        .with_title("Grezi")
        .with_inner_size(logical_size)
        .with_fullscreen(Some(Fullscreen::Borderless(None)))
        .with_transparent(true)
        .build(&event_loop)?;
    let window_size = winit_window.inner_size();
    let mut extents = RafxExtents2D {
        width: window_size.width,
        height: window_size.height,
    };
    let mut s_factor = winit_window.scale_factor();
    #[cfg(debug_assertions)]
    for i in slideshow.iter_mut() {
        i.1 = 1.0;
        i.step();
    }
    let mut skia = skulpin::RendererBuilder::new()
        .coordinate_system(skulpin::CoordinateSystem::VisibleRange(
            visible_range,
            skulpin::skia_safe::matrix::ScaleToFit::Center,
        ))
        .vsync_enabled(!opts.no_vsync)
        .build(&winit_window, extents)?;
    let mut index = 0;
    let mut drawing = true;
    let mut previous_frame_start = Instant::now();
    event_loop.run(move |e, _window_target, control_flow| {
        let frame_start = Instant::now();
        *control_flow = ControlFlow::Wait;

        match e {
            Event::WindowEvent {
                event: WindowEvent::CloseRequested,
                ..
            } => {
                *control_flow = ControlFlow::Exit;
                return;
            }
            Event::WindowEvent {
                event:
                    WindowEvent::ScaleFactorChanged {
                        scale_factor,
                        new_inner_size,
                    },
                ..
            } => {
                s_factor = scale_factor;
                extents = RafxExtents2D {
                    width: new_inner_size.width,
                    height: new_inner_size.height,
                };
            }
            Event::WindowEvent {
                event: WindowEvent::Resized(s),
                ..
            } => {
                extents = RafxExtents2D {
                    width: s.width,
                    height: s.height,
                };
            }
            Event::WindowEvent {
                event:
                    WindowEvent::KeyboardInput {
                        input:
                            KeyboardInput {
                                virtual_keycode,
                                state: ElementState::Pressed,
                                ..
                            },
                        ..
                    },
                ..
            } => match virtual_keycode {
                Some(VirtualKeyCode::Q) => {
                    *control_flow = ControlFlow::Exit;
                    return;
                }
                Some(VirtualKeyCode::Right) => {
                    if index != slideshow.len() - 1 {
                        index += 1;
                        drawing = true;
                        previous_frame_start = frame_start;
                    } else {
                        *control_flow = ControlFlow::Exit;
                        return;
                    }
                }
                Some(VirtualKeyCode::Left) => {
                    if index != 0 {
                        index -= 1;
                        drawing = true;
                        previous_frame_start = frame_start;
                    }
                }
                _ => {}
            },
            Event::RedrawRequested(_) => {
                skia.draw(extents, s_factor, |canvas, _coordinate_system_helper| {
                    canvas.clear(Color4f::new(0.39, 0.39, 0.39, 1.0));
                    draw(&slideshow[index], canvas, font_mgr);
                })
                .unwrap();
            }
            _ => (),
        }

        if drawing {
            slideshow[index].1 += (frame_start - previous_frame_start).as_secs_f64() / opts.delay;
            if slideshow[index].1 >= 1.0 {
                slideshow[index].1 = 1.0;
                drawing = false;
            }
            slideshow[index].step();
            skia.draw(extents, s_factor, |canvas, _coordinate_system_helper| {
                canvas.clear(Color4f::new(0.39, 0.39, 0.39, 1.0));
                draw(&slideshow[index], canvas, font_mgr);
            })
            .unwrap();
            previous_frame_start = frame_start;
            *control_flow = ControlFlow::Poll;
        }
    });
}

pub fn draw(
    slide: &Slide<ObjectType>,
    canvas: &mut skulpin::skia_safe::Canvas,
    collection: &FontCollection,
) {
    for cmd in slide.0.iter() {
        #[cfg(debug_assertions)]
        let workrect = skulpin::skia_safe::Rect::new(
            cmd.obj.parameters.position.x as f32,
            cmd.obj.parameters.position.y as f32,
            cmd.obj.parameters.position.z as f32,
            cmd.obj.parameters.position.w as f32,
        );
        match cmd.obj.obj_type {
            ObjectType::Text {
                ref value,
                font_size,
                ref font_family,
                alignment,
                max_width,
            } => {
                let typeface = Typeface::new(font_family, FontStyle::normal());
                let mut text_style = TextStyle::new();
                text_style
                    .set_font_size(font_size)
                    .set_font_families(&[font_family])
                    .set_typeface(typeface)
                    .set_color(skulpin::skia_safe::Color::from_argb(
                        cmd.obj.parameters.opacity as u8,
                        255,
                        255,
                        255,
                    ));
                let mut style = ParagraphStyle::new();
                style.set_text_align(alignment).set_text_style(&text_style);
                let mut paragraph =
                    skulpin::skia_safe::textlayout::ParagraphBuilder::new(&style, collection);
                paragraph.add_text(value);
                let mut built = paragraph.build();
                built.layout(max_width);
                built.paint(
                    canvas,
                    (
                        cmd.obj.parameters.position.x as f32,
                        cmd.obj.parameters.position.y as f32,
                    ),
                );
            }
            ObjectType::Image(ref img) => {
                let paint = skulpin::skia_safe::Paint::new(Color4f::new(1.0, 1.0, 1.0, 1.0), None);
                canvas.draw_image_rect(
                    img,
                    None,
                    skulpin::skia_safe::Rect::new(
                        cmd.obj.parameters.position.x as f32,
                        cmd.obj.parameters.position.y as f32,
                        cmd.obj.parameters.position.z as f32,
                        cmd.obj.parameters.position.w as f32,
                    ),
                    &paint,
                );
            }
            _ => unimplemented!(),
        }
        #[cfg(debug_assertions)]
        {
            let paint = skulpin::skia_safe::Paint::new(
                skulpin::skia_safe::Color4f::new(1.0, 1.0, 0.5, 0.5),
                None,
            );
            canvas.draw_rect(workrect, &paint);
            canvas.draw_circle((workrect.left, workrect.top), 20.0, &paint);
        }
    }
    #[cfg(debug_assertions)]
    {
        let paint = skulpin::skia_safe::Paint::new(
            skulpin::skia_safe::Color4f::new(1.0, 1.0, 0.5, 0.5),
            None,
        );
        canvas.draw_rect(
            skulpin::skia_safe::Rect::from_xywh(955.0, 0.0, 10.0, 1080.0),
            &paint,
        );
        canvas.draw_rect(
            skulpin::skia_safe::Rect::from_xywh(0.0, 535.0, 1920.0, 10.0),
            &paint,
        );
    }
}
