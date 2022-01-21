use std::time::{Duration, Instant};

use anyhow::Context;
use grezi::{layout::Rect, ObjectType, Slide};
use skulpin::{
    rafx::api::RafxExtents2D,
    skia_safe::{
        textlayout::{FontCollection, ParagraphBuilder, ParagraphStyle, TextStyle},
        Color4f, Font, FontMgr, FontStyle, Typeface,
    },
    winit::{
        event::{ElementState, Event, KeyboardInput, VirtualKeyCode, WindowEvent},
        event_loop::{ControlFlow, EventLoop},
        window::{Fullscreen, WindowBuilder},
    },
};
use structopt::StructOpt;

#[derive(StructOpt)]
struct Opts {
    #[structopt(short = "r", long, default_value = "60")]
    /// The fps to render the slideshow at
    fps: f64,
    #[structopt(short, long, default_value = "0.5")]
    /// The delay between slides in seconds
    delay: f64,
    #[structopt(short, long)]
    /// Active VSYNC
    vsync: bool,
    /// The file for Grezi to open
    file: String,
}

fn main() -> anyhow::Result<()> {
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
    let mut font_mgr = FontCollection::new();
    font_mgr.set_default_font_manager(FontMgr::default(), None);
    let mut slideshow = grezi::file_to_tokens(
        &map,
        size,
        |text, font_size, font_family, alignment| {
            let typeface =
                Typeface::new(font_family, FontStyle::normal()).context("Invalid typeface")?;
            let mut text_style = TextStyle::new();
            let font = Font::new(&typeface, font_size);
            text_style
                .set_font_size(font_size)
                .set_typeface(typeface)
                .set_font_families(&[font_family]);
            let mut style = ParagraphStyle::new();
            style
                .set_text_align(match alignment {
                    grezi::layout::Alignment::Left => {
                        skulpin::skia_safe::textlayout::TextAlign::Left
                    }
                    grezi::layout::Alignment::Right => {
                        skulpin::skia_safe::textlayout::TextAlign::Right
                    }
                    grezi::layout::Alignment::Center => {
                        skulpin::skia_safe::textlayout::TextAlign::Center
                    }
                })
                .set_text_style(&text_style);
            let mut paragraph = ParagraphBuilder::new(&style, &font_mgr);
            paragraph.add_text(text);
            let mut built = paragraph.build();
            let width = text
                .lines()
                .map(|f| font.measure_str(f, None).1.width())
                .max_by(|f, g| f.partial_cmp(g).unwrap())
                .unwrap();
            built.layout(width + 10.0);
            Ok((built.max_width(), built.height()))
        },
        easing::cubic_inout,
        easing::cubic_inout,
        opts.fps * opts.delay,
    )?;
    let mut skia = skulpin::RendererBuilder::new()
        .coordinate_system(skulpin::CoordinateSystem::VisibleRange(
            visible_range,
            skulpin::skia_safe::matrix::ScaleToFit::Center,
        ))
        .vsync_enabled(opts.vsync)
        .build(&winit_window, extents)?;
    let mut index = 0;
    let mut drawing = true;
    let mut previous_frame_start = Instant::now();
    let frame_duration = Duration::from_secs_f64(1.0 / opts.fps);
    event_loop.run(move |e, _window_target, control_flow| {
        let frame_start = Instant::now();
        *control_flow = ControlFlow::Wait;

        match e {
            Event::WindowEvent {
                event: WindowEvent::CloseRequested,
                ..
            } => {
                *control_flow = ControlFlow::Exit;
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
                }
                Some(VirtualKeyCode::Right) => {
                    if index != slideshow.len() - 1 {
                        index += 1;
                        drawing = true;
                    } else {
                        *control_flow = ControlFlow::Exit;
                    }
                }
                Some(VirtualKeyCode::Left) => {
                    if index != 0 {
                        index -= 1;
                        drawing = true;
                    }
                }
                _ => {}
            },
            Event::RedrawRequested(_) => {
                skia.draw(extents, s_factor, |canvas, _coordinate_system_helper| {
                    canvas.clear(Color4f::new(0.39, 0.39, 0.39, 1.0));
                    draw(&slideshow[index], canvas, &font_mgr);
                })
                .unwrap();
            }
            _ => (),
        }

        if drawing {
            if frame_start - previous_frame_start > frame_duration {
                drawing = slideshow[index].step();
                skia.draw(extents, s_factor, |canvas, _coordinate_system_helper| {
                    canvas.clear(Color4f::new(0.39, 0.39, 0.39, 1.0));
                    draw(&slideshow[index], canvas, &font_mgr);
                })
                .unwrap();
                previous_frame_start = frame_start;
            }
            *control_flow = ControlFlow::WaitUntil(previous_frame_start + frame_duration)
        }
    });
}

pub fn draw<I: Iterator<Item = f64>, O: Iterator<Item = f32>>(
    slide: &Slide<I, O>,
    canvas: &mut skulpin::skia_safe::Canvas,
    collection: &FontCollection,
) {
    for cmd in slide.0.iter() {
        match cmd {
            grezi::Cmd(obj, _) => match &obj.obj_type {
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
                        .set_color(skulpin::skia_safe::Color::from_argb(
                            (obj.opacity * 255.0) as u8,
                            255,
                            255,
                            255,
                        ));
                    let mut style = ParagraphStyle::new();
                    style
                        .set_text_align(match alignment {
                            grezi::layout::Alignment::Left => {
                                skulpin::skia_safe::textlayout::TextAlign::Left
                            }
                            grezi::layout::Alignment::Right => {
                                skulpin::skia_safe::textlayout::TextAlign::Right
                            }
                            grezi::layout::Alignment::Center => {
                                skulpin::skia_safe::textlayout::TextAlign::Center
                            }
                        })
                        .set_text_style(&text_style);
                    let mut paragraph =
                        skulpin::skia_safe::textlayout::ParagraphBuilder::new(&style, collection);
                    paragraph.add_text(value);
                    let mut built = paragraph.build();
                    built.layout(*max_width);
                    built.paint(canvas, (obj.position.left as f32, obj.position.top as f32));
                }
                _ => unimplemented!(),
            },
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
