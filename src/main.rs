mod skia_render;

use std::time::{Duration, Instant};

use anyhow::Context;
use glutin::{
    event::{ElementState, Event, KeyboardInput, VirtualKeyCode, WindowEvent},
    event_loop::ControlFlow,
    window::Fullscreen,
};
use glutin::{event_loop::EventLoop, ContextBuilder};
use grezi::{layout::Rect, ObjectType, Slide};
use skia_render::SkiaRenderer;
use skia_safe::{
    textlayout::{FontCollection, ParagraphStyle, TextStyle},
    Canvas, Color4f, Font, FontMgr, FontStyle, Typeface,
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
    let event_loop = EventLoop::new();
    let monitor_size = event_loop.primary_monitor().unwrap().size();
    let size = Rect {
        left: 0.0,
        top: 0.0,
        // windowed_context.window().inner_size().width as u16
        right: monitor_size.width as f64,
        // dbg!(windowed_context.window().inner_size().height) as u16
        bottom: monitor_size.height as f64,
    };
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
            style.set_text_align(alignment).set_text_style(&text_style);
            let mut paragraph = skia_safe::textlayout::ParagraphBuilder::new(&style, &font_mgr);
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
    let winit_window = glutin::window::WindowBuilder::new()
        .with_title("Grezi")
        .with_fullscreen(Some(Fullscreen::Borderless(None)))
        .with_transparent(true);
    let windowed_context = ContextBuilder::new()
        .with_pixel_format(24, 8)
        .with_stencil_buffer(8)
        .with_gl_profile(glutin::GlProfile::Core)
        .with_vsync(opts.vsync)
        .build_windowed(winit_window, &event_loop)?;
    let windowed_context = unsafe { windowed_context.make_current().unwrap() };
    let mut skia = SkiaRenderer::new(&windowed_context);
    let mut index = 0;
    let mut drawing = true;
    let mut previous_frame_start = Instant::now();
    event_loop.run(move |e, _window_target, control_flow| {
        let frame_start = Instant::now();

        match e {
            Event::WindowEvent {
                event: WindowEvent::CloseRequested,
                ..
            } => {
                *control_flow = ControlFlow::Exit;
                return;
            }
            Event::WindowEvent {
                event: WindowEvent::Resized(physical_size),
                ..
            } => {
                skia.resize(&windowed_context);
                windowed_context.resize(physical_size);
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
                    } else {
                        *control_flow = ControlFlow::Exit;
                        return;
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
                {
                    let canvas = skia.canvas();
                    canvas.clear(Color4f::new(0.39, 0.39, 0.39, 1.0));
                    draw(&slideshow[index], canvas, &font_mgr);
                }
                skia.flush();
                windowed_context.swap_buffers().unwrap();
            }

            _ => (),
        }
        let frame_duration = Duration::from_secs_f64(1.0 / opts.fps);

        if frame_start - previous_frame_start > frame_duration {
            if drawing {
                let canvas = skia.canvas();
                canvas.clear(Color4f::new(0.39, 0.39, 0.39, 1.0));
                drawing = slideshow[index].step();
                draw(&slideshow[index], canvas, &font_mgr);
                skia.flush();
                windowed_context.swap_buffers().unwrap();
            }
            previous_frame_start = frame_start;
        }

        *control_flow = ControlFlow::WaitUntil(previous_frame_start + frame_duration)
    });
}

pub fn draw<I: Iterator<Item = f64>, O: Iterator<Item = f32>>(
    slide: &Slide<I, O>,
    canvas: &mut Canvas,
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
                    built.paint(canvas, (obj.position.left as f32, obj.position.top as f32));
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
