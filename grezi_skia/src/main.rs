use std::{
    borrow::Cow,
    mem::MaybeUninit,
    path::PathBuf,
    time::{Duration, Instant},
};

use anyhow::{bail, Context};
use ariadne::Label;
use audio::{Sdl2Backend, Sdl2Settings};
use grezi::{layout::Rect, AHashMap, Functions, Slide};
use kira::{
    manager::{AudioManager, AudioManagerSettings},
    sound::{
        static_sound::{StaticSoundData, StaticSoundHandle, StaticSoundSettings},
        SoundData,
    },
    track::TrackBuilder,
    tween::Tween,
};
use rayon::iter::{IntoParallelRefMutIterator, ParallelIterator};
use sdl2::{
    event::{Event, WindowEvent},
    keyboard::Keycode,
};
use skulpin::{
    rafx::api::RafxExtents2D,
    skia_bindings::{skia_textlayout_ParagraphStyle, skia_textlayout_TextStyle},
    skia_safe::{
        textlayout::{FontCollection, ParagraphBuilder, ParagraphStyle, TextAlign, TextStyle},
        wrapper::PointerWrapper,
        Color4f, Font, FontMgr, FontStyle, Handle, Paint, RefHandle, Typeface,
    },
};
use structopt::StructOpt;
static mut FONT_CACHE: MaybeUninit<FontCollection> = MaybeUninit::uninit();

mod audio;

#[derive(StructOpt)]
struct Opts {
    #[structopt(short, long, default_value = "0.5")]
    /// The delay between slides in seconds
    delay: f32,
    /// The file for Grezi to open
    file: PathBuf,
}

#[derive(Debug, Clone)]
/// The type of the objcet in question
pub enum ObjectType {
    /// Draw text to the screen
    Text {
        style: Handle<skia_textlayout_TextStyle>,
        p_style: *mut skia_textlayout_ParagraphStyle,
        value: *mut [u8],
        max_width: f32,
    },
    Rect,
    Image {
        img: skulpin::skia_safe::Image,
        dont_resize: bool,
    },
}

impl grezi::Object for ObjectType {
    type Error = anyhow::Error;

    fn construct(
        _: &str,
        type_: String,
        mut values: AHashMap<String, String>,
        registers: &AHashMap<Cow<str>, Cow<str>>,
    ) -> anyhow::Result<Self> {
        match type_.as_str() {
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
                    Some("center") | Some("Center") => TextAlign::Center,
                    _ => TextAlign::Right,
                };
                let font_size = values.remove("font_size").map_or_else(
                    || unsafe { registers.get("FONT_SIZE").unwrap_unchecked().parse::<f32>() },
                    |k| k.parse(),
                )?;
                let typeface =
                    Typeface::new(&font_family, FontStyle::normal()).context("Invalid typeface")?;
                let mut text_style = TextStyle::new();
                let font = Font::new(&typeface, font_size);
                text_style
                    .set_font_size(font_size)
                    .set_typeface(typeface)
                    .set_font_families(&[font_family]);

                let max_width = value
                    .lines()
                    .map(|f| font.measure_str(f, None).1.width())
                    .max_by(|f, g| f.partial_cmp(g).unwrap())
                    .unwrap()
                    + 10.0;
                let mut p_style = ParagraphStyle::new();
                p_style
                    .set_text_align(alignment)
                    .set_text_style(&text_style);
                Ok(ObjectType::Text {
                    style: text_style,
                    p_style: p_style.unwrap(),
                    value: value.into_bytes().leak() as *mut [u8],
                    max_width,
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
                    Some("center") | Some("Center") => TextAlign::Center,
                    _ => TextAlign::Right,
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
                let typeface =
                    Typeface::new(&font_family, FontStyle::normal()).context("Invalid typeface")?;
                let mut text_style = TextStyle::new();
                let font = Font::new(&typeface, font_size);
                text_style
                    .set_font_size(font_size)
                    .set_typeface(typeface)
                    .set_font_families(&[font_family]);
                let max_width = value
                    .lines()
                    .map(|f| font.measure_str(f, None).1.width())
                    .max_by(|f, g| f.partial_cmp(g).unwrap())
                    .unwrap()
                    + 10.0;
                let mut p_style = ParagraphStyle::new();
                p_style
                    .set_text_align(alignment)
                    .set_text_style(&text_style);
                Ok(ObjectType::Text {
                    style: text_style,
                    p_style: p_style.unwrap(),
                    value: value.into_bytes().leak() as *mut [u8],
                    max_width,
                })
            }
            "Rect" | "rect" | "Rectangle" | "rectangle" => Ok(ObjectType::Rect),
            /*
            "Image" | "image" | "Picture" | "picture" => {
            let image = skulpin::skia_safe::Bitmap::new();
            Ok(ObjectType::Image {
            data: skulpin::skia_safe::Bitmap::
            })
            }
            */
            "Image" | "image" | "img" | "IMG" | "pic" | "Pic" | "Picture" | "picture" => {
                let dont_resize = values
                    .get("dont_resize")
                    .map_or_else(|| Ok(false), |k| k.parse())?;
                let image = unsafe {
                    memmap::Mmap::map(&std::fs::File::open(
                        values.remove("path").context("Images require a path")?,
                    )?)?
                };
                Ok(ObjectType::Image {
                    img: skulpin::skia_safe::image::Image::from_encoded(
                        skulpin::skia_safe::Data::new_copy(&image),
                    )
                    .context("Invalid image passed")?,
                    dont_resize,
                })
            }
            e => {
                bail!("Object cannot have type {}", e)
            }
        }
    }

    fn bounds(&mut self, w: f32, h: f32) -> anyhow::Result<(f32, f32)> {
        match self {
            ObjectType::Text {
                ref p_style,
                ref value,
                max_width,
                ..
            } => {
                if *max_width > w {
                    *max_width = w
                }
                let p_style = unsafe { RefHandle::wrap(*p_style).unwrap_unchecked() };
                let mut paragraph =
                    ParagraphBuilder::new(&p_style, unsafe { FONT_CACHE.assume_init_ref() });
                paragraph.add_text(unsafe { std::str::from_utf8_unchecked(&**value) });
                let mut built = paragraph.build();
                built.layout(*max_width);
                let width = built.max_width();
                let height = built.height();
                if height > h {
                    bail!("This text overflows the slide. text should be less than {h} pixels high")
                } else {
                    p_style.unwrap();
                    Ok((width, height))
                }
            }
            ObjectType::Image {
                img, dont_resize, ..
            } => {
                let width = img.width() as f32;
                let height = img.height() as f32;
                let nw = if width > w || !*dont_resize { w } else { width };
                let nh = if height > h || !*dont_resize {
                    h
                } else {
                    height
                };
                let ds = nw.min(nh);
                let ss = width.max(height);

                let wh = (ds * width / ss, ds * height / ss);

                let mut canvas =
                    skulpin::skia_safe::Surface::new_raster_n32_premul((wh.0 as i32, wh.1 as i32))
                        .context("Invalid canvas")?;

                canvas.canvas().draw_image_rect(
                    &img,
                    None,
                    skulpin::skia_safe::Rect {
                        left: 0.0,
                        top: 0.0,
                        right: wh.0,
                        bottom: wh.1,
                    },
                    &Paint::default(),
                );

                *img = canvas.image_snapshot();

                Ok(wh)
            }
            ObjectType::Rect => Ok((w, h)),
        }
    }
}

enum SkiaFunctions {
    Play(StaticSoundData),
}

impl Functions for SkiaFunctions {
    type Error = anyhow::Error;

    fn construct(name: String, arg: String) -> Result<Self, Self::Error> {
        match name.as_str() {
            "PlayFile" | "play_file" | "Play" | "play" => Ok(SkiaFunctions::Play(
                StaticSoundData::from_file(arg, StaticSoundSettings::default())?,
            )),
            n => bail!("Invalid function {}", n),
        }
    }
}

enum TransitionDirection {
    Forward,
    Backward,
}

fn main() -> anyhow::Result<()> {
    rayon::ThreadPoolBuilder::new().build_global()?;
    let opts = Opts::from_args();
    let map = unsafe { memmap::Mmap::map(&std::fs::File::open(opts.file)?)? };
    let size = Rect {
        left: 0.0,
        top: 0.0,
        // windowed_context.window().inner_size().width as u16
        right: 1920.0,
        // dbg!(windowed_context.window().inner_size().height) as u16
        bottom: 1080.0,
    };
    unsafe { FONT_CACHE.write(FontCollection::new()) }
        .set_default_font_manager(FontMgr::default(), None);
    let mut slideshow: Vec<Slide<ObjectType, SkiaFunctions>> =
        match grezi::file_to_slideshow(&map, size, 1.0) {
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

    let sdl_ctx = sdl2::init().map_err(|e| anyhow::anyhow!(e))?;
    let game_controller_subsystem = sdl_ctx.game_controller().map_err(|e| anyhow::anyhow!(e))?;

    let available = game_controller_subsystem
        .num_joysticks()
        .map_err(|e| anyhow::anyhow!("can't enumerate joysticks: {}", e))?;

    // Iterate over all available joysticks and look for game controllers.
    let mut controllers = [None, None, None, None, None, None, None, None];
    (0..available).for_each(|id| {
        if game_controller_subsystem.is_game_controller(id) {
            match game_controller_subsystem.open(id) {
                Ok(c) => {
                    // We managed to find and open a game controller,
                    // exit the loop
                    controllers[id as usize] = Some(c);
                }
                Err(_) => {}
            }
        }
    });
    let mut event_pump = sdl_ctx.event_pump().map_err(|e| anyhow::anyhow!(e))?;
    let audio_ctx = sdl_ctx.audio().map_err(|e| anyhow::anyhow!(e))?;
    let mut kira_ctx: AudioManager<Sdl2Backend> = AudioManager::new(AudioManagerSettings {
        backend_settings: Sdl2Settings {
            subsystem: audio_ctx,
            spec: sdl2::audio::AudioSpecDesired {
                freq: None,
                channels: Some(2),
                samples: None,
            },
        },
        capacities: kira::manager::Capacities::default(),
        main_track_builder: TrackBuilder::default(),
    })
    .unwrap();
    let mut sound_vec: Vec<StaticSoundHandle> = Vec::with_capacity(2);

    let video_ctx = sdl_ctx.video().map_err(|e| anyhow::anyhow!(e))?;
    let window = video_ctx
        .window("Grezi", 1920, 1080)
        .fullscreen()
        .allow_highdpi()
        .vulkan()
        .resizable()
        .build()?;

    let window_size = window.vulkan_drawable_size();
    let mut extents = RafxExtents2D {
        width: window_size.0,
        height: window_size.1,
    };
    let visible_range = skulpin::skia_safe::Rect {
        left: 0.0,
        right: 1920.0,
        top: 0.0,
        bottom: 1080.0,
    };
    let mut skia = skulpin::Renderer::new(
        &window,
        extents,
        skulpin::CoordinateSystem::VisibleRange(
            visible_range,
            skulpin::skia_safe::matrix::ScaleToFit::Center,
        ),
        true,
    )?;
    let mut index = 0;
    let mut drawing = true;
    #[cfg(debug_assertions)]
    let fps_font = Font::new(
        Typeface::new("Helvetica", FontStyle::default()).context("Invalid typeface")?,
        10.0,
    );
    let mut paint = Paint::new(Color4f::new(1.0, 1.0, 1.0, 1.0), None);
    let mut bg = Color4f::new(0.39, 0.39, 0.39, 1.0);
    let mut previous_frame_start = Instant::now();
    let slide = unsafe { slideshow.get_unchecked(index) };
    let mut transition_direction = TransitionDirection::Forward;
    sound_vec.par_iter_mut().try_for_each(|f| {
        f.stop(Tween {
            start_time: kira::StartTime::Immediate,
            easing: kira::tween::Easing::Linear,
            duration: Duration::from_secs(2),
        })
    })?;
    slide.calls.iter().try_for_each(|i| match i {
        SkiaFunctions::Play(handle) => {
            sound_vec.push(kira_ctx.play(handle.clone())?);
            Ok::<(), kira::manager::error::PlaySoundError<<StaticSoundData as SoundData>::Error>>(
                (),
            )
        }
    })?;
    for i in slide.calls.iter() {
        match i {
            SkiaFunctions::Play(handle) => {
                sound_vec.push(kira_ctx.play(handle.clone())?);
            }
        }
    }
    'running: loop {
        let frame_start = Instant::now();

        for event in event_pump.poll_iter() {
            match event {
                Event::Window {
                    win_event: WindowEvent::Resized(_, _) | WindowEvent::SizeChanged(_, _),
                    ..
                } => {
                    let new_size = window.vulkan_drawable_size();
                    extents = RafxExtents2D {
                        width: new_size.0,
                        height: new_size.1,
                    };
                }
                Event::Quit { .. } | Event::KeyDown {
                    keycode: Some(Keycode::Q) | Some(Keycode::Escape),
                    ..
                }
                | Event::ControllerButtonDown {
                    button: sdl2::controller::Button::Guide,
                    ..
                } => {
                    break 'running;
                }
                Event::KeyDown {
                    keycode: Some(Keycode::Right)
                        // These are bindings for the 8bitdo Zero 2
                        // No, they don't make sense
                        //
                        // Right Shoulder button
                        | Some(Keycode::M)
                        // Right D-Pad
                        | Some(Keycode::F)
                        // Up D-Pad
                        | Some(Keycode::C)
                        // Start button
                        | Some(Keycode::O)
                        // "A" Button
                        | Some(Keycode::G),
                        ..
                } | Event::MouseButtonDown { mouse_btn: sdl2::mouse::MouseButton::Left, .. } | Event::ControllerButtonDown { button: sdl2::controller::Button::A
                    | sdl2::controller::Button::Start
                        | sdl2::controller::Button::DPadUp
                        | sdl2::controller::Button::DPadRight
                        | sdl2::controller::Button::RightShoulder
                        | sdl2::controller::Button::RightStick, .. } => {
                            if index != slideshow.len() - 1 {
                                if let TransitionDirection::Forward = transition_direction {
                                    index += 1;
                                    unsafe {
                                        slideshow.get_unchecked_mut(index).step = 0.0;
                                    }
                                } else {
                                    transition_direction = TransitionDirection::Forward;

                                }
                                drawing = true;
                                let slide = unsafe { slideshow.get_unchecked(index) };
                                for i in slide.calls.iter() {
                                    match i {
                                        SkiaFunctions::Play(handle) => {
                                            kira_ctx.play(handle.clone())?;
                                        }
                                    }
                                }
                                previous_frame_start = frame_start;
                            }
                        }
                Event::KeyDown { keycode: Some(Keycode::Left)
                    // Left Shoulder button
                    | Some(Keycode::K)
                        // Left D-Pad
                        | Some(Keycode::E)
                        // Down D-Pad
                        | Some(Keycode::D)
                        // Select button
                        | Some(Keycode::N)
                        // "B" Button
                        | Some(Keycode::J), .. } | Event::MouseButtonDown { mouse_btn: sdl2::mouse::MouseButton::Right, .. } | Event::ControllerButtonDown { button: sdl2::controller::Button::B
                            | sdl2::controller::Button::Back
                                | sdl2::controller::Button::DPadDown
                                | sdl2::controller::Button::DPadLeft
                                | sdl2::controller::Button::LeftShoulder
                                | sdl2::controller::Button::LeftStick, .. } => {
                                    if index != 0 {
                                        if let TransitionDirection::Backward = transition_direction {
                                            index -= 1;
                                            unsafe {
                                                slideshow.get_unchecked_mut(index).step = 1.0;
                                            }
                                        } else {
                                            transition_direction = TransitionDirection::Backward;
                                            unsafe {
                                                slideshow.get_unchecked_mut(index - 1).step = 1.0;
                                            }
                                        }
                                        drawing = true;
                                        previous_frame_start = frame_start;
                                    }
                                }
                _ => {}
            }
        }

        if drawing {
            let slide = unsafe { slideshow.get_unchecked_mut(index) };
            match transition_direction {
                TransitionDirection::Forward => {
                    slide.step += (frame_start - previous_frame_start).as_secs_f32() / opts.delay;
                    if slide.step >= 1.0 {
                        slide.step = 1.0;
                        drawing = false;
                    }
                }
                TransitionDirection::Backward => {
                    slide.step -= (frame_start - previous_frame_start).as_secs_f32() / opts.delay;
                    if slide.step <= 0.0 {
                        slide.step = 0.0;
                        drawing = false;
                        index -= 1;
                        transition_direction = TransitionDirection::Forward;
                    }
                }
            }
            previous_frame_start = frame_start;
            let bg_raw = slide.step();
            bg = Color4f::new(bg_raw.x, bg_raw.y, bg_raw.z, bg_raw.w);
        }

        let slide = unsafe { slideshow.get_unchecked_mut(index) };
        skia.draw(extents, 1.0, |canvas, _coordinate_system_helper| {
            canvas.clear(bg);
            paint.set_alpha_f(1.0);
            #[cfg(debug_assertions)]
            canvas.draw_str(
                format!(
                    "{}",
                    1.0 / (frame_start - previous_frame_start).as_secs_f64()
                ),
                (10.0, 10.0),
                &fps_font,
                &paint,
            );
            for cmd in slide.cmds.iter_mut() {
                #[cfg(debug_assertions)]
                let workrect = skulpin::skia_safe::Rect::new(
                    cmd.obj.parameters.position.x,
                    cmd.obj.parameters.position.y,
                    cmd.obj.parameters.position.z,
                    cmd.obj.parameters.position.w,
                );
                match cmd.obj.obj_type {
                    ObjectType::Text {
                        ref mut style,
                        p_style,
                        value,
                        max_width,
                    } => {
                        style.set_color(
                            skulpin::skia_safe::Color4f::new(
                                1.0,
                                1.0,
                                1.0,
                                cmd.obj.parameters.opacity,
                            )
                            .to_color(),
                        );
                        let mut p_style = unsafe { RefHandle::wrap(p_style).unwrap_unchecked() };
                        p_style.set_text_style(style);
                        let mut paragraph = ParagraphBuilder::new(&p_style, unsafe {
                            FONT_CACHE.assume_init_ref()
                        });
                        paragraph.add_text(unsafe { std::str::from_utf8_unchecked(&*value) });
                        let mut built = paragraph.build();
                        built.layout(max_width);
                        built.paint(
                            canvas,
                            (cmd.obj.parameters.position.x, cmd.obj.parameters.position.y),
                        );
                        p_style.unwrap();
                    }
                    ObjectType::Image { ref img, .. } => {
                        paint.set_color4f(
                            Color4f::new(1.0, 1.0, 1.0, cmd.obj.parameters.opacity),
                            None,
                        );
                        paint.set_stroke(false);
                        canvas.draw_image_rect(
                            img,
                            None,
                            skulpin::skia_safe::Rect {
                                left: cmd.obj.parameters.position.x,
                                top: cmd.obj.parameters.position.y,
                                right: cmd.obj.parameters.position.z,
                                bottom: cmd.obj.parameters.position.w,
                            },
                            &paint,
                        );
                    }
                    ObjectType::Rect => {
                        paint.set_color4f(
                            Color4f::new(1.0, 1.0, 1.0, cmd.obj.parameters.opacity),
                            None,
                        );
                        paint.set_stroke(true);
                        paint.set_stroke_width(5.0);
                        canvas.draw_rect(
                            skulpin::skia_safe::Rect {
                                left: cmd.obj.parameters.position.x,
                                top: cmd.obj.parameters.position.y,
                                right: cmd.obj.parameters.position.z,
                                bottom: cmd.obj.parameters.position.w,
                            },
                            &paint,
                        );
                    }
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
        })?;
        std::thread::yield_now();
    }
    Ok(())
}
