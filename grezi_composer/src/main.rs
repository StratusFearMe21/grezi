use std::{borrow::Cow, mem::MaybeUninit};

use anyhow::{bail, Context};
use grezi::AHashMap;
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

static mut FONT_CACHE: MaybeUninit<FontCollection> = MaybeUninit::uninit();

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
    /// Draw a circle to the screen
    Point,
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
            ObjectType::Point => unimplemented!(),
        }
    }
}

fn main() -> anyhow::Result<()> {
    unsafe { FONT_CACHE.write(FontCollection::new()) }
        .set_default_font_manager(FontMgr::default(), None);

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

    let video_ctx = sdl_ctx.video().map_err(|e| anyhow::anyhow!(e))?;
    let window = video_ctx
        .window("Grezi", 960, 640)
        .allow_highdpi()
        .resizable()
        .build()?;

    let window_size = window.vulkan_drawable_size();
    let mut extents = RafxExtents2D {
        width: window_size.0,
        height: window_size.1,
    };
    let mut skia = skulpin::RendererBuilder::new()
        .coordinate_system(skulpin::CoordinateSystem::Physical)
        // I am an actual god, FPS is so high that turning this off produces inf FPS.
        // Thus crashing Vulkan!
        .vsync_enabled(true)
        .build(&window, extents)?;
    let bg = Color4f::new(0.39, 0.39, 0.39, 1.0);
    let mut mouse_pos = (0, 0);
    'running: loop {
        assert!(event_pump.is_event_enabled(sdl2::event::EventType::ControllerButtonDown));

        for event in event_pump.poll_iter() {
            match event {
                Event::Quit { .. } => break 'running,
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
                Event::KeyDown {
                    keycode: Some(Keycode::Q) | Some(Keycode::Escape),
                    ..
                } => break 'running,
                Event::MouseMotion { x, y, .. } => {
                    mouse_pos = (x, y);
                }

                _ => {}
            }
        }

        skia.draw(extents, 1.0, |canvas, _coordinate_system_helper| {
            canvas.clear(bg);
            let mut paint = Paint::default();
            paint.set_argb(255, 255, 0, 0);
            canvas.draw_line(
                (mouse_pos.0, 0),
                (mouse_pos.0, extents.height as i32),
                &paint,
            );
            canvas.draw_line(
                (0, mouse_pos.1),
                (extents.width as i32, mouse_pos.1),
                &paint,
            );
        })?;
        std::thread::yield_now();
    }
    Ok(())
}
