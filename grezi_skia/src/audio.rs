use kira::manager::backend::{Backend, Renderer};
use sdl2::{
    audio::{AudioCallback, AudioDevice, AudioSpec, AudioSpecDesired},
    AudioSubsystem,
};

pub struct Sdl2Backend {
    settings: Sdl2Settings,
    device: Option<AudioDevice<Sdl2Player>>,
}

unsafe impl Sync for Sdl2Player {}
unsafe impl Send for Sdl2Player {}
unsafe impl Sync for Sdl2Backend {}
unsafe impl Send for Sdl2Backend {}

struct Sdl2Player {
    renderer: Renderer,
    spec: AudioSpec,
}

impl AudioCallback for Sdl2Player {
    type Channel = f32;

    fn callback(&mut self, data: &mut [Self::Channel]) {
        self.renderer.on_start_processing();
        for frame in data.chunks_exact_mut(self.spec.channels as usize) {
            let out = self.renderer.process();
            if self.spec.channels == 1 {
                frame[0] = (out.left + out.right) / 2.0;
            } else {
                frame[0] = out.left;
                frame[1] = out.right;
            }
        }
    }
}

pub struct Sdl2Settings {
    pub subsystem: AudioSubsystem,
    pub spec: AudioSpecDesired,
}

impl Backend for Sdl2Backend {
    type Settings = Sdl2Settings;
    type Error = String;

    fn setup(settings: Self::Settings) -> Result<(Self, u32), Self::Error> {
        let rate = settings.spec.freq.unwrap_or(44100) as u32;
        Ok((
            Self {
                settings,
                device: None,
            },
            rate,
        ))
    }
    fn start(&mut self, mut renderer: Renderer) -> Result<(), Self::Error> {
        let device = self
            .settings
            .subsystem
            .open_playback(None, &self.settings.spec, |spec| {
                if self.settings.spec.freq.unwrap_or(44100) != spec.freq {
                    renderer.on_change_sample_rate(spec.freq as u32);
                }
                Sdl2Player { renderer, spec }
            })?;

        device.resume();

        self.device = Some(device);

        Ok(())
    }
}
