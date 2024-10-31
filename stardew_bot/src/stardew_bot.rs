use crate::{Error, FishingUI, RunningLog, TuiDrawRate};

use crossterm::event::Event;
use dotnet_debugger::CachedReader;
use memory_reader::MemoryReader;
use stardew_utils::stardew_valley_pid;
use tui_utils::{
    inputs::{KeyBindingMatch, KeySequence, SigintHandler},
    widgets::DynamicLayout,
    TerminalContext, TuiGlobals, WidgetSideEffects, WidgetWindow,
};

use ratatui::Frame;

pub struct StardewBot {
    /// Information shared to all sub-windows
    tui_globals: TuiGlobals,

    /// Layout of sub-windows
    layout: DynamicLayout,

    /// Container of subwindows
    buffers: TuiBuffers,

    // Interactions owned by the top-level UI
    should_exit: bool,
    keystrokes: KeySequence,
}

struct TuiBuffers {
    running_log: RunningLog,
    draw_rate: TuiDrawRate,
    fishing: FishingUI,
}

impl TuiBuffers {
    fn new(reader: CachedReader<'_>) -> Result<Self, Error> {
        Ok(Self {
            running_log: RunningLog::new(100),
            draw_rate: TuiDrawRate::new(),
            fishing: FishingUI::new(reader)?,
        })
    }

    fn buffer_list(&mut self) -> Vec<Box<&mut dyn WidgetWindow>> {
        vec![
            Box::new(&mut self.running_log),
            Box::new(&mut self.draw_rate),
            Box::new(&mut self.fishing),
        ]
    }
}

impl StardewBot {
    pub fn new() -> Result<Self, Error> {
        let pid = stardew_valley_pid()?;
        let reader = MemoryReader::new(pid)?;

        let tui_globals = TuiGlobals::new(reader);
        let buffers = TuiBuffers::new(tui_globals.cached_reader())?;

        let mut layout = DynamicLayout::new();
        layout.split_horizontally(Some(45), None);
        layout.switch_to_buffer(2);

        Ok(Self {
            tui_globals,
            layout,
            buffers,
            should_exit: false,
            keystrokes: KeySequence::default(),
        })
    }

    pub fn run(&mut self) -> Result<(), Error> {
        use crossterm::event;

        let mut context = TerminalContext::new()?;
        let handler = SigintHandler::new();

        let target_fps = 120;
        let time_per_draw = std::time::Duration::from_secs(1) / target_fps;

        let mut event_timeout = time_per_draw;

        loop {
            let poll = event::poll(event_timeout)?;
            let main_loop_start = std::time::Instant::now();

            if poll {
                let event_received = event::read()?;
                self.handle_event(event_received);
            }
            self.periodic_update();
            context.draw(|frame| self.draw(frame))?;

            if handler.received() || self.should_exit {
                break;
            }

            let main_loop_end = std::time::Instant::now();
            let main_loop_duration = main_loop_end - main_loop_start;
            event_timeout = time_per_draw.saturating_sub(main_loop_duration);
        }
        Ok(())
    }

    pub fn draw(&mut self, frame: &mut Frame) {
        let mut buffers = self.buffers.buffer_list();
        let layout = self.layout.drawable(&mut buffers, &self.tui_globals);

        frame.render_widget(layout, frame.size());
    }

    pub fn handle_event(&mut self, event: Event) {
        if let Err(err) = self.try_handle_event(event) {
            self.buffers.running_log.add_log(format!("Error: {err}"));
        }
    }

    fn try_handle_event(&mut self, event: Event) -> Result<(), Error> {
        match event {
            Event::Key(key) => {
                self.keystrokes.push(key);

                match self.apply_key_binding()? {
                    KeyBindingMatch::Full => {
                        self.keystrokes.clear();
                    }
                    KeyBindingMatch::Partial => {}
                    KeyBindingMatch::Mismatch => {
                        return Err(Error::UnknownKeySequence(std::mem::take(
                            &mut self.keystrokes,
                        )));
                    }
                }
            }
            Event::Mouse(mouse) => self.layout.handle_mouse_event(mouse),
            _ => {}
        }

        Ok(())
    }

    fn apply_key_binding(&mut self) -> Result<KeyBindingMatch, Error> {
        let keystrokes = &self.keystrokes;

        let mut side_effects = WidgetSideEffects::default();

        let result = KeyBindingMatch::Mismatch
            .or_try_binding("C-c", keystrokes, || {
                self.should_exit = true;
            })
            .or_else(|| {
                // The layout will forward the key bindings to either
                // the active window, or to a buffer selection window
                // if present.
                self.layout.apply_key_binding(
                    &keystrokes,
                    &self.tui_globals,
                    &mut side_effects,
                    &mut self.buffers.buffer_list(),
                )
            });

        self.apply_side_effects(side_effects);

        Ok(result)
    }

    pub fn periodic_update(&mut self) {
        let mut buffer_list = self.buffers.buffer_list();

        let mut side_effects = WidgetSideEffects::default();

        let res = buffer_list.iter_mut().try_for_each(|buffer| {
            buffer.periodic_update(&self.tui_globals, &mut side_effects)
        });

        if let Err(err) = res {
            side_effects.add_log(format!("Error: {err}"));
        }

        self.apply_side_effects(side_effects);
    }

    fn apply_side_effects(&mut self, mut side_effects: WidgetSideEffects) {
        if let Err(err) =
            self.buffers
                .buffer_list()
                .iter_mut()
                .try_for_each(|buffer| {
                    buffer.apply_side_effects(
                        &self.tui_globals,
                        &mut side_effects,
                    )
                })
        {
            side_effects.add_log(format!("Error: {err}"));
        }

        // Re-process the log messages last, since handling other side
        // effects may result in additional log messages.
        self.buffers
            .running_log
            .apply_side_effects(&self.tui_globals, &mut side_effects)
            .unwrap();
    }
}
