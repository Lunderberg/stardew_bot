use crate::{Error, RunningLog};

use crossterm::event::Event;
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
}

impl TuiBuffers {
    fn new() -> Self {
        Self {
            running_log: RunningLog::new(100),
        }
    }

    fn buffer_list(&mut self) -> Vec<Box<&mut dyn WidgetWindow>> {
        vec![Box::new(&mut self.running_log)]
    }
}

impl StardewBot {
    pub fn new() -> Result<Self, Error> {
        let pid = stardew_valley_pid()?;
        let reader = MemoryReader::new(pid)?;

        Ok(Self {
            tui_globals: TuiGlobals::new(reader),
            layout: DynamicLayout::new(),
            buffers: TuiBuffers::new(),
            should_exit: false,
            keystrokes: KeySequence::default(),
        })
    }

    pub fn run(&mut self) -> Result<(), Error> {
        use crossterm::event;

        let mut context = TerminalContext::new()?;
        let handler = SigintHandler::new();

        loop {
            context.draw(|frame| self.draw(frame))?;

            let timeout = std::time::Duration::from_millis(30);
            let poll = event::poll(timeout)?;

            if poll {
                let event_received = event::read()?;
                self.handle_event(event_received);
            }
            self.periodic_update();

            if handler.received() || self.should_exit {
                break;
            }
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
