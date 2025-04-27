use crate::{
    game_action::InputState, game_state::GameStateReader, BotLogic, Error,
    FishingUI, GameAction, GameState, KeyboardDisplay, PathfindingUI,
    PlayerStats, RunningLog, TuiDrawRate, X11Handler,
};

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

    /// An object that can generate and update a `GameState` object,
    /// to reflect the current state of the game.
    game_state_reader: GameStateReader,

    /// Previous frame's per-widget timers
    widget_timing_stats: Vec<WidgetTimingStatistics>,

    input_state: InputState,

    x11_handler: X11Handler,

    stardew_window: x11rb::protocol::xproto::Window,

    // Interactions owned by the top-level UI
    should_exit: bool,
    keystrokes: KeySequence,
    most_recent_unknown_key_sequence: Option<std::time::Instant>,
}

struct TuiBuffers {
    running_log: RunningLog,
    draw_rate: TuiDrawRate,
    fishing: FishingUI,
    player_stats: PlayerStats,
    pathfinding: PathfindingUI,
    keyboard: KeyboardDisplay,
}

#[allow(unused)]
#[derive(Clone, Default)]
pub(crate) struct FrameTimingStatistics {
    /// The amount of time intended to be used for each frame.
    /// (e.g. 33.3 ms for a 30 FPS interface)
    pub target_time_per_frame: std::time::Duration,

    /// The amount of sleep requested after completion of the previous
    /// frame.  This is chosen to have `main_loop_duration` and
    /// `sleep_requested` add up to `target_time_per_draw`.
    pub sleep_requested: std::time::Duration,

    /// The amount of sleep that actually occurred after the previous
    /// frame.  May be less than `sleep_requested` if user input was
    /// received, or if the OS wakes the thread early, or if the
    /// OS-dependent result of `std::time::Instant::now()` doesn't
    /// have enough granularity.
    pub sleep_actual: std::time::Duration,

    /// The time required to handle user input, if any.
    pub handle_input: std::time::Duration,

    /// The time required to update any values that are read out for
    /// each frame.
    pub update_per_frame_values: std::time::Duration,

    /// The time required to make a new decision based on the current
    /// state of the game.
    pub update_bot_logic: std::time::Duration,

    /// The time required to process per-frame updates.
    pub periodic_update: std::time::Duration,

    /// The time required to re-draw the GUI
    pub draw: std::time::Duration,

    /// The total processing time used by the frame, excluding any
    /// time spent sleeping.
    pub main_loop_active: std::time::Duration,

    /// The total amount of time spent in the frame, including both
    /// processing and sleeping.
    pub total_frame_time: std::time::Duration,
}

pub(crate) struct WidgetTimingStatistics {
    pub step: &'static str,
    pub widget_name: String,
    pub duration: std::time::Duration,
}

impl TuiBuffers {
    fn new() -> Self {
        Self {
            running_log: RunningLog::new(100),
            draw_rate: TuiDrawRate::new(),
            fishing: FishingUI::new(),
            player_stats: PlayerStats::new(),
            pathfinding: PathfindingUI,
            keyboard: KeyboardDisplay,
        }
    }

    fn buffer_list(&mut self) -> Vec<Box<&mut dyn WidgetWindow<Error>>> {
        vec![
            Box::new(&mut self.running_log),
            Box::new(&mut self.draw_rate),
            Box::new(&mut self.fishing),
            Box::new(&mut self.player_stats),
            Box::new(&mut self.pathfinding),
            Box::new(&mut self.keyboard),
        ]
    }
}

impl StardewBot {
    pub fn new() -> Result<Self, Error> {
        let pid = stardew_valley_pid()?;
        let reader = MemoryReader::new(pid)?;

        let mut tui_globals = TuiGlobals::new(reader);

        let game_state_reader =
            GameState::build_reader(tui_globals.cached_reader())?;
        let game_state =
            game_state_reader.read_full_state(tui_globals.cached_reader())?;

        tui_globals.insert(game_state);

        tui_globals.insert(BotLogic::new());

        let x11_handler = X11Handler::new()?;

        let stardew_window =
            x11_handler.find_window_blocking("Stardew Valley")?;

        let mut buffers = TuiBuffers::new();

        buffers
            .running_log
            .add_log(format!("SD window: {stardew_window:?}"));

        let mut layout = DynamicLayout::new();
        // FPS and %active in top row
        layout.split_vertically(Some(3), None);
        layout.switch_to_buffer(1);
        layout.cycle_next();

        // Placeholder for main window
        layout.split_horizontally(None, Some(45));
        layout.cycle_next();

        // Player states in top-right
        layout.split_vertically(Some(10), None);
        layout.switch_to_buffer(3);
        layout.cycle_next();

        // Keyboard outputs in center-right
        layout.split_vertically(Some(10), None);
        layout.switch_to_buffer(5);
        layout.cycle_next();

        // Log messages in bottom-right
        layout.switch_to_buffer(0);
        layout.cycle_next();

        // Cycle back to the main window, and display something there.
        layout.cycle_next();
        // layout.switch_to_buffer(2);
        layout.switch_to_buffer(4);

        Ok(Self {
            tui_globals,
            layout,
            buffers,
            game_state_reader,
            widget_timing_stats: Vec::new(),
            input_state: InputState::default(),
            x11_handler,
            should_exit: false,
            keystrokes: KeySequence::default(),
            stardew_window,
            most_recent_unknown_key_sequence: None,
        })
    }

    pub fn run(&mut self) -> Result<(), Error> {
        use crossterm::event;

        let mut context = TerminalContext::new()?;
        let handler = SigintHandler::new();

        let target_fps = 120;
        let target_time_per_frame =
            std::time::Duration::from_secs(1) / target_fps;

        let mut timing_stats = FrameTimingStatistics::default();
        let mut event_poll_result = false;

        loop {
            let main_loop_start = std::time::Instant::now();

            // The `event_poll_result` is set at the end of each loop,
            // indicating whether the next loop has user input to
            // process.
            //
            // Not sure where the best place is to split the loop
            // between successive frames.  The current split requires
            // `event_poll_result` to be propagated forward.  However,
            // it makes for an easier interpretation of
            // `FrameTimingStastics`, as all timings are local to the
            // body of the main loop.
            if event_poll_result {
                let event_received = event::read()?;
                self.handle_event(event_received);
            }
            let finished_handle_input = std::time::Instant::now();

            self.update_game_state();
            let finished_updating_per_frame_values = std::time::Instant::now();

            self.update_bot_logic();
            let finished_updating_bot_logic = std::time::Instant::now();

            let mut side_effects = WidgetSideEffects::default();
            side_effects.broadcast(timing_stats.clone());

            for timing_stat in self.take_widget_timing_stats() {
                side_effects.broadcast(timing_stat);
            }
            self.periodic_update(side_effects);

            let finished_periodic_update = std::time::Instant::now();
            context.draw(|frame| self.draw(frame))?;

            if handler.received() || self.should_exit {
                break;
            }

            let main_loop_becomes_inactive = std::time::Instant::now();
            let main_loop_active = main_loop_becomes_inactive - main_loop_start;

            let sleep_requested =
                target_time_per_frame.saturating_sub(main_loop_active);
            event_poll_result = event::poll(sleep_requested)?;
            let finished_sleep = std::time::Instant::now();

            timing_stats = FrameTimingStatistics {
                target_time_per_frame,
                sleep_requested,
                sleep_actual: finished_sleep - main_loop_becomes_inactive,
                handle_input: finished_handle_input - main_loop_start,
                update_per_frame_values: finished_updating_per_frame_values
                    - finished_handle_input,
                update_bot_logic: finished_updating_bot_logic
                    - finished_updating_per_frame_values,
                periodic_update: finished_periodic_update
                    - finished_updating_bot_logic,
                draw: main_loop_becomes_inactive - finished_periodic_update,
                main_loop_active,
                total_frame_time: finished_sleep - main_loop_start,
            };
        }
        Ok(())
    }

    fn take_widget_timing_stats(&mut self) -> Vec<WidgetTimingStatistics> {
        let mut out = Vec::new();
        std::mem::swap(&mut out, &mut self.widget_timing_stats);
        out
    }

    pub fn draw(&mut self, frame: &mut Frame) {
        let mut buffers = self.buffers.buffer_list();
        let mut layout = self
            .layout
            .drawable(&mut buffers, &self.tui_globals)
            .timing_callback(|name, duration| {
                self.widget_timing_stats.push(WidgetTimingStatistics {
                    step: "Render",
                    widget_name: name.into(),
                    duration,
                });
            });

        frame.render_widget(&mut layout, frame.area());
    }

    pub fn update_game_state(&mut self) {
        let res_delta = self
            .game_state_reader
            .read_delta_state(self.tui_globals.cached_reader());

        match res_delta {
            Ok(delta) => {
                self.tui_globals
                    .get_mut::<GameState>()
                    .expect("Globals should always contain a GameState")
                    .apply_delta(delta);
            }
            Err(err) => {
                self.buffers.running_log.add_log(format!("Error: {err}"));
            }
        }
    }

    pub fn update_bot_logic(&mut self) {
        // TODO: Either extend anymap to allow mutable access to
        // multiple keys, or add a generic parameter to WidgetWindow
        // indicating what type of global values are expected.
        let mut bot_logic = self
            .tui_globals
            .take::<BotLogic>()
            .expect("Globals should always contain a BotLogic");

        let game_state = self
            .tui_globals
            .get::<GameState>()
            .expect("Globals should always contain a GameState");

        let res = bot_logic.update(game_state).and_then(|opt_action| {
            if let Some(action) = opt_action {
                self.apply_game_actions(std::iter::once(action))
            } else {
                Ok(())
            }
        });
        if let Err(err) = res {
            self.buffers.running_log.add_log(format!("Error: {err}"));
        }

        self.tui_globals.insert(bot_logic);
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
                        let now = std::time::Instant::now();
                        let raise_err = self
                            .most_recent_unknown_key_sequence
                            .map(|prev| {
                                now - prev > std::time::Duration::from_secs(1)
                            })
                            .unwrap_or(true);
                        if raise_err {
                            self.most_recent_unknown_key_sequence = Some(now);
                            return Err(Error::UnknownKeySequence(
                                std::mem::take(&mut self.keystrokes),
                            ));
                        } else {
                            self.keystrokes.clear();
                        }
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

    pub fn periodic_update(&mut self, mut side_effects: WidgetSideEffects) {
        let mut buffer_list = self.buffers.buffer_list();

        if let Err(err) = buffer_list.iter_mut().try_for_each(|buffer| {
            let before = std::time::Instant::now();
            let res =
                buffer.periodic_update(&self.tui_globals, &mut side_effects);
            let after = std::time::Instant::now();

            self.widget_timing_stats.push(WidgetTimingStatistics {
                step: "Periodic Update",
                widget_name: buffer.title().to_string(),
                duration: after - before,
            });

            res
        }) {
            side_effects.add_log(format!("Error: {err}"));
        }

        self.apply_side_effects(side_effects);
    }

    fn apply_side_effects(&mut self, mut side_effects: WidgetSideEffects) {
        if let Err(err) = self.try_apply_side_effects(&mut side_effects) {
            side_effects.add_log(format!("Error: {err}"));
        }

        // Re-process the log messages last, since handling other side
        // effects may result in additional log messages.
        self.buffers
            .running_log
            .apply_side_effects(&self.tui_globals, &mut side_effects)
            .unwrap();
    }

    fn try_apply_side_effects(
        &mut self,
        side_effects: &mut WidgetSideEffects,
    ) -> Result<(), Error> {
        self.buffers
            .buffer_list()
            .iter_mut()
            .try_for_each(|buffer| {
                buffer.apply_side_effects(&self.tui_globals, side_effects)
            })?;

        self.apply_game_actions(side_effects.into_iter::<GameAction>())
    }

    fn apply_game_actions(
        &mut self,
        actions: impl IntoIterator<Item = GameAction>,
    ) -> Result<(), Error> {
        let active_window = self.x11_handler.query_active_window()?;
        if active_window == self.stardew_window {
            actions.into_iter().try_for_each(|action| {
                action.apply(
                    &mut self.input_state,
                    &mut self.x11_handler,
                    self.stardew_window,
                )
            })?;
        }

        Ok(())
    }
}
