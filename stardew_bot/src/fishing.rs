use std::ops::Range;

use itertools::Itertools as _;
use ratatui::{
    layout::Constraint,
    style::{Color, Modifier, Style, Stylize as _},
    symbols::Marker,
    widgets::{
        Axis, Block, Chart, Dataset, GraphType, Row, StatefulWidget, Table,
        TableState, Widget,
    },
};
use tui_utils::{extensions::SplitRect as _, WidgetWindow};

use crate::{game_state::FishingState, Error, GameAction, GameState};

pub struct FishingUI {
    table_state: TableState,

    history: Vec<FishHistory>,
}

struct FishHistory {
    tick: i32,
    bar_position: Range<f32>,
    bar_velocity: f32,
    fish_in_bar: bool,
    fish_position: f32,
    fish_velocity: f32,
    fish_target_position: f32,
    fish_difficulty: f32,
}

impl FishingUI {
    pub fn new() -> Self {
        Self {
            table_state: TableState::new(),
            history: Vec::new(),
        }
    }

    fn current_state(
        &self,
        fishing_state: &FishingState,
    ) -> Option<FishHistory> {
        let minigame_in_progress = fishing_state.minigame_in_progress;
        let tick = fishing_state.game_tick;

        let is_new_tick = self
            .history
            .last()
            .map(|prev| prev.tick != tick)
            .unwrap_or(true);

        (minigame_in_progress && is_new_tick).then(|| FishHistory {
            tick,
            bar_position: fishing_state.bar_position
                ..fishing_state.bar_position
                    + (fishing_state.bar_height as f32),
            bar_velocity: fishing_state.bar_velocity,
            fish_in_bar: fishing_state.bobber_in_bar,
            fish_position: fishing_state.fish_position,
            fish_velocity: fishing_state.fish_velocity,
            fish_target_position: fishing_state.fish_target_position,
            fish_difficulty: fishing_state.fish_difficulty,
        })
    }
}

impl FishHistory {
    fn predicted_positions(&self) -> impl Fn(f32) -> f32 {
        // m*accel + b*velocity + k*position = 0
        // position = (A*cos(lambda*t) + B*sin(lambda*t))*exp(-lambda*t)
        //
        // The position here is not `fish_position`, but is
        // relative to the `fish_target_position`, since that
        // makes the equation simpler.
        let k = 1.0 / (5.0 * (120.0 - self.fish_difficulty));

        let b = 1.0 / 5.0;
        let lambda = b / 2.0;
        let determinant = lambda * lambda - k;

        let initial_offset = self.fish_position - self.fish_target_position;
        let initial_velocity = self.fish_velocity;

        enum Solution {
            // A*exp(-(lambda+tau)*t) + B*exp(-(lambda-tau)*t)
            Overdamped {
                tau: f32,
                coefficient_of_pos: f32,
                coefficient_of_neg: f32,
            },

            // (A*t + B)*exp(-lambda*t)
            CriticallyDamped {
                linear_offset: f32,
                linear_slope: f32,
            },

            // (A*cos(omega*t) + B*sin(omega*t)) * exp(-lambda*t)
            Underdamped {
                omega: f32,
                cos_coefficient: f32,
                sin_coefficient: f32,
            },
        }

        let solution = {
            let p0 = initial_offset;
            let v0 = initial_velocity;
            if determinant.abs() < 1e-5 {
                // Critically-damped oscillator, which requires special handling
                // (p0 + t*(b*p0/2 + v0))*exp(-b*t/2)
                // (p0 + t*(lambda*p0 + v0))*exp(-lambda*t)
                Solution::CriticallyDamped {
                    linear_offset: p0,
                    linear_slope: lambda * p0 + initial_velocity,
                }
            } else if determinant > 0.0 {
                let tau = determinant.sqrt();
                let coefficient_of_pos =
                    ((tau - lambda) * p0 - initial_velocity) / (2.0 * tau);
                let coefficient_of_neg =
                    ((tau + lambda) * p0 + initial_velocity) / (2.0 * tau);
                Solution::Overdamped {
                    tau,
                    coefficient_of_pos,
                    coefficient_of_neg,
                }
            } else {
                let omega = (-determinant).sqrt();
                let cos_coefficient = p0;
                let sin_coefficient = (lambda * p0 + v0) / omega;

                Solution::Underdamped {
                    omega,
                    cos_coefficient,
                    sin_coefficient,
                }
            }
        };

        let fish_target_position = self.fish_target_position;

        move |t: f32| -> f32 {
            let offset = match solution {
                Solution::Overdamped {
                    tau,
                    coefficient_of_pos,
                    coefficient_of_neg,
                } => {
                    // All fish with a difficulty less than 100
                    // (i.e. everything except the legendary fish)
                    coefficient_of_pos * (-(lambda + tau) * t).exp()
                        + coefficient_of_neg * (-(lambda - tau) * t).exp()
                }
                Solution::CriticallyDamped {
                    linear_offset,
                    linear_slope,
                } => {
                    // Fish with a difficulty of exactly 100 (i.e. the
                    // Glacerfish, and nothing else)
                    let linear_term = linear_offset + linear_slope * t;
                    let exponential_term = (-lambda * t).exp();
                    linear_term * exponential_term
                }
                Solution::Underdamped {
                    omega,
                    cos_coefficient,
                    sin_coefficient,
                } => {
                    // Fish with a difficulty greater than 100
                    // (i.e. the Legend, and nothing else)
                    let sin_term = sin_coefficient * (omega * t).sin();
                    let cos_term = cos_coefficient * (omega * t).cos();
                    let exponential_term = (-lambda * t).exp();

                    (sin_term + cos_term) * exponential_term
                }
            };
            let predicted = fish_target_position + offset;

            predicted
        }
    }

    fn should_move_upward(&self) -> bool {
        const LOOKAHEAD_TIME: f32 = 30.0;

        let fish_position = self.predicted_positions()(LOOKAHEAD_TIME);

        let bar_position =
            (self.bar_position.start + self.bar_position.end - 28.0) / 2.0
                - 16.0;

        let bar_velocity = self.bar_velocity;

        let bar_velocity_if_click = bar_velocity - 0.25;
        let bar_velocity_if_release = bar_velocity + 0.25;

        let bar_position_if_click =
            bar_velocity_if_click * LOOKAHEAD_TIME + bar_position;
        let bar_position_if_release =
            bar_velocity_if_release * LOOKAHEAD_TIME + bar_position;

        let distance_if_click = (fish_position - bar_position_if_click).abs();
        let distance_if_release =
            (fish_position - bar_position_if_release).abs();

        distance_if_click < distance_if_release
    }
}

impl WidgetWindow<Error> for FishingUI {
    fn title(&self) -> std::borrow::Cow<str> {
        "Fishing".into()
    }

    fn periodic_update<'a>(
        &mut self,
        globals: &'a tui_utils::TuiGlobals,
        side_effects: &'a mut tui_utils::WidgetSideEffects,
    ) -> Result<(), Error> {
        let game_state = globals
            .get::<GameState>()
            .expect("Generated/updated in top-level GUI update");
        let fishing_state = &game_state.fishing;

        let is_holding_rod = fishing_state.is_holding_rod;

        let showing_fish = fishing_state.showing_fish;
        let minigame_in_progress = fishing_state.minigame_in_progress;
        let is_fishing = fishing_state.is_fishing;
        let is_nibbling =
            !showing_fish && !minigame_in_progress && fishing_state.is_nibbling;
        let is_casting = fishing_state.is_casting;
        let is_timing_cast = fishing_state.is_timing_cast;

        if is_casting {
            self.history.clear();
        }

        if let Some(state) = self.current_state(fishing_state) {
            self.history.push(state);
        }

        if is_timing_cast {
            let casting_power = fishing_state.casting_power;
            let action = if casting_power > 0.9 {
                GameAction::ReleaseTool
            } else {
                GameAction::HoldTool
            };
            side_effects.broadcast(action);
        } else if minigame_in_progress {
            if let Some(state) = self.history.last() {
                let action = if state.should_move_upward() {
                    GameAction::HoldTool
                } else {
                    GameAction::ReleaseTool
                };
                side_effects.broadcast(action);
            }
        } else if is_nibbling {
            side_effects.broadcast(GameAction::ReleaseTool);
        } else if is_fishing {
            side_effects.broadcast(GameAction::HoldTool);
        } else if showing_fish {
            side_effects.broadcast(GameAction::HoldTool);
        } else if is_holding_rod {
            side_effects.broadcast(GameAction::HoldTool);
        }

        Ok(())
    }

    fn draw<'a>(
        &'a mut self,
        globals: &'a tui_utils::TuiGlobals,
        area: ratatui::layout::Rect,
        buf: &mut ratatui::prelude::Buffer,
    ) {
        let game_state = globals
            .get::<GameState>()
            .expect("Generated/updated in top-level GUI update");
        let fishing_state = &game_state.fishing;

        fn get_value(value: impl std::fmt::Display) -> String {
            format!("{value}")
        }

        let showing_treasure = fishing_state.showing_treasure;
        let showing_fish = fishing_state.showing_fish;
        let pulling_out_of_water = fishing_state.pulling_out_of_water;
        let minigame_in_progress = fishing_state.minigame_in_progress;
        let is_nibbling =
            !showing_fish && !minigame_in_progress && fishing_state.is_nibbling;
        let is_fishing =
            !is_nibbling && !minigame_in_progress && fishing_state.is_fishing;
        let bobber_in_air = fishing_state.bobber_in_air;
        let is_casting = fishing_state.is_casting;
        let is_timing_cast = fishing_state.is_timing_cast;

        let get_style = |is_active: bool| {
            if is_active {
                Style::new().fg(Color::White).add_modifier(Modifier::BOLD)
            } else {
                Style::new().fg(Color::DarkGray)
            }
        };
        let table = Table::new(
            [
                Row::new([
                    "Game tick:".into(),
                    get_value(fishing_state.game_tick),
                ]),
                Row::new([
                    "Selecting cast distance:".into(),
                    get_value(fishing_state.is_timing_cast),
                ])
                .style(get_style(is_timing_cast)),
                Row::new([
                    "Cast distance:".into(),
                    get_value(fishing_state.casting_power),
                ])
                .style(get_style(is_timing_cast)),
                Row::new([
                    "Currently casting:".into(),
                    get_value(fishing_state.is_casting),
                ])
                .style(get_style(is_casting)),
                Row::new([
                    "Bobber in air:".into(),
                    get_value(fishing_state.is_casting),
                ])
                .style(get_style(bobber_in_air)),
                Row::new([
                    "Currently fishing:".into(),
                    get_value(fishing_state.is_fishing),
                ])
                .style(get_style(is_fishing && !is_nibbling)),
                Row::new([
                    "Fish bites at:".into(),
                    get_value(fishing_state.time_until_fishing_bite),
                ])
                .style(get_style(is_fishing && !is_nibbling)),
                Row::new([
                    "Time waiting for bite:".into(),
                    get_value(fishing_state.fishing_bite_accumulator),
                ])
                .style(get_style(is_fishing && !is_nibbling)),
                Row::new([
                    "Fish nibbling hook:".into(),
                    get_value(fishing_state.is_nibbling),
                ])
                .style(get_style(is_nibbling)),
                Row::new([
                    "Fish leaves at:".into(),
                    get_value(fishing_state.time_until_fishing_nibble_done),
                ])
                .style(get_style(is_nibbling)),
                Row::new([
                    "Time waiting with hooked fish:".into(),
                    get_value(fishing_state.fishing_nibble_accumulator),
                ])
                .style(get_style(is_nibbling)),
                Row::new([
                    "Minigame in progress:".into(),
                    get_value(fishing_state.minigame_in_progress),
                ])
                .style(get_style(minigame_in_progress)),
                Row::new([
                    "Fish difficulty:".into(),
                    get_value(fishing_state.fish_difficulty),
                ])
                .style(get_style(minigame_in_progress)),
                Row::new([
                    "Fish position:".into(),
                    get_value(fishing_state.fish_position),
                ])
                .style(get_style(minigame_in_progress)),
                Row::new([
                    "Fish velocity:".into(),
                    get_value(fishing_state.fish_velocity),
                ])
                .style(get_style(minigame_in_progress)),
                Row::new([
                    "Fish target position:".into(),
                    get_value(fishing_state.fish_target_position),
                ])
                .style(get_style(minigame_in_progress)),
                Row::new([
                    "Fishing bar position:".into(),
                    get_value(fishing_state.bar_position),
                ])
                .style(get_style(minigame_in_progress)),
                Row::new([
                    "Fishing bar velocity:".into(),
                    get_value(fishing_state.bar_velocity),
                ])
                .style(get_style(minigame_in_progress)),
                Row::new([
                    "Fishing bar height:".into(),
                    get_value(fishing_state.bar_height),
                ])
                .style(get_style(minigame_in_progress)),
                Row::new([
                    "Bobber in bar:".into(),
                    get_value(fishing_state.bobber_in_bar),
                ])
                .style(get_style(minigame_in_progress)),
                Row::new([
                    "Catch progress:".into(),
                    get_value(fishing_state.catch_progress),
                ])
                .style(get_style(minigame_in_progress)),
                Row::new([
                    "Pulling out of water:".into(),
                    get_value(fishing_state.pulling_out_of_water),
                ])
                .style(get_style(pulling_out_of_water)),
                Row::new([
                    "Showing fish:".into(),
                    get_value(fishing_state.showing_fish),
                ])
                .style(get_style(showing_fish)),
                Row::new([
                    "Showing treasure:".into(),
                    get_value(fishing_state.showing_treasure),
                ])
                .style(get_style(showing_treasure)),
            ],
            [Constraint::Min(33), Constraint::Percentage(100)],
        );

        const MAX_SIZE: f64 = 568.0;

        let fish_points_in_bar: Vec<_> = self
            .history
            .iter()
            .filter(|state| state.fish_in_bar)
            .map(|state| {
                (state.tick as f64, MAX_SIZE - state.fish_position as f64)
            })
            .collect();
        let fish_points_outside_bar: Vec<_> = self
            .history
            .iter()
            .filter(|state| !state.fish_in_bar)
            .map(|state| {
                (state.tick as f64, MAX_SIZE - state.fish_position as f64)
            })
            .collect();
        let fishing_bar_data: Vec<_> = self
            .history
            .iter()
            .flat_map(|state| {
                let top = state.bar_position.end as f64;
                let bottom = state.bar_position.start as f64;

                // The computation of `bobberInBar` requires that the
                // following conditions hold:
                //
                //    bobberPosition + 12f <= bobberBarPos - 32f + bobberBarHeight
                //    bobberPosition - 16f >= bobberBarPos - 32f
                //
                // Presumably, these are to account for the finite
                // size of the fish sprite, which must be entirely
                // within the bar.  Practically, if we want to apply
                // `bar <= fish <= bar+height`, it means that both the
                // position is offset, and that the bar is smaller.
                //
                //    bobberBarPos <= bobberPosition + 16.0 <= bobberBarPos + bobberBarHeight - 28.0
                //    (bobberBarPos-16.0) <= bobberPosition <= (bobberBarPos-16.0) + bobberBarHeight - 28.0
                let top = top - 44.0;
                let bottom = bottom - 16.0;

                let top = MAX_SIZE - top;
                let bottom = MAX_SIZE - bottom;

                let mid = (top + bottom) / 2.0;
                let tick = state.tick as f64;
                [(tick, mid), (tick, bottom), (tick, top), (tick, mid)]
            })
            .collect();

        let fish_target_points: Vec<_> = self
            .history
            .iter()
            .tuples()
            .filter(|(a, b)| a.fish_target_position != b.fish_target_position)
            .map(|(_, b)| b)
            .map(|state| {
                (
                    state.tick as f64,
                    MAX_SIZE - state.fish_target_position as f64,
                )
            })
            .collect();

        let prediction_size = 120;
        let predicted_fish_points: Vec<_> = self
            .history
            .last()
            .map(|state| {
                let predictor = state.predicted_positions();

                (0..prediction_size).map(move |i| {
                    let pos = predictor(i as f32) as f64;
                    let tick = state.tick as f64;
                    let i = i as f64;
                    (tick + i, MAX_SIZE - pos)
                })
            })
            .into_iter()
            .flatten()
            .collect();

        let datasets = vec![
            Dataset::default()
                .data(&fishing_bar_data)
                .graph_type(GraphType::Line)
                .name("Fishing Bar")
                .marker(Marker::Block)
                .style(Color::Rgb(10, 50, 10)),
            Dataset::default()
                .data(&fish_points_outside_bar)
                .graph_type(GraphType::Scatter)
                .name("Fish")
                .marker(Marker::Braille)
                .style(Style::default().blue()),
            Dataset::default()
                .data(&fish_points_in_bar)
                .graph_type(GraphType::Scatter)
                .marker(Marker::Braille)
                .style(Style::default().red()),
            Dataset::default()
                .data(&predicted_fish_points)
                .graph_type(GraphType::Scatter)
                .name("Fish (pred.)")
                .marker(Marker::Braille)
                .style(Style::default().yellow()),
            Dataset::default()
                .data(&fish_target_points)
                .graph_type(GraphType::Scatter)
                .name("Fish's Target")
                .marker(Marker::Dot)
                .style(Style::default().yellow()),
        ];

        let x_axis = Axis::default()
            .title("Game tick")
            .style(Style::default().white())
            .bounds(
                match self
                    .history
                    .iter()
                    .map(|state| state.tick as f64)
                    .minmax()
                {
                    itertools::MinMaxResult::NoElements => [0.0, 0.0],
                    itertools::MinMaxResult::OneElement(value) => {
                        [value, value + 1.0 + (prediction_size as f64)]
                    }
                    itertools::MinMaxResult::MinMax(a, b) => {
                        [a, b + (prediction_size as f64)]
                    }
                },
            );

        let y_axis = Axis::default()
            .title("Height")
            .style(Style::default().white())
            .bounds([0.0, MAX_SIZE]);

        let graph = Chart::new(datasets)
            .block(Block::new().title(format!(
                "Graph with {} data points",
                self.history.len()
            )))
            .x_axis(x_axis)
            .y_axis(y_axis);

        let (table_area, graph_area) = area.split_from_left(50);

        StatefulWidget::render(table, table_area, buf, &mut self.table_state);

        graph.render(graph_area, buf);
    }

    fn apply_key_binding<'a>(
        &'a mut self,
        _keystrokes: &'a tui_utils::inputs::KeySequence,
        _globals: &'a tui_utils::TuiGlobals,
        _side_effects: &'a mut tui_utils::WidgetSideEffects,
    ) -> tui_utils::inputs::KeyBindingMatch {
        tui_utils::inputs::KeyBindingMatch::Mismatch
    }

    fn apply_side_effects<'a>(
        &'a mut self,
        _globals: &'a tui_utils::TuiGlobals,
        _side_effects: &'a mut tui_utils::WidgetSideEffects,
    ) -> Result<(), Error> {
        Ok(())
    }
}
