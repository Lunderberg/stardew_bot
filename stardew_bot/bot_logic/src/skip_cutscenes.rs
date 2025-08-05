use game_state::GameState;
use itertools::Itertools as _;

use crate::{Error, GameAction};

use super::bot_logic::{
    ActionCollector, BotGoal, BotGoalResult, BotInterrupt, LogicStack,
    LogicStackItem,
};

pub struct SkipCutscenes;

pub struct SkipCurrentCutscene;

impl Default for SkipCutscenes {
    fn default() -> Self {
        Self::new()
    }
}

impl SkipCutscenes {
    pub fn new() -> Self {
        Self
    }
}

impl BotInterrupt for SkipCutscenes {
    fn description(&self) -> std::borrow::Cow<str> {
        "Skip Cutscenes".into()
    }

    fn check(
        &mut self,
        game_state: &GameState,
    ) -> Result<Option<LogicStack>, Error> {
        let is_festival = game_state
            .current_room()?
            .current_event
            .as_ref()
            .map(|event| event.is_festival)
            .unwrap_or(false);

        if game_state.globals.event_up && !is_festival {
            let stack =
                [LogicStackItem::PreventInterrupt, SkipCurrentCutscene.into()]
                    .into_iter()
                    .collect();
            Ok(Some(stack))
        } else {
            Ok(None)
        }
    }
}

impl BotGoal for SkipCurrentCutscene {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        "Skip current cutscene".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        let is_festival = game_state
            .current_room()?
            .current_event
            .as_ref()
            .map(|event| event.is_festival)
            .unwrap_or(false);
        if !game_state.globals.event_up || is_festival {
            return Ok(BotGoalResult::Completed);
        }

        let pixel = if let Some(menu) = game_state.dialogue_menu() {
            // Specific responses to questions in cutscenes
            menu.response_pixel("Bats")
                .or_else(|| {
                    menu.responses
                        .is_empty()
                        .then(|| menu.pixel_location.center())
                })
                .unwrap_or_else(|| {
                    todo!(
                        "Handle menu choice between options [{}]",
                        menu.responses
                            .iter()
                            .map(|response| &response.text)
                            .format(", ")
                    )
                })
        } else {
            game_state.display.event_skip_button().center()
        };

        actions.do_action(GameAction::MouseOverPixel(pixel));
        actions.do_action(GameAction::LeftClick);
        Ok(BotGoalResult::InProgress)
    }
}
