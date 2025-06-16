use crate::{Error, GameAction, GameState};

use super::bot_logic::{
    ActionCollector, BotGoal, BotGoalResult, BotInterrupt, LogicStack,
};

pub struct SkipCutscenes;

pub struct SkipCurrentCutscene;

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
        if game_state.globals.event_up {
            Ok(Some(SkipCurrentCutscene.into()))
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
        if !game_state.globals.event_up {
            return Ok(BotGoalResult::Completed);
        }

        let pixel = game_state.display.event_skip_button().center();
        actions.do_action(GameAction::MouseOverPixel(pixel));
        actions.do_action(GameAction::LeftClick);
        Ok(BotGoalResult::InProgress)
    }
}
