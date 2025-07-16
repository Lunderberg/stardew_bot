use game_state::GameState;

use crate::Error;

use super::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult},
    ActivateTile,
};

#[derive(Clone)]
pub struct GoToActionTile {
    action: String,
}

impl GoToActionTile {
    #[allow(dead_code)]
    pub fn new(action: impl Into<String>) -> Self {
        Self {
            action: action.into(),
        }
    }

    pub fn is_completed(&self, game_state: &GameState) -> bool {
        game_state.dialogue_menu.is_some() || game_state.shop_menu.is_some()
    }
}

impl BotGoal for GoToActionTile {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        format!("Go to {}", self.action).into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        _actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        if self.is_completed(game_state) {
            return Ok(BotGoalResult::Completed);
        }

        let (target_room, target_position) = game_state
            .locations
            .iter()
            .flat_map(|loc| {
                loc.action_tiles
                    .iter()
                    .map(move |(tile, action)| (loc, tile, action))
            })
            .filter(|(_, _, action)| action.as_str() == self.action.as_str())
            .map(|(loc, tile, _)| (loc.name.clone(), tile.map(|x| x as f32)))
            .reduce(|(lhs_loc, lhs_tile), (rhs_loc, rhs_tile)| {
                if lhs_loc == rhs_loc {
                    (lhs_loc, (lhs_tile + rhs_tile) / 2.0)
                } else {
                    (lhs_loc, lhs_tile)
                }
            })
            .ok_or_else(|| Error::NoTileWithAction(self.action.clone()))?;
        let target_position = target_position.map(|x| x.round() as isize);

        let goal = ActivateTile::new(target_room, target_position);
        Ok(goal.into())
    }
}
