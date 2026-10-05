use game_state::{GameState, Menu};

use crate::Error;

use super::{
    ActivateTile,
    bot_logic::{ActionCollector, BotGoal, BotGoalResult},
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
        matches!(game_state.menu, Some(Menu::Dialogue(_) | Menu::Shop(_)))
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

        let (target_room, _num_tiles, ave_position) = game_state
            .locations
            .iter()
            .flat_map(|loc| {
                loc.action_tiles
                    .iter()
                    .map(move |(tile, action)| (loc, tile, action))
            })
            .filter(|(_, _, action)| action.as_str() == self.action.as_str())
            .map(|(loc, tile, _)| (loc.name.clone(), 1, tile.map(|x| x as f32)))
            .reduce(
                |(lhs_loc, lhs_count, lhs_tile),
                 (rhs_loc, rhs_count, rhs_tile)| {
                    if lhs_loc == rhs_loc {
                        let new_count = lhs_count + rhs_count;
                        (
                            lhs_loc,
                            new_count,
                            (lhs_tile * (lhs_count as f32)
                                + rhs_tile * (rhs_count as f32))
                                / (new_count as f32),
                        )
                    } else {
                        (lhs_loc, lhs_count, lhs_tile)
                    }
                },
            )
            .ok_or_else(|| Error::NoTileWithAction(self.action.clone()))?;

        let target_position = game_state
            .locations
            .iter()
            .filter(|loc| loc.name == target_room)
            .flat_map(|loc| {
                loc.action_tiles
                    .iter()
                    .map(move |(tile, action)| (tile, action))
            })
            .filter(|(_, action)| action.as_str() == self.action.as_str())
            .map(|(tile, _)| tile)
            .min_by(|tile_a, tile_b| {
                let dist2_a = tile_a.map(|x| x as f32).dist2(ave_position);
                let dist2_b = tile_b.map(|x| x as f32).dist2(ave_position);
                dist2_a.total_cmp(&dist2_b)
            })
            .expect("Already found at least one candidate tile");

        let goal = ActivateTile::new(target_room, *target_position);
        Ok(goal.into())
    }
}
