use crate::{game_state::Vector, Error, GameAction, GameState};

use super::bot_logic::{
    ActionCollector, BotGoal, BotGoalResult, BotInterrupt, LogicStack,
};

pub struct CollectNearbyItems {
    search_radius: f32,
}

struct WalkTowardDebris {
    search_radius_squared: f32,
    goal_dist_squared: f32,
}

impl CollectNearbyItems {
    pub fn new() -> Self {
        Self {
            search_radius: 2.25,
        }
    }

    #[allow(dead_code)]
    pub fn with_search_radius(self, search_radius: f32) -> Self {
        Self {
            search_radius,
            ..self
        }
    }
}

impl BotInterrupt for CollectNearbyItems {
    fn description(&self) -> std::borrow::Cow<str> {
        format!("Collect items within {} tiles", self.search_radius).into()
    }

    fn check(
        &mut self,
        game_state: &GameState,
    ) -> Result<Option<LogicStack>, Error> {
        let goal_dist = 1.7;
        let goal = WalkTowardDebris {
            search_radius_squared: self.search_radius * self.search_radius,
            goal_dist_squared: goal_dist * goal_dist,
        };

        if !goal.is_completed(game_state)? {
            return Ok(Some(goal.into()));
        }

        Ok(None)
    }
}

impl WalkTowardDebris {
    fn is_completed(&self, game_state: &GameState) -> Result<bool, Error> {
        Ok(self.iter_offsets(game_state)?.next().is_none())
    }

    fn iter_offsets<'a>(
        &'a self,
        game_state: &'a GameState,
    ) -> Result<impl Iterator<Item = Vector<f32>> + 'a, Error> {
        let inventory = &game_state.player.inventory;

        let player_pos = game_state.player.center_pos();

        let iter = game_state
            .current_room()?
            .items
            .iter()
            .filter(|item| inventory.can_add(item))
            .map(move |item| item.position / 64.0 - player_pos)
            .filter(|offset| {
                let dist2 = offset.mag2();
                self.goal_dist_squared < dist2
                    && dist2 < self.search_radius_squared
            });

        Ok(iter)
    }
}

impl BotGoal for WalkTowardDebris {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        format!(
            "Walk toward debris within {} tiles",
            self.search_radius_squared
        )
        .into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        let opt_nearby_item = self.iter_offsets(game_state)?.min_by(|a, b| {
            num::traits::float::TotalOrder::total_cmp(&a.mag2(), &b.mag2())
        });

        if let Some(offset) = opt_nearby_item {
            actions.do_action(GameAction::Move(offset.closest_direction()));
            return Ok(BotGoalResult::InProgress);
        }

        Ok(BotGoalResult::Completed)
    }
}
