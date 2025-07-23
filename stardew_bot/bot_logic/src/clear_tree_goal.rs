use std::borrow::Cow;

use game_state::{GameState, ItemId, ObjectKind};
use geometry::{Direction, Vector};

use crate::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult, LogicStack},
    Error, LocationExt as _, MaintainStaminaGoal, MovementGoal, UseItemOnTile,
};

pub struct ClearTreeGoal {
    room: Cow<'static, str>,
    tile: Vector<isize>,
}

impl ClearTreeGoal {
    pub fn new(tile: Vector<isize>) -> Self {
        Self {
            room: "Farm".into(),
            tile,
        }
    }

    fn get_axe<'a>(&self, game_state: &'a GameState) -> Option<&'a ItemId> {
        game_state
            .player
            .inventory
            .iter_items()
            .find(|item| item.id == ItemId::AXE)
            .map(|item| &item.id)
    }

    fn tree_on_tile(&self, game_state: &GameState) -> Result<bool, Error> {
        let room = game_state.get_room(&self.room)?;
        let tree_on_tile = room
            .objects
            .iter()
            .find(|obj| obj.tile == self.tile)
            .map(|obj| match &obj.kind {
                ObjectKind::Tree(tree) => tree.health > 0.0,
                _ => false,
            })
            .unwrap_or(false);

        Ok(tree_on_tile)
    }

    pub fn is_completed(&self, game_state: &GameState) -> Result<bool, Error> {
        let can_clear = self.get_axe(game_state).is_some()
            && self.tree_on_tile(game_state)?;
        Ok(!can_clear)
    }
}

impl BotGoal for ClearTreeGoal {
    fn description(&self) -> Cow<'static, str> {
        format!("Clear tree from {}", self.tile).into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        _actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        let Some(axe) = self.get_axe(game_state) else {
            return Ok(BotGoalResult::Completed);
        };

        if !self.tree_on_tile(game_state)? {
            return Ok(BotGoalResult::Completed);
        }

        let goal = MaintainStaminaGoal::new();
        if !goal.is_completed(game_state)? {
            return Ok(goal.into());
        }
        if game_state.player.current_stamina < 2.0 {
            return Ok(BotGoalResult::Completed);
        }

        let chop_tree =
            UseItemOnTile::new(axe.clone(), self.room.to_string(), self.tile);

        let player_tile = game_state.player.tile();
        let room = game_state.get_room(&self.room)?;

        let distances =
            room.pathfinding(&game_state.statics).distances(player_tile);

        // See if we want the tree to fall in a certain
        // direction for easier collection.
        let fall_dir_heuristic = |dir: Direction| -> u64 {
            (0..5)
                .map(|dist: isize| self.tile + dir.offset() * dist)
                .map(|tile| {
                    let has_hard_block = !room.blocked.is_unset(tile);
                    let has_soft_block = !distances.is_some(tile);
                    (has_hard_block as u64) * 100000
                        + (has_soft_block as u64) * 1000
                })
                .sum()
        };
        let fall_dir = if fall_dir_heuristic(Direction::East)
            < fall_dir_heuristic(Direction::West)
        {
            Direction::East
        } else {
            Direction::West
        };
        let opt_standing_dir = Direction::iter()
            .filter(|stand_dir| stand_dir.offset().dot(fall_dir.offset()) <= 0)
            .filter_map(|stand_dir| {
                let stand_tile = self.tile + stand_dir.offset();
                let steps = distances.get_opt(stand_tile)?;
                Some((stand_dir, steps))
            })
            .min_by_key(|(_, steps)| **steps)
            .map(|(stand_dir, _)| stand_dir);
        let Some(stand_dir) = opt_standing_dir else {
            // We'd prefer the tree to fall in a direction, but can't
            // reach any tile that would allow it to fall in that
            // direction.
            return Ok(chop_tree.into());
        };

        let stand_tile: Vector<isize> = self.tile + stand_dir.offset();
        let (stand_pos, tolerance): (Vector<f32>, _) = match stand_dir {
            Direction::North | Direction::South => {
                let pos: Vector<f32> = stand_tile.into();
                let fall_dir = fall_dir.offset().map(|x| x as f32);
                (pos - fall_dir * 0.25, 0.2)
            }
            _ => (stand_tile.into(), 0.3),
        };

        let stack = LogicStack::new()
            .then(
                MovementGoal::new("Farm", stand_pos).with_tolerance(tolerance),
            )
            .then(chop_tree);

        Ok(stack.into())
    }
}
