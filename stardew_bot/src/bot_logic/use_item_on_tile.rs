use geometry::{Direction, Vector};
use itertools::Itertools as _;

use crate::{
    game_state::{Item, ItemCategory, ItemId},
    Error, GameAction, GameState,
};

use super::{
    bot_logic::{
        ActionCollector, BotGoal, BotGoalResult, LogicStack, LogicStackItem,
    },
    MovementGoal, SelectItemGoal,
};

pub struct UseItemOnTile {
    item: Item,
    room: String,
    tile: Vector<isize>,
    item_used: bool,
}

struct SwingWithAnimationCancel {
    tool_used: bool,
}

impl UseItemOnTile {
    pub fn new(
        item: impl Into<Item>,
        room: impl Into<String>,
        tile: Vector<isize>,
    ) -> Self {
        let item = item.into();
        let room = room.into();
        Self {
            item,
            room,
            tile,
            item_used: false,
        }
    }
}

impl BotGoal for UseItemOnTile {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        format!("Use {} on {} in {}", self.item, self.tile, self.room).into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        let player = &game_state.player;
        let player_tile = player.tile();

        if self.item_used {
            return Ok(BotGoalResult::Completed);
        }

        if player.inventory.current_slot(&self.item).is_none() {
            return Ok(BotGoalResult::Completed);
        }

        let select_item = SelectItemGoal::new(self.item.id.clone());
        if !select_item.is_completed(game_state) {
            return Ok(select_item.into());
        }

        let refilling_watering_can = self.item.as_watering_can().is_some()
            && game_state.get_room(&self.room)?.is_water(self.tile);
        let is_scythe = self.item.id == ItemId::SCYTHE;

        let target_tile = self.tile;
        let is_within_range = move |game_state: &GameState| -> bool {
            !refilling_watering_can
                && !is_scythe
                && game_state.player.tile().manhattan_dist(target_tile) <= 1
        };

        let threshold = if refilling_watering_can {
            // Move closer to the edge of the water.  This avoids some
            // edge conditions where the watering can isn't quite
            // within range of the refill.
            1.0
        } else if is_scythe {
            // Using the scythe to clear multiple tiles, including
            // grass.  Walk as close to the target tile as possible.
            1.0
        } else {
            // Threshold allowing 1 tile in any direction, including
            // diagonals.  This case won't typically hit the
            // threshold, and will instead be cancelled based on tile
            // adjacency.
            1.4
        };
        let movement = MovementGoal::new(self.room.clone(), self.tile.into())
            .with_tolerance(threshold);

        if !movement.is_completed(game_state) && !is_within_range(game_state) {
            let room_name = player.room_name.clone();
            let goal = LogicStack::new()
                .then(movement)
                .cancel_if(is_within_range)
                .cancel_if(move |game_state| {
                    game_state.player.room_name != room_name
                });
            return Ok(goal.into());
        }

        let requires_adjacent_tile = match self.item.id.item_id.as_ref() {
            "(T)Pickaxe" | "(T)Axe" | "(T)Hoe" => true,
            _ => false,
        };
        let requires_noncolliding_tile =
            self.item.id.item_id.starts_with("(BC)")
                || (self.item.id.item_id.starts_with("(O)")
                    && game_state
                        .statics
                        .object_data
                        .get(&self.item.id)
                        .map(|data| match &data.category {
                            ItemCategory::Seed => false,
                            _ => true,
                        })
                        .unwrap_or(true));

        let player_pos = player.center_pos();
        if (requires_adjacent_tile && player_tile == self.tile)
            || (requires_noncolliding_tile
                && player_pos.manhattan_dist(self.tile.into()) < 1.0)
        {
            // This tool requires the player to be standing adjacent
            // to the targeted tile, and cannot be used when standing
            // on the tile itself.  Move to an adjacent clear tile.

            let clear_tiles = game_state.current_room()?.collect_clear_tiles();
            let dir = Direction::iter()
                .filter(|dir| dir.is_cardinal())
                .filter(|dir| clear_tiles[self.tile + dir.offset()])
                .sorted_by(|&dir_a, &dir_b| {
                    let dot_product = |dir: Direction| {
                        let player_offset =
                            player_pos - self.tile.map(|x| x as f32);
                        let dir_offset = dir.offset().map(|x| x as f32);

                        player_offset.dot(dir_offset)
                    };
                    dot_product(dir_a).total_cmp(&dot_product(dir_b))
                })
                .next()
                .expect("At least one direction should be accessible");

            let tile: Vector<f32> = self.tile.into();
            let dir_offset: Vector<f32> = dir.offset().into();
            let dir_scale = if requires_noncolliding_tile { 1.3 } else { 1.0 };
            let goal = MovementGoal::new(
                player.room_name.clone(),
                tile + dir_offset * dir_scale,
            );
            return Ok(goal.into());
        }

        actions.do_action(GameAction::MouseOverTile(self.tile));
        if game_state.inputs.mouse_tile_location == self.tile {
            self.item_used = true;
            let wait_for_animation_cancel = {
                let id = &self.item.id.item_id;
                id.starts_with("(T)") || id.starts_with("(W)")
            };
            if wait_for_animation_cancel {
                let stack: LogicStack = [
                    LogicStackItem::PreventInterrupt,
                    SwingWithAnimationCancel::new().into(),
                ]
                .into_iter()
                .collect();
                return Ok(stack.into());
            } else {
                actions.do_action(GameAction::LeftClick);
            }
        }
        Ok(BotGoalResult::InProgress)
    }
}

impl SwingWithAnimationCancel {
    pub fn new() -> Self {
        Self { tool_used: false }
    }
}

impl BotGoal for SwingWithAnimationCancel {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        "Swing tool".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        // Prior to starting this BotGoal, the player should already
        // be lined up to use the tool, in the correct location, with
        // the tool selected, and with the mouse in the appropriate
        // location to use the tool.
        //
        // This is an implementation detail of `UseItemOnTile`,
        // separated into a distinct `BotGoal` because this portion
        // should not be interrupted.  (e.g. While a tool is swinging,
        // if an enemy comes within range, the tool usage should be
        // animation-cancelled prior to the interrupt triggering.)

        let player = &game_state.player;
        let using_tool = player.using_tool;

        if !using_tool {
            if self.tool_used {
                // The tool use has been completed, and the animation
                // has been cancelled.
                return Ok(BotGoalResult::Completed);
            } else {
                // Start swinging the tool.
                actions.do_action(GameAction::LeftClick);
                return Ok(BotGoalResult::InProgress);
            }
        }

        self.tool_used = true;

        let is_tool = player
            .selected_item()
            .map(|item| item.id.item_id.starts_with("(T)"))
            .unwrap_or(false);
        let can_animation_cancel = if is_tool {
            // When a tool has an effect, the `last_click` field
            // is zeroed out.  At this point, we know that the
            // tool's effect has been applied, and the remaining
            // animation can be skipped.
            player.last_click == Vector::zero()
        } else {
            // For a melee weapon, the `last_click` field is
            // zeroed out almost immediately, but the weapon has
            // an effect for each animation frame of the swipe.
            // The animation has 7 frames.  The first 5 frames
            // take 175 ms total.  The 6th frame takes longer,
            // (e.g. 130 ms for the Scythe).  The player can only
            // move again after the 6th frame finishes.
            //
            // Therefore, animation cancel out of the swipe when
            // reaching the 6th frame.
            player
                .weapon_swipe_animation_frame
                .map(|frame| frame >= 5)
                .unwrap_or(false)
        };
        if can_animation_cancel {
            actions.do_action(GameAction::AnimationCancel);
        }

        Ok(BotGoalResult::InProgress)
    }
}
