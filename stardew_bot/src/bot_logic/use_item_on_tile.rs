use itertools::Itertools as _;

use crate::{
    game_state::{Item, Vector},
    Direction, Error, GameAction, GameState,
};

use super::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult},
    MovementGoal, SelectItemGoal,
};

pub struct UseItemOnTile {
    item: Item,
    room: String,
    tile: Vector<isize>,
    item_has_been_used: bool,
}

impl UseItemOnTile {
    pub fn new(
        item: Item,
        room: impl Into<String>,
        tile: Vector<isize>,
    ) -> Self {
        let room = room.into();
        Self {
            item,
            room,
            tile,
            item_has_been_used: false,
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

        if player.using_tool
            && player
                .selected_item()
                .map(|item| item.item_id.starts_with("(T)"))
                .unwrap_or(false)
        {
            if player.last_click == Vector::zero() {
                actions.do_action(GameAction::AnimationCancel);
            }
            return Ok(BotGoalResult::InProgress);
        }

        if self.item_has_been_used {
            return Ok(BotGoalResult::Completed);
        }

        let tolerance = match self.item.item_id.as_ref() {
            "(T)WateringCan"
                if game_state.get_room(&self.room)?.is_water(self.tile) =>
            {
                1.0
            }
            _ => 1.4,
        };

        let movement =
            MovementGoal::new(self.room.clone(), self.tile.map(|x| x as f32))
                .with_tolerance(tolerance);
        if !movement.is_completed(game_state) {
            return Ok(movement.into());
        }

        let select_item = SelectItemGoal::new(self.item.clone());
        if !select_item.is_completed(game_state) {
            return Ok(select_item.into());
        }

        let requires_adjacent_tile = match self.item.item_id.as_ref() {
            "(T)Pickaxe" | "(T)Axe" | "(T)Hoe" => true,
            other => other.starts_with("(BC)"),
        };
        let player_tile = game_state.player.tile();
        if requires_adjacent_tile && player_tile == self.tile {
            // This tool requires the player to be standing adjacent
            // to the targeted tile, and cannot be used when standing
            // on the tile itself.  Move to an adjacent clear tile.

            let clear_tiles = game_state.current_room()?.collect_clear_tiles();
            let dir = Direction::iter()
                .filter(|dir| dir.is_cardinal())
                .filter(|dir| clear_tiles[player_tile + dir.offset()])
                .sorted_by(|&dir_a, &dir_b| {
                    let dot_product = |dir: Direction| {
                        let player_offset = game_state.player.center_pos()
                            - player_tile.map(|x| x as f32);
                        let dir_offset = dir.offset().map(|x| x as f32);

                        player_offset.dot(dir_offset)
                    };
                    dot_product(dir_a).total_cmp(&dot_product(dir_b))
                })
                .next()
                .expect("At least one direction should be accessible");
            let goal = MovementGoal::new(
                game_state.player.room_name.clone(),
                (player_tile + dir.offset()).into(),
            );
            return Ok(goal.into());
        }

        actions.do_action(GameAction::MouseOverTile(self.tile));
        if game_state.inputs.mouse_tile_location == self.tile
            && !game_state.inputs.left_mouse_down()
        {
            actions.do_action(GameAction::LeftClick);
            self.item_has_been_used = true;
        }
        Ok(BotGoalResult::InProgress)
    }
}
