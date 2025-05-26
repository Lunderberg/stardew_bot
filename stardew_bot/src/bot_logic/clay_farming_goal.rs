use std::collections::HashSet;

use crate::{
    bot_logic::{
        bot_logic::SubGoals, graph_search::GraphSearch as _, BotError,
        MovementGoal, SelectItemGoal,
    },
    game_state::{
        Inventory, Item, Key, ObjectKind, Quality, SeededRng, Vector,
    },
    Direction, Error, GameAction, GameState,
};

use super::bot_logic::{BotGoal, BotGoalResult};

pub struct ClayFarmingGoal;

impl BotGoal for ClayFarmingGoal {
    fn description(&self) -> std::borrow::Cow<str> {
        "Clay farming".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        do_action: &mut dyn FnMut(GameAction),
    ) -> Result<BotGoalResult, Error> {
        // TODO: Handle these are part of some return-to-default
        // logic, rather than needing each goal to release each
        // button.
        {
            let mut cleanup = false;
            if game_state.inputs.left_mouse_down() {
                do_action(GameAction::ReleaseLeftClick.into());
                cleanup = true;
            }
            if game_state.inputs.keys_pressed.iter().any(|key| {
                matches!(key, Key::Delete | Key::RightShift | Key::R)
            }) {
                do_action(GameAction::StopAnimationCanceling);
                cleanup = true;
            }
            if cleanup {
                return Ok(BotGoalResult::InProgress);
            }
        }

        let beach = game_state.get_room("Beach")?;
        let player = &game_state.player;

        if player.using_tool
            && player.last_click == Vector::zero()
            && player
                .selected_item()
                .map(|item| item.item_id.starts_with("(T)"))
                .unwrap_or(false)
        {
            do_action(GameAction::AnimationCancel);
        }

        // Pick up the hoe for use in the rest of the goal.
        let hoe = Item::new("(T)Hoe");
        let pickaxe = Item::new("(T)Pickaxe");
        for item in [&hoe, &pickaxe] {
            let goal = super::InventoryGoal::new(item.clone());
            if !goal.contains_target_item(&player.inventory) {
                return Ok(goal.into());
            }
        }

        // Go to the Beach
        //
        // TODO: Expose the room-to-room movement without requiring
        // local movement afterwards.  In this case, I want to go to
        // the farm, but don't care where I start out within the farm.
        if player.room_name != "Beach" {
            return Ok(MovementGoal::new(
                "Beach".into(),
                Vector::new(38.0, 1.0),
            )
            .with_tolerance(100.0)
            .into());
        }

        let player_tile = player.tile();

        let total_dirt_hoed = game_state
            .global_game_state
            .get_stat("dirtHoed")
            .unwrap_or(0);

        let days_played =
            game_state.global_game_state.get_stat("daysPlayed")?;

        let clay_tiles: HashSet<Vector<isize>> = beach
            .diggable
            .iter()
            .filter(|(_, is_diggable)| **is_diggable)
            .map(|(tile, _)| tile)
            .filter(|tile| {
                let mut rng = SeededRng::from_stardew_seed([
                    days_played as f64,
                    (game_state.global_game_state.unique_id / 2) as f64,
                    (tile.right * 2000) as f64,
                    (tile.down * 77) as f64,
                    total_dirt_hoed as f64,
                ]);
                rng.rand_float() < 0.03
            })
            .collect();

        let opt_adjacent_clay = Direction::iter()
            .map(|dir| player_tile + dir.offset())
            .find(|tile| clay_tiles.contains(&tile));
        if let Some(tile) = opt_adjacent_clay {
            do_action(GameAction::MouseOverTile(tile));

            let has_hoe_dirt = beach
                .objects
                .iter()
                .filter(|obj| matches!(obj.kind, ObjectKind::HoeDirt(_)))
                .any(|obj| obj.tile == tile);
            let tool = if has_hoe_dirt { pickaxe } else { hoe };

            let goal = SelectItemGoal::new(tool);
            if !goal.is_completed(game_state) {
                return Ok(goal.into());
            }

            // Swinging a tool by clicking the mouse uses the previous
            // frame's mouse position, not the current mouse position.
            // While both the MovementGoal and the `MouseOverTile`
            // prior to `SelectItemGoal` try to preemptively move the
            // cursor, if neither substep was necessary then the
            // cursor may not have been updated yet.
            if !player.using_tool
                && tile == game_state.inputs.mouse_tile_location
            {
                do_action(GameAction::LeftClick);
            }

            return Ok(BotGoalResult::InProgress);
        }

        let clear_tiles = beach.collect_clear_tiles();

        let opt_closest_clay = clear_tiles
            .dijkstra_search(player_tile)
            .map(|(tile, _)| tile)
            .filter(|tile| tile != &player_tile)
            .find(|tile| clay_tiles.contains(&tile));

        if let Some(closest_clay) = opt_closest_clay {
            let goal = MovementGoal::new("Beach".into(), closest_clay.into())
                .with_tolerance(1.1);
            Ok(goal.into())
        } else {
            // There's still clay tiles, but we can't reach it.
            Ok(BotGoalResult::Completed)
        }
    }
}
