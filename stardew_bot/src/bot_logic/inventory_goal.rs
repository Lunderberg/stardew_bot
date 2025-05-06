use crate::{
    bot_logic::{bot_logic::SubGoals, MovementGoal},
    game_state::{Inventory, Item, ObjectKind, Quality},
    Error, GameState,
};

use super::bot_logic::{BotGoal, BotGoalResult};

pub struct InventoryGoal {
    item: Item,
}

impl InventoryGoal {
    #[allow(dead_code)]
    pub fn new(item: Item) -> Self {
        Self { item }
    }
}

impl BotGoal for InventoryGoal {
    fn description(&self) -> std::borrow::Cow<str> {
        format!("Pick up {}", self.item).into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
    ) -> Result<BotGoalResult, Error> {
        let contains_target_item = |inventory: &Inventory| -> bool {
            let num_found = inventory
                .items
                .iter()
                .filter_map(|opt_item| opt_item.as_ref())
                .filter(|item| self.item.is_same_item(item))
                .map(|item| item.count)
                .sum::<usize>();

            num_found >= self.item.count
        };

        if contains_target_item(&game_state.player.inventory) {
            return Ok(BotGoalResult::Completed);
        }

        let opt_chest_location = game_state
            .locations
            .iter()
            .flat_map(|loc| {
                loc.objects
                    .iter()
                    .filter_map(|obj| match &obj.kind {
                        ObjectKind::Chest(inventory) => {
                            Some((obj.tile, inventory))
                        }
                        _ => None,
                    })
                    .map(move |(tile, items)| (loc, tile, items))
            })
            .find(|(_, _, items)| contains_target_item(items))
            .map(|(loc, tile, _)| (&loc.name, tile));

        let Some((chest_room, chest_tile)) = opt_chest_location else {
            todo!("Handle case where item isn't found in any chest")
        };

        if chest_room != &game_state.player.room_name
            || game_state
                .player
                .position
                .as_tile()
                .manhattan_dist(chest_tile)
                > 1
        {
            let goals = SubGoals::new().then(MovementGoal::new(
                chest_room.into(),
                chest_tile.map(|x| x as f32),
            ));
            return Ok(goals.into());
        }

        todo!()
    }
}
