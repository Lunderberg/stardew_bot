use crate::{
    bot_logic::{
        CraftItemGoal, GameStateExt as _, InventoryGoal, UseItemOnTile,
    },
    game_state::{Item, Vector},
    Error, GameState,
};

use super::bot_logic::{BotInterrupt, LogicStack};

pub struct ExpandStorageInterrupt;

impl ExpandStorageInterrupt {
    pub fn new() -> Self {
        Self
    }

    const RELATIVE_POS: [Vector<isize>; 7] = [
        // Along the front of the farmhouse
        Vector::new(-2, 3),
        Vector::new(-3, 3),
        Vector::new(-4, 3),
        Vector::new(-5, 3),
        // Above the mailbox
        Vector::new(4, 1),
        Vector::new(4, 0),
        Vector::new(4, -1),
    ];
}

impl BotInterrupt for ExpandStorageInterrupt {
    fn description(&self) -> std::borrow::Cow<str> {
        "Expand storage".into()
    }

    fn check(
        &mut self,
        game_state: &GameState,
    ) -> Result<Option<LogicStack>, Error> {
        if game_state.chest_menu.is_some()
            || game_state.dialogue_menu.is_some()
            || game_state.shop_menu.is_some()
            || game_state.pause_menu.is_some()
            || game_state.mail_menu.is_some()
        {
            // Avoid interrupting any in-progress menus.  This is
            // especially important for this interrupt, because common
            // menu interactions such as storing items into a chest could
            // otherwise trigger this interrupt.
            return Ok(None);
        }

        let farm_door = game_state.get_farm_door()?;
        if game_state.player.room_name != "Farm"
            || game_state.player.center_pos().dist(farm_door.into()) > 20.0
        {
            // The player is far from the FarmHouse, so now isn't a
            // good time to expand the storage.
            return Ok(None);
        }

        // Now, starting the actual checkes
        let farm = game_state.get_room("Farm")?;
        let iter_chests =
            || farm.objects.iter().filter_map(|obj| obj.kind.as_chest());

        let num_free_slots = iter_chests()
            .flat_map(|chest| chest.iter_slots())
            .filter(|opt_item| opt_item.is_none())
            .count();
        if num_free_slots > 5 {
            // The chests currently have spare storage, so no need to
            // expand.
            return Ok(None);
        }

        let available_wood = iter_chests()
            .flat_map(|chest| chest.iter_items())
            .chain(game_state.player.inventory.iter_items())
            .filter(|item| item.is_same_item(&Item::WOOD))
            .map(|item| item.count)
            .sum::<usize>();
        if available_wood < 50 {
            // We'd like to expand the storage, but there isn't enough
            // wood available to do so, either in the existing storage
            // chests or in the player's inventory.
            return Ok(None);
        }

        let walkable = farm.pathfinding().walkable();
        let Some(tile) = Self::RELATIVE_POS
            .iter()
            .map(|offset| farm_door + *offset)
            .find(|tile| walkable[*tile])
        else {
            // All the chests are already made
            return Ok(None);
        };

        let prepare =
            InventoryGoal::current().with(Item::WOOD.clone().with_count(50));
        let craft = CraftItemGoal::new(Item::CHEST);
        let place = UseItemOnTile::new(Item::CHEST, "Farm", tile);

        let interrupt = LogicStack::new().then(prepare).then(craft).then(place);

        Ok(Some(interrupt))
    }
}
