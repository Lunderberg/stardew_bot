use geometry::Vector;

use crate::{
    CraftItemGoal, Error, GameStateExt as _, InventoryGoal, UseItemOnTile,
};
use game_state::{GameState, ItemId};

use super::{
    bot_logic::{BotInterrupt, LogicStack},
    LocationExt as _,
};

pub struct ExpandStorageInterrupt;

impl ExpandStorageInterrupt {
    pub fn new() -> Self {
        Self
    }

    const FROM_FARM_DOOR: [Vector<isize>; 7] = [
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

    const FROM_MINE_ELEVATOR: [Vector<isize>; 2] = [
        // To the left of the mine elevator
        Vector::new(-1, 1),
        Vector::new(-2, 1),
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
        if game_state.any_menu_open() {
            // Avoid interrupting any in-progress menus.  This is
            // especially important for this interrupt, because common
            // menu interactions such as storing items into a chest could
            // otherwise trigger this interrupt.
            return Ok(None);
        }

        let (room, origin, offsets) = if game_state.player.room_name == "Farm" {
            let room = game_state.get_room("Farm")?;
            let origin = game_state.get_farm_door()?;
            (room, origin, &Self::FROM_FARM_DOOR[..])
        } else if game_state.player.room_name == "Mine" {
            let room = game_state.get_room("Mine")?;
            let origin = game_state.get_mine_elevator()?;
            (room, origin, &Self::FROM_MINE_ELEVATOR[..])
        } else {
            // The player isn't in any room with storage, so now isn't
            // a good time to expand the storage.
            return Ok(None);
        };

        if game_state.player.center_pos().dist(origin.into()) > 20.0 {
            // The player may be in a room that has storage space, but
            // isn't anywhere near it.  Wait until later.
            return Ok(None);
        }

        let iter_chests =
            || room.objects.iter().filter_map(|obj| obj.kind.as_chest());

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
            .filter(|item| item.is_same_item(&ItemId::WOOD))
            .map(|item| item.count)
            .sum::<usize>();
        if available_wood < 50 {
            // We'd like to expand the storage, but there isn't enough
            // wood available to do so, either in the existing storage
            // chests or in the player's inventory.
            return Ok(None);
        }

        let walkable = room.pathfinding(&game_state.statics).walkable();
        let Some(tile) = offsets
            .iter()
            .map(|offset| origin + *offset)
            .find(|tile| walkable[*tile])
        else {
            // All the chests are already made
            return Ok(None);
        };

        let prepare = InventoryGoal::current()
            .room(room.name.clone())
            .with(ItemId::WOOD.clone().with_count(50));
        let craft = CraftItemGoal::new(ItemId::CHEST.with_count(1));
        let place = UseItemOnTile::new(ItemId::CHEST, room.name.clone(), tile);

        let interrupt = LogicStack::new().then(prepare).then(craft).then(place);

        Ok(Some(interrupt))
    }
}
