use crate::{bot_logic::UseItemOnTile, Error, GameState};

use super::bot_logic::{BotInterrupt, LogicStack};

pub struct AttackNearbyEnemy {}

impl AttackNearbyEnemy {
    pub fn new() -> Self {
        Self {}
    }
}

impl BotInterrupt for AttackNearbyEnemy {
    fn description(&self) -> std::borrow::Cow<str> {
        "Attack Nearby Enemies".into()
    }

    fn check(
        &mut self,
        game_state: &GameState,
    ) -> Result<Option<LogicStack>, Error> {
        let opt_weapon = game_state
            .player
            .inventory
            .iter_items()
            .find(|item| item.as_weapon().is_some());
        let Some(weapon) = opt_weapon else {
            return Ok(None);
        };

        let player_tile = game_state.player.tile();
        let opt_nearby_monster = game_state
            .current_room()?
            .characters
            .iter()
            .filter(|character| character.health.is_some())
            .find(|monster| player_tile.manhattan_dist(monster.tile()) <= 1);

        let Some(nearby_monster) = opt_nearby_monster else {
            return Ok(None);
        };

        let goal = UseItemOnTile::new(
            weapon.clone(),
            game_state.player.room_name.clone(),
            nearby_monster.tile(),
        )
        .allow_room_change(false);

        Ok(Some(goal.into()))
    }
}
