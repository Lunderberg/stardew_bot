use crate::{bot_logic::UseItemOnTile, Error, GameState};

use super::bot_logic::{BotGoal, BotInterrupt, LogicStack};

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
            .filter(|monster| {
                !monster.is_invisible_duggy && !monster.is_waiting_rock_crab
            })
            .find(|monster| player_tile.manhattan_dist(monster.tile()) <= 1);

        let Some(nearby_monster) = opt_nearby_monster else {
            return Ok(None);
        };

        let distances = game_state
            .current_room()?
            .pathfinding()
            .distances(player_tile);
        let is_actually_nearby = distances
            .get(nearby_monster.tile())
            .map(|opt_dist| opt_dist.map(|dist| dist <= 1))
            .flatten()
            .unwrap_or(false);
        if !is_actually_nearby {
            return Ok(None);
        }

        let room_name = game_state.player.room_name.clone();
        let goal = UseItemOnTile::new(
            weapon.clone(),
            game_state.player.room_name.clone(),
            nearby_monster.tile(),
        )
        .cancel_if(move |game_state| game_state.player.room_name != room_name);

        Ok(Some(goal.into()))
    }
}
