use crate::{
    bot_logic::UseItemOnTile,
    game_state::{Item, TileMap, Vector, WeaponKind},
    Error, GameAction, GameState,
};

use super::{
    best_weapon,
    bot_logic::{
        ActionCollector, BotGoal, BotGoalResult, BotInterrupt, LogicStack,
        LogicStackItem,
    },
    SelectItemGoal,
};

pub struct AttackNearbyEnemy {}

struct ClubSmashNearby;

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
        let club_smash = ClubSmashNearby::new();
        if club_smash.should_start_smash(game_state)? {
            let stack = [LogicStackItem::PreventInterrupt, club_smash.into()]
                .into_iter()
                .collect();
            return Ok(Some(stack));
        }

        let opt_weapon = best_weapon(game_state.player.inventory.iter_items());
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
            .get_opt(nearby_monster.tile())
            .map(|&dist| dist <= 1)
            .unwrap_or(false);
        if !is_actually_nearby {
            return Ok(None);
        }

        let offset =
            nearby_monster.center_pos() - game_state.player.center_pos();
        let dir = offset.closest_direction();

        let room_name = game_state.player.room_name.clone();
        let goal = UseItemOnTile::new(
            weapon.clone(),
            game_state.player.room_name.clone(),
            player_tile + dir.offset(),
        )
        .cancel_if(move |game_state| game_state.player.room_name != room_name);

        Ok(Some(goal.into()))
    }
}

impl ClubSmashNearby {
    /// If an enemy is within this distance, should start a club
    /// smash.
    const START_SMASH_DISTANCE: f32 = 2.0;

    /// If an enemy is within this distance, should continue the club
    /// smash.
    const CONTINUE_SMASH_DISTANCE: f32 = 3.0;

    pub fn new() -> Self {
        Self
    }

    fn opt_club<'a>(&self, game_state: &'a GameState) -> Option<&'a Item> {
        best_weapon(game_state.player.inventory.iter_items()).filter(|item| {
            item.as_weapon()
                .map(|weapon| matches!(weapon.kind, WeaponKind::Club))
                .unwrap_or(false)
        })
    }

    fn distance_to_closest_enemy(
        &self,
        game_state: &GameState,
    ) -> Result<Option<f32>, Error> {
        let player_pos = game_state.player.position / 64.0;
        let player_tile = game_state.player.tile();
        let room = game_state.current_room()?;
        let mut opt_clear_tiles: Option<TileMap<bool>> = None;

        let opt_dist = game_state
            .current_room()?
            .characters
            .iter()
            .filter(|character| character.is_monster())
            .filter(|monster| {
                if monster.ignores_collisions {
                    // This is a flying monster (e.g. bat/ghost), and
                    // can be hit through obstacles.
                    return true;
                }
                if (monster.center_pos() - player_pos).max_abs()
                    > Self::START_SMASH_DISTANCE * 2.0
                {
                    // This monster is far away, no need to do any
                    // collision checks.
                    return false;
                }

                let monster_tile = monster.tile();

                let mut offset = monster_tile - player_tile;
                if offset.right.abs() + offset.down.abs() <= 1 {
                    // This monster's tile is the same or cardinally
                    // adjacent to the player.  No need to do a
                    // collision check, because there are no
                    // intermediate tiles that could contain a block.
                    return true;
                }

                let clear_tiles = opt_clear_tiles
                    .get_or_insert_with(|| room.pathfinding().clear());

                // Iterates through the tiles that must be clear in
                // order to have line-of-sight to the enemy.  Starting
                // at the player's tile, steps along cardinal
                // directions toward the enemy's location.
                let mut iter_tiles =
                    std::iter::from_fn(|| -> Option<Vector<isize>> {
                        (offset != Vector::zero()).then(|| {
                            if offset.down.abs() >= offset.right.abs() {
                                offset.down -= offset.down.signum();
                            } else {
                                offset.right -= offset.right.signum();
                            }

                            monster_tile + offset
                        })
                    });

                iter_tiles.all(|tile| clear_tiles.is_set(tile))
            })
            .map(|character| character.center_pos())
            .map(|pos| (pos - player_pos).max_abs())
            .min_by(|a, b| a.total_cmp(b));
        Ok(opt_dist)
    }

    pub fn can_start_smash(&self, game_state: &GameState) -> bool {
        game_state.player.club_cooldown <= 0
            && self.opt_club(game_state).is_some()
    }

    pub fn should_start_smash(
        &self,
        game_state: &GameState,
    ) -> Result<bool, Error> {
        if !self.can_start_smash(game_state) {
            return Ok(false);
        }

        let enemy_is_nearby =
            if let Some(dist) = self.distance_to_closest_enemy(game_state)? {
                dist < Self::START_SMASH_DISTANCE
            } else {
                false
            };

        if !enemy_is_nearby {
            return Ok(false);
        }

        Ok(true)
    }
}

impl BotGoal for ClubSmashNearby {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        "Smash with club".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        if game_state.player.club_smash_animation_frame.is_some() {
            let should_continue_smash = self
                .distance_to_closest_enemy(game_state)?
                .map(|dist| dist < Self::CONTINUE_SMASH_DISTANCE)
                .unwrap_or(false);

            if should_continue_smash {
                // The player is currently performing a club smash,
                // and there's a nearby enemy.  Damage is dealt on any
                // frame for which either the left mouse button or the
                // 'C' key transition from up to down.  Alternating
                // between the two allows damage to be applied on each
                // frame, rather than every-other frame.
                actions.do_action(if game_state.globals.game_tick % 2 == 0 {
                    GameAction::LeftClick
                } else {
                    GameAction::HoldTool
                });
            } else {
                // The player is currently performing a club smash,
                // but there are no longer any nearby enemies.
                // Therefore, stop the smash.
                actions.do_action(GameAction::AnimationCancel);
            }
            return Ok(BotGoalResult::InProgress);
        }

        if game_state.player.club_cooldown > 0 {
            // The club smash has completed, no further actions
            // required.
            return Ok(BotGoalResult::Completed);
        }

        let Some(club) = self.opt_club(game_state) else {
            // No club, therefore no smash.
            return Ok(BotGoalResult::Completed);
        };

        let select_club = SelectItemGoal::new(club.clone());
        if !select_club.is_completed(game_state) {
            // Select the club in the inventory
            return Ok(select_club.into());
        }

        // All has been checked, time to use the club.  The hurtbox
        // for the club smash doesn't depend on the player's facing
        // direction, but does require the mouse to be in a valid
        // location.  Mousing over the player's tile satisfies this
        // constraint.
        actions.do_action(GameAction::MouseOverTile(game_state.player.tile()));
        actions.do_action(GameAction::RightClick);

        Ok(BotGoalResult::InProgress)
    }
}
