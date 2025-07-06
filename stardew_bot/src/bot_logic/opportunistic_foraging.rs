use crate::{
    bot_logic::{ActivateTile, ObjectKindExt as _, UseItemOnTile},
    game_state::{Item, ObjectKind, TileMap},
    Error, GameState,
};

use super::bot_logic::{BotGoal, BotInterrupt, LogicStack};

pub struct OpportunisticForaging {
    radius: f32,
}

impl OpportunisticForaging {
    pub fn new(radius: f32) -> Self {
        Self { radius }
    }
}

impl BotInterrupt for OpportunisticForaging {
    fn description(&self) -> std::borrow::Cow<str> {
        format!("Forage within {} tiles", self.radius).into()
    }

    fn check(
        &mut self,
        game_state: &GameState,
    ) -> Result<Option<LogicStack>, Error> {
        let loc = game_state.current_room()?;
        let pos = game_state.player.center_pos();

        let num_empty_slots = game_state.player.inventory.num_empty_slots();
        if num_empty_slots < 2 {
            return Ok(None);
        }

        let has_hoe = game_state
            .player
            .inventory
            .iter_items()
            .any(|item| item.is_same_item(&Item::HOE));
        let can_hoe = has_hoe && game_state.player.current_stamina > 2.0;
        let should_hoe = can_hoe && game_state.player.room_name != "Farm";

        let radius2 = self.radius * self.radius;

        let mut distances: Option<TileMap<Option<u64>>> = None;

        let opt_forageable = loc
            .objects
            .iter()
            .filter(|obj| pos.dist2(obj.tile.into()) < radius2)
            .filter(|obj| match &obj.kind {
                ObjectKind::HoeDirt(hoe_dirt) => {
                    hoe_dirt.has_crop() && should_hoe
                }
                ObjectKind::FruitTree(fruit_tree) => fruit_tree.num_fruit > 0,
                ObjectKind::ArtifactSpot | ObjectKind::SeedSpot => can_hoe,
                ObjectKind::Tree(tree) => tree.has_seed && !tree.is_stump,
                other => other.is_forage(),
            })
            .filter(|obj| {
                if distances.is_none() {
                    distances = Some(
                        loc.pathfinding()
                            .include_border(true)
                            .distances(game_state.player.tile()),
                    );
                }
                let map = distances.as_ref().unwrap();

                if let Some(dist) = map[obj.tile] {
                    (dist as f32) < self.radius
                } else {
                    false
                }
            })
            .next();

        let Some(forageable) = opt_forageable else {
            return Ok(None);
        };

        let opt_tool = match &forageable.kind {
            ObjectKind::Tree(_) => None,
            other => other.get_tool(),
        };

        let interrupt: LogicStack = if let Some(tool) = opt_tool {
            let goal =
                UseItemOnTile::new(tool, loc.name.clone(), forageable.tile);
            goal.into()
        } else {
            let goal = ActivateTile::new(loc.name.clone(), forageable.tile);
            goal.into()
        };

        // During screen transitions, the X/Y position is updated
        // before the current room.  If a memory read occurs between
        // those two updates, then the interrupt may trigger
        // erroneously, thinking that the player is at the new X/Y
        // position within the old room.  This check ensures that when
        // the screen transition completes, the erroneously triggered
        // interrupt will be cancelled.
        let current_room = loc.name.clone();
        let interrupt = interrupt.cancel_if(move |game_state| {
            game_state.player.room_name != current_room
        });

        Ok(Some(interrupt))
    }
}
