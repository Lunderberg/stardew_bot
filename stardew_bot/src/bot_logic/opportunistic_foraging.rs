use crate::{
    bot_logic::{ActivateTile, ObjectKindExt as _, UseItemOnTile},
    game_state::{Item, ObjectKind, TileMap},
    Error, GameState,
};

use super::bot_logic::{BotGoal, BotInterrupt};

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
    ) -> Result<Option<Box<dyn BotGoal>>, Error> {
        let loc = game_state.current_room()?;
        let pos = game_state.player.center_pos();

        let has_hoe = game_state
            .player
            .inventory
            .iter_items()
            .any(|item| item.is_same_item(&Item::HOE));
        let can_hoe = has_hoe && game_state.player.current_stamina > 2.0;
        let should_hoe = can_hoe && game_state.player.room_name != "Farm";

        let radius2 = self.radius * self.radius;

        let mut reachable: Option<TileMap<bool>> = None;

        let opt_forageable = loc
            .objects
            .iter()
            .filter(|obj| pos.dist2(obj.tile.into()) < radius2)
            .filter(|obj| match &obj.kind {
                ObjectKind::HoeDirt(hoe_dirt) => {
                    hoe_dirt.has_crop && should_hoe
                }
                ObjectKind::ArtifactSpot | ObjectKind::SeedSpot => true,
                ObjectKind::Other(name) => match name.as_ref() {
                    "Leek" | "Dandelion" => true,
                    "Daffodil" => true,
                    _ => false,
                },
                _ => false,
            })
            .filter(|obj| {
                if reachable.is_none() {
                    reachable = Some(
                        loc.pathfinding()
                            .include_border(true)
                            .reachable(game_state.player.tile()),
                    );
                }
                reachable.as_ref().unwrap()[obj.tile]
            })
            .next();

        let Some(forageable) = opt_forageable else {
            return Ok(None);
        };

        let interrupt: Box<dyn BotGoal> =
            if let Some(tool) = forageable.kind.get_tool() {
                let goal =
                    UseItemOnTile::new(tool, loc.name.clone(), forageable.tile);
                Box::new(goal)
            } else {
                let goal = ActivateTile::new(loc.name.clone(), forageable.tile);
                Box::new(goal)
            };

        Ok(Some(interrupt))
    }
}
