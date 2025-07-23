use crate::{ActivateTile, Error, FarmPlan, UseItemOnTile};
use game_state::{GameState, ItemId, ObjectKind, TileMap};
use geometry::Vector;

use super::{
    bot_logic::{BotInterrupt, LogicStack},
    LocationExt as _,
};

pub struct OpportunisticForaging {
    radius: f32,
    min_empty_slots: usize,
    plan: Option<FarmPlan>,
}

impl OpportunisticForaging {
    pub fn new(radius: f32) -> Self {
        Self {
            radius,
            min_empty_slots: 2,
            plan: None,
        }
    }

    pub fn min_empty_slots(self, min_empty_slots: usize) -> Self {
        Self {
            min_empty_slots,
            ..self
        }
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

        if self.plan.is_none() {
            self.plan = Some(FarmPlan::plan(game_state)?);
        }
        let plan = self.plan.as_ref().unwrap();

        let num_empty_slots = game_state.player.inventory.num_empty_slots();
        if num_empty_slots < self.min_empty_slots {
            return Ok(None);
        }

        let has_hoe = game_state
            .player
            .inventory
            .iter_items()
            .any(|item| item == &ItemId::HOE);
        let can_hoe = has_hoe && game_state.player.current_stamina > 2.0;
        let should_hoe = can_hoe && game_state.player.room_name != "Farm";

        let radius2 = self.radius * self.radius;

        let mut distances: Option<TileMap<Option<u64>>> = None;

        let opt_forageable: Option<(Vector<isize>, Option<ItemId>)> = loc
            .objects
            .iter()
            .filter(|obj| pos.dist2(obj.tile.into()) < radius2)
            .filter_map(|obj| {
                let opt_tool = match &obj.kind {
                    ObjectKind::HoeDirt(hoe_dirt)
                        if hoe_dirt.has_crop() && should_hoe =>
                    {
                        Some(ItemId::HOE)
                    }
                    ObjectKind::FruitTree(fruit_tree)
                        if fruit_tree.num_fruit > 0 =>
                    {
                        None
                    }
                    ObjectKind::ArtifactSpot | ObjectKind::SeedSpot
                        if can_hoe =>
                    {
                        Some(ItemId::HOE)
                    }
                    ObjectKind::Tree(tree)
                        if tree.has_seed
                            && !tree.is_stump
                            && game_state.player.skills.foraging_level()
                                >= 1 =>
                    {
                        // Shake the tree to get a seed from it.
                        None
                    }
                    ObjectKind::Tree(tree)
                        if tree.growth_stage == 0
                            && !plan.is_planned_tree(obj.tile)
                            && can_hoe =>
                    {
                        Some(ItemId::HOE)
                    }
                    other if other.is_forage() => None,
                    _ => {
                        return None;
                    }
                };
                Some((obj.tile, opt_tool))
            })
            .filter(|(tile, _)| {
                if distances.is_none() {
                    distances = Some(
                        loc.pathfinding(&game_state.statics)
                            .include_border(true)
                            .distances(game_state.player.tile()),
                    );
                }
                let map = distances.as_ref().unwrap();

                map.get_opt(*tile)
                    .map(|dist| (*dist as f32) < self.radius)
                    .unwrap_or(false)
            })
            .next();

        let Some((tile, opt_tool)) = opt_forageable else {
            return Ok(None);
        };

        let interrupt: LogicStack = if let Some(tool) = opt_tool {
            let goal = UseItemOnTile::new(tool, loc.name.clone(), tile);
            goal.into()
        } else {
            let goal = ActivateTile::new(loc.name.clone(), tile);
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
