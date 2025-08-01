use geometry::Vector;
use itertools::Itertools as _;

use crate::{
    bot_logic::LogicStack, ActivateTile, Error, InventoryGoal, UseItemOnTile,
};
use game_state::{GameState, ItemId, ObjectKind, TileMap};

use super::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult},
    FarmPlan, GameStateExt as _, LocationExt as _, MaintainStaminaGoal,
};

pub struct ExpandTreeFarm;

impl ExpandTreeFarm {
    pub fn new() -> Self {
        Self
    }

    const SEED_TYPES: [ItemId; 3] =
        [ItemId::OAK_SEED, ItemId::MAPLE_SEED, ItemId::PINE_SEED];

    fn next_step(
        &self,
        game_state: &GameState,
    ) -> Result<Option<(Vector<isize>, Option<ItemId>)>, Error> {
        let opt_axe = game_state.current_axe()?;

        let plan = FarmPlan::plan(game_state)?;
        let farm = game_state.get_room("Farm")?;

        let initial_tile = if game_state.player.room_name == "Farm" {
            game_state.player.tile()
        } else {
            game_state.closest_entrance("Farm")?
        };
        let distances = farm
            .pathfinding(&game_state.statics)
            .include_border(true)
            .distances(initial_tile);
        let clear_tiles = farm.pathfinding(&game_state.statics).clear();

        let opt_tree_seed = game_state
            .player
            .inventory
            .iter_items()
            .map(|item| &item.id)
            .find(|id| Self::SEED_TYPES.iter().any(|seed| id == &seed));

        let current_trees = TileMap::collect_true(
            distances.shape(),
            farm.objects
                .iter()
                .filter(|obj| matches!(obj.kind, ObjectKind::Tree(_)))
                .map(|obj| obj.tile),
        );

        let iter_plant_tree = plan
            .iter_planned_trees()
            .filter(|&tile| !current_trees.is_set(tile))
            .filter(|&tile| clear_tiles.is_set(tile))
            .filter_map(|tile| {
                opt_tree_seed.map(|seed| (tile, Some(seed.clone())))
            });

        let iter_seeds_to_get = farm
            .objects
            .iter()
            .filter_map(|obj| {
                let opt_tool = match &obj.kind {
                    ObjectKind::Tree(tree)
                        if tree.growth_stage == 0 && opt_axe.is_some() =>
                    {
                        opt_axe.cloned()
                    }
                    ObjectKind::Tree(tree)
                        if tree.has_seed && !tree.is_stump =>
                    {
                        None
                    }

                    _ => {
                        return None;
                    }
                };
                Some((obj.tile, opt_tool))
            })
            .filter(|(tile, opt_tool)| {
                let is_tree_farm_tile = plan.is_planned_tree(*tile);

                // If a seed is already planted in a location that
                // will become the tree farm, let it remain where it
                // is.
                let should_collect = opt_tool.is_none() || !is_tree_farm_tile;
                should_collect
            });

        let iter_clear_adj_trees = plan
            .iter_planned_trees()
            .filter(|_| opt_axe.is_some())
            .filter(|tile| current_trees.is_set(*tile))
            .flat_map(|tile| tile.iter_adjacent())
            .unique()
            .filter(|adj| current_trees.is_set(*adj))
            .map(|adj| (adj, opt_axe.cloned()));

        let opt_next_step =
            std::iter::empty::<(Vector<isize>, Option<ItemId>)>()
                .chain(iter_plant_tree)
                .chain(iter_seeds_to_get)
                .chain(iter_clear_adj_trees)
                .filter(|(tile, _)| distances.is_some(*tile))
                .min_by_key(|(tile, opt_item)| {
                    let dist = distances.get_opt(*tile).cloned().expect(
                        "Protected by earlier distances.is_some() check",
                    );

                    let is_planting = opt_item
                        .as_ref()
                        .map(|item| item.item_id.starts_with("(O)"))
                        .unwrap_or(false);

                    (is_planting, dist)
                });

        Ok(opt_next_step)
    }
}

impl BotGoal for ExpandTreeFarm {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        "Expand Tree Farm".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        _actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        let Some((tile, opt_item)) = self.next_step(game_state)? else {
            return Ok(BotGoalResult::Completed);
        };

        let goal = InventoryGoal::current()
            .with(
                Self::SEED_TYPES
                    .iter()
                    .map(|seed| seed.clone().with_count(100)),
            )
            .with(ItemId::HOE)
            .with(game_state.current_axe()?.cloned());

        let inventory = &game_state.player.inventory;
        if !goal.is_completed(game_state)? || inventory.num_empty_slots() < 4 {
            let goal = goal.otherwise_empty().stamina_recovery_slots(2);
            return Ok(goal.into());
        }

        let goal = MaintainStaminaGoal::new();
        if !goal.is_completed(game_state)? {
            return Ok(goal.into());
        }
        if game_state.player.current_stamina < 2.0 {
            return Ok(BotGoalResult::Completed);
        }

        if let Some(item) = opt_item {
            let num_objects = game_state
                .get_room("Farm")
                .map(|loc| loc.objects.len())
                .unwrap_or(0);
            let stack = LogicStack::new()
                .then(UseItemOnTile::new(item.clone(), "Farm", tile))
                .cancel_if(move |game_state| {
                    let current_num_objects = game_state
                        .get_room("Farm")
                        .map(|loc| loc.objects.len())
                        .unwrap_or(0);
                    current_num_objects != num_objects
                });
            Ok(stack.into())
        } else {
            let goal = ActivateTile::new("Farm", tile);
            Ok(goal.into())
        }
    }
}
