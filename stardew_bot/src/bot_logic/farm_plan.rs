use std::collections::HashSet;

use itertools::Itertools as _;

use crate::{
    bot_logic::GameStateExt as _,
    game_state::{ObjectKind, Rectangle, Vector},
    Error, GameState,
};

pub struct FarmPlan {
    initial_plot: Rectangle<isize>,
    tree_farm: Rectangle<isize>,
    regular_sprinklers: Vec<Vector<isize>>,
    scarecrows: Vec<Vector<isize>>,
}

impl FarmPlan {
    pub fn plan(game_state: &GameState) -> Result<FarmPlan, Error> {
        let farm = game_state.get_room("Farm")?;
        let farm_door = game_state.get_farm_door()?;

        let initial_plot = {
            let top_right = farm_door + Vector::new(3, 5);
            // 60 seeds for the first day, plus an extra row for any
            // carrot seeds.
            let num_columns = 11;
            let num_rows = 6;
            Rectangle::new(
                top_right - Vector::new(num_columns - 1, 0),
                Vector::new(num_columns, num_rows),
            )
        };

        let tree_farm = {
            let tree_top_right = initial_plot.top_right() + Vector::new(-1, 12);

            let num_tree_rows = 10;
            let num_tree_columns = 10;

            let shape =
                Vector::new(num_tree_columns * 2 - 1, num_tree_rows * 2 - 1);

            Rectangle::new(
                tree_top_right - Vector::new(shape.right - 1, 0),
                shape,
            )
        };

        let walkable = farm
            .pathfinding()
            .stone_clearing_cost(0)
            .wood_clearing_cost(0)
            .breakable_clearing_cost(0)
            .forage_clearing_cost(0)
            .tree_clearing_cost(0)
            .walkable();
        let diggable = &farm.diggable;

        let regular_sprinklers = {
            const NUM_SPRINKLERS: usize = 50;

            // Off to the bottom/left, so that barns/coops can be
            // closer to the farm house.
            let starting_location = farm_door + Vector::new(-44, 31);

            let mut sprinklers: Vec<Vector<isize>> = Default::default();

            let mut seen: HashSet<Vector<isize>> = Default::default();
            let mut to_visit: HashSet<Vector<isize>> = Default::default();

            for di in -1..=1 {
                for dj in -1..=1 {
                    let loc = starting_location
                        + Vector::<isize>::new(2, -1) * di
                        + Vector::<isize>::new(1, 2) * dj;
                    seen.insert(loc);
                    to_visit.insert(loc);
                }
            }

            while sprinklers.len() < NUM_SPRINKLERS && !to_visit.is_empty() {
                let visiting = to_visit
                    .iter()
                    .cloned()
                    .min_by_key(|tile| {
                        let dist = tile.manhattan_dist(starting_location);
                        (dist, tile.right, std::cmp::Reverse(tile.down))
                    })
                    .expect("Guarded by to_visit.is_some()");
                to_visit.remove(&visiting);

                let is_good_placement = std::iter::once(visiting)
                    .chain(visiting.iter_cardinal())
                    .all(|adj| walkable.is_set(adj) && diggable.is_set(adj));
                if is_good_placement {
                    sprinklers.push(visiting);

                    [
                        Vector::<isize>::new(2, -1),
                        Vector::<isize>::new(1, 2),
                        Vector::<isize>::new(-2, 1),
                        Vector::<isize>::new(-1, -2),
                    ]
                    .into_iter()
                    .map(|offset| visiting + offset)
                    .for_each(|tile| {
                        if !seen.contains(&tile) {
                            seen.insert(tile);
                            to_visit.insert(tile);
                        }
                    });
                }
            }

            sprinklers
        };

        let scarecrows = {
            let allowed = {
                let mut map = walkable.clone();
                regular_sprinklers
                    .iter()
                    .flat_map(|tile| {
                        std::iter::once(*tile).chain(tile.iter_cardinal())
                    })
                    .for_each(|tile| {
                        if map.in_bounds(tile) {
                            map[tile] = false;
                        }
                    });
                map
            };

            let mut scarecrows = Vec::<Vector<isize>>::new();
            let mut to_cover: HashSet<Vector<isize>> = regular_sprinklers
                .iter()
                .flat_map(|tile| tile.iter_cardinal())
                .collect();
            let mut covered = HashSet::<Vector<isize>>::new();

            // The farm may contain existing scarecrows.
            farm.objects
                .iter()
                .filter(|obj| matches!(obj.kind, ObjectKind::Scarecrow))
                .map(|obj| obj.tile)
                .for_each(|scarecrow| {
                    covered.extend(
                        to_cover.extract_if(|b| scarecrow.dist2(*b) < 81),
                    );
                });

            while !to_cover.is_empty() {
                let opt_scarecrow = to_cover
                    .iter()
                    .flat_map(|tile| tile.iter_adjacent())
                    .filter(|&adj| {
                        allowed.is_set(adj)
                            && !to_cover.contains(&adj)
                            && !covered.contains(&adj)
                    })
                    .max_by_key(|adj| {
                        let num_covered = to_cover
                            .iter()
                            .filter(|b| adj.dist2(**b) < 81)
                            .count();
                        num_covered
                    });
                let Some(scarecrow) = opt_scarecrow else {
                    break;
                };

                scarecrows.push(scarecrow);
                covered
                    .extend(to_cover.extract_if(|b| scarecrow.dist2(*b) < 81));
            }

            scarecrows
        };

        Ok(Self {
            initial_plot,
            tree_farm,
            regular_sprinklers,
            scarecrows,
        })
    }

    pub fn is_planned_tree(&self, tile: Vector<isize>) -> bool {
        self.tree_farm.contains(tile)
            && (self.tree_farm.top_left - tile).map(|x| x % 2) == Vector::zero()
    }

    pub fn iter_planned_trees(&self) -> impl Iterator<Item = Vector<isize>> {
        let shape = self.tree_farm.shape;
        let top_left = self.tree_farm.top_left;
        (0..shape.right)
            .step_by(2)
            .rev()
            .flat_map(move |di| {
                (0..shape.down)
                    .step_by(2)
                    .map(move |dj| Vector::new(di, dj))
            })
            .map(move |offset| top_left + offset)
    }

    pub fn iter_initial_plot(
        &self,
    ) -> impl Iterator<Item = Vector<isize>> + '_ {
        self.initial_plot.iter_points()
    }

    pub fn iter_regular_sprinklers(
        &self,
    ) -> impl Iterator<Item = Vector<isize>> + '_ {
        self.regular_sprinklers.iter().cloned()
    }

    pub fn iter_sprinkler_plot(
        &self,
    ) -> impl Iterator<Item = Vector<isize>> + '_ {
        self.regular_sprinklers
            .iter()
            .flat_map(|sprinkler| sprinkler.iter_cardinal())
    }

    pub fn iter_scarecrows(&self) -> impl Iterator<Item = Vector<isize>> + '_ {
        self.scarecrows.iter().cloned()
    }
}
