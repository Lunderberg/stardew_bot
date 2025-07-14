use std::collections::HashSet;

use geometry::{Rectangle, Vector};
use itertools::Itertools as _;

use crate::{bot_logic::GameStateExt as _, Error};
use game_state::{GameState, ObjectKind, TileMap};

use super::LocationExt as _;

pub struct FarmPlan {
    pub tree_farm: Rectangle<isize>,
    pub arable_regions: Vec<ArableRegion>,
}

pub struct ArableRegion {
    /// A map specifying the region within the farm.  Contains true
    /// for locations within the region, and false otherwise.
    pub map: TileMap<bool>,

    /// A list of tiles that may be planted within the region.  Each
    /// plantable tile is within range of a sprinkler and a scarecrow.
    pub plantable: Vec<Vector<isize>>,

    /// A list of tiles that should contain regular sprinklers.
    pub sprinklers: Vec<Vector<isize>>,

    /// A list of tiles that should contain scarecrows.
    pub scarecrows: Vec<Vector<isize>>,
}

impl FarmPlan {
    pub fn plan(game_state: &GameState) -> Result<FarmPlan, Error> {
        let farm = game_state.get_room("Farm")?;
        let farm_door = game_state.get_farm_door()?;

        let regions = Self::segmented_regions(game_state)?;
        let distance_to_water = farm
            .pathfinding()
            .ignoring_obstacles()
            .distances(farm.iter_water_tiles().collect::<Vec<_>>().as_slice());

        let num_regions = regions
            .iter()
            .filter_map(|(_, opt_region)| *opt_region)
            .max()
            .map(|max_index| max_index + 1)
            .expect("Map should have at least one region");
        let arable_regions: Vec<_> = (0..num_regions)
            .map(|i_region| {
                regions.map(|opt_region| opt_region == &Some(i_region))
            })
            .map(|region| {
                let num_tiles = region.iter_true().count();
                let sum_dist = region
                    .iter_true()
                    .filter_map(|tile| distance_to_water.get_opt(tile))
                    .sum::<u64>();

                let avg_dist = (sum_dist as f32) / (num_tiles as f32);
                (region, avg_dist)
            })
            .sorted_by(|(_, a), (_, b)| a.total_cmp(b))
            .map(|(region, _)| region)
            .map(|region| ArableRegion::layout(region))
            .collect();

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

        Ok(Self {
            tree_farm,
            arable_regions,
        })
    }

    fn segmented_regions(
        game_state: &GameState,
    ) -> Result<TileMap<Option<usize>>, Error> {
        let farm = game_state.get_room("Farm")?;
        let farm_door = game_state.get_farm_door()?;

        // Collect a map of borders between arable regions.
        let mut blocked = {
            let blocked = TileMap::collect_true(
                farm.blocked.shape(),
                farm.blocked
                    .iter_true()
                    .chain(farm.iter_water_tiles())
                    .chain(farm.iter_bush_tiles())
                    .chain(farm.iter_building_tiles())
                    .chain(farm.diggable.iter_false()),
            );

            blocked
                .close_over(&TileMap::full(true, 5, 1))
                .close_over(&TileMap::full(true, 1, 5))
        };

        let mut regions: TileMap<Option<usize>> = blocked.map(|_| None);
        let mut region_num = 0usize;

        while let Some(initial) = (farm_door.down..farm.shape.down)
            .map(|j| Vector::new(farm_door.right, j))
            .find(|&tile| regions.is_none(tile) && !blocked.is_set(tile))
            .or_else(|| blocked.iter_false().next())
        {
            let region = blocked.floodfill(initial);
            let num_tiles = region.iter_true().count();

            if num_tiles > 50 {
                for tile in region.iter_true() {
                    regions[tile] = Some(region_num);
                }
                region_num += 1;
            }
            for tile in region.iter_true() {
                blocked[tile] = true;
            }
        }

        // For objects that may be moved later in the game, but not
        // during the initial Spring planning/planting, treat them as
        // clear when determining boundaries between regions, but
        // remove them from the region prior to planning the contents
        // of each region.
        let iter_fruit_trees = farm
            .objects
            .iter()
            .filter(|obj| matches!(obj.kind, ObjectKind::FruitTree(_)))
            .map(|obj| obj.tile);

        let iter_requires_iron_tool = farm
            .resource_clumps
            .iter()
            .flat_map(|clump| clump.shape.iter_points());

        iter_fruit_trees
            .chain(iter_requires_iron_tool)
            .for_each(|tile| {
                regions[tile] = None;
            });

        Ok(regions)
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
}

impl ArableRegion {
    fn layout(map: TileMap<bool>) -> Self {
        let sprinklers: Vec<Vector<isize>> = {
            let mut remaining = map.clone();
            let mut sprinklers = Vec::<Vector<isize>>::new();

            let mut to_visit = Vec::<Vector<isize>>::new();

            while let Some(visiting) = to_visit.pop().or_else(|| {
                remaining.iter_true().find(|tile| {
                    tile.iter_cardinal().all(|adj| remaining.is_set(adj))
                })
            }) {
                sprinklers.push(visiting);
                std::iter::once(visiting)
                    .chain(visiting.iter_cardinal())
                    .for_each(|tile| {
                        remaining[tile] = false;
                    });

                [
                    Vector::<isize>::new(2, -1),
                    Vector::<isize>::new(1, 2),
                    Vector::<isize>::new(-2, 1),
                    Vector::<isize>::new(-1, -2),
                ]
                .into_iter()
                .map(|offset| visiting + offset)
                .for_each(|tile| {
                    let iter_covered =
                        || std::iter::once(tile).chain(tile.iter_cardinal());
                    let is_good_placement =
                        iter_covered().all(|adj| remaining.is_set(adj));
                    if is_good_placement {
                        to_visit.push(tile);
                        iter_covered().for_each(|tile| {
                            remaining[tile] = false;
                        });
                    }
                });
            }

            sprinklers
        };

        let plantable: Vec<Vector<isize>> = sprinklers
            .iter()
            .flat_map(|tile| tile.iter_cardinal())
            .collect();

        let scarecrows: Vec<Vector<isize>> = {
            let mut scarecrows = Vec::<Vector<isize>>::new();
            let mut to_cover: HashSet<Vector<isize>> =
                plantable.iter().cloned().collect();
            let mut covered: HashSet<Vector<isize>> =
                sprinklers.iter().cloned().collect();

            while !to_cover.is_empty() {
                let opt_scarecrow = to_cover
                    .iter()
                    .flat_map(|tile| tile.iter_adjacent())
                    .filter(|&adj| {
                        map.is_set(adj)
                            && !to_cover.contains(&adj)
                            && !covered.contains(&adj)
                    })
                    .max_by_key(|adj| {
                        let num_covered = to_cover
                            .iter()
                            .filter(|b| adj.dist2(**b) < 81)
                            .count();
                        let dist_from_existing = scarecrows
                            .iter()
                            .map(|prev| prev.dist2(*adj))
                            .min();
                        (num_covered, dist_from_existing)
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

        Self {
            map,
            plantable,
            sprinklers,
            scarecrows,
        }
    }
}

impl std::fmt::Display for ArableRegion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut char_map = self.map.map(|b| if *b { '.' } else { ' ' });
        std::iter::empty()
            .chain(self.plantable.iter().map(|tile| (tile, 'o')))
            .chain(self.sprinklers.iter().map(|tile| (tile, '+')))
            .chain(self.scarecrows.iter().map(|tile| (tile, 'S')))
            .for_each(|(tile, c)| {
                char_map[*tile] = c;
            });

        write!(f, "{char_map}")
    }
}
