use std::collections::{HashSet, VecDeque};

use geometry::{Rectangle, Vector};
use itertools::Itertools as _;

use crate::{Error, GameStateExt as _, pathfinding::MovementCost};
use game_state::{GameState, ItemId, ObjectKind, ResourceClumpKind, TileMap};

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

        let regions = Self::size_limited_regions(game_state, 200)?;

        let distance_to_water = farm
            .pathfinding(&game_state.statics)
            .ignoring_obstacles()
            .distances(farm.iter_water_tiles().collect::<Vec<_>>().as_slice());

        let num_regions = regions
            .iter()
            .filter_map(|(_, opt_region)| *opt_region)
            .max()
            .map(|max_index| max_index + 1)
            .expect("Map should have at least one region");
        let iter_regions = (0..num_regions)
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
            .map(|(region, _)| region);
        let mut arable_regions = Vec::new();
        let mut all_scarecrows: Vec<Vector<isize>> = farm
            .objects
            .iter()
            .filter(|obj| matches!(obj.kind, ObjectKind::Scarecrow))
            .map(|obj| obj.tile)
            .collect();
        for region in iter_regions {
            let arable_region = ArableRegion::layout(region, &all_scarecrows);
            all_scarecrows = all_scarecrows
                .into_iter()
                .chain(arable_region.scarecrows.iter().cloned())
                .unique()
                .collect();
            arable_regions.push(arable_region);
        }

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
                    .chain(
                        farm.buildings
                            .iter()
                            .filter(|building| building.kind == "Greenhouse")
                            .flat_map(|building| {
                                let top_left = building.shape.top_left;
                                (-1..8).flat_map(move |di| {
                                    (0..8).map(move |dj| {
                                        top_left + Vector::new(di, dj)
                                    })
                                })
                            }),
                    )
                    .chain(farm.diggable.iter_false())
                    .chain(
                        // Two rows below the farmhouse
                        (-5..=4).flat_map(|di| {
                            (3..=4)
                                .map(move |dj| farm_door + Vector::new(di, dj))
                        }),
                    )
                    .chain(
                        // Two columns to the right of the farmhouse
                        (4..=5).flat_map(|di| {
                            (-1..=1)
                                .map(move |dj| farm_door + Vector::new(di, dj))
                        }),
                    ),
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

        let current_axe = game_state.current_axe()?;

        let iter_clumps =
            farm.resource_clumps
                .iter()
                .filter(|clump| match clump.kind {
                    ResourceClumpKind::Stump => {
                        current_axe != Some(&ItemId::COPPER_AXE)
                    }
                    ResourceClumpKind::LargeLog => true,
                    ResourceClumpKind::Boulder
                    | ResourceClumpKind::Meteorite => true,
                    ResourceClumpKind::MineBoulder => true,
                    ResourceClumpKind::GiantCrop(_) => current_axe.is_none(),
                })
                .flat_map(|clump| clump.shape.iter_points());

        iter_fruit_trees.chain(iter_clumps).for_each(|tile| {
            regions[tile] = None;
        });

        Ok(regions)
    }

    fn size_limited_regions(
        game_state: &GameState,
        max_size: usize,
    ) -> Result<TileMap<Option<usize>>, Error> {
        let mut regions = Self::segmented_regions(game_state)?;

        let farm = game_state.get_room("Farm")?;

        let num_regions = regions
            .iter()
            .filter_map(|(_, opt_region)| opt_region.clone())
            .map(|i_region| i_region + 1)
            .max()
            .unwrap_or(0);

        let mut i_next_subregion = num_regions;

        for i_region in 0..num_regions {
            let region =
                regions.map(|opt_region| *opt_region == Some(i_region));

            let num_tiles = region.iter_true().count();
            let num_subregions = num_tiles.div_ceil(max_size);
            if num_subregions == 1 {
                continue;
            }

            let mut seeds = Vec::<Vector<isize>>::new();
            for i in 0..num_subregions {
                let opt_next_seed: Option<Vector<isize>> = if i == 0 {
                    // For seed of first region, pick the tile that is
                    // closest to the farmhouse.
                    let door = game_state.get_farm_door()?;
                    farm.pathfinding(&game_state.statics)
                        .iter_dijkstra(door)
                        .map(|(tile, _)| tile)
                        .find(|tile| region[*tile])
                } else {
                    // After the first seed, pick the tile that is the
                    // furthest from all previous seeds.
                    MovementCost {
                        map: region.map(|b| if *b { Some(0) } else { None }),
                        allow_diagonal: false,
                        include_border: false,
                    }
                    .iter_dijkstra(&seeds)
                    .map(|(tile, _)| tile)
                    .last()
                };

                // Fallback, in case there's no route to the region,
                // just pick an arbitrary tile.
                let next_seed = opt_next_seed.unwrap_or_else(|| {
                    region
                        .iter_true()
                        .find(|tile| seeds.iter().all(|seed| tile != seed))
                        .expect(
                            "For N subregions, there must be at least N tiles",
                        )
                });
                seeds.push(next_seed);
            }

            // Start a subregion for each seed tile.  Iterate over
            // subregions, expanding each subregion by one tile.
            struct Subregion {
                i_subregion: usize,
                frontier: VecDeque<Vector<isize>>,
                seen: TileMap<bool>,
            }
            let mut subregion_map: TileMap<Option<usize>> =
                region.map(|_| None);
            let mut subregion_queue = VecDeque::<Subregion>::new();
            for (i_subregion, seed) in seeds.into_iter().enumerate() {
                subregion_queue.push_back(Subregion {
                    i_subregion,
                    frontier: std::iter::once(seed).collect(),
                    seen: region.map(|_| false),
                })
            }

            while let Some(mut subregion) = subregion_queue.pop_front() {
                while let Some(point) = subregion.frontier.pop_front() {
                    if subregion_map[point].is_none() {
                        // No other subregion has claimed this point,
                        // so it can be added to this subregion.
                        subregion_map[point] = Some(subregion.i_subregion);
                        point
                            //.iter_cardinal()
                            .iter_adjacent()
                            .filter(|adj| {
                                matches!(subregion_map.get(*adj), Some(None))
                            })
                            .filter(|adj| {
                                matches!(region.get(*adj), Some(true))
                            })
                            .for_each(|adj| {
                                if !subregion.seen[adj] {
                                    subregion.seen[adj] = true;
                                    subregion.frontier.push_back(adj);
                                }
                            });

                        subregion_queue.push_back(subregion);
                        break;
                    }
                }
            }

            subregion_map
                .iter()
                .filter_map(|(tile, opt_subregion)| {
                    opt_subregion.map(|subregion| (tile, subregion))
                })
                .map(|(tile, subregion)| {
                    let new_region = if subregion == 0 {
                        // The first subregion just keeps the same id
                        // as it started with.
                        i_region
                    } else {
                        // All other subregions are mapped to new
                        // global region indices.
                        i_next_subregion + (subregion - 1)
                    };
                    (tile, new_region)
                })
                .for_each(|(tile, new_region)| {
                    assert!(
                        regions[tile] == Some(i_region),
                        "Internal error.  \
                         Splitting region into subregions \
                         should only update the value of tiles \
                         that are within the region being split."
                    );
                    regions[tile] = Some(new_region);
                });

            i_next_subregion += num_subregions - 1;
        }

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
    fn layout(
        map: TileMap<bool>,
        current_scarecrows: &[Vector<isize>],
    ) -> Self {
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

            for &prev in current_scarecrows {
                covered.extend(to_cover.extract_if(|b| prev.dist2(*b) < 81));
                if map.is_set(prev) {
                    scarecrows.push(prev);
                }
            }

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
