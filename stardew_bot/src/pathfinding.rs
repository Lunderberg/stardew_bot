use std::fmt::Display;

use dotnet_debugger::{CachedReader, SymbolicGraph};
use itertools::Itertools as _;
use ratatui::{
    layout::Constraint,
    style::Color,
    symbols::Marker,
    text::Text,
    widgets::{
        canvas::{Canvas, Points, Rectangle as CanvasRectangle},
        Block, Cell, Row, Table, Widget as _,
    },
};
use tui_utils::{extensions::SplitRect as _, WidgetWindow};

use crate::Error;

pub struct PathfindingUI {
    current_location: String,
    position: Position,
    locations: Vec<GameLocation>,
}

/// A region of the game.  Contains information extracted from the
/// `StardewValley.GameLocation` objects.
struct GameLocation {
    /// The unique name of the room.  Used as a lookup to refer to
    /// this location.
    name: String,

    /// The size of the room.
    shape: Rectangle,

    /// Tiles that connect to other rooms.  Standing on a warp moves
    /// the player to the room/location specified by the warp.
    warps: Vec<Warp>,

    /// Larger groups of resources.  On the farm, for example, the
    /// stumps/boulders that require iron tools to break.
    resource_clumps: Vec<ResourceClump>,

    /// Bushes, which include decorative (and berry-yielding) bushes,
    /// player-planted tea tree bushes, and walnut bushes.
    bushes: Vec<Bush>,

    /// Trees.  This includes the small 1x1 stumps left behind after
    /// chopping down a tree, but not the larger 2x2 stumps made
    /// during worldgen.
    trees: Vec<Tree>,

    litter: Vec<Litter>,

    /// Which tiles have water.
    water_tiles: Option<Vec<bool>>,
}

struct Warp {
    location: Tile,
    target: Tile,
    target_room: String,
}

struct ResourceClump {
    location: Position,
    shape: Rectangle,
    kind: ResourceClumpKind,
}

enum ResourceClumpKind {
    Stump,
    Boulder,
    Meterorite,
    MineBoulder,
}

struct Bush {
    size: usize,
    position: Position,
}

struct Tree {
    position: Position,
    #[allow(dead_code)]
    kind: TreeKind,
    #[allow(dead_code)]
    growth_stage: i32,
    #[allow(dead_code)]
    has_seed: bool,
    #[allow(dead_code)]
    is_stump: bool,
}

enum TreeKind {
    Oak,
    Maple,
    Pine,
    DesertPalm,
    IslandPalm,
    Mushroom,
    Mahogany,
    Mystic,
    GreenRain,
    // From Stardew Expanded
    Fir,
    // From Stardew Expanded
    Birch,
}

struct Litter {
    position: Position,
    kind: LitterKind,
}

#[derive(PartialEq, Eq)]
enum LitterKind {
    Stone,
    Wood,
    Fiber,
}

struct Rectangle {
    width: usize,
    height: usize,
}

struct Tile {
    right: isize,
    down: isize,
}

#[derive(PartialEq, Clone, Copy)]
struct Position {
    right: f32,
    down: f32,
}

impl PathfindingUI {
    pub fn new(reader: CachedReader) -> Result<Self, Error> {
        let location_list = "StardewValley.Game1.game1\
                             ._locations\
                             .as::<System.Collections.ObjectModel.Collection`1<StardewValley.GameLocation>>()\
                             .items\
                             .as::<\
                               System.Collections.Generic\
                               .List`1<StardewValley.GameLocation>\
                             >()";

        let current_location = "StardewValley.Game1._player\
                                .currentLocationRef\
                                .locationName\
                                .value";

        let player_x = "StardewValley.Game1._player.position.Field.value.X";
        let player_y = "StardewValley.Game1._player.position.Field.value.Y";

        let res = {
            let mut graph = SymbolicGraph::new();
            let outputs = [
                format!("{location_list}._size").as_str(),
                current_location,
                player_x,
                player_y,
            ]
            .into_iter()
            .map(|expr| graph.parse(expr))
            .collect::<Result<Vec<_>, _>>()?;

            let output = graph.tuple(outputs);
            let main_func = graph.function_def(vec![], output);
            graph.name(main_func, "main")?;
            graph.mark_extern_func(main_func)?;

            let vm = graph.compile(reader)?;
            let res = vm.evaluate(reader)?;

            res
        };
        let _num_locations: usize = res.get_as::<usize>(0)?.unwrap();
        let current_location = res.get(1).unwrap().read_string_ptr(&reader)?;
        let player_x: f32 = res.get_as::<f32>(2)?.unwrap();
        let player_y: f32 = res.get_as::<f32>(3)?.unwrap();

        let num_locations = 2;

        /// Helper struct to hold the per-location fields that are
        /// read out for every location.  Fields within a location
        /// that depend on one of these values are read out
        /// later. (e.g. Reading the warps once it is known how many
        /// warps are present.)
        struct StaticLocationFields {
            index: usize,
            name: String,
            shape: Rectangle,
            num_warps: usize,
            num_resource_clumps: usize,
            num_features: usize,
            num_large_features: usize,
            num_objects: usize,
            has_water_tiles: bool,
        }

        const FIELDS_PER_LOCATION: usize = 9;

        let static_location_fields: Vec<StaticLocationFields> = {
            let mut graph = SymbolicGraph::new();

            let location_list =
                graph.parse(&format!("{location_list}._items"))?;
            let outputs: Vec<_> = (0..num_locations)
                .flat_map(|i| {
                    let location = graph.access_index(location_list, i);

                    let name = graph.access_field(location, "name.value");

                    let (width, height) = {
                        let field =
                            graph.access_field(location, "map.m_layers._items");
                        let field = graph.access_index(field, 0);
                        let field = graph.access_field(field, "m_layerSize");

                        let width = graph.access_field(field, "Width");
                        let height = graph.access_field(field, "Height");

                        (width, height)
                    };

                    let num_warps =
                        graph.access_field(location, "warps.count.value");

                    let num_resource_clumps = graph
                        .access_field(location, "resourceClumps.list._size");

                    let num_features = {
                        let entries = graph.access_field(
                            location,
                            "terrainFeatures.dict._entries",
                        );
                        graph.num_array_elements(entries)
                    };

                    let num_large_features = graph.access_field(
                        location,
                        "largeTerrainFeatures.list._size",
                    );

                    let num_objects = {
                        let array = graph.access_field(
                            location,
                            "objects.compositeDict._entries",
                        );
                        graph.num_array_elements(array)
                    };

                    let water_tiles = {
                        let field = graph.access_field(location, "waterTiles");
                        let field =
                            graph.downcast(field, "StardewValley.WaterTiles");
                        graph.access_field(field, "waterTiles")
                    };

                    [
                        name,
                        width,
                        height,
                        num_warps,
                        num_resource_clumps,
                        num_features,
                        num_large_features,
                        num_objects,
                        water_tiles,
                    ]
                })
                .collect();

            let output = graph.tuple(outputs);
            let main_func = graph.function_def(vec![], output);
            graph.name(main_func, "main")?;
            graph.mark_extern_func(main_func)?;

            let vm = graph.compile(reader)?;
            let values = vm.evaluate(reader)?;

            values
                .chunks_exact(FIELDS_PER_LOCATION)
                .enumerate()
                .map(|(index, chunk)| {
                    let mut next = {
                        let mut iter_chunk = chunk.iter();
                        move || iter_chunk.next().unwrap().as_ref()
                    };

                    let name = next().unwrap().read_string_ptr(&reader)?;
                    let shape = {
                        let width = next().unwrap().try_into()?;
                        let height = next().unwrap().try_into()?;
                        Rectangle { width, height }
                    };

                    let num_warps = next().unwrap().try_into()?;
                    let num_resource_clumps = next().unwrap().try_into()?;
                    let num_features = next()
                        .map(|value| value.try_into())
                        .transpose()?
                        .unwrap_or(0);
                    let num_large_features = next().unwrap().try_into()?;

                    let num_objects = next()
                        .map(|value| value.try_into())
                        .transpose()?
                        .unwrap_or(0);

                    let has_water_tiles = next().is_some();

                    Ok(StaticLocationFields {
                        index,
                        name,
                        shape,
                        num_warps,
                        num_resource_clumps,
                        num_features,
                        num_large_features,
                        num_objects,
                        has_water_tiles,
                    })
                })
                .collect::<Result<Vec<_>, Error>>()?
        };

        let mut iter_values_within_location = {
            let mut graph = SymbolicGraph::new();

            let location_list =
                graph.parse(&format!("{location_list}._items"))?;

            let outputs = static_location_fields
                .iter()
                .flat_map(|loc| {
                    let location = graph.access_index(location_list, loc.index);

                    let warps = graph.access_field(
                        location,
                        "warps.array.value.elements._items",
                    );

                    let warp_fields = (0..loc.num_warps)
                        .flat_map(|i_warp| {
                            let warp = {
                                let element = graph.access_index(warps, i_warp);
                                graph.access_field(element, "value")
                            };

                            ["x", "y", "targetX", "targetY", "targetName"]
                                .into_iter()
                                .map(|name| {
                                    let field = graph.access_field(warp, name);
                                    let field =
                                        graph.access_field(field, "value");
                                    field
                                })
                                .collect::<Vec<_>>()
                                .into_iter()
                        })
                        .collect::<Vec<_>>()
                        .into_iter();

                    let resource_clumps = graph
                        .access_field(location, "resourceClumps.list._items");

                    let clump_fields = (0..loc.num_resource_clumps)
                        .flat_map(|i_clump| {
                            let clump =
                                graph.access_index(resource_clumps, i_clump);

                            let right =
                                graph.access_field(clump, "netTile.value.X");
                            let down =
                                graph.access_field(clump, "netTile.value.Y");
                            let width =
                                graph.access_field(clump, "width.value");
                            let height =
                                graph.access_field(clump, "height.value");

                            let kind_index = graph
                                .access_field(clump, "parentSheetIndex.value");

                            [right, down, width, height, kind_index]
                        })
                        .collect::<Vec<_>>()
                        .into_iter();

                    let features = graph.access_field(
                        location,
                        "terrainFeatures.dict._entries",
                    );
                    let feature_fields = (0..loc.num_features)
                        .flat_map(|i| {
                            let feature = graph.access_index(features, i);

                            let right = graph.access_field(feature, "key.X");
                            let down = graph.access_field(feature, "key.Y");

                            let obj =
                                graph.access_field(feature, "value.value");

                            let grass_health = {
                                let grass = graph.downcast(
                                    obj,
                                    "StardewValley.TerrainFeatures.Grass",
                                );
                                graph.access_field(grass, "grassBladeHealth")
                            };

                            let tree_fields = {
                                let tree = graph.downcast(
                                    obj,
                                    "StardewValley.TerrainFeatures.Tree",
                                );
                                // treeType looks like an integer, but has
                                // been converted to a string.
                                let tree_type =
                                    graph.access_field(tree, "treeType.value");
                                let growth_stage = graph
                                    .access_field(tree, "growthStage.value");
                                let has_seed =
                                    graph.access_field(tree, "hasSeed.value");
                                let is_stump =
                                    graph.access_field(tree, "stump.value");
                                [tree_type, growth_stage, has_seed, is_stump]
                            };

                            [right, down, grass_health]
                                .into_iter()
                                .chain(tree_fields)
                        })
                        .collect::<Vec<_>>()
                        .into_iter();

                    let large_features = graph.access_field(
                        location,
                        "largeTerrainFeatures.list._items",
                    );
                    let large_feature_fields = (0..loc.num_large_features)
                        .flat_map(|i| {
                            let feature = {
                                let field =
                                    graph.access_index(large_features, i);
                                graph.downcast(
                                    field,
                                    "StardewValley.TerrainFeatures.Bush",
                                )
                            };
                            let size =
                                graph.access_field(feature, "size.value");
                            let right = graph.access_field(
                                feature,
                                "netTilePosition.value.X",
                            );
                            let down = graph.access_field(
                                feature,
                                "netTilePosition.value.Y",
                            );
                            [size, right, down]
                        })
                        .collect::<Vec<_>>()
                        .into_iter();

                    let objects = graph.access_field(
                        location,
                        "objects.compositeDict._entries",
                    );
                    let object_fields = (0..loc.num_objects)
                        .flat_map(|i| {
                            let object = graph.access_index(objects, i);

                            let right = graph.access_field(
                                object,
                                "value.tileLocation.value.X",
                            );
                            let down = graph.access_field(
                                object,
                                "value.tileLocation.value.Y",
                            );

                            let category = graph
                                .access_field(object, "value.category.value");
                            let name = graph
                                .access_field(object, "value.netName.value");

                            [right, down, category, name]
                        })
                        .collect::<Vec<_>>()
                        .into_iter();

                    let water_tiles =
                        graph.access_field(location, "waterTiles.waterTiles");
                    let water_tile_fields = (0..loc.shape.width)
                        .cartesian_product(0..loc.shape.height)
                        .filter(|_| loc.has_water_tiles)
                        .map(|(i, j)| {
                            let element =
                                graph.access_indices(water_tiles, vec![i, j]);
                            let element =
                                graph.access_field(element, "isWater");
                            element
                        });

                    std::iter::empty()
                        .chain(warp_fields)
                        .chain(clump_fields)
                        .chain(feature_fields)
                        .chain(large_feature_fields)
                        .chain(object_fields)
                        .chain(water_tile_fields)
                        .collect::<Vec<_>>()
                        .into_iter()
                })
                .collect();

            let output = graph.tuple(outputs);
            let main_func = graph.function_def(vec![], output);
            graph.name(main_func, "main")?;
            graph.mark_extern_func(main_func)?;

            let vm = graph.compile(reader)?;
            let values = vm.evaluate(reader)?;
            values.into_iter()
        };

        let mut next_value = || {
            iter_values_within_location.next().expect(
                "Should have generated sufficient values for all fields",
            )
        };

        let locations = static_location_fields
            .into_iter()
            .map(|loc| -> Result<_, Error> {
                let warps = (0..loc.num_warps)
                    .map(|_| -> Result<_, Error> {
                        let x = next_value().unwrap();
                        let y = next_value().unwrap();
                        let target_x = next_value().unwrap();
                        let target_y = next_value().unwrap();
                        let target_name = next_value().unwrap();
                        let location = {
                            let right = x.try_into()?;
                            let down = y.try_into()?;
                            Tile { right, down }
                        };
                        let target = {
                            let right = target_x.try_into()?;
                            let down = target_y.try_into()?;
                            Tile { right, down }
                        };
                        let target_room =
                            target_name.read_string_ptr(&reader)?;
                        Ok(Warp {
                            location,
                            target,
                            target_room,
                        })
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                let resource_clumps = (0..loc.num_resource_clumps)
                    .map(|_| -> Result<_, Error> {
                        let location = {
                            let right = next_value().unwrap().try_into()?;
                            let down = next_value().unwrap().try_into()?;
                            Position { right, down }
                        };
                        let shape = {
                            let width = next_value().unwrap().try_into()?;
                            let height = next_value().unwrap().try_into()?;
                            Rectangle { width, height }
                        };

                        let kind: ResourceClumpKind = {
                            let index: i32 =
                                next_value().unwrap().try_into()?;
                            index.try_into()?
                        };

                        Ok(ResourceClump {
                            location,
                            shape,
                            kind,
                        })
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                let mut trees = Vec::new();
                let mut grass = Vec::new();
                for _ in 0..loc.num_features {
                    let position = {
                        let right = next_value().unwrap().try_into()?;
                        let down = next_value().unwrap().try_into()?;
                        Position { right, down }
                    };
                    let grass_health: Option<i32> =
                        next_value().map(TryInto::try_into).transpose()?;
                    let tree = {
                        let tree_type = next_value();
                        let growth_stage = next_value();
                        let has_seed = next_value();
                        let is_stump = next_value();
                        if tree_type.is_some() {
                            Some(Tree {
                                position,
                                kind: tree_type
                                    .unwrap()
                                    .read_string_ptr(&reader)?
                                    .parse()?,
                                growth_stage: growth_stage
                                    .unwrap()
                                    .try_into()?,
                                has_seed: has_seed.unwrap().try_into()?,
                                is_stump: is_stump.unwrap().try_into()?,
                            })
                        } else {
                            None
                        }
                    };

                    if let Some(tree) = tree {
                        trees.push(tree);
                    }
                    if let Some(_) = grass_health {
                        grass.push(position);
                    }
                }

                let bushes = (0..loc.num_large_features)
                    .filter_map(|_| {
                        let size = next_value();
                        let right = next_value();
                        let down = next_value();

                        let size = match size?.try_into() {
                            Ok(val) => val,
                            Err(err) => {
                                return Some(Err(err));
                            }
                        };
                        let location = {
                            let right = match right?.try_into() {
                                Ok(val) => val,
                                Err(err) => {
                                    return Some(Err(err));
                                }
                            };
                            let down = match down?.try_into() {
                                Ok(val) => val,
                                Err(err) => {
                                    return Some(Err(err));
                                }
                            };
                            Position { right, down }
                        };
                        Some(Ok(Bush {
                            size,
                            position: location,
                        }))
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                let mut litter = Vec::new();
                for _ in 0..loc.num_objects {
                    let right = next_value();
                    let down = next_value();
                    let category = next_value();
                    let name = next_value();
                    let opt_litter = category
                        .map(|value| value.try_into())
                        .transpose()?
                        .filter(|category: &isize| *category == -999)
                        .map(|_| -> Result<_, Error> {
                            let name =
                                name.unwrap().read_string_ptr(&reader)?;
                            let kind = if name == "Twig" {
                                Some(LitterKind::Wood)
                            } else if name == "Stone" {
                                Some(LitterKind::Stone)
                            } else if name.to_lowercase().contains("weeds") {
                                Some(LitterKind::Fiber)
                            } else {
                                None
                            };
                            Ok(kind)
                        })
                        .transpose()?
                        .flatten()
                        .map(|kind| -> Result<_, Error> {
                            let position = {
                                let right = right.unwrap().try_into()?;
                                let down = down.unwrap().try_into()?;
                                Position { right, down }
                            };
                            Ok(Litter { position, kind })
                        })
                        .transpose()?;
                    if let Some(obj) = opt_litter {
                        litter.push(obj);
                    }
                }

                let water_tiles = loc
                    .has_water_tiles
                    .then(|| -> Result<_, Error> {
                        let is_water = (0..loc.shape.width)
                            .cartesian_product(0..loc.shape.height)
                            .map(|(_i, _j)| next_value().unwrap().try_into())
                            .collect::<Result<Vec<_>, _>>()?;
                        Ok(is_water)
                    })
                    .transpose()?;

                Ok(GameLocation {
                    name: loc.name,
                    shape: loc.shape,
                    warps,
                    resource_clumps,
                    bushes,
                    trees,
                    litter,
                    water_tiles,
                })
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Self {
            locations,
            current_location,
            position: Position {
                right: player_x,
                down: player_y,
            },
        })
    }
}

impl WidgetWindow<Error> for PathfindingUI {
    fn title(&self) -> std::borrow::Cow<str> {
        "Pathfinding".into()
    }

    fn draw<'a>(
        &'a mut self,
        _globals: &'a tui_utils::TuiGlobals,
        area: ratatui::layout::Rect,
        buf: &mut ratatui::prelude::Buffer,
    ) {
        let (left_column, draw_area) = area.split_from_left(30);

        let (top_area, table_area) = left_column.split_from_top(2);

        let longest_name = self
            .locations
            .iter()
            .map(|loc| loc.name.len())
            .max()
            .unwrap_or(0);

        let loc_rows = self.locations.iter().map(|loc| {
            let name = Cell::new(loc.name.as_str());
            let shape = Cell::new(format!(
                "({}, {})",
                loc.shape.width, loc.shape.height
            ));
            Row::new([name, shape])
        });

        let water_rows = self.locations.iter().flat_map(|loc| {
            let name = Cell::new(loc.name.as_str());
            loc.water_tiles
                .as_ref()
                .into_iter()
                .flat_map(|is_water| {
                    let Rectangle { width, height } = loc.shape;
                    assert!(width * height == is_water.len());
                    is_water
                        .iter()
                        .enumerate()
                        .filter(|(_, is_water)| **is_water)
                        .map(move |(index, _)| {
                            let i = index / width;
                            let j = index % width;
                            Cell::new(format!(
                                "({i},{j}) of ({width},{height})"
                            ))
                        })
                })
                .map(move |right| Row::new([name.clone(), right]))
        });

        let warp_rows = self.locations.iter().flat_map(|loc| {
            let name = Cell::new(loc.name.as_str());

            loc.warps.iter().flat_map(move |warp| {
                [
                    Row::new([
                        name.clone(),
                        Cell::new("x"),
                        Cell::new(format!("{}", warp.location.right)),
                    ]),
                    Row::new([
                        name.clone(),
                        Cell::new("y"),
                        Cell::new(format!("{}", warp.location.down)),
                    ]),
                    Row::new([
                        name.clone(),
                        Cell::new("target_x"),
                        Cell::new(format!("{}", warp.target.right)),
                    ]),
                    Row::new([
                        name.clone(),
                        Cell::new("target_y"),
                        Cell::new(format!("{}", warp.target.down)),
                    ]),
                    Row::new([
                        name.clone(),
                        Cell::new("target_name"),
                        Cell::new(format!("{}", warp.target_room)),
                    ]),
                ]
            })
        });

        let rows = std::iter::empty()
            .chain(loc_rows)
            .chain(water_rows)
            .chain(warp_rows);

        let table = Table::new(
            rows,
            [
                Constraint::Min(longest_name as u16),
                Constraint::Min(10),
                Constraint::Min(longest_name as u16),
                Constraint::Percentage(100),
            ],
        );

        table.render(table_area, buf);

        let top_text = Text::raw(format!(
            "Current: {} ({}, {}) ({}, {})",
            self.current_location,
            self.position.right,
            self.position.down,
            self.position.right / 64.0,
            self.position.down / 64.0,
        ));
        top_text.render(top_area, buf);

        let current_room = self
            .locations
            .iter()
            .find(|loc| loc.name == self.current_location);
        if let Some(loc) = current_room {
            Canvas::default()
                .block(Block::new().title(loc.name.as_ref()))
                .marker(Marker::HalfBlock)
                .x_bounds([0.0, loc.shape.width as f64])
                .y_bounds([0.0, loc.shape.height as f64])
                .paint(|ctx| {
                    if let Some(tiles) = &loc.water_tiles {
                        let Rectangle { width, height } = loc.shape;
                        assert!(width * height == tiles.len());
                        let points = tiles
                            .iter()
                            .enumerate()
                            .filter(|(_, is_water)| **is_water)
                            .map(|(index, _)| {
                                let i = index / height;
                                let j = index % height;
                                let x = i as f64;
                                let y = (height - j) as f64;
                                (x, y)
                            })
                            .collect::<Vec<_>>();

                        ctx.draw(&Points {
                            coords: &points,
                            color: Color::Blue,
                        });
                    }

                    loc.resource_clumps
                        .iter()
                        .map(|clump| CanvasRectangle {
                            x: clump.location.right as f64,
                            y: (loc.shape.height as f64)
                                - (clump.location.down as f64),
                            width: clump.shape.width as f64,
                            height: clump.shape.height as f64,
                            color: match clump.kind {
                                ResourceClumpKind::Stump => Color::Yellow,
                                ResourceClumpKind::Boulder => Color::DarkGray,
                                ResourceClumpKind::Meterorite => Color::Magenta,
                                ResourceClumpKind::MineBoulder => {
                                    Color::DarkGray
                                }
                            },
                        })
                        .for_each(|rect| ctx.draw(&rect));

                    loc.bushes
                        .iter()
                        .map(|bush| CanvasRectangle {
                            x: bush.position.right as f64,
                            y: (loc.shape.height as f64)
                                - (bush.position.down as f64),
                            width: bush.width() as f64,
                            height: 1.0,
                            color: Color::Green,
                        })
                        .for_each(|rect| ctx.draw(&rect));

                    {
                        let trees = loc
                            .trees
                            .iter()
                            .map(|tree| {
                                (
                                    tree.position.right as f64,
                                    (loc.shape.height as f64)
                                        - (tree.position.down as f64),
                                )
                            })
                            .collect::<Vec<_>>();
                        ctx.draw(&Points {
                            coords: &trees,
                            color: Color::Rgb(133, 74, 5),
                        });
                    }

                    for (litter_kind, color) in [
                        (LitterKind::Stone, Color::DarkGray),
                        (LitterKind::Wood, Color::Rgb(97, 25, 0)),
                        (LitterKind::Fiber, Color::LightGreen),
                    ] {
                        let litter = loc
                            .litter
                            .iter()
                            .filter(|obj| obj.kind == litter_kind)
                            .map(|obj| obj.position)
                            .map(|pos| {
                                (
                                    pos.right as f64,
                                    (loc.shape.height as f64)
                                        - (pos.down as f64),
                                )
                            })
                            .collect::<Vec<_>>();
                        ctx.draw(&Points {
                            coords: &litter,
                            color,
                        });
                    }
                })
                .render(draw_area, buf);
        }
    }
}

impl Bush {
    fn width(&self) -> usize {
        match self.size {
            0 => 1, // Small bush, 1x1
            1 => 2, // Medium bush, 1x2
            2 => 3, // Large bush, 1x3
            3 => 1, // Green tea bush, 1x1
            4 => 3, // Walnut bush, 1x3
            _ => 0, // Should be unreachable
        }
    }
}

impl TryFrom<i32> for ResourceClumpKind {
    type Error = Error;

    fn try_from(value: i32) -> Result<Self, Self::Error> {
        match value {
            600 | 602 => Ok(Self::Stump),
            672 => Ok(Self::Boulder),
            622 => Ok(Self::Meterorite),
            752 | 754 | 756 | 758 => Ok(Self::MineBoulder),

            other => Err(Error::UnrecognizedResourceClump(other)),
        }
    }
}

impl std::str::FromStr for TreeKind {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "1" => Ok(Self::Oak),
            "2" => Ok(Self::Maple),
            "3" => Ok(Self::Pine),
            "6" => Ok(Self::DesertPalm),
            "9" => Ok(Self::IslandPalm),
            "7" => Ok(Self::Mushroom),
            "8" => Ok(Self::Mahogany),
            "13" => Ok(Self::Mystic),
            "10" | "11" | "12" => Ok(Self::GreenRain),
            "4" | "5" => panic!(
                "Are the 'winterTree1' and 'winterTree2' values \
                 actually used?"
            ),

            "FlashShifter.StardewValleyExpandedCP_Fir_Tree" => Ok(Self::Fir),
            "FlashShifter.StardewValleyExpandedCP_Birch_Tree" => {
                Ok(Self::Birch)
            }
            other => Err(Error::UnrecognizedTreeKind(other.to_string())),
        }
    }
}

impl Display for TreeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TreeKind::Oak => write!(f, "Oak"),
            TreeKind::Maple => write!(f, "Maple"),
            TreeKind::Pine => write!(f, "Pine"),
            TreeKind::DesertPalm => write!(f, "DesertPalm"),
            TreeKind::IslandPalm => write!(f, "IslandPalm"),
            TreeKind::Mushroom => write!(f, "Mushroom"),
            TreeKind::Mahogany => write!(f, "Mahogany"),
            TreeKind::Mystic => write!(f, "Mystic"),
            TreeKind::GreenRain => write!(f, "GreenRain"),
            TreeKind::Fir => write!(f, "Fir"),
            TreeKind::Birch => write!(f, "Birch"),
        }
    }
}
