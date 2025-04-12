use std::fmt::Display;

use dotnet_debugger::{CachedReader, RustNativeObject, SymbolicGraph};
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
#[derive(RustNativeObject, Clone, Debug)]
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

    grass: Vec<Position>,

    litter: Vec<Litter>,

    /// Which tiles have water.
    water_tiles: Option<Vec<bool>>,
}

#[derive(RustNativeObject, Debug, Clone)]
struct Warp {
    location: Tile,
    target: Tile,
    target_room: String,
}

#[derive(RustNativeObject, Debug, Clone)]
struct ResourceClump {
    location: Position,
    shape: Rectangle,
    kind: ResourceClumpKind,
}

#[derive(RustNativeObject, Debug, Clone, Copy)]
enum ResourceClumpKind {
    Stump,
    Boulder,
    Meterorite,
    MineBoulder,
}

#[derive(RustNativeObject, Debug, Clone)]
struct Bush {
    size: usize,
    position: Position,
}

#[derive(RustNativeObject, Debug, Clone)]
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

#[derive(RustNativeObject, Debug, Clone)]
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

#[derive(RustNativeObject, Debug, Clone)]
struct Litter {
    position: Position,
    kind: LitterKind,
}

#[derive(RustNativeObject, Debug, Clone, PartialEq, Eq)]
enum LitterKind {
    Stone,
    Wood,
    Fiber,
}

#[derive(RustNativeObject, Debug, Clone)]
struct Rectangle {
    width: usize,
    height: usize,
}

#[derive(RustNativeObject, Debug, Clone)]
struct Tile {
    right: isize,
    down: isize,
}

#[derive(RustNativeObject, Debug, PartialEq, Clone, Copy)]
struct Position {
    right: f32,
    down: f32,
}


impl PathfindingUI {
    pub fn new(reader: CachedReader) -> Result<Self, Error> {
        let mut graph = SymbolicGraph::new();

        let location_list = graph.parse(
            "StardewValley.Game1.game1\
                 ._locations\
                 .as::<
                     System.Collections.ObjectModel
                     .Collection`1<StardewValley.GameLocation>
                 >()\
                 .items\
                 .as::<\
                   System.Collections.Generic\
                   .List`1<StardewValley.GameLocation>\
                 >()",
        )?;
        graph.name(location_list, "location_list")?;

        let new_tile = graph
            .native_function(|right: isize, down: isize| Tile { right, down });
        graph.name(new_tile, "new_tile")?;

        let new_position = graph
            .native_function(|right: f32, down: f32| Position { right, down });
        graph.name(new_position, "new_position")?;

        let new_rectangle =
            graph.native_function(|width: usize, height: usize| Rectangle {
                width,
                height,
            });
        graph.name(new_rectangle, "new_rectangle")?;

        let new_warp = graph.native_function(
            |location: &Tile, target: &Tile, target_room: &String| Warp {
                location: location.clone(),
                target: target.clone(),
                target_room: target_room.clone(),
            },
        );
        graph.name(new_warp, "new_warp")?;

        let new_resource_clump_kind =
            graph.native_function(|kind: i32| -> ResourceClumpKind {
                kind.try_into().expect(
                    "Allow NativeFunction to propagate \
                         user-defined error types",
                )
            });
        graph.name(new_resource_clump_kind, "new_resource_clump_kind")?;

        let new_resource_clump = graph.native_function(
            |location: &Position,
             shape: &Rectangle,
             kind: &ResourceClumpKind| {
                ResourceClump {
                    location: location.clone(),
                    shape: shape.clone(),
                    kind: kind.clone(),
                }
            },
        );
        graph.name(new_resource_clump, "new_resource_clump")?;

        let new_tree = graph.native_function(
            |position: &Position,
             kind: &String,
             growth_stage: i32,
             has_seed: bool,
             is_stump: bool| Tree {
                position: position.clone(),
                kind: kind.parse().unwrap(),
                growth_stage,
                has_seed,
                is_stump,
            },
        );
        graph.name(new_tree, "new_tree")?;

        let new_litter = graph.native_function(
            |position: &Position, name: &String, category: i32| {
                if category != -999 {
                    return None;
                }
                let opt_kind = if name == "Twig" {
                    Some(LitterKind::Wood)
                } else if name == "Stone" {
                    Some(LitterKind::Stone)
                } else if name.to_lowercase().contains("weeds") {
                    Some(LitterKind::Fiber)
                } else {
                    None
                };
                opt_kind.map(|kind| Litter {
                    position: position.clone(),
                    kind,
                })
            },
        );
        graph.name(new_litter, "new_litter")?;

        let new_bush =
            graph.native_function(|size: usize, position: &Position| Bush {
                size,
                position: position.clone(),
            });
        graph.name(new_bush, "new_bush")?;

        let new_location = graph.native_function(
            |name: &String,
             shape: &Rectangle,
             warps: &Vec<Warp>,
             resource_clumps: &Vec<ResourceClump>,
             grass: &Vec<Position>,
             trees: &Vec<Tree>,
             bushes: &Vec<Bush>,
             litter: &Vec<Litter>,
             water_tiles: &Vec<bool>| {
                GameLocation {
                    name: name.clone(),
                    shape: shape.clone(),
                    warps: warps.clone(),
                    resource_clumps: resource_clumps.clone(),
                    grass: grass.clone(),
                    trees: trees.clone(),
                    bushes: bushes.clone(),
                    litter: litter.clone(),
                    water_tiles: (!water_tiles.is_empty())
                        .then(|| water_tiles.clone()),
                }
            },
        );
        graph.name(new_location, "new_location")?;

        graph.parse(stringify! {
            fn get_location(i_loc: usize) {
                let location = location_list._items[i_loc];
                let name = location.name.value.read_string();
                let size = location
                    .map
                    .m_layers
                    ._items[0]
                    .m_layerSize;
                let width = size.Width.prim_cast::<usize>();
                let height = size.Height.prim_cast::<usize>();
                let shape = new_rectangle(width,height);

                let num_warps = location
                    .warps
                    .count
                    .value
                    .prim_cast::<usize>();

                let warps = (0..num_warps)
                    .map(|i_warp: usize| {
                        let warp = location
                            .warps
                            .array
                            .value
                            .elements
                            ._items[i_warp]
                            .value;
                        let location = new_tile(
                            warp.x.value,
                            warp.y.value,
                        );
                        let target = new_tile(
                            warp.targetX.value,
                            warp.targetY.value,
                        );
                        let target_room = warp
                            .targetName
                            .value
                            .read_string();
                        new_warp(
                            location,
                            target,
                            target_room,
                        )
                    })
                    .filter(|obj| obj.is_some())
                    .collect();

                let num_resource_clumps = location
                    .resourceClumps
                    .list
                    ._size
                    .prim_cast::<usize>();
                let resource_clumps = (0..num_resource_clumps)
                    .map(|i_clump: usize| {
                        let clump = location
                            .resourceClumps
                            .list
                            ._items[i_clump];
                        let location = {
                            let right = clump.netTile.value.X;
                            let down = clump.netTile.value.Y;
                            new_position(right,down)
                        };
                        let shape = {
                            let width = clump.width.value;
                            let height = clump.height.value;
                            new_rectangle(width,height)
                        };

                        let kind = {
                            let kind_index = clump.parentSheetIndex.value;
                            new_resource_clump_kind(kind_index)
                        };
                        new_resource_clump(location,shape,kind)
                    })
                    .filter(|obj| obj.is_some())
                    .collect();

                let num_features = location
                    .terrainFeatures
                    .dict
                    ._entries
                    .len();
                let num_features = if num_features.is_some() { num_features } else { 0 };
                let iter_features = (0..num_features)
                    .map(|i_feat: usize| {
                        location
                            .terrainFeatures
                            .dict
                            ._entries[i_feat]
                    });

                let grass = iter_features
                    .filter(|feature| feature
                            .value
                            .value
                            .as::<StardewValley.TerrainFeatures.Grass>()
                            .grassBladeHealth
                            .is_some())
                    .map(|feature| {
                        let right = feature.key.X;
                        let down = feature.key.Y;
                        new_position(right,down)
                    })
                    .collect();

                let trees = iter_features
                    .filter(|feature| feature
                            .value
                            .value
                            .as::<StardewValley.TerrainFeatures.Tree>()
                            .is_some())
                    .map(|feature| {
                        let tree = feature
                            .value
                            .value
                            .as::<StardewValley.TerrainFeatures.Tree>();
                        let position = {
                            let right = feature.key.X;
                            let down = feature.key.Y;
                            new_position(right,down)
                        };
                        // treeType looks like an integer, but has
                        // been converted to a string.
                        let tree_type =
                            tree.treeType.value.read_string();
                        let growth_stage = tree.growthStage.value;
                        let has_seed = tree.hasSeed.value;
                        let is_stump = tree.stump.value;
                        new_tree(
                            position,
                            tree_type,
                            growth_stage,
                            has_seed,
                            is_stump
                        )
                    })
                    .collect();

                let num_large_features = location
                    .largeTerrainFeatures
                    .list
                    ._size
                    .prim_cast::<usize>();
                let bushes = (0..num_large_features)
                    .map(|i| {
                        location
                            .largeTerrainFeatures
                            .list
                            ._items[i]
                            .as::<StardewValley.TerrainFeatures.Bush>()
                    })
                    .map(|feature| {
                        let size = feature.size.value;
                        let position = {
                            let right = feature.netTilePosition.value.X;
                            let down = feature.netTilePosition.value.Y;
                            new_position(right,down)
                        };
                        new_bush(size,position)
                    })
                    .collect();

                let num_objects = location
                    .objects
                    .compositeDict
                    ._entries
                    .len();
                let litter = (0..num_objects)
                    .map(|i| {
                        location
                            .objects
                            .compositeDict
                            ._entries[i]
                    })
                    .map(|obj| {
                        let position = {
                            let right = obj.value.tileLocation.value.X;
                            let down = obj.value.tileLocation.value.Y;
                            new_position(right,down)
                        };
                        let name = obj.value.netName.value.read_string();
                        let category = obj.value.category.value;
                        new_litter(position,name,category)
                    })
                    .filter(|litter| litter.is_some())
                    .collect();

                let water_tiles = location
                    .waterTiles
                    .as::<StardewValley.WaterTiles>()
                    .waterTiles;
                let has_water_tiles = water_tiles.is_some();
                let flattened_water_tiles = (0..(height*width))
                    .filter(|i| has_water_tiles)
                    .map(|i| {
                        water_tiles[i/height, i%height].isWater
                    })
                    .collect();

                new_location(
                    name,
                    shape,
                    warps,
                    resource_clumps,
                    grass,
                    trees,
                    bushes,
                    litter,
                    flattened_water_tiles,
                )
            }

            pub fn main() {
                let num_locations = location_list._size.prim_cast::<usize>();

                let locations = (0..num_locations)
                    .map(get_location)
                    .collect();

                let player_position = {
                    let pos = StardewValley
                        .Game1
                        ._player
                        .position
                        .Field
                        .value;
                    new_position(pos.X, pos.Y)
                };

                let current_location = StardewValley
                    .Game1
                    ._player
                    .currentLocationRef
                    .locationName
                    .value
                    .read_string();

                (
                    locations,
                    current_location,
                    player_position,
                )
            }
        })?;

        let vm = graph.compile(reader)?;
        let res = vm.evaluate(reader)?;

        let locations = res
            .get_any(0)?
            .expect("Vector should exist")
            .downcast_ref::<Vec<GameLocation>>()
            .expect("Vector should be Vec<Location>");

        let current_location = res
            .get_any(1)?
            .expect("current_location shoudl exist")
            .downcast_ref::<String>()
            .expect("Current location should be string");

        let position = res
            .get_any(2)?
            .expect("Player location shoudl exist")
            .downcast_ref::<Position>()
            .expect("Player location should be Position");

        let pathfinding = Self {
            locations: locations.clone(),
            current_location: current_location.clone(),
            position: position.clone(),
        };
        return Ok(pathfinding);
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

                    {
                        let grass = loc
                            .grass
                            .iter()
                            .map(|grass_pos| {
                                (
                                    grass_pos.right as f64,
                                    (loc.shape.height as f64)
                                        - (grass_pos.down as f64),
                                )
                            })
                            .collect::<Vec<_>>();
                        ctx.draw(&Points {
                            coords: &grass,
                            color: Color::Rgb(10, 80, 10),
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
