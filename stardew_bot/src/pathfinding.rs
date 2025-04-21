#![allow(dead_code)]
use std::{collections::HashSet, fmt::Display};

use dotnet_debugger::{CachedReader, RustNativeObject, SymbolicGraph};
use memory_reader::Pointer;
use ratatui::{
    layout::Constraint,
    style::Color,
    symbols::Marker,
    text::Text,
    widgets::{
        canvas::{
            Canvas, Context as CanvasContext, Points,
            Rectangle as CanvasRectangle,
        },
        Block, Cell, Row, Table, Widget as _,
    },
};
use tui_utils::{extensions::SplitRect as _, WidgetWindow};

use crate::{
    watch_point_definition::{ValueToken, WatchPointResults},
    Error, WatchPointDefinition,
};

pub struct PathfindingUI {
    player_x: ValueToken,
    player_y: ValueToken,
    current_location_token: ValueToken,

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

    buildings: Vec<Building>,

    /// Which tiles are blocked by impassable tiles, either in the
    /// "Back" layer or the "Buildings" layer.
    ///
    /// If either the layer has a non-null tile, and that tile is not
    /// explicitly marked with the "Passable" attribute, then the tile
    /// cannot be passed through.
    blocked: Vec<bool>,
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
    category: i32,
    kind: LitterKind,
}

#[derive(RustNativeObject, Debug, Clone, PartialEq, Eq)]
enum LitterKind {
    Stone,
    Wood,
    Fiber,
    Other(String),
}

#[derive(RustNativeObject, Debug, Clone, Copy)]
struct Rectangle {
    width: usize,
    height: usize,
}

#[derive(RustNativeObject, Debug, Clone, Copy)]
struct Tile {
    right: isize,
    down: isize,
}

#[derive(RustNativeObject, Debug, PartialEq, Clone, Copy)]
struct Position {
    right: f32,
    down: f32,
}

#[derive(RustNativeObject, Debug, Clone)]
struct Building {
    /// The position of the top-left corner of the building
    loc: Tile,

    /// The size of the building
    shape: Rectangle,

    /// The door to the inside of the building
    door: Option<BuildingDoor>,
}

#[derive(RustNativeObject, Debug, Clone)]
struct BuildingDoor {
    /// The location of the door, relative to the top-left corner of
    /// the building.
    relative_location: Tile,

    /// The name of the GameLocation that is inside the building.
    inside_name: String,
}

#[derive(RustNativeObject, Default, Clone, Debug)]
struct MapTileSheets {
    known_sheets: HashSet<Pointer>,
    passable: HashSet<(Pointer, usize)>,
    shadow: HashSet<(Pointer, usize)>,
}

impl MapTileSheets {
    fn define_sheet(&mut self, tile_sheet: Pointer) {
        self.known_sheets.insert(tile_sheet);
    }

    fn define_property(&mut self, tile_sheet: Pointer, index_key: &str) {
        index_key
            .strip_prefix("@TileIndex@")
            .and_then(|entry| entry.split_once('@'))
            .and_then(|(index_str, property)| {
                index_str.parse().ok().map(|index| (index, property))
            })
            .into_iter()
            .for_each(|(index, property): (usize, &str)| match property {
                "Passable" => {
                    self.passable.insert((tile_sheet, index));
                }
                "Shadow" => {
                    self.shadow.insert((tile_sheet, index));
                }
                _ => {}
            });
    }

    fn has_passable_flag(
        &self,
        tile_sheet: Pointer,
        tile_index: usize,
    ) -> bool {
        assert!(self.known_sheets.contains(&tile_sheet));
        self.passable.contains(&(tile_sheet, tile_index))
    }

    fn has_shadow_flag(&self, tile_sheet: Pointer, tile_index: usize) -> bool {
        self.passable.contains(&(tile_sheet, tile_index))
    }
}

impl PathfindingUI {
    pub fn new(
        reader: CachedReader,
        watch_point_spec: &mut WatchPointDefinition,
    ) -> Result<Self, Error> {
        let mut register = |value: &str| -> Result<ValueToken, Error> {
            let expr = watch_point_spec.parse(value)?;
            let token = watch_point_spec.mark_output(expr);
            Ok(token)
        };

        let player_position = "StardewValley
                .Game1
                ._player
                .position
                .Field
                .value";
        let player_x = register(&format!("{player_position}.X"))?;
        let player_y = register(&format!("{player_position}.Y"))?;
        let current_location_token = register(
            "StardewValley
            .Game1
            ._player
            .currentLocationRef
            .locationName
            .value
            .read_string()",
        )?;

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
            |location: &Tile, target: &Tile, target_room: &str| Warp {
                location: location.clone(),
                target: target.clone(),
                target_room: target_room.into(),
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
             kind: &str,
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
            |position: &Position, name: &str, category: i32| {
                // if category != -999 {
                //     return None;
                // }
                let opt_kind = if name == "Twig" {
                    Some(LitterKind::Wood)
                } else if name == "Stone" {
                    Some(LitterKind::Stone)
                } else if name.to_lowercase().contains("weeds") {
                    Some(LitterKind::Fiber)
                } else {
                    //None
                    Some(LitterKind::Other(name.into()))
                };
                opt_kind.map(|kind| Litter {
                    position: position.clone(),
                    category: category.clone(),
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

        let new_building_door = graph.native_function(
            |relative_location: &Tile, inside_name: &str| BuildingDoor {
                relative_location: relative_location.clone(),
                inside_name: inside_name.into(),
            },
        );
        graph.name(new_building_door, "new_building_door")?;

        let new_building = graph.native_function(
            |loc: &Tile, shape: &Rectangle, door: Option<&BuildingDoor>| {
                let loc = loc.clone();
                let shape = shape.clone();
                let door = door.cloned();
                Building { loc, shape, door }
            },
        );
        graph.name(new_building, "new_building")?;

        let new_tile_sheets =
            graph.native_function(|_: usize| MapTileSheets::default());
        graph.name(new_tile_sheets, "new_tile_sheets")?;

        let define_tile_sheet = graph.native_function(
            |sheets: &mut MapTileSheets, tile_sheet: Pointer| {
                sheets.define_sheet(tile_sheet);
            },
        );
        graph.name(define_tile_sheet, "define_tile_sheet")?;

        let define_tile_sheets_property = graph.native_function(
            |sheets: &mut MapTileSheets,
             tile_sheet: Pointer,
             index_key: &str| {
                sheets.define_property(tile_sheet, index_key);
            },
        );
        graph
            .name(define_tile_sheets_property, "define_tile_sheets_property")?;

        let check_passable_flag = graph.native_function(
            |sheets: &MapTileSheets, tile_sheet: Pointer, tile_index: usize| {
                sheets.has_passable_flag(tile_sheet, tile_index)
            },
        );
        graph.name(check_passable_flag, "check_passable_flag")?;

        let check_shadow_flag = graph.native_function(
            |sheets: &MapTileSheets, tile_sheet: Pointer, tile_index: usize| {
                sheets.has_shadow_flag(tile_sheet, tile_index)
            },
        );
        graph.name(check_shadow_flag, "check_shadow_flag")?;

        let new_location = graph.native_function(
            |name: &str,
             shape: &Rectangle,
             warps: &Vec<Warp>,
             resource_clumps: &Vec<ResourceClump>,
             grass: &Vec<Position>,
             trees: &Vec<Tree>,
             bushes: &Vec<Bush>,
             litter: &Vec<Litter>,
             water_tiles: &Vec<bool>,
             buildings: &Vec<Building>,
             blocked: &Vec<bool>| {
                GameLocation {
                    name: name.into(),
                    shape: shape.clone(),
                    warps: warps.clone(),
                    resource_clumps: resource_clumps.clone(),
                    grass: grass.clone(),
                    trees: trees.clone(),
                    bushes: bushes.clone(),
                    litter: litter.clone(),
                    water_tiles: (!water_tiles.is_empty())
                        .then(|| water_tiles.clone()),
                    buildings: buildings.clone(),
                    blocked: blocked.clone(),
                }
            },
        );
        graph.name(new_location, "new_location")?;

        graph.parse(stringify! {
            fn get_location_name_ptr(loc) {
                let unique_name = loc.uniqueName.value;
                let name = loc.name.value;
                if unique_name.is_some() {
                    unique_name
                } else {
                    name
                }
            }

            fn get_location(i_loc: usize) {
                let location = location_list._items[i_loc];
                let name = get_location_name_ptr(location).read_string();
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

                let num_buildings = location
                    .buildings
                    .list
                    ._size
                    .prim_cast::<usize>();
                let buildings = (0..num_buildings)
                    .map(|i| location
                         .buildings
                         .list
                         ._items[i]
                    )
                    .map(|building| {
                        let loc = {
                            let right = building.tileX.value;
                            let down = building.tileY.value;
                            new_tile(right,down)
                        };

                        let shape = {
                            let width = building.tilesWide.value;
                            let height = building.tilesHigh.value;
                            new_rectangle(width,height)
                        };

                        let door = {
                            let relative_location = {
                                let right = building.humanDoor.value.X;
                                let down = building.humanDoor.value.Y;
                                new_tile(right,down)
                            };

                            let inside = building
                                .indoors
                                .value;

                            let inside_name_ptr = if inside.is_some() {
                                get_location_name_ptr(inside)
                            } else {
                                building
                                    .nonInstancedIndoorsName
                                    .value
                            };

                            let inside_name = inside_name_ptr.read_string();

                            new_building_door(relative_location, inside_name)
                        };

                        new_building(loc, shape, door)
                    })
                    .collect();

                let num_tile_sheets = location
                    .map
                    .m_tileSheets
                    ._size
                    .prim_cast::<usize>();
                let tile_sheets = (0..num_tile_sheets)
                    .reduce(new_tile_sheets(i_loc), |tile_sheets, i_layer| {
                        let tile_sheet = location
                            .map
                            .m_tileSheets
                            ._items[i_layer];

                        let tile_sheets = define_tile_sheet(
                            tile_sheets,
                            tile_sheet.prim_cast::<Pointer>()
                        );

                        let num_properties = tile_sheet
                            .m_propertyCollection
                            ._entries
                            .len();
                        (0..num_properties)
                            .reduce(tile_sheets, |tile_sheets, i_property| {
                                define_tile_sheets_property(
                                    tile_sheets,
                                    tile_sheet.prim_cast::<Pointer>(),
                                    tile_sheet
                                        .m_propertyCollection
                                        ._entries[i_property]
                                        .key
                                        .read_string()
                                )
                            })

                    });

                let has_passable_flag = |tile| {
                    let tile = tile.as::<xTile.Tiles.StaticTile>();
                    let tile_sheet = tile
                        .m_tileSheet
                        .prim_cast::<Pointer>();
                    let tile_index = tile
                        .m_tileIndex
                        .prim_cast::<usize>();
                    check_passable_flag(
                        tile_sheets,
                        tile_sheet,
                        tile_index,
                    )
                };

                let has_shadow_flag = |tile| {
                    let tile = tile.as::<xTile.Tiles.StaticTile>();
                    let tile_sheet = tile
                        .m_tileSheet
                        .prim_cast::<Pointer>();
                    let tile_index = tile
                        .m_tileIndex
                        .prim_cast::<usize>();
                    check_shadow_flag(
                        tile_sheets,
                        tile_sheet,
                        tile_index,
                    )
                };


                let back_layer = location
                    .backgroundLayers
                    ._items[0]
                    .key;
                let building_layer = location
                    .buildingLayers
                    ._items[0]
                    .key;

                let blocked = (0..(height*width))
                    .map(|i_flat| {
                        let i = i_flat/height;
                        let j = i_flat%height;
                        let back_tile = back_layer.m_tiles[i,j];
                        let building_tile = building_layer.m_tiles[i,j];

                        let back_tile_blocked = back_tile.is_some()
                            && has_passable_flag(back_tile);

                        let building_tile_blocked = building_tile.is_some()
                            && !has_passable_flag(building_tile)
                            && !has_shadow_flag(building_tile);

                        back_tile_blocked || building_tile_blocked
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
                    buildings,
                    blocked,
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

        // locations
        //     .iter()
        //     .flat_map(|loc| {
        //         loc.buildings.iter().map(move |building| (loc, building))
        //     })
        //     .for_each(|(loc, building)| {
        //         println!(
        //             "Inside location '{}' is a building at ({},{}), \
        //              with {}",
        //             loc.name,
        //             building.loc.right,
        //             building.loc.down,
        //             if let Some(door) = &building.door {
        //                 format!("interior named '{}'", door.inside_name)
        //             } else {
        //                 "no interior".into()
        //             }
        //         );
        //     });

        // locations.iter().for_each(|loc| {
        //     println!("Location {} has {} objects", loc.name, loc.litter.len());
        //     for obj in loc.litter.iter() {
        //         println!(
        //             "\tAt {}, object '{}' of category {}",
        //             obj.position, obj.kind, obj.category,
        //         );
        //     }
        // });

        let current_location = res
            .get_any(1)?
            .expect("current_location should exist")
            .downcast_ref::<String>()
            .expect("Current location should be string");

        let position = res
            .get_any(2)?
            .expect("Player location should exist")
            .downcast_ref::<Position>()
            .expect("Player location should be Position");

        let pathfinding = Self {
            locations: locations.clone(),
            current_location: current_location.clone(),
            position: position.clone(),
            player_x,
            player_y,
            current_location_token,
        };

        return Ok(pathfinding);
    }
}

impl Into<(f64, f64)> for Position {
    fn into(self) -> (f64, f64) {
        let Self { right, down } = self;
        (right.into(), down.into())
    }
}
impl Into<(f64, f64)> for Tile {
    fn into(self) -> (f64, f64) {
        let Self { right, down } = self;
        (right as f64, down as f64)
    }
}

struct DrawableGameLocation<'a> {
    room: &'a GameLocation,
    draw_marker: Marker,
    draw_area: ratatui::layout::Rect,
    player_position: Position,
}

impl<'a> DrawableGameLocation<'a> {
    fn render(&self, buf: &mut ratatui::prelude::Buffer) {
        Canvas::default()
            .block(Block::new().title(self.room.name.as_ref()))
            .marker(self.draw_marker)
            .x_bounds(self.x_bounds())
            .y_bounds(self.y_bounds())
            .paint(|ctx| {
                self.paint_blocked_tiles(ctx);
                self.paint_water_tiles(ctx);
                self.paint_buildings(ctx);
                self.paint_grass(ctx);
                self.paint_resource_clumps(ctx);
                self.paint_bushes(ctx);
                self.paint_trees(ctx);

                self.paint_litter(ctx);

                ctx.print(
                    self.player_position.right as f64,
                    (self.room.shape.height as f64)
                        - (self.player_position.down as f64),
                    "x",
                );
            })
            .render(self.draw_area, buf)
    }

    fn to_draw_coordinates(&self, loc: impl Into<(f64, f64)>) -> (f64, f64) {
        let (x, y): (f64, f64) = loc.into();

        let height = self.room.shape.height as f64;
        let y = height - y;
        (x, y)
    }

    fn to_draw_rectangle(
        &self,
        top_left: impl Into<(f64, f64)>,
        shape: Rectangle,
        color: Color,
    ) -> CanvasRectangle {
        let (left_x, top_y) = self.to_draw_coordinates(top_left);
        let bottom_y = top_y - (shape.height as f64);

        CanvasRectangle {
            x: left_x,
            y: bottom_y,
            width: (shape.width as f64) - 1.0,
            height: (shape.height as f64) - 1.0,
            color,
        }
    }

    fn x_bounds(&self) -> [f64; 2] {
        let (x, _) = self.to_draw_coordinates(self.player_position);
        self.compute_bounds(
            self.room.shape.width,
            self.draw_area.width.into(),
            x,
        )
    }

    fn y_bounds(&self) -> [f64; 2] {
        let (_, y) = self.to_draw_coordinates(self.player_position);
        self.compute_bounds(
            self.room.shape.height,
            self.draw_area.height.into(),
            y,
        )
    }

    fn compute_bounds(
        &self,
        game_extent_in_tiles: usize,
        view_extent_in_terminal: usize,
        position: f64,
    ) -> [f64; 2] {
        let game_tiles_per_terminal_tile = match self.draw_marker {
            Marker::Block => 1,
            Marker::HalfBlock => 2,
            _ => 1,
        };

        let game_extent = game_extent_in_tiles as f64;
        let view_extent =
            (view_extent_in_terminal * game_tiles_per_terminal_tile) as f64;

        if view_extent > game_extent {
            [0.0, view_extent]
        } else if position < view_extent / 2.0 {
            [0.0, view_extent]
        } else if position > game_extent - view_extent / 2.0 {
            [game_extent - view_extent, game_extent]
        } else {
            [position - view_extent / 2.0, position + view_extent / 2.0]
        }
    }

    fn paint_blocked_tiles(&self, ctx: &mut CanvasContext) {
        let Rectangle { width, height } = self.room.shape;
        assert!(width * height == self.room.blocked.len());

        let blocked = self
            .room
            .blocked
            .iter()
            .enumerate()
            .filter(|(_, is_blocked)| **is_blocked)
            .map(|(index, _)| {
                let i = index / height;
                let j = index % height;
                let x = i as f64;
                let y = (height - j) as f64;
                (x, y)
            })
            .collect::<Vec<_>>();
        ctx.draw(&Points {
            coords: &blocked,
            color: Color::Red,
        });
    }

    fn paint_water_tiles(&self, ctx: &mut CanvasContext) {
        let Rectangle { width, height } = self.room.shape;

        let Some(tiles) = &self.room.water_tiles else {
            return;
        };

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

    fn paint_buildings(&self, ctx: &mut CanvasContext) {
        self.room
            .buildings
            .iter()
            .map(|building| {
                self.to_draw_rectangle(
                    building.loc,
                    building.shape,
                    Color::Rgb(45, 20, 0),
                )
            })
            .for_each(|rect| ctx.draw(&rect));
    }

    fn paint_resource_clumps(&self, ctx: &mut CanvasContext) {
        self.room
            .resource_clumps
            .iter()
            .map(|clump| {
                self.to_draw_rectangle(
                    clump.location,
                    clump.shape,
                    match clump.kind {
                        ResourceClumpKind::Stump => Color::Yellow,
                        ResourceClumpKind::Boulder => Color::DarkGray,
                        ResourceClumpKind::Meterorite => Color::Magenta,
                        ResourceClumpKind::MineBoulder => Color::DarkGray,
                    },
                )
            })
            .for_each(|rect| ctx.draw(&rect));
    }

    fn paint_bushes(&self, ctx: &mut CanvasContext) {
        self.room
            .bushes
            .iter()
            .map(|bush| {
                self.to_draw_rectangle(
                    bush.position,
                    Rectangle {
                        width: bush.width(),
                        height: 1,
                    },
                    Color::Green,
                )
            })
            .for_each(|rect| ctx.draw(&rect));
    }

    fn paint_trees(&self, ctx: &mut CanvasContext) {
        let trees = self
            .room
            .trees
            .iter()
            .map(|tree| self.to_draw_coordinates(tree.position))
            .collect::<Vec<_>>();
        ctx.draw(&Points {
            coords: &trees,
            color: Color::Rgb(133, 74, 5),
        });
    }

    fn paint_grass(&self, ctx: &mut CanvasContext) {
        let grass = self
            .room
            .grass
            .iter()
            .map(|grass_pos| self.to_draw_coordinates(*grass_pos))
            .collect::<Vec<_>>();
        ctx.draw(&Points {
            coords: &grass,
            color: Color::Rgb(10, 80, 10),
        });
    }

    fn paint_litter(&self, ctx: &mut CanvasContext) {
        for (litter_kind, color) in [
            (LitterKind::Stone, Color::DarkGray),
            (LitterKind::Wood, Color::Rgb(97, 25, 0)),
            (LitterKind::Fiber, Color::LightGreen),
        ] {
            let litter = self
                .room
                .litter
                .iter()
                .filter(|obj| obj.kind == litter_kind)
                .map(|obj| self.to_draw_coordinates(obj.position))
                .collect::<Vec<_>>();
            ctx.draw(&Points {
                coords: &litter,
                color,
            });
        }
    }
}

impl WidgetWindow<Error> for PathfindingUI {
    fn title(&self) -> std::borrow::Cow<str> {
        "Pathfinding".into()
    }

    fn draw<'a>(
        &'a mut self,
        globals: &'a tui_utils::TuiGlobals,
        area: ratatui::layout::Rect,
        buf: &mut ratatui::prelude::Buffer,
    ) {
        let per_frame_values = globals
            .get::<WatchPointResults>()
            .expect("Generated for each frame");
        let position = {
            let right: f32 = per_frame_values[self.player_x]
                .as_ref()
                .unwrap()
                .try_into()
                .unwrap();
            let right = right / 64.0;
            let down: f32 = per_frame_values[self.player_y]
                .as_ref()
                .unwrap()
                .try_into()
                .unwrap();
            let down = down / 64.0;
            Position { right, down }
        };
        let current_location: &str = per_frame_values
            [self.current_location_token]
            .as_ref()
            .unwrap()
            .as_native::<String>()
            .map(|s| s.as_str())
            .unwrap();

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

        let table = Table::new(
            loc_rows,
            [
                Constraint::Min(longest_name as u16),
                Constraint::Min(10),
                Constraint::Min(longest_name as u16),
                Constraint::Percentage(100),
            ],
        );

        table.render(table_area, buf);

        let top_text = Text::raw(format!(
            "Current: {} ({:.1}, {:.1})",
            current_location, position.right, position.down,
        ));
        top_text.render(top_area, buf);

        let opt_current_room = self
            .locations
            .iter()
            .find(|loc| loc.name == current_location);
        if let Some(current_room) = opt_current_room {
            DrawableGameLocation {
                room: current_room,
                draw_marker: Marker::Block,
                draw_area,
                player_position: position,
            }
            .render(buf);
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

impl Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.right, self.down)
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

impl Display for LitterKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LitterKind::Stone => write!(f, "Stone"),
            LitterKind::Wood => write!(f, "Wood"),
            LitterKind::Fiber => write!(f, "Fiber"),
            LitterKind::Other(other) => write!(f, "Other({other})"),
        }
    }
}
