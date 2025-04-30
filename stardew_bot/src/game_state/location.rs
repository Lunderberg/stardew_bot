use std::collections::HashSet;

use dotnet_debugger::{RustNativeObject, SymbolicGraph, SymbolicValue};
use memory_reader::Pointer;

use crate::Error;

use super::{Rectangle, Vector};

#[derive(RustNativeObject, Debug, Clone)]
pub struct Location {
    /// The unique name of the room.  Used as a lookup to refer to
    /// this location.
    pub name: String,

    /// The size of the room.
    pub shape: Vector<isize>,

    /// Tiles that connect to other rooms.  Standing on a warp moves
    /// the player to the room/location specified by the warp.
    pub warps: Vec<Warp>,

    /// Larger groups of resources.  On the farm, for example, the
    /// stumps/boulders that require iron tools to break.
    pub resource_clumps: Vec<ResourceClump>,

    /// Bushes, which include decorative (and berry-yielding) bushes,
    /// player-planted tea tree bushes, and walnut bushes.
    pub bushes: Vec<Bush>,

    /// Trees.  This includes the small 1x1 stumps left behind after
    /// chopping down a tree, but not the larger 2x2 stumps made
    /// during worldgen.
    pub trees: Vec<Tree>,

    /// Location of grass.  Currently, does not distinguish between
    /// regular grass and blue grass, and does not read out the health
    /// of the grass.
    pub grass: Vec<Vector<isize>>,

    pub litter: Vec<Litter>,

    /// Which tiles have water.
    pub water_tiles: Option<Vec<bool>>,

    pub buildings: Vec<Building>,

    /// Which tiles are blocked by impassable tiles, either in the
    /// "Back" layer or the "Buildings" layer.
    ///
    /// If either the layer has a non-null tile, and that tile is not
    /// explicitly marked with the "Passable" attribute, then the tile
    /// cannot be passed through.
    pub blocked: Vec<bool>,
}

#[derive(RustNativeObject, Debug, Clone)]
pub struct Warp {
    pub location: Vector<isize>,
    pub target: Vector<isize>,
    pub target_room: String,
    pub kind: WarpKind,
}

#[derive(Debug, Clone)]
pub enum WarpKind {
    /// Standing on this tile will activate the warp.  This is used
    /// for transitions between outdoor areas, and for exiting houses.
    Automatic,

    /// A door that must be explicitly activated.  This is used for
    /// entering houses.
    Door,

    /// A door that may only be activated at certain times of the day.
    /// The times are integer values, which show the time of day.
    /// For example, 2:00 PM would be the integer 1400.
    LockedDoor { opens: i32, closes: i32 },
}

#[derive(RustNativeObject, Debug, Clone)]
pub struct ResourceClump {
    pub shape: Rectangle<isize>,
    pub kind: ResourceClumpKind,
}

#[derive(RustNativeObject, Debug, Clone, Copy)]
pub enum ResourceClumpKind {
    Stump,
    Boulder,
    Meterorite,
    MineBoulder,
}

#[derive(RustNativeObject, Debug, Clone)]
pub struct Bush {
    /// The type of bush.  Internally, this is called `size`, but only
    /// corresponds to a width for values 1-3.  It also encodes
    /// whether this bush is a tea tree (4), or a golden walnut bush
    /// (5).  To avoid confusing myself on it, naming it `kind` when I
    /// interact with it.
    pub kind: usize,

    /// The position of the top-left of the bush.  This is stored
    /// internally as a floating-point value, but since all
    /// occurrences are aligned to tiles, I cast it to integers.
    pub top_left: Vector<isize>,
}

#[derive(RustNativeObject, Debug, Clone)]
pub struct Tree {
    pub position: Vector<isize>,
    #[allow(dead_code)]
    pub kind: TreeKind,
    #[allow(dead_code)]
    pub growth_stage: i32,
    #[allow(dead_code)]
    pub has_seed: bool,
    #[allow(dead_code)]
    pub is_stump: bool,
}

#[derive(RustNativeObject, Debug, Clone)]
pub enum TreeKind {
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
pub struct Litter {
    pub tile: Vector<isize>,
    pub category: i32,
    pub kind: LitterKind,
}

#[derive(RustNativeObject, Debug, Clone, PartialEq, Eq)]
pub enum LitterKind {
    Stone,
    Wood,
    Fiber,
    Other(String),
}

#[derive(RustNativeObject, Debug, Clone)]
pub struct Building {
    /// The size of the building
    pub shape: Rectangle<isize>,

    /// The door to the inside of the building
    pub door: Option<BuildingDoor>,
}

#[derive(RustNativeObject, Debug, Clone)]
pub struct BuildingDoor {
    /// The location of the door, relative to the top-left corner of
    /// the building.
    pub relative_location: Vector<isize>,

    /// The name of the GameLocation that is inside the building.
    pub inside_name: String,
}

#[derive(RustNativeObject, Default, Clone, Debug)]
struct MapTileSheets {
    known_sheets: HashSet<Pointer>,
    passable: HashSet<(Pointer, usize)>,
    shadow: HashSet<(Pointer, usize)>,
}

impl Location {
    pub(crate) fn read_all(
        graph: &mut SymbolicGraph,
    ) -> Result<SymbolicValue, Error> {
        graph.named_native_function(
            "new_isize_vector",
            |right: isize, down: isize| Vector::<isize> { right, down },
        )?;

        graph.named_native_function(
            "new_warp",
            |location: &Vector<isize>,
             target: &Vector<isize>,
             target_room: &str| Warp {
                location: location.clone(),
                target: target.clone(),
                target_room: target_room.into(),
                kind: WarpKind::Automatic,
            },
        )?;

        graph.named_native_function(
            "parse_door",
            |warps: &mut Vec<Warp>,
             location_right: isize,
             location_down: isize,
             property_key: &str,
             opt_property_value: Option<&str>| {
                let location = Vector::new(location_right, location_down);

                let parse_warp = || -> Option<Warp> {
                    let action = opt_property_value
                        .filter(|_| property_key == "Action")?;

                    let mut iter_words = action.split(' ');
                    match iter_words.next() {
                        Some("Warp") => {
                            // Warp RIGHT DOWN TARGETNAME
                            //
                            // e.g. 'Warp 2 4 FishShop'
                            let target = {
                                let right: isize = iter_words
                                    .next()
                                    .and_then(|right| right.parse().ok())?;
                                let down: isize = iter_words
                                    .next()
                                    .and_then(|right| right.parse().ok())?;
                                Vector::new(right, down)
                            };
                            let target_room = iter_words.next()?.to_string();
                            Some(Warp {
                                location,
                                target,
                                target_room,
                                kind: WarpKind::Door,
                            })
                        }
                        Some("LockedDoorWarp") => {
                            // LockedDoorWarp RIGHT DOWN ROOM_NAME TIME_OPEN TIME_CLOSE
                            //
                            // e.g. 'LockedDoorWarp 6 19 AdventureGuild 1200 2600'
                            let target = {
                                let right: isize = iter_words
                                    .next()
                                    .and_then(|right| right.parse().ok())?;
                                let down: isize = iter_words
                                    .next()
                                    .and_then(|right| right.parse().ok())?;
                                Vector::new(right, down)
                            };
                            let target_room = iter_words.next()?.to_string();
                            let opens: i32 = iter_words
                                .next()
                                .and_then(|right| right.parse().ok())?;
                            let closes: i32 = iter_words
                                .next()
                                .and_then(|right| right.parse().ok())?;
                            Some(Warp {
                                location,
                                target,
                                target_room,
                                kind: WarpKind::LockedDoor { opens, closes },
                            })
                        }
                        _ => None,
                    }
                };

                if let Some(warp) = parse_warp() {
                    warps.push(warp)
                }
            },
        )?;

        graph.named_native_function(
            "new_tree",
            |position: &Vector<isize>,
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
        )?;

        graph.named_native_function(
            "new_bush",
            |kind: usize, top_left: &Vector<isize>| Bush {
                kind,
                top_left: top_left.clone(),
            },
        )?;

        graph.named_native_function(
            "new_litter",
            |tile: &Vector<isize>, name: &str, category: i32| {
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
                    tile: tile.clone(),
                    category,
                    kind,
                })
            },
        )?;

        graph.named_native_function(
            "new_rectangle",
            |right: isize, down: isize, width: isize, height: isize| {
                Rectangle::<isize> {
                    top_left: Vector { right, down },
                    shape: Vector {
                        right: width,
                        down: height,
                    },
                }
            },
        )?;

        graph.named_native_function(
            "new_resource_clump_kind",
            |kind: i32| -> ResourceClumpKind {
                kind.try_into().expect(
                    "TODO: Allow NativeFunction to propagate \
                     user-defined error types",
                )
            },
        )?;

        graph.named_native_function(
            "new_resource_clump",
            |shape: &Rectangle<isize>, kind: &ResourceClumpKind| {
                ResourceClump {
                    shape: shape.clone(),
                    kind: kind.clone(),
                }
            },
        )?;

        graph.named_native_function(
            "new_building_door",
            |relative_location: &Vector<isize>, inside_name: &str| {
                BuildingDoor {
                    relative_location: relative_location.clone(),
                    inside_name: inside_name.into(),
                }
            },
        )?;

        graph.named_native_function(
            "new_building",
            |shape: &Rectangle<isize>, door: Option<&BuildingDoor>| {
                let shape = shape.clone();
                let door = door.cloned();
                Building { shape, door }
            },
        )?;

        let new_tile_sheets =
            graph.native_function(|_: usize| MapTileSheets::default());
        graph.name(new_tile_sheets, "new_tile_sheets")?;

        graph.named_native_function(
            "define_tile_sheet",
            |sheets: &mut MapTileSheets, tile_sheet: Pointer| {
                sheets.define_sheet(tile_sheet);
            },
        )?;

        graph.named_native_function(
            "define_tile_sheets_property",
            |sheets: &mut MapTileSheets,
             tile_sheet: Pointer,
             index_key: &str| {
                sheets.define_property(tile_sheet, index_key);
            },
        )?;

        graph.named_native_function(
            "check_passable_flag",
            |sheets: &MapTileSheets, tile_sheet: Pointer, tile_index: usize| {
                sheets.has_passable_flag(tile_sheet, tile_index)
            },
        )?;

        graph.named_native_function(
            "check_shadow_flag",
            |sheets: &MapTileSheets, tile_sheet: Pointer, tile_index: usize| {
                sheets.has_shadow_flag(tile_sheet, tile_index)
            },
        )?;

        graph.named_native_function(
            "new_location",
            |name: &str,
             shape: &Vector<isize>,
             warps: &Vec<Warp>,
             resource_clumps: &Vec<ResourceClump>,
             grass: &Vec<Vector<isize>>,
             trees: &Vec<Tree>,
             bushes: &Vec<Bush>,
             litter: &Vec<Litter>,
             water_tiles: &Vec<bool>,
             buildings: &Vec<Building>,
             blocked: &Vec<bool>| {
                Location {
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
        )?;

        let func = graph.parse(stringify! {
            fn get_location_name_ptr(loc) {
                let unique_name = loc.uniqueName.value;
                let name = loc.name.value;
                if unique_name.is_some() {
                    unique_name
                } else {
                    name
                }
            }

            fn read_location(location) {
                let name = get_location_name_ptr(location).read_string();
                let size = location
                    .map
                    .m_layers
                    ._items[0]
                    .m_layerSize;
                let width = size.Width.prim_cast::<usize>();
                let height = size.Height.prim_cast::<usize>();
                let shape = new_isize_vector(width, height);

                // Background features of the map.
                let back_layer = location
                    .backgroundLayers
                    ._items[0]
                    .key;
                // Buildings in the map.  Accessed to determine the
                // location and target of interactive doors.
                let building_layer = location
                    .buildingLayers
                    ._items[0]
                    .key;

                let num_warps = location
                    .warps
                    .count
                    .value
                    .prim_cast::<usize>();

                let warps = (0..num_warps)
                    .map(|i_warp: usize| {
                        location
                            .warps
                            .array
                            .value
                            .elements
                            ._items[i_warp]
                            .value
                    })
                    .map(|warp| {
                        let location = new_isize_vector(
                            warp.x.value,
                            warp.y.value,
                        );
                        let target = new_isize_vector(
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

                let warps = (0..(height*width))
                    .filter(|i_flat| {
                        let i = i_flat/height;
                        let j = i_flat%height;
                        building_layer.m_tiles[i,j].is_some()
                    })
                    .reduce(warps, |warps_inner_0, i_flat| {
                        let i = i_flat/height;
                        let j = i_flat%height;

                        let building_tile = building_layer.m_tiles[i,j];

                        let tile_props = building_tile
                            .m_propertyCollection;

                        let num_tile_props = tile_props
                            ._count
                            .prim_cast::<usize>();

                        (0..num_tile_props)
                            .map(|i_prop| tile_props._entries[i_prop])
                            .reduce(
                                warps_inner_0,
                                |warps_inner_1, prop| {
                                    let key = prop.key.read_string();
                                    let value = prop
                                        .value
                                        .m_value
                                        .as::<System.String>()
                                        .read_string();
                                    parse_door(warps_inner_1,
                                               i, j,
                                               key, value)
                                })

                    });


                let num_resource_clumps = location
                    .resourceClumps
                    .list
                    ._size
                    .prim_cast::<usize>();
                let resource_clumps = (0..num_resource_clumps)
                    .map(|i_clump: usize| {
                        location
                            .resourceClumps
                            .list
                            ._items[i_clump]
                    })
                    .map(|clump| {
                        let shape = {
                            let right = clump.netTile.value.X;
                            let down = clump.netTile.value.Y;
                            let width = clump.width.value;
                            let height = clump.height.value;
                            new_rectangle(right,down,width,height)
                        };

                        let kind = {
                            let kind_index = clump.parentSheetIndex.value;
                            new_resource_clump_kind(kind_index)
                        };
                        new_resource_clump(shape, kind)
                    })
                    .filter(|obj| obj.is_some())
                    .collect();

                let num_features = location
                    .terrainFeatures
                    .dict
                    ._entries
                    .len();
                let num_features = if num_features.is_some() {
                    num_features
                } else {
                    0
                };
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
                        new_isize_vector(right,down)
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
                            new_isize_vector(right,down)
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
                        let kind = feature.size.value;
                        let top_left = {
                            let right = feature.netTilePosition.value.X;
                            let down = feature.netTilePosition.value.Y;
                            new_isize_vector(right,down)
                        };
                        new_bush(kind, top_left)
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
                        let tile = {
                            let right = obj.value.tileLocation.value.X;
                            let down = obj.value.tileLocation.value.Y;
                            new_isize_vector(right,down)
                        };
                        let name = obj.value.netName.value.read_string();
                        let category = obj.value.category.value;
                        new_litter(tile,name,category)
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
                        let shape = {
                            let right = building.tileX.value;
                            let down = building.tileY.value;
                            let width = building.tilesWide.value;
                            let height = building.tilesHigh.value;
                            new_rectangle(right,down,width,height)
                        };

                        let door = {
                            let relative_location = {
                                let right = building.humanDoor.value.X;
                                let down = building.humanDoor.value.Y;
                                new_isize_vector(right,down)
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

                        new_building(shape, door)
                    })
                    .collect();

                let num_tile_sheets = location
                    .map
                    .m_tileSheets
                    ._size
                    .prim_cast::<usize>();
                let tile_sheets = (0..num_tile_sheets)
                    .reduce(
                        new_tile_sheets(num_buildings),
                        |tile_sheets, i_layer| {
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
                                ._count
                                .prim_cast::<usize>();
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

                let has_flag = |flag_checker, tile| {
                    let tile = tile.as::<xTile.Tiles.StaticTile>();
                    let tile_sheet = tile
                        .m_tileSheet
                        .prim_cast::<Pointer>();
                    let tile_index = tile
                        .m_tileIndex
                        .prim_cast::<usize>();
                    flag_checker(
                        tile_sheets,
                        tile_sheet,
                        tile_index,
                    )
                };

                let blocked = (0..(height*width))
                    .map(|i_flat| {
                        let i = i_flat/height;
                        let j = i_flat%height;
                        let back_tile = back_layer.m_tiles[i,j];
                        let building_tile = building_layer.m_tiles[i,j];

                        let back_tile_blocked = back_tile.is_some()
                            && has_flag(check_passable_flag, back_tile);

                        let building_tile_blocked = building_tile.is_some()
                            && !has_flag(check_passable_flag, building_tile)
                            && !has_flag(check_shadow_flag, building_tile);

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
        })?;

        Ok(func)
    }
}

impl Bush {
    pub fn width(&self) -> usize {
        match self.kind {
            0 => 1, // Small bush, 1x1
            1 => 2, // Medium bush, 1x2
            2 => 3, // Large bush, 1x3
            3 => 1, // Green tea bush, 1x1
            4 => 3, // Walnut bush, 1x3
            _ => 0, // Should be unreachable
        }
    }

    pub fn rectangle(&self) -> Rectangle<isize> {
        Rectangle {
            top_left: self.top_left,
            shape: Vector {
                right: self.width() as isize,
                down: 1,
            },
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
        self.shadow.contains(&(tile_sheet, tile_index))
    }
}

impl std::fmt::Display for TreeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Oak => write!(f, "Oak"),
            Self::Maple => write!(f, "Maple"),
            Self::Pine => write!(f, "Pine"),
            Self::DesertPalm => write!(f, "DesertPalm"),
            Self::IslandPalm => write!(f, "IslandPalm"),
            Self::Mushroom => write!(f, "Mushroom"),
            Self::Mahogany => write!(f, "Mahogany"),
            Self::Mystic => write!(f, "Mystic"),
            Self::GreenRain => write!(f, "GreenRain"),
            Self::Fir => write!(f, "Fir"),
            Self::Birch => write!(f, "Birch"),
        }
    }
}

impl std::fmt::Display for LitterKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Stone => write!(f, "Stone"),
            Self::Wood => write!(f, "Wood"),
            Self::Fiber => write!(f, "Fiber"),
            Self::Other(other) => write!(f, "Other({other})"),
        }
    }
}
