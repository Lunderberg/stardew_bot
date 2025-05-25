use std::collections::{HashMap, HashSet};

use dotnet_debugger::{RustNativeObject, SymbolicGraph, SymbolicValue};
use itertools::{Either, Itertools as _};
use memory_reader::Pointer;

use crate::Error;

use super::{Inventory, Rectangle, TileMap, Vector};

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

    /// Dirt that has been cleared with a Hoe
    pub hoe_dirt: Vec<HoeDirt>,

    /// Trees.  This includes the small 1x1 stumps left behind after
    /// chopping down a tree, but not the larger 2x2 stumps made
    /// during worldgen.
    pub trees: Vec<Tree>,

    /// Location of grass.  Currently, does not distinguish between
    /// regular grass and blue grass, and does not read out the health
    /// of the grass.
    pub grass: Vec<Vector<isize>>,

    /// Objects on the ground
    pub objects: Vec<Object>,

    /// Placable furniture in the location
    pub furniture: Vec<Furniture>,

    /// Which tiles have water.
    pub water_tiles: Option<Vec<bool>>,

    pub buildings: Vec<Building>,

    /// Which tiles are blocked by impassable tiles, either in the
    /// "Back" layer or the "Buildings" layer.
    ///
    /// If either the layer has a non-null tile, and that tile is not
    /// explicitly marked with the "Passable" attribute, then the tile
    /// cannot be passed through.
    pub blocked: TileMap<bool>,

    /// Which tiles are marked as diggable, by having the "Diggable"
    /// property in the "Back" layer.
    pub diggable: TileMap<bool>,

    /// Tiles that can have some action performed on them.
    pub action_tiles: Vec<(Vector<isize>, String)>,
}

#[derive(RustNativeObject, Debug, Clone)]
pub struct LocationDelta {
    pub(crate) name: String,
    resource_clumps: Vec<ResourceClump>,
    hoe_dirt: Vec<HoeDirt>,
    trees: Vec<Tree>,
    grass: Vec<Vector<isize>>,
    objects: Vec<Object>,
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

#[derive(RustNativeObject, Debug, Clone)]
pub enum ResourceClumpKind {
    Stump,
    Boulder,
    Meteorite,
    MineBoulder,
    GiantCrop(String),
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
pub struct HoeDirt {
    pub position: Vector<isize>,
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
pub struct Object {
    pub tile: Vector<isize>,
    pub kind: ObjectKind,
}

#[derive(RustNativeObject, Debug, Clone)]
pub enum ObjectKind {
    Stone,
    Wood,
    Fiber,
    Chest(Inventory),
    Other(String),
}

#[derive(RustNativeObject, Debug, Clone)]
pub struct Furniture {
    pub shape: Rectangle<isize>,
    pub kind: FurnitureKind,
}

#[derive(RustNativeObject, Debug, Clone, Copy)]
pub enum FurnitureKind {
    Chair,
    Bench,
    Couch,
    Armchair,
    Dresser,
    LongTable,
    Painting,
    Lamp,
    Decor,
    Other,
    Bookcase,
    Table,
    Rug,
    Window,
    Fireplace,
    Bed,
    Torch,
    Sconce,
}

#[derive(RustNativeObject, Debug, Clone)]
pub struct Building {
    /// The size of the building
    pub shape: Rectangle<isize>,

    /// The door to the inside of the building
    pub door: Option<BuildingDoor>,

    /// An optional set of tiles that are solid within this building.
    /// If not set, all tiles are solid.
    pub collision_map: Option<Vec<Vector<isize>>>,
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
    diggable: HashSet<(Pointer, usize)>,
    tiles: Vec<MapTile>,
}

#[derive(RustNativeObject, Clone, Debug)]
struct MapTile {
    location: Vector<isize>,
    lookup_key: (Pointer, usize),
    // May change this to an enum later.  Currently, 0 for the "Back"
    // layer and 1 for the "Buildings" layer.
    layer_num: usize,
    properties: HashMap<String, Option<String>>,
}

#[derive(RustNativeObject, Default)]
struct BuildingDataLookup {
    collision_maps: HashMap<String, Vec<Vector<isize>>>,
}

impl Location {
    pub(crate) fn def_read_location(
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
            "new_hoe_dirt",
            |position: &Vector<isize>| HoeDirt {
                position: position.clone(),
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
            "new_litter_kind",
            |name: &str, _category: i32| -> Option<ObjectKind> {
                // if _category==-999 {
                //     return None;
                // }
                if name == "Twig" {
                    Some(ObjectKind::Wood)
                } else if name == "Stone" {
                    Some(ObjectKind::Stone)
                } else if name.to_lowercase().contains("weeds") {
                    Some(ObjectKind::Fiber)
                } else {
                    Some(ObjectKind::Other(name.into()))
                }
            },
        )?;

        graph.named_native_function(
            "new_chest_kind",
            |inventory: &Inventory| ObjectKind::Chest(inventory.clone()),
        )?;

        graph.named_native_function(
            "new_object",
            |tile: &Vector<isize>, kind: &ObjectKind| Object {
                tile: tile.clone(),
                kind: kind.clone(),
            },
        )?;

        graph.named_native_function(
            "new_furniture_kind",
            |kind: i32| -> FurnitureKind {
                kind.try_into().expect(
                    "TODO: Allow NativeFunction to propagate \
                     user-defined error types",
                )
            },
        )?;

        graph.named_native_function(
            "new_furniture",
            |shape: &Rectangle<isize>, kind: &FurnitureKind| -> Furniture {
                Furniture {
                    shape: shape.clone(),
                    kind: *kind,
                }
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

        graph.named_native_function("new_giant_crop_kind", |name: &str| {
            ResourceClumpKind::GiantCrop(name.to_string())
        })?;

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

        graph.named_native_function("new_building_data_lookup", || {
            BuildingDataLookup::default()
        })?;

        graph.named_native_function(
            "unpack_building_data_collision_map",
            |data: &mut BuildingDataLookup,
             building_type: &str,
             collision_map: &str| {
                data.unpack_collision_map(building_type, collision_map);
            },
        )?;

        graph.named_native_function(
            "new_building",
            |shape: &Rectangle<isize>,
             door: Option<&BuildingDoor>,
             data_lookup: &BuildingDataLookup,
             building_type: &str| {
                let shape = shape.clone();
                let door = door.cloned();
                let collision_map =
                    data_lookup.collision_maps.get(building_type).cloned();
                Building {
                    shape,
                    door,
                    collision_map,
                }
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
                sheets.define_sheet_property(tile_sheet, index_key);
            },
        )?;

        graph.named_native_function(
            "new_map_tile",
            |&location: &Vector<isize>,
             tile_sheet: Pointer,
             tile_index: usize,
             layer_num: usize| MapTile {
                location,
                lookup_key: (tile_sheet, tile_index),
                layer_num,
                properties: HashMap::default(),
            },
        )?;

        graph.named_native_function(
            "define_map_tile_property",
            |tile: &mut MapTile, key: &str, value: Option<&str>| {
                tile.properties.insert(key.into(), value.map(Into::into));
            },
        )?;

        graph.named_native_function(
            "add_map_tile",
            |sheets: &mut MapTileSheets, opt_tile: Option<&MapTile>| {
                if let Some(tile) = opt_tile {
                    sheets.tiles.push(tile.clone());
                }
            },
        )?;

        graph.named_native_function(
            "new_location",
            |name: &str,
             shape: &Vector<isize>,
             warps: &Vec<Warp>,
             resource_clumps: &Vec<ResourceClump>,
             grass: &Vec<Vector<isize>>,
             hoe_dirt: &Vec<HoeDirt>,
             trees: &Vec<Tree>,
             bushes: &Vec<Bush>,
             objects: &Vec<Object>,
             furniture: &Vec<Furniture>,
             water_tiles: &Vec<bool>,
             tiles: &MapTileSheets,
             buildings: &Vec<Building>| {
                Location {
                    name: name.into(),
                    shape: shape.clone(),
                    warps: warps.clone(),
                    resource_clumps: resource_clumps.clone(),
                    grass: grass.clone(),
                    hoe_dirt: hoe_dirt.clone(),
                    trees: trees.clone(),
                    bushes: bushes.clone(),
                    objects: objects.clone(),
                    furniture: furniture.clone(),
                    water_tiles: (!water_tiles.is_empty())
                        .then(|| water_tiles.clone()),
                    buildings: buildings.clone(),
                    blocked: tiles.collect_blocked_tiles(*shape),
                    diggable: tiles.collect_diggable_tiles(*shape),
                    action_tiles: tiles.collect_action_tiles(),
                }
            },
        )?;

        graph.named_native_function(
            "new_location_delta",
            |name: &str,
             resource_clumps: &Vec<ResourceClump>,
             grass: &Vec<Vector<isize>>,
             hoe_dirt: &Vec<HoeDirt>,
             trees: &Vec<Tree>,
             objects: &Vec<Object>| {
                LocationDelta {
                    name: name.into(),
                    resource_clumps: resource_clumps.clone(),
                    grass: grass.clone(),
                    hoe_dirt: hoe_dirt.clone(),
                    trees: trees.clone(),
                    objects: objects.clone(),
                }
            },
        )?;

        graph.parse(
            "
        let building_data_dict = StardewValley
             .Game1
             .buildingData
             .as::<System.Collections.Generic.Dictionary`2<
                      System.String,
                      StardewValley.GameData.Buildings.BuildingData
             >>();
        ",
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

            let building_data = {
                let dict = building_data_dict;
                let num_entries = dict._entries.len();

                (0..num_entries)
                    .reduce(
                        new_building_data_lookup(),
                        |lookup, i| {
                            let entry = dict._entries[i];

                            let building_type = entry
                                .key
                                .read_string();

                            let collision_map = entry
                                .value
                                .CollisionMap
                                .read_string();

                            let lookup = unpack_building_data_collision_map(
                                lookup,
                                building_type,
                                collision_map,
                            );

                            lookup
                        }
                    )
            };

            fn read_location_resource_clumps(location, filter) {
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
                    .filter(|clump| filter.is_none() || filter(clump))
                    .map(|clump| {
                        let shape = {
                            let right = clump.netTile.value.X;
                            let down = clump.netTile.value.Y;
                            let width = clump.width.value;
                            let height = clump.height.value;
                            new_rectangle(right,down,width,height)
                        };

                        let giant_crop_kind = clump
                            .as::<StardewValley.TerrainFeatures.GiantCrop>()
                            .netId
                            .value
                            .read_string();

                        let kind = if giant_crop_kind.is_some() {
                            new_giant_crop_kind(giant_crop_kind)
                        } else {
                            let kind_index = clump.parentSheetIndex.value;
                            new_resource_clump_kind(kind_index)
                        };
                        new_resource_clump(shape, kind)
                    })
                    .filter(|obj| obj.is_some())
                    .collect();

                resource_clumps
            }

            fn read_location_grass(location, filter) {
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
                    .filter(|feature| filter.is_none() || filter(feature))
                    .map(|feature| {
                        let right = feature.key.X;
                        let down = feature.key.Y;
                        new_isize_vector(right,down)
                    })
                    .collect();

                grass
            }

            fn read_location_hoe_dirt(location, filter) {
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

                let hoe_dirt = iter_features
                    .filter(|feature| feature
                            .value
                            .value
                            .as::<StardewValley.TerrainFeatures.HoeDirt>()
                            .is_some())
                    .filter(|feature| filter.is_none() || filter(feature))
                    .map(|feature| {
                        let hoe_dirt = feature
                            .value
                            .value
                            .as::<StardewValley.TerrainFeatures.HoeDirt>();
                        let position = {
                            let right = feature.key.X;
                            let down = feature.key.Y;
                            new_isize_vector(right,down)
                        };
                        new_hoe_dirt(
                            position,
                        )
                    })
                    .collect();

                hoe_dirt
            }

            fn read_location_trees(location, filter) {
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

                let trees = iter_features
                    .filter(|feature| feature
                            .value
                            .value
                            .as::<StardewValley.TerrainFeatures.Tree>()
                            .is_some())
                    .filter(|feature| filter.is_none() || filter(feature))
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

                trees
            }

            fn read_location_objects(location, filter) {
                let num_objects = location
                    .objects
                    .compositeDict
                    ._entries
                    .len();
                let objects = (0..num_objects)
                    .map(|i| {
                        location
                            .objects
                            .compositeDict
                            ._entries[i]
                            .value
                    })
                    .filter(|obj| filter.is_none() || filter(obj))
                    .map(|obj| {
                        let tile = {
                            let right = obj.tileLocation.value.X;
                            let down = obj.tileLocation.value.Y;
                            new_isize_vector(right,down)
                        };

                        let chest = obj.as::<StardewValley.Objects.Chest>();
                        let kind = if chest.is_some() {
                            let inventory = read_inventory(chest.netItems.value);
                            new_chest_kind(inventory)
                        } else {
                            let name = obj.netName.value.read_string();
                            let category = obj.category.value;
                            new_litter_kind(name,category)
                        };

                        new_object(tile,kind)
                    })
                    .filter(|obj| obj.is_some())
                    .collect();
                objects
            }

            fn read_location_furniture(location, filter) {
                let furniture_list = location
                    .furniture
                    .list;

                let num_furniture = furniture_list
                    ._size
                    .prim_cast::<usize>();

                let furniture = (0..num_furniture)
                    .map(|i| {
                        furniture_list
                            ._items[i]
                    })
                    .filter(|piece| filter.is_none() || filter(piece))
                    .map(|piece| {
                        let shape = {
                            let right = piece.tileLocation.value.X;
                            let down = piece.tileLocation.value.Y;
                            let width = piece.boundingBox.value.Width / 64;
                            let height = piece.boundingBox.value.Height / 64;
                            new_rectangle(right, down, width, height)
                        };

                        let kind = new_furniture_kind(
                            piece.furniture_type.value
                        );

                        new_furniture(shape, kind)
                    })
                    .collect();

                furniture
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


                let resource_clumps = read_location_resource_clumps(
                    location, None);
                let grass = read_location_grass(location, None);
                let hoe_dirt = read_location_hoe_dirt(location, None);
                let trees = read_location_trees(location, None);

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

                let objects = read_location_objects(location, None);
                let furniture = read_location_furniture(location, None);

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

                        let building_type = building
                            .buildingType
                            .value
                            .read_string();

                        new_building(shape, door, building_data, building_type)
                    })
                    .collect();

                // Structure to hold tile-based parameters, which must
                // be unpacked from their string representation.
                let tile_sheets = new_tile_sheets(num_buildings);

                // Iterate through all the sheets, copying data over
                // to the MapTileSheets.  These are lookup tables that
                // may be referenced by different tiles.
                let num_tile_sheets = location
                    .map
                    .m_tileSheets
                    ._size
                    .prim_cast::<usize>();
                let tile_sheets = (0..num_tile_sheets)
                    .reduce(
                        tile_sheets,
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

                fn extract_map_tile(layer, layer_num, i, j) {
                    let loc = new_isize_vector(i,j);

                    let tile = layer.m_tiles[i,j]
                        .as::<xTile.Tiles.StaticTile>();

                    if tile.is_some() {
                        let tile_sheet = tile
                            .m_tileSheet
                            .prim_cast::<Pointer>();
                        let tile_index = tile
                            .m_tileIndex
                            .prim_cast::<usize>();
                        let map_tile = new_map_tile(
                            loc,
                            tile_sheet,
                            tile_index,
                            layer_num
                        );

                        let tile_props = tile
                            .m_propertyCollection;

                        let num_tile_props = tile_props
                            ._count
                            .prim_cast::<usize>();

                        (0..num_tile_props)
                            .map(|i_prop| tile_props._entries[i_prop])
                            .reduce(
                                map_tile,
                                |map_tile, prop| {
                                    let key = prop.key.read_string();
                                    let value = prop
                                        .value
                                        .m_value
                                        .as::<System.String>()
                                        .read_string();
                                    define_map_tile_property(
                                        map_tile,
                                        key,
                                        value
                                    )
                                })

                    } else {
                        None
                    }
                }

                let tile_sheets = (0..(height*width))
                    .reduce(tile_sheets, |tile_sheets, i_flat| {
                        let i = i_flat/height;
                        let j = i_flat%height;

                        let back_tile = extract_map_tile(
                            back_layer,
                            0, i, j);
                        let tile_sheets = add_map_tile(tile_sheets, back_tile);


                        let building_tile = extract_map_tile(
                            building_layer,
                            1, i, j);
                        let tile_sheets = add_map_tile(tile_sheets, building_tile);

                        tile_sheets
                    });

                new_location(
                    name,
                    shape,
                    warps,
                    resource_clumps,
                    grass,
                    hoe_dirt,
                    trees,
                    bushes,
                    objects,
                    furniture,
                    flattened_water_tiles,
                    tile_sheets,
                    buildings,
                )
            }

            fn read_location_delta() {
                let player = StardewValley.Game1._player;
                let location = player
                    .currentLocationRef
                    ._gameLocation;

                let name = get_location_name_ptr(location).read_string();

                let player_pos = player.position.Field.value;
                let player_x = player_pos.X / 64;
                let player_y = player_pos.Y / 64;

                fn is_close_to_player(x,y) {
                    let diff_x = player_x - x;
                    let diff_y = player_y - y;
                    let dist2 = diff_x*diff_x + diff_y*diff_y;

                    dist2.is_some() && dist2 < 36
                }

                let resource_clumps = read_location_resource_clumps(
                    location,
                    |clump| {
                        let pos = clump.netTile.value;
                        is_close_to_player(pos.X, pos.Y)
                    }
                );
                let grass = read_location_grass(
                    location,
                    |feature| {
                        let pos = feature.key;
                        is_close_to_player(pos.X, pos.Y)
                    }
                );
                let hoe_dirt = read_location_hoe_dirt(
                    location,
                    |feature| {
                        let pos = feature.key;
                        is_close_to_player(pos.X, pos.Y)
                    }
                );
                let trees = read_location_trees(
                    location,
                    |feature| {
                        let pos = feature.key;
                        is_close_to_player(pos.X, pos.Y)
                    }
                );
                let objects = read_location_objects(
                    location,
                    |obj| {
                        let pos = obj.tileLocation.value;
                        is_close_to_player(pos.X, pos.Y)
                    }
                );

                new_location_delta(
                    name,
                    resource_clumps,
                    grass,
                    hoe_dirt,
                    trees,
                    objects,
                )
            }
        })?;

        Ok(func)
    }

    pub(crate) fn add_building_warps(locations: &mut [Location]) {
        let entrance_lookup: HashMap<String, Vector<isize>> = locations
            .iter()
            .filter_map(|loc| {
                loc.warps
                    .get(0)
                    .map(|warp| warp.location + Vector::new(0, -1))
                    .map(|entrance| (loc.name.clone(), entrance))
            })
            .collect();

        for location in locations.iter_mut() {
            location
                .buildings
                .iter()
                .filter_map(|building| {
                    building.door.as_ref().map(|door| (building, door))
                })
                .filter_map(|(building, door)| {
                    entrance_lookup.get(&door.inside_name).map(|target| {
                        let location =
                            building.shape.top_left + door.relative_location;
                        Warp {
                            location,
                            target: *target,
                            target_room: door.inside_name.clone(),
                            kind: WarpKind::Door,
                        }
                    })
                })
                .for_each(|warp| location.warps.push(warp));
        }
    }

    pub fn collect_clear_tiles(&self) -> TileMap<bool> {
        let height = self.shape.down as usize;

        let mut map = self.blocked.map(|b| !b);

        let iter_water = self
            .water_tiles
            .iter()
            .flat_map(|vec_bool| vec_bool.iter())
            .enumerate()
            .filter(|(_, is_water)| **is_water)
            .map(|(index, _)| {
                let i = (index / height) as isize;
                let j = (index % height) as isize;
                Vector::new(i, j)
            });

        let iter_clumps = self
            .resource_clumps
            .iter()
            .flat_map(|clump| clump.shape.iter_points());

        let iter_bush = self
            .bushes
            .iter()
            .flat_map(|bush| bush.rectangle().iter_points());

        let iter_tree = self.trees.iter().map(|tree| tree.position);

        let iter_objects = self.objects.iter().map(|obj| obj.tile);

        let iter_furniture = self
            .furniture
            .iter()
            .filter(|piece| !matches!(piece.kind, FurnitureKind::Rug))
            .flat_map(|piece| piece.shape.iter_points());

        let iter_buildings = self
            .buildings
            .iter()
            .flat_map(|building| building.iter_tiles());

        std::iter::empty()
            .chain(iter_water)
            .chain(iter_clumps)
            .chain(iter_bush)
            .chain(iter_tree)
            .chain(iter_objects)
            .chain(iter_furniture)
            .chain(iter_buildings)
            .for_each(|tile| {
                map[tile] = false;
            });

        map
    }

    pub fn apply_delta(
        &mut self,
        delta: LocationDelta,
        player_pos: Vector<f32>,
    ) {
        let player_tile = player_pos / 64.0;
        let is_far_from_player = |tile: Vector<isize>| {
            let pos = tile.map(|x| x as f32);
            let dist2 = pos.dist2(player_tile);
            dist2 >= 25.0
        };

        self.resource_clumps = delta
            .resource_clumps
            .into_iter()
            .chain(
                self.resource_clumps
                    .drain(..)
                    .filter(|clump| is_far_from_player(clump.shape.center())),
            )
            .unique_by(|clump| clump.shape.center())
            .collect();

        self.trees = delta
            .trees
            .into_iter()
            .chain(
                self.trees
                    .drain(..)
                    .filter(|tree| is_far_from_player(tree.position)),
            )
            .unique_by(|tree| tree.position)
            .collect();

        self.hoe_dirt = delta
            .hoe_dirt
            .into_iter()
            .chain(
                self.hoe_dirt
                    .drain(..)
                    .filter(|hoe_dirt| is_far_from_player(hoe_dirt.position)),
            )
            .unique_by(|hoe_dirt| hoe_dirt.position)
            .collect();

        self.grass = delta
            .grass
            .into_iter()
            .chain(
                self.grass
                    .drain(..)
                    .filter(|grass_pos| is_far_from_player(*grass_pos)),
            )
            .unique()
            .collect();

        self.objects = delta
            .objects
            .into_iter()
            .chain(
                self.objects
                    .drain(..)
                    .filter(|obj| is_far_from_player(obj.tile)),
            )
            .unique_by(|obj| obj.tile)
            .collect();
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
            622 => Ok(Self::Meteorite),
            752 | 754 | 756 | 758 => Ok(Self::MineBoulder),

            other => Err(Error::UnrecognizedResourceClump(other)),
        }
    }
}

impl TryFrom<i32> for FurnitureKind {
    type Error = Error;

    fn try_from(value: i32) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Self::Chair),
            1 => Ok(Self::Bench),
            2 => Ok(Self::Couch),
            3 => Ok(Self::Armchair),
            4 => Ok(Self::Dresser),
            5 => Ok(Self::LongTable),
            6 => Ok(Self::Painting),
            7 => Ok(Self::Lamp),
            8 => Ok(Self::Decor),
            9 => Ok(Self::Other),
            10 => Ok(Self::Bookcase),
            11 => Ok(Self::Table),
            12 => Ok(Self::Rug),
            13 => Ok(Self::Window),
            14 => Ok(Self::Fireplace),
            15 => Ok(Self::Bed),
            16 => Ok(Self::Torch),
            17 => Ok(Self::Sconce),

            other => Err(Error::UnrecognizedResourceClump(other)),
        }
    }
}

impl MapTileSheets {
    fn define_sheet(&mut self, tile_sheet: Pointer) {
        self.known_sheets.insert(tile_sheet);
    }

    fn define_sheet_property(&mut self, tile_sheet: Pointer, index_key: &str) {
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
                "Diggable" => {
                    self.diggable.insert((tile_sheet, index));
                }
                _ => {}
            });
    }

    fn collect_blocked_tiles(&self, shape: Vector<isize>) -> TileMap<bool> {
        let mut map = TileMap::empty(shape.right as usize, shape.down as usize);
        self.tiles
            .iter()
            .filter(|tile| match tile.layer_num {
                0 => self.passable.contains(&tile.lookup_key),
                1 => {
                    !self.passable.contains(&tile.lookup_key)
                        && !self.shadow.contains(&tile.lookup_key)
                }
                _ => unreachable!("Only 0 and 1 as allowed values"),
            })
            .map(|tile| tile.location)
            .for_each(|loc| {
                map[loc] = true;
            });
        map
    }

    fn collect_diggable_tiles(&self, shape: Vector<isize>) -> TileMap<bool> {
        let mut map = TileMap::empty(shape.right as usize, shape.down as usize);
        self.tiles
            .iter()
            .filter(|tile| match tile.layer_num {
                0 => self.diggable.contains(&tile.lookup_key),
                _ => false,
            })
            .map(|tile| tile.location)
            .for_each(|loc| {
                map[loc] = true;
            });
        map
    }

    fn collect_action_tiles(&self) -> Vec<(Vector<isize>, String)> {
        self.tiles
            .iter()
            .filter(|tile| tile.layer_num == 1)
            .flat_map(|tile| {
                tile.properties
                    .iter()
                    .filter(|(key, _)| key.as_str() == "Action")
                    .filter_map(|(_, opt_value)| opt_value.as_ref())
                    .map(|value| (tile.location, value))
            })
            .filter(|(_, action)| {
                // Filter out some actions that are either handled
                // elsewhere, such as warps and doors, or aren't worth
                // tracking for now, such as messages.
                [
                    "Message",
                    "Letter",
                    "Warp",
                    "Door",
                    "Notes",
                    "playSound",
                    "LockedDoorWarp",
                ]
                .into_iter()
                .all(|special| !action.starts_with(special))
            })
            .map(|(tile, action)| (tile, action.to_string()))
            .collect()
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

impl std::fmt::Display for ObjectKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Stone => write!(f, "Stone"),
            Self::Wood => write!(f, "Wood"),
            Self::Fiber => write!(f, "Fiber"),
            Self::Chest(inventory) => {
                write!(
                    f,
                    "Chest with {} slots, {} full",
                    inventory.items.len(),
                    inventory
                        .items
                        .iter()
                        .filter(|item| item.is_some())
                        .count()
                )
            }
            Self::Other(other) => write!(f, "Other({other})"),
        }
    }
}

impl BuildingDataLookup {
    fn unpack_collision_map(
        &mut self,
        building_type: &str,
        collision_map: &str,
    ) {
        let collision_map = collision_map
            .trim()
            .lines()
            .enumerate()
            .flat_map(|(down, line)| {
                line.trim()
                    .chars()
                    .enumerate()
                    .filter(|(_, c)| matches!(c, 'X'))
                    .map(move |(right, _)| {
                        Vector::new(right as isize, down as isize)
                    })
            })
            .collect();

        self.collision_maps
            .insert(building_type.into(), collision_map);
    }
}

impl Building {
    pub fn iter_tiles(&self) -> impl Iterator<Item = Vector<isize>> + '_ {
        if let Some(collisions) = &self.collision_map {
            Either::Left(
                collisions
                    .iter()
                    .cloned()
                    .map(|point| point + self.shape.top_left),
            )
        } else {
            Either::Right(self.shape.iter_points())
        }
    }
}
