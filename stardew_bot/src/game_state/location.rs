use std::{
    collections::{HashMap, HashSet},
    ops::Deref,
};

use dotnet_debugger::{RustNativeObject, SymbolicGraph, SymbolicValue};
use itertools::{Either, Itertools as _};
use memory_reader::Pointer;

use crate::Error;

use super::{Inventory, ItemCategory, ItemId, Rectangle, TileMap, Vector};

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

    /// Objects that occupy a single tile of the map.
    pub objects: Vec<Object>,

    /// Items that may be picked up.
    pub items: Vec<FloatingItem>,

    /// Placable furniture in the location
    pub furniture: Vec<Furniture>,

    /// Which tiles have water.
    pub water_tiles: TileMap<bool>,

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

    /// Any Villagers or Monsters in the area
    pub characters: Vec<Character>,

    /// Additional values that are only present for the mines.
    pub mineshaft_details: Option<MineshaftDetails>,
}

#[derive(RustNativeObject, Debug, Clone)]
pub struct MineshaftDetails {
    /// Which level of the mines this location is located at.
    pub mineshaft_level: i32,

    /// If true, this level was initially generated with a ladder.
    /// For these levels, rocks will never produce a ladder when
    /// broken.
    pub generated_ladder: bool,
}

#[derive(RustNativeObject, Debug, Clone)]
pub struct LocationDelta {
    pub(crate) name: String,
    resource_clumps: Vec<ResourceClump>,
    objects: Vec<Object>,
    items: Vec<FloatingItem>,
    characters: Vec<Character>,
}

#[derive(RustNativeObject, Debug, Clone)]
pub struct Warp {
    pub location: Vector<isize>,
    pub target: Vector<isize>,
    pub target_room: String,
    pub kind: WarpKind,
    pub requires_friendship: Option<String>,
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

/// A tile of dirt that has been cleared with a Hoe.
#[derive(Debug, Clone)]
pub struct HoeDirt {
    pub is_watered: bool,
    pub crop: Option<Crop>,
}

#[derive(Debug, Clone)]
pub struct Crop {
    pub phase: CropPhase,
    pub seed: ItemId,
}

#[derive(Debug, Clone, Copy)]
pub enum CropPhase {
    Seed,
    Growing,
    Harvestable,
    Regrowing,
}

#[derive(Debug, Clone)]
pub struct Chest {
    /// The contents of the chest.
    pub inventory: Inventory,

    /// The chest is currently in the process of being opened.  Do not
    /// need to re-click on the chest.
    pub is_opening: bool,
}

#[derive(Debug, Clone)]
pub struct Furnace {
    /// True if the product is ready to harvest.  Otherwise, false.
    pub ready_to_harvest: bool,

    /// True if the machine has been loaded up with ingredients.
    /// Otherwise, false.
    pub has_held_item: bool,
}

#[derive(Debug, Clone)]
pub enum Sprinkler {
    Regular,
    Quality,
    Iridium,
}

/// Non-fruit trees.  This includes the small 1x1 stumps left behind
/// after chopping down a tree, but not the larger 2x2 stumps made
/// during worldgen.
#[derive(Debug, Clone)]
pub struct Tree {
    #[allow(dead_code)]
    pub kind: TreeKind,
    #[allow(dead_code)]
    pub growth_stage: i32,
    #[allow(dead_code)]
    pub has_seed: bool,
    #[allow(dead_code)]
    pub is_stump: bool,
    pub health: f32,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

#[derive(Debug, Clone)]
pub struct FruitTree {
    pub kind: FruitTreeKind,
    pub num_fruit: usize,
}

#[derive(Debug, Clone)]
pub enum FruitTreeKind {
    Cherry,
    Apricot,
    Orange,
    Peach,
    Pomegranate,
    Apple,
    // From Stardew Expanded
    Pear,
    // From Stardew Expanded
    Nectarine,
    // From Stardew Expanded
    Persimmon,
    // From Stardew Expanded
    Money,
}

#[derive(RustNativeObject, Debug, Clone)]
pub struct Object {
    pub tile: Vector<isize>,
    pub kind: ObjectKind,
}

#[derive(RustNativeObject, Debug, Clone)]
pub enum ObjectKind {
    /// The small 1x1 stones on the farm or in the underground mines
    Stone(StoneKind),

    /// A mineral that may be collected from the ground (e.g. Quartz)
    Mineral(MineralKind),

    /// The small 1x1 logs on the farm.  Larger 2x2 stumps are under
    /// LargeTerrainFeatures.
    Wood,

    /// The weeds that grow on the farm.
    Fiber,

    /// Grass.  Currently, only indicates the presence of grass, and
    /// does not distinguish between regular grass and blue grass, nor
    /// does it specify the health of the grass.
    Grass,

    /// Appears on Spring 17
    PotOfGold,

    /// A non-fruit tree (e.g. Oak/Maple)
    Tree(Tree),

    /// A fruit-producing tree (e.g. Apple/Pomegranate)
    FruitTree(FruitTree),

    /// A tile that has been cleared with the Hoe.  May or may not
    /// contain a crop.
    HoeDirt(HoeDirt),

    /// A tile that contains a chest, which may contain items inside
    /// it.
    Chest(Chest),

    /// Gives artifacts when dug
    ArtifactSpot,

    /// Gives seasonal seeds when dug
    SeedSpot,

    /// The ladder to return from the mines to the surface
    MineLadderUp,

    /// A ladder to descend deeper into the mines
    MineLadderDown,

    /// A hole that may be jumped down in skull cavern
    MineHoleDown,

    /// The elevator in the underground mines.
    MineElevator,

    /// A cart in the mines that contains coal.
    MineCartCoal,

    /// A breakable barrel in the mines
    MineBarrel,

    /// A furnace in which ore can be smelted.
    Furnace(Furnace),

    /// A sprinkler
    Sprinkler(Sprinkler),

    /// A scarecrow.  Currently only checks for the default scarecrow
    /// type, and not for any of the rarecrows, or the upgraded
    /// scarecrow.
    Scarecrow,

    /// A placable torch
    Torch,

    /// A tile that contains an unknown object or terrain feature
    /// whose name is known, but for which unpacking has not yet been
    /// implemented.
    Other {
        category: ItemCategory,
        name: String,
        id: ItemId,
    },

    /// A tile that contains an unknown object or terrain feature,
    /// where not even the name is known.
    Unknown,
}

#[derive(Debug, Clone)]
pub enum StoneKind {
    /// A normal stone
    Normal,

    /// The light-gray stone that produces extra stone when mined.
    DoubleStone,

    /// Drops copper ore
    Copper,

    Iron,

    Gold,

    Iridium,

    Gem,

    Mystic,

    Diamond,
    Ruby,
    Jade,
    Amethyst,
    Topaz,
    Emerald,
    Aquamarine,

    Other {
        name: String,
        id: String,
    },
}

#[derive(Debug, Clone)]
pub enum MineralKind {
    Quartz,
    EarthCrystal,
    FrozenTear,
    FireQuartz,
    Other { name: String, id: String },
}

/// An item on the ground that may be picked up
///
/// Deliberately does not use the `Item` struct internally, as not all
/// fields are populated until the item has been picked up.
#[derive(RustNativeObject, Debug, Clone)]
pub struct FloatingItem {
    pub position: Vector<f32>,
    pub id: ItemId,
    pub count: usize,
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
    /// What type of building this is
    pub kind: String,

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

#[derive(RustNativeObject, Debug, Clone)]
pub struct Character {
    /// The name of the NPC/Monster
    pub name: String,

    /// The pixel position of the character.  Must be divided by 64 to
    /// get the tile position.
    pub position: Vector<f32>,

    pub sprite_width: i32,

    /// The health of the character.  Only present for monsters.
    pub health: Option<i32>,

    pub is_invisible_duggy: bool,

    pub is_waiting_rock_crab: bool,
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
            "new_warp",
            |location: &Vector<isize>,
             target: &Vector<isize>,
             target_room: &str| Warp {
                location: location.clone(),
                target: target.clone(),
                target_room: target_room.into(),
                kind: WarpKind::Automatic,
                requires_friendship: None,
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
                                requires_friendship: None,
                            })
                        }
                        Some("LoadMap") => {
                            // LoadMap TARGETNAME RIGHT DOWN ???
                            let target_room = iter_words.next()?.to_string();
                            let target = {
                                let right: isize = iter_words
                                    .next()
                                    .and_then(|right| right.parse().ok())?;
                                let down: isize = iter_words
                                    .next()
                                    .and_then(|right| right.parse().ok())?;
                                Vector::new(right, down)
                            };
                            Some(Warp {
                                location,
                                target,
                                target_room,
                                kind: WarpKind::Automatic,
                                requires_friendship: None,
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

                            let requires_friendship =
                                iter_words.next().map(|name| name.to_string());

                            Some(Warp {
                                location,
                                target,
                                target_room,
                                kind: WarpKind::LockedDoor { opens, closes },
                                requires_friendship,
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
            |is_watered: bool,
             crop_phase: Option<usize>,
             crop_seed: Option<&str>| {
                let crop = if crop_phase.is_some() && crop_seed.is_some() {
                    let phase = match crop_phase.unwrap() {
                        0 => CropPhase::Seed,
                        1 => CropPhase::Growing,
                        2 => CropPhase::Harvestable,
                        3 => CropPhase::Regrowing,
                        other => {
                            unreachable!("Value {other} should not be produced")
                        }
                    };
                    let seed =
                        ItemId::new(format!("(O){}", crop_seed.unwrap()));
                    Some(Crop { phase, seed })
                } else {
                    None
                };

                ObjectKind::HoeDirt(HoeDirt { is_watered, crop })
            },
        )?;

        graph.named_native_function(
            "new_tree_kind",
            |kind: &str,
             growth_stage: i32,
             has_seed: bool,
             is_stump: bool,
             health: f32| {
                ObjectKind::Tree(Tree {
                    kind: kind.parse().unwrap(),
                    growth_stage,
                    has_seed,
                    is_stump,
                    health,
                })
            },
        )?;

        graph.named_native_function(
            "new_fruit_tree_kind",
            |kind: &str, num_fruit: usize| {
                ObjectKind::FruitTree(FruitTree {
                    kind: kind.parse().unwrap(),
                    num_fruit,
                })
            },
        )?;

        graph.named_native_function("new_grass_kind", |_: Pointer| {
            ObjectKind::Grass
        })?;

        graph.named_native_function(
            "new_bush",
            |kind: usize, top_left: &Vector<isize>| Bush {
                kind,
                top_left: top_left.clone(),
            },
        )?;

        graph.named_native_function(
            "new_mineral_kind",
            |name: &str, id: &str| -> ObjectKind {
                match id {
                    "(O)80" => ObjectKind::Mineral(MineralKind::Quartz),
                    "(O)86" => ObjectKind::Mineral(MineralKind::EarthCrystal),
                    "(O)84" => ObjectKind::Mineral(MineralKind::FrozenTear),
                    "(O)82" => ObjectKind::Mineral(MineralKind::FireQuartz),
                    _ => ObjectKind::Mineral(MineralKind::Other {
                        name: name.to_string(),
                        id: id.to_string(),
                    }),
                }
            },
        )?;

        graph.named_native_function(
            "new_litter_kind",
            |name: &str, id: &str| -> ObjectKind {
                match name {
                    "Twig" => ObjectKind::Wood,
                    "Stone" => {
                        let stone_kind = match id {
                            "(O)32" | "(O)34" | "(O)36" | "(O)38" | "(O)40"
                            | "(O)42" | "(O)48" | "(O)50" | "(O)52"
                            | "(O)54" | "(O)56" | "(O)58" => StoneKind::Normal,

                            "(O)668" | "(O)670" => StoneKind::DoubleStone,
                            "(O)751" | "(O)859" => StoneKind::Copper,
                            "(O)290" | "(O)860" => StoneKind::Iron,
                            "(O)764" | "(O)VolcanoGoldNode" => StoneKind::Gold,
                            "(O)765" => StoneKind::Iridium,

                            "(O)44" => StoneKind::Gem,
                            "(O)46" => StoneKind::Mystic,

                            "(O)2" => StoneKind::Diamond,
                            "(O)4" => StoneKind::Ruby,
                            "(O)6" => StoneKind::Jade,
                            "(O)8" => StoneKind::Amethyst,
                            "(O)10" => StoneKind::Topaz,
                            "(O)12" => StoneKind::Emerald,
                            "(O)14" => StoneKind::Aquamarine,

                            _ => StoneKind::Other {
                                name: name.to_string(),
                                id: id.to_string(),
                            },
                        };
                        ObjectKind::Stone(stone_kind)
                    }
                    "PotOfGold" => ObjectKind::PotOfGold,
                    name if name.to_lowercase().contains("weeds") => {
                        ObjectKind::Fiber
                    }
                    other => todo!(
                        "Handle item '{other}' with id '{id}', \
                         when it occurs as a type of litter \
                         (category == -999)"
                    ),
                }
            },
        )?;

        graph.named_native_function(
            "new_mineshaft_tile",
            |index: i32| -> Option<ObjectKind> {
                match index {
                    115 => Some(ObjectKind::MineLadderUp),
                    173 => Some(ObjectKind::MineLadderDown),
                    174 => Some(ObjectKind::MineHoleDown),
                    112 => Some(ObjectKind::MineElevator),
                    194 => Some(ObjectKind::MineCartCoal),
                    _ => None,
                }
            },
        )?;

        graph.named_native_function(
            "new_other_object_kind",
            |category: i32,
             sheet_index: i32,
             name: &str,
             id: &str|
             -> ObjectKind {
                let category: ItemCategory = category.into();
                match (category, sheet_index, name) {
                    (
                        ItemCategory::BigCraftable,
                        118 | 119 | 120 | 121 | 122 | 123 | 124 | 125,
                        _,
                    ) => ObjectKind::MineBarrel,
                    (ItemCategory::Other(0), 93, _) => ObjectKind::Torch,
                    (ItemCategory::Craftable, 599, _) => {
                        ObjectKind::Sprinkler(Sprinkler::Regular)
                    }
                    (ItemCategory::Craftable, 621, _) => {
                        ObjectKind::Sprinkler(Sprinkler::Quality)
                    }
                    (ItemCategory::Craftable, 645, _) => {
                        ObjectKind::Sprinkler(Sprinkler::Iridium)
                    }
                    (ItemCategory::BigCraftable, 8, _) => ObjectKind::Scarecrow,
                    (_, _, "Artifact Spot") => ObjectKind::ArtifactSpot,
                    (_, _, "Seed Spot") => ObjectKind::SeedSpot,
                    _ => ObjectKind::Other {
                        category,
                        name: name.to_string(),
                        id: ItemId::new(id.to_string()),
                    },
                }
            },
        )?;

        graph.named_native_function(
            "new_unknown_object_kind",
            |_: Pointer| ObjectKind::Unknown,
        )?;

        graph.named_native_function(
            "new_chest_kind",
            |inventory: &Inventory, special_kind: usize, is_opening: bool| {
                let capacity: usize = match special_kind {
                    0 => 36, // Normal
                    1 => 9,  // Mini Shipping Bin
                    2 => 9,  // Jumino Chest
                    3 => 36, // AutoLoader
                    4 => 1,  // Enricher
                    5 => 70, // Big chest (either wood or stone)
                    other => panic!("Invalid chest kind: {other}"),
                };
                let iter_explicit_items = inventory.items.iter().cloned();
                let num_trailing_slots = capacity
                    .checked_sub(inventory.items.len())
                    .unwrap_or_else(|| {
                        panic!(
                            "Inventory had {} items, \
                             but chest type {special_kind} \
                             only has capacity of {capacity}.",
                            inventory.items.len()
                        )
                    });
                let iter_trailing_empty_slots =
                    (0..num_trailing_slots).map(|_| None);

                let inventory = Inventory {
                    items: iter_explicit_items
                        .chain(iter_trailing_empty_slots)
                        .collect(),
                };

                ObjectKind::Chest(Chest {
                    inventory,
                    is_opening,
                })
            },
        )?;

        graph.named_native_function(
            "new_furnace_kind",
            |ready_to_harvest: bool, has_held_item: bool| {
                ObjectKind::Furnace(Furnace {
                    ready_to_harvest,
                    has_held_item,
                })
            },
        )?;

        graph.named_native_function(
            "new_object",
            |tile: &Vector<isize>, kind: &ObjectKind| Object {
                tile: tile.clone(),
                kind: kind.clone(),
            },
        )?;

        graph.named_native_function(
            "new_floating_item",
            |right: f32,
             down: f32,
             item_id: &str,
             quality: i32,
             count: usize| FloatingItem {
                position: Vector::new(right, down),
                id: ItemId::new(item_id.to_string())
                    .with_quality(quality.try_into().unwrap()),
                count,
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
                    kind: building_type.to_string(),
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
            "new_character",
            |name: &str,
             x: f32,
             y: f32,
             sprite_width: i32,
             health: Option<i32>,
             is_invisible_duggy: bool,
             is_waiting_rock_crab: bool|
             -> Character {
                Character {
                    name: name.to_string(),
                    position: Vector::new(x, y),
                    sprite_width,
                    health,
                    is_invisible_duggy,
                    is_waiting_rock_crab,
                }
            },
        )?;

        graph.named_native_function(
            "new_mineshaft_details",
            |mineshaft_level: i32, generated_ladder: bool| MineshaftDetails {
                mineshaft_level,
                generated_ladder,
            },
        )?;

        graph.named_native_function(
            "new_location",
            |name: &str,
             shape: &Vector<isize>,
             warps: &Vec<Warp>,
             resource_clumps: &Vec<ResourceClump>,
             bushes: &Vec<Bush>,
             objects: &Vec<Object>,
             items: &Vec<FloatingItem>,
             furniture: &Vec<Furniture>,
             water_tiles: &Vec<bool>,
             tiles: &MapTileSheets,
             buildings: &Vec<Building>,
             characters: &Vec<Character>,
             mineshaft_details: Option<&MineshaftDetails>| {
                let blocked = tiles.collect_blocked_tiles(*shape);
                let diggable = tiles.collect_diggable_tiles(*shape);
                let action_tiles = tiles.collect_action_tiles();
                let warps = warps
                    .iter()
                    .cloned()
                    .chain(tiles.iter_extra_warps())
                    .collect();

                let water_tiles = {
                    let mut map = TileMap::<bool>::empty(
                        blocked.width(),
                        blocked.height(),
                    );
                    let height = map.height();

                    water_tiles
                        .iter()
                        .enumerate()
                        .filter(|(_, is_water)| **is_water)
                        .map(move |(index, _)| {
                            let i = (index / height) as isize;
                            let j = (index % height) as isize;
                            Vector::new(i, j)
                        })
                        .for_each(|tile| {
                            map[tile] = true;
                        });

                    map
                };

                Location {
                    name: name.into(),
                    shape: shape.clone(),
                    warps,
                    resource_clumps: resource_clumps.clone(),
                    bushes: bushes.clone(),
                    objects: objects.clone(),
                    items: items.clone(),
                    furniture: furniture.clone(),
                    water_tiles,
                    buildings: buildings.clone(),
                    blocked,
                    diggable,
                    action_tiles,
                    characters: characters.clone(),
                    mineshaft_details: mineshaft_details.cloned(),
                }
            },
        )?;

        graph.named_native_function(
            "new_location_delta",
            |name: &str,
             resource_clumps: &Vec<ResourceClump>,
             objects: &Vec<Object>,
             items: &Vec<FloatingItem>,
             characters: &Vec<Character>| {
                LocationDelta {
                    name: name.into(),
                    resource_clumps: resource_clumps.clone(),
                    objects: objects.clone(),
                    items: items.clone(),
                    characters: characters.clone(),
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

            fn iter_location_terrain_features(location, filter) {
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
                (0..num_features)
                    .map(|i_feat: usize| {
                        location
                            .terrainFeatures
                            .dict
                            ._entries[i_feat]
                    })
                    .filter(|feature| filter.is_none() || filter(feature))
                    .map(|feature| {
                        let tile = {
                            let right = feature.key.X;
                            let down = feature.key.Y;
                            new_vector_isize(right,down)
                        };

                        let feature_value = feature
                            .value
                            .value;

                        let tree = feature_value
                            .as::<StardewValley.TerrainFeatures.Tree>();
                        let fruit_tree = feature_value
                            .as::<StardewValley.TerrainFeatures.FruitTree>();
                        let grass = feature_value
                            .as::<StardewValley.TerrainFeatures.Grass>();
                        let hoe_dirt = feature_value
                            .as::<StardewValley.TerrainFeatures.HoeDirt>();

                        let kind = if tree.is_some() {
                            // treeType looks like an integer, but has
                            // been converted to a string.
                            let tree_type =
                                tree.treeType.value.read_string();
                            let growth_stage = tree.growthStage.value;
                            let has_seed = tree.hasSeed.value;
                            let is_stump = tree.stump.value;
                            let health = tree.health.value;
                            new_tree_kind(
                                tree_type,
                                growth_stage,
                                has_seed,
                                is_stump,
                                health,
                            )
                        } else if fruit_tree.is_some() {
                            let tree_type = fruit_tree
                                .treeId
                                .value
                                .read_string();

                            let num_fruit = fruit_tree
                                .fruit
                                .count
                                .value
                                .prim_cast::<usize>();

                            new_fruit_tree_kind(
                                tree_type,
                                num_fruit,
                            )
                        } else if grass.is_some() {
                            new_grass_kind(grass.prim_cast::<Ptr>())
                        } else if hoe_dirt.is_some() {
                            let state = hoe_dirt.state.value;
                            let is_watered = state == 1i32;

                            let crop = hoe_dirt.netCrop.value;
                            let crop_phase = if crop.is_none() {
                                None
                            } else {
                                let phase = crop.currentPhase.value.prim_cast::<usize>();
                                let num_phases = crop.phaseDays.count.value.prim_cast::<usize>();
                                let is_fully_grown = crop.fullyGrown.value;
                                let day_of_current_phase = crop.dayOfCurrentPhase.value;

                                let is_growing = phase < num_phases - 1;
                                let is_regrowing = is_fully_grown && day_of_current_phase > 0i32;
                                let can_harvest = !is_growing && !is_regrowing;

                                if phase==0 {
                                    0
                                } else if can_harvest {
                                    2
                                } else if is_regrowing {
                                    3
                                } else {
                                    1
                                }
                            };

                            let crop_seed = crop
                                .netSeedIndex
                                .value
                                .read_string();

                            new_hoe_dirt(
                                is_watered,
                                crop_phase,
                                crop_seed,
                            )
                        } else {
                            new_unknown_object_kind(feature_value)
                        };

                        new_object(tile, kind)
                    })
                    .filter(|obj| obj.is_some())
            }

            fn iter_location_objects(location, filter) {
                let num_objects = location
                    .objects
                    .compositeDict
                    ._entries
                    .len();

                (0..num_objects)
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
                            new_vector_isize(right,down)
                        };

                        let chest = obj.as::<StardewValley.Objects.Chest>();

                        let category = obj.category.value;
                        let sheet_index = obj.parentSheetIndex.value;
                        let name = obj.netName.value.read_string();
                        let id = obj._qualifiedItemId.read_string();


                        let kind = if chest.is_some() {
                            let inventory = read_inventory(chest.netItems.value);
                            let special_kind = chest.specialChestType.value;

                            let is_closed = chest.currentLidFrame == chest.startingLidFrame.value &&
                                chest.frameCounter.value == -1i32;
                            let is_opening = !is_closed;

                            new_chest_kind(
                                inventory,
                                special_kind,
                                is_opening,
                            )
                        } else if category == -999i32 {
                            new_litter_kind(name, id)
                        } else if category == -2i32 {
                            new_mineral_kind(name, id)
                        } else if category == -9i32 && sheet_index == 13i32 {
                            let ready_to_harvest = obj.readyForHarvest.value;
                            let has_held_item = obj
                                .heldObject
                                .value
                                .is_some();
                            new_furnace_kind(
                                ready_to_harvest,
                                has_held_item,
                            )
                        } else {
                            new_other_object_kind(
                                category,
                                sheet_index,
                                name,
                                id,
                            )
                        };

                        new_object(
                            tile,
                            kind,
                        )
                    })
                    .filter(|obj| obj.is_some())
            }

            fn iter_location_mineshaft_tiles(location) {
                let mineshaft = location
                    .as::<StardewValley.Locations.MineShaft>();

                let building_layer = mineshaft
                    .buildingLayers
                    ._items[0]
                    .key;

                let size = building_layer
                        .m_layerSize;

                let width =  if mineshaft.is_some() {
                    size.Width.prim_cast::<usize>()
                } else {
                    0
                };
                let height = if mineshaft.is_some() {
                    size.Height.prim_cast::<usize>()
                } else {
                    0
                };

                (0..(height*width))
                    .map(|i_flat| {
                        let i = i_flat/height;
                        let j = i_flat%height;

                        let index = building_layer
                            .m_tiles[i,j]
                            .as::<xTile.Tiles.StaticTile>()
                            .m_tileIndex;

                        let tile = new_vector_isize(i,j);
                        let kind = new_mineshaft_tile(index);
                        new_object(tile, kind)
                    })
                    .filter(|obj| obj.is_some())
            }

            fn read_location_items(location, filter) {
                let debris_list = location
                    .debris
                    .list;
                let num_debris = debris_list
                    ._size
                    .prim_cast::<usize>();

                (0..num_debris)
                    .map(|i| debris_list._items[i])
                    .filter(|debris| {
                        let ty = debris.debrisType.value;
                        ty == 4i32
                    })
                    .filter(|debris| {
                        debris.chunks.array.elements._size == 1i32
                    })
                    .filter(|debris| filter.is_none() || {
                        let chunk = debris.chunks.array.elements._items[0].value;
                        let pos = chunk.position.Field.value;
                        filter(pos.X, pos.Y)
                    })
                    .map(|debris| {
                        let chunk = debris.chunks.array.elements._items[0].value;
                        let pos = chunk.position.Field.value;
                        let item_id = debris.itemId.value.read_string();
                        let item_quality = debris.netItemQuality.value;
                        // Use the final resting Y position of the
                        // chunk, not the current Y position.  The
                        // current Y position will be closer to the
                        // top of the screen to simulate a Z axis.
                        let debris_Y = debris
                            .netChunkFinalYLevel
                            .value
                            .prim_cast::<f32>();

                        let item = debris.netItem.value;
                        let count = if item.is_some() {
                            item.stack.value
                        } else {
                            1i32
                        };

                        new_floating_item(
                            pos.X,
                            debris_Y,
                            item_id,
                            item_quality,
                            count,
                        )
                    })
                    .filter(|floating_item| floating_item.is_some())
                    .collect()
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

            fn read_location_characters(location) {
                let character_list = location
                    .characters
                    .list;

                let num_characters = character_list
                    ._size
                    .prim_cast::<usize>();

                let characters = (0..num_characters)
                    .map(|i_character| character_list._items[i_character])
                    .map(|character| {
                        let name_ptr = if character._displayName.is_some() {
                            character._displayName
                        } else {
                            character.name.value
                        };
                        let name = name_ptr.read_string();

                        let pos = character
                            .position
                            .Field
                            .value;

                        let sprite_width = character
                            .sprite
                            .value
                            .spriteWidth
                            .value;

                        let health = character
                            .as::<StardewValley.Monsters.Monster>()
                            .health
                            .value;

                        let is_invisible_duggy = character.isInvisible.value;

                        let as_rock_crab = character
                            .as::<StardewValley.Monsters.RockCrab>();
                        let is_waiting_rock_crab = if as_rock_crab.is_some() {
                            as_rock_crab.waiter
                        } else {
                            false
                        };

                        new_character(
                            name,
                            pos.X, pos.Y,
                            sprite_width,
                            health,
                            is_invisible_duggy,
                            is_waiting_rock_crab,
                        )
                    })
                    .collect();

                characters
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
                let shape = new_vector_isize(width, height);

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
                        let location = new_vector_isize(
                            warp.x.value,
                            warp.y.value,
                        );
                        let target = new_vector_isize(
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
                            new_vector_isize(right,down)
                        };
                        new_bush(kind, top_left)
                    })
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
                                new_vector_isize(right,down)
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
                    let loc = new_vector_isize(i,j);

                    let base_tile = layer.m_tiles[i,j];
                    let static_tile = base_tile
                        .as::<xTile.Tiles.StaticTile>();
                    let animated_tile = base_tile
                        .as::<xTile.Tiles.AnimatedTile>();

                    let tile = if static_tile.is_some() {
                        static_tile
                    } else if animated_tile.is_some() {
                        animated_tile.m_tileFrames[0]
                    } else {
                        None
                    };

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

                let resource_clumps = read_location_resource_clumps(
                    location, None);
                let furniture = read_location_furniture(location, None);

                let iter_objects = iter_location_objects(location,None);
                let iter_features = iter_location_terrain_features(
                    location,None
                );
                let iter_mineshaft_tiles = iter_location_mineshaft_tiles(location);

                let objects = iter_objects
                    .chain(iter_features)
                    .chain(iter_mineshaft_tiles)
                    .collect();

                let items = read_location_items(location, None);

                let characters = read_location_characters(location);

                let mineshaft = location
                    .as::<StardewValley.Locations.MineShaft>();
                let mineshaft_details = if mineshaft.is_some() {
                    let mineshaft_level = mineshaft
                        .netMineLevel
                        .value;
                    let generated_ladder = mineshaft
                        .ladderHasSpawned;

                    new_mineshaft_details(
                        mineshaft_level,
                        generated_ladder,
                    )
                } else {
                    None
                };



                new_location(
                    name,
                    shape,
                    warps,
                    resource_clumps,
                    bushes,
                    objects,
                    items,
                    furniture,
                    flattened_water_tiles,
                    tile_sheets,
                    buildings,
                    characters,
                    mineshaft_details,
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
                    let x = x.prim_cast::<f32>();
                    let y = y.prim_cast::<f32>();
                    let diff_x = player_x - x;
                    let diff_y = player_y - y;
                    let dist2 = diff_x*diff_x + diff_y*diff_y;

                    dist2.is_some() && dist2 < 49
                }

                let resource_clumps = read_location_resource_clumps(
                    location,
                    |clump| {
                        let pos = clump.netTile.value;
                        is_close_to_player(pos.X, pos.Y)
                    }
                );
                let iter_objects = iter_location_objects(
                    location,
                    |obj| {
                        let pos = obj.tileLocation.value;
                        is_close_to_player(pos.X, pos.Y)
                    }
                );
                let iter_features = iter_location_terrain_features(
                    location,
                    |feature| {
                        let pos = feature.key;
                        is_close_to_player(pos.X, pos.Y)
                    }
                );
                let iter_mineshaft_tiles = iter_location_mineshaft_tiles(
                    location
                );
                let objects = iter_objects
                    .chain(iter_features)
                    .chain(iter_mineshaft_tiles)
                    .collect();

                let items = read_location_items(location, None);

                let characters = read_location_characters(location);

                new_location_delta(
                    name,
                    resource_clumps,
                    objects,
                    items,
                    characters,
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
                            requires_friendship: None,
                        }
                    })
                })
                .for_each(|warp| location.warps.push(warp));
        }
    }

    pub fn bounds(&self) -> Rectangle<isize> {
        Rectangle {
            top_left: Vector::zero(),
            shape: self.shape,
        }
    }

    pub fn is_water(&self, tile: Vector<isize>) -> bool {
        self.water_tiles.is_set(tile)
    }

    pub fn iter_water_tiles(&self) -> impl Iterator<Item = Vector<isize>> + '_ {
        self.water_tiles
            .iter()
            .filter(|(_, is_water)| **is_water)
            .map(|(tile, _)| tile)
    }

    pub fn iter_bush_tiles(&self) -> impl Iterator<Item = Vector<isize>> + '_ {
        self.bushes
            .iter()
            .flat_map(|bush| bush.rectangle().iter_points())
    }

    pub fn iter_building_tiles(
        &self,
    ) -> impl Iterator<Item = Vector<isize>> + '_ {
        self.buildings
            .iter()
            .flat_map(|building| building.iter_tiles())
    }

    pub fn collect_clear_tiles(&self) -> TileMap<bool> {
        let mut map = self.blocked.map(|b| !b);

        let iter_water = self.iter_water_tiles();

        let iter_clumps = self
            .resource_clumps
            .iter()
            .flat_map(|clump| clump.shape.iter_points());

        let iter_bush = self.iter_bush_tiles();

        let iter_objects = self
            .objects
            .iter()
            .filter(|obj| !obj.kind.is_walkable())
            .map(|obj| obj.tile);

        let iter_furniture = self
            .furniture
            .iter()
            .filter(|piece| !matches!(piece.kind, FurnitureKind::Rug))
            .flat_map(|piece| piece.shape.iter_points());

        let iter_buildings = self.iter_building_tiles();

        std::iter::empty()
            .chain(iter_water)
            .chain(iter_clumps)
            .chain(iter_bush)
            .chain(iter_objects)
            .chain(iter_furniture)
            .chain(iter_buildings)
            .for_each(|tile| {
                map[tile] = false;
            });

        map
    }

    /// Iterator that yields tiles that have a right-click action
    ///
    /// Used when the mouse should be positioned to avoid being
    /// overtop any of these tiles.  Even if using the 'x' key,
    /// activating a tile will take precedence over consuming an item.
    pub fn iter_activatable_tiles(
        &self,
    ) -> impl Iterator<Item = Vector<isize>> + '_ {
        let iter_doors = self
            .warps
            .iter()
            .filter(|warp| {
                matches!(
                    warp.kind,
                    WarpKind::Door | WarpKind::LockedDoor { .. }
                )
            })
            .map(|warp| warp.location);

        let iter_bushes = self.iter_bush_tiles();

        let iter_tree = self
            .objects
            .iter()
            .filter(|obj| {
                matches!(
                    obj.kind,
                    ObjectKind::Tree(_) | ObjectKind::FruitTree(_)
                )
            })
            .map(|obj| obj.tile);

        let iter_resource_clump = self
            .resource_clumps
            .iter()
            .flat_map(|clump| clump.shape.iter_points());

        let iter_furniture = self
            .furniture
            .iter()
            .filter(|piece| !matches!(piece.kind, FurnitureKind::Rug))
            .flat_map(|piece| piece.shape.iter_points());

        let iter_actions = self.action_tiles.iter().map(|(tile, _)| *tile);

        std::iter::empty()
            .chain(iter_doors)
            .chain(iter_bushes)
            .chain(iter_tree)
            .chain(iter_actions)
            .chain(iter_resource_clump)
            .chain(iter_furniture)
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

        // Can't de-duplicate the items by their position, because
        // more than one item may be on a given tile.  So for now,
        // just reading all of them.
        self.items = delta.items;

        self.characters = delta.characters;
    }
}

impl ObjectKind {
    pub fn is_walkable(&self) -> bool {
        match self {
            ObjectKind::Stone(_)
            | ObjectKind::Mineral(_)
            | ObjectKind::Wood
            | ObjectKind::Fiber
            | ObjectKind::PotOfGold
            | ObjectKind::Chest(_) => false,
            ObjectKind::Grass => true,
            ObjectKind::FruitTree(_) => false,
            ObjectKind::Tree(tree) => tree.growth_stage == 0,
            ObjectKind::HoeDirt(_) => {
                // TODO: Check for crops that use a trellis, which
                // cannot be walked over.
                true
            }
            ObjectKind::Torch => true,
            ObjectKind::ArtifactSpot => true,
            ObjectKind::SeedSpot => true,

            ObjectKind::Sprinkler(_)
            | ObjectKind::Scarecrow
            | ObjectKind::MineBarrel
            | ObjectKind::Furnace(_)
            | ObjectKind::MineLadderUp
            | ObjectKind::MineLadderDown
            | ObjectKind::MineHoleDown
            | ObjectKind::MineElevator
            | ObjectKind::MineCartCoal => false,
            ObjectKind::Other { .. } => false,
            ObjectKind::Unknown => false,
        }
    }

    pub fn as_hoe_dirt(&self) -> Option<&HoeDirt> {
        match self {
            ObjectKind::HoeDirt(hoe_dirt) => Some(hoe_dirt),
            _ => None,
        }
    }

    pub fn as_chest(&self) -> Option<&Chest> {
        match self {
            ObjectKind::Chest(chest) => Some(chest),
            _ => None,
        }
    }

    pub fn as_furnace(&self) -> Option<&Furnace> {
        match self {
            ObjectKind::Furnace(furnace) => Some(furnace),
            _ => None,
        }
    }

    pub fn as_stone(&self) -> Option<&StoneKind> {
        match self {
            ObjectKind::Stone(stone) => Some(stone),
            _ => None,
        }
    }

    pub fn is_forage(&self) -> bool {
        let ObjectKind::Other { category, name, .. } = self else {
            return false;
        };

        matches!(
            (category, name.as_str()),
            (
                ItemCategory::Fruit
                    | ItemCategory::Greens
                    | ItemCategory::Flowers
                    | ItemCategory::Vegetable
                    | ItemCategory::Fish
                    | ItemCategory::SeaProduce,
                _
            ) | (ItemCategory::Other(0), "Seaweed")
        )
    }
}

impl HoeDirt {
    pub fn can_harvest(&self) -> bool {
        matches!(
            self.crop,
            Some(Crop {
                phase: CropPhase::Harvestable,
                ..
            })
        )
    }

    pub fn requires_watering(&self) -> bool {
        !self.is_watered
            && self
                .crop
                .as_ref()
                .map(|crop| match crop.phase {
                    CropPhase::Seed => true,
                    CropPhase::Growing => true,
                    CropPhase::Harvestable => false,
                    CropPhase::Regrowing => true,
                })
                .unwrap_or(false)
    }

    pub fn has_crop(&self) -> bool {
        self.crop.is_some()
    }

    pub fn is_empty(&self) -> bool {
        self.crop.is_none()
    }
}

impl Sprinkler {
    pub fn id(&self) -> ItemId {
        let id = match self {
            Sprinkler::Regular => "(O)599",
            Sprinkler::Quality => "(O)621",
            Sprinkler::Iridium => "(O)645",
        };
        ItemId::new(id)
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

impl Furniture {
    pub fn is_walkable(&self) -> bool {
        match &self.kind {
            FurnitureKind::Rug => true,
            _ => false,
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

impl std::str::FromStr for FruitTreeKind {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "628" => Ok(Self::Cherry),
            "629" => Ok(Self::Apricot),
            "630" => Ok(Self::Orange),
            "631" => Ok(Self::Peach),
            "632" => Ok(Self::Pomegranate),
            "633" => Ok(Self::Apple),
            "FlashShifter.StardewValleyExpandedCP_Pear_Sapling" => {
                Ok(Self::Pear)
            }
            "FlashShifter.StardewValleyExpandedCP_Nectarine_Sapling" => {
                Ok(Self::Nectarine)
            }
            "FlashShifter.StardewValleyExpandedCP_Persimmon_Sapling" => {
                Ok(Self::Persimmon)
            }
            "FlashShifter.StardewValleyExpandedCP_Tree_Coin" => Ok(Self::Money),
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

    fn iter_tile_actions(
        &self,
    ) -> impl Iterator<Item = (Vector<isize>, &str)> + '_ {
        self.tiles.iter().flat_map(|tile| {
            tile.properties
                .iter()
                .filter(|(key, _)| {
                    matches!(key.as_str(), "Action" | "TouchAction")
                })
                .filter_map(|(_, opt_value)| opt_value.as_ref())
                .map(|value| (tile.location, value.as_str()))
        })
    }

    fn collect_action_tiles(&self) -> Vec<(Vector<isize>, String)> {
        self.iter_tile_actions()
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
                    "WarpCommunityCenter",
                    "Warp_Sunroom_Door",
                ]
                .into_iter()
                .all(|special| !action.starts_with(special))
            })
            .map(|(tile, action)| (tile, action.to_string()))
            .collect()
    }

    fn iter_extra_warps(&self) -> impl Iterator<Item = Warp> + '_ {
        self.iter_tile_actions().filter_map(|(tile, action)| {
            let (room, x, y) = match action {
                "WarpCommunityCenter" => Some(("CommunityCenter", 32, 23)),
                "Warp_Sunroom_Door" => Some(("Sunroom", 5, 13)),
                other if other.starts_with("LoadMap") => {
                    let mut iter_words = other.split(' ');
                    iter_words.next();
                    let target_room = iter_words.next()?;
                    let right: isize = iter_words
                        .next()
                        .and_then(|right| right.parse().ok())?;
                    let down: isize = iter_words
                        .next()
                        .and_then(|right| right.parse().ok())?;
                    Some((target_room, right, down))
                }
                _ => None,
            }?;

            Some(Warp {
                location: tile,
                target: Vector::new(x, y),
                target_room: room.to_string(),
                kind: WarpKind::Door,
                requires_friendship: None,
            })
        })
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

impl std::fmt::Display for FruitTreeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Cherry => write!(f, "Cherry"),
            Self::Apricot => write!(f, "Apricot"),
            Self::Orange => write!(f, "Orange"),
            Self::Peach => write!(f, "Peach"),
            Self::Pomegranate => write!(f, "Pomegranate"),
            Self::Apple => write!(f, "Apple"),
            Self::Pear => write!(f, "Pear"),
            Self::Nectarine => write!(f, "Nectarine"),
            Self::Persimmon => write!(f, "Persimmon"),
            Self::Money => write!(f, "Money"),
        }
    }
}

impl std::fmt::Display for ObjectKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Stone(stone) => write!(f, "{stone}"),
            Self::Mineral(mineral) => write!(f, "{mineral}"),
            Self::Wood => write!(f, "Wood"),
            Self::Fiber => write!(f, "Fiber"),
            Self::PotOfGold => write!(f, "PotOfGold"),
            Self::Chest(chest) => {
                write!(
                    f,
                    "Chest with {} slots, {} full",
                    chest.inventory.items.len(),
                    chest
                        .inventory
                        .items
                        .iter()
                        .filter(|item| item.is_some())
                        .count()
                )
            }
            Self::Grass => write!(f, "Grass"),
            Self::Tree(tree) => write!(f, "{}", tree.kind),
            Self::FruitTree(tree) => write!(f, "{}", tree.kind),
            Self::HoeDirt(_) => write!(f, "HoeDirt"),
            Self::ArtifactSpot => write!(f, "ArtifactSpot"),
            Self::SeedSpot => write!(f, "SeedSpot"),
            Self::MineLadderUp => write!(f, "MineLadderUp"),
            Self::MineLadderDown => write!(f, "MineLadderDown"),
            Self::MineHoleDown => write!(f, "MineHoleDown"),
            Self::MineElevator => write!(f, "MineElevator"),
            Self::MineCartCoal => write!(f, "MineCartCoal"),
            Self::MineBarrel => write!(f, "MineBarrel"),
            Self::Furnace(furnace) => write!(f, "{furnace}"),
            Self::Torch => write!(f, "Torch"),
            Self::Scarecrow => write!(f, "Scarecrow"),
            Self::Sprinkler(sprinkler) => write!(f, "{sprinkler}"),

            Self::Other { category, name, id } => {
                write!(f, "Other({category}, '{name}', '{id}')")
            }
            Self::Unknown => write!(f, "Unknown"),
        }
    }
}

impl std::fmt::Display for StoneKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StoneKind::Normal => write!(f, "Stone"),
            StoneKind::DoubleStone => write!(f, "DoubleStone"),
            StoneKind::Copper => write!(f, "CopperOre"),
            StoneKind::Iron => write!(f, "IronOre"),
            StoneKind::Gold => write!(f, "GoldOre"),
            StoneKind::Iridium => write!(f, "IridiumOre"),
            StoneKind::Gem => write!(f, "Gem"),
            StoneKind::Mystic => write!(f, "Mystic"),

            StoneKind::Diamond => write!(f, "Diamond"),
            StoneKind::Ruby => write!(f, "Ruby"),
            StoneKind::Jade => write!(f, "Jade"),
            StoneKind::Amethyst => write!(f, "Amethyst"),
            StoneKind::Topaz => write!(f, "Topas"),
            StoneKind::Emerald => write!(f, "Emerald"),
            StoneKind::Aquamarine => write!(f, "Aquamarine"),

            StoneKind::Other { name, id } => {
                write!(f, "StoneKind::Other('{name}', '{id}')")
            }
        }
    }
}

impl std::fmt::Display for MineralKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MineralKind::Quartz => write!(f, "Quartz"),
            MineralKind::EarthCrystal => write!(f, "EarthCrystal"),
            MineralKind::FrozenTear => write!(f, "FrozenTear"),
            MineralKind::FireQuartz => write!(f, "FireQuartz"),
            MineralKind::Other { name, id } => {
                write!(f, "MineralKind::Other('{name}', '{id}')")
            }
        }
    }
}

impl std::fmt::Display for Furnace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.ready_to_harvest {
            write!(f, "Furnace(harvestable)")
        } else if self.has_held_item {
            write!(f, "Furnace(running)")
        } else {
            write!(f, "Furnace(empty)")
        }
    }
}

impl std::fmt::Display for Sprinkler {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Sprinkler::Regular => write!(f, "RegularSprinkler"),
            Sprinkler::Quality => write!(f, "QualitySprinkler"),
            Sprinkler::Iridium => write!(f, "IridiumSprinkler"),
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

impl FloatingItem {
    /// The location of the item, in tile coordinates
    pub fn tile_pos(&self) -> Vector<f32> {
        self.position / 64.0
    }
}

impl Deref for Chest {
    type Target = Inventory;

    fn deref(&self) -> &Self::Target {
        &self.inventory
    }
}

impl AsRef<ItemId> for FloatingItem {
    fn as_ref(&self) -> &ItemId {
        &self.id
    }
}

impl Character {
    fn center_pixel(&self) -> Vector<isize> {
        let offset = Vector::<isize>::new(8, 16);

        let width = self.sprite_width as isize;
        let width = width * 4 * 3 / 4;
        let bounding_box = Vector::<isize>::new(width, 32);

        let position = self.position.map(|x| x as isize);
        position + offset + bounding_box / 2
    }

    /// The tile that contains the center of the player's bounding
    /// box.  This is the tile used to determine tool reach (may only
    /// target tiles at or adjacent to the player's tile).
    pub fn tile(&self) -> Vector<isize> {
        self.center_pixel().map(|x| x / 64)
    }

    /// The center of the player's bounding box, adjusted to be
    /// directly comparable to tile coordinates.
    pub fn center_pos(&self) -> Vector<f32> {
        self.center_pixel().map(|x| {
            let x = x - 32;
            let x = x as f32;
            x / 64.0
        })
    }
}
