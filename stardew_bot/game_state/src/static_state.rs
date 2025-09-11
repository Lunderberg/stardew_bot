use std::collections::HashMap;

use dsl::{RustNativeObject, SymbolicGraph, SymbolicValue};
use geometry::Vector;
use itertools::Itertools as _;

use crate::Error;

use super::{Item, ItemCategory, ItemId, Quality};

#[derive(RustNativeObject, Debug, Clone)]
pub struct StaticState {
    pub geode: GeodeData,
    pub frozen_geode: GeodeData,
    pub magma_geode: GeodeData,
    pub omni_geode: GeodeData,

    /// A lookup from item id to data about that item.  Used in cases
    /// where only an ItemId is present, and not a full item.
    /// (e.g. Item drop tables)
    pub object_data: HashMap<ItemId, ObjectData>,

    /// A lookup from the ItemId of a seed to properties about its use
    /// as a growable crop.
    pub crop_data: HashMap<ItemId, CropData>,

    /// Unchanging definition of the community center bundles.
    /// Completion of bundles is tracked in GlobalState, and is
    /// updated as bundles are completed.
    pub bundles: Vec<Bundle>,
}

#[derive(RustNativeObject, Debug, Clone)]
pub struct Bundle {
    pub community_center_room: String,
    pub bundle_index: i32,
    pub name: String,
    pub reward: Option<Item>,
    pub ingredients: Vec<BundleIngredient>,
    pub num_required: usize,
}

#[derive(Debug, Clone)]
pub enum BundleIngredient {
    Gold(usize),
    Item(Item),
}

#[derive(Debug, Clone)]
pub struct ObjectData {
    pub name: String,
    pub price: i32,
    pub edibility: i32,
    pub category: ItemCategory,
}

#[derive(RustNativeObject, Debug, Clone)]
struct RawObjectData {
    id: String,
    name: String,
    geode: GeodeData,

    price: i32,
    edibility: i32,
    category: ItemCategory,
}

#[derive(Debug, Clone)]
pub struct CropData {
    pub harvest_item: ItemId,
    pub seasons: Vec<Season>,
    pub days_to_grow: u32,
    pub regrow_days: Option<u32>,
    pub uses_trellis: bool,
    pub xp_per_harvest: i32,
    pub harvest_with_scythe: bool,
}

#[derive(RustNativeObject, Debug, Clone, Copy)]
pub enum Season {
    Spring,
    Summer,
    Fall,
    Winter,
}

#[derive(RustNativeObject, Debug, Clone)]
struct RawCropData {
    seed_item: ItemId,
    harvest_item: ItemId,
    seasons: Vec<Season>,
    days_to_grow: u32,
    regrow_days: Option<u32>,
    uses_trellis: bool,
    harvest_with_scythe: bool,
}

#[derive(RustNativeObject, Debug, Clone)]
pub struct GeodeData {
    /// If true, there's a 50% chance that the drop will be determined
    /// from `GeodeData.drops`, and a 50% chance that the drop will be
    /// determined from the hard-coded defaults.  If false, the drop
    /// is always determined from `GeodeData.drops`.
    ///
    /// Regardless of this value, falls back to the default drops if
    /// `GeodeData.drops` fails to produce an item.
    pub drops_default_items: bool,

    /// The custom drops for this geode type.
    pub drops: Vec<GeodeDrop>,
}

#[derive(RustNativeObject, Debug, Clone)]
pub struct GeodeDrop {
    /// If there is more than one list of available items, the
    /// `GeodeDrop` instances are checked in order of increasing
    /// precedence.
    pub precedence: i32,

    /// A condition that must be met for the `GeodeDrop` to apply.
    pub condition: Option<String>,

    /// The chance of producing an item from this list, assuming the
    /// condition is met.
    pub chance: f64,

    /// The list of items that may be produced.
    pub item_list: Vec<ItemId>,
}

impl StaticState {
    pub(crate) fn def_read_static_state(
        graph: &mut SymbolicGraph,
    ) -> Result<SymbolicValue, Error> {
        graph.named_native_function(
            "new_geode_drop",
            |precedence: i32,
             condition: Option<&String>,
             chance: f64,
             item_list: &Vec<String>|
             -> GeodeDrop {
                GeodeDrop {
                    precedence,
                    condition: condition.cloned(),
                    chance,
                    item_list: item_list
                        .iter()
                        .map(|id| ItemId::new(id.to_string()))
                        .collect(),
                }
            },
        )?;

        graph.named_native_function(
            "new_object_data",
            |id: &str,
             name: &str,
             price: i32,
             edibility: i32,
             category: i32,
             geode_drops_default_items: bool,
             geode_drops: &Vec<GeodeDrop>|
             -> RawObjectData {
                let geode_data = GeodeData {
                    drops_default_items: geode_drops_default_items,
                    drops: geode_drops.clone(),
                };

                RawObjectData {
                    id: id.to_string(),
                    name: name.to_string(),
                    geode: geode_data,
                    price,
                    edibility,
                    category: category.into(),
                }
            },
        )?;

        graph.named_native_function("new_season", |value: i32| -> Season {
            match value {
                0 => Season::Spring,
                1 => Season::Summer,
                2 => Season::Fall,
                3 => Season::Winter,
                _ => todo!("Handle invalid season enum: '{value}'"),
            }
        })?;

        graph.named_native_function(
            "new_crop_data",
            |seed_item: &str,
             harvest_item: &str,
             seasons: &Vec<Season>,
             days_to_grow: i32,
             regrow_days: i32,
             uses_trellis: bool,
             harvest_with_scythe: bool| {
                let seed_item = ItemId::new(format!("(O){seed_item}"));
                let harvest_item = ItemId::new(format!("(O){harvest_item}"));

                let regrow_days = if regrow_days > 0 {
                    Some(regrow_days as u32)
                } else {
                    None
                };

                RawCropData {
                    seed_item,
                    harvest_item,
                    seasons: seasons.clone(),
                    days_to_grow: days_to_grow as u32,
                    regrow_days,
                    uses_trellis,
                    harvest_with_scythe,
                }
            },
        )?;

        graph.named_native_function(
            "new_bundle",
            |key: &str, value: &str| -> Bundle {
                let mut iter_key = key.split('/');
                let community_center_room =
                    iter_key.next().unwrap().to_string();
                let bundle_index = iter_key.next().unwrap().parse().unwrap();

                let mut iter_value = value.split('/');
                let name = iter_value.next().unwrap().to_string();
                let reward = iter_value
                    .next()
                    .unwrap()
                    .split(' ')
                    .tuples()
                    .map(|(category, id_str, count)| {
                        let category = match category {
                            "BO" => "BC",
                            other => other,
                        };
                        let count = count.parse().unwrap();

                        ItemId::new(format!("({category}){id_str}"))
                            .with_count(count)
                    })
                    .at_most_one()
                    .unwrap();

                let ingredients: Vec<_> = iter_value
                    .next()
                    .unwrap()
                    .split(' ')
                    .tuples()
                    .map(|(id, count, quality)| {
                        if id == "-1" {
                            let gold = count.parse().unwrap();
                            BundleIngredient::Gold(gold)
                        } else {
                            let count = count.parse().unwrap();
                            let quality: i32 = quality.parse().unwrap();
                            let quality: Quality = quality.try_into().unwrap();
                            let item = ItemId::new(format!("(O){id}"))
                                .with_quality(quality)
                                .with_count(count);
                            BundleIngredient::Item(item)
                        }
                    })
                    .collect();

                // An integer specifying the color of the bundle.
                iter_value.next().unwrap();

                let num_required = match iter_value.next().unwrap() {
                    "" => ingredients.len(),
                    other => other.parse().unwrap(),
                };

                Bundle {
                    community_center_room,
                    bundle_index,
                    name,
                    reward,
                    ingredients,
                    num_required,
                }
            },
        )?;

        graph.named_native_function(
            "new_static_state",
            |objects: &Vec<RawObjectData>,
             crops: &Vec<RawCropData>,
             bundles: &Vec<Bundle>| {
                let get_geode_data = |id: &str| -> GeodeData {
                    let data = objects
                        .iter()
                        .find(|data| data.id == id)
                        .unwrap_or_else(|| {
                            panic!("Could not find item data for {id}")
                        });
                    data.geode.clone()
                };
                let geode = get_geode_data("535");
                let frozen_geode = get_geode_data("536");
                let magma_geode = get_geode_data("537");
                let omni_geode = get_geode_data("749");

                let object_data: HashMap<_, _> = objects
                    .iter()
                    .map(|data| {
                        let id = if matches!(
                            data.category,
                            ItemCategory::BigCraftable
                        ) {
                            format!("(BC){}", data.id)
                        } else {
                            format!("(O){}", data.id)
                        };
                        (
                            ItemId::new(id),
                            ObjectData {
                                name: data.name.clone(),
                                price: data.price,
                                edibility: data.edibility,
                                category: data.category,
                            },
                        )
                    })
                    .collect();

                let crop_data: HashMap<_, _> = crops
                    .iter()
                    .map(|crop| {
                        let harvest_item = crop.harvest_item.clone();
                        let price = object_data
                            .get(&harvest_item)
                            .map(|item| item.price as f32)
                            .unwrap_or_else(|| {
                                panic!(
                                    "Missing price data \
                                     for crop '{harvest_item}'"
                                )
                            });
                        let xp_per_harvest =
                            (16.0 * (0.018 * price + 1.0).ln()).round() as i32;

                        let crop_data = CropData {
                            harvest_item,
                            seasons: crop.seasons.clone(),
                            days_to_grow: crop.days_to_grow,
                            regrow_days: crop.regrow_days,
                            uses_trellis: crop.uses_trellis,
                            xp_per_harvest,
                            harvest_with_scythe: crop.harvest_with_scythe,
                        };
                        (crop.seed_item.clone(), crop_data)
                    })
                    .collect();

                StaticState {
                    geode,
                    frozen_geode,
                    magma_geode,
                    omni_geode,
                    object_data,
                    crop_data,
                    bundles: bundles.clone(),
                }
            },
        )?;

        let func = graph.parse(stringify! {
            fn read_static_state() {
                let object_data = StardewValley.Game1
                    .objectData
                    .as::<"System.Collections.Generic.Dictionary`2"<
                        System.String,
                        StardewValley.GameData.Objects.ObjectData,
                    >>();

                let num_objects = object_data._count.prim_cast::<usize>();
                let objects = (0..num_objects)
                    .map(|i| object_data._entries[i])
                    .map(|entry| {
                        let id = entry.key.read_string();

                        let data = entry.value;
                        let name = data.Name.read_string();
                        let price = data.Price;
                        let edibility = data.Edibility;
                        let category = data.Category;

                        let geode_drops_default_items = data
                            .GeodeDropsDefaultItems;

                        let geode_drops = {
                            let num = data
                                .GeodeDrops
                                ._size
                                .prim_cast::<usize>();
                            (0..num)
                                .map(|i| data.GeodeDrops._items[i])
                                .map(|drops| {
                                    let precedence = drops
                                        .field("<Precedence>k__BackingField");
                                    let chance = drops
                                        .field("<Chance>k__BackingField");
                                    let condition = drops
                                        .field("<Condition>k__BackingField")
                                        .read_string();

                                    let raw_item = drops
                                        .field("<ItemId>k__BackingField");

                                    let raw_item_list = drops
                                        .field("<RandomItemId>k__BackingField");

                                    let item_list = if raw_item.is_some() {
                                        (0..1)
                                            .map(|i| raw_item.read_string())
                                            .collect()
                                    } else {
                                        let num_items = raw_item_list
                                            ._size
                                            .prim_cast::<usize>();
                                        (0..num_items)
                                            .map(|i| raw_item_list._items[i])
                                            .map(|item| item.read_string())
                                            .collect()
                                    };

                                    new_geode_drop(
                                        precedence,
                                        condition,
                                        chance,
                                        item_list,
                                    )
                                })
                                .collect()
                        };

                        new_object_data(
                            id,
                            name,
                            price,
                            edibility,
                            category,
                            geode_drops_default_items,
                            geode_drops,
                        )
                    })
                    .collect();

                let crops = {
                    let dict = StardewValley.Game1
                        .cropData
                        .as::<"System.Collections.Generic.Dictionary`2"
                          <
                            System.String,
                            StardewValley.GameData.Crops.CropData,
                          >
                        >();

                    let num_entries = dict
                        ._count
                        .prim_cast::<usize>();

                    (0..num_entries)
                        .map(|i| dict._entries[i])
                        .map(|entry| {
                            let seed_item = entry.key.read_string();
                            let harvest_item = entry.value.HarvestItemId.read_string();

                            let seasons = {
                                let list = entry.value.Seasons;
                                let num_seasons = list
                                    ._size
                                    .prim_cast::<usize>();
                                (0..num_seasons)
                                    .map(|i| list._items[i])
                                    .map(|season| new_season(season.value__))
                                    .collect()
                            };

                            let days_to_grow = {
                                let phases = entry.value.DaysInPhase;
                                let num_phases = phases
                                    ._size
                                    .prim_cast::<usize>();
                                (0..num_phases)
                                    .map(|i| phases._items[i])
                                    .reduce(0i32, |a,b| a+b)
                            };

                            let uses_trellis = entry.value.IsRaised;

                            let harvest_with_scythe =
                                entry.value.HarvestMethod == 1i32;

                            let regrow_days = entry.value.RegrowDays;

                            new_crop_data(
                                seed_item,
                                harvest_item,
                                seasons,
                                days_to_grow,
                                regrow_days,
                                uses_trellis,
                                harvest_with_scythe,
                            )
                        })
                        .filter(|opt| opt.is_some())
                        .collect()
                };

                let bundles = {
                    let bundle_dict = StardewValley.Game1
                        .netWorldState
                        .value
                        .netBundleData
                        .dict;
                    let num_bundles = bundle_dict
                        ._count
                        .prim_cast::<usize>();
                    (0..num_bundles)
                        .map(|i| bundle_dict._entries[i])
                        .map(|entry| {
                            let key = entry.key.read_string();
                            let value = entry.value.value.read_string();
                            new_bundle(key, value)
                        })
                        .collect()
                };

                new_static_state(
                    objects,
                    crops,
                    bundles,
                )
            }
        })?;

        Ok(func)
    }

    pub fn enrich_item(&self, item: Item) -> Item {
        if let Some(data) = self.object_data.get(&item.id) {
            Item {
                price: data.price,
                edibility: data.edibility,
                category: Some(data.category),
                ..item
            }
        } else {
            item
        }
    }

    pub fn get_crop(
        &self,
        seed: impl AsRef<ItemId>,
    ) -> Result<&CropData, Error> {
        let seed = seed.as_ref();
        self.crop_data
            .get(seed)
            .ok_or_else(|| Error::UnknownSeedKind(seed.clone()))
    }
}

impl Bundle {
    pub fn iter_items(&self) -> impl Iterator<Item = &Item> + '_ {
        self.ingredients
            .iter()
            .filter_map(|ingredient| match ingredient {
                BundleIngredient::Item(item) => Some(item),
                BundleIngredient::Gold(_) => None,
            })
    }

    pub fn num_bundles_to_unlock(&self) -> usize {
        match self.community_center_room.as_str() {
            "Crafts Room" => 0,
            "Pantry" | "Fish Tank" => 1,
            "Boiler Room" => 2,
            "Bulletin Board" => 3,
            "Vault" => 4,
            _ => 99,
        }
    }

    pub fn community_center_tile(&self) -> Option<Vector<isize>> {
        match self.community_center_room.as_str() {
            "Crafts Room" => Some(Vector::new(14, 23)),
            "Pantry" => Some(Vector::new(14, 5)),
            "Fish Tank" => Some(Vector::new(40, 10)),
            "Bulletin Board" => Some(Vector::new(46, 11)),
            "Boiler Room" => Some(Vector::new(63, 14)),
            "Vault" => Some(Vector::new(55, 6)),
            _ => None,
        }
    }
}
