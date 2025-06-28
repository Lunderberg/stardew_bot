use dotnet_debugger::{RustNativeObject, SymbolicGraph, SymbolicValue};

use crate::Error;

use super::ItemId;

#[derive(RustNativeObject, Debug, Clone)]
pub struct StaticState {
    pub geode: GeodeData,
    pub frozen_geode: GeodeData,
    pub magma_geode: GeodeData,
    pub omni_geode: GeodeData,
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

    /// The chance of producing an item from this list.
    pub chance: f64,

    /// The list of items that may be produced.
    pub item_list: Vec<ItemId>,
}

#[derive(RustNativeObject, Debug, Clone)]
struct ObjectData {
    id: String,
    geode: GeodeData,
}

impl StaticState {
    pub(crate) fn def_read_static_state(
        graph: &mut SymbolicGraph,
    ) -> Result<SymbolicValue, Error> {
        graph.named_native_function(
            "new_geode_drop",
            |precedence: i32,
             chance: f64,
             item_list: &Vec<String>|
             -> GeodeDrop {
                GeodeDrop {
                    precedence,
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
             geode_drops_default_items: bool,
             geode_drops: &Vec<GeodeDrop>|
             -> ObjectData {
                let geode_data = GeodeData {
                    drops_default_items: geode_drops_default_items,
                    drops: geode_drops.clone(),
                };
                ObjectData {
                    id: id.to_string(),
                    geode: geode_data,
                }
            },
        )?;

        graph.named_native_function(
            "new_static_state",
            |objects: &Vec<ObjectData>| {
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
                StaticState {
                    geode,
                    frozen_geode,
                    magma_geode,
                    omni_geode,
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

                                    let raw_item_list = drops
                                        .field("<RandomItemId>k__BackingField");
                                    let num_items = raw_item_list
                                        ._size
                                        .prim_cast::<usize>();
                                    let item_list = (0..num_items)
                                        .map(|i| raw_item_list._items[i])
                                        .map(|item| item.read_string())
                                        .collect();

                                    new_geode_drop(
                                        precedence,
                                        chance,
                                        item_list,
                                    )
                                })
                                .collect()
                        };

                        new_object_data(
                            id,
                            geode_drops_default_items,
                            geode_drops,
                        )
                    })
                    .collect();

                new_static_state(objects)
            }
        })?;

        Ok(func)
    }
}
