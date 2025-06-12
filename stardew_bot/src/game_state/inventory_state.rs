use dotnet_debugger::{RustNativeObject, SymbolicGraph, SymbolicValue};
use memory_reader::Pointer;

use crate::Error;

use super::{
    item::{ItemKind, WateringCan},
    Item,
};

#[derive(RustNativeObject, Debug, Clone)]
pub struct Inventory {
    pub items: Vec<Option<Item>>,
}

impl Inventory {
    pub(crate) fn def_read_inventory(
        graph: &mut SymbolicGraph,
    ) -> Result<SymbolicValue, Error> {
        graph.named_native_function("new_inventory", |_: Pointer| {
            Inventory { items: Vec::new() }
        })?;

        graph.named_native_function(
            "new_watering_can_kind",
            |remaining_water: i32, max_water: i32| {
                ItemKind::WateringCan(WateringCan {
                    remaining_water,
                    max_water,
                })
            },
        )?;

        graph.named_native_function(
            "new_item",
            |item_id: &str,
             quality: i32,
             count: usize,
             price: i32,
             edibility: i32,
             kind: Option<&ItemKind>,
             category: Option<i32>| {
                Item::new(item_id.to_string())
                    .with_quality(quality.try_into().unwrap())
                    .with_count(count)
                    .with_price(price)
                    .with_edibility(edibility)
                    .with_item_kind(kind.cloned())
                    .with_category(category.map(Into::into))
            },
        )?;

        graph.named_native_function(
            "add_item_to_inventory",
            |inventory: &mut Inventory, item: Option<&Item>| {
                inventory.items.push(item.cloned())
            },
        )?;

        let func = graph.parse(stringify! {
            fn read_item(item) {
                let item_id = item
                    ._qualifiedItemId
                    .read_string();

                let quality = item
                    .quality
                    .value;

                let count = item
                    .stack
                    .value;

                let object = item
                    .as::<StardewValley.Object>();
                let watering_can = item
                    .as::<StardewValley.Tools.WateringCan>();

                let price = if object.is_some() {
                    object.price.value
                } else {
                    0i32
                };

                let edibility = if object.is_some() {
                    object.edibility.value
                } else {
                    -300i32
                };

                let category = if object.is_some() {
                    object.category.value
                } else {
                    None
                };

                let kind = if watering_can.is_some() {
                    let remaining_water = watering_can
                        .waterLeft
                        .value;
                    let max_water = watering_can
                        .waterCanMax;
                    new_watering_can_kind(
                        remaining_water,
                        max_water,
                    )
                } else {
                    None
                };

                new_item(
                    item_id,
                    quality,
                    count,
                    price,
                    edibility,
                    kind,
                    category
                )
            }

            fn read_inventory(container) {
                let items = container
                    .Items;

                let num_items = items
                    .count
                    .value
                    .prim_cast::<usize>();

                let arr = items
                    .array
                    .value
                    .elements
                    ._items;

                let inventory = (0..num_items)
                    .map(|i| {
                        let item = arr[i].value;
                        read_item(item)
                    })
                    .reduce(
                        new_inventory(container),
                        add_item_to_inventory,
                    );

                inventory
            }
        })?;

        Ok(func)
    }

    pub fn iter_slots(&self) -> impl Iterator<Item = Option<&Item>> + '_ {
        self.items.iter().map(|item| item.as_ref())
    }

    pub fn iter_items(&self) -> impl Iterator<Item = &Item> + '_ {
        self.iter_slots().flatten()
    }

    pub fn contains(&self, item: &Item) -> bool {
        self.iter_items()
            .any(|inv_item| inv_item.is_same_item(item))
    }

    pub fn current_slot(&self, item: &Item) -> Option<usize> {
        self.items
            .iter()
            .enumerate()
            .find(|(_, opt_inv_item)| {
                opt_inv_item
                    .as_ref()
                    .map(|inv_item| item.is_same_item(inv_item))
                    .unwrap_or(false)
            })
            .map(|(i, _)| i)
    }

    pub fn count_item(&self, item: &Item) -> usize {
        self.iter_items()
            .filter(|inv_item| inv_item.is_same_item(item))
            .map(|inv_item| inv_item.count)
            .sum::<usize>()
    }

    pub fn empty_slot(&self) -> Option<usize> {
        self.iter_slots()
            .enumerate()
            .find(|(_, opt_item)| opt_item.is_none())
            .map(|(i, _)| i)
    }

    pub fn has_empty_slot(&self) -> bool {
        self.iter_slots().any(|opt_item| opt_item.is_none())
    }

    /// Returns true if the item can be added to this inventory.
    ///
    /// If the inventory has an empty slot, or if the inventory has a
    /// non-full slot containing the item type, this method will
    /// return true.  Otherwise, will return false.
    pub fn can_add(&self, new_item: &Item) -> bool {
        self.iter_slots().any(|opt_item| match opt_item {
            None => true,
            Some(item) => item.is_same_item(new_item) && !item.is_full_stack(),
        })
    }
}
