use dotnet_debugger::{RustNativeObject, SymbolicGraph, SymbolicValue};
use memory_reader::Pointer;

use crate::Error;

use super::Item;

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
            "new_item",
            |item_id: &str,
             quality: i32,
             count: usize,
             price: i32,
             edibility: i32| {
                Item::new(item_id.to_string())
                    .with_quality(quality.try_into().unwrap())
                    .with_count(count)
                    .with_price(price)
                    .with_edibility(edibility)
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

                new_item(
                    item_id,
                    quality,
                    count,
                    price,
                    edibility,
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
}
