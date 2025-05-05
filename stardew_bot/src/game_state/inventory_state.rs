use dotnet_debugger::{RustNativeObject, SymbolicGraph, SymbolicValue};
use memory_reader::Pointer;

use crate::Error;

use super::Item;

#[derive(RustNativeObject, Debug, Clone)]
pub struct Inventory {
    pub items: Vec<Option<Item>>,
}

impl Inventory {
    pub(crate) fn read_inventory(
        graph: &mut SymbolicGraph,
    ) -> Result<SymbolicValue, Error> {
        graph.named_native_function("new_inventory", |_: Pointer| {
            Inventory { items: Vec::new() }
        })?;

        graph.named_native_function(
            "new_item",
            |item_id: &str, quality: i32, count: usize| Item {
                item_id: item_id.to_string(),
                quality: quality.try_into().unwrap(),
                count,
            },
        )?;

        graph.named_native_function(
            "add_item_to_inventory",
            |inventory: &mut Inventory, item: Option<&Item>| {
                inventory.items.push(item.cloned())
            },
        )?;

        let func = graph.parse(stringify! {
            fn read_inventory(container) {
                let items = container
                    .netItems
                    .value
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

                        let item_id = item
                            .itemId
                            .value
                            .read_string();

                        let quality = item
                            .quality
                            .value;

                        let count = item
                            .stack
                            .value;

                        new_item(item_id, quality, count)
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
}
