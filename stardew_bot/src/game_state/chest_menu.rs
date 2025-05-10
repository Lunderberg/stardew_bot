use dotnet_debugger::{RustNativeObject, SymbolicGraph, SymbolicValue};

use crate::Error;

use super::{Inventory, Item, Vector};

#[derive(RustNativeObject, Debug, Clone)]
pub struct ChestMenu {
    pub player_item_locations: Vec<Vector<isize>>,
    pub chest_item_locations: Vec<Vector<isize>>,
    pub chest_items: Inventory,
    pub held_item: Option<Item>,
    pub ok_button: Vector<isize>,
}

impl ChestMenu {
    pub(crate) fn def_read_chest_menu(
        graph: &mut SymbolicGraph,
    ) -> Result<SymbolicValue, Error> {
        graph.named_native_function(
            "center_of_rectangle",
            |left: isize, top: isize, width: isize, height: isize| {
                Vector::<isize>::new(left + width / 2, top + height / 2)
            },
        )?;

        graph.named_native_function(
            "new_chest_menu",
            |player_item_locations: &Vec<Vector<isize>>,
             chest_item_locations: &Vec<Vector<isize>>,
             chest_items: &Inventory,
             held_item: Option<&Item>,
             ok_button: &Vector<isize>| ChestMenu {
                player_item_locations: player_item_locations.clone(),
                chest_item_locations: chest_item_locations.clone(),
                chest_items: chest_items.clone(),
                held_item: held_item.cloned(),
                ok_button: ok_button.clone(),
            },
        )?;

        let func = graph.parse(stringify! {
            fn read_chest_menu() {
                let menu = StardewValley.Game1
                    ._activeClickableMenu
                    .as::<StardewValley.Menus.ItemGrabMenu>();

                fn center_of_bounds(obj) {
                    let bounds = obj.bounds;
                    center_of_rectangle(
                        bounds.X,
                        bounds.Y,
                        bounds.Width,
                        bounds.Height,
                    )
                }

                fn get_item_locations(inventory_menu) {
                    let num_slots = inventory_menu
                        .capacity
                        .prim_cast::<usize>();

                    let item_locations = (0..num_slots)
                        .map(|i| inventory_menu.inventory._items[i])
                        .map(|tile| center_of_bounds(tile))
                        .collect();

                    item_locations
                }

                let player_item_locations = get_item_locations(menu.inventory);
                let chest_item_locations = get_item_locations(menu.ItemsToGrabMenu);
                let held_item = read_item(menu._heldItem);

                let chest_items = {
                    let items = menu
                        .ItemsToGrabMenu
                        .actualInventory
                        .as::<StardewValley.Inventories.Inventory>();
                    read_inventory(items)
                };

                let ok_button = center_of_bounds(menu.okButton);

                if menu.is_some(){
                    new_chest_menu(
                        player_item_locations,
                        chest_item_locations,
                        chest_items,
                        held_item,
                        ok_button,
                    )
                } else {
                    None
                }
            }
        })?;

        Ok(func)
    }
}
