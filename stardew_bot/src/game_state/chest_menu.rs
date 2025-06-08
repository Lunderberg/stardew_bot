use dotnet_debugger::{RustNativeObject, SymbolicGraph, SymbolicValue};

use crate::Error;

use super::{Inventory, Item, Vector};

#[derive(RustNativeObject, Debug, Clone)]
pub struct ChestMenu {
    pub player_item_locations: Vec<Vector<isize>>,
    pub chest_item_locations: Vec<Vector<isize>>,
    pub chest_items: Inventory,
    pub chest_tile: Vector<isize>,
    pub held_item: Option<Item>,
    pub ok_button: Vector<isize>,
}

impl ChestMenu {
    pub(crate) fn def_read_chest_menu(
        graph: &mut SymbolicGraph,
    ) -> Result<SymbolicValue, Error> {
        graph.named_native_function(
            "new_chest_menu",
            |player_item_locations: &Vec<Vector<isize>>,
             chest_item_locations: &Vec<Vector<isize>>,
             chest_items: &Inventory,
             chest_tile: &Vector<isize>,
             held_item: Option<&Item>,
             ok_button: &Vector<isize>| {
                let num_chest_slots = chest_item_locations.len();
                let num_trailing_slots =
                    num_chest_slots - chest_items.items.len();
                let chest_items = Inventory {
                    items: chest_items
                        .items
                        .iter()
                        .cloned()
                        .chain((0..num_trailing_slots).map(|_| None))
                        .collect(),
                };
                ChestMenu {
                    player_item_locations: player_item_locations.clone(),
                    chest_item_locations: chest_item_locations.clone(),
                    chest_items: chest_items.clone(),
                    chest_tile: chest_tile.clone(),
                    held_item: held_item.cloned(),
                    ok_button: ok_button.clone(),
                }
            },
        )?;

        let func = graph.parse(stringify! {
            fn read_chest_menu() {
                let menu = StardewValley.Game1
                    ._activeClickableMenu
                    .as::<StardewValley.Menus.ItemGrabMenu>();

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

                let chest_tile = {
                    let loc = menu
                        .context
                        .as::<StardewValley.Object>()
                        .tileLocation
                        .value;
                    new_vector_isize(loc.X, loc.Y)
                };

                let ok_button = center_of_gui_rect(menu.okButton);

                if menu.is_some(){
                    new_chest_menu(
                        player_item_locations,
                        chest_item_locations,
                        chest_items,
                        chest_tile,
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
