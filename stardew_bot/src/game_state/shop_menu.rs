use std::ops::Range;

use dotnet_debugger::{RustNativeObject, SymbolicGraph, SymbolicValue};
use geometry::Vector;

use crate::Error;

use super::Item;

#[derive(RustNativeObject, Debug, Clone)]
pub struct ShopMenu {
    pub held_item: Option<Item>,
    pub for_sale: Vec<Item>,
    pub for_sale_buttons: Vec<Vector<isize>>,
    pub for_sale_scroll_index: usize,
    pub player_item_locations: Vec<Vector<isize>>,
    pub exit_button: Vector<isize>,
}

impl ShopMenu {
    pub(crate) fn def_read_shop_menu(
        graph: &mut SymbolicGraph,
    ) -> Result<SymbolicValue, Error> {
        graph.named_native_function(
            "new_shop_menu",
            |held_item: Option<&Item>,
             for_sale: &Vec<Item>,
             for_sale_buttons: &Vec<Vector<isize>>,
             for_sale_scroll_index: usize,
             player_item_locations: &Vec<Vector<isize>>,
             exit_button: &Vector<isize>| ShopMenu {
                held_item: held_item.cloned(),
                for_sale: for_sale.clone(),
                for_sale_buttons: for_sale_buttons.clone(),
                for_sale_scroll_index,
                player_item_locations: player_item_locations.clone(),
                exit_button: exit_button.clone(),
            },
        )?;

        let func = graph.parse(stringify! {
            fn read_shop_menu() {
                let menu = StardewValley
                    .Game1
                    ._activeClickableMenu
                    .as::<StardewValley.Menus.ShopMenu>();

                let held_item = menu.heldItem.as::<StardewValley.Item>();
                let held_item = if held_item.is_some() {
                    read_item(held_item)
                } else {
                    None
                };

                let num_for_sale = menu
                    .forSale
                    ._size
                    .prim_cast::<usize>();
                let for_sale = (0..num_for_sale)
                    .map(|i| menu.forSale._items[i])
                    .map(|item_for_sale| {
                        read_item(item_for_sale.as::<StardewValley.Item>())
                    })
                    .collect();

                let num_buttons = menu
                    .forSaleButtons
                    ._size
                    .prim_cast::<usize>();
                let for_sale_buttons = (0..num_buttons)
                    .map(|i| menu.forSaleButtons._items[i])
                    .map(|button| center_of_gui_rect(button))
                    .collect();

                let for_sale_scroll_index = menu
                    .currentItemIndex
                    .prim_cast::<usize>();

                let num_player_slots = menu
                    .inventory
                    .capacity
                    .prim_cast::<usize>();
                let player_item_locations = (0..num_player_slots)
                        .map(|i| menu.inventory.inventory._items[i])
                        .map(|tile| center_of_gui_rect(tile))
                        .collect();

                let exit_button = center_of_gui_rect(
                    menu.upperRightCloseButton
                );

                if menu.is_some() {
                    new_shop_menu(
                        held_item,
                        for_sale,
                        for_sale_buttons,
                        for_sale_scroll_index,
                        player_item_locations,
                        exit_button,
                    )
                } else {
                    None
                }
            }
        })?;

        Ok(func)
    }

    pub fn visible_items(&self) -> Range<usize> {
        let start = self.for_sale_scroll_index;
        let len = self.for_sale_buttons.len();
        start..start + len
    }
}
