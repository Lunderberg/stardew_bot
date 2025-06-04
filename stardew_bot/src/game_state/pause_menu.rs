use dotnet_debugger::{RustNativeObject, SymbolicGraph, SymbolicValue};

use crate::Error;

use super::{Item, Vector};

#[derive(RustNativeObject, Debug, Clone)]
pub struct PauseMenu {
    pub active_page: MenuPage,
    pub exit_button: Vector<isize>,
}

#[derive(RustNativeObject, Debug, Clone)]
pub enum MenuPage {
    Inventory(InventoryPage),
    Crafting(CraftingPage),
    Other(usize),
}

#[derive(Debug, Clone)]
pub struct InventoryPage {
    pub player_item_locations: Vec<Vector<isize>>,
    pub equipment_icon_locations: Vec<Vector<isize>>,
    pub held_item: Option<Item>,
    pub trash_can: Vector<isize>,
}

#[derive(Debug, Clone)]
pub struct CraftingPage {
    pub current_recipe_page: usize,
    pub recipes: Vec<CraftingRecipe>,
    pub up_button: Option<Vector<isize>>,
    pub down_button: Option<Vector<isize>>,
    pub player_item_locations: Vec<Vector<isize>>,
    pub held_item: Option<Item>,
    pub trash_can: Vector<isize>,
}

#[derive(RustNativeObject, Debug, Clone)]
pub struct CraftingRecipe {
    pub recipe_page: usize,
    pub pixel_location: Vector<isize>,
    pub item: Item,
}

impl PauseMenu {
    pub(crate) fn def_read_pause_menu(
        graph: &mut SymbolicGraph,
    ) -> Result<SymbolicValue, Error> {
        graph.named_native_function(
            "new_inventory_page",
            |player_item_locations: &Vec<Vector<isize>>,
             equipment_icon_locations: &Vec<Vector<isize>>,
             held_item: Option<&Item>,
             trash_can: &Vector<isize>| {
                MenuPage::Inventory(InventoryPage {
                    player_item_locations: player_item_locations.clone(),
                    equipment_icon_locations: equipment_icon_locations.clone(),
                    held_item: held_item.cloned(),
                    trash_can: *trash_can,
                })
            },
        )?;

        graph.named_native_function(
            "new_crafting_page",
            |current_recipe_page: usize,
             recipes: &Vec<CraftingRecipe>,
             up_button: Option<&Vector<isize>>,
             down_button: Option<&Vector<isize>>,
             player_item_locations: &Vec<Vector<isize>>,
             held_item: Option<&Item>,
             trash_can: &Vector<isize>| {
                MenuPage::Crafting(CraftingPage {
                    current_recipe_page,
                    recipes: recipes.clone(),
                    up_button: up_button.cloned(),
                    down_button: down_button.cloned(),
                    player_item_locations: player_item_locations.clone(),
                    held_item: held_item.cloned(),
                    trash_can: *trash_can,
                })
            },
        )?;

        graph.named_native_function(
            "new_unparsed_page",
            |tab_index: usize| MenuPage::Other(tab_index),
        )?;

        graph.named_native_function(
            "new_pause_menu",
            |active_page: &MenuPage, exit_button: &Vector<isize>| PauseMenu {
                active_page: active_page.clone(),
                exit_button: exit_button.clone(),
            },
        )?;

        graph.named_native_function(
            "new_crafting_recipe",
            |recipe_page: usize,
             pixel_location: &Vector<isize>,
             item_id: &str,
             big_craftable: bool| {
                let item_id = if big_craftable {
                    format!("(BC){item_id}")
                } else {
                    format!("(O){item_id}")
                };
                let item = Item::new(item_id);
                CraftingRecipe {
                    recipe_page,
                    pixel_location: *pixel_location,
                    item,
                }
            },
        )?;

        graph.named_native_function(
            "new_vec_recipe",
            |capacity: usize| -> Vec<CraftingRecipe> {
                Vec::with_capacity(capacity)
            },
        )?;

        graph.named_native_function(
            "append_to_vec_recipe",
            |vec: &mut Vec<CraftingRecipe>, recipe: &CraftingRecipe| {
                vec.push(recipe.clone());
            },
        )?;

        let func = graph.parse(stringify! {
            fn read_inventory_page(page) {
                let player_item_locations = get_item_locations(
                    page.inventory
                );

                let num_equipment = page
                    .equipmentIcons
                    ._size
                    .prim_cast::<usize>();
                let equipment = (0..num_equipment)
                    .map(|i| page.equipmentIcons._items[i] )
                    .map(|icon| center_of_gui_rect(icon))
                    .collect();

                let trash_can = center_of_gui_rect(
                    page.trashCan
                );

                let held_item = read_item(
                    StardewValley.Game1
                        ._player
                        .cursorSlotItem
                        .value
                );

                new_inventory_page(
                    player_item_locations,
                    equipment,
                    held_item,
                    trash_can,
                )
            }

            fn read_crafting_page(page) {
                let player_item_locations = get_item_locations(
                    page.inventory
                );

                let trash_can = center_of_gui_rect(
                    page.trashCan
                );

                let held_item = read_item(
                    StardewValley.Game1
                        ._player
                        .cursorSlotItem
                        .value
                );

                let current_crafting_page = page
                    .currentCraftingPage;

                // TODO: Implement .flat_map() to simplify this
                // iterator chain.
                let num_crafting_pages = page
                    .pagesOfCraftingRecipes
                    ._size
                    .prim_cast::<usize>();
                let recipes = (0..num_crafting_pages)
                    .reduce(
                        new_vec_recipe(num_crafting_pages),
                        |vec, i_page| {
                            let recipe_page = page
                                .pagesOfCraftingRecipes
                                ._items[i_page];

                            let num_recipes = recipe_page
                                ._entries
                                .len();
                            (0..num_recipes)
                                .map(|i_recipe| recipe_page._entries[i_recipe])
                                .filter(|entry| entry.hashCode != 0)
                                .map(|entry| {
                                    let pixel_location = center_of_gui_rect(
                                        entry.key
                                    );

                                    let item_id = entry
                                        .value
                                        .itemToProduce
                                        ._items[0]
                                        .read_string();

                                    let big_craftable = entry
                                        .value
                                        .bigCraftable;

                                    new_crafting_recipe(
                                        i_page,
                                        pixel_location,
                                        item_id,
                                        big_craftable,
                                    )
                                })
                                .reduce(vec, append_to_vec_recipe)
                        }
                    );

                let up_button = center_of_gui_rect(page.upButton);
                let down_button = center_of_gui_rect(page.downButton);

                new_crafting_page(
                    current_crafting_page,
                    recipes,
                    up_button,
                    down_button,
                    player_item_locations,
                    held_item,
                    trash_can,
                )
            }

            fn read_pause_menu() {
                let menu = StardewValley.Game1
                    ._activeClickableMenu
                    .as::<StardewValley.Menus.GameMenu>();

                let current_tab = menu.currentTab;

                let num_tabs = menu
                    .tabs
                    ._size
                    .prim_cast::<usize>();
                let tabs = (0..num_tabs)
                    .map(|i_tab: usize| menu.tabs._items[i_tab] )
                    .map(|tab| center_of_gui_rect(tab))
                    .collect();

                let current_page = menu
                    .pages
                    ._items[current_tab];

                let inventory_page = current_page
                    .as::<StardewValley.Menus.InventoryPage>();

                let crafting_page = current_page
                    .as::<StardewValley.Menus.CraftingPage>();

                let active_page = if inventory_page.is_some() {
                    read_inventory_page(inventory_page)
                } else if crafting_page.is_some() {
                    read_crafting_page(crafting_page)
                } else {
                    new_unparsed_page(current_tab)
                };

                let exit_button = center_of_gui_rect(menu.upperRightCloseButton);

                if menu.is_some(){
                    new_pause_menu(
                        active_page,
                        exit_button,
                    )
                } else {
                    None
                }
            }
        })?;

        Ok(func)
    }

    pub fn inventory_page(&self) -> Option<&InventoryPage> {
        match &self.active_page {
            MenuPage::Inventory(page) => Some(page),
            _ => None,
        }
    }

    pub fn crafting_page(&self) -> Option<&CraftingPage> {
        match &self.active_page {
            MenuPage::Crafting(page) => Some(page),
            _ => None,
        }
    }
}
