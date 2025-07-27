use dotnet_debugger::{
    Pointer, RustNativeObject, SymbolicGraph, SymbolicValue,
};

use crate::Error;

use super::{
    ChestMenu, DialogueMenu, GeodeMenu, JunimoMenu, MailMenu, MineElevatorMenu,
    PauseMenu, ShopMenu,
};

#[derive(RustNativeObject, Debug, Clone)]
pub enum Menu {
    Pause(PauseMenu),
    Chest(ChestMenu),
    Dialogue(DialogueMenu),
    Mail(MailMenu),
    Shop(ShopMenu),
    Geode(GeodeMenu),
    MineElevator(MineElevatorMenu),
    Junimo(JunimoMenu),
    Other,
}

impl Menu {
    pub(crate) fn def_read_menu(
        graph: &mut SymbolicGraph,
    ) -> Result<SymbolicValue, Error> {
        ChestMenu::def_read_chest_menu(graph)?;
        DialogueMenu::def_read_dialogue_menu(graph)?;
        ShopMenu::def_read_shop_menu(graph)?;
        PauseMenu::def_read_pause_menu(graph)?;
        MailMenu::def_read_mail_menu(graph)?;
        MineElevatorMenu::def_read_mine_elevator_menu(graph)?;
        GeodeMenu::def_read_geode_menu(graph)?;
        JunimoMenu::def_read_junimo_menu(graph)?;

        graph.named_native_function("other_chest_menu", |_: Pointer| {
            Menu::Other
        })?;

        let func = graph.parse(stringify! {
            fn read_menu() {
                let pause_menu = read_pause_menu();
                let chest_menu = read_chest_menu();
                let dialogue_menu = read_dialogue_menu();
                let shop_menu = read_shop_menu();
                let mail_menu = read_mail_menu();
                let geode_menu = read_geode_menu();
                let mine_elevator_menu = read_mine_elevator_menu();
                let junimo_menu = read_junimo_menu();

                if pause_menu.is_some() {
                    pause_menu
                } else if chest_menu.is_some() {
                    chest_menu
                } else if dialogue_menu.is_some() {
                    dialogue_menu
                } else if shop_menu.is_some() {
                    shop_menu
                } else if mail_menu.is_some() {
                    mail_menu
                } else if geode_menu.is_some() {
                    geode_menu
                } else if mine_elevator_menu.is_some() {
                    mine_elevator_menu
                } else if junimo_menu.is_some() {
                    junimo_menu
                } else {
                    other_chest_menu(
                        StardewValley.Game1
                            ._activeClickableMenu
                    )
                }
            }
        })?;

        Ok(func)
    }

    pub fn pause_menu(&self) -> Option<&PauseMenu> {
        match self {
            Self::Pause(menu) => Some(menu),
            _ => None,
        }
    }

    pub fn chest_menu(&self) -> Option<&ChestMenu> {
        match self {
            Self::Chest(menu) => Some(menu),
            _ => None,
        }
    }

    pub fn dialogue_menu(&self) -> Option<&DialogueMenu> {
        match self {
            Self::Dialogue(menu) => Some(menu),
            _ => None,
        }
    }

    pub fn mail_menu(&self) -> Option<&MailMenu> {
        match self {
            Self::Mail(menu) => Some(menu),
            _ => None,
        }
    }

    pub fn shop_menu(&self) -> Option<&ShopMenu> {
        match self {
            Self::Shop(menu) => Some(menu),
            _ => None,
        }
    }

    pub fn geode_menu(&self) -> Option<&GeodeMenu> {
        match self {
            Self::Geode(menu) => Some(menu),
            _ => None,
        }
    }

    pub fn mine_elevator_menu(&self) -> Option<&MineElevatorMenu> {
        match self {
            Self::MineElevator(menu) => Some(menu),
            _ => None,
        }
    }

    pub fn junimo_menu(&self) -> Option<&JunimoMenu> {
        match self {
            Self::Junimo(menu) => Some(menu),
            _ => None,
        }
    }
}
