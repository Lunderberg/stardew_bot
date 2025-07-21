use game_state::{GameState, Item, ItemId};

use crate::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult},
    Error, GameAction, GameStateExt as _, GoToActionTile, InventoryGoal,
    MenuCloser, ShopMenuExt as _,
};

pub struct UpgradeToolGoal {
    tool: ItemId,
}

impl UpgradeToolGoal {
    pub fn new(tool: ItemId) -> Self {
        Self { tool }
    }

    fn upgraded_tool(&self) -> ItemId {
        if self.tool == ItemId::AXE {
            ItemId::COPPER_AXE
        } else if self.tool == ItemId::COPPER_AXE {
            ItemId::IRON_AXE
        } else if self.tool == ItemId::IRON_AXE {
            ItemId::GOLD_AXE
        } else if self.tool == ItemId::GOLD_AXE {
            ItemId::IRIDIUM_AXE
        } else if self.tool == ItemId::PICKAXE {
            ItemId::COPPER_PICKAXE
        } else if self.tool == ItemId::COPPER_PICKAXE {
            ItemId::IRON_PICKAXE
        } else if self.tool == ItemId::IRON_PICKAXE {
            ItemId::GOLD_PICKAXE
        } else if self.tool == ItemId::GOLD_PICKAXE {
            ItemId::IRIDIUM_PICKAXE
        } else if self.tool == ItemId::HOE {
            ItemId::COPPER_HOE
        } else if self.tool == ItemId::COPPER_HOE {
            ItemId::IRON_HOE
        } else if self.tool == ItemId::IRON_HOE {
            ItemId::GOLD_HOE
        } else if self.tool == ItemId::GOLD_HOE {
            ItemId::IRIDIUM_HOE
        } else if self.tool == ItemId::WATERING_CAN {
            ItemId::COPPER_WATERING_CAN
        } else if self.tool == ItemId::COPPER_WATERING_CAN {
            ItemId::IRON_WATERING_CAN
        } else if self.tool == ItemId::IRON_WATERING_CAN {
            ItemId::GOLD_WATERING_CAN
        } else if self.tool == ItemId::GOLD_WATERING_CAN {
            ItemId::IRIDIUM_WATERING_CAN
        } else {
            panic!("Unknown tool id: '{}'", self.tool)
        }
    }

    fn material_cost(&self) -> Item {
        // TODO: Based on the tool being upgraded, determine which
        // bars are required in order to perform the upgrade.
        ItemId::COPPER_BAR.with_count(5)
    }

    fn gp_cost(&self) -> i32 {
        // TODO: Based on the tool being upgraded, check against the
        // correct amount of required GP.
        2000
    }

    pub fn is_completed(&self, game_state: &GameState) -> Result<bool, Error> {
        let exists_somewhere = game_state
            .iter_accessible_items()?
            .any(|item| item.id == self.tool);

        let material_cost = self.material_cost();
        let material_available = game_state
            .iter_accessible_items()?
            .filter(|item| item.id == material_cost.id)
            .map(|item| item.count)
            .sum::<usize>();
        let have_materials = material_available >= material_cost.count;

        let have_gp = game_state.player.current_money >= self.gp_cost();

        let can_upgrade = have_gp && have_materials && exists_somewhere;

        Ok(!can_upgrade)
    }
}

impl BotGoal for UpgradeToolGoal {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        format!("Upgrade {}", self.tool).into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        if self.is_completed(game_state)? {
            let cleanup = MenuCloser::new();
            if !cleanup.is_completed(game_state) {
                return Ok(cleanup.into());
            }
            return Ok(BotGoalResult::Completed);
        }

        let prepare = InventoryGoal::current()
            .with(self.tool.clone())
            .with(self.material_cost());
        if !prepare.is_completed(game_state)? {
            return Ok(prepare.into());
        }

        if let Some(menu) = &game_state.shop_menu {
            menu.do_menu_navigation(actions, &self.upgraded_tool())?;
            Ok(BotGoalResult::InProgress)
        } else if let Some(menu) = &game_state.dialogue_menu {
            let Some(pixel) = menu.response_pixel("Upgrade") else {
                // Wrong menu, so close it and try again
                let cleanup = MenuCloser::new();
                if cleanup.is_completed(game_state) {
                    return Ok(BotGoalResult::InProgress);
                } else {
                    return Ok(cleanup.into());
                }
            };

            actions.do_action(GameAction::MouseOverPixel(pixel));
            actions.do_action(GameAction::LeftClick);
            Ok(BotGoalResult::InProgress)
        } else {
            Ok(GoToActionTile::new("Blacksmith").into())
        }
    }
}
