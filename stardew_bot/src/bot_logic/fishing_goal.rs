use std::{borrow::Cow, fmt::Display};

use crate::{
    bot_logic::{
        BuyFromMerchantGoal, DiscardItemGoal, InventoryGoal,
        MaintainStaminaGoal, SelectItemGoal, SellToMerchantGoal,
        StepCountForLuck,
    },
    game_state::{FacingDirection, Inventory, Item, ItemCategory, Vector},
    Error, GameAction, GameState,
};

use super::{
    bot_logic::{BotGoal, BotGoalResult, LogicStack},
    movement_goal::{FaceDirectionGoal, MovementGoal},
};

pub struct FishingGoal {
    loc: FishingLocation,
}

pub struct FishOnceGoal {
    started_fishing: bool,
    swapped_to_treasure: bool,
}

#[allow(dead_code)]
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum FishingLocation {
    River,
    Ocean,
    OceanByWilly,
    Lake,
}

struct LoadBaitOntoFishingRod {
    bait: Item,
}

struct FishSelling<'a>(&'a Inventory);

impl FishingGoal {
    pub fn new(loc: FishingLocation) -> Self {
        Self { loc }
    }
}

impl FishingLocation {
    const fn room_name(&self) -> &'static str {
        match self {
            FishingLocation::River => "Forest",
            FishingLocation::Ocean => "Beach",
            FishingLocation::OceanByWilly => "Beach",
            FishingLocation::Lake => "Mountain",
        }
    }

    const fn tile(&self) -> Vector<isize> {
        match self {
            FishingLocation::River => Vector::new(70, 50),
            FishingLocation::Ocean => Vector::new(53, 25),
            FishingLocation::OceanByWilly => Vector::new(31, 36),
            FishingLocation::Lake => Vector::new(60, 37),
        }
    }

    const fn facing(&self) -> FacingDirection {
        match self {
            FishingLocation::River => FacingDirection::South,
            FishingLocation::Ocean => FacingDirection::South,
            FishingLocation::OceanByWilly => FacingDirection::South,
            FishingLocation::Lake => FacingDirection::East,
        }
    }

    fn is_completed(&self, game_state: &GameState) -> bool {
        let player = &game_state.player;
        player.room_name == self.room_name()
            && player.tile() == self.tile()
            && player.facing == self.facing()
    }

    fn movement_goal(&self) -> MovementGoal {
        MovementGoal::new(self.room_name(), self.tile().map(|x| x as f32))
    }

    fn move_to_location(&self) -> LogicStack {
        LogicStack::new()
            .then(self.movement_goal())
            .then(FaceDirectionGoal(self.facing()))
    }
}

impl FishSelling<'_> {
    fn iter_fish(&self) -> impl Iterator<Item = &Item> + '_ {
        self.0
            .iter_items()
            .filter(|item| matches!(item.category, Some(ItemCategory::Fish)))
    }

    fn fish_money(&self) -> i32 {
        self.iter_fish().map(|item| item.stack_price()).sum::<i32>()
    }

    fn sell_all_fish(&self) -> LogicStack {
        self.iter_fish().fold(LogicStack::new(), |stack, item| {
            stack.then(SellToMerchantGoal::new("Buy Fish", item.clone()))
        })
    }
}

impl BotGoal for FishingGoal {
    fn description(&self) -> Cow<str> {
        format!("Fish at {}", self.loc).into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        _do_action: &mut dyn FnMut(GameAction),
    ) -> Result<BotGoalResult, Error> {
        const FISHING_POLES: [Item; 3] =
            [Item::IRIDIUM_ROD, Item::FIBERGLASS_ROD, Item::BAMBOO_POLE];

        let inventory = &game_state.player.inventory;

        if inventory.contains(&Item::FIBERGLASS_ROD) {
            // Discard the Bamboo Pole as soon as we have the
            // Fiberglass Rod.
            let goal = DiscardItemGoal::new(Item::BAMBOO_POLE);
            if !goal.is_completed(game_state) {
                return Ok(goal.into());
            }
        }

        let opt_current_pole = inventory
            .iter_items()
            .find(|item| item.as_fishing_rod().is_some());
        let Some(current_pole) = opt_current_pole else {
            let opt_get_pole_goal = FISHING_POLES
                .iter()
                .map(|pole| InventoryGoal::empty().with(pole.clone()))
                .find(|goal| goal.is_possible(game_state));
            if let Some(goal) = opt_get_pole_goal {
                // Grab a fishing pole from whichever chest has it.
                return Ok(goal.into());
            } else {
                // Empty out the inventory before heading to the Beach
                let trigger_willy_cutscene = FishingLocation::Ocean
                    .movement_goal()
                    .with_tolerance(1000.0);

                let goal = LogicStack::new()
                    .then(InventoryGoal::empty())
                    .then(trigger_willy_cutscene);
                return Ok(goal.into());
            }
        };

        let using_bamboo_pole = current_pole.is_same_item(&Item::BAMBOO_POLE);
        let in_game_time = game_state.globals.in_game_time;

        if using_bamboo_pole
            && game_state.player.skills.fishing_xp >= 380
            && in_game_time < 1700
        {
            // The shop is open, and we have enough XP to unlock the
            // Fiberglass Rod.  Check how much cash we can get by
            // selling all current fish, and whether that's enough to
            // buy the Fiberglass Rod.
            let fish_selling = FishSelling(inventory);
            let can_upgrade = game_state.player.current_money
                + fish_selling.fish_money()
                >= 1800;
            if can_upgrade {
                let goal = fish_selling.sell_all_fish().then(
                    BuyFromMerchantGoal::new("Buy Fish", Item::FIBERGLASS_ROD),
                );
                return Ok(goal.into());
            }
        }

        if !using_bamboo_pole {
            let opt_bait = current_pole
                .as_fishing_rod()
                .expect("Guarded by earlier as_fishing_rod.is_some() check")
                .bait
                .as_ref();
            if opt_bait
                .map(|bait| bait.is_same_item(&Item::BAIT))
                .unwrap_or(true)
                && inventory.contains(&Item::BAIT)
            {
                // Load bait into fishing pole
                let goal = LoadBaitOntoFishingRod::new(Item::BAIT.clone());
                return Ok(goal.into());
            }

            if in_game_time < 1700
                && opt_bait
                    .map(|bait| {
                        bait.is_same_item(&Item::BAIT)
                            && bait.count < 150
                            && self.loc == FishingLocation::OceanByWilly
                            && in_game_time > 1630
                    })
                    .unwrap_or(true)
            {
                // The fish shop is open, and we're either out of bait
                // or low on bait and nearby.  Sell all fish and buy
                // as much bait as available.
                let fish_selling = FishSelling(inventory);
                let total_cash = (game_state.player.current_money
                    + fish_selling.fish_money())
                    as usize;
                let goal = fish_selling.sell_all_fish().then(
                    BuyFromMerchantGoal::new(
                        "Buy Fish",
                        Item::BAIT.with_count(total_cash / 5),
                    ),
                );
                return Ok(goal.into());
            }
        }

        if !self.loc.is_completed(game_state) {
            return Ok(self.loc.move_to_location().into());
        }

        let select_pole = SelectItemGoal::new(current_pole.clone());
        if !select_pole.is_completed(game_state) {
            return Ok(select_pole.into());
        }

        let goal = MaintainStaminaGoal::new();
        if !goal.is_completed(game_state) {
            return Ok(goal.into());
        }
        if game_state.player.current_stamina < 10.0 {
            return Ok(BotGoalResult::Completed);
        }

        if in_game_time > 2000 {
            let goal = StepCountForLuck::new();
            if !goal.is_completed(game_state) {
                return Ok(goal.into());
            }
        }

        let fish_once = FishOnceGoal::new();
        Ok(fish_once.into())
    }
}

impl FishOnceGoal {
    pub fn new() -> Self {
        Self {
            started_fishing: false,
            swapped_to_treasure: false,
        }
    }
}

impl BotGoal for FishOnceGoal {
    fn description(&self) -> Cow<str> {
        "Fish once".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        do_action: &mut dyn FnMut(GameAction),
    ) -> Result<BotGoalResult, Error> {
        let fishing = &game_state.fishing;

        // In case the first-geode popup appears from a treasure
        // chest.
        if let Some(menu) = &game_state.dialogue_menu {
            if menu.responses.is_empty() {
                do_action(GameAction::LeftClick);
                return Ok(BotGoalResult::InProgress);
            }
        }

        if let Some(menu) = &game_state.chest_menu {
            let opt_item_pixel = menu
                .chest_items
                .iter_slots()
                .zip(menu.chest_item_locations.iter())
                .find(|(opt_item, _)| opt_item.is_some())
                .map(|(_, pixel)| pixel)
                .cloned();

            let pixel = opt_item_pixel.unwrap_or(menu.ok_button);

            do_action(GameAction::MouseOverPixel(pixel));
            do_action(GameAction::LeftClick);
            return Ok(BotGoalResult::InProgress);
        }

        if fishing.is_timing_cast {
            if fishing.casting_power > 0.95 {
                do_action(GameAction::ReleaseTool)
            } else {
                do_action(GameAction::HoldTool)
            }
        } else if fishing.minigame_in_progress {
            if fishing.catch_progress > 0.8 {
                self.swapped_to_treasure = true;
            }
            let should_move_upward = if fishing.treasure_position.is_some()
                && self.swapped_to_treasure
            {
                fishing.should_move_upward_for_treasure()
            } else {
                fishing.should_move_upward_for_fish()
            };
            let action = if should_move_upward {
                GameAction::HoldTool
            } else {
                GameAction::ReleaseTool
            };
            do_action(action);
        } else if fishing.showing_fish {
            do_action(GameAction::LeftClick);
        } else if fishing.is_nibbling {
            self.started_fishing = true;
            do_action(GameAction::ReleaseTool)
        } else if fishing.is_fishing
            && (fishing.time_until_fishing_bite
                - fishing.fishing_bite_accumulator
                > 20000.0)
        {
            // Optimization for the BambooRod.  Even though I haven't
            // found a good way to manipulate `Game1.random` calls, I
            // can still view the results.  For the BambooRod, the
            // time to wait for a fish will be between 0.5 and 30
            // seconds.  If the waiting time is longer than 20
            // seconds, we may as well re-roll instead of waiting.
            //
            // This only impacts the fishing with the BambooRod,
            // because use of any bait will decrease the maximum wait
            // time from 30 seconds to 15 seconds.
            do_action(GameAction::ReleaseTool)
        } else if self.started_fishing {
            // This is the second time around, so stop here rather
            // than starting another attempt at fishing.
            do_action(GameAction::ReleaseTool);
            return Ok(BotGoalResult::Completed);
        } else if fishing.is_fishing {
            do_action(GameAction::HoldTool)
        } else if fishing.is_holding_rod {
            do_action(GameAction::HoldTool)
        }

        Ok(BotGoalResult::InProgress)
    }
}

impl LoadBaitOntoFishingRod {
    fn new(bait: Item) -> Self {
        Self { bait }
    }

    fn fishing_rod_slot(&self, game_state: &GameState) -> Option<usize> {
        game_state
            .player
            .inventory
            .iter_slots()
            .enumerate()
            .filter_map(|(i, opt_item)| opt_item.map(|item| (i, item)))
            .filter(|(_, item)| !item.is_same_item(&Item::BAMBOO_POLE))
            .filter(|(_, item)| item.as_fishing_rod().is_some())
            .map(|(i, _)| i)
            .next()
    }

    fn bait_slot(&self, game_state: &GameState) -> Option<usize> {
        game_state.player.inventory.current_slot(&self.bait)
    }

    fn is_completed(&self, game_state: &GameState) -> bool {
        self.fishing_rod_slot(game_state).is_none()
            || self.bait_slot(game_state).is_none()
    }
}

impl BotGoal for LoadBaitOntoFishingRod {
    fn description(&self) -> Cow<str> {
        format!("Load {} onto fishing rod", self.bait).into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        do_action: &mut dyn FnMut(GameAction),
    ) -> Result<BotGoalResult, Error> {
        if let Some(page) = game_state
            .pause_menu
            .as_ref()
            .and_then(|pause| pause.inventory_page())
        {
            if let Some(held_item) = &page.held_item {
                if let Some(rod_slot) = self
                    .fishing_rod_slot(game_state)
                    .filter(|_| held_item.is_same_item(&self.bait))
                {
                    let pixel = page.player_item_locations[rod_slot];
                    do_action(GameAction::MouseOverPixel(pixel));
                    do_action(GameAction::RightClick);
                } else {
                    let slot = game_state.player.inventory.empty_slot().expect(
                        "TODO: Handle case where held item has nowhere to go",
                    );
                    let pixel = page.player_item_locations[slot];
                    do_action(GameAction::MouseOverPixel(pixel));
                    do_action(GameAction::LeftClick);
                }

                return Ok(BotGoalResult::InProgress);
            }
        }

        if self.is_completed(game_state) {
            if let Some(menu) = &game_state.pause_menu {
                do_action(GameAction::MouseOverPixel(menu.exit_button));
                do_action(GameAction::LeftClick);
                return Ok(BotGoalResult::InProgress);
            } else {
                return Ok(BotGoalResult::Completed);
            }
        }

        let bait_slot = self
            .bait_slot(game_state)
            .expect("Guarded by is_completed() check");

        let Some(pause) = &game_state.pause_menu else {
            do_action(GameAction::ExitMenu);
            return Ok(BotGoalResult::InProgress);
        };

        let Some(inventory_page) = pause.inventory_page() else {
            do_action(GameAction::MouseOverPixel(pause.tab_buttons[0]));
            do_action(GameAction::LeftClick);
            return Ok(BotGoalResult::InProgress);
        };

        assert!(
            inventory_page.held_item.is_none(),
            "Should be handled prior to the is_completed step"
        );

        let pixel = inventory_page.player_item_locations[bait_slot];
        do_action(GameAction::MouseOverPixel(pixel));
        do_action(GameAction::LeftClick);
        Ok(BotGoalResult::InProgress)
    }
}

impl Display for FishingLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FishingLocation::River => write!(f, "River"),
            FishingLocation::Ocean => write!(f, "Ocean"),
            FishingLocation::OceanByWilly => {
                write!(f, "Ocean, in front of Willy's")
            }
            FishingLocation::Lake => write!(f, "Lake"),
        }
    }
}
