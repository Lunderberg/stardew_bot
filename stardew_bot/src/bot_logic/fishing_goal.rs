use std::{borrow::Cow, fmt::Display};

use crate::{
    bot_logic::{
        BuyFromMerchantGoal, DiscardItemGoal, GameStateExt as _, InventoryGoal,
        MaintainStaminaGoal, MenuCloser, SelectItemGoal, SellToMerchantGoal,
        StepCountForLuck,
    },
    game_state::{FacingDirection, Inventory, Item, ItemCategory, Key, Vector},
    Error, GameAction, GameState,
};

use super::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult, LogicStack},
    movement_goal::{FaceDirectionGoal, MovementGoal},
    OrganizeInventoryGoal,
};

pub struct FishingGoal {
    loc: FishingLocation,
    stop_time: i32,
}

pub struct FishOnceGoal {
    started_fishing: bool,
    swapped_to_treasure: bool,
    exit_tick: Option<i32>,
    wait_for_treasure_chest: bool,
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
        Self {
            loc,
            stop_time: 2700,
        }
    }

    pub fn stop_time(self, stop_time: i32) -> Self {
        Self { stop_time, ..self }
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
            && FaceDirectionGoal::new(self.facing()).is_completed(game_state)
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
    fn description(&self) -> Cow<'static, str> {
        format!("Fish at {}", self.loc).into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        _actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        if game_state.globals.in_game_time >= self.stop_time {
            return Ok(BotGoalResult::Completed);
        }

        let organization = OrganizeInventoryGoal::new(|item| {
            use super::SortedInventoryLocation as Loc;
            if item.as_fishing_rod().is_some() {
                Loc::HotBarLeft
            } else if matches!(
                item.category,
                Some(ItemCategory::Fish | ItemCategory::Junk)
            ) {
                Loc::Hidden
            } else if item.edibility > 0 {
                Loc::HotBarRight
            } else {
                Loc::HotBar
            }
        });
        if !organization.is_completed(game_state) {
            return Ok(organization.into());
        }

        let inventory = &game_state.player.inventory;

        if inventory.contains(&Item::FIBERGLASS_ROD) {
            // Discard the Bamboo Pole as soon as we have the
            // Fiberglass Rod.
            let goal = DiscardItemGoal::new(Item::BAMBOO_POLE);
            if !goal.is_completed(game_state) {
                return Ok(goal.into());
            }
        }

        let opt_current_pole = game_state
            .iter_accessible_items()?
            .find(|item| item.as_fishing_rod().is_some());
        let Some(current_pole) = opt_current_pole else {
            // Empty out the inventory before heading to the Beach to
            // trigger the Day2 cutscene with Willy.
            let trigger_willy_cutscene = FishingLocation::Ocean
                .movement_goal()
                .with_tolerance(1000.0);

            let goal = LogicStack::new()
                .then(InventoryGoal::empty())
                .then(trigger_willy_cutscene);
            return Ok(goal.into());
        };

        let preparation = if game_state.player.room_name == "Farm" {
            // Before leaving the farm, empty out the current
            // inventory except for the fishing pole.
            InventoryGoal::empty().with(current_pole.clone())
        } else {
            // If not currently on the farm, can resume fishing
            // without emptying out the inventory, so long as we have
            // the fishing rod.
            InventoryGoal::new(current_pole.clone())
        };
        if !preparation.is_completed(game_state) {
            return Ok(preparation.into());
        }

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

        // Load bait into fishing pole, if bait is available and the
        // fishing pole can use bait.
        let goal = LoadBaitOntoFishingRod::new(Item::BAIT.clone());
        if !goal.is_completed(game_state) {
            return Ok(goal.into());
        }

        if !using_bamboo_pole {
            let opt_bait = current_pole
                .as_fishing_rod()
                .expect("Guarded by earlier as_fishing_rod.is_some() check")
                .bait
                .as_ref();

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

        if self.stop_time > 2530 && in_game_time > 2000 {
            // If there isn't enough time to passively manipulate luck
            // at the end of the day, actively walk into a wall to
            // manipulate luck.
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
            exit_tick: None,
            wait_for_treasure_chest: false,
        }
    }
}

impl BotGoal for FishOnceGoal {
    fn description(&self) -> Cow<'static, str> {
        "Fish once".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        let _ = actions;
        let fishing = &game_state.fishing;

        // In case the first-geode popup appears from a treasure
        // chest.
        if let Some(menu) = &game_state.dialogue_menu {
            if menu.responses.is_empty() {
                actions
                    .do_action(GameAction::LeftClick)
                    .annotate("Clear out dialogue menu");
                return Ok(BotGoalResult::InProgress);
            }
        }

        if let Some(menu) = &game_state.chest_menu {
            self.wait_for_treasure_chest = false;
            let opt_item_pixel = menu
                .chest_items
                .iter_slots()
                .zip(menu.chest_item_locations.iter())
                .find(|(opt_item, _)| opt_item.is_some())
                .map(|(_, pixel)| pixel)
                .cloned();

            if let Some(pixel) = opt_item_pixel {
                actions
                    .do_action(GameAction::MouseOverPixel(pixel))
                    .annotate("Mouse over treasure");
                actions
                    .do_action(GameAction::LeftClick)
                    .annotate("Click on treasure");
                return Ok(BotGoalResult::InProgress);
            } else {
                return Ok(MenuCloser::new().into());
            }
        }

        if fishing.showing_treasure {
            // The FishingRod variables get reset slightly earlier
            // than the display of the treasure chest.  If the readout
            // occurs after the FishingRod is reset but before the
            // treasure chest appears, the `FishOnceGoal` may exit
            // prematurely.
            //
            // If we know to expect a treasure chest, then we can wait
            // until the treasure chest appears.
            self.wait_for_treasure_chest = true;
        }

        if fishing.is_casting
            || fishing.bobber_in_air
            || fishing.pulling_out_of_water
            || fishing.showing_treasure
        {
            // Do nothing
        } else if fishing.showing_fish {
            if !game_state.inputs.left_mouse_down()
                && !game_state.inputs.keys_pressed.contains(&Key::C)
                && self
                    .exit_tick
                    .map(|prev_tick| {
                        prev_tick + 5 < game_state.globals.game_tick
                    })
                    .unwrap_or(true)
            {
                self.exit_tick = Some(game_state.globals.game_tick);
                actions
                    .do_action(GameAction::LeftClick)
                    .annotate("Click through the new fish menu");
            }
        } else if fishing.is_timing_cast {
            if fishing.casting_power > 0.95 {
                actions
                    .do_action(GameAction::ReleaseTool)
                    .annotate("Cast bobber");
            } else {
                actions
                    .do_action(GameAction::HoldTool)
                    .annotate("Wait for max cast distance");
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
            actions.do_action(action).annotate("Playing minigame");
        } else if fishing.showing_fish {
            actions
                .do_action(GameAction::LeftClick)
                .annotate("Stop showing fish");
        } else if fishing.is_nibbling {
            self.started_fishing = true;
            actions
                .do_action(GameAction::ReleaseTool)
                .annotate("Start minigame");
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
            actions
                .do_action(GameAction::ReleaseTool)
                .annotate("Skip fish with long wait");
        } else if fishing.is_fishing {
            actions
                .do_action(GameAction::HoldTool)
                .annotate("Wait for nibble");
        } else if self.started_fishing {
            // This is the second time around, so stop here rather
            // than starting another attempt at fishing.
            if !self.wait_for_treasure_chest {
                return Ok(BotGoalResult::Completed);
            }
        } else if fishing.is_holding_rod {
            actions
                .do_action(GameAction::HoldTool)
                .annotate("Start fishing");
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
    fn description(&self) -> Cow<'static, str> {
        format!("Load {} onto fishing rod", self.bait).into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        let Some(rod_slot) = self.fishing_rod_slot(game_state) else {
            return Ok(BotGoalResult::InProgress);
        };

        if let Some(pause) = &game_state.pause_menu {
            if let Some(page) = pause.inventory_page() {
                if let Some(held_item) = &page.held_item {
                    if held_item.is_same_item(&self.bait) {
                        // The cursor is holding the bait, so load it
                        // onto the fishing rod.
                        let pixel = page.player_item_locations[rod_slot];
                        actions.do_action(GameAction::MouseOverPixel(pixel));
                        actions.do_action(GameAction::RightClick);
                        return Ok(BotGoalResult::InProgress);
                    } else {
                        // The inventory currently holds something other
                        // than the bait to be loaded (e.g. different bait
                        // that has just been removed from the fishing
                        // rod).  Place it in an empty slot of the
                        // inventory.
                        let slot = game_state.player.inventory.empty_slot().expect(
                            "TODO: Handle case where held item has nowhere to go",
                        );
                        let pixel = page.player_item_locations[slot];
                        actions.do_action(GameAction::MouseOverPixel(pixel));
                        actions.do_action(GameAction::LeftClick);
                        return Ok(BotGoalResult::InProgress);
                    }
                }
            }
        }

        let Some(bait_slot) = self.bait_slot(game_state) else {
            let cleanup = MenuCloser::new();
            if cleanup.is_completed(game_state) {
                return Ok(BotGoalResult::Completed);
            } else {
                return Ok(cleanup.into());
            }
        };

        let Some(pause) = &game_state.pause_menu else {
            actions.do_action(GameAction::ExitMenu);
            return Ok(BotGoalResult::InProgress);
        };

        let Some(page) = pause.inventory_page() else {
            actions.do_action(GameAction::MouseOverPixel(pause.tab_buttons[0]));
            actions.do_action(GameAction::LeftClick);
            return Ok(BotGoalResult::InProgress);
        };

        if page.held_item.is_none() {
            let pixel = page.player_item_locations[bait_slot];
            actions.do_action(GameAction::MouseOverPixel(pixel));
            actions.do_action(GameAction::LeftClick);
            return Ok(BotGoalResult::InProgress);
        }

        let pixel = page.player_item_locations[rod_slot];
        actions.do_action(GameAction::MouseOverPixel(pixel));
        actions.do_action(GameAction::RightClick);

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
