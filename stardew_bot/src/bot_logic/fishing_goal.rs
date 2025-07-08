use std::{borrow::Cow, fmt::Display};

use crate::{
    bot_logic::{
        BuyFromMerchantGoal, DiscardItemGoal, GameStateExt as _, InventoryGoal,
        MaintainStaminaGoal, MenuCloser, SelectItemGoal, SellToMerchantGoal,
        StepCountForLuck, UseItemOnTile,
    },
    game_state::{
        FacingDirection, FishingRod, Inventory, Item, ItemCategory, ItemId,
        Key, Quality, Vector,
    },
    Error, GameAction, GameState,
};

use super::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult, LogicStack},
    movement_goal::{FaceDirectionGoal, MovementGoal},
    ActivateTile, OrganizeInventoryGoal,
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
    /// The type of bait to load onto the fishing rod.  If None, will
    /// load the same type of bait as is currently on the rod.
    bait: Option<ItemId>,
}

struct UnloadFishingRod {
    rod: ItemId,
}

struct FishSelling<'a> {
    inventory: &'a Inventory,
    current_money: i32,
    multiplier: f32,
}

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

    const fn bait_maker(&self) -> Option<(Vector<isize>, ItemId)> {
        match self {
            FishingLocation::River => {
                Some((Vector::new(71, 49), ItemId::CATFISH))
            }
            FishingLocation::Ocean => None,
            FishingLocation::OceanByWilly => None,
            FishingLocation::Lake => None,
        }
    }
}

impl<'a> FishSelling<'a> {
    pub fn new(game_state: &'a GameState) -> Self {
        let inventory = &game_state.player.inventory;
        let current_money = game_state.player.current_money;
        let professions = &game_state.daily.professions;
        let multiplier = if professions.contains(&8) {
            1.5
        } else if professions.contains(&6) {
            1.25
        } else {
            1.0
        };
        Self {
            inventory,
            current_money,
            multiplier,
        }
    }

    fn iter_fish(&self) -> impl Iterator<Item = &Item> + '_ {
        self.inventory
            .iter_items()
            .filter(|item| matches!(item.category, Some(ItemCategory::Fish)))
    }

    fn fish_money(&self) -> i32 {
        self.iter_fish()
            .map(|item| ((item.stack_price() as f32) * self.multiplier) as i32)
            .sum::<i32>()
    }

    pub fn available_money(&self) -> i32 {
        self.current_money + self.fish_money()
    }

    fn sell_all_fish(&self) -> LogicStack {
        SellToMerchantGoal::new("Buy Fish", self.iter_fish().cloned()).into()
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

        let loc = self.loc;
        let organization = OrganizeInventoryGoal::new(move |item| {
            use super::SortedInventoryLocation as Loc;
            if item.as_fishing_rod().is_some() {
                Loc::HotBarLeft
            } else if loc
                .bait_maker()
                .map(|(_, preferred)| preferred.item_id == item.id.item_id)
                .unwrap_or(false)
            {
                Loc::HotBar
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

        if inventory.contains(&ItemId::FIBERGLASS_ROD) {
            // Discard the Bamboo Pole as soon as we have the
            // Fiberglass Rod.
            let goal = DiscardItemGoal::new(ItemId::BAMBOO_POLE);
            if !goal.is_completed(game_state) {
                return Ok(goal.into());
            }
        }
        if inventory.contains(&ItemId::IRIDIUM_ROD) {
            // Unload and discard the Fiberglass Rod asas soon as we
            // have the Iridium Rod.
            let unload = UnloadFishingRod::new(ItemId::FIBERGLASS_ROD);
            if !unload.is_completed(game_state) {
                return Ok(unload.into());
            }

            let goal = DiscardItemGoal::new(ItemId::FIBERGLASS_ROD);
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

        let opt_bait_maker = self
            .loc
            .bait_maker()
            .map(|(bait_maker_tile, preferred_fish)| -> Result<_, Error> {
                Ok(game_state
                    .get_room(self.loc.room_name())?
                    .objects
                    .iter()
                    .find(|obj| obj.tile == bait_maker_tile)
                    .filter(|obj| obj.kind.as_bait_maker().is_some())
                    .map(|bait_maker| (bait_maker, preferred_fish)))
            })
            .transpose()?
            .flatten();

        let has_bait_maker =
            game_state.player.inventory.contains(&ItemId::BAIT_MAKER);

        // Before leaving the farm, empty out the current inventory
        // except for the fishing pole.  If not currently on the farm,
        // can resume fishing without emptying out the inventory, so
        // long as we have the fishing rod.
        let mut preparation =
            InventoryGoal::current().with(current_pole.clone());
        if game_state.player.room_name == "Farm" {
            preparation = preparation
                .otherwise_empty()
                .craft_missing()
                .with_exactly(
                    ItemId::BAIT_MAKER
                        .with_count(opt_bait_maker.is_none() as usize),
                )
                .with_exactly(
                    InventoryGoal::current()
                        .iter_stored_and_carried(game_state)?
                        .map(|item| &item.id)
                        .filter(|id| id.item_id == ItemId::CATFISH.item_id)
                        .min_by_key(|item| item.quality)
                        .map(|id| {
                            id.clone().with_count(has_bait_maker as usize)
                        })
                        .unwrap_or(ItemId::CATFISH.with_count(0)),
                );
        }

        if !preparation.is_completed(game_state)? {
            return Ok(preparation.into());
        }

        let using_bamboo_pole = current_pole.is_same_item(&ItemId::BAMBOO_POLE);
        let using_fiberglass_rod =
            current_pole.is_same_item(&ItemId::FIBERGLASS_ROD);
        let in_game_time = game_state.globals.in_game_time;
        let fish_shop_open = (900..1700).contains(&in_game_time);

        if using_bamboo_pole
            && game_state.player.skills.fishing_xp >= 380
            && fish_shop_open
        {
            // The shop is open, and we have enough XP to unlock the
            // Fiberglass Rod.  Check how much cash we can get by
            // selling all current fish, and whether that's enough to
            // buy the Fiberglass Rod.
            let fish_selling = FishSelling::new(game_state);
            let can_upgrade = fish_selling.available_money() >= 1800;
            if can_upgrade {
                let goal = fish_selling.sell_all_fish().then(
                    BuyFromMerchantGoal::new(
                        "Buy Fish",
                        ItemId::FIBERGLASS_ROD,
                    ),
                );
                return Ok(goal.into());
            }
        }

        if using_fiberglass_rod
            && game_state.player.skills.fishing_xp >= 3300
            && fish_shop_open
        {
            // The shop is open, and we have enough XP to unlock the
            // Iridium Rod.  Check how much cash we can get by
            // selling all current fish, and whether that's enough to
            // buy the Iridium Rod.
            let fish_selling = FishSelling::new(game_state);
            let can_upgrade = fish_selling.available_money() >= 7500;
            if can_upgrade {
                let goal = fish_selling.sell_all_fish().then(
                    BuyFromMerchantGoal::new("Buy Fish", ItemId::IRIDIUM_ROD),
                );
                return Ok(goal.into());
            }
        }

        // Load bait into fishing pole, if bait is available and the
        // fishing pole can use bait.
        let goal = LoadBaitOntoFishingRod::new().bait(
            self.loc
                .bait_maker()
                .filter(|_| opt_bait_maker.is_some() || has_bait_maker)
                .map(|(_, preferred_fish)| {
                    ItemId::TARGETED_BAIT.with_subtype(preferred_fish)
                })
                .unwrap_or(ItemId::BAIT),
        );
        if !goal.is_completed(game_state) {
            return Ok(goal.into());
        }

        let fishing_rod = current_pole
            .as_fishing_rod()
            .expect("Guarded by earlier as_fishing_rod.is_some() check");

        if !using_bamboo_pole {
            let opt_bait = fishing_rod.bait.as_ref();

            if in_game_time < 1700
                && opt_bait
                    .map(|bait| {
                        bait.is_same_item(&ItemId::BAIT)
                            && bait.count < 150
                            && self.loc == FishingLocation::OceanByWilly
                            && in_game_time > 1630
                    })
                    .unwrap_or(true)
            {
                // The fish shop is open, and we're either out of bait
                // or low on bait and nearby.  Sell all fish and buy
                // as much bait as available.
                let fish_selling = FishSelling::new(game_state);
                let total_cash = fish_selling.available_money() as usize;
                let goal = fish_selling.sell_all_fish().then(
                    BuyFromMerchantGoal::new(
                        "Buy Fish",
                        ItemId::BAIT.with_count(total_cash / 5),
                    ),
                );
                return Ok(goal.into());
            }
        }

        let goal = MaintainStaminaGoal::new();
        if !goal.is_completed(game_state) {
            return Ok(goal.into());
        }
        if game_state.player.current_stamina < 10.0 {
            return Ok(BotGoalResult::Completed);
        }

        let select_pole = SelectItemGoal::new(current_pole.id.clone());
        if !select_pole.is_completed(game_state) {
            return Ok(select_pole.into());
        }

        if opt_bait_maker.is_none() && has_bait_maker {
            if let Some((bait_maker_tile, _)) = self.loc.bait_maker() {
                let goal = UseItemOnTile::new(
                    ItemId::BAIT_MAKER,
                    self.loc.room_name(),
                    bait_maker_tile,
                );
                return Ok(goal.into());
            }
        }

        if let Some((obj, preferred_fish)) = opt_bait_maker {
            let num_preferred_bait = fishing_rod
                .bait
                .as_ref()
                .filter(|bait| {
                    let targeted_bait = ItemId::TARGETED_BAIT
                        .with_subtype(preferred_fish.clone());
                    bait.id == targeted_bait
                })
                .map(|bait| bait.count)
                .unwrap_or(0);

            let bait_maker = obj
                .kind
                .as_bait_maker()
                .expect("Protected by earlier as_bait_maker().is_some() check");

            if !bait_maker.has_held_item {
                if let Some(into_bait) = game_state
                    .player
                    .inventory
                    .worst_quality_of_type(&preferred_fish)
                {
                    let goal = UseItemOnTile::new(
                        into_bait.clone(),
                        self.loc.room_name(),
                        obj.tile,
                    );
                    return Ok(goal.into());
                }
            }

            if num_preferred_bait < 10 && bait_maker.ready_to_harvest {
                let goal = ActivateTile::new(self.loc.room_name(), obj.tile);
                return Ok(goal.into());
            }
        }

        if !self.loc.is_completed(game_state) {
            return Ok(self.loc.move_to_location().into());
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
    pub fn new() -> Self {
        Self { bait: None }
    }

    pub fn bait(self, bait: ItemId) -> Self {
        Self {
            bait: Some(bait),
            ..self
        }
    }

    /// Find the player's fishing rod
    ///
    /// Returns a tuple containing the inventory slot that contains
    /// the fishing rod and the type of bait that should be loaded
    /// onto it.
    fn find_fishing_rod_and_bait<'a>(
        &self,
        game_state: &'a GameState,
    ) -> Option<(usize, ItemId)> {
        game_state
            .player
            .inventory
            .iter_filled_slots()
            .filter(|(_, item)| !item.is_same_item(&ItemId::BAMBOO_POLE))
            .find_map(|(slot, item)| {
                let rod = item.as_fishing_rod()?;
                let bait_to_load = self
                    .bait
                    .as_ref()
                    .or_else(|| rod.bait.as_ref().map(|bait| &bait.id))
                    .cloned()
                    .unwrap_or(ItemId::BAIT);
                Some((slot, bait_to_load))
            })
    }

    fn is_completed(&self, game_state: &GameState) -> bool {
        let can_load_bait = self
            .find_fishing_rod_and_bait(game_state)
            .map(|(_, bait_to_load)| {
                game_state.player.inventory.contains(&bait_to_load)
            })
            .unwrap_or(false);

        !can_load_bait
    }
}

impl BotGoal for LoadBaitOntoFishingRod {
    fn description(&self) -> Cow<'static, str> {
        if let Some(bait) = &self.bait {
            format!("Load {bait} onto fishing rod").into()
        } else {
            "Reload bait on fishing rod".into()
        }
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        let Some((rod_slot, bait_to_load)) =
            self.find_fishing_rod_and_bait(game_state)
        else {
            return Ok(BotGoalResult::Completed);
        };

        if let Some(pause) = &game_state.pause_menu {
            if let Some(page) = pause.inventory_page() {
                if let Some(held_item) = &page.held_item {
                    if held_item.is_same_item(&bait_to_load) {
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

        let Some(bait_slot) =
            game_state.player.inventory.current_slot(&bait_to_load)
        else {
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

impl UnloadFishingRod {
    pub fn new(rod: ItemId) -> Self {
        Self { rod }
    }

    pub fn is_completed(&self, game_state: &GameState) -> bool {
        let inventory = &game_state.player.inventory;

        let has_loaded_fishing_rod = inventory
            .iter_items()
            .filter(|item| item.id == self.rod)
            .any(|item| {
                item.as_fishing_rod()
                    .map(|rod| rod.bait.is_some() || rod.tackle.is_some())
                    .unwrap_or(false)
            });

        let has_empty_slot = inventory.has_empty_slot();

        let can_unload = has_loaded_fishing_rod && has_empty_slot;

        !can_unload
    }
}

impl BotGoal for UnloadFishingRod {
    fn description(&self) -> Cow<'static, str> {
        format!("Unload {}", self.rod).into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        let inventory = &game_state.player.inventory;
        let opt_rod_slot = inventory
            .iter_filled_slots()
            .filter(|(_, item)| item.id == self.rod)
            .find(|(_, item)| {
                item.as_fishing_rod()
                    .map(|rod| rod.bait.is_some() || rod.tackle.is_some())
                    .unwrap_or(false)
            })
            .map(|(slot, _)| slot);

        let opt_empty_slot = inventory.empty_slot();

        let (Some(rod_slot), Some(empty_slot)) = (opt_rod_slot, opt_empty_slot)
        else {
            let cleanup = MenuCloser::new();
            if !cleanup.is_completed(game_state) {
                return Ok(cleanup.into());
            }
            return Ok(BotGoalResult::Completed);
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
            let pixel = page.player_item_locations[rod_slot];
            actions.do_action(GameAction::MouseOverPixel(pixel));
            actions.do_action(GameAction::RightClick);
            return Ok(BotGoalResult::InProgress);
        }

        let pixel = page.player_item_locations[empty_slot];
        actions.do_action(GameAction::MouseOverPixel(pixel));
        actions.do_action(GameAction::LeftClick);

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
