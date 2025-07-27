use std::{borrow::Cow, fmt::Display};

use geometry::Vector;

use crate::{
    BuyFromMerchantGoal, Error, GameAction, GameStateExt as _, InventoryGoal,
    MaintainStaminaGoal, MenuCloser, SelectItemGoal, SellToMerchantGoal,
    StepCountForLuck, UseItem, UseItemOnTile,
};
use game_state::{
    FacingDirection, FishingRod, GameState, Inventory, Item, ItemCategory,
    ItemId, Key,
};

use super::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult, LogicStack},
    movement_goal::{FaceDirectionGoal, MovementGoal},
    ActivateTile, OrganizeInventoryGoal, UpgradeFishingRodGoal,
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

struct LoadBaitTackleOntoFishingRod {
    /// The type of bait to load onto the fishing rod.  If None, will
    /// load the same type of bait as is currently on the rod.
    bait: Option<ItemId>,

    /// The type of tackle to load onto the fishing rod.  If None,
    /// will select `Dressed Spinner`, or `Spinner`, depending on
    /// availability.
    tackle: Option<ItemId>,
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
        let multiplier = game_state.daily.fish_price_multiplier();
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
            .map(|item| item.stack_price_with_perk(self.multiplier))
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

        let inventory = &game_state.player.inventory;
        let has_bait_maker = inventory.contains(&ItemId::BAIT_MAKER);
        let opt_current_pole = game_state
            .iter_accessible_items()?
            .find(|item| item.as_fishing_rod().is_some());

        // Before leaving the farm, empty out the current inventory
        // except for the fishing pole.  If not currently on the farm,
        // can resume fishing without emptying out the inventory, so
        // long as we have the fishing rod.
        let mut preparation = InventoryGoal::current();
        if let Some(pole) = opt_current_pole {
            preparation = preparation.with(pole.clone());
        }
        if game_state.player.skills.fishing_level() >= 6 {
            // This check is a bit generous, since having a sufficient
            // fishing level is no guarantee that we have the Iridium
            // Rod.  However, if we don't already have it, we'll
            // probably get enough cash to buy it during the day, so
            // picking up the tackle ahead of time is still useful.
            preparation = preparation.take_if(|item| {
                matches!(item.category, Some(ItemCategory::Tackle))
            });
        }
        if game_state.player.room_name == "Farm" {
            preparation = preparation
                .stamina_recovery_slots(1)
                .otherwise_empty()
                .craft_missing(true)
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

        let upgrade = UpgradeFishingRodGoal::new();
        if !upgrade.is_completed(game_state)? {
            return Ok(upgrade.into());
        }

        let Some(current_pole) = opt_current_pole else {
            return Ok(BotGoalResult::Completed);
        };

        // Organize inventory for visibility.  Keeps the fishing rod
        // and stamina items visible, while moving any fish that
        // aren't needed for targeted bait into the later inventory
        // slots.
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

        // Load bait into fishing pole, if bait is available and the
        // fishing pole can use bait.
        let goal = LoadBaitTackleOntoFishingRod::new().bait(
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

        let in_game_time = game_state.globals.in_game_time;
        let fishing_rod = current_pole
            .as_fishing_rod()
            .expect("Guarded by earlier as_fishing_rod.is_some() check");

        if fishing_rod.num_attachment_slots > 0 {
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
        if !goal.is_completed(game_state)? {
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
                if let Some(into_bait) =
                    inventory.worst_quality_of_type(&preferred_fish)
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

        if let Some(item) = game_state
            .player
            .inventory
            .iter_items()
            .find(|item| matches!(item.category, Some(ItemCategory::Book)))
        {
            let read = UseItem::new(item.id.clone());
            return Ok(read.into());
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
        if let Some(menu) = game_state.dialogue_menu() {
            if menu.responses.is_empty() {
                actions
                    .do_action(GameAction::LeftClick)
                    .annotate("Clear out dialogue menu");
                return Ok(BotGoalResult::InProgress);
            }
        }

        if let Some(menu) = game_state.chest_menu() {
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
                > 15000.0)
        {
            // Optimization for the BambooRod.  Even though I haven't
            // found a good way to manipulate `Game1.random` calls, I
            // can still view the results.  For the BambooRod, the
            // time to wait for a fish will be between 0.5 and 30
            // seconds.  If the waiting time is too long seconds, we
            // may as well re-roll instead of waiting.
            //
            // This only impacts the fishing with the BambooRod,
            // because use of any bait will decrease the maximum wait
            // time from 30 seconds to 15 seconds.
            //
            // Even though we don't go through the minigame, set the
            // `started_fishing` flag to true, so that the
            // `FishOnceGoal` will return control to the
            // `FishingGoal`.  (e.g.  In case stamina recovery is
            // needed before the re-cast of the fishing rod.)
            self.started_fishing = true;
            actions
                .do_action(GameAction::LeftClick)
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

impl LoadBaitTackleOntoFishingRod {
    pub fn new() -> Self {
        Self {
            bait: None,
            tackle: None,
        }
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
    /// the inventory slot of the fishing rod and the `FishingRod`
    /// info.
    fn find_fishing_rod<'a>(
        &self,
        game_state: &'a GameState,
    ) -> Option<(usize, &'a FishingRod)> {
        game_state
            .player
            .inventory
            .iter_filled_slots()
            .filter_map(|(slot, item)| {
                item.as_fishing_rod().map(|rod| (slot, rod))
            })
            .max_by_key(|(_, rod)| rod.num_attachment_slots)
    }

    fn iter_items(game_state: &GameState) -> impl Iterator<Item = &Item> + '_ {
        let opt_held_item = game_state
            .pause_menu()
            .and_then(|menu| menu.inventory_page())
            .and_then(|page| page.held_item.as_ref());

        opt_held_item
            .into_iter()
            .chain(game_state.player.inventory.iter_items())
    }

    fn select_bait(
        &self,
        game_state: &GameState,
        fishing_rod: &FishingRod,
    ) -> Option<ItemId> {
        self.bait
            .as_ref()
            .or_else(|| fishing_rod.bait.as_ref().map(|bait| &bait.id))
            .cloned()
            .or(Some(ItemId::BAIT))
            .filter(|_| fishing_rod.can_use_bait())
            .filter(|bait| {
                Self::iter_items(game_state).any(|item| &item.id == bait)
            })
    }

    fn select_tackle(
        &self,
        game_state: &GameState,
        fishing_rod: &FishingRod,
    ) -> Option<ItemId> {
        self.tackle
            .as_ref()
            .or_else(|| {
                Self::iter_items(game_state)
                    .filter(|item| {
                        matches!(item.category, Some(ItemCategory::Tackle))
                    })
                    .map(|item| &item.id)
                    .filter(|&id| id != &ItemId::SONAR_BOBBER)
                    .max_by_key(|&id| -> i32 {
                        if id == &ItemId::DRESSED_SPINNER {
                            2
                        } else if id == &ItemId::SPINNER {
                            1
                        } else {
                            0
                        }
                    })
            })
            .cloned()
            .filter(|_| {
                fishing_rod.can_use_tackle() && fishing_rod.tackle.is_none()
            })
    }

    fn is_completed(&self, game_state: &GameState) -> bool {
        let Some((_, rod)) = self.find_fishing_rod(game_state) else {
            return true;
        };

        let opt_bait = self.select_bait(game_state, rod);
        let opt_tackle = self.select_tackle(game_state, rod);

        opt_bait.is_none() && opt_tackle.is_none()
    }
}

impl BotGoal for LoadBaitTackleOntoFishingRod {
    fn description(&self) -> Cow<'static, str> {
        "Load bait/tackle on fishing rod".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        let Some((rod_slot, rod)) = self.find_fishing_rod(game_state) else {
            return Ok(BotGoalResult::Completed);
        };

        let opt_bait = self.select_bait(game_state, rod);
        let opt_tackle = self.select_tackle(game_state, rod);

        if let Some(pause) = game_state.pause_menu() {
            if let Some(page) = pause.inventory_page() {
                if let Some(held_item) = &page.held_item {
                    let is_holding_correct_bait = opt_bait
                        .map(|bait| held_item.is_same_item(bait))
                        .unwrap_or(false);
                    let is_holding_correct_tackle = opt_tackle
                        .map(|tackle| held_item.is_same_item(tackle))
                        .unwrap_or(false);
                    if is_holding_correct_bait || is_holding_correct_tackle {
                        // The cursor is holding the bait, so load it
                        // onto the fishing rod.
                        let pixel = page.player_item_locations[rod_slot];
                        actions.do_action(GameAction::MouseOverPixel(pixel));
                        actions.do_action(GameAction::RightClick);
                        return Ok(BotGoalResult::InProgress);
                    }

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

        let opt_slot_to_pick_up = opt_bait
            .and_then(|bait| game_state.player.inventory.current_slot(bait))
            .or_else(|| {
                opt_tackle.and_then(|tackle| {
                    game_state.player.inventory.current_slot(tackle)
                })
            });

        let Some(slot_to_pick_up) = opt_slot_to_pick_up else {
            let cleanup = MenuCloser::new();
            if cleanup.is_completed(game_state) {
                return Ok(BotGoalResult::Completed);
            } else {
                return Ok(cleanup.into());
            }
        };

        let Some(pause) = game_state.pause_menu() else {
            actions.do_action(GameAction::ExitMenu);
            return Ok(BotGoalResult::InProgress);
        };

        let Some(page) = pause.inventory_page() else {
            actions.do_action(GameAction::MouseOverPixel(pause.tab_buttons[0]));
            actions.do_action(GameAction::LeftClick);
            return Ok(BotGoalResult::InProgress);
        };

        if page.held_item.is_none() {
            let pixel = page.player_item_locations[slot_to_pick_up];
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
