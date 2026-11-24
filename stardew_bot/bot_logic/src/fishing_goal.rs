use std::{borrow::Cow, fmt::Display};

use geometry::{CardinalDirection, Vector};

use crate::{
    BuyFromMerchantGoal, Error, GameAction, GameStateExt as _, InventoryGoal,
    LoadUpFishingRod, MaintainStaminaGoal, MenuCloser, SelectItemGoal,
    SellToMerchantGoal, StepCountForLuck, UseItem, UseItemOnTile,
    WaitUntilTimeOfDay,
};
use game_state::{GameState, Inventory, Item, ItemCategory, ItemId, Key};

use super::{
    ActivateTile, OrganizeInventoryGoal, UpgradeFishingRodGoal,
    bot_logic::{ActionCollector, BotGoal, BotGoalResult, LogicStack},
    movement_goal::{FaceDirectionGoal, MovementGoal},
};

pub struct FishingGoal {
    loc: FishingLocation,
    stop_time: i32,
}

pub struct FishOnceGoal {
    target_bobber_pos: Option<Vector<f32>>,
    started_minigame: bool,
}

struct FishingMinigameGoal {
    target_bobber_pos: Option<Vector<f32>>,
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

    pub fn is_completed(&self, game_state: &GameState) -> bool {
        game_state.globals.in_game_time >= self.stop_time
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

    const fn facing(&self) -> CardinalDirection {
        match self {
            FishingLocation::River => CardinalDirection::South,
            FishingLocation::Ocean => CardinalDirection::South,
            FishingLocation::OceanByWilly => CardinalDirection::South,
            FishingLocation::Lake => CardinalDirection::East,
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
        SellToMerchantGoal::new("Buy Fish")
            .sell_all(self.iter_fish().cloned())
            .into()
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
            .filter(|item| item.as_fishing_rod().is_some())
            .max_by_key(|item| {
                item.as_fishing_rod().unwrap().num_attachment_slots
            });

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
                .stamina_recovery_slots(2)
                .otherwise_empty()
                .craft_missing(true)
                .with(ItemId::OAK_CHAIR)
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
        let goal = LoadUpFishingRod::new().bait(
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
                && (opt_bait_maker.is_none() && !has_bait_maker)
            {
                // The fish shop is open, and we're either out of bait
                // or low on bait and nearby.  Sell all fish and buy
                // as much bait as available.
                let fish_selling = FishSelling::new(game_state);
                let total_cash = fish_selling.available_money() as usize;
                let num_to_buy = total_cash / 5;
                if num_to_buy > 0 {
                    let goal = fish_selling.sell_all_fish().then(
                        BuyFromMerchantGoal::new(
                            "Buy Fish",
                            ItemId::BAIT.with_count(num_to_buy),
                        ),
                    );
                    return Ok(goal.into());
                }
            }
        }

        let goal = MaintainStaminaGoal::new();
        if !goal.is_completed(game_state)? {
            return Ok(goal.into());
        }
        if game_state.player.current_stamina < 10.0 {
            return Ok(BotGoalResult::Completed);
        }

        if opt_bait_maker.is_none()
            && has_bait_maker
            && let Some((bait_maker_tile, _)) = self.loc.bait_maker()
        {
            let goal = UseItemOnTile::new(
                ItemId::BAIT_MAKER,
                self.loc.room_name(),
                bait_maker_tile,
            );
            return Ok(goal.into());
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

            if bait_maker.held_item.is_none()
                && let Some(into_bait) =
                    inventory.worst_quality_of_type(&preferred_fish)
            {
                let goal = UseItemOnTile::new(
                    into_bait.clone(),
                    self.loc.room_name(),
                    obj.tile,
                );
                return Ok(goal.into());
            }

            if num_preferred_bait < 10 && bait_maker.ready_to_harvest {
                let goal = ActivateTile::new(self.loc.room_name(), obj.tile);
                return Ok(goal.into());
            }

            if num_preferred_bait == 0 && bait_maker.held_item.is_some() {
                let goal = WaitUntilTimeOfDay::new(
                    game_state.globals.in_game_time + 10,
                );
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
            if !goal.is_completed(game_state)? {
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
            target_bobber_pos: None,
            started_minigame: false,
        }
    }

    pub fn target_bobber_pos(self, target_bobber_pos: Vector<f32>) -> Self {
        Self {
            target_bobber_pos: Some(target_bobber_pos),
            ..self
        }
    }
}

impl BotGoal for FishOnceGoal {
    fn description(&self) -> Cow<'static, str> {
        if let Some(target) = self.target_bobber_pos {
            format!("Fish once at {target}").into()
        } else {
            "Fish once".into()
        }
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        _actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        if self.started_minigame {
            return Ok(BotGoalResult::Completed);
        }

        let opt_pole = game_state
            .player
            .inventory
            .iter_items()
            .filter(|item| item.as_fishing_rod().is_some())
            .max_by_key(|item| {
                item.as_fishing_rod().unwrap().num_attachment_slots
            });
        let Some(pole) = opt_pole else {
            return Ok(BotGoalResult::Completed);
        };

        let select_pole = SelectItemGoal::new(pole.id.clone());
        if !select_pole.is_completed(game_state) {
            return Ok(select_pole.into());
        }

        self.started_minigame = true;
        let minigame = FishingMinigameGoal::new()
            .target_bobber_pos(self.target_bobber_pos);
        Ok(minigame.into())
    }
}

impl Default for FishOnceGoal {
    fn default() -> Self {
        Self::new()
    }
}

impl FishingMinigameGoal {
    pub fn new() -> Self {
        Self {
            target_bobber_pos: None,
            started_fishing: false,
            swapped_to_treasure: false,
            exit_tick: None,
            wait_for_treasure_chest: false,
        }
    }

    pub fn target_bobber_pos(
        self,
        target_bobber_pos: Option<Vector<f32>>,
    ) -> Self {
        Self {
            target_bobber_pos,
            ..self
        }
    }

    /// Where the mouse should be placed in order to cast the rod in
    /// the correct direction.  Used whenever `target_bobber_pos` is
    /// set.
    fn mouse_hover_tile(
        &self,
        game_state: &GameState,
    ) -> Option<Vector<isize>> {
        let target = self.target_bobber_pos?;

        let player = &game_state.player;
        let offset = target - player.center_pos();
        let facing = offset.closest_cardinal_direction();
        Some(player.tile() + facing.offset())
    }

    fn goal_casting_power(
        &self,
        game_state: &GameState,
    ) -> std::ops::Range<f32> {
        let ideal = if let Some(target) = self.target_bobber_pos {
            let player = &game_state.player;
            let offset = target - player.center_pos();
            let cast_dir = player.facing;
            let dist = cast_dir.offset().map(|i| i as f32).dot(offset);

            let added_distance = match player.skills.fishing_level() {
                0 => 0,
                1..4 => 1,
                4..8 => 2,
                8..15 => 3,
                15.. => 4,
            };

            let max_distance = match cast_dir {
                CardinalDirection::East | CardinalDirection::West => {
                    4 + added_distance
                }
                CardinalDirection::North | CardinalDirection::South => {
                    3 + added_distance
                }
            };

            let offset_dist = match cast_dir {
                CardinalDirection::East | CardinalDirection::West => 8.0 / 64.0,
                CardinalDirection::North | CardinalDirection::South => 0.0,
            };

            (dist + offset_dist) / (max_distance as f32)
        } else {
            1.0
        };

        let target = ideal.clamp(0.0, 1.0);
        target - 0.05..target + 0.05
    }

    fn bobber_steer_direction(
        &self,
        game_state: &GameState,
    ) -> Option<CardinalDirection> {
        let target = self.target_bobber_pos?;

        let landing_pos = game_state
            .fishing
            .bobber_landing_pos
            .map(|x| x / 64.0 - 0.5);

        let offset = target - landing_pos;
        let facing = game_state.player.facing.offset().map(|i| i as f32);
        let perp = offset - facing * offset.dot(facing);
        Some(perp.closest_cardinal_direction())
    }
}

impl BotGoal for FishingMinigameGoal {
    fn description(&self) -> Cow<'static, str> {
        "Fishing minigame".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        let fishing = &game_state.fishing;

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
            }
        }

        let cleanup = MenuCloser::new();
        if !cleanup.is_completed(game_state) {
            // Close any menus, such as a treasure chest that has already
            // been emptied out, or a first-geode popup that appeared from
            // a treasure chest.
            return Ok(cleanup.into());
        }

        if !fishing.is_holding_rod {
            // We can't start fishing unless a fishing rod has been
            // selected, which should be done by the parent goal.
            return Ok(BotGoalResult::Completed);
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
            || fishing.pulling_out_of_water
            || fishing.showing_treasure
        {
            // Do nothing
        } else if fishing.bobber_in_air {
            // Steer the bobber towards the target point, if a target
            // point has been defined.
            if let Some(dir) = self.bobber_steer_direction(game_state) {
                actions
                    .do_action(GameAction::Move(dir.into()))
                    .annotate("Steer bobber in flight");
            }
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
            let goal_casting_power = self.goal_casting_power(game_state);
            if goal_casting_power.contains(&fishing.casting_power) {
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
        } else if let Some(required_mouse_tile) =
            self.mouse_hover_tile(game_state)
            && game_state.inputs.mouse_tile_location != required_mouse_tile
        {
            actions.do_action(GameAction::MouseOverTile(required_mouse_tile));
        } else {
            // Time to start fishing.
            actions
                .do_action(GameAction::HoldTool)
                .annotate("Start fishing");
        }

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
