use game_state::{
    CraftingMachine, GameState, Item, ItemCategory, ItemId, Location,
};
use geometry::Vector;

use crate::{
    ActivateTile, BuyFromMerchantGoal, CheckAllMail, Error, FishInBubbles,
    FishOnceGoal, GameStateExt as _, GoToRoomGoal, InventoryGoal,
    LoadUpFishingRod, LocationExt as _, MaintainStaminaGoal, MovementGoal,
    SellToMerchantGoal, UpgradeFishingRodGoal, UpgradeToolGoal, UseItemOnTile,
    WaterCropsGoal,
    bot_logic::{ActionCollector, BotGoal, BotGoalResult},
    tdiff,
};

pub struct SecondDay;

pub struct PickUpMachine {
    room: String,
    tile: Vector<isize>,
}

impl BotGoal for SecondDay {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        "Second Day".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        _actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        let farm = game_state.get_room("Farm")?;
        let player = &game_state.player;
        let current_day =
            game_state.globals.get_stat("daysPlayed").unwrap_or(1);
        let in_game_time = game_state.globals.in_game_time;
        if current_day != 2 {
            return Ok(BotGoalResult::Completed);
        }

        let goal = CheckAllMail::new();
        if !goal.is_completed(game_state) {
            return Ok(goal.into());
        }

        let goal = MaintainStaminaGoal::new();
        if !goal.is_completed(game_state)? {
            return Ok(goal.into());
        }

        if player.room_name == "Farm" && in_game_time < 800 {
            let opt_furnace = farm.objects.iter().find_map(|obj| {
                obj.kind.as_furnace().map(|furnace| (obj.tile, furnace))
            });

            let num_copper_bars =
                player.inventory.count_item(ItemId::COPPER_BAR);
            let num_copper_ore =
                player.inventory.count_item(ItemId::COPPER_ORE);
            let furnace_pos = game_state.get_farm_door()? + Vector::new(1, 3);
            if num_copper_bars < 1 {
                if let Some((tile, crafter)) = opt_furnace {
                    if crafter.held_item.is_none() && num_copper_ore == 25 {
                        return Ok(UseItemOnTile::new(
                            ItemId::COPPER_ORE,
                            "Farm",
                            tile,
                        )
                        .into());
                    } else if crafter.ready_to_harvest {
                        return Ok(ActivateTile::new("Farm", tile).into());
                    }
                } else {
                    let craft = InventoryGoal::current()
                        .craft_missing(true)
                        .with_exactly(ItemId::FURNACE);
                    if !craft.is_completed(game_state)? {
                        return Ok(craft.into());
                    }

                    let place_down = UseItemOnTile::new(
                        ItemId::FURNACE,
                        "Farm",
                        furnace_pos,
                    );
                    return Ok(place_down.into());
                }
            } else {
                let pickup = PickUpMachine::new("Farm", furnace_pos);
                if !pickup.is_completed(game_state)? {
                    return Ok(pickup.into());
                }
            }

            let goal = WaterCropsGoal::new();
            if !goal.is_completed(game_state)? {
                return Ok(goal.into());
            }

            let preparation = InventoryGoal::empty()
                .stamina_recovery_slots(2)
                .craft_missing(true)
                .with_exactly([
                    ItemId::OAK_CHAIR.with_count(1),
                    ItemId::FURNACE.with_count(1),
                    ItemId::COPPER_BAR.with_count(1),
                    ItemId::COPPER_ORE.with_count(20),
                    ItemId::COAL.with_count(4),
                    ItemId::PICKAXE.with_count(1),
                ]);
            if !preparation.is_completed(game_state)? {
                return Ok(preparation.into());
            }
        }

        let upgrade = UpgradeFishingRodGoal::new();
        if !upgrade.is_completed(game_state)? {
            if let Some(pickup) = PickUpMachine::near_player(game_state)? {
                return Ok(pickup.into());
            }
            return Ok(upgrade.into());
        }

        let opt_current_pole = player
            .inventory
            .iter_items()
            .filter(|item| item.as_fishing_rod().is_some())
            .max_by_key(|item| {
                item.as_fishing_rod().unwrap().num_attachment_slots
            });
        let Some(current_pole) = opt_current_pole else {
            return Ok(BotGoalResult::Completed);
        };

        let load_bait = LoadUpFishingRod::new().bait(ItemId::BAIT);
        if !load_bait.is_completed(game_state) {
            return Ok(load_bait.into());
        }

        if player.room_name == "FishShop" {
            let sell_fish = SellToMerchantGoal::new("Buy Fish").sell_all(
                player
                    .inventory
                    .iter_items()
                    .filter(|item| {
                        matches!(item.category, Some(ItemCategory::Fish))
                    })
                    .cloned(),
            );
            if !sell_fish.is_completed(game_state) {
                return Ok(sell_fish.into());
            }

            let bait_to_buy = (player.current_money - 2000).max(0) / 5;
            let buy_bait = BuyFromMerchantGoal::new(
                "Buy Fish",
                ItemId::BAIT.with_count(bait_to_buy as usize),
            );
            if !buy_bait.is_completed(game_state)? {
                return Ok(buy_bait.into());
            }

            let leave_room = GoToRoomGoal::new("Beach");
            if !leave_room.is_completed(game_state) {
                return Ok(leave_room.into());
            }
        }

        let Some(bubbles) = FishInBubbles::choose_bubbles(
            game_state,
            ["Beach", "Mountain", "Forest", "Town"],
        )?
        else {
            return Ok(BotGoalResult::InProgress);
        };
        let wait_time = tdiff(
            bubbles.bubble_duration.start,
            in_game_time.min(bubbles.bubble_duration.start),
        );

        let loc = game_state.get_room(&bubbles.bubble_room)?;

        if current_pole.id == ItemId::FIBERGLASS_ROD
            && bubbles.bubble_room != player.room_name
        {
            let upgrade = UpgradeToolGoal::new(ItemId::PICKAXE);
            if !upgrade.is_completed(game_state)? {
                if let Some(pickup) = PickUpMachine::near_player(game_state)? {
                    return Ok(pickup.into());
                }
                return Ok(upgrade.into());
            }
        }

        if wait_time > 20 && player.inventory.num_empty_slots() < 5
            || wait_time > 60
        {
            let store_excess = InventoryGoal::empty()
                .stamina_recovery_slots(2)
                .with_exactly([
                    ItemId::OAK_CHAIR,
                    ItemId::BAMBOO_POLE,
                    ItemId::FIBERGLASS_ROD,
                ]);
            if !store_excess.is_completed(game_state)? {
                return Ok(store_excess.into());
            }
        }

        let movement =
            MovementGoal::new(loc.name.clone(), bubbles.player_tile.into())
                .with_tolerance(0.5);
        if !movement.is_completed(game_state) {
            if let Some(pickup) = PickUpMachine::near_player(game_state)? {
                return Ok(pickup.into());
            }
            return Ok(movement.into());
        }

        let should_smelt = player.inventory.contains(ItemId::PICKAXE)
            && player.inventory.count_item(ItemId::COPPER_BAR) < 5;
        let opt_furnace = loc
            .objects
            .iter()
            .filter(|obj| obj.tile.chessboard_dist(bubbles.player_tile) == 1)
            .find_map(|obj| {
                obj.kind.as_furnace().map(|furnace| (obj.tile, furnace))
            });
        if should_smelt {
            if let Some((tile, furnace)) = opt_furnace {
                if furnace.ready_to_harvest {
                    return Ok(ActivateTile::new(loc.name.clone(), tile).into());
                } else if furnace.held_item.is_none() {
                    return Ok(UseItemOnTile::new(
                        ItemId::COPPER_ORE,
                        loc.name.clone(),
                        tile,
                    )
                    .into());
                }
            } else {
                let clear_tiles = loc
                    .pathfinding(&game_state.statics)
                    .reachable(bubbles.bubble_tile);
                let opt_available_tile = bubbles
                    .player_tile
                    .iter_adjacent()
                    .find(|tile| clear_tiles.is_set(*tile));
                if let Some(available_tile) = opt_available_tile {
                    return Ok(UseItemOnTile::new(
                        ItemId::FURNACE,
                        loc.name.clone(),
                        available_tile,
                    )
                    .into());
                }
            }
        } else if let Some((tile, _)) = opt_furnace {
            return Ok(PickUpMachine::new(loc.name.clone(), tile).into());
        }

        let fish_once =
            FishOnceGoal::new().target_bobber_pos(bubbles.target_bobber);
        return Ok(fish_once.into());
    }
}

impl PickUpMachine {
    pub fn new(room: impl Into<String>, tile: Vector<isize>) -> Self {
        Self {
            room: room.into(),
            tile,
        }
    }

    pub fn near_player(game_state: &GameState) -> Result<Option<Self>, Error> {
        let loc = game_state.current_room()?;
        Ok(loc
            .objects
            .iter()
            .find(|obj| obj.kind.as_crafting_machine().is_some())
            .map(|obj| Self::new(loc.name.clone(), obj.tile)))
    }

    pub fn is_completed(&self, game_state: &GameState) -> Result<bool, Error> {
        let loc = game_state.get_room(&self.room)?;
        let can_cleanup = (self.find_machine(loc).is_some()
            && self.choose_tool(game_state).is_some())
            || self.find_pickup(loc).is_some();
        Ok(!can_cleanup)
    }

    fn find_machine<'a>(
        &self,
        loc: &'a Location,
    ) -> Option<&'a CraftingMachine> {
        loc.objects
            .iter()
            .find(|obj| obj.tile == self.tile)
            .and_then(|obj| obj.kind.as_crafting_machine())
            .filter(|machine| {
                machine.ready_to_harvest || machine.held_item.is_none()
            })
    }

    fn choose_tool<'a>(&self, game_state: &'a GameState) -> Option<&'a Item> {
        let player = &game_state.player;
        player
            .inventory
            .iter_filled_slots()
            .filter(|(_, item)| {
                std::iter::empty()
                    .chain(ItemId::AXES)
                    .chain(ItemId::PICKAXES)
                    .chain(ItemId::HOES)
                    .any(|tool| item.id == tool)
            })
            .max_by_key(|(slot, _)| *slot == player.active_hotbar_index)
            .map(|(_, item)| item)
    }

    fn find_pickup(&self, loc: &Location) -> Option<Vector<f32>> {
        loc.items
            .iter()
            .filter(|floating_item| {
                floating_item.id == ItemId::FURNACE
                    || floating_item.id == ItemId::BAIT_MAKER
            })
            .map(|floating_item| floating_item.position / 64.0)
            .filter(|pos| pos.dist(self.tile.into()) < 2.0)
            .min_by(|a, b| {
                let tile = self.tile.into();
                a.dist2(tile).total_cmp(&b.dist2(tile))
            })
    }
}

impl BotGoal for PickUpMachine {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        format!("Pick up machine from {} on {}", self.tile, self.room).into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        _actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        let loc = game_state.get_room(&self.room)?;

        if let Some(_) = self.find_machine(loc)
            && let Some(tool) = self.choose_tool(game_state)
        {
            return Ok(UseItemOnTile::new(
                tool.clone(),
                self.room.clone(),
                self.tile,
            )
            .into());
        }

        if let Some(pickup) = self.find_pickup(loc) {
            let movement = MovementGoal::new(self.room.clone(), pickup);
            if movement.is_completed(game_state) {
                return Ok(BotGoalResult::InProgress);
            } else {
                let num_items = loc.items.len();
                let room_name = loc.name.clone();
                return Ok(movement
                    .cancel_if(move |game_state| {
                        game_state
                            .get_room(&room_name)
                            .map(|loc| loc.items.len() != num_items)
                            .unwrap_or(true)
                    })
                    .into());
            }
        }

        Ok(BotGoalResult::Completed)
    }
}
