use crate::{
    bot_logic::{BotError, GoToActionTile, InventoryGoal, MenuCloser},
    game_state::{Item, ItemCategory, ItemId, SeededRng, StaticState},
    Error, GameAction, GameState,
};

use super::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult, LogicStack},
    SellToMerchantGoal,
};

pub struct GeodeCrackingGoal {
    sell_minerals: bool,
    sell_gems: bool,
}

pub struct GeodePredictor {
    geodes_cracked: u32,
    game_id: u64,
    multiplayer_id: i64,
    lowest_mine_level_reached: i32,
}

impl GeodeCrackingGoal {
    const GEODE_TYPES: [Item; 4] = [
        Item::GEODE,
        Item::FROZEN_GEODE,
        Item::MAGMA_GEODE,
        Item::OMNI_GEODE,
    ];

    pub fn new() -> Self {
        Self {
            sell_minerals: false,
            sell_gems: false,
        }
    }

    pub fn sell_minerals(self, sell_minerals: bool) -> Self {
        Self {
            sell_minerals,
            ..self
        }
    }

    pub fn sell_gems(self, sell_gems: bool) -> Self {
        Self { sell_gems, ..self }
    }

    #[allow(dead_code)]
    pub fn is_completed(&self, game_state: &GameState) -> Result<bool, Error> {
        let mut available =
            InventoryGoal::current().total_stored_and_carried(game_state)?;
        if let Some(held_item) = Self::held_item(game_state) {
            *available.entry(&held_item.id).or_default() += held_item.count;
        }

        let has_any_geode = Self::GEODE_TYPES
            .iter()
            .any(|geode| available.contains_key(&geode.id));

        Ok(!has_any_geode)
    }

    fn held_item(game_state: &GameState) -> Option<&Item> {
        if let Some(menu) = &game_state.geode_menu {
            if let Some(held_item) = &menu.held_item {
                return Some(held_item);
            }
        }
        None
    }

    fn next_geode(
        &self,
        game_state: &GameState,
    ) -> Result<Option<ItemId>, Error> {
        let mut carried = game_state.player.inventory.to_hash_map();
        if let Some(held_item) = Self::held_item(game_state) {
            *carried.entry(held_item.id.clone()).or_default() +=
                held_item.count;
        }

        let carried: [_; 4] = std::array::from_fn(|i| {
            let kind = Self::GEODE_TYPES[i].id.clone();
            let count = carried.get(&kind).cloned().unwrap_or(0);
            (kind, count)
        });

        let predictor = GeodePredictor::new(game_state)?;
        let opt_next = carried
            .into_iter()
            .filter(|(_, count)| *count > 0)
            .max_by_key(|(id, count)| {
                // TODO: Heuristic based on the predicted output.
                let predicted = predictor.predict(&game_state.statics, id);
                *count
            })
            .map(|(id, _)| id);

        Ok(opt_next)
    }
}

impl BotGoal for GeodeCrackingGoal {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        "Crack geodes".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        let inventory = &game_state.player.inventory;

        let opt_next_geode = self.next_geode(game_state)?;

        let to_sell: LogicStack = inventory
            .iter_items()
            .filter(|_| game_state.player.room_name == "Blacksmith")
            .filter(|item| {
                item.category
                    .map(|category| match category {
                        ItemCategory::Gem => self.sell_gems,
                        ItemCategory::Mineral => self.sell_minerals,
                        _ => false,
                    })
                    .unwrap_or(false)
            })
            .map(|item| SellToMerchantGoal::new("Blacksmith", item.clone()))
            .collect();

        let has_full_inventory = inventory.num_empty_slots().saturating_sub(
            if Self::held_item(game_state).is_some() {
                1
            } else {
                0
            },
        ) == 0;

        if opt_next_geode.is_none() || has_full_inventory {
            if game_state.geode_menu.is_some() {
                return Ok(MenuCloser::new().into());
            } else if to_sell.len() > 0 {
                return Ok(to_sell.into());
            }
        }

        let Some(next_geode) = opt_next_geode else {
            return Ok(BotGoalResult::Completed);
        };

        let prepare = Self::GEODE_TYPES
            .iter()
            .fold(InventoryGoal::current().with(Item::HOE), |goal, geode| {
                goal.with(geode.clone().with_count(1000))
            });
        if !prepare.is_completed(game_state)? {
            return Ok(prepare.otherwise_empty().into());
        }

        if let Some(menu) = &game_state.dialogue_menu {
            if menu.responses.len() < 3 {
                // Not the blacksmith menu, so close it and proceed.
                return Ok(MenuCloser::new().into());
            }

            actions.do_action(GameAction::MouseOverPixel(menu.responses[2]));
            actions.do_action(GameAction::LeftClick);
            return Ok(BotGoalResult::InProgress);
        }

        let Some(menu) = &game_state.geode_menu else {
            return Ok(GoToActionTile::new("Blacksmith").into());
        };

        if menu.is_cracking_geode {
            return Ok(BotGoalResult::InProgress);
        }

        let (pixel, click) = if let Some(held_item) = &menu.held_item {
            if held_item.is_same_item(next_geode) {
                // The cursor is holding the correct geode, so drop it onto
                // Clint's anvil.
                (menu.crack_geode_button, GameAction::LeftClick)
            } else {
                // The cursor is holding an item, but it's not the
                // geode we want to open.
                let empty_slot = inventory
                    .empty_slot()
                    .ok_or(BotError::ExpectedEmptySlot)?;
                (
                    menu.player_item_locations[empty_slot],
                    GameAction::LeftClick,
                )
            }
        } else {
            // The cursor is not holding an item, so pick one up.
            let next_slot = inventory
                .current_slot(&next_geode)
                .ok_or_else(|| BotError::ExpectedItemInInventory(next_geode))?;
            (
                menu.player_item_locations[next_slot],
                GameAction::RightClick,
            )
        };

        actions.do_action(GameAction::MouseOverPixel(pixel));
        actions.do_action(click);
        Ok(BotGoalResult::InProgress)
    }
}

impl GeodePredictor {
    pub fn new(game_state: &GameState) -> Result<Self, Error> {
        let geodes_cracked =
            game_state.globals.get_stat("geodesCracked").unwrap_or(0);
        // The `Utility.getTreasureFromGeode` function uses the
        // current number of geodes cracked to seed the RNG.  However,
        // the `GeodeMenu.update` menu increments the number of geodes
        // cracked before calling `Utility.getTreasureFromGeode`.
        // Therefore, we must increment the count in order to match
        // the value used by `getTreasureFromGeode`.
        let geodes_cracked = geodes_cracked + 1;

        let game_id = game_state.globals.game_id;
        let multiplayer_id = game_state.globals.multiplayer_id;
        let lowest_mine_level_reached =
            game_state.globals.lowest_mine_level_reached;

        Ok(Self {
            geodes_cracked,
            game_id,
            multiplayer_id,
            lowest_mine_level_reached,
        })
    }

    fn generate_rng(&self) -> SeededRng {
        let mut rng = SeededRng::from_stardew_seed([
            self.geodes_cracked as f64,
            (self.game_id / 2) as f64,
            ((self.multiplayer_id as i32) / 2) as f64,
        ]);

        for _ in 0..2 {
            let num = rng.rand_in_range(1..10);
            for _ in 0..num {
                rng.rand_i32();
            }
        }

        rng
    }

    pub fn predict(&self, statics: &StaticState, geode: &ItemId) -> Item {
        let geode_data = if geode == &Item::GEODE {
            &statics.geode
        } else if geode == &Item::FROZEN_GEODE {
            &statics.frozen_geode
        } else if geode == &Item::MAGMA_GEODE {
            &statics.magma_geode
        } else if geode == &Item::OMNI_GEODE {
            &statics.omni_geode
        } else {
            panic!("Could not find geode data for {geode}")
        };

        let mut rng = self.generate_rng();

        // Skip one roll used to check for Qi Beans.  (Roll occurs
        // regardless of whether Qi Beans can currently drop.)
        rng.rand_i32();

        if !geode_data.drops_default_items || rng.rand_bool() {
            for drop in &geode_data.drops {
                if rng.rand_weighted_bool(drop.chance as f32) {
                    let num_options = drop.item_list.len() as i32;
                    let index = rng.rand_in_range(0..num_options) as usize;
                    return drop.item_list[index].clone().into();
                }
            }
        }

        let mut amount = rng.rand_in_range(0..3) * 2 + 1;
        if rng.rand_float() < 0.1 {
            amount = 10;
        }
        if rng.rand_float() < 0.01 {
            amount = 20;
        }

        let output_id = if rng.rand_bool() {
            match rng.rand_in_range(0..4) {
                0 | 1 => "(O)390",
                2 => {
                    amount = 1;
                    "(O)330"
                }
                _ => {
                    // Generate an Earth Crystal, Frozen Tear, or Fire Quartz
                    amount = 1;
                    if geode == &Item::GEODE {
                        "(O)86"
                    } else if geode == &Item::FROZEN_GEODE {
                        "(O)84"
                    } else if geode == &Item::MAGMA_GEODE {
                        "(O)82"
                    } else if geode == &Item::OMNI_GEODE {
                        match rng.rand_in_range(0..3) {
                            0 => "(O)82",
                            1 => "(O)84",
                            _ => "(O)86",
                        }
                    } else {
                        unreachable!("Item id {geode} was not a geode")
                    }
                }
            }
        } else {
            if geode == &Item::GEODE {
                match rng.rand_in_range(0..3) {
                    0 => "(O)378",
                    1 if self.lowest_mine_level_reached > 25 => "(O)380",
                    1 => "(O)378",
                    _ => "(O)382",
                }
            } else if geode == &Item::FROZEN_GEODE {
                match rng.rand_in_range(0..4) {
                    0 => "(O)378",
                    1 => "(O)380",
                    2 => "(O)382",
                    _ if self.lowest_mine_level_reached > 75 => "(O)384",
                    _ => "(O)380",
                }
            } else if geode == &Item::MAGMA_GEODE || geode == &Item::OMNI_GEODE
            {
                match rng.rand_in_range(0..5) {
                    0 => "(O)378",
                    1 => "(O)380",
                    2 => "(O)382",
                    3 => "(O)384",
                    _ => {
                        amount = amount / 2 + 1;
                        "(O)386"
                    }
                }
            } else {
                unreachable!("Item id {geode} was not a geode")
            }
        };

        let item: Item = ItemId::new(output_id).into();
        item.with_count(amount as usize)
    }
}
