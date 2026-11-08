use game_state::{GameState, Item, ItemId, SeededRng};

pub struct GarbageCanPredictor {
    game_id: u32,
    days_played: u32,
    has_alleyway_buffet: bool,
    trash_cans_checked: u32,
    joja_became_movie_theater: bool,
    lowest_mine_level_reached: u32,
    finished_cc_vault: bool,
    has_furnace_recipe: bool,
}

/// Which garbage can is being inspected.  These are the
/// `Data/GarbageCans` entry in the data files.  However, that data
/// file only gets loaded on the first search of a trash can, so it
/// can't be reliably read out from memory.
///
/// It would be more reliable to get the path to the data files by
/// reading `StardewValley.Game1.content._CachedContentRoot`, then
/// parsing the data files myself.  However, that would require
/// parsing the XNB file format, which I'm putting off for now.
#[derive(Debug, Clone, Copy)]
pub enum GarbageCan {
    Blacksmith,
    EmilyAndHaley,
    Evelyn,
    JodiAndKent,
    JojaMart,
    Mayor,
    Museum,
    Saloon,
    DesertFestival,
}

#[derive(Debug)]
pub struct GarbageCanPrediction {
    /// The item found in the trash can.
    pub item: Item,

    /// If None, this roll doesn't depend on daily luck.  If
    /// `Some(_)`, this drop depends on the daily luck.  Required luck
    /// below the minumum -0.1 is a guaranteed drop.  Required luck
    /// above the maximum +0.1 will never occur.
    pub required_luck: Option<f32>,
}

impl GarbageCanPredictor {
    pub fn new(game_state: &GameState) -> Self {
        let game_id: u32 = game_state.globals.game_id as u32;
        let days_played: u32 =
            game_state.globals.get_stat("daysPlayed").unwrap_or(1);

        let has_alleyway_buffet =
            game_state.globals.get_stat("Book_Trash").unwrap_or(0) != 0;
        let trash_cans_checked =
            game_state.globals.get_stat("trashCansChecked").unwrap_or(0);

        let events = &game_state.globals.events_triggered;
        let joja_became_movie_theater = events.contains("ccMovieTheater")
            && !events.contains("ccMovieTheaterJoja");

        let finished_cc_vault = events.contains("ccVault");

        let lowest_mine_level_reached =
            game_state.globals.lowest_mine_level_reached as u32;

        // Not technically true, since there's no guarantee that the
        // furnace recipe will be obtained on day 2.  But it's what I
        // currently have the bot set up to do, and it means that I
        // don't need to unpack the recipe unlcks yet.
        let has_furnace_recipe = days_played > 1;

        Self {
            game_id,
            days_played,
            has_alleyway_buffet,
            trash_cans_checked,
            joja_became_movie_theater,
            lowest_mine_level_reached,
            finished_cc_vault,
            has_furnace_recipe,
        }
    }

    pub fn days_played(self, days_played: u32) -> Self {
        let has_furnace_recipe = days_played > 1;
        Self {
            days_played,
            has_furnace_recipe,
            ..self
        }
    }

    pub fn predict(&self, can: GarbageCan) -> Vec<GarbageCanPrediction> {
        let mut rng = SeededRng::from_stardew_seed([
            self.days_played as f64,
            (self.game_id / 2) as f64,
            (777 + can.hash()) as f64,
        ]);
        for _ in 0..2 {
            let prewarm = rng.rand_in_range(0..100);
            for _ in 0..prewarm {
                rng.rand_i32();
            }
        }

        // Initial RNG roll, which gets checked for most item drops.
        // This roll is made prior to any of the item-specific rolls,
        // and those item-specific rolls need to pass both the base
        // roll and the item-specific roll.
        //
        // For example, in order to get an ore drop from the
        // Blacksmith garbage can, need to pass both the base
        // `rand()+luck<0.2` roll, then a subsequent `rand()+luck<0.2`
        // roll for the ore itself.  (And only that first base roll is
        // impacted by Alleyway Buffet.)
        let daily_luck_to_pass_base = {
            let base_chance = 0.2;
            let base_chance =
                base_chance + if self.has_alleyway_buffet { 0.2 } else { 0.0 };
            rng.rand_float() - base_chance
        };

        let mut output = Vec::<GarbageCanPrediction>::new();

        // A couple of rolls for the Garbage Hat and the Trash
        // Catalogue.  Low-probability rolls, but they advance the RNG
        // state.
        if self.trash_cans_checked >= 20 && rng.rand_float() < 0.002 {
            output.push(GarbageCanPrediction {
                item: ItemId::new("(H)66").into(),
                required_luck: None,
            });
        }
        if self.trash_cans_checked >= 50 && rng.rand_float() < 0.002 {
            output.push(GarbageCanPrediction {
                item: ItemId::new("(F)TrashCatalogue").into(),
                required_luck: None,
            });
        }
        // TODO: Read the active special orders for Qi Beans.  There's
        // a 25% chance of dropping Qi beans, but only if that quest
        // is active.

        let synced_roll = |name: &str| -> f32 {
            let hash = SeededRng::hash(name.as_bytes());
            let mut synced_rng = SeededRng::from_stardew_seed([
                hash as f64,
                self.game_id as f64,
                self.days_played as f64,
            ]);
            synced_rng.rand_float()
        };

        let item_roll = |rng: &SeededRng, choices: &[ItemId]| -> Item {
            // Clone the RNG before determining what item will be
            // generated by the selected rule.  Since the
            // success/failure of any of the item-drop rules depends
            // on the daily luck, we need to trace out both branches,
            // with distinct RNG state in each branch.
            let mut rng = rng.clone();

            let num_choices = choices.len() as i32;
            let i = rng.rand_in_range(0..num_choices);
            let mut choice = choices[i as usize].clone();

            if choice == ItemId::new("RANDOM_BASE_SEASON_ITEM") {
                // This is a secondary choice, which occurs if the
                // first roll selected a season-specific group of
                // items.

                let season = (self.days_played - 1) / 28;

                let seasonal: [(&[&str], bool); 9] = [
                    (
                        &[
                            "(O)68", "(O)66", "(O)78", "(O)80", "(O)86",
                            "(O)152", "(O)167", "(O)153", "(O)420",
                        ],
                        true,
                    ),
                    (
                        &["(O)62", "(O)70", "(O)72", "(O)84", "(O)422"],
                        self.lowest_mine_level_reached > 40,
                    ),
                    (
                        &["(O)64", "(O)60", "(O)82"],
                        self.lowest_mine_level_reached > 80,
                    ),
                    (
                        &["(O)88", "(O)90", "(O)164", "(O)165"],
                        self.finished_cc_vault,
                    ),
                    (
                        &["(O)334", "(O)335", "(O)336", "(O)338"],
                        self.has_furnace_recipe,
                    ),
                    (
                        &[
                            "(O)16", "(O)18", "(O)20", "(O)22", "(O)129",
                            "(O)131", "(O)132", "(O)136", "(O)137", "(O)142",
                            "(O)143", "(O)145", "(O)147", "(O)148", "(O)152",
                            "(O)167", "(O)267",
                        ],
                        season == 0,
                    ),
                    (
                        &[
                            "(O)128", "(O)130", "(O)132", "(O)136", "(O)138",
                            "(O)142", "(O)144", "(O)145", "(O)146", "(O)149",
                            "(O)150", "(O)155", "(O)396", "(O)398", "(O)402",
                            "(O)267",
                        ],
                        season == 1,
                    ),
                    (
                        &[
                            "(O)404", "(O)406", "(O)408", "(O)410", "(O)129",
                            "(O)131", "(O)132", "(O)136", "(O)137", "(O)139",
                            "(O)140", "(O)142", "(O)143", "(O)148", "(O)150",
                            "(O)154", "(O)155", "(O)269",
                        ],
                        season == 2,
                    ),
                    (
                        &[
                            "(O)412", "(O)414", "(O)416", "(O)418", "(O)130",
                            "(O)131", "(O)132", "(O)136", "(O)140", "(O)141",
                            "(O)144", "(O)146", "(O)147", "(O)150", "(O)151",
                            "(O)154", "(O)269",
                        ],
                        season == 3,
                    ),
                ];

                let seasonal: Vec<ItemId> = seasonal
                    .into_iter()
                    .filter(|(_, condition)| *condition)
                    .flat_map(|(items, _)| items)
                    .map(|id| ItemId::new(*id))
                    .collect();

                let num_seasonal = seasonal.len() as i32;
                let i = rng.rand_in_range(0..num_seasonal);
                choice = seasonal[i as usize].clone();
            }

            choice.into()
        };

        match can {
            GarbageCan::Blacksmith => {
                let required_luck = rng.rand_float() - 0.2;
                let required_luck = required_luck.max(daily_luck_to_pass_base);

                let item = item_roll(
                    &rng,
                    &[ItemId::COPPER_ORE, ItemId::IRON_ORE, ItemId::COAL],
                );
                output.push(GarbageCanPrediction {
                    item,
                    required_luck: Some(required_luck),
                });
            }
            GarbageCan::EmilyAndHaley => {}
            GarbageCan::Evelyn => {
                let required_luck = rng.rand_float() - 0.2;
                let required_luck = required_luck.max(daily_luck_to_pass_base);
                output.push(GarbageCanPrediction {
                    item: ItemId::COOKIES.into(),
                    required_luck: Some(required_luck),
                });
            }
            GarbageCan::JodiAndKent => {}
            GarbageCan::JojaMart => {
                let joja_rng_roll = synced_roll("garbage_joja");
                if joja_rng_roll < 0.2 {
                    let item = if self.joja_became_movie_theater {
                        item_roll(
                            &rng,
                            &[
                                ItemId::new("(O)809"),
                                ItemId::new("(O)270"),
                                ItemId::new("(O)270"),
                                ItemId::new("(O)270"),
                            ],
                        )
                    } else {
                        ItemId::new("(O)167").into()
                    };
                    output.push(GarbageCanPrediction {
                        item,
                        required_luck: Some(daily_luck_to_pass_base),
                    });
                }
            }
            GarbageCan::Mayor => {}
            GarbageCan::Museum => {
                let geode_rng_roll = synced_roll("garbage_museum_535");
                let required_luck = geode_rng_roll - 0.2;
                let required_luck = required_luck.max(daily_luck_to_pass_base);

                let omnigeode_rng_roll = synced_roll("garbage_museum_749");
                let item = if omnigeode_rng_roll < 0.05 {
                    ItemId::OMNI_GEODE
                } else {
                    ItemId::GEODE
                };
                output.push(GarbageCanPrediction {
                    item: item.into(),
                    required_luck: Some(required_luck),
                });
            }
            GarbageCan::Saloon => {
                let dish_rng_roll = synced_roll("garbage_saloon_dish");
                let required_luck = dish_rng_roll - 0.2;
                let required_luck = required_luck.max(daily_luck_to_pass_base);
                output.push(GarbageCanPrediction {
                    item: ItemId::new("DISH_OF_THE_DAY").into(),
                    required_luck: Some(required_luck),
                });
            }
            GarbageCan::DesertFestival => {
                // Uses a RNG roll for the number of eggs, but I don't
                // currently get to the Desert Festival with the bot,
                // so that's not really important.
                let _num_eggs = rng.rand_in_range(5..9);
                output.push(GarbageCanPrediction {
                    item: ItemId::new("CalicoEgg").into(),
                    required_luck: None,
                });
            }
        }

        if self.trash_cans_checked >= 20 && rng.rand_float() < 0.01 {
            output.push(GarbageCanPrediction {
                item: item_roll(
                    &rng,
                    &[
                        ItemId::new("(O)153"),
                        ItemId::new("(O)216"),
                        ItemId::new("(O)403"),
                        ItemId::OAK_SEED,
                        ItemId::MAPLE_SEED,
                        ItemId::PINE_SEED,
                        ItemId::new("RANDOM_BASE_SEASON_ITEM"),
                    ],
                ),
                required_luck: None,
            });
        }

        output.push(GarbageCanPrediction {
            item: item_roll(
                &rng,
                &[
                    ItemId::new("(O)153"),
                    ItemId::new("(O)216"),
                    ItemId::new("(O)403"),
                    ItemId::OAK_SEED,
                    ItemId::MAPLE_SEED,
                    ItemId::PINE_SEED,
                    ItemId::new("RANDOM_BASE_SEASON_ITEM"),
                    ItemId::new("(O)168"),
                    ItemId::new("(O)167"),
                    ItemId::new("(O)170"),
                    ItemId::new("(O)171"),
                    ItemId::new("(O)172"),
                ],
            ),
            required_luck: Some(daily_luck_to_pass_base),
        });

        output
            .into_iter()
            .scan(0.1, |cum_min, prediction| {
                let required_luck =
                    prediction.required_luck.unwrap_or(-0.1).clamp(-0.1, 0.1);
                let required_luck = if required_luck >= *cum_min {
                    0.2
                } else {
                    required_luck
                };
                *cum_min = cum_min.min(required_luck);
                let required_luck =
                    (required_luck > -0.1).then(|| required_luck);

                Some(GarbageCanPrediction {
                    item: prediction.item,
                    required_luck,
                })
            })
            .filter(|prediction| match prediction.required_luck {
                Some(luck) => luck <= 0.1,
                None => true,
            })
            .collect()
    }
}

impl GarbageCan {
    /// Return the hash of the garbage can's name, as used for seeding
    /// the RNG predictor.
    fn hash(&self) -> i32 {
        let name = match self {
            GarbageCan::Blacksmith => "Blacksmith",
            GarbageCan::EmilyAndHaley => "EmilyAndHaley",
            GarbageCan::Evelyn => "Evelyn",
            GarbageCan::JodiAndKent => "JodiAndKent",
            GarbageCan::JojaMart => "JojaMart",
            GarbageCan::Mayor => "Mayor",
            GarbageCan::Museum => "Museum",
            GarbageCan::Saloon => "Saloon",
            GarbageCan::DesertFestival => "DesertFestival",
        };
        SeededRng::hash(name.as_bytes())
    }
}
