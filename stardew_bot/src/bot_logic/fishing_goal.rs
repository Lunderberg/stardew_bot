use std::{borrow::Cow, fmt::Display};

use crate::{
    bot_logic::{InventoryGoal, MaintainStaminaGoal, SelectItemGoal},
    game_state::{FacingDirection, Item, Vector},
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
}

#[allow(dead_code)]
pub enum FishingLocation {
    River,
    Ocean,
    OceanByWilly,
    Lake,
}

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

        let opt_current_pole =
            game_state.player.inventory.iter_items().find(|item| {
                FISHING_POLES.iter().any(|pole| item.is_same_item(pole))
            });
        let Some(current_pole) = opt_current_pole else {
            let opt_get_pole_goal = FISHING_POLES
                .iter()
                .map(|pole| InventoryGoal::empty().with(pole.clone()))
                .find(|goal| goal.is_possible(game_state));
            if let Some(goal) = opt_get_pole_goal {
                return Ok(goal.into());
            }

            let goal = InventoryGoal::empty();
            if !goal.is_completed(game_state) {
                return Ok(goal.into());
            }

            let trigger_willy_cutscene = FishingLocation::Ocean
                .movement_goal()
                .with_tolerance(1000.0);
            return Ok(trigger_willy_cutscene.into());
        };

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

        let fish_once = FishOnceGoal::new();
        Ok(fish_once.into())
    }
}

impl FishOnceGoal {
    pub fn new() -> Self {
        Self {
            started_fishing: false,
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

        if self.started_fishing && !fishing.showing_fish {
            return Ok(BotGoalResult::Completed);
        }

        if fishing.is_timing_cast {
            if fishing.casting_power > 0.95 {
                do_action(GameAction::ReleaseTool)
            } else {
                do_action(GameAction::HoldTool)
            }
        } else if fishing.minigame_in_progress {
            if fishing.should_move_upward() {
                do_action(GameAction::HoldTool)
            } else {
                do_action(GameAction::ReleaseTool)
            }
        } else if fishing.showing_fish {
            self.started_fishing = true;
            do_action(GameAction::LeftClick);
        } else if fishing.is_nibbling {
            do_action(GameAction::ReleaseTool)
        } else if fishing.is_fishing {
            do_action(GameAction::HoldTool)
        } else if fishing.is_holding_rod {
            do_action(GameAction::HoldTool)
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
