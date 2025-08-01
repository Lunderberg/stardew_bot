use geometry::Vector;

use crate::{Error, GameAction, MovementGoal};
use game_state::{GameState, ItemId};

use super::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult},
    ActivateTile, InventoryGoal,
};

/// Repair the beach bridge
///
/// Where most `BotGoal` implementations aim to have as little state
/// as possible, reading from the `GameState` whenever possible, this
/// goal tracks each step more closely.  This is because the default
/// updates to the `GameState` are insufficient to detect whether the
/// bridge has been repaired, and so the
/// `actions.refresh_current_location()` must be sent to trigger a
/// full re-read of the beach.  This must occur before the goal
/// returns, to ensure that the parent goal is working from an
/// up-to-date version of the `GameState`.
pub struct RepairBeachBridgeGoal {
    triggered_repair_dialogue: bool,
    confirmed_repair_dialogue: bool,
}

impl Default for RepairBeachBridgeGoal {
    fn default() -> Self {
        Self::new()
    }
}

impl RepairBeachBridgeGoal {
    pub fn new() -> Self {
        Self {
            triggered_repair_dialogue: false,
            confirmed_repair_dialogue: false,
        }
    }

    fn bridge_tile(
        &self,
        game_state: &GameState,
    ) -> Result<Option<Vector<isize>>, Error> {
        let opt_tile = game_state
            .get_room("Beach")?
            .action_tiles
            .iter()
            .find(|(_, action)| action == "BrokenBeachBridge")
            .map(|(tile, _)| *tile);
        Ok(opt_tile)
    }

    pub fn is_completed(&self, game_state: &GameState) -> Result<bool, Error> {
        let tile_exists = self.bridge_tile(game_state)?.is_some();
        let have_sufficient_wood = InventoryGoal::current()
            .with(ItemId::WOOD.with_count(300))
            .has_sufficient_stored(game_state)?;

        let can_repair = tile_exists && have_sufficient_wood;

        Ok(!can_repair)
    }
}

impl BotGoal for RepairBeachBridgeGoal {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        "Repair beach bridge".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        if game_state.dialogue_menu().is_some() {
            actions.do_action(GameAction::ConfirmMenu);
            if self.triggered_repair_dialogue {
                self.confirmed_repair_dialogue = true;
            }
            return Ok(BotGoalResult::InProgress);
        }

        let Some(bridge_tile) = self.bridge_tile(game_state)? else {
            // The bridge tile doesn't currently exist.  This means
            // that the bridge has already been repaired, and the game
            // state has been refreshed to recognize that the bridge
            // has been repaired.
            return Ok(BotGoalResult::Completed);
        };

        if self.confirmed_repair_dialogue {
            // We have confirmed the dialogue menu, but the bot thinks
            // that the repair-bridge tile still exists.  Force a full
            // refresh of the beach in order to get back in-sync.
            if game_state.player.room_name != "Beach" {
                return Err(Error::InconsistentBridgeRepairState);
            }
            actions.refresh_current_location();
            return Ok(BotGoalResult::InProgress);
        }

        let prepare =
            InventoryGoal::current().with(ItemId::WOOD.with_count(300));
        if !prepare.has_sufficient_stored(game_state)? {
            // Not enough wood collected to repair the bridge.  No
            // bridge repair is required.
            return Ok(BotGoalResult::Completed);
        }
        if !prepare.is_completed(game_state)? {
            // Enough wood is collected, but some may be in stored.
            // Get the wood out of storage so it can be used.
            return Ok(prepare.into());
        }

        let movement =
            MovementGoal::new("Beach", bridge_tile.into()).with_tolerance(1.0);
        if !movement.is_completed(game_state) {
            // Move to within range of the bridge tile.  This is done
            // explicitly, rather than as part of the `ActivateTile`
            // goal, to better track when the menu has been triggered.
            return Ok(movement.into());
        }

        // Actually repair the bridge.
        self.triggered_repair_dialogue = true;
        let activate = ActivateTile::new("Beach", bridge_tile);
        Ok(activate.into())
    }
}
