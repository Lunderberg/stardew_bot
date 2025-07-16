use game_state::{Friendship, GameState, ItemId};

use crate::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult, LogicStack},
    Error, GameAction, MenuCloser, MovementGoal, SelectItemGoal,
};

pub struct GiveGiftGoal {
    recipient: String,
    gift: ItemId,
}

impl GiveGiftGoal {
    pub fn new(recipient: impl Into<String>, gift: ItemId) -> Self {
        let recipient = recipient.into();
        Self { recipient, gift }
    }

    fn get_friendship<'a>(
        &self,
        game_state: &'a GameState,
    ) -> Option<&'a Friendship> {
        // Friendship entries are only present if the player met the
        // NPC.  If there is no entry, then we haven't met the NPC,
        // and they can therefore be talked to and given a gift.
        game_state
            .player
            .friendships
            .iter()
            .find(|friendship| friendship.name == self.recipient)
    }

    fn can_give_gift(&self, game_state: &GameState) -> bool {
        self.get_friendship(game_state)
            .map(|friendship| {
                friendship.gifts_this_week < 2 && !friendship.gifted_today
            })
            .unwrap_or(true)
    }

    fn can_talk(&self, game_state: &GameState) -> bool {
        self.get_friendship(game_state)
            .map(|friendship| !friendship.talked_today)
            .unwrap_or(true)
    }

    pub fn is_completed(&self, game_state: &GameState) -> Result<bool, Error> {
        let can_give_gift = self.can_give_gift(game_state);
        let has_gift = game_state.player.inventory.contains(&self.gift);

        // TODO: Check if the NPC is in an accessible location.

        let giftable = has_gift && can_give_gift;
        Ok(!giftable)
    }
}

impl BotGoal for GiveGiftGoal {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        format!("Give {} to {}", self.gift, self.recipient).into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        let cleanup = MenuCloser::new();
        if !cleanup.is_completed(game_state) {
            return Ok(cleanup.into());
        }

        let (room, tile) = game_state
            .locations
            .iter()
            .filter(|loc| loc.mineshaft_details.is_none())
            .flat_map(|loc| {
                loc.characters.iter().map(move |character| (loc, character))
            })
            .find(|(_, character)| {
                // character.health.is_none() &&
                character.name == self.recipient
            })
            .map(|(loc, character)| (&loc.name, character.tile()))
            .ok_or_else(|| Error::VillagerNotFound(self.recipient.clone()))?;

        if room != &game_state.player.room_name
            || game_state.player.tile().manhattan_dist(tile) > 1
        {
            let game_tick = game_state.globals.game_tick;
            let stack = LogicStack::new()
                .then(MovementGoal::new(room, tile.into()).with_tolerance(1.0))
                .cancel_if(move |game_state| {
                    game_state.globals.game_tick > game_tick + 60
                });
            return Ok(stack.into());
        }

        actions.do_action(GameAction::MouseOverTile(tile));

        let opt_to_select = self
            .can_talk(game_state)
            .then(|| {
                game_state
                    .player
                    .inventory
                    .iter_items()
                    .map(|item| &item.id)
                    .find(|id| id.item_id.starts_with("(T)"))
            })
            .flatten()
            .or_else(|| self.can_give_gift(game_state).then(|| &self.gift));

        let Some(to_select) = opt_to_select else {
            return Ok(BotGoalResult::Completed);
        };

        let select = SelectItemGoal::new(to_select.clone());
        if !select.is_completed(game_state) {
            return Ok(select.into());
        }

        actions.do_action(GameAction::RightClick);
        Ok(BotGoalResult::InProgress)
    }
}
