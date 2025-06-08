use dotnet_debugger::{
    CachedReader, RustNativeObject, SymbolicGraph, VirtualMachine,
};

use crate::{bot_logic::BotError, Error};

use super::{
    define_utility_functions, rng_state::RngState, ChestMenu, DailyState,
    DialogueMenu, DisplayState, FishingState, GlobalGameState, InputState,
    Inventory, Location, LocationDelta, MailMenu, PauseMenu, PlayerState,
    SeededRng, ShopMenu,
};

#[derive(RustNativeObject, Debug, Clone)]
pub struct GameState {
    pub globals: GlobalGameState,
    pub locations: Vec<Location>,
    pub player: PlayerState,
    pub fishing: FishingState,
    pub daily: DailyState,
    pub inputs: InputState,
    pub display: DisplayState,
    pub rng_state: RngState,

    // Different menus that may be open
    pub chest_menu: Option<ChestMenu>,
    pub dialogue_menu: Option<DialogueMenu>,
    pub shop_menu: Option<ShopMenu>,
    pub pause_menu: Option<PauseMenu>,
    pub mail_menu: Option<MailMenu>,
}

#[derive(Debug)]
pub struct GameStateReader {
    vm: VirtualMachine,
}

#[derive(RustNativeObject, Debug, Clone)]
pub struct GameStateDelta {
    global_game_state: GlobalGameState,
    location_delta: LocationDelta,
    player: PlayerState,
    fishing: FishingState,
    daily: DailyState,
    inputs: InputState,
    display: DisplayState,
    current_rng_state: SeededRng,

    chest_menu: Option<ChestMenu>,
    dialogue_menu: Option<DialogueMenu>,
    shop_menu: Option<ShopMenu>,
    pause_menu: Option<PauseMenu>,
    mail_menu: Option<MailMenu>,
}

impl GameState {
    pub(crate) fn build_reader(
        reader: CachedReader,
    ) -> Result<GameStateReader, Error> {
        let mut graph = SymbolicGraph::new();

        define_utility_functions(&mut graph)?;
        GlobalGameState::def_read_global_game_state(&mut graph)?;
        Inventory::def_read_inventory(&mut graph)?;
        Location::def_read_location(&mut graph)?;
        PlayerState::def_read_player(&mut graph)?;
        FishingState::def_read_fishing(&mut graph)?;
        DailyState::def_read_daily(&mut graph)?;
        InputState::def_read_input_state(&mut graph)?;
        DisplayState::def_read_display_state(&mut graph)?;
        SeededRng::def_read_rng_state(&mut graph)?;

        ChestMenu::def_read_chest_menu(&mut graph)?;
        DialogueMenu::def_read_dialogue_menu(&mut graph)?;
        ShopMenu::def_read_shop_menu(&mut graph)?;
        PauseMenu::def_read_pause_menu(&mut graph)?;
        MailMenu::def_read_mail_menu(&mut graph)?;

        graph.parse(
            "let location_list = StardewValley
                 .Game1
                 .game1
                 ._locations
                 .as::<
                     System.Collections.ObjectModel
                     .Collection`1<StardewValley.GameLocation>
                 >()
                 .items
                 .as::<
                   System.Collections.Generic
                   .List`1<StardewValley.GameLocation>
                 >();",
        )?;

        graph.named_native_function(
            "new_game_state",
            |global_game_state: &GlobalGameState,
             locations: &Vec<Location>,
             player: &PlayerState,
             fishing: &FishingState,
             daily: &DailyState,
             inputs: &InputState,
             display: &DisplayState,
             current_rng: &SeededRng,
             chest_menu: Option<&ChestMenu>,
             dialogue_menu: Option<&DialogueMenu>,
             shop_menu: Option<&ShopMenu>,
             pause_menu: Option<&PauseMenu>,
             mail_menu: Option<&MailMenu>| GameState {
                globals: global_game_state.clone(),
                locations: locations.clone(),
                player: player.clone(),
                fishing: fishing.clone(),
                daily: daily.clone(),
                inputs: inputs.clone(),
                display: display.clone(),
                rng_state: RngState::new(current_rng.clone()),

                chest_menu: chest_menu.cloned(),
                dialogue_menu: dialogue_menu.cloned(),
                shop_menu: shop_menu.cloned(),
                pause_menu: pause_menu.cloned(),
                mail_menu: mail_menu.cloned(),
            },
        )?;

        graph.named_native_function(
            "new_game_state_delta",
            |global_game_state: &GlobalGameState,
             location_delta: &LocationDelta,
             player: &PlayerState,
             fishing: &FishingState,
             daily: &DailyState,
             inputs: &InputState,
             display: &DisplayState,
             rng_state: &SeededRng,
             chest_menu: Option<&ChestMenu>,
             dialogue_menu: Option<&DialogueMenu>,
             shop_menu: Option<&ShopMenu>,
             pause_menu: Option<&PauseMenu>,
             mail_menu: Option<&MailMenu>| GameStateDelta {
                global_game_state: global_game_state.clone(),
                location_delta: location_delta.clone(),
                player: player.clone(),
                fishing: fishing.clone(),
                daily: daily.clone(),
                inputs: inputs.clone(),
                display: display.clone(),
                current_rng_state: rng_state.clone(),

                chest_menu: chest_menu.cloned(),
                dialogue_menu: dialogue_menu.cloned(),
                shop_menu: shop_menu.cloned(),
                pause_menu: pause_menu.cloned(),
                mail_menu: mail_menu.cloned(),
            },
        )?;

        graph.parse(stringify! {
            pub fn read_full_state() {
                let global_game_state = read_global_game_state();

                let num_locations = location_list._size.prim_cast::<usize>();

                let locations = (0..num_locations)
                    .map(|i| location_list._items[i])
                    .map(read_location)
                    .collect();

                let player = read_player();
                let fishing = read_fishing();
                let daily = read_daily();
                let inputs = read_input_state();
                let display = read_display_state();
                let rng_state = read_rng_state();

                let chest_menu = read_chest_menu();
                let dialogue_menu = read_dialogue_menu();
                let shop_menu = read_shop_menu();
                let pause_menu = read_pause_menu();
                let mail_menu = read_mail_menu();

                new_game_state(
                    global_game_state,
                    locations,
                    player,
                    fishing,
                    daily,
                    inputs,
                    display,
                    rng_state,

                    chest_menu,
                    dialogue_menu,
                    shop_menu,
                    pause_menu,
                    mail_menu,
                )
            }

            pub fn read_delta_state() {
                let global_game_state = read_global_game_state();
                let location_delta = read_location_delta();
                let player = read_player();
                let fishing = read_fishing();
                let daily = read_daily();
                let inputs = read_input_state();
                let display = read_display_state();
                let rng_state = read_rng_state();

                let has_open_menu = StardewValley.Game1
                    ._activeClickableMenu
                    .is_some();
                let chest_menu = read_chest_menu();
                let dialogue_menu = read_dialogue_menu();
                let shop_menu = read_shop_menu();
                let pause_menu = read_pause_menu();
                let mail_menu = read_mail_menu();

                new_game_state_delta(
                    global_game_state,
                    location_delta,
                    player,
                    fishing,
                    daily,
                    inputs,
                    display,
                    rng_state,

                    chest_menu,
                    dialogue_menu,
                    shop_menu,
                    pause_menu,
                    mail_menu,
                )
            }
        })?;

        let vm = graph.compile(reader)?;

        Ok(GameStateReader { vm })
    }

    pub fn apply_delta(&mut self, delta: GameStateDelta) {
        let player_pos = delta.player.position;

        self.rng_state.apply_delta(
            delta.current_rng_state,
            delta.global_game_state.game_mode_tick,
        );
        self.globals = delta.global_game_state;
        self.player = delta.player;
        self.fishing = delta.fishing;
        self.daily = delta.daily;
        self.inputs = delta.inputs;
        self.display = delta.display;

        self.chest_menu = delta.chest_menu;
        self.dialogue_menu = delta.dialogue_menu;
        self.shop_menu = delta.shop_menu;
        self.pause_menu = delta.pause_menu;
        self.mail_menu = delta.mail_menu;

        if let Some(loc) = self
            .locations
            .iter_mut()
            .find(|loc| &loc.name == &delta.location_delta.name)
        {
            loc.apply_delta(delta.location_delta, player_pos);
        }
    }

    pub fn get_room<'a>(&'a self, name: &str) -> Result<&'a Location, Error> {
        self.locations
            .iter()
            .find(|loc| loc.name == name)
            .ok_or_else(|| BotError::UnknownRoom(name.into()))
            .map_err(Into::into)
    }

    pub fn current_room(&self) -> Result<&Location, Error> {
        self.get_room(&self.player.room_name)
    }
}

impl GameStateReader {
    pub fn read_full_state(
        &self,
        cache: CachedReader,
    ) -> Result<GameState, Error> {
        let mut state: GameState = self
            .vm
            .get_function("read_full_state")?
            .with_reader(cache)
            .evaluate()?
            .take_obj(0)?
            .ok_or(Error::ExpectedNonEmptyValue)?;

        Location::add_building_warps(&mut state.locations);

        Ok(state)
    }

    pub fn read_delta_state(
        &self,
        cache: CachedReader,
    ) -> Result<GameStateDelta, Error> {
        Ok(self
            .vm
            .get_function("read_delta_state")?
            .with_reader(cache)
            .evaluate()?
            .take_obj(0)?
            .ok_or(Error::ExpectedNonEmptyValue)?)
    }
}
