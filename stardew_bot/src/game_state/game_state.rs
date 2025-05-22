use dotnet_debugger::{
    CachedReader, RustNativeObject, SymbolicGraph, VirtualMachine,
};

use crate::{bot_logic::BotError, Error};

use super::{
    rng_state::RngState, ChestMenu, DailyState, DialogueMenu, DisplayState,
    FishingState, GlobalGameState, InputState, Inventory, Location,
    LocationDelta, PlayerState, SeededRng,
};

#[derive(RustNativeObject, Debug, Clone)]
pub struct GameState {
    pub global_game_state: GlobalGameState,
    pub locations: Vec<Location>,
    pub player: PlayerState,
    pub fishing: FishingState,
    pub daily: DailyState,
    pub inputs: InputState,
    pub display: DisplayState,
    pub chest_menu: Option<ChestMenu>,
    pub dialogue_menu: Option<DialogueMenu>,
    pub rng_state: RngState,
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
    chest_menu: Option<ChestMenu>,
    dialogue_menu: Option<DialogueMenu>,
    current_rng_state: SeededRng,
}

impl GameState {
    pub(crate) fn build_reader(
        reader: CachedReader,
    ) -> Result<GameStateReader, Error> {
        let mut graph = SymbolicGraph::new();

        GlobalGameState::def_read_global_game_state(&mut graph)?;
        Inventory::def_read_inventory(&mut graph)?;
        Location::def_read_location(&mut graph)?;
        PlayerState::def_read_player(&mut graph)?;
        FishingState::def_read_fishing(&mut graph)?;
        DailyState::def_read_daily(&mut graph)?;
        InputState::def_read_input_state(&mut graph)?;
        DisplayState::def_read_display_state(&mut graph)?;
        ChestMenu::def_read_chest_menu(&mut graph)?;
        DialogueMenu::def_read_dialogue_menu(&mut graph)?;
        SeededRng::def_read_rng_state(&mut graph)?;

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
             chest_menu: Option<&ChestMenu>,
             dialogue_menu: Option<&DialogueMenu>,
             current_rng: &SeededRng| GameState {
                global_game_state: global_game_state.clone(),
                locations: locations.clone(),
                player: player.clone(),
                fishing: fishing.clone(),
                daily: daily.clone(),
                inputs: inputs.clone(),
                display: display.clone(),
                chest_menu: chest_menu.cloned(),
                dialogue_menu: dialogue_menu.cloned(),
                rng_state: RngState::new(current_rng.clone()),
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
             chest_menu: Option<&ChestMenu>,
             dialogue_menu: Option<&DialogueMenu>,
             rng_state: &SeededRng| GameStateDelta {
                global_game_state: global_game_state.clone(),
                location_delta: location_delta.clone(),
                player: player.clone(),
                fishing: fishing.clone(),
                daily: daily.clone(),
                inputs: inputs.clone(),
                display: display.clone(),
                chest_menu: chest_menu.cloned(),
                dialogue_menu: dialogue_menu.cloned(),
                current_rng_state: rng_state.clone(),
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
                let chest_menu = read_chest_menu();
                let dialogue_menu = read_dialogue_menu();
                let rng_state = read_rng_state();

                new_game_state(
                    global_game_state,
                    locations,
                    player,
                    fishing,
                    daily,
                    inputs,
                    display,
                    chest_menu,
                    dialogue_menu,
                    rng_state,
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
                let chest_menu = read_chest_menu();
                let dialogue_menu = read_dialogue_menu();
                let rng_state = read_rng_state();

                new_game_state_delta(
                    global_game_state,
                    location_delta,
                    player,
                    fishing,
                    daily,
                    inputs,
                    display,
                    chest_menu,
                    dialogue_menu,
                    rng_state,
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
        self.global_game_state = delta.global_game_state;
        self.player = delta.player;
        self.fishing = delta.fishing;
        self.daily = delta.daily;
        self.inputs = delta.inputs;
        self.display = delta.display;
        self.chest_menu = delta.chest_menu;
        self.dialogue_menu = delta.dialogue_menu;

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
