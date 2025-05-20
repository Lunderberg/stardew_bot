use dotnet_debugger::{
    CachedReader, RustNativeObject, SymbolicGraph, VirtualMachine,
};

use crate::{bot_logic::BotError, Error};

use super::{
    ChestMenu, DailyState, DisplayState, FishingState, InputState, Inventory,
    Location, LocationDelta, PlayerState,
};

#[derive(RustNativeObject, Debug, Clone)]
pub struct GameState {
    pub locations: Vec<Location>,
    pub player: PlayerState,
    pub fishing: FishingState,
    pub daily: DailyState,
    pub inputs: InputState,
    pub display: DisplayState,
    pub chest_menu: Option<ChestMenu>,
}

#[derive(Debug)]
pub struct GameStateReader {
    vm: VirtualMachine,
}

#[derive(RustNativeObject, Debug, Clone)]
pub struct GameStateDelta {
    location_delta: LocationDelta,
    player: PlayerState,
    fishing: FishingState,
    daily: DailyState,
    inputs: InputState,
    display: DisplayState,
    chest_menu: Option<ChestMenu>,
}

impl GameState {
    pub(crate) fn build_reader(
        reader: CachedReader,
    ) -> Result<GameStateReader, Error> {
        let mut graph = SymbolicGraph::new();

        Inventory::def_read_inventory(&mut graph)?;
        Location::def_read_location(&mut graph)?;
        PlayerState::def_read_player(&mut graph)?;
        FishingState::def_read_fishing(&mut graph)?;
        DailyState::def_read_daily(&mut graph)?;
        InputState::def_read_input_state(&mut graph)?;
        DisplayState::def_read_display_state(&mut graph)?;
        ChestMenu::def_read_chest_menu(&mut graph)?;

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
            |locations: &Vec<Location>,
             player: &PlayerState,
             fishing: &FishingState,
             daily: &DailyState,
             inputs: &InputState,
             display: &DisplayState,
             chest_menu: Option<&ChestMenu>| GameState {
                locations: locations.clone(),
                player: player.clone(),
                fishing: fishing.clone(),
                daily: daily.clone(),
                inputs: inputs.clone(),
                display: display.clone(),
                chest_menu: chest_menu.cloned(),
            },
        )?;

        graph.named_native_function(
            "new_game_state_delta",
            |location_delta: &LocationDelta,
             player: &PlayerState,
             fishing: &FishingState,
             daily: &DailyState,
             inputs: &InputState,
             display: &DisplayState,
             chest_menu: Option<&ChestMenu>| GameStateDelta {
                location_delta: location_delta.clone(),
                player: player.clone(),
                fishing: fishing.clone(),
                daily: daily.clone(),
                inputs: inputs.clone(),
                display: display.clone(),
                chest_menu: chest_menu.cloned(),
            },
        )?;

        graph.parse(stringify! {
            pub fn read_full_state() {
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

                new_game_state(
                    locations,
                    player,
                    fishing,
                    daily,
                    inputs,
                    display,
                    chest_menu,
                )
            }

            pub fn read_delta_state() {
                let location_delta = read_location_delta();
                let player = read_player();
                let fishing = read_fishing();
                let daily = read_daily();
                let inputs = read_input_state();
                let display = read_display_state();
                let chest_menu = read_chest_menu();

                new_game_state_delta(
                    location_delta,
                    player,
                    fishing,
                    daily,
                    inputs,
                    display,
                    chest_menu,
                )
            }
        })?;

        let vm = graph.compile(reader)?;

        Ok(GameStateReader { vm })
    }

    pub fn apply_delta(&mut self, delta: GameStateDelta) {
        let player_pos = delta.player.position;

        self.player = delta.player;
        self.fishing = delta.fishing;
        self.daily = delta.daily;
        self.inputs = delta.inputs;
        self.display = delta.display;
        self.chest_menu = delta.chest_menu;
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
