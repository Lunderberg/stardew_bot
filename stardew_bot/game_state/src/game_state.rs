use std::collections::HashMap;

use dotnet_debugger::CachedReader;
use dsl::{
    RustNativeObject, SymbolicGraph, SymbolicGraphCompile as _, VirtualMachine,
};

use crate::{Error, JunimoMenu, Menu};

use super::{
    define_utility_functions, rng_state::RngState, ChestMenu, DailyState,
    DialogueMenu, DisplayState, FishingState, GeodeMenu, GlobalGameState,
    InputState, Inventory, Location, LocationDelta, MailMenu, MineElevatorMenu,
    PauseMenu, PlayerState, SeededRng, ShopMenu, StaticState,
};

#[derive(RustNativeObject, Debug, Clone)]
pub struct GameState {
    pub statics: StaticState,
    pub globals: GlobalGameState,
    pub locations: Vec<Location>,
    pub player: PlayerState,
    pub fishing: FishingState,
    pub daily: DailyState,
    pub inputs: InputState,
    pub display: DisplayState,
    pub rng_state: RngState,

    pub menu: Option<Menu>,
}

#[derive(Debug)]
pub struct GameStateReader {
    vm: VirtualMachine,
}

#[derive(RustNativeObject, Debug, Clone)]
pub struct GameStateDelta {
    global_game_state: GlobalGameState,
    location_delta: LocationDelta,
    nonlocal_location_deltas: HashMap<String, LocationDelta>,
    player: PlayerState,
    fishing: FishingState,
    daily: DailyState,
    inputs: InputState,
    display: DisplayState,
    current_rng_state: SeededRng,

    num_mine_levels: usize,

    menu: Option<Menu>,
}

impl GameState {
    pub fn build_reader(
        reader: CachedReader,
    ) -> Result<GameStateReader, Error> {
        let mut graph = SymbolicGraph::new();

        define_utility_functions(&mut graph)?;
        StaticState::def_read_static_state(&mut graph)?;
        Inventory::def_read_inventory(&mut graph)?;
        GlobalGameState::def_read_global_game_state(&mut graph)?;
        Location::def_read_location(&mut graph)?;
        PlayerState::def_read_player(&mut graph)?;
        FishingState::def_read_fishing(&mut graph)?;
        DailyState::def_read_daily(&mut graph)?;
        InputState::def_read_input_state(&mut graph)?;
        DisplayState::def_read_display_state(&mut graph)?;
        SeededRng::def_read_rng_state(&mut graph)?;

        Menu::def_read_menu(&mut graph)?;

        graph.named_native_function(
            "new_game_state",
            |static_state: &StaticState,
             global_game_state: &GlobalGameState,
             locations: &Vec<Location>,
             player: &PlayerState,
             fishing: &FishingState,
             daily: &DailyState,
             inputs: &InputState,
             display: &DisplayState,
             current_rng: &SeededRng,
             menu: Option<&Menu>| {
                GameState {
                    statics: static_state.clone(),
                    globals: global_game_state.clone(),
                    locations: locations.clone(),
                    player: player.clone(),
                    fishing: fishing.clone(),
                    daily: daily.clone(),
                    inputs: inputs.clone(),
                    display: display.clone(),
                    rng_state: RngState::new(current_rng.clone()),

                    menu: menu.cloned(),
                }
            },
        )?;

        graph.named_native_function(
            "new_game_state_delta",
            |global_game_state: &GlobalGameState,
             location_delta: &LocationDelta,
             nonlocal_location_deltas: &Vec<LocationDelta>,
             player: &PlayerState,
             fishing: &FishingState,
             daily: &DailyState,
             inputs: &InputState,
             display: &DisplayState,
             rng_state: &SeededRng,
             num_mine_levels: usize,
             menu: Option<&Menu>| {
                let nonlocal_location_deltas = nonlocal_location_deltas
                    .iter()
                    .map(|delta| (delta.name.clone(), delta.clone()))
                    .collect();
                GameStateDelta {
                    global_game_state: global_game_state.clone(),
                    location_delta: location_delta.clone(),
                    nonlocal_location_deltas,
                    player: player.clone(),
                    fishing: fishing.clone(),
                    daily: daily.clone(),
                    inputs: inputs.clone(),
                    display: display.clone(),
                    current_rng_state: rng_state.clone(),

                    num_mine_levels,

                    menu: menu.cloned(),
                }
            },
        )?;

        graph.parse(stringify! {
            pub fn read_full_state() {
                let static_state = read_static_state();
                let global_game_state = read_global_game_state();

                let mineshaft_list = StardewValley
                    .Locations
                    .MineShaft
                    .activeMines;
                let num_mine_levels = mineshaft_list._size.prim_cast::<usize>();
                let iter_mine_levels = (0..num_mine_levels)
                    .map(|i| mineshaft_list._items[i])
                    .map(read_location);

                let locations = iter_locations(None)
                    .map(read_location)
                    .chain(iter_mine_levels)
                    .collect();

                let player = read_player();
                let fishing = read_fishing();
                let daily = read_daily();
                let inputs = read_input_state();
                let display = read_display_state();
                let rng_state = read_rng_state();

                let menu = read_menu();

                new_game_state(
                    static_state,
                    global_game_state,
                    locations,
                    player,
                    fishing,
                    daily,
                    inputs,
                    display,
                    rng_state,

                    menu,
                )
            }

            pub fn read_delta_state() {
                let global_game_state = read_global_game_state();
                let location_delta = read_location_delta();

                let nonlocal_location_deltas = iter_locations(
                        |i| {
                            let game_tick = StardewValley.Game1
                                .ticks
                                .prim_cast::<usize>();
                            let offset_tick = game_tick + i;
                            offset_tick % 240 == 0
                        }
                    )
                    .map(read_nonlocal_location_delta)
                    .collect();

                let player = read_player();
                let fishing = read_fishing();
                let daily = read_daily();
                let inputs = read_input_state();
                let display = read_display_state();
                let rng_state = read_rng_state();

                let num_mine_levels = StardewValley
                    .Locations
                    .MineShaft
                    .activeMines
                    ._size
                    .prim_cast::<usize>();

                let menu = read_menu();

                new_game_state_delta(
                    global_game_state,
                    location_delta,
                    nonlocal_location_deltas,
                    player,
                    fishing,
                    daily,
                    inputs,
                    display,
                    rng_state,

                    num_mine_levels,

                    menu,
                )
            }

            pub fn read_full_current_location() {
                let location = StardewValley.Game1
                    ._player
                    .currentLocationRef
                    ._gameLocation;

                read_location(location)
            }
        })?;

        let vm = graph.compile(reader)?;

        Ok(GameStateReader { vm })
    }

    pub fn requires_new_location_read(&self, delta: &GameStateDelta) -> bool {
        self.locations
            .iter()
            .all(|loc| loc.name != delta.location_delta.name)
    }

    pub fn apply_delta(&mut self, mut delta: GameStateDelta) {
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

        self.menu = delta.menu;

        if delta.num_mine_levels == 0 {
            self.locations.retain(|loc| loc.mineshaft_details.is_none());
        }

        if let Some(loc) = self
            .locations
            .iter_mut()
            .find(|loc| loc.name == delta.location_delta.name)
        {
            loc.apply_delta(delta.location_delta, player_pos);
        }

        for loc in &mut self.locations {
            if let Some(delta) =
                delta.nonlocal_location_deltas.remove(&loc.name)
            {
                loc.apply_delta(delta, player_pos);
            }
        }
    }

    pub fn update_location(&mut self, new_loc: Location) {
        for old_loc in &mut self.locations {
            if old_loc.name == new_loc.name {
                *old_loc = new_loc;
                return;
            }
        }

        self.locations.push(new_loc);
    }

    pub fn any_menu_open(&self) -> bool {
        self.menu.is_some()
    }

    pub fn get_room<'a>(&'a self, name: &str) -> Result<&'a Location, Error> {
        self.locations
            .iter()
            .find(|loc| loc.name == name)
            .ok_or_else(|| Error::UnknownRoom(name.into()))
    }

    pub fn current_room(&self) -> Result<&Location, Error> {
        self.get_room(&self.player.room_name)
    }

    pub fn pause_menu(&self) -> Option<&PauseMenu> {
        self.menu.as_ref().and_then(|menu| menu.pause_menu())
    }

    pub fn chest_menu(&self) -> Option<&ChestMenu> {
        self.menu.as_ref().and_then(|menu| menu.chest_menu())
    }

    pub fn dialogue_menu(&self) -> Option<&DialogueMenu> {
        self.menu.as_ref().and_then(|menu| menu.dialogue_menu())
    }

    pub fn mail_menu(&self) -> Option<&MailMenu> {
        self.menu.as_ref().and_then(|menu| menu.mail_menu())
    }

    pub fn shop_menu(&self) -> Option<&ShopMenu> {
        self.menu.as_ref().and_then(|menu| menu.shop_menu())
    }

    pub fn geode_menu(&self) -> Option<&GeodeMenu> {
        self.menu.as_ref().and_then(|menu| menu.geode_menu())
    }

    pub fn mine_elevator_menu(&self) -> Option<&MineElevatorMenu> {
        self.menu
            .as_ref()
            .and_then(|menu| menu.mine_elevator_menu())
    }

    pub fn junimo_menu(&self) -> Option<&JunimoMenu> {
        self.menu.as_ref().and_then(|menu| menu.junimo_menu())
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
        Location::add_minecart_warps(&mut state.locations);
        Location::fix_farm_warps(&mut state.locations);

        if state
            .locations
            .iter()
            .all(|loc| loc.name != state.player.room_name)
        {
            // The player is currently in a temporary location, which
            // is not stored in any of the usual locations.
            // Therefore, read the current location and add it to the
            // list of locations.
            let current_location = self.read_full_current_location(cache)?;
            state.update_location(current_location);
        }

        Ok(state)
    }

    pub fn read_delta_state(
        &self,
        cache: CachedReader,
    ) -> Result<GameStateDelta, Error> {
        self.vm
            .get_function("read_delta_state")?
            .with_reader(cache)
            .evaluate()?
            .take_obj(0)?
            .ok_or(Error::ExpectedNonEmptyValue)
    }

    pub fn read_full_current_location(
        &self,
        cache: CachedReader,
    ) -> Result<Location, Error> {
        self.vm
            .get_function("read_full_current_location")?
            .with_reader(cache)
            .evaluate()?
            .take_obj(0)?
            .ok_or(Error::ExpectedNonEmptyValue)
    }
}
