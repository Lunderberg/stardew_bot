use dotnet_debugger::{
    CachedReader, RustNativeObject, SymbolicGraph, VirtualMachine,
};

use crate::Error;

use super::{DailyState, FishingState, Location, PlayerState};

#[derive(RustNativeObject, Debug, Clone)]
pub struct GameState {
    pub locations: Vec<Location>,
    pub player: PlayerState,
    pub fishing: FishingState,
    pub daily: DailyState,
}

#[derive(Debug)]
pub struct GameStateReader {
    vm: VirtualMachine,
}

#[derive(RustNativeObject, Debug, Clone)]
pub struct GameStateDelta {
    player: PlayerState,
    fishing: FishingState,
    daily: DailyState,
}

impl GameState {
    pub(crate) fn build_reader(
        reader: CachedReader,
    ) -> Result<GameStateReader, Error> {
        let mut graph = SymbolicGraph::new();

        let read_location = Location::read_all(&mut graph)?;
        graph.name(read_location, "read_location")?;

        let read_player = PlayerState::read_all(&mut graph)?;
        graph.name(read_player, "read_player")?;

        let read_fishing = FishingState::read_all(&mut graph)?;
        graph.name(read_fishing, "read_fishing")?;

        let read_daily = DailyState::read_all(&mut graph)?;
        graph.name(read_daily, "read_daily")?;

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
             daily: &DailyState| GameState {
                locations: locations.clone(),
                player: player.clone(),
                fishing: fishing.clone(),
                daily: daily.clone(),
            },
        )?;

        graph.named_native_function(
            "new_game_state_delta",
            |player: &PlayerState,
             fishing: &FishingState,
             daily: &DailyState| GameStateDelta {
                player: player.clone(),
                fishing: fishing.clone(),
                daily: daily.clone(),
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

                new_game_state(locations, player, fishing, daily)
            }

            pub fn read_delta_state() {
                let player = read_player();
                let fishing = read_fishing();
                let daily = read_daily();

                new_game_state_delta(player, fishing, daily)
            }
        })?;

        let vm = graph.compile(reader)?;

        Ok(GameStateReader { vm })
    }

    pub fn apply_delta(&mut self, delta: GameStateDelta) {
        self.player = delta.player;
        self.fishing = delta.fishing;
        self.daily = delta.daily;
    }
}

impl GameStateReader {
    pub fn read_full_state(
        &self,
        cache: CachedReader,
    ) -> Result<GameState, Error> {
        Ok(self
            .vm
            .get_function("read_full_state")?
            .with_reader(cache)
            .evaluate()?
            .take_obj(0)?
            .ok_or(Error::ExpectedNonEmptyValue)?)
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
