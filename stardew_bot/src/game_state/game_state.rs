use dotnet_debugger::{
    CachedReader, RustNativeObject, SymbolicGraph, VirtualMachine,
};

use crate::Error;

use super::{Location, PlayerState};

#[derive(RustNativeObject, Debug, Clone)]
pub struct GameState {
    pub player: PlayerState,
    pub locations: Vec<Location>,
}

#[derive(Debug)]
pub struct GameStateReader {
    vm: VirtualMachine,
}

#[derive(RustNativeObject, Debug, Clone)]
pub struct GameStateDelta {
    player: PlayerState,
}

impl GameState {
    pub(crate) fn build_reader(
        reader: CachedReader,
    ) -> Result<GameStateReader, Error> {
        let mut graph = SymbolicGraph::new();

        let read_player = PlayerState::read_all(&mut graph)?;
        graph.name(read_player, "read_player")?;

        let read_location = Location::read_all(&mut graph)?;
        graph.name(read_location, "read_location")?;

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
            |player: &PlayerState, locations: &Vec<Location>| GameState {
                player: player.clone(),
                locations: locations.clone(),
            },
        )?;

        graph.named_native_function(
            "new_game_state_delta",
            |player: &PlayerState| GameStateDelta {
                player: player.clone(),
            },
        )?;

        graph.parse(stringify! {
            pub fn read_full_state() {
                let player = read_player();

                let num_locations = location_list._size.prim_cast::<usize>();

                let locations = (0..num_locations)
                    .map(|i| location_list._items[i])
                    .map(read_location)
                    .collect();

                new_game_state(player, locations)
            }

            pub fn read_delta_state() {
                let player = read_player();

                new_game_state_delta(player)
            }
        })?;

        let vm = graph.compile(reader)?;

        Ok(GameStateReader { vm })
    }

    pub fn apply_delta(&mut self, delta: GameStateDelta) {
        self.player = delta.player;
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
