use std::collections::VecDeque;

use dotnet_debugger::{RustNativeObject, SymbolicGraph, SymbolicValue};

use crate::Error;

const FRAMES_TO_TRACK: usize = 30;

#[derive(Debug, Clone)]
pub struct RngState {
    rng: SeededRng,
    prev_game_mode_tick: i32,
    prev_rng: SeededRng,
    calls_per_tick: VecDeque<Option<usize>>,
}

#[derive(RustNativeObject, Debug, Clone, PartialEq)]
pub struct SeededRng {
    state: [i32; 56],
    inext: i32,
    inextp: i32,
}

impl RngState {
    pub fn new(current: SeededRng) -> Self {
        Self {
            prev_rng: current.clone(),
            rng: current,
            prev_game_mode_tick: 0,
            calls_per_tick: Default::default(),
        }
    }

    pub fn apply_delta(&mut self, current: SeededRng, game_mode_tick: i32) {
        self.rng = current;

        if self.prev_game_mode_tick != game_mode_tick {
            let mut push = |value: Option<usize>| {
                self.calls_per_tick.push_back(value);
                while self.calls_per_tick.len() > FRAMES_TO_TRACK {
                    self.calls_per_tick.pop_front();
                }
            };

            let num_ticks =
                (game_mode_tick - self.prev_game_mode_tick) as usize;
            let opt_num_calls =
                self.prev_rng.clone().num_calls_until(&self.rng, 200);

            if num_ticks < FRAMES_TO_TRACK {
                if let Some(num_calls) = opt_num_calls {
                    let avg = num_calls / num_ticks;
                    let remainder = num_calls - avg * (num_ticks - 1);
                    for _ in 0..num_ticks - 1 {
                        push(Some(avg));
                    }

                    push(Some(remainder));
                } else {
                    for _ in 0..num_ticks {
                        push(None);
                    }
                }
            } else {
                for _ in 0..FRAMES_TO_TRACK {
                    push(None);
                }
            }

            self.prev_rng = self.rng.clone();
            self.prev_game_mode_tick = game_mode_tick;
        }
    }

    pub fn current(&self) -> SeededRng {
        self.rng.clone()
    }

    pub fn iter_float(&self) -> impl Iterator<Item = f32> {
        let mut rng = self.current();
        std::iter::from_fn(move || Some(rng.rand_float()))
    }

    pub fn iter_prev_calls(&self) -> impl Iterator<Item = Option<usize>> + '_ {
        self.calls_per_tick.iter().cloned()
    }
}

impl SeededRng {
    pub(crate) fn def_read_rng_state(
        graph: &mut SymbolicGraph,
    ) -> Result<SymbolicValue, Error> {
        graph.named_native_function(
            "new_rng_state",
            |inext: i32, inextp: i32, state: &Vec<i32>| SeededRng {
                state: state.as_slice().try_into().expect(
                    "Should be constructed with correct number of elements",
                ),
                inext,
                inextp,
            },
        )?;

        let func = graph.parse(stringify! {
            fn read_rng_state() {
                let rng = StardewValley.Game1
                    .random
                    ._impl
                    .as::<Net5CompatSeedImpl>()
                    ._prng;

                let inext = rng._inext;
                let inextp = rng._inextp;

                let state = (0..56)
                    .map(|i| rng._seedArray[i])
                    .collect();

                new_rng_state(inext,inextp,state)
            }
        })?;

        Ok(func)
    }

    // Adapted from
    // https://blogs.siliconorchid.com/post/coding-inspiration/randomness-in-dotnet
    //   #knuth-s-subtractive-number-generator-implementation-of-system-random-in-net
    pub fn from_seed(seed: i32) -> Self {
        let mut state = [0i32; 56];

        let subtraction = if seed == i32::MIN {
            i32::MAX
        } else {
            seed.abs()
        };
        let mut mj = 161803398 - subtraction;
        state[55] = mj;
        let mut mk = 1;

        let mut ii = 0;

        for _i in 1..55 {
            ii += 21;
            if ii >= 55 {
                ii -= 55;
            }

            state[ii] = mk;
            mk = mj - mk;
            if mk < 0 {
                mk += i32::MAX;
            }

            mj = state[ii];
        }

        for _k in 1..5 {
            for i in 1..56 {
                let mut n = i + 30;
                if n >= 55 {
                    n -= 55;
                }

                state[i] -= state[1 + n];
                if state[i] < 0 {
                    state[i] += i32::MAX;
                }
            }
        }

        Self {
            state,
            inext: 0,
            inextp: 21,
        }
    }

    pub fn rand_i32(&mut self) -> i32 {
        let mut new_inext = self.inext;
        new_inext += 1;
        if new_inext >= 56 {
            new_inext = 1;
        }

        let mut new_inextp = self.inextp;
        new_inextp += 1;
        if new_inextp >= 56 {
            new_inextp = 1;
        }

        let state = &mut self.state;
        let mut sample = state[new_inext as usize] - state[new_inextp as usize];

        if sample == i32::MAX {
            sample -= 1;
        }
        if sample < 0 {
            sample += i32::MAX;
        }

        state[new_inext as usize] = sample;
        self.inext = new_inext;
        self.inextp = new_inextp;

        sample
    }

    pub fn rand_float(&mut self) -> f32 {
        (self.rand_i32() as f32) / (i32::MAX as f32)
    }

    pub fn from_stardew_seed<const N: usize>(values: [f64; N]) -> Self {
        let seed = Self::stardew_seed(values);
        Self::from_seed(seed)
    }

    pub fn stardew_seed<const N: usize>(values: [f64; N]) -> i32 {
        assert!(N <= 5, "Stardew only uses up to 5 seed values");

        let values: [f64; 5] =
            std::array::from_fn(|i| if i < N { values[i] } else { 0.0 });

        let mut bytes = [0u8; 20];
        for (i, seed) in values.into_iter().enumerate() {
            let seed: f64 = seed % (i32::MAX as f64);
            let seed: i32 = seed as i32;
            for (j, byte) in seed.to_le_bytes().into_iter().enumerate() {
                bytes[4 * i + j] = byte;
            }
        }

        let seed: u32 = xxhash_rust::xxh32::xxh32(&bytes, 0);

        let seed: i32 = i32::from_ne_bytes(seed.to_ne_bytes());

        seed
    }

    pub fn legacy_stardew_seed<const N: usize>(values: [f64; N]) -> i32 {
        assert!(N <= 5, "Stardew only uses up to 5 seed values");

        let values: [f64; 5] =
            std::array::from_fn(|i| if i < N { values[i] } else { 0.0 });

        let seed = values
            .into_iter()
            .map(|seed| seed % (i32::MAX as f64))
            .sum::<f64>()
            % (i32::MAX as f64);

        seed as i32
    }

    fn num_calls_until(
        mut self,
        later: &Self,
        max_iter: usize,
    ) -> Option<usize> {
        for i in 0..max_iter {
            if &self == later {
                return Some(i);
            }
            self.rand_i32();
        }
        None
    }
}
