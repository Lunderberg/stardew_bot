use dotnet_debugger::{RustNativeObject, SymbolicGraph, SymbolicValue};

use crate::Error;

#[derive(RustNativeObject, Debug, Clone, PartialEq)]
pub struct RngState {
    pub state: [i32; 56],
    pub inext: i32,
    pub inextp: i32,
}

impl RngState {
    pub(crate) fn def_read_rng_state(
        graph: &mut SymbolicGraph,
    ) -> Result<SymbolicValue, Error> {
        graph.named_native_function(
            "new_rng_state",
            |inext: i32, inextp: i32, state: &Vec<i32>| RngState {
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
}
