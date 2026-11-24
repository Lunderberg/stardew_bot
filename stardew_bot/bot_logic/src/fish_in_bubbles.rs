use std::ops::Range;

use game_state::GameState;
use geometry::{CardinalDirection, Vector};

use env_var_flag::env_var_flag;
use itertools::{Either, Itertools as _};

use crate::{
    Error, GameStateExt as _, LocationExt as _,
    predictors::FishingBubblePredictor,
};

pub struct FishInBubbles {
    /// The room in which the bubbles are located.
    pub bubble_room: String,

    /// Where the bubbles are located.
    pub bubble_tile: Vector<isize>,

    /// When the bubbles will start/stop.
    pub bubble_duration: Range<i32>,

    /// Where the player should stand in order to use the bubbles.
    pub player_tile: Vector<isize>,

    /// The target location for the bobber
    ///
    /// Getting the 4x bite rate from bubbles only requires the
    /// bobber's bounding box to overlap with the bubble tile.  This
    /// allows the bobber to land up to half a tile outside of the
    /// bubbles.  The fish quality depends on the tile with the
    /// bobber's center, so this allows us to catch slightly better
    /// fish while still using the bubbles.
    pub target_bobber: Vector<f32>,
}

impl FishInBubbles {
    pub fn next_bubbles(
        game_state: &GameState,
        bubble_room: impl Into<String>,
    ) -> Result<impl Iterator<Item = Self>, Error> {
        let bubble_room = bubble_room.into();
        let loc = game_state.get_room(&bubble_room)?;
        let player = &game_state.player;

        let reachable = loc.pathfinding(&game_state.statics).reachable(
            if player.room_name == bubble_room {
                player.tile()
            } else {
                game_state.closest_entrance(&loc.name)?
            },
        );

        let predictor = FishingBubblePredictor::new(game_state, &bubble_room)?;
        let iter = predictor.predict().into_iter().filter_map(move |bubbles| {
            let fishing_level = player.skills.fishing_level();

            let opt_stand_on = iter_fish_in_bubbles_offset(fishing_level)
                .map(|offset| bubbles.tile + offset)
                .filter(|to_stand| reachable.is_set(*to_stand))
                .min_by_key(|to_stand| {
                    let offset = (*to_stand - bubbles.tile).map(|x| x.abs());
                    let chebyshev = offset.right.max(offset.down);
                    let manhattan = offset.right + offset.down;
                    (chebyshev, manhattan)
                });
            let Some(stand_on) = opt_stand_on else {
                return None;
            };

            let mut target_bobber = bubbles.tile.map(|i| i as f32);

            // If the water is deeper just past the bubbles, aim for that
            // tile, but still within the area that uses the bubbles.
            let cast_dir =
                (bubbles.tile - stand_on).closest_cardinal_direction();
            if predictor.dist_from_land(bubbles.tile)
                < predictor.dist_from_land(bubbles.tile + cast_dir.offset())
            {
                let offset = cast_dir.offset().map(|i| i as f32) * 0.75;
                target_bobber = target_bobber + offset;
            }

            Some(FishInBubbles {
                bubble_room: bubble_room.clone(),
                bubble_tile: bubbles.tile,
                bubble_duration: bubbles.duration,
                player_tile: stand_on,
                target_bobber,
            })
        });
        Ok(iter)
    }
}

/// Returns relative positions where the player may stand in order to
/// use the fishing bubbles.
fn iter_fish_in_bubbles_offset(
    fishing_level: u8,
) -> impl Iterator<Item = Vector<isize>> {
    let use_exact_check = env_var_flag("CHECK_EXACT_BUBBLE_OFFSET");

    if use_exact_check {
        Either::Left(find_reachable(fishing_level, true))
    } else {
        // A quicker and simpler implementation that covers the
        // majority of cases.  Especially since bubbles can only
        // appear in depths 2-4, there isn't much benefit in
        // performing the more accurate check.
        let range: isize = match fishing_level {
            0 => 4,
            1..4 => 5,
            4..8 => 6,
            8.. => 7,
        };

        let iter = (-range..range)
            .cartesian_product(-range..range)
            .filter(|(di, dj)| {
                // Cannot cast the bobber such that it lands in/near
                // the tile next to the player.
                di.abs() > 1 || dj.abs() > 1
            })
            .filter(|(di, dj)| {
                // Cannot steer more than 2 tiles in the direction
                // perpendicular to the cast.  (Technically possible
                // when facing South with longer casts, but that's not
                // an edge case I care to handle here.)
                di.abs() < 3 || dj.abs() < 3
            })
            .filter(|(di, dj)| {
                // Exclude fishing
                //
                // These are possible, but requires more intra-tile
                // positioning before the cast than I want to handle
                // at the moment.
                di.abs() != 2 || dj.abs() != 2
            })
            .map(|(di, dj)| Vector::new(di, dj));
        Either::Right(iter)
    }
}

fn find_reachable(
    fishing_level: u8,
    bubble_check: bool,
) -> impl Iterator<Item = Vector<isize>> {
    let added_distance = match fishing_level {
        0 => 0,
        1..4 => 1,
        4..8 => 2,
        8..15 => 3,
        15.. => 4,
    };

    let gravity = 0.005;

    // Player's bbox is (48,32) pixels.  The first `bbox[i]/2` pixels
    // of distance must be used to reach the edge of the tile with the
    // player.
    let to_side_of_player_tile = 24;
    let to_top_of_player_tile = 16;

    // The bobber is 64x64 pixels, and must intersect with the bubble
    // tile in order to get the bonus fishing rate.
    let to_bobber_edge = if bubble_check { 30 } else { 0 };

    let iter_east_west = {
        let min_pixels: usize = 120;
        let max_pixels: usize = (4 + added_distance) * 64 - 8;

        let min_tiles =
            (min_pixels - to_side_of_player_tile - to_bobber_edge).div_ceil(64);
        let max_tiles =
            (max_pixels - to_side_of_player_tile + to_bobber_edge).div_ceil(64);

        (min_tiles..=max_tiles).flat_map(move |h_tiles| {
            let h_pixels = (h_tiles * 64 + to_side_of_player_tile)
                .clamp(min_pixels, max_pixels)
                as f32;
            let velocity =
                h_pixels * (gravity / (2.0 * (h_pixels + 96.0))).sqrt();
            let t_ms = (velocity / gravity)
                + (velocity * velocity + 192.0 * gravity).sqrt() / gravity;
            let t_frames = t_ms * 60.0 / 1000.0;
            let v_pixels = (4.0 * t_frames) as usize;
            let max_v_tiles = (v_pixels + to_bobber_edge
                - to_top_of_player_tile)
                .div_ceil(64);
            (0..=max_v_tiles)
                .cartesian_product([
                    CardinalDirection::East,
                    CardinalDirection::West,
                ])
                .cartesian_product([
                    CardinalDirection::North,
                    CardinalDirection::South,
                ])
                .map(move |((v_tiles, dir_facing), dir_perp)| {
                    let offset_facing =
                        dir_facing.offset() * (h_tiles as isize);
                    let offset_perp = dir_perp.offset() * (v_tiles as isize);
                    offset_facing + offset_perp
                })
        })
    };

    let iter_north = {
        let min_pixels: usize = 128;
        let max_pixels: usize = (3 + added_distance) * 64;

        let min_tiles =
            (min_pixels - to_top_of_player_tile - to_bobber_edge).div_ceil(64);
        let max_tiles =
            (max_pixels - to_top_of_player_tile + to_bobber_edge).div_ceil(64);

        (min_tiles..=max_tiles).flat_map(move |v_tiles| {
            let v_pixels = (v_tiles * 64 + to_top_of_player_tile)
                .clamp(min_pixels, max_pixels)
                as f32;
            let t_ms = (256.0 / gravity).sqrt()
                + ((2.0 * v_pixels + 256.0) / gravity).sqrt();
            let t_frames = t_ms * 60.0 / 1000.0 * 1.05 * 1.05;

            let h_pixels = (2.0 * t_frames) as usize;
            let max_h_tiles = (h_pixels + to_bobber_edge
                - to_side_of_player_tile)
                .div_ceil(64);

            (0..=max_h_tiles)
                .cartesian_product([
                    CardinalDirection::East,
                    CardinalDirection::West,
                ])
                .map(move |(h_tiles, dir_perp)| {
                    let offset_facing =
                        CardinalDirection::North.offset() * (v_tiles as isize);
                    let offset_perp = dir_perp.offset() * (h_tiles as isize);
                    offset_facing + offset_perp
                })
        })
    };

    let iter_south = {
        let min_pixels: usize = 128;
        let max_pixels: usize = (3 + added_distance) * 64;

        let min_tiles =
            (min_pixels - to_top_of_player_tile - to_bobber_edge).div_ceil(64);
        let max_tiles =
            (max_pixels - to_top_of_player_tile + to_bobber_edge).div_ceil(64);

        (min_tiles..=max_tiles).flat_map(move |v_tiles| {
            let v_pixels = (v_tiles * 64 + to_top_of_player_tile)
                .clamp(min_pixels, max_pixels)
                as f32;
            let t_ms = ((4.0 * v_pixels + 128.0) / gravity).sqrt()
                + ((2.0 * v_pixels + 128.0) / gravity).sqrt();
            let t_frames = t_ms * 60.0 / 1000.0 * 1.05;
            let h_pixels = (2.0 * t_frames) as usize;
            let max_h_tiles = (h_pixels + to_bobber_edge
                - to_side_of_player_tile)
                .div_ceil(64);

            (0..=max_h_tiles)
                .cartesian_product([
                    CardinalDirection::East,
                    CardinalDirection::West,
                ])
                .map(move |(h_tiles, dir_perp)| {
                    let offset_facing =
                        CardinalDirection::South.offset() * (v_tiles as isize);
                    let offset_perp = dir_perp.offset() * (h_tiles as isize);
                    offset_facing + offset_perp
                })
        })
    };

    std::iter::empty()
        .chain(iter_east_west)
        .chain(iter_north)
        .chain(iter_south)
}

/// Predicts the landing position of the bobber
///
/// Returns the bobber's travel distance in the cast direction, and
/// the maximum perpendicular distance that it can be steered while in
/// flight.
///
/// Currently unused, since decisions from the bot depend instead on
/// quantities derived from this.  For example, determining which
/// tiles to stand on in order to reach bubbles requires the range of
/// outputs this can produce, and determining the desired casting
/// power requires inverting the formula for cast distance.  This
/// function is preserved for reference, and in case it becomes needed
/// later.
#[allow(unused)]
fn bobber_offset(game_state: &GameState, casting_power: f32) -> (isize, isize) {
    let player = &game_state.player;
    let cast_dir = player.facing;
    let fishing_level = player.skills.fishing_level();

    let added_distance = match fishing_level {
        0 => 0,
        1..4 => 1,
        4..8 => 2,
        8..15 => 3,
        15.. => 4,
    };

    let max_distance = match cast_dir {
        CardinalDirection::East | CardinalDirection::West => 4 + added_distance,
        CardinalDirection::North | CardinalDirection::South => {
            3 + added_distance
        }
    };
    let max_distance = max_distance as f32;

    let gravity = 0.005;

    let (distance, t) = match cast_dir {
        CardinalDirection::East | CardinalDirection::West => {
            let distance =
                (casting_power * max_distance * 64.0).max(128.0) - 8.0;
            let velocity =
                distance * (gravity / (2.0 * (distance + 96.0))).sqrt();
            let t = (velocity / gravity)
                + (velocity * velocity + 192.0 * gravity).sqrt() / gravity;
            (distance, t)
        }
        CardinalDirection::North => {
            let distance = (casting_power * max_distance * 64.0).max(128.0);
            let t = (256.0 / gravity).sqrt()
                + ((2.0 * distance + 256.0) / gravity).sqrt();
            (distance, t)
        }
        CardinalDirection::South => {
            let distance = (casting_power * max_distance * 64.0).max(128.0);
            let t = ((4.0 * distance + 128.0) / gravity).sqrt()
                + ((2.0 * distance + 128.0) / gravity).sqrt();
            (distance, t)
        }
    };

    let t_scale = match cast_dir {
        CardinalDirection::North => 1.05 * 1.05,
        CardinalDirection::South => 1.05,
        CardinalDirection::East | CardinalDirection::West => 1.0,
    };
    let t = t * t_scale;

    let pixels_per_tick = match cast_dir {
        CardinalDirection::North | CardinalDirection::South => 2,
        CardinalDirection::East | CardinalDirection::West => 4,
    };

    let num_ticks = t / (1000.0 / 60.0);
    let d_perp = pixels_per_tick * (num_ticks as isize);

    (distance as isize, d_perp)
}
