use bot_logic::GameAction;
use game_state::GameState;
use geometry::Direction;
use x11rb::protocol::xproto::{ButtonIndex, Keycode as X11KeyCode};

use crate::{Error, X11Handler};

// TODO: Actually look up the X11KeyCode using xkbcommon
//
// Example:
// https://github.com/rust-x-bindings/toy_xcb/blob/master/src/keyboard.rs
const KEY_W: X11KeyCode = 25;
const KEY_A: X11KeyCode = 38;
const KEY_S: X11KeyCode = 39;
const KEY_D: X11KeyCode = 40;

const KEY_X: X11KeyCode = 53;
const KEY_C: X11KeyCode = 54;
const KEY_Y: X11KeyCode = 29;

const KEY_ESCAPE: X11KeyCode = 9;

const KEY_1: X11KeyCode = 10;
const KEY_2: X11KeyCode = 11;
const KEY_3: X11KeyCode = 12;
const KEY_4: X11KeyCode = 13;
const KEY_5: X11KeyCode = 14;
const KEY_6: X11KeyCode = 15;
const KEY_7: X11KeyCode = 16;
const KEY_8: X11KeyCode = 17;
const KEY_9: X11KeyCode = 18;
const KEY_0: X11KeyCode = 19;
const KEY_MINUS: X11KeyCode = 20;
const KEY_EQUAL: X11KeyCode = 21;
const KEY_INVENTORY_SELECT: [X11KeyCode; 12] = [
    KEY_1, KEY_2, KEY_3, KEY_4, KEY_5, KEY_6, KEY_7, KEY_8, KEY_9, KEY_0,
    KEY_MINUS, KEY_EQUAL,
];

const KEY_LEFT_SHIFT: X11KeyCode = 50;
const KEY_RIGHT_SHIFT: X11KeyCode = 62;
const KEY_R: X11KeyCode = 27;
const KEY_DELETE: X11KeyCode = 119;
const KEY_ANIMATION_CANCEL: [X11KeyCode; 3] =
    [KEY_RIGHT_SHIFT, KEY_R, KEY_DELETE];

const MOUSE_LEFT: ButtonIndex = ButtonIndex::M1;
const MOUSE_RIGHT: ButtonIndex = ButtonIndex::M3;
const MOUSE_SCROLL_UP: ButtonIndex = ButtonIndex::M4;
const MOUSE_SCROLL_DOWN: ButtonIndex = ButtonIndex::M5;

pub fn apply_game_action(
    action: GameAction,
    handler: &mut X11Handler,
    game_state: &GameState,
) -> Result<(), Error> {
    match action {
        GameAction::HoldTool => {
            handler.send_keystroke(true, KEY_C)?;
        }
        GameAction::ReleaseTool => {
            handler.send_keystroke(false, KEY_C)?;
        }
        GameAction::Move(direction) => {
            let keystate = match direction {
                Direction::North => [true, false, false, false],
                Direction::NorthEast => [true, false, false, true],
                Direction::East => [false, false, false, true],
                Direction::SouthEast => [false, false, true, true],
                Direction::South => [false, false, true, false],
                Direction::SouthWest => [false, true, true, false],
                Direction::West => [false, true, false, false],
                Direction::NorthWest => [true, true, false, false],
            };
            keystate
                .into_iter()
                .zip([KEY_W, KEY_A, KEY_S, KEY_D])
                .try_for_each(|(state, key)| {
                    handler.send_keystroke(state, key)
                })?;
        }
        GameAction::StopMoving => {
            handler.send_keystroke(false, KEY_W)?;
            handler.send_keystroke(false, KEY_A)?;
            handler.send_keystroke(false, KEY_S)?;
            handler.send_keystroke(false, KEY_D)?;
        }
        GameAction::ActivateTile => handler.send_keystroke(true, KEY_X)?,
        GameAction::StopActivatingTile => {
            handler.send_keystroke(false, KEY_X)?
        }
        GameAction::ConfirmMenu => handler.send_keystroke(true, KEY_Y)?,
        GameAction::StopConfirmingMenu => {
            handler.send_keystroke(false, KEY_Y)?
        }

        GameAction::MouseOverTile(tile) => {
            let pixel = game_state.display.center_pixel_of_tile(tile);
            handler.move_mouse(pixel)?;
        }
        GameAction::MouseOverPixel(pixel) => {
            handler.move_mouse(pixel)?;
        }

        GameAction::LeftClick => handler.send_click(true, MOUSE_LEFT)?,
        GameAction::ReleaseLeftClick => {
            handler.send_click(false, MOUSE_LEFT)?
        }
        GameAction::RightClick => handler.send_click(true, MOUSE_RIGHT)?,
        GameAction::ReleaseRightClick => {
            handler.send_click(false, MOUSE_RIGHT)?
        }
        GameAction::ScrollDown => {
            handler.send_click(true, MOUSE_SCROLL_DOWN)?
        }
        GameAction::StopScrollingDown => {
            handler.send_click(false, MOUSE_SCROLL_DOWN)?
        }
        GameAction::ScrollUp => handler.send_click(true, MOUSE_SCROLL_UP)?,
        GameAction::StopScrollingUp => {
            handler.send_click(false, MOUSE_SCROLL_UP)?
        }

        GameAction::ExitMenu => handler.send_keystroke(true, KEY_ESCAPE)?,
        GameAction::StopExitingMenu => {
            handler.send_keystroke(false, KEY_ESCAPE)?
        }

        GameAction::SelectHotbar(i) => {
            handler.send_keystroke(true, KEY_INVENTORY_SELECT[i])?
        }
        GameAction::StopSelectingHotbar(i) => {
            handler.send_keystroke(false, KEY_INVENTORY_SELECT[i])?
        }

        GameAction::AnimationCancel => {
            for key in KEY_ANIMATION_CANCEL {
                handler.send_keystroke(true, key)?
            }
        }
        GameAction::StopAnimationCanceling => {
            for key in KEY_ANIMATION_CANCEL {
                handler.send_keystroke(false, key)?
            }
        }

        GameAction::HoldLeftShift => {
            handler.send_keystroke(true, KEY_LEFT_SHIFT)?
        }
        GameAction::StopHoldingLeftShift => {
            handler.send_keystroke(false, KEY_LEFT_SHIFT)?
        }
    }

    Ok(())
}
