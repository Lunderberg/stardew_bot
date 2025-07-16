use std::fmt::Display;

use geometry::{Direction, Vector};

#[derive(Debug, Clone, Copy)]
pub enum GameAction {
    HoldTool,
    ReleaseTool,
    Move(Direction),
    StopMoving,

    ExitMenu,
    StopExitingMenu,

    /// Send the 'x' keystroke, to activate a tile.
    // TODO: Send a mouse event instead.  This will allow the bot to
    // activate doors from a diagonal.
    ActivateTile,
    StopActivatingTile,

    /// Send the 'y' keystroke, to confirm an action in a menu
    ConfirmMenu,
    StopConfirmingMenu,

    HoldLeftShift,
    StopHoldingLeftShift,

    MouseOverTile(Vector<isize>),
    MouseOverPixel(Vector<isize>),

    LeftClick,
    ReleaseLeftClick,
    RightClick,
    ReleaseRightClick,
    ScrollDown,
    StopScrollingDown,
    ScrollUp,
    StopScrollingUp,

    SelectHotbar(usize),
    StopSelectingHotbar(usize),

    AnimationCancel,
    StopAnimationCanceling,
}

impl Display for GameAction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GameAction::HoldTool => write!(f, "HoldTool ('c')"),
            GameAction::ReleaseTool => write!(f, "ReleaseTool ('c')"),
            GameAction::Move(direction) => write!(f, "Move({direction})"),
            GameAction::StopMoving => write!(f, "StopMoving"),
            GameAction::ExitMenu => write!(f, "ExitMenu ('esc')"),
            GameAction::StopExitingMenu => write!(f, "StopExitingMenu ('esc')"),
            GameAction::ActivateTile => write!(f, "ActivateTile ('x')"),
            GameAction::StopActivatingTile => {
                write!(f, "StopActivatingTile ('x')")
            }
            GameAction::ConfirmMenu => write!(f, "ConfirmMenu ('y')"),
            GameAction::StopConfirmingMenu => {
                write!(f, "StopConfirmingMenu ('y')")
            }
            GameAction::MouseOverTile(tile) => {
                write!(f, "MouseOverTile({tile})")
            }
            GameAction::MouseOverPixel(pixel) => {
                write!(f, "MouseOverPixel({pixel})")
            }
            GameAction::LeftClick => write!(f, "LeftClick"),
            GameAction::ReleaseLeftClick => write!(f, "ReleaseLeftClick"),
            GameAction::RightClick => write!(f, "RightClick"),
            GameAction::ReleaseRightClick => write!(f, "ReleaseRightClick"),
            GameAction::ScrollDown => write!(f, "ScrollDown"),
            GameAction::StopScrollingDown => write!(f, "StopScrollingDown"),
            GameAction::ScrollUp => write!(f, "ScrollUp"),
            GameAction::StopScrollingUp => write!(f, "StopScrollingUp"),
            GameAction::SelectHotbar(i) => write!(f, "SelectHotbar({i})"),
            GameAction::StopSelectingHotbar(i) => {
                write!(f, "StopSelectingHotbar({i})")
            }
            GameAction::AnimationCancel => write!(f, "AnimationCancel"),
            GameAction::StopAnimationCanceling => {
                write!(f, "StopAnimationCanceling")
            }
            GameAction::HoldLeftShift => write!(f, "HoldLeftShift"),
            GameAction::StopHoldingLeftShift => {
                write!(f, "StopHoldingLeftShift")
            }
        }
    }
}
