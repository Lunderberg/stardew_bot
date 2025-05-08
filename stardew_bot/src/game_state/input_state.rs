use dotnet_debugger::{RustNativeObject, SymbolicGraph, SymbolicValue};

use crate::Error;

use super::Vector;

#[derive(RustNativeObject, Debug, Clone)]
pub struct InputState {
    pub keys_pressed: Vec<Key>,
    pub mouse_location: Vector<isize>,
    pub mouse_buttons: u8,
}

impl InputState {
    pub(crate) fn read_all(
        graph: &mut SymbolicGraph,
    ) -> Result<SymbolicValue, Error> {
        graph.named_native_function(
            "new_input_state",
            |keys_pressed: &Vec<i32>,
             mouse_x: isize,
             mouse_y: isize,
             mouse_buttons: u8| {
                let mouse_location = Vector::new(mouse_x, mouse_y);
                InputState {
                    keys_pressed: keys_pressed
                        .iter()
                        .cloned()
                        .map(Into::into)
                        .collect(),
                    mouse_location,
                    mouse_buttons,
                }
            },
        )?;

        let func = graph.parse(stringify! {
            fn read_input_state() {
                let keys = Microsoft.Xna.Framework
                    .Input
                    .Keyboard
                    ._keys;

                let num_keys = keys._size.prim_cast::<usize>();
                let keys_pressed = (0..num_keys)
                    .map(|i| keys._items[i].value__)
                    .collect();

                let mouse = StardewValley.Game1
                    .oldMouseState;

                new_input_state(
                    keys_pressed,
                    mouse._x, mouse._y, mouse._buttons,
                )
            }
        })?;

        Ok(func)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Key {
    Backspace,
    Tab,
    Enter,
    CapsLock,
    Escape,
    Space,
    PageUp,
    PageDown,
    End,
    Home,
    Left,
    Up,
    Right,
    Down,
    Select,
    PrintScreen,
    Insert,
    Delete,
    Digit0,
    Digit1,
    Digit2,
    Digit3,
    Digit4,
    Digit5,
    Digit6,
    Digit7,
    Digit8,
    Digit9,
    A,
    B,
    C,
    D,
    E,
    F,
    G,
    H,
    I,
    J,
    K,
    L,
    M,
    N,
    O,
    P,
    Q,
    R,
    S,
    T,
    U,
    V,
    W,
    X,
    Y,
    Z,
    LeftWindows,
    RightWindows,
    Apps,
    NumPad0,
    NumPad1,
    NumPad2,
    NumPad3,
    NumPad4,
    NumPad5,
    NumPad6,
    NumPad7,
    NumPad8,
    NumPad9,
    Multiply,
    Add,
    Separator,
    Subtract,
    Decimal,
    Divide,
    F1,
    F2,
    F3,
    F4,
    F5,
    F6,
    F7,
    F8,
    F9,
    F10,
    F11,
    F12,
    F13,
    F14,
    F15,
    F16,
    F17,
    F18,
    F19,
    F20,
    F21,
    F22,
    F23,
    F24,
    NumLock,
    Scroll,
    LeftShift,
    RightShift,
    LeftControl,
    RightControl,
    LeftAlt,
    RightAlt,
    OemSemicolon,
    OemPlus,
    OemComma,
    OemMinus,
    OemPeriod,
    OemQuestion,
    OemTilde,
    OemOpenBrackets,
    OemPipe,
    OemCloseBrackets,
    OemQuotes,
    Oem8,
    OemBackslash,
    Pause,
    Other(i32),
}

impl From<i32> for Key {
    fn from(value: i32) -> Self {
        match value {
            8 => Key::Backspace,
            9 => Key::Tab,
            13 => Key::Enter,
            20 => Key::CapsLock,
            27 => Key::Escape,
            32 => Key::Space,
            33 => Key::PageUp,
            34 => Key::PageDown,
            35 => Key::End,
            36 => Key::Home,
            37 => Key::Left,
            38 => Key::Up,
            39 => Key::Right,
            40 => Key::Down,
            41 => Key::Select,
            44 => Key::PrintScreen,
            45 => Key::Insert,
            46 => Key::Delete,
            48 => Key::Digit0,
            49 => Key::Digit1,
            50 => Key::Digit2,
            51 => Key::Digit3,
            52 => Key::Digit4,
            53 => Key::Digit5,
            54 => Key::Digit6,
            55 => Key::Digit7,
            56 => Key::Digit8,
            57 => Key::Digit9,
            65 => Key::A,
            66 => Key::B,
            67 => Key::C,
            68 => Key::D,
            69 => Key::E,
            70 => Key::F,
            71 => Key::G,
            72 => Key::H,
            73 => Key::I,
            74 => Key::J,
            75 => Key::K,
            76 => Key::L,
            77 => Key::M,
            78 => Key::N,
            79 => Key::O,
            80 => Key::P,
            81 => Key::Q,
            82 => Key::R,
            83 => Key::S,
            84 => Key::T,
            85 => Key::U,
            86 => Key::V,
            87 => Key::W,
            88 => Key::X,
            89 => Key::Y,
            90 => Key::Z,
            91 => Key::LeftWindows,
            92 => Key::RightWindows,
            93 => Key::Apps,
            96 => Key::NumPad0,
            97 => Key::NumPad1,
            98 => Key::NumPad2,
            99 => Key::NumPad3,
            100 => Key::NumPad4,
            101 => Key::NumPad5,
            102 => Key::NumPad6,
            103 => Key::NumPad7,
            104 => Key::NumPad8,
            105 => Key::NumPad9,
            106 => Key::Multiply,
            107 => Key::Add,
            108 => Key::Separator,
            109 => Key::Subtract,
            110 => Key::Decimal,
            111 => Key::Divide,
            112 => Key::F1,
            113 => Key::F2,
            114 => Key::F3,
            115 => Key::F4,
            116 => Key::F5,
            117 => Key::F6,
            118 => Key::F7,
            119 => Key::F8,
            120 => Key::F9,
            121 => Key::F10,
            122 => Key::F11,
            123 => Key::F12,
            124 => Key::F13,
            125 => Key::F14,
            126 => Key::F15,
            127 => Key::F16,
            128 => Key::F17,
            129 => Key::F18,
            130 => Key::F19,
            131 => Key::F20,
            132 => Key::F21,
            133 => Key::F22,
            134 => Key::F23,
            135 => Key::F24,
            144 => Key::NumLock,
            145 => Key::Scroll,
            160 => Key::LeftShift,
            161 => Key::RightShift,
            162 => Key::LeftControl,
            163 => Key::RightControl,
            164 => Key::LeftAlt,
            165 => Key::RightAlt,
            186 => Key::OemSemicolon,
            187 => Key::OemPlus,
            188 => Key::OemComma,
            189 => Key::OemMinus,
            190 => Key::OemPeriod,
            191 => Key::OemQuestion,
            192 => Key::OemTilde,
            219 => Key::OemOpenBrackets,
            220 => Key::OemPipe,
            221 => Key::OemCloseBrackets,
            222 => Key::OemQuotes,
            223 => Key::Oem8,
            226 => Key::OemBackslash,
            0x13 => Key::Pause,
            other => Key::Other(other),
        }
    }
}

impl std::fmt::Display for Key {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Key::Other(other) => write!(f, "other(value={other})"),
            _ => write!(f, "{self:?}"),
        }
    }
}
