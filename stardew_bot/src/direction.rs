use dotnet_debugger::RustNativeObject;

use crate::game_state::Vector;

#[derive(RustNativeObject, Debug, Clone, Copy)]
pub enum Direction {
    North,
    NorthEast,
    East,
    SouthEast,
    South,
    SouthWest,
    West,
    NorthWest,
}

impl Direction {
    pub fn offset(self) -> Vector<isize> {
        match self {
            Direction::North => Vector::new(0, -1),
            Direction::NorthEast => Vector::new(1, -1),
            Direction::East => Vector::new(1, 0),
            Direction::SouthEast => Vector::new(1, 1),
            Direction::South => Vector::new(0, 1),
            Direction::SouthWest => Vector::new(-1, 1),
            Direction::West => Vector::new(-1, 0),
            Direction::NorthWest => Vector::new(-1, -1),
        }
    }

    pub fn is_cardinal(self) -> bool {
        matches!(
            self,
            Direction::North
                | Direction::East
                | Direction::South
                | Direction::West
        )
    }

    pub fn iter() -> impl Iterator<Item = Self> {
        [
            Self::North,
            Self::NorthEast,
            Self::East,
            Self::SouthEast,
            Self::South,
            Self::SouthWest,
            Self::West,
            Self::NorthWest,
        ]
        .into_iter()
    }
}

impl std::fmt::Display for Direction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Direction::North => write!(f, "North"),
            Direction::NorthEast => write!(f, "NorthEast"),
            Direction::East => write!(f, "East"),
            Direction::SouthEast => write!(f, "SouthEast"),
            Direction::South => write!(f, "South"),
            Direction::SouthWest => write!(f, "SouthWest"),
            Direction::West => write!(f, "West"),
            Direction::NorthWest => write!(f, "NorthWest"),
        }
    }
}
