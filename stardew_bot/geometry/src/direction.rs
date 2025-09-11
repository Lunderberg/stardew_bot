use dsl::RustNativeObject;

use crate::Vector;

#[derive(RustNativeObject, Debug, Clone, Copy, PartialEq, Eq)]
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
            Self::East,
            Self::South,
            Self::West,
            Self::NorthEast,
            Self::SouthEast,
            Self::SouthWest,
            Self::NorthWest,
        ]
        .into_iter()
    }

    pub fn iter_cardinal() -> impl Iterator<Item = Self> {
        Self::iter().filter(|dir| dir.is_cardinal())
    }

    pub fn opposite(self) -> Direction {
        match self {
            Direction::North => Direction::South,
            Direction::NorthEast => Direction::SouthWest,
            Direction::East => Direction::West,
            Direction::SouthEast => Direction::NorthWest,
            Direction::South => Direction::North,
            Direction::SouthWest => Direction::NorthEast,
            Direction::West => Direction::East,
            Direction::NorthWest => Direction::SouthEast,
        }
    }
}

impl From<Direction> for char {
    fn from(val: Direction) -> Self {
        match val {
            Direction::North => '↑',
            Direction::NorthEast => '↗',
            Direction::East => '→',
            Direction::SouthEast => '↘',
            Direction::South => '↓',
            Direction::SouthWest => '↙',
            Direction::West => '←',
            Direction::NorthWest => '↖',
        }
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
