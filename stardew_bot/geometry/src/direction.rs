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

#[derive(RustNativeObject, Debug, Clone, Copy, PartialEq, Eq)]
pub enum CardinalDirection {
    North,
    East,
    South,
    West,
}

impl Direction {
    pub const VALUES: [Self; 8] = [
        Self::North,
        Self::East,
        Self::South,
        Self::West,
        Self::NorthEast,
        Self::SouthEast,
        Self::SouthWest,
        Self::NorthWest,
    ];

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
        Self::VALUES.into_iter()
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

impl CardinalDirection {
    pub const VALUES: [Self; 4] =
        [Self::South, Self::North, Self::East, Self::West];

    pub fn offset(self) -> Vector<isize> {
        match self {
            CardinalDirection::North => Vector::new(0, -1),
            CardinalDirection::East => Vector::new(1, 0),
            CardinalDirection::South => Vector::new(0, 1),
            CardinalDirection::West => Vector::new(-1, 0),
        }
    }

    pub fn iter() -> impl Iterator<Item = Self> {
        Self::VALUES.into_iter()
    }

    pub fn opposite(self) -> Self {
        match self {
            Self::North => Self::South,
            Self::East => Self::West,
            Self::South => Self::North,
            Self::West => Self::East,
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

impl From<CardinalDirection> for char {
    fn from(val: CardinalDirection) -> Self {
        match val {
            CardinalDirection::North => '↑',
            CardinalDirection::East => '→',
            CardinalDirection::South => '↓',
            CardinalDirection::West => '←',
        }
    }
}

impl std::fmt::Display for Direction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::North => write!(f, "North"),
            Self::NorthEast => write!(f, "NorthEast"),
            Self::East => write!(f, "East"),
            Self::SouthEast => write!(f, "SouthEast"),
            Self::South => write!(f, "South"),
            Self::SouthWest => write!(f, "SouthWest"),
            Self::West => write!(f, "West"),
            Self::NorthWest => write!(f, "NorthWest"),
        }
    }
}

impl std::fmt::Display for CardinalDirection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::North => write!(f, "North"),
            Self::East => write!(f, "East"),
            Self::South => write!(f, "South"),
            Self::West => write!(f, "West"),
        }
    }
}

impl PartialEq<Direction> for CardinalDirection {
    fn eq(&self, other: &Direction) -> bool {
        matches!(
            (self, other),
            (Self::North, Direction::North)
                | (Self::South, Direction::South)
                | (Self::East, Direction::East)
                | (Self::West, Direction::West)
        )
    }
}
impl PartialEq<CardinalDirection> for Direction {
    fn eq(&self, other: &CardinalDirection) -> bool {
        other == self
    }
}

impl From<CardinalDirection> for Direction {
    fn from(dir: CardinalDirection) -> Self {
        match dir {
            CardinalDirection::North => Self::North,
            CardinalDirection::East => Self::East,
            CardinalDirection::South => Self::South,
            CardinalDirection::West => Self::West,
        }
    }
}
