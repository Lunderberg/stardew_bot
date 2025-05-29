use std::borrow::Cow;

use dotnet_debugger::RustNativeObject;

use crate::Error;

#[derive(RustNativeObject, Debug, Clone)]
pub struct Item {
    pub item_id: Cow<'static, str>,
    pub quality: Quality,
    pub count: usize,
    pub price: i32,
    pub edibility: i32,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Quality {
    /// Normal quality (value = 0)
    Normal,

    /// Silver quality (value = 1)
    Silver,

    /// Gold quality (value = 2)
    Gold,

    /// Iridium quality (value = 3)
    Iridium,
}

impl Item {
    pub const CLAY: Item = Item::new_const("(O)330");
    pub const SALAD: Item = Item::new_const("(O)196");
    pub const PARSNIP_SEEDS: Item = Item::new_const("(O)472");

    pub fn new(item_id: impl Into<Cow<'static, str>>) -> Self {
        Self {
            item_id: item_id.into(),
            quality: Quality::Normal,
            count: 1,
            price: 0,
            edibility: -300,
        }
    }

    const fn new_const(item_id: &'static str) -> Self {
        Self {
            item_id: Cow::Borrowed(item_id),
            quality: Quality::Normal,
            count: 1,
            price: 0,
            edibility: -300,
        }
    }

    pub fn with_quality(self, quality: Quality) -> Self {
        Self { quality, ..self }
    }

    pub fn with_count(self, count: usize) -> Self {
        Self { count, ..self }
    }

    pub fn with_price(self, price: i32) -> Self {
        Self { price, ..self }
    }

    pub fn with_edibility(self, edibility: i32) -> Self {
        Self { edibility, ..self }
    }

    pub fn stamina_recovery(&self) -> Option<f32> {
        if self.edibility == -300 {
            None
        } else {
            Some((self.edibility as f32) * 2.5)
        }
    }

    pub fn health_recovery(&self) -> Option<f32> {
        if self.edibility == -300 {
            None
        } else {
            Some((self.edibility as f32) * 1.125)
        }
    }

    pub fn is_same_item(&self, other: &Item) -> bool {
        self.item_id == other.item_id && self.quality == other.quality
    }
}

impl Quality {
    pub fn is_normal(&self) -> bool {
        matches!(self, Self::Normal)
    }
}

impl TryFrom<i32> for Quality {
    type Error = Error;

    fn try_from(value: i32) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Self::Normal),
            1 => Ok(Self::Silver),
            2 => Ok(Self::Gold),
            3 => Ok(Self::Iridium),
            other => Err(Error::InvalidQualityValue(other)),
        }
    }
}

impl std::fmt::Display for Quality {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Quality::Normal => write!(f, "Normal"),
            Quality::Silver => write!(f, "Silver"),
            Quality::Gold => write!(f, "Gold"),
            Quality::Iridium => write!(f, "Iridium"),
        }
    }
}

impl std::fmt::Display for Item {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self {
            item_id,
            quality,
            count,
            ..
        } = &self;
        if quality.is_normal() && *count == 1 {
            write!(f, "{item_id}")
        } else if quality.is_normal() {
            write!(f, "{count} {item_id}")
        } else if *count == 1 {
            write!(f, "{quality} {item_id}")
        } else {
            write!(f, "{count} {quality} {item_id}")
        }
    }
}
