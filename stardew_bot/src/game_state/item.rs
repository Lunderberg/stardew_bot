use std::borrow::Cow;

use dotnet_debugger::RustNativeObject;

use crate::Error;

#[derive(RustNativeObject, Debug, Clone)]
pub struct Item {
    pub id: ItemId,

    pub count: usize,
    pub price: i32,
    pub edibility: i32,

    /// Extra information about a specific item (e.g. watering cans
    /// tracking the amount of water).
    pub kind: Option<ItemKind>,

    /// The numeric category of the item. (e.g. watering cans being
    /// within the Tool category).
    pub category: Option<ItemCategory>,
}

/// A identfier for a type of item
///
/// Can be used as a dictionary lookup.  Only identifies the item
/// type, without any extra fields that may be present for the item.
/// Often, these will be available in contexts that generate items,
/// even before the item itself has been materialized.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ItemId {
    /// The qualified item id (e.g. "(O)CarrotSeeds").
    pub item_id: Cow<'static, str>,

    /// The quality of the item.
    pub quality: Quality,
}

/// Contains extra fields for specific item types.
#[derive(RustNativeObject, Debug, Clone)]
pub enum ItemKind {
    WateringCan(WateringCan),
    FishingRod(FishingRod),
    Weapon(Weapon),
}

#[derive(Debug, Clone)]
pub struct WateringCan {
    pub remaining_water: i32,
    pub max_water: i32,
}

#[derive(Debug, Clone)]
pub struct FishingRod {
    pub bait: Option<Box<Item>>,
    pub tackle: Option<Box<Item>>,
}

#[derive(Debug, Clone)]
pub struct Weapon {
    pub kind: WeaponKind,
    pub min_damage: i32,
    pub max_damage: i32,
}

#[derive(Debug, Clone)]
pub enum WeaponKind {
    Dagger,
    Club,
    Sword,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ItemCategory {
    Tool,
    Fish,
    Seed,
    Junk,
    Ore,
    Other(i32),
}

impl ItemId {
    pub fn new(item_id: impl Into<Cow<'static, str>>) -> Self {
        Self {
            item_id: item_id.into(),
            quality: Quality::Normal,
        }
    }

    pub const fn new_const(item_id: &'static str) -> Self {
        Self {
            item_id: Cow::Borrowed(item_id),
            quality: Quality::Normal,
        }
    }

    pub fn with_quality(self, quality: Quality) -> Self {
        Self { quality, ..self }
    }

    pub fn as_item(self) -> Item {
        self.into()
    }

    pub fn is_tree_seed(&self) -> bool {
        match self.item_id.as_ref() {
            "(O)309"
            | "(O)310"
            | "(O)311"
            | "(O)MossySeed"
            | "(O)FlashShifter.StardewValleyExpandedCP_Fir_Seed"
            | "(O)FlashShifter.StardewValleyExpandedCP_Birch_Seed" => true,
            _ => false,
        }
    }

    pub fn is_fruit_sapling(&self) -> bool {
        match self.item_id.as_ref() {
            "(O)69"
            | "(O)628"
            | "(O)629"
            | "(O)630"
            | "(O)631"
            | "(O)632"
            | "(O)633"
            | "(O)835"
            | "(O)FlashShifter.StardewValleyExpandedCP_Pear_Sapling"
            | "(O)FlashShifter.StardewValleyExpandedCP_Nectarine_Sapling"
            | "(O)FlashShifter.StardewValleyExpandedCP_Persimmon_Sapling"
            | "FlashShifter.StardewValleyExpandedCP_Tree_Coin" => true,
            _ => false,
        }
    }
}

impl Item {
    pub const PICKAXE: Item = Item::new_const("(T)Pickaxe");
    pub const AXE: Item = Item::new_const("(T)Axe");
    pub const SCYTHE: Item = Item::new_const("(W)47");
    pub const HOE: Item = Item::new_const("(T)Hoe");
    pub const WATERING_CAN: Item = Item::new_const("(T)WateringCan");

    pub const BAMBOO_POLE: Item = Item::new_const("(T)BambooPole");
    pub const FIBERGLASS_ROD: Item = Item::new_const("(T)FiberglassRod");
    pub const IRIDIUM_ROD: Item = Item::new_const("(T)IridiumRod");
    pub const BAIT: Item = Item::new_const("(O)685");

    pub const SALAD: Item = Item::new_const("(O)196");
    pub const PARSNIP_SEEDS: Item = Item::new_const("(O)472");
    pub const PARSNIP: Item = Item::new_const("(O)24");
    pub const CARROT_SEEDS: Item = Item::new_const("(O)CarrotSeeds");

    pub const CLAY: Item = Item::new_const("(O)330");
    pub const WOOD: Item = Item::new_const("(O)388");
    pub const STONE: Item = Item::new_const("(O)390");

    pub const OAK_SEED: Item = Item::new_const("(O)309");
    pub const MAPLE_SEED: Item = Item::new_const("(O)310");
    pub const PINE_SEED: Item = Item::new_const("(O)311");

    pub const CHEST: Item = Item::new_const("(BC)130");
    pub const FURNACE: Item = Item::new_const("(BC)13");

    pub const DAFFODIL: Item = Item::new_const("(O)18");

    pub const SEA_JELLY: Item = Item::new_const("(O)SeaJelly");
    pub const RIVER_JELLY: Item = Item::new_const("(O)RiverJelly");
    pub const CAVE_JELLY: Item = Item::new_const("(O)CaveJelly");

    pub const COPPER_ORE: Item = Item::new_const("(O)378");
    pub const IRON_ORE: Item = Item::new_const("(O)380");
    pub const GOLD_ORE: Item = Item::new_const("(O)384");
    pub const IRIDIUM_ORE: Item = Item::new_const("(O)386");
    pub const COAL: Item = Item::new_const("(O)382");

    pub const COPPER_BAR: Item = Item::new_const("(O)334");
    pub const IRON_BAR: Item = Item::new_const("(O)335");
    pub const GOLD_BAR: Item = Item::new_const("(O)336");
    pub const IRIDIUM_BAR: Item = Item::new_const("(O)337");

    pub const GEODE: Item = Item::new_const("(O)535");
    pub const FROZEN_GEODE: Item = Item::new_const("(O)536");
    pub const MAGMA_GEODE: Item = Item::new_const("(O)537");
    pub const OMNI_GEODE: Item = Item::new_const("(O)749");

    pub fn new(item_id: impl Into<Cow<'static, str>>) -> Self {
        let id = ItemId::new(item_id);
        id.into()
    }

    pub const fn new_const(item_id: &'static str) -> Self {
        Self {
            id: ItemId::new_const(item_id),
            count: 1,
            price: 0,
            edibility: -300,
            kind: None,
            category: None,
        }
    }

    pub fn quality(&self) -> Quality {
        self.id.quality
    }

    pub fn with_quality(self, quality: Quality) -> Self {
        Self {
            id: self.id.with_quality(quality),
            ..self
        }
    }

    pub fn with_count(self, count: usize) -> Self {
        Self { count, ..self }
    }

    pub fn with_category(self, category: Option<ItemCategory>) -> Self {
        Self { category, ..self }
    }

    pub fn with_price(self, price: i32) -> Self {
        Self { price, ..self }
    }

    pub fn with_edibility(self, edibility: i32) -> Self {
        Self { edibility, ..self }
    }

    pub fn with_item_kind(self, kind: Option<ItemKind>) -> Self {
        Self { kind, ..self }
    }

    pub fn stamina_recovery(&self) -> Option<f32> {
        if self.edibility == -300 {
            return None;
        }

        if self.is_same_item(&Item::SEA_JELLY)
            || self.is_same_item(&Item::RIVER_JELLY)
            || self.is_same_item(&Item::CAVE_JELLY)
        {
            // Temporary hard-coding to avoid eating supplies that are
            // needed for the Bait Maker.  In the future, eating the
            // jellies should be allowed, so long as either (1) at
            // least one BaitMaker has been crafted, (2) the storage
            // chests contain another instance of the same jelly, or
            // (3) the player contains another instance of the jelly.
            return None;
        }

        let base_recovery = (self.edibility as f32) * 2.5;
        let quality_multiplier = match self.quality() {
            Quality::Normal => 1.0,
            Quality::Silver => 1.4,
            Quality::Gold => 1.8,
            Quality::Iridium => 2.6,
        };
        Some(base_recovery * quality_multiplier)
    }

    pub fn gp_per_stamina(&self) -> Option<f32> {
        let stamina = self.stamina_recovery()?;
        let price = self.per_item_price() as f32;
        Some(price / stamina).filter(|&ratio| ratio > 0.0)
    }

    pub fn health_recovery(&self) -> Option<f32> {
        if self.edibility == -300 {
            None
        } else {
            Some((self.edibility as f32) * 1.125)
        }
    }

    pub fn is_same_item(&self, other: impl AsRef<ItemId>) -> bool {
        &self.id == other.as_ref()
    }

    pub fn as_watering_can(&self) -> Option<&WateringCan> {
        self.kind.as_ref().and_then(|kind| match kind {
            ItemKind::WateringCan(can) => Some(can),
            _ => None,
        })
    }

    pub fn as_fishing_rod(&self) -> Option<&FishingRod> {
        self.kind.as_ref().and_then(|kind| match kind {
            ItemKind::FishingRod(rod) => Some(rod),
            _ => None,
        })
    }

    pub fn as_weapon(&self) -> Option<&Weapon> {
        self.kind.as_ref().and_then(|kind| match kind {
            ItemKind::Weapon(weapon) => Some(weapon),
            _ => None,
        })
    }

    /// Returns true if this item-stack is full, false otherwise.
    ///
    /// TODO: Handle item types that are not allowed to stack.
    pub fn is_full_stack(&self) -> bool {
        self.count == 999
    }

    /// The price of each item, including the multiplier from quality
    pub fn per_item_price(&self) -> i32 {
        let base_price = self.price as f32;
        let price = base_price * self.quality().price_multiplier();
        price as i32
    }

    /// The total price of all items in the stack
    pub fn stack_price(&self) -> i32 {
        (self.count as i32) * self.per_item_price()
    }
}

impl Quality {
    pub fn is_normal(&self) -> bool {
        matches!(self, Self::Normal)
    }

    pub fn price_multiplier(&self) -> f32 {
        match self {
            Quality::Normal => 1.0,
            Quality::Silver => 1.25,
            Quality::Gold => 1.5,
            Quality::Iridium => 2.0,
        }
    }
}

impl TryFrom<i32> for Quality {
    type Error = Error;

    fn try_from(value: i32) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Self::Normal),
            1 => Ok(Self::Silver),
            2 => Ok(Self::Gold),
            4 => Ok(Self::Iridium),
            other => Err(Error::InvalidQualityValue(other)),
        }
    }
}

impl From<i32> for ItemCategory {
    fn from(value: i32) -> Self {
        match value {
            -99 => ItemCategory::Tool,
            -4 => ItemCategory::Fish,
            -74 => ItemCategory::Seed,
            -20 => ItemCategory::Junk,
            -15 => ItemCategory::Ore,
            other => ItemCategory::Other(other),
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

impl std::fmt::Display for ItemId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self { item_id, quality } = &self;
        if quality.is_normal() {
            write!(f, "{item_id}")
        } else {
            write!(f, "{quality} {item_id}")
        }
    }
}

impl std::fmt::Display for Item {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self { id, count, .. } = &self;
        if *count == 1 {
            write!(f, "{id}")
        } else {
            write!(f, "{count} {id}")
        }
    }
}

impl std::cmp::PartialEq<Item> for ItemId {
    fn eq(&self, other: &Item) -> bool {
        self == &other.id
    }
}

impl std::cmp::PartialEq<ItemId> for Item {
    fn eq(&self, other: &ItemId) -> bool {
        &self.id == other
    }
}

impl AsRef<ItemId> for Item {
    fn as_ref(&self) -> &ItemId {
        &self.id
    }
}

impl AsRef<ItemId> for ItemId {
    fn as_ref(&self) -> &ItemId {
        self
    }
}

impl From<ItemId> for Item {
    fn from(id: ItemId) -> Self {
        Self {
            id,
            count: 1,
            price: 0,
            edibility: -300,
            kind: None,
            category: None,
        }
    }
}
