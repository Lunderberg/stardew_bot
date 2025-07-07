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
    Weapon,
    Fruit,
    Greens,
    Flowers,
    Vegetable,
    Fish,
    SeaProduce,
    Seed,
    Junk,
    Ore,
    Gem,
    Mineral,
    Craftable,
    BigCraftable,
    Other(i32),
}

impl ItemId {
    pub const PICKAXE: Self = Self::new_const("(T)Pickaxe");
    pub const AXE: Self = Self::new_const("(T)Axe");
    pub const SCYTHE: Self = Self::new_const("(W)47");
    pub const HOE: Self = Self::new_const("(T)Hoe");
    pub const WATERING_CAN: Self = Self::new_const("(T)WateringCan");

    pub const BAMBOO_POLE: Self = Self::new_const("(T)BambooPole");
    pub const FIBERGLASS_ROD: Self = Self::new_const("(T)FiberglassRod");
    pub const IRIDIUM_ROD: Self = Self::new_const("(T)IridiumRod");
    pub const BAIT: Self = Self::new_const("(O)685");

    pub const SALAD: Self = Self::new_const("(O)196");
    pub const PARSNIP_SEEDS: Self = Self::new_const("(O)472");
    pub const PARSNIP: Self = Self::new_const("(O)24");
    pub const CARROT_SEEDS: Self = Self::new_const("(O)CarrotSeeds");
    pub const KALE_SEEDS: Self = Self::new_const("(O)477");

    pub const CLAY: Self = Self::new_const("(O)330");
    pub const WOOD: Self = Self::new_const("(O)388");
    pub const STONE: Self = Self::new_const("(O)390");
    pub const FIBER: Self = Self::new_const("(O)771");

    pub const OAK_SEED: Self = Self::new_const("(O)309");
    pub const MAPLE_SEED: Self = Self::new_const("(O)310");
    pub const PINE_SEED: Self = Self::new_const("(O)311");

    pub const SPRINKLER: Self = Self::new_const("(O)599");
    pub const SCARECROW: Self = Self::new_const("(BC)8");
    pub const CHEST: Self = Self::new_const("(BC)130");

    pub const FURNACE: Self = Self::new_const("(BC)13");
    pub const BAIT_MAKER: Self = Self::new_const("(BC)BaitMaker");

    pub const DAFFODIL: Self = Self::new_const("(O)18");
    pub const CORAL: Self = Self::new_const("(O)393");
    pub const SEA_URCHIN: Self = Self::new_const("(O)397");

    pub const SEA_JELLY: Self = Self::new_const("(O)SeaJelly");
    pub const RIVER_JELLY: Self = Self::new_const("(O)RiverJelly");
    pub const CAVE_JELLY: Self = Self::new_const("(O)CaveJelly");

    pub const COPPER_ORE: Self = Self::new_const("(O)378");
    pub const IRON_ORE: Self = Self::new_const("(O)380");
    pub const GOLD_ORE: Self = Self::new_const("(O)384");
    pub const IRIDIUM_ORE: Self = Self::new_const("(O)386");
    pub const COAL: Self = Self::new_const("(O)382");

    pub const COPPER_BAR: Self = Self::new_const("(O)334");
    pub const IRON_BAR: Self = Self::new_const("(O)335");
    pub const GOLD_BAR: Self = Self::new_const("(O)336");
    pub const IRIDIUM_BAR: Self = Self::new_const("(O)337");

    pub const GEODE: Self = Self::new_const("(O)535");
    pub const FROZEN_GEODE: Self = Self::new_const("(O)536");
    pub const MAGMA_GEODE: Self = Self::new_const("(O)537");
    pub const OMNI_GEODE: Self = Self::new_const("(O)749");

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

    pub const fn with_count(self, count: usize) -> Item {
        Item {
            id: self,
            count,
            price: 0,
            edibility: -300,
            kind: None,
            category: None,
        }
    }

    pub fn is_tree_seed(&self) -> bool {
        match self.item_id.as_ref() {
            "(O)309"
            | "(O)310"
            | "(O)311"
            | "(O)MossySeed"
            | "(O)FlashShifter.StardewValleyExpandedCP_Fir_Cone"
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

    pub fn iter_recipe(&self) -> Option<impl Iterator<Item = (ItemId, usize)>> {
        let items: [Option<(ItemId, usize)>; 3] = if self == &ItemId::SPRINKLER
        {
            [
                Some((ItemId::COPPER_BAR, 1)),
                Some((ItemId::IRON_BAR, 1)),
                None,
            ]
        } else if self == &ItemId::SCARECROW {
            [
                Some((ItemId::WOOD, 50)),
                Some((ItemId::COAL, 1)),
                Some((ItemId::FIBER, 20)),
            ]
        } else if self == &ItemId::BAIT_MAKER {
            [
                Some((ItemId::IRON_BAR, 3)),
                Some((ItemId::CORAL, 3)),
                Some((ItemId::SEA_URCHIN, 1)),
            ]
        } else {
            return None;
        };

        let iter = items.into_iter().flatten();
        Some(iter)
    }
}

impl Item {
    pub fn new(item_id: impl Into<Cow<'static, str>>) -> Self {
        let id = ItemId::new(item_id);
        id.into()
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

        if self.is_same_item(&ItemId::SEA_JELLY)
            || self.is_same_item(&ItemId::RIVER_JELLY)
            || self.is_same_item(&ItemId::CAVE_JELLY)
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
        if self
            .is_same_item(&ItemId::PARSNIP.clone().with_quality(Quality::Gold))
        {
            // Hack to avoid eating any gold-star parsnips harvested
            // on Day 5.  Eventually, should be replaced by a check on
            // the items remaining for any community center bundles.
            return None;
        }
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

impl std::fmt::Display for ItemCategory {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ItemCategory::Tool => write!(f, "Tool"),
            ItemCategory::Weapon => write!(f, "Weapon"),
            ItemCategory::Fruit => write!(f, "Fruit"),
            ItemCategory::Greens => write!(f, "Greens"),
            ItemCategory::Flowers => write!(f, "Flowers"),
            ItemCategory::Vegetable => write!(f, "Vegetables"),
            ItemCategory::Fish => write!(f, "Fish"),
            ItemCategory::SeaProduce => write!(f, "SeaProduce"),
            ItemCategory::Seed => write!(f, "Seed"),
            ItemCategory::Junk => write!(f, "Junk"),
            ItemCategory::Ore => write!(f, "Ore"),
            ItemCategory::Gem => write!(f, "Gem"),
            ItemCategory::Mineral => write!(f, "Mineral"),
            ItemCategory::Craftable => write!(f, "Craftable"),
            ItemCategory::BigCraftable => write!(f, "BigCraftable"),
            ItemCategory::Other(value) => write!(f, "OtherCategory({value})"),
        }
    }
}

impl From<i32> for ItemCategory {
    fn from(value: i32) -> Self {
        match value {
            -99 => ItemCategory::Tool,
            -98 => ItemCategory::Weapon,
            -79 => ItemCategory::Fruit,
            -81 => ItemCategory::Greens,
            -80 => ItemCategory::Flowers,
            -75 => ItemCategory::Vegetable,
            -4 => ItemCategory::Fish,
            -23 => ItemCategory::SeaProduce,
            -74 => ItemCategory::Seed,
            -20 => ItemCategory::Junk,
            -15 => ItemCategory::Ore,
            -12 => ItemCategory::Mineral,
            -2 => ItemCategory::Gem,
            -8 => ItemCategory::Craftable,
            -9 => ItemCategory::BigCraftable,
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
        id.with_count(1)
    }
}
