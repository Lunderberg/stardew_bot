use std::collections::HashMap;

use dotnet_debugger::{
    Pointer, RustNativeObject, SymbolicGraph, SymbolicValue,
};

use crate::Error;

use super::{
    item::{ItemKind, WateringCan},
    FishingRod, Item, ItemId, Quality, Weapon, WeaponKind,
};

#[derive(RustNativeObject, Debug, Clone)]
pub struct Inventory {
    pub items: Vec<Option<Item>>,
}

impl Inventory {
    pub(crate) fn def_read_inventory(
        graph: &mut SymbolicGraph,
    ) -> Result<SymbolicValue, Error> {
        graph.named_native_function("new_inventory", |_: Pointer| {
            Inventory { items: Vec::new() }
        })?;

        graph.named_native_function(
            "new_watering_can_kind",
            |remaining_water: i32, max_water: i32| {
                ItemKind::WateringCan(WateringCan {
                    remaining_water,
                    max_water,
                })
            },
        )?;

        graph.named_native_function(
            "new_fishing_rod_kind",
            |num_attachment_slots: i32,
             bait: Option<&Item>,
             tackle: Option<&Item>| {
                let bait = bait.cloned().map(Box::new);
                let tackle = tackle.cloned().map(Box::new);
                ItemKind::FishingRod(FishingRod {
                    num_attachment_slots,
                    bait,
                    tackle,
                })
            },
        )?;

        graph.named_native_function(
            "new_weapon_kind",
            |kind: i32, min_damage: i32, max_damage: i32| {
                let kind = if kind == 1 {
                    WeaponKind::Dagger
                } else if kind == 2 {
                    WeaponKind::Club
                } else if kind == 3 {
                    WeaponKind::Sword
                } else {
                    // Not actually a dagger, but fallback in case
                    // there's a misread.
                    WeaponKind::Dagger
                };
                ItemKind::Weapon(Weapon {
                    kind,
                    min_damage,
                    max_damage,
                })
            },
        )?;

        graph.named_native_function(
            "new_item",
            |item_id: &str,
             subtype: Option<&str>,
             quality: i32,
             count: usize,
             price: i32,
             edibility: i32,
             category: Option<i32>| {
                let id = ItemId::new(item_id.to_string()).with_quality(
                    quality.try_into().unwrap_or(Quality::Normal),
                );
                let id = if let Some(subtype) = subtype {
                    let subtype = ItemId::new(format!("(O){subtype}"));
                    id.with_subtype(subtype)
                } else {
                    id
                };

                let item: Item = id.into();
                item.with_count(count)
                    .with_price(price)
                    .with_edibility(edibility)
                    .with_category(category.map(Into::into))
            },
        )?;

        graph.named_native_function(
            "item_with_kind",
            |item: &Item, kind: Option<&ItemKind>| -> Item {
                item.clone().with_item_kind(kind.cloned())
            },
        )?;

        graph.named_native_function(
            "add_item_to_inventory",
            |inventory: &mut Inventory, item: Option<&Item>| {
                inventory.items.push(item.cloned())
            },
        )?;

        let func = graph.parse(stringify! {
            fn read_base_item(item) {
                let item_id = item
                    ._qualifiedItemId
                    .read_string();

                let quality = item
                    .quality
                    .value;

                let count = item
                    .stack
                    .value;

                let object = item
                    .as::<StardewValley.Object>();

                let subtype = object
                    .preservedParentSheetIndex
                    .value
                    .read_string();

                let price = if object.is_some() {
                    object.price.value
                } else {
                    0i32
                };

                let edibility = if object.is_some() {
                    object.edibility.value
                } else {
                    -300i32
                };

                let category = if object.is_some() {
                    object.category.value
                } else {
                    None
                };

                new_item(
                    item_id,
                    subtype,
                    quality,
                    count,
                    price,
                    edibility,
                    category
                )
            }

            fn read_item(item) {
                let base_item = read_base_item(item);


                let watering_can = item
                    .as::<StardewValley.Tools.WateringCan>();
                let fishing_rod = item
                    .as::<StardewValley.Tools.FishingRod>();
                let weapon = item
                    .as::<StardewValley.Tools.MeleeWeapon>();

                let kind = if watering_can.is_some() {
                    let remaining_water = watering_can
                        .waterLeft
                        .value;
                    let max_water = watering_can
                        .waterCanMax;
                    new_watering_can_kind(
                        remaining_water,
                        max_water,
                    )

                } else if fishing_rod.is_some() {
                    let num_attachment_slots = fishing_rod
                        .numAttachmentSlots
                        .value;

                    let num_attachments = fishing_rod
                        .attachments
                        .elements
                        ._size
                        .prim_cast::<usize>();

                    let arr = fishing_rod
                        .attachments
                        .elements
                        ._items;

                    let bait = if num_attachments > 0 {
                        read_base_item(arr[0].value)
                    } else {
                        None
                    };

                    let tackle = if num_attachments > 1 {
                        read_base_item(arr[1].value)
                    } else {
                        None
                    };

                    new_fishing_rod_kind(
                        num_attachment_slots,
                        bait,
                        tackle,
                    )

                } else if weapon.is_some() {
                    let kind = weapon.type.value;
                    let min_damage = weapon.minDamage.value;
                    let max_damage = weapon.maxDamage.value;

                    new_weapon_kind(
                        kind,
                        min_damage,
                        max_damage,
                    )

                } else {
                    None
                };

                if kind.is_some() {
                    item_with_kind(base_item, kind)
                } else {
                    base_item
                }
            }

            fn read_inventory(container) {
                let items = container
                    .Items;

                let num_items = items
                    .count
                    .value
                    .prim_cast::<usize>();

                let arr = items
                    .array
                    .value
                    .elements
                    ._items;

                let inventory = (0..num_items)
                    .map(|i| {
                        let item = arr[i].value;
                        read_item(item)
                    })
                    .reduce(
                        new_inventory(container),
                        add_item_to_inventory,
                    );

                inventory
            }
        })?;

        Ok(func)
    }

    pub fn iter_slots(
        &self,
    ) -> impl DoubleEndedIterator<Item = Option<&Item>> + ExactSizeIterator + '_
    {
        self.items.iter().map(|item| item.as_ref())
    }

    pub fn iter_filled_slots(
        &self,
    ) -> impl DoubleEndedIterator<Item = (usize, &Item)> + '_ {
        self.iter_slots()
            .enumerate()
            .filter_map(|(slot, opt_item)| opt_item.map(|item| (slot, item)))
    }

    pub fn iter_items(&self) -> impl Iterator<Item = &Item> + '_ {
        self.iter_slots().flatten()
    }

    pub fn contains(&self, item: impl AsRef<ItemId>) -> bool {
        let item = item.as_ref();

        self.iter_items().any(|inv_item| inv_item == item)
    }

    pub fn worst_quality_of_type(
        &self,
        item: impl AsRef<ItemId>,
    ) -> Option<&ItemId> {
        let search_str = &*item.as_ref().item_id;

        self.iter_items()
            .map(|item| &item.id)
            .filter(|id| id.item_id == search_str)
            .min_by_key(|id| id.quality)
    }

    pub fn current_slot(&self, item: impl AsRef<ItemId>) -> Option<usize> {
        let item = item.as_ref();

        self.items
            .iter()
            .enumerate()
            .find(|(_, opt_inv_item)| {
                opt_inv_item
                    .as_ref()
                    .map(|inv_item| inv_item == item)
                    .unwrap_or(false)
            })
            .map(|(i, _)| i)
    }

    pub fn count_item(&self, item: impl AsRef<ItemId>) -> usize {
        let item = item.as_ref();
        self.iter_items()
            .filter(|inv_item| inv_item.is_same_item(item))
            .map(|inv_item| inv_item.count)
            .sum::<usize>()
    }

    pub fn empty_slot(&self) -> Option<usize> {
        self.iter_slots()
            .enumerate()
            .find(|(_, opt_item)| opt_item.is_none())
            .map(|(i, _)| i)
    }

    pub fn num_empty_slots(&self) -> usize {
        self.iter_slots()
            .filter(|opt_item| opt_item.is_none())
            .count()
    }

    pub fn has_empty_slot(&self) -> bool {
        self.iter_slots().any(|opt_item| opt_item.is_none())
    }

    /// Returns the preferred slot in which an item should be placed,
    /// or None if the item cannot be stored in the inventory.
    /// Typically used to select the inventory slot in which an item
    /// held in the cursor should be placed.
    pub fn preferred_slot(
        &self,
        new_item: impl AsRef<ItemId>,
    ) -> Option<usize> {
        let new_item = new_item.as_ref();
        self.iter_filled_slots()
            .find(|(_, item)| {
                item.is_same_item(new_item) && !item.is_full_stack()
            })
            .map(|(slot, _)| slot)
            .or_else(|| self.empty_slot())
    }

    /// Returns true if the item can be added to this inventory.
    ///
    /// If the inventory has an empty slot, or if the inventory has a
    /// non-full slot containing the item type, this method will
    /// return true.  Otherwise, will return false.
    pub fn can_add(&self, new_item: impl AsRef<ItemId>) -> bool {
        self.iter_slots().any(|opt_item| match opt_item {
            None => true,
            Some(inv_item) => {
                inv_item == new_item.as_ref() && !inv_item.is_full_stack()
            }
        })
    }

    /// Returns a lookup with the contents of this inventory
    ///
    /// If the same item occurs in multiple stack, the lookup contains
    /// the sum of all stacks.
    pub fn to_hash_map(&self) -> HashMap<ItemId, usize> {
        let mut contents = HashMap::new();
        for item in self.iter_items() {
            let count = contents.entry(item.id.clone()).or_default();
            *count += item.count;
        }
        contents
    }
}
