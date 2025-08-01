use std::{collections::HashMap, sync::LazyLock};

use geometry::Vector;
use itertools::Itertools as _;

use game_state::{
    BundleIngredient, GameState, Item, ItemCategory, ItemId, Location,
    ObjectKind, Quality, ResourceClumpKind, ShopMenu, StaticState,
};

use crate::{
    best_weapon, bot_logic::ActionCollector, Error, GameAction, MovementGoal,
    Pathfinding,
};

pub trait GameStateExt {
    fn get_farm_door(&self) -> Result<Vector<isize>, Error>;

    fn get_mine_elevator(&self) -> Result<Vector<isize>, Error>;

    fn iter_accessible_items(
        &self,
    ) -> Result<impl Iterator<Item = &Item> + '_, Error>;

    fn iter_stored_items(
        &self,
        loc: &str,
    ) -> Result<impl Iterator<Item = &Item> + '_, Error>;

    fn iter_bundle_items(
        &self,
    ) -> Result<impl Iterator<Item = (&str, &ItemId, usize)> + '_, Error>;

    fn iter_reserved_items(
        &self,
    ) -> Result<impl Iterator<Item = (&ItemId, usize)> + '_, Error>;

    fn closest_entrance(
        &self,
        target_room: &str,
    ) -> Result<Vector<isize>, Error>;

    fn collect_clearable_tiles(
        &self,
    ) -> Result<HashMap<Vector<isize>, Option<ItemId>>, Error>;

    fn current_axe(&self) -> Result<Option<&ItemId>, Error>;
}

static RESERVED_FOR_GIFTS: LazyLock<[(ItemId, usize); 1]> =
    LazyLock::new(|| [(ItemId::PARSNIP.with_quality(Quality::Gold), 2)]);

impl GameStateExt for GameState {
    fn get_farm_door(&self) -> Result<Vector<isize>, Error> {
        let farm = self.get_room("Farm")?;
        let farm_door = farm
            .buildings
            .iter()
            .find_map(|building| {
                building
                    .door
                    .as_ref()
                    .filter(|door| door.inside_name == "FarmHouse")
                    .map(|door| {
                        building.shape.top_left + door.relative_location
                    })
            })
            .ok_or_else(|| Error::FarmhouseDoorNotFound)?;

        Ok(farm_door)
    }

    fn get_mine_elevator(&self) -> Result<Vector<isize>, Error> {
        let mine = self.get_room("Mine")?;
        let elevator = mine
            .action_tiles
            .iter()
            .find(|(_, action)| action == "MineElevator")
            .map(|(tile, _)| *tile)
            .ok_or_else(|| Error::MineElevatorNotFound)?;

        Ok(elevator)
    }

    fn iter_stored_items(
        &self,
        loc: &str,
    ) -> Result<impl Iterator<Item = &Item> + '_, Error> {
        let iter = self
            .get_room(loc)?
            .objects
            .iter()
            .filter_map(|obj| match &obj.kind {
                ObjectKind::Chest(chest) => Some(&chest.inventory),
                _ => None,
            })
            .flat_map(|inventory| inventory.iter_items());

        Ok(iter)
    }

    fn iter_accessible_items(
        &self,
    ) -> Result<impl Iterator<Item = &Item> + '_, Error> {
        let iter = self
            .player
            .inventory
            .iter_items()
            .chain(self.iter_stored_items("Farm")?);

        Ok(iter)
    }

    fn iter_bundle_items(
        &self,
    ) -> Result<impl Iterator<Item = (&str, &ItemId, usize)> + '_, Error> {
        let iter = self
            .statics
            .bundles
            .iter()
            .filter_map(|bundle| {
                let flags = self.globals.bundles.get(&bundle.bundle_index)?;

                let num_done = flags.iter().map(|b| *b as usize).sum::<usize>();
                (num_done < bundle.num_required).then(|| {
                    bundle
                        .ingredients
                        .iter()
                        .zip(flags)
                        .filter(|(_, done)| !**done)
                        .filter_map(|(ingredient, _)| match ingredient {
                            BundleIngredient::Item(item) => Some(item),
                            BundleIngredient::Gold(_) => None,
                        })
                        .map(|item| {
                            (bundle.name.as_str(), &item.id, item.count)
                        })
                })
            })
            .flatten();

        Ok(iter)
    }

    fn iter_reserved_items(
        &self,
    ) -> Result<impl Iterator<Item = (&ItemId, usize)> + '_, Error> {
        // On most days, avoid eating items that are reserved for
        // bundles, or for planned crafting in the future.  On days
        // 1/2, any fish and forage may be eaten, regardless of
        // whether we'll need it later, because stamina/inventory
        // management are much tighter.
        let is_early_startup = self.globals.days_played() <= 2;

        let iter_worst_fish = self
            .iter_accessible_items()?
            .filter(|item| matches!(item.category, Some(ItemCategory::Fish)))
            .map(|item| (&item.id.item_id, item))
            .into_grouping_map()
            .min_by_key(|_, item| item.quality())
            .into_iter()
            .map(|(_, item)| (&item.id, 1));

        let iter_bundles =
            self.iter_bundle_items()?.map(|(_, id, count)| (id, count));

        let iter_gifts =
            RESERVED_FOR_GIFTS.iter().map(|(id, count)| (id, *count));

        let iter_reserved = iter_worst_fish
            .chain(iter_bundles)
            .chain(iter_gifts)
            .filter(move |_| !is_early_startup);

        Ok(iter_reserved)
    }

    fn closest_entrance(
        &self,
        target_room: &str,
    ) -> Result<Vector<isize>, Error> {
        // Workaround since pathfinding doesn't currently know how
        // to return from the underground mines.
        if target_room == "Farm"
            && self.player.room_name.starts_with("UndergroundMine")
        {
            return self.get_farm_door();
        }
        MovementGoal::closest_entrance(self, target_room)
    }

    fn collect_clearable_tiles(
        &self,
    ) -> Result<HashMap<Vector<isize>, Option<ItemId>>, Error> {
        let current_room = self.current_room()?;
        let opt_weapon = best_weapon(self.player.inventory.iter_items())
            .map(|item| &item.id);

        current_room.collect_clearable_tiles(opt_weapon)
    }

    fn current_axe(&self) -> Result<Option<&ItemId>, Error> {
        let opt_axe =
            self.iter_accessible_items()?
                .map(|item| &item.id)
                .find(|&id| {
                    id == &ItemId::AXE
                        || id == &ItemId::COPPER_AXE
                        || id == &ItemId::IRON_AXE
                        || id == &ItemId::GOLD_AXE
                        || id == &ItemId::IRIDIUM_AXE
                });
        Ok(opt_axe)
    }
}

pub trait ObjectKindExt {
    fn get_tool(&self) -> Option<ItemId>;
}

impl ObjectKindExt for ObjectKind {
    fn get_tool(&self) -> Option<ItemId> {
        match self {
            ObjectKind::Stone(_)
            | ObjectKind::Torch
            | ObjectKind::Sprinkler(_)
            | ObjectKind::Scarecrow => Some(ItemId::PICKAXE),

            ObjectKind::Mineral(_) => None,
            ObjectKind::Wood => Some(ItemId::AXE),
            ObjectKind::Tree(tree) => (tree.health > 0.0).then(|| ItemId::AXE),

            ObjectKind::MineBarrel | ObjectKind::Fiber | ObjectKind::Grass => {
                Some(ItemId::SCYTHE)
            }

            ObjectKind::ArtifactSpot | ObjectKind::SeedSpot => {
                Some(ItemId::HOE)
            }

            ObjectKind::MineLadderUp
            | ObjectKind::MineLadderDown
            | ObjectKind::MineHoleDown
            | ObjectKind::MineElevator
            | ObjectKind::MineCartCoal
            | ObjectKind::PotOfGold
            | ObjectKind::FruitTree(_)
            | ObjectKind::HoeDirt(_)
            | ObjectKind::Chest(_)
            | ObjectKind::CraftingMachine(_)
            | ObjectKind::Other { .. }
            | ObjectKind::Unknown => None,
        }
    }
}

pub trait LocationExt {
    fn generate_tile_lookup(&self) -> HashMap<Vector<isize>, &ObjectKind>;

    #[allow(dead_code)]
    fn iter_planted_seeds(&self) -> impl Iterator<Item = &ItemId>;

    fn iter_stored_items(&self) -> impl Iterator<Item = &Item>;

    fn collect_clearable_tiles(
        &self,
        opt_weapon: Option<&ItemId>,
    ) -> Result<HashMap<Vector<isize>, Option<ItemId>>, Error>;

    fn pathfinding<'a>(&'a self, statics: &'a StaticState) -> Pathfinding<'a>;
}
impl LocationExt for Location {
    fn pathfinding<'a>(&'a self, statics: &'a StaticState) -> Pathfinding<'a> {
        Pathfinding::new(self, statics)
    }

    fn generate_tile_lookup(&self) -> HashMap<Vector<isize>, &ObjectKind> {
        self.objects
            .iter()
            .sorted_by_key(|obj| {
                // Since some objects may be placed on top of hoe dirt
                // or grass, collect these object types first.  The
                // object overtop will then overwrite them in the
                // HashMap.
                match &obj.kind {
                    ObjectKind::Grass => 0,
                    ObjectKind::HoeDirt(_) => 1,
                    _ => 2,
                }
            })
            .map(|obj| (obj.tile, &obj.kind))
            .collect()
    }

    fn iter_planted_seeds(&self) -> impl Iterator<Item = &ItemId> {
        self.objects
            .iter()
            .filter_map(|obj| obj.kind.as_hoe_dirt())
            .filter_map(|hoe_dirt| hoe_dirt.crop.as_ref())
            .map(|crop| &crop.seed)
    }

    fn iter_stored_items(&self) -> impl Iterator<Item = &Item> {
        self.objects
            .iter()
            .filter_map(|obj| obj.kind.as_chest())
            .flat_map(|chest| chest.iter_items())
    }

    fn collect_clearable_tiles(
        &self,
        opt_weapon: Option<&ItemId>,
    ) -> Result<HashMap<Vector<isize>, Option<ItemId>>, Error> {
        let iter_clearable_obj = self.objects.iter().filter_map(|obj| {
            let opt_tool = match &obj.kind {
                ObjectKind::Stone(_) => Some(ItemId::PICKAXE),
                ObjectKind::Wood => Some(ItemId::AXE),

                ObjectKind::Fiber
                | ObjectKind::Grass
                | ObjectKind::MineBarrel
                    if opt_weapon.is_some() =>
                {
                    opt_weapon.cloned()
                }
                ObjectKind::Chest(_)
                | ObjectKind::MineCartCoal
                | ObjectKind::Mineral(_) => None,

                other if other.is_forage() => None,

                _ => {
                    return None;
                }
            };
            Some((obj.tile, opt_tool))
        });
        let iter_clearable_clump = self
            .resource_clumps
            .iter()
            .filter_map(|clump| {
                let tool = match &clump.kind {
                    ResourceClumpKind::MineBoulder => Some(ItemId::PICKAXE),
                    _ => None,
                }?;
                Some(
                    clump
                        .shape
                        .iter_points()
                        .map(move |tile| (tile, Some(tool.clone()))),
                )
            })
            .flatten();

        let clearable_tiles =
            iter_clearable_obj.chain(iter_clearable_clump).collect();

        Ok(clearable_tiles)
    }
}

pub trait ShopMenuExt {
    fn do_menu_navigation(
        &self,
        actions: &mut ActionCollector,
        item: &ItemId,
    ) -> Result<(), Error>;
}
impl ShopMenuExt for ShopMenu {
    fn do_menu_navigation(
        &self,
        actions: &mut ActionCollector,
        item: &ItemId,
    ) -> Result<(), Error> {
        let to_buy_index = self
            .for_sale
            .iter()
            .enumerate()
            .find(|(_, for_sale)| &for_sale.id == item)
            .map(|(i, _)| i)
            .ok_or_else(|| Error::ItemNotSold {
                merchant: self.shop_name.clone(),
                item: item.clone(),
            })?;
        if self.visible_items().contains(&to_buy_index) {
            let button_index = to_buy_index
                .checked_sub(self.for_sale_scroll_index)
                .expect("Guarded by menu.visible_items().contains");
            let pixel = self.for_sale_buttons[button_index];

            actions.do_action(GameAction::MouseOverPixel(pixel));
            actions.do_action(GameAction::LeftClick);
        } else if to_buy_index > self.for_sale_scroll_index {
            actions.do_action(GameAction::ScrollDown);
        } else {
            actions.do_action(GameAction::ScrollUp);
        }

        Ok(())
    }
}

mod detail {
    use super::*;

    pub trait ItemOrItemRef {
        fn item_id(self) -> ItemId;
        fn item_id_ref(&self) -> &ItemId;
        fn count(&self) -> usize;
    }
    impl ItemOrItemRef for Item {
        fn item_id(self) -> ItemId {
            self.id
        }
        fn item_id_ref(&self) -> &ItemId {
            &self.id
        }
        fn count(&self) -> usize {
            self.count
        }
    }
    impl ItemOrItemRef for &Item {
        fn item_id(self) -> ItemId {
            self.id.clone()
        }
        fn item_id_ref(&self) -> &ItemId {
            &self.id
        }
        fn count(&self) -> usize {
            self.count
        }
    }
    impl ItemOrItemRef for (&ItemId, usize) {
        fn item_id(self) -> ItemId {
            self.0.clone()
        }

        fn item_id_ref(&self) -> &ItemId {
            self.0
        }

        fn count(&self) -> usize {
            self.1
        }
    }
    impl ItemOrItemRef for (ItemId, usize) {
        fn item_id(self) -> ItemId {
            self.0
        }

        fn item_id_ref(&self) -> &ItemId {
            &self.0
        }

        fn count(&self) -> usize {
            self.1
        }
    }
}

pub trait ItemIterExt {
    /// Collect the iterator into a lookup of the number of each item
    /// available.  Items with different qualities are collected under
    /// separate entries.
    fn item_counts(self) -> HashMap<ItemId, usize>;
}

impl<Iter> ItemIterExt for Iter
where
    Iter: Iterator,
    <Iter as Iterator>::Item: detail::ItemOrItemRef,
{
    fn item_counts(self) -> HashMap<ItemId, usize> {
        use detail::ItemOrItemRef;

        let mut counts = HashMap::new();
        for item in self {
            if let Some(prev) = counts.get_mut(item.item_id_ref()) {
                *prev += item.count();
            } else {
                let count = item.count();
                counts.insert(item.item_id(), count);
            }
        }

        counts
    }
}

pub trait ItemLookupExt {
    /// Returns the number of items that exactly match the specified
    /// item id.
    fn item_count_with_exact_quality(&self, id: &ItemId) -> usize;

    /// Returns the number of items that match the id, and meet or
    /// exceed the specified item quality.
    fn item_count(&self, id: &ItemId) -> usize;

    /// Returns the items that should be used for the bundle.  Because
    /// the bundle may require items of multiple qualities in order to
    /// be completed, this may require more than one item stack.
    /// (e.g. "10 Wheat" being fulfilled by 5 normal-quality Wheat and
    /// 5 silver-star Wheat.)
    fn items_with_quality(&self, item: &Item) -> [Item; 4];

    /// Iterate over the items necessary to provide the specified item/count, with quality being at least
    fn iter_items_with_quality(
        &self,
        item: &Item,
    ) -> impl Iterator<Item = Item> + use<Self>;

    /// Remove an item from this lookup.
    fn remove_item(&mut self, id: &ItemId, count: usize);
}
impl ItemLookupExt for HashMap<ItemId, usize> {
    fn item_count(&self, id: &ItemId) -> usize {
        Quality::iter()
            .filter(|quality| quality >= &id.quality)
            .map(|quality| {
                self.item_count_with_exact_quality(
                    &id.clone().with_quality(quality),
                )
            })
            .sum()
    }

    fn items_with_quality(&self, item: &Item) -> [Item; 4] {
        let mut num_remaining = item.count;
        let mut outputs = [
            Quality::Normal,
            Quality::Silver,
            Quality::Gold,
            Quality::Iridium,
        ]
        .map(|quality| item.id.clone().with_quality(quality).with_count(1));

        for output in &mut outputs {
            let count = self
                .item_count_with_exact_quality(&output.id)
                .min(num_remaining);
            output.count = count;
            num_remaining -= count;
        }

        outputs
    }

    fn iter_items_with_quality(
        &self,
        item: &Item,
    ) -> impl Iterator<Item = Item> + use<> {
        self.items_with_quality(item)
            .into_iter()
            .filter(|item| item.count > 0)
    }

    fn item_count_with_exact_quality(&self, id: &ItemId) -> usize {
        self.get(id).cloned().unwrap_or(0)
    }

    fn remove_item(&mut self, id: &ItemId, count: usize) {
        if let Some(prev) = self.get_mut(id) {
            *prev = prev.saturating_sub(count);
            if *prev == 0 {
                self.remove(id);
            }
        }
    }
}
