use std::num::NonZeroUsize;

use arrayvec::ArrayVec;
use itertools::Itertools as _;
use lru::LruCache;

use dotnet_debugger::{CachedReader, MethodTable};

use dsl_ir::{Pointer, TypedPointer};

use crate::Error;

pub trait Reader {
    fn read_bytes(
        &mut self,
        loc: Pointer,
        output: &mut [u8],
    ) -> Result<(), Error>;

    fn read_byte_regions(
        &mut self,
        regions: &mut [(Pointer, &mut [u8])],
    ) -> Result<(), Error> {
        for (ptr, buf) in regions.iter_mut() {
            self.read_bytes(*ptr, buf)?;
        }
        Ok(())
    }

    /// From the specified location, how many bytes are safe to read?
    /// Used for caching, when reading more bytes than requested may
    /// be beneficial, in case they can be returned for future
    /// requests.
    fn safe_read_extent(&self, loc: Pointer) -> std::ops::Range<Pointer> {
        loc..loc
    }

    fn with_cache(self) -> impl Reader
    where
        Self: Sized,
    {
        CachedVMReader::new(self)
    }

    // TODO: Remove the need for this method.  Would be better for
    // everything to be implemented/implementable in terms of
    // `read_bytes`.
    fn is_dotnet_base_class_of(
        &mut self,
        parent_class_ptr: TypedPointer<MethodTable>,
        child_class_ptr: TypedPointer<MethodTable>,
    ) -> Result<bool, Error>;
}

impl Reader for CachedReader<'_> {
    fn read_bytes(
        &mut self,
        loc: Pointer,
        output: &mut [u8],
    ) -> Result<(), Error> {
        let reader = self.underlying_reader();
        Ok(reader.read_exact(loc, output)?)
    }

    fn read_byte_regions(
        &mut self,
        regions: &mut [(Pointer, &mut [u8])],
    ) -> Result<(), Error> {
        let reader = self.underlying_reader();
        Ok(reader.read_regions(regions)?)
    }

    fn safe_read_extent(&self, loc: Pointer) -> std::ops::Range<Pointer> {
        let reader = self.underlying_reader();
        reader.find_containing_region_range(loc).unwrap_or(loc..loc)
    }

    fn is_dotnet_base_class_of(
        &mut self,
        parent_class_ptr: TypedPointer<MethodTable>,
        child_class_ptr: TypedPointer<MethodTable>,
    ) -> Result<bool, Error> {
        self.is_base_of(parent_class_ptr, child_class_ptr)
            .map_err(Into::into)
    }
}

pub struct DummyReader;
impl Reader for DummyReader {
    fn read_bytes(
        &mut self,
        _loc: Pointer,
        _output: &mut [u8],
    ) -> Result<(), Error> {
        Err(Error::ReadOccurredDuringLocalEvaluation)
    }

    fn is_dotnet_base_class_of(
        &mut self,
        _parent_class_ptr: TypedPointer<MethodTable>,
        _child_class_ptr: TypedPointer<MethodTable>,
    ) -> Result<bool, Error> {
        Err(Error::ReadOccurredDuringLocalEvaluation)
    }
}

const PAGE_SIZE: usize = 4096;
const NUM_CACHE_PAGES: usize = 1024;

struct CachedVMReader<Inner> {
    inner: Inner,

    /// The cached data
    cache: Vec<u8>,

    /// Lookup from remote address to index within the cache
    lru_cache: LruCache<Pointer, usize>,
}

impl<Inner> CachedVMReader<Inner> {
    fn new(inner: Inner) -> Self {
        let cache = vec![0; NUM_CACHE_PAGES * PAGE_SIZE];

        // Initialize the cache with dummy pointers, and valid indices.
        let mut lru_cache =
            LruCache::new(NonZeroUsize::new(NUM_CACHE_PAGES).unwrap());
        for i in 0..NUM_CACHE_PAGES {
            lru_cache.push(Pointer::null() + 1 + i, i * PAGE_SIZE);
        }

        Self {
            inner,
            cache,
            lru_cache,
        }
    }

    fn from_cache<'a>(
        lru_cache: &mut LruCache<Pointer, usize>,
        cache: &'a [u8],
        range: std::ops::Range<Pointer>,
    ) -> Option<(&'a [u8], &'a [u8])> {
        let page_start = range.start.prev_multiple_of(PAGE_SIZE);
        let page_end = range.end.next_multiple_of(PAGE_SIZE);

        let num_pages = (page_end - page_start) / PAGE_SIZE;

        let empty_slice = &[0u8; 0];

        match num_pages {
            0 => Some((empty_slice, empty_slice)),
            1 => {
                let page_index = lru_cache.get(&page_start)?;
                let cache_index = page_index + (range.start - page_start);
                let num_bytes = range.end - range.start;
                let from_cache = &cache[cache_index..cache_index + num_bytes];
                Some((from_cache, empty_slice))
            }
            2 => {
                let page_boundary = page_start + PAGE_SIZE;
                let num_bytes_page_0 = page_boundary - range.start;
                let num_bytes_page_1 = range.end - page_boundary;

                let page_0_start_index = *lru_cache.get(&page_start)?;
                let page_0_end_index = page_0_start_index + PAGE_SIZE;
                let cache_range_0 =
                    page_0_end_index - num_bytes_page_0..page_0_end_index;

                let page_1_start_index = *lru_cache.get(&page_boundary)?;
                let cache_range_1 =
                    page_1_start_index..page_1_start_index + num_bytes_page_1;

                let slice_0 = &cache[cache_range_0];
                let slice_1 = &cache[cache_range_1];

                Some((slice_0, slice_1))
            }
            _ => panic!(
                "Region in cached read may cross at most one page boundary, \
                 but pointer range from {} to {} consists of {} bytes, \
                 and would require reading from {} cached pages.",
                range.start,
                range.end,
                range.end - range.start,
                num_pages,
            ),
        }
    }

    fn choose_cache_range(
        requested_range: std::ops::Range<Pointer>,
    ) -> std::ops::Range<Pointer> {
        let page_start = requested_range.start.prev_multiple_of(PAGE_SIZE);
        let page_end = requested_range.end.next_multiple_of(PAGE_SIZE);
        page_start..page_end
    }

    fn iter_pages(
        region: std::ops::Range<Pointer>,
    ) -> impl Iterator<Item = Pointer> {
        let start = region.start.prev_multiple_of(PAGE_SIZE);
        let end = region.end;
        (0..)
            .map(move |i| start.checked_add(i * PAGE_SIZE))
            .take_while(|opt_ptr| opt_ptr.is_some())
            .map(|opt_ptr| opt_ptr.unwrap())
            .take_while(move |ptr| *ptr < end)
    }
}

impl<Inner> Reader for CachedVMReader<Inner>
where
    Inner: Reader,
{
    fn read_byte_regions(
        &mut self,
        regions: &mut [(Pointer, &mut [u8])],
    ) -> Result<(), Error> {
        assert_eq!(self.lru_cache.len(), NUM_CACHE_PAGES);

        const MAX_PAGES_PER_READ: usize = 16;
        let mut to_read =
            ArrayVec::<(Pointer, usize), MAX_PAGES_PER_READ>::new();
        const MAX_DELAYED_UPDATES: usize = 64;
        let mut update_after_read =
            ArrayVec::<usize, MAX_DELAYED_UPDATES>::new();

        const _: () = assert!(
            MAX_PAGES_PER_READ < NUM_CACHE_PAGES,
            "Cannot read more pages in a batched read \
             than are contained in the cache.."
        );

        macro_rules! flush {
            () => {{
                to_read.sort_by_key(|(_, index)| *index);

                {
                    let mut subregions = ArrayVec::<
                        (Pointer, &mut [u8]),
                        MAX_PAGES_PER_READ,
                    >::new();

                    let mut prev_index = None;
                    let mut remaining_slice = &mut self.cache[..];
                    for (page, index) in to_read.iter().cloned() {
                        let split = match prev_index {
                            Some(prev_index) => index - prev_index,
                            None => index + PAGE_SIZE,
                        };
                        assert!(split >= PAGE_SIZE);
                        assert!(
                            split <= remaining_slice.len(),
                            "Split location {split}, \
                             derived from index={index} \
                             and prev_index={prev_index:?}, \
                             exceeds remaining length {}.  \
                             Initial length {} has been consumed at indices [{}].",
                            remaining_slice.len(),
                            100usize*PAGE_SIZE,
                            to_read.iter().map(|(_,index)| *index).format(", "),
                        );
                        let (left, right) = remaining_slice.split_at_mut(split);
                        let page_slice = &mut left[split-PAGE_SIZE..];
                        subregions.push((page, page_slice));
                        remaining_slice = right;
                        prev_index = Some(index);
                    }

                    self.inner.read_byte_regions(&mut subregions)?;
                }

                for (page, index) in to_read.iter().cloned() {
                    let evicted = self.lru_cache.push(page, index);
                    assert!(
                        evicted.is_none(),
                        "No evictions should be required, \
                         since this only contains pages not in cache, \
                         and LRU was already popped to decide the index.  \
                         Pages in current batch = [{}].",
                        to_read.iter().map(|(page,_)| *page)
                            .counts()
                            .into_iter()
                            .sorted_by_key(|(page,counts)| (std::cmp::Reverse(*counts),*page))
                            .map(|(page,counts)| format!("{page}: {counts}"))
                            .format(", ")
                    );
                }

                for i in update_after_read.iter().cloned() {
                    let loc = regions[i].0;
                    let output = &mut regions[i].1;
                    let remote_range = loc..loc + output.len();
                    let (cache_a, cache_b) = Self::from_cache(
                        &mut self.lru_cache,
                        &self.cache,
                        remote_range.clone(),
                    )
                    .expect("Cache entry should be populated");
                    output[..cache_a.len()].copy_from_slice(cache_a);
                    output[cache_a.len()..].copy_from_slice(cache_b);
                }

                to_read.clear();
                update_after_read.clear();
            }};
        }

        for i in 0..regions.len() {
            // Looping over indices rather than `regions.iter_mut()`,
            // because ownership of `regions` needs to be dropped
            // whenever flushing out the collected
            // `update_after_read`.
            let loc = regions[i].0;
            let output = &mut regions[i].1;
            let remote_range = loc..loc + output.len();
            if let Some((cache_a, cache_b)) = Self::from_cache(
                &mut self.lru_cache,
                &self.cache,
                remote_range.clone(),
            ) {
                output[..cache_a.len()].copy_from_slice(cache_a);
                output[cache_a.len()..].copy_from_slice(cache_b);
                continue;
            }

            for page in Self::iter_pages(remote_range.clone()) {
                let already_in_next_batch =
                    to_read.iter().any(|(queued_page, _)| *queued_page == page);
                if !already_in_next_batch {
                    let index = self
                        .lru_cache
                        .pop_lru()
                        .map(|(_, index)| index)
                        .expect("Cache should never be empty");
                    to_read.push((page, index));
                }
            }
            update_after_read.push(i);

            if to_read.len() + 1 >= to_read.capacity()
                || update_after_read.len() == update_after_read.capacity()
            {
                flush!();
                assert_eq!(self.lru_cache.len(), NUM_CACHE_PAGES);
            }
        }

        flush!();
        assert_eq!(self.lru_cache.len(), NUM_CACHE_PAGES);

        Ok(())
    }

    fn read_bytes(
        &mut self,
        loc: Pointer,
        output: &mut [u8],
    ) -> Result<(), Error> {
        let range_end = loc
            .checked_add(output.len())
            .ok_or_else(|| Error::InvalidPointerAddition(loc, output.len()))?;
        let remote_range = loc..range_end;
        let cache_range = Self::choose_cache_range(remote_range.clone());
        let num_pages = (cache_range.end - cache_range.start) / PAGE_SIZE;

        if num_pages > self.lru_cache.len().min(2) {
            // This single read would exceed the capacity of the
            // entire cache (e.g. a large string), or would exceed the
            // size of a single cached read.  For a read occupies
            // several entire page, it probably wouldn't be shared by
            // other objects, so we can just let it skip the cache
            // altogether.
            return self.inner.read_bytes(loc, output);
        }

        for page in Self::iter_pages(remote_range.clone()) {
            self.lru_cache.promote(&page);
        }

        assert!(
            num_pages <= self.lru_cache.len(),
            "Read of {} bytes would use {num_pages} pages, \
             more than are the {} pages in the cache.",
            output.len(),
            self.lru_cache.len(),
        );

        for page in Self::iter_pages(remote_range.clone()) {
            if self.lru_cache.contains(&page) {
                continue;
            }

            let index = self
                .lru_cache
                .pop_lru()
                .map(|(_, index)| index)
                .expect("Cache should never be empty");

            let res = self
                .inner
                .read_bytes(page, &mut self.cache[index..index + PAGE_SIZE]);

            if res.is_ok() {
                self.lru_cache.push(page, index);
            } else {
                self.lru_cache.push(page + 1, index);
                self.lru_cache.demote(&(page + 1));
            }

            res?;
        }

        let (cache_a, cache_b) = Self::from_cache(
            &mut self.lru_cache,
            &self.cache,
            remote_range.clone(),
        )
        .unwrap_or_else(|| {
            panic!(
                "Cache should be populated at this point, \
                     but doesn't contain {}-{}.",
                remote_range.start, remote_range.end,
            )
        });

        output[..cache_a.len()].copy_from_slice(cache_a);
        output[cache_a.len()..].copy_from_slice(cache_b);
        Ok(())
    }

    fn is_dotnet_base_class_of(
        &mut self,
        parent_class_ptr: TypedPointer<MethodTable>,
        child_class_ptr: TypedPointer<MethodTable>,
    ) -> Result<bool, Error> {
        self.inner
            .is_dotnet_base_class_of(parent_class_ptr, child_class_ptr)
    }
}
