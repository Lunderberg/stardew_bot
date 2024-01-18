use std::{
    fs::File,
    ops::Range,
    path::{Path, PathBuf},
};

use elf::{endian::AnyEndian, ElfStream};

use crate::{Error, MemoryMapRegion, Pointer};

#[derive(Clone)]
pub struct Symbol {
    pub name: String,
    pub location: Range<Pointer>,
}

struct FileSymbol {
    name: String,
    location: Range<usize>,
}

impl Symbol {
    pub fn iter_symbols(
        region: &MemoryMapRegion,
    ) -> impl Iterator<Item = Symbol> {
        let region_address = region.mmap_start_address();
        region
            .name
            .clone()
            .into_iter()
            .map(|path| -> PathBuf { path.into() })
            .filter(|path| path.exists())
            .flat_map(|path| {
                FileSymbol::collect_symbols(path.clone()).into_iter()
            })
            .flatten()
            .map(move |file_symbol| {
                let FileSymbol { name, location } = file_symbol;
                let location = (region_address + location.start)
                    ..(region_address + location.end);
                Symbol { name, location }
            })
    }
}

impl FileSymbol {
    fn collect_symbols(path: impl AsRef<Path>) -> Result<Vec<Self>, Error> {
        // Unfortunately, the API for ElfStream requires extra
        // copying.  Ideally, this function would return an iterator,
        // rather than a Vec.  However, because the ElfStream caches
        // internally, the `symbol_table` and `dynamic_symbol_table`
        // methods take a mutable reference, the ElfStream needs to
        // outlive the iterator.  Because the ElfStream cannot be
        // moved into the iterator, the iterator must be collected
        // into a vector before the function ends.
        let file_obj = File::open(path)?;
        let mut elf = ElfStream::<AnyEndian, _>::open_stream(&file_obj)?;

        let mut symbols = Vec::new();

        for i in 0..2 {
            // This would be much cleaner to do as
            // [elf.symbol_table()?, elf.dynamic_symbol_table()?],
            // except that the lifetime of the returned SymbolTable is
            // the same as the lifetime of the mutable borrow of
            // `elf`.  So instead, it has to be in a loop.
            let Some((table, names)) = if i == 0 {
                elf.symbol_table()
            } else {
                elf.dynamic_symbol_table()
            }?
            else {
                continue;
            };
            let symbol_iter = table
                .into_iter()
                .filter(|symbol| !symbol.is_undefined())
                .filter(|symbol| symbol.st_name > 0)
                .map(move |symbol| -> Result<_, Error> {
                    let name = names.get(symbol.st_name as usize)?.to_string();
                    let start = symbol.st_value as usize;
                    let len = symbol.st_size as usize;
                    let location = start..start + len;
                    Ok(FileSymbol { name, location })
                });

            for symbol in symbol_iter {
                symbols.push(symbol?);
            }
        }
        Ok(symbols)
    }
}
