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
        let start_of_file = region.mmap_start_address();
        region
            .name
            .clone()
            .into_iter()
            .map(|path| -> PathBuf { path.into() })
            .flat_map(|path| {
                // Technically, I should be determining the path to
                // the .dbg file based on the contents of the
                // .gnu_debuglink section (from command line, `readelf
                // --string-dump=.gnu_debuglink $FILENAME`).  But this
                // is good enough for now.
                let dbg_path = path.file_name().map(|file_name| {
                    let mut file_name = file_name.to_owned();
                    file_name.push(".dbg");
                    path.with_file_name(file_name)
                });
                [Some(path), dbg_path]
            })
            .flatten()
            // Remove paths that don't exist.  Note: For
            // `libcoreclr.so`, the official route debug symbols is
            // really convoluted, and requires installing nuget, using
            // to install `dotnet-symbol`, then using it to download
            // the debug symbols.  Much easier is to just determine
            // the URL in bash.
            //
            //     BUILD_ID=$(readelf --notes libcoreclr.so | grep Build | awk '{print $3;}')
            //     URL=http://msdl.microsoft.com/download/symbols/_.debug/elf-buildid-sym-${BUILD_ID}/_.debug
            //     wget $URL -O libcoreclr.so.dbg
            .filter(|path| path.exists())
            .flat_map(|path| FileSymbol::collect_symbols(path.clone()))
            .flatten()
            .map(move |file_symbol| {
                let FileSymbol { name, location } = file_symbol;
                let location = (start_of_file + location.start)
                    ..(start_of_file + location.end);
                Symbol { name, location }
            })
            .map(|symbol| {
                let name = match cpp_demangle::Symbol::new(&symbol.name) {
                    Ok(demangled) => format!("{demangled}"),
                    Err(_) => symbol.name,
                };
                Symbol { name, ..symbol }
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
