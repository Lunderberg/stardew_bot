Overview
--------

This is eventually intended to be a bot for Stardew Valley, but
currently is largely a learning exercise.  The goal is to make a bot
that can passively observe the memory-state of Stardew Valley, but
only interact through sending of keyboard and mouse events.

Current Functionality
---------------------

1. Identify the PID for `Stardew Valley`.
2. Display the memory for the `[stack]` memory region.
3. Follow pointers to other memory regions.
4. Identify pointers to executable symbols based on the ELF symbol
   table.

Debug Symbols
-------------

The `libcoreclr.so` library, part of the .NET runtime, does not ship
with debug symbols.  Nor are debug symbols part of the release
binaries available
[here](https://dotnet.microsoft.com/en-us/download/dotnet) contain the
debug symbols.  While there is an open issue on the Github project for
`dotnet` to provide `*-dbg` packages, no such packages are available
as of March 2024.

The supported way to get debug symbols for `libcoreclr.so`
([link](https://github.com/dotnet/diagnostics/blob/main/documentation/debugging-coredump.md))
is to first install `nuget`, then use it to install `dotnet-symbol`,
and finally run `dotnet-symbol` while pointing at a core dump using
the version of `libcoreclr.so` for which you need debug symbols.  Much
easier is to just download the debug symbols directly, located at
`http://msdl.microsoft.com/download/symbols/_.debug%2Felf-buildid-sym-${BUILD_ID}%2F_.debug`,
then rename it as `libcoreclr.so.dbg`.  The `BUILD_ID` can be
determined with `readelf --notes libcoreclr.so | grep "Build ID: "`.
