# `main.cpp` — the `M2` executable's entry point

`main.cpp` is **`int main()` for the `M2` binary** — the final
link target after the engine + interpreter come together.
Initialises GC, the engine, the supervisor, the interpreter, and
hands off to `interp_topLevel`.

Part of the [`bin/` directory](README.md).

[← bin/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Top of the file

```cpp
#define BOOST_STACKTRACE_USE_ADDR2LINE /* show source file and line number */
// #define BOOST_STACKTRACE_USE_NOOP /* disable stacktrace */

#include <M2/gc-include.h>

#include "interp-exports.h"
#include <interface/m2-types.h>

#include "M2mem.h"
#include "types.h"

#include <engine.h> /* to get IM2_initialize() : */
#include "supervisorinterface.h"

#include <gdbm.h>
#include <mpfr.h>

#include <boost/stacktrace.hpp>
#include <atomic>
#include <chrono>
#include <fstream>
#include <iostream>
#include <string>
#include <thread>
#include <vector>
```

The include block is essentially a map of what `main.cpp` wires up:

- **`gc-include.h`** — Boehm-Demers-Weiser garbage collector.
- **`interp-exports.h`** — interpreter's public symbols
  (`interp_topLevel`).
- **`engine.h`** — `IM2_initialize()`.
- **`supervisorinterface.h`** — thread supervisor.
- **`<boost/stacktrace.hpp>`** — for crash backtraces.
- **`<gdbm.h>`, `<mpfr.h>`** — library version checks at startup.

## Boot sequence

The order things happen in `main`:

1. **GC init** — `GC_INIT()` before anything else (touches
   `malloc`).
2. **Library version check** — assert MPFR / GMP / FLINT versions
   match what M2 was built against.
3. **Engine init** — `IM2_initialize()` (sets up global rings).
4. **Supervisor init** — set up the thread pool.
5. **Crash handler** — install `boost::stacktrace`-based handler
   so segfaults dump useful traces.
6. **Interpreter init** — `interp_init()`.
7. **Top-level loop** — `interp_topLevel(argc, argv)`.
8. **Cleanup on return**.

## Why a stacktrace handler?

The conditional defines at the top:

```cpp
#define BOOST_STACKTRACE_USE_ADDR2LINE /* show source file and line number */
// #define BOOST_STACKTRACE_USE_NOOP /* disable stacktrace */
```

`addr2line` gives **source-file-and-line-number** resolution.
Without it, a crash dump would show just hex addresses — useless
for diagnosis. With it, you get something like:

```
./Macaulay2/e/matrix.cpp(123): Matrix::do_thing(...)
./Macaulay2/e/poly.cpp(456): PolyRing::reduce(...)
...
```

The `BOOST_STACKTRACE_USE_NOOP` commented line is the no-op
fallback — useful only when you want to ship a no-stacktrace
build for some reason.

## Why C++ for `main.cpp`?

Most of the engine is C++; the supervisor wants C++ for `std::thread`
/ `std::atomic`; Boost stacktrace is C++. `main.cpp` needs to
weave them together — pure C would be much more painful.

## Used by

- The linker, to produce the final `M2` (or `M2-binary`)
  executable.

## Related

- [`README.md`](README.md) — bin/ overview.
- [`file-timestamp.md`](file-timestamp.md) — sister build-info
  symbol.
- [`file-startup.md`](file-startup.md) — generated startup table.
- [`../d/file-M2lib.md`](../d/file-M2lib.md) — `M2lib.c`
  alternative entry point used in some configurations.
- [`../e/file-engine-h.md`](../e/file-engine-h.md) —
  `IM2_initialize`.
- [`../system/file-supervisor.md`](../system/file-supervisor.md) —
  supervisor wired up here.
