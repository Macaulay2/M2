# `main.cpp` — C++ `main()` for the interpreter binary

`main.cpp` is the **C++ entry point** for the M2 interpreter
binary — the `int main(int argc, char **argv)` function that
process startup actually calls. Most of its work is wiring; the
substantive logic happens in [`M2lib.c`](file-M2lib.md) and
[`interp.dd`](file-interp.md).

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```cpp
#define BOOST_STACKTRACE_USE_ADDR2LINE /* show source file and line number */
// #define BOOST_STACKTRACE_USE_NOOP /* disable stacktrace */

#include <M2/gc-include.h>

#include "interp-exports.h"
#include <interface/m2-types.h>

#include "M2mem.h"
#include "types.h"

#include <engine.h>                  /* to get IM2_initialize() */
#include "supervisorinterface.h"

#include <gdbm.h>
```

Two configuration toggles:

- **`BOOST_STACKTRACE_USE_ADDR2LINE`** — produce useful source-line
  info in crash reports (slower at crash time, much more helpful).
- **`BOOST_STACKTRACE_USE_NOOP`** — disable stacktrace entirely
  (commented out; would be used to debug stacktrace-related issues).

The includes pull in:

- **`gc-include.h`** — bdwgc startup.
- **`interp-exports.h`** — generated from `.d` files.
- **`engine.h`** — engine boundary (for `IM2_initialize()`).
- **`supervisorinterface.h`** — supervisor startup.
- **`gdbm.h`** — for documentation databases.

## `main()`'s job

The C++ `main`:

1. Initialise Boost.Stacktrace's crash handler.
2. Initialise bdwgc (`GC_INIT()`).
3. Initialise the engine (`IM2_initialize()` — see
   [`../e/file-engine-h.md`](../e/file-engine-h.md)).
4. Initialise the supervisor (start the worker pool).
5. Hand off to the interpreter entry point (a function imported
   from `interp.dd`).

After step 5, the interpreter is in charge until process exit.

## Why both `M2lib.c` (C) and `main.cpp` (C++)

The split is historical and reflects different needs:

- **`M2lib.c`** — older. Pure C for compatibility with
  `.d`-generated C glue. Handles signal setup, readline, longjmp
  targets.
- **`main.cpp`** — newer. C++ for crash-stack-trace support and
  modern initialisation idioms.

Today `main.cpp` is the actual entry; it calls into pieces of
`M2lib.c` for the C-only bits.

## Used by

The M2 binary's process entry — this is `main()`.

## Related

- [`README.md`](README.md) — d/ overview.
- [`file-M2lib.md`](file-M2lib.md) — C-side counterpart.
- [`file-interp.md`](file-interp.md) — the loop `main()` hands off to.
- [`../e/file-engine-h.md`](../e/file-engine-h.md) — `IM2_initialize`
  declaration.
- [`../system/README.md`](../system/README.md) — supervisor entry.
- [`../bin/file-main.md`](../bin/README.md) (when added) — the final
  `M2` binary linkage step.
