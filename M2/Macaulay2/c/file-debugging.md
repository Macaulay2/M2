# `debugging.c`, `debugging.h` — translator-side debug helpers

`debugging.c` is a tiny module providing **debug flags and the
`trap()` breakpoint hook** used while developing `scc1`.

Part of the [`c/` scc1 translator](README.md).

[← back to c/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's in the file

```c
#include "scc.h"
#include "debugging.h"
bool debug = FALSE;
int debugLevel = 0;

void trap() {}			/* set a breakpoint here */

int tty(){
   ...
}
```

Three things:

1. **`debug` flag** and **`debugLevel`** — global toggles.
2. **`trap()`** — an empty function used as a deliberate
   gdb/lldb breakpoint target. When something goes wrong, code
   calls `trap()`; the developer has a breakpoint set on
   `trap` and the debugger stops there.
3. **`tty()`** — returns whether stderr is a terminal (so
   coloured diagnostics can be enabled).

## Why an empty `trap()`?

The cleanest way to make "set a breakpoint here from gdb without
recompiling" work. Set `break trap` once at the start of your
gdb session; every call to `trap()` becomes a stop. Way better
than `__builtin_debugtrap` or `raise(SIGTRAP)`, which would
require recompiling and would also fire under non-debug runs.

## When `debug` / `debugLevel` matter

The flags gate verbose translator diagnostics:

- `debug = true` → print AST nodes during type-check.
- `debugLevel = 2` → also print scope contents per pass.
- Higher levels → more detail.

These aren't user-facing; they're toggled by editing
`debugging.c` and rebuilding `scc1` during translator
development.

## Used by

- Translator developers debugging `scc1` itself.
- [`file-chk.md`](file-chk.md) and [`file-cprint.md`](file-cprint.md)
  call `trap()` from internal-error paths.

## Related

- [`README.md`](README.md) — c/ overview.
- [`file-error.md`](file-error.md) — production-facing error
  reporting.
