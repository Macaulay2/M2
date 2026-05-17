# `scc.h`, `scc-core.c`, `scc-core.h` — core data structures & runtime support

These three files define the **core types** the rest of `scc1` uses
internally (`scc.h` — used by `scc1` itself) and the **runtime
helpers** that every `scc1`-generated file links against
(`scc-core.{c,h}`).

Part of the [`c/` scc1 translator](README.md).

[← back to c/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## `scc.h` — the translator's own header

```c
/*		Copyright 1993 by Daniel R. Grayson		*/

#include "M2/config.h"
#include "compat.h"

#define EQUAL 0

#define new(type) (type *)getmem(sizeof(type))
```

`scc.h` is the umbrella header used **only inside the translator**.
It:

- Brings in [`compat.h`](file-compat.md) for portability shims.
- Brings in `M2/config.h` for `HAVE_*` defines.
- Defines `new(type)` as a typed `getmem` call (the translator's
  own GC-managed allocator).
- Pulls in `chk.h`, `cprint.h`, `dictionary.h`, `error.h`,
  `grammar.h`, `list.h`, `readfile.h`, `type.h` — basically every
  module in this directory.

After including `scc.h`, a `.c` file in this directory has the
full translator API available.

## `scc-core.{h,c}` — the generated-file runtime

A separate concern: `scc1`-generated `.c` / `.cpp` files include
`scc-core.h` (not `scc.h`) to get the small runtime they need —
GC allocator, primitive type defs, etc.

```c
/* this file gets included into each file created by scc1 */

#ifndef SCC_CORE_H
#define SCC_CORE_H

#include <M2/gc-include.h>

#if defined(__cplusplus)
...
```

Whereas:

```c
/* this file (or a replacement) gets linked into each program created using scc1 */
#include <scc-core.h>
#include <stdio.h>
#include <unistd.h>
#include <stdarg.h>
#define ERROR (-1)
#define STDERR 2
#define TRUE 1
```

is `scc-core.c` — implements the small runtime (error printer,
fatal, etc.) that the generated code may need.

## The two-tier `node` type

The single biggest type in `scc.h` is `node` — the AST node. It's
a tagged union with several dozen tags (`INT_CONST`,
`STRING_CONST`, `INSTRUCTION`, `BINARY_OP`, `TYPE`, etc.). Every
function in [`type.c`](file-type.md), [`chk.c`](file-chk.md),
[`cprint.c`](file-cprint.md) takes and returns `node`.

The two-tier split helps because:

- **`scc.h` `node`** is the translator's internal AST type with
  lots of cases.
- The **generated code** never sees `node`; it sees ordinary C
  types and the `scc-core.h` helpers.

## Used by

- Every `.c` file in `c/` (via `scc.h`).
- Every generated `.c` / `.cpp` (via `scc-core.h`).

## Related

- [`README.md`](README.md) — c/ overview.
- [`file-list.md`](file-list.md) — generic list helpers on `node`.
- [`file-type.md`](file-type.md) — type-system functions on `node`.
- [`file-compat.md`](file-compat.md) — portability shims.
