# `equality.dd` — engine-aware equality

`equality.dd` implements **`a == b`** for the full range of M2
types — including the engine boundary cases (RawMatrix,
RawFreeModule, RawMonomialIdeal) that need to call into the C++
engine to compare.

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```d
--		Copyright 1994,2010 by Daniel R. Grayson
use util;
use tokens;

header "// required for equality checks
#include <interface/freemodule.h>           // for IM2_FreeModule_is_equal
#include <interface/matrix.h>               // for IM2_Matrix_is_equal
#include <interface/monomial-ideal.h>       // for IM2_MonomialIdeal_is_equal
```

Three engine headers pulled in: `freemodule.h`, `matrix.h`,
`monomial-ideal.h`. Each provides a C-callable `IM2_*_is_equal`
function that the engine implements.

## What equality looks like

For a typical M2 value, equality is recursive:

```
a == b   ⟺   class(a) == class(b)  AND  contents(a) ≡ contents(b)
```

For engine-backed values, the contents-comparison delegates:

```
M == M'  (Matrix)   ⟺   class(M) == class(M')
                        AND target(M) == target(M')
                        AND source(M) == source(M')
                        AND IM2_Matrix_is_equal(raw M, raw M')
```

`equality.dd` is where this dispatch is decided.

## Why hand-coded equality

For most M2 types, the default "compare fields recursively" works.
For engine types it can't — the raw representation isn't visible
to `.d` code, and the engine's internal canonical forms matter.
Hence the engine-side `IM2_*_is_equal` functions.

## Used by

- The `==` operator binding in `actors*.d`
  ([`file-actors.md`](file-actors.md)).
- Hash-table lookup (which needs `==` to find matching keys).
- Test suites comparing computed answers to expected answers.

## Related

- [`README.md`](README.md) — d/ overview.
- [`file-engine-dd.md`](file-engine-dd.md) — engine type
  declarations.
- [`file-hashtables.md`](file-hashtables.md) — uses `==` for
  bucket lookups.
- [`../e/file-interface.md`](../e/file-interface.md) (if added) —
  engine-side `IM2_*_is_equal` implementations.
