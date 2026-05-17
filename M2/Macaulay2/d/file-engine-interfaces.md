# `interface.dd`, `interface2.d`, `monoid.dd`, `monomial_ordering.dd` — engine-call wrappers

These four files contain the **top-level interpreter-side wrappers
for engine C++ functions** — the M2-callable thin shells around
`Macaulay2/e/` exports.

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## The four files

All four open with the same comment:

```d
-- this file contains top level routines that call the C++ code in the engine

use engine;
use common;
use hashtables;
use struct;
```

The split is by *engine area*:

| File | Wraps engine area |
|---|---|
| `interface.dd` | rings, ideals, modules, matrices (the main interface) |
| `interface2.d` | newer additions (NAG, Schreyer res, NC, F4 GB, ...) |
| `monoid.dd` | `Monoid` factory and operations |
| `monomial_ordering.dd` | `MonomialOrdering` construction |

When you call e.g. `rawMatrixMultiply(m1, m2)` in M2 user code,
control flows:

1. M2 source → interpreter looks up `rawMatrixMultiply`.
2. Falls through to a binding in `interface.dd`.
3. `interface.dd` validates args, calls `Ccode(...)` into engine.
4. Engine C++ executes the matrix multiply.
5. Result wrapped as an M2 `RawMatrix` and returned.

## The naming convention

Engine wrappers conventionally start with `raw`. The M2-level
wrappers in [`../m2/file-newring.md`](../m2/file-newring.md) and
related Core files then call these `raw*` primitives.

```
User M2 code:    M = R^3
                  ↓
m2/freemod.m2:   ... newFreeModule R^3 ...
                  ↓
m2/freemod.m2:   ... rawFreeModule(...) ...   (calling engine wrapper)
                  ↓
interface.dd:    rawFreeModule := (rt:RawRing, n:int) -> ...
                  ↓
engine/interface/freemodule.cpp:  rawFreeModule_(...)
```

## `interface.dd` vs `interface2.d`

The first (2004) covered ring/ideal/module/matrix; the second
(2008-2009) added NAG, Schreyer resolutions, NC algebras, F4 GB,
etc. — features added after the original split. They could be
merged but the historical division persists.

## Why `.dd` for some and `.d` for others

C++ engine calls cross the C++ ABI; the engine boundary needs
`.dd` for files that pass C++ types directly. `.d` works when
everything passes through C-shaped declarations in
`engine.h` / `interface/*.h`.

## Used by

- [`../m2/`](../m2/README.md) Core M2 files — every M2 operation
  on a ring / matrix / module ultimately reaches one of these
  wrappers.
- The engine boundary in [`file-engine-dd.md`](file-engine-dd.md).

## Related

- [`README.md`](README.md) — d/ overview.
- [`file-engine-dd.md`](file-engine-dd.md) — engine.dd declares
  the engine types these files manipulate.
- [`../e/README.md`](../e/README.md) — engine catalogue.
- [`../e/file-engine-h.md`](../e/file-engine-h.md) (if added) —
  C interface header these wrappers call into.
