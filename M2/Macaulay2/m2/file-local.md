# `local.m2` — local-cohomology helpers

`local.m2` provides **local-cohomology** helpers — Ext-based
computations of local cohomology modules and truncated duals.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```m2
--		Copyright 1993-1998 by Daniel R. Grayson, Michael E. Stillman

needs "complexes.m2"   -- for Ext

-- local cohomology

truncatedDual := (M, e) -> (
    -- find (k-dual M), truncated in degrees >= e.
    -- depends on truncate methods
    needsPackage "Truncations";
    ...
)
```

The file lazily loads the `Truncations` user package — its operations
require `truncate`, which lives in the package.

## What it provides

- **`truncatedDual(M, e)`** — the `k`-dual of `M` truncated to
  degrees `≥ e`. Useful for finitely-presenting otherwise-infinite-
  dimensional duals.
- **Local-cohomology operations** — typically computed as
  `Ext^i(R/m^k, M)` for `k → ∞`. The file provides the limit-taking
  machinery.

## Why a Core stub

The full local-cohomology toolkit lives in user packages
(`LocalCohomology`, `Truncations`). `local.m2` is just the Core
entry point that:

- Provides a uniform call style.
- Loads the relevant package on demand.

Same pattern as [`file-localring.md`](file-localring.md): Core stub +
package implementation.

## Used by

- Algebraic-geometry packages computing local cohomology of varieties.
- Commutative-algebra packages working with depth and projective
  dimension.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-complexes.md`](file-complexes.md) — supplies `Ext`.
- `LocalCohomology` and `Truncations` user packages.
- [`file-localring.md`](file-localring.md) — sister Core stub for
  local rings.
