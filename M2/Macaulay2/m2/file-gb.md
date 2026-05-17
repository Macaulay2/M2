# `gb.m2` — the M2-side Gröbner basis front-end

`gb.m2` is the **M2-side wrapper** for the engine's Gröbner basis
machinery. It defines the `gb(...)` user-facing function, parses the
strategy options, and routes everything through the engine's
[`GBComputation`](../e/file-comp-gb.md) class family.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What it exposes to users

The main entry point is the `gb` method:

```m2
G = gb I                              -- default strategy
G = gb(I, Algorithm => Homogeneous)   -- explicit strategy
G = gb(I, DegreeLimit => 5)           -- with a stop condition
generators G                          -- the GB matrix
gens G                                -- shorthand
```

Plus related entry points:

- `groebnerBasis I` / `gens gb I` — the matrix.
- `mingens I` — minimal generators.
- `forceGB M` — register a user-supplied matrix as a GB without
  running an algorithm.
- `status G` — Computation status.

## Stop-condition status codes

The file includes a hard-coded mirror of the engine's
[`ComputationStatusCode`](../e/interface/file-computation-interface.md)
enum:

```m2
-- Keep this in sync with the ComputationStatusCode in Macaulay2/e/interface/computation.h
RawStatusCodes := new HashTable from {
    1 => "need resize",              -- COMP_NEED_RESIZE
    2 => "error",                    -- COMP_ERROR
    -- ... mirrors enum values 1..N ...
}
```

The comment says it out loud: **keep this in sync** with the C
enum. Two-file invariants like this are inherently fragile; the
codebase relies on developer discipline.

## Dispatching to the engine

The `gb` method:

1. Validates the input ring and module.
2. Builds a `Computation` object via `interface/groebner.h`'s
   `rawGB(...)`.
3. Sets stop conditions from the user options.
4. Calls `start_computation()` on the Computation.
5. Marshals the result back into M2 types.

Most of this is automatic — the user just sees `gb I` and gets a
result.

## Algorithm selection

The `Algorithm => …` option maps to the engine's strategy values:

| Option | Engine strategy | Engine file |
|---|---|---|
| `Default` | Default (gbA) | [`../e/file-gb-default.md`](../e/file-gb-default.md) |
| `Homogeneous` | gb-homog2 | [`../e/file-gb-variants.md`](../e/file-gb-variants.md) |
| `Sugarless` | gb-sugarless | same |
| `Toric` | gb-toric | same |
| `LinearAlgebra` | F4 ([`../e/f4/`](../e/f4/README.md)) | F4 family |
| `Walk` | gb-walk | gb-variants |
| `MathicGB` | mathicgb | [`../e/file-mathicgb-interface.md`](../e/file-mathicgb-interface.md) |

## Related

- [`README.md`](README.md) — m2/ overview.
- [`../e/file-comp-gb.md`](../e/file-comp-gb.md) — engine GBComputation.
- [`../e/interface/file-groebner-interface.md`](../e/interface/file-groebner-interface.md)
  — C entry points.
- [`../e/interface/file-computation-interface.md`](../e/interface/file-computation-interface.md)
  — status / stop-condition enums.
- [`../e/groebner-bases.md`](../e/groebner-bases.md) — engine-side area.
