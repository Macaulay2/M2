# `betti.m2` — M2-side Betti tables

`betti.m2` defines **`BettiTally`** — the M2-side representation of
graded Betti numbers — and the `betti(...)` function that produces
them from resolutions. The file was moved from `chaincomplexes.m2`
during the refactor that produced the modern `Complexes` package.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header notes

```m2
-- moved from chaincomplexes.m2

-* TODO
- https://github.com/Macaulay2/M2/issues/647
- https://github.com/Macaulay2/M2/issues/2159
*-

needs "gb.m2"          -- for GroebnerBasis
needs "hilbert.m2"
needs "modules2.m2"
```

The header preserves a record of where the code came from and lists
two outstanding GitHub issues (the same two referenced in
[`file-complexes.md`](file-complexes.md)).

The `needs` list reveals the trio of computations that produce Betti
data:

- **`gb.m2`** — produces the Gröbner basis whose leading terms drive
  Betti shape.
- **`hilbert.m2`** — gives Hilbert-series shortcuts when full
  resolution is overkill.
- **`modules2.m2`** — supplies the `Module` type Betti tables index.

## What's defined here

- **`BettiTally`** — the type. Internally a `HashTable` keyed by
  `(i, d, totaldeg)` triples; values are integer counts.
- **`betti C`** — given a chain complex `C`, return its Betti table.
- **`betti(M, ...)`** — shortcut for `betti(res M, ...)`.
- **`MultigradedBettiTally`** — multigraded variant.

Plus helper formatters that produce the standard rectangular table:

```
       0  1  2  3
total: 1  3  3  1
    0: 1  .  .  .
    1: .  3  3  .
    2: .  .  .  1
```

(Same format the engine's [`file-betti.md`](../e/file-betti.md)
produces via `BettiDisplay`, since the M2 side is the consumer.)

## Used by

- M2 user calling `betti C` or `betti res I`.
- Algebraic-geometry packages that inspect resolution shapes.
- `installPackage` rendering of example outputs.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`../e/file-betti.md`](../e/file-betti.md) — engine `BettiDisplay`.
- [`file-complexes.md`](file-complexes.md) — chain-complex type.
- [`file-hilbert.md`](file-hilbert.md) — adjacent computation.
- [`file-gb.md`](file-gb.md) — supplies the GB whose leading terms
  drive Betti shape.
