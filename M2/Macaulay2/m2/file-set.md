# `set.m2` — `Set`, `Tally`, `VirtualTally`

`set.m2` defines the **`Set`**, **`Tally`**, and **`VirtualTally`**
types — M2's hash-keyed collection types for representing finite
sets and multisets.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```m2
needs "methods.m2"
needs "shared.m2"     -- for union

VirtualTally.synonym = "virtual tally"
```

## Type hierarchy

```
HashTable
 └── VirtualTally   — counts can be any integer (including 0 or negative)
      └── Tally     — counts are positive (i.e., multisets)
           └── Set  — counts are exactly 1
```

The three types form a nested hierarchy. `Set` is a `Tally` whose
counts are all 1; `Tally` is a `VirtualTally` whose counts are
positive. Algebraically, `VirtualTally` is the free abelian group on
the underlying set; `Tally` is the positive cone.

## Why this hierarchy

Many M2 operations (counting solutions of an equation, intersections
of varieties) produce **virtual** counts — sometimes negative,
representing "subtract this many." `VirtualTally` accommodates this
without losing typing precision.

## Operations

- **Construction** — `set L`, `tally L` (count occurrences of each
  element in `L`).
- **Membership** — `member(x, S)`, `S#?x`.
- **Operations** — `S + T` (union with sum of counts), `S - T`,
  `S * T` (product/cross), `S ** T`, `S ^** n`.
- **Conversion** — `keys S`, `toList S`, `values T`.

## Used by

- Combinatorics packages.
- Algebraic-geometry packages tallying solution-counts.
- M2's `apropos` and similar search routines (which produce sets of
  matches).
- [`file-lists.md`](file-lists.md) — sister collection-type file.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-lists.md`](file-lists.md) — `List`/`Sequence`/`Array`.
- `combinatorics.m2` ([`file-combinatorics.md`](file-combinatorics.md))
  — enumeration over sets.
- `shared.m2` — supplies `union` and similar helpers.
