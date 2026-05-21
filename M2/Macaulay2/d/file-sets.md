# `sets.dd` — `Set`, `Tally`, `VirtualTally`

`sets.dd` implements the **`Set`**, **`Tally`**, and
**`VirtualTally`** types — M2's hash-based set and multi-set
abstractions.

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```d
--		Copyright 1994,2010 by Daniel R. Grayson
use hashtables;
use evaluate;

newtypeof(parent:HashTable):HashTable := newHashTableWithHash(typeClass,parent);
export VirtualTally := newtypeof(hashTableClass);
setupconst("VirtualTally",Expr(VirtualTally));
export Tally := newtypeof(VirtualTally);
```

The file builds the type tree:

```
HashTable
└── VirtualTally       -- key -> arbitrary integer (can be negative)
    └── Tally          -- key -> nonneg integer (multiplicity)
        └── Set        -- multiplicity always 1
```

Each level inherits hash-table machinery from the parent.

## VirtualTally vs Tally vs Set

| Type | Multiplicity range |
|---|---|
| `Set` | always 1 (membership only) |
| `Tally` | nonneg integer |
| `VirtualTally` | any integer (positive, zero, negative) |

The `VirtualTally` type exists for cases like signed counting —
e.g., `Tally A - Tally B` can have negative entries and stays a
`VirtualTally` even if both operands were `Tally`s.

## Why this lives in d/, not m2/

Hash-table multiplication / union / intersection has very tight
inner loops. Implementing them in C-via-`.d` rather than M2 makes
typical workloads (Hilbert function tallying, monomial sets) much
faster.

## Operations

- **Construction** — `set {a, b, c}`, `tally l`.
- **Predicates** — `a ? S`, `a ∈ S`.
- **Set algebra** — `S + T`, `S - T`, `S * T` (union /
  difference / intersection).
- **Cardinality** — `# S`, `sum values T`.

## Used by

- [`../m2/file-set.md`](../m2/file-set.md) (M2-side wrapper).
- Combinatorics packages.
- The Hilbert-function machinery indirectly (tallying degrees).
- Documentation system (tallying examples).

## Related

- [`README.md`](README.md) — d/ overview.
- [`file-hashtables.md`](file-hashtables.md) — building block.
- [`file-buckets.md`](file-buckets.md) — sister "Dictionary
  contents" iterator.
- [`../m2/file-set.md`](../m2/file-set.md) — M2-side consumer.
