# `hashtables.dd` — `HashTable` and `MutableHashTable` implementations

`hashtables.dd` implements **`HashTable`** and
**`MutableHashTable`** — the workhorse key-value data structures
used pervasively in M2 (as the base for `Module`, `Matrix`,
`Package`, options, caches, etc.).

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```d
--		Copyright 1994,2010 by Daniel R. Grayson

use equality;
use classes;

header "#include <cassert>";

-- applyEE and applyEEE are not defined until evaluate.d,
```

The header is short — `hashtables.dd` is a foundational type, so
it has few dependencies. The `<cassert>` include is for debug-time
sanity checks.

The `applyEE` / `applyEEE` comment points to the same forward-
reference issue many `.d` files have: the file needs to know that
"apply a function to a hash-table value" will exist later, but the
implementation lives in [`file-evaluate.md`](file-evaluate.md).

## Why `.dd`, not `.d`

`hashtables.dd` is `.dd` (compiles to C++) because:

- Uses C++ STL hash containers in some paths.
- Interacts with the engine's hash-table machinery, which is C++.

## Two HashTable flavours

| Type | Mutable? | Use |
|---|---|---|
| `HashTable` | No | Immutable lookup — typical user values, options |
| `MutableHashTable` | Yes | Caches, packages' symbol tables, growable maps |

Both share the same lookup machinery; the difference is whether
`put` returns a new table (immutable) or mutates in place (mutable).

## Layout

A `HashTable` has:

- A **bucket array** — list of `(key, value, next)` chains.
- A **mutability flag** — distinguishes the two flavours.
- A **class** — the M2 type tag (e.g., `Module`, `Matrix`).
- A **hash code** — cached.

The bucket array grows dynamically as entries are added.

## Hash collisions

Standard chaining: equal-hash entries form a linked list. Equality
testing uses M2's `===` (identity) and `==` (value equality).

## Used by

- Essentially every M2 type — `Module`, `Matrix`, `Ring`, `Package`,
  `OptionTable`, ... are all `HashTable` subclasses.
- M2-level `new HashTable from {...}`.
- The engine wraps engine values via M2 hash tables for the
  `Raw* ↔ M2` boundary.

## Related

- [`README.md`](README.md) — d/ overview.
- [`file-classes.md`](file-classes.md) (if added) — `Type` system
  using `HashTable`.
- [`file-expr.md`](file-expr.md), [`file-evaluate.md`](file-evaluate.md)
  — pattern-match on `HashTable` Expr variants.
- [`../m2/file-classes.md`](../m2/file-classes.md) — M2-side type
  hierarchy built on `HashTable`.
