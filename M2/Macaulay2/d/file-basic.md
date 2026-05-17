# `basic.d` — universal `hash` and `Expr` helpers

`basic.d` defines the **universal `hash(Expr)`** function and a
handful of `Expr` helpers used everywhere. It's deliberately
small but central — every dictionary lookup, set membership, and
hash-table operation reaches it.

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```d
--		Copyright 1994 by Daniel R. Grayson

use expr;

header "#include <engine.h>"; -- required for raw hash functions

-- used to hash sequences below and for quick method lookup in hashtables.dd
export seqHashSeed := hash_t(27449);
export seqHashMult := hash_t(27457);

export hash(e:Expr):hash_t := (
     when e
     is x:HashTable do x.hash
     is x:SymbolClosure do x.symbol.hash
     is s:SymbolBody do s.symbol.hash
     is x:Database do x.hash
     is x:ZZcell do hash(x.v)
     is b:Boolean do Ccode(hash_t, b.v)
     is Nothing do hash_t(333889)
     is x:List do x.hash
     ...);
```

The `seqHashSeed = 27449` and `seqHashMult = 27457` are chosen
**primes** that give good hash distribution on typical M2
sequences — a sequence's hash is computed as
`(((seed * mult + h0) * mult + h1) * mult + ... ) mod 2^64`.

## What `hash(Expr)` covers

The pattern-match dispatches on every `Expr` variant:

- **Pre-computed** — `HashTable`, `SymbolClosure`, `Database`,
  `List` already carry a `.hash` field. Just return it.
- **Cell types** — `ZZcell`, `RRcell`, `QQcell` etc. hash their
  value.
- **Singletons** — `Boolean` uses `b.v` directly; `Nothing` uses
  the constant `333889`.
- **Engine values** — calls into engine via `Ccode(hash_t, ...)`
  with `<engine.h>` providing the raw-hash functions.

The single function is the entry point for *every* hash-table
operation in M2.

## Why a separate file

`hash(Expr)` is needed by [`file-hashtables.md`](file-hashtables.md),
which is itself foundational. Putting `hash` in `basic.d` (which
only depends on `expr.d`) keeps the dependency graph tight.

## Used by

- [`file-hashtables.md`](file-hashtables.md) — every put / get.
- [`file-sets.md`](file-sets.md) — Set / Tally lookups.
- [`file-evaluate.md`](file-evaluate.md) — method dispatch (which
  uses hash codes for fast lookup).
- Every place that puts an `Expr` into a `HashTable`.

## Related

- [`README.md`](README.md) — d/ overview.
- [`file-expr.md`](file-expr.md) — defines the `Expr` sum type.
- [`file-hashtables.md`](file-hashtables.md) — main consumer.
- [`file-arithmetic.md`](file-arithmetic.md) — defines `hash_t`.
- [`../e/file-hash.md`](../e/file-hash.md) (if added) — engine-side
  hash primitives.
