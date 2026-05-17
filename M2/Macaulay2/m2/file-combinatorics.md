# `combinatorics.m2` — `subsets`, `partitions`, memoised counts

`combinatorics.m2` provides the standard **combinatorial enumeration**
routines: `subsets`, `partitions`, `permutations`, plus the
memoisation infrastructure that lets recursive counts (`binomial`,
factorial of large arguments) run quickly.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's declared

```m2
needs "remember.m2"     -- for memoize

subsets = method(TypicalValue => List)
subsets(ZZ, ZZ) := (n, j) -> (
    if n < 0 then error "expected a nonnegative number";
    if j < 0 then return {};
    ...
)
```

The `memoize` import is critical: combinatorial functions are
recursively defined (`binomial(n, k) = binomial(n-1, k-1) +
binomial(n-1, k)`) and naive recursion is exponential. Memoisation
makes them linear.

## Core operations

- **`subsets(n, j)`** — return the list of all *j*-subsets of
  `{0, …, n-1}`.
- **`subsets L`** — all subsets of a list.
- **`subsets(L, j)`** — j-subsets of a list.
- **`partitions(n)`**, **`partitions(n, max)`** — integer
  partitions.
- **`permutations L`** — all permutations.
- **`composition(n, k)`** — ordered compositions of `n` into `k`
  parts.

## How they're implemented

The recursion shapes are standard:

```text
subsets(n, j) = subsets(n-1, j) ++ {append(s, n-1) | s ∈ subsets(n-1, j-1)}
```

Each recursion is wrapped with `memoize` so repeated subproblems are
cached.

## Engine partnership

For very large arguments, M2 dispatches to the engine's
[`file-comb.md`](../e/file-comb.md) (`Subsets`) class for
performance. The dividing line is roughly `n > 100`; below that the
M2-side recursion is fast enough.

## Used by

- Schubert calculus packages
  ([`file-schur2.md`](file-schur2.md)-related).
- Enumeration packages.
- M2 users learning combinatorics.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`../e/file-comb.md`](../e/file-comb.md) — engine `Subsets`.
- `remember.m2` — `memoize` machinery.
- `lists.m2` ([`file-lists.md`](file-lists.md)) — list operations
  these enumerations produce.
