# `OverlapTable.{cpp,hpp}` — `OverlapTable`

`OverlapTable` is the **S-pair queue** for non-commutative Gröbner
bases — the NC analogue of [`file-spair.md`](../file-spair.md) /
[`file-f4-spairs.md`](../f4/file-f4-spairs.md). Instead of S-pairs of
monomials it manages **overlaps** of words.

Part of the [`NCAlgebras/`](README.md) subdirectory.

[← NCAlgebras overview](README.md) · [← engine overview](../README.md)

## What is an overlap

In the non-commutative setting there is no single S-polynomial concept —
two basis elements `f, g` can produce *multiple* reduction candidates,
one for each way the leading word of `f` overlaps with the leading word
of `g`. Specifically, for each position `k` where a suffix of `lw(f)`
matches a prefix of `lw(g)`, the overlap `(f, g, k)` represents a
distinct S-polynomial candidate.

```cpp
// tuple is (i, j, k, bool) where:
//   i    = index of first word,
//   j    = position of overlap in first word,
//   k    = index of the second word,
//   bool = whether the overlap still needs to be computed.
using Overlap = std::tuple<int, int, int, bool>;

using OverlapMap = std::map<std::pair<int, bool>, std::deque<Overlap>>;
```

## Data structure

`OverlapTable` is an `OverlapMap` (a `std::map` of `std::deque`s) keyed by
`(degree, processed-flag)`. This gives:

- **Degree-sorted iteration** — pull all overlaps of the current degree
  before moving to the next.
- **Persistent storage of unprocessed overlaps** — the second flag
  separates pairs still pending from pairs already absorbed by a basis
  update.

Each pending overlap eventually:

1. Is selected for processing.
2. Has its S-polynomial computed.
3. Has its S-polynomial reduced modulo the current basis.
4. Either yields a new basis element (and produces fresh overlaps with
   it) or reduces to zero (and is discarded).

## Pruning

After each new basis element is added, `OverlapTable` walks the pending
overlaps and discards those subsumed by the new element. This is the
non-commutative chain-criterion analogue. The dependency on
[`Polynomial.hpp`](../polynomial-rings.md) (`ConstPolyList`) reflects that
pruning sometimes needs to inspect the actual polynomials, not just their
leading words.

## Related

- [`README.md`](README.md) — NCAlgebras overview.
- [`file-NCGroebner.md`](file-NCGroebner.md), [`file-NCF4.md`](file-NCF4.md)
  — primary consumers.
- [`file-WordTable.md`](file-WordTable.md) — used to find overlap positions.
- [`../file-spair.md`](../file-spair.md), [`../f4/file-f4-spairs.md`](../f4/file-f4-spairs.md)
  — commutative analogues.
