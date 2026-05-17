# `SPairs.{cpp,hpp}` — refactored F4 S-pair management

`SPairs` is the **S-pair queue** for the refactored F4 engine in
[`gb-f4/`](README.md). It is the modern counterpart of
[`f4/file-f4-spairs.md`](../f4/file-f4-spairs.md) — same role, cleaner
factoring around an explicit "pre-S-pair" concept.

Part of the [`gb-f4/`](README.md) subdirectory.

[← gb-f4 overview](README.md) · [← engine overview](../README.md)

## Pre-S-pairs vs. S-pairs

The header design notes (lightly paraphrased):

> Each S-pair's monomials will be included in a special "hash table".
> What about "pre-S-pairs"? Are they in their own, or not yet inserted?
> Maybe try both?
>
> Inserting new S-pairs:
>   - Make pre-S-pairs.
>   - Sort them.
>   - Find minimal elements (via masks, MonomialIdeal-style structure,
>     and maybe some mathic structures).
>   - Insert each S-pair directly.
>
> Grabbing a set of S-pairs to do (but if interrupted, don't lose the
> pairs!):
>   - Sort them, at least by degree.
>   - How to remove a pair that isn't required? (the more complicated
>     S-pair approach).

The plan is **two-stage**: when a new basis element arrives, its
candidate S-pairs are computed as *pre-S-pairs* (lighter-weight,
just enough info to test cheap prunings), sorted and filtered, and only
the survivors are promoted to full S-pairs and inserted into the main
queue.

This avoids the work of fully constructing S-pairs that get pruned
immediately, which is the dominant cost in the old single-stage approach.

## Hashed monomials

```cpp
#include "MonomialTypes.hpp"
#include "MonomialView.hpp"
#include "Basis.hpp"

#include <map>
```

S-pair monomials (lcm of two leading words) are stored in a shared
monomial hash table. The table maps each lcm to an integer column index
that the
[`MacaulayMatrix`](file-MacaulayMatrix.md) can use directly. No copies.

## Resumability

A persistent concern in the design notes: "if interrupted, don't lose
the pairs." Because the algorithm may run inside a
[`Computation`](../file-computation-framework.md) with a time budget,
the queue must survive a `start_computation()` return without dropping
work. The two-stage design accommodates this — pre-S-pairs are easy to
rebuild from the basis if needed, full S-pairs are persisted.

## Status

This file is part of the long-running F4 refactor (see `TODO-refactor-f4`
in [`gb-f4/`](README.md)). The interfaces will continue to evolve.

## Related

- [`README.md`](README.md) — gb-f4 overview.
- [`file-GBF4Computation.md`](file-GBF4Computation.md) — top-level driver.
- [`file-MacaulayMatrix.md`](file-MacaulayMatrix.md), [`file-Basis.md`](file-Basis.md)
  — neighbouring classes.
- [`../f4/file-f4-spairs.md`](../f4/file-f4-spairs.md) — older `F4SPairSet`.
- [`../file-spair.md`](../file-spair.md) — legacy `s_pair` in `gbA`.
