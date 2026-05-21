# `f4-spairs.{cpp,hpp}` — `F4SPairSet`

`F4SPairSet` manages the **S-pair queue** for the F4 algorithm: the
priority queue of S-pairs waiting to be processed, plus the pruning rules
that discard pairs known to reduce to zero.

Part of the [`f4/`](README.md) subdirectory.

[← f4 overview](README.md) · [← engine overview](../README.md)

## S-pair representation

The `spair` struct is declared in [`f4-types.hpp`](README.md). Each
`spair` carries:

- A **type tag** (`SPairType`) — distinguishes a Buchberger S-pair from a
  ring/quotient pair from a generation pair.
- A **degree** (used for sugar-aware selection).
- Indices into the basis (`i`, `j`) for the two elements involved.
- An **lcm monomial** (`lcm`) — allocated but **not initialised by
  `make_spair`**. The header comment is explicit about this: the caller
  must initialise the lcm before adding the spair to the set.

## Internal priority queue

`F4SPairSet` uses a `std::priority_queue` of `spair*` ordered by degree
(lower first), with ties broken by lcm. The queue is repopulated as new
basis elements arrive: each new element generates pairs with every prior
element.

## Pruning

Two pruning rules are implemented as methods:

- `pair_not_needed(spair*, gbelem*)` — checks the chain criterion against a
  candidate basis element. If the criterion succeeds, the pair is
  unnecessary.
- `remove_unneeded_pairs()` — bulk-prune the queue after a new basis element
  is added (returns the number of pairs removed). This sweeps for all
  pairs subsumed by the new element.

Both are critical for performance — without pruning, the queue size grows
quadratically in basis size for the typical Buchberger algorithm.

## Memory

`F4SPairSet` allocates `spair`s and lcm monomials from a
[`MemoryBlock`](../utilities.md) bump allocator. Each generation of the
algorithm clears the block at the end, freeing all transient pairs at once
without per-allocation overhead.

## Related

- [`README.md`](README.md) — F4 overview.
- [`file-f4-computation.md`](file-f4-computation.md) — top-level glue.
- [`../file-spair.md`](../file-spair.md) — older `s_pair` used by `gbA`.
- [`f4-types.hpp`](README.md) — `spair` declaration.
- [`memblock.hpp`](README.md) — bump allocator.
