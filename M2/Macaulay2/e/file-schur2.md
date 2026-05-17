# `schur2.{cpp,hpp}` — `SchurRing2` (refactored Schur ring)

`schur2.cpp` implements **`SchurRing2`** — a refactored, more
flexible Schur-function ring class that supersedes
[`file-schur.md`](file-schur.md)'s `SchurRing`. The "2" in the name
indicates the second-generation implementation; both are in the tree.

Part of the [Polynomial rings](polynomial-rings.md) area.

[← per-area: polynomial-rings](polynomial-rings.md) · [← engine overview](README.md)

## State

```cpp
#include <vector>
#include "poly.hpp"

using schur_word = int;
// typedef int schur_word;

// Format for a schur_partition, const_schur_partition
// (n+1) a1 a2 ... an
// where a1 >= a2 >= ... >= an (can these be negative?  I think so)
// and n+1 is the length of the entire allocated amount.

typedef schur_word        *schur_partition;
typedef const schur_word  *const_schur_partition;

class tableau2 {
    friend class SchurRing2;
    // ... Young tableau data ...
};
```

Key differences from the original `SchurRing`:

- **Partition representation** is more explicit: a `schur_partition`
  is `[n+1, a_1, a_2, …, a_n]` with the length stored alongside the
  parts.
- **Negative parts allowed** — the header comment "can these be
  negative? I think so" indicates the second-generation class
  intentionally relaxes the partition constraint, enabling
  computation over the **virtual** Schur ring (Schur polynomials
  of negative or formal partitions).
- **`tableau2`** is the new tableau type, parallel to the original
  `tableau`.

## Why a refactor

The original [`SchurRing`](file-schur.md) (in `schur.cpp`) had several
limitations:

- Partitions had to be strictly positive.
- The LR enumeration was specialised to small partition widths.
- Multiplication was the main supported operation; other Schur-
  function operations needed bolted-on helpers.

`SchurRing2` cleans these up by providing a more general partition
representation and a wider operation set (including
**plethysm**, **Frobenius characteristic**, and **virtual** Schur
combinations).

## Subclass: `SchurSnRing`

[`file-schurSn.md`](file-schurSn.md) (next file deep-dive)
specialises `SchurRing2` for the symmetric-group representation
ring.

## Companion: `schur-poly-heap.hpp`

[`file-schur-poly-heap.md`](file-schur-poly-heap.md) provides a
heap-based accumulator for Schur polynomial sums — the LR
enumeration produces many partial sums that this heap consolidates
efficiently.

## Used by

- M2-level `schurRing(R, n)` constructor.
- Schubert calculus packages that need a Schur ring of fixed rank.
- `Posets` and `Combinatorics` packages that perform Schur
  expansions.

## Related

- [`polynomial-rings.md`](polynomial-rings.md) — area overview.
- [`file-schur.md`](file-schur.md) — original `SchurRing` (kept).
- [`file-schurSn.md`](file-schurSn.md) — symmetric-group subclass.
- [`file-schur-poly-heap.md`](file-schur-poly-heap.md) — accumulator.
- [`m2/schubert.m2`](../m2/README.md) — M2-side Schur consumers.
