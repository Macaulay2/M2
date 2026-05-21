# `res-f4.{cpp,hpp}` — `F4Res` (F4-style reduction loop for resolutions)

`res-f4.cpp` is **the F4-style inner loop** specialised for free
resolutions. Given a [`SchreyerFrame`](file-res-schreyer-frame.md), it
processes one (level, degree) cell at a time by building a Macaulay
matrix from the frame's pending syzygies, reducing it, and feeding the
results back into the frame.

Part of the [`schreyer-resolution/`](README.md) subdirectory.

[← schreyer-resolution overview](README.md) · [← engine overview](../README.md)

## Class shape

```cpp
#include "VectorArithmetic.hpp"
#include "f4/monhashtable.hpp"
#include "schreyer-resolution/res-memblock.hpp"
#include "schreyer-resolution/res-poly-ring.hpp"
#include "monomial-sets.hpp"

class SchreyerFrame;

class F4Res {
    friend class ResColumnsSorter;

public:
    F4Res(SchreyerFrame &res);
    ~F4Res();
    // ...
};
```

`F4Res` is constructed from a `SchreyerFrame &` — it holds a reference,
not ownership. The frame is the source of pending work and the
destination for results.

## Why a separate class from `SchreyerFrame`

`SchreyerFrame` ([`file-res-schreyer-frame.md`](file-res-schreyer-frame.md))
stores the *state* of an in-progress resolution. `F4Res` is the
*algorithm* that moves that state forward. Separating these lets:

- The frame outlive any particular reduction step (useful for
  resumability).
- The algorithm be re-instantiated with different parameters (precision,
  threading, monomial encoding) without touching the frame.
- Tests construct a frame independently of the reduction loop.

## The reduction loop

For one `(level, degree)` cell:

```text
1. Read pending syzygies from the frame.
2. Identify which prior basis elements need to participate as reducers.
3. Allocate a Macaulay matrix:
       rows   = pending syzygies + tail reducers
       cols   = all monomials seen in any row (sorted by Schreyer order)
4. Reduce the matrix to row-echelon form via VectorArithmetic.
5. Extract new syzygies from nonzero echelon rows.
6. Insert them into the frame's next level.
```

The `friend class ResColumnsSorter` line gives the column-sorting
helper access to F4's internal column-index map.

## Dependencies

- **`VectorArithmetic`** ([`../file-VectorArithmetic.md`](../file-VectorArithmetic.md))
  — templated arithmetic over the coefficient ring.
- **`f4/monhashtable.hpp`** ([`../f4/file-monhashtable.md`](../f4/file-monhashtable.md))
  — the hash-table trait classes shared between F4 and the resolution
  code.
- **`res-memblock.hpp`** — `F4MemoryBlock`-style allocator for transient
  monomial slabs.
- **`monomial-sets.hpp`** — set-of-monomials structure for column
  enumeration.

## Related

- [`README.md`](README.md) — schreyer-resolution overview.
- [`file-res-f4-computation.md`](file-res-f4-computation.md) — the
  Computation that drives `F4Res`.
- [`file-res-schreyer-frame.md`](file-res-schreyer-frame.md) —
  state owner.
- [`file-res-poly-ring.md`](file-res-poly-ring.md) — value-type host.
- [`../f4/file-f4.md`](../f4/file-f4.md) — non-resolution F4 sibling.
