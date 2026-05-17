# `gauss.{cpp,hpp}` — Gaussian-elimination based GB / submodule

`gauss.cpp` implements an **explicit Gaussian-elimination** computation
of a Gröbner basis (or submodule) over a field. It is the engine's
straightforward "row-reduce a matrix of generators to echelon form"
path — simpler and sometimes faster than the full Buchberger machinery
for inputs where the answer really is just linear-algebra reduction.

Part of the [Gröbner bases](groebner-bases.md) area.

[← per-area: groebner-bases](groebner-bases.md) · [← engine overview](README.md)

## State

```cpp
#include "relem.hpp"
#include "matrix.hpp"
#include "polyring.hpp"
#include "comp-gb.hpp"

struct gm_elem : public our_new_delete {
    gm_elem *next;
    int      nterms;
    vec      f;
    vec      fsyz;
};

class GaussElimComputation : public GBComputation {
    // sorted linked list of gm_elem; pivot tracking; coefficient ring info
};
```

A `gm_elem` is a single row in the "matrix" being reduced. The fields:

- **`next`** — intrusive linked list (rows sorted by leading position).
- **`nterms`** — term count, used as a sort key.
- **`f`** — the polynomial value (as a `vec`, i.e. sparse-column form).
- **`fsyz`** — its syzygy: the image in the syzygy module.

The list is kept sorted so the pivoting step (find a row with the
lowest leading position) is O(1).

## When this beats general GB

For these inputs Gaussian elimination is **the** right algorithm:

- **Module presentations over a field** — no monomial-order machinery
  needed; just row-reduce.
- **Field-coefficient ideals with trivial monoid** — purely linear.
- **Inputs where every variable lives in degree 1 and the answer is a
  short basis** — Buchberger would do the same work but with more
  overhead.

The dispatcher in [`file-comp-gb.md`](file-comp-gb.md) selects
`GaussElimComputation` for the right inputs; the user can force it via
`Strategy => …`.

## Operations

The computation maintains the invariant that, at any moment, the
linked list represents a partial echelon form. Each step:

1. Take the next pending generator.
2. Reduce it against the existing echelon rows (subtract scalar
   multiples of rows whose leading position matches).
3. If the result is nonzero, insert it into the sorted list at the
   right position.
4. If a row had a pivot in the same column, the smaller one (by some
   tiebreak) is the one that survives; the other gets reduced
   further.

After all generators are processed, the list is a complete echelon
form.

## Related

- [`groebner-bases.md`](groebner-bases.md) — area overview.
- [`file-hermite.md`](file-hermite.md) — analogous algorithm over `ZZ`.
- [`file-comp-gb.md`](file-comp-gb.md) — `GBComputation` base.
- [`file-gb-default.md`](file-gb-default.md) — full Buchberger
  alternative.
