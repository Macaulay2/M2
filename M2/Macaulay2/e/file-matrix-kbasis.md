# `matrix-kbasis.cpp` — k-basis of a graded module

`matrix-kbasis.cpp` implements the **k-basis** computation: given a
graded module and a set of degrees, return the `k`-vector-space basis
of that module restricted to those degrees. It is the engine code
behind M2's `basis(d, M)` built-in.

Part of the [Matrices](matrices.md) area.

[← per-area: matrices](matrices.md) · [← engine overview](README.md)

## Header preamble

```cpp
#include <stddef.h>
#include <vector>

#include "ExponentVector.hpp"
#include "interface/m2-mem.h"
#include "engine-includes.hpp"
#include "error.h"
#include "freemod.hpp"
#include "int-bag.hpp"
#include "interrupted.hpp"
#include "matrix-con.hpp"
#include "matrix.hpp"
#include "monideal.hpp"
#include "monoid.hpp"
#include "newdelete.hpp"
#include "polyring.hpp"
#include "ring.hpp"
#include "ringelem.hpp"
#include "style.hpp"
#include "util.hpp"
```

The wide include list reflects how much engine state goes into
computing a k-basis: monomial machinery, monomial ideals, free
modules, the polynomial ring, all of it.

## What `basis(d, M)` does

Given a graded module `M` over a polynomial ring and a degree vector
`d`:

1. Compute the leading-term ideal of `M`'s presentation (via GB if not
   already available).
2. Enumerate all monomials in degree `d` that are **not** in the
   leading-term ideal — these are the **standard monomials** of `M`
   in degree `d`.
3. Return them as a `Matrix*` whose columns are the basis elements.

The output is a one-row matrix over the polynomial ring; the entries
are the basis monomials.

## Algorithmic insight

The naïve approach is exponential in the number of variables. The
engine uses a **monomial-ideal traversal**: it walks the monomial
ideal tree once and enumerates the standard monomials in degree `d`
by complement.

## Variants

The same file implements related operations:

- **`basis(lo, hi, M)`** — basis in a range of degrees.
- **`leadTermsBasis(d, M)`** — leading terms of degree-`d` basis
  elements (i.e. the in-ideal monomials).
- **k-basis in a free module of arbitrary rank** — extends the same
  algorithm per generator.

## Companion: `matrix-ncbasis.cpp`

The non-commutative analogue lives in `matrix-ncbasis.cpp` — a
separate file because the NC monomial structure is different
([`NCAlgebras/file-FreeMonoid.md`](NCAlgebras/file-FreeMonoid.md)).

## Used by

- M2 `basis`, `cover`, `hilbertFunction` and similar built-ins.
- `Polyhedra`-related packages that compute lattice-point
  enumerations.
- `Schubert` calculus packages that need k-bases of cohomology rings.

## Related

- [`matrices.md`](matrices.md) — area overview.
- [`file-matrix-con.md`](file-matrix-con.md) — used to build the
  output matrix.
- [`file-monideal.md`](file-monideal.md) — monomial-ideal traversal
  is the algorithmic engine.
- `matrix-ncbasis.cpp` — non-commutative sibling.
- [`file-hilb.md`](file-hilb.md) — produces *counts* of k-basis sizes
  by degree.
