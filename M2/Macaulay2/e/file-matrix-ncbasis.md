# `matrix-ncbasis.{cpp,hpp}` — non-commutative `basis`

`matrix-ncbasis.cpp` implements the **non-commutative analogue** of
[`file-matrix-kbasis.md`](file-matrix-kbasis.md)'s `basis(d, M)`:
given a non-commutative algebra module and a degree, enumerate the
standard words of that degree.

Part of the [Matrices](matrices.md) area.

[← per-area: matrices](matrices.md) · [← engine overview](README.md)

## Header preamble

```cpp
#include "matrix-ncbasis.hpp"

#include <memory>

#include "NCAlgebras/FreeAlgebra.hpp"
#include "NCAlgebras/FreeMonoid.hpp"
#include "NCAlgebras/Word.hpp"
#include "NCAlgebras/WordTable.hpp"

#include "interrupted.hpp"
#include "monoid.hpp"

std::unique_ptr<WordTable> constructWordTable(const FreeAlgebra &A,
                                              const ConstPolyList &gb) {
    // ...
}
```

The file lives at the top level of `e/` (alongside its commutative
sibling) but pulls in everything from
[`NCAlgebras/`](NCAlgebras/README.md): `FreeAlgebra`, `FreeMonoid`,
`Word`, `WordTable`. The first heavy thing it does is construct a
`WordTable` ([`NCAlgebras/file-WordTable.md`](NCAlgebras/file-WordTable.md))
holding the leading words of the input GB.

## What `ncBasis(d, M)` does

Given:

- A module `M` over a non-commutative algebra
  ([`file-M2FreeAlgebra.md`](file-M2FreeAlgebra.md) or
  [`file-M2FreeAlgebraQuotient.md`](file-M2FreeAlgebraQuotient.md)).
- A degree (or degree vector) `d`.

Compute the **standard words** of that degree in `M` — words not
containing any leading word of the GB of the module's defining
ideal.

The output is a `Matrix*` over the non-commutative ring whose
columns are the standard words.

## Why a separate file from `matrix-kbasis`

Even though the user-facing operation is morally the same, the
internals differ substantially:

- **Word enumeration vs. monomial enumeration** — words are
  ordered sequences; monomials are commutative multisets.
- **Subword checking vs. divisibility checking** — the
  non-commutative version of "monomial divides another" is "word is
  a contiguous subword of another."
- **`WordTable` vs. `MonomialTable`** — the index structures differ.

Sharing code between the two would force enough abstraction that the
hot loops would slow down; keeping them separate is the pragmatic
choice.

## Interrupt support

The file pulls in [`file-interrupted.md`](file-interrupted.md) and
polls `system_interrupted()` because the enumeration can take
unbounded time for free algebras with many generators.

## Used by

- M2's `ncBasis(d, M)` built-in.
- Non-commutative algebra packages.
- `M2FreeAlgebra` and `M2FreeAlgebraQuotient` info routines.

## Related

- [`matrices.md`](matrices.md) — area overview.
- [`file-matrix-kbasis.md`](file-matrix-kbasis.md) — commutative
  sibling.
- [`NCAlgebras/file-WordTable.md`](NCAlgebras/file-WordTable.md) —
  leading-word index.
- [`NCAlgebras/file-FreeAlgebra.md`](NCAlgebras/file-FreeAlgebra.md),
  [`NCAlgebras/file-FreeMonoid.md`](NCAlgebras/file-FreeMonoid.md) —
  host ring/monoid.
