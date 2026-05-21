# `NCGroebnerTest.cpp` — non-commutative Gröbner-basis tests

`NCGroebnerTest.cpp` exercises the **non-commutative GB engine**
in [`../NCAlgebras/`](../NCAlgebras/README.md) — free algebras,
word tables, overlap tables, and the suffix-tree-based reduction
algorithm.

Part of the [engine unit-tests suite](README.md).

[← back to unit-tests overview](README.md) · [← engine overview](../README.md)

## What's pulled in

```cpp
#include "MemoryBlock.hpp"
#include "interface/ring.h"

#include "poly.hpp"
#include "aring-glue.hpp"
#include "NCAlgebras/FreeAlgebra.hpp"
#include "NCAlgebras/FreeAlgebraQuotient.hpp"
#include "NCAlgebras/WordTable.hpp"
#include "NCAlgebras/NCGroebner.hpp"
#include "NCAlgebras/OverlapTable.hpp"
#include "NCAlgebras/SuffixTree.hpp"
#include "NCAlgebras/NCReduction.hpp"
#include "monordering.hpp"
#include "monoid.hpp"

#include "util-polyring-creation.hpp"
```

Almost every file in `../NCAlgebras/` is touched. The test file
acts as a **smoke test** for the whole NC pipeline.

## What's tested

| Component | Source | Tests verify |
|---|---|---|
| `FreeAlgebra` | `../NCAlgebras/FreeAlgebra.hpp` | Construction, basic arithmetic |
| `FreeAlgebraQuotient` | `../NCAlgebras/FreeAlgebraQuotient.hpp` | Reduction modulo a side ideal |
| `WordTable` | `../NCAlgebras/WordTable.hpp` | Insertion / lookup of monomial words |
| `OverlapTable` | `../NCAlgebras/OverlapTable.hpp` | Critical-pair generation |
| `SuffixTree` | `../NCAlgebras/SuffixTree.hpp` | Suffix-tree-based factor search |
| `NCReduction` | `../NCAlgebras/NCReduction.hpp` | Polynomial reduction |
| `NCGroebner` | `../NCAlgebras/NCGroebner.hpp` | End-to-end NC GB |

## Why suffix trees

NC Gröbner bases need to find all places where one monomial word
overlaps another. Naive search is `O(L^2)` per query (`L` =
total word length). Suffix trees turn it into `O(L + matches)`.

The suffix-tree implementation is delicate; bugs are catastrophic
(missed overlaps → incomplete GB → wrong answer). This test file
is heavy on suffix-tree correctness.

## A typical test

```cpp
TEST(NCGroebner, smallExample)
{
  // Set up: F<x, y> / (xy + yx - 1)
  FreeAlgebra* A = ...;
  std::vector<Poly> gens = { ... };
  NCGroebner gb(A, gens);
  gb.compute(/* degree limit */ 10);
  ASSERT_EQ(gb.size(), expected_size);
}
```

Real test names include `OverlapTable.insert`,
`SuffixTree.constructionMatches`, `NCGroebner.commutator`, etc.

## Used by

- Engine developers working on NC algebras.
- The `AssociativeAlgebras` user package.
- CI on every PR.

## Related

- [`README.md`](README.md) — unit-tests overview.
- [`../NCAlgebras/README.md`](../NCAlgebras/README.md) — NC engine
  under test.
- [`../NCResolutions/README.md`](../NCResolutions/README.md) —
  resolution counterpart.
- [`../groebner-bases.md`](../groebner-bases.md) — broader area.
