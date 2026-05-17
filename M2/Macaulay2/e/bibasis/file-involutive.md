# `involutive.hpp` — the templated involutive-basis algorithm

`involutive.hpp` is the **core templated involutive-basis
algorithm** — Zinin's variant of Janet's involutive division for
boolean polynomial rings. Templated on the monomial type so the
inner reduction loop is monomorphised per ordering.

Part of [`bibasis/`](README.md).

[← bibasis/ overview](README.md) · [← engine overview](../README.md)

## Header

```cpp
#include <list>
#include <algorithm>
#include "pcomparator.hpp"
```

Pulls in:

- **`<list>`** — `std::list` for the polynomial collection.
- **`pcomparator.hpp`** — the **polynomial comparator** used to
  sort basis elements during reduction.

## Involutive basis vs Gröbner basis

A **Janet involutive basis** is a richer alternative to a
Gröbner basis. Where GB has "leading monomial divides," involutive
basis adds a finer **involutive division** relation:

```
Standard GB: f reduces g if lt(f) | some monomial of g
Involutive:  f reduces g if lt(f) "involutively divides" m for some m in g
```

The involutive relation is more restrictive — it considers which
*variables* can appear. The benefit: a unique reduction at every
step, no S-pair processing needed.

For boolean rings specifically (`F_2[x_i]/(x_i^2 - x_i)`), the
involutive approach is dramatically faster than standard F4.

## What `Involutive<MonomType>` does

```cpp
template <typename MonomType>
class Involutive
{
  std::list<Polynom<MonomType>> basis;
  ...

public:
  void compute(const std::vector<Polynom<MonomType>>& gens);
  const std::list<Polynom<MonomType>>& result() const;
};
```

Algorithm (approximate):

1. Initialise basis with input generators.
2. Loop:
   - For each polynomial in basis, normalise via involutive
     reduction by the others.
   - If a new polynomial gets added, restart.
   - When no more reductions apply, done.
3. Return basis.

Templated on `MonomType` (one of `MonomLex`, `MonomDL`,
`MonomDRL`) — the comparison operators are inlined at compile
time.

## Why templated, not virtual

The reduction inner loop runs **billions** of times for
non-trivial inputs. Each iteration calls `cmp(a, b)` on monomials.
A virtual dispatch per call would add ~5 ns × billions = seconds.

Templated dispatch: zero overhead, perfectly inlinable.

## Used by

- [`file-launcher.md`](file-launcher.md) — instantiates one copy
  per monomial type.

## Related

- [`README.md`](README.md) — bibasis/ overview.
- [`file-polynom.md`](file-polynom.md) — the polynomial type.
- [`file-pcomparator.md`](file-pcomparator.md) — comparator used
  for basis ordering.
- [`file-tset.md`](file-tset.md), [`file-qset.md`](file-qset.md)
  — auxiliary sets the algorithm maintains.
- [`file-janettree.md`](file-janettree.md) — the Janet-division
  index structure.
