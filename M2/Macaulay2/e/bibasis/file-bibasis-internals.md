# `triple.hpp`, `tset.hpp`, `qset.hpp`, `pcomparator.hpp`, `settings-manager.{hpp,cpp}` — BIBasis internal data structures

Five smaller files providing the **internal data structures** the
involutive-basis algorithm maintains: triples, the T-set, the
Q-set, polynomial comparators, and the settings manager.

Part of [`bibasis/`](README.md).

[← bibasis/ overview](README.md) · [← engine overview](../README.md)

## `triple.hpp` — `(Polynomial, Variables, MultiplicativeVars)`

```cpp
#include <set>
#include <string>
#include "allocator.hpp"
```

A `Triple` is the **fundamental unit** the involutive-basis
algorithm operates on. Each Triple carries:

- A **polynomial** (the actual algebraic object).
- A set of **non-multiplicative variables** — variables the
  involutive division forbids multiplying by.
- A set of **multiplicative variables** — variables the
  involutive division allows.

The non-mult / mult split is the *signature* of Janet division:
it's what makes the involutive form deterministic.

## `tset.hpp` — the T-set (terminal set)

```cpp
#include <list>
#include <algorithm>
#include "janettree.hpp"
```

The **T-set** is the *current involutive basis* — the set of
"terminal" triples the algorithm has processed and confirmed in
the basis. Indexed by [`JanetTree`](file-janettree.md) for fast
involutive-division lookups.

Operations:

- `Insert(triple)` — add to T-set, update Janet tree.
- `FindDivisor(monom)` — find a triple whose leading monomial
  involutively divides `monom`.
- `IsAutoReduced(t)` — check if triple `t` is reduced w.r.t. the
  rest.

## `qset.hpp` — the Q-set (queue set)

```cpp
#include <list>
#include <algorithm>
#include "triple.hpp"
```

The **Q-set** is the *queue of pending triples* — triples
discovered but not yet processed.

The main algorithm loop pops from Q, processes via T, and
pushes new findings back to Q. When Q is empty, the involutive
basis is complete.

## `pcomparator.hpp` — polynomial comparator

```cpp
#include <string>
#include "error.h"
```

A function-object that compares two polynomials for sort order.
Used by `std::set<Polynom, PComparator>` in the involutive
algorithm.

The comparison is **lexicographic on leading monomials** — newer
polynomials with smaller leading terms sort earlier. This
ordering matters for the algorithm's correctness, not just
deterministic output.

## `settings-manager.{hpp,cpp}` — algorithm parameters

```cpp
#include "monom.hpp"

namespace BIBasis
{
```

A simple **parameters singleton** holding:

- **`numberOfVariables`** — fixed at run start.
- **`maxDegree`** — degree cutoff.
- **`monomialOrder`** — which order to use.

The launcher initialises this once; the rest of the algorithm
reads from it.

```cpp
SettingsManager::Instance().SetNumberOfVariables(n);
SettingsManager::Instance().SetMaxDegree(d);
SettingsManager::Instance().SetMonomialOrder(MonomLex::Order());
```

Why a singleton: too many classes need the parameters; passing
them through every constructor would clutter the API. Singleton
is the cleanest path even though it's not great style.

## Used by

- [`file-involutive.md`](file-involutive.md) — primary consumer
  of T/Q sets and triples.
- [`file-launcher.md`](file-launcher.md) — initialises settings.
- All bibasis files — share `FastAllocator` via `triple.hpp`.

## Related

- [`README.md`](README.md) — bibasis/ overview.
- [`file-involutive.md`](file-involutive.md) — algorithm consuming
  these.
- [`file-janettree.md`](file-janettree.md) — index over T-set.
- [`file-allocator.md`](file-allocator.md) — used pervasively.
- [`file-monom.md`](file-monom.md), [`file-monom-orders.md`](file-monom-orders.md)
  — monomials these data structures hold.
