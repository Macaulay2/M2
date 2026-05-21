# `res-moninfo-dense.{cpp,hpp}`, `res-moninfo-sparse.{cpp,hpp}` — `ResMonoid` implementations

`ResMonoid` (the dispatcher in
[`file-res-moninfo.md`](file-res-moninfo.md)) has **two concrete
implementations**: `ResMonoidDense` for dense exponent storage and
`ResMonoidSparse` for sparse exponent storage. This doc covers
both implementations together since their interfaces are
parallel.

Part of [`schreyer-resolution/`](README.md).

[← schreyer-resolution/ overview](README.md) · [← engine overview](../README.md)

## `res-moninfo-dense.{cpp,hpp}` — dense exponent storage

```cpp
// Copyright 2016  Michael E. Stillman

#include "schreyer-resolution/res-monomial-types.hpp"
#include "skew.hpp"  // for SkewMultiplication

class ResMonoidDense
{
  int nvars;
  int nslots;
  std::unique_ptr<res_monomial_word[]>
      hashfcn;  // array 0..nvars-1 of hash values for each variable
  res_monomial_word mask;
  std::vector<int> mVarDegrees;  // array 0..nvars-1 of primary (heft) degrees
  ...
};
```

In **dense** storage, every monomial keeps **one slot per
variable**, even for variables with exponent 0:

```
3 vars, monomial x^2 y z^5:
  [2, 1, 5]    (dense)
```

Memory: `nvars * sizeof(int)` per monomial. Fast access:
`exponents[i]` is `O(1)`. Fast comparison: just memcmp the
arrays.

`hashfcn` is a precomputed array — `hashfcn[i]` is a random hash
contribution for variable `i`. The total hash of a monomial is
`sum(exponents[i] * hashfcn[i]) & mask`. Hashing is O(nvars).

## `res-moninfo-sparse.{cpp,hpp}` — sparse exponent storage

```cpp
// Copyright 2016-2017  Michael E. Stillman

// Format for monomials here:
// a. length (in bytes) (int32)
// b. hash value (int32)
// c. component (int32)
// d. v1 v2 ... vd
// where d = length-2
// v1 >= v2 >= ... >= vd >= 0 are indices of variables.
// or, maybe also have degree before this, and other weight values...
```

In **sparse** storage, each monomial stores only the variables
that actually appear:

```
3 vars, monomial x^2 y z^5:
  length=8, hash=..., component=0, [0, 0, 1, 2, 2, 2, 2, 2]
  (8 entries: x appears twice, y once, z five times)
```

The format has 3 prefix slots (length, hash, component) then a
sorted **multiset of variable indices**. Repetition encodes the
exponent.

Memory: `O(total_degree)` per monomial, not `O(nvars)`. Wins big
when monomials are sparse (mostly-zero exponents).

## When to use which

| Use case | Dense | Sparse |
|---|---|---|
| Few variables (< 20) | ✓ | |
| Many variables (> 100) | | ✓ |
| Dense exponents (many nonzero) | ✓ | |
| Sparse exponents (mostly zero) | | ✓ |
| Cache-friendly inner loops | ✓ | |
| Memory-limited inputs | | ✓ |

`ResMonoid` ([`file-res-moninfo.md`](file-res-moninfo.md))
dispatches between the two based on heuristics about the input.

## Both expose the same interface

Both classes provide:

```cpp
void mult(monomial a, monomial b, monomial result);
int compare(monomial a, monomial b);
bool divides(monomial a, monomial b);
res_monomial_word hash(monomial m);
int degree(monomial m);
void to_exponents(monomial m, int* exp);
```

Same names, same semantics, different internal representation.
The interchangeability is what makes `ResMonoid` polymorphism
work.

## `skew.hpp` — skew-commutative support

```cpp
#include "skew.hpp"  // for SkewMultiplication
```

Both files include `skew.hpp`. M2's `ResMonoid` supports
**skew-commutative variables** (variables that anti-commute,
like exterior-algebra generators). Multiplication then includes
sign-handling for the anti-commutation.

## Used by

- [`file-res-moninfo.md`](file-res-moninfo.md) — the dispatcher.
- [`file-res-poly-ring.md`](file-res-poly-ring.md) — uses
  `ResMonoid` to hold polynomial monomials.
- [`file-res-f4.md`](file-res-f4.md) — F4 reduction loop.

## Related

- [`README.md`](README.md) — schreyer-resolution/ overview.
- [`file-res-moninfo.md`](file-res-moninfo.md) — the dispatcher
  picking between these.
- [`file-res-monomial-types.md`](file-res-monomial-types.md) —
  type vocabulary.
- [`../unit-tests/file-ResTest.md`](../unit-tests/file-ResTest.md)
  — tests `ResMonoidDense` specifically.
