# `PolynomialList.{cpp,hpp}` — list of polynomials in the new F4

`PolynomialList` is a **typed container of polynomials** keyed against a
specific [`MonomialHashTable`](file-MonomialHashTable.md). It is the
storage class for the F4 input and any intermediate polynomial set the
algorithm produces.

Part of the [`gb-f4/`](README.md) subdirectory.

[← gb-f4 overview](README.md) · [← engine overview](../README.md)

## State

```cpp
#include "../VectorArithmetic.hpp"
#include "../BasicPolyList.hpp"
#include "MonomialHashTable.hpp"
#include "MonomialTypes.hpp"
#include "PolynomialStream.hpp"

namespace newf4 {

class Polynomial;

class PolynomialList {
private:
    const VectorArithmetic   &mVectorArithmetic;
    MonomialHashTable        &mHashTable;
    std::vector<Polynomial>   mPolynomials;

public:
    PolynomialList(const VectorArithmetic &VA, MonomialHashTable &hashTable)
        : mVectorArithmetic(VA), mHashTable(hashTable)
    { /* … */ }
    // ...
};

}
```

Three pieces of state:

- A **reference to a `VectorArithmetic`** ([`../file-VectorArithmetic.md`](../file-VectorArithmetic.md))
  — provides coefficient arithmetic.
- A **reference to a `MonomialHashTable`** ([`file-MonomialHashTable.md`](file-MonomialHashTable.md))
  — provides monomial indexing.
- A **vector of `Polynomial`** values.

The references mean the list **does not own** the arithmetic or hash
table — those are shared across the whole F4 computation. The list owns
only the `Polynomial` vector.

## Tied to a hash table

The key design choice: every `Polynomial` in the list references monomials
**by index into the shared hash table**, not by value. Two consequences:

- Polynomials are tiny — a vector of `(coefficient, index)` pairs.
- The hash table and the list must have matching lifetimes; the
  comment "This class will store the input to the GB commands, as well
  as any intermediate polynomials encountered along the way" makes
  clear they're scoped together.

## Used by

- [`file-GBF4Computation.md`](file-GBF4Computation.md) — owns the list of
  input polynomials as `mInput` and uses additional lists for
  intermediate state.
- [`file-Basis.md`](file-Basis.md) — uses `PolynomialList` as its
  underlying storage for basis elements.

## Streaming construction

The header includes `PolynomialStream.hpp` — `PolynomialList` can be
built incrementally via a streaming interface, which lets the
[`BasicPolyListParser`](../polynomial-rings.md) feed polynomials in
without an intermediate buffer.

## Related

- [`README.md`](README.md) — gb-f4 overview.
- [`file-MonomialHashTable.md`](file-MonomialHashTable.md), [`file-Basis.md`](file-Basis.md),
  [`file-GBF4Computation.md`](file-GBF4Computation.md) — sibling classes.
- [`../file-Polynomial.md`](../file-Polynomial.md) — top-level
  `Poly` value type.
- [`../file-VectorArithmetic.md`](../file-VectorArithmetic.md) — arithmetic
  dispatcher.
