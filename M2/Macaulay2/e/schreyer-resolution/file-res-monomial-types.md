# `res-monomial-types.hpp` — resolution-side typed integers and encodings

`res-monomial-types.hpp` declares the **typed integer aliases and
monomial-encoding typedefs** the Schreyer-resolution code uses. It is
the resolution-side counterpart of
[`gb-f4/file-MonomialTypes.md`](../gb-f4/file-MonomialTypes.md): same
idea, different specific types and conventions.

Part of the [`schreyer-resolution/`](README.md) subdirectory.

[← schreyer-resolution overview](README.md) · [← engine overview](../README.md)

## Type aliases

```cpp
#include <cstdint>

#include "ExponentList.hpp"
#include "ExponentVector.hpp"

enum class MonomialOrderingType { Lex, GRevLex, Weights };

typedef int32_t myword;
typedef myword  component_index;

// Legacy specialization for ExponentVector
using res_ntuple_monomials = ExponentVector<myword, false>;
typedef res_ntuple_monomials::Exponent  res_ntuple_word;
typedef res_ntuple_word                *res_ntuple_monomial;
typedef const res_ntuple_word          *res_const_ntuple_monomial;

// Legacy specialization for ExponentList
using res_varpower_monomials      = ExponentList<myword, false>;
using index_res_varpower_monomial = ExponentListIterator<myword, false>;
```

Two parallel encodings:

- **`res_ntuple_monomial`** — dense exponent vector, indexed by variable.
- **`res_varpower_monomial`** — sparse `(variable, exponent)` list.

Both are `int32_t`-keyed (`myword`). The dual `cpp`/`hpp` design lets
the resolution algorithm pick whichever encoding it wants per call site,
without needing to specialise downstream code.

## `MonomialOrderingType`

```cpp
enum class MonomialOrderingType { Lex, GRevLex, Weights };
```

A small subset of the engine's full monomial-ordering vocabulary
(compare with [`interface/file-monomial-ordering-interface.md`](../interface/file-monomial-ordering-interface.md)).
The resolution code supports only `Lex`, `GRevLex`, and `Weights` —
the three orderings that have a clean Schreyer interpretation.

## Why a separate vocabulary

The resolution code wants compact, cache-friendly representations
tuned to its access pattern. Top-level types like
[`ExponentList`](../file-ExponentList.md) and
[`ExponentVector`](../monoids-and-monomials.md) are more general; this
file specialises them for resolution use.

`myword` is `int32_t` — smaller than the engine's standard `int`,
matching the resolution code's exponent-size assumptions.

## Used by

- [`file-res-moninfo.md`](file-res-moninfo.md) (and its `dense` /
  `sparse` siblings) — defines `ResMonoid` on top of these types.
- [`file-res-schreyer-frame.md`](file-res-schreyer-frame.md) — frame
  entries use `component_index`.
- [`file-res-poly-ring.md`](file-res-poly-ring.md) — `ResPolynomial`
  uses `myword` for monomials.
- [`file-res-f4-monlookup.md`](file-res-f4-monlookup.md) — uses both
  encodings as the `Key` parameter.

## Related

- [`README.md`](README.md) — schreyer-resolution overview.
- [`../gb-f4/file-MonomialTypes.md`](../gb-f4/file-MonomialTypes.md)
  — sibling vocabulary in the refactored F4.
- [`../file-ExponentList.md`](../file-ExponentList.md), `ExponentVector.hpp`
  — top-level templates this header specialises.
