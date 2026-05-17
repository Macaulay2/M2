# `BasicPoly.{cpp,hpp}` and `BasicPolyList*` — lightweight polynomial values

`BasicPoly` is a **minimal, portable polynomial value type** the engine
uses where the full machinery of [`Polynomial`](file-Polynomial.md) or
`gbvector` ([`file-gbring.md`](file-gbring.md)) would be overkill —
typically for serialisation, test inputs, and the streaming
constructors of newer GB code.

Part of the [Polynomial rings](polynomial-rings.md) area.

[← per-area: polynomial-rings](polynomial-rings.md) · [← engine overview](README.md)

## Class shape

```cpp
#include "exceptions.hpp"
#include <vector>
#include <iosfwd>
#include <string>
#include <unordered_map>
#include <gmpxx.h>

class BasicPoly {
public:
    std::vector<mpz_class> mCoefficients;
    std::vector<int>       mComponents;   // empty = all components 0
    std::vector<int>       mMonomials;    // concatenated varpower monomials, each prefixed by length

    void clear();                          // reset to zero polynomial
    // ...
};
```

Three parallel arrays:

- **`mCoefficients`** — one `mpz_class` per term. The header comment
  notes the current restriction: coefficients must be integers, and
  a future TODO is to support `GF(p^n)`, `QQ`, fraction fields, or
  polynomial coefficients.
- **`mComponents`** — one integer per term identifying the free-module
  component the term lives in. An empty vector is shorthand for
  "scalar polynomial; all components are 0."
- **`mMonomials`** — a flat concatenation of varpower-monomial encodings.
  Each entry starts with its own length, so the array can be walked
  sequentially.

## Why a separate, minimal type

The engine's main polynomial types
([`Polynomial`](file-Polynomial.md), `gbvector`, `BasicPoly`)
each optimise for different workflows:

| Type | Best at | Stored in |
|---|---|---|
| `Polynomial` / `Poly` ([`file-Polynomial.md`](file-Polynomial.md)) | NC + new F4 | Column-store + shared monomial table |
| `gbvector` ([`file-gbring.md`](file-gbring.md)) | Buchberger GB | Sorted intrusive linked list |
| `BasicPoly` (this file) | Portable serialised I/O | Parallel `std::vector`s with `mpz_class` |

`BasicPoly` is the **portable** choice: it doesn't depend on a specific
ring instance and can be serialised / parsed without knowing the
target's monomial layout.

## Companion classes

### `BasicPolyList`

A `std::vector<BasicPoly>` plus a few convenience methods. Used as the
shared container type by `gb-f4` (see
[`gb-f4/file-PolynomialList.md`](gb-f4/file-PolynomialList.md)) and by
the engine's streaming I/O routines.

### `BasicPolyListParser`

A parser that reads a textual `BasicPolyList` representation —
useful for unit tests and for feeding example inputs to F4 without
constructing M2 objects. The format is intentionally simple:
human-readable text with one polynomial per line, term-by-term.

## Limitations

Per the header's TODOs:

- **Coefficients must currently be `mpz_class`** (integers).
- No support yet for `GF(p^n)`, `QQ`, fraction fields, polynomial
  coefficients.
- Single monomial encoding (varpower-style); no choice between dense
  and sparse.

These are flagged for future work.

## Related

- [`polynomial-rings.md`](polynomial-rings.md) — area overview.
- [`file-Polynomial.md`](file-Polynomial.md) — heavier modern type.
- [`file-gbring.md`](file-gbring.md) — `gbvector` (GB inner-loop type).
- [`gb-f4/file-PolynomialList.md`](gb-f4/file-PolynomialList.md) —
  primary consumer.
