# `aring-tower.{cpp,hpp}` — `M2::ARingTower` (iterated finite extension)

`aring-tower.cpp` implements an **iterated tower of finite-field
extensions** — used to build very large Galois fields by composition of
simpler extensions. It is the engine's general-purpose path for
`GF(p^k)` with large `k`.

Part of the [Coefficient rings](coefficient-rings.md) area.

[← per-area: coefficient-rings](coefficient-rings.md) · [← engine overview](README.md)

## Header preamble

```cpp
#include <vector>
#include <string>

#include "ExponentVector.hpp"
#include "aring-zzp-ffpack.hpp"
#include "style.hpp"
#include "aring.hpp"
#include "ringelem.hpp"

namespace M2 {

typedef struct ARingPolynomialStruct *ARingPolynomial;

struct ARingPolynomialStruct {
    int deg;
    int len;
    // ... coefficients, etc.
};

}
```

Three things to notice:

1. The header pulls in [`file-aring-zzp-ffpack.md`](file-aring-zzp-ffpack.md)
   for the **base field arithmetic**: the bottom of the tower is `Z/p`
   via FFLAS-FFPACK.
2. `ExponentVector.hpp` ([`file-ExponentList.md`](file-ExponentList.md))
   supplies the monomial encoding for the **intermediate polynomials**.
3. `ARingPolynomial` is a custom struct holding a polynomial in one
   variable over the next level down — the basic unit of a tower
   element.

## How a tower works

A tower is built as:

```
L_0 = Z/p                              (base field)
L_1 = L_0[t_1] / f_1(t_1)               (extension by minimal poly)
L_2 = L_1[t_2] / f_2(t_2)               (further extension)
...
L_k = L_{k-1}[t_k] / f_k(t_k)
```

A field element at level `k` is a polynomial in `t_k` of degree less
than `deg(f_k)`, with coefficients in `L_{k-1}`. Multiplication at
level `k` is polynomial multiplication followed by reduction modulo
`f_k`.

`ARingTower` represents an element as a recursive `ARingPolynomial`
whose coefficients are themselves `ARingPolynomial`s, until the base
case `Z/p` is reached.

## Why useful

Direct construction of `GF(p^k)` for large `k` requires:

- A primitive polynomial of degree `k` over `Z/p`.
- Storage and tables sized for `p^k - 1` elements.

For `p = 2, k = 60`, that's `2^60 - 1` ≈ `10^18` — far too large to
tabulate. A tower lets the same field be built as e.g.
`((Z/2)[t_1]/f_1)[t_2]/f_2` with much smaller individual extensions.
Memory drops from `O(p^k)` to `O(sum of subfield sizes)`.

## Trade-off vs. `aring-gf-flint-big`

| Aspect | `ARingTower` | `aring-gf-flint-big` |
|---|---|---|
| Storage | Recursive polynomials | Single polynomial mod a quotient |
| Multiplication cost | Walks the tower | Single FLINT call |
| Best for | Custom subfield structure | Standard `GF(p^k)` |

The dispatcher picks based on whether the user specified an explicit
tower structure.

## Related

- [`coefficient-rings.md`](coefficient-rings.md) — area overview.
- [`file-aring-gf-flint.md`](file-aring-gf-flint.md) — single-step FLINT
  alternative.
- [`file-aring-zzp-ffpack.md`](file-aring-zzp-ffpack.md) — base layer.
- [`file-ExponentList.md`](file-ExponentList.md) — polynomial encoding.
