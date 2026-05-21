# `cra.{cpp,hpp}` — `ChineseRemainder` (CRT / rational reconstruction internals)

`cra.cpp` implements the **`ChineseRemainder`** class — the engine's
internal CRT / rational-reconstruction machinery. The public C entry
points live in [`interface/file-cra-interface.md`](interface/file-cra-interface.md);
this file is the implementation behind them.

Part of the [Other computations](computations.md) area.

[← per-area: computations](computations.md) · [← engine overview](README.md)

## Header preamble

```cpp
#include "cra.hpp"

#include <assert.h>
#include <stddef.h>

#include "error.h"
#include "freemod.hpp"
#include "matrix-con.hpp"
#include "matrix.hpp"
#include "monoid.hpp"
#include "poly.hpp"
#include "ring.hpp"
#include "style.hpp"

void ChineseRemainder::CRA0(mpz_srcptr a, /* ... */) {
    // ...
}
```

The implementation works directly with GMP `mpz_srcptr` values to
avoid the allocation overhead of going through the engine's wrapped
ring-element types in the inner loop.

## What `ChineseRemainder` provides

The class is a **static-method namespace** for:

- **`CRA0(a, mod_a, b, mod_b)`** — basic two-modulus CRT lift.
  Returns the unique value `c mod (mod_a · mod_b)` such that
  `c ≡ a mod mod_a` and `c ≡ b mod mod_b`.
- **`CRA(...)` for multiple moduli** — fold a list of (value, modulus)
  pairs into a single combined lift.
- **`ratreconstruct(c, mod, a, b)`** — given an integer `c` mod some
  big `mod`, find the unique pair `(a, b)` with `a/b ≡ c mod mod`
  and `|a|, |b| ≤ √(mod / 2)`. This is rational reconstruction in
  the sense of the Wang/Pan-Wang algorithm.

The methods also work on `RingElement*` and `Matrix*` arguments,
walking the input element-wise.

## How it's used

A typical workflow:

1. Pick `k` primes `p_1, …, p_k` whose product exceeds the expected
   output size by a safety margin.
2. Run the actual computation (e.g. a GB) mod each `p_i`. These runs
   are independent — they can be parallelised.
3. Use `CRA` to lift the `k` results into a single result mod
   `p_1 · … · p_k`.
4. Use `ratreconstruct` to recover the rational answer.

Step 2 is the bulk of the work; the engine wrappers in
[`interface/file-cra-interface.md`](interface/file-cra-interface.md)
hide the rest from the M2 user.

## Naming overlap

This file is the **internal implementation** at the engine top level.
The header [`interface/cra.h`](interface/file-cra-interface.md) is the
**public interface**. Both exist; the deep-dive linked above describes
the public side.

## Related

- [`computations.md`](computations.md) — area overview.
- [`interface/file-cra-interface.md`](interface/file-cra-interface.md)
  — public C entry points.
- [`file-aring-zz-flint.md`](file-aring-zz-flint.md), [`file-aring-qq-flint.md`](file-aring-qq-flint.md)
  — the ZZ / QQ arithmetic this builds on.
- FLINT and GMP — external libraries used internally.
