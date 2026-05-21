# `aring-qq-gmp.hpp`, `aring-qq-gmp.cpp` — `ARingQQGMP` (GMP-backed rationals)

`ARingQQGMP` is the **GMP `mpq_t`-backed rational ring** —
arbitrary-precision rationals through GMP. The legacy `ARingQQ`
wraps this for the `Ring` interface.

Part of the [engine](README.md) — coefficient rings.

[← engine overview](README.md) · [coefficient rings](coefficient-rings.md)

## Header

```cpp
// Copyright 2013 Michael E. Stillman

#include "interface/gmp-util.h"  // for mpz_reallocate_limbs
#include "interface/random.h"    // for rawSetRandomQQ

#include "aring.hpp"
#include "buffer.hpp"
#include "ringelem.hpp"
#include <iosfwd>
#include "exceptions.hpp"

// promote needs ring.hpp.  After moving promote out, remove it here!
#include "ring.hpp"

namespace M2 {
/**
   @ingroup rings

   @brief wrapper for the gmp mpq_t integer representation
*/

class ARingQQGMP : public SimpleARing<ARingQQGMP>
```

The doxygen says "wrapper for the gmp mpq_t" — that's exactly
what it is. The TODO "After moving promote out, remove it here!"
flags a pending refactor: `promote` (ring-element conversion to a
larger ring) currently lives partly in `ARingQQGMP`, partly in
`Ring`.

The base class `SimpleARing<ARingQQGMP>` is the CRTP template
that provides default implementations of operations layered on
top of the basics each aring must provide.

## What `ARingQQGMP` provides

The standard `aring` API:

- **`ElementType`** — typedef'd to `mpq_t` (the GMP rational
  type).
- **Initialisation** — `init`, `clear`, `set_zero`, `set_from_long`,
  `set_from_mpz`, `set_from_mpq`.
- **Arithmetic** — `add`, `subtract`, `mult`, `divide`, `negate`,
  `invert`.
- **Comparison** — `is_zero`, `is_equal`, `is_unit`.
- **Random** — `random` produces a rational with `mMaxHeight`
  bound (defaults to 50).

## Implementation details

```cpp
ARingQQGMP::ARingQQGMP()
{
  gmp_randinit_default(mRandomState);
  mMaxHeight = 50;
}

ARingQQGMP::~ARingQQGMP() { gmp_randclear(mRandomState); }
void ARingQQGMP::eval(const RingMap* map,
                      const ElementType& f,
                      int first_var,
                      ring_elem& result) const
{
  ...
  bool ok = map->get_ring()->from_rational(&f, result);
  ...
}
```

The `eval` method handles ring-map evaluation: when M2 evaluates
a `RingMap` on a rational, it eventually reaches here. The
implementation defers to `target_ring->from_rational(...)` since
the target ring knows how to embed `QQ`.

## `ARingQQGMP` vs `ARingQQFlint`

Two parallel implementations:

- **`ARingQQGMP`** (this file) — straight GMP `mpq_t`.
- **`ARingQQFlint`** — FLINT `fmpq_t` (smaller-integer
  optimisations).

Both have identical observable behaviour. M2 picks based on which
is enabled and per-context heuristics. Tests in
[`unit-tests/file-aring-zz-tests.md`](unit-tests/file-aring-zz-tests.md)
cross-validate.

## Used by

- `M2::ConcreteRing<ARingQQGMP>` — bridges to legacy `Ring`.
- M2 user code via `QQ` (or the user picks one via configure).
- Tests in [`unit-tests/file-aring-zz-tests.md`](unit-tests/file-aring-zz-tests.md).

## Related

- [`README.md`](README.md) — engine overview.
- [`file-aring-qq-flint.md`](file-aring-qq-flint.md) — sister
  FLINT implementation.
- [`file-aring-qq.md`](file-aring-qq.md) — legacy wrapper.
- [`coefficient-rings.md`](coefficient-rings.md) — area.
- [`file-aring-zz-gmp.md`](file-aring-zz-gmp.md) — integer
  counterpart.
