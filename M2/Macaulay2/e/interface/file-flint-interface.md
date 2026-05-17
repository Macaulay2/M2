# `flint.{h,cpp}` (in `interface/`) — public C entry points for FLINT-backed services

`interface/flint.h` declares the **public C functions** the interpreter
uses to access FLINT's primality, factorisation, and similar number-theory
services. The FLINT library is vendored as a
[submodule](../../../submodules/README.md).

Part of the [`interface/`](README.md) subdirectory.

[← interface overview](README.md) · [← engine overview](../README.md)

## Header shape

```c
#include "engine-includes.hpp"

#if defined(__cplusplus)
extern "C" {
#endif

M2_bool       rawZZisPrime(gmp_ZZ a);
M2_bool       rawZZisProbablePrime(gmp_ZZ a);
gmp_arrayZZ   rawZZfactor(gmp_ZZ a);

#if defined(__cplusplus)
}
#endif
```

The interface is intentionally tight — only three functions. FLINT is
used pervasively *inside* the engine (in the `aring-*-flint.{cpp,hpp}`
files, in `dmat-*-flint.hpp`, in `polyroots.cpp`) but most of those uses
are private to a specific class. This header is the **only** place where
FLINT-backed services are exposed directly to the interpreter.

## Entry points

- **`rawZZisPrime(a)`** — exact primality test via FLINT's
  `fmpz_is_prime`. Returns `M2_bool` (`true` / `false`). Used by M2's
  `isPrime` built-in.
- **`rawZZisProbablePrime(a)`** — Miller-Rabin probabilistic test via
  FLINT. Faster than `rawZZisPrime` for huge integers.
- **`rawZZfactor(a)`** — full factorisation via FLINT, returned as a
  `gmp_arrayZZ` of alternating primes and exponents.

All three accept `gmp_ZZ` (an `mpz_t*` from GMP) and route through
GMP→FLINT conversion internally.

## Why a separate header

These functions live in `interface/flint.h` rather than a more obvious
"number theory" header because the legacy organisation kept anything
FLINT-specific in its own file. Future cleanups may move them — see the
discussion in [`../README.md`](../README.md) about migration away from
the flat layout.

## Related

- [`README.md`](README.md) — interface overview.
- [`../coefficient-rings.md`](../coefficient-rings.md) — broader FLINT
  consumption via `aring-*-flint`.
- FLINT submodule under [`../../../submodules/README.md`](../../../submodules/README.md).
- [`file-factory-interface.md`](file-factory-interface.md) — sibling for
  polynomial GCD / factorisation.
