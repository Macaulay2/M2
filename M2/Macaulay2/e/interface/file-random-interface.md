# `random.{h,cpp}` (in `interface/`) — public C entry points for the engine RNG

`interface/random.h` declares the **public C functions** the interpreter
uses to seed and query the engine's pseudo-random number generator. Many
internal engine paths take a seed indirectly via these functions.

Part of the [`interface/`](README.md) subdirectory.

[← interface overview](README.md) · [← engine overview](../README.md)

## Header shape

```c
#include "engine-includes.hpp"

#if defined(__cplusplus)
extern "C" {
#endif

void rawRandomInitialize();
void rawSetRandomSeed(gmp_ZZ newseed);
void rawSetRandomMax(gmp_ZZ);
// ...
#if defined(__cplusplus)
}
#endif
```

No opaque classes — the RNG state is engine-global (thread-local where
threads matter). Inputs use `gmp_ZZ` (GMP integer pointer) to allow
arbitrary-precision seeds.

## Entry points

- **`rawRandomInitialize()`** — seed the RNG to its default initial
  state. Called by the interpreter at startup.
- **`rawSetRandomSeed(seed)`** — set the seed explicitly. Lets users
  reproduce a previous random sequence by passing the same seed.
- **`rawSetRandomMax(max)`** — set the upper bound for random integer
  draws.
- **`rawRandomInteger(max)`** — draw a random integer in `[0, max)`.
- **`rawRandomQQ(max)`** — draw a random rational with bounded
  numerator / denominator.
- **`randomDouble()`** — draw a random `double` in `[0, 1)`. Used by
  [`../file-aring-RR.md`](../file-aring-RR.md) and friends.

## Determinism

The RNG state is **thread-local**: parallel paths each have their own
RNG, seeded from a global seed at thread creation. This means:

- Single-threaded runs are fully reproducible from a given seed.
- Multi-threaded runs are reproducible *for the same thread count* but
  not across different counts (the per-thread seed sequence depends on
  the number of workers).

Engine code that needs strict determinism (regression tests,
floating-point-sensitive outputs) takes care to use one thread.

## Implementation

The actual RNG is a Mersenne Twister with a 64-bit state. The
implementation lives in `random.cpp` and delegates to FLINT's
`flint_rand_t` for FLINT-backed aring random draws.

## Related

- [`README.md`](README.md) — interface overview.
- [`../file-aring-RR.md`](../file-aring-RR.md), `aring-CC`, `aring-zzp-flint`,
  etc. — primary consumers via `randomDouble` / `rawRandomInteger`.
- [`../file-NAG.md`](../file-NAG.md) — NAG path tracking uses the RNG.
