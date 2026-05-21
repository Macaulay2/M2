# `ARingGFTest.cpp`, `GivaroTest.cpp` — Galois-field tests

`ARingGFTest.cpp` and `GivaroTest.cpp` test the engine's
**Galois-field implementations** — `M2::ARingGF*` (FLINT, M2-side,
table-based) and the Givaro library's field types.

Part of the [engine unit-tests suite](README.md).

[← back to unit-tests overview](README.md) · [← engine overview](../README.md)

## `ARingGFTest.cpp`

```cpp
#include "aring-gf-flint.hpp"
#include "ARingTest.hpp"

static const int nelements = 200;
static int randomVals[nelements] = {
    2666036,  85344567, 71531106, ... };
```

Tests FLINT-backed Galois fields (`M2::ARingGFFlintBig`, etc.).

Why the **hardcoded random table** instead of a generator? The
test wants:

- **Deterministic** runs — flaky tests due to RNG instability are
  the worst kind in CI.
- **Reasonable distribution** — 200 numbers picked at file-write
  time give good coverage.
- **No dependency on the M2 random subsystem** during the test
  itself.

The exact numbers in the array don't matter much; what matters is
they cover the field's value range.

## `GivaroTest.cpp`

```cpp
// Copyright (c) 994-2009 by The Givaro group
...
#include <givaro/modular.h>
#include <givaro/montgomery.h>
#include <givaro/gfq.h>
#include <givaro/gfqext.h>
using namespace Givaro;
template <class Field>
void TestField(const Field& F) { ... }
```

This file is a **smoke test for the Givaro library itself** —
verifies that Givaro's `modular`, `montgomery`, `gfq`, `gfqext`
templates work on this build.

The copyright shows it's adapted from Givaro's own tests (note
the `(c) 994-2009` typo for `1994`). Keeping it in M2's tree
means M2's CI catches Givaro packaging breakage too.

## Why M2 has multiple GF implementations

| Implementation | When used |
|---|---|
| `ARingGFFlintBig` | Large fields where bit-packing matters |
| `ARingGFFlint` | Small-to-medium fields, FLINT-optimised |
| `ARingGFM2` | Pure M2 implementation (fallback) |
| `ARingGFGivaro` | When Givaro is available (often faster) |
| `ARingGFTable` | Tiny fields where a precomputed table beats anything |

Each has trade-offs: setup cost vs. per-operation cost. The test
suite exercises each separately to catch backend-specific bugs.

## Used by

- Engine developers verifying GF changes.
- CI on every PR.

## Related

- [`README.md`](README.md) — unit-tests overview.
- [`file-ARingTest-hpp.md`](file-ARingTest-hpp.md) — fixture.
- [`../file-aring-gf-flint.md`](../file-aring-gf-flint.md) (if
  added) — primary backend.
- [`../coefficient-rings.md`](../coefficient-rings.md) — area.
- Givaro — external linked library.
