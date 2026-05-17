# `ntl-debugio.cpp`, `ntl-internal.cpp` — NTL integration glue

`ntl-debugio.cpp` and `ntl-internal.cpp` are the **glue files**
between M2 and [NTL](https://libntl.org/) (Victor Shoup's Number
Theory Library) — debug printing and namespace-isolated internal
operations.

Part of the [engine](README.md) — utilities.

[← engine overview](README.md) · [utilities](utilities.md)

## `ntl-debugio.cpp`

```cpp
// Copyright 2005, Michael Stillman

#include <iostream>
#include "ntl-interface.hpp"

void dntl_matZZ(const NTL::mat_ZZ *A) { std::cout << *A << std::endl; }
void dntl_ZZ(const NTL::ZZ *f) { std::cout << *f << std::endl; }
```

Two **debug-print helpers** for NTL types:

- `dntl_matZZ(A)` — print an `NTL::mat_ZZ`.
- `dntl_ZZ(f)` — print an `NTL::ZZ`.

The `d` prefix is a debugger-friendly convention: from gdb you
can `call dntl_matZZ(A)` to dump an NTL matrix.

## `ntl-internal.cpp`

```cpp
// Copyright 2005, Michael Stillman

// This file contains routines which often conflict with our names

#include <cstdio>

#include <M2/config.h>
#include <M2/gc-include.h>
#include <stddef.h>
#include "ntl-interface.hpp"

void ntl_ZZ_to_mpz(mpz_t result, const NTL::ZZ &a)
// Assumption: 'result' is already 'init'ed
// I could imagine there is a faster way to do this, but currently the only
// place
```

The "**routines which often conflict with our names**" comment
explains the file split: NTL's namespace has functions whose
names clash with M2's internal helpers when both are visible.
Isolating these in a dedicated `.cpp` keeps the conflict
contained.

Examples of conflict-prone names: `gcd`, `random`, `mul`,
`norm`, `print`. NTL exposes them in `namespace NTL`; M2 has its
own with the same names. The wrapper file `using namespace NTL;`
locally, performs the conversion, and the rest of the engine never
sees the conflict.

## What NTL is used for

In M2, NTL backs:

- **LLL** (lattice reduction) — `LLL M` via
  [`file-LLL.md`](file-LLL.md) and `LLLglue.cpp`.
- **Polynomial factorisation over `ZZ`** (van Hoeij algorithm).
- **Some primality testing** (older path).
- **`ZZ` arithmetic** in places where NTL beats GMP for specific
  patterns.

FLINT has subsequently taken over many of these — NTL remains as a
fallback / option.

## Used by

- `LLLglue.cpp` ([`file-LLL.md`](file-LLL.md)).
- Polynomial factorisation paths.
- Developers debugging NTL-using code via the `dntl_*` helpers.

## Related

- [`README.md`](README.md) — engine overview.
- [`file-LLL.md`](file-LLL.md) — primary consumer.
- [`file-ntl-interface.md`](file-ntl-interface.md) if added —
  shared NTL helpers header.
- [`file-fplll.md`](file-fplll.md) — alternative LLL backend.
- NTL — external linked library.
