# `overflow-test.cpp` — integer-overflow benchmark / sanity

`overflow-test.cpp` is a **standalone microbenchmark** for the
engine's safe-integer-arithmetic primitives in `overflow.hpp`.
It's not part of the engine library — it's compiled separately
when developers want to measure overflow-check cost.

Part of the [engine](README.md) — utilities (sandbox).

[← engine overview](README.md) · [utilities](utilities.md)

## What it does

```cpp
#define outer 20000
#define inner 5000

#include <stdio.h>
#include "overflow.hpp"
#include "assert.h"
#include <stdlib.h>
#include <string.h>

#if 0
     return 0;
               x = safe::add(j,s); //  /2000000000 repetitions
// this pair of timings shows that there is virtually no loop overhead
#define stmt x = 1, x = 1  // 0m8.973s/5000000000 repetitions, no power
#define stmt x = 1         // 0m4.488s/5000000000 repetitions, no power
```

The `#if 0` block preserves the developer's experiment notes:
inline timings from when the benchmark was last run. They show:

- **`x = 1`** (single assignment): 4.5ns per iteration on the test
  machine.
- **`x = 1, x = 1`** (two assignments): 8.9ns — confirms loop
  overhead is negligible.

The "no power" annotation means **`safe::pow` was excluded** —
pow tests were too slow to fit in the benchmark budget.

## Why a separate binary

The benchmark wants to measure overhead in the **single-digit
nanosecond** range. To do that:

- Compile with **optimisation but no LTO** (so the call sites
  aren't inlined into death).
- Run **billions** of iterations (`outer × inner = 1×10^8` for
  some, more for others).
- No engine overhead — no rings, no matrices, just the safe
  primitives.

A standalone binary lets the developer get clean numbers without
fighting the rest of the engine's setup cost.

## The safe::add etc. that's being tested

`overflow.hpp` provides:

- `safe::add(a, b)` — adds with overflow detection.
- `safe::mult(a, b)` — likewise.
- `safe::pow(a, n)` — power with detection.

Used in degree arithmetic for polynomial computations: a
polynomial with degree-50 monomials and 5 variables can easily
overflow `int` exponent products. The `safe::*` family catches
this and reports an error to the user instead of silently
wrapping.

## Used by

- Engine developers profiling `safe::*` after changes.
- Manual benchmarking — not in the build by default.

## Related

- [`README.md`](README.md) — engine overview.
- [`file-overflow-hpp.md`](file-overflow-hpp.md) if added —
  the `overflow.hpp` primitives being tested.
- [`utilities.md`](utilities.md) — area.
