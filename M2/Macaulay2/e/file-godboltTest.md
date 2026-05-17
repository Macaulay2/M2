# `godboltTest.cpp` — standalone log/exp table generator (test harness)

`godboltTest.cpp` is a **standalone test harness** for verifying
log/exp/Zech tables for `Z/p` over small primes. The file name
references [Compiler Explorer](https://godbolt.org/) (popularly
"godbolt"), suggesting it was written as a code-style /
microbenchmark experiment.

Part of the engine internals, not a production code path.

[← engine overview](README.md)

## What it contains

```cpp
int exp_table[]  = { 1, 2, 4, 8, 16, 32, 64, 27, 54, 7, /* … */, 0 };
int log_table[]  = { 100, 0, 1, 69, 2, 24, /* … */, 50 };

int exp_table2[] = { 0, 2, 4, /* … */, 1 };
int log_table2[] = { 0, 100, 1, /* … */, 50 };

int p1 = 100;
int p  = 101;
int zero       = 100;
int minus_one  = 50;

// aring-zzp.hpp
void subtract_multiple(int &result, int a, int b) {
    // ...
}
```

The file contains:

- **Precomputed log/exp tables** for `p = 101` (so `p - 1 = 100`).
  Each value `i` in the tables is the log or exp of `i` to the
  primitive root.
- **Two variants** (`_table` vs. `_table2`) — likely different
  treatments of the zero element across the boundary `n = p - 1`.
- A `subtract_multiple` function copied verbatim from
  [`file-aring-zzp.md`](file-aring-zzp.md)'s inner loop.

## Status

A scratch / sandbox file. It is **not** part of the production
build — CMake / autotools exclude it via per-file rules. The author
was likely:

1. Trying out alternative Z/p arithmetic shapes.
2. Comparing assembly output on Compiler Explorer.
3. Pasting the result back into the file as a reference.

Today the production Z/p paths are in
[`file-aring-zzp.md`](file-aring-zzp.md),
[`file-aring-zzp-flint.md`](file-aring-zzp-flint.md), and
[`file-aring-zzp-ffpack.md`](file-aring-zzp-ffpack.md). The tables
in this file are not used at runtime; they're hard-coded for `p =
101` specifically.

## Why keep the file

It is a useful **reference** for "what the table-based path looked
like in isolation, with no engine surrounding scaffolding." A
developer working on a new Z/p back end might consult it as a
worked example.

A future cleanup pass will likely move this file out of `e/` and into
a `docs/` or `examples/` subdir.

## Related

- [`file-aring-zzp.md`](file-aring-zzp.md) — production table-based path.
- [`file-coeffrings.md`](file-coeffrings.md) — `CoefficientRingZZp`
  (similar approach, fully integrated).
