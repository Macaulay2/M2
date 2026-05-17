# `mpreal.h` — vendored MPFR C++ wrapper

`mpreal.h` is a **vendored copy of `mpreal`** by Pavel Holoborodko
— an MPFR C++ wrapper that provides operator-overloaded
arbitrary-precision real arithmetic on top of MPFR's C API.

Part of the [engine](README.md) — vendored third-party.

[← engine overview](README.md)

## What it is

```c
/*
    MPFR C++: Multi-precision floating point number class for C++.
    Based on MPFR library:    http://mpfr.org

    Project homepage:    http://www.holoborodko.com/pavel/mpfr
    Contact e-mail:      pavel@holoborodko.com

    Copyright (c) 2008-2022 Pavel Holoborodko

    Contributors:
    Dmitriy Gubanov, Konstantin Holoborodko, ... etc.
```

A single-header library — `mpreal.h` is the entire thing. It
provides:

- `mpfr::mpreal` — a C++ class wrapping `mpfr_t`.
- Operator overloads (`+`, `-`, `*`, `/`, comparison) so you can
  write `mpreal a = b * c + d;` naturally.
- All standard math functions: `sin`, `cos`, `exp`, `log`, etc.
- IEEE-754 special values: `nan()`, `inf()`.
- Stream I/O — `cout << a;`.

## Why vendored

`mpreal` isn't usually packaged separately by Linux distros, so
M2 vendors it. The license is GPL/LGPL (matches M2's licensing
constraints). Single header, no build glue needed.

Vendoring means:

- M2 builds work without needing a system `mpreal`.
- M2 controls the version (won't be surprised by upstream
  changes).
- Trade-off: M2 has to update vendored copies manually when
  newer versions ship.

## Where it's used

M2's engine mostly uses **MPFR's C API directly** through
`ARingRRR`. `mpreal.h` shows up in places where operator overloads
make code dramatically more readable:

- `boostmath.dd` (in [`../d/file-boostmath.md`](../d/file-boostmath.md))
  needs `boost::multiprecision::mpfr_float` which itself uses
  `mpreal`-shaped types.
- Some experimental numeric-analysis files that prefer C++
  expressions to C function calls.

## Why not just always use `mpreal`?

Two reasons:

1. **Inner-loop performance** — the C API can express
   "no-allocation reuse-this-result" patterns; the C++
   value-semantics API can't.
2. **Footprint** — MPFR's C API alone is enough for most work;
   pulling in mpreal everywhere adds compile time.

## Used by

- `boostmath.dd` via Boost.Multiprecision.
- Numerical-analysis paths that need readable expressions.

## Related

- [`README.md`](README.md) — engine overview.
- [`file-aring-RRR.md`](file-aring-RRR.md) — M2's primary MPFR
  wrapping path.
- [`../d/file-boostmath.md`](../d/file-boostmath.md) —
  Boost.Math binding that uses mpreal-shaped types.
- MPFR, Boost.Multiprecision — external linked libraries.
