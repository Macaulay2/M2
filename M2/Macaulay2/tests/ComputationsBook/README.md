# `M2/Macaulay2/tests/ComputationsBook/` — *Computations in Algebraic Geometry with Macaulay 2*

This directory contains tests derived from **the book** *Computations
in Algebraic Geometry with Macaulay 2* (Eisenbud, Grayson, Stillman,
Sturmfels, eds., Springer 2002). Each chapter's M2 examples is
replayed as a regression test, ensuring that the engine still produces
the same outputs the book documents.

[← back to tests overview](../README.md)

## Per-chapter subdirectories

| Chapter dir | Topic |
|---|---|
| `completeIntersections/` | Complete intersections |
| `constructions/` | Standard constructions |
| `d-modules/` | D-modules |
| `exterior-algebra/` | Exterior algebra |
| `geometry/` | Algebraic geometry basics |
| `monomialIdeals/` | Monomial ideals |
| `patterns/` | Patterns in code |
| `preface/` | Preface examples |
| `programming/` | M2 programming |
| `schemes/` | Schemes |
| `solving/` | Solving systems |
| `toricHilbertScheme/` | Toric Hilbert scheme |
| `varieties/` | Varieties |

## Drivers

| File | Role |
|---|---|
| `CMakeLists.txt` | CTest registration |
| `Makefile.in`, `Makefile.chapter.in` | Autotools build glue (templated per chapter) |
| `capture.m2` | Master script that runs all chapters |
| `README` | Original notes from the test author |

## How tests run

Each chapter directory contains `.m2` scripts (one or more per book
chapter). The `capture.m2` driver runs them in sequence and compares
the captured outputs against canonical files (`.out` files in each
subdirectory).

A test passes if every chapter's actual output matches the canonical
one exactly. Differences typically indicate an engine regression
that changes a Hilbert series, a Betti table, or a Gröbner basis in
a user-visible way.

## Triggering

```sh
ctest -R "ComputationsBook" --output-on-failure
```

The test target is `check-ComputationsBook`. It runs on every PR to
`stable` and `development`.

## Why this suite matters

It is **the** regression check that ensures the engine produces
mathematically identical results across versions. A subtle change in
monomial ordering or normal-form choice would break a printed book —
this suite catches that before it ships.

## Catalogue

[`file-computations-book-catalogue.md`](file-computations-book-catalogue.md)
— full walkthrough of the canonical per-chapter file pattern
(`chapter.m2`, `chapter.out.expected`, `test.m2`, `test.oldvalues`,
`patterns`, ...), how the `capture.m2` value-capture harness works, and
chapter-by-chapter contents.

## Related

- The book itself: *Computations in Algebraic Geometry with Macaulay
  2*, Eisenbud / Grayson / Stillman / Sturmfels (Springer 2002).
- [`../README.md`](../README.md) — overall test-suite overview.
- [`../normal/README.md`](../normal/README.md) — broader regression
  suite.
