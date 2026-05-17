# `M2/submodules/` — vendored upstream sources

Git submodules pointing at the upstream repositories of libraries and programs
Macaulay2 links against. The goal is for everything in
[`M2/libraries/`](../libraries/README.md) to eventually have a corresponding
submodule here, but the conversion is incremental.

| Submodule | Purpose |
|---|---|
| `bdwgc` | The Boehm-Demers-Weiser conservative garbage collector — M2's GC throughout the engine, interpreter, and supervisor |
| `flint` | Fast Library for Number Theory — coefficient arithmetic in `e/aring-*-flint.*` |
| `frobby` | Monomial ideal computations |
| `fflas_ffpack` | Dense linear algebra over finite fields |
| `givaro` | Finite-field arithmetic (used together with fflas-ffpack) |
| `googletest` | Test framework for `Macaulay2/e/unit-tests/` |
| `mathic` | Generic algebraic data structures |
| `mathicgb` | Signature-based Gröbner basis engine |
| `memtailor` | Custom memory allocator used by mathic / mathicgb |

The `M2-emacs` submodule lives separately, at
[`Macaulay2/editors/emacs/`](../Macaulay2/editors/README.md).

## Initialization

By default `git clone` does **not** pull submodules — `configure` will detect
whether each library is already installed on the system. To check out
everything for a fully self-contained source tree:

```sh
git clone --recursive https://github.com/Macaulay2/M2
# or, after the fact:
git submodule update --init --recursive
```

## Adding a new submodule

Follow the same instructions as in [`../libraries/README`](../libraries/README)
for adding a new library or program.

## Related

- [`M2/libraries/`](../libraries/README.md) — per-library build wrappers
  (autotools).
- [`M2/cmake/`](../cmake/README.md) — `Find*.cmake` + `build-libraries.cmake`
  equivalents (CMake).

[← back to repository TOC](../../README.md#under-m2)
