# Coefficient rings (`e/aring-*`, `ZZ`, `ZZp`, `GF`, `coeffrings`)

This area implements the **coefficient rings** used throughout the engine —
the ground rings polynomials and matrices have entries in. Coefficient rings
sit at the bottom of the [engine architecture](README.md#subdirectories): they
have no upward dependencies, but nearly everything else depends on them.

[← engine overview](README.md) · [← top-level TOC](../../../README.md#engine-deep-dive-m2macaulay2e) · [per-area docs](README.md#top-level-files-per-area-docs)

## Two parallel APIs

There are two coexisting interfaces to coefficient rings in this codebase:

1. **`Ring`** — the legacy virtual-base class (`ring.{cpp,hpp}` in
   [`interface/`](interface/README.md)). Polymorphic, but dispatch goes
   through virtual calls.
2. **`aring`** ("abstract ring") — newer, template-based. Each ring is a
   concrete struct; performance-critical code is templated on the ring type so
   the compiler inlines arithmetic. The `aring*.hpp` files in this directory
   are the bulk of new work.

`aring-glue.hpp`, `aring-translate.hpp`, and `aring-wrap.{cpp,hpp}` bridge the
two APIs. They allow a legacy `Ring*` to wrap an `aring` instance (and vice
versa) so old code can call new and vice-versa.

## Files

### Core abstractions

| File pair | Purpose |
|---|---|
| `aring.{cpp,hpp}` | Tag enum + dispatcher base for all aring types. **Deep dive:** [`file-aring.md`](file-aring.md) |
| `aring-glue.hpp` | Templates that build a legacy `Ring` wrapper around an aring |
| `aring-translate.hpp` | Compile-time translation between ring types (e.g. promoting `int` to `mpz_t`) |
| `aring-wrap.{cpp,hpp}` | Concrete wrapper instantiations |
| `coeffrings.{cpp,hpp}` | Registry: maps a coefficient-ring tag to its concrete type |

### Integers (ZZ)

| File pair | Backed by |
|---|---|
| `ZZ.{cpp,hpp}` | Legacy ZZ (GMP) |
| `aring-zz-gmp.{cpp,hpp}` | aring ZZ using `mpz_t` |
| `aring-zz-flint.{cpp,hpp}` | aring ZZ using FLINT's `fmpz_t` |

### Rationals (QQ)

| File pair | Backed by |
|---|---|
| `aring-qq.hpp` | aring QQ — header-only abstract base |
| `aring-qq-gmp.{cpp,hpp}` | QQ via GMP's `mpq_t` |
| `aring-qq-flint.{cpp,hpp}` | QQ via FLINT's `fmpq_t` |

### Finite prime fields (Z/p)

| File pair | Notes |
|---|---|
| `ZZp.{cpp,hpp}` | Legacy Z/p |
| `aring-zzp.{cpp,hpp}` | Generic aring Z/p (table-based for small p) |
| `aring-zzp-flint.{cpp,hpp}` | Z/p via FLINT (`nmod`) |
| `aring-zzp-ffpack.{cpp,hpp}` | Z/p via FFLAS-FFPACK (BLAS-style dispatch) |

### Galois fields (GF)

| File pair | Notes |
|---|---|
| `GF.{cpp,hpp}` | Legacy GF (table-based, characteristic small) |
| `aring-m2-gf.{cpp,hpp}` | Native M2 implementation |
| `aring-gf-flint.{cpp,hpp}` | GF via FLINT for small extension |
| `aring-gf-flint-big.{cpp,hpp}` | GF via FLINT for large extension |

### Real and complex numbers

| File pair | Precision model |
|---|---|
| `aring-RR.{cpp,hpp}` | Hardware `double` |
| `aring-RRR.{cpp,hpp}` | Arbitrary-precision via MPFR |
| `aring-RRi.{cpp,hpp}` | Real intervals (Arb / MPFI) |
| `aring-CC.{cpp,hpp}` | Complex (pair of `double`) |
| `aring-CCC.{cpp,hpp}` | Complex with MPFR precision |
| `aring-CCi.{cpp,hpp}` | Complex intervals |

### Iterated extensions

| File pair | Purpose |
|---|---|
| `aring-tower.{cpp,hpp}` | Tower of finite extension rings (used to build large GF efficiently) |

## How to add a new coefficient ring

1. Choose a representative concrete C type for elements (e.g. `mpz_t`,
   `fmpq_t`, `double`).
2. Copy an existing pair, e.g. `aring-zz-flint.{cpp,hpp}` → `aring-foo.{cpp,hpp}`,
   and rewrite the operations.
3. Register the type in [`coeffrings.cpp`](coeffrings.cpp).
4. Add `case` arms in `aring.{cpp,hpp}` if downstream code dispatches by tag.
5. Add a unit test in [`unit-tests/ARingFooTest.cpp`](unit-tests/README.md).
6. Expose construction through [`interface/aring.{h,cpp}`](interface/README.md)
   if user code should be able to build the new ring at the M2 level.

## Related

- [`interface/aring.{h,cpp}`](interface/README.md) — public C entry points.
- [`flint.{h,cpp}`](interface/README.md) — FLINT-backed ring constructors.
- [`polynomial-rings.md`](polynomial-rings.md) — rings built on top of these.
- FLINT submodule under [`submodules/`](../../submodules/README.md).
