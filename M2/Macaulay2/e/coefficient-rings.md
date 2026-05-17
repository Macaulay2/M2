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
| `aring-glue.hpp` | Templates that build a legacy `Ring` wrapper around an aring. **Deep dive:** [`file-aring-glue.md`](file-aring-glue.md) |
| `aring-translate.hpp` | Compile-time translation between ring types (e.g. promoting `int` to `mpz_t`) |
| `aring-wrap.{cpp,hpp}` | Concrete wrapper instantiations |
| `coeffrings.{cpp,hpp}` | Registry: maps a coefficient-ring tag to its concrete type. **Deep dive:** [`file-coeffrings.md`](file-coeffrings.md) |

### Integers (ZZ)

| File pair | Backed by |
|---|---|
| `ZZ.{cpp,hpp}` | Legacy ZZ (GMP) |
| `aring-zz-gmp.{cpp,hpp}` | aring ZZ using `mpz_t`. **Deep dive:** [`file-aring-zz-gmp.md`](file-aring-zz-gmp.md) |
| `aring-zz-flint.{cpp,hpp}` | aring ZZ using FLINT's `fmpz_t`. **Deep dive:** [`file-aring-zz-flint.md`](file-aring-zz-flint.md) |

### Rationals (QQ)

| File pair | Backed by |
|---|---|
| `aring-qq.hpp` | aring QQ — header-only abstract base |
| `aring-qq-gmp.{cpp,hpp}` | QQ via GMP's `mpq_t` |
| `aring-qq-flint.{cpp,hpp}` | QQ via FLINT's `fmpq_t`. **Deep dive:** [`file-aring-qq-flint.md`](file-aring-qq-flint.md) |

### Finite prime fields (Z/p)

| File pair | Notes |
|---|---|
| `ZZp.{cpp,hpp}` | Legacy Z/p |
| `aring-zzp.{cpp,hpp}` | Generic aring Z/p (table-based for small p). **Deep dive:** [`file-aring-zzp.md`](file-aring-zzp.md) |
| `aring-zzp-flint.{cpp,hpp}` | Z/p via FLINT (`nmod`). **Deep dive:** [`file-aring-zzp-flint.md`](file-aring-zzp-flint.md) |
| `aring-zzp-ffpack.{cpp,hpp}` | Z/p via FFLAS-FFPACK (BLAS-style dispatch). **Deep dive:** [`file-aring-zzp-ffpack.md`](file-aring-zzp-ffpack.md) |

### Galois fields (GF)

| File pair | Notes |
|---|---|
| `GF.{cpp,hpp}` | Legacy GF (table-based, characteristic small) |
| `aring-m2-gf.{cpp,hpp}` | Native M2 implementation. **Deep dive:** [`file-aring-m2-gf.md`](file-aring-m2-gf.md) |
| `aring-gf-flint.{cpp,hpp}` | GF via FLINT for small extension. **Deep dive:** [`file-aring-gf-flint.md`](file-aring-gf-flint.md) |
| `aring-gf-flint-big.{cpp,hpp}` | GF via FLINT for large extension. **Deep dive:** [`file-aring-gf-flint-big.md`](file-aring-gf-flint-big.md) |

### Real and complex numbers

| File pair | Precision model |
|---|---|
| `aring-RR.{cpp,hpp}` | Hardware `double`. **Deep dive:** [`file-aring-RR.md`](file-aring-RR.md) |
| `aring-RRR.{cpp,hpp}` | Arbitrary-precision via MPFR. **Deep dive:** [`file-aring-RRR.md`](file-aring-RRR.md) |
| `aring-RRi.{cpp,hpp}` | Real intervals (Arb / MPFI). **Deep dive:** [`file-aring-RRi.md`](file-aring-RRi.md) |
| `aring-CC.{cpp,hpp}` | Complex (pair of `double`). **Deep dive:** [`file-aring-CC.md`](file-aring-CC.md) |
| `aring-CCC.{cpp,hpp}` | Complex with MPFR precision. **Deep dive:** [`file-aring-CCC.md`](file-aring-CCC.md) |
| `aring-CCi.{cpp,hpp}` | Complex intervals. **Deep dive:** [`file-aring-CCi.md`](file-aring-CCi.md) |

### Iterated extensions

| File pair | Purpose |
|---|---|
| `aring-tower.{cpp,hpp}` | Tower of finite extension rings (used to build large GF efficiently). **Deep dive:** [`file-aring-tower.md`](file-aring-tower.md) |

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
