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

## M2 ring constructor → engine class

The mapping from what an M2 user types to which engine class actually handles the arithmetic:

| M2 expression | Engine class | Source file | Notes |
|---|---|---|---|
| `ZZ` (the constant) | `ARingZZGMP` via `ConcreteRing<ARingZZGMP>` | `aring-zz-gmp.{cpp,hpp}` | GMP `mpz_t`; default integer backend |
| `QQ` (the constant) | `ARingQQGMP` via `ConcreteRing<ARingQQGMP>` | `aring-qq-gmp.{cpp,hpp}` | GMP `mpq_t` |
| `ZZ/p` (small `p` ≤ 32 749) | `ARingZZpFlint` | `aring-zz-flint.{cpp,hpp}` | FLINT `nmod`; word-size; the fast default |
| `ZZ/p` (medium-`p`, BLAS-style code paths) | `ARingZZpFFPACK` | `aring-zzp-ffpack.{cpp,hpp}` | Linbox-style dispatch; selected when an FFLAS-FFPACK-backed dense matrix is involved |
| `ZZ/p` (any `p`, generic) | `ARingZZp` | `aring-zzp.{cpp,hpp}` | Generic table-based; fallback |
| `GF(p, n)` (small `p^n`) | `ARingGFFlint` | `aring-gf-flint.{cpp,hpp}` | FLINT `fq_nmod`; default for small extensions |
| `GF(p, n)` (large `p^n`) | `ARingGFFlintBig` | `aring-gf-flint-big.{cpp,hpp}` | FLINT `fq`; default for big extensions |
| `GF(q, Variable => …)` legacy | `ARingGFM2` via `GF` | `aring-m2-gf.{cpp,hpp}`, `GF.{cpp,hpp}` | M2's own table-based GF; used when the user explicitly names the generator |
| `RR` (default precision) | `ARingRR` | `aring-RR.{cpp,hpp}` | Hardware `double` (53-bit) |
| `RR_53`, `RR_n` | `ARingRRR` | `aring-RRR.{cpp,hpp}` | MPFR; `n`-bit precision |
| `RRi` | `ARingRRi` | `aring-RRi.{cpp,hpp}` | Real intervals (MPFI) |
| `CC` | `ARingCC` | `aring-CC.{cpp,hpp}` | `complex<double>` |
| `CC_n` | `ARingCCC` | `aring-CCC.{cpp,hpp}` | MPC; `n`-bit precision |
| `CCi` | `ARingCCi` | `aring-CCi.{cpp,hpp}` | Complex intervals |
| `frac R` | `FractionField` (legacy `Ring`) | `frac.{cpp,hpp}` | Fraction field; not an `aring` — see [`polynomial-rings.md`](polynomial-rings.md) |
| User-defined ring as coefficient | `ConcreteRing<…>` wrapping any of the above | `aring-glue.hpp` | The bridge between `aring` and the legacy `Ring*` hierarchy |

Construction routes through:

```
M2: R = ZZ/101
   ↓
m2/setup.m2  →  rawZZp(101)        (or similar raw* call)
   ↓
d/interface.dd  →  Ccode(RawRing, "IM2_Ring_ZZp(101)")
   ↓
e/interface/ring.h  →  IM2_Ring_ZZp(p)
   ↓
e/aring-zz-flint.cpp  →  new ARingZZpFlint(p)
   ↓ wrap
e/aring-glue.hpp  →  new ConcreteRing<ARingZZpFlint>(...)
   ↓
returned as Ring* to interpreter
```

The `ConcreteRing<ARingType>` wrapper makes an `aring` look like a legacy `Ring`, which is how the interpreter holds it. Inside the engine, performance-critical loops cast back to the concrete `ARingType` and use the templated arithmetic.

## Choosing a backend — quick reference

When implementing a new GB / matrix / resolution algorithm that templates on the coefficient ring, the **backend choice for the test inputs** dominates benchmarks. Rough rules:

| Want | Pick |
|---|---|
| Generic correctness testing | `ARingQQGMP` (slow but always correct, never overflows) |
| Fast Z/p for **small p** | `ARingZZpFlint` (FLINT `nmod`) |
| Z/p for **dense linear algebra** | `ARingZZpFFPACK` (BLAS dispatch — the matrix ops auto-select this) |
| Z/p with **arbitrary p** | `ARingZZp` (generic) |
| Galois field, `q = p^n` small | `ARingGFFlint` (FLINT `fq_nmod`) |
| Galois field, large `q` | `ARingGFFlintBig` (FLINT `fq`) |
| Floating-point with **adaptive precision** | `ARingRRR` (MPFR) or `ARingCCC` (MPC) |
| Interval arithmetic for **certified bounds** | `ARingRRi` or `ARingCCi` |
| New ring with **fast inline arithmetic** | New `aring-*.{cpp,hpp}` — see "How to add a new coefficient ring" below |

The benchmark suite in [`unit-tests/ARingTest-hpp.md`](unit-tests/file-ARingTest-hpp.md) compares backends across a standard battery of operations — run it after adding a new ring to see how your implementation stacks up.

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
