# `cra.hpp`, `cra.cpp` — Chinese Remainder + rational reconstruction

`cra.hpp` and `cra.cpp` implement **`ChineseRemainder`** — the
engine's CRT and rational-reconstruction primitives. Used to lift
modular computations back to `ZZ` / `QQ`.

Part of the [engine](README.md) — utilities.

[← engine overview](README.md) · [utilities](utilities.md)

## The class

```cpp
class ChineseRemainder
{
 public:
  static void CRA0(mpz_srcptr a,
                   mpz_srcptr b,
                   mpz_srcptr um,
                   mpz_srcptr vn,
                   ... );
};
```

`CRA0` is the core CRT operation:

> Given `a mod m`, `b mod n`, `gcd(m, n) = 1`, and precomputed
> Bezout coefficients `um`, `vn` (so `um + vn = 1` in some sense),
> compute the unique value mod `mn` reducing to `a mod m` and
> `b mod n`.

```cpp
void ChineseRemainder::CRA0(mpz_srcptr a, mpz_srcptr b,
                            mpz_srcptr um, mpz_srcptr vn,
                            mpz_srcptr mn, mpz_t result)
{
  mpz_t mn_half;
  mpz_init(mn_half);
  mpz_mul(result, um, b);
  mpz_addmul(result, vn, a);
  ...
}
```

The formula: `result = vn*a + um*b mod mn`, balanced to live in
`[-mn/2, mn/2]`.

## Why "balanced residue"

The header comment:

```cpp
// !!!! we need the balanced residue class in chinese remainder !!!
```

flags it as essential. Balanced residue (range `[-mn/2, mn/2]`)
is needed so that **small `ZZ` values stay small** after CRT —
otherwise CRT-then-mod would always give nonneg values, defeating
the "lift back to ZZ" use case.

## What CRT is used for in M2

Modular methods for hard computations:

- **Gröbner bases over `QQ`** — too slow directly; instead
  compute mod many primes, CRT-lift, rational-reconstruct.
- **Determinants over `ZZ`** — same trick.
- **GCDs of polynomials** — same.

Each modular run is fast; the CRT machinery combines them. With
enough primes, rational reconstruction recovers the rational
answer with high probability.

## Rational reconstruction

Beyond CRT, `cra.cpp` provides **rational reconstruction**: given
`x mod m`, find a fraction `p/q` with small `p, q` such that
`p ≡ x*q (mod m)`. This is the "lift back to QQ" step.

The math is the extended Euclidean algorithm with a bound on `q`
— if `m` is large enough relative to `p*q`, the answer is unique.

## Used by

- M2's `gb` over `QQ` (modular path).
- M2's polynomial GCD over `QQ`.
- Numerical-algebraic-geometry homotopy code that uses modular
  preprocessing.
- [`interface/file-cra-interface.md`](interface/file-cra-interface.md)
  — C API.

## Related

- [`README.md`](README.md) — engine overview.
- [`utilities.md`](utilities.md) — area.
- [`interface/file-cra-interface.md`](interface/file-cra-interface.md)
  — C entry points.
- GMP `mpz_*` — underlying big-integer operations.
