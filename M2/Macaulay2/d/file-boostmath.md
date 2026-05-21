# `boostmath.dd` — Boost.Math special-function bindings

`boostmath.dd` exposes Boost.Math's **special functions** —
`beta`, `erf`, `gamma`, the regularised variants, the inverse
functions — operating on multi-precision `RR` numbers via
Boost.Multiprecision over MPFR.

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```d
use util;
use common;

header "#include <iostream>
  #include <boost/config.hpp>
  #ifdef HAVE_BOOST_MATH_TOOLS_ATOMIC_HPP
    #include <boost/math/tools/atomic.hpp>
  #endif
  #if defined(BOOST_HAS_THREADS) && \\
     (defined(BOOST_NO_CXX11_HDR_MUTEX) || defined(BOOST_MATH_NO_ATOMIC_INT))
  #define BOOST_MATH_BERNOULLI_UNTHREADED
  #endif

  #include <boost/multiprecision/mpfr.hpp>
  #include <boost/math/special_functions/beta.hpp>
  #include <boost/math/special_functions/erf.hpp>
  #include <boost/math/special_functions/gamma.hpp>

  #define RR_TO_BOOST(x) boost::multiprecision::mpfr_float(x)

  #define BOOST_MATH_CALL(f, prec, ...)                                  \\
  try {                                                                  \\
    boost::multiprecision::mpfr_float::default_precision(                \\
	boost::multiprecision::detail::digits2_2_10(prec));              \\
    return gmp_toRR(boost::math::f(__VA_ARGS__).backend().data(), prec); \\
```

The header is mostly **macro plumbing** to handle Boost.Math's
thread-safety / Bernoulli-table requirements and to define
`BOOST_MATH_CALL` — the shared try/catch wrapper that:

1. Sets MPFR precision to the M2 caller's requested bits.
2. Calls `boost::math::f(...)`.
3. Converts the result back to M2 `RR`.
4. Catches exceptions and converts to M2 errors.

## What functions are wrapped

- **Beta** — `beta`, `betaRegularized` (incomplete beta).
- **Erf** — `erf`, `erfc`, `erfInv`, `erfcInv`.
- **Gamma** — `Gamma`, `lgamma`, `digamma`,
  `gammaRegularized`, `inverseGammaRegularized`.
- **Bernoulli numbers** — `bernoulli(n)`.

These complement MPFR's built-in special functions; the Boost set
is broader and has better numerical-stability properties for the
inverse / regularised variants.

## Why Boost.Math

MPFR provides good basics (sin, cos, log, exp, gamma) but
historically had gaps:

- No regularised incomplete beta.
- Limited inverse erf / inverse gamma.
- No inverse-regularised functions.

Boost.Math fills these in.

## Used by

- M2 user code calling `betaRegularized(a, b, x)`, `erfInv(x)`,
  `bernoulli(50)`.
- Numerical-analysis packages.
- Statistics packages.

## Related

- [`README.md`](README.md) — d/ overview.
- [`file-gmp.md`](file-gmp.md), [`file-ballarith.md`](file-ballarith.md)
  — sister MPFR / Arb wrappers.
- Boost.Math, Boost.Multiprecision — external linked libraries.
- MPFR — underlying real-number library.
