# `overflow.{cpp,hpp}` — overflow-checked arithmetic

`overflow.cpp` provides **checked integer arithmetic** for the small-integer
operations the engine performs on monomial exponents and degrees. A silent
overflow in a monomial exponent can corrupt a Gröbner basis without any
visible error — these helpers ensure that doesn't happen.

Part of the [Utilities](utilities.md) area.

[← per-area: utilities](utilities.md) · [← engine overview](README.md)

## Why the engine needs this

Monomial exponents are stored in **32-bit signed integers** (`int32_t`)
throughout the engine. A multiplication of two monomials adds their exponent
vectors — if `e_i + e'_i > INT32_MAX`, the wrap-around result is not just
wrong, it can be **silently lower** than either input, which violates the
monomial-order invariant.

The result: GB algorithms can loop indefinitely, or produce a basis that
"reduces" to a nonzero answer.

`overflow.cpp` adds a check after each potentially-dangerous operation and
raises an engine error if the result would wrap.

## Compiler builtin path

The header detects `__has_builtin(__builtin_add_overflow)` and similar to
use the compiler's native checked-arithmetic intrinsics where available.
Fallback paths exist for older compilers:

```c
#if defined(__has_builtin)
  // use __builtin_add_overflow etc.
#else
  // manual range-check path
#endif
```

The fast path is just an add plus one branch. The slow path is still cheap
— a couple of comparisons — but the compiler can vectorise the fast path
inside a tight loop.

## Files that depend on `overflow.hpp`

The list (lifted from the `README.md` historical-notes section, which
preserved it from the original engine codebase):

`CC, CCC, Eschreyer, GF, QQ, RR, RRR, ZZ, ZZp, comp-gb-declared, comp-gb,
comp-res, debug, frac, freemod, gb-default, gb-homog2, gb-sugarless,
gb-toric, gbring, gbweight, imonorder, matrix-kbasis, matrix, monoid,
monorder, montable, ntuple, overflow, polyring, qring, reducedgb-ZZ,
reducedgb-field-local, reducedgb-field, reducedgb, res-a2-gb, res-a2,
schorder, skewpoly, solvable, spair, varpower, weylalg, x-gb, x-mat,
x-relem`.

Essentially: every file that touches monomial arithmetic or degree
computation.

## Error handling

When overflow is detected, the helpers in `overflow.cpp` throw a C++
exception that the engine's top-level error machinery
([`error.{cpp,hpp}`](utilities.md)) catches and surfaces to the user as
an `error: monomial overflow` message.

## Related

- [`utilities.md`](utilities.md) — area overview.
- [`file-monoid.md`](file-monoid.md), [`file-imonorder.md`](file-imonorder.md)
  — primary consumers via monomial arithmetic.
- `error.{cpp,hpp}` — top-level error reporting.
- `exceptions.hpp` — exception types used here.
