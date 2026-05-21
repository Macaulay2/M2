# `MonoidTest.cpp` — monoid and exponent-vector tests

`MonoidTest.cpp` tests the **`Monoid`** class and the lower-level
**`ExponentVector`** primitive — the foundation of every
polynomial multiplication in M2.

Part of the [engine unit-tests suite](README.md).

[← back to unit-tests overview](README.md) · [← engine overview](../README.md)

## What's exercised

```cpp
TEST(ExponentVector, All)
{
  int n = 3;
  int a0[3] = {1, 2, 3}, b0[3] = {7, 5, 3}, c0[3], d0[3];
  exponents_t a = static_cast<exponents_t>(a0), b = static_cast<exponents_t>(b0),
            c = static_cast<exponents_t>(c0), d = static_cast<exponents_t>(d0);
  exponents::copy(n, a, c);
  ...
}
```

The `ExponentVector` tests exercise the raw exponent-array
primitives:

- **`copy(n, src, dst)`** — copy exponents.
- **`mult(n, a, b, result)`** — entry-wise add (i.e. polynomial
  multiplication on monomials).
- **`divide(n, a, b, result)`** — entry-wise subtract.
- **`divides(n, a, b)`** — predicate: does `a` divide `b`?
- **`gcd`, `lcm`** — entry-wise min / max.
- **`hash`** — needed by `MonomialHashTable`.

The `Monoid` tests add:

- Construction with various orderings (Lex, GRevLex, weight).
- Comparison.
- Heft vectors and term-degree computation.

## Why this matters

Every Gröbner basis computation, every Hilbert function
calculation, every monomial-ideal operation depends on these
primitives running correctly. A subtle off-by-one in `divides`
shows up as "F4 GB returns wrong answer" five layers up — the
test here catches it in one second.

## Why exponent vectors as raw `int*`

```cpp
typedef int* exponents_t;
```

Speed. `ExponentVector` operations are inner-loop hot in the F4
engine. A `std::vector<int>` would add an extra indirection per
access; the raw-pointer form is one load instruction. The
trade-off is callers must pre-allocate (the size is known from
the monoid).

## Used by

- Engine developers tweaking monoid / exponent code.
- The F4 engine inner loops.
- CI on every PR.

## Related

- [`README.md`](README.md) — unit-tests overview.
- [`../file-monoid.md`](../file-monoid.md) (if added) — the
  `Monoid` class.
- [`../monoids-and-monomials.md`](../monoids-and-monomials.md) —
  area.
- [`../f4/`](../f4/README.md) — F4 engine that builds on
  exponent-vector primitives.
