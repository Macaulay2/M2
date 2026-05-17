# `monomcurve.m2` — `monomialCurveIdeal`

`monomcurve.m2` defines **`monomialCurveIdeal(S, a)`** — given a
polynomial ring `S` and a list of nonnegative integers `a`, return
the toric ideal of the monomial curve `(t^{a_1}, t^{a_2}, …, t^{a_n})`
in `S`.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's declared

```m2
-- Copyright 1996 Michael E. Stillman
-- Based on the Macaulay script written by David Eisenbud

needs "newring.m2"

monomialCurveIdeal = (S, a) -> (
    -- check that S is a polynomial ring over a field
    n := # a;
    topa := max a;
    if not all(a, i -> instance(i, ZZ) and i >= 1)
    ...
)
```

The function:

1. Validates that `S` is a polynomial ring over a field with at
   least as many variables as the length of `a`.
2. Validates that each `a_i` is a positive integer.
3. Constructs an auxiliary variable `t` and the polynomial ring `S[t]`.
4. Computes the kernel of the map `S → S[t]` sending the *i*-th
   variable of `S` to `t^{a_i}`.
5. Returns that kernel as an ideal of `S`.

## What a monomial curve ideal looks like

For `a = (1, 2, 3)` in `S = QQ[x, y, z]`:

```m2
monomialCurveIdeal(S, {1, 2, 3})
-- ideal(y^2 - x*z, x^3 - y*z, ...)
```

These are the relations among `t`, `t^2`, `t^3` after eliminating `t`.

## Why a separate file

`monomialCurveIdeal` is one of M2's classic examples in tutorials and
the book. It is small and pedagogically useful, so it has its own
file even though the implementation is brief.

## Used by

- M2 tutorial scripts.
- Toric geometry packages.
- Algebraic geometry courses using M2.
- `ComputationsBook` test suite (verifies the implementation).

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-newring.md`](file-newring.md) — `flattenRing` etc. used
  internally.
- [`file-gb.md`](file-gb.md) — kernel of a ring map uses GB.
- [`file-ringmap.md`](file-ringmap.md) — the underlying ring-map
  framework.
- `Toric` and `ToricVarieties` packages (in `packages/`) — full
  toric machinery built on top of this.
