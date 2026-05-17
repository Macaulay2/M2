# `schubert.m2` — Schubert calculus helpers

`schubert.m2` provides M2's **Schubert-calculus** entry points —
operations on Grassmannians, Schubert classes, and related
intersection-theoretic constructions.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```m2
-- Bernd Sturmfels and Josephine Yu

needs "matrix1.m2"

--signOfPermu := P -> (
--     sign := 1;
--     scan(#P-1, a->
--          scan(a+1..#P-1, b ->
--               if P#a > P#b then
--               sign = -1 * sign
```

Authored by Bernd Sturmfels and Josephine Yu. The commented-out
`signOfPermu` is a worked-out helper kept for reference (the
production sign-of-permutation function lives elsewhere).

## What's implemented

The file defines functions for:

- **Grassmannian** — construct the Plücker-coordinate ideal of
  `Gr(k, n)`.
- **Schubert** — construct the Schubert variety in `Gr(k, n)`
  corresponding to a given partition / index set.
- **Pieri rules** — `c_i · σ_λ` decompositions.
- **`SchurRing` integration** — Schubert classes can be expressed in
  terms of Schur polynomials via the [`file-schur.md`](file-schur.md)
  / [`file-schur2.md`](file-schur2.md) machinery.

## Used by

- M2 users doing Schubert calculus.
- Algebraic-geometry courses (Grassmannian computations).
- The `Book3264Examples` package.
- [`tests/ComputationsBook/`](../tests/ComputationsBook/README.md)
  Schubert chapter.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-schur.md`](../e/file-schur.md), [`file-schur2.md`](../e/file-schur2.md)
  — engine-side Schur rings.
- `Schubert2` user package — more powerful Schubert calculus engine.
- [`file-genmat.md`](file-genmat.md) — generic matrices used to
  define Grassmannians.
- [`file-monomcurve.md`](file-monomcurve.md) — sister classical-
  ideal helper.
