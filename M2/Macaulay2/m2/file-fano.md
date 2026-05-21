# `fano.m2` — Fano variety construction

`fano.m2` defines **`Fano(...)`** — the Fano variety of a projective
variety, parametrising the *r*-planes lying in it. The
implementation is meant to mirror the M2 tutorial `Fano.m2` exactly.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```m2
-- Copyright 1997 by Michael Stillman and David Eisenbud
--
-- The code here should stay the SAME as the code
-- in the tutorial 'Fano.m2', if that is possible.
--

needs "matrix1.m2"
needs "genmat.m2"

Fano = method()
```

The unusual "stay the SAME as the tutorial" note is a constraint:
the function's code in this file is supposed to read identically to
the version shown in M2's user tutorial. This way, users learning
from the tutorial can copy-paste and the result matches what they'd
get by calling `Fano` directly.

## What `Fano` does

Given a projective variety `X ⊂ P^n` and a positive integer `r`,
`Fano(r, X)` computes the ideal of the variety of *r*-planes
contained in `X`.

The classical example: the Fano variety of lines on a cubic
threefold. Salmon's classical theorem says there are 27 lines on a
smooth cubic surface — `Fano(1, cubic_surface_ideal)` gives a 27-
dimensional 0-scheme.

## Implementation sketch

Internally:

1. Set up a [`genericMatrix`](file-genmat.md) parameterising
   `r`-planes.
2. Substitute into the defining equations of `X` to get conditions
   on the parameter ring.
3. Eliminate the projective-plane parameters to leave the conditions
   on the parameter ring's Grassmannian coordinates.
4. Return that ideal.

## Used by

- M2 tutorial users following the Fano variety example.
- Algebraic-geometry courses.
- `Book3264Examples` and similar pedagogical packages.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-genmat.md`](file-genmat.md) — `genericMatrix` used internally.
- [`file-schubert.md`](file-schubert.md) — sister classical-
  geometry helper.
- [`tests/ComputationsBook/`](../tests/ComputationsBook/README.md)
  geometry chapter.
