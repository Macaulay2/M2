# `galois.m2` — `GaloisField` type

`galois.m2` defines the M2-side **`GaloisField`** type — the
user-facing `GF(q)` construction. It is a thin wrapper around the
engine's GF implementations
([`../e/file-aring-gf-flint.md`](../e/file-aring-gf-flint.md),
[`../e/file-aring-gf-flint-big.md`](../e/file-aring-gf-flint-big.md),
[`../e/file-aring-m2-gf.md`](../e/file-aring-m2-gf.md),
[`../e/file-GF.md`](../e/file-GF.md)).

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's declared

```m2
needs "enginering.m2"
needs "quotring.m2"
needs "polyrings.m2"

GaloisField = new Type of EngineRing
GaloisField.synonym = "Galois field"

isField GaloisField := F -> true
```

Two pieces of essential setup:

1. `GaloisField` is an `EngineRing` subclass.
2. `isField` is unconditionally `true` for every Galois field.

The latter cascades through M2 — algorithms that branch on
`isField R` will take the field path for any `GaloisField`
automatically.

## User-facing construction

- **`GF q`** — construct `GF(q)` for a prime power `q = p^k`.
- **`GF(p, k)`** — same with explicit `p` and `k`.
- **`GF(R)`** — for `R = (Z/p)[t]/f(t)` a quotient by a primitive
  polynomial, return the Galois field.
- **`GF(p, k, Variable => …)`** — name the primitive generator.
- **`GF(p, k, PrimitiveElement => …)`** — supply a primitive
  element explicitly.

The dispatch in the file picks between the engine's GF back ends
based on:

- **`q`** — small (Zech tables fit) vs. large (`fq_nmod` polynomial
  representation).
- **Implementation option** — user can force a specific back end.

## Operations on `GaloisField`

- Arithmetic (`+`, `*`, etc.) — flows through the engine.
- **`order F`** — number of elements (`q`).
- **`generators F`** — the primitive element.
- **`F_i`** — the *i*-th element of `F` under some canonical
  enumeration.
- **`ambient F`** — the underlying `(Z/p)[t]/f(t)` polynomial-
  quotient form.

## Used by

- Number-theory packages working in finite fields.
- Coding theory (`CodingTheory` package).
- Some toric geometry packages over finite fields.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`../e/file-aring-gf-flint.md`](../e/file-aring-gf-flint.md), [`../e/file-aring-gf-flint-big.md`](../e/file-aring-gf-flint-big.md),
  [`../e/file-aring-m2-gf.md`](../e/file-aring-m2-gf.md),
  [`../e/file-GF.md`](../e/file-GF.md) — engine implementations.
- [`file-enginering.md`](file-enginering.md) — parent type.
- [`file-quotring.md`](file-quotring.md) — `(Z/p)[t]/f(t)` form.
