# `gbring.{cpp,hpp}` — `GBRing`, the Gröbner-tuned ring view

`GBRing` is a polynomial-ring **view** specialised for Gröbner basis arithmetic.
It carries the same data as a [`PolynomialRing`](file-polyring.md) — coefficient
ring, monoid, ring flags — but lays out values in a form designed for fast
head/tail decomposition, S-pair reduction, and (optionally) implicit Schreyer
ordering.

Part of the [Gröbner bases](groebner-bases.md) area.

[← per-area: groebner-bases](groebner-bases.md) · [← engine overview](README.md)

## The `gbvector` value type

```cpp
struct gbvector {
    gbvector *next;     // intrusive linked list (highest term first)
    ring_elem coeff;    // coefficient in the base ring
    int comp;           // free-module component index (0 if scalar ring)
    int monom[1];       // flexible array of encoded monomial ints
};
```

`gbvector` is a singly-linked list of `(coeff, component, monomial)` triples
sorted highest-term-first by the monomial order. The flexible `monom[1]` is
the C idiom for trailing variable-length data; allocation actually uses
`MonoidLayout::monomial_size()` more ints.

## Capabilities

The header comments enumerate `GBRing`'s design goals:

- Carry **both** the ambient free module `F` and a syzygy module `Fsyz` so a
  reduction step can produce a pair `(reduced gbvector, syzygy gbvector)` in
  one pass.
- Hide Schreyer order details so polynomial code doesn't need to know
  whether a Schreyer order is installed.
- Handle the **ZZ-coefficient** case correctly (signs and content matter).
- Provide a `gbvectorHeap` data structure to do "polynomial addition with
  large numbers of summands" in `O(n log k)` time.

## Implementations

The header lists the matrix of `GBRing` subclasses:

| Axis | Choices |
|---|---|
| Schreyer encoding | encoded · explicit order · none |
| Coefficient kind | field (`KK`) · ZZ |
| Ring flavour | polynomial · skew · Weyl · solvable |
| Quotient | base · quotient by an ideal |

Each cell is realised by one `GBRing` subclass (commonly `GBRingSkew`,
`GBRingWeyl`, …). Construction is parameterised on a `PolynomialRing*`.

## Reduction

The single most important method is the **vector reduction**:

```cpp
void reduce_vec(gbvector *&v, /* sources of reducers */ ...);
```

This is the inner loop of every Buchberger-style GB algorithm. Its
specialisation per `GBRing` flavour is what makes ring-specific GB code small.

## How `PolynomialRing` and `GBRing` differ

| Aspect | `PolynomialRing` | `GBRing` |
|---|---|---|
| Value type | `ring_elem` (opaque) | `gbvector*` |
| Storage | Internal data structure | Sorted linked list |
| Coeff arithmetic | Through `Ring*` or `aring` | Through `CoefficientRingZZp` etc. |
| Schreyer order | Lives on `FreeModule` | Optionally encoded in monomial |
| Primary use | M2-level computation | GB inner loop |

A `GBRing` is constructed once per GB computation from a `PolynomialRing` and
torn down at the end.

## Files this depends on

- [`monoid.hpp`](file-monoid.md) — monomial encoding.
- `ringelem.hpp` — `ring_elem` type definition.
- [`skew.hpp`](skewpoly.cpp) — skew-multiplication tables.
- [`gbvectorHeap`](#) — implemented in `gbring.cpp`.

## Related

- [`file-polyring.md`](file-polyring.md) — the host `PolynomialRing`.
- [`groebner-bases.md`](groebner-bases.md) — area overview.
- [`file-comp-gb.md`](file-comp-gb.md) — the Computation using `GBRing` internally.
- [`gbweight.{cpp,hpp}`](groebner-bases.md), [`spair.{cpp,hpp}`](groebner-bases.md) — adjacent files.
