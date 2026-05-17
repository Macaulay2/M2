# `NCGroebner.{cpp,hpp}` — `NCGroebner`

`NCGroebner` is the engine's **non-commutative Buchberger-style Gröbner
basis** driver. Given a list of input polynomials in a
[`FreeAlgebra`](file-FreeAlgebra.md), it produces a (possibly partial)
two-sided Gröbner basis.

Part of the [`NCAlgebras/`](README.md) subdirectory.

[← NCAlgebras overview](README.md) · [← engine overview](../README.md)

## State

```cpp
class NCGroebner : public our_new_delete {
private:
    const FreeAlgebra& mFreeAlgebra;
    WordTable mWordTable;
    // SuffixTree mWordTable;   (experimental alternative)
    OverlapTable mOverlapTable;
    // ...
};
```

The non-commutative analogue of S-pairs is **overlaps** — pairs of words
where the suffix of one matches the prefix of the other. The
`OverlapTable` ([`OverlapTable.cpp`](README.md)) maintains a queue of these.

`WordTable` ([`WordTable.cpp`](README.md)) is the leading-word index — the
non-commutative analogue of [`MonomialTable`](../file-montable.md).
`SuffixTree` is an experimental alternative ([`SuffixTree.cpp`](README.md))
that the code is staged to swap in.

## High-level loop

```text
while overlaps in queue:
    pick an overlap o = (g1, g2, prefix, suffix)
    compute the corresponding S-polynomial
    reduce modulo current basis (NCReduction.cpp)
    if nonzero:
        add to basis
        compute new overlaps with every existing element
        flush old overlaps subsumed by the new element
```

This is the same shape as [`gbA`](../file-gb-default.md) but with
"S-pair" → "overlap" and "monomial divisibility" → "word containment".

## Reduction

The reduction step uses
[`NCReduction.cpp`](README.md)'s `PolynomialHeap` — a min-heap of monomial
operations that lets the algorithm combine many tail polynomials at once,
analogous to `gbvectorHeap` in [`gbring`](../file-gbring.md).

## `tryOutMathicCode`

The header declares an `extern void tryOutMathicCode()` — a hook for
benchmarking experimental integration with the [`mathic`](../../submodules/README.md)
data-structures library. Not part of the production API.

## Termination

Unlike the commutative case, non-commutative Gröbner bases are **not
guaranteed to terminate** — there exist finitely-generated two-sided
ideals whose Gröbner basis is infinite. `NCGroebner` respects user-supplied
degree limits and stops; partial bases are still useful for normal-form
calculations up to that degree.

## Related

- [`README.md`](README.md) — NCAlgebras overview.
- [`file-NCF4.md`](file-NCF4.md) — F4-style alternative for NC GB.
- [`file-FreeAlgebra.md`](file-FreeAlgebra.md), [`file-FreeMonoid.md`](file-FreeMonoid.md) — ring/monoid.
- [`../file-gb-default.md`](../file-gb-default.md) — commutative analogue.
- [`../file-comp-gb.md`](../file-comp-gb.md) — `GBComputation` base
  (commutative side).
