# `res-f4-computation.{cpp,hpp}` — `F4ResComputation`

`F4ResComputation` is the top-level **Schreyer-frame resolution** driver. It
subclasses [`ResolutionComputation`](../file-comp-res.md) and wraps the
F4-style inner loop that lives in
[`res-f4.{cpp,hpp}`](README.md) (and its supporting files in this
subdirectory).

Part of the [`schreyer-resolution/`](README.md) subdirectory.

[← schreyer-resolution overview](README.md) · [← engine overview](../README.md)

## State

```cpp
class F4ResComputation : public ResolutionComputation {
private:
    F4ResComputation(const PolynomialRing *origR,
                     ResPolyRing *R,
                     const Matrix *gbmatrix,
                     int max_level,
                     int numThreads,
                     bool parallelizeByDegree);

public:
    friend ResolutionComputation *createF4Res(
        const Matrix *groebnerBasisMatrix, ...);
    // ResolutionComputation interface
    FreeModule *get_free(int level) const override;
    Matrix     *get_matrix(int level) const override;
    M2_arrayint get_betti(int type) const override;
    // ...
};
```

The constructor is **private**; the factory function `createF4Res(...)`
(a friend) is the only way to construct one. This enforces the "build from
a precomputed GB" requirement: the input matrix must already be a Gröbner
basis with respect to the chosen monomial order.

## Inputs

- **`gbmatrix`** — the Gröbner basis of the input ideal (computed elsewhere).
- **`max_level`** — homological-degree limit.
- **`numThreads`** — TBB worker count.
- **`parallelizeByDegree`** — split work across threads by homological
  degree (vs. within a single matrix).

## Inner loop

The actual algorithm runs in
[`res-f4.{cpp,hpp}`](README.md). `F4ResComputation` owns a
[`SchreyerFrame`](file-res-schreyer-frame.md) instance and steps the
frame one homological level at a time:

```text
for level = 1, 2, …, max_level:
    construct frame entries for new generators (syzygies of previous level)
    for each degree of the new level:
        build and reduce a MacaulayMatrix
        extract new syzygies from the echelon form
    insert syzygies into the frame
yield Betti table on demand
```

## ResPolyRing

The constructor takes both the original `PolynomialRing` and a
[`ResPolyRing`](file-res-poly-ring.md) — the resolution-tuned ring view.
The split mirrors the GB-side split between `PolynomialRing` and `GBRing`.

## Related

- [`README.md`](README.md) — schreyer-resolution overview.
- [`file-res-schreyer-frame.md`](file-res-schreyer-frame.md) — frame data.
- [`file-res-poly-ring.md`](file-res-poly-ring.md) — resolution-tuned ring.
- [`../file-comp-res.md`](../file-comp-res.md) — `ResolutionComputation` base.
