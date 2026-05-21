# `hilb-fcn.{cpp,hpp}` — `HilbertController` (F4 Hilbert-driven early exit)

`hilb-fcn.cpp` implements **`HilbertController`** — the helper that F4
uses to terminate a Gröbner basis computation early when the user
supplies the expected Hilbert series of the input ideal.

Part of the [`f4/`](README.md) subdirectory.

[← f4 overview](README.md) · [← engine overview](../README.md)

## Class shape

```cpp
class HilbertController : public our_new_delete {
public:
    HilbertController(const FreeModule *F0, const RingElement *hf);
    ~HilbertController();

    int nRemainingExpected() {
        return hilb_n_in_degree;
    }
    // This number is decremented each time addMonomial is called.

    bool setDegree(int this_degree);
    // recomputes Hilbert function, if necessary.
    // ...
};
```

Two methods drive the interaction with F4:

- **`setDegree(d)`** — F4 calls this when it starts processing degree
  `d` S-pairs. The controller reads off the expected Hilbert-series
  coefficient at degree `d` and stores it in `hilb_n_in_degree`.
- **`addMonomial(...)`** — F4 calls this every time a new leading
  monomial of degree `d` lands in the basis. The controller
  decrements `hilb_n_in_degree`.
- **`nRemainingExpected()`** — when this hits zero, F4 knows it has
  found all degree-`d` basis elements and can skip remaining
  S-pairs of that degree.

## Why early exit matters

A Gröbner basis computation may produce thousands of S-pairs at a
single degree, of which only a handful reduce to non-zero residues.
The standard algorithm processes them all to be sure. If the user
*already knows* the Hilbert function (typically from a smaller
related computation), F4 can stop processing degree `d` as soon as
the expected number of new generators has been found, skipping all
remaining S-pairs of that degree.

The savings compound across degrees: skipping `k` S-pairs at degree
`d` means avoiding the matrix builds those pairs would have triggered
at degree `d`, plus all their downstream propagation.

## How the user supplies the Hilbert function

At the M2 level:

```m2
gb(I, Hilbert => h)
```

where `h` is a `RingElement` in `ZZ[t]` representing the numerator of
the Hilbert series of `R/I`. The dispatcher in
[`file-comp-gb.md`](../file-comp-gb.md) wires this to
`F4Computation`, which constructs a `HilbertController` and hands it
to F4's inner loop.

## Related

- [`README.md`](README.md) — f4 overview.
- [`file-f4-computation.md`](file-f4-computation.md) — wires up the
  controller.
- [`file-f4.md`](file-f4.md) — calls `setDegree` / `addMonomial`.
- [`../file-hilb.md`](../file-hilb.md) — top-level Hilbert function
  computation (independent of GB).
- `Matrix`, `MatrixConstructor` — `HilbertController` builds matrices
  when reconstructing.
