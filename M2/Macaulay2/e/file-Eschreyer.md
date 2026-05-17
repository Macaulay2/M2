# `Eschreyer.{cpp,hpp}` — Schreyer-style kernel computation

`Eschreyer.cpp` implements an **older Schreyer-style resolution** —
specifically, a `GBKernelComputation` that computes the kernel of a matrix
between free modules using a Schreyer order on the source. It is the direct
predecessor of the newer F4-based implementation in
[`schreyer-resolution/`](schreyer-resolution/README.md).

Part of the [Resolutions](resolutions.md) area.

[← per-area: resolutions](resolutions.md) · [← engine overview](README.md)

## What it does

Given a matrix `f : F → G` whose columns are a Gröbner basis of `im(f)`,
`GBKernelComputation` produces:

- The **kernel** `K ⊂ F` of `f`,
- in such a form that its generators are *themselves* a Gröbner basis under
  the Schreyer order on `F` induced by `(f, lt(f_1), …, lt(f_n))`.

The output's Schreyer order is what makes this useful for iterated
resolutions: feed the kernel back as the input to the next step, and you
have computed two homological degrees in linear time.

## Supporting types

### `GBMatrix`

A lightweight matrix type whose columns are `gbvector*` rather than
opaque `vec` values:

```cpp
struct GBMatrix : public our_new_delete {
    const FreeModule *F;
    gc_vector<gbvector*> elems;

    GBMatrix(const Matrix *m);
    GBMatrix(const FreeModule *F);
    void append(gbvector *f);   // takes ownership
    Matrix *to_matrix();
};
```

This is the format consumed by `GBKernelComputation`; it sidesteps a
`Matrix → gbvector*` conversion at every loop step.

### `GBKernelComputation`

Subclasses [`Computation`](file-computation-framework.md). Its
`start_computation()` implements the Schreyer-style reduction loop.

## Position relative to the new code

| Old (`Eschreyer.cpp`) | New (`schreyer-resolution/`) |
|---|---|
| Inner loop is single-row reduction | Inner loop is matrix reduction (F4) |
| One generator at a time | All generators in a degree at once |
| Used as the body of `res-a*.cpp` resolutions | Self-contained Computation |

Both produce mathematically equivalent output. The new implementation is
faster on most inputs, but the old one is retained for regression testing and
for the (rare) case where it wins on a small example.

## Related

- [`resolutions.md`](resolutions.md) — area overview.
- [`schreyer-resolution/`](schreyer-resolution/README.md) — modern
  implementation.
- [`file-comp-res.md`](file-comp-res.md) — `ResolutionComputation` framework.
- [`file-gbring.md`](file-gbring.md) — `gbvector` type used here.
- [`schorder.{cpp,hpp}`](free-modules.md) — Schreyer-order storage on free
  modules.
