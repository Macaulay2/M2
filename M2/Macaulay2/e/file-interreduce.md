# `interreduce.{cpp,hpp}` — `Interreducer` (inter-reduction helper)

`interreduce.cpp` defines **`Interreducer`** — a helper class for
performing **inter-reduction** of a list of polynomials. Given a
collection of `gbvector*` values, `Interreducer` reduces each one
modulo the others until no further simplification is possible,
producing a Gröbner-basis-like canonical form.

Part of the [Gröbner bases](groebner-bases.md) area.

[← per-area: groebner-bases](groebner-bases.md) · [← engine overview](README.md)

## Class shape

```cpp
#include "gbring.hpp"

class Interreducer {
    GBRing             *R;
    FreeModule         *F;
    VECTOR(gbvector *)  G;

public:
    Interreducer(GBRing *R, FreeModule *F, VECTOR(gbvector *) &elems0);

    void showElem(int i, int nterms);
    void show(int nterms);

    int cancelLT(gbvector *&f, const gbvector *g);
    // ...
};
```

Three pieces of state:

- **`R`** — the [`GBRing`](file-gbring.md) supplying the value type.
- **`F`** — the free module the elements live in.
- **`G`** — the list of elements being inter-reduced.

## What inter-reduction is

For a list `{f_1, …, f_n}`:

1. Pick any `f_i`.
2. For each other `f_j`, if `lt(f_j) | lt(f_i)`, subtract a multiple
   of `f_j` from `f_i` to cancel `lt(f_i)`.
3. Repeat until no `lt(f_i)` is divisible by any other `lt(f_j)`.

The result is **head-reduced** but not necessarily fully reduced
(tail terms can still be cancellable by some other `f_j`).

## `cancelLT`

```cpp
int cancelLT(gbvector *&f, const gbvector *g);
```

The single elementary reduction operation: cancel the leading term of
`f` using `g`. Returns the new leading-term degree (or a sentinel if
`f` became zero).

## When this is used

Inter-reduction is a building block for many GB-related tasks:

- After a GB is computed, `gbA` ([`file-gb-default.md`](file-gb-default.md))
  inter-reduces the result before handing it back.
- The reduced-GB family ([`file-reducedgb.md`](file-reducedgb.md))
  uses inter-reduction as part of its canonicalisation pass.
- M2-level operations like `forceGB` use this to bring a user-supplied
  basis into a sane state.

## Display helpers

`showElem(i, nterms)` and `show(nterms)` print the *i*-th element (or
the whole list) up to a chosen number of terms each — useful for
debugging tight loops.

## Status

The file is a small implementation (~one screen of `Interreducer`'s
ctor in the `.cpp`); most of the work is in the reduction loops that
chain `cancelLT` calls. It is a stable, in-production utility used
across the engine.

## Related

- [`groebner-bases.md`](groebner-bases.md) — area overview.
- [`file-gbring.md`](file-gbring.md) — value-type host.
- [`file-reducedgb.md`](file-reducedgb.md) — primary consumer.
- [`file-gb-default.md`](file-gb-default.md) — uses inter-reduction at
  finalisation.
