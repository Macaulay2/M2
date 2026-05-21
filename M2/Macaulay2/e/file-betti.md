# `betti.{cpp,hpp}` — `BettiDisplay`

`BettiDisplay` is the engine-side data structure that holds and renders a
**Betti table**: the array of ranks `β_{i,j}` of the *i*-th free module in
homological degree *j* of a free resolution.

Part of the [Resolutions](resolutions.md) area.

[← per-area: resolutions](resolutions.md) · [← engine overview](README.md)

## What is a Betti table

For a free resolution

```
⋯ → F_2 → F_1 → F_0 → M → 0,
```

with each `F_i` a graded free module, the **graded Betti number**
`β_{i,j} = rank((F_i)_j)`. Displayed:

```
       0  1  2  3
total: 1  3  3  1
    0: 1  .  .  .
    1: .  3  3  .
    2: .  .  .  1
```

Rows are degrees (`j - i`), columns are homological degrees (`i`).

## State

```cpp
class BettiDisplay {
public:
    BettiDisplay();
    BettiDisplay(int lodegree, int hidegree, int hilen);

    int& entry(int deg, int lev);   // mutable access
    // copy, swap, output ...
};
```

The class is intentionally just a 2-D `int` array with bounds — it does no
mathematics itself. All the heavy lifting (figuring out what the entries
should be) happens in the various `ResolutionComputation` subclasses.

## Construction

A `BettiDisplay` is built once a resolution is complete:

1. Walk the resolution `F_0, F_1, …`.
2. For each `F_i`, group its generators by degree.
3. Increment the corresponding entry.

The result is what the M2 `betti` function displays.

## Memtailor

The header includes `memtailor.h` (the mathic/mathicgb allocator) for fast
allocation of small Betti-table fragments. Most Betti tables are small, but
ones from large monomial-ideal resolutions can have hundreds of rows /
columns.

## Output

The `BettiDisplay` produces text in three formats:

- Plain: the standard table shown above.
- HTML: for `installPackage`-generated documentation.
- LaTeX: for `latex` output.

Format selection happens at print time via the engine's `buffer` machinery
([`utilities.md`](utilities.md)).

## Related

- [`resolutions.md`](resolutions.md) — area overview.
- [`file-comp-res.md`](file-comp-res.md) — every `ResolutionComputation`
  emits a `BettiDisplay`.
- [`m2/betti.m2`](../m2/README.md) — M2-side `betti`, `betti2` functions.
- [`interface/groebner.{h,cpp}`](interface/README.md) — public API.
