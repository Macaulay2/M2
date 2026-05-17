# `comb.{cpp,hpp}` — combinatorial helpers (`Subsets`)

`comb.cpp` provides combinatorial helpers used internally by the engine —
most notably the `Subsets` class that enumerates and encodes `p`-element
subsets of `{0, 1, …, n-1}`.

Part of the [Other computations](computations.md) area.

[← per-area: computations](computations.md) · [← engine overview](README.md)

## `Subsets`

```cpp
class Subsets {
    // Manipulate p-subsets of 0..n-1, for fixed n, possibly several p.
};
```

The class encodes a `p`-subset as a single integer using the **combinatorial
number system**: each subset has a unique index based on its sorted
elements.

The header comment gives an example for 3-subsets of a 5-element set:

```
0 = {0,1,2}    5 = {0,2,4}
1 = {0,1,3}    6 = {0,3,4}
2 = {0,2,3}    7 = {1,2,4}
3 = {1,2,3}    8 = {1,3,4}
4 = {0,1,4}    9 = {2,3,4}
```

The encoding is intentionally **stable under extension**: if you extend the
ambient set from 5 to 6 elements, the existing 10 subsets keep their indices
and the new ones are appended.

## API surface

- `Subsets::encode(p, exp)` — encode a sorted `p`-subset `exp[]` to an
  integer.
- `Subsets::decode(p, n, idx, exp)` — recover the subset.
- `Subsets::next(p, n, exp)` — step to the next subset in canonical order.

## Use sites

- **Exterior-algebra arithmetic** — index basis elements
  `e_{i_1} ∧ ⋯ ∧ e_{i_p}` by their subset code.
- **Schubert calculus** ([`m2/schubert.m2`](../m2/README.md)) — enumerate
  index subsets for partition operations.
- **Minor computation** — index `p × p` minors of a matrix.

## Related

- [`computations.md`](computations.md) — area overview.
- [`file-skewpoly.md`](file-skewpoly.md) — exterior algebra arithmetic.
- [`m2/schubert.m2`](../m2/README.md) — Schubert calculus uses these encodings.
