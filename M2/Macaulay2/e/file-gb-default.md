# `gb-default.{cpp,hpp}` — the default Buchberger-style GB engine (`gbA`)

`gbA` is the engine's **default Gröbner basis algorithm** — a Buchberger-style
algorithm with carefully chosen S-pair selection, monomial-table reductions,
and a tuned `gbring` value representation. It is the algorithm M2 uses when
no `Strategy =>` option overrides the default.

Subclasses [`GBComputation`](file-comp-gb.md). Part of the
[Gröbner bases](groebner-bases.md) area.

[← per-area: groebner-bases](groebner-bases.md) · [← engine overview](README.md)

## Status flags

Three element-status flags live near the top of `gb-default.hpp`:

```cpp
const int ELEM_IN_RING = 1;
const int ELEM_MINGEN  = 2;
const int ELEM_MINGB   = 4;
```

Every basis element carries a bitmask of these:

- **`ELEM_IN_RING`** — the element actually lies in the ambient ring (not in
  a "trash" pile used during reduction).
- **`ELEM_MINGEN`** — among the minimal generators of the input.
- **`ELEM_MINGB`** — part of the minimal GB.

The interpreter can read these out at the end of a computation to extract
either the original minimal generators or the minimal GB.

## High-level loop

```text
while there are S-pairs:
    pick an S-pair (p, q) by the selection strategy
    compute the S-polynomial s = lcm(lt(p), lt(q)) (p / lt(p)) − …
    reduce s modulo the current basis
    if the result is nonzero:
        add it to the basis
        generate new S-pairs against it
        flush any S-pairs no longer useful
```

Each line above has a dedicated method in `gbA`.

## Key supporting types

| Type | File | Purpose |
|---|---|---|
| `GBRing` | [`file-gbring.md`](file-gbring.md) | The ring view storing polynomials as `gbvector` |
| `MonomialTable` | [`file-montable.md`](file-montable.md) | Index of leading monomials |
| `MonomialTableZZ` | [`montableZZ.cpp`](monoids-and-monomials.md) | ZZ-coefficient case |
| `GBWeight` | [`gbweight.{cpp,hpp}`](groebner-bases.md) | Weight tracking |
| `ReducedGB` | [`reducedgb.{cpp,hpp}`](groebner-bases.md) | Final reduction |
| `SPair` | [`spair.{cpp,hpp}`](groebner-bases.md) | S-pair data |

## S-pair selection

The default selection strategy is **sugar-aware degree-first**: pick
S-pairs whose sugar (a degree-based proxy for the eventual S-polynomial's
degree) is smallest. This dominates "lowest LCM degree" on most inputs.

Alternative strategies live in sibling files:

- `gb-homog2.cpp` — homogeneous specialisation
- `gb-sugarless.cpp` — pure-LCM, ignores sugar (useful for testing)
- `gb-toric.cpp` — toric ideals
- `gb-walk.cpp` — Gröbner walk

## Memory

The bookkeeping is heavy — basis, monomial tables, S-pair queue, reduced-GB
caches. All allocations are GC-managed; `gbA` is designed to be entirely
GC-collectable once the interpreter releases it.

## Related

- [`groebner-bases.md`](groebner-bases.md) — area overview.
- [`file-comp-gb.md`](file-comp-gb.md) — `GBComputation` base.
- [`file-gbring.md`](file-gbring.md) — value type.
- [`file-montable.md`](file-montable.md) — leading-monomial index.
- [`reducedgb.{cpp,hpp}`](groebner-bases.md) — final reduction pass.
