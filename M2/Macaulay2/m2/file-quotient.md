# `quotient.m2` — `quotient` (ideal quotient `I : J`)

`quotient.m2` defines the **`quotient`** method — `I : J` for
ideals, the operation that produces the ideal of all `r ∈ R` with
`r · J ⊆ I`.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```m2
-- Copyright 1993-1999 by Daniel R. Grayson
-- Copyright 1996 by Michael E. Stillman

needs "max.m2"        -- for infinity
needs "methods.m2"

-- ideal quotient methods moved to packages/Saturation.m2 in July 2020
quotient = method(
    Options => {
        DegreeLimit       => {},
        ...
    })
```

The comment captures an important refactor: in July 2020 the
heavier ideal-quotient methods (`quotient(I, J)`, `saturate`, ...) moved
from Core to the **`Saturation`** user package. Core still declares
the method `quotient` here and dispatches to the package on demand.

## What `quotient` computes

For ideals `I, J ⊂ R`:

```
quotient(I, J)  =  { r ∈ R  :  r · J ⊆ I }
```

Often written `I : J`. It is the algebraic analogue of "remove the
common factor `J` from `I`."

Special cases:

- `I : J` for `J = (f)` a principal ideal — single-element quotient.
- `I : J^∞` — `saturate(I, J)` — repeated quotient until stable
  (lives in `Saturation` package).

## Options

The `Options =>` block sets standard GB-style stop conditions:

- `DegreeLimit` — stop when intermediate degrees exceed this.
- `MinimalGenerators` — return minimal generators.
- `Strategy` — algorithm choice.

The options correspond to engine-level GB stop conditions
([`../e/interface/file-computation-interface.md`](../e/interface/file-computation-interface.md)).

## Algorithm sketch

`quotient(I, J)` for `J = (j_1, …, j_n)` computes:

```
quotient(I, J)  =  ⋂ quotient(I, j_i)
```

where each `quotient(I, j_i)` is the principal-ideal case (a GB
computation involving syzygies).

## Used by

- M2 users computing `I : J`.
- Algebraic-geometry packages working with localisation, saturation.
- The `Saturation` user package builds on this.

## Related

- [`README.md`](README.md) — m2/ overview.
- `Saturation` user package — the post-2020 home for heavier
  quotient operations.
- [`file-gb.md`](file-gb.md) — uses GB to compute quotients.
- [`file-intersect.md`](file-intersect.md) — uses `quotient` in some
  strategies.
- [`file-monideal.md`](file-monideal.md) — fast monomial-ideal
  quotient path.
