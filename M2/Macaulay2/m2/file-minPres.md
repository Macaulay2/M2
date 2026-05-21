# `minPres.m2` — `minimalPresentation` for ideals and rings

`minPres.m2` defines **`minimalPresentation`** — the M2-side
operation that simplifies an ideal or quotient ring by removing
redundant generators and variables.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## Author

```m2
-- This file written by Amelia Taylor <ataylor@stolaf.edu>
```

Authored by Amelia Taylor. The file was last updated in June 2006
(per a comment near the top).

## What `minimalPresentation` does

For an ideal `I ⊂ R` or a quotient ring `R/I`, simplify in two
ways:

1. **Linear elimination** — if `I` contains an element of the form
   `x - p` where `p` doesn't involve `x`, then `x` can be eliminated
   by substituting `p`. This shrinks the variable count.
2. **Trivial-generator removal** — generators of `I` that become
   trivially zero after the substitution are dropped.

The output is an equivalent ideal / quotient ring in a smaller
ambient polynomial ring.

## Why this matters

Computer-generated ideals often have many redundant generators (from
GB computations, intersection operations, etc.). For example, an
ideal might be presented as:

```
J = (x - y - 1, y - z + 2, x^2 - z^2)
```

Linear elimination of `x` and `y` reduces this to a single
generator in one variable. `minimalPresentation` finds the smallest
equivalent presentation.

## Algorithm

Internally:

1. Compute a GB of `I` with respect to a lex order.
2. Walk the GB looking for linear generators `x_i - p_i`.
3. Substitute and re-check until no more linear eliminations are
   possible.
4. Construct the resulting smaller polynomial ring + the residual
   generators.

The implementation uses [`file-gb.md`](file-gb.md) for the GB and
[`file-ringmap.md`](file-ringmap.md) for the substitutions.

## Used by

- M2 users simplifying intermediate computations.
- Algebraic-geometry packages — `presentation R` for the user's
  variety often produces redundant presentations that
  `minimalPresentation` cleans up.
- Documentation examples showing succinct ideal generators.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-gb.md`](file-gb.md) — uses GB internally.
- [`file-ringmap.md`](file-ringmap.md) — substitution machinery.
- `newring.m2` — produces the smaller ambient polynomial ring.
- `quotient.m2`, [`file-quotring.md`](file-quotring.md) — quotient
  ring construction.
