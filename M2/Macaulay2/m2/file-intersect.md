# `intersect.m2` — `intersect` method dispatcher

`intersect.m2` defines the M2-side **`intersect`** method — the
generic dispatcher for set-intersection-style operations across
varieties of types: ideals, modules, sets, coherent sheaves, etc.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```m2
-- TODO: move this to an appropriate package
-- TODO: now we can defined intersect for Set, CoherentSheaf, etc.
-- TODO: add tests
-- TODO: add intersection with a ring, via selectInSubring
-- TODO: how to cache partial computation?

needs "matrix1.m2"
needs "shared.m2"

-- This is a map from method keys to strategy hash tables
```

The TODOs capture the file's mid-refactor status:

- "Move this to an appropriate package" — `intersect` is a candidate
  for moving out of Core into a dedicated `IntersectThings` or
  similar package.
- "Now we can define intersect for Set, CoherentSheaf, etc." —
  the strategy table allows adding new types declaratively rather
  than via ad-hoc overrides.
- The other TODOs are open questions.

## Strategy table

The file maintains a **map from method keys to strategy hash
tables**. Each (input-type, output-type) pair gets a strategy entry
that lists the available algorithms and a `Default` choice.

For `intersect(Ideal, Ideal)`, the strategy table currently has
strategies:

- **`Default`** — pick automatically based on inputs.
- **`Quotient`** — via `(I * R^1) : J`.
- **`Elimination`** — homogenisation + elimination of an auxiliary
  variable.
- **`Iterate`** — split a long argument list into pairs.

Each strategy is a function the user can invoke explicitly via
`intersect(..., Strategy => "Quotient")`.

## Usage pattern

```m2
intersect(I, J)                       -- two ideals
intersect(I, J, K)                    -- multiple
intersect({I, J, K})                  -- list form
intersect(M, N)                       -- modules
intersect(S, T)                       -- sets
intersect(F, G)                       -- coherent sheaves (when loaded)
```

The output type matches the input type.

## Used by

- M2 users computing intersections.
- Algebraic-geometry packages that take intersections of varieties /
  schemes / cycles.
- Combinatorics packages working with `Set` intersections.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-monideal.md`](file-monideal.md) — `MonomialIdeal`
  intersections (much faster path).
- `shared.m2` — utility helpers.
- `selectInSubring` (in `quotring.m2` / `newring.m2`) — adjacent
  pattern for ideal-from-quotient.
