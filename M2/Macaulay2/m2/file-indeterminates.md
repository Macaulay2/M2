# `indeterminates.m2` — variable-name management

`indeterminates.m2` provides **automatic generation of variable
names** — the machinery behind `a, b, c, …` for ring construction
without manually spelling out every name. It maps integer indices to
ASCII letters in a canonical sequence.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's declared

```m2
needs "expressions.m2"
needs "methods.m2"

varIndices := new MutableHashTable

varName := i -> (
    if 0 <= i and i < 26 then ascii(97 + i)
    else if 26 <= i and i < 52 then ascii(65 + i - 26)
    ...
)
```

The mapping:

- **`0..25`** → `a, b, c, …, z` (ASCII 97..122).
- **`26..51`** → `A, B, C, …, Z` (ASCII 65..90).
- **`52..`** → indexed variables (`x_0`, `x_1`, …).

`varIndices` is a mutable hash table that caches inverse lookups
(symbol → index).

## `runLengthEncode`

A neighbouring function `runLengthEncode` is implemented here. It
takes a list and groups consecutive equal elements into
`(count, value)` pairs:

```m2
runLengthEncode {1, 1, 1, 2, 2, 3}
-- {(3, 1), (2, 2), (1, 3)}
```

Used by polynomial-ring display to compress repeated degree vectors
(e.g. `Degrees => {{1,0}, {1,0}, {0,1}, {0,1}, {0,1}}` displays as
`{2:{1,0}, 3:{0,1}}`).

## How it all flows

When the user writes:

```m2
R = QQ[a..f]
```

The parser:

1. Recognises `a..f` as a `Range`.
2. Asks `indeterminates.m2` for variable indices.
3. The result is `{0, 1, 2, 3, 4, 5}` → `{a, b, c, d, e, f}`.
4. The polynomial-ring constructor takes those.

When the user writes `R = QQ[x_0..x_5]`:

1. The parser recognises `x_0..x_5` as a range of indexed variables.
2. Each is an [`IndexedVariable`](file-variables.md), not a single
   symbol.

The two paths differ but produce equivalent ring constructions.

## Used by

- Every M2 polynomial ring constructed with `a..z` ranges.
- [`file-monoids.md`](file-monoids.md) — accepts these as
  variable lists.
- The variable-name display logic in
  [`file-expressions.md`](file-expressions.md).

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-variables.md`](file-variables.md) — `IndexedVariable`.
- [`file-monoids.md`](file-monoids.md) — consumer.
- [`file-polyrings.md`](file-polyrings.md) — polynomial-ring
  constructor.
