# `variables.m2` — `IndexedVariable` and related types

`variables.m2` defines the **`IndexedVariable`** type — the M2-side
representation of subscripted variables like `x_0`, `y_(1,2)`,
`p_{n}`. These let users construct polynomial rings with named
families of variables.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's declared

```m2
needs "expressions.m2"
needs "methods.m2"

-- indexed variables

IndexedVariable = new Type of BasicList
IndexedVariable.synonym = "indexed variable"
expressionValue IndexedVariable := value          -- do we really want this?
```

`IndexedVariable` is a `BasicList` subclass with two fields:

- The **base symbol** (e.g. `x`).
- The **index** (`ZZ`, `Sequence`, or `Array`).

`x_0` parses to `IndexedVariable{x, 0}`; `y_(1, 2)` parses to
`IndexedVariable{y, (1, 2)}`.

## Why indexed variables matter

In polynomial-ring construction:

```m2
R = QQ[x_0 .. x_5]                    -- {x_0, x_1, ..., x_5}
S = QQ[x_(0,0), x_(0,1), x_(1,0)]     -- generic matrix entries
T = QQ[apply(genericMatrixVars, v -> x_v)]
```

The indexed-variable mechanism lets users describe arbitrarily-shaped
families of generators without spelling them all out.

## Variable name generation

When M2 displays variables, it walks the `IndexedVariable` to
produce subscripted output:

```text
x_0     →  x₀     (Unicode)
x_0     →  x_{0}  (LaTeX, via expression)
x_0     →  x<sub>0</sub>  (HTML)
```

The formatting goes through the
[`Expression`](file-expressions.md) AST, with the indexed-variable
display being a single AST node (`Subscript`).

## `expressionValue`

The comment "-- do we really want this?" flags an open question:
`expressionValue` on `IndexedVariable` returns its value (i.e.,
the variable's stored value if assigned). The author noted this
behaviour might be more confusing than helpful.

## Used by

- Every M2 polynomial ring constructed with subscripted variables.
- `Polyhedra`, `ChainComplexes`, and similar packages that
  programmatically generate variables.
- Schubert calculus and combinatorics packages that index variables
  by partitions, Young tableaux, etc.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-indeterminates.md`](file-indeterminates.md) — single-letter
  variable management.
- [`file-monoids.md`](file-monoids.md) — uses these in polynomial-
  ring construction.
- [`file-expressions.md`](file-expressions.md) — display via
  `Expression`.
