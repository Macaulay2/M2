# `pretty.m2` — `pretty` (pretty-print intermediate forms)

`pretty.m2` defines **`pretty`** — a debugging / introspection helper
that produces a pretty-printed representation of M2 values,
typically for inspection of structural details that the standard
output doesn't show.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's declared

```m2
--		Copyright 2006 by Daniel R. Grayson

needs "nets.m2"
needs "hypertext.m2"

pretty  = method(Dispatch => Thing)
pretty2 = method(Dispatch => Thing)
pretty Thing := x -> stack pretty2 x

pr := ou -> x -> (
    ...
)
```

Two parallel methods:

- **`pretty`** — produces a `Net` (terminal-friendly pretty form).
- **`pretty2`** — produces a list of lines (the intermediate form
  before `stack`-ing into a `Net`).

`pretty x` wraps `pretty2` by stacking the result into a 2-D net.

## What `pretty` shows

The standard `<< x` displays the result of evaluating `x`.
`pretty x` instead reveals the **structural form** — useful when
debugging types or learning how M2 stores something:

```m2
pretty {1, 2, {3, 4}, "five"}
-- shows the list as a tree with type info per element
```

For complex objects (modules, rings, matrices), `pretty` reveals
internal hash-table fields.

## Difference from `peek`

[`file-peek.md`](file-peek.md) (`peek`) is the more commonly used
introspection tool. The difference:

- **`peek x`** — show the underlying hash-table representation,
  with arbitrary depth control.
- **`pretty x`** — show a typed, hierarchical, multi-line breakdown.

`pretty` is closer to "what is this thing's algebraic structure";
`peek` is closer to "what does this object look like in memory."

## Used by

- M2 users debugging.
- Documentation that needs to show internal structure.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-peek.md`](file-peek.md) — sister introspection helper.
- [`file-nets.md`](file-nets.md) — output type.
- [`file-hypertext.md`](file-hypertext.md) — `pretty` produces
  output usable by the hypertext system.
