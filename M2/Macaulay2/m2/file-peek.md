# `peek.m2` — `peek` (inspect underlying structure)

`peek.m2` defines **`peek`** — the diagnostic that reveals an M2
value's underlying hash-table / list / sequence structure. It is the
go-to "what does this thing really look like?" tool.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's declared

```m2
needs "expressions.m2"    -- for precedence
needs "hypertext.m2"
needs "methods.m2"

peek' = method(TypicalValue => Net)

peek'(ZZ, ZZ)      := (depth, n) -> toString n
peek'(ZZ, Nothing) := (depth, s) -> "null"
```

`peek'` is the depth-bounded recursive entry: `peek'(depth, x)`
walks `x` to the given depth and produces a `Net` describing its
structure.

## API

- **`peek x`** — peek at depth 1.
- **`peek(x, n)`** — peek to depth `n`.
- **`peek' depth value`** — primitive recursive form.

## Why depth limits

For deeply nested objects (like a chain complex with multiple
matrices, each with multiple columns, each with multiple terms),
unbounded `peek` would print pages of structural detail. The depth
bound cuts the walk at a sensible level, showing the top structure
and summarising deeper levels.

## What `peek` reveals

For a typical engine-backed object (say `Matrix`), `peek` shows:

```text
Matrix {
    RawMatrix => RawMatrix
    source => Module
    target => Module
    cache => CacheTable
}
```

This makes clear that `Matrix` is just a tagged hash table with
specific fields. The user can then `peek` into one of those fields
to dig deeper.

## Compared to `pretty`

[`file-pretty.md`](file-pretty.md)'s `pretty` produces a more
mathematically-oriented breakdown. `peek` is closer to "what's
literally stored." Both are useful at different stages of debugging.

## Used by

- M2 users debugging types / data structures.
- Test code verifying internal structure.
- Documentation that needs to show how a type is laid out.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-pretty.md`](file-pretty.md) — sister introspection helper.
- [`file-code.md`](file-code.md) — `code` for *function* bodies.
- [`file-classes.md`](file-classes.md) — `class` for *type* lookup.
