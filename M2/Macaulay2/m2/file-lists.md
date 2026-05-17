# `lists.m2` — `List`, `Sequence`, `Array` operations

`lists.m2` defines the M2-side **list / sequence / array** types
and their operations: indexing, slicing, iteration, `map`, `apply`,
`scan`, `fold`, `select`, `unique`, `sort`, and many more.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## Type tags

```m2
needs "set.m2"
needs "methods.m2"

   Sequence.synonym = "sequence"
      Array.synonym = "array"
       List.synonym = "list"
  BasicList.synonym = "basic list"
VisibleList.synonym = "visible list"
```

The five list-like types:

| Type | Syntax | Mutable? | Use |
|---|---|---|---|
| `Sequence` | `(a, b, c)` | No | Multi-return, multi-arg unpacking |
| `Array` | `[a, b, c]` | No | Variable lists in polynomial rings |
| `List` | `{a, b, c}` | No | Default container |
| `BasicList` | (abstract) | No | Parent of the above |
| `VisibleList` | (abstract) | No | Sub-parent for serialisable lists |

The distinct types let M2 disambiguate: `(x, y)` is a multi-return
sequence, `[x, y]` is a polynomial variable list, `{x, y}` is a
regular list.

## Operations

The file defines hundreds of operations. The headline ones:

- **`#L`** — length.
- **`L#i`** — indexing.
- **`L_i`** — alternative indexing (subscript form).
- **`apply(L, f)`, `apply(f, L)`** — map `f` over `L`. Returns a new
  list.
- **`scan(L, f)`** — apply `f` for side effects; discard results.
- **`fold(f, init, L)`** — left fold.
- **`select(L, p)`** — filter `L` by predicate `p`.
- **`positions(L, p)`** — indices of `L` satisfying `p`.
- **`take(L, n)`, `drop(L, n)`** — slicing.
- **`reverse L`, `rotate(n, L)`, `unique L`, `sort L`** — utility.
- **`L + M`, `L * x`** — element-wise arithmetic.

## Sequence semantics

Sequences have **distinguished single-element behaviour**: `(x)` is
literally `x` (parentheses just group). To get a one-element
sequence, write `1 : x` or `singleton x`. This convention lets
multi-return work cleanly: `(a, b, c) = f()` deals with a 3-element
sequence; `a = g()` deals with a single value.

## Used by

Essentially every M2 file. `apply`, `scan`, `fold` are the
workhorses of M2 programming.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-classes.md`](file-classes.md) — type hierarchy.
- `fold.m2`, `iterators.m2` — adjacent iteration helpers.
- `set.m2` — `Set`-based operations.
- [`file-files.md`](file-files.md) — `lines` produces lists.
