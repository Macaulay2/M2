# `iterators.m2` — `Iterator` type and `for...in` machinery

`iterators.m2` defines the **`Iterator`** type — Macaulay2's lazy-
sequence abstraction — and the `iterator` / `next` methods that the
`for...in` loop syntax dispatches to.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's declared

```m2
needs "classes.m2"
needs "methods.m2"

-- originally defined (as null) in evaluate.d
iterator = method(Dispatch => Thing)
next = method()

Iterator = new SelfInitializingType of FunctionClosure
Iterator.synonym = "iterator"
```

The two key methods:

- **`iterator x`** — produce an `Iterator` for `x`.
- **`next iter`** — advance the iterator, returning either the next
  element or `null` if exhausted.

`Iterator` extends `FunctionClosure` — under the hood an iterator
is a closure that yields one element each time it's called, plus
state for "are we done?"

## `for x in expr do ...`

The M2 syntax `for x in expr do body` desugars to:

```m2
iter := iterator expr
while (val := next iter) =!= null do (
    x := val;
    body
)
```

Hence anything that has an `iterator` method works in `for` loops —
not just lists and sequences but also tallies, sets, infinite
generators, file streams, etc.

## Lazy semantics

Iterators are **lazy** — they generate elements one at a time on
demand. This lets M2 represent and traverse infinite sequences:

```m2
ones = new Iterator from (() -> 1)
for x in ones do if x > 10 then break  -- never breaks; safety brake here
```

In practice the engine prefers strict (eagerly-materialised) data
where possible, but iterators are useful for streamed inputs (file
lines, network responses, large polynomial generators).

## `SelfInitializingType`

`Iterator` is declared `new SelfInitializingType of FunctionClosure`.
`SelfInitializingType` is a meta-type from
[`file-classes.md`](file-classes.md) that lets `Iterator(...)`
construct instances directly without calling a separate constructor.

## Used by

- M2's `for...in` loops.
- Lazy data streams in network / file packages.
- [`file-lists.md`](file-lists.md) iteration paths.
- The original-form `apply`, `scan`, `fold` over iterators.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-classes.md`](file-classes.md) — type system + `SelfInitializingType`.
- [`file-lists.md`](file-lists.md) — strict collection-type sibling.
- `fold.m2` — fold + sister operations.
