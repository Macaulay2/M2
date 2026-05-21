# `fold.m2` — `accumulate` and `fold` family

`fold.m2` defines M2's **fold / accumulate** family — the
functional-programming primitives for reducing a list (or iterator)
to a single value via repeated application of a binary function.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's declared

```m2
-- TODO: implement a copy-free accumulate and fold without reverse and drop in the interpreter
-- One solution is to use iterators; see https://github.com/Macaulay2/M2/issues/1904

needs "methods.m2"

accumulate = method()
accumulate(Function, Thing, VisibleList) := VisibleList =>
accumulate(Function, Thing, Thing)       := Iterator    => (f, x, v) -> (
    ...
)
```

The TODO captures a performance concern: today's `accumulate` does a
`reverse` + `drop` internally, which copies. A future iterator-based
rewrite (see [#1904](https://github.com/Macaulay2/M2/issues/1904))
would avoid the copying.

## API

- **`fold(f, init, L)`** — left fold: `f(...f(f(init, L#0), L#1)..., L#-1)`.
- **`fold(f, L)`** — left fold using `L#0` as the seed.
- **`accumulate(f, init, L)`** — like `fold` but returns the
  intermediate results (one per element).
- **`accumulate(f, L)`** — accumulate without seed.
- **`scan(L, f)`** — apply `f` to each element for side effects.
- **`apply(L, f)`** — produce `{f(x) | x ∈ L}`.

## Difference between `fold` and `accumulate`

```m2
fold((a, b) -> a + b, 0, {1, 2, 3, 4})
-- 10

accumulate((a, b) -> a + b, 0, {1, 2, 3, 4})
-- {1, 3, 6, 10}
```

`fold` returns only the final value; `accumulate` returns the list of
all intermediate values.

## Iterator support

`accumulate(Function, Thing, Thing)` accepts an arbitrary `Thing`
(typically an `Iterator`) as the source, producing another iterator.
This makes it work with lazy infinite sequences.

## Used by

- Every M2 program — `fold` and `apply` are foundational.
- Algorithm implementations across the engine and packages.
- [`file-lists.md`](file-lists.md) — companion list operations.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-lists.md`](file-lists.md) — list operations.
- [`file-iterators.md`](file-iterators.md) — `Iterator` type.
