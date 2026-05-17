# `dotdot.m2` — `..` operator overloads

`dotdot.m2` extends the M2 `..` (range) operator to support a wide
variety of types — `ZZ`, `Symbol`, `IndexedVariable`,
`MonoidElement`, etc. — by registering per-type overloads after the
load sequence has progressed far enough to know about all of them.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's declared

```m2
needs "code.m2"
needs "indeterminates.m2"
needs "monoids.m2"
needs "variables.m2"

-- this code should go after the last method installed for baseName
scan(join(apply(methods baseName, last), {MonoidElement}),
    X -> if X =!= Symbol and X =!= IndexedVariable and X =!= Thing
         and not ancestor(Expression, X) then (
        err1 := lookup(symbol .., Thing, Thing);
        ...
    ))
```

The pattern:

1. Wait until all `baseName` methods have been installed.
2. For each type `X` that has a `baseName` method (and is not
   `Symbol`, `IndexedVariable`, `Thing`, or any `Expression`
   subclass), register a `X .. X` method that delegates via
   `baseName`.

This automatic registration means new types can opt into `..`
support just by declaring a `baseName` — without `dotdot.m2`
itself needing to know about them.

## What `..` does

Idiomatic uses:

- `1..10` — integer range.
- `a..z` — symbol range, expanded by `indeterminates.m2`.
- `x_0..x_5` — indexed-variable range.
- `R_0..R_n` — ring-variable range.

The convention is: produce the inclusive range from start to end.

## Why a late load

The dispatch table can only be assembled once every `baseName`
method is registered. Hence `dotdot.m2` loads near the end of the
sequence — see [`file-loadsequence.md`](file-loadsequence.md).

## Used by

- Every M2 user typing `a..b`.
- Polynomial-ring constructors accepting variable ranges.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-loadsequence.md`](file-loadsequence.md) — `dotdot.m2` is
  near the end.
- [`file-indeterminates.md`](file-indeterminates.md),
  [`file-variables.md`](file-variables.md) — primary consumers.
- [`file-monoids.md`](file-monoids.md) — `MonoidElement` is one of
  the registered types.
