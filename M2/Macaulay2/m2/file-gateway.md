# `gateway.m2` — `ScriptedFunctor` and helpers (functor glue)

`gateway.m2` defines **`ScriptedFunctor`** — M2's mechanism for
operators that look like indexed access (`R^n`, `R^{1, 2, 3}`,
`coker f`, `id_R`) — plus a set of small helpers for "flatten the
arguments to a functor" and similar argument-pre-processing.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's declared

```m2
needs "expressions.m2"
needs "methods.m2"

-----------------------------------------------------------------------------
-- helpers for functors
-----------------------------------------------------------------------------

-- flatten the arguments given to a scripted functor
```

The file's main contribution:

- **`ScriptedFunctor`** — a type whose instances respond to `^`,
  `_`, and similar operators with custom behaviour. Examples in M2:

  ```m2
  R^3            -- ScriptedFunctor: free module of rank 3
  R^{1, 2}       -- ScriptedFunctor: graded free module
  ker f          -- ScriptedFunctor: kernel
  id_R           -- ScriptedFunctor: identity matrix
  ```

- **Argument-flattening helpers** — when a user writes `R^{1, 2,
  3}`, M2 has to unfold the bracketed list into method-call shape.
  These helpers do that uniformly.

## Why functors are different

A method like `add(x, y)` is called as `add(x, y)` — straightforward.
But `R^3` parses as the `^` operator on `R` and `3`. M2's parser
handles a small set of these operators (`^`, `_`, `**`, etc.)
specially; `ScriptedFunctor` is the type that lets a value behave
nicely in those positions.

Without `ScriptedFunctor`, you would write `R^3` as `freeModule(R,
3)`. The functor mechanism gives the M2 user the more natural
mathematical notation.

## `id`

The most-used `ScriptedFunctor` in the engine. The line `id_R`
parses as `id` applied to `R` via the functor mechanism, producing
the identity map of `R`. Other scripted-functor consumers
(`module`, `image`, `kernel`, `coker`) follow the same pattern.

## Used by

- Type declarations across the M2 layer — `Module`, `Matrix`, and
  many subtypes register `ScriptedFunctor` methods for `^`, `_`,
  etc.
- [`file-modules.md`](file-modules.md) — primary user via `R^n`,
  `R^{...}`.
- [`file-matrix.md`](file-matrix.md) — `f * matrix{{...}}`,
  `submatrix`, etc.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-modules.md`](file-modules.md), [`file-matrix.md`](file-matrix.md)
  — primary consumers.
- [`file-methods.md`](file-methods.md), [`file-classes.md`](file-classes.md)
  — adjacent infrastructure.
