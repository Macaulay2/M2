# `classes.m2` — the M2 type hierarchy

`classes.m2` implements the **type hierarchy** the M2 language is
built on: how `Type`, `Thing`, `class`, `parent`, and the inheritance
relation work. It is one of the earliest files in
[`loadsequence`](file-loadsequence.md) because almost every later
file depends on it.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's here

```m2
-----------------------------------------------------------------------------
-- Functions dealing with types
-----------------------------------------------------------------------------

ancestors  = T -> unique join({T}, while (T = parent T) =!= Thing list T, {Thing})
ancestors' = T -> unique join({T}, while (T = class  T) =!= Type  list T, {Type})

synonym = X -> X.synonym ?? "object of class " | toString X

plurals = new MutableHashTable from {
    "body"       => "bodies",
    "dictionary" => "dictionaries",
    "matrix"     => "matrices",
    "sheaf"      => "sheaves",
    "variety"    => "varieties",
}
```

Three pieces:

- **`ancestors T`** — the chain of `parent`s from `T` up to
  `Thing`. Used by `?` (help) and by method dispatch.
- **`ancestors' T`** — the chain of `class`es from `T` up to `Type`
  (the meta hierarchy).
- **`synonym X`** — a human-readable name for the class of `X`,
  used in error messages.
- **`plurals`** — irregular plurals so `n matrices` formats correctly.

## The two parallel hierarchies

M2 has **two orthogonal type hierarchies**:

| Relation | Traversed via | Example |
|---|---|---|
| Inheritance ("is-a") | `parent` | `ZZ → Ring → EngineRing → Ring → InexactField → Thing` |
| Meta ("class of") | `class` | `5 → ZZ → Ring → Type → Type` |

`ancestors` walks the inheritance hierarchy; `ancestors'` walks the
meta hierarchy. Both terminate by convention at `Thing` / `Type`.

## How this connects to method dispatch

When you call `f(x)`, M2's method dispatch ([`file-methods.md`](file-methods.md))
walks `ancestors(class x)` looking for a registered handler. The
walk uses `classes.m2`'s `ancestors` helper, so this file is the
foundation that the whole method-dispatch system builds on.

## Used by

- [`file-methods.md`](file-methods.md) — method dispatch.
- Every other Core file that defines `class` / `parent` relations.
- M2's `class`, `parent`, `instance` built-ins surface this logic.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-methods.md`](file-methods.md) — primary consumer.
- `file-option.md` — sibling primitive.
- `code.m2` — sibling that surfaces type info to the user.
