# `option.m2` — `Option`, `OptionTable`, and option-handling

`option.m2` defines the **`Option`** and **`OptionTable`** types,
plus the machinery that lets M2 methods declare and consume keyword
options (`MyMethod := method(Options => {Strategy => "Default"})`).

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's declared

```m2
needs "classes.m2"    -- for codeHelper

Option.synonym       = "option"
OptionTable.synonym  = "option table"

all' := (L, p) -> not any(L, x -> not p x)

new OptionTable from List := (OptionTable, opts) -> (
    ...
)
```

Two types:

- **`Option`** — a single key-value pair, written `key => value` in
  M2 syntax.
- **`OptionTable`** — a collection of options accessible by key.

`OptionTable` is implemented as a `HashTable` subclass.

## Why options are core

Almost every method in M2 takes options. The pattern:

```m2
f = method(Options => {Strategy => "Default", DegreeLimit => infinity})
f Ideal := opts -> I -> (
    if opts.Strategy === "Default" then ...
    else ...
)
```

Without `option.m2`'s machinery, every method would need to
hand-roll its own option-parsing — error-prone and inconsistent.
Centralising this here gives every method the same calling
conventions.

## Method-option binding

When a user writes:

```m2
f(I, Strategy => "Fast", DegreeLimit => 5)
```

M2 separates the positional argument `I` from the keyword options.
The method body receives:

- `opts` — an `OptionTable` with the user's overrides merged into
  the defaults.
- The positional argument(s).

The merging logic lives in this file.

## `all'` helper

```m2
all' := (L, p) -> not any(L, x -> not p x)
```

Local utility "all of L satisfy p" — needed early (because
[`file-classes.md`](file-classes.md) loads before `lists.m2` where
the standard `all` lives).

## Used by

- Every method declared with `method(Options => …)`.
- Internal Core options like `Strategy`, `DegreeLimit`,
  `MinimalGenerators`, `Variable`.
- User packages — they declare their own options the same way.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-methods.md`](file-methods.md) — method dispatch (uses
  option tables).
- [`file-classes.md`](file-classes.md) — `Option` is part of the
  class hierarchy.
