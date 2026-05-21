# `code.m2` — code introspection (`code`, `methods`, `locate`)

`code.m2` implements **code introspection** — the `code(...)`,
`methods(...)`, `locate(...)` functions that let M2 users inspect
how a function is implemented. It is the engine of M2's
"show me the source" feature at the interactive prompt.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's declared

```m2
-- TODO: needs "document.m2" for formatDocumentTag, but this causes a loop

needs "debugging.m2"    -- for FilePosition
needs "gateway.m2"
needs "lists.m2"
needs "methods.m2"
needs "nets.m2"
```

The TODO captures a circular-dependency problem: `code.m2` would
ideally use `document.m2`'s `formatDocumentTag`, but
`document.m2` itself needs `code.m2` (for `code` blocks in
documentation). Today the dependency loop is broken by an
ad-hoc workaround.

## What `code(...)` does

```m2
code(rank, Matrix)
-- prints the body of the `rank Matrix` method
```

The flow:

1. Look up the method via [`file-methods.md`](file-methods.md)'s
   dispatch table.
2. Find its definition's **file + line range** (stored when the
   method was declared).
3. Read the source file and slice out the relevant lines.
4. Format as a net for display.

The result is the actual M2 source the method runs — invaluable for
debugging or learning how a feature is implemented.

## Companion functions

- **`methods f`** — list every method registered for `f`.
- **`locate f`** — return the file + line where `f` was defined.
- **`?`** (single question mark) — display `code(...)` for a value
  via the operator form declared in [`file-help.md`](file-help.md).

## `FilePosition`

`code.m2` works with **`FilePosition`** objects (declared in
`debugging.m2`) — tuples of `(filename, start_line, end_line,
start_col, end_col)`. Every M2 function call carries its
`FilePosition`, which is what makes `code` and `locate` work at all.

## Used by

- M2's interactive prompt — `code`, `locate`, `methods`.
- Documentation system ([`file-document.md`](file-document.md)) —
  includes inline `code` for cross-references.
- Editor integrations (Emacs `M2-mode`) — "jump to definition" uses
  `locate`.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-methods.md`](file-methods.md) — method dispatch.
- `debugging.m2` — `FilePosition` type.
- [`file-document.md`](file-document.md) — circular-dependency
  partner.
- [`file-help.md`](file-help.md) — uses `code` to surface implementations.
