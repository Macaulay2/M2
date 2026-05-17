# `validate.m2` — `Hypertext` validation

`validate.m2` implements **validation** of `Hypertext` AST nodes —
checking that doc-node bodies are well-formed, that no block-level
elements appear inside inline elements, that links point to known
targets, etc.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```m2
--		Copyright 2006 by Daniel R. Grayson
-- validate and fix Hypertext objects

needs "content.m2"
needs "hypertext.m2"
needs "methods.m2"

-- TODO: make this local
currentHelpTag = null
```

The file's job, in two words from the comment: **validate and fix**.
"Fix" is the lenient mode where small malformations get auto-
corrected; "validate" is the strict mode where they raise errors.

`currentHelpTag` is a global tracking the doc-node currently being
processed — used in error messages to tell the user *where* the
malformed node is.

## Validation rules

Some of the validations:

- **Inline-vs-block** — `EM` (inline) cannot contain `P` (block).
- **Link targets** — `TO foo` must reference an existing symbol or
  doc node.
- **Image sources** — `IMG{src => "x.png"}` — `x.png` must exist.
- **Table shape** — rows in a `TABLE` must have consistent column
  counts.
- **Heading order** — `H3` shouldn't appear without a containing
  `H2`.

Violations raise errors (in strict mode) or warnings (in lenient
mode).

## `fixup`

`fixup` is the lenient counterpart of `validate`. It walks a
`Hypertext` AST and auto-corrects common issues:

- Wrapping bare strings in a `Hypertext` paragraph.
- Inserting missing `LI`s inside `UL`/`OL`.
- Normalising whitespace.

Used by `document.m2` so authors don't have to write perfectly-
formed ASTs by hand.

## Used by

- [`file-document.md`](file-document.md) — every doc node passes
  through `fixup` before storage.
- [`file-installPackage.md`](file-installPackage.md) — runs `validate`
  on every node before rendering.
- [`file-help.md`](file-help.md) — re-validates when surfacing nodes
  interactively.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-hypertext.md`](file-hypertext.md) — `Hypertext` type
  hierarchy.
- [`file-document.md`](file-document.md), [`file-installPackage.md`](file-installPackage.md)
  — primary consumers.
- `content.m2` — supplies validation helpers.
