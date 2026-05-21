# `installPackage.m2` — render package documentation to HTML / info / PDF

`installPackage.m2` is the M2 user's primary tool for building a
package's documentation. It runs every example, captures every
output, validates every doc node, and produces HTML / info / PDF
artefacts in the package's `share/` tree.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What it does

```m2
needs "document.m2"
needs "examples.m2"
needs "hypertext.m2"
needs "packages.m2"
needs "printing.m2"
needs "validate.m2"

-----------------------------------------------------------------------------
-- Generate the package documentation (html, info, pdf)
-----------------------------------------------------------------------------

-- TODO: add relative directory to minimizeFilename
-- TODO: make orphan overview nodes subnodes of the top node
-- FIXME: majority of this file is not reentrant
```

The header `FIXME` flags one of the file's known shortcomings —
it is not safe to run `installPackage` from inside another package
build. In practice users run one at a time.

`installPackage` invokes:

1. **Doc-node validation** — every `document{...}` in the package
   must be well-formed.
2. **Example execution** — every `EXAMPLE` block is run in a fresh
   M2 session; outputs are captured.
3. **HTML rendering** — `html.m2`'s formatter produces per-node
   `.html` files.
4. **Info rendering** — `texinfo`-based output for `M2 -e 'help …'`.
5. **PDF rendering** — optional, via the LaTeX backend in
   `latex.m2`.
6. **Index building** — `gdbm`-backed lookup tables consumed by
   [`file-help.md`](file-help.md).

## Output layout

```
share/M2/<Package>/
    html/
        index.html
        _<topic>.html
        ...
    info/
        <Package>.info
    examples/
        <topic>.m2
```

The HTML files are self-contained and can be served directly.
`viewHelp <Package>` opens `index.html` in the user's browser.

## Why it's slow

Running every example in a fresh M2 session is the dominant cost.
For packages with hundreds of examples, `installPackage` can take
many minutes. There are flags to skip examples (faster, but the
docs lose their output captures) — see the `RunExamples => false`
option.

## Companion: `examples.m2`

`examples.m2` implements the example-running machinery
`installPackage` invokes. It manages the subprocess, timeouts,
error handling, and output diff-ing.

## Used by

- Every M2 package author preparing a new release.
- The Macaulay2 build itself, when distributed packages are
  pre-installed.
- CI checks via `check-Packages`.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-document.md`](file-document.md) — input DSL.
- [`file-help.md`](file-help.md) — consumer of generated databases.
- `examples.m2` — example runner.
- `html.m2`, `latex.m2`, `mathml.m2`, `texmacs.m2`, `markdown.m2`,
  `book.m2` — output formatters.
- [`../packages/README.md`](../packages/README.md) — packages that
  consume this.
