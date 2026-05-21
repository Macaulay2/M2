# `document.m2` — the Macaulay2 documentation DSL

`document.m2` implements **`document(...)`** — the DSL Macaulay2
packages use to declare documentation nodes. It is the engine of M2's
in-language help system and the input format for
`installPackage`-generated HTML / info / PDF docs.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What it provides

```m2
needs "code.m2"
needs "hypertext.m2"
needs "methods.m2"
needs "packages.m2"
needs "reals.m2"      -- for ImmutableType
needs "validate.m2"   -- for fixup

-- TODO: deprecate
rootPath = "";
rootURI = "file://";
```

Six dependencies, reflecting how thoroughly documentation cuts
across the M2 language:

- **`code.m2`** — pretty-printed M2 code in examples.
- **`hypertext.m2`** — the AST for documentation bodies (`TT`, `EM`,
  `UL`, `TO`, etc.).
- **`methods.m2`** — declared methods become documentation
  cross-references.
- **`packages.m2`** — each documentation node belongs to a package.
- **`reals.m2`** — pulls in `ImmutableType` used as a sentinel.
- **`validate.m2`** — `fixup` repairs lightly malformed nodes.

## The `document(...)` function

A documentation node is declared like:

```m2
document {
    Key => "my function",
    Headline => "describes what it does",
    Usage => "f x",
    Inputs => { "x" => ZZ => "an integer" },
    Outputs => { ZZ => "the doubled value" },
    "Some prose here.",
    EXAMPLE lines ///
        f(3)
    ///,
    SeeAlso => { "g", "h" }
}
```

`document.m2` parses this hash-table-shaped input, validates it
against the M2 documentation grammar, and stores the result in the
package's documentation database.

The `Key =>` field is the **lookup key** — the name (or
symbol-sequence) by which `help` will find this node later.

## Helpers

The file also defines:

- **`SYNOPSIS`** — a sub-block specifically for method signatures.
- **`SUBSECTION`**, **`SUBNODES`** — sub-structure within a node.
- **`hypertext`** — the umbrella formatter that produces M2's
  hypertext AST.
- **`ExampleItem`**, **`CITE`**, etc. — citation / example types.

## Companion: `installPackage.m2`

[`file-installPackage.md`](file-installPackage.md) consumes the
documentation database `document.m2` populates and renders it into
HTML, info, and (optionally) PDF outputs. The two files together
constitute M2's documentation pipeline.

## Used by

- Every M2 package — `document {...}` is the standard way to declare
  documentation.
- The `help` / `viewHelp` functions surface these nodes interactively
  ([`file-help.md`](file-help.md)).
- `installPackage` renders them to HTML / info / PDF.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-installPackage.md`](file-installPackage.md) — primary
  consumer.
- [`file-help.md`](file-help.md) — interactive help.
- `hypertext.m2`, `html.m2`, `latex.m2`, `mathml.m2`, `markdown.m2`,
  `texmacs.m2`, `book.m2` — output formatters.
