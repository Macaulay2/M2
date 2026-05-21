# `xmlactors.d` — XML operator overloads

`xmlactors.d` overloads the **`[]` and `.`-style operators** on
the `XMLnode` / `XMLattr` types declared in
[`file-xml.md`](file-xml.md). It's the operator-side companion
file the same way `actors*.d` is for general M2 operators.

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's in the file

```d
-- Copyright 2009 by Daniel R. Grayson
use common;
use util;

header "
#include <libxml/parser.h>
#include <libxml/tree.h>
";

-- xmlparse(e:Expr):Expr := (...)
-- setupfun("XML$parse",xmlparse);

type(n:xmlNode) ::= Ccode(int, "((", n, ")->type)");
ElementNode() ::= Ccode(int,"XML_ELEMENT_NODE");
TextNode() ::= Ccode(int,"XML_TEXT_NODE");
isElement(n:xmlNode) ::= type(n) == ElementNode();
isText(n:xmlNode) ::= type(n) == TextNode();
```

A mix of:

- **Inline node-type tests** — `isElement(n)`, `isText(n)`.
- **Operator definitions** for navigating XML trees.
- **C-flavored helpers** — `Ccode(int, "(...)->type")` — that map
  libxml2 macros to `.d` predicates.

## What operators are overloaded

The M2-visible XML navigation API:

- **`n.tag`** — element name (for elements).
- **`n.content`** — text content.
- **`n.attributes`** — `XMLattr` list.
- **`n.children`** — child list.
- **`n[i]`** — i-th child.
- **`a.name`** / **`a.value`** — attribute name / value.

Each of these is wired in `xmlactors.d` via the standard
`installMethod` calls.

## Why split from `xml.d`

[`file-xml.md`](file-xml.md) handles the libxml2 *parsing* and
data-type declarations. `xmlactors.d` handles the *M2 syntactic
sugar*. Splitting them lets the parser file stay minimal and
contains the operator-registration noise in one place.

## Used by

- M2 user code working with XML.
- Documentation export → MathML / HTML
  ([`../m2/file-mathml.md`](../m2/file-mathml.md)).
- The TeXmacs frontend ([`file-texmacs.md`](file-texmacs.md)).

## Related

- [`README.md`](README.md) — d/ overview.
- [`file-xml.md`](file-xml.md) — parser / type declarations.
- [`file-actors.md`](file-actors.md) — sister operator-overload
  files for general M2 types.
- libxml2 — external linked library.
