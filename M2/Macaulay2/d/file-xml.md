# `xml.d` — libxml2 bindings

`xml.d` implements M2's **XML bindings** — wrappers around
[libxml2](http://xmlsoft.org/) for parsing XML files, building DOM
trees, and producing XML output.

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```d
-- Copyright 2009 by Daniel R. Grayson
use M2;

-- TO DO: declare these types in this file and have xml-c.c include xml-exports.h, for better type checking:
declarations "
#include <libxml/parser.h>
#include <libxml/tree.h>
";
```

The TODO captures planned cleanup: move the type declarations from
`xml-c.c` into this file so `xml-exports.h` (auto-generated) can be
the type-checking boundary.

## What's exposed

- **`XMLnode`** — M2 type wrapping `xmlNodePtr`.
- **`XMLattr`** — M2 type for attribute lists.
- **`parseXML`** — parse a string or file into a tree.
- **`tagName`, `tagAttr`, `children`** — DOM navigation.
- **`xmlPrint`** — render a tree back to text.

## Companion files

- **`xml-c.c`** — C-side glue. Declares helper functions and
  exception handlers that libxml2's C API needs.
- **`xml-c.h`** — header for the above.
- **`xmlactors.d`** — operator overloads for XML types (`<<`, `[]`,
  etc.).

## When XML is used

- The TeXmacs frontend exchange format involves XML.
- Documentation export to MathML
  ([`../m2/file-mathml.md`](../m2/file-mathml.md)) uses XML.
- Some packages parse external XML data (configuration files,
  research data).

## libxml2 vs. simpler parsers

libxml2 is heavyweight (full DOM, XPath, XSLT support). For simple
XML M2 could use a minimal parser, but libxml2 is widely available,
well-tested, and supports the namespacing M2 needs for MathML and
other XML applications.

## Used by

- TeXmacs frontend
  ([`../m2/file-texmacs.md`](../m2/file-texmacs.md)).
- MathML output ([`../m2/file-mathml.md`](../m2/file-mathml.md)).
- Packages parsing external XML.

## Related

- [`README.md`](README.md) — d/ overview.
- `xml-c.{c,h}`, `xmlactors.d` — companion files.
- libxml2 — external linked library.
- [`file-python.md`](file-python.md), [`file-ffi.md`](file-ffi.md)
  — sibling FFI bindings.
