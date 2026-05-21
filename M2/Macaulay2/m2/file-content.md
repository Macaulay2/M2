# `content.m2` — MathML content / element-type tables

`content.m2` is a **lookup table** of MathML 2 content / element
types — which elements can contain what, attribute defaults, etc.
It is data-only (no functions), supplying tables that
[`file-mathml.md`](file-mathml.md) and
[`file-validate.md`](file-validate.md) consult.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's declared

```m2
needs "set.m2"

protect qname                     -- an internal key

-- from xhtml-math11-f.dtd:
-- the one I looked at first:    "http://www.w3.org/TR/MathML2/dtd/xhtml-math11-f.dtd"
-- the newer version: "http://www.w3.org/Math/DTD/mathml2/xhtml-math11-f.dtd"

PCDATA = set {"#PCDATA"}
```

The file is essentially a transcription of the **XHTML/MathML 2
DTD** into M2 hash tables. The two URL comments at the top track
which version of the DTD was the source.

## Why a separate file

Validating that `<mfrac>` contains exactly two children, or that
`<msup>` accepts only specific element types, requires the same
DTD-derived knowledge that's spelled out in the W3C document. Rather
than encoding it inline in `mathml.m2`, the maintainers transcribed
it here.

If MathML 3 is ever wanted, only this file needs updating.

## `PCDATA`

`PCDATA = set {"#PCDATA"}` — the standard XML "parseable character
data" marker. Used to identify text-only content models.

## Used by

- [`file-mathml.md`](file-mathml.md) — content-model lookups.
- [`file-validate.md`](file-validate.md) — element nesting
  validation.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-mathml.md`](file-mathml.md), [`file-validate.md`](file-validate.md)
  — primary consumers.
- W3C MathML 2 DTD — the upstream source.
