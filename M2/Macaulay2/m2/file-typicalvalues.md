# `typicalvalues.m2` — `typicalValues` (return-type hints)

`typicalvalues.m2` implements the **`typicalValues`** mechanism —
M2's lightweight return-type-annotation system. Methods can declare
what type they typically produce, which helps the documentation
system and the M2 prompt show useful information without running
the function.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## Pattern

Methods can be annotated:

```m2
rank = method(TypicalValue => ZZ)
rank Matrix := M -> ...

dim = method(TypicalValue => ZZ)
dim Module := M -> ...
```

`TypicalValue => ZZ` records that `rank` returns a `ZZ`. This shows
up in:

- The interactive prompt's `? rank` help.
- Auto-generated documentation.
- IDE / editor tools that surface type information.

## How `typicalValues` is stored

A global mutable hash table maps `(method, ArgClass)` pairs to a
declared return type. Methods can have **per-argument-type** return
declarations:

```m2
f = method()
typicalValues#(f, ZZ) = String
typicalValues#(f, RR) = String
```

Different argument types can produce different return types — the
table tracks them all.

## Why not full static typing

M2 is dynamically typed and intentionally so — algebraic objects
change types based on operations (`R` is a `PolynomialRing` until
you quotient it, then it's a `QuotientRing`). Full static typing
would force the user to write conversions everywhere.

`typicalValues` is a **hint**, not a constraint. It is consulted by
helpful tools but never enforced. Functions can return whatever they
want at runtime; the hint just gives users / docs a useful default.

## Used by

- [`file-document.md`](file-document.md) — uses typical values to
  pre-fill `Outputs => …` in doc nodes.
- [`file-help.md`](file-help.md) — display in `help` output.
- Editor integrations.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-methods.md`](file-methods.md) — method declarations.
- [`file-option.md`](file-option.md) — sister type-decoration
  mechanism (for options).
- [`file-document.md`](file-document.md) — primary consumer.
