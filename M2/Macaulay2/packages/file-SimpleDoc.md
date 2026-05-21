# `SimpleDoc.m2` — the doc DSL most packages actually use

The `SimpleDoc` package exports **`doc`** and **`multidoc`** — the
literate, indentation-aware DSL that almost every M2 package uses
for its documentation. **Auto-loaded** so package authors don't
need `needsPackage "SimpleDoc"` boilerplate before each `doc`
block.

Sister to `m2/document.m2` (the lower-level typed-doc-node
machinery in Core): `SimpleDoc` is the **parser** that takes a
human-friendly indented string and emits the typed doc nodes that
`document.m2` consumes. See
[`DOCUMENTATION-SYSTEM.md`](../../../DOCUMENTATION-SYSTEM.md) for
the full end-to-end pipeline.

- Main file: `SimpleDoc.m2` (329 lines)
- Auxiliary directory: `SimpleDoc/` (4 files, 600 lines)
- Authors: Daniel R. Grayson, Mike Stillman, Mahrud Sayrafi
- Imports: [`Text`](Text.m2) (for hypertext primitives)
- Date: March 2025

[← back to packages overview](README.md) ·
[← top-level TOC](../../../README.md#packages)

## Exported API

```m2
doc String                       -- parse + register a single doc node
multidoc String                  -- alias (parses multiple Node blocks)
packageTemplate "PkgName"        -- emit a starter package skeleton

arXiv "1234.5678"                -- HTML link to arXiv paper
arXiv("1234.5678", "title")
stacksProject(tag, title)        -- HTML link to a Stacks Project tag
wikipedia "topic"                -- HTML link to Wikipedia

docTemplate                      -- string template for a doc block
docExample                       -- sample doc block (rendered)
testExample                      -- sample TEST block
simpleDocFrob                    -- a dummy frobenius function used in examples
```

## The DSL

A `doc` block is a triple-quoted string with **YAML-like
indentation** marking node types:

```m2
doc ///
Node
  Key
    (foo, Bar, Baz)
  Headline
    one-line description
  Usage
    result = foo(b, c)
  Inputs
    b : Bar
    c : Baz
  Outputs
    result : Frobnitz
  Consequences
    Item
      side effect description
  Description
    Text
      Free-form paragraph text. You can @TO Bar@ inline.
    Example
      foo(bar, baz)
    Tree
      bullet-list display
    Code
      M2 code shown verbatim
    Pre
      preformatted text
    CannedExample
      example shown but not executed
  ExampleFiles
    extraFile.m2
  Acknowledgement
    funded by NSF DMS-1234567
  Contributors
    People who helped
  References
    @arXiv "2401.12345"@
    @stacksProject("0123", "tag-0123")@
    @wikipedia "Frobenius_endomorphism"@
  Caveat
    Edge-case warnings
  SeeAlso
    bar
    (baz, ZZ)
  Subnodes
    :a sub-heading
    childNode
///
```

Every keyword (`Node`, `Key`, `Headline`, `Usage`, `Inputs`,
`Outputs`, `Consequences/Item`, `Description/Text/Example/Tree/Code/Pre/CannedExample`,
`ExampleFiles`, `Acknowledgement`, `Contributors`, `References`,
`Caveat`, `SeeAlso`, `Subnodes`) corresponds to a **typed
doc-node** in `m2/document.m2`. SimpleDoc's job is just to parse
the indented form into that AST.

The **`@TO foo@`** inline syntax is parsed by SimpleDoc into a
typed `TO` link node that the doc-system will later validate
(broken links are a build error). Similarly `@arXiv "..."@`,
`@TT "..."@`, `@EM "..."@`, etc. become typed nodes.

## How parsing works

Inside `doc`:

1. **Scan the string line-by-line**, tracking indentation.
2. **Match keywords** at the current indent against a known set
   (`Node`, `Key`, `Headline`, …).
3. **Nest**: each more-indented block becomes children of the
   parent keyword.
4. **Emit `Node` objects** (a private `IntermediateMarkUpType of
   Hypertext`) for each parsed section.
5. **Hand off to `document.m2`**: `Node` objects are converted
   into the typed doc nodes that the rest of the doc system
   processes.

Line numbers are tracked in `topLinenum` for error messages — when
SimpleDoc fails it reports the line within the triple-quoted
string, not the line in the `.m2` file, which is why error
messages start with `currentString:1:0`. The header comment notes
two specific edge cases (typos under `SubNodes`, broken `@TO@`
links) where the error location is less helpful than it should be
— known TODOs.

## `multidoc` vs `doc`

In current SimpleDoc both names point at the same method:

```m2
multidoc = doc
```

The distinction is historical — older code passed a multi-`Node`
string to `multidoc` and a single-`Node` string to `doc`. Today
both parse any number of `Node` blocks. Use whichever name fits
the reader.

## Helper hypertext functions (`arXiv`, `stacksProject`, `wikipedia`)

These produce ready-made `HREF` nodes. Use them inside `Text` or
`References` sections:

```m2
References
  @arXiv("2401.12345", "Smith and Jones 2024")@
  @wikipedia "Cohen-Macaulay_ring"@
  @stacksProject("00FE", "tag 00FE")@
```

The URL formatting is centralised so a future change to e.g. the
arXiv URL scheme can be made in one place.

## `packageTemplate`

Emits a starter package skeleton — useful when bootstrapping a new
package:

```m2
print packageTemplate "MyNewPackage"
```

Produces the full `newPackage(...)`, `export {}`,
`beginDocumentation()`, `doc ///...///`, and `TEST ///...///`
boilerplate (template lives in `SimpleDoc/templates.m2`).

## Auxiliary files

| File | Role |
|---|---|
| `templates.m2` | String constants: `docTemplate`, `packagetemplate`, `testtemplate` |
| `helpers.m2` | The `arXiv` / `stacksProject` / `wikipedia` `HREF` generators |
| `example.m2` | Worked example showing `doc` use end-to-end |
| `TestSimpleDoc.m2` | Test suite for `check "SimpleDoc"` (392 lines) |

## Boundary with `m2/document.m2`

| Layer | Role | Lives in |
|---|---|---|
| **DSL** | `doc /// … ///` indented string syntax | `SimpleDoc.m2` (this package) |
| **Typed nodes** | `Key`, `Headline`, `Description`, `Example`, `SeeAlso`, … as classes | `m2/document.m2` |
| **Validation** | Cross-reference check, missing-key detection | `m2/document.m2` |
| **Execution** | `Example` blocks run as M2 code, output captured | `m2/examples.m2`, `m2/installPackage.m2` |
| **Storage** | Compiled into a GDBM `*.db` file | `installPackage` flow |
| **Rendering** | HTML / info / in-session `help` output | `m2/html.m2`, `m2/info.m2`, `m2/help.m2` |

Editing the DSL keyword set means editing this file. Editing what
those keywords compile to means editing `m2/document.m2`. Editing
how they render means editing `m2/html.m2` etc.

## See also

- [Repo `DOCUMENTATION-SYSTEM.md`](../../../DOCUMENTATION-SYSTEM.md) — the end-to-end doc pipeline
- [`file-Macaulay2Doc.md`](file-Macaulay2Doc.md) — biggest consumer (the main M2 reference)
- [`file-Style.md`](file-Style.md) — companion package: doc styling + grammar generation
- [`file-package-conventions.md`](file-package-conventions.md) — package conventions including doc style
- M2-level doc machinery: [`m2/file-document.md`](../m2/file-document.md), [`m2/file-help.md`](../m2/file-help.md), [`m2/file-html.md`](../m2/file-html.md), [`m2/file-examples.md`](../m2/file-examples.md)
- [Repo `PACKAGES.md`](../../../PACKAGES.md) — package ecosystem reference
- [`Text.m2`](Text.m2) — imported by this package for hypertext primitives
