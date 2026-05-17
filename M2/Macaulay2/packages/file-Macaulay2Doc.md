# `Macaulay2Doc.m2` + `Macaulay2Doc/` — main user documentation

The `Macaulay2Doc` package holds **the user-facing documentation
of every Core feature** — the `viewHelp`/`help`/`?` pages users
see for built-in types, functions, operators, and method
hierarchies. The largest package shipped with M2 by file count.

Part of [`packages/`](README.md).

[← packages/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```m2
newPackage ("Macaulay2Doc",
    Version => version#"VERSION",
    Headline => "Macaulay2 documentation",
    HomePage => "https://macaulay2.com/",
    Authors => { -* see the contributors listed on the main page *- },
    Keywords => { "Documentation" },
    InfoDirSection => "Macaulay2 and its packages",
    AuxiliaryFiles => true
)

-- a local way to use private global symbols from Core
core = nm -> value Core#"private dictionary"#nm
isMissingDoc = core "isMissingDoc"
isUndocumented = core "isUndocumented"

beginDocumentation()

-- move undocumented nodes from Core here
scan(keys Core#"raw documentation", key ->
    Macaulay2Doc#"raw documentation"#key =
    remove(Core#"raw documentation", key))

-- load the full documentation
load "./Macaulay2Doc/loads.m2"
```

The header pattern:

1. Standard `newPackage` declaration.
2. **`AuxiliaryFiles => true`** — auxiliary files live in the
   `Macaulay2Doc/` subdir.
3. **Private-symbol access** — `core` lambda fetches private
   helpers from Core's "private dictionary."
4. **`beginDocumentation()`** — switch to doc-loading mode.
5. **Migrate raw docs from Core** — Core has stub doc nodes; this
   package takes ownership.
6. **`load "./Macaulay2Doc/loads.m2"`** — load the actual doc
   files from the auxiliary dir.

## The version-link to Core

```m2
Version => version#"VERSION"
```

The version isn't a literal string — it's pulled from the M2
binary's own `version` hashtable. This means `Macaulay2Doc` always
matches the binary's version exactly; no version-skew problems.

## `Macaulay2Doc/loads.m2`

The auxiliary subdir contains hundreds of `.m2` files, each
documenting one cluster of Core functionality. `loads.m2` is the
manifest pulling them all in:

```m2
load "operators.m2"
load "monoids.m2"
load "matrices.m2"
load "modules.m2"
load "rings.m2"
load "groebner.m2"
load "free-resolutions.m2"
load "hashtables.m2"
load "strings.m2"
...
```

Each file uses M2's documentation DSL
([`../m2/file-document.md`](../m2/file-document.md)):

```m2
doc ///
Key
   Module
Headline
   the class of finitely-presented modules
Description
  Text
    A Module is ...
  Example
    M = R^3
    presentation M
SeeAlso
   FreeModule
   Matrix
///
```

## Why so much code, not just data?

Doc nodes need to be **typed (and tested) M2 values**, not just
strings:

- Cross-references are validated — `SeeAlso => Module` errors if
  `Module` doesn't exist.
- Examples are executed during `installPackage` — typos become
  test failures, not silent rot.
- Type-binding ensures doc nodes attach to the right symbols
  (function vs symbol vs option).

## Why a separate package, not Core?

Three reasons:

1. **Build time** — Core must be small (loads at startup). Docs
   are huge.
2. **Iteration** — `installPackage "Macaulay2Doc"` rebuilds docs
   without rebuilding Core.
3. **Modularity** — users can opt out (`M2 --no-doc`) if they
   don't need docs in a script.

## End-of-file sanity check

```m2
erase \ { symbol core, symbol isMissingDoc, symbol isUndocumented }
if keys Macaulay2Doc#"private dictionary" =!= {}
then error splice (
```

Cleans up the private helpers, then asserts that the **private
dictionary** is empty — i.e., no symbols leaked into the package
namespace during loading. This catches accidental `private`
declarations in doc nodes.

## Used by

- Every M2 user — `help foo`, `?foo`, `viewHelp` all pull from
  here.
- `installPackage "Macaulay2Doc"` regenerates the HTML/info
  databases.

## Related

- [`README.md`](README.md) — packages/ overview.
- [`../m2/file-document.md`](../m2/file-document.md) — the
  documentation DSL.
- [`../m2/file-installPackage.md`](../m2/file-installPackage.md)
  — what runs the docs build.
- [`file-Style.md`](file-Style.md) — sister package for visual
  styling.
- [`../../docs/README.md`](../../docs/README.md) — separate
  developer-facing Sphinx docs.
