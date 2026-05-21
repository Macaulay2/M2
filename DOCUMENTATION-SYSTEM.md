# Documentation system

M2 has an elaborate **typed documentation system** that's worth
its own architectural reference. This doc is the end-to-end story
of how `doc ///...///` blocks become user-visible
`help`/`viewHelp`/HTML output. Complements
[`PACKAGES.md`](PACKAGES.md) (which mentions docs in passing) by
going deep on the doc machinery.

[← repository TOC](README.md) · [Glossary](GLOSSARY.md) · [Tour](TOUR.md) · [Packages](PACKAGES.md)

## The pipeline

```
doc ///...///  (in a .m2 file)
   ↓ parsed at installPackage time
typed M2 doc nodes (HashTables with Key, Headline, ...)
   ↓ Example blocks executed
captured outputs interleaved into nodes
   ↓ render
HTML pages   +   info database   +   in-session help cache
   ↓ used by
viewHelp     +   info Macaulay2   +   help / ? / about
```

Each transformation is its own M2 file. The whole machinery
lives in [`M2/Macaulay2/m2/`](M2/Macaulay2/m2/architecture.md) plus
the `Macaulay2Doc` and `Style` packages.

## Why typed docs

M2's docs are **not just text strings**. They're typed M2 values
that the system can manipulate, validate, and render multiple
ways. Benefits:

- **Cross-references** are validated. `SeeAlso => myFunction`
  errors if `myFunction` doesn't exist.
- **Examples are executed**. `Example` blocks run in real M2 and
  their output is captured.
- **Outputs match reality**. If an algorithm changes, the
  captured output changes with it — no stale docs.
- **Render targets are plural**: HTML, info, in-session text,
  manuals.
- **Search/indexing** works because nodes have structured `Key`
  fields.

The trade-off: doc installation is **slow** (every example
runs).

## The doc DSL

**Source**: [`m2/file-document.md`](M2/Macaulay2/m2/file-document.md).

Top-level syntax:

```m2
doc ///
Node
   Key
      myFunction
   Headline
      compute the foo of a bar
   Usage
      myFunction(x, y)
   Inputs
      x:Ring
         the base ring
      y:ZZ
         degree limit
   Outputs
      :Sequence
         a pair of matrices
   Description
      Text
         The myFunction computes ...
      Example
         R = QQ[x, y];
         myFunction(R, 3)
      CannedExample
         i1 : myFunction R
         o1 = ...
      Pre
         literal code block, not executed
      Code
         code block in M2 syntax, *executed at render time*
   Consequences
      Item
         a side effect of calling this function
   SeeAlso
      relatedFunction
      anotherFunction
   Subnodes
      childNode1
      childNode2
   Caveat
      a warning about edge cases
   Acknowledgement
      ...
///
```

Multiple `Node` blocks may share one `doc ///` block; conversely
one package may have many `doc` blocks.

## `Key` field — what gets documented

The `Key` field identifies what this doc node is about. Forms:

```
Key
   myFunction              -- the function itself
   (myFunction, Ring)      -- the method myFunction(Ring)
   (myFunction, Ring, ZZ)  -- the method myFunction(Ring, ZZ)
   [myFunction, Strategy]  -- the keyword arg Strategy
   "myFunction"            -- a string-keyed node (for narrative docs)
   MyClass                 -- a type
   MyClass := X            -- a method declaration
```

Different keys for different documented items. Method-key tuples
let M2 know which dispatch path each doc describes.

## `Example` execution

Inside an `Example` block:

```m2
Example
   R = QQ[x, y];
   I = ideal(x^2, y^2);
   gb I
```

This M2 code is **actually executed** when `installPackage` runs.
The captured output is stored in the doc node and rendered as:

```
i1 : R = QQ[x, y]

o1 = R

o1 : PolynomialRing

i2 : I = ideal(x^2, y^2)
...
```

Implementation: [`m2/file-examples.md`](M2/Macaulay2/m2/file-examples.md).
The example runner spawns a fresh M2 session per package (to
isolate state), runs each example, captures stdin/stdout/stderr,
and stores everything.

A failing example **fails the install** — typos in example code
become test failures. This catches doc rot.

## `CannedExample` vs `Example`

```
Example
   << M2 executes this and captures real output >>

CannedExample
   i1 : the output is taken literally
   o1 = ...
```

Use `Example` (default) — produces real, current output.

Use `CannedExample` only when:
- The example output isn't reproducible (random results, depends
  on environment).
- The example takes too long to run during install.
- The example would intentionally crash and you want to document
  that.

## Cross-references

```
SeeAlso
   relatedFunction
   anotherFunction
   (yetAnotherFunction, Ring)
   "narrative-node"
```

Each entry is **validated at install time**:

- Function references must resolve to a `Key` somewhere.
- Method-tuple references must resolve to a specific method.
- String references must match a `Key => "..."`.

Unresolved → install error.

Inside `Description Text`, inline references use:

```
The @TO myFunction@ function does ...
For background, see @TOH relatedFunction@.
```

| Markup | Renders as |
|---|---|
| `@TO foo@` | link to `foo`'s doc page |
| `@TOH foo@` | "foo" + headline + link |
| `@TT "literal"@` | typewriter font |
| `@EM "italic"@` | italic |
| `@BOLD "bold"@` | bold |
| `@TEX "$x^2+y^2$"@` | inline LaTeX |
| `@HREF{"url", "text"}@` | external link |

## `Subnodes` — building a tree

```
doc ///
Node
   Key
      Macaulay2Doc
   Subnodes
      :Introduction
      :Mathematical types
      :Programming basics
///

doc ///
Node
   Key
      "Introduction"
   Subnodes
      :Quick start
      :Basic syntax
///
```

`Subnodes` builds a tree of doc pages. The root is typically the
package name (`Macaulay2Doc`); subnodes are major topics; each
in turn has its own subnodes. The HTML rendering uses this tree
as the navigation sidebar.

## Render targets

### HTML

**Source**: [`m2/file-html.md`](M2/Macaulay2/m2/file-html.md).

Each doc node renders to one `.html` file. Cross-references
become `<a>` tags. Examples are styled with the
[`Style`](M2/Macaulay2/packages/file-Style.md) package's CSS.

The generated HTML is **self-contained** — no JavaScript
required, just CSS. Easy to host, easy to mirror, easy to read
offline.

Generated files land in
`${docdir}/Macaulay2/<PackageName>/html/*.html` after
`installPackage`.

### Info / texinfo

**Source**: same `installPackage` pipeline.

Doc nodes also render to texinfo (`.texi`) and then to GNU info
(`.info` files). Once installed, `info Macaulay2` opens the
M2 manual in the terminal info reader. The `dir` file
([`files/file-files-content.md`](M2/files/file-files-content.md))
registers M2 with the system info database.

### In-session help

When the user types `help foo` inside M2:

1. Lookup `foo` in the doc database (a GDBM file).
2. Render the doc node as terminal text (with `Net`-based
   formatting — see [`m2/file-nets.md`](M2/Macaulay2/m2/file-nets.md)).
3. Display.

The GDBM database is endian/word-size-aware:

```
lib/Macaulay2/<PackageName>/cache/rawdocumentation-<endian>-<wordsize>.db
```

See [`m2/file-installPackage.md`](M2/Macaulay2/m2/file-installPackage.md)
and [`file-Makefile-doc-dist.md`](M2/file-Makefile-doc-dist.md).

### Indices

`Macaulay2Doc/loads.m2` and other top-level package files build:

- Alphabetical function index.
- Type index.
- Keyword-grouped indexes (`Keywords => {"Algebra"}` on
  `newPackage` declarations).

Rendered as HTML pages users can navigate.

## `installPackage` workflow

**Source**:
[`m2/file-installPackage.md`](M2/Macaulay2/m2/file-installPackage.md).

```
installPackage "Foo"
   ↓
1. loadPackage "Foo"          -- reload code if changed
   ↓
2. processDocumentationBody    -- parse doc /// blocks into nodes
   ↓
3. checkCrossReferences        -- validate SeeAlso etc.
   ↓
4. runExamples                 -- execute Example blocks
   ↓
5. captureOutput               -- store outputs
   ↓
6. renderHTML                  -- generate HTML files
   ↓
7. buildInfoFile               -- generate texinfo + info
   ↓
8. buildDocDatabase            -- GDBM cache
   ↓
9. installFiles                -- copy to docdir
```

Each step can fail and report a specific error. Errors
typically include the source-file line where the doc block
lives.

## Speed and slow paths

`installPackage` is **slow**:

- Step 4 (run examples) — minutes to hours per package depending
  on size and example complexity.
- Step 6 (render HTML) — seconds for typical packages.
- Step 7 (info generation) — seconds.
- Step 8 (database build) — seconds.

For development iteration:

```m2
loadPackage("Foo", Reload => true)   -- no doc rebuild; ~1 second
```

Only re-runs Step 1. The price: docs are stale (showing the old
package's content).

```m2
installPackage("Foo", IgnoreExampleErrors => true)
```

Continues install even if example execution fails. Useful when
you're iterating on docs and don't want to fix examples yet.

## Symbol auto-completion: `editors/`

Beyond standalone HTML/info, M2's documentation system also
feeds **editor integration**:

**Source**: [`editors/file-make-M2-symbols.md`](M2/Macaulay2/editors/file-make-M2-symbols.md).

After Core M2 is loaded, the `make-M2-symbols.m2` script walks
every exported symbol and emits per-editor grammar files:

```
prism/macaulay2.js     -- for Prism (used on macaulay2.com)
pygments/macaulay2.py  -- for Pygments (used in Sphinx docs)
vim/m2.vim.syntax      -- Vim syntax
emacs/M2-symbols.el    -- Emacs symbol completion
```

These give editor users syntax highlighting + completion for M2
code. See
[`editors/file-subdirs.md`](M2/Macaulay2/editors/file-subdirs.md).

## Engine API docs (separate)

The engine has **its own documentation system**: Doxygen +
Sphinx for the C++ API.

**Source**: [`docs/file-Doxyfile-in.md`](M2/Macaulay2/docs/file-Doxyfile-in.md)
+ [`docs/file-conf-py.md`](M2/Macaulay2/docs/file-conf-py.md).

This is **separate from** the M2-level doc system:

| M2 doc system | Engine doc system |
|---|---|
| `doc ///...///` DSL | Doxygen `/** */` comments |
| Output: HTML / info | Output: HTML + cross-refs |
| Built by `installPackage` | Built by `cmake --target docs` |
| User-facing | Engine-developer-facing |
| Per-package | One unified site |

End users see only the M2 doc system. Engine developers can
opt-in to building the engine docs locally.

## Documentation contribution conventions

When writing or editing docs:

1. **Use `Example`, not `CannedExample`**, when output is
   reproducible. Reality stays in sync.
2. **`Key` precision** — use the most specific form.
   `(foo, Ring)` is better than `foo` if the doc is about that
   specific method.
3. **`Headline` is short**. One line, ~40 chars.
4. **`Description Text` is fully sentence-formatted**. The
   renderer can wrap; don't manually wrap mid-paragraph.
5. **Cross-reference liberally** — `SeeAlso`, `@TO@` inline.
   Validators catch typos.
6. **Test by installing the package**:
   ```m2
   installPackage("Foo", IgnoreExampleErrors => true)
   viewHelp Foo
   ```

## Common pitfalls

### `beginDocumentation()` placement

```m2
newPackage("Foo", ...)

-- code here loads at loadPackage time

beginDocumentation()

-- doc /// blocks here load at installPackage time
```

A `doc ///` before `beginDocumentation()` is silently ignored.
A code definition after `beginDocumentation()` doesn't run at
load time.

### Stale captured output

```m2
Example
   1 + 1
```

If the example output was captured years ago and now M2 prints
something different, install fails with a mismatch.

Fix: re-run `installPackage "Foo"` to recapture.

### `SeeAlso` typo

```
SeeAlso
   myFunctioin   -- typo
```

Install errors: "no documentation node found for `myFunctioin`."
Fix the typo.

### `@TO@` in code blocks

```m2
Example
   x = 1 -- @TO@ doesn't render inside Example
```

Markup only renders in `Description Text`, `Caveat`,
`Acknowledgement`, etc. Inside `Example`, all text is verbatim.

### Doc not visible after `loadPackage`

`loadPackage` skips doc parsing. To see docs you need
`installPackage` (or `help` falls back to a generic message).

## Used by

- Anyone writing M2 documentation (package authors).
- The `installPackage` machinery itself.
- The Macaulay2 doc site.
- Editor / IDE integrations.

## Related

- [`README.md`](README.md) — repository TOC.
- [`PACKAGES.md`](PACKAGES.md) — package ecosystem (this is a
  deep-dive on docs which packages produce).
- [`TESTING.md`](TESTING.md) — `Example` blocks are effectively
  tests.
- [`BUILD.md`](BUILD.md) — phase 7 (`install-packages`) does the
  doc build.
- [`M2/Macaulay2/m2/file-document.md`](M2/Macaulay2/m2/file-document.md)
  — DSL implementation.
- [`M2/Macaulay2/m2/file-installPackage.md`](M2/Macaulay2/m2/file-installPackage.md)
  — installation pipeline.
- [`M2/Macaulay2/m2/file-examples.md`](M2/Macaulay2/m2/file-examples.md)
  — example runner.
- [`M2/Macaulay2/m2/file-html.md`](M2/Macaulay2/m2/file-html.md)
  — HTML renderer.
- [`M2/Macaulay2/m2/file-help.md`](M2/Macaulay2/m2/file-help.md)
  — `help` / `viewHelp` / `?` machinery.
- [`M2/Macaulay2/packages/file-Macaulay2Doc.md`](M2/Macaulay2/packages/file-Macaulay2Doc.md)
  — the main user-doc package.
- [`M2/Macaulay2/packages/file-Style.md`](M2/Macaulay2/packages/file-Style.md)
  — doc styling + `generateGrammar`.
- [`M2/Macaulay2/editors/file-make-M2-symbols.md`](M2/Macaulay2/editors/file-make-M2-symbols.md)
  — editor grammar generation.
- [`M2/Macaulay2/docs/`](M2/Macaulay2/docs/README.md) — separate
  engine API docs.
