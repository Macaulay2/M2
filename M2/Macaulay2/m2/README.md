# `M2/Macaulay2/m2/` — Core M2 source

The ~100 `.m2` files in this directory are loaded at startup by the
`M2-interpreter` binary and together define the **Core package** — everything
you get in a fresh `M2` session before any `needsPackage`.

Position in the [four-language stack](../../../README.md#the-four-language-stack):

```
.d / .dd  ──▶  .c / .cpp  ──▶  M2-interpreter ──▶  M2 (loads m2/*.m2 here)
                                     ▲
                                     │ linked
                                 M2-engine (C++)
                                          [Macaulay2/m2/]   ← you are here
```

The exact file load order is governed by the [`loadsequence`](loadsequence)
file. Editing anything here requires rebuilding the `M2-core` CMake target (or
the equivalent autotools target) so the bundled startup script is regenerated.

## File groups

### Bootstrap and infrastructure

| File | Role |
|---|---|
| `Core.m2` | The first user-visible package — defines `Core`, configures globals |
| `startup.m2.in` | Template for the bundled startup script consumed by `M2-interpreter` |
| `version.m2.in` | Templated version constants |
| `loadsequence` | The authoritative list of `.m2` files to load, in order |
| `exports.m2` | Public symbol export list for the Core package |
| `autoload.m2` | Lazy-loading machinery |
| `last.m2` | Last file loaded; final wiring |
| `shared.m2` | Definitions used by many files; loaded early |
| `system.m2`, `run.m2`, `files.m2` | OS interaction and process control |

### Language extension and method dispatch

| File | Role |
|---|---|
| `classes.m2` | Type hierarchy at the M2 level |
| `methods.m2`, `option.m2`, `typicalvalues.m2` | Method dispatch, optional arguments, return-type hints |
| `gateway.m2` | Glue between M2-level types and engine-level types |
| `expressions.m2`, `printing.m2`, `pretty.m2`, `format.m2`, `nets.m2` | Pretty-printing pipeline |
| `code.m2`, `peek.m2`, `debugging.m2` | Introspection / debugging |
| `validate.m2`, `testing.m2` | Assertion and test framework |
| `robust.m2` | Wrapping operations to survive bad input |
| `iterators.m2`, `fold.m2`, `lists.m2`, `set.m2`, `combinatorics.m2`, `max.m2` | Iteration / functional / set primitives |
| `regex.m2`, `dotdot.m2`, `remember.m2`, `profile.m2` | Various language helpers |

### Mathematical objects

| File | Role |
|---|---|
| `rings.m2`, `polyrings.m2`, `quotring.m2`, `localring.m2`, `enginering.m2`, `newring.m2`, `quotient.m2`, `galois.m2`, `freealgebras.m2` | Ring constructors and views |
| `monoids.m2`, `indeterminates.m2`, `variables.m2`, `powers.m2` | Monoids, generators, monomials |
| `integers.m2`, `rationals.m2`, `reals.m2`, `intervals.m2`, `flint.m2` | Concrete coefficient rings and bindings |
| `matrix.m2`, `matrix1.m2`, `matrix2.m2`, `genmat.m2`, `mutablemat.m2` | Matrix machinery (split across files for load order) |
| `modules.m2`, `modules2.m2`, `multilin.m2` | Free / quotient module support |
| `monideal.m2` | Monomial ideals |
| `ringmap.m2` | Ring maps |
| `Hom.m2`, `intersect.m2`, `content.m2`, `minPres.m2`, `pushforward.m2` | Standard categorical operations |
| `factor.m2`, `integrate.m2` | Factorization, symbolic integration |
| `gb.m2`, `hilbert.m2`, `betti.m2`, `complexes.m2`, `computations.m2`, `basis.m2`, `local.m2`, `schubert.m2`, `fano.m2`, `monomcurve.m2` | Gröbner bases, Hilbert functions, Betti numbers, resolutions, applications |
| `engine.m2` | The wrapper over the engine's C API for use from `.m2` code |

### Documentation, packages, distribution

| File | Role |
|---|---|
| `document.m2` | The Macaulay2 documentation system (`Node`, `Item`, …) |
| `installPackage.m2` | Builds package HTML/info/examples |
| `help.m2` | Interactive help |
| `examples.m2` | Example execution / capture |
| `hypertext.m2`, `html.m2`, `latex.m2`, `mathml.m2`, `texmacs.m2`, `markdown.m2`, `book.m2` | Output formatters |
| `packages.m2` | Package loader, manifest |
| `programs.m2` | External program detection |
| `webapp.m2`, `http.m2` | Browser front-end glue |

### Threads, miscellaneous

| File | Role |
|---|---|
| `threads.m2` | M2-level thread API |
| `obsolete.m2` | Deprecated symbol shims |
| `basictests` | Sanity-check script run during build |

### Build glue

| File | Role |
|---|---|
| `CMakeLists.txt`, `Makefile.in` | Drive copying / installation of these files into the build tree |

## Workflow tips

- Iterate inside a running M2 with `loadPackage("Foo", Reload => true)` — much
  faster than `installPackage` if you don't need doc rebuilds.
- Editing files here triggers a Core rebuild but no recompilation of `.d`/`e/`
  code, so the turnaround is usually fast.
- The order in [`loadsequence`](loadsequence) matters; new files must be
  inserted where their dependencies are already satisfied.

## Related

- [`Macaulay2/d/`](../d/README.md) — the interpreter that loads these files.
- [`Macaulay2/e/`](../e/README.md) — the engine these files reach into.
- [`Macaulay2/packages/`](../packages/README.md) — additional packages, loaded
  on demand rather than at startup.

[← back to repository TOC](../../../README.md#under-m2macaulay2)
