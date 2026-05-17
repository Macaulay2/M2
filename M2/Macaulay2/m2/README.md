# `M2/Macaulay2/m2/` — Core M2 source

The ~100 `.m2` files in this directory are loaded at startup by the
`M2-interpreter` binary and together define the **Core package** —
everything you get in a fresh `M2` session before any `needsPackage`.

Position in the [four-language stack](../../../README.md#the-four-language-stack):

```
.d / .dd  ──▶  .c / .cpp  ──▶  M2-interpreter ──▶  M2 (loads m2/*.m2 here)
                                     ▲
                                     │ linked
                                 M2-engine (C++)
                                          [Macaulay2/m2/]   ← you are here
```

The exact file load order is governed by the
[`loadsequence`](file-loadsequence.md) file. Editing anything here
requires rebuilding the `M2-core` CMake target (or the equivalent
autotools target) so the bundled startup script is regenerated.

## Per-file deep dives

Documented Core files (the rest will be added incrementally):

### Bootstrap and load order

| File doc | Subject |
|---|---|
| [`file-Core.md`](file-Core.md) | `Core.m2` — Core package definition |
| [`file-loadsequence.md`](file-loadsequence.md) | `loadsequence` — load order manifest |

### Type system and dispatch

| File doc | Subject |
|---|---|
| [`file-classes.md`](file-classes.md) | `classes.m2` — M2 type hierarchy |
| [`file-methods.md`](file-methods.md) | `methods.m2` — method dispatch |
| [`file-expressions.md`](file-expressions.md) | `expressions.m2` — `Expression` AST + precedence |

### Rings & ideals

| File doc | Subject |
|---|---|
| [`file-polyrings.md`](file-polyrings.md) | `polyrings.m2` — `PolynomialRing` type |
| [`file-gb.md`](file-gb.md) | `gb.m2` — M2-side GB front-end |

### Documentation and help

| File doc | Subject |
|---|---|
| [`file-document.md`](file-document.md) | `document.m2` — docs DSL |
| [`file-installPackage.md`](file-installPackage.md) | `installPackage.m2` — render to HTML / info / PDF |
| [`file-help.md`](file-help.md) | `help.m2` — interactive help system |

## File groups (overview)

The 100+ files fall into broad categories. The
[engine-side overview](../e/README.md) describes the partnership
between Core M2 and the engine; this directory's
[`README` (original)](../../../README.md#under-m2macaulay2) lists the
groups. A summary:

| Group | Representative files |
|---|---|
| Bootstrap / infrastructure | `Core.m2`, `startup.m2.in`, `version.m2.in`, `loadsequence`, `exports.m2`, `autoload.m2`, `last.m2`, `shared.m2`, `system.m2`, `run.m2`, `files.m2` |
| Language extension / dispatch | `classes.m2`, `methods.m2`, `option.m2`, `typicalvalues.m2`, `gateway.m2`, `expressions.m2`, `printing.m2`, `pretty.m2`, `format.m2`, `nets.m2`, `code.m2`, `peek.m2`, `debugging.m2`, `validate.m2`, `testing.m2`, `robust.m2`, `iterators.m2`, `fold.m2`, `lists.m2`, `set.m2`, `combinatorics.m2`, `max.m2`, `regex.m2`, `dotdot.m2`, `remember.m2`, `profile.m2` |
| Mathematical objects | `rings.m2`, `polyrings.m2`, `quotring.m2`, `localring.m2`, `enginering.m2`, `newring.m2`, `quotient.m2`, `galois.m2`, `freealgebras.m2`, `monoids.m2`, `indeterminates.m2`, `variables.m2`, `powers.m2`, `integers.m2`, `rationals.m2`, `reals.m2`, `intervals.m2`, `flint.m2`, `matrix.m2`, `matrix1.m2`, `matrix2.m2`, `genmat.m2`, `mutablemat.m2`, `modules.m2`, `modules2.m2`, `multilin.m2`, `monideal.m2`, `ringmap.m2`, `Hom.m2`, `intersect.m2`, `content.m2`, `minPres.m2`, `pushforward.m2`, `factor.m2`, `integrate.m2`, `gb.m2`, `hilbert.m2`, `betti.m2`, `complexes.m2`, `computations.m2`, `basis.m2`, `local.m2`, `schubert.m2`, `fano.m2`, `monomcurve.m2`, `engine.m2` |
| Docs / packages / output | `document.m2`, `installPackage.m2`, `help.m2`, `examples.m2`, `hypertext.m2`, `html.m2`, `latex.m2`, `mathml.m2`, `texmacs.m2`, `markdown.m2`, `book.m2`, `packages.m2`, `programs.m2`, `webapp.m2`, `http.m2` |
| Threads / misc | `threads.m2`, `obsolete.m2`, `basictests` |
| Build glue | `CMakeLists.txt`, `Makefile.in` |

## Workflow tips

- Iterate inside a running M2 with `loadPackage("Foo", Reload => true)`
  — much faster than `installPackage` if you don't need doc rebuilds.
- Editing files here triggers a Core rebuild but no recompilation of
  `.d`/`e/` code, so the turnaround is usually fast.
- The order in [`loadsequence`](file-loadsequence.md) matters; new
  files must be inserted where their dependencies are already
  satisfied.

## Related

- [`Macaulay2/d/`](../d/README.md) — the interpreter that loads these
  files.
- [`Macaulay2/e/`](../e/README.md) — the engine these files reach into.
- [`Macaulay2/packages/`](../packages/README.md) — additional
  packages, loaded on demand rather than at startup.

[← back to repository TOC](../../../README.md#under-m2macaulay2)
