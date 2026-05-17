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
| [`file-exports.md`](file-exports.md) | `exports.m2` — public-symbol manifest |
| [`file-packages.md`](file-packages.md) | `packages.m2` — `newPackage` / `needsPackage` machinery |
| [`file-autoload.md`](file-autoload.md) | `autoload.m2` — lazy symbol loading |
| [`file-shared.md`](file-shared.md) | `shared.m2` — method-stub manifest |
| [`file-typicalvalues.md`](file-typicalvalues.md) | `typicalvalues.m2` — return-type hints |

### Type system and dispatch

| File doc | Subject |
|---|---|
| [`file-classes.md`](file-classes.md) | `classes.m2` — M2 type hierarchy |
| [`file-methods.md`](file-methods.md) | `methods.m2` — method dispatch |
| [`file-expressions.md`](file-expressions.md) | `expressions.m2` — `Expression` AST + precedence |
| [`file-option.md`](file-option.md) | `option.m2` — `Option` / `OptionTable` |
| [`file-gateway.md`](file-gateway.md) | `gateway.m2` — `ScriptedFunctor` |
| [`file-code.md`](file-code.md) | `code.m2` — code introspection |
| [`file-debugging.md`](file-debugging.md) | `debugging.m2` — warnings + interactive debugger |
| [`file-remember.md`](file-remember.md) | `remember.m2` — `memoize` |

### Numeric types and output pipeline

| File doc | Subject |
|---|---|
| [`file-integers.md`](file-integers.md) | `integers.m2` — `Number` + `ZZ` |
| [`file-rationals.md`](file-rationals.md) | `rationals.m2` — `QQ` |
| [`file-reals.md`](file-reals.md) | `reals.m2` — `RR`, `CC`, intervals, `ImmutableType` |
| [`file-nets.md`](file-nets.md) | `nets.m2` — 2-D character grids |
| [`file-printing.md`](file-printing.md) | `printing.m2` — `pad` and printing primitives |
| [`file-html.md`](file-html.md) | `html.m2` — HTML output formatter |

### System integration

| File doc | Subject |
|---|---|
| [`file-files.md`](file-files.md) | `files.m2` — file / directory operations |
| [`file-system.md`](file-system.md) | `system.m2` — system commands / external processes |

### Rings & ideals

| File doc | Subject |
|---|---|
| [`file-rings.md`](file-rings.md) | `rings.m2` — `Ring` abstract base |
| [`file-enginering.md`](file-enginering.md) | `enginering.m2` — `EngineRing` + `RingElement` |
| [`file-monoids.md`](file-monoids.md) | `monoids.m2` — `Monoid` type |
| [`file-polyrings.md`](file-polyrings.md) | `polyrings.m2` — `PolynomialRing` type |
| [`file-quotring.md`](file-quotring.md) | `quotring.m2` — `QuotientRing` (`R/I`) |
| [`file-matrix.md`](file-matrix.md) | `matrix.m2` — M2-side `Matrix` (basics) |
| [`file-matrix1.md`](file-matrix1.md) | `matrix1.m2` — `Ideal`, kernel, image, submatrix |
| [`file-matrix2.md`](file-matrix2.md) | `matrix2.m2` — LU, det, solve, rank |
| [`file-modules.md`](file-modules.md) | `modules.m2` — `Module` type |
| [`file-multilin.md`](file-multilin.md) | `multilin.m2` — `exteriorPower`, `minors`, `pfaffians` |
| [`file-minPres.md`](file-minPres.md) | `minPres.m2` — `minimalPresentation` |
| [`file-pushforward.md`](file-pushforward.md) | `pushforward.m2` — `pushForward` |
| [`file-quotient.md`](file-quotient.md) | `quotient.m2` — `I : J` ideal quotient |
| [`file-newring.md`](file-newring.md) | `newring.m2` — `flattenRing`, `tensor`, ring extensions |
| [`file-variables.md`](file-variables.md) | `variables.m2` — `IndexedVariable` |
| [`file-indeterminates.md`](file-indeterminates.md) | `indeterminates.m2` — variable-name generation |
| [`file-basis.md`](file-basis.md) | `basis.m2` — `basis(d, M)` |
| [`file-mutablemat.md`](file-mutablemat.md) | `mutablemat.m2` — `MutableMatrix` |
| [`file-genmat.md`](file-genmat.md) | `genmat.m2` — `genericMatrix` / `genericSymmetricMatrix` |
| [`file-computations.md`](file-computations.md) | `computations.m2` — `Computation` framework |
| [`file-gb.md`](file-gb.md) | `gb.m2` — M2-side GB front-end |
| [`file-hilbert.md`](file-hilbert.md) | `hilbert.m2` — Hilbert function / series / polynomial |
| [`file-betti.md`](file-betti.md) | `betti.m2` — `BettiTally` |
| [`file-complexes.md`](file-complexes.md) | `complexes.m2` — `Complexes` / `OldChainComplexes` dispatch |
| [`file-ringmap.md`](file-ringmap.md) | `ringmap.m2` — `RingMap` |
| [`file-monideal.md`](file-monideal.md) | `monideal.m2` — `MonomialIdeal` |
| [`file-Hom.md`](file-Hom.md) | `Hom.m2` — `Hom` / `Ext` / `Tor` |
| [`file-intersect.md`](file-intersect.md) | `intersect.m2` — generic `intersect` |
| [`file-factor.md`](file-factor.md) | `factor.m2` — polynomial / integer factorisation |
| [`file-localring.md`](file-localring.md) | `localring.m2` — `LocalRing` stub |
| [`file-galois.md`](file-galois.md) | `galois.m2` — `GaloisField` |
| [`file-freealgebras.md`](file-freealgebras.md) | `freealgebras.m2` — `FreeAlgebra` stubs |

### Documentation and help

| File doc | Subject |
|---|---|
| [`file-document.md`](file-document.md) | `document.m2` — docs DSL |
| [`file-installPackage.md`](file-installPackage.md) | `installPackage.m2` — render to HTML / info / PDF |
| [`file-examples.md`](file-examples.md) | `examples.m2` — example runner |
| [`file-help.md`](file-help.md) | `help.m2` — interactive help system |
| [`file-hypertext.md`](file-hypertext.md) | `hypertext.m2` — `Hypertext` AST |
| [`file-latex.md`](file-latex.md) | `latex.m2` — `tex` / `texMath` |
| [`file-markdown.md`](file-markdown.md) | `markdown.m2` — Markdown output |
| [`file-mathml.md`](file-mathml.md) | `mathml.m2` — MathML output |
| [`file-texmacs.md`](file-texmacs.md) | `texmacs.m2` — TeXmacs frontend protocol |
| [`file-format.md`](file-format.md) | `format.m2` — formatter dispatch + `info` / `net` |

### Language primitives

| File doc | Subject |
|---|---|
| [`file-lists.md`](file-lists.md) | `lists.m2` — `List` / `Sequence` / `Array` |
| [`file-set.md`](file-set.md) | `set.m2` — `Set` / `Tally` / `VirtualTally` |
| [`file-iterators.md`](file-iterators.md) | `iterators.m2` — `Iterator` |
| [`file-combinatorics.md`](file-combinatorics.md) | `combinatorics.m2` — `subsets`, `partitions`, … |
| [`file-regex.md`](file-regex.md) | `regex.m2` — regex operations |
| [`file-threads.md`](file-threads.md) | `threads.m2` — `AtomicInt` + `Task` |
| [`file-programs.md`](file-programs.md) | `programs.m2` — external programs |
| [`file-engine.md`](file-engine.md) | `engine.m2` — `Raw*` type wrappers |
| [`file-fold.md`](file-fold.md) | `fold.m2` — `accumulate` and `fold` |
| [`file-max.md`](file-max.md) | `max.m2` — `InfiniteNumber`, `max`, `min` |
| [`file-powers.md`](file-powers.md) | `powers.m2` — `binomial` |
| [`file-intervals.md`](file-intervals.md) | `intervals.m2` — interval constructors |
| [`file-flint.md`](file-flint.md) | `flint.m2` — `ZZFlintRing` / `QQFlintRing` |
| [`file-pretty.md`](file-pretty.md) | `pretty.m2` — pretty-printer |
| [`file-peek.md`](file-peek.md) | `peek.m2` — structural introspection |
| [`file-robust.md`](file-robust.md) | `robust.m2` — `timelimit` (legacy) |
| [`file-profile.md`](file-profile.md) | `profile.m2` — profiler reporting |
| [`file-validate.md`](file-validate.md) | `validate.m2` — `Hypertext` validation |
| [`file-testing.md`](file-testing.md) | `testing.m2` — `check Package`, `TEST` blocks |

### Mathematical helpers

| File doc | Subject |
|---|---|
| [`file-monomcurve.md`](file-monomcurve.md) | `monomcurve.m2` — `monomialCurveIdeal` |
| [`file-schubert.md`](file-schubert.md) | `schubert.m2` — Schubert calculus |
| [`file-fano.md`](file-fano.md) | `fano.m2` — Fano variety |
| [`file-integrate.md`](file-integrate.md) | `integrate.m2` — Simpson's rule |

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
