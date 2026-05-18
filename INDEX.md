# Documentation index

Flat alphabetical catalogue of **every documentation file** in
the M2 source tree. Counts: 19 top-level meta docs + 70
per-directory READMEs + 13 architecture references + 10 engine
area docs + 489 per-file deep dives = **~601 total docs**.

This file is the **"I remember a doc about X but not where it
lives"** lookup. The [`README.md`](README.md) is the
hierarchical view; this is the flat-by-name view.

[← repository TOC](README.md) · [Glossary](GLOSSARY.md) · [Tour](TOUR.md)

## How to read this index

- **Top-level meta docs** are in `/home/ubuntu/M2/` (the repo
  root).
- **Per-directory `README.md`** is at `<dir>/README.md`.
- **Architecture refs** are at `<dir>/architecture.md`.
- **Engine area docs** are in `M2/Macaulay2/e/<area>.md`.
- **Per-file deep dives** are at `<dir>/file-<basename>.md`
  alongside their source files.

## Top-level meta docs (16)

| Doc | Subject |
|---|---|
| [`BUILD.md`](BUILD.md) | End-to-end build pipeline (cmake → M2-binary) |
| [`CHEATSHEET.md`](CHEATSHEET.md) | One-page quick-reference card: build/test/debug/find/add commands |
| [`COMPUTATIONS.md`](COMPUTATIONS.md) | Catalogue of every computation engine (GB / resolution / Hilbert / LLL / NAG / factoring / …) |
| [`CONTRIBUTING-DOCS.md`](CONTRIBUTING-DOCS.md) | Doc-tree conventions and link-integrity audit |
| [`DEBUG.md`](DEBUG.md) | Practical debugging reference (symptom-driven recipes) |
| [`DEPENDENCIES.md`](DEPENDENCIES.md) | External-library catalogue (required vs optional, versions, licenses) |
| [`DOCUMENTATION-SYSTEM.md`](DOCUMENTATION-SYSTEM.md) | The `doc ///...///` DSL pipeline end-to-end |
| [`GLOSSARY.md`](GLOSSARY.md) | M2-specific terminology dictionary |
| [`INDEX.md`](INDEX.md) | This file — flat doc index |
| [`MEMORY.md`](MEMORY.md) | Five-layer memory model (Boehm GC → pools → externals → overflow) |
| [`PACKAGES.md`](PACKAGES.md) | Package ecosystem reference |
| [`README.md`](README.md) | Repository TOC and architectural overview |
| [`RING-ZOO.md`](RING-ZOO.md) | Catalogue of every ring M2 supports |
| [`SYMBOLS.md`](SYMBOLS.md) | Symbol-to-doc reverse index: engine class name or M2 function name → source file + deep-dive doc |
| [`STARTUP.md`](STARTUP.md) | End-to-end boot path (`main()` → ready prompt) |
| [`STYLE.md`](STYLE.md) | Code style guide across the four layers |
| [`TESTING.md`](TESTING.md) | Six test infrastructures unified |
| [`THREADING.md`](THREADING.md) | Supervisor + TBB + per-thread state |
| [`TOUR.md`](TOUR.md) | Audience-specific reading orders |

## Architecture references (13)

| Doc | Subsystem |
|---|---|
| [`M2/Macaulay2/c/architecture.md`](M2/Macaulay2/c/architecture.md) | The `scc1` translator |
| [`M2/Macaulay2/d/architecture.md`](M2/Macaulay2/d/architecture.md) | The interpreter |
| [`M2/Macaulay2/e/architecture.md`](M2/Macaulay2/e/architecture.md) | The engine |
| [`M2/Macaulay2/e/NCAlgebras/architecture.md`](M2/Macaulay2/e/NCAlgebras/architecture.md) | Non-commutative algebras |
| [`M2/Macaulay2/e/NCResolutions/architecture.md`](M2/Macaulay2/e/NCResolutions/architecture.md) | NC free resolutions |
| [`M2/Macaulay2/e/bibasis/architecture.md`](M2/Macaulay2/e/bibasis/architecture.md) | Boolean involutive bases |
| [`M2/Macaulay2/e/f4/architecture.md`](M2/Macaulay2/e/f4/architecture.md) | Original F4 GB engine |
| [`M2/Macaulay2/e/gb-f4/architecture.md`](M2/Macaulay2/e/gb-f4/architecture.md) | Refactored F4 GB engine |
| [`M2/Macaulay2/e/interface/architecture.md`](M2/Macaulay2/e/interface/architecture.md) | Public C interface |
| [`M2/Macaulay2/e/schreyer-resolution/architecture.md`](M2/Macaulay2/e/schreyer-resolution/architecture.md) | Modern Schreyer-frame resolution |
| [`M2/Macaulay2/e/unit-tests/architecture.md`](M2/Macaulay2/e/unit-tests/architecture.md) | Engine gtest suite |
| [`M2/Macaulay2/m2/architecture.md`](M2/Macaulay2/m2/architecture.md) | Core M2 layer |
| [`M2/Macaulay2/system/architecture.md`](M2/Macaulay2/system/architecture.md) | Thread supervisor |

## Engine area docs (10)

| Doc | Area |
|---|---|
| [`M2/Macaulay2/e/coefficient-rings.md`](M2/Macaulay2/e/coefficient-rings.md) | Coefficient rings (`aring-*`) |
| [`M2/Macaulay2/e/computations.md`](M2/Macaulay2/e/computations.md) | Hilbert, LLL, NAG, SLP, etc. |
| [`M2/Macaulay2/e/free-modules.md`](M2/Macaulay2/e/free-modules.md) | Free modules + Schreyer orders |
| [`M2/Macaulay2/e/groebner-bases.md`](M2/Macaulay2/e/groebner-bases.md) | GB engines + GBRing |
| [`M2/Macaulay2/e/matrices.md`](M2/Macaulay2/e/matrices.md) | Matrices: dense, sparse, mutable |
| [`M2/Macaulay2/e/monoids-and-monomials.md`](M2/Macaulay2/e/monoids-and-monomials.md) | Monoids, monomial orderings, exponent encoding |
| [`M2/Macaulay2/e/polynomial-rings.md`](M2/Macaulay2/e/polynomial-rings.md) | Polynomial rings + variants |
| [`M2/Macaulay2/e/resolutions.md`](M2/Macaulay2/e/resolutions.md) | Free resolution engines |
| [`M2/Macaulay2/e/ring-elements-and-maps.md`](M2/Macaulay2/e/ring-elements-and-maps.md) | `ring_elem` / `ElementType` / `RingMap` |
| [`M2/Macaulay2/e/utilities.md`](M2/Macaulay2/e/utilities.md) | Allocators, buffers, error, overflow, TBB |

## Per-directory READMEs (70)

### Source-tree READMEs

```
M2/README.md
M2/BUILD/README.md
M2/BUILD/docker/README.md
M2/BUILD/rpm/README.md
M2/check-configure/README.md
M2/cmake/README.md
M2/distributions/README.md
M2/distributions/dmg/README.md
M2/distributions/freebsd/README.md
M2/distributions/install/README.md
M2/distributions/tar/README.md
M2/distributions/top/README.md
M2/files/README.md
M2/include/README.md
M2/libraries/README.md
M2/m4/README.md
M2/Macaulay2/README.md
M2/Macaulay2/bin/README.md
M2/Macaulay2/c/README.md
M2/Macaulay2/d/README.md
M2/Macaulay2/docs/README.md
M2/Macaulay2/editors/README.md
M2/Macaulay2/editors/emacs/README.md
M2/Macaulay2/editors/prism/README.md
M2/Macaulay2/editors/pygments/README.md
M2/Macaulay2/editors/vim/README.md
M2/Macaulay2/e/README.md
M2/Macaulay2/e/NCAlgebras/README.md
M2/Macaulay2/e/NCResolutions/README.md
M2/Macaulay2/e/bibasis/README.md
M2/Macaulay2/e/doxygen-settings/README.md
M2/Macaulay2/e/f4/README.md
M2/Macaulay2/e/gb-f4/README.md
M2/Macaulay2/e/interface/README.md
M2/Macaulay2/e/schreyer-resolution/README.md
M2/Macaulay2/e/unit-tests/README.md
M2/Macaulay2/html-check-links/README.md
M2/Macaulay2/m2/README.md
M2/Macaulay2/man/README.md
M2/Macaulay2/packages/README.md
M2/Macaulay2/system/README.md
M2/Macaulay2/tests/README.md
M2/Macaulay2/tests/ComputationsBook/README.md
M2/Macaulay2/tests/engine/README.md
M2/Macaulay2/tests/gigantic/README.md
M2/Macaulay2/tests/goals/README.md
M2/Macaulay2/tests/normal/README.md
M2/Macaulay2/tests/quarantine/README.md
M2/Macaulay2/tests/rationality/README.md
M2/Macaulay2/tests/slow/README.md
M2/Macaulay2/tests/threads/README.md
M2/submodules/README.md
```

Each describes the contents of its directory and links into the
per-file deep dives.

## Per-file deep dives (483)

Per-file docs are at `<dir>/file-<basename>.md`. To find one,
follow the chain:

1. Identify the source file you care about (e.g.,
   `M2/Macaulay2/e/matrix.cpp`).
2. Look in the same directory for `file-<basename>.md` —
   `M2/Macaulay2/e/file-matrix.md` in this case.
3. If the file is part of a consolidated family (e.g., all
   `dmat-lu-*.hpp`), look for the family doc instead:
   `file-dmat-lu-variants.md`.

The full enumeration is impractical to list here (483 entries).
Each subdirectory's `README.md` indexes its own per-file docs.

### Per-directory per-file index pointers

The per-directory READMEs link to every per-file deep dive they
own. Quick links:

- Translator deep dives: [`M2/Macaulay2/c/README.md`](M2/Macaulay2/c/README.md)
- Interpreter deep dives: [`M2/Macaulay2/d/README.md`](M2/Macaulay2/d/README.md)
- Engine deep dives (147+): [`M2/Macaulay2/e/README.md`](M2/Macaulay2/e/README.md)
- Engine `interface/` deep dives: [`M2/Macaulay2/e/interface/README.md`](M2/Macaulay2/e/interface/README.md)
- Engine `f4/` deep dives: [`M2/Macaulay2/e/f4/README.md`](M2/Macaulay2/e/f4/README.md)
- Engine `gb-f4/` deep dives: [`M2/Macaulay2/e/gb-f4/README.md`](M2/Macaulay2/e/gb-f4/README.md)
- Engine `schreyer-resolution/` deep dives: [`M2/Macaulay2/e/schreyer-resolution/README.md`](M2/Macaulay2/e/schreyer-resolution/README.md)
- Engine `NCAlgebras/` deep dives: [`M2/Macaulay2/e/NCAlgebras/README.md`](M2/Macaulay2/e/NCAlgebras/README.md)
- Engine `bibasis/` deep dives: [`M2/Macaulay2/e/bibasis/README.md`](M2/Macaulay2/e/bibasis/README.md)
- Engine `unit-tests/` deep dives: [`M2/Macaulay2/e/unit-tests/README.md`](M2/Macaulay2/e/unit-tests/README.md)
- Core M2 deep dives: [`M2/Macaulay2/m2/README.md`](M2/Macaulay2/m2/README.md)
- Build-glue deep dives: [`M2/cmake/README.md`](M2/cmake/README.md), [`M2/libraries/README.md`](M2/libraries/README.md), [`M2/Macaulay2/bin/README.md`](M2/Macaulay2/bin/README.md)
- Supervisor deep dives: [`M2/Macaulay2/system/README.md`](M2/Macaulay2/system/README.md)

## Alphabetical lookup hints

If you have an M2 concept name in mind:

| If looking for… | Start here |
|---|---|
| `aring`, `Ring`, `ring_elem` | [`GLOSSARY.md`](GLOSSARY.md) → [`e/ring-elements-and-maps.md`](M2/Macaulay2/e/ring-elements-and-maps.md) |
| Boolean GB | [`COMPUTATIONS.md`](COMPUTATIONS.md) → [`e/bibasis/architecture.md`](M2/Macaulay2/e/bibasis/architecture.md) |
| build pipeline | [`BUILD.md`](BUILD.md) |
| Ccode escape | [`GLOSSARY.md`](GLOSSARY.md) → [`c/architecture.md`](M2/Macaulay2/c/architecture.md) |
| Computation framework | [`COMPUTATIONS.md`](COMPUTATIONS.md) → [`e/file-comp.md`](M2/Macaulay2/e/file-comp.md) |
| coefficient rings | [`RING-ZOO.md`](RING-ZOO.md) → [`e/coefficient-rings.md`](M2/Macaulay2/e/coefficient-rings.md) |
| crashes (at startup) | [`STARTUP.md`](STARTUP.md) → [`DEBUG.md`](DEBUG.md) |
| crashes (runtime) | [`DEBUG.md`](DEBUG.md) |
| debugger | [`DEBUG.md`](DEBUG.md) → [`d/file-debugging.md`](M2/Macaulay2/d/file-debugging.md) |
| doc DSL | [`DOCUMENTATION-SYSTEM.md`](DOCUMENTATION-SYSTEM.md) |
| editing M2 syntax | [`PACKAGES.md`](PACKAGES.md) → [`DOCUMENTATION-SYSTEM.md`](DOCUMENTATION-SYSTEM.md) |
| engine boundary | [`e/interface/architecture.md`](M2/Macaulay2/e/interface/architecture.md) |
| F4 algorithm | [`COMPUTATIONS.md`](COMPUTATIONS.md) → [`e/f4/architecture.md`](M2/Macaulay2/e/f4/architecture.md) |
| FLINT | [`DEPENDENCIES.md`](DEPENDENCIES.md) → many `file-aring-*-flint.md` |
| free modules | [`e/free-modules.md`](M2/Macaulay2/e/free-modules.md) |
| Galois fields | [`RING-ZOO.md`](RING-ZOO.md) → many `file-aring-gf-*.md` |
| garbage collection | [`MEMORY.md`](MEMORY.md) |
| GB engines | [`COMPUTATIONS.md`](COMPUTATIONS.md) → [`e/groebner-bases.md`](M2/Macaulay2/e/groebner-bases.md) |
| Hilbert function | [`COMPUTATIONS.md`](COMPUTATIONS.md) → [`e/file-hilb.md`](M2/Macaulay2/e/file-hilb.md) |
| installPackage | [`DOCUMENTATION-SYSTEM.md`](DOCUMENTATION-SYSTEM.md) → [`m2/file-installPackage.md`](M2/Macaulay2/m2/file-installPackage.md) |
| interpreter | [`d/architecture.md`](M2/Macaulay2/d/architecture.md) |
| layered architecture | [`README.md`](README.md) → [`e/architecture.md`](M2/Macaulay2/e/architecture.md) |
| LLL | [`COMPUTATIONS.md`](COMPUTATIONS.md) → [`e/file-LLL.md`](M2/Macaulay2/e/file-LLL.md) |
| Macaulay matrix | [`COMPUTATIONS.md`](COMPUTATIONS.md) → [`e/f4/architecture.md`](M2/Macaulay2/e/f4/architecture.md) |
| matrices | [`e/matrices.md`](M2/Macaulay2/e/matrices.md) → [`e/file-mat.md`](M2/Macaulay2/e/file-mat.md) |
| memory model | [`MEMORY.md`](MEMORY.md) |
| monoid / monomial ordering | [`e/monoids-and-monomials.md`](M2/Macaulay2/e/monoids-and-monomials.md) |
| MPFR | [`DEPENDENCIES.md`](DEPENDENCIES.md) → [`e/file-aring-RRR.md`](M2/Macaulay2/e/file-aring-RRR.md) |
| NAG (Numerical AG) | [`COMPUTATIONS.md`](COMPUTATIONS.md) → [`e/file-NAG.md`](M2/Macaulay2/e/file-NAG.md) |
| non-commutative algebras | [`RING-ZOO.md`](RING-ZOO.md) → [`e/NCAlgebras/architecture.md`](M2/Macaulay2/e/NCAlgebras/architecture.md) |
| package author guide | [`PACKAGES.md`](PACKAGES.md) → [`TOUR.md`](TOUR.md) Path C |
| polynomial rings | [`RING-ZOO.md`](RING-ZOO.md) → [`e/polynomial-rings.md`](M2/Macaulay2/e/polynomial-rings.md) |
| resolutions | [`COMPUTATIONS.md`](COMPUTATIONS.md) → [`e/resolutions.md`](M2/Macaulay2/e/resolutions.md) |
| `scc1` translator | [`c/architecture.md`](M2/Macaulay2/c/architecture.md) |
| Schreyer-frame | [`GLOSSARY.md`](GLOSSARY.md) → [`e/schreyer-resolution/architecture.md`](M2/Macaulay2/e/schreyer-resolution/architecture.md) |
| signal handlers, Ctrl-C | [`d/file-interrupts.md`](M2/Macaulay2/d/file-interrupts.md) → [`THREADING.md`](THREADING.md) |
| startup sequence | [`STARTUP.md`](STARTUP.md) |
| stop conditions | [`COMPUTATIONS.md`](COMPUTATIONS.md) → [`e/file-comp.md`](M2/Macaulay2/e/file-comp.md) |
| supervisor | [`system/architecture.md`](M2/Macaulay2/system/architecture.md) |
| `Task` / `schedule` | [`THREADING.md`](THREADING.md) → [`d/file-threads.md`](M2/Macaulay2/d/file-threads.md) |
| TBB | [`THREADING.md`](THREADING.md) → [`e/file-m2tbb.md`](M2/Macaulay2/e/file-m2tbb.md) |
| testing | [`TESTING.md`](TESTING.md) |
| thread safety | [`THREADING.md`](THREADING.md) |

## How to find a doc — flow chart

```
I want to know about X.
   │
   ▼
Is X a concept name (jargon)?
   yes → GLOSSARY.md
   no
   │
Is X a directory or area?
   yes → that directory's README.md
   no
   │
Is X a specific source file?
   yes → that directory's file-<basename>.md
   no
   │
Is X an audience role (newcomer, debugger, ...)?
   yes → TOUR.md
   no
   │
Is X a cross-cutting concern (build, memory, threading)?
   yes → relevant top-level meta doc
   no
   │
Is X something concrete (ring, GB engine)?
   yes → RING-ZOO.md or COMPUTATIONS.md
   no
   │
Probably search this file (INDEX.md) — Ctrl-F.
```

## Related

- [`README.md`](README.md) — hierarchical TOC. The "what's in
  the source tree" view.
- [`GLOSSARY.md`](GLOSSARY.md) — vocabulary lookup.
- [`TOUR.md`](TOUR.md) — audience-specific entry points.
- [`CONTRIBUTING-DOCS.md`](CONTRIBUTING-DOCS.md) — the
  conventions this index reflects.
