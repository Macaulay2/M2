# Documentation tour — where to start reading

The M2 documentation tree has four levels of depth
([per README.md](README.md#documentation-map)). This tour
**suggests reading orders for specific audiences** — so you don't
have to navigate 300+ files trying to find the right entry point.

[← repository TOC](README.md) · [Glossary](GLOSSARY.md)

## Pick your role

| If you are… | Start with |
|---|---|
| A new contributor wanting an overview | [Path A: Newcomer overview](#path-a-newcomer-overview) |
| Debugging the engine or fixing a bug | [Path B: Engine debugger](#path-b-engine-debugger) |
| Writing an M2 user package | [Path C: Package author](#path-c-package-author) |
| Writing pure-M2 code as a user | [Path D: M2 user](#path-d-m2-user) |
| Porting M2 to a new platform | [Path E: Build / port maintainer](#path-e-build--port-maintainer) |
| Adding a new ring / GB algorithm / resolution variant | [Path F: Engine extension](#path-f-engine-extension) |
| Curious about a specific algorithm | [Path G: Algorithm-specific](#path-g-algorithm-specific) |

Each path lists ~5-15 docs in suggested reading order.

---

## Path A: Newcomer overview

You want to understand "what is this codebase?"

1. **[`README.md`](README.md)** — repository TOC. Look at:
   - "The four-language stack" — high-level architecture diagram.
   - "Repository Architecture Table of Contents."
2. **Project [Wiki](https://github.com/Macaulay2/M2/wiki) and
   `.github/workflows/test_build.yml`** — build commands and CI
   matrix.
3. **[`GLOSSARY.md`](GLOSSARY.md)** — terminology you'll see
   everywhere.
4. The four **per-layer architecture refs**, in pipeline order:
   - [`c/architecture.md`](M2/Macaulay2/c/architecture.md) — the
     `scc1` translator (smallest).
   - [`d/architecture.md`](M2/Macaulay2/d/architecture.md) — the
     interpreter.
   - [`e/architecture.md`](M2/Macaulay2/e/architecture.md) — the
     engine.
   - [`m2/architecture.md`](M2/Macaulay2/m2/architecture.md) —
     the Core M2 language.
5. **One sample deep-dive** from any layer — e.g.
   [`d/file-evaluate.md`](M2/Macaulay2/d/file-evaluate.md) (the
   interpreter's evaluator) — to see the per-file doc shape.

By the end: you can navigate any source file by clicking from a
README to its `file-*.md` deep dive.

**Time investment**: ~2-3 hours.

---

## Path B: Engine debugger

A bug shows up. You need to find the responsible code fast.

1. **Reproduce in M2** — get a minimal test case.
2. **Identify the operation involved** — `gb`, `resolution`,
   matrix-multiply, ring-construction, ...
3. **Find the M2-side wrapper**:
   - It lives in `M2/Macaulay2/m2/`. Filename usually matches the
     operation: `gb.m2`, `res.m2`, `matrix.m2`, etc.
4. **Find the `raw…` call** in that file — that's the engine
   boundary call. Names like `rawGB`, `rawMatrix`.
5. **Find the engine entry point** in
   [`M2/Macaulay2/e/interface/`](M2/Macaulay2/e/interface/README.md)
   — e.g., `rawGB` → `IM2_GB_make` in
   [`interface/file-groebner-interface.md`](M2/Macaulay2/e/interface/file-groebner-interface.md).
6. **Trace into the algorithm**:
   - GB: [`groebner-bases.md`](M2/Macaulay2/e/groebner-bases.md)
     or specific engine ([`f4/architecture.md`](M2/Macaulay2/e/f4/architecture.md),
     [`gb-f4/architecture.md`](M2/Macaulay2/e/gb-f4/architecture.md),
     [`NCAlgebras/architecture.md`](M2/Macaulay2/e/NCAlgebras/architecture.md)).
   - Resolution: [`resolutions.md`](M2/Macaulay2/e/resolutions.md)
     or [`schreyer-resolution/architecture.md`](M2/Macaulay2/e/schreyer-resolution/architecture.md).
   - Matrix: [`matrices.md`](M2/Macaulay2/e/matrices.md).
   - Linear algebra: [`file-dmat-lu-variants.md`](M2/Macaulay2/e/file-dmat-lu-variants.md).
7. **Check the gtest suite**:
   [`e/unit-tests/`](M2/Macaulay2/e/unit-tests/README.md) — has
   a test exercising the buggy path? Add one if not.
8. **Check the engine-area README** for known limitations.

**Tip**: every per-file deep dive in the engine has a "**Used
by**" section pointing back upstream — useful for tracking the
caller chain.

**Time investment**: highly variable; the docs collapse the
"where do I start looking?" step from hours to minutes.

---

## Path C: Package author

You're writing or maintaining a package in
[`M2/Macaulay2/packages/`](M2/Macaulay2/packages/README.md).

1. **[`packages/README.md`](M2/Macaulay2/packages/README.md)** —
   how packages register, the `=distributed-packages` file.
2. **[`packages/file-package-conventions.md`](M2/Macaulay2/packages/file-package-conventions.md)**
   — file layout, `newPackage` declaration, the three sections
   of a package (setup / code / docs+tests).
3. **The documentation DSL**:
   - [`m2/file-document.md`](M2/Macaulay2/m2/file-document.md) —
     `doc ///...///` syntax.
   - [`packages/file-Macaulay2Doc.md`](M2/Macaulay2/packages/file-Macaulay2Doc.md)
     — the largest example.
4. **`installPackage`** mechanics — what runs when you
   "install" a package:
   - [`m2/file-installPackage.md`](M2/Macaulay2/m2/file-installPackage.md)
     — the engine of `installPackage`.
   - [`m2/file-examples.md`](M2/Macaulay2/m2/file-examples.md) —
     how examples get executed and their output captured.
5. **HTML / info generation** — [`m2/file-html.md`](M2/Macaulay2/m2/file-html.md)
   and [`packages/file-Style.md`](M2/Macaulay2/packages/file-Style.md).
6. **Testing**:
   - `TEST ///...///` blocks (`check "PackageName"`).
   - For more complex tests:
     [`tests/normal/file-normal-tests-catalogue.md`](M2/Macaulay2/tests/normal/file-normal-tests-catalogue.md).
7. **If your package depends on an external library**:
   - Add a [`Find<Lib>.cmake`](M2/cmake/file-find-cmakes.md)
     and a [`libraries/<lib>/`](M2/libraries/file-per-library-subdirs.md)
     subdir.
   - Mark in [`packages/CMakeLists.txt`](M2/Macaulay2/packages/).

**Time investment**: ~2-4 hours to understand the conventions; then it's iterative.

---

## Path D: M2 user

You're writing pure-M2 code (no engine internals).

1. **[`README.md`](README.md)** — Quick links → "Packages" line
   gives the user-facing surface.
2. **The book**: *Computations in Algebraic Geometry with Macaulay
   2* (Eisenbud, Grayson, Stillman, Sturmfels). The chapter
   structure is also mirrored in
   [`tests/ComputationsBook/file-computations-book-catalogue.md`](M2/Macaulay2/tests/ComputationsBook/file-computations-book-catalogue.md).
3. **In an M2 session**:
   - `help foo` — opens the manual page for `foo`.
   - `viewHelp` — opens HTML docs in a browser.
   - `?foo` — short form of `help`.
4. **For builtins**, the docs come from
   [`Macaulay2Doc`](M2/Macaulay2/packages/file-Macaulay2Doc.md).
5. **You probably don't need to read this repo's source** — but
   if you're curious about *how M2 does X*, the per-file deep
   dives in [`m2/`](M2/Macaulay2/m2/architecture.md) are
   approachable.

**Time investment**: read user docs in an M2 session, not this
repo. This repo is for contributors.

---

## Path E: Build / port maintainer

You're packaging M2 for a distro, building from source, or
porting to a new platform.

1. **Project [Wiki](https://github.com/Macaulay2/M2/wiki) and
   `.github/workflows/test_build.yml`** — canonical build commands.
2. **Pick your build system**:
   - CMake (preferred):
     [`M2/cmake/README.md`](M2/cmake/README.md). The key files:
     - [`configure.cmake`](M2/cmake/file-configure-cmake.md) —
       all options.
     - [`check-libraries.cmake`](M2/cmake/file-check-libraries-cmake.md)
       — library detection.
     - [`build-libraries.cmake`](M2/cmake/file-build-libraries-cmake.md)
       — build-from-source fallback.
     - The 25 [`Find*.cmake`](M2/cmake/file-find-cmakes.md)
       modules.
   - autotools:
     [`M2/libraries/README.md`](M2/libraries/README.md),
     [`configure.ac`](M2/file-configure-ac.md),
     [`Makefile.in`](M2/file-Makefile-in.md).
3. **Dependencies**:
   - System libraries M2 needs:
     [`libraries/file-per-library-subdirs.md`](M2/libraries/file-per-library-subdirs.md)
     for the full list with license info.
   - Vendored submodules:
     [`submodules/file-submodules.md`](M2/submodules/file-submodules.md).
4. **Packaging**:
   - [`distributions/file-distributions.md`](M2/distributions/file-distributions.md)
     — autotools-side packaging.
   - [`BUILD/docker/file-docker.md`](M2/BUILD/docker/file-docker.md)
     — Docker-based recipes for `.deb` / `.rpm` / `.dmg` /
     `.tar.gz`.
   - [`cmake/file-misc-cmakes.md`](M2/cmake/file-misc-cmakes.md)
     — CPack on the CMake side.
5. **CI** — `.github/workflows/test_build.yml`. The build matrix
   covers Ubuntu + macOS, autotools + CMake.

**Time investment**: ~4-8 hours to get a build going on a new
platform; less for routine packaging.

---

## Path F: Engine extension

You're adding a new ring, GB algorithm, resolution variant, or
similar engine feature.

1. **[`e/architecture.md`](M2/Macaulay2/e/architecture.md)** —
   the four-layer architecture; the legacy-`Ring` vs modern-`aring`
   story.
2. **The relevant area doc**:
   - New coefficient ring → [`coefficient-rings.md`](M2/Macaulay2/e/coefficient-rings.md).
   - New polynomial ring → [`polynomial-rings.md`](M2/Macaulay2/e/polynomial-rings.md).
   - New GB algorithm → [`groebner-bases.md`](M2/Macaulay2/e/groebner-bases.md)
     plus one of the engine architectures
     ([`f4/`](M2/Macaulay2/e/f4/architecture.md),
     [`gb-f4/`](M2/Macaulay2/e/gb-f4/architecture.md),
     [`NCAlgebras/`](M2/Macaulay2/e/NCAlgebras/architecture.md)).
   - New resolution variant → [`resolutions.md`](M2/Macaulay2/e/resolutions.md)
     and [`schreyer-resolution/architecture.md`](M2/Macaulay2/e/schreyer-resolution/architecture.md).
3. **The "Cross-cutting flows" recipe** in the top-level
   [`README.md`](README.md#cross-cutting-flows) for your specific
   case (adding a coefficient ring, adding a computation, etc.).
4. **Look at a model example** — find an existing similar entry
   in the deep-dive layer:
   - For a new coefficient ring: model on
     [`file-aring-zz-flint.md`](M2/Macaulay2/e/file-aring-zz-flint.md).
   - For a new GB algorithm: model on
     [`gb-f4/file-GBF4Computation.md`](M2/Macaulay2/e/gb-f4/file-GBF4Computation.md).
   - For a new Computation: read
     [`file-comp.md`](M2/Macaulay2/e/file-comp.md) (the base).
5. **Wire through the boundary**:
   - Add a function in
     [`interface/<area>.{h,cpp}`](M2/Macaulay2/e/interface/architecture.md).
   - Add the `.d` binding (one of `d/<area>.dd`).
   - Add the M2 wrapper (one of `m2/<area>.m2`).
6. **Add tests**:
   - C++ side: [`e/unit-tests/`](M2/Macaulay2/e/unit-tests/README.md).
   - M2 side:
     [`tests/normal/`](M2/Macaulay2/tests/normal/file-normal-tests-catalogue.md)
     or
     [`packages/EngineTests.m2`](M2/Macaulay2/packages/file-EngineTests.md).

**Time investment**: ~1 day to write a model-following implementation; longer for novel algorithms.

---

## Path G: Algorithm-specific

You're curious about *one specific algorithm*. Quick references:

| Algorithm | Start here |
|---|---|
| Gröbner basis (default) | [`groebner-bases.md`](M2/Macaulay2/e/groebner-bases.md) → [`file-gb-default.md`](M2/Macaulay2/e/file-gb-default.md) |
| F4 GB | [`f4/architecture.md`](M2/Macaulay2/e/f4/architecture.md) |
| F4 GB (refactored) | [`gb-f4/architecture.md`](M2/Macaulay2/e/gb-f4/architecture.md) |
| Toric GB | [`file-gb-variants.md`](M2/Macaulay2/e/file-gb-variants.md) — `gb-toric` |
| Gröbner walk | [`file-gb-variants.md`](M2/Macaulay2/e/file-gb-variants.md) — `gb-walk` |
| Sugarless GB | [`file-gb-variants.md`](M2/Macaulay2/e/file-gb-variants.md) — `gb-sugarless` |
| NC Gröbner basis | [`NCAlgebras/architecture.md`](M2/Macaulay2/e/NCAlgebras/architecture.md) |
| NC F4 | [`NCAlgebras/file-NCF4.md`](M2/Macaulay2/e/NCAlgebras/file-NCF4.md) |
| Boolean involutive basis | [`bibasis/architecture.md`](M2/Macaulay2/e/bibasis/architecture.md) |
| Free resolution (modern) | [`schreyer-resolution/architecture.md`](M2/Macaulay2/e/schreyer-resolution/architecture.md) |
| Free resolution (older) | [`file-res-old.md`](M2/Macaulay2/e/file-res-old.md) |
| NC free resolution | [`NCResolutions/file-nc-res-computation.md`](M2/Macaulay2/e/NCResolutions/file-nc-res-computation.md) |
| Hilbert function (Bigatti) | [`file-hilb.md`](M2/Macaulay2/e/file-hilb.md) |
| LLL lattice reduction | [`file-LLL.md`](M2/Macaulay2/e/file-LLL.md) (+ [`file-fplll.md`](M2/Macaulay2/e/file-fplll.md), [`file-ntl-glue.md`](M2/Macaulay2/e/file-ntl-glue.md)) |
| Polynomial GCD / factoring | [`interface/file-factory-interface.md`](M2/Macaulay2/e/interface/file-factory-interface.md) |
| Univariate root finding | [`interface/file-polyroots.md`](M2/Macaulay2/e/interface/file-polyroots.md) (MPSolve) |
| Chinese Remainder + rational lift | [`file-cra.md`](M2/Macaulay2/e/file-cra.md) |
| Numerical algebraic geometry | [`file-NAG.md`](M2/Macaulay2/e/file-NAG.md) |
| Dense matrix LU (Z/p, QQ, GF) | [`file-dmat-lu-variants.md`](M2/Macaulay2/e/file-dmat-lu-variants.md) |
| Geometric buckets (reduction) | [`file-geobucket.md`](M2/Macaulay2/e/file-geobucket.md) |

Each entry above links to a deep-dive. From there, "**Used by**"
sections in each deep-dive point at upstream callers; "**Related**"
sections link to sister algorithms and supporting code.

**Time investment**: ~30 min per algorithm if you read just the
deep-dive and one or two cross-references.

---

## Beyond the per-file deep dives

When you need to read **actual source code**:

- Source files live alongside their deep dives. The deep dive at
  `file-foo.md` describes `foo.{cpp,hpp}` (or similar) in the same
  directory.
- For grep / cscope work, the per-area docs (e.g.
  [`groebner-bases.md`](M2/Macaulay2/e/groebner-bases.md)) list
  file groups.
- The engine has a separate [Sphinx + Doxygen](M2/Macaulay2/docs/README.md)
  build that auto-generates class diagrams; opt-in.

## Other helpful entry points

- **[`GLOSSARY.md`](GLOSSARY.md)** — terminology dictionary.
- Project [Wiki](https://github.com/Macaulay2/M2/wiki) and
  `.github/workflows/test_build.yml` — build / test / lint commands.
- The wiki: https://github.com/Macaulay2/M2/wiki
- The mailing list: https://groups.google.com/group/macaulay2

## Feedback

If a documentation path was unclear, that's a signal the docs
could be improved. The `file-*.md` deep dives are editable; PRs
welcome.
