# `M2/Macaulay2/` — Macaulay2 source code

All Macaulay2 source code is rooted here. The directory is organised around
the [four-language stack](../../README.md#the-four-language-stack):

```
.d / .dd  ──scc1──▶  .c / .cpp  ──C/C++──▶  M2-interpreter ──▶ M2
   │                                              ▲
   │                                              │ linked
   │                                          M2-engine (C++)
   ▼
[d/]       [c/ defines the lang scc1 reads]    [e/]
```

## Layer-by-layer

| Directory | Layer | Purpose |
|---|---|---|
| [`c/`](c/README.md) | 1. Translator | `scc1`: turns `.d`/`.dd` into C/C++ |
| [`d/`](d/README.md) | 2. Interpreter | `.d`/`.dd` sources of `M2-interpreter` |
| [`e/`](e/README.md) | 3. Engine | C++ math kernel (~340 files) — see [engine deep-dive](../../README.md#engine-deep-dive-m2macaulay2e) |
| [`m2/`](m2/README.md) | 4. Core M2 | `.m2` sources loaded at startup |

## Everything else under here

| Directory | Purpose |
|---|---|
| [`packages/`](packages/README.md) | ~400 distributed packages |
| [`bin/`](bin/README.md) | Final `M2` binary linkage and `startup.c` shim |
| [`system/`](system/README.md) | Thread supervisor (`M2-supervisor`) |
| [`editors/`](editors/README.md) | Editor grammar generation + `M2-emacs` submodule |
| [`docs/`](docs/README.md) | Sphinx + Doxygen docs for the C++ engine |
| [`tests/`](tests/README.md) | Top-level CTest suites |
| [`man/`](man/README.md) | Unix man page source |
| [`html-check-links/`](html-check-links/README.md) | HTML link checker used by `make check` |

## Build glue files here

| File | Purpose |
|---|---|
| `CMakeLists.txt` | Entry point for the CMake build of this subtree |
| `Makefile.in` | Top-level autotools makefile template |
| `LAYOUT` | Specification of the install-time directory layout |
| `srcdir.in` | Templated source-dir marker |
| `COPYING-GPL-2`, `COPYING-GPL-3` | License text bundled with binaries |

## Where to make changes

| If you are editing... | Rebuild target | Notes |
|---|---|---|
| `.d` / `.dd` in `d/` | full interpreter rebuild | scc1 runs first; autotools may need 2 `make` runs |
| `e/` C++ | `M2-core` (CMake) | Add gtest in `e/unit-tests/` for new code |
| `m2/*.m2` | `M2-core` (CMake) | Edit `loadsequence` if you add a new file |
| `packages/Foo.m2` | none (use `loadPackage`) | Don't forget to list in `=distributed-packages` |
| `c/` (the translator) | scc1 + everything downstream | Rare; touch with care |

[← back to repository TOC](../../README.md#under-m2macaulay2)
