# `M2/include/` — generated and shared headers

Public C/C++ headers that are either generated at configure time or shared
across multiple parts of the source tree.

| Path | Purpose |
|---|---|
| `M2/` | M2-specific headers installed into the build / install tree |
| `valgrind/` | Bundled Valgrind suppression and macro headers |
| `configuration.in` | Template for `configuration.h` — fills in build-time constants (paths, versions, feature flags). Substituted by both build systems |
| `config.Makefile.in` | Templated build-time `Makefile` fragment with the same constants |

Generated outputs from this directory are consumed throughout
[`Macaulay2/d/`](../Macaulay2/d/README.md), [`Macaulay2/e/`](../Macaulay2/e/README.md),
and [`Macaulay2/bin/`](../Macaulay2/bin/README.md).

## Editing rules

- Never edit a generated header in a build tree — change the corresponding
  `.in` template here and let configure regenerate.
- New build-time switches normally need three edits: `configuration.in`,
  `configure.ac` (autotools), and `M2/cmake/configure.cmake` (CMake).

[← back to repository TOC](../../README.md#under-m2)
