# `M2/include/` — generated and shared headers

Public C/C++ headers that are either generated at configure time or shared
across multiple parts of the source tree.

| Path | Purpose | Deep dive |
|---|---|---|
| `M2/` | M2-specific headers (`gc-include.h`, `math-include.h`, `atomic-field.h`, generated `config.h`) | [`file-M2-headers.md`](file-M2-headers.md) |
| `valgrind/` | Bundled Valgrind suppression and macro headers | [`file-M2-headers.md`](file-M2-headers.md) |
| `configuration.in`, `config.Makefile.in` | Templates for `configuration.h` and the Makefile fragment | [`file-configuration-in.md`](file-configuration-in.md) |

**Coverage:** every file and subdir has a dedicated deep-dive doc.

Generated outputs from this directory are consumed throughout
[`Macaulay2/d/`](../Macaulay2/d/README.md), [`Macaulay2/e/`](../Macaulay2/e/README.md),
and [`Macaulay2/bin/`](../Macaulay2/bin/README.md).

## Editing rules

- Never edit a generated header in a build tree — change the corresponding
  `.in` template here and let configure regenerate.
- New build-time switches normally need three edits: `configuration.in`,
  `configure.ac` (autotools), and `M2/cmake/configure.cmake` (CMake).

[← back to repository TOC](../../README.md#under-m2)
