# `M2/check-configure/` — configure-time sanity checks

A tiny directory with build-system glue that runs sanity checks during
`configure`. The work is driven by `Makefile.in` here, invoked from the
top-level autotools build.

| File | Role |
|---|---|
| `Makefile.in` | Templated rules that verify configure has produced a usable build environment |

This is intentionally minimal — most actual checks live in
`M2/configure.ac` itself. The directory exists so those checks have a place
to put their generated artefacts without polluting the main build tree.

[← back to repository TOC](../../README.md#under-m2)
