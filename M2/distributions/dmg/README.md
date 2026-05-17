# `M2/distributions/dmg/` — macOS disk-image distribution

Templates and helpers for building the `.dmg` (Apple disk image) distribution
of Macaulay2.

| File | Role |
|---|---|
| `Makefile.in` | Builds the `.dmg` from an installed tree |
| `ReadMe-MacOSX.rtf` | Rich-text README displayed inside the mounted disk image |
| `ReadMe-MacOSX.txt` | Plain-text variant |
| `ReadMe-MacOSX-fink.txt` | Variant for the (now-historical) Fink-based macOS distribution |

The actual `.dmg` packaging is driven from the top-level
[`distributions/Makefile.in`](../Makefile.in) and (on CMake builds) by
[`M2/cmake/packaging.cmake`](../../cmake/README.md).

## Related

- [`../`](../README.md) — packaging overview.
- [`../../cmake/darwin.cmake`](../../cmake/README.md) — macOS-specific build
  tweaks (separate from packaging).
