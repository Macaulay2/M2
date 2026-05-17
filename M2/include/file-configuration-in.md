# `configuration.in`, `config.Makefile.in` — generated-header templates

`configuration.in` and `config.Makefile.in` are the **templates**
from which configure-time constants land in the build:

- **`configuration.h`** — C header, included throughout the C/C++
  sources.
- **`config.Makefile`** — Makefile fragment, included by build
  Makefiles.

Part of [`include/`](README.md).

[← include/ overview](README.md) · [← top-level repo TOC](../../README.md)

## What configure fills in

Both files have `@VAR@`-style placeholders that autoconf
(`config.status`) or CMake (`configure_file`) substitute at
configure time. Typical variables:

| `@VAR@` | Value at configure time |
|---|---|
| `@PACKAGE_VERSION@` | e.g. `"1.26.05"` |
| `@PACKAGE_TARNAME@` | `"Macaulay2"` |
| `@prefix@` | e.g. `/usr/local` |
| `@exec_prefix@` | typically `${prefix}` |
| `@bindir@`, `@libdir@`, `@docdir@` | install paths |
| `@HAVE_PTHREAD_H@` | `1` if available, undefined otherwise |
| `@HAVE_GMP@`, `@HAVE_MPFR@`, ... | per-library presence flags |
| `@MPFR_VERSION@`, `@FLINT_VERSION@`, ... | detected versions |
| `@DEBUG@`, `@OPTIMIZE@` | build-mode flags |
| `@OS@`, `@HOST_OS@` | platform info |

## Where the substituted versions land

```
configure.ac      → reads configuration.in
                  → produces M2/include/M2/configuration.h
                  → distributed throughout the binary install

configure.ac      → reads include/config.Makefile.in
                  → produces a build-tree config.Makefile
                  → included by every Makefile in the build
```

Equivalents on the CMake side:

```
cmake/configure.cmake  → configure_file(configuration.in ...)
                       → produces the same configuration.h
```

Both build systems produce **the same end-product**: a
`configuration.h` consumed by C/C++ code uniformly.

## Why share through a header?

C/C++ files include `<M2/configuration.h>` and read constants
like `M2_VERSION` directly. No runtime config files; no env-var
parsing in inner loops. The pattern is the standard
"autoconf-defines-go-here" header.

## `config.Makefile` — Makefile counterpart

Some build-time decisions need to be visible to Makefiles, not
just C code:

- Install paths.
- Tool overrides (`@CC@`, `@CXX@`, `@AR@`, `@INSTALL@`).
- Per-library flags from `pkg-config`.
- `PARALLEL`, `OPTIMIZE`, `DEBUG` flags.

`config.Makefile.in` collects these into a single Makefile
fragment that downstream Makefiles `include ../include/config.Makefile`.

## Editing rules

The README at the top of [`include/`](README.md) captures the
golden rule:

- **Never edit `configuration.h` in a build tree** — it gets
  overwritten on every configure.
- **Edit `configuration.in` here** and re-run configure.
- **New build-time switches need 3 edits**: this file,
  `configure.ac`, and
  [`../cmake/file-configure-cmake.md`](../cmake/file-configure-cmake.md).

The three-edit rule ensures both build systems stay in sync.

## Used by

- Every C/C++ file in the engine and interpreter.
- Every Makefile in the autotools build.
- The packaging machinery
  ([`../distributions/README.md`](../distributions/README.md)) for
  filling in version / path placeholders.

## Related

- [`README.md`](README.md) — include/ overview.
- [`file-M2-headers.md`](file-M2-headers.md) — the rest of
  `include/M2/`.
- `configure.ac` — primary autotools consumer.
- [`../cmake/file-configure-cmake.md`](../cmake/file-configure-cmake.md)
  — CMake counterpart.
