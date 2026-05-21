# `openmp.m4`, `search-libraries.m4`, `files` — M2-local m4 helpers

Three m4 files that **don't come from autoconf-archive** — M2's
own helpers plus the bootstrap manifest.

Part of [`m4/`](README.md).

[← m4/ overview](README.md) · [← top-level repo TOC](../../README.md)

## `search-libraries.m4` — static-link-preferring search

```m4
# Here we introduce a macro that is derived from AC_SEARCH_LIBS, but modified
# so we can specify libraries as absolute paths, such as /usr/local/lib/libfoo.a or -lfoo.
# The point of this is so we can link with the static version of the library instead
# of the dynamic version.  We need something like this, because the BSD "ld" command
# provided by Mac OS does not offer a command line option to achieve it.

# -- Macro: SEARCH_LIBRARIES (FUNCTION, SEARCH-LIBS, [ACTION-IF-FOUND],
#          [ACTION-IF-NOT-FOUND], [OTHER-LIBRARIES])
```

The header explains the problem this macro solves:

- M2 wants to **prefer static linking** for many libraries (so the
  final binary is self-contained).
- Standard `AC_SEARCH_LIBS` only accepts `-lname` syntax — which
  the linker resolves to a `.so` if available.
- macOS's `ld` doesn't have a "prefer static" flag.

`SEARCH_LIBRARIES` works around this by accepting **absolute
paths** like `/usr/local/lib/libfoo.a`. The linker then takes
exactly that file.

## `openmp.m4` — OpenMP detection

A custom detection macro for OpenMP support. Not from
autoconf-archive because:

- Autoconf has an `AC_OPENMP` but it doesn't probe for the
  *specific* flags M2 needs across compilers.
- The macro tests `-fopenmp` (gcc), `-fopenmp=libomp` (clang),
  `/openmp` (MSVC) in order.

The result is exported as `${OPENMP_CFLAGS}` and
`${OPENMP_CXXFLAGS}` for inclusion in compile commands.

## `files` — the bootstrap manifest

```
Makefile
check-configure/Makefile
include/config.Makefile
include/configuration
distributions/Makefile
distributions/install/Makefile
distributions/top/Makefile
distributions/top/INSTALL
distributions/top/preremove
distributions/top/postinstall
```

A **plain-text list of `Makefile.in` / template files** that
`configure.ac` should process via `AC_CONFIG_FILES(...)`.
Splitting this into a separate `files` file (rather than
inlining in `configure.ac`) lets it be:

- **Edited cleanly** — one path per line, easy to add/remove.
- **Generated** — some build steps regenerate `files` from
  globbing the tree.
- **Versioned independently** — git diffs to this file are
  readable.

`configure.ac` reads `files`, prefixes each path with
`AC_CONFIG_FILES`, and the autotools machinery does the rest.

The same pattern appears in `libraries/m4/files` and other
subdirs.

## Why hand-rolled instead of more autoconf-archive

Each of these solves a problem too M2-specific for upstream
adoption:

- **`search-libraries.m4`** — depends on M2's particular static-
  linking convention.
- **`openmp.m4`** — needs M2's specific cross-compiler flag
  preferences.
- **`files`** — pure project bookkeeping, not really an m4
  macro.

## Used by

- The autotools `configure.ac` includes these via `m4_include`.
- The autogen / autoreconf step.

## Related

- [`README.md`](README.md) — m4/ overview.
- [`file-autoconf-archive.md`](file-autoconf-archive.md) — sister
  doc covering the upstream-derived macros.
- `configure.ac` (at repo root) — primary consumer.
