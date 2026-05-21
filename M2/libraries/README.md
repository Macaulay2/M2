# `M2/libraries/` — autotools build wrappers for third-party libraries

This directory contains per-library build wrappers used by the **autotools**
build to compile each external dependency from source when it is not available
on the system. The CMake build does the same job through
[`M2/cmake/`](../cmake/README.md) (`Find*.cmake` + `build-libraries.cmake`).

The original notes in plain text are in [`README`](README); this file is the
documentation index.

## Distinction: libraries vs programs

- A **library** is code linked directly into the `M2` binary.
- A **program** is code that runs separately, or a library linked only into a
  separate program — never into M2.

Both are managed here, but their licenses and packaging treatment differ.

## Per-library subdirectories

Each subdirectory mirrors the upstream library name and contains an
appropriate `Makefile.in`:

`4ti2`, `M2`, `Macaulay2-docs`, `cddlib`, `cohomcalg`, `csdp`, `eigen`,
`factory`, `fflas_ffpack`, `flint`, `fplll`, `frobby`, `gc`, `gdbm`, `gfan`,
`givaro`, `glpk`, `gmp`, `gtest`, `lapack`, `linbox`, `lrslib`, `mathic`,
`mathicgb`, `memtailor`, `mpfi`, `mpfr`, `mpsolve`, `msolve`, `nauty`,
`normaliz`, `ntl`, `polymake`, `readline`, `tbb`, `topcom`.

Plus shared build glue: `Makefile.in`, `Makefile.library.in`,
`Makefile.template`.

## Per-file deep dives

| File / pattern | Subject | Deep dive |
|---|---|---|
| `Makefile.in` | Top-level driver that loops over every library/program | [`file-Makefile-in.md`](file-Makefile-in.md) |
| `Makefile.library.in` | Shared per-library build recipe | [`file-Makefile-library-in.md`](file-Makefile-library-in.md) |
| `Makefile.template` | Starter template for new libraries | [`file-Makefile-template.md`](file-Makefile-template.md) |
| `*/Makefile.in` (36 subdirs) | Per-library wrappers — catalogued by role | [`file-per-library-subdirs.md`](file-per-library-subdirs.md) |

**Coverage:** every shared build file and the 36 per-library subdirs have dedicated deep-dive docs.

## Adding a new library / program

Summary (see [`README`](README) for full details):

1. Add a subdirectory here with a `Makefile.in` modelled on existing ones.
2. Update `../configure.ac` to detect / build the library (add to `LIBLIST`
   for libraries, `PROGLIST` for programs).
3. For libraries: add the version to `../Macaulay2/d/version.dd` and update
   the copyright string in `../Macaulay2/m2/startup.m2.in`.
4. Document the library / program in
   `../Macaulay2/packages/Macaulay2Doc/overview3.m2`, linked from the
   "Copyright and license" node.
5. Verify license compatibility — the resulting binary is distributed under
   GPL-3.

## Related

- [`M2/cmake/`](../cmake/README.md) — equivalent machinery for the CMake build.
- [`M2/submodules/`](../submodules/README.md) — git submodules used as upstream
  source for many of these libraries.

[← back to repository TOC](../../README.md#under-m2)
