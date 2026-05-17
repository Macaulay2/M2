# `M2-suppressions.supp`, `info-dir-template` — runtime auxiliary files

The two files in `M2/files/` are **static auxiliary files**
bundled with the M2 distribution.

Part of [`files/`](README.md).

[← files/ overview](README.md) · [← top-level repo TOC](../../README.md)

## `M2-suppressions.supp` — Valgrind suppression file

A list of Valgrind suppression rules for known M2 false
positives. Example entries (approximate):

```
{
   bdwgc_known_uninit
   Memcheck:Cond
   fun:GC_*
}

{
   gmp_realloc_warns
   Memcheck:Leak
   fun:__gmp_default_allocate
}
```

Each suppression block silences a specific class of warning that
isn't an actual M2 bug:

- **Boehm GC** — its mark-sweep scans uninitialised memory
  intentionally; Memcheck flags it.
- **GMP** — `mp_alloc` patterns Valgrind misinterprets.
- **MPFR** — similar.
- **flint** — similar.

Without these suppressions, running M2 under `valgrind --track-origins=yes`
produces thousands of spurious warnings, drowning out actual
bugs.

Usage:

```sh
valgrind --suppressions=$M2/files/M2-suppressions.supp M2
```

The CMake / autotools build wires this into the
`valgrind-check` target so developers don't have to remember the
path.

## `info-dir-template` — texinfo dir template

```
Macaulay2: M2 software
* Macaulay2: (Macaulay2).            Software system for research in algebraic geometry.
* Macaulay2Doc: (Macaulay2Doc).      User documentation.
...
```

(Approximate content.)

A template for the `dir` file that lives next to texinfo `.info`
files in an info-system installation. When `info` runs, it reads
`dir` to know which info files are installed.

M2's installer:

1. Reads `info-dir-template`.
2. Substitutes paths and versions.
3. Installs the result as `${docdir}/dir` (or merges into
   existing).
4. Runs `install-info` to register M2 with the system's info
   database.

This is how `info Macaulay2` from a shell finds M2 docs.

## Why these specific two files

The directory is a **catch-all** for runtime auxiliary data
that doesn't fit elsewhere:

- Not source code → not in `Macaulay2/`.
- Not packaging metadata → not in `distributions/`.
- Not headers → not in `include/`.

Pattern: if you have a file that ships with M2 but is just data,
this is where it goes.

## Used by

- Valgrind, when developers run `valgrind M2`.
- `install-info`, when M2 is installed to a path with info-system
  integration.
- The packaging machinery
  ([`../distributions/README.md`](../distributions/README.md)).

## Related

- [`README.md`](README.md) — files/ overview.
- [`../include/file-M2-headers.md`](../include/file-M2-headers.md)
  — Valgrind macro headers (`valgrind/`).
- [`../distributions/README.md`](../distributions/README.md) —
  packaging consumes these files.
