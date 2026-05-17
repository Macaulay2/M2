# `ax_*.m4` — autoconf-archive macros

Six of the `m4` files in this directory are **vendored copies of
[autoconf-archive](https://www.gnu.org/software/autoconf-archive/)
macros**. They provide detection / utility logic that's not in
core autoconf.

Part of [`m4/`](README.md).

[← m4/ overview](README.md) · [← top-level repo TOC](../../README.md)

## The six macros

| File | Purpose | Used in configure.ac for |
|---|---|---|
| `ax_blas.m4` | Detect BLAS implementation | LAPACK linking |
| `ax_lapack.m4` | Detect LAPACK | numerical linear algebra |
| `ax_boost_base.m4` | Boost library root detection | Boost.Regex, Boost.Stacktrace |
| `ax_boost_regex.m4` | Boost.Regex detection | M2's regex backend (Boost is the default) |
| `ax_compare_version.m4` | Compare two version strings | `if BOOST_VERSION >= 1.70 ...` style checks |
| `ax_recursive_eval.m4` | Recursive variable substitution | Building install paths like `${exec_prefix}/${tail_bindir}` |

## Example: `ax_blas.m4`

```m4
# ===========================================================================
#         https://www.gnu.org/software/autoconf-archive/ax_blas.html
# ===========================================================================
#
# SYNOPSIS
#
#   AX_BLAS([ACTION-IF-FOUND[, ACTION-IF-NOT-FOUND]])
#
# DESCRIPTION
```

The macro tries the common BLAS variants:

- **Apple Accelerate** (macOS).
- **ATLAS**.
- **Intel MKL**.
- **OpenBLAS**.
- **Generic `-lblas`**.

For each, it tries linking a small test program that calls a BLAS
function. The first one that links wins.

`ax_lapack.m4` is structured the same way but for LAPACK
(typically depending on BLAS having been detected first).

## Boost detection

```m4
# AX_BOOST_BASE([MINIMUM-VERSION])
```

`ax_boost_base.m4` finds the Boost library installation and sets:

- `BOOST_CPPFLAGS` — include flags.
- `BOOST_LDFLAGS` — link flags.
- `BOOST_VERSION` — detected version.

After it runs, `ax_boost_regex.m4` can build on that base and
test specifically for `boost/regex.hpp` plus the right library
name (depending on multithreading, layout conventions).

## Version comparison

```m4
# AX_COMPARE_VERSION(version_a, op, version_b, [action-if-true], [action-if-false])
```

Compares version strings like `"1.2.3"` vs `"1.2.0"`. Used
liberally in configure.ac to guard "feature X requires library Y
≥ Z" conditionals.

## Why vendor instead of system?

`autoconf-archive` is sometimes installed system-wide (e.g.,
`apt install autoconf-archive`), but:

- **Not always available** on every system M2 builds on.
- **Version drift** — different distros ship different
  autoconf-archive versions with subtly different behaviour.
- **Reproducibility** — vendoring locks the exact macro
  behaviour M2 tested against.

Vendoring trades some duplication for build reproducibility.

## Used by

- `configure.ac` at the repo root.
- Indirectly, by every part of the autotools build that depends
  on BLAS/LAPACK/Boost.

## Related

- [`README.md`](README.md) — m4/ overview.
- [`file-m4-local.md`](file-m4-local.md) — M2's hand-written
  `.m4` macros.
- [autoconf-archive](https://www.gnu.org/software/autoconf-archive/)
  — upstream.
- [`../cmake/file-find-cmakes.md`](../cmake/file-find-cmakes.md)
  — CMake equivalent.
