# `M2/m4/` — Autoconf m4 macros

Custom and vendored autoconf macros consumed by `configure.ac`. Used only by
the **autotools** build; the CMake build's equivalent detection logic lives in
[`M2/cmake/`](../cmake/README.md).

| File | Source / purpose |
|---|---|
| `ax_blas.m4` | Detect BLAS — from [autoconf-archive](https://www.gnu.org/software/autoconf-archive/) |
| `ax_lapack.m4` | Detect LAPACK — from autoconf-archive |
| `ax_boost_base.m4` | Boost base detection — autoconf-archive |
| `ax_boost_regex.m4` | Boost.Regex detection — autoconf-archive |
| `ax_compare_version.m4` | Version comparison utility — autoconf-archive |
| `ax_recursive_eval.m4` | Recursive variable expansion — autoconf-archive |
| `openmp.m4` | OpenMP detection |
| `search-libraries.m4` | M2-local helper that tries a list of candidate library names |
| `files` | List of m4 files for the autoconf bootstrap |

To regenerate `configure` after editing here, run `autogen.sh` at
`M2/`.

## Per-file deep dives

| Files | Deep dive |
|---|---|
| `ax_blas.m4`, `ax_lapack.m4`, `ax_boost_*.m4`, `ax_compare_version.m4`, `ax_recursive_eval.m4` | [`file-autoconf-archive.md`](file-autoconf-archive.md) |
| `openmp.m4`, `search-libraries.m4`, `files` | [`file-m4-local.md`](file-m4-local.md) |

**Coverage:** every m4 macro and helper has a dedicated deep-dive doc.

[← back to repository TOC](../../README.md#under-m2)
