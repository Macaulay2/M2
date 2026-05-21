# `configure.ac` — top-level autoconf input

`configure.ac` is the **autoconf input file** that gets processed
by `autogen.sh` (which runs `autoreconf`) into an executable
`configure` script. Counterpart to
[`CMakeLists.txt`](file-CMakeLists-txt.md) on the autotools side.

Part of [the M2 source root](README.md).

[← M2/ overview](README.md) · [← repo root](../README.md)

## Header

```m4
AC_INIT([Macaulay2],[m4_esyscmd_s([cat VERSION])],[https://github.com/Macaulay2/M2/issues],[Macaulay2],[https://macaulay2.com/])
AC_MSG_NOTICE([configuring Macaulay2 version $PACKAGE_VERSION])
AC_CONFIG_SRCDIR([VERSION])
AC_CONFIG_HEADERS(include/M2/config.h)
AC_CONFIG_FILES(m4_include(m4/files))
AC_SUBST(CONFIGURED_FILES,"$ac_config_files")
AC_SUBST(CONFIG_ARGS,"$ac_configure_args")
echo "'$0' $ac_configure_args" > config.args
```

Key autoconf calls:

- **`AC_INIT([Macaulay2], [VERSION], ...)`** — package name,
  version (read at configure time via `m4_esyscmd_s` from the
  `VERSION` file), bug-tracker URL, tarname, homepage.
- **`AC_CONFIG_HEADERS`** — output a generated `config.h` (the
  one M2's C/C++ code includes via `<M2/config.h>`).
- **`AC_CONFIG_FILES`** — list of `*.in` templates to process
  through `config.status`. The list is in
  [`m4/files`](m4/file-m4-local.md).

## m4 macro inclusions

```m4
m4_include(m4/ax_compare_version.m4)
m4_include(m4/ax_recursive_eval.m4)
m4_include(m4/openmp.m4)
m4_include(m4/search-libraries.m4)
m4_include(m4/ax_boost_base.m4)
m4_include(m4/ax_boost_regex.m4)
m4_include(m4/ax_blas.m4)
m4_include(m4/ax_lapack.m4)
```

Pulls in the custom and vendored m4 macros from
[`m4/`](m4/README.md). Without these, `configure` wouldn't know
how to test for BLAS, LAPACK, Boost, etc.

## What `configure.ac` declares

Approximate sections in order:

1. **Package metadata** — version, name, URL.
2. **Configure options** — `--enable-xxx`, `--with-xxx` knobs
   parallel to CMake's cache variables.
3. **Library detection** — `AX_BLAS`, `AX_LAPACK`,
   `SEARCH_LIBRARIES` calls for every dependency in
   [`libraries/`](libraries/README.md).
4. **Header / function probes** — `AC_CHECK_HEADERS`,
   `AC_CHECK_FUNCS` filling in `HAVE_*` macros.
5. **Type checks** — `AC_CHECK_SIZEOF` for pointers, ints.
6. **Compiler flags** — accumulate platform-appropriate
   `CFLAGS`/`CXXFLAGS`.
7. **`AC_OUTPUT`** — finalise, write all `Makefile`s and
   `config.h`.

## CONFIG_ARGS preservation

```m4
echo "'$0' $ac_configure_args" > config.args
C_CONFIG_ARGS=` echo "$ac_configure_args" | sed -e 's=\\\\=\\\\\\\\=g' -e 's=\\"=\\\\"=g' `
AC_DEFINE_UNQUOTED(CONFIG_ARGS,"$C_CONFIG_ARGS",arguments used for configure)
AC_SUBST(CONFIG_CMD,"'$0' $ac_configure_args")
```

Captures the exact `configure` invocation in two forms:

- **`config.args`** — a one-line file you can `bash config.args`
  to re-run configure identically. Useful when iterating.
- **`CONFIG_ARGS`** — a `#define` baked into `config.h` so the
  running binary can report how it was configured (M2's `about M2`
  shows this).

## The two-build-system contract

Anything declared here in `configure.ac` (an `--enable-foo`
option, a `HAVE_FOO` macro) should have a counterpart in
[`cmake/configure.cmake`](cmake/file-configure-cmake.md). The two
build systems must agree on:

- Which `HAVE_*` macros exist.
- Which `--enable-foo` user-facing options exist.
- What `config.h` ends up containing.

When adding a build-time switch, **edit both build files**.

## Used by

- The autotools build, after `autogen.sh` runs to produce
  `configure`.
- Distribution maintainers building from a release tarball.

## Related

- [`README.md`](README.md) — M2/ overview.
- [`file-autogen-sh.md`](file-autogen-sh.md) — bootstraps this
  into a `configure` script.
- [`file-CMakeLists-txt.md`](file-CMakeLists-txt.md) — CMake
  counterpart.
- [`m4/`](m4/README.md) — m4 macros referenced.
- [`include/file-configuration-in.md`](include/file-configuration-in.md)
  — template for the generated `config.h`.
