# `Makefile.library.in` — shared per-library build recipe

`Makefile.library.in` is the **shared autotools build recipe**
that every per-library `Makefile.in` includes (via
`include ../Makefile.library`). Centralises download, configure,
build, install, and patch logic so per-library Makefiles can be
20 lines of overrides instead of 200 lines of boilerplate.

Part of [`libraries/`](README.md).

[← libraries/ overview](README.md) · [← top-level repo TOC](../../README.md)

## Header

```makefile
# -*- Makefile -*-
LIBRARY_MODE = yes
include ../../include/config.Makefile
TARFILE_DIR = @abs_top_srcdir@/BUILD/tarfiles
export LD_LIBRARY_PATH:=$(BUILTLIBPATH)/lib:$(LD_LIBRARY_PATH)
# some libraries (gdbm, mpfr, readline, gmp, and gc) use libtool, which 
# insists on installing its files to a directory whose trailing part is the
# prefix that was used at compile time, i.e., the only change allowed to the prefix
# between compile time and install time is to prepend a string
PREFIX = $(BUILTLIBPATH)
ifneq ($(PARALLEL),yes)
NOTPARALLEL = -j1
endif
LIBNAME ?= $(shell basename `pwd`)
GIT_REF ?= v$(VERSION)
UNTARDIR = build
OLDUNTARDIR = build-old
TARDIR ?= $(LIBNAME)-$(VERSION)
TARFILE ?= $(LIBNAME)-$(VERSION).tar.gz
```

The library-name (`LIBNAME`) defaults to the directory name —
typical Makefile-pattern for per-lib customisation that "just
works."

## What the recipe does

Standard targets every library inherits:

| Target | Action |
|---|---|
| `fetch` | Download `TARFILE` from `URL` (or pull submodule) |
| `unpack` | Extract into `UNTARDIR` (default `build/`) |
| `patch` | Apply patches in `patch-<version>` |
| `configure` | Run `./configure --prefix=$(PREFIX) $(CONFIGOPTIONS)` |
| `build` | Run `make $(BUILDOPTIONS)` |
| `check` | Run `make check` (test suite) |
| `install` | Run `make install` |
| `clean` | Wipe `UNTARDIR` |
| `unconfigure` | Remove `.configured` marker so configure re-runs |
| `reinstall` | `unconfigure` + `install` |

A per-library `Makefile.in` typically just sets variables
(`HOMEPAGE`, `URL`, `VERSION`, `LICENSEFILES`, `CONFIGOPTIONS`,
`PATCHFILE`) and includes this file. The shared recipe does the
rest.

## Why centralised?

Without `Makefile.library.in`, each of the 36 libraries would
need ~200 lines of identical Makefile boilerplate. Maintenance
nightmare:

- Adding a new build mode (e.g., LTO) would mean editing 36
  files.
- A bug in the download/extract path would propagate to 36
  places.

Centralising means **one change here propagates everywhere**.

## Key shared logic

The non-obvious bits the shared recipe handles:

- **libtool prefix lock** — the comment notes that gdbm, mpfr,
  readline, gmp, gc need the install prefix to match the
  configure-time prefix. The recipe ensures this.
- **`LD_LIBRARY_PATH`** — propagated so a freshly-built library
  can be linked against by the next one in dependency order.
- **`PKG_CONFIG_PATH`** — same for pkg-config-discovered libs.
- **Cross-compile awareness** — detects `--build`/`--host`
  configure args and passes them on.
- **macOS install-name padding** — `-headerpad_max_install_names`
  so the resulting `.dylib` can be rewritten by
  `install_name_tool` during deployment.

## Used by

- Every per-library `Makefile.in` in this directory.

## Related

- [`README.md`](README.md) — libraries/ overview.
- [`file-Makefile-in.md`](file-Makefile-in.md) — top-level driver
  invoking each library.
- [`file-Makefile-template.md`](file-Makefile-template.md) — the
  starter template for new libraries.
- [`../cmake/file-build-libraries-cmake.md`](../cmake/file-build-libraries-cmake.md)
  — CMake equivalent.
