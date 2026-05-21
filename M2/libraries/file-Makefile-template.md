# `Makefile.template` — starter template for new libraries

`Makefile.template` is the **starter template** developers copy
to bootstrap a new per-library subdirectory. Almost empty —
just the variables a per-library Makefile must define.

Part of [`libraries/`](README.md).

[← libraries/ overview](README.md) · [← top-level repo TOC](../../README.md)

## The whole file

```makefile
# replace FOO by the name of the library or package
# install this file as FOO/Makefile.in
# add libraries/FOO/Makefile to ../m4/files
# run make -C ../..
# reconfigure in the build directory
HOMEPAGE = 
URL = 
VERSION = 
TARFILE = 
# TARDIR = 
PROGRAMS = 
LIBRARIES = 
INCLUDEFILES =
LICENSEFILES = 
# CONFIGURECMD = 
# BUILDOPTIONS = 
# INSTALLCMD = 
include ../Makefile.library
Makefile: @srcdir@/Makefile.in ; cd ../.. && ./config.status libraries/FOO/Makefile
# Local Variables:
# mode: makefile-gmake
# compile-command: "make -C $M2BUILDDIR/libraries/FOO "
# End:
```

The header is a **mini-instruction-set** for adding a new library:

1. Replace every `FOO` with your library's name.
2. Save as `<name>/Makefile.in`.
3. Add `libraries/<name>/Makefile` to `../m4/files`.
4. Re-run autotools (`make -C ../..`).
5. Reconfigure in the build directory.

## The required variables

| Variable | Meaning |
|---|---|
| `HOMEPAGE` | Library's homepage URL |
| `URL` | Where to download the source from |
| `VERSION` | Exact version to fetch |
| `TARFILE` | Source archive name |
| `TARDIR` | Directory the archive unpacks into (default `<name>-<version>`) |
| `PROGRAMS` | Programs the library installs |
| `LIBRARIES` | Libraries the library installs |
| `INCLUDEFILES` | Headers to install |
| `LICENSEFILES` | License files to bundle (`COPYING`, `LICENSE.txt`) |
| `CONFIGURECMD` | Override the default `./configure` |
| `BUILDOPTIONS` | Extra `make` flags |
| `INSTALLCMD` | Override the default `make install` |

Everything else (download, unpack, configure, build, check,
install, patch logic) comes from the included
[`Makefile.library`](file-Makefile-library-in.md).

## Typical concrete example

Looking at `flint/Makefile.in`:

```makefile
SUBMODULE = true
HOMEPAGE = http://flintlib.org
URL = https://github.com/flintlib/flint
VERSION = 3.5.0
CHECKTARGET = .
LICENSEFILES = COPYING
CFLAGS += -std=c90 -pedantic-errors
PRECONFIGURE = ./bootstrap.sh
CONFIGOPTIONS += --disable-shared

include ../Makefile.library
```

About 20 lines including overrides. The shared recipe takes care
of the other ~200 lines of work.

## Why a template plus shared recipe

The split keeps each library's Makefile **focused on what's
unique about it**:

- "FLINT needs `bootstrap.sh` first."
- "FLINT's test target is `.` not `check`."
- "FLINT needs C90-pedantic compile flags."

Everything else (where to download, where to put outputs, how to
run check, how to clean) is shared.

## Used by

- Developers adding a new library to M2's autotools build.

## Related

- [`README.md`](README.md) — libraries/ overview.
- [`file-Makefile-library-in.md`](file-Makefile-library-in.md) —
  shared recipe.
- [`file-Makefile-in.md`](file-Makefile-in.md) — top-level driver.
- Per-library subdirectories — concrete examples of the template
  filled out.
