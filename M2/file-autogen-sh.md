# `autogen.sh` — autotools bootstrap script

`autogen.sh` is the **autotools bootstrap script**. Runs
`autoreconf` to turn [`configure.ac`](file-configure-ac.md) into
an executable `configure`. Run once after a fresh git clone, then
re-run only when `configure.ac` or m4 macros change.

Part of [the M2 source root](README.md).

[← M2/ overview](README.md) · [← repo root](../README.md)

## The whole script

```sh
#!/bin/sh

set -e

srcdir=$(dirname $0)
test -z "$srcdir" && srcdir=.

echo "-- Generating configure script"
autoreconf --verbose --force --install $srcdir

# These files may not be created by older versions of autoconf
if test ! -f $srcdir/config.guess
then
    cp -v $(automake --print-libdir)/config.guess $srcdir
fi

if test ! -f $srcdir/config.sub
then
    cp -v $(automake --print-libdir)/config.sub $srcdir
fi

if test ! -f $srcdir/install-sh
then
    cp -v $(automake --print-libdir)/install-sh $srcdir
fi
```

About 25 lines total. Three things happen:

1. **`autoreconf --verbose --force --install`** — runs the
   autotools toolchain in sequence:
   - `aclocal` — gathers m4 macros.
   - `autoconf` — produces `configure` from `configure.ac`.
   - `automake` — produces `Makefile.in` from `Makefile.am`
     (M2 doesn't use this style; the `Makefile.in`s here are
     hand-written).
   - `autoheader` — produces `config.h.in` from
     `configure.ac`'s `AC_DEFINE` calls.
2. **`config.guess` / `config.sub` / `install-sh` fallback** —
   on older autoconf versions, these helper scripts aren't
   auto-installed. The script copies them from automake's lib dir
   if missing.

## When to run

- **First fresh clone**: `cd M2 && ./autogen.sh`.
- **After changing `configure.ac`** or anything in `m4/`.
- **Never** when using the CMake build — CMake doesn't need
  these files.

Released source tarballs ship with `configure` already
generated, so end users typically don't need to run `autogen.sh`.

## `--force` and `--install`

- **`--force`** — overwrite even up-to-date `configure`. Avoids
  the "I edited `configure.ac` but my `configure` is older"
  trap.
- **`--install`** — install missing auxiliary files (config.guess
  etc.). The fallback below this line is defensive — newer
  autoreconf does this; older versions skip it.

## Why not autogen everything in CI?

CI builds run `autogen.sh` once per build. For a fresh CI VM:

- ~5 seconds, mostly autoreconf overhead.
- One-time cost; subsequent `make` runs reuse the produced
  `configure`.

## Used by

- Developers bootstrapping a fresh clone.
- CI workflows that build from source.
- Anyone editing `configure.ac` or `m4/`.

## Related

- [`README.md`](README.md) — M2/ overview.
- [`file-configure-ac.md`](file-configure-ac.md) — input that
  this script processes.
- [`m4/`](m4/README.md) — m4 macros that get gathered.
- [`file-Makefile-in.md`](file-Makefile-in.md) — top-level
  Makefile produced from `Makefile.in` template.
