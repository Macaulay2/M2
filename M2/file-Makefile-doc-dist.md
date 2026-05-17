# `Makefile.doc-dist` — documentation-only distribution

`Makefile.doc-dist` builds a **documentation-only distribution
tarball** — the pre-built M2 documentation databases without any
binaries. Used by distributions / packagers who want to ship the
docs separately from the binaries.

Part of [the M2 source root](README.md).

[← M2/ overview](README.md) · [← repo root](../README.md)

## Header

```makefile
prefix ?= /usr/local
exec_prefix ?= $(prefix)
datadir ?= $(prefix)/share
libdir ?= $(exec_prefix)/lib

PACKAGES := $(patsubst dump/%.dump, %, $(wildcard dump/*.dump))
ENDIAN := $(shell printf '\x61\x62\x63\x64' | od -An -tx4 -N4 | tr -d ' \n' | \
	awk '{if($$0=="61626364") print "abcd"; else print "dcba"}')
POINTER_SIZE := $(shell expr $(shell getconf LONG_BIT) / 8)
DB_FILENAME = rawdocumentation-$(ENDIAN)-$(POINTER_SIZE).db
DB_FILES := $(patsubst %, lib/Macaulay2/%/cache/$(DB_FILENAME), $(PACKAGES))
```

Three notable things:

1. **`prefix ?= /usr/local`** — overridable install prefix. The
   `?=` form lets the caller override; otherwise default.
2. **`ENDIAN` detection** — runs a 4-byte test pattern through
   `od` to determine host endianness. M2's doc databases are
   **endianness-specific** because they use packed binary GDBM
   format.
3. **`POINTER_SIZE` detection** — `getconf LONG_BIT` divided by
   8 gives pointer size in bytes (4 or 8).

The doc database file name embeds both:

```
rawdocumentation-abcd-8.db    # little-endian, 64-bit
rawdocumentation-dcba-4.db    # big-endian, 32-bit
```

This way a single tarball can ship databases for multiple
architectures, and the runtime picks the right one.

## Why a separate Makefile

Why not roll this into `Makefile.in`?

- **No `configure` dependency** — `Makefile.doc-dist` works
  standalone, given pre-built `dump/*.dump` files. Useful for
  packagers who pre-build docs once and ship them across many
  package versions.
- **Distinct purpose** — building docs and installing them is
  conceptually separate from building M2.
- **Faster** — `make -f Makefile.doc-dist install` skips all the
  `configure` / build-tree machinery.

## What gets built

For each package in `dump/`:

```
dump/Macaulay2Doc.dump          (raw dump)
    ↓ gdbm_load
lib/Macaulay2/Macaulay2Doc/cache/rawdocumentation-abcd-8.db
```

`gdbm_load` is the GDBM utility that turns a dump file into a
binary GDBM database.

## Install target

```makefile
install:
	mkdir -p $(DESTDIR)$(datadir)
	cp -r share/* $(DESTDIR)$(datadir)
	mkdir -p $(DESTDIR)$(libdir)
	cp -r lib/* $(DESTDIR)$(libdir)
```

Standard `$(DESTDIR)` pattern — staging install into a temporary
prefix, supporting packagers' staged builds.

## When to use

- **Producing a doc-only distribution tarball** (release manager).
- **Installing pre-built docs** into a Linux package layout.
- **Re-installing docs after rebuilding** without full M2 rebuild.

Not typical for end users; mostly a packager / release tool.

## Used by

- Release-tarball production for the doc-distribution.
- Some Linux packagers who split M2 into `macaulay2` and
  `macaulay2-doc` packages.

## Related

- [`README.md`](README.md) — M2/ overview.
- [`file-Makefile-in.md`](file-Makefile-in.md) — sister top-level
  Makefile.
- [`Macaulay2/m2/file-installPackage.md`](Macaulay2/m2/file-installPackage.md)
  — produces the `dump/*.dump` files this consumes.
