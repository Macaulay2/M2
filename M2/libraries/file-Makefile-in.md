# `Makefile.in` — top-level libraries-build driver

`Makefile.in` at the top of `libraries/` is the **driver
Makefile** that orchestrates building every library /
program in this directory. Loops over `LIBLIST` and `PROGLIST`
from `../configure.ac` and dispatches into each subdirectory's
Makefile.

Part of [`libraries/`](README.md).

[← libraries/ overview](README.md) · [← top-level repo TOC](../../README.md)

## Header

```makefile
VPATH = @srcdir@
include ../include/config.Makefile
BUILT_TARGETS = patch fetch all check unconfigure reinstall
ALL_TARGETS = $(BUILT_TARGETS) clean distclean
all:fetch
$(ALL_TARGETS): Makefile.library
$(foreach d,@LIBLIST@ @PROGLIST@ M2 Macaulay2-docs,	\
	$(foreach t, $(ALL_TARGETS),			\
		$(eval $t-in-$d:; + $$(MAKE) -C $d $t)	\
		$(eval .PHONY: $t $t-in-$d)))
$(foreach d,@BUILDLIST@,			\
	$(foreach t, $(BUILT_TARGETS),		\
		$(eval $t: $t-in-$d)		\
		$(eval .PHONY: $t $t-in-$d)))
```

The double-nested `$(foreach ... $(eval ...))` is heavy makefile
metaprogramming. It generates **rules at make-evaluation time**:

```
patch-in-flint:
	+ $(MAKE) -C flint patch
fetch-in-flint:
	+ $(MAKE) -C flint fetch
...
```

— one set per library × target combination.

## The three loops

1. **Generate per-target / per-dir rules** — every library /
   program gets every standard target.
2. **Hook `BUILDLIST` libraries into top-level targets** —
   libraries actually being built (not just system-installed)
   wire up `patch:` to depend on `patch-in-flint`, etc.
3. **Hook `LIBLIST` / `PROGLIST` clean targets** — even
   system-installed libraries can be cleaned (removing their
   downloaded source).

## Variables filled in by configure

| `@VAR@` | Source |
|---|---|
| `@LIBLIST@` | configure.ac list of libraries |
| `@PROGLIST@` | configure.ac list of programs |
| `@BUILDLIST@` | which need building from source |
| `@srcdir@` | source-tree path |
| `@INSTALL@` | install command (`install -m 644`) |
| `@TAR@` | tar command |
| `@pre_programsdir@` | install-prefix for programs |
| `@pre_licensesdir@` | install-prefix for licenses |

## Top-level targets

```makefile
all:install-programs install-licenses
install-programs:
	: installing programs in @pre_programsdir@
	...
install-licenses:
	: installing library and submodule licenses in @pre_licensesdir@
	...
install-dynamic-libraries:
	...
```

After all libraries are built and installed into
`$(BUILTLIBPATH)`, these top-level targets copy artefacts into
their final install locations.

## Usage

```sh
cd M2/BUILD/build
make -C libraries -j$(nproc)         # build all
make -C libraries check               # run test suites
make -C libraries clean-in-flint      # clean one library
```

The latter pattern is *very* useful when iterating on a library
patch — clean just that lib, leave the rest of the tree.

## Used by

- The autotools build (`make` from the top-level build dir).
- Developers iterating on library patches.

## Related

- [`README.md`](README.md) — libraries/ overview.
- [`file-Makefile-library-in.md`](file-Makefile-library-in.md) —
  shared recipe each library includes.
- [`file-Makefile-template.md`](file-Makefile-template.md) — new-lib
  template.
- `../configure.ac` — populates `@LIBLIST@` / `@PROGLIST@`.
