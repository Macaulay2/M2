# `Makefile.in` — top-level autotools driver

`Makefile.in` at `M2/` is the **top-level autotools Makefile
template** that gets substituted by `config.status` into the
working `Makefile`. Drives the full M2 build, install, package,
and test from autotools.

Part of [the M2 source root](README.md).

[← M2/ overview](README.md) · [← repo root](../README.md)

## Header

```makefile
# @configure_input@
.NOTPARALLEL:
include include/config.Makefile
VPATH = @srcdir@
.PRECIOUS:@srcdir@/configure config.status
.PHONY: unconfigure-libs reconfigure configure-help report-M2-location help \
	scripts dist doc-dist
all install:										\
	check-make config.status configured check-for-undefined-configure-variables	\
	srcdir protect-configs configured-files check-machine				\
	M2 all-in-subdirs report-M2-location
configured-files: @CONFIGURED_FILES@
$(foreach f,@CONFIGURED_FILES@,$(eval $f: @srcdir@/$f.in; ./config.status $f))
M2:
	rm -f M2
	(echo '#! /bin/sh'; echo 'exec @pre_bindir@/M2 "$$@"') >M2
	chmod a+x M2
define package
all install-$1: all-$1
install: install-$1
install-$1 all-$1:; $(MAKE) -C Macaulay2/packages $$@
endef
$(foreach i, $(PACKAGES), $(eval $(call package,$i)))
```

The opening lines set the stage:

- **`@configure_input@`** — substituted to "Makefile.  Generated
  from Makefile.in by configure." (autotools courtesy boilerplate).
- **`.NOTPARALLEL:`** — top-level recipes run serially. Inner
  Makefiles parallelise.
- **`include include/config.Makefile`** — pulls in build-time
  constants
  ([`include/file-configuration-in.md`](include/file-configuration-in.md)).
- **`VPATH = @srcdir@`** — supports out-of-tree builds (`make`
  from a build dir, source elsewhere).
- **`.PRECIOUS:`** — don't delete `configure` / `config.status`
  on partial failure.

## The `all install:` target

```makefile
all install:										\
	check-make config.status configured check-for-undefined-configure-variables	\
	srcdir protect-configs configured-files check-machine				\
	M2 all-in-subdirs report-M2-location
```

Both `make` and `make install` share the same dependency list:

1. **`check-make`** — verify GNU Make ≥ 3.81 (uses `order-only`).
2. **`config.status`** — ensure configure has run.
3. **`configured`** — sanity-check produced files.
4. **`check-for-undefined-configure-variables`** — catch `@VAR@`
   placeholders that didn't get substituted.
5. **`srcdir`** — verify source tree integrity.
6. **`protect-configs`** — back up configuration files.
7. **`configured-files`** — generate every `@CONFIGURED_FILES@`.
8. **`check-machine`** — verify host machine spec.
9. **`M2`** — produce the launcher shell script.
10. **`all-in-subdirs`** — descend into Macaulay2/, libraries/,
    etc.
11. **`report-M2-location`** — print where M2 ends up.

The launcher shell script:

```sh
#! /bin/sh
exec @pre_bindir@/M2 "$@"
```

— a one-line wrapper that just forwards to the installed binary.
Same idea as
[`Macaulay2/bin/file-M2-in.md`](Macaulay2/bin/file-M2-in.md) but
even simpler (no LD_LIBRARY_PATH munging — the real wrapper does
that).

## Per-package target generation

```makefile
define package
all install-$1: all-$1
install: install-$1
install-$1 all-$1:; $(MAKE) -C Macaulay2/packages $$@
endef
$(foreach i, $(PACKAGES), $(eval $(call package,$i)))
```

For each package in `$(PACKAGES)`, generate four rules:
`all-<pkg>`, `install-<pkg>`, plus dependencies on the top-level
`all` / `install` targets. Same metaprogramming pattern as
[`libraries/file-Makefile-in.md`](libraries/file-Makefile-in.md).

## Common make targets

| Target | Effect |
|---|---|
| `make` | Build everything |
| `make install` | Install to configure-time prefix |
| `make check` | Run test suites |
| `make clean` | Remove build artifacts |
| `make distclean` | Wipe configure-generated files |
| `make dist` | Produce `.tar.gz` source distribution |
| `make doc-dist` | Just the documentation distribution |
| `make help` | Print available targets |

## Used by

- The autotools build, after `configure` runs.
- Anyone running `make` from a build dir.

## Related

- [`README.md`](README.md) — M2/ overview.
- [`file-Makefile-doc-dist.md`](file-Makefile-doc-dist.md) —
  sister doc-only Makefile.
- [`file-configure-ac.md`](file-configure-ac.md) — produces
  `configure` that generates this Makefile.
- [`include/file-configuration-in.md`](include/file-configuration-in.md)
  — provides `config.Makefile` included here.
- [`libraries/file-Makefile-in.md`](libraries/file-Makefile-in.md)
  — sister Makefile pattern for library builds.
