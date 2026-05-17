# `Makefile.in` — configure sanity-check

`Makefile.in` in `check-configure/` is the **only file** here.
Provides a `check-config` target that **re-runs the configure
script in a sandbox** to verify it correctly detects M2's
pre-installed libraries.

Part of [`check-configure/`](README.md).

[← check-configure/ overview](README.md) · [← top-level repo TOC](../../README.md)

## What's in the Makefile

```makefile
VPATH = @srcdir@
LDFLAGS = -L$(BUILTLIBPATH)/lib
CPPFLAGS = -I$(BUILTLIBPATH)/include
SYSTEM_MEMTAILOR = $(if $(filter memtailor, @BUILDLIBLIST@),no,yes)
SYSTEM_MATHIC    = $(if $(filter mathic   , @BUILDLIBLIST@),no,yes)
SYSTEM_MATHICGB  = $(if $(filter mathicgb , @BUILDLIBLIST@),no,yes)
include ../include/config.Makefile
check: clean-and-config
clean:; rm -rf tmp
clean-and-config: check-config
check-config:
	rm -rf tmp
	mkdir tmp
	@ echo "make: [check-config] remark: the test below is important (only) for testing whether the configure script detects pre-installed libraries properly" >&2
	cd tmp &&								\
	    LD_LIBRARY_PATH="$(BUILTLIBPATH)/lib:$(LD_LIBRARY_PATH)"		\
	    PATH="$(BUILTLIBPATH)/bin:$(PATH)"					\
	    @abs_top_srcdir@/configure						\
	    PKG_CONFIG_PATH=$(BUILTLIBPATH)/lib/pkgconfig:$(PKG_CONFIG_PATH)	\
	    GFTABLESDIR=@PRE_GFTABLESDIR@					\
	    LDFLAGS="$(LDFLAGS)"						\
	    LIBS="@BUILTLIBS@"							\
	    CPPFLAGS="$(CPPFLAGS)"						\
	    CC="$(CC)"								\
	    CXX="$(CXX)"							\
	    CFLAGS="$(CFLAGS)"							\
	    CXXFLAGS="$(CXXFLAGS)"						\
	    --build="@build_alias@"						\
	    --with-system-memtailor=$(SYSTEM_MEMTAILOR)				\
	    --with-system-mathic=$(SYSTEM_MATHIC)				\
```

The `check-config` target:

1. **Clears `tmp/`** — fresh sandbox.
2. **Sets `LD_LIBRARY_PATH`** to find M2's freshly-built libraries.
3. **Re-runs `configure`** inside `tmp/`, pointing at the freshly
   built libraries in `BUILTLIBPATH`.
4. **Asks for `--with-system-*=yes` for libraries we just built**
   — checks that configure correctly *finds* the installed
   versions, vs. trying to build them again.

The comment inside the script explains the purpose:

> the test below is important (only) for testing whether the
> configure script detects pre-installed libraries properly

## Why this check matters

M2's build has a **subtle bootstrap issue**:

1. First pass — configure says "GMP missing, FLINT missing".
2. `build-libraries` builds them into `$(BUILTLIBPATH)/`.
3. Second pass — configure should now say "GMP found, FLINT
   found" and re-link against them.

If step 3 fails (e.g. due to a `pkg-config` path issue), the user
gets a confusing error: "FLINT not found" even after `make` just
built FLINT successfully.

`check-configure` catches this in CI: run a normal build, then
re-run configure in this isolated sandbox. If it can't find the
just-built libraries, fail the build.

## SYSTEM_FOO logic

```makefile
SYSTEM_MEMTAILOR = $(if $(filter memtailor, @BUILDLIBLIST@),no,yes)
```

If memtailor is **in `BUILDLIBLIST`** (i.e., it was built from
source), then `SYSTEM_MEMTAILOR=no` (it's *not* a system library —
it's M2's local build). Otherwise `yes`.

This three-state model (system / built / unavailable) handles
all combinations.

## Used by

- The autotools `make check` workflow.
- CI runs it after every build to catch configure regressions.

## Related

- [`README.md`](README.md) — check-configure/ overview.
- [`../file-configure-ac.md`](../file-configure-ac.md) — the
  configure script being tested.
- [`../libraries/README.md`](../libraries/README.md) — libraries
  whose detection is being verified.
- [`../file-Makefile-in.md`](../file-Makefile-in.md) — top-level
  Makefile that calls into here.
