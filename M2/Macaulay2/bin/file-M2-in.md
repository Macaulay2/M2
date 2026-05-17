# `M2.in` — `M2` shell-wrapper template

`M2.in` is the **autotools template** for the `M2` shell wrapper.
On Linux/macOS the installed `M2` is actually a tiny shell script
that sets `LD_LIBRARY_PATH` (or platform equivalent) and then
execs the real `M2-binary` ELF executable.

Part of the [`bin/` directory](README.md).

[← bin/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## The whole file

```sh
#! /bin/sh
# We assume that @ pre_bindir @/.. is ${pre_exec_prefix} (see configure.ac)
case "@host_os@" in
   cygwin) export PATH=`dirname "$0"`/../@tail_librariesdir@:$PATH ;;
  darwin*) # Mac OS X:
           export DYLD_LIBRARY_PATH=`dirname "$0"`/../@tail_librariesdir@:$DYLD_LIBRARY_PATH ;;
        *) export   LD_LIBRARY_PATH=`dirname "$0"`/../@tail_librariesdir@:$LD_LIBRARY_PATH ;;
esac
exec `dirname "$0"`/M2@EXE@ "$@"
```

Eight lines that do four things:

1. **Determine the platform** (`@host_os@` filled in by autotools
   to `linux-gnu`, `darwin22.0`, etc.).
2. **Set the right library-path env-var** per platform:
   - cygwin → `PATH`
   - macOS → `DYLD_LIBRARY_PATH`
   - everything else → `LD_LIBRARY_PATH`
3. **Add M2's bundled `lib/` dir** so the embedded libraries are
   findable.
4. **Exec the real binary** (`M2-binary` on Linux,
   `M2-binary.exe` on Windows via `@EXE@`).

## Why a wrapper script?

M2 ships with bundled copies of:

- libgmp / libmpfr / libflint / libfactory / libgivaro / libfplll
  ... and other math libraries.

These bundled copies sit in `lib/` next to the binary. Without
the wrapper, the user would need to set `LD_LIBRARY_PATH`
themselves before running M2 — and forget to once, and they get
"library not found" errors.

The wrapper is essentially a tiny portable shim that does the
configuration the user shouldn't have to think about.

## Why not RPATH?

You *could* avoid the wrapper by linking the binary with the
right `RPATH`. M2 doesn't do this consistently because:

- RPATH support is platform-dependent (works differently on Linux
  vs macOS vs cygwin).
- The wrapper makes the env-var visible — useful for debugging.
- The wrapper-based approach is more portable across the matrix
  of platforms M2 supports.

## Template variables

| `@VAR@` | Filled in by |
|---|---|
| `@host_os@` | autoconf (config.guess) |
| `@tail_librariesdir@` | configure (relative path to libdir) |
| `@EXE@` | empty on Unix, `.exe` on Windows |

## Autotools only

The CMake build doesn't use `M2.in`. It generates an equivalent
wrapper via its own templating (or sets RPATH directly,
depending on policy).

## Used by

- The autotools install target.
- End users who run `M2` (they don't realise it's a wrapper).

## Related

- [`README.md`](README.md) — bin/ overview.
- [`file-main.md`](file-main.md) — `main.cpp` becomes
  `M2-binary` (or `M2.exe`).
- `configure.ac` — defines the autotools variables filled in here.
