# `ffi.d` — generic foreign-function interface (libffi)

`ffi.d` implements M2's **generic FFI** — bindings to
[libffi](https://sourceware.org/libffi/) that let M2 call into
arbitrary shared libraries at runtime, without writing per-library
M2-side bindings.

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```d
use system;
use common;
use hashtables;
use evaluate;

header "#include <dlfcn.h>
    #include <ffi.h>
    /* FFI_BAD_ARGTYPE not introduced until libffi 3.4 in 2021 */
```

Two external libraries:

- **`<dlfcn.h>`** — `dlopen` / `dlsym` for loading shared libraries
  at runtime.
- **`<ffi.h>`** — libffi for calling foreign functions with dynamic
  signatures.

The "FFI_BAD_ARGTYPE not introduced until libffi 3.4" comment
captures version-compatibility work for older libffi.

## What FFI enables

```m2
foreignSymbol("/usr/lib/libm.so.6", "sin")
-- a callable M2 function
```

A user can:

1. Load a shared library (`dlopen`).
2. Look up a symbol (`dlsym`).
3. Describe its signature (return type + argument types) via libffi
   primitives.
4. Call it.

Plus all the type-marshalling: M2 `Number` ↔ C `double`, M2
`String` ↔ C `char *`, etc.

## Why FFI

Without FFI, calling into a library requires hand-writing a `.d`
bridge file. FFI lets users do it dynamically with no rebuild.
Used by:

- The `ForeignFunctions` user package.
- Quick experiments with external libraries.
- Bridges to research libraries that don't yet have proper M2
  packages.

## Limitations

- **No callbacks from C into M2** — libffi supports them, but the
  M2 wrapping doesn't expose this.
- **Manual lifetime management** — the user must keep loaded
  libraries alive.
- **No struct passing** — pass-by-pointer only.

These limitations are why a serious M2 ↔ external-library bridge
typically gets its own dedicated `.d` file instead.

## Used by

- The `ForeignFunctions` user package.
- Research code experimenting with new dependencies.

## Related

- [`README.md`](README.md) — d/ overview.
- [`file-python.md`](file-python.md), [`file-mysql.md`](file-mysql.md),
  [`file-xml.md`](file-xml.md) — purpose-built FFI siblings.
- libffi — external linked library.
- `ForeignFunctions` user package.
