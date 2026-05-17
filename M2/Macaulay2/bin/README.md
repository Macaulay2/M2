# `M2/Macaulay2/bin/` — final binary linkage and startup shim

This directory produces the final `M2` executable. It is the smallest of the
core source dirs — its job is to combine three things:

1. the **interpreter** (built from [`d/`](../d/README.md)),
2. the **engine** (built from [`e/`](../e/README.md)),
3. a **startup shim** that locates Macaulay2's data files at runtime,

and link them into a single binary along with the bundled startup script.

## Files

| File | Role |
|---|---|
| `main.cpp` | `int main()` — the program entry point. Wires interpreter + engine and hands control to `interp.dd` |
| `startup.c.cmake` | Template for `startup.c`. The CMake module [`M2/cmake/startup.cmake`](../../cmake/) substitutes paths into this template at configure time so the binary knows where its installed `share/`, `lib/`, etc. live. Autotools has the equivalent in `M2.in` |
| `M2.in` | The autotools-side wrapper script (used in development builds when running an uninstalled `M2`) |
| `timestamp.cpp`, `timestamp.h` | Records build timestamp and configure-time metadata so `version` reports something useful |
| `Makefile.in`, `CMakeLists.txt` | Build glue for both build systems |
| `README` | Brief original notes |

## How the runtime locates its data

Macaulay2 needs to find a bundled startup script (`startup.m2` generated from
[`m2/startup.m2.in`](../m2/startup.m2.in)) plus a tree of `.m2` files at runtime.
Two strategies coexist:

- **Installed binary:** `startup.c` is generated with the install prefix baked
  in by `cmake/startup.cmake`.
- **Uninstalled dev build:** `M2.in` is a shell wrapper that sets environment
  variables so the in-tree `M2-binary` can still find everything.

This is why you usually want to use `M2/BUILD/build/M2` (which is the wrapped
form) rather than running the inner `M2-binary` directly.

## Related

- [`M2/cmake/startup.cmake`](../../cmake/) — generates `startup.c` from
  `startup.c.cmake`.
- [`Macaulay2/system/`](../system/README.md) — the supervisor process
  (`M2-supervisor`) that wraps `M2` for threading purposes.
- [`Macaulay2/d/`](../d/README.md) and [`Macaulay2/e/`](../e/README.md) — the
  interpreter and engine linked in here.

[← back to repository TOC](../../../README.md#under-m2macaulay2)
