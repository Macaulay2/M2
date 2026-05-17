# `M2/Macaulay2/c/` — the `scc1` translator

This directory contains the source for **`scc1`**, the custom compiler-compiler
that translates Macaulay2's home-grown `.d` / `.dd` language into ordinary C
(`.c`) and C++ (`.cpp`). It is the first stage of the [four-language
stack](../../../README.md#the-four-language-stack):

```
.d / .dd  ──scc1──▶  .c / .cpp  ──C/C++──▶  M2-interpreter
   ▲
   │ defines the language scc1 reads
[Macaulay2/c/]   ← you are here
```

`scc1` is built as a standalone binary first, then invoked by the build system
on every `.d` and `.dd` source file in [`Macaulay2/d/`](../d/README.md) before
the C/C++ compiler ever sees them.

## What is `.d`?

A safer C-like language with sum types, type-cased pattern matching (`when … is
…`), garbage-collected pointers, and an `Ccode(t, …)` escape hatch for inline
C. The complete syntax reference is in the plain-text [`README`](README) file
in this directory — that file is the spec, this one is the overview.

The `.dd` variant compiles to C++ instead of C; everything else is the same.

## Files

| File | Role |
|---|---|
| `scc1.c`, `scc1.h` | Driver / entry point for the translator |
| `scc.h`, `scc-core.c`, `scc-core.h` | Core data structures shared by the front end and the code generator |
| `grammar.y`, `grammar.h` | Yacc/Bison grammar for the `.d` language |
| `keywords.h` | Reserved word table consumed by the lexer |
| `type.c`, `type.h` | Type system (`Type`, `atomicType`, sum types) |
| `dictionary.c`, `dictionary.h` | Symbol table / scope management |
| `cprint.c`, `cprint.h` | C/C++ code emitter — the back end |
| `error.c`, `error.h` | Translator-side error reporting |
| `chk.c`, `chk.h` | Semantic checks |
| `list.c`, `list.h` | Generic linked-list helpers used throughout the translator |
| `readfile.c`, `readfile.h` | Source file reader |
| `compat.c`, `compat.h` | Portability shims |
| `debugging.c`, `debugging.h` | Debug-only diagnostics |
| `foo.d` | Tiny `.d` smoke-test input |
| `README` | The `.d` language specification (plain text) |
| `COPYRIGHT` | License notice |
| `CMakeLists.txt`, `Makefile.in` | Build glue |

## How it fits in

- **Inputs:** `.d` / `.dd` files from [`Macaulay2/d/`](../d/README.md).
- **Outputs:** generated `.c` / `.cpp` plus `.sig` signature files and `.dep`
  dependency makefiles, all consumed by the regular C/C++ build.
- **Gotcha (autotools):** when `.sig` files become stale, `make` may need to
  run twice — the first pass corrects the dependency files, the second uses
  them. See the to-do list in [`README`](README) for context.

## Related

- The next stage of the pipeline: [`d/`](../d/README.md) (interpreter sources
  written in `.d`).
- The garbage collector that `.d`-generated code targets: bdwgc, vendored as a
  [submodule](../../submodules/README.md).
- Build wiring: `M2/cmake/scc.cmake` (CMake) and `M2/configure.ac` (autotools)
  invoke `scc1` once it is built.

[← back to repository TOC](../../../README.md#under-m2macaulay2)
