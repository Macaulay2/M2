# `M2/Macaulay2/c/` — the `scc1` translator

**See [`architecture.md`](architecture.md)** for the standalone architectural reference (pipeline, `node` tagged-union AST, layered design, "how to extend `scc1`").

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

| File | Role | Deep dive |
|---|---|---|
| `scc1.c`, `scc1.h` | Driver / entry point | [`file-scc1.md`](file-scc1.md) |
| `scc.h`, `scc-core.c`, `scc-core.h` | Core data structures & runtime support | [`file-scc-h.md`](file-scc-h.md) |
| `grammar.y`, `grammar.h`, `keywords.h` | Bison grammar + reserved words | [`file-grammar.md`](file-grammar.md) |
| `type.c`, `type.h` | `.d` type system | [`file-type.md`](file-type.md) |
| `dictionary.c`, `dictionary.h` | Symbol table / scope | [`file-dictionary.md`](file-dictionary.md) |
| `cprint.c`, `cprint.h` | C/C++ code emitter | [`file-cprint.md`](file-cprint.md) |
| `chk.c`, `chk.h` | Type checker | [`file-chk.md`](file-chk.md) |
| `list.c`, `list.h` | Linked-list helpers | [`file-list.md`](file-list.md) |
| `readfile.c`, `readfile.h` | Source file reader | [`file-readfile.md`](file-readfile.md) |
| `error.c`, `error.h` | Error reporting | [`file-error.md`](file-error.md) |
| `compat.c`, `compat.h` | Portability shims | [`file-compat.md`](file-compat.md) |
| `debugging.c`, `debugging.h` | Debug helpers | [`file-debugging.md`](file-debugging.md) |
| `foo.d` | Smoke-test input | [`file-foo.md`](file-foo.md) |
| `README` | The `.d` language specification (plain text) | — |
| `COPYRIGHT` | License notice | — |
| `CMakeLists.txt`, `Makefile.in` | Build glue | — |

**Coverage:** every source file in this directory has a dedicated deep-dive doc.

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
