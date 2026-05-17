# `M2/Macaulay2/d/` — the interpreter

The Macaulay2 **interpreter** is implemented here. These files become the
`M2-interpreter` binary, which is then linked against the [C++ engine](../e/README.md)
to produce the final `M2` executable.

Position in the [four-language stack](../../../README.md#the-four-language-stack):

```
.d / .dd  ──scc1──▶  .c / .cpp  ──C/C++──▶  M2-interpreter ──▶ M2
                                                  ▲
                                                  │ linked
                                              M2-engine (C++)
[Macaulay2/d/]   ← you are here
```

Sources here are **mostly `.d` and `.dd`**, translated to C and C++ by
[`scc1`](../c/README.md). A few hand-written `.c`/`.cpp`/`.h` files supply
runtime glue.

## What lives here

The interpreter is essentially a collection of inter-dependent `.d` "packages."
Each file uses `export` to publish names and `use foo;` to import them. After
translation, signature (`.sig`) files declare cross-file types — sometimes
`make` needs to run twice on the autotools build when these get stale.

### Front-end (parsing & evaluation)

| File | Purpose |
|---|---|
| `lex.d` | Lexer |
| `parse.d`, `parser.d`, `tokens.d` | Parser and token definitions |
| `binding.d` | Name resolution / scope binding |
| `evaluate.d` | Tree-walking evaluator |
| `expr.d` | Expression AST representation |
| `interp.dd` | Top-level interpreter loop |
| `actors.d`, `actors2.dd`, `actors3.d`, `actors4.d`, `actors5.d` | Built-in operator implementations ("actors") split across files for compile-time reasons |

### Core runtime

| File | Purpose |
|---|---|
| `M2.d`, `M2lib.c` | Top-level startup and main-line glue |
| `M2mem.c`, `M2mem.h`, `memdebug.c`, `memdebug.h` | Memory allocation wrappers over bdwgc |
| `M2types.c`, `types.h` | C-side type definitions |
| `scclib.c` | scc1 runtime support library |
| `main.cpp` | C++ entry point |
| `err.d`, `errio.d` | Error handling and reporting |
| `debugging.dd`, `profiler.dd` | Debug/profile hooks |
| `interrupts.d` | Signal handling and user-interrupt logic |
| `system.d` | OS interaction (fork, exec, env) |
| `version.dd` | Embedded version string |
| `startup-header.h`, `startup-trailer.h` | Wrappers around the generated `startup.m2` |

### Data structures

| File | Purpose |
|---|---|
| `hashtables.dd` | Hash tables |
| `sets.dd` | Set type |
| `buckets.dd` | Mutable buckets used by polynomial arithmetic |
| `vararray.d`, `varstrin.d`, `varnets.d` | Variable-length arrays / strings / nets |
| `strings.d`, `strings1.d` | String type |
| `struct.d` | Generic struct support |
| `nets.d` | "Nets" — 2-D character grids used for pretty-printing |
| `classes.dd` | Class/type machinery exposed to the M2 language |

### I/O

| File | Purpose |
|---|---|
| `stdio.d`, `stdio0.d`, `stdiop.d`, `stdiop0.d` | Buffered I/O layer (split for bootstrapping reasons) |
| `getline.d` | Line input |
| `texmacs.d` | TeXmacs frontend protocol |
| `gdbm_interface.c` | GDBM database binding (used for the package info DB) |

### Numerics

| File | Purpose |
|---|---|
| `gmp.d`, `gmp1.d`, `gmp_aux.c`, `gmp_aux.h` | GMP integer/rational bindings |
| `ballarith.d` | Arb ball arithmetic |
| `boostmath.dd` | Boost.Math bindings |
| `arithmetic.d` | Generic numeric operations |
| `atomic.d`, `atomic2.d` | Atomic operations |

### Foreign function interface

| File | Purpose |
|---|---|
| `ffi.d` | libffi bindings (generic FFI) |
| `python.d`, `python-c.c`, `pythoncapi_compat.h` | CPython embedding |
| `mysql.d`, `mysqldummy.d` | MySQL client (dummy provided when MySQL is absent) |
| `xml.d`, `xml-c.c`, `xml-c.h`, `xmlactors.d` | libxml2 binding |
| `regex.dd`, `boost-regex.cpp` | Boost regex binding |
| `json.d` | JSON parser |
| `chrono.dd` | C++ `<chrono>` binding |

### Threads & concurrency

| File | Purpose |
|---|---|
| `pthread.d`, `pthread0.d` | POSIX threads binding |
| `threads.dd` | High-level threading primitives exposed to M2 |

### Engine bridge

| File | Purpose |
|---|---|
| `engine.dd` | Calls into the C++ engine via the C interface in [`e/engine.h`](../e/engine.h) and [`e/interface/`](../e/interface) |
| `interface.dd`, `interface2.d` | Higher-level wrappers used by the M2 layer |
| `monoid.dd`, `monomial_ordering.dd` | Monoid / monomial-order bindings |

### Misc

| File | Purpose |
|---|---|
| `basic.d`, `common.d`, `equality.d`, `convertr.d`, `ctype.d`, `lex.d`, `util.d` | Cross-cutting helpers |
| `getpagesize.h` | Portability shim |
| `Makefile.in`, `Makefile.files.in`, `CMakeLists.txt` | Build glue |
| `README` | Original TODO notes |
| `COPYRIGHT` | License notice |

## Workflow for adding a feature

If you are adding a new engine-backed operation:

1. Implement it in C++ in [`Macaulay2/e/`](../e/README.md).
2. Expose it through [`e/interface/<area>.{h,cpp}`](../e/interface).
3. Add the interpreter binding **here**, in `d/<area>.dd`.
4. Add the M2-language wrapper in [`m2/<area>.m2`](../m2/README.md).
5. Add a gtest in `e/unit-tests/<area>.cpp`.

## Related

- [`Macaulay2/c/`](../c/README.md) — the `.d` language and `scc1` translator.
- [`Macaulay2/e/`](../e/README.md) — the C++ engine linked into the interpreter.
- [`Macaulay2/m2/`](../m2/README.md) — Core M2 sources that run on top of the interpreter.

[← back to repository TOC](../../../README.md#under-m2macaulay2)
