# `M2/Macaulay2/d/` — the interpreter

**See [`architecture.md`](architecture.md)** for the standalone architectural reference (five-stage pipeline, `Expr` sum type, engine boundary, error/interrupt model, memory model, FFI bindings, "how to extend the interpreter").

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

## Per-file deep dives

### Bootstrap and main loop

| File doc | Subject |
|---|---|
| [`file-M2.md`](file-M2.md) | `M2.d` — top-level interpreter module declarations |
| [`file-M2lib.md`](file-M2lib.md) | `M2lib.c` — interpreter `main()` / startup glue |
| [`file-main.md`](file-main.md) | `main.cpp` — C++ entry point |
| [`file-interp.md`](file-interp.md) | `interp.dd` — top-level interpreter loop |
| [`file-scclib-c.md`](file-scclib-c.md) | `scclib.c` — `scc1` runtime support |

### Lexer / parser / evaluator

| File doc | Subject |
|---|---|
| [`file-lex.md`](file-lex.md) | `lex.d` — the M2 lexer |
| [`file-tokens.md`](file-tokens.md) | `tokens.d` — token / `Symbol` / `Word` types |
| [`file-parse.md`](file-parse.md) | `parse.d` — parser-level declarations |
| [`file-parser.md`](file-parser.md) | `parser.d` — actual parser (with stdio) |
| [`file-expr.md`](file-expr.md) | `expr.d` — operations on `Expr` |
| [`file-binding.md`](file-binding.md) | `binding.d` — name binding and the symbol table |
| [`file-evaluate.md`](file-evaluate.md) | `evaluate.d` — the M2 expression evaluator |

### Built-in operators

| File doc | Subject |
|---|---|
| [`file-actors.md`](file-actors.md) | `actors.d`, `actors2.dd`, …, `actors5.d` — built-in operators |

### I/O

| File doc | Subject |
|---|---|
| [`file-stdio.md`](file-stdio.md) | `stdio.d`, `stdio0.d`, `stdiop.d`, `stdiop0.d` — buffered I/O |

### Engine boundary

| File doc | Subject |
|---|---|
| [`file-engine-dd.md`](file-engine-dd.md) | `engine.dd` — interpreter ↔ engine bridge |

### Numeric foundations

| File doc | Subject |
|---|---|
| [`file-gmp.md`](file-gmp.md) | `gmp.d`, `gmp1.d` — GMP integer / rational bindings |
| [`file-ballarith.md`](file-ballarith.md) | `ballarith.d` — FLINT/Arb ball arithmetic |

### Errors and interrupts

| File doc | Subject |
|---|---|
| [`file-err.md`](file-err.md) | `err.d`, `errio.d` — error reporting |
| [`file-interrupts.md`](file-interrupts.md) | `interrupts.d` — Ctrl+C / signal handling |
| [`file-atomic.md`](file-atomic.md) | `atomic.d`, `atomic2.d` — atomic operations |

### Data structures

| File doc | Subject |
|---|---|
| [`file-hashtables.md`](file-hashtables.md) | `hashtables.dd` — `HashTable` / `MutableHashTable` |

### Threading

| File doc | Subject |
|---|---|
| [`file-threads.md`](file-threads.md) | `threads.dd` — M2-level `Task` API |

### FFI and external libraries

| File doc | Subject |
|---|---|
| [`file-python.md`](file-python.md) | `python.d` — CPython embedding |
| [`file-ffi.md`](file-ffi.md) | `ffi.d` — libffi generic FFI |
| [`file-xml.md`](file-xml.md) | `xml.d` — libxml2 bindings |
| [`file-regex-dd.md`](file-regex-dd.md) | `regex.dd` — Boost.Regex bindings |
| [`file-json.md`](file-json.md) | `json.d` — JSON parser (Jansson) |
| [`file-mysql.md`](file-mysql.md) | `mysql.d`, `mysqldummy.d` — MySQL client bindings |
| [`file-boostmath.md`](file-boostmath.md) | `boostmath.dd` — Boost.Math special functions |
| [`file-texmacs.md`](file-texmacs.md) | `texmacs.d` — TeXmacs frontend bindings |

### Shared infrastructure

| File doc | Subject |
|---|---|
| [`file-classes-dd.md`](file-classes-dd.md) | `classes.dd` — M2 root type registry |
| [`file-common.md`](file-common.md) | `common.d` — shared helpers |
| [`file-util.md`](file-util.md) | `util.d` — argument-checking helpers |
| [`file-system.md`](file-system.md) | `system.d` — POSIX / OS bindings |
| [`file-equality.md`](file-equality.md) | `equality.dd` — engine-aware equality |
| [`file-version.md`](file-version.md) | `version.dd` — build / version constants |

### Strings, nets, sets

| File doc | Subject |
|---|---|
| [`file-strings.md`](file-strings.md) | `strings.d`, `strings1.d`, `varstrin.d` — strings |
| [`file-nets.md`](file-nets.md) | `nets.d`, `varnets.d` — 2D character grids |
| [`file-sets.md`](file-sets.md) | `sets.dd` — `Set`, `Tally`, `VirtualTally` |
| [`file-buckets.md`](file-buckets.md) | `buckets.dd` — Dictionary bucket iteration |

### Profiling, debugging, timing

| File doc | Subject |
|---|---|
| [`file-profiler.md`](file-profiler.md) | `profiler.dd` — line-level profiler |
| [`file-debugging.md`](file-debugging.md) | `debugging.dd` — interactive debugger backend |
| [`file-chrono.md`](file-chrono.md) | `chrono.dd` — wall-clock / CPU timing |

### Engine boundary (top-level wrappers)

| File doc | Subject |
|---|---|
| [`file-engine-interfaces.md`](file-engine-interfaces.md) | `interface.dd`, `interface2.d`, `monoid.dd`, `monomial_ordering.dd` |
| [`file-pthread.md`](file-pthread.md) | `pthread.d`, `pthread0.d` — POSIX threads bindings |

### Low-level foundations

| File doc | Subject |
|---|---|
| [`file-arithmetic.md`](file-arithmetic.md) | `arithmetic.d` — integer type aliases |
| [`file-basic.md`](file-basic.md) | `basic.d` — universal `hash(Expr)` |
| [`file-convertr.md`](file-convertr.md) | `convertr.d` — late-bound function-pointer registry |
| [`file-ctype.md`](file-ctype.md) | `ctype.d` — character classification table |
| [`file-getline.md`](file-getline.md) | `getline.d` — `getLine(file)` primitive |
| [`file-vararray.md`](file-vararray.md) | `vararray.d` — variable-length `int` arrays |
| [`file-xmlactors.md`](file-xmlactors.md) | `xmlactors.d` — XML operator overloads |

### C / C++ glue

| File doc | Subject |
|---|---|
| [`file-c-glue.md`](file-c-glue.md) | `M2mem.{c,h}`, `M2types.c`, `types.h`, `gmp_aux`, `memdebug`, `gdbm_interface`, `xml-c`, `python-c`, `boost-regex` — hand-written C/C++ glue |

## Original notes

The plain-text [`README`](README) preserved in this directory is the
original to-do list — many items have been resolved over time.

## What lives here

The interpreter is essentially a collection of inter-dependent `.d` "packages."
Each file uses `export` to publish names and `use foo;` to import them. After
translation, signature (`.sig`) files declare cross-file types — sometimes
`make` needs to run twice on the autotools build when these get stale.

For the broader catalogue of files (and quick navigation to those not yet
documented), see the
[original layout in `M2/Macaulay2/d/README`](README) and the [repository TOC](../../../README.md#under-m2macaulay2).

## Related

- [`Macaulay2/c/`](../c/README.md) — the `.d` language and `scc1` translator.
- [`Macaulay2/e/`](../e/README.md) — the C++ engine linked into the interpreter.
- [`Macaulay2/m2/`](../m2/README.md) — Core M2 sources that run on top of the interpreter.

[← back to repository TOC](../../../README.md#under-m2macaulay2)
