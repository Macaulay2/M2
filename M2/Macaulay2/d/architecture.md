# Interpreter architecture

This document is the **architectural reference** for the
`M2/Macaulay2/d/` interpreter — the layer that becomes
`M2-interpreter` after `scc1` translation, and is linked against
the engine to produce the final `M2` binary.

[← d/ overview](README.md) · [← top-level TOC](../../../README.md#repository-architecture-table-of-contents)

## Position in the four-language stack

```
.d / .dd     ──scc1──▶   .c / .cpp     ──C/C++──▶   M2-interpreter ──▶ M2
                                                          ▲
                                                          │ linked
                                                      M2-engine (C++)
[Macaulay2/d/]   ← you are here
```

`scc1` ([`../c/architecture.md`](../c/architecture.md)) turns
the source files in this directory into C/C++ that the system
compiler then builds.

## The five-stage pipeline

The interpreter itself is a classic interpreter pipeline:

```
M2 source text
   │
   ▼ lex.d                          tokens
tokens
   │
   ▼ parser.d                       parse tree
parse tree
   │
   ▼ binding.d                      bound AST (Code)
bound AST (Code values)
   │
   ▼ evaluate.d                     Expr values
Expr values (runtime values)
   │
   ▼ stdio.d / formatter            text output
text output to user
```

Each stage lives in its own `.d` / `.dd` file. The boundary types
between stages are sum types ([`file-tokens.md`](file-tokens.md)
defines `Token`, `Code`, `Expr` — three different stages'
intermediate forms).

## The five concentric concerns

```
                ┌──────────────────────────────────────────────┐
                │   REPL / top-level                            │ ← interp.dd, main loop
                ├──────────────────────────────────────────────┤
                │   Built-in operators                          │
                │   (actors.d, actors2.dd, …, actors5.d)        │
                ├──────────────────────────────────────────────┤
                │   Lexer / parser / evaluator                  │
                │   (lex, tokens, parse, parser, expr,          │
                │    binding, evaluate)                         │
                ├──────────────────────────────────────────────┤
                │   Runtime types                               │
                │   (HashTable, Sequence, Set, Net, varstring)  │
                ├──────────────────────────────────────────────┤
                │   Primitives + FFI                            │
                │   (atomic, system, GMP, python, ffi, xml,     │
                │    libffi, MySQL, Jansson, Boost.Regex, …)    │
                └──────────────────────────────────────────────┘
```

## The `Expr` sum type

Every runtime value in M2 is an **`Expr`** — a discriminated
union of every possible kind of M2 value (`ZZcell`, `Sequence`,
`HashTable`, `SymbolClosure`, function, error, ...). Defined in
[`file-expr.md`](file-expr.md).

```d
Expr := Error or HashTable or Sequence or List or
        SymbolClosure or FunctionClosure or
        ZZcell or QQcell or RRcell or CCcell or
        Boolean or Nothing or stringCell or ...
```

The evaluator returns `Expr` everywhere. Pattern-matching via
`when ... is ... do ...` is the `.d`-native way to dispatch.

## Engine boundary

The interpreter calls into the C++ engine via:

```
m2/foo.m2          calls a Core method
   ↓
d/foo.dd           interpreter binding in .dd
   ↓
d/engine.dd        catch-all engine bridge (file-engine-dd.md)
   ↓ Ccode(...) escape
e/interface/foo.h  public C entry point
   ↓
e/foo.{cpp,hpp}    internal C++ implementation
```

See [`file-engine-dd.md`](file-engine-dd.md) for the bridge and
[`../e/architecture.md`](../e/architecture.md) for the engine
side.

## Error handling: flags, not exceptions

The `.d` translation pipeline can't easily propagate C++ exceptions
across the `scc1`-generated C boundary. Instead errors use a
**thread-local flag** mechanism ([`file-err.md`](file-err.md)):

1. A function sets `errorOccurred = true` and records the error.
2. The function returns a "sentinel" or the partially-completed
   result.
3. The caller checks the flag after every operation.
4. The interpreter's main loop sees the flag, formats and prints,
   resets, returns to prompt.

The same mechanism handles **interrupts** ([`file-interrupts.md`](file-interrupts.md))
— Ctrl-C sets a flag; inner loops poll regularly and bail out.

## Memory model

Everything is **GC-managed via Boehm GC** (bdwgc). `scc1`-emitted
code uses:

- `getmem(n)` — allocate `n` bytes, GC-tracked.
- `getmem_atomic(n)` — allocate `n` bytes of *leaf* data (no
  pointers to scan).
- Each `.d` type declaration produces matching `our_new_delete`-style
  hooks via the generated runtime.

External-library types (GMP `mpz_t`, MPFR `mpfr_t`, Python
`PyObject*`) get **finalisers** so external memory is released
when their wrapper is collected.

## FFI: bindings to external libraries

The interpreter is the FFI layer. Each external dependency has
one or two `.d` files:

| Library | File | Purpose |
|---|---|---|
| GMP | [`file-gmp.md`](file-gmp.md) | Integer / rational arithmetic |
| FLINT/Arb | [`file-ballarith.md`](file-ballarith.md) | Interval arithmetic |
| Boost.Math | [`file-boostmath.md`](file-boostmath.md) | Special functions |
| Python | [`file-python.md`](file-python.md) | CPython embedding |
| libffi | [`file-ffi.md`](file-ffi.md) | Generic FFI |
| libxml2 | [`file-xml.md`](file-xml.md) | XML parsing |
| Boost.Regex | [`file-regex-dd.md`](file-regex-dd.md) | Regular expressions |
| Jansson | [`file-json.md`](file-json.md) | JSON parsing |
| MySQL | [`file-mysql.md`](file-mysql.md) | Database access |
| Engine | [`file-engine-dd.md`](file-engine-dd.md) | M2 ↔ engine bridge |

Each follows the same pattern: declare the foreign types, expose
M2-friendly wrappers, handle conversion + cleanup.

## Threading

The interpreter exposes `Task` / `schedule` / `taskResult` to
M2 user code via [`file-threads.md`](file-threads.md). The
implementation delegates to the supervisor in
[`../system/file-supervisor.md`](../system/file-supervisor.md).
The supervisor manages a worker pool that runs M2 tasks in
parallel.

This is **separate from** the engine's TBB-based parallelism (in
`schreyer-resolution/file-res-dep-graph.md`) — the supervisor
parallelises *across* engine workloads, TBB parallelises *within*
one workload.

## C-side hand-written glue

A handful of files in this directory are hand-written C/C++
(not `.d` / `.dd`) because they need direct access to features
`.d` doesn't model well:

| File | Why hand-written |
|---|---|
| `M2lib.c` | `main()`-adjacent startup code |
| `M2mem.{c,h}` | Inline GC allocators |
| `M2types.c` | C ↔ M2 type conversions at the C ABI |
| `gmp_aux.{c,h}` | Custom GMP hash functions |
| `gdbm_interface.c` | GDBM database glue |
| `boost-regex.cpp` | Boost.Regex C++ glue |
| `xml-c.{c,h}` | libxml2 C glue |
| `python-c.c` | CPython C glue |

See [`file-c-glue.md`](file-c-glue.md) for the consolidated tour.

## How to extend the interpreter

Adding a new built-in operator:

1. **Tokenise it** in [`file-lex.md`](file-lex.md) and
   [`../c/file-grammar.md`](../c/file-grammar.md)`keywords.h`.
2. **Set precedence** in [`file-parser.md`](file-parser.md).
3. **Implement the runtime behaviour** in `actors*.d`
   ([`file-actors.md`](file-actors.md)).
4. **Document** in
   [`../packages/file-Macaulay2Doc.md`](../packages/file-Macaulay2Doc.md).

Adding a new FFI binding:

1. **Create `foo.d`** modelled on
   [`file-json.md`](file-json.md) or
   [`file-xml.md`](file-xml.md).
2. **Declare foreign types** with `Pointer "..."` /
   `Type "..."`.
3. **Wrap library calls** with `Ccode(...)` escapes.
4. **Handle errors** by setting the error flag.

## Related

- [`README.md`](README.md) — d/ navigation hub.
- [`../c/architecture.md`](../c/architecture.md) — the `scc1`
  translator that produces this layer's C output.
- [`../e/architecture.md`](../e/architecture.md) — the engine
  this layer links against.
- [`../m2/architecture.md`](../m2/architecture.md) — the Core
  M2 layer that runs on top of this interpreter.
- [`../../../README.md#the-four-language-stack`](../../../README.md#the-four-language-stack)
  — overview of all four layers.
