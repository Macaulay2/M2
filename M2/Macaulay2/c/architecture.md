# `scc1` translator architecture

This document is the **architectural reference** for `scc1` —
M2's compiler-compiler that translates the `.d` / `.dd`
home-grown language into ordinary C and C++. The smallest of
the four language layers but the most quietly important: without
it the rest of the build doesn't happen.

[← c/ overview](README.md) · [← top-level TOC](../../../README.md#repository-architecture-table-of-contents)

## The translator pipeline

```
foo.d / foo.dd
   │
   ▼ readfile          (file-readfile.md)
in-memory source buffer
   │
   ▼ lexer + Bison      (file-grammar.md)
AST (tree of `node`s)
   │
   ▼ chk                (file-chk.md)
type-checked AST
   │
   ▼ cprint             (file-cprint.md)
foo.c / foo.cpp
   │
   ▼ + .sig + .dep files
ready for the C/C++ compiler
```

Each arrow is a separately-testable phase. The intermediate
representation is `node` (a tagged-union AST type) throughout.

## The single tagged-union: `node`

The whole translator is built around **one type**: `node`. It's a
tagged union representing every syntactic construct — integer
constants, string constants, function calls, types, function
bodies, `when` clauses, etc.

```c
struct node_struct {
  enum tag tag;
  union {
    struct int_const_body int_const;
    struct binary_op_body binary_op;
    struct function_body function;
    struct type_body type;
    ...
  } body;
};
typedef struct node_struct *node;
```

Every function in the translator takes and returns `node`. The
**Lisp-style list helpers** in [`file-list.md`](file-list.md) —
`car`, `cdr`, `cons`, `length`, `member` — operate on lists of
`node`s. Statement bodies are `cons`-cells; argument lists are
`cons`-cells; type tuples are `cons`-cells. Pervasive but
consistent.

## Why home-grown instead of LLVM / Clang as a frontend?

Three reasons:

1. **Age** — `scc1` predates widely-available LLVM. Macaulay2's
   interpreter was being written when ANSI C was new.
2. **Tight semantics** — `.d`'s sum types, exhaustive
   pattern-matching, and GC-aware pointer types let interpreter
   code be both fast and safe. Bolting equivalents onto C/C++
   would be painful.
3. **`Ccode(...)` escape hatch** — `.d` files can drop into C for
   one expression at a time, which is essential for FFI-heavy
   interpreter work. No mainstream language provides this
   cleanly.

The trade-off: M2 maintains its own translator. The trade-off
seems to be worth it because the translator is small (~14 source
files) and stable (mostly unchanged since 1993).

## The output contract

`scc1` produces three files per input:

| Output | Purpose |
|---|---|
| `foo.c` / `foo.cpp` | C/C++ source for the system compiler |
| `foo.sig` | "Signature" — types and exports, consumed by other `.d` files via `use foo;` |
| `foo.dep` | Makefile fragment listing dependencies |

The `.sig` mechanism is what makes cross-`.d`-file references
work. After running `scc1 foo.d`, other files can write `use foo;`
and refer to `foo`'s exported names — they read `foo.sig` to
know the types.

The make-twice problem: when `.sig` files become stale, `make`
may need to run twice — first pass updates `.sig`s, second pass
uses them.

## Layered architecture

```
┌──────────────────────────────────────────────────────┐
│   Driver: scc1.c, scc1.h                              │
│   (argument parsing, pipeline orchestration)          │
├──────────────────────────────────────────────────────┤
│   Phase 1: Source reading + lexing + parsing          │
│   readfile.{c,h}, grammar.y, grammar.h, keywords.h    │
├──────────────────────────────────────────────────────┤
│   Phase 2: Semantic analysis                          │
│   chk.{c,h}, type.{c,h}, dictionary.{c,h}             │
├──────────────────────────────────────────────────────┤
│   Phase 3: Code generation                            │
│   cprint.{c,h}                                        │
├──────────────────────────────────────────────────────┤
│   Cross-cutting: error reporting, debug, portability  │
│   error.{c,h}, debugging.{c,h}, compat.{c,h}, list.{c,h} │
├──────────────────────────────────────────────────────┤
│   Generated-file runtime (linked into output binary)  │
│   scc.h, scc-core.{c,h}                               │
└──────────────────────────────────────────────────────┘
```

The bottom layer (`scc-core`) is **shipped into the output** —
every `scc1`-generated `.c` file includes `scc-core.h` to get the
small runtime (GC allocator, fatal-error handler, etc.). The
other layers run *inside* `scc1` itself.

## Files at a glance

| File pair / file | Role | Deep dive |
|---|---|---|
| `scc1.{c,h}` | Driver / `main()` | [`file-scc1.md`](file-scc1.md) |
| `scc.h`, `scc-core.{c,h}` | Core types + generated-file runtime | [`file-scc-h.md`](file-scc-h.md) |
| `grammar.y`, `grammar.h`, `keywords.h` | Bison grammar | [`file-grammar.md`](file-grammar.md) |
| `type.{c,h}` | `.d` type system | [`file-type.md`](file-type.md) |
| `dictionary.{c,h}` | Symbol table / scope | [`file-dictionary.md`](file-dictionary.md) |
| `cprint.{c,h}` | C/C++ code emitter | [`file-cprint.md`](file-cprint.md) |
| `chk.{c,h}` | Semantic analysis | [`file-chk.md`](file-chk.md) |
| `list.{c,h}` | Lisp-style list helpers | [`file-list.md`](file-list.md) |
| `readfile.{c,h}` | Source-file reader | [`file-readfile.md`](file-readfile.md) |
| `error.{c,h}` | Error reporting | [`file-error.md`](file-error.md) |
| `compat.{c,h}` | Portability shims | [`file-compat.md`](file-compat.md) |
| `debugging.{c,h}` | Debug helpers | [`file-debugging.md`](file-debugging.md) |
| `foo.d` | Smoke-test input | [`file-foo.md`](file-foo.md) |

## How to extend `scc1`

Adding a new `.d` language feature requires touching three places:

1. **Grammar** ([`grammar.y`](file-grammar.md)) — recognise the
   new syntax, produce the appropriate `node`.
2. **Type checking** ([`chk.c`](file-chk.md)) — validate
   the new construct's semantics; reject ill-typed uses.
3. **Code generation** ([`cprint.c`](file-cprint.md)) — emit
   the corresponding C/C++.

If the feature needs runtime support, also touch:

4. **Runtime** ([`scc-core.c`](file-scc-h.md)) — add helper
   functions every generated file can call.

## Related

- [`README.md`](README.md) — c/ navigation hub.
- The plain-text [`README`](README) — the canonical `.d`
  language specification.
- [`../d/architecture.md`](../d/architecture.md) — the next
  pipeline stage that consumes `scc1`'s output.
- [`../../../README.md#the-four-language-stack`](../../../README.md#the-four-language-stack)
  — the four-language-stack overview.
