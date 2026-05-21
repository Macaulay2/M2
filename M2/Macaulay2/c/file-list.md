# `list.c`, `list.h` — generic list helpers

`list.c` and `list.h` provide **Lisp-style list primitives** —
`car`, `cdr`, `cons`, `length`, etc. — operating on `node`. They
are the bedrock data-structure primitives `scc1` uses everywhere
to represent sequences (statement lists, argument lists, type
lists, etc.).

Part of the [`c/` scc1 translator](README.md).

[← back to c/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's exported

```c
bool member(node, node);
node car(node x);
node cdr(node x);
node cons(node x, node y);
int length(node x);
node last(node l);
```

Plus more not shown: `reverse`, `append`, `nth`, mapping variants.
Each takes / returns `node`.

## Lisp-shaped AST

Why Lisp lists? `scc1` predates the routine use of `std::vector`
or generic containers — it's 1993 C. Linked lists give:

- Constant-time push (cons).
- Easy reversal.
- Trivial recursive walks.

The AST itself uses lists pervasively: a function body is a
`cons` of statements; a sequence type is a `cons` of element
types; a `when` clause is a `cons` of cases.

## A representative function

```c
node car(node x){
     node y = CAR(x);
     return y;
     }
```

`CAR(x)` is a macro in `scc.h` extracting the head field. `car`
wraps it in a function so the address can be taken / used as a
callback. Same pattern for `cdr`, `cons`.

## `member` and search

```c
bool member(node, node);
```

Tests whether a `node` appears in a list (by `equal()`, which is
structural equality). Used heavily during scope resolution and
type comparisons.

## Used by

- Essentially every other `.c` file in this directory.
- [`file-chk.md`](file-chk.md), [`file-cprint.md`](file-cprint.md),
  [`file-dictionary.md`](file-dictionary.md), [`file-type.md`](file-type.md)
  all use list operations.

## Related

- [`README.md`](README.md) — c/ overview.
- [`file-scc-h.md`](file-scc-h.md) — `node` and `CAR`/`CDR`
  macros.
