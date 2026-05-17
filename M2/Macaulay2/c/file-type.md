# `type.c`, `type.h` — `.d` type-system functions

`type.c` and `type.h` implement the **`.d` type system** —
type predicates, type construction, sum-type discrimination, and
the constant-folding helpers used during semantic analysis and
code generation.

Part of the [`c/` scc1 translator](README.md).

[← back to c/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's exported

```c
bool is_atomic_memory(node type);
node arrayElementLength(node arraytype);
bool israwtype(node e);
bool pointer_to_atomic_memory(node type);
node typedefinition(node);
node ormemberindex(node, node);
```

These functions are called from
[`chk.c`](file-chk.md) (semantic analysis) and
[`cprint.c`](file-cprint.md) (code generation).

## Type concepts in `.d`

The `.d` language has:

- **Atomic types** — primitives like `int`, `char`, `bool`. No
  GC scanning needed.
- **Pointer types** — wrap a GC-managed object. Scanned.
- **Struct types (`{...}`)** — record types.
- **Array types (`array(T)`)** — homogeneous arrays.
- **Sum types (`A or B or C`)** — discriminated unions.
- **Function types** — including curried / multi-arg.

The functions in `type.c` answer "is this type atomic?", "what's
the element type of this array?", "which member-index in this
sum type does this case correspond to?".

## `is_atomic_memory`

Critical for **GC efficiency**: an `array(int)` should allocate
into atomic GC pages (`getmem_atomic`) so the GC doesn't scan it
for pointers. `is_atomic_memory(T)` decides whether `T` is fully
atomic, partially atomic, or pointer-bearing.

## `ormemberindex`

For sum types, every variant gets a tag. `ormemberindex(sumtype,
membertype)` returns the tag index. Used in code generation:

```d
when x is i:int do branch1 is s:string do branch2
```

becomes:

```c
switch (x->tag) {
case 0:  /* int */    ... branch1 ...
case 1:  /* string */ ... branch2 ...
}
```

where `0` / `1` come from `ormemberindex`.

## A representative function

```c
node integer(int n){
     node q = newnode(INT_CONST,int_const_tag);
     q->body.int_const.contents = strperm(intToString(n));
     return q;
```

Builds a fresh `INT_CONST` node — used throughout `chk.c` to
synthesise integer constants during semantic analysis.

## Used by

- [`file-chk.md`](file-chk.md) — every typecheck rule.
- [`file-cprint.md`](file-cprint.md) — code generation needs
  type info to emit casts, allocators, GC hints.
- [`file-grammar.md`](file-grammar.md) — parser uses constructors
  like `integer(n)` to build literal nodes.

## Related

- [`README.md`](README.md) — c/ overview.
- [`file-chk.md`](file-chk.md) — main consumer.
- [`file-cprint.md`](file-cprint.md) — secondary consumer.
- [`file-scc-h.md`](file-scc-h.md) — `node` definition.
