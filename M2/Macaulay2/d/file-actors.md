# `actors.d`, `actors2.dd`, `actors3.d`, `actors4.d`, `actors5.d` — built-in operators

The `actors*` family of files implements **the built-in operators** —
the M2-level `+`, `*`, `==`, `if`, `for`, `try`, `..`, and many
others. Each `Actor` is a function-pointer-shaped value the
evaluator dispatches to when it sees the corresponding operator in
an `Expr` AST.

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Why split across five files

The `actors*` files are split across multiple `.d` (and one `.dd`)
files because:

- **Compilation-time** — splitting reduces per-file translation cost.
- **Dependencies** — different operators need different `use`
  imports (e.g. `actors2.dd` is `.dd` because it needs C++-side
  string utilities; `actors4.d` deals with GMP integers and gets
  pulled in late).
- **Conceptual grouping** — each file roughly holds a category of
  operators.

The split is mostly historical. Operators are added wherever they
fit; there's no strict rule.

## What an "Actor" is

An **Actor** is a value of type `function pointer + metadata` that
the evaluator dispatches to when handling a specific operator. For
the M2 expression `x + y`, the evaluator:

1. Parses the AST node for `+`.
2. Looks up the registered `Actor` for `+`.
3. Calls the actor with arguments `(x, y)`.
4. Returns the actor's result.

Each actor takes `Expr`s, dispatches on their types, performs the
operation, and returns an `Expr`.

## Categories (rough)

| File | Approximate scope |
|---|---|
| `actors.d` | Foundational — arithmetic on basic types |
| `actors2.dd` | String + sequence operators (C++ for STL) |
| `actors3.d` | Control flow (`if`, `for`, `while`, `try`) |
| `actors4.d` | GMP integer / rational operators |
| `actors5.d` | Larger operators (matrix arithmetic, etc.) |

The boundaries are fuzzy — operators sometimes move between files
during refactors.

## How actors are registered

Each actor file ends with calls like:

```d
setupop("+", Actor(plusop));
setupop("*", Actor(timesop));
```

`setupop("name", actor)` registers the actor under the operator
name in the global symbol table. The M2 parser then knows that
`x + y` parses to a function call with `+`'s actor as the callee.

## Used by

- [`file-evaluate.md`](file-evaluate.md) — dispatches to actors.
- Every M2 expression that uses an operator.

## Related

- [`README.md`](README.md) — d/ overview.
- [`file-evaluate.md`](file-evaluate.md) — primary consumer.
- [`file-binding.md`](file-binding.md) — actors are stored as
  `Symbol`s.
- [`../m2/file-methods.md`](../m2/file-methods.md) — M2-side method
  layer that builds on top of actors.
