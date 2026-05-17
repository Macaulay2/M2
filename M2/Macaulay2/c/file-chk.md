# `chk.c`, `chk.h` — semantic analysis / type checker

`chk.c` is the **type-checker** of `scc1` — walks the AST produced
by [`grammar.y`](file-grammar.md), checks types, resolves overloads,
and produces a fully-decorated AST ready for code emission.

Part of the [`c/` scc1 translator](README.md).

[← back to c/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's exported

```c
node lookupfunction(node fun, node argtypes);
bool inside_defun(scope);
node chkprogram(node e);
node chktype(node,scope);
node chk(node, scope);
node chklist(node e, scope v);
```

- **`chkprogram(e)`** — top-level entry. Returns the
  type-checked program.
- **`chk(node, scope)`** — recursive case: check one expression
  in the given scope.
- **`chktype(node, scope)`** — variant for type expressions
  (where type-level evaluation matters).
- **`lookupfunction(fun, argtypes)`** — overload resolution.

## Top-of-file helper

```c
static bool isjump(node e){
     return (iscons(e) && (equal(car(e),return_S) || equal(car(e),goto__S))) || equal(e,break_S);
```

A small predicate the type-checker uses: "is this expression a
control-flow jump?" Matters because jumps don't have an ordinary
result type — they can complete *any* type because they don't
fall through.

## Type checking rules

The main rules `chk` implements:

| `.d` form | Check |
|---|---|
| `x := e` | infer `T = typeof(e)`, bind `x` to type `T` |
| `x:T := e` | check `typeof(e) <: T`, bind `x:T` |
| `e1 + e2` | dispatch on `(typeof(e1), typeof(e2))` |
| `f(a, b)` | overload-resolve on `f` with arg types of `a, b` |
| `when e is x:T do body` | check `typeof(e)` covers `T`; check `body` in scope with `x:T` |
| `Ccode(t, ...)` | trust `t` as the return type; don't recurse into the C body |

## Overload resolution

`.d` allows multiple definitions with the same name and different
argument types:

```d
plus(x:int, y:int):int := x + y;
plus(x:string, y:string):string := concat(x, y);
```

`lookupfunction(plus, (int, int))` finds the first; with
`(string, string)` finds the second. The C output uses the mangled
name to disambiguate at the C level.

## Why "chk" not "typecheck"

Historical naming. The file has been here since 1993; the short
abbreviation was a tighter convention.

## Used by

- [`file-scc1.md`](file-scc1.md) — calls `chkprogram` in `main()`.
- [`file-cprint.md`](file-cprint.md) — consumes the type-checked
  AST `chk` produces.

## Related

- [`README.md`](README.md) — c/ overview.
- [`file-type.md`](file-type.md) — type predicates / constructors
  `chk` uses.
- [`file-dictionary.md`](file-dictionary.md) — scope lookup.
- [`file-error.md`](file-error.md) — error reporting from
  type-check failures.
