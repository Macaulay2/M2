# `foo.d` — smoke-test input

`foo.d` is a **tiny `.d` smoke-test file** used to sanity-check
`scc1` during development.

Part of the [`c/` scc1 translator](README.md).

[← back to c/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## The whole file

```d
header "int main () { return 0; }";

A := {+ x:int };
B := {+ y:int };
C := A or B;
f(x:C):int := when x is A do 1 is B do 2 else 3;	    -- oops!  This needs to be fixed.

threadLocal t := A(123);
```

A six-line program exercising:

- **`header`** — verbatim C escape.
- **Struct types** with `{+ x:int }`.
- **Sum types** with `A or B`.
- **`when ... is ... do ...`** — type-cased pattern match.
- **`threadLocal`** — thread-local storage qualifier.
- **A literal `main`** so the compiled output links to a runnable
  executable.

## "oops!" comment

The TODO comment marks a behaviour the developer found suspicious
but never fixed. Likely: the `else 3` branch should be a `chk`
error because `A or B` should be exhaustive after `is A` and
`is B`. Either way, the file documents an open issue rather than
hides it.

## How to use it

Running `scc1 foo.d` should produce `foo.c` that compiles cleanly
with the system C compiler. The cycle:

```sh
./scc1 foo.d > foo.c
cc foo.c -o foo
./foo
```

If `scc1` crashes, you have a regression. If the output `foo.c`
doesn't compile, you have a code-generator bug.

## What it doesn't cover

`foo.d` is tiny — it doesn't exercise:

- Multi-file `use ...; export ...;`.
- Complex function signatures with defaults.
- `Ccode` escapes.
- Garbage-collected pointer manipulation.

For richer test coverage, point `scc1` at any `.d` file in
[`../d/`](../d/README.md).

## Used by

- Developers smoke-testing `scc1` changes.
- The build system: `make foo` builds it as a basic sanity check.

## Related

- [`README.md`](README.md) — c/ overview.
- [`README`](README) — the plain-text `.d` language spec.
- [`file-scc1.md`](file-scc1.md) — the driver this exercises.
