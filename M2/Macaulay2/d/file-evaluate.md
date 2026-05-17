# `evaluate.d` — the M2 expression evaluator

`evaluate.d` implements the **expression evaluator** — the bit of
the interpreter that takes an `Expr` AST (produced by
[`parser.d`](file-parser.md)) and computes the value it represents.
It is the M2 language runtime's core.

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```d
--		Copyright 1994-2003 by Daniel R. Grayson
-- put bindings to variables before the forward references, for safety
use hashtables;
use convertr;
use debugging;

-- evalprof is not defined until profiler.dd
-- so we use a pointer and populate it later.
dummyevalprof(c:Code):Expr := nullE;
export evalprofpointer := dummyevalprof;

export globalAssignmentHooks := newHashTableWithHash(mutableHashTableClass, nothingClass);
setupconst("globalAssignmentHooks", Expr(globalAssignmentHooks));
export threadLocal evalSequenceHadError := false;
export threadLocal evalSequenceErrorMessage := nullE;
```

Several pieces:

- **`evalprofpointer`** — function pointer to the profiler-aware
  evaluator. Initialised as a no-op; replaced when
  [`profiler.dd`](README.md) loads.
- **`globalAssignmentHooks`** — a hash table of hooks the user can
  install to run code when global variables are assigned.
- **`threadLocal evalSequenceHadError`** — per-thread error flag.
  The `threadLocal` keyword makes this safe for the supervisor's
  parallel computation.

## Tree-walking evaluator

The core of `evaluate.d` is a giant `eval(c:Code):Expr` function
that pattern-matches on every kind of AST node:

```d
when c
is null do ...
is i:Integer do ...
is s:Sequence do ...
is f:functionCall do (
    -- resolve f, then evaluate arguments, then dispatch
    ...
)
is a:Assign do ...
is i:ifexpr do ...
is f:forCode do ...
...
```

For each node type there is a corresponding branch that:

1. Recursively evaluates sub-nodes.
2. Performs the node's specific action.
3. Returns the result as an `Expr`.

This is the classic **tree-walking interpreter** pattern. M2 doesn't
JIT or compile to bytecode — every evaluation walks the AST every
time.

## Error propagation

When evaluation hits an error, `eval` returns an `Error`-variant
`Expr` rather than throwing. The pattern-matching cascade
short-circuits up the call stack on errors. `evalSequenceHadError`
is a thread-local flag higher-level callers consult.

## Profile hook

The `evalprofpointer` indirection lets the profiler instrument
every evaluation step without modifying the evaluator itself. When
profiling is enabled, the pointer is swapped to the
instrumented version which increments per-function counters before
delegating.

## Used by

- [`file-interp.md`](file-interp.md) — top-level read-eval-print
  loop calls `eval` once per top-level form.
- Every M2 expression at runtime.

## Related

- [`README.md`](README.md) — d/ overview.
- [`file-parse.md`](file-parse.md), [`file-parser.md`](file-parser.md)
  — producers of input `Expr`s.
- [`file-interp.md`](file-interp.md) — driver loop.
- [`file-expr.md`](file-expr.md), [`file-tokens.md`](file-tokens.md)
  — `Expr` and `Code` declarations.
- `profiler.dd` — install the profile hook.
- `actors*.d` — built-in operator implementations called from `eval`.
