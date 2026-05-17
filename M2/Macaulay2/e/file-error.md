# `error.{c,h}` — engine error reporting

`error.c` is the engine's **error-reporting primitive**. It maintains a
thread-local error message string and provides the macros engine code
uses to raise errors that the interpreter surfaces to the M2 user.

Part of the [Utilities](utilities.md) area.

[← per-area: utilities](utilities.md) · [← engine overview](README.md)

## Why `.c` and not `.cpp`

`error.c` predates the engine's C++ refactor and is one of the few
remaining C-only files. The C linkage is intentional: error-reporting
needs to be callable from the `.d`-generated C glue, the `.cpp` engine
code, and (occasionally) external library callbacks. Keeping it C avoids
name-mangling concerns at every boundary.

## The error contract

The engine uses a **thread-local error message** rather than C++
exceptions across the C/engine boundary. The contract:

1. A `.cpp` engine function detects a problem.
2. It calls `ERROR(...)` (a macro defined in `error.h`).
3. The macro sets the thread-local error string, sets a "we have an
   error" flag, and returns control to the caller.
4. Every layer above checks the error flag after calling into the engine
   and propagates an early return.
5. The interpreter's main loop catches the propagation and renders the
   error to the user.

This pattern is used everywhere — overflow detection, ring incompatibility,
missing GB inputs, time-budget exhaustion, malformed monomials.

## API surface

The header declares (paraphrased):

- `ERROR(format, ...)` — printf-style error message setter.
- `error()` — query: has an error been set?
- `error_message()` — get the message.
- `clear_error()` — interpreter-side reset after surfacing the message.
- `error_oom()`, `error_internal()` — short-hands for common cases.

The `format`-string handling uses a small fixed buffer (the engine
deliberately avoids `std::ostringstream` here to keep the path
allocation-free in OOM scenarios).

## Cross-thread correctness

The thread-local nature of the message matters because the engine's
[`Computation`](file-computation-framework.md) machinery can run in
worker threads under the [`system/` supervisor](../system/README.md).
Each worker has its own error state; the supervisor checks before
returning to the interpreter.

## Related

- [`utilities.md`](utilities.md) — area overview.
- [`file-buffer.md`](file-buffer.md), [`file-text-io.md`](file-text-io.md) —
  used to *format* the error message before installing it.
- [`file-overflow.md`](file-overflow.md) — biggest single source of
  `ERROR(...)` calls (silent overflow detection).
- `M2_gbTrace` — the verbosity global the error path also writes through
  at high trace levels.
