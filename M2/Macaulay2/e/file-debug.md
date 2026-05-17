# `debug.{cpp,hpp}` — engine debug-printing helpers

`debug.cpp` is the engine's **debug-printing toolkit**: a set of `d…(…)`
helper functions, callable from a debugger or from instrumented engine
code, that pretty-print engine values to stdout. It is the engine's
equivalent of `gdb`-callable `print` helpers.

Part of the [Utilities](utilities.md) area.

[← per-area: utilities](utilities.md) · [← engine overview](README.md)

## Forward declarations

The header forward-declares essentially every printable engine type:

```cpp
class Matrix;
class RingElement;
class FreeModule;
class Ring;
class GBRing;
class gbvector;
class res_poly;
class res2_poly;
struct resterm;
struct res2term;
class MutableMatrix;
class MonomialIdeal;
```

That breadth is intentional. The helpers below take any of these and
emit a human-readable representation; the `d…` prefix keeps them easy
to remember at the debugger prompt.

## Helper functions

```cpp
void showint(mpz_srcptr a);

void dmatrix(const Matrix *M);
void drelem(const RingElement *f);
void dfree(const FreeModule *F);
extern "C" void dringelem(const Ring *R, const ring_elem f);
```

…and similar for `gbvector*`, `res_poly`, `MutableMatrix*`,
`MonomialIdeal*`, etc.

The `extern "C"` on `dringelem` is important: from a debugger you cannot
call C++-mangled names directly. The non-mangled helpers exist so you
can type `call dringelem(R, f)` in `gdb` and get readable output.

## Why these can't just be `text_out`

Every engine class already has a `text_out(buffer &)` virtual that
serialises it. The `d…` helpers are different in two ways:

1. **Direct stdout output** — `text_out` writes to a `buffer`; the `d…`
   helpers print to stdout (so a debugger sees the result without
   inspecting an in-memory buffer).
2. **Standalone linkage** — `text_out` requires a `buffer` instance,
   which means the type must be in scope; the `d…` helpers take only
   opaque pointers, so you can use them from a stack frame that doesn't
   have `buffer` available.

## Usage from a debugger

```text
(gdb) call drelem(some_ring_element_pointer)
(gdb) call dmatrix(M)
(gdb) call dringelem(R, f)
```

The output goes to the engine's stdout, formatted via
[`text-io`](file-text-io.md) and the engine's `text_out` methods.

## Related

- [`utilities.md`](utilities.md) — area overview.
- [`file-buffer.md`](file-buffer.md), [`file-text-io.md`](file-text-io.md) —
  underlying formatting layer.
- `M2_gbTrace` — the verbosity global controlling when production code
  emits debug traces.
