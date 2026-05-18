# `atomic-field.h` — atomic-int field helper

A 22-line header providing a **portable atomic-int field type** used
by the interpreter's atomic types (see
[`d/atomic.d`](../../Macaulay2/d/file-atomic.md)).

The header solves one annoying problem: C and C++ have different
atomic APIs (C uses `<stdatomic.h>`, C++ uses `<atomic>` with
namespaced types), and the interpreter needs to use the same type
from both sides of the language boundary.

## What it provides

```c
struct atomic_field {
  atomic_int field;
};

#define load_Field(x)         atomic_load(&(x).field)
#define test_Field(x)         (load_Field(x) != 0)
#define store_Field(x, val)   atomic_store(&(x).field, val)
```

The three macros are the **only** way the rest of the codebase touches
the field — by going through these, callers don't have to know
whether they're in a C or a C++ translation unit.

## C vs C++ dispatch

The first `#ifdef __cplusplus` block aliases the C++ `<atomic>`
symbols into the same names the C side gets from `<stdatomic.h>`:

```c
#ifdef __cplusplus
  #include <atomic>
  using std::atomic_int;
  using std::atomic_load;
  using std::atomic_store;
#else
  #include <stdatomic.h>
#endif
```

After this block, the rest of the code can use `atomic_int`,
`atomic_load`, `atomic_store` without caring about the language.

## Why a `struct` wrapper, not a bare `atomic_int`?

C++'s `std::atomic<T>` is **non-copyable** in a way that surprises C
code. Wrapping it in a `struct` field gives us a regular
copyable-by-value containing type whose internal atomic state is
only touched through the macros — which know to take addresses
rather than copy.

## Consumers

- [`d/atomic.d`](../../Macaulay2/d/file-atomic.md) — exposes the
  M2-level atomic-int type used for interrupt flags, supervisor
  state, and a few cross-thread counters.
- [`d/interrupts.d`](../../Macaulay2/d/file-interrupts.md) — uses
  atomic fields for the interrupt-pending flag.

## Where it gets installed

Compiled into `$prefix/include/M2/atomic-field.h` so external code
that wants to interoperate with M2's atomic types can include it.

## See also

- [`file-M2-headers.md`](../file-M2-headers.md) — overview of M2's
  C/C++ public headers
- [`README.md`](README.md) — `include/M2/` overview
- [Repo `THREADING.md`](../../../THREADING.md) — where atomic-int
  flags fit into the threading model
- [`../../Macaulay2/system/file-mutex.md`](../../Macaulay2/system/file-mutex.md) — the supervisor-side concurrency primitives
