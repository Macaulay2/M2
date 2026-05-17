# `atomic.d` / `atomic2.d` — atomic operations primitives

`atomic.d` and `atomic2.d` implement **atomic operations** — the
low-level thread-safe integer / pointer manipulation primitives
the supervisor and the interrupt machinery depend on.

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```d
use arithmetic;

declarations "
  #include \"M2/atomic-field.h\"
  #ifdef __cplusplus
    #include <atomic>
    using std::atomic_fetch_add;
    using std::atomic_signal_fence;
```

Two atomic interfaces wrapped:

- **C side** — `M2/atomic-field.h` (in
  [`../../include/M2/`](../../include/README.md)) provides the
  `atomic_field` struct and `test_Field`/`set_Field` macros.
- **C++ side** — `std::atomic` from C++11. `atomic2.d` is the
  C++-shaped variant.

The C / C++ split matches the `.d` / `.dd` distinction: `atomic.d`
compiles to C and uses `atomic_field`; `atomic2.dd` compiles to C++
and uses `std::atomic`.

## What atomic operations are needed

Thread-safe operations the interpreter needs:

- **Atomic counter increment** — for thread-local IDs.
- **Atomic flag set / test** — for `interruptPending`,
  `alarmedFlag`.
- **Memory barriers** — `atomic_signal_fence` to enforce ordering
  between the signal handler and main-line code.

These are the primitives `atomic.d` exposes. Without them, the
thread-local flag pattern in
[`file-interrupts.md`](file-interrupts.md) would have torn-write
race conditions on weakly-ordered platforms (ARM, POWER).

## Used by

- [`file-interrupts.md`](file-interrupts.md) — interrupt flag.
- [`file-threads.md`](file-threads.md) — `AtomicInt` type.
- [`../system/README.md`](../system/README.md) — supervisor
  coordination.
- Whenever `threadLocal` is used.

## Related

- [`README.md`](README.md) — d/ overview.
- [`../../include/M2/`](../../include/README.md) — `atomic-field.h`
  source.
- `pthread.d` / `pthread0.d` — POSIX threads (use atomics
  underneath).
- [`file-threads.md`](file-threads.md) — M2-level threading API.
