# `interrupted.{cpp,hpp}` — `system_interrupted()`

`interrupted.cpp` is a **tiny one-function module** exposing the
single global predicate `system_interrupted()` — true iff the user
has requested an interrupt (typically by pressing Ctrl+C while M2 was
running an engine computation). Every engine inner loop polls this
periodically and bails out when it returns true.

Part of the [Utilities](utilities.md) area.

[← per-area: utilities](utilities.md) · [← engine overview](README.md)

## Full source

```cpp
#include "../system/supervisor.hpp"
#include "../system/supervisorinterface.h"

#define interrupted() \
    test_Field(THREADLOCAL(interrupts_interruptedFlag, struct atomic_field))

bool system_interrupted() { return interrupted(); }
```

The whole file is a one-liner: define `system_interrupted()` as a
wrapper around the thread-local atomic flag the
[`system/` supervisor](../system/README.md) maintains.

## The interrupt mechanism

The chain from "Ctrl+C" to "engine bails out":

1. The user presses Ctrl+C. The OS sends `SIGINT`.
2. The [supervisor](../system/README.md) catches `SIGINT` and sets
   `interrupts_interruptedFlag` to true. This is an atomic write — no
   locking needed.
3. Engine inner loops periodically call `system_interrupted()`. The
   read is also atomic and very cheap (one test of an atomic flag).
4. When the flag is true, the inner loop returns with
   `COMP_INTERRUPTED` status
   ([`interface/file-computation-interface.md`](interface/file-computation-interface.md)).
5. The interpreter sees `COMP_INTERRUPTED`, surfaces the message to
   the user, and returns control.

## Where it's polled

Every engine inner loop that may run for more than a few milliseconds
checks `system_interrupted()`:

- [`file-gb-default.md`](file-gb-default.md), [`file-gauss.md`](file-gauss.md),
  [`file-hermite.md`](file-hermite.md) — top of each major loop.
- [`file-comp-res.md`](file-comp-res.md) — between homological levels.
- [`file-NAG.md`](file-NAG.md) — between path-tracking steps.
- All the F4 variants — between matrix passes.

The polling rate is a trade-off: too rare and the user waits too
long after Ctrl+C; too frequent and the atomic-read overhead becomes
non-negligible. The engine convention is roughly "every 1 ms of
expected work."

## Cross-thread safety

Because the supervisor and the worker thread that's polling are
different threads, the flag has to be a thread-safe atomic.
`THREADLOCAL(...)` and `test_Field(...)` come from the engine's
atomic-primitives layer in [`../system/`](../system/README.md).

## Related

- [`utilities.md`](utilities.md) — area overview.
- [`../system/README.md`](../system/README.md) — the supervisor that
  sets the flag.
- [`interface/file-computation-interface.md`](interface/file-computation-interface.md)
  — `COMP_INTERRUPTED` is in the status enum.
- [`file-computation-framework.md`](file-computation-framework.md) —
  every `Computation` respects this flag.
