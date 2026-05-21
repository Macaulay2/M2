# `tests.cpp` — supervisor self-tests

`tests.cpp` is the **supervisor's standalone test suite** —
sanity tests for `schedule` / `cancel` / `taskResult` that don't
require the engine or interpreter. Compiled into a separate test
binary, not part of `M2`.

Part of the [`system/` directory](README.md).

[← system/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```cpp
#include "supervisor.hpp"
#include "supervisorinterface.h"
#include <cassert>
#include <iostream>
#include <stdlib.h>
#include <M2/config.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

static volatile bool finished[2000][2000];

struct tuple
{
  int x, y;
};

static void* TS_Test1_Func(void* vtup)
{
  ...
}
```

The `finished[2000][2000]` matrix is the **test fixture**: 4
million booleans, each tracking whether one specific `(x, y)`
test task has completed.

## What gets tested

The main test pattern:

1. Spawn `N` × `M` tasks, each writing `finished[x][y] = true`.
2. Wait for all to complete.
3. Assert every cell is `true`.

This is sufficient to catch:

- **Lost tasks** — supervisor dropped a task without running it.
- **Duplicate runs** — supervisor ran a task twice.
- **Race conditions** — `finished` writes lost to torn-write.
- **Deadlocks** — test hangs (CI timeout catches it).

Additional tests cover:

- Cancellation: cancel running task, verify status.
- Result passing: task returns value, parent reads it.
- Nested tasks: task spawns sub-tasks.

## Why a standalone binary

The supervisor must be testable without M2's other layers:

- No engine (avoid pulling in 300+ files).
- No interpreter (avoid `.d` translation).
- No GC bootstrap (the tests don't need M2 memory model
  guarantees).

This isolation lets developers iterate on the supervisor quickly.
If `tests.cpp` passes, the supervisor's core invariants hold.

## Running

```sh
cd Macaulay2/system
make tests        # produces ./tests
./tests
```

(Exact commands depend on the build system in use.)

## Used by

- Supervisor developers verifying threading changes.
- Manual smoke-tests during system-layer refactors.

## Related

- [`README.md`](README.md) — system/ overview.
- [`file-supervisor.md`](file-supervisor.md) — what's being
  tested.
- [`../e/unit-tests/README.md`](../e/unit-tests/README.md) —
  sister test suite for the engine.
