# `m2file.{cpp,hpp}`, `m2fileinterface.h` — per-thread file-handle state

`m2file.{cpp,hpp}` and `m2fileinterface.h` implement
**`M2File`** — a thread-safe wrapper around M2's file objects.
Each `M2File` carries per-thread output state so multiple threads
can write to the same file without scrambling each other's
output.

Part of the [`system/` directory](README.md).

[← system/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## What `M2File` is

```cpp
M2File::M2File(stdio0_fileOutputSyncState fileUnsyncState):
    currentThreadMode(0),
    unsyncState(fileUnsyncState),
    ownerChangeCondition(PTHREAD_COND_INITIALIZER),
    recurseCount(0),
    exclusiveRecurseCount(0)
{
  clearThread(syncOrExclOwner);
}
```

The constructor names the key state:

- **`currentThreadMode`** — sync vs unsync mode (synchronised or
  threadwise).
- **`unsyncState`** — per-file Net / 2D-output state when in
  unsync mode.
- **`ownerChangeCondition`** — condvar for switching modes.
- **`recurseCount` / `exclusiveRecurseCount`** — re-entrance
  counters for nested locks.

## Why multi-thread file state matters

Without coordination:

```
Thread 1: print "Computing GB of "
Thread 2: print "ideal "
Thread 1: print I.ideal
Thread 2: print "in R"
```

might produce `Computing GB of ideal I.idealin R`.

`M2File`'s **sync mode** acquires an exclusive lock so each `print`
call atomically writes its message — outputs interleave only at
message boundaries.

Its **unsync mode** is more elaborate: each thread maintains its
own Net (2D character array) state in `unsyncState`, and a sync
operation later flushes them in some canonical order.

## `m2fileinterface.h` — C bridge

```c
#ifdef __cplusplus
extern "C" {
#endif

  struct M2File;

  int M2File_GetThreadMode(struct M2File* file);
  void M2File_SetThreadMode(struct M2File* file, int threadMode);
  struct M2File*  M2File_New(stdio0_fileOutputSyncState fileUnsyncState);
  stdio0_fileOutputSyncState M2File_UnsyncState(struct M2File* file);
  ...
```

The C-callable wrapper. Interpreter `.d` code calls these:

```d
-- in d/stdio.d (or threads.dd)
M2File_SetThreadMode(file, modeSync);
```

The two-layer split (C++ class + C wrapper) lets the C++ side use
full RAII / condvars while the C side stays compatible with the
interpreter's `.d`-generated C code.

## How threads coordinate

```
print x:
   if file.threadMode == sync:
      lock(file.mutex)
      write(x.toString())
      unlock(file.mutex)
   else:  # unsync
      lookup_or_create per-thread state in file.unsyncState
      append x to that state's Net
      # caller flushes later via M2File_ReleaseState
```

The default for stdout / stderr is sync. Per-task output that
the user wants tagged-per-thread uses unsync.

## Used by

- [`../d/file-stdio.md`](../d/file-stdio.md) — interpreter
  primary consumer.
- M2 user code calling `print`, `<<`, etc. — they reach
  `M2File` through stdio.
- [`file-supervisor.md`](file-supervisor.md) — coordinates with
  threading.

## Related

- [`README.md`](README.md) — system/ overview.
- [`file-supervisor.md`](file-supervisor.md) — sister threading
  primitive.
- [`file-mutex.md`](file-mutex.md) — mutex / lock helpers.
- [`../d/file-stdio.md`](../d/file-stdio.md) — interpreter side.
