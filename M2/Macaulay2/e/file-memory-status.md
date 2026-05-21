# `memory-status.cpp` — placeholder memory-stats hooks

`memory-status.cpp` is an **almost-empty placeholder file** that
exposes three trivial `int`-returning functions intended for future
memory-statistics reporting. Today they return constant sentinels.

Part of the [Utilities](utilities.md) area.

[← per-area: utilities](utilities.md) · [← engine overview](README.md)

## Full content

```cpp
int memorystat1(void) { return 123;   }
int memorystat2(void) { return 1234;  }
int memorystat3(void) { return 12345; }
```

The three functions return distinct constants (123, 1234, 12345),
suggesting they are placeholders for "report X bytes for stat Y" that
hasn't been implemented yet.

## Why this file exists

The three function names are referenced from the interpreter side
(via `.d` glue) as part of M2's `memory()` builtin reporting. Removing
the file would force changes throughout the build; leaving the
placeholders lets the build keep working while the real
implementation is deferred.

The constants 123 / 1234 / 12345 are obvious sentinels — they should
never appear in real memory reporting. A user who runs
`memory()` and sees these values is effectively learning that "this
stat hasn't been implemented yet."

## Status

A long-running TODO. There is no compelling reason to wire real
numbers up to these functions, because:

- bdwgc already exposes detailed memory stats through its own
  reporting API.
- The engine's own [`file-mem.md`](file-mem.md) (`stash`) allocator
  exposes its own counters per type.
- `memorystat1/2/3` was the original design for a single unified
  view; it was never finished.

## Related

- [`utilities.md`](utilities.md) — area overview.
- [`file-mem.md`](file-mem.md), [`file-myalloc.md`](file-myalloc.md)
  — alternative memory-stats sources.
- bdwgc submodule under [`../../submodules/README.md`](../../submodules/README.md)
  — has its own stats API.
