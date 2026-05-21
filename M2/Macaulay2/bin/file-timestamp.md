# `timestamp.cpp`, `timestamp.h` — build-time stamp

`timestamp.cpp` and `timestamp.h` are a **two-line trick** that
records the precise time M2 was compiled. The two files together
are just **3 lines of code** but they exist for a specific
reason: they force a rebuild every build.

Part of the [`bin/` directory](README.md).

[← bin/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## The entire files

```cpp
// timestamp.cpp
char timestamp[] = __DATE__ ", " __TIME__;
```

```cpp
// timestamp.h
extern char timestamp[];
```

That's it. `__DATE__` and `__TIME__` are C preprocessor macros
that expand to strings of the current compilation date and time.

## Why a dedicated file

If you put `timestamp[]` inline in another `.cpp` (e.g. `main.cpp`),
that whole file would have to be recompiled every build to stay
current. By isolating it in `timestamp.cpp` — a tiny file with
nothing else in it — only this one translation unit gets rebuilt.

The Makefile / CMake usually marks `timestamp.cpp` as
**always-out-of-date** (`.PHONY` or `force_rebuild`), so a fresh
timestamp lands in the binary every build.

## Where it's used

The `timestamp` global is shown:

- In M2's startup banner ("compiled at YYYY-MM-DD, HH:MM:SS").
- In bug-report output (`about M2` includes it).
- In version-info commands.

This gives a precise build identifier even when `git describe`
isn't available — useful for distinguishing locally-built copies.

## Used by

- [`file-main.md`](file-main.md) — `main.cpp` references
  `timestamp` directly.
- The version-reporting code in
  [`../d/file-version.md`](../d/file-version.md).

## Related

- [`README.md`](README.md) — bin/ overview.
- [`../d/file-version.md`](../d/file-version.md) — the larger
  `version` HashTable that includes this.
- [`file-main.md`](file-main.md) — primary consumer.
