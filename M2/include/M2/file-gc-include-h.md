# `gc-include.h` — Boehm GC + pthreads include wrapper

A 60-line header that pulls in the **Boehm-Demers-Weiser garbage
collector** (`bdwgc`) with all the configuration knobs M2 needs set
correctly. **Every file in the engine and interpreter that interacts
with GC should include this**, not `<gc.h>` directly.

`bdwgc` is one of the most macro-sensitive C libraries around — its
include order, thread model, and platform variant all matter — so
M2 funnels everything through this single header.

## What it sets up (in order)

```
include <pthread.h>          ← MUST come before <gc.h> per bdwgc docs
__need_sigset_t = 1
include <signal.h>           ← for sigset_t inside bdwgc's pthread shim
include <stdlib.h>           ← for size_t
include <string.h>           ← for memcpy

#define GC_LINUX_THREADS     ← when on Linux
#define _REENTRANT 1
#define GC_THREADS 1
#define GC_IGNORE_WARN 1     ← when NDEBUG
#define GC_INITIAL_HEAP_SIZE 70000000   ← ~70 MB warm-start

include <gc/gc.h>            ← finally the GC itself

#ifdef __cplusplus
  #define GC_NEW_ABORTS_ON_OOM
  include <gc/gc_cpp.h>      ← C++ overloaded new/delete
#endif
```

## Why each macro

| Macro | Effect |
|---|---|
| `GC_LINUX_THREADS` | Tell bdwgc to use Linux-specific thread support (LWP polling, signal-based stop-the-world). Set automatically by `#if defined(__linux__)`. |
| `_REENTRANT` | Activates thread-safe versions of libc functions (`strtok_r`, `localtime_r`). |
| `GC_THREADS` | The master switch enabling multi-threaded GC. |
| `GC_IGNORE_WARN` | Suppress bdwgc's runtime warnings about heap-growth heuristics when `NDEBUG` is set. Otherwise stderr fills up under load. |
| `GC_INITIAL_HEAP_SIZE = 70 MB` | Skip the slow ramp-up phase where bdwgc starts at ~256 KB and doubles. Can be overridden in `bin/main.cpp`. |
| `GC_NEW_ABORTS_ON_OOM` (C++ only) | Make `GC_new` abort instead of returning NULL — matches the engine's "out of memory means die" policy. |
| `__CYGWIN__` workaround | Prevent `gc_cpp.h` from re-defining global `new`/`delete` on Cygwin (it conflicts with M2's [`our_new_delete`](../../Macaulay2/e/file-newdelete.md)). |

## `IWYU pragma: begin_exports`

The whole block is marked as `IWYU pragma: begin_exports` /
`end_exports`, telling include-what-you-use that anything that
includes `gc-include.h` is considered to have included the inner
headers (`<pthread.h>`, `<gc/gc.h>`, etc.).

## Heap size tuning

The 70 MB initial heap is a deliberate compromise:

- Smaller (default ~256 KB) → many collections during the first
  second of any non-trivial M2 session → noticeable lag.
- Larger → wastes memory in lightweight one-shot sessions.

The number was picked empirically; see the comment in
[`../../Macaulay2/bin/file-main.md`](../../Macaulay2/bin/file-main.md)
about overriding it for `M2 -q --check 3` and other long runs.

## What goes wrong if you include `<gc.h>` directly

- Missing `_REENTRANT` ⇒ libc reentrancy bugs in multi-threaded
  builds.
- Missing `GC_THREADS` ⇒ collections happen on the wrong thread,
  manifesting as random "moved finalizer fired" warnings.
- Including before `<pthread.h>` ⇒ subtle race on macOS where
  bdwgc's signal handler is installed before `pthread`-aware
  signal masks are set up.

The single include point eliminates these bugs at the source.

## Consumers

Everything in `Macaulay2/e/`, `Macaulay2/d/`, and `Macaulay2/system/`
that touches GC. Grep the codebase: `#include <M2/gc-include.h>`
appears in ~100+ files.

The `engine-includes.hpp` umbrella header (see
[`../../Macaulay2/e/file-engine-includes-hpp.md`](../../Macaulay2/e/file-engine-includes-hpp.md))
re-pulls this in for the C side of the engine.

## See also

- [`file-M2-headers.md`](../file-M2-headers.md) — overview of M2's public headers
- [`README.md`](README.md) — `include/M2/` overview
- [Repo `MEMORY.md`](../../../MEMORY.md) — the 5-layer memory model bdwgc sits at the base of
- [`../../Macaulay2/e/file-newdelete.md`](../../Macaulay2/e/file-newdelete.md) — the C++ engine's GC integration that builds on top of this
- [`../../Macaulay2/bin/file-main.md`](../../Macaulay2/bin/file-main.md) — runtime heap-size override point
- [bdwgc upstream](https://github.com/ivmai/bdwgc) — vendored as submodule
