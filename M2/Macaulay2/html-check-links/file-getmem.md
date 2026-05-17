# `getmem.c`, `getmem.h` — GC-aware allocator helpers

`getmem.c` and `getmem.h` provide the **memory allocator** the
HTML link checker uses — a thin wrapper over Boehm GC with
out-of-memory handling and a few syntactic helpers.

Part of [`html-check-links/`](README.md).

[← html-check-links/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## `getmem.c`

```c
#include <stdlib.h>
#include "html-check-links.h"
#include "getmem.h"
void outofmem() {
  fprintf(stderr,"out of memory\n");
  exit(1);
}
```

One function: print an error and exit on OOM. Called by the
inline allocators in `getmem.h` when `GC_malloc` returns NULL.

## `getmem.h`

```c
#ifndef GETMEM_H
#define GETMEM_H
#include <string.h>

#include <M2/gc-include.h>

extern void outofmem() __attribute__ ((noreturn));
static inline char *getmem(unsigned n) {
  char *x = GC_malloc(n);
  if (x == NULL) outofmem();
  return x;
}
#define new(type) (type *)getmem(sizeof(type))
#define newarray(type,n) (type *) getmem((n) * sizeof(type))
#define getmemfor(x) ((x) = (typeof(x))getmem(sizeof *x))
#define clearmem(x)  memset(&(x),0,sizeof(x))
static inline char *strperm(char *s) {
  char *t = getmem(strlen(s)+1);
  strcpy(t,s);
  return t;
```

The toolkit:

- **`getmem(n)`** — base allocator; calls `GC_malloc`.
- **`new(T)`** — sizeof-aware single-object allocation.
- **`newarray(T, n)`** — array allocation.
- **`getmemfor(x)`** — allocate matching the pointer type of `x`.
- **`clearmem(x)`** — zero out a struct.
- **`strperm(s)`** — GC-allocated copy of a C string.

The `__attribute__ ((noreturn))` on `outofmem()` tells GCC the
function never returns, so the compiler doesn't warn about
"missing return after `outofmem()`" in callers.

## Why Boehm GC for a tiny tool?

The link checker could use plain `malloc`/`free`. Using GC means:

- **No free()s needed** — simpler code, no leaks possible.
- **Consistency** — M2's other tools use GC; this one matches.
- **OK trade-off** — the tool runs briefly (seconds), GC overhead
  is irrelevant.

If the tool ran for hours, the analysis would shift; for a build-
time check, GC is the easier choice.

## Used by

- Every other `.c` file in this directory.
- [`file-buffer.md`](file-buffer.md) — `BUFFER(T)` calls
  `getmem` from generated code.

## Related

- [`README.md`](README.md) — html-check-links overview.
- [`file-buffer.md`](file-buffer.md) — consumer.
- Boehm GC (bdwgc) — external library.
