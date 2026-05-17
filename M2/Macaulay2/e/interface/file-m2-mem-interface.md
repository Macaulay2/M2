# `m2-mem.{h,cpp}` (in `interface/`) — engine memory allocation hooks

`interface/m2-mem.h` declares the **memory allocation hooks** the engine
provides to the interpreter and downstream consumers. It centralises the
`getmem` / `getmem_atomic` interface and adds debug-build instrumentation
for tracking allocations.

Part of the [`interface/`](README.md) subdirectory.

[← interface overview](README.md) · [← engine overview](../README.md)

## Debug instrumentation

```c
#ifndef NDEBUG

#if defined(__cplusplus)
extern "C" {
#endif

extern void  trap(void);
extern void *trapaddr;
extern int   trapcount;
extern int   trapset;
extern void  trapchk(void *);
extern void  trapchk_size(size_t);
extern int   badBlock();
```

The `trap*` helpers exist only in non-`NDEBUG` builds. A developer
chasing down a memory bug can:

1. Set `trapaddr` to the address of interest, or `trapset = 1` plus
   `trapcount = N` to trap on the *N*-th allocation.
2. Run the engine under a debugger.
3. The next call to `trapchk(ptr)` matching the criteria invokes
   `trap()`, which is a tiny noop function with a breakpoint set.

This is the engine's home-grown counterpart to `MALLOC_TRACE`,
`mtrace(3)`, or Valgrind's allocation hooks — useful when those tools
are too heavy for an issue.

## The `getmem` family

The header also declares the engine's standard allocation calls:

- `getmem(size_t)` — GC-managed allocation; may contain pointers.
- `getmem_atomic(size_t)` — GC-managed allocation guaranteed not to
  contain pointers (lets bdwgc skip scanning it).
- `getmem_clear(size_t)` — `getmem` + zero-initialise.

These delegate to bdwgc (`GC_malloc`, `GC_malloc_atomic`,
`GC_malloc_uncollectable`, …). The wrapper exists so the engine can
swap GC backends without touching every allocation call site.

## Why a C header

Allocation hooks have to be callable from both C and C++ code (the
`.d`-generated C glue, the `.cpp` engine, hand-written C parts). Plain
C with `extern "C"` linkage is the only common denominator.

## Related

- [`README.md`](README.md) — interface overview.
- [`file-gmp-util-interface.md`](file-gmp-util-interface.md) — `mpz_reallocate_limbs`
  uses `getmem_atomic`.
- [`../file-MemoryBlock.md`](../file-MemoryBlock.md) — non-GC bump
  allocator used in hot loops.
- bdwgc submodule under [`../../../submodules/README.md`](../../../submodules/README.md).
