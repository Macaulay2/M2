# `freemodule.{h,cpp}` (in `interface/`) — public C entry points for `FreeModule`

`interface/freemodule.h` declares the **public C functions** the interpreter
uses to construct and query the engine's [`FreeModule`](../file-freemod.md)
class.

Part of the [`interface/`](README.md) subdirectory.

[← interface overview](README.md) · [← engine overview](../README.md)

## Header shape

```c
#if defined(__cplusplus)
class FreeModule;
class Matrix;
class Ring;
#else
typedef struct FreeModule FreeModule;
typedef struct Matrix     Matrix;
typedef struct Ring       Ring;
#endif

/**
   FreeModule interface routines

   A FreeModule in the engine is always over a specific
   ...
 */
```

The doc-comment header is a stub that gets filled in by each routine's
own description. The pattern matches every other `interface/*.h` file —
dual-mode declarations, forward-declared opaque classes, `extern "C"`
linkage.

## Entry points

The functions exposed here cover what the interpreter needs to build and
inspect free modules:

- **Construction** — `rawFreeModule(R, n)`, `rawSubmoduleOfFree`,
  `rawDirectSum`, `rawTensor`, `rawDual`.
- **Inspection** — `rawRank(F)`, `rawRing(F)`, `rawDegrees(F)`.
- **Schreyer orders** — `rawSchreyerOrder` and friends, which install a
  Schreyer order ([`file-schorder.md`](../file-schorder.md)) on a free
  module.

The constructors come in two flavours: pass a list of degrees explicitly,
or omit and let the ring's default degree apply.

## How operations dispatch

Inside `freemodule.cpp` each entry point validates inputs (ring
compatibility, degree-vector length) and constructs a `FreeModule*`. The
class itself is immutable; "modify" always means "return a new one."

The associated M2-level wrappers live in
[`m2/modules.m2`](../../m2/README.md) and `m2/modules2.m2`.

## Related

- [`README.md`](README.md) — interface overview.
- [`../file-freemod.md`](../file-freemod.md) — `FreeModule` implementation.
- [`../free-modules.md`](../free-modules.md) — free-module area overview.
- [`../file-schorder.md`](../file-schorder.md) — Schreyer order storage.
