# `aring.{cpp,hpp}` — the `aring` framework

`aring.hpp` defines the **abstract-ring framework** (`namespace M2`) that
unifies the engine's coefficient rings under one templated interface. Each
concrete coefficient ring (ZZ via GMP, ZZ via FLINT, Z/p via FFLAS, GF, RR,
CC, …) lives in its own `aring-*.{cpp,hpp}` pair; this file is the
**dispatcher and shared base** that ties them together.

Part of the [Coefficient rings](coefficient-rings.md) area.

[← per-area: coefficient-rings](coefficient-rings.md) · [← engine overview](README.md)

## Why two ring APIs

The original engine used a single virtual base class `Ring`. Virtual dispatch
was a major bottleneck for arithmetic-heavy code (every multiply through a
v-table). `aring` is a **template-based** alternative: ring-specific
operations are inlined per call site at compile time. The two APIs coexist —
existing code still uses `Ring*`, hot code uses `aring`. The bridges in
[`aring-glue.hpp`](aring-glue.hpp), [`aring-translate.hpp`](aring-translate.hpp),
and [`aring-wrap.{cpp,hpp}`](aring-wrap.cpp) move values across the boundary.

## Dispatch

The engine identifies an aring by a **type tag** (an `enum` in
[`coeffrings.hpp`](coeffrings.hpp)):

```cpp
enum RingID {
   ring_ZZ_GMP,
   ring_ZZ_FLINT,
   ring_QQ_GMP,
   ring_QQ_FLINT,
   ring_ZZp,
   ring_ZZp_FLINT,
   ring_ZZp_FFPACK,
   ring_GF,
   ring_GF_FLINT,
   ring_GF_FLINT_BIG,
   ring_RR,
   ring_RRR,
   ring_RRi,
   ring_CC,
   ring_CCC,
   ring_CCi,
   ring_tower,
   // ...
};
```

Each concrete `aring-foo.cpp` registers itself with the dispatcher; consumers
can either branch on the tag (cold paths) or use a template specialised on
the concrete type (hot paths).

## FLINT randomness compatibility

The header also papers over a FLINT API change:

```cpp
#ifdef HAVE_FLINT_RAND_INIT
#define FLINT_RAND_INIT(x) flint_rand_init(x)
#else
#define FLINT_RAND_INIT(x) flint_randinit(x)
#endif
```

This is the only file most aring users touch when bumping the FLINT submodule
version.

## Files that participate

See the table in [`coefficient-rings.md`](coefficient-rings.md). At a glance:

- **Glue / bridge**: `aring.hpp`, `aring-glue.hpp`, `aring-translate.hpp`,
  `aring-wrap.{cpp,hpp}`, `coeffrings.{cpp,hpp}`.
- **Concrete rings**: every `aring-<NAME>.{cpp,hpp}` pair.

## Related

- [`coefficient-rings.md`](coefficient-rings.md) — area overview, full file list.
- [`coeffrings.cpp`](coeffrings.cpp) — registry of ring tags.
- [`ring.hpp`](ring.hpp) — legacy virtual-base ring API.
- [`interface/aring.{h,cpp}`](interface/README.md) — public C interface.
