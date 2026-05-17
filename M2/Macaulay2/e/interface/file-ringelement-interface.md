# `ringelement.{h,cpp}` (in `interface/`) — public C entry points for `RingElement`

`interface/ringelement.h` declares the **public C functions** the
interpreter uses to construct, query, and operate on
[`RingElement`](../file-relem.md) values.

Part of the [`interface/`](README.md) subdirectory.

[← interface overview](README.md) · [← engine overview](../README.md)

## Header shape

```c
#if defined(__cplusplus)
class Matrix;
class EngineMonomial;
class Ring;
class RingElement;
#else
typedef struct Matrix         Matrix;
typedef struct EngineMonomial EngineMonomial;
typedef struct Ring           Ring;
typedef struct RingElement    RingElement;
#endif
```

Forward-declares the four opaque types the routines deal with. Same
dual-mode `extern "C"` block as every other `interface/*.h`.

## Entry points

The functions cover the complete life cycle of a `RingElement`:

- **Construction** — `rawRingElement(R, i)` from an integer, `rawFromQQ`,
  `rawRingVar(R, i, e)` (the *i*-th generator raised to power *e*).
- **Arithmetic** — `rawAdd`, `rawSubtract`, `rawNegate`, `rawMult`,
  `rawDivide`, `rawPower`.
- **Conversion / projection** — `rawLeadCoefficient`, `rawLeadMonomial`,
  `rawTerms`, `rawCoefficient(f, m)`.
- **Promotion / lift** — `rawPromote(S, f)`, `rawLift(S, f)` — move a
  `RingElement` from one ring to a compatible super- or sub-ring.
- **Predicates** — `rawIsZero`, `rawIsUnit`, `rawIsHomogeneous`.

The corresponding `m2/` files are mostly [`m2/rings.m2`](../../m2/README.md),
`m2/enginering.m2`, and the specific ring-construction files
(`polyrings.m2`, `quotring.m2`, …).

## Engine-side dispatch

Every entry point begins by reading the input's `Ring*` pointer and
dispatching through it. `RingElement` carries no logic of its own;
arithmetic is `R->add(a.val, b.val)` and similar.

## Related

- [`README.md`](README.md) — interface overview.
- [`../file-relem.md`](../file-relem.md) — `RingElement` class.
- [`../ring-elements-and-maps.md`](../ring-elements-and-maps.md) — area
  overview.
- [`file-ring-interface.md`](file-ring-interface.md) — sibling ring
  construction API.
- [`file-ringmap-interface.md`](file-ringmap-interface.md) — sibling
  ring-map API.
