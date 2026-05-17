# `aring.{h,cpp}` (in `interface/`) — public C entry points for `aring`-backed rings

`interface/aring.h` declares the **public C functions** that the interpreter
calls to construct rings backed by the [`aring`](../file-aring.md) framework.
These are the only stable, C-callable entry points for the abstract-ring
family.

Part of the [`interface/`](README.md) subdirectory.

[← interface overview](README.md) · [← engine overview](../README.md)

## Header shape

```c
#if defined(__cplusplus)
class Ring;
class RingElement;
#else
typedef struct Ring Ring;
typedef struct RingElement RingElement;
#endif

#if defined(__cplusplus)
extern "C" {
#endif

const Ring /* or null */ *rawARingZZp(unsigned long p);  /* connected */
/* Expects a prime number p in range 2 <= p <= 32749 */

const Ring /* or null */ *rawARingGaloisField1(const RingElement *prim);
const Ring /* or null */ *rawARingGaloisFieldFlintBig(...);
// ...

#if defined(__cplusplus)
}
#endif
```

Pattern: every function name starts with `raw` (the convention for engine
C entry points), takes plain C types (or opaque pointers to forward-declared
C++ classes), and is wrapped in `extern "C"` for the interpreter to call.

The dual `class` / `typedef struct` definition lets the header be included
from both `.cpp` files and the `.d`-generated C files.

## Entry points

The functions declared here construct each `aring`-backed coefficient ring:

| Function | Constructs |
|---|---|
| `rawARingZZp(p)` | Z/p via aring (table-based for small p) |
| `rawARingGaloisField1(prim)` | Native M2 Galois field |
| `rawARingGaloisFieldFlintBig(...)` | Galois field via FLINT, large extension |
| `rawARingGaloisFieldFlint(...)` | Galois field via FLINT, small extension |
| `rawARingRR(prec)` | RR with MPFR precision |
| `rawARingRRR(prec)` | Arbitrary-precision real |
| `rawARingCC(prec)`, `rawARingCCC(prec)` | Complex variants |
| `rawARingZZFlint()`, `rawARingQQFlint()` | FLINT-backed ZZ, QQ |
| ... | ... |

Each returns a `Ring*` wrapping the appropriate `aring` instance, or `null`
on construction failure (the interpreter then surfaces an error).

## The `connected` annotation

Comments like `/* connected */` indicate the function is wired all the way
through to a corresponding `.dd` binding in [`d/`](../../d/README.md). When
adding a new entry point, follow the workflow in the engine's
[`file-aring.md`](../file-aring.md) deep dive — declare here, implement in
`aring.cpp`, bind in `d/engine.dd`, expose at M2 level.

## Related

- [`README.md`](README.md) — interface overview.
- [`../file-aring.md`](../file-aring.md) — aring framework.
- [`../coefficient-rings.md`](../coefficient-rings.md) — area overview.
- [`../../d/engine.dd`](../../d/README.md) — interpreter side.
