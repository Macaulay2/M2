# `aring-qq.hpp` — `ARingQQ` typedef and dispatcher

`aring-qq.hpp` is a **tiny header** whose only job is to choose a
default `ARingQQ` from among the available QQ implementations and
typedef it for the rest of the engine.

Part of the [Coefficient rings](coefficient-rings.md) area.

[← per-area: coefficient-rings](coefficient-rings.md) · [← engine overview](README.md)

## Full content

```cpp
#include "aring-qq-flint.hpp"
#include "aring-qq-gmp.hpp"

namespace M2 {
class ARingQQFlint;
class ARingQQGMP;

typedef ARingQQGMP ARingQQ;
};
```

Three lines of substance:

1. Pull in both QQ-backed aring headers
   ([`file-aring-qq-flint.md`](file-aring-qq-flint.md) and
   [`file-aring-qq-gmp.md`](file-aring-qq-gmp.md)).
2. Forward-declare both concrete classes.
3. **`typedef ARingQQGMP ARingQQ`** — choose GMP as the default.

The choice of `ARingQQGMP` over `ARingQQFlint` for `ARingQQ` is
historical; FLINT-backed QQ is now faster in most cases but the
typedef hasn't been flipped to avoid disturbing existing code paths.
Either implementation is fully functional and exposed by name.

## Why a typedef at all

Lots of engine code wants to say "the QQ aring" without picking GMP
vs. FLINT. The typedef provides a single name; the actual
implementation is decided in this one file. To switch defaults
engine-wide, the only edit needed is here.

This is the same pattern as
[`file-res-moninfo.md`](schreyer-resolution/file-res-moninfo.md) for
the dense vs. sparse `ResMonoid`: a header that's nothing but a
dispatcher typedef.

## What "ARingQQ" gets used for

References to `ARingQQ` in engine code mean "whichever QQ
implementation this header points at." Examples:

- `DMat<M2::ARingQQ>` — dense QQ matrix.
- `template<>` specialisations against `ARingQQ` in `mat-linalg.hpp`.

To force a specific backend, code can use `ARingQQFlint` or
`ARingQQGMP` directly.

## Related

- [`coefficient-rings.md`](coefficient-rings.md) — area overview.
- [`file-aring-qq-flint.md`](file-aring-qq-flint.md) — FLINT QQ.
- [`file-aring-qq-gmp.md`](file-aring-qq-gmp.md) — GMP QQ.
- [`file-aring.md`](file-aring.md) — `aring` framework.
