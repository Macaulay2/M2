# `aring-glue.hpp` — `ConcreteRing<R>` (bridge between `aring` and legacy `Ring`)

`aring-glue.hpp` defines the **`ConcreteRing<R>` template** — the bridge
that lets every [`aring`-based](file-aring.md) coefficient ring appear
to the rest of the engine as a legacy [`Ring*`](file-polyring.md). It is
the single most important piece of glue holding the two parallel ring
APIs together.

Part of the [Coefficient rings](coefficient-rings.md) area.

[← per-area: coefficient-rings](coefficient-rings.md) · [← engine overview](README.md)

## What problem it solves

The engine has two coexisting ring APIs:

- **Legacy `Ring`** — virtual base class, virtual-dispatch arithmetic,
  used by most existing engine code.
- **`aring`** — templated, inlined arithmetic, used by newer hot paths.

If every consumer had to handle both, the codebase would double. Instead,
**every `aring` is automatically wrapped in a `ConcreteRing<R>`** that
implements the legacy `Ring` virtuals by forwarding to the `aring`. The
old API works against the new rings transparently.

## The template

```cpp
namespace M2 {

template <class RingType>
class ConcreteRing : public Ring {
    std::unique_ptr<RingType> R;
    // ...
};

}

static const bool displayArithmeticCalls = false;

#define COERCE_RING(RingType, R) dynamic_cast<const RingType *>(R)
```

For a concrete aring `RingType` (e.g. `ARingZZ` from
[`file-aring-zz-flint.md`](file-aring-zz-flint.md)):

- `ConcreteRing<RingType>` is a `Ring` subclass.
- It owns a `std::unique_ptr<RingType>` (the underlying aring instance).
- Every `Ring` virtual (`add`, `mult`, `from_int`, `to_string`, …)
  forwards to the `aring`'s corresponding inline method via the unique
  pointer.

Because `ConcreteRing<RingType>` is templated on `RingType`, the
compiler can **inline** every forwarding call. The virtual-dispatch cost
hits *only* the outer `Ring*` boundary — once inside, all arithmetic is
inlined.

## `COERCE_RING` macro

```cpp
#define COERCE_RING(RingType, R)  dynamic_cast<const RingType *>(R)
```

A small `dynamic_cast` helper that converts a `Ring*` into a concrete
`aring` type pointer when a hot path knows it has, say, an
`ARingZZpFlint`. After the cast, all arithmetic inlines without going
through `Ring`'s virtuals.

This is the optimisation that makes `aring`-templated hot paths fast
while keeping legacy code paths unchanged.

## `displayArithmeticCalls`

The static debug flag at the top of the header lets a developer turn on
per-operation logging — useful when chasing down arithmetic bugs in a new
aring. Production builds keep it `false`.

## Translation tables

The companion header [`aring-translate.hpp`](coefficient-rings.md) handles
*cross-ring* coercions (e.g. promoting an integer to a rational) and is
used together with `aring-glue.hpp` whenever values cross ring
boundaries.

## Related

- [`coefficient-rings.md`](coefficient-rings.md) — area overview.
- [`file-aring.md`](file-aring.md) — `aring` framework.
- [`file-coeffrings.md`](file-coeffrings.md) — `SimpleARing` CRTP example.
- `aring-wrap.{cpp,hpp}`, `aring-translate.hpp` — siblings.
- [`file-polyring.md`](file-polyring.md) — legacy `Ring` consumers.
