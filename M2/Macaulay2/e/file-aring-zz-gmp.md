# `aring-zz-gmp.{cpp,hpp}` — `M2::ARingZZGMP` (ZZ via GMP)

`aring-zz-gmp.cpp` implements **`ZZ` using GMP's `mpz_t`** value type
directly. It is the original aring ZZ back end; the FLINT-based
[`file-aring-zz-flint.md`](file-aring-zz-flint.md) is now the default
because it inlines small-value arithmetic. Both remain available.

Part of the [Coefficient rings](coefficient-rings.md) area.

[← per-area: coefficient-rings](coefficient-rings.md) · [← engine overview](README.md)

## Class structure

```cpp
namespace M2 {

class ARingZZGMP : public SimpleARing<ARingZZGMP> {
public:
    static const RingID ringID = ring_ZZ;
    // typedef <mpz-handle> elem;
    // arithmetic, conversion, RNG
};

}
```

`elem` wraps GMP's `mpz_struct`. Every value is heap-allocated (GMP's
model); arithmetic delegates to `mpz_add`, `mpz_mul`, etc.

## Why a GMP path remains

FLINT depends on GMP underneath, so removing the GMP-only path would
not eliminate GMP from the build. The GMP path stays because:

- **Simplicity** — `aring-zz-gmp.cpp` is much shorter and is the
  reference path used to validate FLINT's results.
- **Compatibility** — some platforms or builds may lack FLINT; GMP is
  more universally available.
- **Cross-checking** — regression tests sometimes use the GMP path as
  a known-good baseline.

## Allocation pattern

Every operation produces a fresh `mpz_t`. The engine routes the result
through [`mpz_reallocate_limbs`](interface/file-gmp-util-interface.md)
so the limbs end up in the GC heap, not GMP's malloc heap. Without
this step, GMP's limbs would be invisible to bdwgc and could be freed
out from under the engine.

## How the engine picks between `ARingZZ` and `ARingZZGMP`

The default factory `rawARingZZ` (in
[`interface/aring.h`](interface/file-aring-interface.md)) chooses the
FLINT path when available. The GMP path is reached via specific
strategy options or when FLINT is unavailable.

## Related

- [`coefficient-rings.md`](coefficient-rings.md) — area overview.
- [`file-aring-zz-flint.md`](file-aring-zz-flint.md) — FLINT default.
- [`file-coeffrings.md`](file-coeffrings.md) — registry.
- [`interface/file-gmp-util-interface.md`](interface/file-gmp-util-interface.md)
  — `mpz_reallocate_limbs` helper.
- `ZZ.{cpp,hpp}` — even older non-aring ZZ implementation.
