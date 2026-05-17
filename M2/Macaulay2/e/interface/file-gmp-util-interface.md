# `gmp-util.h` (in `interface/`) — multiprecision allocation helpers

`interface/gmp-util.h` is a tiny **utility header** for bridging GMP's
allocation model (uses `malloc`) and the engine's GC-managed heap (uses
bdwgc via `getmem_atomic`). Its single inline function copies an `mpz_t`'s
limbs out of the malloc heap and into a GC-managed buffer so the GC can
see them.

Part of the [`interface/`](README.md) subdirectory.

[← interface overview](README.md) · [← engine overview](../README.md)

## The single key function

```c
inline void mpz_reallocate_limbs(mpz_ptr _z) {
    int  _s  = _z->_mp_size;
    int  _as = (_s > 0) ? _s : -_s;
    mp_limb_t *_p = (mp_limb_t *)getmem_atomic(_as * sizeof(mp_limb_t));
    memcpy(_p, _z->_mp_d, _as * sizeof(mp_limb_t));
    mpz_clear(_z);
    // ... rewire _z->_mp_d to _p ...
}
```

The flow:

1. Allocate a new buffer of the right size via `getmem_atomic` (bdwgc-
   managed, won't contain pointers, eligible for the atomic-GC pool).
2. Copy the existing limb data into it.
3. Free the GMP-managed buffer via `mpz_clear`.
4. Update the `mpz_struct` to point at the new GC-managed buffer.

After the call, the integer's value is unchanged, but its storage is in
the GC heap rather than the system heap — important when handing the
integer back to engine code that expects everything to be GC-managed.

## Why this helper exists

The engine never wants two memory regimes in the same data structure. By
walking every `mpz_t` returned by GMP through `mpz_reallocate_limbs`, the
engine guarantees that *every* engine-visible integer is fully GC-managed.

This matters at the engine ↔ interpreter boundary, where the interpreter
holds values for unpredictable lifetimes — they need to be visible to
the GC's reachability analysis.

## Companion macros

The header also declares moves and conversions:

- `moveTo_gmpZZ`, `moveTo_gmpQQ`, `moveTo_gmpRR`, `moveTo_gmpCC` —
  copy a value out of an engine type into a GC-allocated GMP value
  with the right structure.
- `mpfr_reallocate_limbs` — analogous helper for MPFR floats.

All are used pervasively in `aring-*-gmp.cpp` and the engine's
return-value paths.

## Related

- [`README.md`](README.md) — interface overview.
- [`../file-aring-zz-flint.md`](../file-aring-zz-flint.md), `aring-zz-gmp`
  — primary callers of `mpz_reallocate_limbs`.
- [`file-m2-mem-interface.md`](file-m2-mem-interface.md) — sibling for
  generic engine memory hooks.
- [`../file-MemoryBlock.md`](../file-MemoryBlock.md) — non-GC bump
  allocator used in hot loops.
