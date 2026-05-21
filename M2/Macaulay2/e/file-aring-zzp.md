# `aring-zzp.{cpp,hpp}` — `M2::ARingZZp` (generic small Z/p via log-tables)

`aring-zzp.cpp` implements **`Z/p` for small primes** using precomputed
log / exp tables of a primitive root. It is the **portable** Z/p
back end — no FLINT, no FFLAS-FFPACK dependency, no Givaro. Used
whenever neither of those libraries is available or applicable.

Part of the [Coefficient rings](coefficient-rings.md) area.

[← per-area: coefficient-rings](coefficient-rings.md) · [← engine overview](README.md)

## Class structure

```cpp
namespace M2 {

class ARingZZp : public SimpleARing<ARingZZp> {
    // Integers mod p, implemented as
    // exponents of a primitive element a.
    //
    // Representation:
    //   0  means 0
    //   1 <= n <= p-1  means a^n (mod p)
    //
    // ...
};

}
```

A value `n` (with `1 <= n <= p-1`) represents `α^n mod p` where `α` is
a primitive root chosen at ring construction time. The value `0`
remains zero. With this representation:

- **Multiplication** is integer addition modulo `p - 1`.
- **Inversion** is negation modulo `p - 1`.
- **Powers** are multiplication modulo `p - 1`.
- **Addition** uses a precomputed Zech-log table:
  `α^a + α^b = α^a (1 + α^{b-a}) = α^{a + Zech(b-a)}`.

Every operation becomes O(1) integer arithmetic plus one table lookup.
For small primes (`p` up to ~32000), the tables fit in L2 cache.

## Tables

The class owns:

- **`log_table[0..p-1]`** — log table: `log_table[α^n mod p] = n`.
- **`exp_table[0..p-1]`** — exp table: `exp_table[n] = α^n mod p`.

These are built at construction. Storage is `O(p)` words.

For larger primes the engine falls back to
[`file-aring-zzp-flint.md`](file-aring-zzp-flint.md) which uses
hardware-fast modular reduction (no tables).

## Why this implementation matters

It is the **only** Z/p path with no external dependencies. When M2 is
built with stripped-down dependencies (e.g. for some embedded or
diagnostic builds), `ARingZZp` is what the user gets.

It is also conceptually the simplest of the Z/p paths and is the
reference implementation that regression tests compare against.

## Related

- [`coefficient-rings.md`](coefficient-rings.md) — area overview.
- [`file-aring-zzp-flint.md`](file-aring-zzp-flint.md) — FLINT alternative
  (faster for larger primes).
- [`file-aring-zzp-ffpack.md`](file-aring-zzp-ffpack.md) — FFLAS-FFPACK
  alternative (best for dense matrices).
- [`file-coeffrings.md`](file-coeffrings.md) — `CoefficientRingZZp` is
  the older non-aring counterpart with the same encoding.
