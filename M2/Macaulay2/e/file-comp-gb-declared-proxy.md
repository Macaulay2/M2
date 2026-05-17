# `comp-gb-declared.{cpp,hpp}`, `comp-gb-proxy.{cpp,hpp}` — declared + proxy GBs

These two file pairs implement **`GBDeclared`** (a "declared" GB:
the user asserts a precomputed GB is correct, M2 trusts it) and
**`GBProxy`** (an indirection wrapper around another GB
computation, used historically for retrofitting).

Part of the [engine](README.md) — Gröbner bases.

[← engine overview](README.md) · [Gröbner bases](groebner-bases.md)

## `GBDeclared` — user-asserted GB

```cpp
class GBDeclared : public GBComputation
// This contains a GBComputation, which can be changed.
```

Constructed from:

- `m0` — the original generators.
- `gb` — the user's claimed reduced GB.
- `change` — the matrix expressing `gb` in terms of `m0`.
- `syz0` — the user's claimed syzygy module.

```cpp
GBDeclared::GBDeclared(const Matrix *m0,
                       const Matrix *gb,
                       const Matrix *change,
                       const Matrix *syz0)
    : trimmed_gens(m0), syz(syz0)
{
  ...
}
```

When a user calls `forceGB`, this is what they get back: M2
treats `gb` as the GB without ever computing it, so all
subsequent operations (`%`, `//`, `quotient`, ...) are immediate.

The trust model: **the user is responsible**. If they declare a
wrong GB, downstream answers are wrong. M2 doesn't verify (that
would defeat the point of `forceGB`).

## Why `forceGB` matters

Use cases:

- **Imported GBs** — user computed elsewhere (msolve, OSCAR,
  Singular) and wants M2's reductions / quotients without
  recomputing.
- **Theoretical GBs** — user proved on paper that `gb` is the GB.
- **Caching** — save a long GB run to disk, reload via `forceGB`.

## `GBProxy` — indirection wrapper

```cpp
/**
    @brief handle to a GB.  Should be expunged, as full functionality isn't
   used.
*/
class GBProxy : public GBComputation
```

The doxygen comment says "Should be expunged" — `GBProxy` is
**deprecated**. Originally a wrapper to swap GB strategies at
runtime, but it's rarely used now.

```cpp
GBProxy::~GBProxy()
{
  // Intentionally left blank
}
```

The empty destructor confirms it's a thin wrapper — no owned
resources, just delegation.

## Used by

- M2's `forceGB(...)` (for `GBDeclared`).
- Some legacy code paths (for `GBProxy`).
- [`interface/file-groebner-interface.md`](interface/file-groebner-interface.md)
  — C API.

## Related

- [`README.md`](README.md) — engine overview.
- [`groebner-bases.md`](groebner-bases.md) — area.
- [`file-comp-gb.md`](file-comp-gb.md) if added — `GBComputation`
  base class.
- [`../m2/file-gb.md`](../m2/file-gb.md) — M2-side `gb` /
  `forceGB`.
