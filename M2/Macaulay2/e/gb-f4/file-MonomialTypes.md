# `MonomialTypes.hpp` — typed integers for the new F4 (`newf4`)

`MonomialTypes.hpp` is the **type-vocabulary header** for the refactored
F4 in [`gb-f4/`](README.md). It declares strongly-typed integer
aliases — `Index`, `MonomialIndex`, `MonomialInt`, `ComponentIndex`,
`HashInt` — that the rest of the namespace `newf4` uses everywhere
instead of bare `int`s.

Part of the [`gb-f4/`](README.md) subdirectory.

[← gb-f4 overview](README.md) · [← engine overview](../README.md)

## Type aliases

```cpp
namespace newf4 {

// Index of a GB element
using Index         = int32_t;

// Index of a monomial in a monomial hash table.
// 0 is a sentinel, meaning that something that usually would
// point to a monomial is currently undefined.  Valid values are > 0.
using MonomialIndex = int32_t;

// Data type of the underlying monomial store.
// A monomial is a sequence of MonomialInts.
// Also used to store degree of a monomial.
using MonomialInt   = int32_t;

// Number indicating the free module component.
using ComponentIndex = int32_t;

// Value of hashing a monomial (which may or may not include the component).
using HashInt        = uint64_t;

// ... possibly more (MonomialMask, etc.)

}
```

Five typed aliases, all backed by `int32_t` or `uint64_t`:

| Alias | Meaning | Width |
|---|---|---|
| `Index` | Index into the GB basis | `int32_t` |
| `MonomialIndex` | Index into the monomial hash table (0 = sentinel) | `int32_t` |
| `MonomialInt` | One word of an encoded monomial | `int32_t` |
| `ComponentIndex` | Free-module component / row index | `int32_t` |
| `HashInt` | 64-bit hash value | `uint64_t` |

## Why types matter

The five aliases all compile to the same machine type, but giving each
a distinct C++ name lets the compiler (and the human reader) catch
mix-ups:

```cpp
void foo(MonomialIndex mi, Index i);

foo(some_index, some_monomial_index);   // compile error!
```

The aliases prevent "I passed a monomial-table index where a basis
index was expected" bugs that would silently misbehave with bare
`int`s. The 0-sentinel convention for `MonomialIndex` (only valid
indices are positive) is an extra layer of explicit invariant.

## Used by

Every other file in [`gb-f4/`](README.md) — `Basis`, `MacaulayMatrix`,
`MonomialHashTable`, `MonomialLookupTable`, `PolynomialList`, `SPairs`,
`MonomialView`, `GBF4Computation`, `Polynomial` — uses these aliases
in its signatures.

## Related

- [`README.md`](README.md) — gb-f4 overview.
- [`file-Basis.md`](file-Basis.md), [`file-MonomialHashTable.md`](file-MonomialHashTable.md),
  [`file-MonomialView.md`](file-MonomialView.md) — primary consumers.
- `MonomialMask` and similar mask types — likely declared near these
  aliases in the same header.
