# `MonomialHashTable.{cpp,hpp}` — `MonomialHashFunction` + hash table (new F4)

`MonomialHashTable.hpp` defines the **monomial hash function and table**
used by the refactored F4 in [`gb-f4/`](README.md). It is the modern,
self-contained successor to
[`f4/file-monhashtable.md`](../f4/file-monhashtable.md), which only
declared trait classes for an external (mathic) hash table.

Part of the [`gb-f4/`](README.md) subdirectory.

[← gb-f4 overview](README.md) · [← engine overview](../README.md)

## `MonomialHashFunction`

```cpp
namespace newf4 {

class MonomialHashFunction {
private:
    std::vector<HashInt> mHashValues;
public:
    MonomialHashFunction()
        : mHashValues({
              12550986463692465404ul, 3911555212215091238ul,
              15090669942851505316ul, 16174113364685515424ul,
              // ... many more 64-bit constants ...
          })
    {}
    // ...
};

}
```

The hash function uses a **precomputed random constant per variable**
to convert a monomial `x_1^{e_1} · … · x_n^{e_n}` into a 64-bit hash:

```text
hash(m) = ⊕_i c_i^{e_i}  (mod 2^64, where ⊕ is bitwise XOR-with-tail)
```

The hash values are random 64-bit integers baked into the source. Two
properties matter:

- **Determinism** — the same hash values across runs let the engine
  reproduce floating-point-sensitive outputs deterministically.
- **Uniformity** — random 64-bit constants give near-uniform distribution
  over hash buckets for typical monomial sets.

## Internal storage

The class also owns the underlying hash table: a vector of buckets,
each bucket a small array of `(monomial_view, value_index)` pairs.
Collisions are resolved by linear probing.

## What gets stored

For the refactored F4 the table maps each **monomial** seen anywhere in
the algorithm — basis element leading terms, tail monomials, S-pair lcms,
Macaulay-matrix column heads — to a single canonical integer index. The
algorithm then works entirely on indices; monomial data is consulted only
when actually reading or printing.

Memory ownership: monomials live in a [`MemoryBlock`](../file-MemoryBlock.md)
owned by the [`GBF4Computation`](file-GBF4Computation.md); the table
stores only views.

## Used by

- [`file-GBF4Computation.md`](file-GBF4Computation.md) — owns the table
  as `mBasisMonomials`.
- [`file-MacaulayMatrix.md`](file-MacaulayMatrix.md) — references the
  table for column indexing.
- [`file-SPairs.md`](file-SPairs.md) — stores S-pair lcm indices.

## Related

- [`README.md`](README.md) — gb-f4 overview.
- [`../f4/file-monhashtable.md`](../f4/file-monhashtable.md) — older trait-only counterpart.
- `MonomialLookupTable.{cpp,hpp}` — sibling structure for leading-term
  divisibility lookup.
- `MonomialView.{cpp,hpp}`, `MonomialTypes.hpp` — value types stored by
  the table.
