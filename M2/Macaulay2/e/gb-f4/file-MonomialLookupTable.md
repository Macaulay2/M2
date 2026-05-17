# `MonomialLookupTable.{cpp,hpp}` — divisibility-aware monomial lookup

`MonomialLookupTable` is the **leading-term lookup index** for the
refactored F4 in [`gb-f4/`](README.md). It complements
[`MonomialHashTable`](file-MonomialHashTable.md) (which indexes by
exact equality) by providing **divisibility** queries: given a target
monomial, find a basis element whose leading monomial divides it.

Part of the [`gb-f4/`](README.md) subdirectory.

[← gb-f4 overview](README.md) · [← engine overview](../README.md)

## Per-entry metadata

```cpp
namespace newf4 {

struct MonomialInfo {
    bool         mIsUsed;         // whether to use for divisibility checks
    int          mSimpleDegree;   // simple degree
    MonomialMask mMask;           // divisibility mask
    int          mOffset;         // offset where monomial starts
    int          mValue;          // index of polynomial associated to this MonomialInfo
};

class MonomialLookupTable {
    friend class MonomialLookupIterator;
private:
    // ...
};

}
```

Each entry carries:

- **`mIsUsed`** — false means "this entry has been retired"; the lookup
  walks past it without testing.
- **`mSimpleDegree`** — the monomial's degree, used for ordering /
  early-exit.
- **`mMask`** — a `MonomialMask` (one bit per variable, set iff the
  exponent is positive). Used for cheap negative divisibility tests
  (`(target.mask & m.mask) != m.mask` → not divisible, skip).
- **`mOffset`** — pointer into the encoded-monomial pool where the
  actual exponent vector lives.
- **`mValue`** — index of the owning polynomial in the basis.

## Lookup operation

Given a target monomial, the algorithm:

1. Compute the target's `MonomialMask`.
2. Walk the table; for each `MonomialInfo` with `mIsUsed = true`:
   - Pre-filter by mask (cheap).
   - Pre-filter by `mSimpleDegree` (target degree must be at least
     entry's degree).
   - Full divisibility test on exponent vectors only if both
     pre-filters pass.
3. Return the first match (or no-match).

The mask + degree pre-filters discard the vast majority of candidates
before the expensive exponent comparison runs.

## Soft delete

Retiring an entry (when its owning polynomial becomes non-minimal) just
flips `mIsUsed` to false. No reshuffling. The table tolerates many
soft-deleted entries before a compaction pass runs.

## Used by

- [`file-GBF4Computation.md`](file-GBF4Computation.md) — for finding
  reducers during the F4 reduction step.
- [`file-SPairs.md`](file-SPairs.md) — for S-pair pruning by
  divisibility.

## Related

- [`README.md`](README.md) — gb-f4 overview.
- [`file-MonomialHashTable.md`](file-MonomialHashTable.md) — sibling
  exact-equality index.
- [`../f4/file-monhashtable.md`](../f4/file-monhashtable.md) — older F4
  counterpart.
- [`../file-montable.md`](../file-montable.md) — legacy GB counterpart.
