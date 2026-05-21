# `Word.{cpp,hpp}` — `Word` (lightweight non-owning word)

`Word` is the **lightweight non-owning view** used throughout
[`NCAlgebras/`](README.md) to refer to a non-commutative word. It is
the NC analogue of [`MonomialView`](../gb-f4/file-MonomialView.md) and
plays the same role.

Part of the [`NCAlgebras/`](README.md) subdirectory.

[← NCAlgebras overview](README.md) · [← engine overview](../README.md)

## Class shape

```cpp
class Word {
public:
    // Caller-managed lifetime: begin / end pointers must outlive this Word.
    Word() : mBegin(nullptr), mEnd(nullptr), mSize(0) {}

    Word(const int *begin, const int *end)
        : mBegin(begin), mEnd(end), mSize(end - begin) {}

    // 'explicit' to prevent implicit conversion; used in tests only.
    explicit Word(const std::vector<int> &val)
        : mBegin(val.data()),
          mEnd(val.data() + val.size()),
          mSize(val.size()) {}

    void init(const int *begin, const int *end) {
        mBegin = begin;
        mEnd = end;
        mSize = end - begin;
    }

    const int *begin() const { return mBegin; }
    const int *end()   const { return mEnd;   }
    // ...
};
```

A `Word` carries three values:

- `mBegin`, `mEnd` — pointer pair over the underlying int array.
- `mSize` — cached `end - begin`.

`init(begin, end)` lets a `Word` be **rebound** without reconstruction —
useful in tight loops where a single `Word` variable is reused for many
different underlying buffers.

## Storage convention

A non-commutative word is encoded as a flat `int` array `[v_1, v_2, …,
v_k]` where each `v_i` is a variable index. Compare with
[`file-FreeMonoid.md`](file-FreeMonoid.md), which prepends a length and
optionally weight prefixes. `Word` does **not** carry the length /
weight prefix — it sits one level lower, exposing just the variable
sequence.

## Non-owning semantics

The header comment is explicit:

> warning: the pointers begin, end, should not go out of scope while
> this Word is in use.

The class **never allocates or frees** the int array. Callers are
responsible for lifetime — typically the array lives in a
[`MemoryBlock`](../file-MemoryBlock.md) or in a `std::vector` that
outlives the `Word`.

The `explicit` constructor from `std::vector<int>` is restricted by
documentation to test code, because it's especially easy to get the
lifetime wrong with vector storage.

## Used by

- [`file-WordTable.md`](file-WordTable.md), [`file-SuffixTree.md`](file-SuffixTree.md)
  — index entries.
- [`file-OverlapTable.md`](file-OverlapTable.md) — overlap positions
  reference `Word`s.
- [`file-FreeMonoid.md`](file-FreeMonoid.md), [`file-FreeAlgebra.md`](file-FreeAlgebra.md)
  — produce `Word`s as the output of multiplication / leading-word
  extraction.

## Related

- [`README.md`](README.md) — NCAlgebras overview.
- [`file-FreeMonoid.md`](file-FreeMonoid.md) — the higher-level word
  layout with length and weight prefixes.
- `WordWithData.{cpp,hpp}` — a `Word` plus auxiliary metadata (used by
  some lookups).
- [`../gb-f4/file-MonomialView.md`](../gb-f4/file-MonomialView.md) —
  commutative analogue.
