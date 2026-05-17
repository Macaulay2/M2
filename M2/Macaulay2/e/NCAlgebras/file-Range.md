# `Range.hpp` — lightweight `Range<T>` (iterator-pair view)

`Range<T>` is a **non-owning iterator-pair** type used throughout
[`NCAlgebras/`](README.md). It is the C++ analogue of a Python slice:
a pair of pointers `(first, last)` representing the half-open interval
`[first, last)`, with `begin()`/`end()` accessors so it can be used
with range-based `for`.

Part of the [`NCAlgebras/`](README.md) subdirectory.

[← NCAlgebras overview](README.md) · [← engine overview](../README.md)

## Class shape

```cpp
#include "newdelete.hpp"
#include <utility>
#include <vector>

template <typename T>
class Range {
private:
    T *mFirst;
    T *mLast;
public:
    Range() : mFirst(nullptr), mLast(nullptr) {}
    Range(T *first, T *last) : mFirst(first), mLast(last) {}
    Range(std::pair<T*, T*> a) : mFirst(a.first), mLast(a.second) {}

    template <class S>
    Range(const Range<S> &copy)
        : mFirst(copy.begin()), mLast(copy.end()) {}

    explicit Range(std::vector<T> &vec)
        : mFirst(vec.data()), mLast(vec.data() + vec.size()) {}

    explicit Range(VECTOR(T) &vec)
        : mFirst(vec.data()), mLast(vec.data() + vec.size()) {}

    int size() const { return mLast - mFirst; }
    T *begin() { return mFirst; }
    T *end()   { return mLast;  }
    // ...
};
```

Five constructors covering the common cases:

1. Default — empty range.
2. Pointer pair — view over an external buffer.
3. From `std::pair<T*, T*>` — typical when receiving from
   `MemoryBlock::allocateArray<T>(n)`
   ([`../file-MemoryBlock.md`](../file-MemoryBlock.md)).
4. Cross-type construction — letting `Range<const T>` accept a
   `Range<T>` (the standard const-ness widening).
5. From `std::vector<T>` and the engine's `VECTOR(T)` macro — explicit
   to avoid surprises.

## Why not `std::span`?

`std::span` is C++20; the engine's baseline is C++17 (per
[`../../CLAUDE.md`](../../../CLAUDE.md)). `Range` is the engine's
home-rolled equivalent. If/when the baseline moves to C++20, this
file can probably be replaced with a type alias.

## Used by

- [`file-NCF4.md`](file-NCF4.md) — passes ranges of monomials through
  the matrix-reduction step.
- [`file-NCGroebner.md`](file-NCGroebner.md) — same role in the
  Buchberger-style loop.
- [`file-FreeAlgebra.md`](file-FreeAlgebra.md) — produces ranges as
  the output of multiplication.

## Related

- [`README.md`](README.md) — NCAlgebras overview.
- [`file-MemoryBlock.md`](../file-MemoryBlock.md) — produces
  `std::pair<T*, T*>` consumable by `Range`.
- [`../gb-f4/file-MonomialView.md`](../gb-f4/file-MonomialView.md) —
  a related non-owning view at the monomial level.
