# `MonomialView.{cpp,hpp}` — non-owning view over an encoded monomial

`MonomialView` is the **non-owning view** type used throughout
[`gb-f4/`](README.md) to refer to an encoded monomial without copying
its data. It is essentially a pointer-with-helpers — the data itself
lives in a [`MemoryBlock`](../file-MemoryBlock.md) or a `std::vector`
managed elsewhere.

Part of the [`gb-f4/`](README.md) subdirectory.

[← gb-f4 overview](README.md) · [← engine overview](../README.md)

## Class shape

```cpp
#include "../MemoryBlock.hpp"
#include "MonomialTypes.hpp"
#include <vector>
#include <functional>
#include <cstring>  // For std::memcmp

namespace newf4 {

class MonomialView {
private:
    MonomialInt *mData;   // We do not own the data pointed to

public:
    explicit MonomialView(MonomialInt *data) : mData(data) {}
    explicit MonomialView(std::vector<MonomialInt> &data) : mData(data.data()) {}

    MonomialView(std::vector<MonomialInt> data, MemoryBlock &block) {
        MonomialView m(data.data());
        auto rng = block.allocateArray<MonomialInt>(m.size());
        mData = rng.first;
        std::copy(m.dataBegin(), m.dataEnd(), mData);
    }
    // ...
};

}
```

Three constructors:

1. **From raw pointer** — view over an externally-owned buffer.
2. **From `std::vector`** — view over a vector's internal buffer
   (vector must outlive the view).
3. **Copy into MemoryBlock** — the only constructor that *allocates*:
   given a `std::vector` of monomial ints, copy into a fresh slab from
   the `MemoryBlock` and view that copy.

Comparison uses `std::memcmp` — the encoded monomial format is designed
so that byte-equal arrays mean monomial-equal values.

## Why a view

The refactored F4 generates and discards monomials in great numbers.
Copying each one would dominate runtime. `MonomialView` lets the same
encoded monomial be referenced from:

- a [`MonomialHashTable`](file-MonomialHashTable.md) entry,
- multiple polynomial-list rows,
- multiple S-pair `lcm` slots,

all sharing one allocation. When the F4 step completes, the
[`MemoryBlock`](../file-MemoryBlock.md) is reset and every view is
invalidated together.

## Lifetime discipline

The class itself doesn't enforce lifetime — that's the caller's job.
Engine convention: a `MonomialView` should never outlive the
`MemoryBlock` (or `vector`) it views.

Compilers can't catch lifetime mistakes here, but the alternative —
ref-counted shared ownership — would defeat the performance goal.

## Related

- [`README.md`](README.md) — gb-f4 overview.
- [`file-MonomialHashTable.md`](file-MonomialHashTable.md), [`file-MonomialLookupTable.md`](file-MonomialLookupTable.md)
  — primary consumers.
- [`../file-MemoryBlock.md`](../file-MemoryBlock.md) — underlying
  allocator.
- `MonomialTypes.hpp` — the `MonomialInt` type and helper macros.
