# `res-memblock.hpp` — `ResMemoryBlock<T>` (resolution-side slab allocator)

`ResMemoryBlock<T, NSLAB>` is the **resolution-side slab allocator** —
a per-type bump allocator the F4 resolution loop uses for transient
monomial / polynomial buffers. It is the direct twin of
[`f4/file-memblock.md`](../f4/file-memblock.md) in the GB path.

Part of the [`schreyer-resolution/`](README.md) subdirectory.

[← schreyer-resolution overview](README.md) · [← engine overview](../README.md)

## Class shape

```cpp
template <typename T, long int NSLAB = 4092>
class ResMemoryBlock {
    struct slab {
        slab *next;
        T     block[NSLAB];
    };

    slab *first_slab;
    slab *current_slab;
    slab *last_slab;
    T    *next_free;     /* points into current_slab */

private:
    slab *new_slab();

public:
    ResMemoryBlock();
    ~ResMemoryBlock();

    // ... reserve, take, reset ...
};
```

Identical pattern to `F4MemoryBlock`:

- A linked list of slabs.
- `take(n)` returns the next `n` `T`s, allocating a new slab when full.
- `reset()` rewinds `next_free` so the same slabs are reused for the
  next degree.
- The block's destructor releases every slab.

`NSLAB = 4092` keeps each slab around 16 KB for typical `T`, cache-
friendly for the inner loop.

## Differences from `F4MemoryBlock`

- The class is **not** a `our_new_delete` subclass (unlike
  `F4MemoryBlock`). The resolution code typically allocates
  `ResMemoryBlock`s as members of larger structs and lets ownership
  flow with the parent.
- The block does not have a separate "atomic" flag — every `T` stored
  in a `ResMemoryBlock` is assumed to be plain-old-data.

These are minor differences; the underlying algorithm is the same.

## Used by

- [`file-res-f4.md`](file-res-f4.md) — primary user; allocates encoded
  monomials per degree.
- [`file-res-schreyer-frame.md`](file-res-schreyer-frame.md) — uses
  `ResMemoryBlock<int>` for frame metadata.
- [`file-res-poly-ring.md`](file-res-poly-ring.md) — uses the block
  for polynomial term arrays.

## Why two slab allocators

The engine has three slab-style allocators now:

| Allocator | Backed by | Used by |
|---|---|---|
| `F4MemoryBlock<T>` | In-engine slabs | [`f4/`](../f4/README.md) |
| `ResMemoryBlock<T>` (this file) | In-engine slabs | [`schreyer-resolution/`](README.md) |
| [`MemoryBlock`](../file-MemoryBlock.md) | memtailor `Arena` | [`gb-f4/`](../gb-f4/README.md), [`NCAlgebras/`](../NCAlgebras/README.md) |

The first two are essentially the same template instantiated for
different subsystems; the third is the newer pattern that newer code
adopts.

## Related

- [`README.md`](README.md) — schreyer-resolution overview.
- [`../f4/file-memblock.md`](../f4/file-memblock.md) — GB-side sibling.
- [`../file-MemoryBlock.md`](../file-MemoryBlock.md) — newer pattern.
- [`file-res-f4.md`](file-res-f4.md) — primary consumer.
