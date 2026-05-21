# `memblock.hpp` — `F4MemoryBlock<T>` (F4 slab allocator)

`memblock.hpp` declares **`F4MemoryBlock<T, NSLAB>`** — the F4 engine's
slab-based bump allocator for in-loop transient values. It is a
templated, in-engine alternative to the
[memtailor](../../../submodules/README.md)-backed
[`MemoryBlock`](../file-MemoryBlock.md), specialised for F4's access
pattern.

Part of the [`f4/`](README.md) subdirectory.

[← f4 overview](README.md) · [← engine overview](../README.md)

## Class shape

```cpp
template <typename T, long int NSLAB = 4092>
class F4MemoryBlock : public our_new_delete {
    struct slab : public our_new_delete {
        slab *next;
        T     block[NSLAB];
    };

    slab *first_slab;
    slab *current_slab;
    slab *last_slab;
    T    *next_free;        /* points into current_slab */

private:
    slab *new_slab();

public:
    F4MemoryBlock();
    // ... reserve, take, reset ...
};
```

Two template parameters:

- **`T`** — the value type. F4 typically instantiates this for
  encoded-monomial ints, S-pair indices, and polynomial coefficient
  arrays.
- **`NSLAB`** — slab size in `T`s. Default `4092` keeps each slab
  near 16 KB for typical `T`, which fits comfortably in L1 cache.

## How allocation works

The allocator maintains a linked list of slabs. Each `take(n)` call
either:

- Returns the next `n` `T`s from `current_slab` if they fit.
- Advances to the next slab (allocating a new one if needed).

There is **no per-allocation freeing**. The whole block is freed by
either:

- **Destruction** — releases all slabs.
- **`reset()`** — rewinds `next_free` to `first_slab->block`, keeping
  the slabs allocated for reuse.

The reset pattern is the key: an F4 step allocates many transient
values, finishes, and resets the block. The slabs stay around for the
next step. After enough steps, the working set of slabs stabilises and
allocation costs go to zero.

## Compared to engine `MemoryBlock`

| Aspect | `F4MemoryBlock<T>` (this file) | [`MemoryBlock`](../file-MemoryBlock.md) |
|---|---|---|
| Backed by | In-engine slabs | memtailor's `Arena` |
| Templated on | Element type | None (byte-level) |
| Reset | Reuse slabs | Reuse arena |
| GC integration | `our_new_delete` slabs | System heap + arena |

`F4MemoryBlock<T>` predates the `MemoryBlock`-based pattern in newer
code; both exist. `gb-f4/` and `schreyer-resolution/` use the newer one.

## Related

- [`README.md`](README.md) — f4 overview.
- [`../file-MemoryBlock.md`](../file-MemoryBlock.md) — newer pattern.
- [`file-f4.md`](file-f4.md), [`file-f4-spairs.md`](file-f4-spairs.md)
  — primary users.
