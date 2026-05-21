# `MemoryBlock.hpp` — bump-pointer allocator

`MemoryBlock` is the engine's **bump-pointer allocator**, used in the
inner loops of F4, the resolution code, and the GB engines whenever a
large number of transient small objects need to be allocated and freed
together.

Part of the [Utilities](utilities.md) area.

[← per-area: utilities](utilities.md) · [← engine overview](README.md)

## API

```cpp
class MemoryBlock {
public:
    MemoryBlock()  { mArena = new memt::Arena; }
    ~MemoryBlock() { delete mArena; }

    template <typename T>
    std::pair<T*, T*> allocateArray(size_t nelems) {
        return mArena->allocArrayNoCon<T>(nelems);
    }
    // ...
};
```

`MemoryBlock` is a thin wrapper around `memt::Arena` from the
[memtailor](../../submodules/README.md) submodule. `Arena` implements
the classic **bump-pointer** strategy:

- All allocations come out of a single growing buffer.
- `allocateArray<T>(n)` returns the next `n * sizeof(T)` bytes, bumps the
  cursor, and returns `(begin, end)`.
- Individual allocations **cannot be freed**.
- The entire arena is released when the `MemoryBlock` is destroyed.

This is the right model for "I'll allocate millions of small things during
one GB step and free them all when the step ends" — common in the engine's
hot paths.

## Why not just use Boehm GC

bdwgc is excellent for long-lived structures but adds per-allocation
overhead and is not as cache-friendly as a bump allocator. For tight
short-lived loops, `MemoryBlock` is significantly faster.

`MemoryBlock` allocates from the *system* heap, not bdwgc — anything
allocated from a `MemoryBlock` must not contain pointers to GC-managed
memory (because the GC won't scan it). Engine code is careful to use
`MemoryBlock` only for "pure data" (encoded monomials, exponent vectors,
S-pair structures).

## `allocArrayNoCon`

The `NoCon` suffix on `mArena->allocArrayNoCon<T>` is memtailor's term for
"no constructor call" — the bytes are returned uninitialised. Callers must
either initialise the bytes themselves or treat them as plain-old-data.
This skips per-element constructor overhead that doesn't apply to monomial
ints.

## Used by

- [`f4/`](f4/README.md), [`gb-f4/`](gb-f4/README.md),
  [`schreyer-resolution/`](schreyer-resolution/README.md) — all rely
  heavily on `MemoryBlock` to allocate per-step transient monomials.
- [`NCAlgebras/`](NCAlgebras/README.md) — same role for word allocation.
- [`file-monideal.md`](file-monideal.md) — monomial-ideal storage.

## Related

- [`utilities.md`](utilities.md) — area overview.
- memtailor submodule under [`submodules/`](../../submodules/README.md) —
  underlying `Arena` implementation.
- bdwgc submodule — the slower-but-richer GC alternative.
