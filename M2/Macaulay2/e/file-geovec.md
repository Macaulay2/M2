# `geovec.hpp` — geometric heap for `vec` accumulation

`geovec.hpp` declares the engine's **geometric heap** specialised for
adding many `vec` values
([`file-ring-vecs.md`](file-ring-vecs.md)) efficiently — the
ring-element analogue of `gbvectorHeap` in
[`file-gbring.md`](file-gbring.md).

Part of the [Ring elements & maps](ring-elements-and-maps.md) area.

[← per-area: ring-elements-and-maps](ring-elements-and-maps.md) · [← engine overview](README.md)

## Why a heap

When a polynomial reduction step subtracts many multiples of basis
elements from an accumulator, the naïve approach (one merge per
subtraction) is `O(n^2)` in the number of terms. A heap-based
accumulation is `O(n log k)` where `k` is the number of pending
contributions.

The heap pattern: pending contributions live in a min-heap keyed by
their leading monomial. To get the next term of the result, pop the
heap's minimum and merge in any later contributions whose leading
monomial matches.

## Geometric levels

The heap is "geometric" because it consists of multiple **levels**
each holding a bounded number of terms before promotion:

- Level 0: up to `heap_size[0]` terms (e.g. 4).
- Level 1: up to `heap_size[1]` (e.g. 16).
- ...

When a level fills, its contents are merged into the next level. The
heap maintains the invariant that each level is internally sorted,
which makes pop-min fast.

`GEOHEAP_SIZE = 15` ([`file-style.md`](file-style.md)) is the number of
levels.

## How `geovec.hpp` differs from `gbvectorHeap`

| | `geovec` (this file) | `gbvectorHeap` ([`file-gbring.md`](file-gbring.md)) |
|---|---|---|
| Value type | `vec` (sparse list of `(component, coefficient)`) | `gbvector*` (intrusive linked list) |
| Use site | General ring-element accumulation | GB inner loop |
| Sortedness | Component-descending | Monomial-descending |

Both follow the same heap-based pattern, optimised for different value
types.

## The header note

The header comment captures a recurring design tension:

> This should probably be done by:
> (a) making a type `const FreeModule`, that FreeModule, and res_poly
>     both can inherit from: but this is a bit of a kludge...
> (b) making a vector type with a `next` and `coeff` field, that
>     is then inherited by `vecterm`, `resterm`.

The unification didn't happen; today there are several distinct
linked-list vector types. `geovec.hpp` works with `vec`/`vecterm`
specifically.

## Used by

- [`file-ring-vecs.md`](file-ring-vecs.md) — adds many `vec`s.
- General polynomial multiplication paths via the `Ring` class.
- Some GB and resolution code that operates on `vec`s rather than
  `gbvector*`.

## Related

- [`ring-elements-and-maps.md`](ring-elements-and-maps.md) — area overview.
- [`file-ring-vecs.md`](file-ring-vecs.md) — `vec` operations.
- [`file-gbring.md`](file-gbring.md) — sibling `gbvectorHeap`.
- [`file-style.md`](file-style.md) — `GEOHEAP_SIZE` constant.
