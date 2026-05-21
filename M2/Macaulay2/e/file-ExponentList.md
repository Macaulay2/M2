# `ExponentList.{cpp,hpp}` — variable-length sparse exponent encoding

`ExponentList` is the newer **sparse exponent-list** encoding for monomials
at the top level of the engine. A monomial is stored as a sequence of
`(variable, exponent)` pairs — compact for monomials of low support, easy
to iterate without unpacking.

Part of the [Monoids & monomials](monoids-and-monomials.md) area.

[← per-area: monoids-and-monomials](monoids-and-monomials.md) · [← engine overview](README.md)

## Layout

```
[ len, v_1, e_1, v_2, e_2, …, v_r, e_r ]
```

where `len = 2r + 1` is the number of `int`s in the list. The
representation is **dense in support** (variables that appear) but **sparse
in the ambient variable count** (variables that don't appear consume zero
bytes).

A canonical-form `ExponentList` has `v_1 < v_2 < … < v_r` so that
divisibility / equality can be tested by a single linear walk.

## Iteration

The header exposes a small iterator type so callers can iterate without
seeing the layout details:

```cpp
for (auto a : monom) {
    // a is a std::pair<variable_index, exponent>
    use(a.first, a.second);
}
```

This pattern is borrowed verbatim from [`file-monideal.md`](file-monideal.md),
which uses the same encoding for monomial-ideal generators.

## Allocation strategies

The header (and the comment block at the top of
[`file-monideal.md`](file-monideal.md)) mentions several allocation patterns
used by callers:

- `std::vector<int>` — easy / safe / heap-allocated.
- `Range` — pre-allocated contiguous span over an existing buffer.
- [`MemoryBlock`](utilities.md) — bump-pointer allocator for transient
  monomials during a tight inner loop.

The `ExponentList` doesn't own its storage — it's a *view* over ints. The
caller is responsible for keeping the underlying buffer alive.

## Relation to other encodings

| Encoding | Where | Sparsity |
|---|---|---|
| `ExponentList` (this file) | top of `e/` | Sparse — `(v, e)` pairs |
| `ExponentVector.hpp` | top of `e/` | Dense — `[e_1, …, e_n]` |
| [`f4/varpower-monomial.hpp`](f4/file-varpower-monomial.md) | `f4/` only | Sparse — F4 internal |
| [`f4/ntuple-monomial.hpp`](f4/file-ntuple-monomial.md) | `f4/` only | Dense — F4 internal |

`ExponentList` and `varpower-monomial` are conceptually the same idea but
were written at different times for different consumers and have diverged
in API. The newer F4 code in [`gb-f4/`](gb-f4/README.md) is consolidating
on its own `MonomialView` / `MonomialTypes`.

## Related

- [`monoids-and-monomials.md`](monoids-and-monomials.md) — area overview.
- [`file-monideal.md`](file-monideal.md) — heavy consumer.
- [`file-monoid.md`](file-monoid.md) — overall monoid layer.
- [`utilities.md`](utilities.md) — `MemoryBlock` and overflow.
