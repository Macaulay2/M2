# `schorder.{cpp,hpp}` — `SchreyerOrder`

`SchreyerOrder` is the engine's storage of a **Schreyer order** on a free
module. It is a lightweight class that lives on a [`FreeModule`](file-freemod.md)
and supplies the data needed to compare two basis elements during
resolution code.

Part of the [Free modules](free-modules.md) area.

[← per-area: free-modules](free-modules.md) · [← engine overview](README.md)

## State

```cpp
class SchreyerOrder : public our_new_delete {
    const Monoid    *M;
    gc_vector<int>   _order;    // flat array; one entry per basis element
    int              _nslots;   // ints per entry: 1 (compare_num) + monomial_size
    int              _rank;     // number of basis elements (entries)
};
```

For each basis element `e_i` of the free module we store:

- A **`compare_num`** — a sortable tiebreaker integer (typically the order
  in which the basis element was added during a GB computation).
- An **encoded monomial** — the "leading monomial" of the polynomial that
  introduced `e_i`. The monomial uses the same encoding as elsewhere in
  the engine (see [`file-monoid.md`](file-monoid.md)).

The entries are packed flatly in `_order` for cache-friendly iteration.

## Comparison

To compare `e_i · m` vs `e_j · m'`:

```
compare(m_i · m, m_j · m')           // ambient monomial order
   if equal, tiebreak by compare_num[i] vs compare_num[j]
```

where `m_i` and `m_j` are the stored monomials. This is the standard
Schreyer-order definition, and the engine's monomial-order code can
compare the encoded products without unpacking them.

## When a Schreyer order is set

A free module starts **without** a Schreyer order. One is installed by:

- `Eschreyer.cpp` / `schreyer-resolution/` when the free module is the
  **target** of a Schreyer syzygy step;
- `gb-default.cpp` and similar GB algorithms when the user requests Schreyer
  encoding via the `Strategy =>` option.

Once installed, the Schreyer order persists for the lifetime of the free
module.

## Allocation

`SchreyerOrder` uses `our_new_delete` (Boehm GC). The flat `_order` is a
`gc_vector<int>`, growing as basis elements are appended.

## Related

- [`free-modules.md`](free-modules.md) — area overview.
- [`file-freemod.md`](file-freemod.md) — the host class.
- [`file-Eschreyer.md`](file-Eschreyer.md) — primary code path that
  installs Schreyer orders.
- [`schreyer-resolution/`](schreyer-resolution/README.md) — modern Schreyer
  resolution implementation.
- [`file-monoid.md`](file-monoid.md) — monomial encoding used for entries.
