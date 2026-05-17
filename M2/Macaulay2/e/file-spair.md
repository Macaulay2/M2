# `spair.{cpp,hpp}` — S-pair data structures

`spair.cpp` defines the **S-pair** data structure used by
[`gbA`](file-gb-default.md) and several sibling GB algorithms. An S-pair is
the unit of work in Buchberger-style algorithms: pick two basis elements,
compute the S-polynomial, reduce it.

Part of the [Gröbner bases](groebner-bases.md) area.

[← per-area: groebner-bases](groebner-bases.md) · [← engine overview](README.md)

## `gb_elem` — a basis element with S-pair list

```cpp
struct gb_elem : public our_new_delete {
    gb_elem  *next;        // intrusive linked list of basis elements
    gb_elem  *next_min;    // intrusive sublist of "minimal" elements
    s_pair   *pair_list;   // S-pairs that have this element as their "first"
    gbvector *f;           // the polynomial
    gbvector *fsyz;        // its syzygy (image in the free module)
    int      *lead_exp;    // exponent vector of the leading monomial
    int       is_min;      // bitmask flags (see gb-default.hpp)
    int       me;          // index in the basis
};
```

Two intrusive linked lists thread through `gb_elem`:

- `next` walks every basis element in insertion order.
- `next_min` walks only those still considered minimal (others have been
  pruned).

Each basis element keeps its **own list of pending S-pairs** (`pair_list`)
where it is the "first" element of the pair. When the algorithm processes
all S-pairs that pair this element with earlier ones, the list shrinks to
zero and the element can be promoted to a finalised GB element.

## `s_pair`

```cpp
struct s_pair {
    s_pair   *next;
    int       compare_num;     // priority for the queue
    int       degree;          // degree of the eventual S-polynomial
    int       lcm[…];          // packed lcm of leading monomials (flexible)
    gb_elem  *first;           // one of the two basis elements
    gb_elem  *second;
    // ...
};
```

The S-pair carries enough information to determine its **selection priority**
without computing the S-polynomial yet — sugar-aware degree, lcm of the two
leading monomials, an integer tiebreaker. The polynomial is computed only
when the S-pair is finally pulled off the queue.

## The S-pair queue

The actual queue is owned by [`gbA`](file-gb-default.md) and the other GB
algorithms. It uses sugar-aware degree-first selection by default. The
`compare_num` field above is one of the tiebreakers.

## Spair pruning rules

Several pruning rules eliminate S-pairs cheaply before reduction:

- **Coprime LCM** — if `lt(f)` and `lt(g)` are coprime, the S-pair
  reduces to zero by general theory.
- **Chain criterion** — given three basis elements `f, g, h` such that
  `lt(f) | lcm(lt(g), lt(h))` and the pairs `(f, g)` and `(f, h)` are
  already in the queue, `(g, h)` is redundant.

`spair.cpp` implements these tests; the GB algorithm calls them on every
new S-pair before adding it to the queue.

## Related

- [`groebner-bases.md`](groebner-bases.md) — area overview.
- [`file-gb-default.md`](file-gb-default.md) — primary consumer.
- [`file-gbring.md`](file-gbring.md) — `gbvector` value type.
- [`file-montable.md`](file-montable.md) — leading-monomial index.
