# `f4-monlookup.{cpp,hpp}` — `F4MonomialLookupTableT<Key>`

`f4-monlookup.hpp` defines **`F4MonomialLookupTableT<Key>`** — a
**templated tree-structured monomial index** used by F4 to answer
divisibility queries efficiently. It implements an explicit *monomial
ideal* trie internally; its node type is exposed at the top of the
header.

Part of the [`f4/`](README.md) subdirectory.

[← f4 overview](README.md) · [← engine overview](../README.md)

## Node structure

```cpp
template <typename Key>
class F4MonomialLookupTableT {
    struct mi_node {  // monomial ideal internal node
        varpower_word  var;
        varpower_word  exp;
        mi_node       *left;
        mi_node       *right;
        mi_node       *header;
        enum { node, leaf } tag;
        union {
            mi_node *down;  // 'up' node, if this is a head of a list
            Key      key;   // payload, if this is a leaf
        };
        // ...
    };

    // ...
};
```

Each `mi_node` is a node in a binary-tree representation of a
**monomial ideal**:

- **`var`, `exp`** — the variable index and exponent test the node
  performs.
- **`left`, `right`** — children (the standard "matches" / "doesn't
  match" split).
- **`header`** — back-pointer for fast traversal.
- **`tag`** — discriminates between `node` (internal) and `leaf` (a
  registered basis element).
- **`down` vs. `key`** — `union` based on `tag`: internal nodes link
  down to deeper tests, leaves carry the user's `Key` (typically a
  polynomial index).

## What it does

For each leading monomial inserted, the table extends the tree along
the path defined by the monomial's variable-exponent pairs. A
divisibility query for a target monomial walks the tree, taking the
"matches" branch whenever the target's exponent at the node's variable
is at least the node's exponent.

The first leaf reached is a polynomial whose leading monomial divides
the target. If no leaf is reached, no basis element's leading monomial
divides the target.

## Templated `Key`

The class is templated on `Key`, the payload type:

- F4 uses `Key = int` (polynomial index in the basis).
- The resolution code in
  [`../schreyer-resolution/`](../schreyer-resolution/README.md) uses
  the same machinery with a richer `Key`.

This template parameterisation lets the same tree implementation serve
multiple subsystems.

## Operations

- **`insert(monomial, key)`** — add a (monomial, payload) pair.
- **`find_divisor(monomial)`** — return any payload whose monomial
  divides the input, or no-match.

Both run in time proportional to the monomial's variable count
(*not* the basis size). On dense bases this is dramatically faster
than the simpler `MonomialTable` ([`../file-montable.md`](../file-montable.md)).

## Related

- [`README.md`](README.md) — f4 overview.
- [`file-moninfo.md`](file-moninfo.md) — monomial layout used by Keys.
- [`file-varpower-monomial.md`](file-varpower-monomial.md) — `varpower_word`
  type used in nodes.
- [`../file-montable.md`](../file-montable.md) — simpler list-based
  index used by `gbA`.
- [`../gb-f4/file-MonomialLookupTable.md`](../gb-f4/file-MonomialLookupTable.md)
  — refactored counterpart.
