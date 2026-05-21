# `SuffixTree.{cpp,hpp}` — generalised suffix tree (experimental)

`SuffixTree` is an **experimental** alternative to
[`WordTable`](file-WordTable.md) for indexing the leading words of basis
elements in non-commutative Gröbner basis computations. It exposes the
same interface but uses a generalised suffix-tree data structure that
delivers `O(|target|)`-per-query subword search, vs. the naïve word-table's
`O(|target| · |basis|)`.

Part of the [`NCAlgebras/`](README.md) subdirectory.

[← NCAlgebras overview](README.md) · [← engine overview](../README.md)

## What a generalised suffix tree is

A suffix tree of a single string `s` is a compact trie of all suffixes of
`s`. Searching for a substring `t` walks down the tree along `t`; if the
walk completes, `t` is a substring and the leaf below the walk records
its position. Construction is `O(|s|)` (Ukkonen / McCreight) and search
is `O(|t|)`.

A **generalised** suffix tree extends this to a *set* of strings (the
non-commutative leading words, in our case). Each leaf now records
*which* string + position. Search remains `O(|t|)`.

## Node structure

```cpp
class SuffixTreeNode {
public:
    friend std::ostream &operator<<(std::ostream &, const SuffixTreeNode &);
    std::ostream &dump(std::ostream &, int depth, bool dumpChildren = true) const;

    SuffixTreeNode() : /* root constructor */ { }
    // ...
};
```

Each node carries:

- An **edge label** — a `std::vector<int>` (variable indices).
- A **child map** — `std::map<int, SuffixTreeNode*>` keyed by next-int.
- An optional **leaf marker** — basis index + position when this node is
  a suffix endpoint.

Construction goes through Ukkonen's online algorithm so insertion of a
new basis word doesn't require rebuilding from scratch.

## Status

The header is decorated **"experimental"**: the comment block in
[`NCGroebner.hpp`](file-NCGroebner.md) shows the consumer staged to swap
between `WordTable` and `SuffixTree` via toggled `using` declarations.
Today the production path uses `WordTable`; benchmarking on real inputs
will decide whether to switch.

## Why it might win

For dense non-commutative inputs (many overlaps, long words), per-query
performance dominates. The suffix tree's `O(|target|)` query is a
significant constant-factor win. For sparse inputs the per-edge overhead
hurts; the word table wins.

## Related

- [`README.md`](README.md) — NCAlgebras overview.
- [`file-WordTable.md`](file-WordTable.md) — production alternative.
- [`file-NCGroebner.md`](file-NCGroebner.md), [`file-NCF4.md`](file-NCF4.md)
  — eventual consumers.
- [`file-Word.md`](README.md) — `Word` type the tree indexes.
