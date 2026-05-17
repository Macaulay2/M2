# `WordTable.{cpp,hpp}` — `WordTable`

`WordTable` is the **leading-word index** for non-commutative Gröbner
basis computations — the NC analogue of the commutative
[`MonomialTable`](../file-montable.md). Given a target word, it answers
"is there a basis element whose leading word *contains* this word as a
subword?" — the non-commutative analogue of divisibility.

Part of the [`NCAlgebras/`](README.md) subdirectory.

[← NCAlgebras overview](README.md) · [← engine overview](../README.md)

## State

```cpp
class WordTable {
private:
    // sketched in the header's TODO comment:
    //   std::vector<int> mIndices;        // -1 means "removed"
    //   std::vector<Word> mWords;
    //   MemoryBlock mWordStorage;
};
```

The implementation stores:

- A vector of `Word`s (the leading words).
- A parallel vector of indices that map back to the owning polynomial.
- A `MemoryBlock` ([`../file-MemoryBlock.md`](../file-MemoryBlock.md)) for
  the word storage.

The header carries a TODO about adding an "indices" vector where `-1`
marks removed entries — this is the planned soft-delete pattern that
avoids reshuffling the table on every removal.

## Operations

- **`insert(word, idx)`** — record `(word, idx)`. Returns the slot index.
- **`subword(target, out)`** — find a basis word that occurs as a subword
  of `target`. Returns its `idx` or no-match.
- **`subwords(target, out)`** — collect all subword matches.

`subword` is the workhorse: it's called millions of times per GB run,
once per word ever produced during reduction.

## Searching for subwords

Unlike the commutative case (where divisibility reduces to comparing
exponent vectors), checking whether one word contains another as a
*contiguous subword* is a string-matching problem. The naïve approach is
`O(|target| · |word|)` per query. The header notes the existence of an
alternative [`SuffixTree`](README.md)-based implementation that does this
in `O(|target|)` per query after a one-time setup; the GB driver
[`NCGroebner`](file-NCGroebner.md) is staged to swap between the two via
a comment-toggled `using` declaration.

## Memory ownership

Word data is allocated from a single `MemoryBlock`, so the entire table
can be cleared in one operation (drop the memory block) without
per-entry deallocation. Useful when the algorithm restarts at a new
degree.

## Related

- [`README.md`](README.md) — NCAlgebras overview.
- [`SuffixTree.{cpp,hpp}`](README.md) — experimental alternative.
- [`file-NCGroebner.md`](file-NCGroebner.md), [`file-NCF4.md`](file-NCF4.md)
  — primary consumers.
- [`../file-montable.md`](../file-montable.md) — commutative analogue.
- [`../file-MemoryBlock.md`](../file-MemoryBlock.md) — backing allocator.
