# `binding.d` — name binding and the global symbol table

`binding.d` implements **name binding** — the symbol table that
maps identifiers to their values, plus the scope-resolution
machinery that decides which binding a given identifier refers to.

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's declared

```d
--		Copyright 1994 by Daniel R. Grayson
use tokens;
use parser;
use lex;

-----------------------------------------------------------------------------
-- first, the global symbol table and functions for making symbols
-- thread safe because always called from enlarge and thus never has readers while appending
append(buckets:array(SymbolList), word:Word, entry:Symbol):void := (
    h := word.hash & (length(buckets) - 1);
    when buckets.h
    is null do buckets.h = SymbolListCell(word, entry, NULL)
    is e:SymbolListCell do (
        while true do (
            when e.next
            ...
        )
    )
)
```

The global symbol table is implemented as a **hash table with
chaining**. The `append` function:

1. Computes the bucket from the word's hash.
2. Appends a new `SymbolListCell` to the bucket's linked list.

The thread-safety comment is important: appending is safe because
it always happens during table growth, which is single-writer by
design.

## Scope hierarchy

M2 has three scope levels:

- **Local scope** — function arguments and let-bound names.
- **Package scope** — symbols declared inside the current package.
- **Global scope** — the global dictionary all packages share.

Lookup walks them in that order. `binding.d` maintains the data
structures for all three.

## Symbol vs. word

Two distinct types in the M2 lexicon:

- **`Word`** — the textual form of an identifier ("hello").
  Interned in the lexer's hash table ([`file-lex.md`](file-lex.md)).
- **`Symbol`** — a binding: word + scope + value. Multiple
  `Symbol`s can share a `Word` (one per scope).

`binding.d` is where Symbols are created and inserted into scopes.

## Used by

- [`file-evaluate.md`](file-evaluate.md) — looks up names during
  evaluation.
- [`file-parser.md`](file-parser.md) — recognises declared names.
- `actors*.d` — registers operator names as `Symbol`s.

## Related

- [`README.md`](README.md) — d/ overview.
- [`file-lex.md`](file-lex.md) — `Word` interning.
- [`file-tokens.md`](file-tokens.md) — `Symbol`, `Word` types.
- [`file-evaluate.md`](file-evaluate.md) — primary consumer.
- [`../m2/file-packages.md`](../m2/file-packages.md) — M2-side
  package machinery built on top of this.
