# `lex.d` — the M2 lexer

`lex.d` implements the **lexer** — the bit of the interpreter that
reads characters from input and produces tokens for the parser to
consume.

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's declared

```d
--		Copyright 1994 by Daniel R. Grayson
-- dictionary entries
use ctype;
use tokens;
use varstrin;

export wordEOF := dummyWord;       -- replaced later
export wordEOC := dummyWord;       -- replaced later
export (o:file) << (w:Word) : file := o << w.name;
export WordListCell := { word:Word, next:WordList };
export WordList := WordListCell or null;
export hashTable := (
    new array(WordList)
    len 7313                       -- just a convenient prime number
    do provide null()
)
```

Three pieces:

- **`use ctype`, `tokens`, `varstrin`** — pull in character
  classification, the token type, and variable-length string
  builders.
- **Sentinel words** — `wordEOF` (end-of-file), `wordEOC`
  (end-of-command). Lexer-distinguished terminators.
- **`hashTable`** — global symbol table indexed by hash, 7313
  buckets. This is the **only** place lookup of "is this identifier
  already known?" happens.

## Why 7313

The comment "just a convenient prime number" is the only
justification given. 7313 is prime, big enough to keep chains short
for a typical M2 session's symbol count (a few thousand), and small
enough to fit comfortably in L2 cache.

## What lexing produces

For an input like:

```m2
R = QQ[x, y]
```

The lexer produces a stream of `Token`s:

1. `R` (identifier)
2. `=` (operator)
3. `QQ` (identifier)
4. `[` (delimiter)
5. `x` (identifier)
6. `,` (delimiter)
7. `y` (identifier)
8. `]` (delimiter)
9. End-of-command

Each `Token` carries a position (file + line + col) so the parser
and error reporter can produce useful messages.

## Sentinels

`wordEOF` and `wordEOC` are typed as `dummyWord` at declaration time
and **replaced later** during the lexer's own initialisation. The
two-step is forced by the `.d` language: types and values must be
declared before they're used; circular dependencies need fixup.

## Used by

- [`file-parse.md`](file-parse.md) — primary consumer.
- [`file-parser.md`](file-parser.md) — token stream.
- Everything that reads M2 source — interactive prompt, file load.

## Related

- [`README.md`](README.md) — d/ overview.
- [`file-tokens.md`](file-tokens.md) — `Token` type.
- [`file-parse.md`](file-parse.md), [`file-parser.md`](file-parser.md)
  — parser layers.
- `ctype.d` — character classification.
- `varstrin.d` — variable-length string builders.
