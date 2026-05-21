# `ctype.d` — character classification table

`ctype.d` builds M2's own **256-entry character-class table** — a
private replacement for `<ctype.h>` that gives the lexer fast,
locale-independent character classification.

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's in the file

```d
--		Copyright 1994 by Daniel R. Grayson
use strings;
chartypes := new array(short) len 256 do provide short(0);
setchartype(c:char,t:int):void := chartypes.(int(uchar(c))) = short(t | int(chartypes.(int(uchar(c)))));
LOWER := 1;
UPPER := 2;
DIGIT := 4;
WHITE := 8;
NEWLINE := 16;
QUOTE := 32;
CTRL := 64;
ALNUMEXTRA := 128;
HEX := 256;
BINARY := 512;
SPACE := WHITE | NEWLINE;
ALPHA := UPPER | LOWER;
ALNUM := ALPHA | DIGIT | ALNUMEXTRA;

foreach c in "ABCDEFGHIJKLMNOPQRSTUVWXYZ"  do setchartype(c,UPPER);
foreach c in "abcdefghijklmnopqrstuvwxyz"  do setchartype(c,LOWER);
...
```

## Design

- A single 256-element `array(short)` indexed by character code.
- Bit-flag layout: each character carries a bitmask of attributes.
- Helpers like `isAlpha(c)`, `isDigit(c)`, `isWhitespace(c)` are
  tiny: `(chartypes.(c) & ALPHA) != 0`.

## Why not `<ctype.h>`

Three reasons:

1. **Locale-independence** — `<ctype.h>` honors `LC_CTYPE`. M2
   wants its lexer to behave identically regardless of locale
   (an `é` in a Spanish locale shouldn't suddenly become an
   identifier character).
2. **Custom classes** — `ALNUMEXTRA`, `BINARY`, `HEX`, and the
   M2-specific `QUOTE` aren't part of POSIX.
3. **Speed** — single array lookup, no branches.

## Used by

- [`file-lex.md`](file-lex.md) — primary consumer; every lexer
  inner loop tests `isalpha` / `isdigit` / `isspace` here.
- [`file-nets.md`](file-nets.md) — net-rendering uses character
  classification for tab handling.
- The `texmacs.d` and `xml.d` parsers do their own classification
  but call into here for fallbacks.

## Related

- [`README.md`](README.md) — d/ overview.
- [`file-lex.md`](file-lex.md) — primary consumer.
- [`file-strings.md`](file-strings.md) — provides the underlying
  `string` / `char` types.
