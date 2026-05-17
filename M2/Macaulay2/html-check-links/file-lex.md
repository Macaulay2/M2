# `lex.l` — Flex lexer for HTML scanning

`lex.l` is the **Flex lexer** that feeds the Bison grammar. It
emits tokens for HTML structure (`<`, `>`, `/`, `=`, names,
strings, text) plus line/column tracking for error reporting.

Part of [`html-check-links/`](README.md).

[← html-check-links/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```c
%{
#include <unistd.h>
#include "getmem.h"
#include "grammar.h"
#include "grammar.tab.h"
unsigned lastcount;
#define BOL lineno++, column = 1
#define COUNT column += lastcount = yyleng
#define COUNTN do { \
		  int i; \
		  lastcount = yyleng; \
		  for(i=0; i<yyleng; i++) if (yytext[i]=='\n') BOL; else column++; \
		  } while (0)
static void yyunput(int, char *) __attribute__ ((unused));
char *filename = "", *Dirname="", *rootname = "";
int lineno = 1, column = 1, tagline, tagcol;
int yywrap(void) { lineno = 1, column = 1; return 1; }
#define YY_NO_INPUT
%}
```

The macros at the top are **line/column tracking**:

- **`BOL`** — beginning of line: increment `lineno`, reset
  `column = 1`.
- **`COUNT`** — count one token's worth of columns.
- **`COUNTN`** — count a multi-line token (e.g., a string
  containing newlines).

Flex matches tokens but doesn't intrinsically know about lines —
the lexer rules must call `BOL`/`COUNT` explicitly.

## What the lexer recognises

Token classes (approximate):

| Pattern | Token |
|---|---|
| `[<]` | `LT` |
| `[>]` | `GT` |
| `[/]` (inside tag) | `SLASH` |
| `[=]` | `EQ` |
| `[a-zA-Z][a-zA-Z0-9_-]*` | `NAME` |
| `"..."` or `'...'` | `STRING` |
| Anything else | `TEXT` (discarded) |
| `<!--` ... `-->` | comment (skipped) |
| `<!DOCTYPE ...>` | doctype (skipped) |

The lexer is **mode-based**: outside a tag, only `<` and text;
inside a tag, names/attributes/strings/`>`. Flex's `BEGIN(state)`
mechanism handles the modes.

## `YY_NO_INPUT`

```c
#define YY_NO_INPUT
```

A Flex directive that disables the generated `input()` function.
This shaves the binary slightly and silences a warning when
`input()` is unused.

## `yywrap`

```c
int yywrap(void) { lineno = 1, column = 1; return 1; }
```

Called by Flex when EOF is reached on the current input. Returns
non-zero to signal "no more input." The line/column reset
prepares for the next file (the tool processes files in
sequence).

## Used by

- [`file-grammar.md`](file-grammar.md) — Bison grammar calls
  `yylex()` (Flex-generated).
- [`file-html-check-links.md`](file-html-check-links.md) — the
  main program drives the lexer indirectly via `yyparse()`.

## Related

- [`README.md`](README.md) — html-check-links overview.
- [`file-grammar.md`](file-grammar.md) — grammar consuming these
  tokens.
- Flex / Bison — external build dependencies (typical on most
  systems).
