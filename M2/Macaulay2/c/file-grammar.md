# `grammar.y`, `grammar.h`, `keywords.h` — the parser

`grammar.y` is the **Yacc/Bison grammar** for the `.d` language.
`grammar.h` declares the parser API (`yyparse`, `parservalue`,
`yyinit`); `keywords.h` is the reserved-word table fed to the
lexer.

Part of the [`c/` scc1 translator](README.md).

[← back to c/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## `grammar.y` — the Bison grammar

```yacc
/*		Copyright 1993 by Daniel R. Grayson		*/

   /* declarations */

%{
#include "scc.h"
#define YYSTYPE node
node parservalue;
```

The grammar:

- **`YYSTYPE`** is `node` — every grammar rule's value is an AST
  node.
- **`parservalue`** is the final result variable set by the
  top-level rule.
- The whole grammar is hand-tuned LALR(1).

The `.d` syntax it accepts is documented in the plain-text
[`README`](README) file. Highlights:

- `Type "..."` — declare a type.
- `e := value` — short-form definition.
- `f(x:T):R := body` — function with typed args.
- `when e is x:T do branch else branch` — type-cased pattern match.
- `Ccode(t, "...", args, "...")` — inline C escape hatch.
- `header "..."` — emit a literal block into the generated `.c`.

## `keywords.h` — reserved words

```c
f("!",not)
f("!=",unequal)
f("&&",andand)
f("+",plus)
f("++",plusplus)
f(",",comma)
f("-",minus)
f(".",dot)
```

A long sequence of `f(text, name)` macro invocations. The macro
`f` is `#define`d differently by callers to:

- Build a hash table of operator → token-id (lexer).
- Build a switch over token-id (parser).
- Build documentation tables.

This **X-macro** pattern lets the keyword list be defined once
and consumed multiple ways.

## `grammar.h` — declarations

```c
extern int yyparse(void);
extern node parservalue;
extern int yydebug;
extern void yyinit(void);
int setopleft(int priority, char *str);
int setopright(int priority, char *str);
```

The translator-side parser interface:

- **`yyparse()`** — Bison's generated entry point.
- **`yyinit()`** — initializes the keyword table, operator
  priorities.
- **`setopleft` / `setopright`** — adjust operator precedences at
  startup (a `.d` quirk: precedences are partially data-driven).

## How the parser interacts with the lexer

`scc1`'s lexer is hand-written, not generated. It tokenises the
source from `readfile`'s buffer and feeds tokens to Bison's
`yyparse` via the `yylex()` callback. The lexer:

- Skips whitespace and comments.
- Recognises identifiers and looks them up in the keyword table
  (from `keywords.h`).
- Recognises numeric, character, and string literals.
- Returns token IDs to Bison.

## Used by

- [`file-scc1.md`](file-scc1.md) — calls `yyparse()` from
  `main()`.
- [`file-chk.md`](file-chk.md) — receives the AST from the
  parser.

## Related

- [`README.md`](README.md) — c/ overview.
- [`README`](README) — plain-text `.d` language spec.
- [`file-scc-h.md`](file-scc-h.md) — `node` type used as
  `YYSTYPE`.
- [`file-readfile.md`](file-readfile.md) — source the lexer
  reads from.
