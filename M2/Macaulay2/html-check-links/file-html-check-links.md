# `html-check-links.c`, `html-check-links.h` — main program

`html-check-links.c` is the **`main()` driver** for the
`html-check-links` standalone tool. Walks the generated HTML
docs, extracts every `href`/`src`, and verifies the targets exist.

Part of [`html-check-links/`](README.md).

[← html-check-links/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```c
/* Copyright 1998 by Daniel R. Grayson */

#include <stdlib.h>
#include <M2/gc-include.h>
#include "html-check-links.h"
#include "grammar.h"
#include "getmem.h"
int ERRLIMIT = 64;
int abs_links = TRUE;
int verbose = FALSE;
extern FILE *yyin, *yyout;
extern int yyparse(void);
```

Three configuration globals:

- **`ERRLIMIT = 64`** — after 64 reported errors, stop. Avoids
  drowning the user.
- **`abs_links = TRUE`** — verify absolute (cross-document) links.
- **`verbose = FALSE`** — extra noise.

The flex/bison-style `yyin`, `yyout`, `yyparse` declarations show
the tool is built on **flex + bison** like other M2 mini-parsers.

## Demangle table

```c
static char *tab[][2] = {
   {" " , "sp"},
   {"*" , "st"},
   {"|" , "vb"},
   ...
};
```

A character-name table for **demangling** the M2 documentation
file-name encoding. M2's HTML doc generator escapes special chars
in identifiers as `_sp_`, `_st_`, etc. (since the FS can't have
spaces in URLs). To verify a link like
`Macaulay2Doc/foo___sp__bar.html`, the checker first demangles
back to `foo bar.html` and looks for that.

## `html-check-links.h`

```c
#ifndef C2_H
#define C2_H
#include <stdio.h>
#define TRUE 1
#define FALSE 0
#define put(s) fputs(s,stdout)
#define EQUAL 0
#define numberof(x) (sizeof(x)/sizeof(x[0]))
#define forarray(i,x) for(i=0; i<numberof(x); i++)
#define forlist(p) for(;p;p=p->next)
#define ckarray(x,i) (assert(i >= 0),assert(i < numberof(x)))
```

Minimal utility macros — the file pre-dates `<stdbool.h>` and
standard library conveniences. Defines `TRUE`/`FALSE`, iteration
helpers (`forarray`, `forlist`), and array-bounds checks
(`ckarray`).

## What `main()` does

The driver loop:

1. Parse command-line: `-v` for verbose, `--root <dir>`, etc.
2. For each HTML file in the doc tree:
   a. Run `yyparse()` over it (lexer extracts hrefs).
   b. For each href found, resolve relative to the file's path.
   c. If the target doesn't exist as a file, report a broken
      link.
3. Exit with non-zero code if any errors were found.

## Why a custom tool?

Other broken-link checkers (W3C link checker, `linkchecker`,
`htmlproofer`) exist but:

- They have **heavy dependencies** (perl, ruby, python).
- They **don't understand** M2's filename mangling.
- They're **slower** — overkill for a build-time check.

A small C tool keeps the build self-contained and fast.

## Used by

- `make check` (autotools).
- CTest target on CMake builds.
- CI on every PR (catches doc breakage).

## Related

- [`README.md`](README.md) — html-check-links overview.
- [`file-grammar.md`](file-grammar.md) — Bison grammar.
- [`file-lex.md`](file-lex.md) — Flex lexer.
- [`../m2/file-installPackage.md`](../m2/file-installPackage.md)
  — generates the HTML that's checked.
