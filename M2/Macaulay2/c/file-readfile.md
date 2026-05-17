# `readfile.c`, `readfile.h` — source file reader

`readfile.c` reads a `.d` / `.dd` source file into memory and
maintains the **read cursor** (`cur`) that the lexer consumes
from.

Part of the [`c/` scc1 translator](README.md).

[← back to c/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's exported

```c
struct CURRENT {
     char *filename, *text, *eot;
     int lineno, column;
     bool wrapit;
     } ;
extern struct CURRENT cur;
```

A single global `cur` struct:

- **`filename`** — source path.
- **`text`** — pointer to source buffer (entire file).
- **`eot`** — end-of-text sentinel (one past last char).
- **`lineno`, `column`** — current 1-based position.
- **`wrapit`** — whether multi-line wrapping is active.

The lexer (in `grammar.y` / hand-written `lexer.c`-style code
inside `grammar.y`) advances `cur.text` and updates
`lineno`/`column` as it consumes input.

## Why a single global

1993-era C. Single-threaded translator. A single global is the
simplest possible thing.

## `tabwidth`

```c
int tabwidth = 8;
```

A global controlling how tabs are counted for column purposes.
Matters for error messages — "column 23" should consistently mean
the same visual position whether you have 8-column or 4-column
tabs configured in your editor.

## What `readfile` actually does

1. `fopen` the source.
2. `fseek(SEEK_END)` to find the length.
3. `malloc` a buffer.
4. `fread` the whole file.
5. `fclose`.
6. Set `cur.text`, `cur.eot`, `cur.filename`.
7. Initialise `lineno = 1, column = 1`.

After that, the lexer reads from `cur` until `cur.text >=
cur.eot`.

## Used by

- [`file-scc1.md`](file-scc1.md) — calls `readfile` near start
  of `main()`.
- The lexer inside `grammar.y` — reads from `cur`.
- [`file-error.md`](file-error.md) — uses `cur.lineno` /
  `cur.column` for error positions.

## Related

- [`README.md`](README.md) — c/ overview.
- [`file-grammar.md`](file-grammar.md) — primary consumer.
- [`file-error.md`](file-error.md) — uses position info for error
  messages.
