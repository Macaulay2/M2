# `scc1.c`, `scc1.h` — translator driver

`scc1.c` is the **`main()` of the `scc1` translator** — argument
parsing, file driving, and the top-level pipeline that orchestrates
the other modules (`readfile`, parser, `chk`, `cprint`).

Part of the [`c/` scc1 translator](README.md).

[← back to c/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```c
/*		Copyright 1993,2010 by Daniel R. Grayson		*/

#include "scc.h"

scope global_scope;
FILE *dependfile;
char *targetname;
char *outfilename;
```

The four globals visible at the top set the stage:

- **`global_scope`** — the outermost scope into which top-level
  declarations get inserted.
- **`dependfile`** — output stream for the `.dep` file (Makefile
  dependencies).
- **`targetname`** — the basename being built (e.g. `parser`).
- **`outfilename`** — the C/C++ file being written.

## Pipeline executed by `main()`

```
1.  parse command-line flags
2.  yyinit()              -- prime the grammar
3.  readfile(<input>.d)   -- slurp source into memory
4.  yyparse()             -- build the AST in `parservalue`
5.  chkprogram(...)       -- semantic-check the AST
6.  cprintlist(...)       -- emit C/C++ code to outfilename
7.  emit .sig (signature) file
8.  emit .dep (Make dependency) file
```

Each of these steps is implemented in its own module:

| Step | Module |
|---|---|
| `readfile` | [`file-readfile.md`](file-readfile.md) |
| `yyparse` | [`file-grammar.md`](file-grammar.md) |
| `chkprogram` | [`file-chk.md`](file-chk.md) |
| `cprintlist` | [`file-cprint.md`](file-cprint.md) |

## Command-line flags

The flags `scc1` recognises (from `scc1.h`):

| Flag | Meaning |
|---|---|
| `--cxx` | output C++ (`.cpp`) instead of C |
| `--noline` | omit `#line` directives in output |
| `--noarraychk` | disable array-bounds checks |
| `-o FILE` | output file |
| `-D NAME=VAL` | predefine a name |

These map to the `do_this_cxx`, `noline`, `arraychks` etc. globals.

## Why a custom driver instead of `make`?

`scc1` does more than just "compile one file" — it also produces
the `.sig` files that other `.d` files depend on, and the `.dep`
Makefile fragment that drives downstream builds. The Makefile
glue would be unwieldy; centralising in `scc1.c` is simpler.

## Used by

- The Macaulay2 build system: every `.d` / `.dd` in
  [`../d/`](../d/README.md) is processed by `scc1`.
- The smoke-test `foo.d` ([`file-foo.md`](file-foo.md)) in this
  directory.

## Related

- [`README.md`](README.md) — c/ overview.
- [`file-scc-h.md`](file-scc-h.md) — `scc.h` headers / globals.
- [`file-readfile.md`](file-readfile.md) — source reading.
- [`file-grammar.md`](file-grammar.md) — parser.
- [`file-chk.md`](file-chk.md) — semantic analysis.
- [`file-cprint.md`](file-cprint.md) — code generator.
