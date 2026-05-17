# `M2/Macaulay2/html-check-links/` — HTML link checker

A small custom tool that walks the generated HTML documentation tree and
verifies that internal hyperlinks resolve. Invoked from the top-level
`make check` (autotools) or via a CTest target on the CMake side.

## Files

| File | Role |
|---|---|
| `html-check-links.c`, `html-check-links.h` | Main program |
| `lex.l` | Flex lexer that pulls `<a href=…>`, `<img src=…>`, etc. out of HTML |
| `grammar.y`, `grammar.h` | Yacc grammar driving the lexer |
| `buffer.h`, `getmem.c`, `getmem.h` | Tiny support library |
| `Makefile.in` | Build glue |

It is intentionally self-contained — no library dependencies beyond libc and
flex/bison — so it can run early in the build pipeline before anything else is
linked.

## Related

- [`Macaulay2/m2/installPackage.m2`](../m2/installPackage.m2) — generates the
  HTML this tool checks.

[← back to repository TOC](../../../README.md#under-m2macaulay2)
