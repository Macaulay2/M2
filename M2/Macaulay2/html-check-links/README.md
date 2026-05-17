# `M2/Macaulay2/html-check-links/` — HTML link checker

A small custom tool that walks the generated HTML documentation tree and
verifies that internal hyperlinks resolve. Invoked from the top-level
`make check` (autotools) or via a CTest target on the CMake side.

## Files

| File | Role | Deep dive |
|---|---|---|
| `html-check-links.{c,h}` | `main()` driver, demangle table, link-resolution loop | [`file-html-check-links.md`](file-html-check-links.md) |
| `grammar.y`, `grammar.h` | Bison grammar for HTML scanning | [`file-grammar.md`](file-grammar.md) |
| `lex.l` | Flex lexer for HTML tokens | [`file-lex.md`](file-lex.md) |
| `buffer.h` | `BUFFER(T)` macro — typed resizable arrays in C | [`file-buffer.md`](file-buffer.md) |
| `getmem.{c,h}` | GC-aware allocator helpers | [`file-getmem.md`](file-getmem.md) |
| `Makefile.in` | Build glue | — |

**Coverage:** every source file in this directory has a dedicated deep-dive doc.

It is intentionally self-contained — no library dependencies beyond libc and
flex/bison — so it can run early in the build pipeline before anything else is
linked.

## Related

- [`Macaulay2/m2/installPackage.m2`](../m2/installPackage.m2) — generates the
  HTML this tool checks.

[← back to repository TOC](../../../README.md#under-m2macaulay2)
