# C-side runtime glue — `M2mem.{c,h}`, `M2types.c`, `types.h`, `gmp_aux.{c,h}`, `memdebug.{c,h}`, `gdbm_interface.c`, `xml-c.{c,h}`, `python-c.c`, `boost-regex.cpp`

These are the **hand-written C / C++ files** in `Macaulay2/d/`
that supplement the `scc1`-translated `.d` files. They provide
memory management, type conversions, library glue, and exception-
safe wrappers.

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Memory primitives

| File | Role |
|---|---|
| `M2mem.h` | declares the GC allocator wrappers used by `.d`-generated C |
| `M2mem.c` | implements them — `getmem`, `getmem_atomic`, etc. |
| `memdebug.h`/`.c` | optional `--enable-memdebug` instrumentation |

`M2mem.c` is the canonical entry point for **GC allocations from
`.d` code**. Every `new Foo` in `.d` ultimately compiles to a call
through `getmem` (for pointer-bearing memory) or `getmem_atomic`
(for leaf data).

## Type conversions

| File | Role |
|---|---|
| `M2types.c` | C ↔ M2 conversions: `tostring(const char*)` etc. |
| `types.h` | shared C-side type declarations |

`M2types.c` is the C-side equivalent of the M2 conversion helpers
in `util.d` — it converts between C `char *` and M2 `M2_string`,
and similar.

## GMP / GDBM auxiliary

| File | Role |
|---|---|
| `gmp_aux.h`/`.c` | hash functions for `mpz_t`, `mpq_t`, `mpfr_t`, `mpfi_t` |
| `gdbm_interface.c` | wrappers around the GDBM key-value database |

`gmp_aux.c`'s functions (e.g. `mpz_hash`) are what
[`file-basic.md`](file-basic.md)'s `hash(Expr)` calls via
`Ccode(hash_t, ...)` for `ZZ` / `QQ` / `RR` cases.

`gdbm_interface.c` is the C side of M2's `Database` type — used
by `installPackage` to write `.dbm` info files for the help
system.

## libxml / Python / Boost glue

| File | Role |
|---|---|
| `xml-c.h`/`.c` | C-side glue for [`file-xml.md`](file-xml.md) |
| `python-c.c` | C-side glue for [`file-python.md`](file-python.md) |
| `pythoncapi_compat.h` | compatibility shim across CPython versions |
| `boost-regex.cpp` | C++-side glue for [`file-regex-dd.md`](file-regex-dd.md) |

Each `.d` FFI binding has a matching C / C++ glue file. The split
is **scc1-friendliness**: scc1 can produce C from `.d`, but it
can't easily handle e.g. CPython's reference-counting idioms or
Boost's exception-throwing API. The glue file does the unsafe
stuff in plain C / C++; the `.d` side calls into it.

## Why hand-written, not generated

`scc1` could in principle produce all of this. In practice:

- Many of these are **external-library glue** — the C declarations
  come from `<Python.h>`, `<libxml/parser.h>`, etc. Re-declaring
  them in `.d` syntax is verbose and error-prone.
- Some involve **C-side exception handling** (`boost-regex.cpp`)
  that `.d` doesn't have nice syntax for.
- Memory primitives need to be **inline / macro-able** for speed.

## Used by

- `scc1`-translated `.d` and `.dd` files in this directory.
- The C++ engine indirectly (via the shared C ABI).
- The C++ entry point in [`file-main.md`](file-main.md) and
  C startup in [`file-M2lib.md`](file-M2lib.md).

## Related

- [`README.md`](README.md) — d/ overview.
- [`../c/README.md`](../c/README.md) — `scc1` translator that
  emits the generated counterparts.
- [`file-basic.md`](file-basic.md), [`file-gmp.md`](file-gmp.md),
  [`file-xml.md`](file-xml.md), [`file-python.md`](file-python.md),
  [`file-regex-dd.md`](file-regex-dd.md) — `.d`-side consumers.
- [`file-scclib-c.md`](file-scclib-c.md) — the larger scc1 runtime
  support library.
