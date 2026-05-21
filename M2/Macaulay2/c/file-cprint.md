# `cprint.c`, `cprint.h` — C/C++ code generator (back end)

`cprint.c` is the **back end of `scc1`** — walks the type-checked
AST and emits ordinary C (or C++ for `.dd` inputs).

Part of the [`c/` scc1 translator](README.md).

[← back to c/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's exported

```c
struct POS *pos2(node);
void cprintlist(node);
void cprinttypes(void);
void pprintl(node);
void cprint(node);
void dprinttype(node e);
```

The driver function is `cprintlist(program)`. Internally it dispatches
based on the node kind (`INT_CONST`, `INSTRUCTION`, `BINARY_OP`,
etc.) to specialised emitters.

## Top-of-file thread-local hack

```c
//are we currently printing a declaration?
//used only for when in pthread mode to tell symbols not to print workaround for symbol 
int threadLocalDeclarationFlag=0;
```

A piece of code-generator state: when emitting a *declaration* in
pthread mode, certain workarounds for cross-platform `__thread`
support need to be activated. The flag is set/reset around
declaration emission.

## Emission strategy

`scc1` doesn't optimise — it emits "clear" C that the system C
compiler (gcc / clang) can optimise. Examples:

| `.d` source | Generated C |
|---|---|
| `x:int := 5` | `int x = 5;` |
| `f(x:int):int := x+1` | `static int f(int x) { return x + 1; }` |
| `when e is i:int do A is s:string do B` | `switch (e->tag) { case 0: ... case 1: ... }` |
| `Ccode(int, "abs(", x, ")")` | `abs(x)` |
| `header "#include <foo.h>"` | `#include <foo.h>` (verbatim) |

## `#line` directives

By default `scc1` emits `#line` directives in the output so C
compiler errors point back at the original `.d` source. The
`--noline` flag suppresses these (useful when reading the
generated `.c` by hand).

## `.sig` file emission

After emitting the `.c`, `cprint.c` also writes a `.sig` file —
the public-API summary of the module. Other `.d` files use these
when they `use modulename;`.

## C++ emission for `.dd`

When `do_this_cxx` is set, `cprint.c` switches to C++ output:

- Allocators use `new` instead of `getmem` (where appropriate).
- Class definitions emit `class { public: ... }`.
- Some templates allowed.

The C++ mode is more conservative than C — `.dd` files tend to
keep close to C semantics, using C++ mainly for STL containers
and exception safety.

## Used by

- [`file-scc1.md`](file-scc1.md) — `main()` calls
  `cprintlist(parservalue)` after `chkprogram`.
- The entire output `.c` / `.cpp` pipeline.

## Related

- [`README.md`](README.md) — c/ overview.
- [`file-chk.md`](file-chk.md) — input AST is type-checked
  before reaching `cprint`.
- [`file-type.md`](file-type.md) — type queries during emission.
- [`file-scc-h.md`](file-scc-h.md) — `node` definitions.
