# `loadsequence` — the Core load order

`loadsequence` is a **plain-text manifest** that lists every `.m2`
file the Core package loads, in the order they must be loaded. It is
the single source of truth for Core's load order — adding a new
`.m2` file requires inserting it here.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## Format

Plain text, one filename per line. Comments begin with `--`. Blank
lines are separators.

```text
run.m2
classes.m2
option.m2
methods.m2
shared.m2
autoload.m2
system.m2
regex.m2
threads.m2

profile.m2
debugging.m2
remember.m2
iterators.m2
files.m2

...
```

The blank lines aren't significant to the loader — they're
visual grouping for human readers.

## Why ordering matters

M2 has no forward declarations. If file `A.m2` uses a symbol that
file `B.m2` defines, `B.m2` must come **before** `A.m2` in the
sequence. Most of the load order is constrained:

1. `run.m2` — system-level boot stuff (must be first).
2. `classes.m2` — the type system (`Type`, `class`, `parent`).
3. `option.m2`, `methods.m2`, `shared.m2` — language primitives
   (option handling, method dispatch, shared values).
4. `autoload.m2`, `system.m2`, `regex.m2`, `threads.m2` —
   environment + OS interaction.
5. Higher-level features (debugging, files, lists, …) follow.

## How a new file gets added

To add a new `.m2` file `foo.m2`:

1. Place the file in [`m2/`](README.md).
2. Insert its name into `loadsequence` at the right position —
   after every file it `needs`-includes, before every file that
   `needs`-includes it.
3. Add a `needs "foo.m2"` directive in any file that depends on it.

`needs` is the M2 directive used inside `.m2` files to declare
dependencies. The loader uses `loadsequence` plus `needs` directives
to resolve the actual load order; missing or out-of-order entries
produce an early error.

## Companion: `exports.m2`

After the load sequence completes, [`file-exports.md`](file-exports.md)
declares which symbols are public. The two files together define
Core's surface area.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-Core.md`](file-Core.md) — package tuple.
- [`file-exports.md`](file-exports.md) — symbol exports.
- [`file-autoload.md`](file-autoload.md) — lazy loading on top of this.
