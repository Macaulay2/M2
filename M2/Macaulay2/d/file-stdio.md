# `stdio.d` and `stdio0.d`, `stdiop.d`, `stdiop0.d` — buffered I/O

The `stdio*` family of files implements **buffered I/O** for the
M2 interpreter — file streams, line buffering, position tracking,
the read/write primitives every `.d` file uses for I/O.

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Why four files

The split is forced by the `.d` language's no-forward-reference
rule plus the fact that parser-level declarations
([`file-parse.md`](file-parse.md)) want to use file types without
pulling in the full I/O machinery:

| File | Role |
|---|---|
| `stdio0.d` | Bare type declarations (used by `parse.d`) |
| `stdiop0.d` | Position-tracking declarations (used by `parse.d`) |
| `stdio.d` | Full I/O implementation (operates on stdio0's types) |
| `stdiop.d` | Full position-tracking I/O (uses stdiop0's types) |

The `0` suffix means "declarations only, no implementations." The
suffix-free versions add the implementations.

## What I/O looks like in `.d`

```d
use stdio;

stdout << "Hello, M2!\n";

f := openIn "/tmp/data";
when readContents f
is s:string do print(s)
is null do error("read failed")
```

Three operations:

- **`<<`** — write to a file or stream.
- **`openIn` / `openOut`** — create file handles.
- **`readContents`, `getline`, `readBytes`** — read primitives.

All of these are declared somewhere in the stdio family.

## Position tracking

`stdiop.d` adds **position tracking** to file streams: every read
records the current line and column, so the parser can report
"error at file.m2:42:7" instead of just "error".

The position tracking is what makes M2 error messages useful for
debugging.

## Used by

- [`file-parser.md`](file-parser.md) — parses position-tracked
  input.
- [`file-interp.md`](file-interp.md) — reads from `stdin` at the
  prompt.
- Every `.d` file that does I/O.

## Related

- [`README.md`](README.md) — d/ overview.
- [`file-parse.md`](file-parse.md) — uses stdio0/stdiop0 declarations.
- [`file-parser.md`](file-parser.md) — full implementation.
- `getline.d` — line-input helper layered on top.
- `texmacs.d` — TeXmacs frontend wrapping of stdio.
