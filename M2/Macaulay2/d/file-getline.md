# `getline.d` — read-a-line primitive

`getline.d` provides **`getLine(file)`** — the basic read-a-line
primitive used by the REPL, file loading, and any code that wants
line-buffered input.

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's in the file

```d
--		Copyright 1994-2000 by Daniel R. Grayson
use tokens;
threadLocal tokenbuf := newvarstring(100);
export getLine(o:file):StringOrError := (
     ch := 0;
     while (
	  ch = getc(o);
	  if iserror(ch) then return StringOrError(errmsg("failed to read file : "+syserrmsg()));
	  !(isnewline(ch) || iseof(ch))
	  )
     do (
	  tokenbuf << char(ch);
	  );
     StringOrError(stringCell(takestring(tokenbuf))));
```

The whole file is essentially one function plus a thread-local
`varstring` buffer.

## How it works

1. Grab the thread-local `tokenbuf` (a `varstring` that gets
   reused across calls).
2. Read characters one at a time via `getc(o)`.
3. Each character that isn't newline / EOF is appended to
   `tokenbuf`.
4. On newline / EOF, snapshot `tokenbuf` to a `string` and
   return it.

The `threadLocal` annotation matters — multiple threads reading
lines simultaneously each have their own buffer.

## `StringOrError`

The return type is a sum: either a `String` (success) or an
`error` (read failure). Callers pattern-match:

```d
when getLine(o) is s:string do ... is e:error do ...
```

This is the standard `.d` idiom for "operation that can fail" —
explicit at the type level, no exceptions across the C ABI.

## What "getLine" doesn't include

- **No line continuation** — backslash-newline is just two chars.
- **No prompt printing** — that's the REPL's job.
- **No history** — handled by the REPL (or readline / editline
  separately).
- **No tab completion** — same.

It's a deliberately minimal primitive.

## Used by

- The REPL in [`file-interp.md`](file-interp.md).
- File-loading paths.
- `getline()` exposed to M2 user code.
- The TeXmacs frontend ([`file-texmacs.md`](file-texmacs.md))
  for prompt input.

## Related

- [`README.md`](README.md) — d/ overview.
- [`file-stdio.md`](file-stdio.md) — provides `getc`, `iseof`,
  `isnewline`.
- [`file-strings.md`](file-strings.md) — provides `varstring`.
- [`file-interp.md`](file-interp.md) — primary consumer.
