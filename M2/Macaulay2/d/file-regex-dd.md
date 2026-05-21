# `regex.dd` — Boost.Regex bindings

`regex.dd` provides M2's **Boost.Regex bindings** — the underlying
implementation of `match`, `regex`, `replace`, `separate`, `select`
that [`../m2/file-regex.md`](../m2/file-regex.md) surfaces.

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```d
-- Copyright 2020 by Mahrud Sayrafi

use arithmetic;
use hashtables;
use common;
use util;

-----------------------------------------------------------------------------
```

Authored by Mahrud Sayrafi (2020). The `.dd` extension is forced
because Boost.Regex is C++-only — the bindings need C++ to call
into `boost::regex` API.

## `RegexFlags`

The file defines a `RegexFlags` enum exposing the standard
Boost.Regex flags M2 users can pass:

- **`Extended`** — POSIX extended regex syntax.
- **`Icase`** — case-insensitive matching.
- **`Newline`** — `.` matches any character except newline.
- **`Optimize`** — pre-optimise the pattern.
- **`Perl`** — PCRE-style syntax.

The flag names are exported to M2 so users can write:

```m2
match("(?i)foo", "FOO bar")    -- equivalent to passing Icase
```

## What `regex.dd` exposes

The primitives the M2 layer in
[`../m2/file-regex.md`](../m2/file-regex.md) wraps:

- **`regexMatch`** — boolean match test.
- **`regexSearch`** — find first match with position info.
- **`regexReplace`** — substitute.
- **`regexSeparate`** — split.
- **`regexQuote`** — escape special characters.

Plus error handling for malformed patterns (Boost.Regex throws
exceptions; the binding converts to M2 errors).

## Why Boost.Regex

Three reasons:

1. **Reliable PCRE-style behaviour** across platforms.
2. **Well-tested** — Boost.Regex has been in widespread use for
   over a decade.
3. **Header-only or linked** — flexibility in how M2 ships.

`std::regex` could also work but has historically had quality
issues; `boost::regex` is the safer choice.

## Companion: `boost-regex.cpp`

A C++ file in this directory provides the glue between Boost's
exception-throwing API and M2's error-flag-based error model.
`regex.dd` declares the bindings; `boost-regex.cpp` is where the
unwinding happens.

## Used by

- [`../m2/file-regex.md`](../m2/file-regex.md) — primary consumer.
- M2 users via `match`, `replace`, etc.
- File-content searches.

## Related

- [`README.md`](README.md) — d/ overview.
- [`../m2/file-regex.md`](../m2/file-regex.md) — M2-side wrapper.
- `boost-regex.cpp` — C++ glue.
- Boost.Regex — external linked library.
