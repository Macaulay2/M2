# `regex.m2` — regular expression operations

`regex.m2` is the M2-side wrapper for **regular expression**
operations, layered on top of the engine's Boost.Regex binding
in [`../d/regex.dd`](../d/README.md).

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```m2
-* Copyright 2020 by Mahrud Sayrafi *-

-- See RegexFlags defined in Macaulay2/d/regex.dd for a list of available flags.
-- More flags can be added there.

needs "methods.m2"

regexSpecialChars = concatenate(
    "([", apply({"\\", "^", "$", ".", "|", "?", "*", "+", "(", ")", "[", "]", "{", "}"}, c -> "\\" | c), "])")
```

The list of `regexSpecialChars` is the set of characters that have
special meaning in a regex and need escaping to match literally.
The pattern is used by `regexQuote` and similar helpers.

## User-facing API

- **`match(pat, str)`** — `pat` matches `str`?
- **`regex(pat, str)`** — return match positions or `null`.
- **`replace(pat, repl, str)`** — substitute matches with
  replacement.
- **`select(pat, str)`** — extract matched substrings.
- **`separate(pat, str)`** — split `str` on regex matches.
- **`regexQuote str`** — escape `str` so it matches literally.

All of these dispatch to the engine's Boost.Regex implementation
via the [`../d/regex.dd`](../d/README.md) bindings.

## Regex dialect

Macaulay2 supports the standard PCRE-style regex syntax:

- Character classes `[...]`, ranges `[a-z]`.
- Anchors `^`, `$`.
- Quantifiers `*`, `+`, `?`, `{n,m}`.
- Groups `(...)` and back-references `\1`.
- Alternation `|`.

Per the header comment, additional flags (e.g. case-insensitive
matching) live in [`../d/regex.dd`](../d/README.md). The M2 side
just plumbs through.

## Author

Mostly Mahrud Sayrafi (2020 rewrite). The original regex paths
predate this file substantially but were consolidated here.

## Used by

- Engine bindings of `match`, `replace`, etc. that user packages
  rely on.
- [`file-hypertext.md`](file-hypertext.md) — `toLower` (via regex).
- File-content searches in
  [`file-files.md`](file-files.md)-routed code.
- M2's `about` (full-text search).

## Related

- [`README.md`](README.md) — m2/ overview.
- [`../d/regex.dd`](../d/README.md) — engine-side Boost.Regex
  binding.
- [`file-files.md`](file-files.md) — file-content patterns.
- Boost.Regex — external library used underneath.
