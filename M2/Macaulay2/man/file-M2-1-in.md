# `M2.1.in` — the `M2(1)` man page template

`M2.1.in` is the **groff source** for M2's Unix man page. The
`.in` extension means it's a template — `configure` substitutes
`@PACKAGE_TARNAME@` / `@PACKAGE_VERSION@` / `@datarootdir@` to
produce the installable `M2.1`.

Part of [`man/`](README.md).

[← man/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```groff
.TH M2 1 "" "@PACKAGE_TARNAME@-@PACKAGE_VERSION@" "@PACKAGE_NAME@"
.\" see groff(7) and groff_man(7)
.\" Mention @datarootdir@
.SH "NAME"
M2 - start \fIMacaulay2\fP, a software system for algebraic geometry research.

.SH SYNOPSIS
\fBM2\fP [\fIoption\fP ...] [\fIfile\fP ...]
.SH DESCRIPTION
The command \fBM2\fP starts \fIMacaulay2\fP, which is a software system devoted to supporting research in algebraic geometry and commutative algebra.
```

Standard `groff_man(7)` structure:

- **`.TH NAME SECTION ...`** — page header: name, section number,
  date, version, source.
- **`.SH NAME`** — name section with the standard one-liner
  format the `whatis` database parses.
- **`.SH SYNOPSIS`** — invocation syntax with bold (`\fB`) and
  italic (`\fI`) markup.
- **`.SH DESCRIPTION`** — main prose body.

The `.\"` lines are groff comments — the second one (`Mention
@datarootdir@`) is a reminder to the author that the file
should mention the data root directory.

## Template variables

| `@VAR@` | Filled in |
|---|---|
| `@PACKAGE_TARNAME@` | `Macaulay2` (the package tar prefix) |
| `@PACKAGE_VERSION@` | e.g. `1.26.05` |
| `@PACKAGE_NAME@` | `Macaulay2` |
| `@datarootdir@` | e.g. `/usr/share` |

Substitution happens at autotools `config.status` time. The
resulting `M2.1` installs to `${mandir}/man1/M2.1` (typically
`/usr/share/man/man1/M2.1`).

## Section 1, not Section 7

M2's man page is in **Section 1** (user commands), not Section 7
(miscellaneous). The convention:

- Section 1 = "if I type this name at a shell, what happens?"
- Section 7 = "what is this convention / format / system?"

`M2` is a binary you invoke; section 1 fits.

## What it covers

Beyond the snippet above, the man page typically covers:

- Command-line options (`--script`, `--no-prompt`, `--check`, `--help`,
  `--version`).
- Environment variables (`M2_INTERPRETER`).
- Files (locations of `Core.m2`, package docs, etc.).
- See-also references (other M2 tools).
- Author / license / bug-report addresses.

## How users see it

```sh
man M2
M2 -- start Macaulay2, a software system for algebraic geometry research
M2 [option ...] [file ...]
...
```

On a sensibly-configured system. M2 is one of the few math
systems with a real man page; many others ship only HTML/PDF
docs.

## CMake equivalent

The CMake build has its own man-page install logic in
[`../bin/CMakeLists.txt`](../bin/), but the source file is the
same (the autotools template). The two build systems pick up
this file uniformly.

## Used by

- The autotools install target (`install-data`).
- The CMake install target.
- Distribution packagers including the man page in their
  `.deb`/`.rpm`.

## Related

- [`README.md`](README.md) — man/ overview.
- [`../bin/file-main.md`](../bin/file-main.md) — the binary this
  man page describes.
- `groff(1)`, `groff_man(7)` — the format reference.
