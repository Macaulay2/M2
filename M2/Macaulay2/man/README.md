# `M2/Macaulay2/man/` — Unix man pages

Source for the `M2(1)` man page.

| File | Role |
|---|---|
| `M2.1.in` | Templated man-page source. `@…@` substitutions are filled in at configure time |
| `Makefile.in` | Build glue that runs the substitution and installs into `share/man/man1/` |

The installed man page is intentionally minimal — long-form documentation for
the Macaulay2 language lives in the in-language help system (`help`,
`viewHelp`) and in the package HTML docs produced by
[`installPackage`](../m2/installPackage.m2).

## Related

- [`Macaulay2/docs/`](../docs/README.md) — Sphinx docs for the C++ engine
  (developer-facing, separate from this man page).

[← back to repository TOC](../../../README.md#under-m2macaulay2)
