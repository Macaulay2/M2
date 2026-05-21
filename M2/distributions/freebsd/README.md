# `M2/distributions/freebsd/` — FreeBSD port packaging

Templates for the FreeBSD port of Macaulay2.

| File | Role |
|---|---|
| `Makefile.in` | Build glue for the FreeBSD port |
| `description` | Port description shown in `pkg info` |
| `post-install.in` | Templated post-install script (runs after the port lands on disk) |
| `post-deinstall.in` | Templated post-deinstall script (cleanup) |

Both `.in` templates have `@…@` substitutions filled in at configure time.

## Related

- [`../`](../README.md) — packaging overview.
