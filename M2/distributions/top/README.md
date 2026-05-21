# `M2/distributions/top/` — top-level distribution files

Templates for the user-facing files placed at the top of every Macaulay2
distribution (regardless of packaging format).

| File | Role |
|---|---|
| `INSTALL.in` | Template for the `INSTALL` file shown to end users |
| `Makefile.in` | Build rules for the templated outputs |
| `postinstall.in` | Templated generic post-install script |
| `preremove.in` | Templated generic pre-remove script (uninstall) |

These templates are independent of the chosen packaging format — the same
`INSTALL` is delivered in the `.dmg`, `.tar`, `.deb`, etc. Format-specific
overrides live in the sibling [`dmg/`](../dmg/README.md),
[`freebsd/`](../freebsd/README.md), and similar directories.

## End-user `INSTALL`

Edit [`INSTALL.in`](INSTALL.in) — never the generated `INSTALL` file in a
build tree.

## Related

- [`../`](../README.md) — packaging overview.
