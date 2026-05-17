# `M2/distributions/install/` — generic install-time helpers

Generic, format-independent install-time helpers used by every packaging
flavour.

| File | Role |
|---|---|
| `Makefile.in` | Shared install rules invoked by per-format Makefiles |

This is intentionally a thin layer; most packaging logic lives in the
format-specific subdirectories ([`../dmg/`](../dmg/README.md),
[`../freebsd/`](../freebsd/README.md), [`../tar/`](../tar/README.md), etc.).

## Related

- [`../`](../README.md) — packaging overview.
