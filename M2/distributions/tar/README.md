# `M2/distributions/tar/` — portable tarball distribution

Build glue for the portable `.tar` distribution of Macaulay2 — the
architecture-independent (or single-architecture) tarball produced by
`make tar` (autotools) or CPack (CMake).

| File | Role |
|---|---|
| `Makefile.in` | Tarball assembly rules |

The list of paths *excluded* from the tarball lives at the parent level in
[`../tar-exclusions`](../tar-exclusions).

## Related

- [`../`](../README.md) — packaging overview.
- [`../tar-exclusions`](../tar-exclusions) — paths to omit.
