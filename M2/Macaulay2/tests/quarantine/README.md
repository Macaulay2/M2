# `M2/Macaulay2/tests/quarantine/` — disabled tests awaiting fixes

This directory contains **disabled tests** — ones that previously
passed but currently fail for known reasons. They are kept in the
tree as a record of the regression: each file there represents a
specific bug we want to one day fix.

[← back to tests overview](../README.md)

## What's in here

10 disabled tests:

| File | Topic |
|---|---|
| `2-homog-bug.m2` | Homogeneous-GB edge case |
| `C05.m2` | Engine boundary issue |
| `HH.m2` | Hilbert function regression |
| `Makefile.in` | Build glue (excludes tests from default run) |
| `issac-97.m2` | An ISSAC-97 example that broke |
| `ker8.m2` | Kernel computation regression |
| `lapack.m2` | LAPACK path issue |
| `newlines.m2` | Output-format newline handling |
| `res9.m2` | Resolution edge case |
| `testmulti.m2` | Multigraded test |

The `Makefile.in` here is configured to **not** run these tests by
default — that's the "quarantine" part.

## How tests end up here

Two paths:

1. **Triage** — a CI failure is identified as "this is a real
   regression, but we don't have a fix yet." The test moves from
   [`normal/`](../normal/README.md) to here so the rest of CI
   stays green.
2. **Engine refactor** — a test breaks during a known engine change.
   Moving it here documents the breakage and gives the refactor a
   record to come back to.

## When are tests rescued?

When a fix is identified and lands, the corresponding test moves
back to [`normal/`](../normal/README.md) (or [`slow/`](../slow/README.md)
if appropriate). The PR doing so should reference the original
issue.

If a test sits in `quarantine/` for a very long time, that's a
signal: either the underlying issue is no longer relevant (delete
the test) or it's a hard problem that needs explicit prioritisation.

## Running them anyway

For debugging, you can run quarantined tests manually:

```sh
M2 --silent --no-debug < tests/quarantine/<file>.m2
```

Just don't expect them all to pass.

## Related

- [`../README.md`](../README.md) — overall test-suite overview.
- [`../normal/README.md`](../normal/README.md) — where these tests
  should eventually return.
- [`../engine/README.md`](../engine/README.md) — bulk-skipped engine
  tests with a different rationale.
- [GitHub Issues](https://github.com/Macaulay2/M2/issues) — typically
  has matching tickets for each quarantined test.
