# Cross-reference: the three `ZZ` paths

This is a **navigation-only** document that compares the engine's
three integer-ring implementations side by side. It does not introduce
a new file — each path has its own deep-dive linked below.

Part of the [Coefficient rings](coefficient-rings.md) area.

[← per-area: coefficient-rings](coefficient-rings.md) · [← engine overview](README.md)

## The three ZZ paths

| Path | File | Approach | Default? |
|---|---|---|---|
| Legacy `ZZ` (`Ring`-based) | [`file-ZZ.md`](file-ZZ.md) | GMP `mpz_t`, virtual dispatch | No |
| `ARingZZGMP` (aring + GMP) | [`file-aring-zz-gmp.md`](file-aring-zz-gmp.md) | GMP `mpz_t`, aring (CRTP) | No |
| `ARingZZ` (aring + FLINT) | [`file-aring-zz-flint.md`](file-aring-zz-flint.md) | FLINT `fmpz_t`, aring (CRTP) | **Yes** |

## How to tell them apart

| Criterion | Legacy `ZZ` | `ARingZZGMP` | `ARingZZ` (FLINT) |
|---|---|---|---|
| Inherits from | `Ring` (virtual) | `SimpleARing<ARingZZGMP>` | `SimpleARing<ARingZZ>` |
| Per-call cost | Virtual dispatch | Inlined (CRTP) | Inlined (CRTP) |
| Small-value inlining | No | No | **Yes** (FLINT trick) |
| External dep | GMP only | GMP only | GMP + FLINT |
| Used for | Legacy paths only | Some `aring` paths | Default modern path |

## When to choose

For **users**: the engine picks. `ZZ` in M2 transparently uses the
fastest available path (FLINT-backed `ARingZZ` in standard builds).

For **engine developers** adding new code:

- New code → `ARingZZ` (templated, inlined arithmetic).
- Cross-ring boundary code → use `ConcreteRing<ARingZZ>` via
  [`file-aring-glue.md`](file-aring-glue.md) so legacy `Ring*`
  callers continue to work.
- Don't introduce new dependencies on `ZZ` (legacy) unless paired
  with a removal plan.

## See also

| Topic | File |
|---|---|
| The aring framework | [`file-aring.md`](file-aring.md) |
| Aring ↔ Ring bridge | [`file-aring-glue.md`](file-aring-glue.md) |
| Cross-ring coercion | [`file-aring-translate.md`](file-aring-translate.md) |
| The Q analogue | [`file-aring-qq.md`](file-aring-qq.md) (dispatcher), [`file-aring-qq-flint.md`](file-aring-qq-flint.md), [`file-aring-qq-gmp.md`](file-aring-qq-gmp.md) |
| The Z/p analogue | [`file-aring-zzp.md`](file-aring-zzp.md), [`file-aring-zzp-flint.md`](file-aring-zzp-flint.md), [`file-aring-zzp-ffpack.md`](file-aring-zzp-ffpack.md) |

## Related

- [`coefficient-rings.md`](coefficient-rings.md) — area overview.
