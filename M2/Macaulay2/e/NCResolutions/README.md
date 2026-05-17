# `M2/Macaulay2/e/NCResolutions/` — non-commutative free resolutions

**See [`architecture.md`](architecture.md)** for the standalone architectural reference (current scope, two-line architecture, entry point, why-separate-from-NCAlgebras, algorithmic differences vs commutative, pending future work).

Resolution code for non-commutative graded algebras (working over the rings
defined in [`../NCAlgebras/`](../NCAlgebras/README.md)).

| File | Role |
|---|---|
| `nc-res-computation.{cpp,hpp}` | Top-level NC-resolution Computation. **Deep dive:** [`file-nc-res-computation.md`](file-nc-res-computation.md) |
| `notes.txt` | Design and algorithm notes |

This subdirectory is small and very young; expect more files to appear as the
NC-resolution work matures. See the commutative analogue in
[`../schreyer-resolution/`](../schreyer-resolution/README.md) for the broader
shape of resolution code in the engine.

## Related

- [`../NCAlgebras/`](../NCAlgebras/README.md) — non-commutative rings these
  resolutions compute over.
- [`../comp-res.{cpp,hpp}`](../README.md) — generic resolution Computation
  framework.

[← back to engine overview](../README.md)
