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

## What triggers this engine

The NC resolutions engine is selected when:

| M2 user code | What happens | Why this engine |
|---|---|---|
| `freeResolution M` for `M` a module over a `FreeAlgebra` or `FreeAlgebraQuotient` | Engine dispatcher → `NCResolutionComputation` here | Auto-routed by ring shape; **no flag needed** |
| Equivalent calls to `res M` (alias) in an NC ring | Same path | Same as above |
| `Ext^i(M, N)`, `Tor_i(M, N)` from `Complexes` in an NC ring | Internally calls `freeResolution` → routes here | Indirect through the resolution it computes |

For the full M2-spec → engine mapping, see the parent [`resolutions.md`](../resolutions.md). NC inputs are detected by the ring's [`M2FreeAlgebra`](../file-M2FreeAlgebra.md) or [`M2FreeAlgebraQuotient`](../file-M2FreeAlgebraQuotient.md) wrapper type.

## Where in the engine pipeline this fits

```
M2:  freeResolution M             (where M is over an NC algebra)
   ↓ comp-res.cpp dispatcher
   ↓ recognises NC ring via M2FreeAlgebra(Quotient) wrapper
NCResolutionComputation (this dir, file-nc-res-computation.md)
   ↓ uses
NCAlgebras infrastructure: FreeAlgebra, NCGroebner, NCReduction
                          (see ../NCAlgebras/README.md)
   ↓ produces partial resolution at each level
   ↓ returned to comp-res.cpp as a normal Computation
```

This subdirectory is the **smallest** and **youngest** engine subdir (one `{cpp,hpp}` pair plus design notes). Its scope is currently the early levels of NC resolutions; more comprehensive coverage tracks the corresponding research progress upstream. See [`architecture.md`](architecture.md) for the algorithmic differences from the commutative case.

## Related

- [`../NCAlgebras/`](../NCAlgebras/README.md) — non-commutative rings these
  resolutions compute over.
- [`../comp-res.{cpp,hpp}`](../README.md) — generic resolution Computation
  framework.
- [`../resolutions.md`](../resolutions.md) — parent area doc; NC entry mentioned in the strategy catalogue.
- [`../schreyer-resolution/`](../schreyer-resolution/README.md) — the commutative analogue (much larger and more mature).

[← back to engine overview](../README.md)
