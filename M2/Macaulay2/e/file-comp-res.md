# `comp-res.{cpp,hpp}` — `ResolutionComputation`

`ResolutionComputation` is the abstract base for all **free-resolution**
algorithms in the engine, mirroring the role of
[`GBComputation`](file-comp-gb.md) on the GB side. It subclasses
[`Computation`](file-computation-framework.md) and adds resolution-specific
methods: per-degree free modules, differentials, Betti tables.

Part of the [Resolutions](resolutions.md) area.

[← per-area: resolutions](resolutions.md) · [← engine overview](README.md)

## Inheritance

```
Computation                          ← comp.hpp
 └── ResolutionComputation           ← this file
      ├── res_comp                   ← res-a0.{cpp,hpp}
      ├── res2_comp                  ← res-a1.{cpp,hpp}
      ├── ResolutionComputationA2    ← res-a2.{cpp,hpp}
      ├── EschreyerComputation       ← Eschreyer.{cpp,hpp}
      ├── F4ResComputation           ← schreyer-resolution/
      └── NCResComputation           ← NCResolutions/
```

## Required virtuals

```cpp
class ResolutionComputation : public Computation {
public:
    virtual FreeModule *get_free(int level) const = 0;
    virtual Matrix     *get_matrix(int level) const = 0;
    virtual M2_arrayint get_betti(int type) const = 0;
    virtual int complete_thru_degree() const = 0;
};
```

`get_free(level)` returns the *level*-th free module in the resolution,
`get_matrix(level)` the differential `F_{level} → F_{level-1}`, and
`get_betti(type)` the Betti table (with `type` selecting "minimal", "total",
"graded", etc.).

## Strategy

The factory `ResolutionComputation::choose_res(...)` reads the M2-side
`Strategy =>` option to pick an implementation. The most-frequently-used
strategies map to subclasses as:

| Strategy | Subclass |
|---|---|
| `1` | `res_comp` (`res-a0`) |
| `2` | `res2_comp` (`res-a1`) |
| `3` | `ResolutionComputationA2` (`res-a2`) |
| `4` | `EschreyerComputation` |
| `4.1`, `5`, … | `F4ResComputation` ([`schreyer-resolution/`](schreyer-resolution/README.md)) |

Older strategies remain available primarily for regression testing and the
occasional edge case where the new strategy is slower.

## Resumability

Resolutions are usually run with a `degree_limit` stop condition: compute up
to homological degree *k*, look at the Betti numbers, decide whether to
continue. `ResolutionComputation` subclasses keep their per-degree state on
the heap so the interpreter can resume by relaxing the limit.

## Related

- [`file-computation-framework.md`](file-computation-framework.md) — abstract
  base.
- [`file-comp-gb.md`](file-comp-gb.md) — sibling for Gröbner bases.
- [`resolutions.md`](resolutions.md) — area overview.
- [`schreyer-resolution/`](schreyer-resolution/README.md) — modern
  implementation.
- [`interface/groebner.{h,cpp}`](interface/README.md) — public C interface
  (shared with GB).
