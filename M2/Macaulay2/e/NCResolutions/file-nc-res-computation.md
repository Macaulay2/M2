# `nc-res-computation.{cpp,hpp}` — `NCResComputation`

`NCResComputation` is the top-level **non-commutative free-resolution**
driver. It subclasses [`ResolutionComputation`](../file-comp-res.md) and
computes a free resolution of a module over a
[`FreeAlgebraQuotient`](../NCAlgebras/README.md).

Part of the [`NCResolutions/`](README.md) subdirectory.

[← NCResolutions overview](README.md) · [← engine overview](../README.md)

## State

```cpp
class NCResComputation : public ResolutionComputation {
private:
    NCResComputation(const FreeAlgebraQuotient &ring,
                     const Matrix             &gbIdealMatrix,
                     int                       max_level);

public:
    friend ResolutionComputation *createNCRes(const Matrix *groebnerBasisMatrix,
                                              int           max_level,
                                              int           strategy);
    virtual ~NCResComputation() {}

protected:
    bool stop_conditions_ok() {
        // ignore all stopping conditions except length_limit, degree_limit
        return true;
    }
    // ...
};
```

The constructor is private; the factory `createNCRes(...)` (a friend) is
the only way to build one. As with the commutative
[`F4ResComputation`](../schreyer-resolution/file-res-f4-computation.md),
the input must already be a non-commutative Gröbner basis.

## Stop conditions

The base [`Computation`](../file-computation-framework.md) supports many
stop conditions; `NCResComputation` ignores all of them except
**`length_limit`** and **`degree_limit`**. This is a deliberate
simplification — non-commutative resolutions are exploratory in nature, and
the user mainly wants to bound how far they go.

## Inputs

- **`ring`** — a [`FreeAlgebraQuotient`](../NCAlgebras/README.md), i.e. a
  free algebra modulo a two-sided ideal whose GB is fixed.
- **`gbIdealMatrix`** — the Gröbner basis of the *module* being resolved,
  presented as a matrix over `ring`.
- **`max_level`** — homological-degree limit.

## Algorithm sketch

```text
for level = 1, 2, …, max_level:
    new_syzygies = syzygies of previous level's matrix
    new_F = free module on the new syzygies
    next_matrix = differential F_{level} → F_{level-1}
    add (new_F, next_matrix) to the resolution
```

Each step calls into [`NCGroebner`](../NCAlgebras/file-NCGroebner.md) (or
`NCF4`) to compute syzygies of the previous level. The accumulation of
free modules and their differentials becomes the output.

## Output

- `get_free(level)` returns the *level*-th free module.
- `get_matrix(level)` returns the differential `F_{level} → F_{level-1}`.
- `get_betti(type)` returns a Betti table.

Per the standard `ResolutionComputation` contract.

## Status

This subdirectory is young — see the sibling `notes.txt` for design notes
and the still-open questions about non-commutative resolution algorithms.

## Related

- [`README.md`](README.md) — NCResolutions overview.
- [`../NCAlgebras/README.md`](../NCAlgebras/README.md) — host ring family.
- [`../NCAlgebras/file-NCGroebner.md`](../NCAlgebras/file-NCGroebner.md) — GB engine
  used per level.
- [`../file-comp-res.md`](../file-comp-res.md) — base class.
- [`../schreyer-resolution/file-res-f4-computation.md`](../schreyer-resolution/file-res-f4-computation.md)
  — commutative analogue.
