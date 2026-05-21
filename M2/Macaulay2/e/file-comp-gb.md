# `comp-gb.{cpp,hpp}` — `GBComputation`

`GBComputation` is the abstract base for **all** Gröbner basis algorithms in
the engine. It subclasses [`Computation`](file-computation-framework.md) and
adds GB-specific entry points (the basis, leading-term matrix, syzygies).

Part of the [Gröbner bases](groebner-bases.md) area.

[← per-area: groebner-bases](groebner-bases.md) · [← engine overview](README.md)

## Inheritance

```
Computation                       ← comp.hpp
 └── GBComputation                ← this file
      ├── GB_default              ← gb-default.{cpp,hpp}
      ├── GB_homog2               ← gb-homog2.{cpp,hpp}
      ├── GB_sugarless            ← gb-sugarless.{cpp,hpp}
      ├── gbA_toric               ← gb-toric.{cpp,hpp}
      ├── GB_walk                 ← gb-walk.{cpp,hpp}
      ├── F4Computation           ← f4/
      ├── GBF4Computation         ← gb-f4/
      ├── MathicGBInterface       ← mathicgb-interface.{cpp,hpp}
      └── NCGroebnerComputation   ← NCAlgebras/
```

(plus the declared / proxy variants — see below.)

## Required virtuals

```cpp
class GBComputation : public Computation {
public:
    virtual const Matrix *get_gb() = 0;            // the GB matrix
    virtual const Matrix *get_mingens() = 0;       // minimal generators
    virtual const Matrix *get_change() = 0;        // change-of-basis matrix
    virtual const Matrix *get_syzygies() = 0;      // syzygies of input
    virtual const Matrix *get_initial(int nparts) = 0;  // leading terms
    virtual int complete_thru_degree() const = 0;
};
```

These are the operations the [interpreter](../d/README.md) can request once a
GB has been (partially) computed. They are virtual rather than baked in
because each algorithm has its own internal data structures — F4 keeps a
`Basis` of polynomials, the default algorithm keeps a Buchberger-style list
of marked polynomials, mathicgb returns a `MonoMonoid`-encoded basis, etc.

## Dispatching

The factory function `GBComputation::choose_gb(...)` reads the user's
`Strategy =>` option, the ring flavour, and the input shape, then constructs
the appropriate subclass. Most user code goes through this rather than
constructing a subclass directly.

## Declared and proxy variants

| File pair | What it is |
|---|---|
| `comp-gb-declared.{cpp,hpp}` | A `GBComputation` whose basis was supplied by the user (no computation; we trust it) — built when M2 code passes `forceGB(...)` |
| `comp-gb-proxy.{cpp,hpp}` | Used when a GB runs in a supervisor task — the proxy forwards method calls to the worker thread |

## Related

- [`file-computation-framework.md`](file-computation-framework.md) — abstract
  base.
- [`file-comp-res.md`](file-comp-res.md) — sibling for resolutions.
- [`file-gbring.md`](file-gbring.md) — value type used by most subclasses.
- [`groebner-bases.md`](groebner-bases.md) — area overview.
- [`interface/groebner.{h,cpp}`](interface/README.md) — public C interface.
