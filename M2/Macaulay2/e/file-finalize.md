# `finalize.{cpp,hpp}` — engine-object finalisation hooks

`finalize.cpp` declares the **engine-object finalisation hooks** that
let long-lived engine objects (polynomial rings, GBs, monomial
ideals, …) be properly torn down when the user releases them. The
helpers cooperate with bdwgc to run cleanup code when an object
becomes unreachable.

Part of the [Utilities](utilities.md) area.

[← per-area: utilities](utilities.md) · [← engine overview](README.md)

## What's exposed

```cpp
class PolynomialRing;
class MonomialIdeal;
class MutableMatrix;
class GBComputation;
class ResolutionComputation;
class SchreyerOrder;

// These functions should be called if G will not be freed by its owner
void intern_polyring(const PolynomialRing *G);
void intern_monideal(MonomialIdeal *G);
MutableMatrix *internMutableMatrix(MutableMatrix *G);
void intern_GB(GBComputation *G);
void intern_res(ResolutionComputation *G);
// ... and similar for SchreyerOrder ...
```

Each `intern_<type>(ptr)`:

1. Registers `ptr` with bdwgc as an object whose finaliser should be
   called before reclamation.
2. Hands the engine a back-reference so that explicit teardown can
   run if the user requests it.

The comment **"if G will not be freed by its owner"** is the key:
some engine objects are owned by stack-based holders that delete them
explicitly. Those don't need finalisation. The ones that escape into
the M2 heap — typically returned to the interpreter — need this
machinery.

## Why finalisers matter

Without them, an engine object could leak:

- C++ `delete` doesn't run when bdwgc reclaims memory; only the
  bytes are released.
- If the engine object owns OS resources (file descriptors,
  externally-allocated FLINT contexts, NTL state), those would leak.

Registering a finaliser ensures the C++ destructor — and any
externally-tracked cleanup — runs before bdwgc actually drops the
bytes.

## The `internMutableMatrix` exception

`internMutableMatrix` is the one function that *returns* a pointer
(rather than just registering). It is sometimes called with an
ownership-transfer pattern: the caller hands in a `MutableMatrix*` it
no longer needs, and gets back a possibly-rewrapped pointer for use
elsewhere.

## Used by

- Interpreter binding code in [`d/engine.dd`](../d/README.md) —
  every engine function that constructs a new long-lived object calls
  an `intern_<type>` before returning.
- Top-level engine factories.
- `MutableMatrix` snapshotting paths (`internMutableMatrix`).

## Related

- [`utilities.md`](utilities.md) — area overview.
- [`file-newdelete.md`](file-newdelete.md) — `our_new_delete` (the
  allocator that puts objects in the GC heap).
- bdwgc submodule under [`../../submodules/README.md`](../../submodules/README.md)
  — the GC that runs finalisers.
