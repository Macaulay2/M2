# Ring elements and ring maps

The **`RingElement`** type is what the interpreter holds onto when M2 code
manipulates an element of a ring; the **`RingMap`** type is the engine
representation of a homomorphism between rings.

This page is the **architectural overview** for how individual *values* (elements
of a ring) and *homomorphisms* between rings are represented in the engine. It
complements [`coefficient-rings.md`](coefficient-rings.md) (which is about the
*containers* — the rings themselves) by drilling into the *things they hold*.

[← engine overview](README.md) · [← top-level TOC](../../../README.md#engine-deep-dive-m2macaulay2e) · [per-area docs](README.md#top-level-files-per-area-docs)

## Two parallel value representations

The engine has historically grown two coexisting representations of a ring
element:

### Legacy `ring_elem` — the universal tagged-pointer value

A union type (in [`file-ringelem.md`](file-ringelem.md)) that can hold any
ring's element via tagged pointers / packed integers:

```cpp
union ring_elem {
  int    int_val;     // for tiny modular rings
  mpz_t* mpz_val;     // for ZZ via GMP
  Nterm* poly_val;    // for polynomial rings
  ...
};
```

Used **at the C ABI boundary** because:

- All ring-element values must cross the interpreter ↔ engine boundary
  uniformly.
- The interpreter's `.d`-translated C code can manipulate `ring_elem`
  without knowing each ring's internal type.
- `Ring`-method dispatch is virtual on `Ring*`, with `ring_elem` as
  the parameter.

### Modern `ElementType` — typed values for the `aring` framework

Each `aring` ring (in [`file-aring.md`](file-aring.md)) declares its own
typed element:

```cpp
class ARingZZpFlint : public SimpleARing<ARingZZpFlint> {
public:
  typedef ulong ElementType;
  ...
};
```

Used **inside templated inner loops** because:

- The compiler can inline arithmetic per ring (no virtual dispatch).
- Type-safety catches "I passed a `ZZ` to `GF(7^2)` arithmetic" mistakes.
- Allows specialised representations (packed bits for `Z/2`, `__int128`
  for large primes, etc.).

### The bridge — `ConcreteRing<ARingType>`

`ConcreteRing<RingType>` (in [`file-aring-glue.md`](file-aring-glue.md))
wraps an `aring`-style ring so it can present itself as a legacy `Ring`.
Then:

```
Modern engine code:    DMat<ARingZZpFlint>          (templated, fast)
Legacy / boundary:     ConcreteRing<ARingZZpFlint>  (wraps the above)
Interpreter ABI:       ring_elem (tagged pointer to either)
```

`MutableEngineObject` ([`file-hash.md`](file-hash.md)) is the shared GC base
both representations inherit from.

## Ring elements

| File pair | Purpose |
|---|---|
| `relem.{cpp,hpp}` | `RingElement` — pair of `(Ring*, ring-specific-value)`. **Deep dive:** [`file-relem.md`](file-relem.md) |

A `RingElement` is essentially a tagged union: a pointer to the ring it
belongs to plus an opaque value the ring knows how to interpret. All
arithmetic dispatches through the ring's virtual methods (or through the
templated [`aring`](coefficient-rings.md) machinery for newer code).

## Ring maps (homomorphisms)

| File pair | Purpose |
|---|---|
| `ringmap.{cpp,hpp}` | `RingMap` — specifies a homomorphism by giving images of generators. **Deep dive:** [`file-ringmap.md`](file-ringmap.md) |

A `RingMap : R → S` is built from:

- a source ring `R`
- a target ring `S`
- a list of images, one per generator of `R`

Applying a ring map to a polynomial means substituting images for generators
and evaluating in the target ring. Composition is handled by walking through
generators.

## M2-facing wrappers for non-commutative algebras

| File pair | Purpose |
|---|---|
| `M2FreeAlgebra.{cpp,hpp}` | M2-side `RingElement`-compatible wrapper for [`NCAlgebras/FreeAlgebra`](NCAlgebras/README.md) |
| `M2FreeAlgebraQuotient.{cpp,hpp}` | Wrapper for [`NCAlgebras/FreeAlgebraQuotient`](NCAlgebras/README.md) |

These exist because the non-commutative classes in `NCAlgebras/` keep their
internal representation private; the wrappers here translate to/from
`RingElement` so the rest of the engine can treat NC rings like any other.

## Conversions across rings

Cross-ring value conversion lives in:

- [`file-aring-translate.md`](file-aring-translate.md) — generic cross-ring
  coercion templates (`ConversionMap<From, To>`).
- `Ring::promote` / `Ring::lift` virtual methods on each `Ring` subclass —
  the legacy mechanism for embedding `R → S` (promote up) and recovering
  `S → R` (lift down) when possible.

The two systems coexist because legacy rings still rely on `promote`/`lift`
while newer modular code prefers `ConversionMap` templates.

## Lifecycle of a ring element

```
M2 user:  3_R + 4_R
   ↓ (interpreter, m2/ringelement.m2)
   rawAdd(3, 4)              [a ring_elem ABI call]
   ↓ (interface/ringelement.cpp)
   IM2_RingElement_add(...)
   ↓ dispatches via Ring::add
   ConcreteRing<ARingZZpFlint>::add(a, b)
   ↓ unpacks ring_elem → ElementType
   ARingZZpFlint::add(a, b, c)   [inlined inner loop]
   ↓
   wraps result back as ring_elem
   ↓ (interpreter)
   return to user
```

Each step is one or two function calls; the templated inner loop is the hot
path. Modern code shortcuts to call `ARingZZpFlint::add` directly when both
operands' types are statically known.

## Memory model

Both `ring_elem` and `ElementType` are GC-managed:

- **Atomic-leaf elements** (`int`-packed Z/p, `ulong`-packed GF) — no GC
  scanning needed, allocated via `getmem_atomic`.
- **Pointer-bearing elements** (polynomials with `Nterm*` linked lists,
  `mpz_t`-wrapping ZZ) — scanned by Boehm GC.
- **External-library types** (`mpz_t`, `mpfr_t`, `fmpz_t`) — wrapped in
  GC-managed handles with finalisers from
  [`file-finalize.md`](file-finalize.md) that call the library's free
  function when the wrapper is collected.

## Related

- [`coefficient-rings.md`](coefficient-rings.md) — concrete rings that elements
  live in.
- [`polynomial-rings.md`](polynomial-rings.md) — polynomial-ring elements
  flow through `RingElement`.
- [`free-modules.md`](free-modules.md) — vectors of ring elements.
- [`matrices.md`](matrices.md) — matrices of ring elements.
- [`utilities.md`](utilities.md) — universal allocators and helpers values
  use.
- [`interface/ringelement.{h,cpp}`](interface/README.md) and
  [`interface/ringmap.{h,cpp}`](interface/README.md) — public API.
- [`file-aring-glue.md`](file-aring-glue.md) — the `ConcreteRing` bridge.
- [`file-aring-wrap.md`](file-aring-wrap.md) — typed wrapper `RElementWrap<R>`.
