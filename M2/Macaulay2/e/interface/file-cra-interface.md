# `cra.{h,cpp}` (in `interface/`) — public C entry points for CRT / rational reconstruction

`interface/cra.h` declares the **public C functions** for Chinese
Remainder Algorithm (CRT) lifting and rational reconstruction — the
techniques used to reconstruct rational-coefficient answers from a
collection of modular computations.

Part of the [`interface/`](README.md) subdirectory.

[← interface overview](README.md) · [← engine overview](../README.md)

## Header shape

```c
#if defined(__cplusplus)
class Matrix;
class Ring;
class RingElement;
#else
typedef struct Matrix      Matrix;
typedef struct Ring        Ring;
typedef struct RingElement RingElement;
#endif

/**
   Chinese remainder and rational reconstruction interface routines 
 */
```

The standard dual-mode forward declarations. The routines deal with
matrices, rings, and ring elements as the three transferred types.

## What CRT-based computation looks like

A computation over `QQ` is often done by:

1. Compute the same result modulo several primes `p_1, …, p_k`.
2. Lift each modular result to its CRT combination modulo `p_1 · … · p_k`.
3. Run rational reconstruction to recover the unique fraction `a/b` with
   `|a|, |b| <= sqrt(p_1 · … · p_k)/2`.

This is much faster than working in `QQ` directly because:

- Modular arithmetic is hardware-fast.
- Each modular run can be parallelised.
- Coefficient blow-up is bounded.

## Entry points

The functions exposed:

- **`rawCRA0`** and similar — basic CRT step for `ring_elem`s and
  matrices.
- **`rawRingElementCRA`**, **`rawMatrixCRA`** — convenience wrappers that
  manage the modulus stack.
- **`rawRingElementRationalReconstruction`**,
  **`rawMatrixRationalReconstruction`** — reconstruct rational outputs
  from CRT-lifted results.

Used heavily by:

- F4 with rational coefficients — running the algorithm mod many primes
  and reconstructing afterwards.
- Hilbert-function and resolution computations over `QQ`.

## Related

- [`README.md`](README.md) — interface overview.
- [`../computations.md`](../computations.md) — area overview.
- [`../file-aring-zz-flint.md`](../file-aring-zz-flint.md), `aring-qq-flint`
  — FLINT-backed CRT machinery used internally.
- FLINT submodule under [`../../../submodules/README.md`](../../../submodules/README.md).
