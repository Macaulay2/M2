# `Eschreyer.cpp` — implementation notes for `GBKernelComputation` / `Eschreyer`

`Eschreyer.cpp` is the **`.cpp` implementation file** for
[`file-Eschreyer.md`](file-Eschreyer.md)'s `GBKernelComputation`
class. This file deep-dive complements the existing one by zooming
in on the *implementation*, especially the points where the modern
[`schreyer-resolution/`](schreyer-resolution/README.md) family takes
a different design choice.

Part of the [Resolutions](resolutions.md) area.

[← per-area: resolutions](resolutions.md) · [← engine overview](README.md)

## The two parts of the file

`Eschreyer.cpp` is conceptually two pieces:

1. **`GBMatrix` operations** — the lightweight matrix-of-`gbvector*`
   type declared in [`file-Eschreyer.md`](file-Eschreyer.md). Its
   constructors, ownership-transferring `append(gbvector *)`, and
   `to_matrix()` converter live here.
2. **`GBKernelComputation` logic** — the actual kernel-finding loop.
   Implements `start_computation()` by walking the input matrix's
   columns, computing syzygies via repeated GB-style reduction.

## Reduction inner loop

The hot path is the **head-reduce-then-tail-reduce** pattern:

```text
input: a polynomial f in the source free module
output: its syzygy s such that f - reductions_in_image = 0

# head reduction
while lt(f) is divisible by lt(g_i) for some g_i in the GB:
    f -= (lt(f) / lt(g_i)) * g_i
    s += (lt(f) / lt(g_i)) * e_i   # track the multiplier

# tail reduction (over the residue, optional)
for each tail term of f:
    if divisible by some lt(g_j):
        subtract appropriately, update s
```

The result is a syzygy `s` whose generators are the basis of the
kernel of the input matrix mod the GB.

## Why the design differs from `schreyer-resolution/`

[`file-Eschreyer.md`](file-Eschreyer.md) explained the high-level
positioning. The implementation-level distinction:

- **`Eschreyer.cpp`** does one polynomial at a time. The `gbvector*`
  linked list is traversed term-by-term; reductions happen
  sequentially.
- **`schreyer-resolution/`** does many polynomials at once via the
  Macaulay-matrix path. The polynomials are flattened into rows of
  a sparse matrix and reduced in bulk.

For small inputs the difference is negligible; for large ones the
matrix approach can be orders of magnitude faster.

## Status

The file remains in the engine for compatibility with code paths
that don't yet use the matrix-based approach. New work targets
[`schreyer-resolution/`](schreyer-resolution/README.md).

## Related

- [`file-Eschreyer.md`](file-Eschreyer.md) — class-level overview.
- [`resolutions.md`](resolutions.md) — area overview.
- [`schreyer-resolution/`](schreyer-resolution/README.md) — successor.
- [`file-gbring.md`](file-gbring.md), [`file-gb-default.md`](file-gb-default.md)
  — neighbouring code paths.
