// Copyright 2011 Michael E. Stillman

#ifndef _schurSn_hh_
#define _schurSn_hh_

/**
 * @file schurSn.hpp
 * @brief `SchurSnRing` --- `SchurRing2` subclass intended for symmetric-group representation rings (Kronecker product is a stub).
 *
 * Declares `SchurSnRing`, a `SchurRing2` subclass meant to
 * carry characters of irreducible `S_n` representations indexed
 * by partitions of `n`. Construction forwards `(A, n)` to
 * `SchurRing2(A, n)` and runs the same `initialize_SchurRing2`,
 * so the rank cap and coefficient ring follow the parent's
 * conventions (a default `n = -1` reaches `SchurRing2`'s
 * infinite-rank mode, same as `createInfinite`). The
 * `cast_to_SchurSnRing` overrides let engine code distinguish
 * this specialisation from a generic Schur ring without
 * resorting to `dynamic_cast`.
 *
 * The class adds a `tensor_mult(f, g)` method alongside the
 * overridden `mult(f, g)` --- but both implementations in
 * `schurSn.cpp` are one-line passthroughs to
 * `SchurRing2::mult(f, g)`. The intended Kronecker-product
 * semantics for `tensor_mult` are not yet implemented; right
 * now it is just an alias for the ordinary Schur-function
 * product.
 *
 * @see schur2.hpp
 * @see schur-poly-heap.hpp
 */

#include "schur2.hpp"

/**
 * @brief `SchurRing2` subclass implementing the symmetric-group character
 * ring (the "Schur ring of `S_n`"), with multiplication given by
 * inner-product convolution rather than Littlewood-Richardson.
 *
 * @details Overrides `mult` to compute the symmetric-group analogue: the
 * coefficient of `s_lambda` in `s_mu * s_nu` is the number of
 * ways to decompose representations rather than the LR count.
 * `cast_to_SchurSnRing()` lets engine code distinguish this ring
 * from the plain `SchurRing2` it inherits from.
 */
class SchurSnRing : public SchurRing2
{
 public:
  SchurSnRing(const Ring *A, int n = -1);

  static SchurSnRing *create(const Ring *A, int n = -1);

  virtual const SchurSnRing *cast_to_SchurSnRing() const { return this; }
  virtual SchurSnRing *cast_to_SchurSnRing() { return this; }
  virtual ring_elem mult(const ring_elem f, const ring_elem g) const;
  ring_elem tensor_mult(const ring_elem f, const ring_elem g) const;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
