// Copyright 1996-2011 Michael E. Stillman

#ifndef _Schurring_hh_
#define _Schurring_hh_

/**
 * @file schur.hpp
 * @brief `SchurRing` --- symmetric-function ring with Schur-basis multiplication via Littlewood-Richardson.
 *
 * Declares `SchurRing`, a `PolyRing` subclass whose elements
 * are Z-linear combinations of Schur functions `s_lambda`
 * indexed by partitions, plus the supporting `tableau` class
 * (partition vector `p`, partition vector `lambda`, and
 * `(xloc, yloc)` coordinate arrays giving the horizontal /
 * vertical positions of each cell in the skew shape).
 * Multiplication computes the structure constants
 * `s_lambda * s_mu = sum_nu c_{lambda mu}^nu s_nu` --- the
 * Littlewood-Richardson coefficients --- via the private
 * `skew_schur(lambda, p)` and `SM()` recursion, with reusable
 * `_SMtab` / `_SMfilled` scratch tableaux kept on the ring so
 * `mult_monomials` does not re-allocate on every product.
 *
 * `SCHUR_MAX_WT = 100` caps the partition weight the engine
 * will multiply; `LARGE_NUMBER = 32000` is the sentinel marking
 * empty slots in the tableau scratch space. `schur2.hpp` is a
 * parallel symmetric-function ring built on a different basis,
 * and `schur-poly-heap.hpp` is the bucketed-collector helper
 * for assembling large sums of Schur monomials.
 *
 * @see poly.hpp
 * @see schur2.hpp
 * @see schur-poly-heap.hpp
 */

#include <vector>
#include "poly.hpp"

const int SCHUR_MAX_WT = 100;
const int LARGE_NUMBER = 32000;

class tableau
{
  friend class SchurRing;

  int dim;
  int maxwt;
  int wt;
  int *lambda;  // A partition vector 0..nvars-1
  int *p;       // A partition vector 0..nvars-1
  int *xloc;    // array 1..|p|-|lambda| of the horizontal location
                // of this number in the skew table
  int *yloc;    // array 1..|p|-|lambda| of the vertical location
                // of this number in the skew table

  void initialize(int nvars);
  tableau() {}
  ~tableau() {}
  void resize(int max_wt);

  int elem(int x, int y) const;
  void fill(int *lamb, int *pp);
  void display() const;
};

/**
 * @brief `PolyRing` subclass implementing the Schur (symmetric-function)
 * ring whose monomials are partitions and whose multiplication is
 * the Littlewood-Richardson rule.
 *
 * @details Exponent vectors are interpreted as partitions and converted
 * back and forth via `to_partition` / `from_partition`. Schur
 * multiplication uses the skew-tableau recursion `SM()` driven by
 * `_SMtab` / `_SMfilled` / `_SMcurrent` scratch state, accumulating
 * the result into `_SMresult`. Used by the engine's symmetric
 * function code paths to compute products and skew Schur expansions.
 */
class SchurRing : public PolyRing
{
 private:
  // These are variables that are used in the recursive routine
  // SM(), which is called from skew_schur().
  tableau _SMtab;
  tableau _SMfilled;
  int _SMcurrent;
  int _SMfinalwt;
  Nterm *_SMresult;

  void to_partition(const int *m, int *exp) const;
  // exp[1]..exp[nvars] are set
  void from_partition(const int *exp, int *m) const;

  void bounds(int &lo, int &hi);
  void SM();
  Nterm *skew_schur(int *lambda, int *p);
  ring_elem mult_monomials(const int *m, const int *n);

 protected:
  bool initialize_schur();
  SchurRing() {}
  virtual ~SchurRing() {}
 public:
  static SchurRing *create(const PolynomialRing *R);

  static SchurRing *create(const Ring *A, int n);

  static SchurRing *createInfinite(const Ring *A);

  virtual const SchurRing *cast_to_SchurRing() const { return this; }
  virtual SchurRing *cast_to_SchurRing() { return this; }
  virtual void text_out(buffer &o) const;
  virtual void elem_text_out(buffer &o,
                             const ring_elem f,
                             bool p_one = true,
                             bool p_plus = false,
                             bool p_parens = false) const;

  void dimension(const int *exp, mpz_t result) const;
  ring_elem dimension(const ring_elem f) const;

  virtual ring_elem mult_by_term(const ring_elem f,
                                 const ring_elem c,
                                 const int *m) const;

  ring_elem power(const ring_elem f, mpz_srcptr n) const;
  ring_elem power(const ring_elem f, int n) const;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
