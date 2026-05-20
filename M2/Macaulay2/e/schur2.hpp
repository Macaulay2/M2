// Copyright 1996-2017 Michael E. Stillman

#ifndef _Schurring2_hh_
#define _Schurring2_hh_

/**
 * @file schur2.hpp
 * @brief `SchurRing2` --- refactored Schur ring with length-prefixed partitions and an explicit `Ring` base.
 *
 * Declares `SchurRing2`, the second-generation Schur-function
 * ring. Unlike `schur.hpp`'s `SchurRing` (which inherits from
 * `PolyRing`), `SchurRing2` derives directly from `Ring`.
 * Partitions are stored in the `schur_partition` layout
 * `[n + 1, a_1, ..., a_n]` --- a length prefix followed by the
 * non-increasing parts `a_1 >= a_2 >= ... >= a_n` (an in-source
 * note flags that parts may be negative). Elements are
 * `schur_poly` objects carrying parallel `coeffs` and `monoms`
 * vectors, where `monoms` is a flat concatenation of
 * `schur_partition`s walked by the length prefix. Multiplication
 * uses Littlewood-Richardson enumeration through the private
 * `skew_schur` / `SM` recursion with reusable `SMtab` / `SMfilled`
 * `tableau2` scratch tableaux, and accumulates into the
 * `schur_poly_heap *SMheap` defined in `schur-poly-heap.hpp`.
 *
 * The class is allowed to have a free coefficient ring
 * (`coefficientRing`) and an optional variable-count cap `nvars`
 * (`-1` means "infinite-rank" virtual partitions, served by
 * `createInfinite`). `dimension(f)` evaluates the Schur
 * polynomial at the all-ones specialisation; `invert`,
 * `divide`, and `syzygy` are explicitly no-ops because they
 * have no meaning in this ring.
 *
 * @see schur.hpp
 * @see schur-poly-heap.hpp
 * @see poly.hpp
 */

#include <vector>
#include "poly.hpp"

using schur_word = int;
// typedef int schur_word;

// Format for a schur_partition, const_schur_partition
// (n+1) a1 a2 ... an
// where a1 >= a2 >= ... >= an (can these be negative?  I think so)
// and n+1 is the length of the entire allocated amount.

typedef schur_word *schur_partition;
typedef const schur_word *const_schur_partition;

class tableau2
{
  friend class SchurRing2;

  int maxwt;
  int wt;
  int *lambda;  // A partition vector 0..nvars-1
  int *p;       // A partition vector 0..nvars-1
  int *xloc;    // array 1..|p|-|lambda| of the horizontal location
                // of this number in the skew table
  int *yloc;    // array 1..|p|-|lambda| of the vertical location
                // of this number in the skew table

  void initialize(int nvars);
  void initialize(int maxrows, int maxwt);
  tableau2() {}
  ~tableau2() {}
  void resize(int max_wt);

  int elem(int x, int y) const;
  void fill(int *lamb, int *pp);
  void display() const;
};

class schur_poly_iterator;
class schur_poly_heap;

class schur_poly : public our_new_delete
{
  friend class SchurRing2;
  friend class schur_poly_iterator;
  VECTOR(ring_elem) coeffs;
  VECTOR(schur_word)
  monoms;  // each monomial is aschur_partition, all of these concatenated
           // together
 public:
  typedef schur_poly_iterator iterator;

  iterator begin() const;
  iterator end() const;
  size_t size() const { return coeffs.size(); }
  void append(iterator &first, iterator &last);
  void appendTerm(ring_elem coeff, const_schur_partition monom);
};

class schur_poly_iterator
{
  VECTOR(ring_elem)::const_iterator ic;
  VECTOR(schur_word)::const_iterator im;

  friend class schur_poly;

  schur_poly_iterator(const schur_poly &f)
      : ic(f.coeffs.begin()), im(f.monoms.begin())
  {
  }
  schur_poly_iterator(const schur_poly &f, int) : ic(f.coeffs.end()) {}
 public:
  void operator++()
  {
    ++ic;
    im += *im;
  }
  ring_elem getCoefficient() { return *ic; }
  const_schur_partition getMonomial() { return &*im; }
  friend bool operator==(const schur_poly_iterator &a,
                         const schur_poly_iterator &b);
  friend bool operator!=(const schur_poly_iterator &a,
                         const schur_poly_iterator &b);
};

bool operator==(const schur_poly::iterator &a, const schur_poly::iterator &b);
bool operator!=(const schur_poly::iterator &a, const schur_poly::iterator &b);

inline schur_poly::iterator schur_poly::begin() const
{
  return iterator(*this);
}
inline schur_poly::iterator schur_poly::end() const
{
  return iterator(*this, 1);
}

/**
 * @brief Refactored Schur (symmetric-function) ring whose elements are
 * `schur_poly` sums of partitions over a configurable coefficient
 * ring.
 *
 * @details Replaces the older `SchurRing` (in `schur.hpp`) with a leaner
 * implementation: partitions are stored in `schur_partition`
 * sorted form rather than exponent vectors, and `nvars` caps the
 * partition length (`-1` means no cap, i.e. infinitely many
 * Schur generators). Multiplication uses the Littlewood-Richardson
 * rule via `truncate`. Forms the base class for `SchurSnRing`.
 */
class SchurRing2 : public Ring
{
 private:
  const Ring *coefficientRing;
  int nvars;  // max size of a partition, or -1 meaning infinity
 protected:
  bool initialize_schur();
  bool initialize_SchurRing2();

  SchurRing2() {}
  SchurRing2(const Ring *A, int n = -1);
  virtual ~SchurRing2() {}
  int compare_partitions(const_schur_partition a,
                         const_schur_partition b) const;
  ring_elem truncate(const ring_elem f) const;
  bool promote_coeffs(const SchurRing2 *Sf,
                      const ring_elem f,
                      ring_elem &resultRE) const;
  bool lift_coeffs(const SchurRing2 *Sg,
                   const ring_elem f,
                   ring_elem &resultRE) const;

 public:
  int n_vars() const { return nvars; }
  const Ring *getCoefficientRing() const { return coefficientRing; }
  static SchurRing2 *create(const Ring *A, int n = -1);
  static SchurRing2 *createInfinite(const Ring *A);

  virtual const SchurRing2 *cast_to_SchurRing2() const { return this; }
  virtual SchurRing2 *cast_to_SchurRing2() { return this; }
  bool is_valid_partition(M2_arrayint part, bool set_error = true) const;
  // sets global error message by default

  ring_elem from_coeff(ring_elem a) const;
  ring_elem from_partition(M2_arrayint part) const;

  schur_poly *mult_by_coefficient(ring_elem a, const schur_poly *f) const;
  bool get_scalar(const schur_poly *f, ring_elem &result) const;

  size_t size(ring_elem f) const { return f.get_schur_poly()->coeffs.size(); }
  void dimension(const int *exp, mpz_t result) const;
  ring_elem dimension(const ring_elem f) const;
  // only allowed if nvars >= 0

  engine_RawArrayPairOrNull list_form(const Ring *coeffR,
                                      const ring_elem f) const;

  ////// from Ring //////////////////////////////////////////
  virtual unsigned int computeHashValue(const ring_elem a) const;

  virtual void text_out(buffer &o) const;
  virtual void elem_text_out(buffer &o,
                             const ring_elem f,
                             bool p_one = true,
                             bool p_plus = false,
                             bool p_parens = false) const;

  virtual ring_elem from_long(long n) const;
  virtual ring_elem from_int(mpz_srcptr n) const;
  virtual bool from_rational(mpq_srcptr q, ring_elem &result) const;

  virtual bool promote(const Ring *R,
                       const ring_elem f,
                       ring_elem &result) const;
  virtual bool lift(const Ring *R, const ring_elem f, ring_elem &result) const;

  virtual bool is_unit(const ring_elem f) const;
  virtual bool is_zero(const ring_elem f) const;
  virtual bool is_equal(const ring_elem f,
                        const ring_elem g) const;  // NOT DONE...

  virtual int compare_elems(const ring_elem f, const ring_elem g) const;
  virtual ring_elem copy(const ring_elem f) const;
  virtual void remove(ring_elem &f) const
  {
    (void) f;
  }  // let the GC do it!
  virtual ring_elem negate(const ring_elem f) const;
  virtual ring_elem add(const ring_elem f, const ring_elem g) const;
  virtual ring_elem subtract(const ring_elem f, const ring_elem g) const;
  virtual ring_elem mult(const ring_elem f, const ring_elem g) const;

  virtual ring_elem eval(const RingMap *map,
                         const ring_elem f,
                         int first_var) const;

  // These make little or no sense for this ring
  virtual ring_elem invert(const ring_elem f) const;  // do nothing
  virtual ring_elem divide(const ring_elem f,
                           const ring_elem g) const;  // do nothing
  virtual void syzygy(const ring_elem a,
                      const ring_elem b,
                      ring_elem &x,
                      ring_elem &y) const;  // do nothing

  ///////////////////////////////////////////////////////////
 private:
  // Littlewood-Richardson variables and functions
  int SMmaxweight;
  int SMmaxrows;
  tableau2 SMtab;
  tableau2 SMfilled;
  int SMcurrent;
  int SMfinalwt;
  schur_poly_heap *SMheap;  // where the answer is collected

  void SMinitialize(int max_rows, int max_weight);
  void SMbounds(int &lo, int &hi);
  void SM();
  void SMappendTerm(const_schur_partition f);
  void SMsetPartitionLength(schur_word *p, int SMmaxrows);
  // sets p[0] in the range 2..SMmaxrows+2

  ring_elem skew_schur(const_schur_partition lambda, const_schur_partition p);
  ring_elem mult_terms(const_schur_partition a, const_schur_partition b);
};

#include "schur-poly-heap.hpp"

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
