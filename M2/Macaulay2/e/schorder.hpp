// Copyright 2002  Michael E. Stillman

#ifndef _schorder_hpp_
#define _schorder_hpp_

/**
 * @file schorder.hpp
 * @brief `SchreyerOrder` --- per-basis-element data backing the Schreyer order on a free module.
 *
 * Declares `SchreyerOrder`, the lightweight side-table that
 * lives on a `FreeModule` and supplies the data the engine
 * needs to compare two basis elements under a Schreyer order.
 * For each of the `_rank` basis elements the structure stores
 * one `(compare_num, encoded monomial)` entry, packed flatly
 * in `_order` with `_nslots = monoid->monomial_size() + 1`
 * ints per entry --- `compare_num(i)` reads slot `i * _nslots`
 * and `base_monom(i)` returns the monomial pointer at slot
 * `i * _nslots + 1`. `schreyer_compare` /
 * `schreyer_compare_encoded` use these to compare
 * `(m, m_comp)` against `(n, n_comp)` by applying the ambient
 * monomial order to `base * m` vs `base * n` and tiebreaking
 * by `compare_num`; the helpers `schreyer_up(m, comp, result)`
 * and `schreyer_down(m, comp, result)` multiply / divide `m` by
 * the inducing base monomial and write into `result` (which is
 * allowed to alias `m`).
 *
 * The destructor `~SchreyerOrder()` is `abort()` --- the class
 * is not meant to be `delete`d. Use `remove()` to release the
 * `_order` storage when a holder is finished with it. A free
 * module starts without a Schreyer order; `Eschreyer.cpp` and
 * the modern `schreyer-resolution/` install one (via
 * `create(Matrix*)` / `create(GBMatrix*)`) when the module is
 * the target of a Schreyer syzygy step. The standard
 * transformations `sub_space`, `direct_sum`, `tensor`,
 * `exterior`, `symm`, and `append_order` return a fresh
 * `SchreyerOrder` matching the corresponding `FreeModule`
 * operations.
 *
 * @see freemod.hpp
 * @see monoid.hpp
 * @see Eschreyer.hpp
 */

#include "buffer.hpp"
#include "monoid.hpp"

class GBMatrix;
class Matrix;

/**
 * @brief Per-component tie-breaker data for a Schreyer monomial order on a
 * `FreeModule`.
 *
 * @details For each of the `_rank` components, stores a `compare_num` plus
 * the component's base monomial as a flat
 * `[compare_num, encoded_monom]` slice of width `_nslots` inside
 * `_order`. When two `FreeModule` entries with different
 * components are compared, their monomials get pre-multiplied by
 * their respective `base_monom`s and then ranked by `compare_num`
 * --- the standard Schreyer recipe that keeps the resolution's GB
 * machinery consistent across levels.
 */
class SchreyerOrder : public our_new_delete
{
  const Monoid *M;

  // Each 'entry' is an array of ints of length _nslots:
  // compare_num, followed by the (encoded) monomial.
  gc_vector<int> _order;
  int _nslots;
  int _rank;

  SchreyerOrder(const Monoid *m)
      : M(m), _nslots(m->monomial_size() + 1), _rank(0)
  {
  }
  ~SchreyerOrder() { abort(); }
 public:
  static SchreyerOrder *create(const Monoid *m);
  static SchreyerOrder *create(const Matrix *m);
  static SchreyerOrder *create(const GBMatrix *m);

  void remove();

  int rank() const { return _rank; }
  int compare_num(int i) const { return _order[i * _nslots]; }
  const_monomial base_monom(int i) const { return _order.data() + i * _nslots + 1; }
  const Monoid *getMonoid() const { return M; }
  bool is_equal(const SchreyerOrder *G) const;
  SchreyerOrder *copy() const;
  SchreyerOrder *sub_space(int n) const;
  SchreyerOrder *sub_space(M2_arrayint a) const;
  void append_order(const SchreyerOrder *G);
  SchreyerOrder *direct_sum(const SchreyerOrder *G) const;
  SchreyerOrder *tensor(const SchreyerOrder *G) const;
  SchreyerOrder *exterior(int p) const;

  SchreyerOrder *symm(int n) const;

  void append(int compare_num, const_monomial base_monom);
  // Copies the monomial

  void schreyer_up(const_monomial m, int comp, monomial result) const
  // 'result' is allowed to be 'm'.
  {
    M->mult(m, base_monom(comp), result);
  }

  void schreyer_down(const_monomial m, int comp, monomial result) const
  // 'result' is allowed to be 'm'.
  {
    M->divide(m, base_monom(comp), result);
  }

  int schreyer_compare(const_monomial m,
                       int m_comp,
                       const_monomial n,
                       int n_comp) const;

  int schreyer_compare_encoded(const_monomial m,
                               int m_comp,
                               const_monomial n,
                               int n_comp) const;

  void text_out(buffer &o) const;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
