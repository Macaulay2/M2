// Copyright 2002  Michael E. Stillman

#ifndef _schorder_hpp_
#define _schorder_hpp_

#include "buffer.hpp"
#include "monoid.hpp"

class GBMatrix;
class Matrix;

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
