// Copyright 1996.  Michael E. Stillman

#ifndef _respoly2_hh_
#define _respoly2_hh_

#include "monideal.hpp"
#include "res-a0-pair.hpp"
struct res2term;

struct res2term : public our_new_delete
{
  res2term *next;
  res2_pair *comp;
  ring_elem coeff;
  int monom[1];
};

class res2_poly : public our_new_delete
{
  const PolynomialRing *R;
  const Monoid *M;
  const Ring *K;  // Coefficient field of R.

  size_t respoly_size;
  stash *resterm_stash;

  res2term *new_term() const;

  void sort(res2term *&f) const;

 public:
  res2_poly(PolynomialRing *R);
  ~res2_poly();

  const res2_pair *lead_component(const res2term *f) const;
  //  int lead_coefficient(const res2term *f) const;
  const int *lead_monomial(const res2term *f) const;  // Lead TOTAL monomial

  res2term *new_term(ring_elem c, const int *m, res2_pair *comp) const;
  res2term *mult_by_monomial(const res2term *f, const int *m) const;
  res2term *mult_by_coefficient(const res2term *f, const ring_elem c) const;
  void make_monic(res2term *&f) const;
  res2term *mult_by_term(const res2term *f, ring_elem c, const int *m) const;
  res2term *ring_mult_by_term(const ring_elem f,
                              ring_elem c,
                              const int *m,
                              res2_pair *x) const;
  void add_to(res2term *&f, res2term *&g) const;  // Destroys both f and g.
  void subtract_multiple_to(res2term *&f,
                            ring_elem c,
                            const int *m,
                            const res2term *g) const;
  void ring_subtract_multiple_to(res2term *&f,
                                 ring_elem c,
                                 const int *m,
                                 res2_pair *x,
                                 const ring_elem g) const;

  int compare(const res2term *a, const res2term *b) const;

  res2term *strip(const res2term *f) const;
  const res2term *component_occurs_in(const res2_pair *x,
                                      const res2term *f) const;

  res2term *copy(const res2term *f) const;
  void remove(res2term *&f) const;

  vec to_vector(const res2term *f,
                const FreeModule *F,
                int to_minimal = 0) const;
  res2term *from_vector(const VECTOR(res2_pair *)& base, const vec v) const;

  int n_terms(const res2term *f) const;  // Used for stats
  void elem_text_out(buffer &o,
                     const res2term *f) const;  // Used for debugging and stats
  void elem_text_out(const res2term *f) const;  // Used for debugging and stats

  const PolynomialRing *get_ring() const { return R; }
};

inline const res2_pair *res2_poly::lead_component(const res2term *f) const
{
  return f->comp;
}
// inline int res2_poly::lead_coefficient(const res2term *f) const { return
// f->coeff; }
inline const int *res2_poly::lead_monomial(const res2term *f) const
{
  return f->monom;
}

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
