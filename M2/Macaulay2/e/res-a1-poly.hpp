// Copyright 1996.  Michael E. Stillman

#ifndef _respoly_hh_
#define _respoly_hh_

#include "monideal.hpp"

struct resterm;

// The following are the possible types of res_pairs's
enum {
  SYZ_RING_ELEM,       // Base ring elements: will go away...
  SYZ_EXTERIOR_VAR,    // possibly will go away...
  SYZ_BASE_COMPONENT,  // Base component at level 0

  SYZ_S_PAIR,     // Pre computation: s-pair
  SYZ_RING_PAIR,  // Pre computation: (module elem, ring elem)
  SYZ_ONE_PAIR,   // Pre computation: (module elem * variable)
  SYZ_GEN,        // Pre computation: original generator at level 1

  SYZ_NOT_COMPUTED,
  SYZ_MINIMAL,      // Post s-pair computation: element is minimal syzygy
  SYZ_NOT_MINIMAL,  // Post s-pair computation: element is not minimal
  SYZ_NOT_NEEDED    // S-pair computation for this pair cancelled
};

class res_pair
// Only do new and delete via res_comp::new_res_pair, res_comp::delete_res_pair
{
 public:
  // The schreyer order part:

  int me;
  int compare_num;  // Schreyer order of this stripped component
  int *base_monom;

  res_pair *next;          // Next pair to compute in the same degree
  res_pair *next_compare;  // List of pairs in the level in ascending
                           // 'compare_num' value
  res_pair *first;
  res_pair *second;
  res_pair *base_comp;
  int syz_type;

  MonomialIdeal *mi;  // Monomial ideal of total monomials
  res_pair *mi2;      // List of res_pairs having this as lead term
  res_pair *next_mi;  // If this is part of a list of mi2, this is the
                      // next-link.
  resterm *syz;       // The syzygy itself, once computed

  // The following are used only for minimalization of the resolution
  int minimal_me;         // SYZ_MINIMAL: the number of this min syzygy
  resterm *pivot_term;    // SYZ_NOT_MINIMAL: Points into 'syz', to the
                          // term containing the constant.
  resterm *stripped_syz;  // If syz_type is SYZ_MINIMAL: this is the
                          // reduced stripped version.
                          // If syz_type is SYZ_NOT_MINIMAL: this is the
                          // stripped (possibly reduced) version.
};

struct resterm
{
  resterm *next;
  res_pair *comp;
  ring_elem coeff;
  int monom[1];
};

class res_poly : public our_new_delete
{
  const PolynomialRing *R;
  const Monoid *M;
  const Ring *K;  // Coefficient field of R.
  size_t element_size;
  stash *resterm_stash;

  resterm *new_term() const;

  void sort(resterm *&f) const;

 public:
  res_poly(PolynomialRing *R);
  ~res_poly();

  const res_pair *lead_component(const resterm *f) const;
  //  int lead_coefficient(const resterm *f) const;
  const int *lead_monomial(const resterm *f) const;  // Lead TOTAL monomial

  resterm *new_term(ring_elem c, const int *m, res_pair *comp) const;
  resterm *mult_by_monomial(const resterm *f, const int *m) const;
  void make_monic(resterm *&f) const;
  resterm *mult_by_term(const resterm *f, ring_elem c, const int *m) const;
  resterm *ring_mult_by_term(const ring_elem f,
                             ring_elem c,
                             const int *m,
                             res_pair *x) const;
  void add_to(resterm *&f, resterm *&g) const;  // Destroys both f and g.
  void subtract_multiple_to(resterm *&f,
                            ring_elem c,
                            const int *m,
                            const resterm *g) const;
  void ring_subtract_multiple_to(resterm *&f,
                                 ring_elem c,
                                 const int *m,
                                 res_pair *x,
                                 const ring_elem g) const;

  int compare(const resterm *a, const resterm *b) const;

  resterm *strip(const resterm *f) const;
  const resterm *component_occurs_in(const res_pair *x, const resterm *f) const;

  resterm *copy(const resterm *f) const;
  void remove(resterm *&f) const;

  vec to_vector(const resterm *f,
                const FreeModule *F,
                int to_minimal = 0) const;
  resterm *from_vector(const VECTOR(res_pair *)& base, const vec v) const;

  int n_terms(const resterm *f) const;  // Used for stats
  void elem_text_out(buffer &o,
                     const resterm *f) const;  // Used for debugging and stats
  void elem_text_out(const resterm *f) const;  // Used for debugging and stats
};

inline const res_pair *res_poly::lead_component(const resterm *f) const
{
  return f->comp;
}
// inline int res_poly::lead_coefficient(const resterm *f) const { return
// f->coeff; }
inline const int *res_poly::lead_monomial(const resterm *f) const
{
  return f->monom;
}

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
