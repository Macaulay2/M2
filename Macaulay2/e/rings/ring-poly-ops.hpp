#ifndef __ring_poly_ops_hpp_
#define __ring_poly_ops_hpp_

#include "ring-poly.hpp"

template <class CoeffRing, class Monoid>
class PolyRingOps
{
public:
  typedef typename PolyRing<CoeffRing,Monoid>::poly poly;
  typedef typename PolyRing<CoeffRing,Monoid>::coeff_type coeff_type;

  void set_freemodules(const FreeModule *F,
		       const FreeModule *Fsyz);

  void poly_reduce_lead_term(poly * flead,
			     poly * &f,
			     poly * &fsyz,
			     const poly *g,
			     const poly *gsyz,
			     bool use_denom,
			     coeff_type &denom);
  // Assumption: leadmonomial(g) divides leadmonomial(f).

  bool poly_reduce_lead_term_ZZ(poly * &f,
				poly * &fsyz,
				const poly *g,
				const poly *gsyz);
  // Never multiplies f by anything.  IE before(f), after(f) are equiv. mod g.
  // this should ONLY be used if K is coeffZZ.

  void poly_replace_2by2_ZZ(poly * &f,
			    poly * &fsyz,
			    poly * &g,
			    poly * &gsyz);
  // ASSUMPTIONS: coefficient ring is ZZ
  //  lead monomial of f and g are the same (with possibly different coeffs)
  // If u * leadcoeff(f) + v * leadcoeff(g) = gd is the gcd,
  // then:
  //  g <-- u * f + v * g
  //  f <-- c * f - d * g, where c = leadcoeff(g)//gd, d = leadcoeff(f)//gd

  void poly_combine_lead_terms_ZZ(
		    const poly *f,
		    const poly *fsyz,
		    const poly *g,
		    const poly *gsyz,
		    poly *&result,
		    poly *&result_syz);
  // If u*x^A*leadmonom(f) + v*x^B*leadmonom(g) = gcd(u,v)*monom (mod lower terms),
  // set result := u*x^A*f + v*x^B*g
  //     resultsyz := u*x^A*fsyz + v*x^B*gyz
  // To keep in mind:
  //  (a) Schreyer orders
  //  (b) Quotient ideal
  // Currently: this does nothing with the quotient ring
};

template <class CoeffRing, class Monoid>
void PolyRingOps<CoeffRing,Monoid>::poly_reduce_lead_term(poly * flead,
				 poly * &f,
				 poly * &fsyz,
				 const poly *g,
				 const poly *gsyz,
				 bool use_denom,
				 coeff_type &denom)
// Assumption: leadmonomial(g) divides leadmonomial(f).
{
  int comp;
  const coeff_type a = f->coeff;
  const coeff_type b = g->coeff;
  coeff_type u,v;
  K->init(u);
  K->init(v);
  K->syzygy(u,v,a,b); // If possible, u==1, anyway, u>0
  if (g->comp == 0)
    comp = f->comp;
  else
    comp = 0;
  // mult f,flead by u (if u != 1)
  if (!K->is_one(u))
    {
      poly_mult_by_coeff_to(f,u); // modifies f
      poly_mult_by_coeff_to(flead,u);
      poly_mult_by_coeff_to(fsyz,u);
      if (use_denom) K->mult(denom,denom, u);
    }
  // now mult g to cancel
  if (is_skew_commutative())
    {
      poly_get_lead_exponents(F,f,_EXP1); // Removes the Schreyer part
      poly_get_lead_exponents(F,g,_EXP2); // Removes the Schreyer part
      divide_exponents(_EXP1,_EXP2,_EXP3);
      if (_skew.mult_sign(_EXP3, _EXP2) < 0)
	K->negate_to(v);
    }
  M->divide(_MONOM1, f->monom, g->monom);
  poly *result1 = mult_by_term(F,g, v,_MONOM1,comp);
  poly_add_to(F,f,result1);
  if (fsyz != 0 || gsyz != 0) 
    {
      poly *result_syz1 = mult_by_term(Fsyz,gsyz, v,_MONOM1, comp);
      poly_add_to(Fsyz,fsyz,result_syz1);
    }
}

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
