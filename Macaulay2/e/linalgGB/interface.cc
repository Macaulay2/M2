#include "../polyring.hpp"
#include "../freemod.hpp"
#include "Monomials.h"
#include "MonomialSet.h"
#include "SPairSet.h"
#include "linalgGB.hpp"
#include "monoms.h"

#include "interface.h"
#include "../matrixcon.hpp"
#include "../matrix.hpp"
#include <vector>

void from_M2_vec(MonomialSet *H,
		 const FreeModule *F, 
		 vec v,
		 int &deg,
		 int &alpha, // difference in degree of lead term with 
		             // highest degree of a term
		 poly &result)
{
  const PolynomialRing *R = F->get_ring()->cast_to_PolynomialRing();
  const Monoid *M = R->getMonoid();
  int n = 0;
  for (vec w = v; w != 0; w = w->next)
    {
      for (Nterm *t = w->coeff; t != 0; t = t->next)
	n++;
    }
  intarray vp;
  result.len = n;
  result.coeffs = newarray(COEFF_TYPE, n);
  result.monoms = newarray(monomial, n);
  n = 0;
  for (vec w = v; w != 0; w = w->next)
    {
      for (Nterm *t = w->coeff; t != 0; t = t->next)
	{
	  vp.shrink(0);
	  result.coeffs[n] = t->coeff; // COPY THIS
	  M->to_varpower(t->monom, vp);
	  vp[0] = (vp[0]-1)/2;
	  H->find_or_insert(vp.raw(), result.monoms[n]);
	  n++;
	}

    }
}

void from_M2_matrix(const Matrix *m, 
		    MonomialSet *H,
		    gb_array &result_polys)
{
  const FreeModule *F = m->rows();
  for (int i=0; i<m->n_cols(); i++)
    {
      gbelem *g = new gbelem;
      from_M2_vec(H,F,m->elem(i),g->deg,g->alpha,g->f);
      result_polys.push_back(g);
    }
}

vec to_M2_vec(poly &f,
	      const FreeModule *F)
{
#warning "only handles polynomials in one component, assumes polynomials are in order"
  const PolynomialRing *R = F->get_ring()->cast_to_PolynomialRing();
  const Monoid *M = R->getMonoid();
  
  Nterm head;
  Nterm *inresult = &head;

  intarray vp;
  int *m1 = M->make_one();

  for (int i=0; i<f.len; i++)
    {
      vp.shrink(0);
      ring_elem c = f.coeffs[i];
      monomial m = f.monoms[i];
      int *vpa = vp.alloc(2*(*m)+1);
      for (int j=0; j<2*(*m)+1; j++)
	vpa[j] = m[j];
      vpa[0] = 2*(*m)+1;
      M->from_varpower(vpa, m1);
      inresult->next = R->make_flat_term(c, m1);
      inresult = inresult->next;
    }
  inresult->next = 0;
  return R->make_vec(0, head.next);
}

Matrix *to_M2_matrix(gb_array &polys, const FreeModule *F)
{
  MatrixConstructor result(F,polys.size());
  for (int i=0; i<polys.size(); i++)
    result.set_column(i, to_M2_vec(polys[i]->f, F));
  return result.to_matrix();
}

void spair_testing(MonomialSet *H,
		   gb_array &polys)
{
  SPairSet *S = new SPairSet(H);
  gb_array gb;
  for (int i=0; i<polys.size(); i++)
    {
      // First make a gbelem, and insert it into gb
      gbelem *g;
      *g = *(polys[i]);
      g->is_minimal = 1;
      gb.push_back(g);

      // Now call spair update
      S->find_new_pairs(gb, false);
      S->display();
    }
}

#if 0
Todo:
  Define the matrix type
  Create the matrix, row and column info
    Sort the matrix
    Polynomial arithmetic
  Solve the matrix
  

#endif
// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e/linalgGB "
//  End:
