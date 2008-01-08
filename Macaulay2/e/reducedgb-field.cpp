// Copyright 2005, Michael E. Stillman

#include "reducedgb-field.hpp"
#include "monideal.hpp"
#include <functional>
#include <algorithm>

ReducedGB_Field::~ReducedGB_Field()
{
  delete T;
  Rideal = 0;
}

void ReducedGB_Field::set_gb(VECTOR(POLY) &polys0)
{
}

struct ReducedGB_Field_sorter : public std::binary_function<int,int,bool> {
  GBRing *R;
  const FreeModule *F;
  const VECTOR(POLY) &gb;
  ReducedGB_Field_sorter(GBRing *R0,
			 const FreeModule *F0,
			 const VECTOR(POLY) &gb0)
    : R(R0), F(F0), gb(gb0) {}
  bool operator()(int xx, int yy) {
    gbvector *x = gb[xx].f;
    gbvector *y = gb[yy].f;
    return R->gbvector_compare(F,x,y) == LT;
  }
};

ReducedGB_Field::ReducedGB_Field(GBRing *R0,
				 const PolynomialRing *originalR0,
				 const FreeModule *F0,
				 const FreeModule *Fsyz0) 
: ReducedGB(R0,originalR0,F0,Fsyz0), T(0)
{
  T = MonomialTable::make(R0->n_vars());
  if (originalR->is_quotient_ring())
    Rideal = originalR->get_quotient_monomials();
}

void ReducedGB_Field::minimalize(const VECTOR(POLY) &polys0,
				 bool auto_reduced)
// I have to decide: does this ADD to the existing set?
{
  // First sort these elements via increasing lex order (or monomial order?)
  // Next insert minimal elements into T, and polys

  VECTOR(int) positions;
  positions.reserve(polys0.size());

  for (int i=0; i<polys0.size(); i++)
    positions.push_back(i);

  sort(positions.begin(), positions.end(), ReducedGB_Field_sorter(R,F,polys0));

  // Now loop through each element, and see if the lead monomial is in T.
  // If not, add it in , and place element into 'polys'.

  for (VECTOR(int)::iterator i = positions.begin(); i != positions.end(); i++)
    {
      Bag *not_used;
      gbvector *f = polys0[*i].f;
      exponents e = R->exponents_make();
      R->gbvector_get_lead_exponents(F,f,e);
      if ((!Rideal || !Rideal->search_expvector(e, not_used))
	  && T->find_divisors(1, e, f->comp) == 0)
	{
	  // Keep this element
	  
	  POLY h;
	  ring_elem junk;
	  
	  h.f = R->gbvector_copy(f);
	  h.fsyz = R->gbvector_copy(polys0[*i].fsyz);
	  
	  if (auto_reduced)
	    remainder(h,false,junk); // This auto-reduces h.

	  R->gbvector_remove_content(h.f,h.fsyz);

	  T->insert(e, f->comp, polys.size());
	  polys.push_back(h);
	}
      else
	R->exponents_delete(e);
    }
}

void ReducedGB_Field::remainder(POLY &f, bool use_denom, ring_elem &denom)
{
  gbvector head;
  gbvector *frem = &head;
  frem->next = 0;
  POLY h = f;
  exponents _EXP = R->exponents_make();
  while (!R->gbvector_is_zero(h.f))
    {
      R->gbvector_get_lead_exponents(F, h.f, _EXP);
      int x = h.f->comp;
      Bag *b;
      if (Rideal != 0 && Rideal->search_expvector(_EXP,b))
	{
	  const gbvector *g = originalR->quotient_gbvector(b->basis_elem());
	  R->gbvector_reduce_lead_term(F, Fsyz,
				       head.next,
				       h.f, h.fsyz,
				       g, 0,
				       use_denom, denom);
	  
	}
      else
	{
	  int w = T->find_divisor(_EXP,x);
	  if (w >= 0)
	    {
	      POLY g = polys[w];
	      R->gbvector_reduce_lead_term(F, Fsyz,
					   head.next,
					   h.f, h.fsyz,
					   g.f, g.fsyz,
					   use_denom, denom);
	    }
	  else
	    {
	      frem->next = h.f;
	      frem = frem->next;
	      h.f = h.f->next;
	      frem->next = 0;
	    }
	}
    }
  h.f = head.next;
  //  R->gbvector_remove_content(h.f, h.fsyz, use_denom, denom);
  f.f = h.f;
  originalR->get_quotient_info()->gbvector_normal_form(Fsyz, h.fsyz);
  f.fsyz = h.fsyz;
  R->exponents_delete(_EXP);
}

void ReducedGB_Field::remainder(gbvector *&f, bool use_denom, ring_elem &denom)
{
  gbvector *zero = 0;
  gbvector head;
  gbvector *frem = &head;
  frem->next = 0;
  gbvector * h = f;
  exponents _EXP = R->exponents_make();
  while (!R->gbvector_is_zero(h))
    {
      R->gbvector_get_lead_exponents(F, h, _EXP);
      int x = h->comp;
      Bag *b;
      if (Rideal != 0 && Rideal->search_expvector(_EXP,b))
	{
	  const gbvector *g = originalR->quotient_gbvector(b->basis_elem());
	  R->gbvector_reduce_lead_term(F, Fsyz,
				       head.next,
				       h, zero,
				       g, zero,
				       use_denom, denom);
	  
	}
      else 
	{
	  int w = T->find_divisor(_EXP,x);
	  if (w < 0)
	    {
	      frem->next = h;
	      frem = frem->next;
	      h = h->next;
	      frem->next = 0;
	    }
	  else
	    {
	      POLY g = polys[w];
	      R->gbvector_reduce_lead_term(F, Fsyz,
					   head.next,
					   h, zero,
					   g.f, zero,
					   use_denom, denom);
	    }
	}
    }
  h = head.next;
  // R->gbvector_remove_content(h, 0, use_denom, denom);
  f = h;
  R->exponents_delete(_EXP);
}


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:

