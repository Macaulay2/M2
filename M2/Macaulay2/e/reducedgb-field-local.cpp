// Copyright 2005, Michael E. Stillman

#include "reducedgb-field-local.hpp"
#include "monideal.hpp"
#include <functional>
#include <algorithm>

ReducedGB_Field_Local::~ReducedGB_Field_Local()
{
}

ReducedGB_Field_Local::ReducedGB_Field_Local(GBRing *R0,
				 const PolynomialRing *originalR0,
				 const FreeModule *F0,
				 const FreeModule *Fsyz0) 
: ReducedGB_Field(R0,originalR0,F0,Fsyz0)
{
}

void ReducedGB_Field_Local::minimalize(const vector<POLY, gc_allocator<POLY> > &polys0)
{
  ReducedGB_Field::minimalize(polys0);

  // Now set the alpha and degree values for these
}

void ReducedGB_Field_Local::remainder(POLY &f, bool use_denom, ring_elem &denom)
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
#warning "reduce h.fsyz??"
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
  f.fsyz = h.fsyz;
  R->exponents_delete(_EXP);
}

void ReducedGB_Field_Local::remainder(gbvector *&f, bool use_denom, ring_elem &denom)
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

