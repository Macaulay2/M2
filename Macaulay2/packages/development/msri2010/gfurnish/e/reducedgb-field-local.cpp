// Copyright 2005, Michael E. Stillman

#include "reducedgb-field-local.hpp"
#include "montable.hpp"
#include "gbweight.hpp"
#include "poly.hpp"
#include <functional>
#include <algorithm>
#include "text-io.hpp"

ReducedGB_Field_Local::~ReducedGB_Field_Local()
{
}

ReducedGB_Field_Local::ReducedGB_Field_Local(GBRing *R0,
				 const PolynomialRing *originalR0,
				 const FreeModule *F0,
				 const FreeModule *Fsyz0,
				 const GBWeight *wt0) 
  : ReducedGB_Field(R0,originalR0,F0,Fsyz0),
    T1(0),
    wt(wt0)
{
     // fprintf(stderr, "creating GB with local order\n");
  if (wt == 0)
    wt = new GBWeight(F0, 0);
  for (int i=0; i<originalR0->n_quotients(); i++)
    {
      int f_lead_wt;
      const gbvector *f = originalR0->quotient_gbvector(i);
      int d = wt->gbvector_weight(f, f_lead_wt);
      int a = d - f_lead_wt;

      divisor_info t;
      t.g.f = const_cast<gbvector *>(f);
      t.g.fsyz = 0;
      t.size = R->gbvector_n_terms(f);
      t.alpha = a;

      ring_elems.push_back(t);
    }
}

void ReducedGB_Field_Local::minimalize(const VECTOR(POLY) &polys0,
				       bool auto_reduced)
{
  // auto_reduced flag is ignored, since it can lead to infinite loops here
  ReducedGB_Field::minimalize(polys0,false);

  for (int i=0; i<polys.size(); i++)
    {
      int f_lead_wt;
      gbvector *f = polys[i].f;
      int d = wt->gbvector_weight(f,f_lead_wt);
      int a = d - f_lead_wt;

      divisor_info t;
      t.g = polys[i];
      t.size = R->gbvector_n_terms(f);
      t.alpha = a;

      gb_elems.push_back(t);
    }
}

bool ReducedGB_Field_Local::find_good_divisor(exponents h_exp,
					      int h_comp,
					      int h_deg,
					      int &h_alpha,           // result value
					      POLY &result_g,         // result value
					      int & result_g_alpha)   // result value
{
  VECTOR(MonomialTable::mon_term *) divisors;
  MonomialTable *ringtable = originalR->get_quotient_MonomialTable();

  h_alpha = h_deg - wt->exponents_weight(h_exp,h_comp);

  int n0 = (ringtable ? ringtable->find_divisors(-1, h_exp, 1, &divisors) : 0);
  int n1 = T1->find_divisors(-1, h_exp, 1, &divisors);
  int n2 = T->find_divisors(-1, h_exp, h_comp, &divisors);
  int n = divisors.size();
  if (n == 0) return false;

  divisor_info *div_info = newarray(divisor_info, divisors.size());

  int next = 0;

  // ring divisors
  for (int i=0; i<n0; i++)
    {
      int id = divisors[i]->_val;
      div_info[next++] = ring_elems[id];
    }
  // new divisors
  for (int i=0; i<n1; i++)
    {
      int id = divisors[n0+i]->_val;
      div_info[next++] = new_poly_elems[id];
    }
  // gb divisors
  for (int i=0; i<n2; i++)
    {
      int id = divisors[n0+n1+i]->_val;
      div_info[next++] = gb_elems[id];
    }

  if (gbTrace>=4)
    {
      buffer o;
      o << "\nfind good divisor:";
      if (n0 > 0)
	{
	  o << "\n  ndivisors from quotient ring elements " << n0;
	  for (int j=0; j<n0; j++)
	    {
	      divisor_info &t = div_info[j];
	      o << "\n    size " << t.size << " alpha " << t.alpha << " lead ";
	      gbvector *f = R->gbvector_lead_term(-1,F,t.g.f);
	      R->gbvector_text_out(o,F,f);
	      R->gbvector_remove(f);
	    }
	}
      if (n1 > 0)
	{
	  o << "\n  ndivisors from appended elements " << n1;
	  for (int j=0; j<n1; j++)
	    {
	      divisor_info &t = div_info[n0+j];
	      o << "\n    size " << t.size << " alpha " << t.alpha << " lead ";
	      gbvector *f = R->gbvector_lead_term(-1,F,t.g.f);
	      R->gbvector_text_out(o,F,f);
	      R->gbvector_remove(f);
	    }
	}
      if (n2 > 0)
	{
	  o << "\n  ndivisors from gb elements " << n1;
	  for (int j=0; j<n2; j++)
	    {
	      divisor_info &t = div_info[n0+n1+j];
	      o << "\n    size " << t.size << " alpha " << t.alpha << " lead ";
	      gbvector *f = R->gbvector_lead_term(-1,F,t.g.f);
	      R->gbvector_text_out(o,F,f);
	      R->gbvector_remove(f);
	    }
	}
      emit(o.str());
    }

  // Now all of the desired elements are in div_info
  // First, find the minimum alpha value
  int min_alpha = div_info[0].alpha;
  for (int i=1; i<n; i++)
    if (div_info[i].alpha < min_alpha)
      min_alpha = div_info[i].alpha;
  result_g_alpha = min_alpha;

  int min_size = -1;
  int result_i = -1;
  int nmatches = 0;
  // Now, out of the ones with this alpha, find the minimum size
  for (int i=0; i<n; i++)
    {
      if (div_info[i].alpha == min_alpha)
	{
	  int this_size = div_info[i].size;
	  if (min_size < 0 || this_size < min_size)
	    {
	      min_size = this_size;
	      result_i = i;
	      nmatches = 1;
	    }
	  else if (this_size == min_size)
	    {
	      nmatches++;
	    }
	}
    }

  if (nmatches > 1 && gbTrace == 3)
    {
      buffer o;
      o << nmatches;
      emit_wrapped(o.str());
    }

  // At this point, result_i points to the element we wish to return
  assert(result_i >= 0);
  result_g = div_info[result_i].g;

  if (gbTrace>=4)
    {
      buffer o;
      if (nmatches > 1)
	o << "\n  nmatches " << n;
      o << "\n  chosen value: ";
      int size = R->gbvector_n_terms(result_g.f);
      o << "\n    size " << size << " alpha " << result_g_alpha << " lead ";
      gbvector *f = R->gbvector_lead_term(-1,F,result_g.f);
      R->gbvector_text_out(o,F,f);
      R->gbvector_remove(f);
      emit(o.str());
    }

  return true;

#if 0

  MonomialTable *ringtable = originalR->get_quotient_MonomialTable();
  if (ringtable)
    {
      n = ringtable->find_divisors(-1, h_exp, 1, &divisors);

      if (n > 0)
	{
	  POLY p;
	  p.fsyz = 0;
	  for (int i=0; i<divisors.size(); i++)
	    {
	      MonomialTable::mon_term *t = divisors[i];
	      int id = t->_val;
	      p.f = const_cast<gbvector *>(originalR->quotient_gbvector(id));
	      int g_alpha = ring_alpha[id];
	      if (g_alpha <= h_alpha)
		{
		  result_g = p;
		  result_g_alpha = g_alpha;
		  return true;
		}
	      if (min_alpha < 0 || g_alpha < min_alpha)
		{
		  min_alpha = g_alpha;
		  result_g = p;
		  result_g_alpha = g_alpha;
		}
	    }
	}
    }
  divisors.clear();

  if (gbTrace>=4)
    {
      buffer o;
      o << "\nfind good divisor:";
      emit(o.str());
    }

  // check the new polys
  n = T1->find_divisors(-1, h_exp, h_comp, &divisors);
  if (n > 0)
    {
      POLY p;
      if (gbTrace>=4)
	{
	  buffer o;
	  o << "\n  ndivisors from appended elements " << n;
	  for (int j=0; j<n; j++)
	    {
	      MonomialTable::mon_term *t = divisors[j];
	      int id = t->_val;
	      p = newpol[id];
	      int g_alpha = newpol_alpha[id];
	      int size = R->gbvector_n_terms(p.f);
	      o << "\n    size " << size << " alpha " << g_alpha << " lead ";
	      gbvector *f = R->gbvector_lead_term(-1,F,p.f);
	      R->gbvector_text_out(o,F,f);
	    }
	  emit(o.str());
	}
      for (int i=0; i<divisors.size(); i++)
	{
	  MonomialTable::mon_term *t = divisors[i];
	  int id = t->_val;
	  p = newpol[id];
	  int g_alpha = newpol_alpha[id];
	  if (result_g_alpha < 0 && g_alpha <= h_alpha)
	    {
	      result_g = p;
	      result_g_alpha = g_alpha;
	      min_alpha = g_alpha;
	      //break; //return true;
	    }
	  if (min_alpha < 0 ||  g_alpha < min_alpha)
	    {
	      min_alpha = g_alpha;
	      result_g = p;
	      result_g_alpha = g_alpha;
	    }
	}
    }
  divisors.clear();
      
  // Now check the GB itself
  n = T->find_divisors(-1, h_exp, h_comp, &divisors);
  if (n > 0)
    {
      POLY p;
      if (gbTrace>=4)
	{
	  buffer o;
	  o << "\n  ndivisors from GB " << n;
	  for (int j=0; j<n; j++)
	    {
	      MonomialTable::mon_term *t = divisors[j];
	      int id = t->_val;
	      p = polys[id];
	      int g_alpha = alpha[id];
	      int size = R->gbvector_n_terms(p.f);
	      o << "\n     size " << size << " alpha " << g_alpha << " lead ";
	      gbvector *f = R->gbvector_lead_term(-1,F,p.f);
	      R->gbvector_text_out(o,F,f);
	    }
	  emit(o.str());
	}

      for (int i=0; i<divisors.size(); i++)
	{
	  MonomialTable::mon_term *t = divisors[i];
	  int id = t->_val;
	  p = polys[id];
	  int g_alpha = alpha[id];
	  if (result_g_alpha < 0 && g_alpha <= h_alpha)
	    {
	      result_g = p;
	      result_g_alpha = g_alpha;
	      min_alpha = g_alpha;
	      //break;
	      //return true;
	    }
	  if (min_alpha < 0 || g_alpha < min_alpha)
	    {
	      min_alpha = g_alpha;
	      result_g = p;
	      result_g_alpha = g_alpha;
	    }
	}
    }
  divisors.clear();


  if (gbTrace>=4)
    {
      buffer o;
      o << "\n  chosen value: ";
      int size = R->gbvector_n_terms(result_g.f);
      o << "\n    size " << size << " alpha " << result_g_alpha << " lead ";
      gbvector *f = R->gbvector_lead_term(-1,F,result_g.f);
      R->gbvector_text_out(o,F,f);
      R->gbvector_remove(f);
      emit(o.str());
    }
  
  return (min_alpha >= 0);
#endif
}

void ReducedGB_Field_Local::reset_table()
{
  new_poly_elems.clear();
  delete T1;
}

void ReducedGB_Field_Local::store_in_table(const POLY &h, 
					   exponents h_exp,
					   int h_comp,
					   int h_alpha)
{
  int id = new_poly_elems.size();
  divisor_info t;
  t.g = h;
  t.alpha = h_alpha;
  t.size = R->gbvector_n_terms(t.g.f);
  new_poly_elems.push_back(t);
  T1->insert(h_exp,h_comp,id); // grabs h_exp
}

void ReducedGB_Field_Local::remainder(POLY &f, bool use_denom, ring_elem &denom)
{
  if (f.f == 0) return;
  T1 = MonomialTable::make(R->n_vars());
  gbvector head;
  gbvector *frem = &head;
  frem->next = 0;
  POLY h = f;
  exponents h_exp = R->exponents_make();
  int h_alpha, g_alpha;
  int h_deg = wt->gbvector_weight(f.f);
  while (!R->gbvector_is_zero(h.f))
    {
      POLY g;
      R->gbvector_get_lead_exponents(F, h.f, h_exp);
      int h_comp = h.f->comp;
      if (find_good_divisor(h_exp,h_comp,h_deg,
			    h_alpha,g,g_alpha)) // sets these three values
	{
	  if (g_alpha > h_alpha)
	    {
	      if (head.next != 0)
		{
		  // In this case, we can't reduce the tail without 
		  // risking an infinite loop.  So we delcare ourselves done
		  // Attach the rest of h.f to frem
		  frem->next = h.f;
		  break;
		}
	      // place h into T1, and store its (value,deg,alpha) values.
	      store_in_table(h, h_exp, h_comp, h_alpha);
	      h_deg += g_alpha - h_alpha;
	      h_exp = R->exponents_make();
	    }
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

  f.f = head.next;
  f.fsyz = h.fsyz;
  R->exponents_delete(h_exp);
  reset_table();
}

void ReducedGB_Field_Local::remainder(gbvector *&f, bool use_denom, ring_elem &denom)
{
  if (f == 0) return;
  T1 = MonomialTable::make(R->n_vars());
  gbvector *zero = 0;
  gbvector head;
  gbvector *frem = &head;
  frem->next = 0;
  POLY h;
  h.f = f;
  h.fsyz = NULL;
  exponents h_exp = R->exponents_make();
  int h_alpha, g_alpha;
  int h_deg = wt->gbvector_weight(f);
  while (!R->gbvector_is_zero(h.f))
    {
      if (gbTrace == 3)
	emit_wrapped(".");
      POLY g;
      R->gbvector_get_lead_exponents(F, h.f, h_exp);
      int h_comp = h.f->comp;

      if (gbTrace >= 4)
	{
	  buffer o;
	  o << "\nreducing ";
	  R->gbvector_text_out(o,F,h.f);
	  emit(o.str());
	}

      if (find_good_divisor(h_exp,h_comp,h_deg,
			    h_alpha,g,g_alpha)) // sets these three values
	{
	  if (gbTrace >= 4)
	    {
	      buffer o;
	      o << "  h_alpha " << h_alpha << " g_alpha " << g_alpha; // << " reducing using ";
	      //R->gbvector_text_out(o,F,g.f);
	      //o << newline;
	      emit(o.str());
	    }
	  if (g_alpha > h_alpha)
	    {
	      if (head.next != 0)
		{
		  // In this case, we can't reduce the tail without 
		  // risking an infinite loop.  So we delcare ourselves done
		  // Attach the rest of h.f to frem
		  frem->next = h.f;
		  break;
		}
	      // place h into T1, and store its (value,deg,alpha) values.
	      POLY h_copy;
	      h_copy.f = R->gbvector_copy(h.f);
	      h_copy.fsyz = 0;
	      store_in_table(h_copy, h_exp, h_comp, h_alpha);
	      if (gbTrace == 3) emit_wrapped("x");
	      if (gbTrace == 4) emit("\nstored result\n");
	      h_deg += g_alpha - h_alpha;
	      h_exp = R->exponents_make();
	    }
	  R->gbvector_reduce_lead_term(F, Fsyz,
				       head.next,
				       h.f, zero,
				       g.f, zero,
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

  f = head.next;
  R->exponents_delete(h_exp);
  reset_table();
}


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:

