// Copyright 1999  Michael E. Stillman

#include "Eschreyer.hpp"
#include "matrix.hpp"
#include "comp.hpp"
#include "text_io.hpp"

GBKernelComputation::GBKernelComputation(const Matrix *m)
  : R(m->get_ring()->cast_to_PolynomialRing()),
    K(m->get_ring()->Ncoeffs()),
    M(m->get_ring()->Nmonoms()),
    F(m->rows()),
    G(m->cols()),
    n_ones(0),
    n_unique(0),
    n_others(0),
    total_reduce_count(0)
{
  one = K->from_int(1);
  PAIRS_mon = M->make_one();
  REDUCE_mon = M->make_one();
  REDUCE_exp = new int[M->n_vars()];

  // Set 'gb'.
  strip_gb(m);
}

GBKernelComputation::~GBKernelComputation()
{
  M->remove(PAIRS_mon);
  M->remove(REDUCE_mon);
  delete [] REDUCE_exp;
  K->remove(one);
  // Remove gb
  // Remove syzygies
  // Remove mi
}

int GBKernelComputation::calc()
{
  // First find the skeleton
  for (int i=0; i<gb.length(); i++) new_pairs(i);

  Matrix *mm = new Matrix(G);
  for (int p=0; p<syzygies.length(); p++)
    mm->append(G->copy(syzygies[p]));

#if 0
  buffer o;
  o << "skeleton = " << newline;
  mm.text_out(o);
  emit(o.str());
#endif
  // Sort the skeleton now?

  // Now reduce each one of these elements
  for (int j=0; j<syzygies.length(); j++)
    {
      vec v = s_pair(syzygies[j]);
      reduce(v,syzygies[j]);
    }
  return COMP_DONE;
}

Matrix *GBKernelComputation::get_syzygies()
{
  // Make the Schreyer free module H.
  Matrix *result = new Matrix(G);
  for (int i=0; i<syzygies.length(); i++)
    {
      result->append(syzygies[i]);
      syzygies[i] = 0;
    }
  return result;
}

//////////////////////
// Private routines //
//////////////////////
vec GBKernelComputation::make_syz_term(ring_elem c, const int *m, int comp) const
{
  return G->new_term(comp, c, m);
}
void GBKernelComputation::strip_gb(const Matrix *m)
{
  int i;
  int *components = new int[F->rank()];
  for (i=0; i<m->n_rows(); i++)
    components[i] = 0;
  for (i=0; i<m->n_cols(); i++)
    if ((*m)[i] != 0)
      components[(*m)[i]->comp]++;
  for (i=0; i<m->n_cols(); i++)
    {
      vecterm head;
      vecterm *last = &head;
      for (vec v = (*m)[i]; v != 0; v = v->next)
	if (components[v->comp] > 0)
	  {
	    vec t = F->copy_term(v);
	    last->next = t;
	    last = t;
	  }
      last->next = 0;
      gb.append(head.next);
    }
#if 0
  for (i=0; i<F->rank(); i++)
    if (components[i] > 0)
      mi[i] = MonomialIdeal(R);
  else
    mi[i] = 0;
#endif
  for (i=0; i<F->rank(); i++)
    mi[i] = new MonomialIdeal(R);
  delete [] components;
}

void GBKernelComputation::new_pairs(int i)
    // Create and insert all of the pairs which will have lead term 'gb[i]'.
    // This also places 'in(gb[i])' into the appropriate monomial ideal
{
  Index<MonomialIdeal> j;
  queue<Bag *> elems;
  intarray vp;			// This is 'p'.
  intarray thisvp;

  M->divide(gb[i]->monom, F->base_monom(gb[i]->comp), PAIRS_mon);
  M->to_varpower(PAIRS_mon, vp);

  // First add in syzygies arising from exterior variables
  // At the moment, there are none of this sort.

  if (M->is_skew())
    {
      intarray vplcm;
      intarray find_pairs_vp;

      int *skewvars = new int[M->n_vars()];
      varpower::to_ntuple(M->n_vars(), vp.raw(), find_pairs_vp);
      int nskew = M->exp_skew_vars(find_pairs_vp.raw(), skewvars);
      
      // Add in syzygies arising from exterior variables
      for (int v=0; v < nskew; v++)
	{
	  int w = skewvars[v];

	  thisvp.shrink(0);
	  varpower::var(w,1,thisvp);
	  Bag *b = new Bag((void *)0, thisvp);
	  elems.insert(b);
	}
      // Remove the local variables
      delete [] skewvars;
    }

  // Second, add in syzygies arising from the base ring, if any
  // The baggage of each of these is NULL
  if (R->is_quotient_ring())
    {
      const MonomialIdeal * Rideal = R->get_quotient_monomials();
      for (j = Rideal->first(); j.valid(); j++)
	{
	  // Compute (P->quotient_ideal->monom : p->monom)
	  // and place this into a varpower and Bag, placing
	  // that into 'elems'
	  thisvp.shrink(0);
	  varpower::quotient((*Rideal)[j]->monom().raw(), vp.raw(), thisvp);
	  if (varpower::is_equal((*Rideal)[j]->monom().raw(), thisvp.raw()))
	    continue;
	  Bag *b = new Bag((void *)0, thisvp);
	  elems.insert(b);
	}
    }

  // Third, add in syzygies arising from previous elements of this same level
  // The baggage of each of these is their corresponding res2_pair

  MonomialIdeal *mi_orig = mi[gb[i]->comp];
  for (j = mi_orig->first(); j.valid(); j++)
    {
      Bag *b = new Bag();
      varpower::quotient((*mi_orig)[j]->monom().raw(), vp.raw(), b->monom());
      elems.insert(b);
    }

  // Make this monomial ideal, and then run through each minimal generator
  // and insert into the proper degree. (Notice that sorting does not
  // need to be done yet: only once that degree is about to begin.

  mi_orig->insert_minimal(new Bag(i, vp));

  queue<Bag *> rejects;
  Bag *b;
  MonomialIdeal * mi = new MonomialIdeal(R, elems, rejects);
  while (rejects.remove(b))
    delete b;

  int *m = M->make_one();
  for (j = mi->first(); j.valid(); j++)
    {
      M->from_varpower((*mi)[j]->monom().raw(), m);
      M->mult(m, gb[i]->monom, m);
      
      vec q = make_syz_term(K->from_int(1),m,i);
      syzygies.append(q);
    }
}

//////////////////////////////////////////////
//  S-pairs and reduction ////////////////////
//////////////////////////////////////////////

bool GBKernelComputation::find_ring_divisor(const int *exp, ring_elem &result)
     // If 'exp' is divisible by a ring lead term, then 1 is returned,
     // and result is set to be that ring element.
     // Otherwise 0 is returned.
{
  if (!R->is_quotient_ring()) return false;
  Bag *b;
  if (!R->get_quotient_monomials()->search_expvector(exp, b))
    return false;
  result = (Nterm *) b->basis_ptr();
  return true;
}

int GBKernelComputation::find_divisor(const MonomialIdeal *mi, 
				   const int *exp,
				   int &result)
{
  // Find all the posible matches, use some criterion for finding the best...
  array<Bag *> bb;
  mi->find_all_divisors(exp, bb);
  int ndivisors = bb.length();
  if (ndivisors == 0) return 0;
  result = bb[0]->basis_elem();
  // Now search through, and find the best one.  If only one, just return it.
  if (comp_printlevel >= 5)
    if (mi->length() > 1)
      {
	buffer o;
	o << ":" << mi->length() << "." << ndivisors << ":";
	emit(o.str());
      }
  if (ndivisors == 1)
    {
      if (mi->length() == 1)
	n_ones++;
      else
	n_unique++;
      return 1;
    }
  n_others++;

  int lowest = result;
  for (int i=1; i<ndivisors; i++)
    {
      int p = bb[i]->basis_elem();
      if (p < lowest)
	lowest = p;
    }
  result = lowest;
  return ndivisors;
}

vec GBKernelComputation::s_pair(vec gsyz) const
{
  vec result = NULL;
  int *si = M->make_one();
  for (vec f = gsyz; f != 0; f = f->next)
    {
      M->divide(f->monom, G->base_monom(f->comp), si);
      vec h = F->mult_by_term(f->coeff, si, gb[f->comp]);
      F->add_to(result, h);
    }
  M->remove(si);
  return result;
}

void GBKernelComputation::reduce(vec &f, vec &fsyz)
{
  vec lastterm = fsyz;  // fsyz has only ONE term.
  ring_elem rg;
  vecHeap fb(F);
  fb.add(f);
  f = NULL;
  const vecterm *lead;
  int q;

  int count = 0;
  if (comp_printlevel >= 4)
    emit_wrapped(",");

  while ((lead = fb.get_lead_term()) != NULL)
    {
      M->divide(lead->monom, F->base_monom(lead->comp), REDUCE_mon);
      M->to_expvector(REDUCE_mon, REDUCE_exp);
      if (find_ring_divisor(REDUCE_exp, rg))
	{
	  // Subtract off f, leave fsyz alone
	  Nterm *r = rg;
	  M->divide(lead->monom, r->monom, REDUCE_mon);
	  ring_elem c = K->negate(lead->coeff);
	  vec h = F->imp_ring_mult_by_term(r, c, REDUCE_mon, lead->comp);
	  K->remove(c);
	  fb.add(h);
	  total_reduce_count++;
	  count++;
	}
      else if (find_divisor(mi[lead->comp], REDUCE_exp, q))
	{
	  ring_elem c = K->negate(lead->coeff);
	  M->divide(lead->monom, gb[q]->monom, REDUCE_mon);
	  vec h = F->imp_mult_by_term(c, REDUCE_mon, gb[q]);
	  lastterm->next = make_syz_term(c, lead->monom, q); // grabs c.
	  lastterm = lastterm->next;
	  fb.add(h);
	  total_reduce_count++;
	  count++;
	}
      else
	{
	  // To get here is an ERROR!
	  fb.remove_lead_term();
	  emit_line("error in Schreyer reduction: element does not reduce to zero!");
	}
    }

  if (comp_printlevel >= 4)
    {
      buffer o;
      o << count;
      emit_wrapped(o.str());
    }
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
