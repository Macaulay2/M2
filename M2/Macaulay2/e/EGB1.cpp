// Copyright 1999  Michael E. Stillman

#include "style.hpp"
#include "EGB1.hpp"
#include "polyring.hpp"
#include "matrix.hpp"
#include "text_io.hpp"
#include "ntuple.hpp"
#include "interp.hpp"
extern char system_interrupted;

stash *EGB1::mystash;
stash *egb_elem::mystash;
void i_EGB()
{
  egb_elem::mystash = new stash("egb_elem", sizeof(egb_elem));
  EGB1::mystash = new stash("EGB1", sizeof(EGB1));
  es_pair::mystash = new stash("es_pair", sizeof(es_pair));
}
void make_EGB_comp(const Matrix &m, bool dosyz, int nsyz, int strategy)
{
  EGB1 *p = new EGB1(m,dosyz,nsyz,strategy);
  gStack.insert(p);
}

ERingTable *EGB1::create_ring_table() const
{
  // Loop through all quotient elements, make ering_elem's, and insert into the
  // result table.
  ERingTable *ring_table = new ERingTable(I);
  array< ering_elem * > junk;

  for (int i=0; i<I.n_quotients(); i++)
    {
      ering_elem *r = new ering_elem;
      r->f = I.get_quotient_element(i);
      exponent_vector *t = new_exponent_vector();
      I.to_exponents(I.lead_monomial_of_polynomial(r->f), t);
      r->lcm = t;
      r->degree = ntuple::weight(nvars,r->lcm,heuristicWeightVector);
      ring_table->insert(r, junk);  // We are guaranteed that all the 'r' s are minimal.
    }
  return ring_table;
}

///////////////////////
// ESPairLookupTable //
///////////////////////

ESPairLookupTable::ESPairLookupTable(int nvars, es_pair *pairs)
  : nvars(nvars), table(0), last(0)
{
  table = new node;  // A list header
  table->next = 0;
  last = table;
  append_list(pairs);
}
ESPairLookupTable::~ESPairLookupTable()
{
  last = 0;
  delete table;
  table = 0;
}
void ESPairLookupTable::append(es_pair *&p)
{
  if (p == 0) return;
  node *t = new node;
  t->elem = p;
  t->mask = ntuple::mask(nvars, p->lcm);
  t->next = 0;
  last->next = t;
  last = t;
  p = 0;
}
void ESPairLookupTable::append_list(es_pair *&pairs)
{
  while (pairs != 0) {
    es_pair *tmp = pairs;
    pairs = pairs->next;
    append(tmp);
  }
}
bool ESPairLookupTable::find_divisor(const exponent_vector *exp, 
				     es_pair *&result) const
{
  unsigned int expmask = ~(ntuple::mask(nvars,exp));

  for (node *p = table->next; p != 0; p = p->next)
    if ((expmask & p->mask) == 0)
      {
	if (ntuple::divides(nvars, p->elem->lcm, exp))
	  {
	    result = p->elem;
	    return true;
	  }
      }
  return false;
}
es_pair *ESPairLookupTable::value()
  // Returns a list of es_pair's.
{
  es_pair head;
  es_pair *q = &head;
  node *p = table->next;
  table->next = 0;
  last = table;
  while (p != 0)
    {
      node *tmp;
      tmp = p;
      p = p->next;
      q->next = tmp->elem;
      q = q->next;
      delete tmp;
    }
  return head.next;
}
///////////////////////////////////////////////////////////////


bool EGB1::set_up(const Matrix &m, int csyz, int nsyz, int strat)
{
  // Returns false if an error is found.  That is, if the ring of 'm' is not
  // appropriate.
  n_gb = n_subring = 0;
  n_pairs = n_computed = 0;
  n_saved_gcd = n_saved_lcm = n_saved_gcd_choice = 0;
  next_gb_num = 1;

  int i;
  nvars = I.n_vars();
  exponent_stash = new stash("exponents", nvars * sizeof(int));
  heuristicWeightVector = new_exponent_vector();
  for (i=0; i<nvars; i++)
    heuristicWeightVector[i] = 1;
  mSkewVars = new_exponent_vector();
  mReduceExp = new_exponent_vector();

  if (I.ring_is_quotient())
    ring_table = create_ring_table();
  else
    ring_table = 0;

  spairs = new ESPairSet;
  this_set = 0;

  int ncols = m.n_cols();
  int nrows = m.n_rows();
  
  if (nsyz < 0 || nsyz > ncols)
    nsyz = ncols;
  n_comps_per_syz = nsyz;
  collect_syz = csyz;

  F = m.rows();
  Fsyz = m.cols()->sub_space(n_comps_per_syz);
  bump_up(F);
  bump_up(Fsyz);

  for (int i=0; i<nrows; i++)
    gb[i] = new EGBLookupTable(I);

  moreGenerators(0, ncols - 1, m);

  strategy = strat;

  is_ideal = (nrows == 1 && !csyz) && (!I.ring_is_weyl_algebra());
  use_hilb_function = false;
  return true;
}

void EGB1::inter_reduce(egb_elem *&/*gens*/)
{
  // MES
}

//////////////////////////////////////////////
//  s pair construction //////////////////////
//////////////////////////////////////////////
exponent_vector *EGB1::new_exponent_vector() const
{
  exponent_vector *result = (exponent_vector *) exponent_stash->new_elem();
  return result;
}
void EGB1::remove_exponent_vector(exponent_vector *&a) const
{
  exponent_stash->delete_elem(a);
}

void EGB1::increment(egb_elem *p) const
{
  p->npairs++;
}
void EGB1::decrement(egb_elem *p) const
{
  p->npairs--;
  if (p->npairs > 0 || p->me > 0) return;
  I.remove_vector(F,p->f);
  I.remove_vector(Fsyz,p->fsyz);
}
int EGB1::F_degree(int i) const
{
  // HOW TO HANDLE THIS??
  return 0;
}
int EGB1::lead_component(es_pair *p) const
{
  switch (p->type) {
  case SP_GEN:
    return I.lead_component(p->s.gen.f);
  case SP_SYZ:
    return I.lead_component(p->s.syz.i->f);
  case SP_SKEW:
    return I.lead_component(p->s.ringsyz.i->f);
  case SP_RING:
    return I.lead_component(p->s.ringsyz.i->f);
  default:
    emit_line("internal error: unknown spair type");
    abort();
  }

}
exponent_vector *EGB1::make_skew_lcm(const exponent_vector *exp,
				     int v,
				     int deg_of_exp,
				     int & result_degree) const
{
  result_degree = deg_of_exp + heuristicWeightVector[v];
  exponent_vector *result = new_exponent_vector();
  ntuple::copy(nvars, exp, result);
  result[v]++;
  return result;
}
exponent_vector *EGB1::make_lcm(const exponent_vector *exp1,
				const exponent_vector *exp2,
				int deg_of_exp1,
				int deg_of_exp2,
				int & result_degree) const
{
  exponent_vector *result = new_exponent_vector();
  int deg1 = deg_of_exp1;
  int deg2 = deg_of_exp2;
  for (int i=0; i<nvars; i++)
    {
      int a = exp1[i] - exp2[i];
      if (a == 0)
	{
	  result[i] = exp1[i];
	}
      else if (a > 0)
	{
	  result[i] = exp1[i];
	  deg2 += heuristicWeightVector[i] * a;
	}
      else
	{
	  result[i] = exp2[i];
	  deg1 += heuristicWeightVector[i] * (-a);
	}
    }
  if (deg1 > deg2)
    result_degree = deg1;
  else
    result_degree = deg2;
  return result;
}

egb_elem *EGB1::make_gb_elem(int degree,
			     EVector &f, 
			     EVector &fsyz,
			     bool minimal)
{
  egb_elem *result = new egb_elem;
  result->degree = degree;  // This may be the actual degree of 'f' or the 'sugar'.
  result->f = f;
  result->fsyz = fsyz;
  result->npairs = 0;
  int *e = new_exponent_vector();
  I.to_exponents(I.lead_monomial(f), e);
  result->lcm = e;
  result->me = next_gb_num++;
  result->is_minimal = minimal;
  return result;
}

es_pair *EGB1::make_ring_s_pair(egb_elem *p, ering_elem *r) const
{
  es_pair *result = new es_pair;
  result->next = NULL;
  result->type = SP_RING;
  result->s.ringsyz.i = p;
  result->s.ringsyz.j = r;
  increment(p);

  const exponent_vector *m1 = p->lcm;
  const exponent_vector *m2 = r->lcm;
  int deg2 = F_degree(I.lead_component(p->f) + r->degree);
  result->lcm = make_lcm(m1, m2, p->degree, deg2, result->degree);

  return result;
}

es_pair *EGB1::make_skew_s_pair(egb_elem *p, int v) const
{
  es_pair *result = new es_pair;
  result->next = NULL;
  result->type = SP_SKEW;
  result->s.skewsyz.i = p;
  result->s.skewsyz.v = v;
  increment(p);

  result->lcm = make_skew_lcm(p->lcm,
			      v,
			      p->degree,
			      result->degree);
  return result;
}

es_pair *EGB1::make_s_pair(egb_elem *a, egb_elem *b) const
{
  es_pair *result = new es_pair;
  result->next = NULL;
  result->type = SP_SYZ;
  result->s.syz.i = a;
  result->s.syz.j = b;
  increment(a);
  increment(b);
  result->lcm = make_lcm(a->lcm,
			 b->lcm,
			 a->degree,
			 b->degree,
			 result->degree); // sets result->degree.
  return result;
}

es_pair *EGB1::make_gen_pair(int i, const EVector &f)
{
  vec fsyz;

  if (i < n_comps_per_syz)
    fsyz = I.e_sub_i(Fsyz,i);
  else
    fsyz = I.zero_vector(Fsyz);

  if (I.is_zero_vector(F,f))
    {
      collect_syzygy(fsyz);
      return 0;
    }

  es_pair *result = new es_pair;
  result->next = NULL;
  result->type = SP_GEN;
  result->s.gen.f = I.copy_vector(F,f);
  result->s.gen.fsyz = fsyz;

  result->lcm = new_exponent_vector();
  I.to_exponents(I.lead_monomial(f), result->lcm);
  result->degree = F_degree(I.lead_component(f)) 
                        + ntuple::weight(nvars,result->lcm,
					 heuristicWeightVector);
  return result;
}

void EGB1::remove_pair(es_pair *& p) const
{
  p->next = NULL;
  remove_exponent_vector(const_cast<int *&>(p->lcm));
  switch (p->type) {
  case SP_RING:
    decrement(p->s.ringsyz.i);
  case SP_SKEW:
    decrement(p->s.skewsyz.i);
    break;
  case SP_SYZ:
    decrement(p->s.syz.i);
    decrement(p->s.syz.j);
    break;
  case SP_GEN:
    // There are times when these should be removed though... MES
    //    F->remove(p->s.gen.f);
    //    Fsyz->remove(p->s.gen.fsyz);
    break;
  }

  delete p;
  p = NULL;
}
bool EGB1::is_gcd_one_pair(es_pair *p) const
{
  if (p->type != SP_SYZ) return false;
  const exponent_vector *e1 = p->s.syz.i->lcm;
  const exponent_vector *e2 = p->s.syz.j->lcm;
  for (int i=0; i<nvars; i++)
    if (e1[i] > 0 && e2[i] > 0)
      return false;
  return true;
}

void EGB1::compute_s_pair(es_pair *p, vector_heap &f, vector_heap &fsyz)
{
  egb_elem *g1, *g2;
  ering_elem *r2;
  ringelement a,b;
  int sign = 1;
  exponent_vector *e1 = new_exponent_vector();
  exponent_vector *e2 = new_exponent_vector();
  switch (p->type)
    {
    case SP_GEN:
      f.add(p->s.gen.f);
      fsyz.add(p->s.gen.fsyz);
      break;
    case SP_SYZ:
      // First determine the syzygy on the monomial, and coefficients
      g1 = p->s.syz.i;
      g2 = p->s.syz.j;
      I.coefficient_syzygy(I.lead_coefficient(g1->f),
			   I.lead_coefficient(g2->f),
			   a, b);
      I.exponent_syzygy(g1->lcm, g2->lcm, e1, e2, sign);
      if (sign == 1)
	{
	  ringelement c = I.negate_coefficient(b);
	  I.remove_coefficient(b);
	  b = c;
	}

      I.add_multiple_to(f, a, e1, g1->f);
      I.add_multiple_to(fsyz, a, e1, g1->fsyz);

      I.add_multiple_to(f, b, e2, g2->f);
      I.add_multiple_to(fsyz, b, e2, g2->fsyz);

      I.remove_coefficient(a);
      I.remove_coefficient(b);
      break;
    case SP_SKEW:
      ntuple::one(nvars, e1);
      e1[p->s.skewsyz.v] = 1;
      I.add_multiple_to(f, I.one(), e1, p->s.skewsyz.i->f);
      I.add_multiple_to(fsyz, I.one(), e1, p->s.skewsyz.i->fsyz);
      break;
    case SP_RING:
      g1 = p->s.ringsyz.i;
      r2 = p->s.ringsyz.j;
      I.coefficient_syzygy(I.lead_coefficient(g1->f),
			   I.lead_coefficient_of_polynomial(r2->f),
			   a, b);
      I.exponent_syzygy(g1->lcm, r2->lcm, e1, e2, sign);
      if (sign == 1)
	{
	  ringelement c = I.negate_coefficient(b);
	  I.remove_coefficient(b);
	  b = c;
	}

      I.add_multiple_to(f, a, e1, g1->f);
      I.add_multiple_to(fsyz, a, e1, g1->fsyz);

      I.add_ring_multiple_to(f, b, e2, I.lead_component(g1->f), r2->f);

      I.remove_coefficient(a);
      I.remove_coefficient(b);
      break;
    };
  remove_exponent_vector(e1);
  remove_exponent_vector(e2);
}
///////////////////////
// S-pair operations //
///////////////////////
void EGB1::remove_pairs(es_pair *&pair_list) const
{
  while (pair_list != 0)
    {
      es_pair *tmp = pair_list;
      pair_list = tmp->next;
      remove_pair(tmp);
    }
}
int EGB1::compare_pairs(es_pair *f, es_pair *g) const
{
  // These pairs all have the same degree lcm's.
  int cmp = ntuple::lex_compare(nvars, f->lcm, g->lcm);
  return cmp;
}

es_pair *EGB1::merge_pairs(es_pair *f, es_pair *g) const
{
  // Sort in ascending degree order, then ascending monomial order
  if (g == NULL) return f;
  if (f == NULL) return g;
  es_pair head;
  es_pair *result = &head;
  while (1)
    switch (compare_pairs(f, g))
      {
      case LT:
	result->next = g;
	result = result->next;
	g = g->next;
	if (g == NULL) 
	  {
	    result->next = f;
	    return head.next;
	  }
	break;
      case GT:
      case EQ:
	result->next = f;
	result = result->next;
	f = f->next;
	if (f == NULL) 
	  {
	    result->next = g; 
	    return head.next;
	  }
	break;
      }
}

void EGB1::sort_pairs(es_pair *&p) const
{
  if (p == NULL || p->next == NULL) return;
  es_pair *p1 = NULL;
  es_pair *p2 = NULL;
  while (p != NULL)
    {
      es_pair *tmp = p;
      p = p->next;
      tmp->next = p1;
      p1 = tmp;

      if (p == NULL) break;
      tmp = p;
      p = p->next;
      tmp->next = p2;
      p2 = tmp;
    }

  sort_pairs(p1);
  sort_pairs(p2);
  p = merge_pairs(p1, p2);
}

void EGB1::choose_nice_pair(es_pair *&p)
{
  if (comp_printlevel >= 5)
    {
      int len = 0; 
      for (es_pair *a = p; a!=0; a=a->next) len++;
      if (len > 1)
	{
	  buffer o;
	  if (comp_printlevel >= 7)
	    {
	      o << "choice of spair: ";
	      for (es_pair *q = p; q!= 0; q = q->next)
		{
		  o << "    ";
		  spair_debug_out(o, q);
		}
	    }
	  else
	    o << "sp" << len;
	  emit(o.str());
	}
    }
  if (p->next == 0) return;
  if (is_ideal)
    {
      // See if one is a gcd 1 pair.
      // If so, keep that one.
      if (is_gcd_one_pair(p))
	{
	  es_pair *rest = p->next;
	  p->next = 0;
	  remove_pairs(rest);
	  return;
	}
      for (es_pair *q = p; q->next != 0; q = q->next)
	{
	  if (is_gcd_one_pair(q->next))
	    {
	      es_pair *answer = q->next;
	      q->next = answer->next;
	      remove_pairs(p);
	      p = answer;
	      p->next = 0;
	      n_saved_gcd_choice++;
	      return;
	    }
	}
    }
  es_pair *rest = p->next;
  p->next = 0;
  remove_pairs(rest);
}
void EGB1::choose_unique_pairs(es_pair *&p)
{
  if (p == 0) return;
  es_pair head;
  head.next = 0;
  es_pair *last = &head;
  while (p != 0)
    {
      es_pair *first = p;
      es_pair *q = first;
      while (q->next != 0 && ntuple::lex_compare(nvars ,p->lcm, q->next->lcm) == EQ)
	q = q->next;
      p = q->next;
      q->next = 0;
      choose_nice_pair(first);
      last->next = first;  // Other elements have been removed
      last = first;
    }
  p = head.next;
}
//////////////////
// Pair updates //
//////////////////
bool EGB1::pair_not_needed(es_pair *p, egb_elem *m) const
{
  // Check the criterion: in(m) divides lcm(p).
  // If so: check if lcm(p1,m) == lcm(p)  (if so, return false)
  //        check if lcm(p2,m) == lcm(p)  (if so, return false)
  // If still here, return true.
  if (p->type != SP_SYZ && p->type != SP_RING) return false;
  if (lead_component(p) != lead_component(m)) return false;
  const int *mexp = m->lcm;
  const int *lcm = p->lcm;
  const int *p1exp = p->s.syz.i->lcm;
  const int *p2exp = p->s.syz.j->lcm;  // WRONG FOR others kinds of pairs!!!
  int i;
  for (i=0; i<nvars; i++)
    if (mexp[i] > lcm[i]) return false;
      
  bool firstok = false;
  for (i=0; i<nvars; i++)
    {
      if (mexp[i] == lcm[i]) continue;
      if (p1exp[i] == lcm[i]) continue;
      firstok = true;
      break;
    }
  if (!firstok) return false;
  for (i=0; i<nvars; i++)
    {
      if (mexp[i] == lcm[i]) continue;
      if (p2exp[i] == lcm[i]) continue;
      return true;
    }
  return false;
}

void EGB1::minimalize_pairs(es_pair *&p)
{
  int d;
  if (p == 0) return;
  array<es_pair *> bins;

  // All the pairs in the list 'p' must have the same component.
  // Step 1: Divide up by actual degree of the monomial:

  while (p != 0)
    {
      es_pair *tmp = p;
      p = p->next;
      int d = ntuple::degree(nvars,tmp->lcm);
      if (d >= bins.length())
	{
	  for (int i=bins.length(); i<=d; i++)
	    bins.append(0);
	}
      tmp->next = bins[d];
      bins[d] = tmp;
    }

  // Step 2: 

  for (d=0; bins[d] == 0; d++);
  sort_pairs(bins[d]);
  choose_unique_pairs(bins[d]);
  ESPairLookupTable table(nvars, bins[d]);
  bins[d] = 0;

  for (d++; d<bins.length(); d++)
    {
      es_pair *a, *c;
      es_pair head;
      head.next = bins[d];
      bins[d] = 0;

      // Check divisibility by previous stuff.  If not minimal
      // remove that pair.
      es_pair *b = &head;
      while (b->next != 0)
	if (table.find_divisor(b->next->lcm, c))
	  {
	    // Remove element:
	    es_pair *tmp = b->next;
	    b->next = tmp->next;
	    remove_pair(tmp);
	  }
	else
	  b = b->next;
      a = head.next;

      // Sort the list of remaining elements
      sort_pairs(a);

      // Check for duplicates: Choose one.
      choose_unique_pairs(a);

      // Place onto the list of s-pairs (At the end).
      table.append_list(a);
    }


  // Step 3:
  p = table.value(); // This list of pairs.

}

void EGB1::update_pairs(egb_elem *m)
{
  // Step 1: remove un-needed old pairs
  // NOTE: we don't need to check the elements of the current degree?
  // THIS UPDATE of old pairs uses PRIVATE DATA in 'spairs'.
  es_pair head;
  head.next = spairs->heap;
  es_pair *p = &head;
  while (p->next != 0)
    if (pair_not_needed(p->next, m))
      {
	es_pair *tmp = p->next;
	p->next = tmp->next;
	tmp->next = 0;
	remove_pair(tmp);
	n_saved_lcm++;
	spairs->nelems--;
      }
  else
    p = p->next;
  spairs->heap = head.next;

  // Step 2: find the possible new s-pairs
  es_pair *new_set = 0;
  
  // S-pairs from skew commuting variables
  if (I.ring_is_skew_commutative())
    {
      int nskew = I.exp_skew_vars(m->lcm, mSkewVars);
      for (int v=0; v<nskew; v++)
	{
	  es_pair *s = make_skew_s_pair(m,mSkewVars[v]);
	  s->next = new_set;
	  new_set = s;
	}
    }

  // S-pairs from monomial syzygies involving ring elements.
  if (I.ring_is_quotient())
    {
      for (ERingTable::iterator p = ring_table->first(); p.valid(); ++p)
	{
	  ering_elem *r = *p;
	  es_pair *s = make_ring_s_pair(m, r);
	  s->next = new_set;
	  new_set = s;
	}
    }

  // S-pairs from the vectors themselves.
  int x = lead_component(m);
  for (EGBLookupTable::iterator p = gb[x]->first(); p.valid(); ++p)
    {
      egb_elem *g = *p;
      es_pair *s = make_s_pair(m, g);
      s->next = new_set;
      new_set = s;
    }

  // Step 3: minimalize this set.  Choose a minimal generator in an
  //     intelligent way
  if (comp_printlevel >= 8) debug_pairs("before minimal pairs", new_set);
  minimalize_pairs(new_set);
  if (comp_printlevel >= 8) debug_pairs("new minimal pairs", new_set);

  // Step 4: insert these into the SPairSet, removing gcd=1 pairs if possible
  while (new_set != 0)
    {
      es_pair *tmp = new_set;
      new_set = new_set->next;
      tmp->next = 0;
      if (is_ideal && is_gcd_one_pair(tmp))
	{
	  // can we remove this pair??
	  n_saved_gcd++;
	  remove_pair(tmp);
	}
      else
	{
	  n_pairs++;
	  spairs->insert(tmp);
	}	
    }
}

///////////////
// Reduction //
///////////////


int EGB1::gb_reduce(vector_heap &fh, vector_heap &fsyzh, EVector &f, EVector &fsyz) const
{
  vector_collector freduced = I.start_collection(F);

  ringelement hcoefficient;
  int *hexponents = const_cast<int *&>(mReduceExp);
  int hcomponent = 0;
  const monomial *hmonomial;

  ering_elem *r = 0;
  egb_elem *g = 0;
  while (I.get_lead_term_from_heap(fh,hcoefficient,hmonomial,hcomponent) != 0)
    {
      I.to_exponents(hmonomial, hexponents);
      if (I.ring_is_quotient() && ring_table->find_divisor(hexponents, r))
	{
	  I.ring_cancel_lead_terms(fh,
				   hcoefficient, hexponents, hcomponent,
				   r->lcm, r->f);
	}
      else if (gb[hcomponent]->find_divisor(hexponents, g))
	{
	  I.cancel_lead_terms(fh,fsyzh,
			      hcoefficient,hexponents,
			      g->lcm, g->f, g->fsyz);
	}
      else
	{
	  term t;
	  I.remove_lead_term_from_heap(fh,t);
	  I.append_to_collection(freduced, t);
	}
    }
  fsyz = I.end_heap(fsyzh);
  f = I.end_collection(freduced);
  return true;  // NOT deferred...
}
///////////////////////
// Insertion into GB //
///////////////////////

void EGB1::auto_reduce_by(egb_elem *new_elem)
{
  // The particular strategy to use is unclear,
  // and might depend alot on the kind of problem.
  // Technique 1: Only loop through the elements of the same sugar degree,
  //    and only do K-operations to remove exactly the lead term.
  // Technique 2: Do the same, but on all GB elements of the same sugar degree
  //    (even ones that have been made not-minimal).
  //    problem: it might be that a lead term of a previous element can be reduced.
  //    This should be avoided?
  // Technique 3: Do a full reduction of the elements of the same sugar degree.
  // Technique 4: Do a full reduction.
  // What to do with lead terms?
  
  for (int i=gbLarge.length()-2; i >= 0 && gbLarge[i]->degree == new_elem->degree; --i)
    {
      I.auto_reduce(F,Fsyz,gbLarge[i]->f,gbLarge[i]->fsyz,
		    new_elem->f, new_elem->fsyz);
    }

}

void EGB1::gb_insert(int degree, EVector &f, EVector &fsyz, 
	       bool minimal)
  // Insert the element 'f' as a new element in the GB.
{
  I.make_monic(F,Fsyz,f,fsyz);
  egb_elem *new_elem = make_gb_elem(degree,f,fsyz,minimal);
  if (comp_printlevel >= 8) gb_debug_out(new_elem);
  update_pairs(new_elem);

  array< egb_elem * > nonminimals;
  gbLarge.append(new_elem);

  gb[lead_component(new_elem)]->insert(new_elem, nonminimals);
  n_gb++;

  for (int i=0; i<nonminimals.length(); i++)
    {
      n_gb--;
      nonminimals[i]->me = - nonminimals[i]->me;
      if (nonminimals[i]->npairs == 0) decrement(nonminimals[i]);
    }

  auto_reduce_by(new_elem);
}

void EGB1::collect_syzygy(EVector &fsyz)
{
  if (collect_syz && !I.is_zero_vector(Fsyz,fsyz))
    {
      syz.append(fsyz);
      if (comp_printlevel < 8)
	emit_wrapped(3,"z");
    }
  else
    {
      I.remove_vector(Fsyz,fsyz);
      if (comp_printlevel < 8)
	emit_wrapped(3,"o");
    }
}

void EGB1::s_pair_step(es_pair *p)
{
  EVector f, fsyz;
  vector_heap fh(F);
  vector_heap fsyzh(Fsyz);
  n_computed++;
  bool is_gen = (p->type == SP_GEN);
  int degree = p->degree;
  if (comp_printlevel >= 8) spair_debug_out(p);
  compute_s_pair(p, fh, fsyzh);
  remove_pair(p);
  bool ok = gb_reduce(fh,fsyzh,f,fsyz);
  if (!ok) 
    {
      // call a 'deferred' routine...
      emit_wrapped(3,"d"); // deferred
    }
  else if (!I.is_zero_vector(F,f))
    {
      gb_insert(degree,f,fsyz,is_gen);
      if (comp_printlevel < 8)
	emit_wrapped(3,"m");
    }
  else
    collect_syzygy(fsyz);
}

int EGB1::is_computation_complete(const EStopConditions &stop) const
  // Test whether the current computation is done.
{
  if (stop.gb_limit > 0 && n_gb >= stop.gb_limit) return COMP_DONE_GB_LIMIT;
  if (stop.syz_limit > 0 && syz.length() >= stop.syz_limit) return COMP_DONE_SYZ_LIMIT;
  if (stop.pair_limit > 0 && n_computed >= stop.pair_limit) return COMP_DONE_PAIR_LIMIT;
  if (stop.subring_limit > 0 && n_subring >= stop.subring_limit) return COMP_DONE_SUBRING_LIMIT;
  return COMP_COMPUTING;
}

int EGB1::calc(const int *deg, const intarray &stop)
{
  if (stop.length() != 7) 
    {
      gError << "inappropriate stop conditions for GB computation";
      return COMP_ERROR;
    }
  EStopConditions s;
  if (deg == 0)
    s.degree = false;
  else {
    s.degree = true;
    s.degree_limit = *deg;
  }
  s.gb_limit = stop[0]; //ngb
  s.syz_limit = stop[1]; //nsyz
  s.pair_limit = stop[2]; //npairs
  s.subring_limit = stop[5]; //subring limit
  s.mingens_limit = -1;
  s.codim = false;
  return new_calc(s);
}

int EGB1::new_calc(const EStopConditions &stop)
{
  int is_done = COMP_COMPUTING;
  
  for (;;)
    {
      system_spincursor();
      if (system_interrupted) 
	{
	  is_done = COMP_INTERRUPTED;
	  break;
	}

      is_done = is_computation_complete(stop);
      if (is_done != COMP_COMPUTING) break;

      if (this_set == 0)
	{
	  // This would be the place to auto-reduce the old guys?
	  // Also: where to sort the GB?
	  // Also: if delayed spair finding: do it here.
	  int npairs = spairs->get_next_degree(this_degree, this_set);
	  sort_pairs(this_set);
	  if (this_set == 0)
	    {
	      is_done = COMP_DONE;
	      break;
	    }
	  if (stop.degree && this_degree > stop.degree_limit)
	    {
	      is_done = COMP_DONE_DEGREE_LIMIT;
	      break;
	    }
	  if (comp_printlevel >= 1)
	    {
	      buffer o;
	      o << '{' << this_degree << '}';
	      o << '(';
	      if (use_hilb_function) 
		o << n_in_degree << ',';
	      o << npairs << ',' << spairs->n_elems_left() << ')';
	      if (comp_printlevel >= 8) o << newline;
	      emit(o.str());
	    }
	}
      
      es_pair *p = this_set;
      this_set = this_set->next;
      s_pair_step(p);
    }
  
  // MES: complete the reduction of the GB here
  if (comp_printlevel >= 1) emit_line("");
  if (comp_printlevel >= 4)
    {
      buffer o;
      o << "Number of min gb elements   = " << n_gb << newline;
      o << "Number of all gb elements   = " << gbLarge.length() << newline;
      o << "Number of pairs             = " << n_pairs << newline;
      o << "Number of gcd=1 pairs       = " << n_saved_gcd << newline;
      o << "Number of gcd=1 pairs chosen= " << n_saved_gcd_choice << newline;
      o << "Number of gcd tails=1 pairs = " << n_saved_lcm << newline;
      o << "Number of pairs computed    = " << n_computed << newline;
      emit(o.str());
    }
  return is_done;
}

////////////////////////
// Interface routines //
////////////////////////

EGB1::EGB1(const Matrix &m, int csyz, int nsyz, int strat)
  : gb_comp(13), I(m.get_ring())
{
  set_up(m, csyz, nsyz, strat);
}

void EGB1::moreGenerators(int lo, int hi, const Matrix &m)
{
  for (int i=hi; i>=lo; i--)
    {
      es_pair *p = make_gen_pair(i, m[i]);  // MES MES: use what 'i' here??
      if (p != 0)
	spairs->insert(p);
      n_pairs++;
    }
}

EGB1::~EGB1()
{
  // remove any remaining s-pairs
  while (this_set != 0)
    {
      es_pair *tmp = this_set;
      this_set = tmp->next;
      remove_pair(tmp);
    }
  while (spairs->heap != 0)
    {
      es_pair *tmp = spairs->heap;
      spairs->heap = tmp->next;
      remove_pair(tmp);
    }
  delete spairs;

  // remove the gb_elem's
  for (int i=0; i < gb.length(); i++)
    delete gb[i];

  for (int i=0; i < gbLarge.length(); i++)
    {
      remove_exponent_vector(const_cast<int *&>(gbLarge[i]->lcm));
      I.remove_vector(F,gbLarge[i]->f);
      I.remove_vector(Fsyz,gbLarge[i]->fsyz);
      delete gbLarge[i];
    }

  // Remove the syzygies
  for (int i=0; i<syz.length(); i++)
    I.remove_vector(Fsyz,syz[i]);

  // Finally, decrement ref counts
  bump_down(F);
  bump_down(Fsyz);
}

//////////////////////////////////////
// Old (gb_comp) interface routines //
//////////////////////////////////////

void EGB1::gb_reduce(EVector &f, EVector &fsyz) const
{
  vector_heap fh(F);
  vector_heap fsyzh(Fsyz);

  fh.add(f);
  fsyzh.add(fsyz);
  gb_reduce(fh,fsyzh,f,fsyz);
}

Matrix EGB1::reduce(const Matrix &m, Matrix &lift)
{
  Matrix red(m.rows(), m.cols());
  lift = Matrix(Fsyz, m.cols());
  vector_heap fh(F);
  vector_heap fsyzh(Fsyz);

  for (int i=0; i<m.n_cols(); i++)
    {
      vec f = I.copy_vector(F,m[i]);
      vec fsyz = NULL;
      gb_reduce(f, fsyz);
      Fsyz->negate_to(fsyz);
      red[i] = f;
      lift[i] = fsyz;
    }
  return red;
}

Vector EGB1::reduce(const Vector &v, Vector &lift)
{
  if (!v.free_of()->is_equal(F))
    {
      gError << "reduce: vector is in incorrect free module";
      return Vector(F, NULL);
    }
  vec f = I.copy_vector(F,v.get_value());
  vec fsyz = NULL;

  gb_reduce(f, fsyz);
  Fsyz->negate_to(fsyz);

  lift = Vector(Fsyz, fsyz);
  return Vector(F, f);
}

int EGB1::contains(const Matrix &m)
  // Return -1 if every column of 'm' reduces to zero.
  // Otherwise return the index of the first column that
  // does not reduce to zero.
{
  // Reduce each column of m one by one.
  int result = -1;
  for (int i=0; result == -1 && i<m.n_cols(); i++)
    {
      vec f = F->translate(m.rows(),m[i]);
      vec fsyz = NULL;
      gb_reduce(f, fsyz);
      I.remove_vector(Fsyz,fsyz);
      result = i;
      if (!I.is_zero_vector(F,f))
	result = i;
      I.remove_vector(F,f);
    }
  return -1;
}
bool EGB1::is_equal(const gb_comp * /*q*/)
{
  gError << "== not yet implemented for inhomogeneous GB's";
  return false;
}

//--- Obtaining matrices as output -------
Matrix EGB1::min_gens_matrix()
{
  array< vec > columns;
  for (iterator i = first(); i.valid(); ++i)
    {
      egb_elem *q = *i;
      if (q->is_minimal)
	columns.append(I.copy_vector(F,q->f));
    }
  return I.make_matrix(F, columns);
}

Matrix EGB1::initial_matrix(int n)
{
  array< vec > columns;
  for (iterator i = first(); i.valid(); ++i)
    {
      egb_elem *q = *i;
      columns.append(F->lead_term(n,q->f));
    }
  return I.make_matrix(F, columns);
}

Matrix EGB1::gb_matrix()
{
  array< vec > columns;
  for (iterator i = first(); i.valid(); ++i)
    {
      egb_elem *q = *i;
      columns.append(I.copy_vector(F,q->f));
    }
  return I.make_matrix(F, columns);
}

Matrix EGB1::change_matrix()
{
  array< vec > columns;
  for (iterator i = first(); i.valid(); ++i)
    {
      egb_elem *q = *i;
      columns.append(I.copy_vector(Fsyz,q->fsyz));
    }
  return I.make_matrix(Fsyz, columns);
}

Matrix EGB1::syz_matrix()
{
  return I.make_matrix(Fsyz, syz);
}

#if 0
//// NEW INTERFACE ROUTINES
EVector EGB1::reduceVector(const EVector &v) const
{
  if (!v.getFreeModule()->isEqual(F))
    {
      gError << "reduce: vector is in incorrect free module";
      return F->zero();
    }
  EVector f = v.clone();
  Evector fsyz = Fsyz->zero();

  gb_reduce(f, fsyz);
  // Hopefully fsyz is just removed automatically 
  return f;
}

EVector EGB1::reduceVector(const EVector &v, EVector &fsyz) const
{
  if (!v.getFreeModule()->isEqual(F))
    {
      gError << "reduce: vector is in incorrect free module";
      return F->zero();
    }
  EVector f = v.clone();
  fsyz = Fsyz->zero();

  gb_reduce(f, fsyz);
  fsyz.negateTo();

  return f;
}

EMatrix *EGB1::getGenerators() const
{
  EMutableMatrix *m = EMutableMatrix::make(F);
  for (egb_elem *q = gb->next_min; q != NULL; q = q->next_min)
    if (q->is_minimal)
      m->appendColumn(q->f.clone());
  EMatrix *result = m->sort()->toMatrix();
  delete m;
  return result;
}

EMatrix *getGB() const
{
  EMutableMatrix *m = EMutableMatrix::make(F);
  for (egb_elem *q = gb->next_min; q != NULL; q = q->next_min)
    m->appendColumn(q->f.clone());
  EMatrix *result = m->sort()->toMatrix();
  delete m;
  return result;
}

EMatrix *getChangeOfBasis() const
{
  EMutableMatrix *m = EMutableMatrix::make(Fsyz);
  for (egb_elem *q = gb->next_min; q != NULL; q = q->next_min)
    m->appendColumn(q->fsyz.clone());
  EMatrix *result = m->sort()->toMatrix();
  delete m;
  return result;
}

EMatrix *getSyzygies() const
{
  return syz->toMatrix();  // Resets the matrix back to 0 columns
}

EMatrix *getLeadTerms(int n) const
{
  EMutableMatrix *m = EMutableMatrix::make(F);
  for (egb_elem *q = gb->next_min; q != NULL; q = q->next_min)
    m->appendColumn(q->f.leadTerm(n));
  EMatrix *result = m->sort()->toMatrix();
  delete m;
  return result;
}

EMatrix *getSubring(int n) const
{
  EMutableMatrix *m = EMutableMatrix::make(F);
  for (egb_elem *q = gb->next_min; q != NULL; q = q->next_min)
    if (q->min_in_subring)
    m->appendColumn(q->f.clone());
  EMatrix *result = m->sort()->toMatrix();
  delete m;
  return result;
}

EMatrix *getSubringGB(int n) const
{
  EMutableMatrix *m = EMutableMatrix::make(F);
  for (egb_elem *q = gb->next_min; q != NULL; q = q->next_min)
    if (q->in_subring)
    m->appendColumn(q->f.clone());
  EMatrix *result = m->sort()->toMatrix();
  delete m;
  return result;
}
#endif

////////////////////
// Debugging code //
////////////////////
void EGB1::spair_debug_out(es_pair *q) const
{
  buffer o;
  spair_debug_out(o,q);
  emit(o.str());
}

void EGB1::spair_debug_out(buffer &o, es_pair *q) const
{
  o << "--- spair ";
  o << "deg " << q->degree;
  o << " lcm "; I.display_exponents(o,q->lcm);
  switch (q->type) {
  case SP_GEN:
    o << " gen ";
    I.display_vector(o,F,q->s.gen.f);
    break;
  case SP_SYZ:
    o << " syz ";
    o << q->s.syz.i->me << " " << q->s.syz.j->me;
    break;
  case SP_SKEW:
    o << " skew "<< q->s.ringsyz.i->me;
    break;
  case SP_RING:
    o << " ring "<< q->s.ringsyz.i->me;
    break;
  }
  o << newline;
}

void EGB1::gb_debug_out(egb_elem *p) const
{
  buffer o;
  gb_debug_out(o,p);
  emit(o.str());
}
void EGB1::gb_debug_out(buffer &o, egb_elem *p) const
{
  o << "--- gb[" << p->me << "] = " ;
  o << (p->is_minimal ? "minimal" : "nonminimal");
  o << " degree " << p->degree;
  o << " vec "; I.display_vector(o,F,p->f);
  o << newline;
}

void EGB1::debug_pairs(char *heading, es_pair *qlist) const
{
  buffer o;
  debug_pairs(o,heading,qlist);
  emit(o.str());
}
void EGB1::debug_pairs(buffer &o, char *heading, es_pair *qlist) const
{
  o << heading << newline;
  while (qlist != 0)
    {
      o << "\t"; spair_debug_out(o,qlist);
      qlist = qlist->next;
    }
}
void EGB1::stats() const
{
  spairs->stats();
  buffer o;
  if (comp_printlevel >= 5 && comp_printlevel % 2 == 1)
    {
      for (iterator p = first(); p.valid(); ++p)
	gb_debug_out(o,*p);
    }
  emit(o.str());
}

#if 0
/// MES: new linear algebra code.

class LinearAlgebraGBMatrix
{
  struct column_info {
    const int *monom;
    int component;
  };
  struct row_info {
    const int *monom;
    int component;
    int who_divides_me;
    int compare_value;
  };

  vector< row_info > cols;
  vector< column_info > rows;

  // Also need a hash table of monomials, which links into 'rows'.

  SparseMutableMatrix mat;

  int nextrow;  // Next one to process
  int nextcol;  // Next one to process

public:
  void append_row(int *monom, int component);
  // Only appends this row if it is not already there.

  void append_column(int *monom, int component);
  // Doesn't bother to check whether the column is here.

  void process_next_row();
  // Takes the next row, determines who divides it, and appends the appropriate
  // column.

  void process_next_column();
  // Takes the next column, performs the multiplication, and creates the
  // sparse_vector, add it to 'mat'.  In the process, many new rows might be
  // appended.

public:
  // Create the matrix, given a set of spairs, and a list of GB elements.

  // LU Decompose the matrix

  // Read off the new GB elements

  // Read off the syzygies
};

void LinearAlgebraGBMatrix::append_row(int *monom, int component)
{
  row_info r;
  r.monom = monom;
  r.component = component;
  if (MT[component]->find_divisor(monom, b))
    r.who_divides_me = b->tag();
  else
    r.who_divides_me = -1;
  r.compare_value = 0;
  rows.push_back(r);
}
void LinearAlgebraGBMatrix::append_column(int *monom, int component)
{
  col_info c;
  c.monom = monom;
  c.component = component;
}
void LinearAlgebraGBMatrix::process_next_row()
{
  if (++next_row == rows.size())
    return;
  row_info &r = rows[next_row];
  if (r.who_divides_me < 0)
    return;
  I.divide_monomials(r.monom
  
}

#endif


#if 0

int EGB1::compare(const gb_elem *p, const gb_elem *q) const
{
  int cmp = M->compare(p->f->monom, q->f->monom);
  if (cmp == -1) return LT;
  if (cmp == 1) return GT;
  cmp = p->f->comp - q->f->comp;
  if (cmp < 0) return LT;
  if (cmp > 0) return GT;
  return EQ;
}

int EGB1::search(const int *exp, int comp, gb_elem *&result)
{
  int nvars = M->n_vars();
  int *exp2;
  for (gb_elem *p = gbLarge->next; p != NULL; p = p->next)
    {
      if (p->f->comp != comp) continue;
      exp2 = p->lead_exp;
      int is_div = 1;
      for (int i=0; i<nvars; i++)
	if (exp2[i] > exp[i])
	  {
	    is_div = 0;
	    break;
	  }
      if (is_div)
	{
	  result = p;
	  return 1;
	}
    }
  return 0;
}

#endif

#if 0
  queue<SOMETHING *> elems;

  int my_gb_num = p->me;
  const monomial *m = p->f->monom;
  if (Rskew != 0)
    {
      int nskew = Rskew->skew_vars(m, mSkewvars);
      intarray exp;
      exp.append(0);
      exp.append(1);
      for (int v=0; v<nskew; v++)
	{
	  exp[0] = mSkewvars[v];
	  const monomial *m1 = M->monomial_from_variable_exponent_pairs(exp);

	  evec *vsyz = Gsyz->make_term(my_gb_num, one, m1);
	  // The following is the sugar degree:
	  elems.insert(make_spair(vsyz, p->degree + R->degree(w)));
	}
    }
  
  if (R->isQuotientRing())
    {
      for (j=0; j<R->get_quotient_length(); j++)
	{
	  ERingElement &f = R->get_quotient_element(j);
	  M->monomialSyzygy(f->monom, m, mon2, mon1);
	  ERingElement g1 = K->one();
	  ERingElement g2 = K->one();
	  if (gb_monic)
	    {
	    }
	  else
	    {
	    }
	  evec *gsyz = Gsyz->make_term(my_gb_num, g1, mon1);
	  gsyz->next = Gsyz->make_term(-j-1, g2, mon2);
	  elems.insert(make_spair(gsyz, p->degree + M->degree(mon1)));
	}
    }

  for (j=0; j<me; j++)
    {
      // Only consider those with the same component as me

      M->monomialSyzygy(f->monom, m, mon2, mon1);
      ERingElement g1 = K->one();
      ERingElement g2 = K->one();  // NEGATE!!!!
      if (gb_monic)
	{
	}
      else
	{
	}
      evec *gsyz = Gsyz->make_term(my_gb_num, g1, mon1);
      gsyz->next = Gsyz->make_term(other_gb_num, g2, mon2);
      elems.insert(make_spair(gsyz, p->degree + M->degree(mon1)));
    }

  // At this point we have made a generating set (awful, to be sure)
  // of all of the syzygies on the lead terms.

  // Now we need to minimalize.

  // What about removing all those bad old elements??
}
int EGB1::gb_reduce_ZZ(EVectorHeap &f, EVectorHeap &fsyz)
{
  EVector::collector result(F);
  EVectorHeap fb(F);
  EVectorHeap fsyzb(Fsyz);
  fb.add(f);
  fsyzb.add(fsyz);

  const evec *lead;
  bool ret = true;
  int count = 0;
  while ((lead = fb.getLeadTerm()) != 0)
    {
      bool reduces = search(lead,gsyz);
      if (gsyz != 0)
	{
	  // Reduce by the GB, and by ring elements
	  apply_gb_elements(fb,fsyzb,gsyz);
	  gsyz.remove();
	}
      if (!reduces)
	{
	  ret = false;
	  evec *t = f.removeLeadTerm();
	  result.append(t);
	}
      else
	count++;
    }
  fsyz = fsyzb.getValue();
  f = fb.getValue();
  return ret;
}
void EGB1::apply_gb_elements(EVectorHeap &f, EVectorHeap &fsyz, evec *gsyz) const
{
  // f in F
  // fsyz in Fsyz
  // gsyz in Gsyz
  // Modify f, fsyz using the GB elements in gsyz.
  //int *s = M->make_one();
  for (evec *t = gsyz; t != NULL; t = t->next)
    {
      int r = t->comp;
      if (r >= 0)
	{
	  // Reduce by a GB element
	  egb_elem *g = gb[r];
	  ERingElement a = K->negate(t->coeff);
	  evec *f1 = F->getCover()->mult_by_term(a,t->monom,g->f);
	  evec *fsyz1 = Fsyz->getCover()->mult_by_term(a,t->monom,g->fsyz);
	  if (R->isQuotientRing())
	    Fsyz->normal_form(fsyz1);
	  f.add(f1);
	  fsyz.add(fsyz1);
	  K->remove(a);
	}
      else
	{
	  // Reduce by a ring element
	  r = -r-1;  // Indices start at 0.
	  const ERingElement &h = R->getQuotientPolynomial(r);
	  ERingElement a = K->negate(t->coeff);
	  evec *f1 = F->getCover()->ring_mult_by_term(a,t->monom,fcomp,h);
	  f.add(f1);
	  K->remove(a);
	  
	}
    }
}

#endif
