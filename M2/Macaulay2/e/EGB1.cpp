// Copyright 1999  Michael E. Stillman

#include "style.hpp"
#include "EGB1.hpp"
#include "matrix.hpp"
#include "text_io.hpp"
extern char system_interrupted;
extern int comp_printlevel;

stash *EGB1::mystash;

////////////////////////////////
// EMonomialLinearLookupTable //
////////////////////////////////
EMonomialLinearLookupTable::EMonomialLinearLookupTable()
{
  // What to set here??
}
int EMonomialLinearLookupTable::monomial_mask(const int *exp) const
{
  int result = 0;
  int i,j;
  for (i=0, j=0; i<nvars; i++, j++)
    {
      if (j == 8*sizeof(int)) j=0;
      if (exp[i] > 0)
	result |= (1 << j);
    }
  return result;
}

int EMonomialLinearLookupTable::exp_divides(const int *e, const int *f) const
{
  for (int i=0; i<nvars; i++)
    if (e[i] > f[i]) return 0;
  return 1;
}

int EMonomialLinearLookupTable::compare(node *p, node *q) const
{
  return EQ;
}

void EMonomialLinearLookupTable::insert(egb_elem *g, const int *exponents,
					array<egb_elem *> &nonminimals)
{
  // 'table' is sorted in increasing monomial order (actual degree first)
  // So: search to the point where 'p' should be inserted, then remove elements
  // past that which are divisible by 'p'.

  node *t = new node;
  t->next = 0;
  t->elem = g;
  t->exponents = exponents;
  t->mask = monomial_mask(t->exponents);
  // MES  t->degree = actual_degree(t->exponents);
  node *p;
  node head;
  head.next = table;
  for (p = &head; p->next != 0; p = p->next)
    {
      int cmp = compare(p, t);
      if (cmp == LT) continue;
      // note that cmp == EQ should NOT happen
      break;
    }
  t->next = p->next;
  p->next = t;
  table = head.next;

  // Now we must check and remove those which are divisible by 't'.
  for (p = t; p->next != 0; p = p->next)
    {
      if (exp_divides(t->exponents, p->next->exponents))
	{
	  node *tmp = p->next;
	  p->next = tmp->next;
	  tmp->next = 0;
	  nonminimals.append(tmp->elem);
	  tmp->elem = 0;
	  delete tmp;
	}
    }
}

bool EMonomialLinearLookupTable::find_divisor(
	      const int *exp,
	      egb_elem * & result) const
{
  int expmask = ~(monomial_mask(exp));

  // Need to consider ring elements here?

  for (node *p = table; p != 0; p = p->next)
    if ((expmask & p->mask) == 0)
      {
	for (int i=0; i<nvars; i++)
	  if (exp_divides(p->exponents, exp))
	    {
	      result = p->elem;
	      return true;
	    }
      }
  return false;
}
bool EMonomialLinearLookupTable::find_all_divisors(const int *exp, 
						   array<egb_elem *> &result) const
  // Return value: true means that a monomial divisor with lead coeff '1' was found.
{
  int expmask = ~(monomial_mask(exp));

  // Need to consider ring elements here?

  for (node *p = table; p != 0; p = p->next)
    if ((expmask & p->mask) == 0)
      {
	for (int i=0; i<nvars; i++)
	  if (exp_divides(p->exponents, exp))
	    result.append(p->elem);
      }
  return (result.length() > 0);
}
///////////////////////////////////////////////////////////////


bool EGB1::set_up(const Matrix &m, int csyz, int nsyz, int strat)
{
  // Returns false if an error is found.  That is, if the ring of 'm' is not
  // appropriate.
  
  R = m.Ring_of()->cast_to_PolynomialRing();
  if (R == NULL)
    {
      gError << "ring is not a polynomial ring";
      return false;
    }
  M = m.Ring_of()->Nmonoms();

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
    {
      gb[i].gb = 0;
      gb[i].mi = new EMonomialLookupTable();
    }
  moreGenerators(0, ncols - 1, m);

  strategy = strat;

  is_ideal = (nrows == 1 && !csyz);

  n_gb = n_subring = 0;
  n_pairs = n_computed = 0;
  n_saved_gcd = n_saved_lcm = 0;

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
  if (p->npairs > 0 || p->is_min_GB) return;
  F->remove(p->f);
  Fsyz->remove(p->fsyz);
}

egb_elem *EGB1::make_gb_elem(int degree,
			     EVector &f, 
			     EVector &fsyz,
			     bool maybe_minimal, 
			     bool maybe_subring_minimal) const
{
  egb_elem *result = new egb_elem;
  result->degree = degree;  // This may be the actual degree of 'f' or the 'sugar'.
  result->f = f;
  result->fsyz = fsyz;
  result->npairs = 0;
  int *e = new_exponent_vector();
  M->to_expvector(f->monom, e);
  result->lcm = e;
  // Set other fields MES
  return result;
}

es_pair *EGB1::make_ring_s_pair(egb_elem *p, int j) const
{
  es_pair *result = new es_pair;
  result->next = NULL;
  result->type = SP_RING;
  result->s.ringsyz.i = p;
  result->s.ringsyz.j = j;
  increment(p);

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

  return result;
}

es_pair *EGB1::make_gen_pair(int i, const EVector &f)
{
  vec fsyz;

  if (i < n_comps_per_syz)
    fsyz = Fsyz->e_sub_i(i);
  else
    fsyz = Fsyz->zero();

  if (F->is_zero(f))
    {
      collect_syzygy(fsyz);
      return 0;
    }

  es_pair *result = new es_pair;
  result->next = NULL;
  result->type = SP_GEN;
  result->s.gen.f = F->copy(f);
  result->s.gen.fsyz = fsyz;

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
    F->remove(p->s.gen.f);
    Fsyz->remove(p->s.gen.fsyz);
    break;
  }

  delete p;
  p = NULL;
}

void EGB1::compute_s_pair(es_pair *p, EVectorHeap &f, EVectorHeap &fsyz)
{
  switch (p->type)
    {
    case SP_GEN:
      f.add(p->s.gen.f);
      fsyz.add(p->s.gen.fsyz);
      break;
    case SP_SYZ:
      // WRITE
      break;
    case SP_SKEW:
      // WRITE
      break;
    case SP_RING:
      // WRITE
      break;
    };
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
  if (lead_component(p) != lead_component(m)) return false;
  const int *mexp = m->lcm;
  const int *lcm = p->lcm;
  const int *p1exp = p->s.syz.i->lcm;
  const int *p2exp = p->s.syz.j->lcm;  // WRONG FOR others kinds of pairs!!!
  int i;
  int nvars = M->n_vars();
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

void EGB1::minimalize_pairs(es_pair *p) const
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
      // MES int d = actual_degree(tmp);
      d = -124124124;
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
  ESPairLookupTable table(bins[d]);
  bins[d] = 0;

  for (d++; d<bins.length(); d++)
    {
      es_pair *a, *c;
      es_pair head;
      head.next = bins[d];
      bins[d] = 0;

      // Check divisibility by previous stuff.  If not minimal
      // remove that pair.
      for (es_pair *b = &head; b->next != 0; b = b->next)
	if (table.find_divisor(b->next->lcm, c))
	  {
	    // Remove element:
	    es_pair *tmp = b->next;
	    b->next = tmp->next;
	    remove_pair(tmp);
	  }
      a = head.next;

      // Sort the list of remaining elements
      sort_pairs(a);

      // Check for duplicates: Choose one.
      choose_unique_pairs(a);

      // Place onto the list of s-pairs (At the end).
      table.append(a);
    }

  // Step 3:
  p = table.getList(); // This list of pairs.
  
}

void EGB1::update_pairs(egb_elem *m)
{
  // Step 1: remove un-needed old pairs
  // NOTE: we don't need to check the elements of the current degree?
  es_pair head;
  head.next = spairs->heap;
  for (es_pair *p = &head; p->next != 0; p = p->next)
    if (pair_not_needed(p->next, m))
      {
	es_pair *tmp = p->next;
	p->next = tmp->next;
	tmp->next = 0;
	remove_pair(tmp);
	n_saved_lcm++;
      }
  spairs->heap = head.next;

  // Step 2: find the possible new s-pairs
  es_pair *new_set = 0;
  
  // S-pairs from skew commuting variables
#if 0
  if (Rskew != 0)
    {
      int nskew = Rskew->skew_vars(m_monom, mSkewVars);
      for (int v=0; v<nskew; v++)
	{
	  es_pair *s = make_skew_s_pair(m,mSkewVars[v]);
	  s->next = new_set;
	  new_set = s;
	}
    }
#endif

  // S-pairs from monomial syzygies involving ring elements.
#if 0
  if (Rcover != 0)
    {
      for (int i=0; i < R->n_quotient(); i++)
	{
	  es_pair *s = make_ring_s_pair(m,i);
	  s->next = new_set;
	  new_set = s;
	}
    }
#endif

  // S-pairs from the vectors themselves.
  int x = lead_component(m);
  for (egb_elem *g = gb[x].mi->first(); g != 0; g = g->next)
    {
      es_pair *s = make_s_pair(m, g);
      s->next = new_set;
      new_set = s;
    }

  // Step 3: minimalize this set.  Choose a minimal generator in an
  //     intelligent way
  minimalize_pairs(new_set);

  // Step 4: insert these into the SPairSet.
  spairs->insert(new_set);
}

///////////////
// Reduction //
///////////////


int EGB1::gb_reduce(EVectorHeap &fh, EVectorHeap &fsyzh, EVector &f, EVector &fsyz)
{
  int *exp;  // INITIALIZE THIS!!
  vecterm head;
  vec inresult;
  inresult = &head;

  vecterm *lead;
  bool ret = true;
  while ((lead = fh.get_lead_term()) != 0)
    {
      M->to_expvector(lead->monom, exp);
      egb_elem *g;
      bool reduces = gb[lead->comp].mi->find_divisor(exp, g);
      if (reduces)
	cancel_lead_term(fh,fsyzh,lead,g);
      else
	{
	  ret = false;
	  vec t = fh.remove_lead_term();
	  inresult->next = t;
	  inresult = inresult->next;
	}
    }
  fsyz = fsyzh.get_value();
  inresult->next = 0;
  f = head.next;
  head.next = 0;
  return ret;
}
///////////////////////
// Insertion into GB //
///////////////////////

void EGB1::make_monic(EVector &f, EVector &fsyz) const
{
  // TO BE WRITTEN
}
void EGB1::auto_reduce_by(egb_elem *new_elem)
{
  // TO BE WRITTEN
}


void EGB1::insert_and_minimalize(egb_elem *new_elem)
{
  array< egb_elem * > nonminimals;
  // Step 1: Place into gbLarge.
  gbLarge.append(new_elem);

  // Step 2: Insert into the gb
  gb[lead_component(new_elem)].mi->insert(new_elem, nonminimals);

  // Step 3: Change all of these nonminimals:
  for (int i=0; i<nonminimals.length(); i++)
    {
      nonminimals[i]->is_min_GB = false;
      if (nonminimals[i]->npairs == 0) decrement(nonminimals[i]);
    }
}

void gb_insert(int degree, EVector &f, EVector &fsyz, 
	       bool maybe_minimal, 
	       bool maybe_subring_minimal)
  // Insert the element 'f' as a new element in the GB.
{
  make_monic(f,fsyz);
  egb_elem *p = make_gb_elem(degree,f,fsyz,maybe_minimal,maybe_subring_minimal);
  update_pairs(p);
  insert_and_minimalize(p);
  auto_reduce_by(p);
}

void EGB1::collect_syzygy(EVector &fsyz)
{
  if (collect_syz && !Fsyz->is_zero(fsyz))
    {
      syz.append(fsyz);
      emit_wrapped(3,"z");
    }
  else
    {
      Fsyz->remove(fsyz);
      emit_wrapped(3,"o");
    }
}

void EGB1::s_pair_step(es_pair *p)
{
  EVector f, fsyz;
  EVectorHeap fh, fsyzh;
  n_computed++;
  bool is_gen = p->is_gen;
  int degree = p->degree;
  compute_s_pair(p, fh, fsyzh);
  remove_pair(p);
  bool ok = gb_reduce(fh,fsyzh,f,fsyz);
  if (!ok) 
    {
      // call a 'deferred' routine...
      emit_wrapped(3,"d"); // deferred
    }
  else if (!F->is_zero(f))
    {
      gb_insert(degree,f,fsyz,is_gen);
      emit_wrapped(3,"m");
    }
  else
    collect_syzygy(fsyz);
}

int EGB1::is_computation_complete(const EStopConditions &stop) const
  // Test whether the current computation is done.
{
  if (stop.gb_limit > 0 && n_gb >= stop.gb_limit) return COMP_DONE_GB_LIMIT;
  if (stop.syz_limit > 0 && syz->n_cols() >= stop.syz_limit) return COMP_DONE_SYZ_LIMIT;
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
  StopConditions s;
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
	  int npairs = spairs->get_pair_set(this_degree, this_set);
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
	      if (use_hilb) 
		o << n_in_degree << ',';
	      o << npairs << ',' << spairs->n_elems_left() << ')';
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
      o << "Number of pairs             = " << n_pairs << newline;
      o << "Number of gb elements       = " << n_gb << newline;
      o << "Number of gcd=1 pairs       = " << n_saved_gcd << newline;
      o << "Number of gcd tails=1 pairs = " << n_saved_lcm << newline;
      o << "Number of pairs computed    = " << n_computed << newline;
      emit(o.str());
    }
  return is_done;
}

////////////////////////
// Interface routines //
////////////////////////

EGB1::EGB1(const Matrix &m, int csyz, int nsyz, int strategy)
{
  set_up(m, csyz, nsyz, strat);
}

bool EGB1::moreGenerators(int lo, int hi, const Matrix &m)
{
  for (int i=hi; i>=lo; i--)
    {
      es_pair *p = new_gen(i, m[i]);  // MES MES: use what 'i' here??
      if (p != 0)
	spairs->insert(p);
      n_pairs++;
    }
}

EGB1::~EGB1()
{
  // remove any remaining s-pairs
  s_pair *p;
  while ((p = spairs->remove()) != NULL)
    remove_pair(p);
  delete spairs;

  // remove the gb_elem's
  while (gbLarge != NULL)
    {
      gb_elem *tmp = gbLarge;
      gbLarge = tmp->next;

      // remove gb_elem fields, and itself
      F->remove(tmp->f);
      Fsyz->remove(tmp->fsyz);
      delete [] tmp->lead_exp;
      delete tmp;
    }

  // Finally, decrement ref counts
  bump_down(F);
  bump_down(Fsyz);
}

//////////////////////////////////////
// Old (gb_comp) interface routines //
//////////////////////////////////////

void EGB1::gb_reduce(EVector &f, EVector &fsyz) const
{
  EVectorHeap fh(F);
  EVectorHeap fsyzh(Fsyz);

  fh.add(f);
  fsyzh.add(fsyz);
  gb_reduce(fh,fsyzh,f,fsyz);
}

Matrix EGB1::reduce(const Matrix &m, Matrix &lift)
{
  Matrix red(m.rows(), m.cols());
  lift = Matrix(Fsyz, m.cols());
  EVectorHeap fh(F);
  EVectorHeap fsyzh(Fsyz);

  for (int i=0; i<m.n_cols(); i++)
    {
      vec f = F->copy(m[i]);
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
  vec f = F->copy(v.get_value());
  vec fsyz = NULL;

  gb_reduce(f, fsyz);
  Fsyz->negate_to(fsyz);

  lift = Vector(Fsyz, fsyz);
  return Vector(F, f);
}

int GBinhom_comp::contains(const Matrix &m)
  // Return -1 if every column of 'm' reduces to zero.
  // Otherwise return the index of the first column that
  // does not reduce to zero.
{
  // Reduce each column of m one by one.
  for (int i=0; i<m.n_cols(); i++)
    {
      vec f = F->translate(m.rows(),m[i]);
      vec fsyz = NULL;
      gb_reduce(f, fsyz);
      Fsyz->remove(fsyz);
      if (f != NULL)
	{
	  F->remove(f);
	  return i;
	}
    }
  return -1;
}
bool GBinhom_comp::is_equal(const gb_comp * /*q*/)
{
  gError << "== not yet implemented for inhomogeneous GB's";
  return false;
}

//--- Obtaining matrices as output -------
Matrix EGB1::min_gens_matrix()
{
  array< EVector > columns;
  for (iterator i = first(); i.valid(); ++i)
    {
      egb_elem *q = *i;
      if (q->is_min)
	columns.append(F->copy(q->f));
    }
  return make_matrix(F, columns);
}

Matrix EGB1::initial_matrix(int n)
{
  array< EVector > columns;
  for (iterator i = first(); i.valid(); ++i)
    {
      egb_elem *q = *i;
      columns.append(F->lead_term(n,q->f));
    }
  return make_matrix(F, columns);
}

Matrix EGB1::gb_matrix()
{
  array< EVector > columns;
  for (iterator i = first(); i.valid(); ++i)
    {
      egb_elem *q = *i;
      columns.append(F-copy(q->f));
    }
  return make_matrix(F, columns);
}

Matrix EGB1::change_matrix()
{
  array< EVector > columns;
  for (iterator i = first(); i.valid(); ++i)
    {
      egb_elem *q = *i;
      columns.append(Fsyz-copy(q->fsyz));
    }
  return make_matrix(Fsyz, columns);
}

Matrix EGB1::syz_matrix()
{
  return make_matrix(Fsyz, syz);
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
    if (q->is_min)
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
void EGB1::debug_out(s_pair *q) const
{
  buffer o;
  debug_out(o,q);
  emit(o.str());
}

void EGB1::debug_out(buffer &o, s_pair *q) const
{
  if (q == NULL) return;
  int *m = M->make_one();
  o << "(";
  if (q->first != NULL) o << q->first->me; else o << ".";
  o << " ";
  if (q->second != NULL) o << q->second->me; else o << ".";
  o << " ";
  if (q->first != NULL)
    {
      M->divide(q->lcm, q->first->f->monom, m);
      M->elem_text_out(o, m);
      o << ' ';
    }
  if (q->second != NULL)
    {
      M->divide(q->lcm, q->second->f->monom, m);
      M->elem_text_out(o, m);
      o << ' ';
    }
  M->elem_text_out(o, q->lcm);
  M->remove(m);
  if (q->compare_num < 0)
    o << " marked";
  o << ") ";
}

void EGB1::debug_pairs_out(gb_elem *p) const
{
  buffer o;
  debug_pairs_out(o,p);
  emit(o.str());
}
void EGB1::debug_pairs_out(buffer &o, gb_elem *p) const
{
  s_pair *q;
  int n = 0;
  for (q = p->pair_list; q != NULL; q = q->next_same)
    {
      debug_out(o,q);
      n++;
      if (n % 10 == 0) o << newline;
    }
  o << newline;
}

void EGB1::debug_pairs() const
{
  buffer o;
  debug_pairs(o);
  emit(o.str());
}
void EGB1::debug_pairs(buffer &o) const
{
  for (gb_elem *p = gbLarge->next; p != NULL; p = p->next)
    debug_pairs_out(o,p);

  for (int i=0; i<NHEAP; i++)
    {
      s_pair *q = spairs->debug_list(i);
      if (q == NULL) continue;
      o << "---- pairs in bin " << i << " -----" << newline;
      int n = 0;
      for ( ; q != NULL; q = q->next)
	{
	  debug_out(o,q);
	  n++;
	  if (n % 10 == 0) o << newline;
	}
      o << newline;
    }
}
void EGB1::stats() const
{
  spairs->stats();
  buffer o;
  if (comp_printlevel >= 5 && comp_printlevel % 2 == 1)
    {
      int i = 0;
      for (gb_elem *q = gb->next_min; q != NULL; q = q->next_min)
	{
	  o << i << '\t';
	  i++;
	  F->elem_text_out(o, q->f);
	  o << newline;
	}
    }
  emit(o.str());
}



#if 0

void EGB1::find_pairs(gb_elem *p)
  // compute min gen set of {m | m lead(p) is in (p1, ..., pr, f1, ..., fs)}
  // (includes cases m * lead(p) = 0).
  // Returns a list of new s_pair's.
{
  queue<Bag *> elems;
  Index<MonomialIdeal> j;
  intarray vplcm;
  s_pair *q;
  int nvars = M->n_vars();
  int *find_pairs_exp = new int[nvars];
  int *find_pairs_lcm = new int[nvars];
  int *find_pairs_mon = new int[nvars];
  int *pi = new int [nvars];
  int *pj = new int [nvars];
  int *pij = new int [nvars];

  if (M->is_skew())
    {
      int *skewvars = new int[M->n_vars()];
      M->to_expvector(p->f->monom, find_pairs_exp);
      int nskew = M->exp_skew_vars(find_pairs_exp, skewvars);
      
      // Add in syzygies arising from exterior variables
      for (int v=0; v < nskew; v++)
	{
	  int w = skewvars[v];

	  find_pairs_exp[w]++;
	  M->from_expvector(find_pairs_exp, find_pairs_lcm);
	  find_pairs_exp[w]--;
	      
	  vplcm.shrink(0);
	  M->to_varpower(find_pairs_lcm, vplcm);
	  s_pair *q = new_ring_pair(p, find_pairs_lcm);
	  elems.insert(new Bag(q, vplcm));
	}
      delete [] skewvars;
    }

  // Add in syzygies arising from a base ring

  if (F->is_quotient_ring)
    for (j = R->Rideal.first(); j.valid(); j++)
      {
	Nterm * f = (Nterm *) R->Rideal[j]->basis_ptr();
	M->lcm(f->monom, p->f->monom, find_pairs_lcm);
	vplcm.shrink(0);
	M->to_varpower(find_pairs_lcm, vplcm);
	q = new_ring_pair(p, find_pairs_lcm);
	elems.insert(new Bag(q, vplcm));
      }

  // Add in syzygies arising as s-pairs
  for (gb_elem *s = gb->next_min; s != NULL; s = s->next_min)
    {
      if (p->f->comp != s->f->comp) continue;
      M->lcm(p->f->monom, s->f->monom, find_pairs_lcm);
      vplcm.shrink(0);
      M->to_varpower(find_pairs_lcm, vplcm);
      q = new_s_pair(p, s, find_pairs_lcm);
      elems.insert(new Bag(q, vplcm));
    }

  // Now minimalize these elements, and insert the minimal ones

  queue<Bag *> rejects;
  Bag *b;
  MonomialIdeal mi(R, elems, rejects);
  while (rejects.remove(b))
    {
      s_pair *q = (s_pair *) b->basis_ptr();
      remove_pair(q);
      delete b;
    }

  s_pair head;
  s_pair *nextsame = &head;
  int len = 0;
  for (j = mi.first(); j.valid(); j++)
    {
      q = (s_pair *) mi[j]->basis_ptr();
      nextsame->next = q;
      nextsame = q;
      len++;
      if (is_ideal && q->syz_type == SP_SYZ)
	{
	  M->gcd(q->first->f->monom, q->second->f->monom, find_pairs_mon);
	  if (M->is_one(find_pairs_mon))
	    {
	      n_saved_gcd++;
	      q->compare_num = -1; // MES: change name of field!!
				    // This means: don't compute spair.
	      if (comp_printlevel >= 8)
		{
		  buffer o;
		  o << "removed pair[" << q->first->me << " " 
		    << q->second->me << "]";
		  emit_line(o.str());
		}
	    }
	}
    }
  n_pairs += len;
  nextsame->next = NULL;
  p->pair_list = head.next;
  spairs->sort_list(p->pair_list);
  if (comp_printlevel >= 8)
    {
      buffer o;
      for (q = p->pair_list; q != NULL; q = q->next)
	{
	  o << "insert ";
	  debug_out(o,q);
	  o << newline;
	}
      emit(o.str());
    }
  for (q = p->pair_list; q != NULL; q = q->next)
    q->next_same = q->next;
  spairs->insert(p->pair_list, len);

  // remove those pairs (i,j) for which gcd(p:i, p:j) = 1
  // and for which (p,i), (p,j) are both in the previous list of add-ons.
  // MES: this does not catch all of the un-necessary pairs...
  // Also much optimization might be able to be done, as far as removing
  // keeping the 'correct' minimal generator of the lcms.

  for (s_pair *s1 = p->pair_list; s1 != NULL; s1 = s1->next_same)
    {    
      if (s1->syz_type != SP_SYZ) continue;
      M->divide(s1->lcm, s1->second->f->monom, pi);
      for (s_pair *t1 = s1->next_same; t1 != NULL; t1 = t1->next_same)
	{
	  if (t1->syz_type != SPAIR_PAIR) continue;
	  M->divide(t1->lcm, t1->second->f->monom, pj);
	  M->gcd(pi, pj, pij);
	  if (M->is_one(pij))
	    {
	      if (mark_pair(s1->second, t1->second))
		{
		  n_saved_lcm++;
		}
	    }
	}
    }

  // Remove the local variables
  delete [] find_pairs_exp;
  delete [] find_pairs_lcm;
  delete [] find_pairs_mon;
  delete [] pi;
  delete [] pj;
  delete [] pij;
}
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
