// Copyright 2000  Michael E. Stillman

#include "EEGB1.hpp"
#include "text_io.hpp"

/////////////////////////////////////////
// Creation,destruction of a GB object //
/////////////////////////////////////////

EGB1::EGB1(const Matrix &m, int csyz, int nsyz, int strat)
  : gb_comp(13), I(m.get_ring())
{
  set_up(m, csyz, nsyz, strat);
}

bool EGB1::set_up(const Matrix &m, int csyz, int nsyz, int strat)
{
  // Returns false if an error is found.  That is, if the ring of 'm' is not
  // appropriate.
  n_subring = 0;

  nvars = I.n_vars();

  //// Set syzygy information ////
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

  is_ideal = (nrows == 1 && !collect_syz) && (!I.ring_is_weyl_algebra());

  //// Create and initialize the computation ////
  MEM = new EGBMemory(F,Fsyz);
  SPAIRS = new SPairSet(MEM);
  GB = new EGB(MEM,...);

  moreGenerators(0, ncols - 1, m);

  //  strategy = strat;

  //  hilb_tracker = 0;

  //  mSkewVars = MEM->new_exponent_vector();
  //  mReduceExp = MEM->new_exponent_vector();
  return true;
}

EGB1::~EGB1()
{
  // TODO: write this.
  // remove any remaining s-pairs
  delete SPAIRS;
  delete GB;

#if 0  
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
#endif
  // Finally, decrement ref counts
  bump_down(F);
  bump_down(Fsyz);

  delete MEM;
}

void EGB1::insert_generator(int i, EVector f)
{
  // TODO: do we need to find the content, and change the ring, set the denom?
  // Very likely we DO.

  EVector fsyz;

  if (i < n_comps_per_syz)
    fsyz = I.e_sub_i(Fsyz,i);
  else
    fsyz = I.zero_vector(Fsyz);

  if (I.is_zero_vector(F,f))
    {
      collect_syzygy(fsyz);
      return;
    }

  es_pair *p = MEM->make_gen_pair(f,fsyz);
  SPAIRS->insert(p);
}

void EGB1::moreGenerators(int lo, int hi, const Matrix &m)
{
  for (int i=hi; i>=lo; i--)
    insert_generator(i, m[i]);
}

void EGB1::use_Hilbert_function(const RingElement &hf)
{
  hilb_tracker = new HilbertTracker(hf,F);
  update_hilb = false;
}

///////////////////////////////
// Main logic of computation //
///////////////////////////////

int EGB1::is_computation_complete(const EStopConditions &stop) const
  // Test whether the current computation is done.
{
  if (stop.gb_limit > 0 && GB->n_gb_large() >= stop.gb_limit) return COMP_DONE_GB_LIMIT;
  if (stop.syz_limit > 0 && syz->length() >= stop.syz_limit) return COMP_DONE_SYZ_LIMIT;
  if (stop.pair_limit > 0 && SPAIRS->n_computed() >= stop.pair_limit) return COMP_DONE_PAIR_LIMIT;
  if (stop.subring_limit > 0 && n_subring >= stop.subring_limit) return COMP_DONE_SUBRING_LIMIT;
  return COMP_COMPUTING;
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

      if (error())
	{
	  gError << error_message();
	  is_done = COMP_ERROR;
	  break;
	}

      if (SPAIRS->n_pairs_in_degree() == 0)
	{
	  int this_degree;
	  int npairs = SPAIRS->get_next_degree(this_degree);

	  auto_loc.shrink(0);

	  if (npairs == 0)
	    {
	      is_done = COMP_DONE;
	      break;
	    }
	  if (stop.degree && this_degree > stop.degree_limit)
	    {
	      is_done = COMP_DONE_DEGREE_LIMIT;
	      break;
	    }

	  if (hilb_tracker) update_hilb = true;

	  if (comp_printlevel >= 1)
	    {
	      buffer o;
	      o << '{' << this_degree << '}';
	      o << '(';
	      o << npairs << ',' << SPAIRS->n_elems_left() << ')';
	      if (comp_printlevel >= 8) o << newline;
	      emit(o.str());
	    }
	}

      if (update_hilb)
	{
	  if (!hilb_tracker->update(this_degree))
	    break;

	  update_hilb = false;
	  if (comp_printlevel >= 1)
	    {
	      buffer o;
	      o << "[expected " << hilb_tracker->n_left_in_degree() << ']';
	      emit(o.str());
	    }

	  if (hilb_tracker->n_left_in_degree() == 0)
	    SPAIRS->flush_this_degree();
	}

      s_pair_step(SPAIRS->remove_smallest());
    }

  if (is_done)
    {
      // TODO: at this point, we have completed the GB computation proper.
      // Here the minimal GB should be sorted, and completely reduced.
      // What should be kep??
    }
  else
    {
    }

  if (comp_printlevel >= 1) emit_line("");
  if (comp_printlevel >= 4)
    {
      buffer o;
      o << "Number of min gb elements   = " << n_min_gb << newline;
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

///////////////////////////
// Main support routines //
///////////////////////////

void EGB1::s_pair_step(es_pair *p)
{
  compute_s_pair(p);
  bool deferred = gb_reduce(p);
  if (deferred)
    {
      SPAIRS->insert(p);
      emit_wrapped(3,"d"); // deferred
    }
  else if (!F->is_zero(p->f))
    {
      // We have a new GB element...
      gb_reduce_tail(p);
      gb_insert(p);
      emit_wrapped(3,7,"m");
    }
  else
    {
      collect_syzygy(p->syz); // frees p->syz, or grabs it.
      R->remove(p->denom);
      p->syz = 0;
      MEM->remove_pair(p);
    }
  // TODO: what if the lead term just replaces a current one?
  // Is this just another possible return from gb_reduce.
  // TODO: also consider an error return: in case the computation results
  // in a monomial overflow.
}

void EGB1::update_pairs(egb_elem *m)
{
  // Step 1: remove un-needed old pairs
  // NOTE: we don't need to check the elements of the current degree?
  // THIS UPDATE of old pairs uses PRIVATE DATA in 'spairs'.
  SPAIRS->remove_unneeded(m);

  // Step 2: find the possible new s-pairs
  es_pair *new_set = 0;
  
  // S-pairs from skew commuting variables
  if (I.ring_is_skew_commutative())
    {
      int nskew = I.exp_skew_vars(m->lcm, mSkewVars);
      for (int v=0; v<nskew; v++)
	{
	  es_pair *s = MEM->make_skew_s_pair(m,mSkewVars[v]);
	  s->next = new_set;
	  new_set = s;
	}
    }

  // S-pairs from monomial syzygies involving ring elements.
  if (I.ring_is_quotient())
    {
      for (EGB::ring_iterator p = GB; p.valid(), ++p)
	{
	  ering_elem *r = *p;
	  es_pair *s = MEM->make_ring_s_pair(m, r);
	  s->next = new_set;
	  new_set = s;
	}
    }

  // S-pairs from the vectors themselves.
  int x = lead_component(m);
  for (EGB::iterator p = GB->iterate(x); p.valid(), ++p)
    {
      egb_elem *g = *p;
      es_pair *s = MEM->make_s_pair(m, g);
      s->next = new_set;
      new_set = s;
    }

  // Step 3: minimalize this set.  Choose a minimal generator in an
  //     intelligent way

  SPAIRS->insert_only_minimals(new_set); 
    // Any elements ...: sorts, makes unique, remove gcd one elements, if nec.

}

void EGB1::collect_syzygy(EVector &z)
{
  // TODO: p->f, etc might not be the names of these components...
  // p->f is 0.
  // p->fsyz is the syzygy.
  // p->denom is the denominator, if used.
  // Note that we do not need the denominator here...
  if (collect_syz && z != 0)
    {
      Fsyz->remove_content(z);
      syz->eat(z);
      emit_wrapped(3,7,"z");
    }
  else
    {
      emit_wrapped(3,7,"o");
      Fsyz->remove(z);
    }
}

void EGB1::gb_insert(es_pair *&p)
  //int degree, EVector &f, EVector &fsyz, 
  //       bool minimal)
  // Insert the element 'f' as a new element in the GB.
{
  egb_elem *g = p->elem;
  p->elem = 0;
  remove_pair(p);

  g->is_trimmed_gen = (p->type == SP_GEN);
  remove_content(g->f,g->fsyz,g->denom);  // make_monic in the finite field case.

  update_pairs(g);
  GB->insert(g);
  auto_reduce_by(g);

  if (hilb_tracker)
    {
      hilb_tracker->increment(g->f);
      if (hilb->tracker->n_left_in_degree() == 0)
	SPAIRS->flush_this_degree();
    }
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
    case SP_DEFERRED:
    case SP_GEN:
      f.add(p->first()->f());
      fsyz.add(p->first()->fsyz());  // POSSIBLE BUG: can this element have a denominator??
      break;
    case SP_SYZ:
      // First determine the syzygy on the monomial, and coefficients
      g1 = p->element();
      g2 = p->_s._syz;
      I.coefficient_syzygy(I.lead_coefficient(g1->f()),
			   I.lead_coefficient(g2->f()),
			   a, b);
      I.exponent_syzygy(g1->lead_exponents(), g2->lead_exponents(), e1, e2, sign);
      if (sign == 1)
	{
	  ringelement c = I.negate_coefficient(b);
	  I.remove_coefficient(b);
	  b = c;
	}

      I.add_multiple_to(f, a, e1, g1->f());
      I.add_multiple_to(f, b, e2, g2->f());

      if (nsyz >= 1)
	{
	  I.add_multiple_to(fsyz, a, g1->denominator(), e1, g1->fsyz());
	  I.add_multiple_to(fsyz, b, g2->denominator(), e2, g2->fsyz());
	}

      I.remove_coefficient(a);
      I.remove_coefficient(b);
      break;
    case SP_SKEW:
      ntuple::one(nvars, e1);
      e1[p->_s._skewvar] = 1;
      I.add_multiple_to(f, I.one(), e1, p->first()->f());

      if (nsyz >= 1)
	I.add_multiple_to(fsyz, I.one(), p->first()->denominator(), e1, p->first()->fsyz());
      break;
    case SP_RING:
      g1 = p->first();
      r2 = p->_s._ringsyz;
      I.coefficient_syzygy(I.lead_coefficient(g1->f()),
			   I.lead_coefficient_of_polynomial(r2->f()),
			   a, b);
      I.exponent_syzygy(g1->lead_exponents(), r2->lead_exponents(), e1, e2, sign);
      if (sign == 1)
	{
	  ringelement c = I.negate_coefficient(b);
	  I.remove_coefficient(b);
	  b = c;
	}

      I.add_multiple_to(f, a, e1, g1->f());

      I.add_ring_multiple_to(f, b, e2, I.lead_component(g1->f()), r2->f());

      if (nsyz >= 1)
	I.add_multiple_to(fsyz, a, g1->denominator(), e1, g1->fsyz());

      I.remove_coefficient(a);
      I.remove_coefficient(b);
      break;
    };
  remove_exponent_vector(e1);
  remove_exponent_vector(e2);
}


// Possible return values for gb_reduce
//     NF_ZERO
//     NF_NEW_LEAD_MONOMIAL
//     NF_DEFERRED
//     NF_MONOMIAL_OVERFLOW
//
// Possible return values for find_good_divisor
//     FOUND_NONE
//     FOUND_DIVIDES
//     FOUND_TERM_DIVIDES
//     FOUND_RING_DIVIDES
//     FOUND_RING_TERM_DIVIDES
int EGB1::gb_reduce(es_pair *p)
{
  // This routine reduces only the first term of the spolynomial of p.
  // There are several cases:
  // a. f reduces to a new lead monomial
  // b. f reduces to a lead monomial that is same, but the coeff is better (ZZ case only)
  // c. f reduces to 0
  // d. f would reduce, except for the sugar degree.
  //    In this case, we add f to the monomial search tables, possibly add new pairs,
  //    modify p by t^a*orig - (something in the mon table)*monomial
  //    after this, we put p back onto the pair list.

  // This routine also handles the Mora algorithm:
  //   elements are added into the gb-global monomial table, and are not removed.
  //   Question: do we need to keep denominators around??

  while (p->f() != 0)
    {
      int alpha;
      I.to_exponents(I.lead_monomial(F,p->f()), hexponents);
      int red = GB->find_good_divisor(p->f()->lead_component(), 
				  p->f()->coeff,
				  hexponents,
				  egb_elem * & result,  // This value is set
				  int & alpha);	// This value is set.
      if (red == FOUND_NONE) return NF_NEW_LEAD_MONOMIAL;
      if (alpha > 0)
	{
	  GB->insert_non_minimal(p->g); // TODO: pass a copy of this?
	}
      if (red == FOUND_RING_DIVIDES || red == FOUND_RING_TERM_DIVIDES)
	{
	  // TODO: write this.
	  // reduce as a ring...  Use coefficient syzygy?
	  // Ignore fsyz
	}
      else if (red == FOUND_DIVIDES || red == FOUND_TERM_DIVIDES)
	{
	  // TODO: write this.
	  // reduce as a module element. Both f, fsyz.
	  // Don't forget the denominators!
	}
      if (alpha > 0)
	{
	  p->degree() += alpha;
	  SPAIRS->insert(p);
	  return NF_DEFERRED;
	}
    }
  // At this point, f reduced to zero
  return NF_ZERO;
}


void EGB1::auto_reduce_by(egb_elem *new_elem)
{
  for (int i=auto_loc.length()-1; i >= 0; --i)
    {
      I.auto_reduce(F,Fsyz,auto_loc[i]->f,auto_loc[i]->fsyz,
		    new_elem->f, new_elem->fsyz);
    }

  // TODO: consider denominators in fsyz's?

  auto_loc.push_back(new_elem);
}

//////////////////////////////////////
// gb_comp interface routines ////////
//////////////////////////////////////

void EGB1::gb_reduce(EVector &f, EVector &fsyz) const
{
  // WRITE THIS
}

Matrix EGB1::reduce(const Matrix &m, Matrix &lift)
{
  Matrix red(m.rows(), m.cols());
  lift = Matrix(Fsyz, m.cols());
  // WRITE THIS
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
  return syz->value();
}

