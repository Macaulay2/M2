// Copyright 2004 Michael E. Stillman

#include "intarray.hpp"
#include "polyring.hpp"
#include "polyquotient.hpp"
#include "matrixcon.hpp"
#include "matrix.hpp"
#include "montableZZ.hpp"
#include "montable.hpp"

PolyRingQuotient::~PolyRingQuotient()
{
}

void PolyRingQuotient::makeQuotientIdeal(const vector<Nterm *, gc_alloc> &quotients)
// This constructs the quotient ideal, and sets the MonomialIdeal.
// A subset of 'quotients' must be a minimal GB, and any non-minimal elements
// should be writable in terms of previous ones.
// IE, in constructing (R/I)/J, the GB elements of J (mod I), together with the
// GB elements of I, form a GB, but the GB elements of I might not be minimal.
// In this case, 'quotients' should be the list of GB elements of J followed by those
// of I.
//
// The ideal may be in a tower of polynomial rings, in which case it needs to be
// a GB over the flattened ring.
{
  Rideal_ = new MonomialIdeal(getAmbientRing());
  ringtable_ = MonomialTable::make(n_vars());
  intarray vp;
  int *exp = newarray(int,n_vars());
  for (int i=0; i<quotients.size(); i++)
    {
      // Make a varpower element.  See if it is in Rideal_.
      // If not, place it into quotient_elements_.

      Nterm *f = quotients[i];
      getMonoid()->to_expvector(f->monom, exp);

      Bag *not_used;

      if (!Rideal_->search_expvector(exp, not_used))
	{
	  // The element is part of a minimal GB
	  int index = n_quotients();
	  gbvector *g = R_->translate_gbvector_from_ringelem(f);
	  appendQuotientElement(f, g);
	  vp.shrink(0);
	  getMonoid()->to_varpower(f->monom, vp);
	  Bag *b = new Bag(index, vp);
	  Rideal_->insert(b);
	  ringtable_->insert(exp, 1, i); // consumes exp
	  exp = newarray(int,n_vars());
	}
    }

  // Now we need to set the homogeniety of this quotient ring.
  for (int i=0; i<n_quotients(); i++)
    {
      if (!R_->is_homogeneous(quotient_element(i)))
	{
	  setIsGraded(false);
	  break;
	}
    }
  deletearray(exp);
}

void PolyRingQuotient::makeQuotientIdealZZ(const vector<Nterm *, gc_alloc> &quotients)
// This constructs the quotient ideal, and sets ringtableZZ.
// A subset of 'quotients' must be a minimal GB, and any non-minimal elements
// should be writable in terms of previous ones.
// IE, in constructing (R/I)/J, the GB elements of J (mod I), together with the
// GB elements of I, form a GB, but the GB elements of I might not be minimal.
// In this case, 'quotients' should be the list of GB elements of J followed by those
// of I.
//
// The ideal may be in a tower of polynomial rings, in which case it needs to be
// a GB over the flattened ring.
{
  ringtableZZ_ = MonomialTableZZ::make(n_vars());
  int *exp = newarray(int,n_vars());
  for (int i=0; i<quotients.size(); i++)
    {
      // Make a varpower element.  See if it is in Rideal_.
      // If not, place it into quotient_elements_.

      Nterm *f = quotients[i];
      getMonoid()->to_expvector(f->monom, exp);

      if (!ringtableZZ_->is_strong_member(MPZ_VAL(f->coeff), exp, 1))
	{
	  // The element is part of a minimal GB
	  // Also, this grabs exp.
	  int index = n_quotients();
	  ringtableZZ_->insert(MPZ_VAL(f->coeff), exp, 1, index);
	  gbvector *g = R_->translate_gbvector_from_ringelem(f);
	  appendQuotientElement(f, g);
	  exp = newarray(int,n_vars());

	  if (f->next == 0 && getMonoid()->is_one(f->monom))
	    {
	      is_ZZ_quotient_ = true;
	      ZZ_quotient_value_ = f->coeff;
	    }
	}
    }

  // Now we need to set the homogeniety of this quotient ring.
  for (int i=0; i<n_quotients(); i++)
    {
      if (!R_->is_homogeneous(quotient_element(i)))
	{
	  setIsGraded(false);
	  break;
	}
    }
  deletearray(exp);
}


PolyRingQuotient *PolyRingQuotient::create(const PolyRing *R, 
					   std::vector<Nterm *, gc_alloc> &elems)
  // Grabs 'elems'.  Each element of 'elems' should be in the ring R.
  // They should also form a GB.
{
  PolyRingQuotient *result = new PolyRingQuotient;
  result->initialize_ring(R->charac(),
			  R->n_vars(),
			  R->total_n_vars(),
			  R->get_degree_ring());
  result->R_ = R;

  result->overZZ_ = R->getCoefficients()->is_ZZ();
  if (result->overZZ_)
    result->makeQuotientIdealZZ(elems);
  else
    result->makeQuotientIdeal(elems);

  result->EXP1_ = newarray(int, R->n_vars());
  result->EXP2_ = newarray(int, R->n_vars());
  result->MONOM1_ = R->getMonoid()->make_one();
  return result;
}


PolyRingQuotient *PolyRingQuotient::create(const PolynomialRing *R, 
					   const Matrix *M)
{
  if (M->get_ring() != R)
    {
      ERROR("quotient elements not in the expected polynomial ring");
      return 0;
    }
  std::vector<Nterm *, gc_alloc> elems;
  for (int i=0; i<R->n_quotients(); i++)
    elems.push_back(R->quotient_element(i));
  for (int i=0; i<M->n_cols(); i++)
    elems.push_back(M->elem(0,i));

  return create(R->getAmbientRing(),elems);
}

PolyRingQuotient *PolyRingQuotient::create(const PolyRing *R, 
					   const PolynomialRing *B)
  // R should be an ambient poly ring
  // B should have: ambient of B is the logical coeff ring of R
  //   i.e. R = A[x], B = A/I
  // return A[x]/I.
{
  std::vector<Nterm *, gc_alloc> elems;

  for (int i=0; i<B->n_quotients(); i++)
    {
      ring_elem f;
      R->promote(B->getAmbientRing(), B->quotient_element(i), f);
      elems.push_back(f);
    }
  return create(R,elems);
}

Matrix * PolyRingQuotient::getPresentation() const
{
  const PolyRing *R = getAmbientRing();

  MatrixConstructor mat(R->make_FreeModule(1), 0);
  for (int i=0; i<n_quotients(); i++)
    mat.append(R->make_vec(0, quotient_element(i)));
  return mat.to_matrix();
}

void PolyRingQuotient::text_out(buffer &o) const
{
  o << "Quotient ring of ";
  R_->text_out(o);
  o << newline << "quotient elements" << newline;
  for (int i=0; i<n_quotients(); i++)
    {
      o << "    "; elem_text_out(o, quotient_element(i)); o << newline;
    }
}

bool PolyRingQuotient::is_unit(ring_elem) const
{
#warning "todo: is_unit"
  return is_field();
  return false;
}

bool PolyRingQuotient::lift(const Ring * Rg, const ring_elem f, ring_elem &result) const
// f is an element of 'this'.  Rg is the desired ring.
{
#warning "lift and promote of quotient poly rings require ambient ring only..."
  if (Rg == R_)
    {
#warning "do we really need to copy f here?"
      result = copy(f);
      return true;
    }
  return R_->PolyRing::lift(Rg,f,result);
}

bool PolyRingQuotient::promote(const Ring *Rf, const ring_elem f, ring_elem &result) const
// f is an element of Rf.  result will be an element in 'this'.
{
  const PolynomialRing *R1 = Rf->cast_to_PolynomialRing();
  if (Rf == R_ || (R1 != 0 && R_ == R1->getAmbientRing()))
    {
      result = copy(f);
      normal_form(result);
      return true;
    }
  return R_->PolyRing::promote(Rf,f,result);
}

void PolyRingQuotient::reduce_lead_term_basic_field(Nterm * &f, const Nterm * g) const
// Assumes that g is monic, and that getCoefficients() is a (basic) field.
{
  getMonoid()->divide(f->monom, g->monom, MONOM1_);
  ring_elem c = getCoefficients()->negate(f->coeff);
  if (is_skew_commutative())
    {
      // We need to determine the sign
      getMonoid()->to_expvector(g->monom, EXP2_);
      getMonoid()->to_expvector(MONOM1_, EXP1_);
      if (R_->getSkewInfo().mult_sign(EXP1_, EXP2_) < 0)
	getCoefficients()->negate_to(c);
    }
  ring_elem g1 = const_cast<Nterm *>(g);
  g1 = R_->mult_by_term(g1,c,MONOM1_);
  ring_elem f1 = f;
  R_->internal_add_to(f1, g1);
  f = f1;
}

void PolyRingQuotient::normal_form_basic_field(ring_elem& f) const
// This handles the case of monic GB over a small field
// It must handle skew multiplication too
{
  Nterm head;
  Nterm *result = &head;
  Nterm *t = f;
  while (t != NULL)
    {
      getMonoid()->to_expvector(t->monom, EXP1_);
      int_bag *b;
      if (Rideal_->search_expvector(EXP1_, b))
	{
	  Nterm *s = quotient_element(b->basis_elem());
	  // Now we must replace t with 
	  // t + c*m*s, where in(t) = in(c*m*s), and c is 1 or -1.
	  reduce_lead_term_basic_field(t, s);
	}
      else
	{
	  result->next = t;
	  t = t->next;
	  result = result->next;
	}
    }
  result->next = NULL;
  f = head.next;
}

bool PolyRingQuotient::reduce_lead_term_ZZ(Nterm * &f, const Nterm * g) const
  // Never multiplies f by anything.  IE before(f), after(f) are equiv. mod g.
  // this should ONLY be used if K is globalZZ.
{
  const ring_elem a = f->coeff;
  const ring_elem b = g->coeff;
  ring_elem u,v,rem;
  rem = globalZZ->remainderAndQuotient(a,b,v);
  if (globalZZ->is_zero(v)) return false;
  v = globalZZ->negate(v);
  bool result = globalZZ->is_zero(rem);
  getMonoid()->divide(f->monom, g->monom, MONOM1_);
  if (R_->is_skew_commutative())
    {
      // We need to determine the sign
      getMonoid()->to_expvector(g->monom, EXP2_);
      getMonoid()->to_expvector(MONOM1_, EXP1_);
      if (R_->getSkewInfo().mult_sign(EXP1_, EXP2_) < 0)
	getCoefficients()->negate_to(v);
    }

  // now mult g to cancel
  ring_elem g1 = const_cast<Nterm *>(g);
  g1 = R_->mult_by_term(g1, v, MONOM1_);
  ring_elem f1 = f;
  R_->internal_add_to(f1,g1);
  f = f1;
  return result;
}

void PolyRingQuotient::normal_form_ZZ(ring_elem& f) const
// This handles the case of monic GB over a small field
// It must handle skew multiplication too
{
  Nterm head;
  Nterm *result = &head;
  Nterm *t = f;
  while (t != NULL)
    {
      getMonoid()->to_expvector(t->monom, EXP1_);
      int w = ringtableZZ_->find_smallest_coeff_divisor(EXP1_, 1);
      if (w >= 0)
	{
	  // reduce lead term as much as possible
	  // If the lead monomial reduces away, continue,
	  //   else tack the monomial onto the result
	  Nterm *g = quotient_element(w);
	  if (reduce_lead_term_ZZ(t,g))
	    continue;
	}
      result->next = t;
      t = t->next;
      result = result->next;
    }
  result->next = NULL;
  f = head.next;
}

void PolyRingQuotient::normal_form(ring_elem &f) const
{
  if (overZZ_)
    normal_form_ZZ(f);
  else
    normal_form_basic_field(f);
}

ring_elem PolyRingQuotient::power(const ring_elem f, mpz_t n) const
{
  return Ring::power(f,n);
}

ring_elem PolyRingQuotient::power(const ring_elem f, int n) const
{
  return Ring::power(f,n);
}

ring_elem PolyRingQuotient::invert(const ring_elem f) const
{
  return ZERO_RINGELEM;
}

ring_elem PolyRingQuotient::divide(const ring_elem f, const ring_elem g) const
{
  return ZERO_RINGELEM;
}

ring_elem PolyRingQuotient::gcd(const ring_elem f, const ring_elem g) const
{
  return ZERO_RINGELEM;
}

ring_elem PolyRingQuotient::gcd_extended(const ring_elem f, const ring_elem g, 
					 ring_elem &u, ring_elem &v) const
{
  return ZERO_RINGELEM;
}

ring_elem PolyRingQuotient::remainder(const ring_elem f, const ring_elem g) const
{
  return ZERO_RINGELEM;
}

ring_elem PolyRingQuotient::quotient(const ring_elem f, const ring_elem g) const
{
  return ZERO_RINGELEM;
}

ring_elem PolyRingQuotient::remainderAndQuotient(const ring_elem f, const ring_elem g, 
						 ring_elem &quot) const
{
  return ZERO_RINGELEM;
}

void PolyRingQuotient::syzygy(const ring_elem a, const ring_elem b,
			      ring_elem &x, ring_elem &y) const
{
}

ring_elem PolyRingQuotient::random() const
{
  return ZERO_RINGELEM;
}

ring_elem PolyRingQuotient::eval(const RingMap *map, const ring_elem f) const
{
  return R_->PolyRing::eval(map, f);
}


#if 0

// This is old (pre-gg-removal) code, that we might want to ressurrect.
void PolyRing::make_RidealZZ(const array<ring_elem> &polys)
{
  // If coefficients_are_ZZ, then
  // this routine sets the fields:
  // _quotient_ideal, RidealZ.

  const PolyRing *S = _base_ring;
  while (S->_base_ring != NULL) S = S->_base_ring;

  _RidealZZ = TermIdeal::make_ring_termideal(S,
			   _base_ring->_quotient_ideal, 
			   polys,
			   _quotient_ideal);
}

// This is part of remainderAndQuotient, for quotient rings.

      else if (false) //(n_vars() == 1 && K_->is_field())
	{
	  // Case 2: There is a quotient ideal, but we have one variable, over
	  //         a field.  In this case, we can use gcd in k[x].
	  //         The ring must be commutative here: skew in one variable isn't
	  //         handled here.
	}
      else if (K_->is_field() || K_->is_ZZ())
	{

// MES Aug 2002: ifdef'ed this section because gb_comp is not back yet
	  // Case 3: There is a quotient ideal.  Here we do a GB computation
	  //         of the ideal (g), and reduce f wrt this ideal.

	  // Create a GB of (g).
	  intarray syzygy_stop_conditions;
	  syzygy_stop_conditions.append(0); // ngb
	  syzygy_stop_conditions.append(0); // nsyz
	  syzygy_stop_conditions.append(0); // npairs
	  syzygy_stop_conditions.append(0);
	  syzygy_stop_conditions.append(0); 
	  syzygy_stop_conditions.append(0); // subring limit
	  syzygy_stop_conditions.append(0);
	  
	  const FreeModule *F = make_FreeModule(1);
	  Matrix *m = new Matrix(F);
	  m->append(F->raw_term(copy(g),0));
	  gb_comp *g = gb_comp::make(m,false,-1,0);
	  g->calc(0, syzygy_stop_conditions);

	  // Reduce f wrt this GB.
	  Vector *v = Vector::make_raw(F,F->raw_term(copy(f),0));
	  Vector *lifted;
	  Vector *red = g->reduce(v,lifted);
	  // Now grab the two polynomials of interest:
	  ring_elem result = F->get_coefficient(red->get_value(),0); // Rermainder
	  quot = lifted->free_of()->get_coefficient(lifted->get_value(),0); // Quotient

	  // Remove the GB.
	  deleteitem(g);
	  return result;
#endif






// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
