// Copyright 2004 Michael E. Stillman

#include "intarray.hpp"
#include "polyring.hpp"
#include "polyquotient.hpp"
#include "matrixcon.hpp"
#include "matrix.hpp"
#include "montableZZ.hpp"
#include "montable.hpp"
#include "comp_gb.hpp"

PolyRingQuotient::~PolyRingQuotient()
{
}

#if 0
PolyRingQuotient *PolyRingQuotient::create(const PolyRing *R, 
					   VECTOR(Nterm *) &elems)
  // Grabs 'elems'.  Each element of 'elems' should be in the ring R.
  // They should also form a GB.
{
  PolyRingQuotient *result = new PolyRingQuotient;
  result->initialize_ring(R->charac(),
			  R->get_degree_ring());
  result->R_ = R;

  result->overZZ_ = R->getCoefficients()->is_ZZ();
  if (result->overZZ_)
    result->setQuotientInfo(new QRingInfo_ZZ(R,elems));
  else
    result->setQuotientInfo(new QRingInfo_field_basic(R,elems));

  for (int i=0; i<result->n_quotients(); i++)
    {
      if (!R->is_homogeneous(result->quotient_element(i)))
	{
	  result->setIsGraded(false);
	  break;
	}
    }

  result->zeroV = result->from_int(0);
  result->oneV = result->from_int(1);
  result->minus_oneV = result->from_int(-1);
  
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
  VECTOR(Nterm *) elems;
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
  VECTOR(Nterm *) elems;

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
#endif

void PolyRingQuotient::text_out(buffer &o) const
{
  o << "Quotient ring of ";
  numerR_->text_out(o);
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
  const PolynomialRing *Rg1 = Rg->cast_to_PolynomialRing();
  if (Rg == numerR_ || (Rg1 != 0 && Rg1->getAmbientRing() == numerR_))
    {
      result = f;
      return true;
    }
  return numerR_->PolyRing::lift(Rg,f,result);
}

bool PolyRingQuotient::promote(const Ring *Rf, const ring_elem f, ring_elem &result) const
// f is an element of Rf.  result will be an element in 'this'.
{
  const PolynomialRing *R1 = Rf->cast_to_PolynomialRing();
  if (Rf == numerR_ || (R1 != 0 && numerR_ == R1->getAmbientRing()))
    {
      result = copy(f);
      normal_form(result);
      return true;
    }
  return numerR_->PolyRing::promote(Rf,f,result);
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
#warning "division should mean what for quotient rings?"
  ring_elem a = numerR_->divide(f,g);
  normal_form(a);
  return a;
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
  MatrixConstructor mat(make_FreeModule(1),2);
  mat.set_entry(0,0,a);
  mat.set_entry(0,1,b);
  Matrix *m = mat.to_matrix(); // {a,b}
  M2_arrayint weights = makearrayint(n_vars());
  for (int i=0; i<n_vars(); i++) weights->array[i] = 1;
  GBComputation *G = GBComputation::choose_gb(m, 
					      true, // collect syz
					      -1, // keep all rows
					      weights,
					      false,
					      -1,
					      0,0);
  G->set_stop_conditions(false,
			 false,
			 NULL,
			 -1,
			 1, // syzygy limit
			 -1,
			 -1,
			 -1,
			 false,
			 NULL);
  G->start_computation();
  const Matrix *s = G->get_syzygies();

  // Now extract the two pieces of info
  x = s->elem(0,0);
  y = s->elem(1,0);
  ring_elem c = preferred_associate(x);
  ring_elem x1 = mult(c,x);
  ring_elem y1 = mult(c,y);
  x = x1;
  y = y1;
}

ring_elem PolyRingQuotient::random() const
{
  return ZERO_RINGELEM;
}

ring_elem PolyRingQuotient::eval(const RingMap *map, const ring_elem f,int first_var) const
{
  return numerR_->PolyRing::eval(map, f, first_var);
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
