// Copyright 2005 Michael E. Stillman

#include "polyQQ.hpp"

PolyQQ::~PolyQQ()
{
}
PolyQQ *PolyQQ::create(const PolyRing *P)
{
  PolyQQ *R = new PolyQQ;

  R->initialize_ring(P->charac(),
		     P->get_degree_ring());

  R->initialize_PolynomialRing(P->getCoefficients(),
			       P->getMonoid(),
			       P,
			       R,
			       globalZZ);

  R->zeroV = R->from_int(0);
  R->oneV = R->from_int(1);
  R->minus_oneV = R->from_int(-1);

  R->gb_ring_ = P->get_gb_ring();

  return R;
}

#if 0
PolyQQ *PolyQQ::create_quotient(const PolyQQ *P, const Matrix *I)
  // I should be be a one row matrix, and a GB in P.
  // These facts are NOT checked.
  // Any quotient elements of P are ignored?
{
}

PolyQQ *PolyQQ::create_quotient(const PolyQQ *P, const PolyQQ *B)
  // B should be a logical coeff ring of P.
  // All quotient elements of B are extended up to P.
  // and a new ring is made.
{
}
#endif

void PolyQQ::text_out(buffer &o) const
{
  o << "polyring(QQ,";
  getMonoid()->text_out(o);
  o << ")\n";
#warning "display quotient polynomials"
}

Nterm * PolyQQ::numerator(ring_elem f) const 
{
}

ring_elem PolyQQ::from_double(double n) const 
{
}
ring_elem PolyQQ::from_int(int n) const 
{
}
ring_elem PolyQQ::from_int(mpz_ptr n) const 
{
}

ring_elem PolyQQ::var(int v) const 
{
}
bool PolyQQ::promote(const Ring *R, const ring_elem f, ring_elem &result) const 
{
}
bool PolyQQ::lift(const Ring *R, const ring_elem f, ring_elem &result) const 
{
}

ring_elem PolyQQ::preferred_associate(ring_elem f) const 
{
}

bool PolyQQ::is_unit(const ring_elem f) const 
{
} // TODO

bool PolyQQ::is_zero(const ring_elem f) const 
{
}

bool PolyQQ::is_equal(const ring_elem f, const ring_elem g) const 
{
}

ring_elem PolyQQ::copy(const ring_elem f) const 
{
}

void PolyQQ::remove(ring_elem &f) const 
{
}

ring_elem PolyQQ::negate(const ring_elem f) const 
{
}

ring_elem PolyQQ::add(const ring_elem f, const ring_elem g) const 
{
}

ring_elem PolyQQ::subtract(const ring_elem f, const ring_elem g) const 
{
}

ring_elem PolyQQ::mult(const ring_elem f, const ring_elem g) const 
{
}

ring_elem PolyQQ::power(const ring_elem f, mpz_t n) const 
{
}

ring_elem PolyQQ::power(const ring_elem f, int n) const 
{
}

ring_elem PolyQQ::invert(const ring_elem f) const 
{
}

ring_elem PolyQQ::divide(const ring_elem f, const ring_elem g) const 
{
}

ring_elem PolyQQ::gcd(const ring_elem f, const ring_elem g) const 
{
}

ring_elem PolyQQ::gcd_extended(const ring_elem f, const ring_elem g, 
			       ring_elem &u, ring_elem &v) const 
{
}

ring_elem PolyQQ::remainder(const ring_elem f, const ring_elem g) const 
{
}

ring_elem PolyQQ::quotient(const ring_elem f, const ring_elem g) const 
{
}

ring_elem PolyQQ::remainderAndQuotient(const ring_elem f, const ring_elem g, 
				       ring_elem &quot) const 
{
}

void PolyQQ::syzygy(const ring_elem a, const ring_elem b,
		    ring_elem &x, ring_elem &y) const 
{
}

ring_elem PolyQQ::random() const 
{
}

void PolyQQ::elem_text_out(buffer &o, const ring_elem f) const 
{
}

ring_elem PolyQQ::eval(const RingMap *map, const ring_elem f) const 
{
}

/////////////////////////
// Polynomial routines //
/////////////////////////
int PolyQQ::index_of_var(const ring_elem a) const 
{
}

M2_arrayint PolyQQ::support(const ring_elem a) const 
{
}

bool PolyQQ::is_homogeneous(const ring_elem f) const 
{
}

void PolyQQ::degree(const ring_elem f, int *d) const 
{
}

bool PolyQQ::multi_degree(const ring_elem f, int *d) const 
{
}

int PolyQQ::primary_degree(const ring_elem f) const 
{
}

void PolyQQ::degree_weights(const ring_elem f, const M2_arrayint wts, 
			    int &lo, int &hi) const 
{
}

ring_elem PolyQQ::homogenize(const ring_elem f, int v, int deg, 
			     const M2_arrayint wts) const 
{
}

ring_elem PolyQQ::homogenize(const ring_elem f, int v, const M2_arrayint wts) const 
{
}

ring_elem PolyQQ::mult_by_term(const ring_elem f, 
			       const ring_elem c, const int *m) const 
{
}

int PolyQQ::n_flat_terms(const ring_elem f) const 
{
}

int PolyQQ::n_logical_terms(int nvars0,const ring_elem f) const 
{
}

ArrayPairOrNull PolyQQ::list_form(const Ring *coeffR, const ring_elem f) const 
{
}

ring_elem PolyQQ::make_flat_term(const ring_elem a, const int *m) const 
{
}

ring_elem PolyQQ::make_logical_term(const Ring *coeffR, const ring_elem a, const int *exp) const 
{
}

ring_elem PolyQQ::lead_flat_coeff(const ring_elem f) const 
{
}

ring_elem PolyQQ::lead_logical_coeff(const Ring *coeffR, const ring_elem f) const 
{
}

ring_elem PolyQQ::get_coeff(const Ring *coeffR, const ring_elem f, const int *vp) const 
{
}

// vp is a varpower monomial, in the logical monoid.
// The result will be an element in the logical coefficient ring.

ring_elem PolyQQ::get_terms(int nvars0,const ring_elem f, int lo, int hi) const 
{
}

// get the (logical) terms from lo to hi in f.  A negative value means count from
// the end.  get_terms(--,f,0,0) is the logical lead term of f.

const int * PolyQQ::lead_flat_monomial(const ring_elem f) const 
{
}

void PolyQQ::lead_logical_exponents(int nvars0, const ring_elem f, int * result_exp) const 
{
}

void PolyQQ::mult_coeff_to(ring_elem a, ring_elem &f) const 
{
}

void PolyQQ::monomial_divisor(const ring_elem a, int *exp) const 
{
}

ring_elem PolyQQ::diff(ring_elem a, ring_elem b, int use_coeff) const 
{
}

bool PolyQQ::in_subring(int nslots, const ring_elem a) const 
{
}

void PolyQQ::degree_of_var(int n, const ring_elem a, int &lo, int &hi) const 
{
}

ring_elem PolyQQ::divide_by_var(int n, int d, const ring_elem a) const 
{
}

ring_elem PolyQQ::divide_by_expvector(const int *exp, const ring_elem a) const 
{
}

const vecterm * PolyQQ::vec_locate_lead_term(const FreeModule *F, vec v) const 
{
}

vec PolyQQ::vec_lead_term(int nparts, const FreeModule *F, vec v) const 
{
}

vec PolyQQ::vec_top_coefficient(const vec v, int &x, int &e) const 
{
}

gbvector * PolyQQ::translate_gbvector_from_ringelem(ring_elem coeff) const 
{
}

gbvector * PolyQQ::translate_gbvector_from_vec(const FreeModule *F, 
					       const vec v, 
					       ring_elem &result_denominator) const 
{
}

vec PolyQQ::translate_gbvector_to_vec(const FreeModule *F, const gbvector *v) const 
{
}

vec PolyQQ::translate_gbvector_to_vec_denom(const FreeModule *F, 
					    const gbvector *v,
					    const ring_elem denom) const 
{
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
