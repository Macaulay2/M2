#include "poly.hpp"
#include "ring.hpp"
#include "monoid.hpp"
#include "qring.hpp"
#include "polyquotient.hpp"
#include "matrix.hpp"
#include "matrix-con.hpp"

PolynomialRing::~PolynomialRing()
{
}

void PolynomialRing::setQuotientInfo(QRingInfo *qinfo0) 
{
  qinfo_ = qinfo0;
  const PolyRing *numerR = getNumeratorRing(); // might be 'this'

  for (int i=0; i<n_quotients(); i++)
    {
      if (!numerR->is_homogeneous(quotient_element(i)))
	{
	  setIsGraded(false);
	  break;
	}
    }

  overZZ_ = (coeff_type_ == Ring::COEFF_ZZ);
}

void PolynomialRing::initialize_PolynomialRing(
					       const Ring *K,
					       const Monoid *M,
					       const PolyRing *numeratorR,
					       const PolynomialRing *ambientR,
					       const Ring *denomR)
{
  nvars_ = M->n_vars();
  K_ = K;
  M_ = M;
  numerR_ = numeratorR;
  ambientR_ = ambientR;
  denomR_ = denomR;

  
  if (K->is_QQ() || (K == globalZZ && denomR != 0))
    coeff_type_ = Ring::COEFF_QQ;
  else if (K == globalZZ && denomR == 0)
    coeff_type_ = Ring::COEFF_ZZ;
  else 
    coeff_type_ = Ring::COEFF_BASIC;

  is_weyl_ = false;
  is_solvable_ = false;
  is_skew_ = false;
  overZZ_ = false;
  qinfo_ = new QRingInfo;
  is_ZZ_quotient_ = false;
  ZZ_quotient_value_ = ZERO_RINGELEM;

  if (numeratorR != this)
    {
      // We must set the non-commutative settings ourselves at this time
      if (numeratorR->cast_to_WeylAlgebra() != 0)
	is_weyl_ = true;
      else if (numeratorR->cast_to_SolvableAlgebra() != 0)
	is_solvable_ = true;
      else if (numeratorR->is_skew_commutative())
	{
	  is_skew_ = true;
	  skew_ = numeratorR->getSkewInfo();
	}
    }

  poly_size_ = 0; // The callee needs to set this later
  gb_ring_ = 0;  // The callee needs to set this later

  // Also: callee should call setIsGraded, and set oneV, minus_oneV, zeroV
}

PolynomialRing *PolynomialRing::create_quotient(const PolynomialRing *R, 
						VECTOR(Nterm *) &elems)
  // Grabs 'elems'.  Each element of 'elems' should be in the ring R.
  // They should also form a GB.
{
  // Here are the cases:
  // (1) R is a polynomial ring over a basic field
  // (2) R is a polynomial ring over ZZ
  // (3) R is a polynomial ring over QQ

  // case (1), (2): PolyRingQuotient
  // case (3): PolyQQ

  PolynomialRing *result = NULL;
  Ring::CoefficientType coeff_type = R->coefficient_type();

  QRingInfo *qrinfo = NULL;
  switch (coeff_type) {
  case COEFF_BASIC:
    qrinfo = new QRingInfo_field_basic(R->getNumeratorRing(),elems);
    result = new PolyRingQuotient;
    break;
  case COEFF_QQ:
    qrinfo = new QRingInfo_field_QQ(R->getNumeratorRing(),elems);
    result = new PolyRingQuotient;
    break;
  case COEFF_ZZ:
    QRingInfo_ZZ *qrinfoZZ = new QRingInfo_ZZ(R->getNumeratorRing(),elems);
    qrinfo = qrinfoZZ;
    result = new PolyRingQuotient;
    result->is_ZZ_quotient_ = qrinfoZZ->is_ZZ_quotient();
    result->ZZ_quotient_value_ = qrinfoZZ->ZZ_quotient_value();
    break;
  }

  result->initialize_ring(R->charac(),
			  R->get_degree_ring());

  result->initialize_PolynomialRing(R->getCoefficients(),
				    R->getMonoid(),
				    R->getNumeratorRing(),
				    R->getAmbientRing(),
				    R->getDenominatorRing());

  result->gb_ring_ = R->get_gb_ring();
  result->setQuotientInfo(qrinfo); // Also sets graded-ness

  result->zeroV = result->from_int(0);
  result->oneV = result->from_int(1);
  result->minus_oneV = result->from_int(-1);
  
  return result;
}

PolynomialRing *PolynomialRing::create_quotient(const PolynomialRing *R, 
						const Matrix *M)
{
  if (M->get_ring() != R)
    {
      ERROR("quotient elements not in the expected polynomial ring");
      return 0;
    }
  VECTOR(Nterm *) elems;

  for (int i=0; i<M->n_cols(); i++)
    {
      Nterm *f = R->numerator(M->elem(0,i));
      elems.push_back(f);
    }

  for (int i=0; i<R->n_quotients(); i++)
    elems.push_back(R->quotient_element(i));

  return create_quotient(R->getAmbientRing(),elems);
}

PolynomialRing *PolynomialRing::create_quotient(const PolynomialRing *R, 
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
      R->promote(B->getNumeratorRing(), B->quotient_element(i), f);
      elems.push_back(f);
    }
  return create_quotient(R,elems);
}

Matrix * PolynomialRing::getPresentation() const
{
  const PolynomialRing *R = getAmbientRing();

  MatrixConstructor mat(R->make_FreeModule(1), 0);
  for (int i=0; i<n_quotients(); i++)
    // NEED: to make this into a fraction, if R has fractions.
    mat.append(R->make_vec(0, quotient_element(i)));
  return mat.to_matrix();
}


#if 0
// const RRing *PPolynomialRing::findCoefficientRing(const RRing *A) const
// {
//   // There are two cases: (1) A is a basic ring, (2) A is a poly ring
//   // possibly with fractions or quotients.
//   const PolyRing *R = getAmbientRing();
//   const PPolynomialRing *B = A->cast_to_PPolynomialRing();
//   if (B == 0)
//     {
//       // This means that A is a basic ring
//       if (R->getFlatCoefficients() == A) return A;
//       return 0;
//     }
//   const PolyRing *C = B->getAmbientRing(); // This is basically the ring A
//   while (R != 0)
//     {
//       if (C == R) return C;
//       const RRing *RC = R->getCoefficients();
//       R = RC->cast_to_PolyRing();
//     }
//   return 0;
// }
// 
// 
// const PolyRing *PolyRing::create(const RRing *K, const Monoid *M)
//   // Create the ring K[M].  
//   // K must be either a basic ring, or an ambient polynomial ring,
//   //  possibly non-commutative of some sort.
// {
//   const PPolynomialRing *A = K->cast_to_PPolynomialRing();
//   if (A == 0)
//     {
//       // A is a basic ring
//       PolyRing *R = new PolyRing;
//       R->initialize(K,M,  K,M);
//       return R;
//     }
//   const PolyRing *B = A->getAmbientRing();
//   return B->createPolyRing(M);
// }
// 
// const PolyRing *PolyRingSkew::createPolyRing(const Monoid *M) const
// {
//   M2_arrayint skew;
//   getSkewInfo(skew);
//   int n = M->n_vars();
//   M2_arrayint newskew = addScalar(skew, n);
//   const Monoid *flatM = Monoid::tensor_product(M, getMonoid());
//   
//   PolyRingSkew *R = new PolyRingSkew;
//   R->initialize(this, M,  getCoefficients(), flatM);
//   R->setSkewInfo(newskew);
//   return R;
// }
// 
// const PolyRingSkew *PolyRingSkew::create(const PolyRing *P, 
// 					 M2_arrayint skewvars)
// {
//   PolyRingSkew *R = new PolyRingSkew;
//   R->initialize(P->getCoefficients(),
// 		P->getMonoid(),
// 		P->getFlatCoefficients(),
// 		P->getFlatMonoid());
//   R->setSkewInfo(skewvars);
//   return R;
// }
// 
// const PolyRingWeyl *PolyRingWeyl::create(const PolyRing *P,
// 					 M2_arrayint derivatives,
// 					 M2_arrayint commutatives,
// 					 int homog_var)
// {
//   PolyRingWeyl *R = new PolyRingWeyl;
//   R->initialize(P->getCoefficients(),
// 		P->getMonoid(),
// 		P->getFlatCoefficients(),
// 		P->getFlatMonoid());
//   R->setWeylInfo(derivatives, commutatives, homog_var);
//   return R;
// }
// 
// const PolyQuotient *PolyQuotient::create(const Matrix *quotient_gb)
// {
//   // There are two cases here?
//   // If the base is a field, then set Rideal, MonomialTable.
//   //   and the normal form routine only considers monomials
//   //   and the GB is set to be monic (or assumed so?).
//   // If the base is ZZ (the only other case), then we make a MonomialTableZZ,
//   //   and use a different normal form.
// }
// PPolynomialRing *PPolynomialRing::createQuotient(const Matrix *quotients) const
// {
//   // This depends on the kind of ring
//   // (a) PolyRing
//   //    Create a PolyQuotient
//   // (b) PolyQuotient
//   //    Create a new PolyQuotient, with the union of two GB's.
//   // (c) PolyFrac
//   //    Create a PolyFracQuotient
//   // (d) PolyFracQuotient
//   //    Create a new PolyFracQuotient, with the union of two GB's.
// 
//   return makeQuotientRing(quotients); // virtual call
// }
// 
// PPolynomialRing *PPolynomialRing::createQuotient(const PPolynomialRing *B) const
// {
//   // First check that B is a valid quotient of this.
//   // Then make the quotient elements in this ring
//   return makeQuotientRing(quotients); // virtual call: 
// }
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
