#include "util.hpp"
#include "polyring.hpp"
#include "ring.hpp"
#include "monoid.hpp"
#include "qring.hpp"
#include "polyquotient.hpp"
#include "matrix.hpp"
#include "matrix-con.hpp"
#include "geopoly.hpp"

PolynomialRing::~PolynomialRing() {}
void PolynomialRing::setQuotientInfo(QRingInfo *qinfo0)
{
  qinfo_ = qinfo0;
  const PolyRing *numerR = getNumeratorRing();  // might be 'this'

  for (int i = 0; i < n_quotients(); i++)
    {
      if (!numerR->is_homogeneous(quotient_element(i)))
        {
          setIsGraded(false);
          break;
        }
    }

  overZZ_ = (coeff_type_ == Ring::COEFF_ZZ);
}

void PolynomialRing::initialize_PolynomialRing(const Ring *K,
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

  exp_size = EXPONENT_BYTE_SIZE(nvars_);

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

  poly_size_ = 0;  // The callee needs to set this later
  gb_ring_ = 0;    // The callee needs to set this later

  // Also: callee should call setIsGraded, and set oneV, minus_oneV, zeroV
}

PolynomialRing *PolynomialRing::create_quotient(const PolynomialRing *R,
                                                VECTOR(Nterm *) & elems)
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
  switch (coeff_type)
    {
      case COEFF_BASIC:
        qrinfo = new QRingInfo_field_basic(R->getNumeratorRing(), elems);
        result = new PolyRingQuotient;
        break;
      case COEFF_QQ:
        qrinfo = new QRingInfo_field_QQ(R->getNumeratorRing(), elems);
        result = new PolyRingQuotient;
        break;
      case COEFF_ZZ:
        QRingInfo_ZZ *qrinfoZZ = new QRingInfo_ZZ(R->getNumeratorRing(), elems);
        qrinfo = qrinfoZZ;
        result = new PolyRingQuotient;
        result->is_ZZ_quotient_ = qrinfoZZ->is_ZZ_quotient();
        result->ZZ_quotient_value_ = qrinfoZZ->ZZ_quotient_value();
        break;
    }

  result->initialize_ring(
      R->characteristic(), R->get_degree_ring(), R->get_heft_vector());

  result->initialize_PolynomialRing(R->getCoefficients(),
                                    R->getMonoid(),
                                    R->getNumeratorRing(),
                                    R->getAmbientRing(),
                                    R->getDenominatorRing());

  result->gb_ring_ = R->get_gb_ring();
  result->setQuotientInfo(qrinfo);  // Also sets graded-ness

  result->zeroV = result->from_long(0);
  result->oneV = result->from_long(1);
  result->minus_oneV = result->from_long(-1);

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

  for (int i = 0; i < M->n_cols(); i++)
    {
      Nterm *f = R->numerator(M->elem(0, i));
      elems.push_back(f);
    }

  for (int i = 0; i < R->n_quotients(); i++)
    elems.push_back(R->quotient_element(i));

  return create_quotient(R->getAmbientRing(), elems);
}

PolynomialRing *PolynomialRing::create_quotient(const PolynomialRing *R,
                                                const PolynomialRing *B)
// R should be an ambient poly ring
// B should have: ambient of B is the logical coeff ring of R
//   i.e. R = A[x], B = A/I
// return A[x]/I.
{
  VECTOR(Nterm *) elems;

  for (int i = 0; i < B->n_quotients(); i++)
    {
      ring_elem f;
      R->promote(B->getNumeratorRing(), B->quotient_element(i), f);
      elems.push_back(f);
    }
  return create_quotient(R, elems);
}

Matrix *PolynomialRing::getPresentation() const
{
  const PolynomialRing *R = getAmbientRing();

  MatrixConstructor mat(R->make_FreeModule(1), 0);
  for (int i = 0; i < n_quotients(); i++)
    // NEED: to make this into a fraction, if R has fractions.
    mat.append(R->make_vec(0, quotient_element(i)));
  return mat.to_matrix();
}

class SumCollectorPolyHeap : public SumCollector
{
  polyheap H;

 public:
  SumCollectorPolyHeap(const PolynomialRing *R0) : H(R0) {}
  ~SumCollectorPolyHeap() {}
  virtual void add(ring_elem f) { H.add(f); }
  virtual ring_elem getValue() { return H.value(); }
};

SumCollector *PolynomialRing::make_SumCollector() const
{
  return new SumCollectorPolyHeap(this);
}

unsigned int PolynomialRing::computeHashValue(const ring_elem a) const
{
  unsigned int hash = 0;
  unsigned int seed1 = 103;
  unsigned int seed2 = 347654;
  for (Nterm& t : a.poly_val)
    {
      unsigned int hash1 = getCoefficientRing()->computeHashValue(t.coeff);
      unsigned int hash2 = getMonoid()->computeHashValue(t.monom);
      hash += seed1 * hash1 + seed2 * hash2;
      seed1 += 463633;
      seed2 += 7858565;
    }
  return hash;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
