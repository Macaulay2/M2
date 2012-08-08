#include "engine.h"
#include "exceptions.hpp"

#include "relem.hpp"
#include "aring-glue.hpp"
#include "aring-zzp.hpp"
#include "aring-gf.hpp"
#include "aring-m2-gf.hpp"
#include "aring-ffpack.hpp"
#include "aring-tower.hpp"

#include "polyring.hpp"

const Ring /* or null */ *rawARingZZp(int p)
  /* p must be a prime number <= 32767 */
{
  if (p <= 1 || p >= 32750)
    {
      ERROR("ZZP: expected a prime number p in range 2 <= p <= 32749");
      return 0;
    }
  M2::ARingZZp *A = new M2::ARingZZp(p);
  return M2::ConcreteRing<M2::ARingZZp>::create(A);
}

const Ring /* or null */ *rawARingGaloisField1(const RingElement *f)
{
  // Check that the ring R of f is a polynomial ring in one var over a ZZ/p
  // Check that f has degree >= 2
  // Check that f is monic
  // If any of these fail, then return 0.
  const PolynomialRing *R = f->get_ring()->cast_to_PolynomialRing();
  if (R == 0)
    {
      ERROR("expected poly ring of the form ZZ/p[x]/(f)");
      return 0;
    }
  if (R->n_vars() != 1)
    {
      ERROR("expected poly ring of the form ZZ/p[x]/(f)");
      return 0;
    }
  if (R->n_quotients() != 1)
    {
      ERROR("expected poly ring of the form ZZ/p[x]/(f)");
      return 0;
    }
  if (R->charac() == 0)
    {
      ERROR("expected poly ring of the form ZZ/p[x]/(f)");
      return 0;
    }
  try {
    M2::ARingGFM2 *A = new M2::ARingGFM2(*R,f->get_value());
    return M2::ConcreteRing<M2::ARingGFM2>::create(A);
  }
  catch (exc::engine_error e) {
    ERROR(e.what());
    return NULL;
  }
}


/// @todo why parameters are ints and not longs or mpz's? is an overflow possible?
/// @todo check prime for primality (probably at top of Macaulay2)
/// @todo ARingGF uses tables and may consume a huge amount of memory -
///        pass therefore a 'MaxMemoryConsumption' parameter and if the value is overstepped by ARingGF, create polynomial representation?
/// @todo  the check if in general polynomial representation is needed cost some additional work, similar to linbox/field/givaro-gfq.h. Use GivaroGfq instead of Givaro::GFqDom in ARingGF?
///@todo: return Macaulay Galois field in some cases.

const Ring /* or null */ *rawARingGaloisField(int prime, int dimension)
{
        if (dimension < 0  )
        {
            ERROR(" givaroGF/FFPACK: help, dimension is negative ! ");
            return 0;
        }
     try {
#if defined(HAVE_FFLAS_FFPACK) && defined(HAVE_GIVARO)

        if (prime <= 1  )
        {
            ERROR("givaroGF/FFPACK: expected a prime number p ");
            return 0;
        }

        //-- NEED_POLYNOMIAL_REPRESENTATION is not usable(namespace problems) and is not correct, because the answer depends on the template used for Givaro::GFqDom.
        /*if (Givaro::NEED_POLYNOMIAL_REPRESENTATION(prime,dimension) )  
        {
            ERROR("givaro Galois Field: polynomial representation is needed  - todo ");
            return 0;
        }*/
        if (dimension==1 && M2::ARingZZpFFPACK::getMaxModulus()> prime) 
        {
	  std::cout << "maximum modulus = " << M2::ARingZZpFFPACK::getMaxModulus() << std::endl;
          M2::ARingZZpFFPACK *A = new M2::ARingZZpFFPACK(prime);
          return M2::ConcreteRing<M2::ARingZZpFFPACK>::create(A);
        }
	if (dimension==1)
	  {
	    ERROR("maximum modulus = %f\n", M2::ARingZZpFFPACK::getMaxModulus());
	    return 0;
	  }
        M2::ARingGF *A = new M2::ARingGF(prime,dimension);
        return M2::ConcreteRing<M2::ARingGF>::create(A);
#else
       ERROR("add --enable-fflas-ffpack --enable-givaro when building M2");
       return 0;
#endif
     }
     catch (exc::engine_error e) {
          ERROR(e.what());
          return NULL;
     }
}

const M2_arrayint getPolynomialCoefficients(const PolynomialRing *R, const ring_elem f)
{
  // Assumption: ring of f has one variable.
  if (R == 0 || R->n_vars() > 1)
    {
      ERROR("expected polynomial ring in one variable");
      return 0;
    }

  int lo, deg; // ignore lo, and deg == degree of the univariate polynomial f.
  R->degree_of_var(0, f, lo, deg);
  M2_arrayint polynomialCoeffs = M2_makearrayint(deg+1);
  for (int i=0; i<=deg; i++)
    polynomialCoeffs->array[i] = 0;
  int exp[1];
  for (Nterm *t = f; t != NULL; t = t->next)
      {
	int coef = R->getCoefficientRing()->coerce_to_int(t->coeff);
        R->getMonoid()->to_expvector(t->monom, exp);
	ASSERT(exp[0] >= 0);
	ASSERT(exp[0] <= deg);
	polynomialCoeffs->array[exp[0]] = coef;
      }
  return polynomialCoeffs;
}

const Ring /* or null */ *rawARingGaloisFieldFromQuotient(const RingElement *a)
{
  // Check that the ring R of f is a polynomial ring in one var over a ZZ/p
  // Check that f has degree >= 2
  // Check that f is monic
  // If any of these fail, then return 0.
  const PolynomialRing *R = a->get_ring()->cast_to_PolynomialRing();
  if (R == 0)
    {
      ERROR("expected poly ring of the form ZZ/p[x]/(f)");
      return 0;
    }
  if (R->n_vars() != 1)
    {
      ERROR("expected poly ring of the form ZZ/p[x]/(f)");
      return 0;
    }
  if (R->n_quotients() != 1)
    {
      ERROR("expected poly ring of the form ZZ/p[x]/(f)");
      return 0;
    }
  if (R->charac() == 0)
    {
      ERROR("expected poly ring of the form ZZ/p[x]/(f)");
      return 0;
    }
  
  if (!R->is_equal(a->get_value(), R->var(0)))
    {
      ERROR("primitive element needs to be the generator of the ring, we think...!");
      return 0;
    }
  
  M2_arrayint modPoly = getPolynomialCoefficients(R, R->quotient_element(0));
  if (modPoly == 0)
    return 0;

  // Now get the generator for the group of units (a 'primitive' element)
  M2_arrayint primitiveElementPoly = getPolynomialCoefficients(R, a->get_value());
  if (primitiveElementPoly == 0)
    return 0;
    
  try {
    M2::ARingGF *A = new M2::ARingGF(R->charac(), modPoly, primitiveElementPoly, *R);
    return M2::ConcreteRing<M2::ARingGF>::create(A);
  }
  catch (exc::engine_error e) {
    ERROR(e.what());
    return NULL;
  }
}


M2_arrayintOrNull rawARingGFPolynomial(const Ring *R)
{
#if defined(HAVE_GIVARO)
  const M2::ConcreteRing<M2::ARingGF> *RGF = dynamic_cast<const M2::ConcreteRing<M2::ARingGF> *>(R);
  if (RGF == 0)
    {
      ERROR("expected a GaloisField");
      return 0;
    }
  const M2::ARingGF &A = RGF->ring();
  return A.getModPolynomialCoeffs();
#else
  ERROR("add --enable-fflas-ffpack --enable-givaro when building M2");
  return 0;
#endif
}


const RingElement* rawARingGFGenerator(const Ring *R)
{
#if defined(HAVE_GIVARO)
 const M2::ConcreteRing<M2::ARingGF> *RGF = dynamic_cast<const M2::ConcreteRing<M2::ARingGF> *>(R);
  if (RGF == 0)
    {
      ERROR("expected a GaloisField");
      return 0;
    }
  const M2::ARingGF &A = RGF->ring();
  return RingElement::make_raw( R, A.getGenerator() );
#else
  ERROR("add --enable-fflas-ffpack --enable-givaro when building M2");
  return 0;
#endif
}


M2_arrayintOrNull rawARingGFCoefficients(const RingElement *f)
{
#if defined(HAVE_GIVARO)
  const M2::ConcreteRing<M2::ARingGF> *RGF = dynamic_cast<const M2::ConcreteRing<M2::ARingGF> *>(f->get_ring());
  if (RGF == 0)
  {
      ERROR("expected a GaloisField");
      return 0;
  }
  const M2::ARingGF &A = RGF->ring();
  M2::ARingGF::ElementType a;
  A.from_ring_elem(a, f->get_value());
  return A.fieldElementToM2Array(a);
#else
  ERROR("add --enable-fflas-ffpack --enable-givaro when building M2");
  return 0;
#endif
}

void M2_ArrayString_to_stdvector(M2_ArrayString strs, std::vector<std::string> &result)
{
  for (size_t i = 0; i< strs->len; i++)
    {
      M2_string a = strs->array[i];
      std::string b(a->array, a->len);
      result.push_back(b);
    }
}

const Ring /* or null */ *rawARingTower1(const Ring *K, M2_ArrayString names)
{
  try {
    const M2::ConcreteRing<M2::ARingZZpFFPACK> *Kp = dynamic_cast<const M2::ConcreteRing<M2::ARingZZpFFPACK> *>(K);
    if (Kp == 0)
      {
	ERROR("expected a base ring ZZ/p");
	return NULL;
      }
    const M2::ARingZZpFFPACK &A = Kp->ring();

    // Get the names into the correct form:
    std::vector<std::string> varnames;
    M2_ArrayString_to_stdvector(names, varnames);
    const M2::ARingTower *T = M2::ARingTower::create(A, varnames);
    return M2::ConcreteRing<M2::ARingTower>::create(T);
  }
  catch (exc::engine_error e) {
    ERROR(e.what());
    return NULL;
  }
}

const Ring /* or null */ *rawARingTower2(const Ring *R1, M2_ArrayString new_names)
{
  try {
    const M2::ConcreteRing<M2::ARingTower> *K = dynamic_cast<const M2::ConcreteRing<M2::ARingTower> *>(K);
    if (K == 0)
      {
	ERROR("expected a tower ring");
	return NULL;
      }
    const M2::ARingTower &A = K->ring();
    
    std::vector<std::string> new_varnames;
    M2_ArrayString_to_stdvector(new_names, new_varnames);
    const M2::ARingTower *T = M2::ARingTower::create(A, new_varnames);
    return M2::ConcreteRing<M2::ARingTower>::create(T);
  }
  catch (exc::engine_error e) {
    ERROR(e.what());
    return NULL;
  }
}

const Ring /* or null */ *rawARingTower3(const Ring *R1, engine_RawRingElementArray eqns)
{
  try {
    const M2::ConcreteRing<M2::ARingTower> *K = dynamic_cast<const M2::ConcreteRing<M2::ARingTower> *>(K);
    if (K == 0)
      {
	ERROR("expected a tower ring");
	return NULL;
      }
    const M2::ARingTower &A = K->ring();

    std::vector<M2::ARingTower::ElementType> extensions;
    
    for (int i=0; i<eqns->len; i++)
      {
        const RingElement *f = eqns->array[i];
        M2::ARingTower::ElementType f1;
        if (f->get_ring() != R1)
          {
            ERROR("extension element has incorrect base ring");
            return 0;
          }
        A.from_ring_elem(f1, f->get_value());
        extensions.push_back(f1);
      }
    const M2::ARingTower *T = M2::ARingTower::create(A, extensions);
    return M2::ConcreteRing<M2::ARingTower>::create(T);
  }
  catch (exc::engine_error e) {
    ERROR(e.what());
    return 0;
  }
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
