#include "interface/aring.h"

#include <algorithm>
#include <utility>
#include <vector>

#include "aring-gf-givaro.hpp"
#include "aring-gf-flint-big.hpp"
#include "aring-gf-flint.hpp"
#include "aring-glue.hpp"
#include "aring-m2-gf.hpp"
#include "aring-qq.hpp"
#include "aring-tower.hpp"
#include "aring-zz-flint.hpp"
#include "aring-zzp-ffpack.hpp"
#include "aring-zzp-flint.hpp"
#include "aring-zzp.hpp"
#include "exceptions.hpp"
#include "polyring.hpp"
#include "relem.hpp"

const RingQQ *globalQQ;

void initializeRationalRing()
{
  M2::ARingQQ *A = new M2::ARingQQ();
  globalQQ = RingQQ::create(A);
}

const RingQQ *rawARingQQ() { return globalQQ; }
const Ring * /* or null */ rawARingZZFlint()
{
  M2::ARingZZ *A = new M2::ARingZZ();
  return M2::ConcreteRing<M2::ARingZZ>::create(A);
}

const Ring * /* or null */ rawARingQQFlint()
{
  M2::ARingQQFlint *A = new M2::ARingQQFlint();
  return M2::ConcreteRing<M2::ARingQQFlint>::create(A);
}

const Ring /* or null */ *rawARingZZp(unsigned long p)
{
  if (p <= 1 || p >= 32750)
    {
      ERROR("ZZP: expected a prime number p in range 2 <= p <= 32749");
      return 0;
    }
  M2::ARingZZp *A = new M2::ARingZZp(p);
  return M2::ConcreteRing<M2::ARingZZp>::create(A);
}
const Ring /* or null */ *rawARingZZpFlint(unsigned long p)
{
  M2::ARingZZpFlint *A = new M2::ARingZZpFlint(p);
  return M2::ConcreteRing<M2::ARingZZpFlint>::create(A);
}

static const PolynomialRing * /* or null */ checkGaloisFieldInput(
    const RingElement *f)
{
  // Check that the ring R of f is a polynomial ring in one var over a ZZ/p
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
  if (R->characteristic() == 0)
    {
      ERROR("expected poly ring of the form ZZ/p[x]/(f)");
      return 0;
    }
  return R;
}
const Ring /* or null */ *rawARingGaloisField1(const RingElement *f)
{
  const PolynomialRing *R = checkGaloisFieldInput(f);
  if (R == 0) return 0;  // error message has already been logged
  try
    {
      M2::ARingGFM2 *A = new M2::ARingGFM2(*R, f->get_value());
      return M2::ConcreteRing<M2::ARingGFM2>::create(A);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}
const Ring /* or null */ *rawARingGaloisFieldFlintBig(const RingElement *f)
{
  const PolynomialRing *R = checkGaloisFieldInput(f);
  if (R == 0) return 0;  // error message has already been logged
  try
    {
      M2::ARingGFFlintBig *A = new M2::ARingGFFlintBig(*R, f->get_value());
      return M2::ConcreteRing<M2::ARingGFFlintBig>::create(A);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

const Ring /* or null */ *rawARingGaloisFieldFlintZech(const RingElement *f)
{
  const PolynomialRing *R = checkGaloisFieldInput(f);
  if (R == 0) return 0;  // error message has already been logged
  try
    {
      M2::ARingGFFlint *A = new M2::ARingGFFlint(*R, f->get_value());
      return M2::ConcreteRing<M2::ARingGFFlint>::create(A);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

/// @todo why parameters are ints and not longs or mpz's? is an overflow
/// possible?
/// @todo check prime for primality (probably at top of Macaulay2)
/// @todo ARingGFGivaro uses tables and may consume a huge amount of memory -
///        pass therefore a 'MaxMemoryConsumption' parameter and if the value is
///        overstepped by ARingGFGivaro, create polynomial representation?
/// @todo  the check if in general polynomial representation is needed cost some
/// additional work, similar to linbox/field/givaro-gfq.h. Use GivaroGfq instead
/// of Givaro::GFqDom in ARingGFGivaro?
///@todo return Macaulay Galois field in some cases.

const Ring /* or null */ *rawARingGaloisField(int prime, int dimension)
{
  if (dimension < 0)
    {
      ERROR(" givaroGF/FFPACK: help, dimension is negative ! ");
      return 0;
    }
  try
    {
#if 1

      if (prime <= 1)
        {
          ERROR("givaroGF/FFPACK: expected a prime number p ");
          return 0;
        }

      //-- NEED_POLYNOMIAL_REPRESENTATION is not usable(namespace problems) and
      // is not correct, because the answer depends on the template used for
      // Givaro::GFqDom.
      /*if (Givaro::NEED_POLYNOMIAL_REPRESENTATION(prime,dimension) )
      {
          ERROR("givaro Galois Field: polynomial representation is needed  -
      todo ");
          return 0;
      }*/
      if (dimension == 1 && M2::ARingZZpFFPACK::getMaxModulus() > prime)
        {
          // std::cout << "maximum modulus = " <<
          // M2::ARingZZpFFPACK::getMaxModulus() << std::endl;
          M2::ARingZZpFFPACK *A = new M2::ARingZZpFFPACK(prime);
          return M2::ConcreteRing<M2::ARingZZpFFPACK>::create(A);
        }
      if (dimension == 1)
        {
          ERROR("maximum modulus = %f\n", M2::ARingZZpFFPACK::getMaxModulus());
          return 0;
        }
      M2::ARingGFGivaro *A = new M2::ARingGFGivaro(prime, dimension);
      return M2::ConcreteRing<M2::ARingGFGivaro>::create(A);
#else
      ERROR("add --enable-fflas-ffpack --enable-givaro when building M2");
      return 0;
#endif
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
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
  if (R->characteristic() == 0)
    {
      ERROR("expected poly ring of the form ZZ/p[x]/(f)");
      return 0;
    }

  if (!R->is_equal(a->get_value(), R->var(0)))
    {
      ERROR(
          "primitive element needs to be the generator of the ring, we "
          "think...!");
      return 0;
    }

  try
    {
      RingElement F(R, R->quotient_element(0));
      M2_arrayint modPoly = F.getSmallIntegerCoefficients();
      if (modPoly == 0)
        {
          ERROR("internal error: this should not happen");
          return NULL;
        }
      M2_arrayint primitiveElementPoly = a->getSmallIntegerCoefficients();
      if (primitiveElementPoly == 0) return NULL;

      M2::ARingGFGivaro *A = new M2::ARingGFGivaro(
          R->characteristic(), modPoly, primitiveElementPoly, *R);
      return M2::ConcreteRing<M2::ARingGFGivaro>::create(A);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

M2_arrayintOrNull rawARingGFPolynomial(const Ring *R)
{
#if 1
  const M2::ConcreteRing<M2::ARingGFGivaro> *RGF =
      dynamic_cast<const M2::ConcreteRing<M2::ARingGFGivaro> *>(R);
  if (RGF == 0)
    {
      ERROR("expected a GaloisField");
      return 0;
    }
  const M2::ARingGFGivaro &A = RGF->ring();
  return A.getModPolynomialCoeffs();
#else
  ERROR("add --enable-fflas-ffpack --enable-givaro when building M2");
  return 0;
#endif
}

M2_arrayintOrNull rawARingGFCoefficients(const RingElement *f)
{
#if 1
  const M2::ConcreteRing<M2::ARingGFGivaro> *RGF =
      dynamic_cast<const M2::ConcreteRing<M2::ARingGFGivaro> *>(f->get_ring());
  if (RGF == 0)
    {
      ERROR("expected a GaloisField");
      return 0;
    }
  const M2::ARingGFGivaro &A = RGF->ring();
  M2::ARingGFGivaro::ElementType a;
  A.from_ring_elem(a, f->get_value());
  return A.fieldElementToM2Array(a);
#else
  ERROR("add --enable-fflas-ffpack --enable-givaro when building M2");
  return 0;
#endif
}

const Ring /* or null */ *rawARingTower1(const Ring *K, M2_ArrayString names)
{
  try
    {
      const M2::ConcreteRing<M2::ARingZZpFFPACK> *Kp =
          dynamic_cast<const M2::ConcreteRing<M2::ARingZZpFFPACK> *>(K);
      if (Kp == 0)
        {
          ERROR("expected a base ring ZZ/p");
          return NULL;
        }
      const M2::ARingZZpFFPACK &A = Kp->ring();

      // Get the names into the correct form:
      auto varnames = M2_ArrayString_to_stdvector(names);
      const M2::ARingTower *T = M2::ARingTower::create(A, varnames);
      return M2::ConcreteRing<M2::ARingTower>::create(T);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

const Ring /* or null */ *rawARingTower2(const Ring *R1,
                                         M2_ArrayString new_names)
{
  try
    {
      const M2::ConcreteRing<M2::ARingTower> *K =
          dynamic_cast<const M2::ConcreteRing<M2::ARingTower> *>(R1);
      if (K == 0)
        {
          ERROR("expected a tower ring");
          return NULL;
        }
      const M2::ARingTower &A = K->ring();

      auto new_varnames = M2_ArrayString_to_stdvector(new_names);
      const M2::ARingTower *T = M2::ARingTower::create(A, new_varnames);
      return M2::ConcreteRing<M2::ARingTower>::create(T);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

const Ring /* or null */ *rawARingTower3(const Ring *R1,
                                         engine_RawRingElementArray eqns)
{
  try
    {
      const M2::ConcreteRing<M2::ARingTower> *K =
          dynamic_cast<const M2::ConcreteRing<M2::ARingTower> *>(R1);
      if (K == 0)
        {
          ERROR("expected a tower ring");
          return NULL;
        }
      const M2::ARingTower &A = K->ring();

      std::vector<M2::ARingTower::ElementType> extensions;

      for (int i = 0; i < eqns->len; i++)
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
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return 0;
  }
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
