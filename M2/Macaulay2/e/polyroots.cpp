#include "polyroots.hpp"

engine_RawRingElementArrayOrNull rawRoots(const RingElement *p,
                                          long prec,
                                          int unique)
{
  const Ring *R = p->get_ring();
  const PolynomialRing *P = R->cast_to_PolynomialRing();
  if (P == 0)
    {
      ERROR("expected a polynomial ring");
      return nullptr;
    }
  const int n = P->n_vars();
  if (n != 1)
    {
      ERROR("expected a univariate polynomial ring");
      return nullptr;
    }
  const Ring *K = P->getCoefficients();

  if (true)
    {
      ERROR("'roots' is not yet reimplemented.");
      return nullptr;
    }
  else
    {
      // TODO: implement roots of a polynomial, via e.g. arb or mpsolve.
      engine_RawRingElementArrayOrNull result = nullptr;
      
      const size_t num_roots = 1; // TODO: set this correctly
      result = getmemarraytype(engine_RawRingElementArray, num_roots);
      result->len = static_cast<int>(num_roots);
      
      // TODO: set the roots correctly.
      
      return result;
    }
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
