class Matrix;
class RingElement;
class FreeModule;
class Ring;
class GBRing;
class gbvector;

#include "ringelem.hpp"

void dmatrix(const Matrix *M);

void drelem(const RingElement *f);

void dfree(const FreeModule *F);

extern "C" void dringelem(const Ring *R, const ring_elem f);

extern "C" void dvec(const GBRing *R, gbvector *v);


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
