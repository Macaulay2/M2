class Matrix;
class RingElement;
class FreeModule;
class Ring;
class GBRing;
class gbvector;
class res_poly;
class res2_poly;
struct resterm;
struct res2term;
class MutableMatrixXXX;
class MonomialIdeal;

#include "ringelem.hpp"

void dmatrix(const Matrix *M);

void drelem(const RingElement *f);

void dfree(const FreeModule *F);

extern "C" void dringelem(const Ring *R, const ring_elem f);

extern "C" void dvec(const Ring *R, const vec v);

extern "C" void dgbvec(const GBRing *R, gbvector *v);

void drespoly(const res_poly *R, const resterm *f);

void drespoly2(const res2_poly *R, const res2term *f);

extern "C" void dmutablemat(MutableMatrixXXX *m);

extern "C" void dmonideal(MonomialIdeal *m);

extern "C" void dintarray(M2_arrayint a);
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
