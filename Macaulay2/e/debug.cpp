#include "text_io.hpp"
#include "matrix.hpp"
#include "relem.hpp"
#include "gbring.hpp"

void dmatrix(const Matrix *M)
{
  buffer o;
  M->text_out(o);
  emit(o.str());
}

void drelem(const RingElement *f)
{
  buffer o;
  f->text_out(o);
  emit(o.str());
}

void dfree(const FreeModule *F)
{
  buffer o;
  F->text_out(o);
  emit(o.str());
}

void dringelem(const Ring *R, const ring_elem f)
{
  buffer o;
  R->elem_text_out(o,f);
  emit(o.str());
}

void dvec(const GBRing *R, gbvector *v)
{
  buffer o;
  const FreeModule *F = 0;
  R->gbvector_text_out(o, F, v);
  emit(o.str());
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
