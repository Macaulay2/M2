#include "debug.hpp"
#include "text-io.hpp"
#include "matrix.hpp"
#include "relem.hpp"
#include "gbring.hpp"
#include "res-a1-poly.hpp"
#include "res-a0-poly.hpp"
#include "hermite.hpp"
#include "mat.hpp"
#include "monideal.hpp"

void showint(mpz_srcptr a)
{
  char s[1000];
  mpz_get_str(s, 10, a);
  fprintf(stderr, " %s ", s);
}

void dintarray(M2_arrayint a)
{
  buffer o;
  o << "[";
  for (int i = 0; i < a->len; i++)
    {
      if (i > 0) o << ", ";
      o << a->array[i];
    }
  o << "]";
  emit(o.str());
}

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
  R->elem_text_out(o, f);
  emit(o.str());
}

void dNterm(const Ring *R, const Nterm *f)
{
  buffer o;
  ring_elem g = const_cast<Nterm *>(f);
  R->elem_text_out(o, g);
  emit(o.str());
}

void dvec(const Ring *R, const vec v)
{
  buffer o;
  R->vec_text_out(o, v);
  emit_line(o.str());
}

void dgbvec(const GBRing *R, gbvector *v)
{
  buffer o;
  const FreeModule *F = 0;
  R->gbvector_text_out(o, F, v);
  emit(o.str());
}

void drespoly(const res_poly *R, const resterm *f)
{
  buffer o;
  R->elem_text_out(o, f);
  emit(o.str());
}

void drespoly2(const res2_poly *R, const res2term *f)
{
  buffer o;
  R->elem_text_out(o, f);
  emit(o.str());
}

void dhermite(HermiteComputation *G)
{
  buffer o;
  G->text_out(o);
  emit(o.str());
}

void dmutablemat(MutableMatrix *m)
{
  buffer o;
  m->text_out(o);
  emit(o.str());
}

void dmonideal(MonomialIdeal *m)
{
  buffer o;
  m->text_out(o);
  emit(o.str());
}

void dstash()
{
  buffer o;
  stash::stats(o);
  emit(o.str());
}

void dRRR(gmp_RR a)
{
  buffer o;
  o << M2_tocharstar((*gmp_tostringRRpointer)(a)) << newline;
  emit(o.str());
}

void pring(const Ring *R)
{
  buffer o;
  R->text_out(o);
  emit(o.str());
}
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
