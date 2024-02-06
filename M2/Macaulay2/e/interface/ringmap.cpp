#include "interface/ringmap.h"

#include "buffer.hpp"
#include "error.h"
#include "exceptions.hpp"
#include "freemod.hpp"
#include "mat.hpp"
#include "matrix.hpp"
#include "ringmap.hpp"

class Ring;
class RingElement;

const Ring *IM2_RingMap_target(const RingMap *F) { return F->get_ring(); }
M2_string IM2_RingMap_to_string(const RingMap *F)
{
  buffer o;
  try
    {
      F->text_out(o);
      return o.to_string();
  } catch (const exc::engine_error& e)
    {
      o << "[unprintable ringmap]";
      return o.to_string();
  }
}

unsigned int rawRingMapHash(const RingMap *F) { return F->hash(); }
M2_bool IM2_RingMap_is_equal(const RingMap *f, const RingMap *g)
{
  return f->is_equal(g);
}

const RingMap *IM2_RingMap_make(const Matrix *M, const Ring *base); /* TODO */

const RingMap *IM2_RingMap_make1(const Matrix *M) { return RingMap::make(M); }
const RingElement /* or null */ *IM2_RingMap_eval_ringelem(const RingMap *F,
                                                           const RingElement *a)
{
  try
    {
      return F->eval(a);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

const Matrix /* or null */ *IM2_RingMap_eval_matrix(const RingMap *F,
                                                    const FreeModule *newTarget,
                                                    const Matrix *M)
{
  if (newTarget->rank() < M->n_rows())
    {
      ERROR("expected FreeModule of rank at least %d", M->n_rows());
      return NULL;
    }
  try
    {
      return F->eval(newTarget, M);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

MutableMatrix /* or null */ *rawRingMapEvalMutableMatrix(const RingMap *F,
                                                         const MutableMatrix *M)
{
  try
    {
      return MutableMatrix::zero_matrix(
          F->get_ring(), M->n_rows(), M->n_cols(), M->is_dense());
      // TODO: now map it!
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
