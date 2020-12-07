// Copyright 2010 Michael E. Stillman.

#include "finalize.hpp"
#include "engine-includes.hpp"

#include "atomic_ops.h"
#include "monideal.hpp"
#include "comp-gb.hpp"
#include "comp-res.hpp"
#include "schorder.hpp"
#include "mat.hpp"

#include <M2/gc-include.h>

static volatile AO_t monideals_nfinalized = 0;
static volatile AO_t monideals_nremoved = 0;

static volatile AO_t mutablematrices_nfinalized = 0;
static volatile AO_t mutablematrices_nremoved = 0;

static volatile AO_t polyrings_nfinalized = 0;
static volatile AO_t polyrings_nremoved = 0;

static volatile AO_t gbs_nfinalized = 0;
static volatile AO_t gbs_nremoved = 0;

static volatile AO_t res_nfinalized = 0;
static volatile AO_t res_nremoved = 0;

static volatile AO_t comp_nfinalized = 0;
static volatile AO_t comp_nremoved = 0;

static volatile AO_t schorder_nfinalized = 0;
static volatile AO_t schorder_nremoved = 0;

//////////////////////////////////////////////////////
extern "C" void remove_monideal(void *p, void *cd)
{
  MonomialIdeal *G = static_cast<MonomialIdeal *>(p);
  AO_t nremoved = AO_fetch_and_add1(&monideals_nremoved);
  if (M2_gbTrace >= 3)
    fprintf(stderr, "\n -- removing monomial ideal %zd at %p\n", nremoved, G);
  G->remove_MonomialIdeal();
}
void intern_monideal(MonomialIdeal *G)
{
  GC_REGISTER_FINALIZER(G, remove_monideal, 0, 0, 0);
  AO_t nfinalized = AO_fetch_and_add1(&monideals_nfinalized);
  if (M2_gbTrace >= 3)
    fprintf(stderr,
            "\n   -- registering monomial ideal %zd at %p\n",
            nfinalized,
            (void *)G);
}
//////////////////////////////////////////////////////
extern "C" void remove_polyring(void *p, void *cd)
{
  PolynomialRing *G = static_cast<PolynomialRing *>(p);
  AO_t nremoved = AO_fetch_and_add1(&polyrings_nremoved);
  if (M2_gbTrace >= 3)
    fprintf(stderr, "\n -- removing polynomial ring %zd at %p\n", nremoved, G);
  G->clear();
}
void intern_polyring(const PolynomialRing *G)
{
  GC_REGISTER_FINALIZER_IGNORE_SELF(
      const_cast<PolynomialRing *>(G), remove_polyring, 0, 0, 0);
  AO_t nfinalized = AO_fetch_and_add1(&polyrings_nfinalized);
  if (M2_gbTrace >= 3)
    fprintf(stderr,
            "\n   -- registering polynomial ring %zd at %p\n",
            nfinalized,
            (const void *)G);
}
//////////////////////////////////////////////////////
extern "C" void remove_gb(void *p, void *cd)
{
  GBComputation *G = static_cast<GBComputation *>(p);
  AO_t nremoved = AO_fetch_and_add1(&gbs_nremoved);
  if (M2_gbTrace >= 3)
    fprintf(stderr, "\n --removing gb %zd at %p\n", nremoved, G);
  G->remove_gb();
}
void intern_GB(GBComputation *G)
{
  //  GC_REGISTER_FINALIZER(G,remove_gb,0,0,0);
  GC_REGISTER_FINALIZER_IGNORE_SELF(G, remove_gb, 0, 0, 0);
  AO_t nfinalized = AO_fetch_and_add1(&gbs_nfinalized);
  if (M2_gbTrace >= 3)
    {
      // -- there is no gettid under Solaris
      // int tid = static_cast<int>(syscall(SYS_gettid));
      // fprintf(stderr, "\n   -- thread %d registering gb %zd at %p\n", tid,
      // nfinalized, (void *)G);
      fprintf(
          stderr, "\n   -- registering gb %zd at %p\n", nfinalized, (void *)G);
    }
}
//////////////////////////////////////////////////////
extern "C" void remove_res(void *p, void *cd)
{
  ResolutionComputation *G = static_cast<ResolutionComputation *>(p);
  AO_t nremoved = AO_fetch_and_add1(&res_nremoved);
  if (M2_gbTrace >= 3)
    fprintf(stderr, "\n -- removing res %zd at %p\n", nremoved, G);
  delete G;
}
void intern_res(ResolutionComputation *G)
{
  GC_REGISTER_FINALIZER(G, remove_res, 0, 0, 0);
  AO_t nfinalized = AO_fetch_and_add1(&res_nfinalized);
  if (M2_gbTrace >= 3)
    fprintf(
        stderr, "\n   -- registering res %zd at %p\n", nfinalized, (void *)G);
}
//////////////////////////////////////////////////////
extern "C" void remove_computation(void *p, void *cd)
{
  EngineComputation *G = static_cast<EngineComputation *>(p);
  AO_t nremoved = AO_fetch_and_add1(&comp_nremoved);
  if (M2_gbTrace >= 3)
    fprintf(
        stderr, "\n -- removing engine computation %zd at %p\n", nremoved, G);
  G->destroy();
}
void intern_computation(EngineComputation *G)
{
  GC_REGISTER_FINALIZER(G, remove_computation, 0, 0, 0);
  AO_t nfinalized = AO_fetch_and_add1(&comp_nfinalized);
  if (M2_gbTrace >= 3)
    {
      // -- there is no gettid under Solaris
      // int tid = static_cast<int>(syscall(SYS_gettid));
      // fprintf(stderr, "\n   -- thread %d registering gb %zd at %p\n", tid,
      // nfinalized, (void *)G);
      fprintf(
          stderr, "\n   -- registering gb %zd at %p\n", nfinalized, (void *)G);
    }
}
//////////////////////////////////////////////////////
extern "C" void remove_SchreyerOrder(void *p, void *cd)
{
  SchreyerOrder *G = static_cast<SchreyerOrder *>(p);
  AO_t nremoved = AO_fetch_and_add1(&schorder_nremoved);
  if (M2_gbTrace >= 3)
    fprintf(stderr, "\n -- removing SchreyerOrder %zd at %p\n", nremoved, G);
  G->remove();
}
void intern_SchreyerOrder(SchreyerOrder *G)
{
  GC_REGISTER_FINALIZER(G, remove_SchreyerOrder, 0, 0, 0);
  AO_t nfinalized = AO_fetch_and_add1(&schorder_nfinalized);
  if (M2_gbTrace >= 3)
    fprintf(stderr,
            "\n   -- registering SchreyerOrder %zd at %p\n",
            nfinalized,
            (void *)G);
}
//////////////////////////////////////////////////////

extern "C" void remove_MutableMatrix(void *p, void *cd)
{
  MutableMatrix *G = static_cast<MutableMatrix *>(p);
  AO_t nremoved = AO_fetch_and_add1(&mutablematrices_nremoved);
  if (M2_gbTrace >= 3)
    fprintf(stderr, "\n -- removing mutable matrix %zd at %p\n", nremoved, G);
  G->~MutableMatrix();
}
MutableMatrix *internMutableMatrix(MutableMatrix *G)
{
  if (G == 0) return 0;
  GC_REGISTER_FINALIZER(G, remove_MutableMatrix, 0, 0, 0);
  AO_t nfinalized = AO_fetch_and_add1(&mutablematrices_nfinalized);
  if (M2_gbTrace >= 3)
    fprintf(stderr,
            "\n   -- registering mutable matrix %zd at %p\n",
            nfinalized,
            (void *)G);
  return G;
}
//////////////////////////////////////////////////////
extern long nres;
extern long nres_destruct;

extern "C" // TODO: remove when this function is in e/interface
M2_string engineMemory()
{
  buffer o;
  try
    {
      stash::stats(o);
      o << newline;

      o << "Finalizations of new resolutions:" << newline;
      o << "# of res objects constructed/deconstructed=(" << nres << ","
        << nres_destruct << ") #left = " << (nres - nres_destruct) << newline;
      o << newline;

      o << "# of GB objects       registered/finalized=(" << gbs_nfinalized
        << "," << gbs_nremoved
        << ") #left = " << (gbs_nfinalized - gbs_nremoved) << newline;

      o << "# of res objects      registered/finalized=(" << res_nfinalized
        << "," << res_nremoved
        << ") #left = " << (res_nfinalized - res_nremoved) << newline;

      o << "# of computations     registered/finalized=(" << comp_nfinalized
        << "," << comp_nremoved
        << ") #left = " << (comp_nfinalized - comp_nremoved) << newline;

      o << newline;

      o << "# of monomial ideals  registered/finalized=("
        << monideals_nfinalized << "," << monideals_nremoved
        << ") #left = " << (monideals_nfinalized - monideals_nremoved)
        << newline;

      o << "# of mutable matrices registered/finalized=("
        << mutablematrices_nfinalized << "," << mutablematrices_nremoved
        << ") #left = "
        << (mutablematrices_nfinalized - mutablematrices_nremoved) << newline;

      o << "# of polynomial rings registered/finalized=("
        << polyrings_nfinalized << "," << polyrings_nremoved
        << ") #left = " << (polyrings_nfinalized - polyrings_nremoved)
        << newline;

      o << "# of schreyer orders  registered/finalized=(" << schorder_nfinalized
        << "," << schorder_nremoved
        << ") #left = " << (schorder_nfinalized - schorder_nremoved) << newline;

      return o.to_string();
  } catch (const exc::engine_error& e)
    {
      o << "Internal error: [unprintable memory display]";
      return o.to_string();
  }
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
