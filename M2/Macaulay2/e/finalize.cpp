// Copyright 2010 Michael E. Stillman.

#include "finalize.hpp"
#include "engine-includes.hpp"

#include <atomic>

#include "monideal.hpp"
#include "comp-gb.hpp"
#include "comp-res.hpp"
#include "schorder.hpp"
#include "mat.hpp"

#include <M2/gc-include.h>

#ifdef MEMDEBUG
#include "memdebug.h"
#endif

static volatile std::atomic<int> monideals_nfinalized = 0;
static volatile std::atomic<int> monideals_nremoved = 0;

static volatile std::atomic<int> mutablematrices_nfinalized = 0;
static volatile std::atomic<int> mutablematrices_nremoved = 0;

static volatile std::atomic<int> polyrings_nfinalized = 0;
static volatile std::atomic<int> polyrings_nremoved = 0;

static volatile std::atomic<int> gbs_nfinalized = 0;
static volatile std::atomic<int> gbs_nremoved = 0;

static volatile std::atomic<int> res_nfinalized = 0;
static volatile std::atomic<int> res_nremoved = 0;

static volatile std::atomic<int> comp_nfinalized = 0;
static volatile std::atomic<int> comp_nremoved = 0;

static volatile std::atomic<int> schorder_nfinalized = 0;
static volatile std::atomic<int> schorder_nremoved = 0;

//////////////////////////////////////////////////////
extern "C" void remove_monideal(void *p, void *cd)
{
#ifdef MEMDEBUG
  p = M2_debug_to_inner(p);
#endif
  MonomialIdeal *G = static_cast<MonomialIdeal *>(p);
  std::atomic<int> nremoved = monideals_nremoved++;
  if (M2_gbTrace >= 3)
    fprintf(stderr, "\n -- removing monomial ideal %d at %p\n", nremoved.load(),
            G);
  G->remove_MonomialIdeal();
}
void intern_monideal(MonomialIdeal *G)
{
#ifdef MEMDEBUG
  GC_REGISTER_FINALIZER(M2_debug_to_outer(G), remove_monideal, 0, 0, 0);
#else
  GC_REGISTER_FINALIZER(G,                    remove_monideal, 0, 0, 0);
#endif
  std::atomic<int> nfinalized = monideals_nfinalized++;
  if (M2_gbTrace >= 3)
    fprintf(stderr,
            "\n   -- registering monomial ideal %d at %p\n",
            nfinalized.load(),
            (void *)G);
}
//////////////////////////////////////////////////////
// extern "C" void remove_polyring(void *p, void *cd)
// {
// #ifdef MEMDEBUG
//   p = M2_debug_to_inner(p);
// #endif
//   PolynomialRing *G = static_cast<PolynomialRing *>(p);
//   AO_t nremoved = AO_fetch_and_add1(&polyrings_nremoved);
//   if (M2_gbTrace >= 3)
//     fprintf(stderr, "\n -- removing polynomial ring %zd at %p\n", nremoved, G);
//   G->clear();
// }
void intern_polyring(const PolynomialRing *G)
{
  // We are already setting a finalizer for rings in newdelete.hpp,
  // I believe this one is just unsetting that one.
  return;
// #ifdef MEMDEBUG
//   GC_REGISTER_FINALIZER_IGNORE_SELF(M2_debug_to_outer(const_cast<PolynomialRing *>(G)), remove_polyring, 0, 0, 0);
// #else
//   GC_REGISTER_FINALIZER_IGNORE_SELF(                  const_cast<PolynomialRing *>(G) , remove_polyring, 0, 0, 0);
// #endif
//   AO_t nfinalized = AO_fetch_and_add1(&polyrings_nfinalized);
//   if (M2_gbTrace >= 3)
//     fprintf(stderr,
//             "\n   -- registering polynomial ring %zd at %p\n",
//             nfinalized,
//             (const void *)G);
}
//////////////////////////////////////////////////////
extern "C" void remove_gb(void *p, void *cd)
{
#ifdef MEMDEBUG
  p = M2_debug_to_inner(p);
#endif
  GBComputation *G = static_cast<GBComputation *>(p);
  std::atomic<int> nremoved = gbs_nremoved++;
  if (M2_gbTrace >= 3)
    fprintf(stderr, "\n --removing gb %d at %p\n", nremoved.load(), G);
  G->remove_gb();
}
void intern_GB(GBComputation *G)
{
#ifdef MEMDEBUG
  GC_REGISTER_FINALIZER_IGNORE_SELF(M2_debug_to_outer(G), remove_gb, 0, 0, 0);
#else
  GC_REGISTER_FINALIZER_IGNORE_SELF(                  G , remove_gb, 0, 0, 0);
#endif
  std::atomic<int> nfinalized = gbs_nfinalized++;
  if (M2_gbTrace >= 3)
    {
      // -- there is no gettid under Solaris
      // int tid = static_cast<int>(syscall(SYS_gettid));
      // fprintf(stderr, "\n   -- thread %d registering gb %zd at %p\n", tid,
      // nfinalized, (void *)G);
      fprintf(
          stderr, "\n   -- registering gb %d at %p\n", nfinalized.load(),
          (void *)G);
    }
}
//////////////////////////////////////////////////////
extern "C" void remove_res(void *p, void *cd)
{
#ifdef MEMDEBUG
  p = M2_debug_to_inner(p);
#endif
  ResolutionComputation *G = static_cast<ResolutionComputation *>(p);
  std::atomic<int> nremoved = res_nremoved++;
  if (M2_gbTrace >= 3)
    fprintf(stderr, "\n -- removing res %d at %p\n", nremoved.load(), G);
  delete G;
}
void intern_res(ResolutionComputation *G)
{
#ifdef MEMDEBUG
  GC_REGISTER_FINALIZER(M2_debug_to_outer(G), remove_res, 0, 0, 0);
#else
  GC_REGISTER_FINALIZER(                  G , remove_res, 0, 0, 0);
#endif
  std::atomic<int> nfinalized = res_nfinalized++;
  if (M2_gbTrace >= 3)
    fprintf(
        stderr, "\n   -- registering res %d at %p\n", nfinalized.load(),
        (void *)G);
}
//////////////////////////////////////////////////////
extern "C" void remove_SchreyerOrder(void *p, void *cd)
{
#ifdef MEMDEBUG
  p = M2_debug_to_inner(p);
#endif
  SchreyerOrder *G = static_cast<SchreyerOrder *>(p);
  std::atomic<int> nremoved = schorder_nremoved++;
  if (M2_gbTrace >= 3)
    fprintf(stderr, "\n -- removing SchreyerOrder %d at %p\n", nremoved.load(),
            G);
  G->remove();
}
void intern_SchreyerOrder(SchreyerOrder *G)
{
#ifdef MEMDEBUG
  GC_REGISTER_FINALIZER(M2_debug_to_outer(G), remove_SchreyerOrder, 0, 0, 0);
#else
  GC_REGISTER_FINALIZER(                  G , remove_SchreyerOrder, 0, 0, 0);
#endif
  std::atomic<int> nfinalized = schorder_nfinalized++;
  if (M2_gbTrace >= 3)
    fprintf(stderr,
            "\n   -- registering SchreyerOrder %d at %p\n",
            nfinalized.load(),
            (void *)G);
}
//////////////////////////////////////////////////////

extern "C" void remove_MutableMatrix(void *p, void *cd)
{
#ifdef MEMDEBUG
  p = M2_debug_to_inner(p);
#endif
  MutableMatrix *G = static_cast<MutableMatrix *>(p);
  std::atomic<int> nremoved = mutablematrices_nremoved++;
  if (M2_gbTrace >= 3)
    fprintf(stderr, "\n -- removing mutable matrix %d at %p\n", nremoved.load(),
            G);
  G->~MutableMatrix();
}
MutableMatrix *internMutableMatrix(MutableMatrix *G)
{
  if (G == 0) return 0;
#ifdef MEMDEBUG
  GC_REGISTER_FINALIZER(M2_debug_to_outer(G), remove_MutableMatrix, 0, 0, 0);
#else
  GC_REGISTER_FINALIZER(                  G,  remove_MutableMatrix, 0, 0, 0);
#endif
  std::atomic<int> nfinalized = mutablematrices_nfinalized++;
  if (M2_gbTrace >= 3)
    fprintf(stderr,
            "\n   -- registering mutable matrix %d at %p\n",
            nfinalized.load(),
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
