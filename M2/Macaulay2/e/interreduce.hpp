/* Copyright 2010, Michael E. Stillman */

#ifndef _interreduce_h_
#define _interreduce_h_

/**
 * @file interreduce.hpp
 * @brief `Interreducer` --- skeleton class for inter-reducing a list of `gbvector*` elements (currently unused).
 *
 * Declares `Interreducer`, the intended helper for bringing a
 * collection of `gbvector*` polynomials into a head-reduced
 * state by applying smaller-pivot cancellation between any
 * pair `(f_i, f_j)` whose leading terms divide each other. The
 * class stores its working `GBRing* R`, `FreeModule* F`, and
 * the live `VECTOR(gbvector*) G`. The declared reduction
 * primitives are `cancelLT(f, g)` (cancel `lt(f)` using `g`,
 * returning the reduction count), `reduceTail(f, g)` (keep
 * going until no term of `f` is divisible by `lt(g)`), plus
 * the index-flavoured `reduceLT(f, i)` / `reduceTail(f, i)`
 * that reduce by every element of `G` except entry `i`;
 * `showElem` / `show` are debug printers.
 *
 * **Caveat:** this is orphan scaffolding. The constructor in
 * `interreduce.cpp` is a `(void) R0; (void) F0; (void) elems0;`
 * stub that does not initialise any field, and no other engine
 * translation unit references `Interreducer` --- a grep of
 * `*.cpp` / `*.hpp` finds only the declaration and the empty
 * `.cpp`. The class exists as a planned API surface but is not
 * wired into any GB driver today.
 *
 * @see gbring.hpp
 * @see reducedgb.hpp
 * @see gb-default.hpp
 */

#include "gbring.hpp"

/**
 * @brief Interreduces a list of `gbvector*`s in place so that no element's
 * leading term divides any other element's terms.
 *
 * @details Owns a `GBRing*` and a target `FreeModule*` plus the working set
 * `G` (a vector of `gbvector*`). `cancelLT(f, g)` reduces the
 * leading term of `f` by `g` until they no longer divide;
 * `reduceTail(f, g)` does the same to tail terms; the public
 * driver iterates these calls across the whole list. Used as the
 * cleanup pass after a GB computation finishes, before the result
 * is handed back to the front end.
 */
class Interreducer
{
  GBRing *R;
  FreeModule *F;
  VECTOR(gbvector *) G;

 public:
  Interreducer(GBRing *R, FreeModule *F, VECTOR(gbvector *) & elems0);

  void showElem(int i, int nterms);
  void show(int nterms);

  int cancelLT(gbvector *&f, const gbvector *g);
  // reduces f by g, until lt(f) is not divisible by lt(g).
  // returns #reductions performed.

  int reduceTail(gbvector *&f, const gbvector *g);
  // reduces f by g, until no term of f is divisible by lt(g).
  // returns #reductions performed.

  bool reduceLT(gbvector *&f, int i);
  // reduces f w.r.t G \ i.

  bool reduceTail(gbvector *&f, int i);
  // reduces f w.r.t G \ i.
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e"
// indent-tabs-mode: nil
// End:
