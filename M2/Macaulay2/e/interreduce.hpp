/* Copyright 2010, Michael E. Stillman */

#ifndef _interreduce_h_
#define _interreduce_h_

#include "gbring.hpp"

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
