/* Copyright 2010, Michael E. Stillman */

#include "interreduce.hpp"
#include "freemod.hpp"
#include "text-io.hpp"

Interreducer::Interreducer(GBRing *R0,
                           FreeModule *F0,
                           VECTOR(gbvector *) & elems0)
{
}

void Interreducer::showElem(int i, int nterms)
{
  buffer o;
  R->gbvector_text_out(o, F, G[i], nterms);
  emit(o.str());
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e"
// indent-tabs-mode: nil
// End:
