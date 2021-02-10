// Copyright 1997-2006 Michael E. Stillman

#include "ntuple-monomial.hpp"
#include "buffer.hpp"

void ntuple_monomials::elem_text_out(buffer &o,
                                     unsigned int nvars,
                                     const_ntuple_monomial a,
                                     M2_ArrayString varnames,
                                     bool p_one)
{
  int len = 0;
  for (unsigned int v = 0; v < nvars; v++)
    if (a[v] != 0)
      {
        len++;
        if (varnames->len < v)
          o << ".";
        else
          o << varnames->array[v];
        ntuple_word e = a[v];
        bool single = (varnames->array[v]->len == 1);
        if (e > 1 && single)
          o << e;
        else if (e > 1)
          o << "^" << e;
        else if (e < 0)
          o << "^(" << e << ")";
      }
  if (len == 0 && p_one) o << "1";
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
