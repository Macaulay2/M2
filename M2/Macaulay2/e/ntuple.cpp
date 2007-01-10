// Copyright 1997 Michael E. Stillman

#include "ntuple.hpp"
#include "text-io.hpp"

void ntuple::elem_text_out(buffer &o, 
			   unsigned int nvars,
			   const int *a, 
			   M2_stringarray varnames)
{
  int len = 0;
  for (unsigned int v=0; v<nvars; v++)
    if (a[v] != 0) {
      len++;
	if (varnames->len < v)
	  o << ".";
	else
	  o << varnames->array[v];
      int e = a[v];
      int single = (varnames->array[v]->len == 1);
      if (e > 1 && single) o << e;
      else if (e > 1) o << "^" << e;
      else if (e < 0) o << "^(" << e << ")";	
    }
  if (len == 0 && p_one) o << "1";
}


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
