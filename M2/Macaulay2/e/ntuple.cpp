// Copyright 1997 Michael E. Stillman

#include "ntuple.hpp"
#include "style.hpp"
#include "array.hpp"
#include "text_io.hpp"
#include "bin_io.hpp"

void ntuple::elem_text_out(buffer &o, 
			   int nvars,
			   const int *a, 
			   const array<char *> &varnames)
{
  int len = 0;
  for (int v=0; v<nvars; v++)
    if (a[v] != 0) {
      len++;
      o << varnames[v];
      int e = a[v];
      int single = (varnames[v][1] == '\0');
      if (e > 1 && single) o << e;
      else if (e > 1) o << "^" << e;
      else if (e < 0) o << "^(" << e << ")";	
    }
  if (len == 0 && p_one) o << "1";
}

void ntuple::elem_bin_out(buffer &o, int nvars, const int *a)
{
  int i;
  int len = 0;
  for (i=0; i<nvars; i++)
    if (a[i] != 0) len++;

  bin_int_out(o, len);
  for (i=0; i<nvars; i++)
    if (a[i] != 0)
      {
	bin_int_out(o, i);
	bin_int_out(o, a[i]);
      }
}
