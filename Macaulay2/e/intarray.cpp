// (c) 1995  Michael E. Stillman

#include "intarray.hpp"
#include "bin_io.hpp"

stash *intarray::mystash;

void intarray::expand(int newtop)
{
  int *tmp = (int *) doubles->new_elem(sizeof(int)*(newtop+1));
  for (int j = 0; j<max; j++) tmp[j] = entries[j];
  doubles->delete_elem(entries);
  entries = tmp;
  len = doubles->allocated_size(entries)/sizeof(int);
}

void intarray::bin_out(buffer &o) const
{
  bin_int_out(o, max);
  for (int i=0; i<max; i++)
    bin_int_out(o, entries[i]);
}
void intarray::text_out(buffer &o) const
{
  o << '[';
  for (int i=0; i<max-1; i++)
      o << entries[i] << ' ';
  if (max > 0) o << entries[max-1];
  o << ']';
}
