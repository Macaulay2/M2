// (c) 1995  Michael E. Stillman

#include "intarray.hpp"

void intarray::expand(int newtop)
{
  int *tmp =
      reinterpret_cast<int *>(doubles->new_elem(sizeof(int) * (newtop + 1)));
  for (int j = 0; j < max; j++) tmp[j] = entries[j];
  doubles->delete_elem(entries);
  entries = tmp;
  len = static_cast<int>(doubles->allocated_size(entries) / sizeof(int));
}

void intarray::text_out(buffer &o) const
{
  o << '[';
  for (int i = 0; i < max - 1; i++) o << entries[i] << ' ';
  if (max > 0) o << entries[max - 1];
  o << ']';
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
