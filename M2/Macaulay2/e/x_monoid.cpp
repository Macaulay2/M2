// (c) 1994-2002 Michael E. Stillman

#include "monoid.hpp"
#include "engine.h"

bool check_all_positive(const M2_arrayint degs)
{
  for (unsigned int i=0; i<degs->len; i++)
    if (degs->array[i] <= 0)
      {
	ERROR("all primary(first) degrees must be strictly positive");
	return false;
      }
  return true;
}

Monoid *IM2_Monoid_trivial()
{
  return Monoid::get_trivial_monoid(); // Set up in IM2_initialize()
}

Monoid *IM2_Monoid_make(MonomialOrdering *mo,
			M2_stringarray names,
			Monoid *deg_monoid,
			M2_arrayint degs)
{
  return Monoid::create(mo,names,deg_monoid,degs);
}

unsigned long IM2_Monoid_hash(Monoid *M)
{
  return M->get_hash_value();
}

M2_string IM2_Monoid_to_string(const Monoid *M)
{
  buffer o;
  M->text_out(o);
  return o.to_string();
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
