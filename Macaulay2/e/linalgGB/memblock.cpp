/////////////////
// MemoryBlock //
/////////////////

#include "memblock.hpp"

MemoryBlock::MemoryBlock()
  : first_slab(0),
    last_slab(0),
    next_free(0)
{
  first_slab = new_slab();
  last_slab = first_slab;
  next_free = last_slab->block;
}

MemoryBlock::~MemoryBlock()
{
  // Destroy the slabs one by one
  while (first_slab != 0)
    {
      slab *tmp = first_slab;
      first_slab = first_slab->next;
      deleteitem(tmp);
    }
}

MemoryBlock::slab *MemoryBlock::new_slab()
{
  slab *result = new slab;
  result->next = 0;
  return result;
}

int * MemoryBlock::reserve(int len)
{
  if (next_free + len > last_slab->block + NSLAB)
    {
      last_slab->next = new slab;
      last_slab = last_slab->next;
      next_free = last_slab->block;
    }
  return next_free;
}

void MemoryBlock::intern(int len)
{
  next_free += len;
}

int MemoryBlock::n_slabs() const
{
  int result = 0;
  for (slab *p = first_slab; p != 0; p = p->next) result++;
  return result;
}

/*
// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e/linalgGB "
//  End:
*/
