// Copyright 2004 Michael E. Stillman
#ifndef __memblock_h_
#define __memblock_h_

#include "../newdelete.hpp"

template<typename T, long int NSLAB = 4092>
class MemoryBlock : public our_new_delete
{
  struct slab : public our_new_delete {
    slab *next;
    T block[NSLAB];
  };

  slab *first_slab;
  slab *current_slab;
  slab *last_slab;
  T *next_free; /* points into current_slab */
  
 private:
  slab *new_slab();

 public:
  MemoryBlock();
  ~MemoryBlock();

  void reset();

  T * reserve(int len); // returns space for len T's.
  void intern(int len); // increments 
  T * allocate(int len=1); // reserve and intern

  int n_slabs() const;
};

/////////////////
// MemoryBlock //
/////////////////

template<typename T, long int NSLAB>
MemoryBlock<T,NSLAB>::MemoryBlock()
  : first_slab(0),
    current_slab(0),
    last_slab(0),
    next_free(0)
{
  first_slab = new_slab();
  current_slab = first_slab;
  last_slab = first_slab;
  next_free = current_slab->block;
}

template<typename T, long int NSLAB>
MemoryBlock<T,NSLAB>::~MemoryBlock()
{
  // Destroy the slabs one by one
  while (first_slab != 0)
    {
      slab *tmp = first_slab;
      first_slab = first_slab->next;
      deleteitem(tmp);
    }

  current_slab = 0;
  last_slab = 0;
  next_free = 0;
}

template<typename T, long int NSLAB>
typename MemoryBlock<T,NSLAB>::slab *MemoryBlock<T,NSLAB>::new_slab()
{
  slab *result = new slab;
  result->next = 0;
  return result;
}

template<typename T, long int NSLAB>
void MemoryBlock<T,NSLAB>::reset()
{
  current_slab = first_slab;
  next_free = current_slab->block;
}

template<typename T, long int NSLAB>
T * MemoryBlock<T,NSLAB>::reserve(int len)
{
  if (next_free + len > current_slab->block + NSLAB)
    {
      if (current_slab->next == 0)
	{
	  last_slab->next = new slab;
	  last_slab = last_slab->next;
	  current_slab = last_slab;
	}
      else
	{
	  current_slab = current_slab->next;
	}
      next_free = current_slab->block;
    }
  return next_free;
}

template<typename T, long int NSLAB>
void MemoryBlock<T,NSLAB>::intern(int len)
{
  next_free += len;
}

template<typename T, long int NSLAB>
T * MemoryBlock<T,NSLAB>::allocate(int len)
{
  T * result = reserve(len);
  next_free += len;
  return result;
}

template<typename T, long int NSLAB>
int MemoryBlock<T,NSLAB>::n_slabs() const
{
  int result = 0;
  for (slab *p = first_slab; p != 0; p = p->next) result++;
  return result;
}

#endif

// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
//  End:
