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
  slab *last_slab;
  T *next_free; /* points into last_slab */
  
 private:
  slab *new_slab();

 public:
  MemoryBlock();
  ~MemoryBlock();

  T * reserve(int len); // returns space for len T's.
  void intern(int len); // increments 
  T * allocate(int len=1); // reserve and intern

  int n_slabs() const;
};

#include "memblock.cpp"

#endif

// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
//  End:
