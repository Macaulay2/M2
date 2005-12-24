// Copyright 2004 Michael E. Stillman
#ifndef __memblock_h_
#define __memblock_h_

#include "../newdelete.hpp"

const int NSLAB = 4092;

class MemoryBlock : public our_new_delete
{
  struct slab : public our_new_delete {
    slab *next;
    long block[NSLAB];
  };

  slab *first_slab;
  slab *last_slab;
  long *next_free; /* points into last_slab */
  
 private:
  slab *new_slab();

 public:
  MemoryBlock();
  ~MemoryBlock();

  long * reserve(int len); // returns space for len ints
  void intern(int len); // increments 

  int n_slabs() const;
};

#endif

// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
//  End:
