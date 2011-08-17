// Copyright 2011 Michael E. Stillman

#include "aring-zzp.hpp"

namespace M2 {
  int ARingZZp::findPrimitiveRoot(int P)
  {
    int i,j,q;

    int prim_root;
    if (P==2)
      prim_root = 1;
    else
      {
	j=1;
	for (i=2; (i<P && j<P-1); i++)
	  for (q=i,j=1; (q!=1 && j<P); q=(q*i)%P,j++);
	prim_root = i-1;
      }
    return prim_root;
  }

  void ARingZZp::initialize_tables()
  {
    int i,j,q,n;
    
    prim_root = findPrimitiveRoot(p);

    log_table = new int[p]; // newarray_atomic(int,p);
    exp_table = new int[p]; // newarray_atomic(int,p);
    for (i=0, n=1; i<p-1; i++, n=(n*prim_root)%p)
      {
	log_table[n] = i;  // i = log_(base _prim_root)(n)
	exp_table[i] = n;  // n = (_prim_root)^i 
      }
    exp_table[0] = 0;
    log_table[0] = 0;
  }

  ARingZZp::ARingZZp(int p0)
    : p(p0),
      p1(p-1)
  {
    if (p==2)
      minus_one = 1;
    else
      minus_one = (p-1)/2;

    initialize_tables();
  }
  
  
};

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// End:
