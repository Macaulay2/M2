/* Copyright 2005, Michael E. Stillman */

#ifndef _F4_h_
#define _F4_h_

#include "../newdelete.hpp"

template<typename CoeffRing, typename MonomialInfo>
class F4 : public our_new_delete
{
  typedef typename CoeffRing::ring_type RingType;
  typedef typename CoeffRing::elem elem;
  typedef typename CoeffRing::elem COEFF_TYPE;

  typedef typename MonomialInfo::monomial monomial;

  struct mypoly : public our_new_delete 
  {
    int len;
    COEFF_TYPE *coeffs;
    monomial *monoms;
  };

private:

public:
  F4(const RingType *K,
	   const Matrix *m, 
	   M2_bool collect_syz, 
	   int n_rows_to_keep,
	   M2_arrayint gb_weights,
	   int strategy, 
	   M2_bool use_max_degree,
	   int max_degree);

  virtual ~F4();
};

#endif


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
