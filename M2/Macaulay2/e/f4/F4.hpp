/* Copyright 2005, Michael E. Stillman */

#ifndef _F4_h_
#define _F4_h_

#include "../newdelete.hpp"
#include "F4types.hpp"

template<typename CoeffRing, typename MonInfo>
class F4 : public our_new_delete
{
  typedef typename CoeffRing::ring_type RingType;
  typedef typename CoeffRing::elem elem;
  typedef typename CoeffRing::elem COEFF_TYPE;

  typedef typename MonInfo::monomial packed_monomial;
  typedef mypoly<COEFF_TYPE> poly;
  typedef mygbelem<COEFF_TYPE> gbelem;
  typedef gb_array<COEFF_TYPE> gb_array;

  gb_array gens; // unhandled generators.  Handled ones are replaced with 0 poly.
  gb_array gb;
public:
  F4(const CoeffRing *K,
     const MonInfo *MI,
     M2_bool collect_syz, 
     int n_rows_to_keep,
     M2_arrayint gb_weights,
     int strategy, 
     M2_bool use_max_degree,
     int max_degree);

  void set_generators(gb_array &new_gens);
  // This grabs these elements, possibly by doing a swap

  const gb_array &get_generators() const { return gens; }
  gb_array &get_generators() { return gens; }
  
  virtual ~F4();
};

#endif


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
