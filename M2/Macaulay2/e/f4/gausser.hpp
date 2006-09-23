#ifndef __gausser_h_
#define __gausser_h_

#include "../ring.hpp"
#include "../z_mod_p.hpp"
#include "../coeffrings.hpp"

typedef void *F4CoefficientArray;

class Gausser : public our_new_delete
{
  enum {ZZp} typ;
  const Ring *K;

  CoefficientRingZZp *Kp;

  Gausser(const Z_mod *K0);
public:
  ~Gausser() {}

  static Gausser *newGausser(const Ring *K);

  const Ring * get_ring() const { return K; }

  F4CoefficientArray from_ringelem_array(int len, ring_elem *elems) const;

  void to_ringelem_array(int len, F4CoefficientArray, ring_elem *result) const;

  // other routines:
  void remove_array(F4CoefficientArray a) const;

  // reduce mod p. (QQ --> ZZ/p) (place in double's??)

  // other reductions

  // combine them (ZZ/p1, ..., ZZ/pn --> QQ)

  // Hensel lift routine?

  // evaluation of multivariate poly's or fcn's.

#if 0
  // Not ready yet.
  void allocate_dense_row_zero(int nelems, dense_row &r);
#endif
  // create a row of 0's (over K).

  void fill_from_sparse(F4CoefficientArray dense, 
			F4CoefficientArray sparse,
			int len,
			int *comps,
			int &first,
			int &last);
  // Fills 'dense' from 'sparse' (and 'comps'), setting first and last.

  void subtract_sparse(F4CoefficientArray dense, 
		       F4CoefficientArray sparse,
		       int len,
		       int *comps,
		       int &pivot,
		       int &last);
  // dense += c * sparse, where c is chosen to cancel column comps[0].
  // There should also be a version of this routine which records this value c
  // into a F4CoefficientArray.

  void to_sparse_array(F4CoefficientArray dense,
		       F4CoefficientArray &result_sparse,
		       int &result_len,
		       int *&result_comps);

  
};

#endif

// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
//  End:
