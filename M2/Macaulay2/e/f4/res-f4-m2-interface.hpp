#ifndef __res_f4_to_m2_interface_hpp_
#define __res_f4_to_m2_interface_hpp_

#include "../engine-includes.hpp"

#include "interface/groebner.h"
#include "res-schreyer-frame.hpp"
#include "res-gausser.hpp"
#include "res-poly-ring.hpp"
#include "../dmat.hpp"

class MonomialInfo;
class ResGausser;
class Polynomial;  // vector in a free module

class ResF4toM2Interface
{
 public:
  static void from_M2_vec(const ResPolyRing& R,
                          const FreeModule* F,
                          vec v,
                          poly& result);

  static vec to_M2_vec(const ResPolyRing& R,
                       const poly& f,
                       const FreeModule* F);

  static FreeModule* to_M2_freemodule(const PolynomialRing* R,
                                      SchreyerFrame& C,
                                      int lev);

  static FreeModule* to_M2_freemodule(const PolynomialRing* R,
                                      const FreeModule* F,
                                      SchreyerFrame& C,
                                      int lev);

  static Matrix* to_M2_matrix(SchreyerFrame& C,
                              int lev,
                              const FreeModule* tar,
                              const FreeModule* src);

  static MutableMatrix* to_M2_MutableMatrix(SchreyerFrame& C,
                                            const Ring* R,  // a polynomial
                                                            // ring, same monoid
                                                            // as C, coeffs
                                                            // allowed depend on
                                                            // ring of C.
                                            int lev);

  static MutableMatrix* to_M2_MutableMatrix(
      SchreyerFrame& C,
      const Ring* K,  // should be a ZZ/p ring.
      int lev,
      int degree);

  template <typename RingType>
  static double setDegreeZeroMap(SchreyerFrame& C,
                                 DMat<RingType>& result,
                                 int slanted_degree,
                                 int lev);

  static std::pair<Matrix*,double> setDegreeZeroMap(SchreyerFrame& C,
                                                    int slanted_degree,
                                                    int lev);

};

#endif

// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
//  End:
