// Copyright 1998  Michael E. Stillman

#ifndef _LLL_hpp_
#define _LLL_hpp_

#include "mutablemat.hpp"
#include "comp.hpp"

#include "relem.hpp"

class LLLoperations
{
  static bool checkThreshold(ring_elem num, ring_elem den);
  static bool setThreshold(const RingElement *threshold, ring_elem& num, ring_elem &den);

  static bool Lovasz(MutableMatrix *lambda,
		     int k,
		     ring_elem alphaTop,
		     ring_elem alphaBottom);

  static void REDI(int k, int ell,
		   MutableMatrix *A,
		   MutableMatrix *lambda);
  static void SWAPI(int k, int kmax,
		    MutableMatrix *A,
		    MutableMatrix *lambda);
public:
  static bool initializeLLL(const Matrix *m,
			    const RingElement *threshold,
			    bool useChangeOfBasisMatrix,
			    MutableMatrix *& A,
			    MutableMatrix *& LLLstate);
  static bool initializeLLL(const MutableMatrix *A,
			    const RingElement *threshold,
			    MutableMatrix *& LLLstate);
  // Returns false if there is an error, and sets gError.

  static int doLLL(MutableMatrix *A,
		   MutableMatrix *LLLstate,
		   int nsteps=-1);
  // Return values: COMP_DONE, COMP_DONE_STEPS, COMP_INTERRUPTED

  static void setMultipliers(const MutableMatrix *A, MutableMatrix *lambda);
  static bool isLLL(const Matrix *m, const RingElement *threshold);
  static bool isLLL(const MutableMatrix *A, const RingElement *threshold);

  ///////////////////////////
  // packaged LLL routines //
  ///////////////////////////
  
  // These routines return false if the computation is interrupted
  static bool LLL(MutableMatrix *M, const RingElement *threshold);
  // M is replaced with the LLL basis. false is returned if there was an 
  // error or an interrupt.

  static bool LLL(const Matrix *m, const RingElement *threshold, Matrix *&LLLbasis);
  static bool LLL(const Matrix *m, const RingElement *threshold, Matrix *&LLLbasis, Matrix * &ChangeOfBasis);
};
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
