// Copyright 1998  Michael E. Stillman

#ifndef _LLL_hpp_
#define _LLL_hpp_

#include "sparsemat.hpp"
#include "comp.hpp"

class LLLoperations
{
  static bool checkThreshold(ring_elem num, ring_elem den);
  static bool setThreshold(const RingElement threshold, ring_elem& num, ring_elem &den);

  static bool Lovasz(SparseMutableMatrix *lambda,
		     int k,
		     ring_elem alphaTop,
		     ring_elem alphaBottom);

  static void REDI(int k, int ell,
		   SparseMutableMatrix *A,
		   SparseMutableMatrix *lambda);
  static void SWAPI(int k, int kmax,
		    SparseMutableMatrix *A,
		    SparseMutableMatrix *lambda);
public:
  static bool initializeLLL(const Matrix &m,
			    RingElement threshold,
			    bool useChangeOfBasisMatrix,
			    SparseMutableMatrix *& A,
			    SparseMutableMatrix *& LLLstate);
  static bool initializeLLL(const SparseMutableMatrix *A,
			    RingElement threshold,
			    SparseMutableMatrix *& LLLstate);
  // Returns false if there is an error, and sets gError.

  static int doLLL(SparseMutableMatrix *A,
		   SparseMutableMatrix *LLLstate,
		   int nsteps=-1);
  // Return values: COMP_DONE, COMP_DONE_STEPS, COMP_INTERRUPTED

  static void setMultipliers(const SparseMutableMatrix *A, SparseMutableMatrix *lambda);
  static bool isLLL(const Matrix &m, const RingElement &threshold);
  static bool isLLL(const SparseMutableMatrix *A, const RingElement &threshold);

  ///////////////////////////
  // packaged LLL routines //
  ///////////////////////////
  
  // These routines return false if the computation is interrupted
  static bool LLL(const Matrix &m, const RingElement &threshold, Matrix &LLLbasis);
  static bool LLL(const Matrix &m, const RingElement &threshold, Matrix &LLLbasis, Matrix &ChangeOfBasis);
};
#endif
