//
#include <NTL/mat_ZZ.h>
#include "ntl_interface.hpp"
void makeNTLMatrixZZ(int nrows, int ncols)
{
  mat_ZZ *X = new mat_ZZ;	// this uses builtin new, so is probably a memory leak.
  X->SetDims(nrows,ncols);
}

//mat_ZZ *NTLMatrix(const MutableMatrix *M)
//{
//  // Createt the matrix.
//  // Change the gmp alloc routines
//  // Rip through and set the matrix
//  // register a finalizer with gc
//  // now we can return.
//}
