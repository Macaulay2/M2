//
#include <NTL/mat_ZZ.h>
#include "ntl_interface.hpp"
void makeNTLMatrixZZ(int nrows, int ncols)
{
  mat_ZZ *X = new mat_ZZ;
  X->SetDims(nrows,ncols);
}
