// Copyright 2013  Michael E. Stillman

#ifndef _mat_util_hpp_
#define _mat_util_hpp_

// Functions for all mutable matrices, or that don't fit in
// mat-elem-ops, mat-arith, or mat-linalg

#include "buffer.hpp"
#include "text-io.hpp"

template <typename Mat>
void displayMat(buffer& o, const Mat& A)
{
  // Assumption: Mat is either DMat<RingType> or SMat<RingType>, in that it
  // defines the following:
  // Mat::ElementType
  // A.ring()
  // A.numRows(), A.numColumns()
  // A.entry(r,c)
  // and A.ring().elem_text_out

  size_t nrows = A.numRows();
  size_t ncols = A.numColumns();
  buffer* p = new buffer[nrows];
  size_t r;
  for (size_t c = 0; c < ncols; c++)
    {
      size_t maxcount = 0;
      for (r = 0; r < nrows; r++)
        {
          const typename Mat::ElementType& a = A.entry(r, c);
          if (!A.ring().is_zero(a))
            A.ring().elem_text_out(p[r], a, true, false, false);
          else
            p[r] << ".";
          if (p[r].size() > maxcount) maxcount = p[r].size();
        }
      for (r = 0; r < nrows; r++)
        for (size_t k = maxcount + 1 - p[r].size(); k > 0; k--) p[r] << ' ';
    }
  for (r = 0; r < nrows; r++)
    {
      p[r] << '\0';
      char* s = p[r].str();
      o << s << newline;
    }
  delete[] p;
}

template <typename Mat>
void displayMat(const Mat& A)
{
  buffer o;
  displayMat(o, A);
  emit(o.str());
}

template <typename Mat>
static void concatenateMatrices(const Mat& A, const Mat& B, Mat& C)
{
  assert(A.numRows() == B.numRows());
  C.resize(A.numRows(), A.numColumns() + B.numColumns());
  for (long r = 0; r < A.numRows(); r++)
    for (long c = 0; c < A.numColumns(); c++)
      A.ring().set(C.entry(r, c), A.entry(r, c));
  for (long r = 0; r < A.numRows(); r++)
    for (long c = 0; c < B.numColumns(); c++)
      A.ring().set(C.entry(r, c + A.numColumns()), B.entry(r, c));
}

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
