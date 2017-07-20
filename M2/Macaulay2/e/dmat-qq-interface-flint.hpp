// Copyright 2014  Michael E. Stillman

#ifndef _flintqq_mat_hpp_
#define _flintqq_mat_hpp_

// This class is designed to use DMat<M2::ARingQQ>, which stores elements as gmp
// ints
// This sets up flint fmpq_mat matrices, and provides translation.  This is
// significantly faster than doing the operations in a naive manner.
// This will become un-needed once DMat<ARingQQ> starts using flint
// integers/rationals.

class FlintZZMat
{
 public:
  FlintZZMat(const DMatZZGMP& mat)
  {
    fmpz_mat_init(mMatrix, mat.numRows(), mat.numColumns());
    to_fmpz_mat(mat, mMatrix);
  }
  FlintZZMat(long numrows, long numcolumns)
  {
    fmpz_mat_init(mMatrix, numrows, numcolumns);
  }

  ~FlintZZMat() { fmpz_mat_clear(mMatrix); }
  fmpz_mat_struct* value() { return mMatrix; }
  void toDMat(DMatZZGMP& result)
  {
    result.resize(fmpz_mat_nrows(mMatrix), fmpz_mat_ncols(mMatrix));
    from_fmpz_mat(mMatrix, result);
  }

  long numRows() const { return fmpz_mat_nrows(mMatrix); }
  long numColumns() const { return fmpz_mat_ncols(mMatrix); }
 private:
  fmpz_mat_t mMatrix;
  static void to_fmpz_mat(const DMatZZGMP& mat1, fmpz_mat_t result_mat)
  {
    DMatZZGMP& mat = const_cast<DMatZZGMP&>(mat1);
    for (long r = 0; r < mat.numRows(); r++)
      {
        auto end = mat.rowEnd(r);
        long c = 0;
        for (auto it = mat.rowBegin(r); it != end; ++it, ++c)
          fmpz_set_mpz(fmpz_mat_entry(result_mat, r, c), &(*it));
      }
  }
  static void from_fmpz_mat(fmpz_mat_t mat, DMatZZGMP& result_mat)
  {
    for (long r = 0; r < result_mat.numRows(); r++)
      {
        auto end = result_mat.rowEnd(r);
        long c = 0;
        for (auto it = result_mat.rowBegin(r); it != end; ++it, ++c)
          fmpz_get_mpz(&(*it), fmpz_mat_entry(mat, r, c));
      }
  }
};

class FlintQQMat
{
 public:
  typedef DMat<M2::ARingQQ> DMatQQ;

  FlintQQMat(const DMatQQ& mat)
  {
    fmpq_mat_init(mMatrix, mat.numRows(), mat.numColumns());
    to_fmpq_mat(mat, mMatrix);
  }
  FlintQQMat(long numrows, long numcolumns)
  {
    fmpq_mat_init(mMatrix, numrows, numcolumns);
  }

  ~FlintQQMat() { fmpq_mat_clear(mMatrix); }
  fmpq_mat_struct* value() { return mMatrix; }
  void toDMat(DMatQQ& result)
  {
    result.resize(fmpq_mat_nrows(mMatrix), fmpq_mat_ncols(mMatrix));
    from_fmpq_mat(mMatrix, result);
  }

  long numRows() const { return fmpq_mat_nrows(mMatrix); }
  long numColumns() const { return fmpq_mat_ncols(mMatrix); }
  void set_from_fmpz(long r, long c, fmpz_t val)
  {
    fmpz_set(fmpq_numref(fmpq_mat_entry(mMatrix, r, c)), val);
    fmpz_set_ui(fmpq_denref(fmpq_mat_entry(mMatrix, r, c)), 1);
  }

 private:
  fmpq_mat_t mMatrix;
  static void to_fmpq_mat(const DMatQQ& mat1, fmpq_mat_t result_mat)
  {
    DMatQQ& mat = const_cast<DMatQQ&>(mat1);
    for (long r = 0; r < mat.numRows(); r++)
      {
        auto end = mat.rowEnd(r);
        long c = 0;
        for (auto it = mat.rowBegin(r); it != end; ++it, ++c)
          fmpq_set_mpq(fmpq_mat_entry(result_mat, r, c), &(*it));
      }
  }
  static void from_fmpq_mat(fmpq_mat_t mat, DMatQQ& result_mat)
  {
    for (long r = 0; r < result_mat.numRows(); r++)
      {
        auto end = result_mat.rowEnd(r);
        long c = 0;
        for (auto it = result_mat.rowBegin(r); it != end; ++it, ++c)
          fmpq_get_mpq(&(*it), fmpq_mat_entry(mat, r, c));
      }
  }
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
