// Copyright 2014  Michael E. Stillman

#ifndef _flintqq_mat_hpp_
#define _flintqq_mat_hpp_

/**
 * @file dmat-qq-interface-flint.hpp
 * @brief Translation bridge that lets GMP-backed `DMat<ARingQQ>` borrow FLINT matrix arithmetic.
 *
 * Declares `FlintZZMat` (and the corresponding `FlintQQMat` used
 * by neighbouring LU code), small RAII wrappers that hold a FLINT
 * `fmpz_mat_t` / `fmpq_mat_t` and copy entries in from a
 * `DMat<M2::ARingQQ>` (or `DMatZZGMP`) on construction so the work
 * itself can run through FLINT's fast `fmpz_mat_*` / `fmpq_mat_*`
 * routines. After the FLINT call the wrapper provides a back-copy
 * path to land the result in the engine's native matrix type, and
 * the destructor releases the FLINT storage.
 *
 * The layer exists because the default `ARingQQ` alias still
 * resolves to `ARingQQGMP`, so `DMat<ARingQQ>` stores `mpq_t`
 * values --- but FLINT's matrix routines need FLINT storage to
 * deliver their speedups. Once the default `ARingQQ` flips to
 * `ARingQQFlint` (see `aring-qq.hpp`), callers can talk to
 * `dmat-qq-flint.hpp` directly and this translation file can be
 * removed.
 *
 * @see dmat-qq-flint.hpp
 * @see aring-qq.hpp
 * @see aring-qq-gmp.hpp
 */

// This class is designed to use DMat<M2::ARingQQ>, which stores elements as gmp
// ints
// This sets up flint fmpq_mat matrices, and provides translation.  This is
// significantly faster than doing the operations in a naive manner.
// This will become un-needed once DMat<ARingQQ> starts using flint
// integers/rationals.

/**
 * @brief RAII wrapper around FLINT's `fmpz_mat_t` for translating dense
 * `ZZ`-coefficient matrices between the engine and FLINT.
 *
 * @details Constructed from a `DMatZZGMP` (or empty with given dimensions),
 * the wrapper holds an initialised `fmpz_mat_t` for the duration
 * of its lifetime and frees it via `fmpz_mat_clear` in the
 * destructor. `to_fmpz_mat` / `from_fmpz_mat` translate entry by
 * entry via `fmpz_set_mpz` / `fmpz_get_mpz`. Used so FLINT's
 * matrix routines (HNF, determinant, ...) can run against engine
 * matrices that store coefficients as GMP `mpz`s --- a stopgap
 * until `DMat<ARingQQ>` is rebased onto FLINT integers directly.
 */
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
      for (long c = 0; c < mat.numColumns(); c++)
      {
        fmpz_set_mpz(fmpz_mat_entry(result_mat, r, c), & mat1.entry(r,c));
      }
  }

  static void from_fmpz_mat(fmpz_mat_t mat, DMatZZGMP& result_mat)
  {
    for (long r = 0; r < result_mat.numRows(); r++)
      for (long c = 0; c < result_mat.numColumns(); c++)
      {
        fmpz_get_mpz(& result_mat.entry(r,c), fmpz_mat_entry(mat, r, c));
      }
  }
};

/**
 * @brief RAII wrapper around FLINT's `fmpq_mat_t` for translating dense
 * `QQ`-coefficient matrices between the engine and FLINT.
 *
 * @details Rational counterpart of `FlintZZMat`: bridges
 * `DMat<M2::ARingQQ>` (GMP-backed) and FLINT's `fmpq_mat_t` via
 * entrywise translation, frees the FLINT matrix in the destructor,
 * and exposes `value()` so callers can hand the underlying
 * `fmpq_mat_struct*` straight to FLINT's matrix routines.
 */
class FlintQQMat
{
 private:
  fmpq_mat_t mMatrix;
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
  static void to_fmpq_mat(const DMatQQ& mat1, fmpq_mat_t result_mat)
  {
    DMatQQ& mat = const_cast<DMatQQ&>(mat1);
    for (long r = 0; r < mat.numRows(); r++)
      for (long c = 0; c < mat.numColumns(); c++)
        {
          fmpq_set_mpq(fmpq_mat_entry(result_mat, r, c), & mat1.entry(r,c));
        }
  }

  static void from_fmpq_mat(fmpq_mat_t mat, DMatQQ& result_mat)
  {
    for (long r = 0; r < result_mat.numRows(); r++)
      for (long c = 0; c < result_mat.numColumns(); c++)
      {
        fmpq_get_mpq(& result_mat.entry(r,c), fmpq_mat_entry(mat, r, c));
      }
  }
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
