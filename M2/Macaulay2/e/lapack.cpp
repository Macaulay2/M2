#include "coeffrings.hpp"
#include "lapack.hpp"
#include <M2/config.h>
#include <iostream>

#include "mat-arith.hpp"

// lapack arrays are all arrays of doubles, and are
// placed in column-major order, as that is what lapack uses.
// Lapack arrays of complex numbers are done in the same way, except
// they have twice the length, and 2 contiguous doubles are used to
// represent a complex value.
// These arrays are grabbed via newarray_atomic, or newarray_atomic_clear
// and should be freed via freemem.

typedef double *LapackDoubles;

double *make_lapack_array(const DMat<M2::ARingRRR> &mat)
// changing to RR and column-major order
{
  size_t len = mat.numRows() * mat.numColumns();
  double *result = newarray_atomic(double, len);
  double *p = result;
  for (size_t c = 0; c < mat.numColumns(); c++)
    {
      auto end = mat.columnEnd(c);
      for (auto a = mat.columnBegin(c); a != end; ++a)
        *p++ = mpfr_get_d(&(*a), MPFR_RNDN);
    }
  return result;
}

double *make_lapack_array(const DMat<M2::ARingCCC> &mat)
// changing to CC and column-major order
{
  size_t len = mat.numRows() * mat.numColumns();
  double *result = newarray_atomic(double, 2 * len);
  double *p = result;
  for (size_t c = 0; c < mat.numColumns(); c++)
    {
      auto end = mat.columnEnd(c);
      for (auto a = mat.columnBegin(c); a != end; ++a)
        {
          *p++ = mpfr_get_d(&(*a).re, MPFR_RNDN);
          *p++ = mpfr_get_d(&(*a).im, MPFR_RNDN);
        }
    }
  return result;
}

void fill_from_lapack_array(const double *lapack_array,
                            DMat<M2::ARingRRR> &result)
// from RR and column-major order
{
  const double *p = lapack_array;
  for (size_t c = 0; c < result.numColumns(); c++)
    {
      auto end = result.columnEnd(c);
      for (auto a = result.columnBegin(c); a != end; ++a)
        result.ring().set_from_double(*a, *p++);
    }
}

void fill_from_lapack_array(const double *lapack_array,
                            DMat<M2::ARingCCC> &result)
{
  const double *p = lapack_array;
  for (size_t c = 0; c < result.numColumns(); c++)
    {
      auto end = result.columnEnd(c);
      for (auto a = result.columnBegin(c); a != end; ++a)
        {
          result.ring().real_ring().set_from_double((*a).re, *p++);
          result.ring().real_ring().set_from_double((*a).im, *p++);
        }
    }
}

void fill_from_lapack_upper(double *lapack_numbers,  // column-major order
                            int numrows,
                            int numcols,
                            DMat<M2::ARingRR> &upper)
// original matrix has size nrows x ncols
// lapack_numbers is an array of this size
// result: upper: size min(nrows, ncols) x ncols
//
// lapack_numbers is in column major form
// upper is in row major form
{
  // At this point, upper should be a zero matrix.
  assert(MatrixOps::isZero(upper));
#if 0  
  for (int i=0; i< numrows * numcols; ++i)
    std::cout << lapack_numbers[i] << " ";
  std::cout << std::endl;
#endif
  auto U = upper.rowMajorArray();

  for (size_t c = 0; c < upper.numColumns(); c++)
    {
      auto U1 = U;
      for (size_t r = 0; r <= c; r++)
        {
          if (r >= upper.numRows()) break;
          *U1 = lapack_numbers[r];
          U1 += upper.numColumns();
        }
      U++;  // change to next column
      lapack_numbers += numrows;
    }
}

void fill_from_lapack_upper(double *lapack_numbers,  // column-major order
                            int numrows,
                            int numcols,
                            DMat<M2::ARingCC> &upper)
// original matrix has size nrows x ncols
// lapack_numbers is an array of this size
// result: upper: size min(nrows, ncols) x ncols
//
// lapack_numbers is in column major form
// upper is in row major form
{
  // At this point, upper should be a zero matrix.
  std::cout << "entering fill_from_lapack_upper " << std::endl;
  assert(MatrixOps::isZero(upper));
#if 0  
  for (int i=0; i< numrows * numcols; ++i)
    std::cout << lapack_numbers[i] << " ";
  std::cout << std::endl;
#endif
  auto U = upper.rowMajorArray();

  for (size_t c = 0; c < upper.numColumns(); c++)
    {
      std::cout << "doing col " << c << std::endl;
      auto U1 = U;
      for (size_t r = 0; r <= c; r++)
        {
          std::cout << "doing row " << r << std::endl;
          if (r >= upper.numRows()) break;
          double re = lapack_numbers[2 * r];
          double im = lapack_numbers[2 * r + 1];
          upper.ring().set_from_doubles(*U1, re, im);

          U1 += upper.numColumns();
        }
      U++;  // change to next column
      lapack_numbers += 2 * numrows;
    }
  std::cout << "done!" << std::endl;
}

void fill_lower_and_upper(double *lapack_numbers,  // column-major order
                          DMat<M2::ARingRRR> &lower,
                          DMat<M2::ARingRRR> &upper)
// original matrix has size nrows x ncols
// lapack_numbers is an array of this size
// result: lower: size: nrows x min(nrows, ncols)
// result: upper: size min x ncols
//
// lapack_numbers is in column major form
// lower and upper are in row major form
{
  // At this point, lower and upper should be zero matrices.
  assert(MatrixOps::isZero(lower));
  assert(MatrixOps::isZero(upper));

  auto L = lower.rowMajorArray();
  auto U = upper.rowMajorArray();

  for (size_t c = 0; c < upper.numColumns(); c++)
    {
      auto U1 = U;
      for (size_t r = 0; r <= c; r++)
        {
          if (r >= upper.numRows()) break;
          upper.ring().set_from_double(*U1, *lapack_numbers++);
          U1 += upper.numColumns();
        }
      U++;  // change to next column

      if (c < lower.numColumns())
        {
          lower.ring().set_from_long(*L, 1);  // diagonal entry of L should be 1
          L += lower.numColumns();  // pointing to entry right below diagonal
          auto L1 = L;  // will increment by lower.numRows() each loop here
          for (size_t r = c + 1; r < lower.numRows(); r++)
            {
              lower.ring().set_from_double(*L1, *lapack_numbers++);
              L1 += lower.numColumns();  // to place next entry.
            }
          L++;  // change to next column
        }
    }
}

void fill_lower_and_upper(double *lapack_numbers,  // column-major order
                          DMat<M2::ARingCCC> &lower,
                          DMat<M2::ARingCCC> &upper)
// original matrix has size nrows x ncols
// lapack_numbers is an array of this size (*2)
// result: lower: size: nrows x min(nrows, ncols)
// result: upper: size min x ncols
//
// lapack_numbers is in column major form
// lower and upper are in row major form
{
  // At this point, lower and upper should be zero matrices.
  assert(MatrixOps::isZero(lower));
  assert(MatrixOps::isZero(upper));

  auto L = lower.rowMajorArray();
  auto U = upper.rowMajorArray();

  for (size_t c = 0; c < upper.numColumns(); c++)
    {
      auto U1 = U;
      for (size_t r = 0; r <= c; r++)
        {
          if (r >= upper.numRows()) break;
          double re = *lapack_numbers++;
          double im = *lapack_numbers++;
          upper.ring().set_from_doubles(*U1, re, im);
          U1 += upper.numColumns();
        }
      U++;  // change to next column

      if (c < lower.numColumns())
        {
          lower.ring().set_from_long(*L, 1);  // diagonal entry of L should be 1
          L += lower.numColumns();  // pointing to entry right below diagonal
          auto L1 = L;  // will increment by lower.numRows() each loop here
          for (size_t r = c + 1; r < lower.numRows(); r++)
            {
              double re = *lapack_numbers++;
              double im = *lapack_numbers++;
              lower.ring().set_from_doubles(*L1, re, im);
              L1 += lower.numColumns();  // to place next entry.
            }
          L++;  // change to next column
        }
    }
}

/* void printmat(int N, int M, mpreal * A, int LDA)
{
    mpreal mtmp;

    printf("[ ");
    for (int i = 0; i < N; i++) {
        printf("[ ");
        for (int j = 0; j < M; j++) {
            mtmp = A[i + j * LDA];
            mpfr_printf("%5.2Re", mpfr_ptr(mtmp));
            if (j < M - 1)
                printf(", ");
        }
        if (i < N - 1)
            printf("]; ");
        else
            printf("] ");
    }
    printf("]");
} */

M2_arrayintOrNull Lapack::LU(const LMatrixRRR *A, LMatrixRRR *L, LMatrixRRR *U)
{
  int rows = static_cast<int>(A->numRows());
  int cols = static_cast<int>(A->numColumns());
  int info;
  int min = (rows <= cols) ? rows : cols;
  M2_arrayint result = M2_makearrayint(rows);

  L->resize(rows, min);
  U->resize(min, cols);

  if (min == 0)
    {
      if (rows > 0)
        for (int i = 0; i < rows; i++) result->array[i] = i;
      return result;
    }

  int *perm = newarray_atomic(int, min);

  LapackDoubles copyA = make_lapack_array(*A);

  dgetrf_(&rows, &cols, copyA, &rows, perm, &info);

  fill_lower_and_upper(copyA, *L, *U);

  for (int i = 0; i < rows; i++) result->array[i] = i;
  for (int i = 0; i < min; i++)
    {
      int thisloc = perm[i] - 1;
      int tmp = result->array[thisloc];
      result->array[thisloc] = result->array[i];
      result->array[i] = tmp;
    }

  freemem(copyA);
  freemem(perm);

  if (info < 0)
    {
      ERROR("argument passed to dgetrf had an illegal value");
      return 0;
    }

  return result;
}

bool Lapack::solve(const LMatrixRRR *A, /* read only */
                   const LMatrixRRR *b, /* read only */
                   LMatrixRRR *x)       /* output value */
{
  int size = static_cast<int>(A->numRows());
  int bsize = static_cast<int>(b->numColumns());
  int info;

  /* make sure matrix is square */
  if (A->numRows() != A->numColumns())
    {
      ERROR("expected a square matrix");
      return false;
    }

  /* make sure dimensions of b make sense for Ax=b */
  if (b->numRows() != size)
    {
      ERROR("expected matrices to have same number of rows");
      return false;
      ;
    }

  if (size == 0)
    {
      x->resize(size, bsize);
      return true;
    }

  int *permutation = newarray_atomic(int, size);
  LapackDoubles copyA = make_lapack_array(*A);
  LapackDoubles copyb = make_lapack_array(*b);

  dgesv_(&size,
         &bsize,
         copyA,
         &size,
         permutation,
         copyb,  // also the result
         &size,
         &info);

  // Now set x
  x->resize(size, bsize);
  fill_from_lapack_array(copyb, *x);

  if (info > 0)
    {
      ERROR("according to dgesv, matrix is singular");
      return false;
    }
  else if (info < 0)
    {
      ERROR("argument passed to dgesv had an illegal value");
      return false;
    }

  return true;
}

bool Lapack::eigenvalues(const LMatrixRRR *A, LMatrixCCC *eigvals)
{
  int size = static_cast<int>(A->numRows());
  if (size != static_cast<int>(A->numColumns()))
    {
      ERROR("expected a square matrix");
      return false;
    }

  if (size == 0)
    {
      eigvals->resize(0, 1);
      return true;
    }

  bool ret = true;
  char dont = 'N';
  int wsize = 3 * size;
  double *workspace = newarray_atomic(double, wsize);
  int info;

  double *copyA = make_lapack_array(*A);
  double *real = newarray_atomic(double, size);  // real components of eigvals
  double *imag = newarray_atomic(double, size);  // imaginary components

  dgeev_(&dont,
         &dont,
         &size,
         copyA,
         &size,
         real,
         imag,
         static_cast<double *>(0),
         &size, /* left eigenvectors */
         static_cast<double *>(0),
         &size, /* right eigenvectors */
         workspace,
         &wsize,
         &info);

  if (info < 0)
    {
      ERROR("argument passed to dgeev had an illegal value");
      ret = false;
    }
  else if (info > 0)
    {
      ERROR("the QR algorithm in dgeev failed to compute all eigvals");
      ret = false;
    }
  else
    {
      eigvals->resize(size, 1);
      LMatrixCCC::ElementType *elems = eigvals->rowMajorArray();
      for (int i = 0; i < size; i++)
        eigvals->ring().set_from_doubles(elems[i], real[i], imag[i]);
    }

  freemem(copyA);
  freemem(real);
  freemem(imag);
  return ret;
}

bool Lapack::eigenvectors(const LMatrixRRR *A,
                          LMatrixCCC *eigvals,
                          LMatrixCCC *eigvecs)
{
  int size = static_cast<int>(A->numRows());
  if (size != static_cast<int>(A->numColumns()))
    {
      ERROR("expected a square matrix");
      return false;
    }

  if (size == 0)
    {
      eigvals->resize(0, 1);
      eigvecs->resize(0, 0);
      return true;
    }

  bool ret = true;
  char dont = 'N';
  char doit = 'V';
  int wsize = 4 * size;

  double *workspace = newarray_atomic(double, wsize);
  int info;

  double *copyA = make_lapack_array(*A);
  double *real = newarray_atomic(double, size);  // real components of eigvals
  double *imag = newarray_atomic(double, size);  // imaginary components
  double *eigen = newarray_atomic(double, size *size);  // eigvecs

  dgeev_(&dont, /* left e-vectors */
         &doit, /* right e-vectors */
         &size,
         copyA,
         &size,
         real,
         imag,
         static_cast<double *>(0),
         &size, /* left eigvecs */
         eigen,
         &size, /* right eigvecs */
         workspace,
         &wsize,
         &info);

  if (info < 0)
    {
      ERROR("argument passed to dgeev had an illegal value");
      ret = false;
    }
  else if (info > 0)
    {
      ERROR("the QR algorithm in dgeev failed to compute all eigvals");
      ret = false;
    }
  else
    {
      // Make the complex arrays of eigvals and eigvecs
      eigvals->resize(size, 1);
      eigvecs->resize(size, size);
      //      LMatrixCCC::ElementType* elems = eigvecs->array();
      LMatrixCCC::ElementType *elems = eigvals->rowMajorArray();
      for (int j = 0; j < size; j++)
        {
          eigvals->ring().set_from_doubles(elems[j], real[j], imag[j]);
          auto end = eigvecs->columnEnd(j);

          // mpfr_get_d(&(*a), MPFR_RNDN);
          int loc = j * size;
          if (imag[j] == 0)
            {
              auto a = eigvecs->columnBegin(j);
              for (int i = 0; a != end; ++a, ++i)
                eigvecs->ring().set_from_doubles((*a), eigen[loc + i], 0);
            }
          else if (imag[j] > 0)
            {
              auto a = eigvecs->columnBegin(j);
              auto b = eigvecs->columnBegin(j + 1);
              for (int i = 0; a != end; ++a, ++b, ++i)
                {
                  eigvecs->ring().set_from_doubles(
                      *a, eigen[loc + i], eigen[loc + size + i]);
                  eigvecs->ring().set_from_doubles(
                      *b, eigen[loc + i], -eigen[loc + size + i]);
                }
            }
        }
    }

  freemem(copyA);
  freemem(workspace);
  freemem(real);
  freemem(imag);
  freemem(eigen);
  return ret;
}

bool Lapack::eigenvalues_symmetric(const LMatrixRRR *A, LMatrixRRR *eigvals)
{
  int size = static_cast<int>(A->numRows());
  if (size != static_cast<int>(A->numColumns()))
    {
      ERROR("expected a square matrix");
      return false;
    }

  if (size == 0)
    {
      eigvals->resize(0, 1);
      return true;
    }

  bool ret = true;
  char dont = 'N';
  char triangle = 'U'; /* Upper triangular part makes symmetric matrix. */
  int info;

  int wsize = 3 * size - 1;
  double *workspace = newarray_atomic(double, wsize);

  double *copyA = make_lapack_array(*A);
  double *evals = newarray_atomic(double, size);

  dsyev_(
      &dont, &triangle, &size, copyA, &size, evals, workspace, &wsize, &info);

  if (info < 0)
    {
      ERROR("argument passed to dsyev had an illegal value");
      ret = false;
    }
  else if (info > 0)
    {
      ERROR("dsyev did not converge");
      ret = false;
    }
  else
    {
      // Copy eigenvalues back to eigvals
      eigvals->resize(size, 1);
      fill_from_lapack_array(evals, *eigvals);
    }

  freemem(workspace);
  freemem(copyA);
  freemem(evals);

  return ret;
}

bool Lapack::eigenvectors_symmetric(const LMatrixRRR *A,
                                    LMatrixRRR *eigvals,
                                    LMatrixRRR *eigvecs)
{
  int size = static_cast<int>(A->numRows());
  if (size != static_cast<int>(A->numColumns()))
    {
      ERROR("expected a square matrix");
      return false;
    }

  if (size == 0)
    {
      eigvals->resize(0, 1);
      eigvecs->resize(0, 0);
      return true;
    }

  bool ret = true;
  char doit = 'V';
  char triangle = 'U'; /* Upper triangular part makes symmetric matrix */

  int wsize = 3 * size - 1;
  double *workspace = newarray_atomic(double, wsize);
  int info;

  double *evecs = make_lapack_array(*A);
  double *evals = newarray_atomic(double, size);

  dsyev_(
      &doit, &triangle, &size, evecs, &size, evals, workspace, &wsize, &info);

  if (info < 0)
    {
      ERROR("argument passed to dsyev had an illegal value");
      ret = false;
    }
  else if (info > 0)
    {
      ERROR("dsyev did not converge");
      ret = false;
    }
  else
    {
      // Copy results to eigvals, eigvecs
      eigvecs->resize(size, size);
      fill_from_lapack_array(evecs, *eigvecs);
      eigvals->resize(size, 1);
      fill_from_lapack_array(evals, *eigvals);
    }

  freemem(workspace);
  freemem(evecs);
  freemem(evals);

  return ret;
}

bool Lapack::SVD(const LMatrixRRR *A,
                 LMatrixRRR *Sigma,
                 LMatrixRRR *U,
                 LMatrixRRR *VT)
{
  bool ret = true;
  char doit = 'A';  // other options are 'S' and 'O' for singular vectors only
  int rows = static_cast<int>(A->numRows());
  int cols = static_cast<int>(A->numColumns());
  int info;
  int min = (rows <= cols) ? rows : cols;

  if (min == 0)
    {
      ERROR("expected a matrix with positive dimensions");
      return false;
    }

  int max = (rows >= cols) ? rows : cols;
  int wsize = (3 * min + max >= 5 * min) ? 3 * min + max : 5 * min;
  double *workspace = newarray_atomic(double, wsize);

  double *copyA = make_lapack_array(*A);
  double *u = newarray_atomic(double, rows *rows);
  double *vt = newarray_atomic(double, cols *cols);
  double *sigma = newarray_atomic(double, min);

  dgesvd_(&doit,
          &doit,
          &rows,
          &cols,
          copyA,
          &rows,
          sigma,
          u,
          &rows,
          vt,
          &cols,
          workspace,
          &wsize,
          &info);

  if (info < 0)
    {
      ERROR("argument passed to dgesvd had an illegal value");
      ret = false;
    }
  else if (info > 0)
    {
      ERROR("dgesvd did not converge");
      ret = false;
    }
  else
    {
      U->resize(rows, rows);
      VT->resize(cols, cols);
      Sigma->resize(min, 1);
      fill_from_lapack_array(u, *U);
      fill_from_lapack_array(vt, *VT);
      fill_from_lapack_array(sigma, *Sigma);
    }

  freemem(workspace);
  freemem(copyA);
  freemem(u);
  freemem(vt);
  freemem(sigma);

  return ret;
}

bool Lapack::SVD_divide_conquer(const LMatrixRRR *A,
                                LMatrixRRR *Sigma,
                                LMatrixRRR *U,
                                LMatrixRRR *VT)
{
  bool ret = true;
  char doit = 'A';  // other options are 'S' and 'O' for singular vectors only
  int rows = static_cast<int>(A->numRows());
  int cols = static_cast<int>(A->numColumns());
  int info;
  int min = (rows <= cols) ? rows : cols;

  if (min == 0)
    {
      ERROR("expected a matrix with positive dimensions");
      return false;
    }

  int max = (rows >= cols) ? rows : cols;
  int wsize = 4 * min * min + max + 9 * min;
  double *workspace = newarray_atomic(double, wsize);
  int *iworkspace = newarray_atomic(int, 8 * min);

  double *copyA = make_lapack_array(*A);
  double *u = newarray_atomic(double, rows *rows);
  double *vt = newarray_atomic(double, cols *cols);
  double *sigma = newarray_atomic(double, min);

  dgesdd_(&doit,
          &rows,
          &cols,
          copyA,
          &rows,
          sigma,
          u,
          &rows,
          vt,
          &cols,
          workspace,
          &wsize,
          iworkspace,
          &info);

  if (info < 0)
    {
      ERROR("argument passed to dgesdd had an illegal value");
      ret = false;
    }
  else if (info > 0)
    {
      ERROR("dgesdd did not converge");
      ret = false;
    }
  else
    {
      U->resize(rows, rows);
      VT->resize(cols, cols);
      Sigma->resize(min, 1);
      fill_from_lapack_array(u, *U);
      fill_from_lapack_array(vt, *VT);
      fill_from_lapack_array(sigma, *Sigma);
    }

  freemem(workspace);
  freemem(iworkspace);
  freemem(copyA);
  freemem(u);
  freemem(vt);
  freemem(sigma);

  return ret;
}

bool Lapack::least_squares(const LMatrixRRR *A,
                           const LMatrixRRR *b,
                           LMatrixRRR *x)
{
  bool ret = true;
  char job = 'N';
  int info;
  int rows = static_cast<int>(A->numRows());
  int cols = static_cast<int>(A->numColumns());
  int brows = static_cast<int>(b->numRows());
  int bcols = static_cast<int>(b->numColumns());
  int min = (rows <= cols) ? rows : cols;
  int max = (rows >= cols) ? rows : cols;
  int wsize = min + ((bcols >= max) ? bcols : max);

  if (brows != rows)
    {
      ERROR("expected compatible right hand side");
      return false;
    }

  if (min == 0 || bcols == 0)
    {
      x->resize(cols, bcols);
      return true;
    }

  double *copyA = make_lapack_array(*A);
  double *copyb = make_lapack_array(*b);
  double *workspace = newarray_atomic(double, wsize);

  if (rows < cols)
    {
      // Make 'b' (copyb) into a cols x bcols matrix, by adding a zero block at
      // the bottom
      double *copyb2 = newarray_atomic_clear(double, cols *bcols);
      int bloc = 0;
      int copyloc = 0;
      for (int j = 0; j < bcols; j++)
        {
          copyloc = j * cols;
          for (int i = 0; i < brows; i++)
            {
              copyb2[copyloc++] = copyb[bloc++];
            }
        }
      freemem(copyb);
      copyb = copyb2;
    }

  dgels_(&job,
         &rows,
         &cols,
         &bcols,
         copyA,
         &rows,
         copyb,
         &max,
         workspace,
         &wsize,
         &info);

  if (info != 0)
    {
      ERROR("argument passed to dgels had an illegal value");
      ret = false;
    }
  else
    {
      x->resize(cols, bcols);
      if (rows > cols)
        {
          // We only need the first 'cols' rows of copyb
          int copyloc = 0;
          for (int j = 0; j < bcols; j++)
            {
              auto xj = x->columnBegin(j);
              copyloc = j * rows;
              for (int i = 0; i < cols; i++, ++xj)
                {
                  x->ring().set_from_double(*xj, copyb[copyloc++]);
                }
            }
        }
      else
        {
          fill_from_lapack_array(copyb, *x);
        }
    }

  freemem(copyA);
  freemem(copyb);
  freemem(workspace);

  return ret;
}

bool Lapack::least_squares_deficient(const LMatrixRRR *A,
                                     const LMatrixRRR *b,
                                     LMatrixRRR *x)
{
  bool ret = true;
  int rows = static_cast<int>(A->numRows());
  int cols = static_cast<int>(A->numColumns());
  int brows = static_cast<int>(b->numRows());
  int bcols = static_cast<int>(b->numColumns());
  double rcond = -1.0;  // use machine precision
  int rank, info;
  int min = (rows < cols) ? rows : cols;
  int max = (rows > cols) ? rows : cols;
  int tempmax = ((2 * min > max) ? 2 * min : max);
  int wsize = 3 * min + ((tempmax > bcols) ? tempmax : bcols);

  if (brows != rows)
    {
      ERROR("expected compatible right hand side");
      return false;
    }

  if (min == 0 || bcols == 0)
    {
      x->resize(cols, bcols);
      return true;
    }

  double *copyA = make_lapack_array(*A);
  double *copyb = make_lapack_array(*b);
  double *workspace = newarray_atomic(double, wsize);
  double *sing = newarray_atomic(double, min);

  if (rows < cols)
    {
      // Make 'b' (copyb) into a cols x bcols matrix, by adding a zero block at
      // the bottom
      double *copyb2 = newarray_atomic_clear(double, cols *bcols);
      int bloc = 0;
      int copyloc = 0;
      for (int j = 0; j < bcols; j++)
        {
          copyloc = j * cols;
          for (int i = 0; i < brows; i++)
            {
              copyb2[copyloc++] = copyb[bloc++];
            }
        }
      freemem(copyb);
      copyb = copyb2;
    }

  dgelss_(&rows,
          &cols,
          &bcols,
          copyA,
          &rows,
          copyb,
          &max,
          sing,
          &rcond,
          &rank,
          workspace,
          &wsize,
          &info);

  if (info != 0)
    {
      ERROR("argument passed to dgelss had an illegal value");
      ret = false;
    }
  else
    {
      x->resize(cols, bcols);
      if (rows > cols)
        {
          int copyloc = 0;
          for (int j = 0; j < bcols; j++)
            {
              auto xj = x->columnBegin(j);
              copyloc = j * rows;
              for (int i = 0; i < cols; i++, ++xj)
                {
                  x->ring().set_from_double(*xj, copyb[copyloc++]);
                }
            }
        }
      else
        {
          fill_from_lapack_array(copyb, *x);
        }
    }

  freemem(copyA);
  freemem(copyb);
  freemem(workspace);
  freemem(sing);

  return ret;
}

M2_arrayintOrNull Lapack::LU(const LMatrixCCC *A, LMatrixCCC *L, LMatrixCCC *U)
{
  int rows = static_cast<int>(A->numRows());
  int cols = static_cast<int>(A->numColumns());
  int info;
  int min = (rows <= cols) ? rows : cols;
  M2_arrayint result = M2_makearrayint(rows);

  L->resize(rows, min);
  U->resize(min, cols);

  if (min == 0)
    {
      if (rows > 0)
        for (int i = 0; i < rows; i++) result->array[i] = i;
      return result;
    }

  int *perm = newarray_atomic(int, min);

  double *copyA = make_lapack_array(*A);

  zgetrf_(&rows, &cols, copyA, &rows, perm, &info);

  if (info < 0)
    {
      ERROR("argument passed to zgetrf had an illegal value");
      freemem(result);
      result = NULL;
    }
  else
    {
      fill_lower_and_upper(copyA, *L, *U);

      for (int i = 0; i < rows; i++) result->array[i] = i;
      for (int i = 0; i < min; i++)
        {
          int thisloc = perm[i] - 1;
          int tmp = result->array[thisloc];
          result->array[thisloc] = result->array[i];
          result->array[i] = tmp;
        }

#if 0
      /* set the permutation array */
      for (int row=1; row<=min; row++) {
        int targ = row;
        for (int i=1; i<=min; i++) {
          if (i == targ)
            targ = perm[i-1];
          else if (perm[i-1] == targ)
            targ = i;
        }
        result->array[row-1] = targ-1;
      }
      for (int i=min; i<rows; i++)
        result->array[i] = i;
#endif
    }

  freemem(perm);
  freemem(copyA);

  return result;
}

bool Lapack::solve(const LMatrixCCC *A, const LMatrixCCC *b, LMatrixCCC *x)
{
  bool ret = true;
  int size = static_cast<int>(A->numRows());
  int bsize = static_cast<int>(b->numColumns());
  int info;

// TODO: MES The next 6 lines need to be removed/cleaned up
#if 0
  const CCC *CCR = A->get_ring()->cast_to_CCC();
  assert(CCR != 0);
  unsigned long precision= CCR->get_precision();
#endif
  unsigned long precision =
      53;  // Just used below in code that I think is not active.

  /* make sure matrix is square */
  if (size != static_cast<int>(A->numColumns()))
    {
      ERROR("expected a square matrix");
      return false;
    }

  /* make sure dimensions of b make sense for Ax=b */
  if (b->numRows() != size)
    {
      ERROR("expected matrices to have same number of rows");
      return false;
    }

  if (size == 0)
    {
      x->resize(size, bsize);
      return true;
    }

  if (precision > 53)
    {
      ERROR("high precision requested, but not yet implemented");
      return false;
    }

  int *permutation = newarray_atomic(int, size);
  double *copyA = make_lapack_array(*A);
  double *copyb = make_lapack_array(*b);

  zgesv_(&size, &bsize, copyA, &size, permutation, copyb, &size, &info);

  if (info > 0)
    {
      ERROR("according to zgesv, matrix is singular");
      ret = false;
    }
  else if (info < 0)
    {
      ERROR("argument passed to zgesv had an illegal value");
      ret = false;
    }
  else
    {
      x->resize(size, bsize);
      fill_from_lapack_array(copyb, *x);
    }

  freemem(permutation);
  freemem(copyA);
  freemem(copyb);

  return ret;
}

bool Lapack::eigenvalues(const LMatrixCCC *A, LMatrixCCC *eigvals)
{
  int size = static_cast<int>(A->numRows());
  if (size != static_cast<int>(A->numColumns()))
    {
      ERROR("expected a square matrix");
      return false;
    }

  if (size == 0)
    {
      eigvals->resize(0, 1);
      return true;
    }

  bool ret = true;
  char dont = 'N';
  int info;
  int wsize = 4 * size;
  int rsize = 2 * size;
  double *workspace = newarray_atomic(double, wsize);
  double *rwork = newarray_atomic(double, rsize);

  double *copyA = make_lapack_array(*A);
  double *evals = newarray_atomic(double, 2 * size);

  zgeev_(&dont,
         &dont,
         &size,
         copyA,
         &size,
         evals,
         static_cast<double *>(0),
         &size, /* left eigenvectors */
         static_cast<double *>(0),
         &size, /* right eigenvectors */
         workspace,
         &wsize,
         rwork,
         &info);

  if (info < 0)
    {
      ERROR("argument passed to zgeev had an illegal value");
      ret = false;
    }
  else if (info > 0)
    {
      ERROR("the QR algorithm in zgeev failed to compute all eigvals");
      ret = false;
    }
  else
    {
      eigvals->resize(size, 1);
      fill_from_lapack_array(evals, *eigvals);
    }

  freemem(copyA);
  freemem(evals);
  freemem(workspace);
  freemem(rwork);

  return ret;
}

bool Lapack::eigenvectors(const LMatrixCCC *A,
                          LMatrixCCC *eigvals,
                          LMatrixCCC *eigvecs)
{
  int size = static_cast<int>(A->numRows());
  if (size != static_cast<int>(A->numColumns()))
    {
      ERROR("expected a square matrix");
      return false;
    }

  if (size == 0)
    {
      eigvals->resize(0, 1);
      eigvecs->resize(0, 0);
      return true;
    }

  bool ret = true;
  char dont = 'N';
  char doit = 'V';
  int wsize = 4 * size;
  int rsize = 2 * size;
  double *workspace = newarray_atomic(double, wsize);
  double *rwork = newarray_atomic(double, rsize);
  int info;

  double *copyA = make_lapack_array(*A);
  double *evals = newarray_atomic(double, 2 * size);
  double *evecs = newarray_atomic(double, 2 * size * size);

  zgeev_(&dont,
         &doit,
         &size,
         copyA,
         &size,
         evals,
         static_cast<double *>(0),
         &size, /* left eigvecs */
         evecs,
         &size, /* right eigvecs */
         workspace,
         &wsize,
         rwork,
         &info);

  if (info < 0)
    {
      ERROR("argument passed to zgeev had an illegal value");
      ret = false;
    }
  else if (info > 0)
    {
      ERROR("the QR algorithm in zgeev failed to compute all eigvals");
      ret = false;
    }
  else
    {
      eigvals->resize(size, 1);
      fill_from_lapack_array(evals, *eigvals);
      eigvecs->resize(size, size);
      fill_from_lapack_array(evecs, *eigvecs);
    }

  freemem(copyA);
  freemem(evals);
  freemem(evecs);
  freemem(workspace);
  freemem(rwork);

  return ret;
}

bool Lapack::eigenvalues_hermitian(const LMatrixCCC *A, LMatrixRRR *eigvals)
{
  int size = static_cast<int>(A->numRows());
  if (size != static_cast<int>(A->numColumns()))
    {
      ERROR("expected a square matrix");
      return false;
    }

  if (size == 0)
    {
      eigvals->resize(0, 1);
      return true;
    }

  bool ret = true;
  char dont = 'N';
  char triangle = 'U'; /* Upper triangular part makes symmetric matrix. */

  int wsize = 4 * size - 2;
  double *workspace = newarray_atomic(double, wsize);
  double *rwork = newarray_atomic(double, 3 * size - 2);
  int info;

  double *copyA = make_lapack_array(*A);
  double *evals = newarray_atomic(double, size);

  zheev_(&dont,
         &triangle,
         &size,
         copyA,
         &size,
         evals,
         workspace,
         &wsize,
         rwork,
         &info);

  if (info < 0)
    {
      ERROR("argument passed to zheev had an illegal value");
      ret = false;
    }
  else if (info > 0)
    {
      ERROR("zheev did not converge");
      ret = false;
    }
  else
    {
      eigvals->resize(size, 1);
      fill_from_lapack_array(evals, *eigvals);
    }

  freemem(copyA);
  freemem(evals);
  freemem(workspace);
  freemem(rwork);

  return ret;
}

bool Lapack::eigenvectors_hermitian(const LMatrixCCC *A,
                                    LMatrixRRR *eigvals,
                                    LMatrixCCC *eigvecs)
{
  int size = static_cast<int>(A->numRows());
  if (size != static_cast<int>(A->numColumns()))
    {
      ERROR("expected a square matrix");
      return false;
    }

  if (size == 0)
    {
      eigvals->resize(0, 1);
      eigvecs->resize(0, 0);
      return true;
    }

  bool ret = true;
  char doit = 'V';
  char triangle = 'U'; /* Upper triangular part makes symmetric matrix */

  int wsize = 4 * size - 2;
  double *workspace = newarray_atomic(double, wsize);
  double *rwork = newarray_atomic(double, 3 * size - 2);
  int info;

  double *evecs = make_lapack_array(*A);
  double *evals = newarray_atomic(double, size);

  zheev_(&doit,
         &triangle,
         &size,
         evecs,
         &size,
         evals,
         workspace,
         &wsize,
         rwork,
         &info);

  if (info < 0)
    {
      ERROR("argument passed to zheev had an illegal value");
      ret = false;
    }
  else if (info > 0)
    {
      ERROR("zheev did not converge");
      ret = false;
    }
  else
    {
      eigvals->resize(size, 1);
      fill_from_lapack_array(evals, *eigvals);
      eigvecs->resize(size, size);
      fill_from_lapack_array(evecs, *eigvecs);
    }

  freemem(evals);
  freemem(evecs);
  freemem(workspace);
  freemem(rwork);

  return ret;
}

bool Lapack::SVD(const LMatrixCCC *A,
                 LMatrixRRR *Sigma,
                 LMatrixCCC *U,
                 LMatrixCCC *VT)
{
  bool ret = true;
  char doit = 'A';  // other options are 'S' and 'O' for singular vectors only
  int rows = static_cast<int>(A->numRows());
  int cols = static_cast<int>(A->numColumns());
  int info;
  int min = (rows <= cols) ? rows : cols;

  if (min == 0)
    {
      ERROR("expected a matrix with positive dimensions");
      return false;
    }

  int max = (rows >= cols) ? rows : cols;
  int wsize = 4 * min + 2 * max;
  double *workspace = newarray_atomic(double, 2 * wsize);
  double *rwork = newarray_atomic(double, 5 * max);

  double *copyA = make_lapack_array(*A);
  double *u = newarray_atomic(double, 2 * rows * rows);
  double *vt = newarray_atomic(double, 2 * cols * cols);
  double *sigma = newarray_atomic(double, 2 * min);

  zgesvd_(&doit,
          &doit,
          &rows,
          &cols,
          copyA,
          &rows,
          sigma,
          u,
          &rows,
          vt,
          &cols,
          workspace,
          &wsize,
          rwork,
          &info);

  if (info < 0)
    {
      ERROR("argument passed to zgesvd had an illegal value");
      ret = false;
    }
  else if (info > 0)
    {
      ERROR("zgesvd did not converge");
      ret = false;
    }
  else
    {
      U->resize(rows, rows);
      fill_from_lapack_array(u, *U);
      VT->resize(cols, cols);
      fill_from_lapack_array(vt, *VT);
      Sigma->resize(min, 1);
      fill_from_lapack_array(sigma, *Sigma);
    }

  freemem(workspace);
  freemem(rwork);
  freemem(copyA);
  freemem(u);
  freemem(vt);
  freemem(sigma);

  return ret;
}

bool Lapack::SVD_divide_conquer(const LMatrixCCC *A,
                                LMatrixRRR *Sigma,
                                LMatrixCCC *U,
                                LMatrixCCC *VT)
{
  bool ret = true;
  char doit = 'A';  // other options are 'S' and 'O' for singular vectors only
  int rows = static_cast<int>(A->numRows());
  int cols = static_cast<int>(A->numColumns());
  int info;
  int min = (rows <= cols) ? rows : cols;

  if (min == 0)
    {
      ERROR("expected a matrix with positive dimensions");
      return false;
    }

  int max = (rows >= cols) ? rows : cols;
  int wsize = 2 * min * min + 4 * min + 2 * max;

  double *workspace = newarray_atomic(double, 2 * wsize);
  int *iworkspace = newarray_atomic(int, 8 * min);
  double *rwork = newarray_atomic(double, 5 * min * min + 7 * min);

  double *copyA = make_lapack_array(*A);
  double *u = newarray_atomic(double, 2 * rows * rows);
  double *vt = newarray_atomic(double, 2 * cols * cols);
  double *sigma = newarray_atomic(double, 2 * min);

  zgesdd_(&doit,
          &rows,
          &cols,
          copyA,
          &rows,
          sigma,
          u,
          &rows,
          vt,
          &cols,
          workspace,
          &wsize,
          rwork,
          iworkspace,
          &info);

  if (info < 0)
    {
      ERROR("argument passed to zgesdd had an illegal value");
      ret = false;
    }
  else if (info > 0)
    {
      ERROR("zgesdd did not converge");
      ret = false;
    }
  else
    {
      U->resize(rows, rows);
      fill_from_lapack_array(u, *U);
      VT->resize(cols, cols);
      fill_from_lapack_array(vt, *VT);
      Sigma->resize(min, 1);
      fill_from_lapack_array(sigma, *Sigma);
    }

  freemem(workspace);
  freemem(iworkspace);
  freemem(rwork);
  freemem(copyA);
  freemem(u);
  freemem(vt);
  freemem(sigma);

  return ret;
}

bool Lapack::least_squares(const LMatrixCCC *A,
                           const LMatrixCCC *b,
                           LMatrixCCC *x)
{
  bool ret = true;
  char job = 'N';
  int info;
  int rows = static_cast<int>(A->numRows());
  int cols = static_cast<int>(A->numColumns());
  int brows = static_cast<int>(b->numRows());
  int bcols = static_cast<int>(b->numColumns());
  int min = (rows <= cols) ? rows : cols;
  int max = (rows >= cols) ? rows : cols;
  int wsize = min + ((bcols >= max) ? bcols : max);

  if (brows != rows)
    {
      ERROR("expected compatible right hand side");
      return false;
    }

  if (min == 0 || bcols == 0)
    {
      x->resize(cols, bcols);
      return true;
    }

  double *copyA = make_lapack_array(*A);
  double *copyb = make_lapack_array(*b);
  double *workspace = newarray_atomic(double, 2 * wsize);

  if (rows < cols)
    {
      // Make 'b' (copyb) into a cols x bcols matrix, by adding a zero block at
      // the bottom
      double *copyb2 = newarray_atomic_clear(double, 2 * cols * bcols);
      int bloc = 0;
      int copyloc = 0;
      for (int j = 0; j < bcols; j++)
        {
          copyloc = 2 * j * cols;
          for (int i = 0; i < 2 * brows; i++)
            {
              copyb2[copyloc++] = copyb[bloc++];
            }
        }
      freemem(copyb);
      copyb = copyb2;
    }

  zgels_(&job,
         &rows,
         &cols,
         &bcols,
         copyA,
         &rows,
         copyb,
         &max,
         workspace,
         &wsize,
         &info);

  if (info != 0)
    {
      ERROR("argument passed to zgels had an illegal value");
      ret = false;
    }
  else
    {
      x->resize(cols, bcols);
      if (rows > cols)
        {
          int copyloc = 0;
          for (int j = 0; j < bcols; j++)
            {
              auto xj = x->columnBegin(j);
              copyloc = 2 * j * rows;
              for (int i = 0; i < cols; i++, ++xj)
                {
                  double re = copyb[copyloc++];
                  double im = copyb[copyloc++];
                  x->ring().set_from_doubles(*xj, re, im);
                }
            }
        }
      else
        {
          fill_from_lapack_array(copyb, *x);
        }
    }

  freemem(copyA);
  freemem(copyb);
  freemem(workspace);

  return ret;
}

bool Lapack::least_squares_deficient(const LMatrixCCC *A,
                                     const LMatrixCCC *b,
                                     LMatrixCCC *x)
{
  bool ret = true;
  int rows = static_cast<int>(A->numRows());
  int cols = static_cast<int>(A->numColumns());
  int brows = static_cast<int>(b->numRows());
  int bcols = static_cast<int>(b->numColumns());
  double rcond = -1.0;
  int rank, info;
  int min = (rows < cols) ? rows : cols;
  int max = (rows > cols) ? rows : cols;
  int wsize = 2 * min + ((bcols > max) ? bcols : max);

  if (brows != rows)
    {
      ERROR("expected compatible right hand side");
      return false;
    }

  if (min == 0 || bcols == 0)
    {
      x->resize(cols, bcols);
      return true;
    }

  double *copyA = make_lapack_array(*A);
  double *copyb = make_lapack_array(*b);
  double *workspace = newarray_atomic(double, 2 * wsize);
  double *sing = newarray_atomic(double, min);
  double *rwork = newarray_atomic(double, 5 * min);

  if (rows < cols)
    {
      // Make 'b' (copyb) into a cols x bcols matrix, by adding a zero block at
      // the bottom
      double *copyb2 = newarray_atomic_clear(double, 2 * cols * bcols);
      int bloc = 0;
      int copyloc = 0;
      for (int j = 0; j < bcols; j++)
        {
          copyloc = 2 * j * cols;
          for (int i = 0; i < 2 * brows; i++)
            {
              copyb2[copyloc++] = copyb[bloc++];
            }
        }
      freemem(copyb);
      copyb = copyb2;
    }

  zgelss_(&rows,
          &cols,
          &bcols,
          copyA,
          &rows,
          copyb,
          &max,
          sing,
          &rcond,
          &rank,
          workspace,
          &wsize,
          rwork,
          &info);

  if (info != 0)
    {
      ERROR("argument passed to zgelss had an illegal value");
      ret = false;
    }
  else
    {
      x->resize(cols, bcols);
      if (rows > cols)
        {
          int copyloc = 0;
          for (int j = 0; j < bcols; j++)
            {
              auto xj = x->columnBegin(j);
              copyloc = 2 * j * rows;
              for (int i = 0; i < cols; i++, ++xj)
                {
                  double re = copyb[copyloc++];
                  double im = copyb[copyloc++];
                  x->ring().set_from_doubles(*xj, re, im);
                }
            }
        }
      else
        {
          fill_from_lapack_array(copyb, *x);
        }
    }

  freemem(copyA);
  freemem(copyb);
  freemem(workspace);
  freemem(sing);
  freemem(rwork);

  return ret;
}

//////////////xxx /////////////////////// same for RR/CC
///////////////////////////////////////////////////

double *make_lapack_array(const DMat<M2::ARingRR> &mat)
// changing to RR and column-major order
{
  size_t len = mat.numRows() * mat.numColumns();
  double *result = newarray_atomic(double, len);
  double *p = result;
  for (size_t c = 0; c < mat.numColumns(); c++)
    {
      auto end = mat.columnEnd(c);
      for (auto a = mat.columnBegin(c); a != end; ++a) *p++ = *a;
    }
  return result;
}

double *make_lapack_array(const DMat<M2::ARingCC> &mat)
// changing to CC and column-major order
{
  size_t len = mat.numRows() * mat.numColumns();
  double *result = newarray_atomic(double, 2 * len);
  double *p = result;
  for (size_t c = 0; c < mat.numColumns(); c++)
    {
      auto end = mat.columnEnd(c);
      for (auto a = mat.columnBegin(c); a != end; ++a)
        {
          *p++ = (*a).re;
          *p++ = (*a).im;
        }
    }
  return result;
}

void fill_from_lapack_array(const double *lapack_array,
                            DMat<M2::ARingRR> &result)
// from RR and column-major order
{
  const double *p = lapack_array;
  for (size_t c = 0; c < result.numColumns(); c++)
    {
      auto end = result.columnEnd(c);
      for (auto a = result.columnBegin(c); a != end; ++a) *a = *p++;
    }
}

void fill_from_lapack_array(const double *lapack_array,
                            DMat<M2::ARingCC> &result)
{
  const double *p = lapack_array;
  for (size_t c = 0; c < result.numColumns(); c++)
    {
      auto end = result.columnEnd(c);
      for (auto a = result.columnBegin(c); a != end; ++a)
        {
          double re = *p++;
          double im = *p++;
          result.ring().set_from_doubles(*a, re, im);
        }
    }
}

void fill_lower_and_upper(double *lapack_numbers,  // column-major order
                          DMat<M2::ARingRR> &lower,
                          DMat<M2::ARingRR> &upper)
// original matrix has size nrows x ncols
// lapack_numbers is an array of this size
// result: lower: size: nrows x min(nrows, ncols)
// result: upper: size min x ncols
//
// lapack_numbers is in column major form
// lower and upper are in row major form
{
  // At this point, lower and upper should be zero matrices.
  assert(MatrixOps::isZero(lower));
  assert(MatrixOps::isZero(upper));

  auto L = lower.rowMajorArray();
  auto U = upper.rowMajorArray();

  for (size_t c = 0; c < upper.numColumns(); c++)
    {
      auto U1 = U;
      for (size_t r = 0; r <= c; r++)
        {
          if (r >= upper.numRows()) break;
          *U1 = *lapack_numbers++;
          U1 += upper.numColumns();
        }
      U++;  // change to next column

      if (c < lower.numColumns())
        {
          lower.ring().set_from_long(*L, 1);  // diagonal entry of L should be 1
          L += lower.numColumns();  // pointing to entry right below diagonal
          auto L1 = L;  // will increment by lower.numRows() each loop here
          for (size_t r = c + 1; r < lower.numRows(); r++)
            {
              *L1 = *lapack_numbers++;
              L1 += lower.numColumns();  // to place next entry.
            }
          L++;  // change to next column
        }
    }
}

void fill_lower_and_upper(double *lapack_numbers,  // column-major order
                          DMat<M2::ARingCC> &lower,
                          DMat<M2::ARingCC> &upper)
// original matrix has size nrows x ncols
// lapack_numbers is an array of this size (*2)
// result: lower: size: nrows x min(nrows, ncols)
// result: upper: size min x ncols
//
// lapack_numbers is in column major form
// lower and upper are in row major form
{
  // At this point, lower and upper should be zero matrices.
  assert(MatrixOps::isZero(lower));
  assert(MatrixOps::isZero(upper));

  auto L = lower.rowMajorArray();
  auto U = upper.rowMajorArray();

  for (size_t c = 0; c < upper.numColumns(); c++)
    {
      auto U1 = U;
      for (size_t r = 0; r <= c; r++)
        {
          if (r >= upper.numRows()) break;
          double re = *lapack_numbers++;
          double im = *lapack_numbers++;
          upper.ring().set_from_doubles(*U1, re, im);
          U1 += upper.numColumns();
        }
      U++;  // change to next column

      if (c < lower.numColumns())
        {
          lower.ring().set_from_long(*L, 1);  // diagonal entry of L should be 1
          L += lower.numColumns();  // pointing to entry right below diagonal
          auto L1 = L;  // will increment by lower.numRows() each loop here
          for (size_t r = c + 1; r < lower.numRows(); r++)
            {
              double re = *lapack_numbers++;
              double im = *lapack_numbers++;
              lower.ring().set_from_doubles(*L1, re, im);
              L1 += lower.numColumns();  // to place next entry.
            }
          L++;  // change to next column
        }
    }
}

/* void printmat(int N, int M, mpreal * A, int LDA)
{
    mpreal mtmp;

    printf("[ ");
    for (int i = 0; i < N; i++) {
        printf("[ ");
        for (int j = 0; j < M; j++) {
            mtmp = A[i + j * LDA];
            mpfr_printf("%5.2Re", mpfr_ptr(mtmp));
            if (j < M - 1)
                printf(", ");
        }
        if (i < N - 1)
            printf("]; ");
        else
            printf("] ");
    }
    printf("]");
} */

M2_arrayintOrNull Lapack::LU(const LMatrixRR *A, LMatrixRR *L, LMatrixRR *U)
{
  int rows = static_cast<int>(A->numRows());
  int cols = static_cast<int>(A->numColumns());
  int info;
  int min = (rows <= cols) ? rows : cols;
  M2_arrayint result = M2_makearrayint(rows);

  L->resize(rows, min);
  U->resize(min, cols);

  if (min == 0)
    {
      if (rows > 0)
        for (int i = 0; i < rows; i++) result->array[i] = i;
      return result;
    }

  int *perm = newarray_atomic(int, min);

  LapackDoubles copyA = make_lapack_array(*A);

  dgetrf_(&rows, &cols, copyA, &rows, perm, &info);

  fill_lower_and_upper(copyA, *L, *U);

  for (int i = 0; i < rows; i++) result->array[i] = i;
  for (int i = 0; i < min; i++)
    {
      int thisloc = perm[i] - 1;
      int tmp = result->array[thisloc];
      result->array[thisloc] = result->array[i];
      result->array[i] = tmp;
    }

  freemem(copyA);
  freemem(perm);

  if (info < 0)
    {
      ERROR("argument passed to dgetrf had an illegal value");
      return 0;
    }

  return result;
}

bool Lapack::solve(const LMatrixRR *A, /* read only */
                   const LMatrixRR *b, /* read only */
                   LMatrixRR *x)       /* output value */
{
  int size = static_cast<int>(A->numRows());
  int bsize = static_cast<int>(b->numColumns());
  int info;

  /* make sure matrix is square */
  if (A->numRows() != A->numColumns())
    {
      ERROR("expected a square matrix");
      return false;
    }

  /* make sure dimensions of b make sense for Ax=b */
  if (b->numRows() != size)
    {
      ERROR("expected matrices to have same number of rows");
      return false;
      ;
    }

  if (size == 0)
    {
      x->resize(size, bsize);
      return true;
    }

  int *permutation = newarray_atomic(int, size);
  LapackDoubles copyA = make_lapack_array(*A);
  LapackDoubles copyb = make_lapack_array(*b);

  dgesv_(&size,
         &bsize,
         copyA,
         &size,
         permutation,
         copyb,  // also the result
         &size,
         &info);

  // Now set x
  x->resize(size, bsize);
  fill_from_lapack_array(copyb, *x);

  if (info > 0)
    {
      ERROR("according to dgesv, matrix is singular");
      return false;
    }
  else if (info < 0)
    {
      ERROR("argument passed to dgesv had an illegal value");
      return false;
    }

  return true;
}

bool Lapack::eigenvalues(const LMatrixRR *A, LMatrixCC *eigvals)
{
  int size = static_cast<int>(A->numRows());
  if (size != static_cast<int>(A->numColumns()))
    {
      ERROR("expected a square matrix");
      return false;
    }

  if (size == 0)
    {
      eigvals->resize(0, 1);
      return true;
    }

  bool ret = true;
  char dont = 'N';
  int wsize = 3 * size;
  double *workspace = newarray_atomic(double, 2*wsize);
  int info;

  double *copyA = make_lapack_array(*A);
  double *real = newarray_atomic(double, size);  // real components of eigvals
  double *imag = newarray_atomic(double, size);  // imaginary components

  dgeev_(&dont,
         &dont,
         &size,
         copyA,
         &size,
         real,
         imag,
         static_cast<double *>(0),
         &size, /* left eigenvectors */
         static_cast<double *>(0),
         &size, /* right eigenvectors */
         workspace,
         &wsize,
         &info);

  if (info < 0)
    {
      ERROR("argument passed to dgeev had an illegal value");
      ret = false;
    }
  else if (info > 0)
    {
      ERROR("the QR algorithm in dgeev failed to compute all eigvals");
      ret = false;
    }
  else
    {
      eigvals->resize(size, 1);
      LMatrixCC::ElementType *elems = eigvals->rowMajorArray();
      for (int i = 0; i < size; i++)
        eigvals->ring().set_from_doubles(elems[i], real[i], imag[i]);
    }

  freemem(copyA);
  freemem(real);
  freemem(imag);
  return ret;
}

bool Lapack::eigenvectors(const LMatrixRR *A,
                          LMatrixCC *eigvals,
                          LMatrixCC *eigvecs)
{
  int size = static_cast<int>(A->numRows());
  if (size != static_cast<int>(A->numColumns()))
    {
      ERROR("expected a square matrix");
      return false;
    }

  if (size == 0)
    {
      eigvals->resize(0, 1);
      eigvecs->resize(0, 0);
      return true;
    }

  bool ret = true;
  char dont = 'N';
  char doit = 'V';
  int wsize = 4 * size;

  double *workspace = newarray_atomic(double, 2*wsize);
  int info;

  double *copyA = make_lapack_array(*A);
  double *real = newarray_atomic(double, size);  // real components of eigvals
  double *imag = newarray_atomic(double, size);  // imaginary components
  double *eigen = newarray_atomic(double, size *size);  // eigvecs

  dgeev_(&dont, /* left e-vectors */
         &doit, /* right e-vectors */
         &size,
         copyA,
         &size,
         real,
         imag,
         static_cast<double *>(0),
         &size, /* left eigvecs */
         eigen,
         &size, /* right eigvecs */
         workspace,
         &wsize,
         &info);

  if (info < 0)
    {
      ERROR("argument passed to dgeev had an illegal value");
      ret = false;
    }
  else if (info > 0)
    {
      ERROR("the QR algorithm in dgeev failed to compute all eigvals");
      ret = false;
    }
  else
    {
      // Make the complex arrays of eigvals and eigvecs
      eigvals->resize(size, 1);
      eigvecs->resize(size, size);
      //      LMatrixCC::ElementType* elems = eigvecs->array();
      LMatrixCC::ElementType *elems = eigvals->rowMajorArray();
      for (int j = 0; j < size; j++)
        {
          eigvals->ring().set_from_doubles(elems[j], real[j], imag[j]);
          auto end = eigvecs->columnEnd(j);

          // mpfr_get_d(&(*a), MPFR_RNDN);
          int loc = j * size;
          if (imag[j] == 0)
            {
              auto a = eigvecs->columnBegin(j);
              for (int i = 0; a != end; ++a, ++i)
                {
                  (*a).re = eigen[loc + i];
                  (*a).im = 0;
                }
            }
          else if (imag[j] > 0)
            {
              auto a = eigvecs->columnBegin(j);
              auto b = eigvecs->columnBegin(j + 1);
              for (int i = 0; a != end; ++a, ++b, ++i)
                {
                  eigvecs->ring().set_from_doubles(
                      *a, eigen[loc + i], eigen[loc + size + i]);
                  eigvecs->ring().set_from_doubles(
                      *b, eigen[loc + i], -eigen[loc + size + i]);
                }
            }
        }
    }

  freemem(copyA);
  freemem(workspace);
  freemem(real);
  freemem(imag);
  freemem(eigen);
  return ret;
}

bool Lapack::eigenvalues_symmetric(const LMatrixRR *A, LMatrixRR *eigvals)
{
  int size = static_cast<int>(A->numRows());
  if (size != static_cast<int>(A->numColumns()))
    {
      ERROR("expected a square matrix");
      return false;
    }

  if (size == 0)
    {
      eigvals->resize(0, 1);
      return true;
    }

  bool ret = true;
  char dont = 'N';
  char triangle = 'U'; /* Upper triangular part makes symmetric matrix. */
  int info;

  int wsize = 3 * size - 1;
  double *workspace = newarray_atomic(double, wsize);

  double *copyA = make_lapack_array(*A);
  double *evals = newarray_atomic(double, size);

  dsyev_(
      &dont, &triangle, &size, copyA, &size, evals, workspace, &wsize, &info);

  if (info < 0)
    {
      ERROR("argument passed to dsyev had an illegal value");
      ret = false;
    }
  else if (info > 0)
    {
      ERROR("dsyev did not converge");
      ret = false;
    }
  else
    {
      // Copy eigenvalues back to eigvals
      eigvals->resize(size, 1);
      fill_from_lapack_array(evals, *eigvals);
    }

  freemem(workspace);
  freemem(copyA);
  freemem(evals);

  return ret;
}

bool Lapack::eigenvectors_symmetric(const LMatrixRR *A,
                                    LMatrixRR *eigvals,
                                    LMatrixRR *eigvecs)
{
  int size = static_cast<int>(A->numRows());
  if (size != static_cast<int>(A->numColumns()))
    {
      ERROR("expected a square matrix");
      return false;
    }

  if (size == 0)
    {
      eigvals->resize(0, 1);
      eigvecs->resize(0, 0);
      return true;
    }

  bool ret = true;
  char doit = 'V';
  char triangle = 'U'; /* Upper triangular part makes symmetric matrix */

  int wsize = 3 * size - 1;
  double *workspace = newarray_atomic(double, wsize);
  int info;

  double *evecs = make_lapack_array(*A);
  double *evals = newarray_atomic(double, size);

  dsyev_(
      &doit, &triangle, &size, evecs, &size, evals, workspace, &wsize, &info);

  if (info < 0)
    {
      ERROR("argument passed to dsyev had an illegal value");
      ret = false;
    }
  else if (info > 0)
    {
      ERROR("dsyev did not converge");
      ret = false;
    }
  else
    {
      // Copy results to eigvals, eigvecs
      eigvecs->resize(size, size);
      fill_from_lapack_array(evecs, *eigvecs);
      eigvals->resize(size, 1);
      fill_from_lapack_array(evals, *eigvals);
    }

  freemem(workspace);
  freemem(evecs);
  freemem(evals);

  return ret;
}

bool Lapack::SVD(const LMatrixRR *A,
                 LMatrixRR *Sigma,
                 LMatrixRR *U,
                 LMatrixRR *VT)
{
  bool ret = true;
  char doit = 'A';  // other options are 'S' and 'O' for singular vectors only
  int rows = static_cast<int>(A->numRows());
  int cols = static_cast<int>(A->numColumns());
  int info;
  int min = (rows <= cols) ? rows : cols;

  if (min == 0)
    {
      ERROR("expected a matrix with positive dimensions");
      return false;
    }

  int max = (rows >= cols) ? rows : cols;
  int wsize = (3 * min + max >= 5 * min) ? 3 * min + max : 5 * min;
  double *workspace = newarray_atomic(double, wsize);

  double *copyA = make_lapack_array(*A);
  double *u = newarray_atomic(double, rows *rows);
  double *vt = newarray_atomic(double, cols *cols);
  double *sigma = newarray_atomic(double, min);

  dgesvd_(&doit,
          &doit,
          &rows,
          &cols,
          copyA,
          &rows,
          sigma,
          u,
          &rows,
          vt,
          &cols,
          workspace,
          &wsize,
          &info);

  if (info < 0)
    {
      ERROR("argument passed to dgesvd had an illegal value");
      ret = false;
    }
  else if (info > 0)
    {
      ERROR("dgesvd did not converge");
      ret = false;
    }
  else
    {
      U->resize(rows, rows);
      VT->resize(cols, cols);
      Sigma->resize(min, 1);
      fill_from_lapack_array(u, *U);
      fill_from_lapack_array(vt, *VT);
      fill_from_lapack_array(sigma, *Sigma);
    }

  freemem(workspace);
  freemem(copyA);
  freemem(u);
  freemem(vt);
  freemem(sigma);

  return ret;
}

bool Lapack::SVD_divide_conquer(const LMatrixRR *A,
                                LMatrixRR *Sigma,
                                LMatrixRR *U,
                                LMatrixRR *VT)
{
  bool ret = true;
  char doit = 'A';  // other options are 'S' and 'O' for singular vectors only
  int rows = static_cast<int>(A->numRows());
  int cols = static_cast<int>(A->numColumns());
  int info;
  int min = (rows <= cols) ? rows : cols;

  if (min == 0)
    {
      ERROR("expected a matrix with positive dimensions");
      return false;
    }

  int max = (rows >= cols) ? rows : cols;
  int wsize = 4 * min * min + max + 9 * min;
  double *workspace = newarray_atomic(double, wsize);
  int *iworkspace = newarray_atomic(int, 8 * min);

  double *copyA = make_lapack_array(*A);
  double *u = newarray_atomic(double, rows *rows);
  double *vt = newarray_atomic(double, cols *cols);
  double *sigma = newarray_atomic(double, min);

  dgesdd_(&doit,
          &rows,
          &cols,
          copyA,
          &rows,
          sigma,
          u,
          &rows,
          vt,
          &cols,
          workspace,
          &wsize,
          iworkspace,
          &info);

  if (info < 0)
    {
      ERROR("argument passed to dgesdd had an illegal value");
      ret = false;
    }
  else if (info > 0)
    {
      ERROR("dgesdd did not converge");
      ret = false;
    }
  else
    {
      U->resize(rows, rows);
      VT->resize(cols, cols);
      Sigma->resize(min, 1);
      fill_from_lapack_array(u, *U);
      fill_from_lapack_array(vt, *VT);
      fill_from_lapack_array(sigma, *Sigma);
    }

  freemem(workspace);
  freemem(iworkspace);
  freemem(copyA);
  freemem(u);
  freemem(vt);
  freemem(sigma);

  return ret;
}

bool Lapack::least_squares(const LMatrixRR *A, const LMatrixRR *b, LMatrixRR *x)
{
  bool ret = true;
  char job = 'N';
  int info;
  int rows = static_cast<int>(A->numRows());
  int cols = static_cast<int>(A->numColumns());
  int brows = static_cast<int>(b->numRows());
  int bcols = static_cast<int>(b->numColumns());
  int min = (rows <= cols) ? rows : cols;
  int max = (rows >= cols) ? rows : cols;
  int wsize = min + ((bcols >= max) ? bcols : max);

  if (brows != rows)
    {
      ERROR("expected compatible right hand side");
      return false;
    }

  if (min == 0 || bcols == 0)
    {
      x->resize(cols, bcols);
      return true;
    }

  double *copyA = make_lapack_array(*A);
  double *copyb = make_lapack_array(*b);
  double *workspace = newarray_atomic(double, wsize);

  if (rows < cols)
    {
      // Make 'b' (copyb) into a cols x bcols matrix, by adding a zero block at
      // the bottom
      double *copyb2 = newarray_atomic_clear(double, cols *bcols);
      int bloc = 0;
      int copyloc = 0;
      for (int j = 0; j < bcols; j++)
        {
          copyloc = j * cols;
          for (int i = 0; i < brows; i++)
            {
              copyb2[copyloc++] = copyb[bloc++];
            }
        }
      freemem(copyb);
      copyb = copyb2;
    }

  dgels_(&job,
         &rows,
         &cols,
         &bcols,
         copyA,
         &rows,
         copyb,
         &max,
         workspace,
         &wsize,
         &info);

  if (info != 0)
    {
      ERROR("argument passed to dgels had an illegal value");
      ret = false;
    }
  else
    {
      x->resize(cols, bcols);
      if (rows > cols)
        {
          // We only need the first 'cols' rows of copyb
          int copyloc = 0;
          for (int j = 0; j < bcols; j++)
            {
              auto xj = x->columnBegin(j);
              copyloc = j * rows;
              for (int i = 0; i < cols; i++, ++xj)
                {
                  *xj = copyb[copyloc++];
                }
            }
        }
      else
        {
          fill_from_lapack_array(copyb, *x);
        }
    }

  freemem(copyA);
  freemem(copyb);
  freemem(workspace);

  return ret;
}

bool Lapack::least_squares_deficient(const LMatrixRR *A,
                                     const LMatrixRR *b,
                                     LMatrixRR *x)
{
  bool ret = true;
  int rows = static_cast<int>(A->numRows());
  int cols = static_cast<int>(A->numColumns());
  int brows = static_cast<int>(b->numRows());
  int bcols = static_cast<int>(b->numColumns());
  double rcond = -1.0;  // use machine precision
  int rank, info;
  int min = (rows < cols) ? rows : cols;
  int max = (rows > cols) ? rows : cols;
  int tempmax = ((2 * min > max) ? 2 * min : max);
  int wsize = 3 * min + ((tempmax > bcols) ? tempmax : bcols);

  if (brows != rows)
    {
      ERROR("expected compatible right hand side");
      return false;
    }

  if (min == 0 || bcols == 0)
    {
      x->resize(cols, bcols);
      return true;
    }

  double *copyA = make_lapack_array(*A);
  double *copyb = make_lapack_array(*b);
  double *workspace = newarray_atomic(double, wsize);
  double *sing = newarray_atomic(double, min);

  if (rows < cols)
    {
      // Make 'b' (copyb) into a cols x bcols matrix, by adding a zero block at
      // the bottom
      double *copyb2 = newarray_atomic_clear(double, cols *bcols);
      int bloc = 0;
      int copyloc = 0;
      for (int j = 0; j < bcols; j++)
        {
          copyloc = j * cols;
          for (int i = 0; i < brows; i++)
            {
              copyb2[copyloc++] = copyb[bloc++];
            }
        }
      freemem(copyb);
      copyb = copyb2;
    }

  dgelss_(&rows,
          &cols,
          &bcols,
          copyA,
          &rows,
          copyb,
          &max,
          sing,
          &rcond,
          &rank,
          workspace,
          &wsize,
          &info);

  if (info != 0)
    {
      ERROR("argument passed to dgelss had an illegal value");
      ret = false;
    }
  else
    {
      x->resize(cols, bcols);
      if (rows > cols)
        {
          int copyloc = 0;
          for (int j = 0; j < bcols; j++)
            {
              auto xj = x->columnBegin(j);
              copyloc = j * rows;
              for (int i = 0; i < cols; i++, ++xj)
                {
                  *xj = copyb[copyloc++];
                }
            }
        }
      else
        {
          fill_from_lapack_array(copyb, *x);
        }
    }

  freemem(copyA);
  freemem(copyb);
  freemem(workspace);
  freemem(sing);

  return ret;
}

bool Lapack::QR(const LMatrixRR *A, LMatrixRR *Q, LMatrixRR *R, bool return_QR)
{
  // sizes:
  //  input: A[m,n]
  //  output for returnQR==true:
  //   case m >= n:  Q[m,n], R[n,n]
  //   case m < n: Q[m,m], R[m,n]
  //  output for returnQR==false:
  //   Q[m,n], R[1,min(m,n)]
  bool ret = true;
  int rows = static_cast<int>(A->numRows());
  int cols = static_cast<int>(A->numColumns());
  int info1 = 0, info2 = 0, info3 = 0, info4 = 0;
  int min = (rows <= cols) ? rows : cols;

  if (min == 0)
    {
      ERROR("expected a matrix with positive dimensions");
      return false;
    }

  double *copyA = make_lapack_array(*A);       // delete as well.
  double *tau = newarray_atomic(double, min);  // make via new.
  double workspace_size[1];
  int work_size = -1;
  // find optimal workspace
  dgeqrf_(&rows, &cols, copyA, &rows, tau, workspace_size, &work_size, &info1);
  work_size = static_cast<int>(workspace_size[0]);
  // std::cout << "work size for QR:  " << work_size << std::endl;
  double *workspace = new double[work_size];

  dgeqrf_(&rows, &cols, copyA, &rows, tau, workspace, &work_size, &info2);

  if (info1 < 0 or info2 < 0)
    {
      ERROR("argument passed to dgeqrf had an illegal value");
      ret = false;
    }
  if (info1 > 0 or info2 > 0)
    {
      ERROR("can this happen?");
      ret = false;
    }

  if (ret)
    {
      if (return_QR)
        {
          Q->resize(rows, cols);
          R->resize(cols, cols);
          fill_from_lapack_upper(copyA, rows, cols, *R);

          // Reset Q, R, with their values.
          int orgqr_work_size = -1;
          dorgqr_(&rows,
                  &cols,
                  &min,
                  copyA,
                  &rows,  // lda?
                  tau,
                  workspace_size,
                  &orgqr_work_size,
                  &info3);
          orgqr_work_size = static_cast<int>(workspace_size[0]);
          if (orgqr_work_size > work_size)
            {
              delete[] workspace;
              work_size = orgqr_work_size;
              workspace = new double[work_size];
              std::cout << "work size increased to: " << work_size << std::endl;
            }
          dorgqr_(&rows,
                  &cols,
                  &min,
                  copyA,
                  &rows,  // lda?
                  tau,
                  workspace,
                  &work_size,
                  &info4);
          if (info3 < 0 or info4 < 0)
            {
              ERROR("argument passed to dorgqr or dorgqr had an illegal value");
              ret = false;
            }
          else if (info3 > 0 or info4 > 0)
            {
              ERROR("can this happen?");
              ret = false;
            }
          else
            {
              fill_from_lapack_array(copyA, *Q);
            }
        }
      else
        {
          // Return the raw values for QR: the "A" matrix encodes R and the
          // Householders, and tau has the multipliers.
          Q->resize(rows, cols);
          R->resize(1, min);
          fill_from_lapack_array(copyA, *Q);
          fill_from_lapack_array(tau, *R);
        }
    }

  delete[] workspace;
  freemem(copyA);
  freemem(tau);
  return ret;
}

bool Lapack::QR(const LMatrixCC *A, LMatrixCC *Q, LMatrixCC *R, bool return_QR)
{
  // sizes:
  //  input: A[m,n]
  //  output for returnQR==true:
  //   case m >= n:  Q[m,n], R[n,n]
  //   case m < n: Q[m,m], R[m,n]
  //  output for returnQR==false:
  //   Q[m,n], R[1,min(m,n)]
  bool ret = true;
  int rows = static_cast<int>(A->numRows());
  int cols = static_cast<int>(A->numColumns());
  int info1 = 0, info2 = 0, info3 = 0, info4 = 0;
  int min = (rows <= cols) ? rows : cols;

  if (min == 0)
    {
      ERROR("expected a matrix with positive dimensions");
      return false;
    }

  double *copyA = make_lapack_array(*A);  // delete as well.
  double *tau = new double[10 * min];     // make via new.
  double workspace_size[1];
  int work_size = -1;
  // find optimal workspace
  zgeqrf_(&rows, &cols, copyA, &rows, tau, workspace_size, &work_size, &info1);
  work_size = static_cast<int>(workspace_size[0]);
  double *workspace = new double[20 * work_size];

  zgeqrf_(&rows, &cols, copyA, &rows, tau, workspace, &work_size, &info2);

  if (info1 < 0 or info2 < 0)
    {
      ERROR("argument passed to zgeqrf had an illegal value");
      ret = false;
    }
  if (info1 > 0 or info2 > 0)
    {
      ERROR("can this happen?");
      ret = false;
    }

  if (ret)
    {
      if (return_QR)
        {
          Q->resize(rows, cols);
          R->resize(cols, cols);
          fill_from_lapack_upper(copyA, rows, cols, *R);

          // Reset Q, R, with their values.
          int orgqr_work_size = -1;
          zungqr_(&rows,
                  &cols,
                  &min,
                  copyA,
                  &rows,  // lda?
                  tau,
                  workspace_size,
                  &orgqr_work_size,
                  &info3);
          orgqr_work_size = static_cast<int>(workspace_size[0]);
          if (orgqr_work_size > work_size)
            {
              delete[] workspace;
              work_size = orgqr_work_size;
              workspace = new double[2 * work_size];
            }
          zungqr_(&rows,
                  &cols,
                  &min,
                  copyA,
                  &rows,  // lda?
                  tau,
                  workspace,
                  &work_size,
                  &info4);
          if (info3 < 0 or info4 < 0)
            {
              ERROR("argument passed to dorgqr or dorgqr had an illegal value");
              ret = false;
            }
          else if (info3 > 0 or info4 > 0)
            {
              ERROR("can this happen?");
              ret = false;
            }
          else
            {
              fill_from_lapack_array(copyA, *Q);
            }
        }
      else
        {
          // Return the raw values for QR: the "A" matrix encodes R and the
          // Householders, and tau has the multipliers.
          Q->resize(rows, cols);
          R->resize(1, min);
          fill_from_lapack_array(copyA, *Q);
          fill_from_lapack_array(tau, *R);
        }
    }

  delete[] workspace;
  delete[] tau;
  freemem(copyA);
  return ret;
}

M2_arrayintOrNull Lapack::LU(const LMatrixCC *A, LMatrixCC *L, LMatrixCC *U)
{
  int rows = static_cast<int>(A->numRows());
  int cols = static_cast<int>(A->numColumns());
  int info;
  int min = (rows <= cols) ? rows : cols;
  M2_arrayint result = M2_makearrayint(rows);

  L->resize(rows, min);
  U->resize(min, cols);

  if (min == 0)
    {
      if (rows > 0)
        for (int i = 0; i < rows; i++) result->array[i] = i;
      return result;
    }

  int *perm = newarray_atomic(int, min);

  double *copyA = make_lapack_array(*A);

  zgetrf_(&rows, &cols, copyA, &rows, perm, &info);

  if (info < 0)
    {
      ERROR("argument passed to zgetrf had an illegal value");
      freemem(result);
      result = NULL;
    }
  else
    {
      fill_lower_and_upper(copyA, *L, *U);

      for (int i = 0; i < rows; i++) result->array[i] = i;
      for (int i = 0; i < min; i++)
        {
          int thisloc = perm[i] - 1;
          int tmp = result->array[thisloc];
          result->array[thisloc] = result->array[i];
          result->array[i] = tmp;
        }

#if 0
      /* set the permutation array */
      for (int row=1; row<=min; row++) {
        int targ = row;
        for (int i=1; i<=min; i++) {
          if (i == targ)
            targ = perm[i-1];
          else if (perm[i-1] == targ)
            targ = i;
        }
        result->array[row-1] = targ-1;
      }
      for (int i=min; i<rows; i++)
        result->array[i] = i;
#endif
    }

  freemem(perm);
  freemem(copyA);

  return result;
}

bool Lapack::solve(const LMatrixCC *A, const LMatrixCC *b, LMatrixCC *x)
{
  bool ret = true;
  int size = static_cast<int>(A->numRows());
  int bsize = static_cast<int>(b->numColumns());
  int info;

// TODO: MES The next 6 lines need to be removed/cleaned up
#if 0
  const CCC *CCR = A->get_ring()->cast_to_CCC();
  assert(CCR != 0);
  unsigned long precision= CCR->get_precision();
#endif
  unsigned long precision =
      53;  // Just used below in code that I think is not active.

  /* make sure matrix is square */
  if (size != static_cast<int>(A->numColumns()))
    {
      ERROR("expected a square matrix");
      return false;
    }

  /* make sure dimensions of b make sense for Ax=b */
  if (b->numRows() != size)
    {
      ERROR("expected matrices to have same number of rows");
      return false;
    }

  if (size == 0)
    {
      x->resize(size, bsize);
      return true;
    }

  if (precision > 53)
    {
      ERROR("high precision requested, but not yet implemented");
      return false;
    }

  int *permutation = newarray_atomic(int, size);
  double *copyA = make_lapack_array(*A);
  double *copyb = make_lapack_array(*b);

  zgesv_(&size, &bsize, copyA, &size, permutation, copyb, &size, &info);

  if (info > 0)
    {
      ERROR("according to zgesv, matrix is singular");
      ret = false;
    }
  else if (info < 0)
    {
      ERROR("argument passed to zgesv had an illegal value");
      ret = false;
    }
  else
    {
      x->resize(size, bsize);
      fill_from_lapack_array(copyb, *x);
    }

  freemem(permutation);
  freemem(copyA);
  freemem(copyb);

  return ret;
}

bool Lapack::eigenvalues(const LMatrixCC *A, LMatrixCC *eigvals)
{
  int size = static_cast<int>(A->numRows());
  if (size != static_cast<int>(A->numColumns()))
    {
      ERROR("expected a square matrix");
      return false;
    }

  if (size == 0)
    {
      eigvals->resize(0, 1);
      return true;
    }

  bool ret = true;
  char dont = 'N';
  int info;
  int wsize = 2 * size;
  int rsize = 2 * size;
  double *workspace = newarray_atomic(double, 2*wsize);
  double *rwork = newarray_atomic(double, rsize);

  double *copyA = make_lapack_array(*A);
  double *evals = newarray_atomic(double, 2 * size);

  zgeev_(&dont,
         &dont,
         &size,
         copyA,
         &size,
         evals,
         static_cast<double *>(0),
         &size, /* left eigenvectors */
         static_cast<double *>(0),
         &size, /* right eigenvectors */
         workspace,
         &wsize,
         rwork,
         &info);

  if (info < 0)
    {
      ERROR("argument passed to zgeev had an illegal value");
      ret = false;
    }
  else if (info > 0)
    {
      ERROR("the QR algorithm in zgeev failed to compute all eigvals");
      ret = false;
    }
  else
    {
      eigvals->resize(size, 1);
      fill_from_lapack_array(evals, *eigvals);
    }

  freemem(copyA);
  freemem(evals);
  freemem(workspace);
  freemem(rwork);

  return ret;
}

bool Lapack::eigenvectors(const LMatrixCC *A,
                          LMatrixCC *eigvals,
                          LMatrixCC *eigvecs)
{
  int size = static_cast<int>(A->numRows());
  if (size != static_cast<int>(A->numColumns()))
    {
      ERROR("expected a square matrix");
      return false;
    }

  if (size == 0)
    {
      eigvals->resize(0, 1);
      eigvecs->resize(0, 0);
      return true;
    }

  bool ret = true;
  char dont = 'N';
  char doit = 'V';
  int wsize = 2 * size;
  int rsize = 2 * size;
  double *workspace = newarray_atomic(double, 2*wsize);
  double *rwork = newarray_atomic(double, rsize);
  int info;

  double *copyA = make_lapack_array(*A);
  double *evals = newarray_atomic(double, 2 * size);
  double *evecs = newarray_atomic(double, 2 * size * size);

  zgeev_(&dont,
         &doit,
         &size,
         copyA,
         &size,
         evals,
         static_cast<double *>(0),
         &size, /* left eigvecs */
         evecs,
         &size, /* right eigvecs */
         workspace,
         &wsize,
         rwork,
         &info);

  if (info < 0)
    {
      ERROR("argument passed to zgeev had an illegal value");
      ret = false;
    }
  else if (info > 0)
    {
      ERROR("the QR algorithm in zgeev failed to compute all eigvals");
      ret = false;
    }
  else
    {
      eigvals->resize(size, 1);
      fill_from_lapack_array(evals, *eigvals);
      eigvecs->resize(size, size);
      fill_from_lapack_array(evecs, *eigvecs);
    }

  freemem(copyA);
  freemem(evals);
  freemem(evecs);
  freemem(workspace);
  freemem(rwork);

  return ret;
}

bool Lapack::eigenvalues_hermitian(const LMatrixCC *A, LMatrixRR *eigvals)
{
  int size = static_cast<int>(A->numRows());
  if (size != static_cast<int>(A->numColumns()))
    {
      ERROR("expected a square matrix");
      return false;
    }

  if (size == 0)
    {
      eigvals->resize(0, 1);
      return true;
    }

  bool ret = true;
  char dont = 'N';
  char triangle = 'U'; /* Upper triangular part makes symmetric matrix. */

  int wsize = 2 * size - 1;
  double *workspace = newarray_atomic(double, 2*wsize);
  double *rwork = newarray_atomic(double, 3 * size - 2);
  int info;

  double *copyA = make_lapack_array(*A);
  double *evals = newarray_atomic(double, size);

  zheev_(&dont,
         &triangle,
         &size,
         copyA,
         &size,
         evals,
         workspace,
         &wsize,
         rwork,
         &info);

  if (info < 0)
    {
      ERROR("argument passed to zheev had an illegal value");
      ret = false;
    }
  else if (info > 0)
    {
      ERROR("zheev did not converge");
      ret = false;
    }
  else
    {
      eigvals->resize(size, 1);
      fill_from_lapack_array(evals, *eigvals);
    }

  freemem(copyA);
  freemem(evals);
  freemem(workspace);
  freemem(rwork);

  return ret;
}

bool Lapack::eigenvectors_hermitian(const LMatrixCC *A,
                                    LMatrixRR *eigvals,
                                    LMatrixCC *eigvecs)
{
  int size = static_cast<int>(A->numRows());
  if (size != static_cast<int>(A->numColumns()))
    {
      ERROR("expected a square matrix");
      return false;
    }

  if (size == 0)
    {
      eigvals->resize(0, 1);
      eigvecs->resize(0, 0);
      return true;
    }

  bool ret = true;
  char doit = 'V';
  char triangle = 'U'; /* Upper triangular part makes symmetric matrix */

  int wsize = 2 * size - 1;
  double *workspace = newarray_atomic(double, 2*wsize);
  double *rwork = newarray_atomic(double, 3 * size - 2);
  int info;

  double *evecs = make_lapack_array(*A);
  double *evals = newarray_atomic(double, size);

  zheev_(&doit,
         &triangle,
         &size,
         evecs,
         &size,
         evals,
         workspace,
         &wsize,
         rwork,
         &info);

  if (info < 0)
    {
      ERROR("argument passed to zheev had an illegal value");
      ret = false;
    }
  else if (info > 0)
    {
      ERROR("zheev did not converge");
      ret = false;
    }
  else
    {
      eigvals->resize(size, 1);
      fill_from_lapack_array(evals, *eigvals);
      eigvecs->resize(size, size);
      fill_from_lapack_array(evecs, *eigvecs);
    }

  freemem(evals);
  freemem(evecs);
  freemem(workspace);
  freemem(rwork);

  return ret;
}

bool Lapack::SVD(const LMatrixCC *A,
                 LMatrixRR *Sigma,
                 LMatrixCC *U,
                 LMatrixCC *VT)
{
  bool ret = true;
  char doit = 'A';  // other options are 'S' and 'O' for singular vectors only
  int rows = static_cast<int>(A->numRows());
  int cols = static_cast<int>(A->numColumns());
  int info;
  int min = (rows <= cols) ? rows : cols;

  if (min == 0)
    {
      ERROR("expected a matrix with positive dimensions");
      return false;
    }

  int max = (rows >= cols) ? rows : cols;
  int wsize = 2 * min + max;
  double *workspace = newarray_atomic(double, 2 * wsize);
  double *rwork = newarray_atomic(double, 5 * min);

  double *copyA = make_lapack_array(*A);
  double *u = newarray_atomic(double, 2 * rows * rows);
  double *vt = newarray_atomic(double, 2 * cols * cols);
  double *sigma = newarray_atomic(double, 2 * min);

  zgesvd_(&doit,
          &doit,
          &rows,
          &cols,
          copyA,
          &rows,
          sigma,
          u,
          &rows,
          vt,
          &cols,
          workspace,
          &wsize,
          rwork,
          &info);

  if (info < 0)
    {
      ERROR("argument passed to zgesvd had an illegal value");
      ret = false;
    }
  else if (info > 0)
    {
      ERROR("zgesvd did not converge");
      ret = false;
    }
  else
    {
      U->resize(rows, rows);
      fill_from_lapack_array(u, *U);
      VT->resize(cols, cols);
      fill_from_lapack_array(vt, *VT);
      Sigma->resize(min, 1);
      fill_from_lapack_array(sigma, *Sigma);
    }

  freemem(workspace);
  freemem(rwork);
  freemem(copyA);
  freemem(u);
  freemem(vt);
  freemem(sigma);

  return ret;
}

bool Lapack::SVD_divide_conquer(const LMatrixCC *A,
                                LMatrixRR *Sigma,
                                LMatrixCC *U,
                                LMatrixCC *VT)
{
  bool ret = true;
  char doit = 'A';  // other options are 'S' and 'O' for singular vectors only
  int rows = static_cast<int>(A->numRows());
  int cols = static_cast<int>(A->numColumns());
  int info;
  int min = (rows <= cols) ? rows : cols;

  if (min == 0)
    {
      ERROR("expected a matrix with positive dimensions");
      return false;
    }

  int max = (rows >= cols) ? rows : cols;
  int wsize = min * min + 2 * min + max;

  double *workspace = newarray_atomic(double, 2 * wsize);
  int *iworkspace = newarray_atomic(int, 8 * min);
  double *rwork = newarray_atomic(double, 5 * min * min + 7 * min); // documentation not clear

  double *copyA = make_lapack_array(*A);
  double *u = newarray_atomic(double, 2 * rows * rows);
  double *vt = newarray_atomic(double, 2 * cols * cols);
  double *sigma = newarray_atomic(double, 2 * min);

  zgesdd_(&doit,
          &rows,
          &cols,
          copyA,
          &rows,
          sigma,
          u,
          &rows,
          vt,
          &cols,
          workspace,
          &wsize,
          rwork,
          iworkspace,
          &info);

  if (info < 0)
    {
      ERROR("argument passed to zgesdd had an illegal value");
      ret = false;
    }
  else if (info > 0)
    {
      ERROR("zgesdd did not converge");
      ret = false;
    }
  else
    {
      U->resize(rows, rows);
      fill_from_lapack_array(u, *U);
      VT->resize(cols, cols);
      fill_from_lapack_array(vt, *VT);
      Sigma->resize(min, 1);
      fill_from_lapack_array(sigma, *Sigma);
    }

  freemem(workspace);
  freemem(iworkspace);
  freemem(rwork);
  freemem(copyA);
  freemem(u);
  freemem(vt);
  freemem(sigma);

  return ret;
}

bool Lapack::least_squares(const LMatrixCC *A, const LMatrixCC *b, LMatrixCC *x)
{
  bool ret = true;
  char job = 'N';
  int info;
  int rows = static_cast<int>(A->numRows());
  int cols = static_cast<int>(A->numColumns());
  int brows = static_cast<int>(b->numRows());
  int bcols = static_cast<int>(b->numColumns());
  int min = (rows <= cols) ? rows : cols;
  int max = (rows >= cols) ? rows : cols;
  int wsize = min + ((bcols >= max) ? bcols : max);

  if (brows != rows)
    {
      ERROR("expected compatible right hand side");
      return false;
    }

  if (min == 0 || bcols == 0)
    {
      x->resize(cols, bcols);
      return true;
    }

  double *copyA = make_lapack_array(*A);
  double *copyb = make_lapack_array(*b);
  double *workspace = newarray_atomic(double, 2 * wsize);

  if (rows < cols)
    {
      // Make 'b' (copyb) into a cols x bcols matrix, by adding a zero block at
      // the bottom
      double *copyb2 = newarray_atomic_clear(double, 2 * cols * bcols);
      int bloc = 0;
      int copyloc = 0;
      for (int j = 0; j < bcols; j++)
        {
          copyloc = 2 * j * cols;
          for (int i = 0; i < 2 * brows; i++)
            {
              copyb2[copyloc++] = copyb[bloc++];
            }
        }
      freemem(copyb);
      copyb = copyb2;
    }

  zgels_(&job,
         &rows,
         &cols,
         &bcols,
         copyA,
         &rows,
         copyb,
         &max,
         workspace,
         &wsize,
         &info);

  if (info != 0)
    {
      ERROR("argument passed to zgels had an illegal value");
      ret = false;
    }
  else
    {
      x->resize(cols, bcols);
      if (rows > cols)
        {
          int copyloc = 0;
          for (int j = 0; j < bcols; j++)
            {
              auto xj = x->columnBegin(j);
              copyloc = 2 * j * rows;
              for (int i = 0; i < cols; i++, ++xj)
                {
                  double re = copyb[copyloc++];
                  double im = copyb[copyloc++];
                  x->ring().set_from_doubles(*xj, re, im);
                }
            }
        }
      else
        {
          fill_from_lapack_array(copyb, *x);
        }
    }

  freemem(copyA);
  freemem(copyb);
  freemem(workspace);

  return ret;
}

bool Lapack::least_squares_deficient(const LMatrixCC *A,
                                     const LMatrixCC *b,
                                     LMatrixCC *x)
{
  bool ret = true;
  int rows = static_cast<int>(A->numRows());
  int cols = static_cast<int>(A->numColumns());
  int brows = static_cast<int>(b->numRows());
  int bcols = static_cast<int>(b->numColumns());
  double rcond = -1.0;
  int rank, info;
  int min = (rows < cols) ? rows : cols;
  int max = (rows > cols) ? rows : cols;
  int wsize = 2 * min + ((bcols > max) ? bcols : max);

  if (brows != rows)
    {
      ERROR("expected compatible right hand side");
      return false;
    }

  if (min == 0 || bcols == 0)
    {
      x->resize(cols, bcols);
      return true;
    }

  double *copyA = make_lapack_array(*A);
  double *copyb = make_lapack_array(*b);
  double *workspace = newarray_atomic(double, 2 * wsize);
  double *sing = newarray_atomic(double, min);
  double *rwork = newarray_atomic(double, 5 * min);

  if (rows < cols)
    {
      // Make 'b' (copyb) into a cols x bcols matrix, by adding a zero block at
      // the bottom
      double *copyb2 = newarray_atomic_clear(double, 2 * cols * bcols);
      int bloc = 0;
      int copyloc = 0;
      for (int j = 0; j < bcols; j++)
        {
          copyloc = 2 * j * cols;
          for (int i = 0; i < 2 * brows; i++)
            {
              copyb2[copyloc++] = copyb[bloc++];
            }
        }
      freemem(copyb);
      copyb = copyb2;
    }

  zgelss_(&rows,
          &cols,
          &bcols,
          copyA,
          &rows,
          copyb,
          &max,
          sing,
          &rcond,
          &rank,
          workspace,
          &wsize,
          rwork,
          &info);

  if (info != 0)
    {
      ERROR("argument passed to zgelss had an illegal value");
      ret = false;
    }
  else
    {
      x->resize(cols, bcols);
      if (rows > cols)
        {
          int copyloc = 0;
          for (int j = 0; j < bcols; j++)
            {
              auto xj = x->columnBegin(j);
              copyloc = 2 * j * rows;
              for (int i = 0; i < cols; i++, ++xj)
                {
                  double re = copyb[copyloc++];
                  double im = copyb[copyloc++];
                  x->ring().set_from_doubles(*xj, re, im);
                }
            }
        }
      else
        {
          fill_from_lapack_array(copyb, *x);
        }
    }

  freemem(copyA);
  freemem(copyb);
  freemem(workspace);
  freemem(sing);
  freemem(rwork);

  return ret;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
