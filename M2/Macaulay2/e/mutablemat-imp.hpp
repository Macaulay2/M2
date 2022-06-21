// Copyright 2015 Anton Leykin and Mike Stillman

// Anton Leykin's code in this file is in the public domain.

#ifndef _mutable_mat_imp_hpp_
#define _mutable_mat_imp_hpp_

template <typename Mat>
M2SLEvaluator* MutableMat<Mat>::createSLEvaluator(M2SLProgram* P,
                                                M2_arrayint constsPos,
                                                M2_arrayint varsPos) const
{
  if (n_rows() != 1 || n_cols() != constsPos->len) {
    ERROR("1-row matrix expected; or numbers of constants don't match");
    return nullptr;
  } else return new M2SLEvaluator(
    new SLEvaluatorConcrete<typename Mat::CoeffRing> (&(P->value()), constsPos, varsPos, this)
  );
}

template <typename T>
size_t MutableMat<T>::rank() const
{
  return MatrixOps::rank(mat);
}

template <typename T>
const RingElement* MutableMat<T>::determinant() const
{
  ring_elem det;
  elem a;
  mat.ring().init(a);
  MatrixOps::determinant(mat, a);
  //  MatrixOps::BasicLinAlg<MatType>::determinant(mat, a);
  mat.ring().to_ring_elem(det, a);
  mat.ring().clear(a);
  return RingElement::make_raw(get_ring(), det);
}

template <typename T>
MutableMatrix* MutableMat<T>::invert() const
{
  assert(n_rows() == n_cols());
  MutableMat<T>* result = makeZeroMatrix(n_rows(), n_cols());
  bool val = MatrixOps::inverse(mat, result->mat);
  if (!val)
    {
      delete result;
      return 0;
    }
  return result;
}

template <typename T>
MutableMatrix* MutableMat<T>::rowReducedEchelonForm() const
{
  MutableMat<T>* result = makeZeroMatrix(n_rows(), n_cols());
  try
    {
      // ignore returned value (the rank of mat):
      MatrixOps::rowReducedEchelonForm(mat, result->mat);
      return result;
  } catch (const exc::engine_error& e)
    {
      delete result;
      throw;
  }
}

template <typename T>
MutableMatrix* MutableMat<T>::solveLinear(const MutableMatrix* B) const
{
  const T* B1 = B->coerce_const<T>();
  if (B1 == 0) throw exc::engine_error("expected matrices of the same type");
  if (B->get_ring() != get_ring())
    throw exc::engine_error("expected same ring");
  if (B->n_rows() != n_rows())
    throw exc::engine_error("expected matrices with same number of rows");
  MutableMat<T>* solns = makeZeroMatrix(0, 0);
  bool retval = MatrixOps::solveLinear(mat, *B1, solns->mat);
  if (retval) return solns;
  delete solns;
  return NULL;
}

template <typename T>
MutableMatrix* MutableMat<T>::solveInvertible(const MutableMatrix* B) const
{
  const T* B1 = B->coerce_const<T>();
  if (B1 == 0) throw exc::engine_error("expected matrices of the same type");
  if (B->get_ring() != get_ring())
    throw exc::engine_error("expected same ring");
  if (n_rows() != n_cols()) throw exc::engine_error("expected a square matrix");
  if (B->n_rows() != n_rows())
    throw exc::engine_error("expected matrices with same number of rows");
  MutableMat<T>* solns = makeZeroMatrix(0, 0);
  bool retval = MatrixOps::solveInvertible(mat, *B1, solns->mat);
  if (retval) return solns;
  delete solns;
  return NULL;
}

template <typename T>
MutableMatrix* MutableMat<T>::nullSpace() const
{
  MutableMat<T>* ker = makeZeroMatrix(0, 0);
  MatrixOps::nullSpace(mat, ker->mat);  // ignore return value of nullSpace...
  return ker;
}

template <typename T>
MutableMatrix /* or null */* MutableMat<T>::mult(const MutableMatrix* B) const
{
  // First, make sure B has the same ring/type as 'this'.
  const T* B1 = B->coerce_const<T>();
  if (B1 == 0)
    {
      ERROR(
          "mutable matrix/ring type for (mutable) matrix multiplication "
          "required to be the same");
      return 0;
    }
  // Second, make sure the sizes are correct.
  if (mat.numColumns() != B1->numRows())
    {
      ERROR("matrix sizes do not match in matrix multiplication");
      return 0;
    }
  // create the result matrix
  MutableMat<T>* result = makeZeroMatrix(n_rows(), B1->numColumns());
  MatrixOps::mult(mat, *B1, result->mat);

  return result;
}

template <typename T>
void MutableMat<T>::addMultipleTo(const MutableMatrix* A,
                                  const MutableMatrix* B)
{
  // First: make sure that A, B have the right ring/matrix type
  const T* A1 = A->coerce_const<T>();
  const T* B1 = B->coerce_const<T>();
  if (A1 == 0 or B1 == 0)
    throw exc::engine_error(
        "mutable matrix/ring type for (mutable) matrix multiplication required "
        "to be the same");
  if (mat.numRows() != A1->numRows() or mat.numColumns() != B1->numColumns())
    throw exc::engine_error(
        "expected matrix sizes to be compatible with matrix multiplication");
  MatrixOps::addMultipleTo(mat, *A1, *B1);
}

template <typename T>
void MutableMat<T>::subtractMultipleTo(const MutableMatrix* A,
                                       const MutableMatrix* B)
{
  // First: make sure that A, B have the right ring/matrix type
  const T* A1 = A->coerce_const<T>();
  const T* B1 = B->coerce_const<T>();
  if (A1 == 0 or B1 == 0)
    throw exc::engine_error(
        "mutable matrix/ring type for (mutable) matrix multiplication required "
        "to be the same");
  if (mat.numRows() != A1->numRows() or mat.numColumns() != B1->numColumns())
    throw exc::engine_error(
        "expected matrix sizes to be compatible with matrix multiplication");
  MatrixOps::subtractMultipleTo(mat, *A1, *B1);
}

template <typename T>
M2_arrayintOrNull MutableMat<T>::rankProfile(bool row_profile) const
{
  //  LUComputation<T> C(mat);
  //  return C.rankProfile(row_profile);
  return MatrixOps::rankProfile(mat, row_profile);
}

template <typename T>
M2_arrayintOrNull MutableMat<T>::LU(MutableMatrix* L, MutableMatrix* U) const
{
  T* L1 = L->coerce<T>();
  T* U1 = U->coerce<T>();
  if (L1 == 0 or U1 == 0)
    throw exc::engine_error("expected matrices of the same ring/type");
  return MatrixOps::LU(mat, *L1, *U1);
}

template <typename T> // T should be a matrix type, generally DMat<RT>
M2_arrayintOrNull MutableMat<T>::LUincremental(std::vector<size_t>& P,
                                  const MutableMatrix* v,
                                  int m)
{
  T* LU1 = this->coerce<T>();
  const T* v1 = const_cast<MutableMatrix*>(v)->coerce<T>();
  if (LU1 == nullptr or v1 == nullptr)
    throw exc::engine_error("expected matrices of the same ring/type");
  return MatrixOps::LUincremental(P, *LU1, *v1, m);
}

template <typename T> // T should be a matrix type, generally DMat<RT>
void MutableMat<T>::triangularSolve(MutableMatrix* x,
                                    int m,
                                    int strategy)
{
  T* Lv1 = this->coerce<T>();
  T* x1 = x->coerce<T>();
  if (Lv1 == nullptr or x1 == nullptr)
    throw exc::engine_error("expected matrices of the same ring/type");
  MatrixOps::triangularSolve(*Lv1, *x1, m, strategy);
}

template <typename T>
bool MutableMat<T>::eigenvalues(MutableMatrix* eigenvals,
                                bool is_symm_or_hermitian) const
{
  if (!is_dense())
    throw exc::engine_error(
        "'eigenvalues' is only implemented for dense matrices");
  if (is_symm_or_hermitian)
    {
      auto E1 = eigenvals->coerce<DMat<HermitianEigenvalueType> >();
      if (E1 == 0)
        throw exc::engine_error("eigenvalue matrix is of the wrong type/ring");
      return MatrixOps::eigenvaluesHermitian(mat, *E1);
    }
  else
    {
      auto E1 = eigenvals->coerce<DMat<EigenvalueType> >();
      if (E1 == 0)
        throw exc::engine_error("eigenvalue matrix is of the wrong type/ring");
      return MatrixOps::eigenvalues(mat, *E1);
    }
}

template <typename T>
bool MutableMat<T>::eigenvectors(MutableMatrix* eigenvals,
                                 MutableMatrix* eigenvecs,
                                 bool is_symm_or_hermitian) const
{
  if (!is_dense())
    throw exc::engine_error(
        "'eigenvalues' is only implemented for dense matrices");
  if (is_symm_or_hermitian)
    {
      DMat<HermitianEigenvalueType>* E1 =
          eigenvals->coerce<DMat<HermitianEigenvalueType> >();
      DMat<HermitianEigenvectorType>* evecs1 =
          eigenvecs->coerce<DMat<HermitianEigenvectorType> >();
      if (E1 == 0 or evecs1 == 0)
        throw exc::engine_error(
            "eigenvalue/vector matrix is of the wrong type/ring");
      return MatrixOps::eigenvectorsHermitian(mat, *E1, *evecs1);
    }
  else
    {
      DMat<EigenvalueType>* E1 = eigenvals->coerce<DMat<EigenvalueType> >();
      DMat<EigenvectorType>* evecs1 =
          eigenvecs->coerce<DMat<EigenvectorType> >();
      if (E1 == 0 or evecs1 == 0)
        throw exc::engine_error(
            "eigenvalue/vector matrix is of the wrong type/ring");
      return MatrixOps::eigenvectors(mat, *E1, *evecs1);
    }
}

template <typename T>
bool MutableMat<T>::least_squares(const MutableMatrix* B,
                                  MutableMatrix* X,
                                  bool assume_full_rank) const
{
  const T* B1 = B->coerce_const<T>();
  T* X1 = X->coerce<T>();
  if (B1 == 0 or X1 == 0)
    throw exc::engine_error("expected matrices of the same type");
  bool retval = MatrixOps::leastSquares(mat, *B1, *X1, assume_full_rank);
  return retval;
}

template <typename T>
bool MutableMat<T>::QR(MutableMatrix* Q, MutableMatrix* R, bool return_QR) const
{
  if (!is_dense())
    throw exc::engine_error("'QR' is only implemented for dense matrices");

  auto Q1 = Q->coerce<DMat<CoeffRing> >();
  if (Q1 == 0) throw exc::engine_error("Q matrix is of the wrong type/ring");
  auto R1 = R->coerce<DMat<CoeffRing> >();
  if (R1 == 0) throw exc::engine_error("R matrix is of the wrong type/ring");
  return MatrixOps::QR(mat, *Q1, *R1, return_QR);
}

template <typename T>
bool MutableMat<T>::SVD(MutableMatrix* Sigma,
                        MutableMatrix* U,
                        MutableMatrix* Vt,
                        bool use_divide_and_conquer) const
{
  auto Sigma2 = Sigma->coerce<DMat<HermitianEigenvalueType> >();
  T* U2 = U->coerce<T>();
  T* Vt2 = Vt->coerce<T>();
  if (Sigma2 == 0 || U2 == 0 || Vt2 == 0)
    throw exc::engine_error("expected matrices of the same type");
  int strategy = (use_divide_and_conquer ? 1 : 0);
  return MatrixOps::SVD(mat, *Sigma2, *U2, *Vt2, strategy);
}

template <typename Mat>
engine_RawArrayIntPairOrNull MutableMat<Mat>::LQUPFactorizationInPlace(
    bool transpose)
{
  throw exc::engine_error(
      "LU decomposition currently not implemented for this ring and matrix "
      "type");
}

template <typename T>
void MutableMat<T>::clean(gmp_RR epsilon)
{
  MatrixOps::clean(epsilon, mat);
}

template <typename T>
gmp_RRorNull MutableMat<T>::norm() const
{
  if (get_ring()->get_precision() == 0)
    throw exc::engine_error("expected a matrix over RR or CC");

  gmp_RRmutable nm = getmemstructtype(gmp_RRmutable);
  mpfr_init2(nm, get_ring()->get_precision());
  mpfr_set_si(nm, 0, MPFR_RNDN);

  MatrixOps::increase_norm(nm, mat);
  return moveTo_gmpRR(nm);
}

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
