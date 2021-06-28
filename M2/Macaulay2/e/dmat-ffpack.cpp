// Copyright 2005-2012  Michael E. Stillman

#if 0
// This file is not in use.  These functions are now in dmat.cpp.
#if 0
    template<typename  CoeffRing >
    template<class RingType>
    size_t DMat < CoeffRing >::rank(typename enable_if<is_givaro_or_ffpack<RingType>::value >::type* dummy ) const
    {
        // assert not necessary because the test is already done by  "enable_if<is_givaro_or_ffpack<RingType>::value >"
        // assert( typeid(CoeffRing) == typeid(M2::ARingZZpFFPACK) || typeid(CoeffRing) == typeid(M2::ARingGFGivaro ));
        std::cout << "Calling rankGF_or_FFPACK" << std::endl;
        ElementType *N = newarray(ElementType, n_rows() * n_cols() );
        /// @jakob replace with memcopy or something fast.
        /// @jakob potention problem: (  n_rows()*n_cols() ) - overflow for big matrices 
        /// @jakob write a logger ur ose a logger for warnings/messages. Ideal case: if disabled, logger messaging is optimized out by compiler.
        copy_elems( n_rows()*n_cols(), N, 1, get_array(), 1); 
        /// @note 1. matrix data (N) is modified by FFPACK
        /// @note 2. FFPACK expects row-wise stored matrices while dmat stores them column-wise => switch n_rows and n_cols -parameters!
    
        /* //debug
        typename MatrixType::ElementType *Npos=N;
        for ( int currRow=0; currRow < n_rows(); currRow++ )
        for ( int currCol =0; currCol < n_cols(); currCol++ )
        {
            typename MatrixType::ElementType entry;
                get_entry(currRow, currCol,entry)  ;
                ring().field().init(Npos, entry );
            //  mat.setEntry( currRow, currCol ,( (int)rand() ) % characteristic );
        }*/
    
        size_t result = FFPACK::Rank(ring().field(), n_cols(), n_rows(),  N,  n_rows() );
        deletearray(N);
        return result;
    }
#endif

    template<typename CoeffRing>
    size_t FFpackRank(const DMat<CoeffRing>& mat)
    {
        typedef typename CoeffRing::ElementType ElementType;
        std::cout << "Calling FFpackRank" << std::endl;
        assert( typeid(CoeffRing) == typeid(M2::ARingZZpFFPACK) || typeid(CoeffRing) == typeid(M2::ARingGFGivaro ));
        ElementType* N = newarray( ElementType, mat.n_rows() * mat.n_cols());    
        mat.copy_elems(mat.n_rows()*mat.n_cols(), N, 1, mat.get_array(), 1); 
        /// @note 1. matrix data (N) is modified by FFPACK
        /// @note 2. FFPACK expects row-wise stored matrices while dmat stores them column-wise => switch n_rows and n_cols -parameters!
        size_t result = FFPACK::Rank(mat.ring().field(), mat.n_cols(), mat.n_rows(),  N,  mat.n_rows());
        deletearray(N);
        return result;
    }

    template<typename CoeffRing>
    void FFpackDeterminant(const DMat<CoeffRing>& mat, 
                                 typename CoeffRing::ElementType& result )
    {
        typedef typename CoeffRing::ElementType ElementType;
        std::cout << "Calling FFpackDeterminant" << std::endl;
        assert( typeid(CoeffRing) == typeid(M2::ARingZZpFFPACK) || typeid(CoeffRing) == typeid(M2::ARingGFGivaro ));
        ElementType* N = newarray( ElementType, mat.n_rows() * mat.n_cols());    
        mat.copy_elems(mat.n_rows()*mat.n_cols(), N, 1, mat.get_array(), 1); 
        /// @note 1. matrix data (N) is modified by FFPACK
        /// @note 2. FFPACK expects row-wise stored matrices while dmat stores them column-wise => switch n_rows and n_cols -parameters!
        result = FFPACK::Det(mat.ring().field(), mat.n_cols(), mat.n_rows(),  N,  mat.n_rows());
        deletearray(N);
    }

    template<typename CoeffRing>
    bool FFpackInvert(const DMat<CoeffRing> &mat, DMat<CoeffRing> &inverse)
    {
        typedef typename CoeffRing::ElementType ElementType;
        assert(mat.n_rows() == mat.n_cols());
        ElementType* N = newarray( ElementType, mat.n_rows() * mat.n_cols());    
        mat.copy_elems(mat.n_rows()*mat.n_cols(), N, 1, mat.get_array(), 1); 
    
        size_t n = mat.n_rows(); // same as n_cols()
        int nullspacedim;
        FFPACK::Invert2(mat.ring().field(), n, N, n, inverse.get_array(), n, nullspacedim);
    
        deletearray(N);
        return true;
    }

    template<typename CoeffRing>
    void FFpackNullSpace(const DMat<CoeffRing> &mat, 
                         DMat<CoeffRing> &nullspace, 
                         bool right_side)
    {
        right_side = !right_side; // because FFPACK stores by rows, not by columns.

        typedef typename CoeffRing::ElementType ElementType;
        ElementType* N = newarray( ElementType, mat.n_rows() * mat.n_cols());    
        mat.copy_elems(mat.n_rows()*mat.n_cols(), N, 1, mat.get_array(), 1); 
    
        size_t nr = mat.n_rows();
        size_t nc = mat.n_cols();
    
        ElementType *nullspaceFFPACK = 0;
    
        size_t nullspace_dim;
        size_t nullspace_leading_dim;
    
        FFPACK::NullSpaceBasis(mat.ring().field(),
                               (right_side ? FFLAS::FflasRight : FFLAS::FflasLeft),
                               nc, nr, N, nr, nullspaceFFPACK, nullspace_leading_dim, nullspace_dim);
    
        std::cerr << "leading dim = " << nullspace_leading_dim << " and dim = " << nullspace_dim << std::endl;
        //NOTUSED? size_t nullspace_nrows = (right_side ? nc : nullspace_dim);
        if (right_side && nullspace_dim != nullspace_leading_dim)
          {
            std::cerr << "error: this should not happen!" << std::endl;
          }
        else if (!right_side && nullspace_leading_dim != nc)
          {
            std::cerr << "error: this should not happen either!" << std::endl;
          }
    
        if (right_side)
          nullspace.resize(nullspace_dim,nr);
        else
          nullspace.resize(nc,nullspace_dim);
    
        mat.copy_elems(nullspace.n_rows() * nullspace.n_cols(), nullspace.get_array(), 1, nullspaceFFPACK, 1); 
    
        delete [] nullspaceFFPACK;
    }

    template<typename CoeffRing>
    M2_arrayintOrNull FFpackRankProfile(const DMat<CoeffRing> &mat,
                                        bool row_profile)
    {
        // Note that FFPack stores matrices by row, not column, the opposite of what we do.
        // So row_profile true means use ffpack column rank profile!

        typedef typename CoeffRing::ElementType ElementType;
        ElementType* N = newarray( ElementType, mat.n_rows() * mat.n_cols());    
        mat.copy_elems(mat.n_rows()*mat.n_cols(), N, 1, mat.get_array(), 1); 
    
        size_t * prof; // this is where the result will be placed
    
        size_t rk;
        if (!row_profile)
          rk = FFPACK::RowRankProfile(mat.ring().field(),
                                      mat.n_cols(),mat.n_rows(),
                                      N,mat.n_rows(),
                                      prof);
        else
          rk = FFPACK::ColumnRankProfile(mat.ring().field(),
                                         mat.n_cols(),mat.n_rows(),
                                         N,mat.n_rows(),
                                         prof);
        
        M2_arrayint profile = M2_makearrayint(static_cast<int>(rk));
        for (size_t i=0; i<rk; i++)
          profile->array[i] = static_cast<int>(prof[i]);
    
        delete [] prof;
        deletearray(N);

        return profile;
    }

    template<typename CoeffRing>
    bool FFpackSolveLinear(const DMat<CoeffRing> &mat, 
                           DMat<CoeffRing> &X, 
                           const DMat<CoeffRing> &B, 
                           bool right_side)
    {
        std::cerr << "inside FFpackSolveLinear" << std::endl;

        typedef typename CoeffRing::ElementType ElementType;
        size_t a_rows = mat.n_rows();
        size_t a_cols = mat.n_cols();
    
        size_t b_rows = B.n_rows();
        size_t b_cols = B.n_cols();
    
        ElementType* ffpackA = newarray(ElementType, mat.n_rows() * mat.n_cols());
        mat.copy_elems(mat.n_rows()*mat.n_cols(), ffpackA, 1, mat.get_array(), 1); 
    
        ElementType* ffpackB = newarray(ElementType, b_rows * b_cols);
        B.copy_elems(b_rows * b_cols, ffpackB, 1, B.get_array(), 1); 
    
        // preallocate the space for the solutions:
        size_t x_rows = (right_side ? a_cols : b_rows);
        size_t x_cols = (right_side ? b_cols : a_rows);
        //NOTUSED? size_t n_eqns = (right_side ? b_cols : b_rows);
    
        ElementType *ffpackX = newarray_clear(ElementType, x_rows * x_cols);
    
        int info; // >0 if the system is inconsistent, ==0 means success
    
        FFPACK::fgesv(mat.ring().field(),
                      (!right_side ? FFLAS::FflasLeft : FFLAS::FflasRight),
                      a_cols, a_rows, 
                      (!right_side ? b_cols : b_rows),
                      ffpackA,
                      a_rows, // leading dim of A
                      ffpackX, x_rows,
                      ffpackB, b_rows,
                      &info);
    
        if (info > 0)
          {
            // the system is inconsistent
            ERROR("the system is inconsistent");
            return false;
          }
    
        X.resize(x_rows, x_cols);
        X.copy_elems(x_rows * x_cols, X.get_array(), 1, ffpackX, 1); 
    
        delete [] ffpackX;
    
        return true;
    } 

 

   
    template<typename CoeffRing>
    void FFpackScalarMultiplyInPlace(DMat<CoeffRing>& A, 
                                            
                             const typename CoeffRing::ElementType& scalar)
    {
        size_t incx = 1;
         FFLAS::fgemm( A.ring().field(), A.n_rows()*A.n_cols(), scalar, A.get_array(), incx);
    }
    

 template<typename CoeffRing>
    void FFpackMatrixAdd(DMat<CoeffRing>& C, 
                             const DMat<CoeffRing>& A,
                             const DMat<CoeffRing>& B)
    {

        FFLAS::fadd ( C.ring().field(),  
                C.n_cols(),  C.n_rows(),
                A.get_array(), A.n_rows(),
                B.get_array(), B.n_rows(),
                C.get_array(), C.n_rows() );
    }

 template<typename CoeffRing>
    void FFpackMatrixAddInPlace(DMat<CoeffRing>& C, 
                                const DMat<CoeffRing>& A)
    {

        FFLAS::faddin ( C.ring().field(),  
                C.n_cols(),  C.n_rows(),
                A.get_array(), A.n_rows(),
                C.get_array(), C.n_rows() );
    }

 template<typename CoeffRing>
    void FFpackMatrixSub(DMat<CoeffRing>& C, 
                             const DMat<CoeffRing>& A,
                             const DMat<CoeffRing>& B)
    {

        FFLAS::fsub ( C.ring().field(),  
                C.n_cols(),  C.n_rows(),
                A.get_array(), A.n_rows(),
                B.get_array(), B.n_rows(),
                C.get_array(), C.n_rows() );
    }

    // there is no BLAS or even FFPACK routine for transposing a matrix.
    
    template<typename CoeffRing>
    void FFpackAddMultipleTo(DMat<CoeffRing>& C, 
                             const DMat<CoeffRing>& A,
                             const DMat<CoeffRing>& B,
                             bool transposeA,
                             bool transposeB,
                             const typename CoeffRing::ElementType& a,
                             const typename CoeffRing::ElementType& b)
    /* A,B,C should be mutable matrices over a finite prime field, and a,b
       elements of this field.
       C = b*C + a * op(A)*op(B),
       where op(A) = A or transpose(A), depending on transposeA
       where op(B) = B or transpose(B), depending on transposeB
    */
    { 

        FFLAS::FFLAS_TRANSPOSE tA = (transposeA ? FFLAS::FflasTrans : FFLAS::FflasNoTrans);
        FFLAS::FFLAS_TRANSPOSE tB = (transposeB ? FFLAS::FflasTrans : FFLAS::FflasNoTrans);

        size_t m = (transposeB ? B.n_rows() : B.n_cols());
        size_t n = (transposeA ? A.n_cols() : A.n_rows());
        
        size_t k = (transposeA ? A.n_rows() : A.n_cols());
        size_t k2 = (transposeB ? B.n_cols() : B.n_rows());

        assert(k == k2); // The user of this function must insure that sizes are correct.
        if (k!=k2)
        {
            ERROR("matrices are not composable. this error is handled in Macaulay2 code and this message should never appear! ");
            return;
        }
            

        FFLAS::fgemm( C.ring().field(),
                     tB, tA,
                     m,n,k,
                     a,
                     B.get_array(),
                     B.n_rows(),
                     A.get_array(),
                     A.n_rows(),
                     b,
                     C.get_array(),
                     C.n_rows()
                     );
    }

    //////////////////////////////////////////////////////
    // ARingZZpFFPACK specific linear algebra functions //
    //////////////////////////////////////////////////////
    template<>
    void DMat<M2::ARingZZpFFPACK>::addInPlace(const DMat& B)
    {
        std::cout << "DMat<M2::ARingZZpFFPACK>::addInPlace()" << std::endl;
        FFpackMatrixAddInPlace<M2::ARingZZpFFPACK>(*this,B);
    }


    template<>
    size_t DMat<M2::ARingZZpFFPACK>::rank() const
    {
        std::cout << "DMat<M2::ARingZZpFFPACK>::rank()" << std::endl;
        return FFpackRank<M2::ARingZZpFFPACK>(*this);
    }

    template<>
    void DMat<M2::ARingZZpFFPACK>::determinant(elem &result) const
    {
        std::cout << "Calling  DMat<M2::ARingZZpFFPACK>::determinant" << std::endl;
        FFpackDeterminant<M2::ARingZZpFFPACK>(*this, result);
    }

    template<>
    bool DMat<M2::ARingZZpFFPACK>::invert(DMat<M2::ARingZZpFFPACK> &inverse) const
    {
        std::cout << "Calling  DMat<M2::ARingZZpFFPACK>::inverse" << std::endl;
        return FFpackInvert<M2::ARingZZpFFPACK>(*this, inverse);
    }

    template<>
    void DMat<M2::ARingZZpFFPACK>::nullSpace(DMat<M2::ARingZZpFFPACK> &nullspace, bool right_side) const
    {
        std::cout << "Calling  DMat<M2::ARingZZpFFPACK>::nullspace" << std::endl;
        FFpackNullSpace<M2::ARingZZpFFPACK>(*this, nullspace, right_side);
    }

    template<>
    M2_arrayintOrNull DMat<M2::ARingZZpFFPACK>::rankProfile(bool row_profile) const
    {
        std::cout << "Calling  DMat<M2::ARingZZpFFPACK>::rankProfile" << std::endl;
        return FFpackRankProfile(*this, row_profile);
    }

    template<>
    bool DMat<M2::ARingZZpFFPACK>::solveLinear(DMat<M2::ARingZZpFFPACK> &X, 
                                               const DMat<M2::ARingZZpFFPACK> &B, 
                                               bool right_side) const
    {
        std::cout << "Calling  DMat<M2::ARingZZpFFPACK>::solveLinear" << std::endl;
        return FFpackSolveLinear(*this, X, B, right_side);
    }

    template<>
    void DMat<M2::ARingZZpFFPACK>::addMultipleTo(const DMat<M2::ARingZZpFFPACK> &A,
                                                 const DMat<M2::ARingZZpFFPACK> &B,
                                                 bool transposeA,
                                                 bool transposeB,
                                                 const ElementType& a,
                                                 const ElementType& b)
    {
        std::cout << "Calling  DMat<M2::ARingZZpFFPACK>::addMultipleTo *" << std::endl;
        FFpackAddMultipleTo(*this, A, B, transposeA, transposeB, a, b);
    }

#if 1

    //////////////////////////////////////////////////////
    // ARingGFGivaro specific linear algebra functions /////////
    //////////////////////////////////////////////////////

    template<>
    void DMat<M2::ARingGFGivaro>::addInPlace(const DMat& B)
    {
      std::cout << "not implemented yet" << std::endl;
    }

    template<>
    size_t DMat<M2::ARingGFGivaro>::rank() const
    {
        std::cout << "Calling DMat<M2::ARingGFGivaro>::rank()" << std::endl;
        return FFpackRank<M2::ARingGFGivaro>(*this);
    }
    
    template<>
    void DMat<M2::ARingGFGivaro>::determinant(elem &result) const
    {
        std::cout << "Calling  DMat<M2::ARingGFGivaro>::determinant" << std::endl;
        FFpackDeterminant<M2::ARingGFGivaro>(*this, result );
    }
    
    template<>
    bool DMat<M2::ARingGFGivaro>::invert(DMat<M2::ARingGFGivaro> &inverse) const
    {
        std::cout << "Calling  DMat<M2::ARingGFGivaro>::inverse" << std::endl;
        return FFpackInvert<M2::ARingGFGivaro>(*this, inverse);
    }

    template<>
    void DMat<M2::ARingGFGivaro>::nullSpace(DMat<M2::ARingGFGivaro> &nullspace, bool right_side) const
    {
        std::cout << "Calling  DMat<M2::ARingGFGivaro>::nullspace" << std::endl;
        FFpackNullSpace<M2::ARingGFGivaro>(*this, nullspace, right_side);
    }

    template<>
    M2_arrayintOrNull DMat<M2::ARingGFGivaro>::rankProfile(bool row_profile) const
    {
        std::cout << "Calling  DMat<M2::ARingGFGivaro>::rankProfile" << std::endl;
        return FFpackRankProfile(*this, row_profile);
    }

    template<>
    bool DMat<M2::ARingGFGivaro>::solveLinear(DMat<M2::ARingGFGivaro> &X, 
                                               const DMat<M2::ARingGFGivaro> &B, 
                                               bool right_side) const
    {
        std::cout << "Calling  DMat<M2::ARingGFGivaro>::solveLinear" << std::endl;
        return FFpackSolveLinear(*this, X, B, right_side);
    }

    template<>
    void DMat<M2::ARingGFGivaro>::addMultipleTo(const DMat<M2::ARingGFGivaro> &A,
                                                 const DMat<M2::ARingGFGivaro> &B,
                                                 bool transposeA,
                                                 bool transposeB,
                                                 const ElementType& a,
                                                 const ElementType& b)
    {
        std::cout << "Calling  DMat<M2::ARingGFGivaro>::addMultipleTo" << std::endl;
        FFpackAddMultipleTo(*this, A, B, transposeA, transposeB, a, b);
    }

#endif
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
