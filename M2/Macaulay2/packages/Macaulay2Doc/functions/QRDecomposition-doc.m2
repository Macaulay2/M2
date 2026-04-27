doc ///
   Key
     QRDecomposition
     (QRDecomposition, MutableMatrix)
     (QRDecomposition, Matrix)
   Headline
     compute a QR decomposition of a real matrix
   Usage
     (Q,R) = QRDecomposition A
   Inputs
     A:Matrix
       or @ofClass MutableMatrix@, over $RR_{53}$
   Outputs
     Q:Matrix
     R:Matrix
       $(Q,R)$ is the QR-decomposition of $A$
   Description
    Text
      If $A$ is a $m \times n$ matrix whose
      columns are linearly independent, the $QR$ decomposition of $A$
      is $QR = A$, where $Q$ is an $m \times m$ orthogonal matrix and
      $R$ is an upper triangular $m \times n$ matrix.
    Example
      A = random(RR^5, RR^3)
      (Q,R) = QRDecomposition A
    Text
      $R$ is upper triangular, and $Q$ is (close to) orthogonal.
    Example
      R
      (transpose Q) * Q
      clean(1e-10, oo)
      R - (transpose Q) * A
      clean(1e-10, oo)
    Text
      If the input is a @TO "MutableMatrix"@, then so are the output matrices.
    Example
      A = mutableMatrix(RR_53, 13, 5);
      fillMatrix A
      (Q,R) = QRDecomposition A
      Q*R-A
      clean(1e-10,oo)
    Text
      This function works by calling LAPACK routines, and so only uses the first 53 bits of precision.
      Lapack also has a way of returning an encoded pair of matrices that contain 
      enough information to reconstruct $Q, R$.
   Caveat
     If the matrices are over higher precision real or complex fields, such as $RR_100$, this
     extra precision is not used in the computation
   SeeAlso
     clean
///
