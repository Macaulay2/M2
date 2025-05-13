doc ///
Node
  Key
    ConnectionMatrices
  Headline
    writing $D$-ideals in connection form
  Description
    Text
      -- excerpt from the paper
      Systems of homogeneous, linear partial differential equations (PDEs)
      with polynomial coefficients are encoded by left ideals in the Weyl algebra, denoted
      $$ D_n=\CC[x_1,\ldots,x_n]\langle \partial_1,\ldots,\partial_n \rangle. $$
      Such systems can be systematically written as a first-order matrix system known as a "Pfaffian system" or "connection form,"
      by utilizing Gröbner bases in the Weyl algebra [@HREF("https://link.springer.com/book/10.1007/978-3-662-04112-3","SST")@, p. 37].
      The systematic computation of connection matrices requires Gröbner basis computations in the rational Weyl algebra
      $$ R_n=\CC(x_1,\ldots,x_n)\langle \partial_1,\ldots,\partial_n \rangle. $$

      The theoretical foundations of our algorithms is described the companion paper to this package, available at @arXiv "2504.01362"@.
    Tree
      :Working with the rational Weyl algebra
        normalForm
	standardMonomials
	baseFractionField
      :Computing and displaying $D$-ideals in connection form
        connectionMatrices
        connectionMatrix
      :Changing basis of a system of connection matrices
        gaugeMatrix
        gaugeTransform
	isEpsilonFactorized
      :Testing integrability of a list of matrices
        isIntegrable
      :Examples
        "Cosmological correlator for the 2-site chain"
        "Massless one-loop triangle Feynman diagram"
        "Gauss' hypergeometric function"
  References
    The main reference for our algorithms is the book [@HREF("https://link.springer.com/book/10.1007/978-3-662-04112-3","SST")@] M. Saito, B. Sturmfels, and N. Takayama
    {\em Gröbner Deformations of Hypergeometric Differential Equations}, volume 6 of {\em Algorithms and Computation in Mathematics.} Springer, 2000.
  Acknowledgement
    Work on this package began at the workshop {\it Macaulay2 in the Sciences} held at MPI-MiS in Leipzig in November 2024.
    Devlin Mallory and Carlos Gustavo Rodriguez Fernandez contributed to the development of this package during the workshop.
  SeeAlso
    "Dmodules :: Dmodules"
    "HolonomicSystems :: HolonomicSystems"
  Subnodes
    normalForm
    standardMonomials
    baseFractionField
    connectionMatrices
    connectionMatrix
    gaugeMatrix
    gaugeTransform
    isEpsilonFactorized
    isIntegrable
    "Cosmological correlator for the 2-site chain"
    "Massless one-loop triangle Feynman diagram"
    "Gauss' hypergeometric function"
///

doc ///
Key
    normalForm
   (normalForm, RingElement, RingElement)
   (normalForm, RingElement, List)
Headline
    computes the normal form within the rational Weyl algebra
Usage
    normalForm(P, Q)
    normalForm(P, G)
Inputs
    P:RingElement
        in the Weyl algebra
    Q:RingElement
        in the Weyl algebra
    G:List
        of elements in the Weyl algebra (typically a Gröbner basis of a $D_n$-ideal)
Outputs
    normalForm:RingElement
        normalForm, computed in rational Weyl algebra with respect to a list of elements in the Weyl algebra
Description
  Text
    This method computes the normal form of an element $P$ in the Weyl algebra $D_n$ with respect to another element in the Weyl algebra, or a whole list of such elements.
    The reduction step is carried out over the rational Weyl algebra $R_n$.
  Example
    D = makeWA(QQ[x,y], {1,1});
    P = dx^2 ; Q = x*dx+1;
    normalForm(P, Q)
References
  See [@HREF("https://link.springer.com/book/10.1007/978-3-662-04112-3","SST")@, Theorem 1.1.7].
Caveat
  Due to technical limitations, the output lives in the graded associative algebra of the rational Weyl algebra,
  which is a commutative ring over the @TO2{baseFractionField, "base fraction field"}@ of $D_n$ where
  the partial differentials are adjoined as commuting variables.
///

doc ///
Key
    baseFractionField
   (baseFractionField, FractionField)
   (baseFractionField, PolynomialRing)
Headline
    extracts the fraction field of the base polynomial ring of a Weyl algebra
Usage
    baseFractionField D
Inputs
    D:"a Weyl algebra"
Outputs
    :FractionField
        the fraction field of the base polynomial ring of @TT "D"@
Description
  Text
    Several functions in this package return an object over the fraction field of the base
    polynomial ring of a Weyl algebra. This function extracts that ring.
  Example
    D = makeWA(frac(QQ[e])[a,b,c, DegreeRank => 0][x,y]);
    baseFractionField D
    gens oo
///

doc ///
Key
    gaugeMatrix
   (gaugeMatrix, List, List)
   (gaugeMatrix, Ideal, List)
Headline
    computes the base change over the field of rational functions
Usage
    gaugeMatrix(G, B)
Inputs
    I:Ideal
      of the Weyl algebra
    G:List
      of generators of a Gröbner basis for $I$
    B:List
      of standard monomials for $I$,
      i.e. any basis of $R_n/R_nI$ over the field of rational functions
Outputs
    M:Matrix
      encoding the change of basis with regard to to $B$
Description
  Text
    Let $I = D_n\langle G\rangle$ be a $D_n$-ideal, $B$ basis of $R_n/R_nI$.
    This methods computes the matrix which encodes the change of basis from the set of standard monomials of $R_nI$ to the basis $B$.
  Example
    D1 = makeWeylAlgebra(QQ[x, y], w1 = {2, 1});
    I = ideal(x*dx^2-y*dy^2+2*dx-2*dy, x*dx+y*dy+1);
    SM1 = standardMonomials I
    F = baseFractionField D1;
    --
    D2 = makeWeylAlgebra(QQ[x, y], w2 = {1, 2});
    SM2 = standardMonomials sub(I, D2)
    gaugeMatrix(I, SM2)
SeeAlso
  standardMonomials
  gaugeTransform
  "Cosmological correlator for the 2-site chain"
///

doc ///
Key
    connectionMatrices
    (connectionMatrices, Ideal)
    (connectionMatrices, Ideal, List)
Headline
    computes the connection matrices of a $D_n$-ideal $I$ for a chosen basis
Usage
    connectionMatrices I
    connectionMatrices(I, B)
Inputs
    I:Ideal
      $D_n$-ideal
    B:List
      a basis of $R_n/R_nI$
Outputs
    A:List
      the system of connection matrices of $I$ over the @TO2{baseFractionField, "base fraction field"}@ of $D_n$
Description
  Text
    Let $I$ be an ideal in the Weyl algebra $D_n$ and $B$ a basis for $R_n/R_nI$ over the
    @TO2{baseFractionField, "base fraction field"}@ of $D_n$. If no basis is provided by the user,
    the basis is chosen to be the set of standard monomials of a Gröbner basis on $R_nI$ with
    regards to the weighted Lex order $(\partial_1 > \cdots > \partial_n > x_1 > \cdots > x_n)$
    on the Weyl algebra.
  Example
    D = makeWeylAlgebra(QQ[x,y], {2, 1})
    I = ideal (x*dx^2-y*dy^2+2*dx-2*dy, x*dx+y*dy+1)
    A = connectionMatrices I
References
  For more details, see [@HREF("https://link.springer.com/book/10.1007/978-3-662-04112-3","SST")@, pp. 37-40].
///

doc ///
Key
    gaugeTransform
    (gaugeTransform, Matrix, List, PolynomialRing)
    (gaugeTransform, Matrix, List)
Headline
    computes the gauge transform of a system of connection matrices
Usage
    gaugeTransform(M, A, D)
    gaugeTransform(M, A)
Inputs
    M:Matrix
      change of basis matrix
    A:List
      a system of connection matrices
    D:Ring
      the Weyl algebra (if omitted, it will be inferred from first element of @TT "A"@)
Outputs
    L:List
      the system of connection matrices $\widetilde{A}_i = M A_i M^{-1} + (\partial_i \bullet M)M^{-1}$ after the gauge transformation with respect to $M$.
Description
  Text
    This method computes the gauge transform of a system of connection matrices for a given invertible matrix that encodes a change of basis.
  Example
    D = makeWeylAlgebra(QQ[x,y]);
    I = ideal(x*dx^2-y*dy^2+2*dx-2*dy, x*dx+y*dy+1);
    A = connectionMatrices I;
    M = matrix {{x,0}, {0,y}};
    gaugeTransform(M, A, D)
  Text
    It is also possible to compute the gauge transform of a system of connection matrices containing parameters.
SeeAlso
  gaugeMatrix
  "Cosmological correlator for the 2-site chain"
///

doc ///
Key
    connectionMatrix
    (connectionMatrix, Ideal)
    (connectionMatrix, List)
Headline
    computes the connection matrix
Usage
    connectionMatrix I
    connectionMatrix A
Inputs
    I:Ideal
      of the Weyl algebra
    A:List
      a system of connection matrices
Outputs
    :Net
      the connection matrix
Description
  Text
    This method encodes the system of connection matrices in a single matrix.
    Its entries are differential one-forms in the variables of the underlying Weyl algebra.
  Example
    D = makeWeylAlgebra(QQ[x,y]);
    I = ideal(x*dx^2-y*dy^2+2*dx-2*dy, x*dx+y*dy+1);
    connectionMatrix I
Caveat
  The output is purely for visualizing purposes.
SeeAlso
  connectionMatrices
///

doc ///
Key
    standardMonomials
    (standardMonomials, Ideal)
    (standardMonomials, List)
Headline
    computes the standard monomials for a $D_n$-ideal
Usage
    standardMonomials I
    standardMonomials G
Inputs
    I:Ideal
      of the Weyl algebra
    G:List
      of generators of the ideal
Outputs
    L:List
      of standard monomials
Description
  Text
    This method computes the standard monomials of a Gröbner basis of $R_nI$ with respect
    to the order $\prec'$ on $R_n$ which is induced by the order $\prec$ of the Weyl algebra $D_n$.
  Example
    D = makeWeylAlgebra(QQ[x,y]);
    I = ideal(x*dx^2-y*dy^2+2*dx-2*dy, x*dx+y*dy+1);
    standardMonomials I
    holonomicRank I
SeeAlso
  "Dmodules::holonomicRank"
  gaugeMatrix
///


doc ///
Key
    isEpsilonFactorized
    (isEpsilonFactorized, List, RingElement)
    (isEpsilonFactorized, Matrix, RingElement)
Headline
    checks whether a system of connection matrices is in $\epsilon$-factorized form
Usage
    isEpsilonFactorized(A,eps)
    isEpsilonFactorized(M,eps)
Inputs
    A:List
      a system of connection matrices
    M:Matrix
      a connection matrix
    eps:RingElement
      parameter for which the factorization property should be tested
Outputs
    b:Boolean
      @TT "true"@ or @TT "false"@
Description
  Text
    This method returns true if the system of connection matrices factors out (a power of) @TT "eps"@,
    such that the resulting matrices are independent of @TT "eps"@.
  Example
    D = makeWeylAlgebra(frac(QQ[ϵ])[x]);
    I = ideal(x*(1-x)*dx^2 - ϵ*(1-x)*dx);
    A = connectionMatrices(I, {1_D, 1/ϵ*dx})
    isEpsilonFactorized(A, ϵ)
SeeAlso
  isIntegrable
  gaugeTransform
///


doc ///
Key
    isIntegrable
    (isIntegrable, PolynomialRing, List)
    (isIntegrable, List)
Headline
    checks whether a list of matrices fulfills the integrability conditions
Usage
    isIntegrable(D, A)
    isIntegrable A
Inputs
    D:"a Weyl algebra"
    A:List
      a system of matrices
Outputs
    p:Boolean
      whether $[A_i,A_j] = \partial_i(A_j) - \partial_j(A_i)$ for all $i,j$
Description
  Text
    Checks whether a list of $n$ matrices $A_i$ in $\operatorname{Mat}_{m\times m}(k(x_1..x_n))$ satisfy
    $[A_i,A_j] = \partial_i(A_j) - \partial_j(A_i)$ for all $i,j$.
    This is the case, in particular, when they come from a $D_n$-module, respectively from a $D_n$-ideal.
  Example
    D = makeWeylAlgebra(QQ[x,y], {1, 2});
    I = ideal(x*dx^2 - y*dy^2 + dx-dy, x*dx+y*dy+1);
    A = connectionMatrices I;
    assert isIntegrable(D, A)
    assert isIntegrable A
Caveat
  The matrices need to be defined over the @TO2{baseFractionField, "base fraction field"}@ of $D_n$.
SeeAlso
  connectionMatrices
  isEpsilonFactorized
///
