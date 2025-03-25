doc ///
Node
  Key
    ConnectionMatrices
  Headline
    Writing $D$-ideals in connection form.
  SeeAlso
    "Dmodules :: Dmodules"
///

doc ///
Key
    normalForm
   (normalForm, RingElement, RingElement)
   (normalForm, RingElement, List)
Headline
    computes the normal form of an element in the Weyl algebra
Usage
    normalForm(P,Q)
    normalForm(P,G)
Inputs
    P:RingElement
        in the Weyl algebra
    Q:RingElement
        in the Weyl algebra
    G:List
        of elements in the Weyl algebra (tipically a Gröbner basis of a $D_n$-ideal)
Outputs
    normalForm:RingElement
        reduced form with respect to a list of elements in the Weyl algebra
Description
  Text
    This method computes the normal form of an element $P$ in the Weyl algebra $D_n$ with respect to another element in the Weyl algebra, or a whole list of such elements.
    The reduction step is carried out over the rational Weyl algebra $R_n$.
  Example
    w = {0,0,1,1};
    D = makeWA(QQ[x,y],w);
    P = dx^2 ; Q = x*dx+1;
    normalForm(P, Q)
Caveat
  Currently, the output lives in the fraction field of the variables, but this is not encoded as an element of the rational Weyl algebra
SeeAlso

///

doc ///
Key
    gaugeMatrix
    (gaugeMatrix, List, List)
    (gaugeMatrix, Ideal, List)
Headline
    computes base changes over the field of rational functions
Usage
    gaugeMatrix(G, B)
Inputs
    I:Ideal
      of the Weyl algebra
    G:List
      of the elements of a Gröbner basis of $I$
    B:List
      a basis of $R_n/R_nI$ over the field of rational functions
Outputs
    M:Matrix
      encoding the change of basis wrt. to B
Description
  Text
    Let $I$ be a $D$-ideal, $G$ a Gröbner basis of $I$ with respect to some elimination term order on $D_n$ and a $B$ basis of $R_n/R_nI$.
    This methods computes the matrix which encodes the change of basis from the set of standard monomials of $R_nI$ to the basis $B$.
  Example
    w1 = {0,0,2,1}; D1 = makeWeylAlgebra(QQ[x,y],w1);
    I = sub(ideal(x*dx^2-y*dy^2+2*dx-2*dy,x*dx+y*dy+1),D1);
    SM1 = standardMonomials(I);
    w2 = {0,0,1,2}; D2 = makeWeylAlgebra(QQ[x,y],w2);
    SM2 = standardMonomials(sub(I,D2));
    G = flatten entries gens gb I;
    gaugeMatrix(G,SM2)
SeeAlso

///

doc ///
Key
    connectionMatrices
    (connectionMatrices, Ideal)
    (connectionMatrices, List, Ideal)
Headline
    computes the connection matrices of a $D$-ideal $I$ for a choosen basis
Usage
    connectionMatrices(I)
    connectionMatrices(B, I)
Inputs
    I:Ideal
      $D_n$-ideal
    B:List
      a basis of $R_n/R_nI$
Outputs
    A:List
      the system of connection matrices of $I$ in @TT "fractionField(ring I)"@
Description
  Text
    Let $I$ be an ideal in the Weyl algebra $D_n$ and $B$ a basis over @TT "fractionField(D)"@ for $R_n/R_nI$.

    If no basis is provided by the user, the basis is chosen to be the set of standard monomials of a Gröbner basis on $R_nI$ with regards to the weighted
    Lex order $(\partial_1 > \cdots > \partial_n > x_1 > \cdots > x_n)$ on the Weyl algebra.
  Example
    D = makeWeylAlgebra(QQ[x,y], w = {0,0,2,1})
    I = ideal (x*dx^2-y*dy^2+2*dx-2*dy, x*dx+y*dy+1)
    A = connectionMatrices(I)
  Text
    More interesting examples arising from physics can also be handled. For example, the connection matrices of the annihilating $D$-ideal of a correlation function in cosmology can be computed as follows.
  Example

Caveat

///

doc ///
Key
    gaugeTransform
    (gaugeTransform, Matrix, List, PolynomialRing)
Headline
    computes the gauge transform of a system of connection matrices
Usage
    gaugeTransform(M, A, D)
Inputs
    M:Matrix
      change of basis matrix
    A:List
      a system of connection matrices
    D:Ring
      the Weyl algebra
Outputs
    L:List
      the system of connection matrices $\widetilde{A}_i = M A_i M^{-1} + (\partial_i \bullet M)M^{-1}$ after the gauge transformation with respect to $M$.
Description
  Text
    This method computes the gauge transform of the system of connection matrices for a given change of basis.
  Example
    D = makeWeylAlgebra(QQ[x,y]);
    I = ideal(x*dx^2-y*dy^2+2*dx-2*dy, x*dx+y*dy+1);
    A = connectionMatrices(I);
    M = matrix{{x,0},{0,y}};
    gaugeTransform(M,A,D)
SeeAlso

///

doc ///
Key
    connectionMatrix
    (connectionMatrix, Ideal)
    (connectionMatrix, List)
Headline
    computes the connection matrix
Usage
    connectionMatrix(I)
    connectionMatrix(A)
Inputs
    I:Ideal
      of the Weyl algebra
    A:List
      a system of connection matrices
Outputs
    M:Matrix
      the connection matrix
Description
  Text
    This method encodes the system of connection matrices in a single matrix.
    Its entries are differential one-forms in the variables of the underlying Weyl algebra.
  Example
    D = makeWeylAlgebra(QQ[x,y]);
    I = ideal(x*dx^2-y*dy^2+2*dx-2*dy, x*dx+y*dy+1);
    A = connectionMatrices(I);
    connectionMatrix(A)
Caveat
  The output is purely for visualizing purposes.
SeeAlso

///

doc ///
Key
    standardMonomials
    (standardMonomials, Ideal)
    (standardMonomials, List)
Headline
    computes the standard monomials for a $D$-ideal
Usage
    standardMonomials(I)
    standardMonomials(G)
Inputs
    I:Ideal
      of the Weyl algebra
    G:List
      of the elements of a Gröbner basis of an ideal in the Weyl algebra
Outputs
    L:List
      of standard monomials
Description
  Text
    This method computes the standard monomials of a Gröbner basis of $R_nI$ with respect
    to the order $\prec'$ on $R_n$ that is induced by the order $\prec$ of the Weyl algebra $D_n$.
  Example
    D = makeWeylAlgebra(QQ[x,y]);
    I = ideal(x*dx^2-y*dy^2+2*dx-2*dy, x*dx+y*dy+1);
    standardMonomials(I)
SeeAlso

///


doc ///
Key
    isEpsilonFactorized
    (isEpsilonFactorized, List, RingElement)
    (isEpsilonFactorized, Matrix, RingElement)
Headline
    checks if a system of connection matrices is in epsilon-factorized form
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
      @TT "true"@ or @TT "false"
Description
  Text
    This method returns true if the system of connection matrices factors out (a power of) @TT "eps"@, such that the resulting matrices are independent of @TT "eps"@.
  Example
    D = makeWeylAlgebra(frac(QQ[eps,DegreeRank=>0])[x]);
    I = ideal(x*(1-x)*dx^2 - eps*(1-x)*dx);
    B = {sub(1,D),sub(1/eps,D)*dx};
    A = connectionMatrices(B,I)
    isEpsilonFactorized(A,eps)
SeeAlso

///


doc ///
Key
    isIntegrable
    (isIntegrable, PolynomialRing, List)
    (isIntegrable, List)
Headline
    checks whether a list of matrices fulfills the integrability conditions
Usage
    isIntegrable(D,A)
    isIntegrable(A)
Inputs
    D:PolynomialRing
      the Weyl algebra
    A:List
      a system of matrices
Outputs
    p:Boolean
      whether $[A_i,A_j] = \partial_i(A_j) - \partial_j(A_i)$ for all $i,j$
Description
  Text
    Checks whether a list of $n$ matrices $A_i$ in $Mat_{m\times m}(k(x_1..x_n))$ satisfy
    $[A_i,A_j] = dx_i(A_j) - dx_j(A_i)$ for all $i,j$.
    This is the case, in particular, when they come from a $D$-module, respectively from a $D$-ideal.
  Example
    D = makeWeylAlgebra(QQ[x,y], w = {0,0,1,2});
    I = ideal(x*dx^2 - y*dy^2 + dx-dy, x*dx+y*dy+1);
    A = connectionMatrices I;
    assert(isIntegrable(D,A))
    assert(isIntegrable(A))
Caveat
  The matrices need to be defined over the field of rational functions @TT "fractionField(D)"@.
SeeAlso

///

end--

uninstallPackage "ConnectionMatrices"
restart
installPackage "ConnectionMatrices"
viewHelp "ConnectionMatrices"
