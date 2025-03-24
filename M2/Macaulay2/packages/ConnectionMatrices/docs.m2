doc ///
Node
  Key
    ConnectionMatrices
  Headline
    System of connection matrices associated to $D$-ideals.
  SeeAlso
    "HolonomicSystems::HolonomicSystems"
///

doc ///
Key
    normalForm
   (normalForm, RingElement, RingElement)
   (normalForm, RingElement, List)
Headline
    computes normal form of an element in the Weyl algebra
Usage
    normalForm(f,g)
    normalForm(f,G)
Inputs
    f:RingElement
        in the Weyl algebra
    g:RingElement
        in the Weyl algebra
    G:List
        of elements in the Weyl algebra
Outputs
    normalForm:RingElement
        reduced form with respect to a list of elements in the Weyl algebra
Description
  Text
    This method computes the normal form of an element $f$ in the Weyl algebra $D$ with respect to another element in the Weyl algebra, or a whole list of such elements.
    Usually this list is a Gröbner basis for a left $D$-ideal.
  Example
    w = {0,0,1,1};
    D = makeWA(QQ[x,y],w);
    f = dx^2 ; g = x*dx+1;
    normalForm(f, g)
Caveat
  Currently the output lives in the fraction field of the variables, but this is not the correctly implemented rational Weyl algebra
SeeAlso

///

doc ///
Key
    gaugeMatrix
    (gaugeMatrix, List, List)
    (gaugeMatrix, Ideal, List)
Headline
    computes change of basis matrix of a D-ideal or a Groebner basis of a D-ideal wrt. to a new basis
Usage
    gaugeMatrix(G, B)
Inputs
    I:Ideal
      of the Weyl algebra
    G:List
      of Gröbner basis elements
    B:List
      of standard monomials
Outputs
    M:Matrix
      encoding the change of basis wrt. to B
Description
  Text
    Let $I$ be a $D$-module, $G$ a Gröbner basis of $I$ with respect to some monomial ordering on $D$ and a $B$ basis of standard monomials for the quotient of the rational Weyl algebra by the ideal.
    This methods computes the change of basis matrix over the rational Weyl algebra for going from $B$ to another basis of the quotient.
  Example
    w1 = {0,0,2,1}; D1 = makeWeylAlgebra(QQ[x,y],w1);
    I = sub(ideal(x*dx^2-y*dy^2+2*dx-2*dy,x*dx+y*dy+1),D1);
    SM1 = standardMonomials(I);
    w2 = {0,0,1,2}; D2 = makeWeylAlgebra(QQ[x,y],w2);
    SM2 = standardMonomials(sub(I,D2));
    G = flatten entries gens gb I;
    gaugeMatrix(G,SM2)
Caveat
  Currently the output lives in the fraction field of the variables, but this is not the correctly implemented rational Weyl algebra
SeeAlso

///

doc ///
Key
    connectionMatrices
    (connectionMatrices, Ideal)
    (connectionMatrices, List, Ideal)
Headline
    computes a system of connection matrices associated to I with respect to a basis of standard monomials of I
Usage
    connectionMatrices(I)
    connectionMatrices(B, I)
Inputs
    I:Ideal
      $D$-ideal
    B:List
      a basis of standard monomials for I
Outputs
    A:List
      the system of connection matrices of $I$ in @TT "fractionField(ring I)"@
Description
  Text
    Let $I$ be an ideal in the Weyl algebra $D_n$ and $B$ a basis over @TT "fractionField(D)"@ for $R_n/R_nI$.
    The $i$-th matrix in system of connection matrices encodes the action of $\partial_i$ on $R_n/R_nI$ with respect to the basis $B$.

    If a basis is not provided by the user, a basis of standard monomials will be chosen with regards to the weighted
    Lex order $(\partial_1 > \cdots > \partial_n > x_1 > \cdots > x_n)$ on the Weyl algebra.
  Example
    D = makeWeylAlgebra(QQ[x,y], w = {0,0,2,1})
    I = ideal (x*dx^2-y*dy^2+2*dx-2*dy, x*dx+y*dy+1)
    A = connectionMatrices(I)
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
      the system of connection matrices $\widetilde{A}_i = M A_i M^{-1} + (\partial_i \bullet M)M^{-1}$ after the gauge transform.
Description
  Text
    This method computes the gauge transform for system of connection matrices given a change of basis matrix.
  Example
    D = makeWeylAlgebra(QQ[x,y]);
    I = ideal(x*dx^2-y*dy^2+2*dx-2*dy, x*dx+y*dy+1);
    A = connectionMatrices(I);
    M = matrix{{x,0},{0,y}};
    gaugeTransform(M,A,D)
Caveat
  Currently the output lives in the fraction field of the variables adjoin differentials, but this is not the correctly implemented rational Weyl algebra
SeeAlso

///

doc ///
Key
    connectionMatrix
    (connectionMatrix, Ideal)
    (connectionMatrix, List)
Headline
    computes the differential connection matrix
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
      the differential connection matrix
Description
  Text
    This method encodes the system connection matrices in a single square matrix of size holonomic rank of the underlying $D$-module.
    Its entries are differential one forms in the variables of the Weyl algebra.
    We obtain the system of connection matrices by multiplying with a differential of the Weyl algebra.
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
      of generators of the Gröbner basis of an ideal in the Weyl algbra
Outputs
    L:List
      of standard monomials
Description
  Text
    This method computes the standard monomials which constitute a basis of the $D$-modules obtained from taking the quotient of the rational Weyl algebra by an ideal.
  Example
    D = makeWeylAlgebra(QQ[x,y]);
    I = ideal(x*dx^2-y*dy^2+2*dx-2*dy, x*dx+y*dy+1);
    standardMonomials(I)
Caveat
  Currently the output lives in the fraction field of the variables adjoin differentials, but this is not the correctly implemented rational Weyl algebra
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
      variable for which the factorization property should be tested
Outputs
    b:Boolean
Description
  Text
    This method returns true if the system of connection matrices allows to factor out @TT "eps"@
  Example
    D = makeWeylAlgebra(frac(QQ[e,DegreeRank=>0])[x]);
    I = ideal(x*(1-x)*dx^2 - e*(1-x)*dx);
    B = {sub(1,D),sub(1/e,D)*dx};
    A = connectionMatrices(B,I)
    isEpsilonFactorized(A,e)
SeeAlso

///


doc ///
Key
    isIntegrable
    (isIntegrable, PolynomialRing, List)
    (isIntegrable, List)
Headline
    verifies that the system of connection matrices describe an integrable connection
Usage
    isIntegrable(D,A)
    isIntegrable(A)
Inputs
    D:PolynomialRing
      the Weyl algebra
    A:List
      a system of connection matrices
Outputs
    p:Boolean
      whether $[A_i,A_j] = \partial_i(A_j) - \partial_j(A_i)$ for all $i,j$
Description
  Text
    A list of n matrices A_i in Mat_r(k(x_1..x_n)), describing n differential equations
      dx_i(vec(f)) = A_i * vec(f) (for all i),
    comes from an integrable connection if its matrices satisfy
      [A_i,A_j] = dx_i(A_j) - dx_j(A_i) for all i,j.
    This is the case, in particular, when they come from a $D$-module, respectively from a $D$-ideal.
  Example
    D = makeWeylAlgebra(QQ[x,y], w = {0,0,1,2});
    I = ideal(x*dx^2 - y*dy^2 + dx-dy, x*dx+y*dy+1);
    A = connectionMatrices I;
    assert(isIntegrable(D,A))
    assert(isIntegrable(A))
Caveat
  The matrices need to be defined over @TT "fractionField(D)"@.
SeeAlso

///

end--

uninstallPackage "ConnectionMatrices"
restart
installPackage "ConnectionMatrices"
viewHelp "ConnectionMatrices"
