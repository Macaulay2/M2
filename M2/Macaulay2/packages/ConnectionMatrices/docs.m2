doc ///
Node
  Key
    ConnectionMatrices
  Headline
    writing and working with $D$-ideals in connection form
  Description
    Text
      -- excerpt from the paper
      Systems of homogeneous, linear partial differential equations (PDEs)
      with polynomial coefficients are encoded by left ideals in the Weyl algebra, denoted
      $$ D_n=\CC[x_1,\ldots,x_n]\langle \partial_1,\ldots,\partial_n \rangle. $$
      Such systems can be systematically written as a first-order matrix system known as a "Pfaffian system" or "connection form",
      by utilizing Gröbner bases in the Weyl algebra [@HREF("https://mathscinet.ams.org/mathscinet/pdf/1734566.pdf","SST")@, pp. 37].
      The systematic computation of connection matrices requires Gröbner basis computations in the rational Weyl algebra
      $$ R_n=\CC(x_1,\ldots,x_n)\langle \partial_1,\ldots,\partial_n \rangle. $$
    Tree
      :Working with the rational Weyl algebra
        normalForm
	standardMonomials
      :Computing and displaying $D$-ideals in connection form
        connectionMatrices
        connectionMatrix
      :Changing basis of a system of connection matrices
        gaugeMatrix
        gaugeTransform
	isEpsilonFactorized
      :Testing integrability of a list of matrices
        isIntegrable
    Text
      As an example, consider the GKZ system representing the Gauss hypergeometric function
      (c.f. Examples 1.2.9 [SST, pp. 14]).
    Example
      -- Example 1.2.9 in SST, pp. 14
      D = makeWeylAlgebra(QQ[a,b,c, DegreeRank => 0][x_1..x_4]);
      I = ideal(
	  dx_2*dx_3 - dx_1*dx_4,
	  x_1*dx_1 - x_4*dx_4 + 1 - c,
	  x_2*dx_2 + x_4*dx_4 + a,
	  x_3*dx_3 + x_4*dx_4 + b);
      -- TODO: make it so we don't need to specify these weights
      holonomicRank({0,0,0,0,0,0,0}, comodule I)
      standardMonomials I
    Text
      Example 1.2.9 [SST, pp. 39] computes the connection matrices (also known as the Pfaffian system)
      for this system with constants $a=1/2,b=1/2,c=1$. Using the @TO connectionMatrices@ function,
      we can find the system for arbitrary constants.
    Example
      A = connectionMatrices I;
      isIntegrable A
      netList(Boxes => false, VerticalSpace => 1, apply(4, i -> i+1 => A#i))
    Text
      Substituting the constants in Example 1.2.9, we note that example contains a misprint.
    -- Example
    --   -- Example 1.4.23 in SST, pp. 39
    --   netList(Boxes => false, VerticalSpace => 1,
    -- 	  apply(4, i -> i+1 => sub(A#i, {a => 1/2, b => 1/2, c => 1})))
    Code
      D = makeWeylAlgebra(QQ[a,b,c, DegreeRank => 0][x_1..x_4]);
      I = ideal(
	  dx_2*dx_3 - dx_1*dx_4,
	  x_1*dx_1 - x_4*dx_4 + 1 - c,
	  x_2*dx_2 + x_4*dx_4 + a,
	  x_3*dx_3 + x_4*dx_4 + b);
      A = connectionMatrices I;
      UL apply(4, i -> concatenate_("$A_"|i+1|"=") substring_1 tex sub(A#i, {a => 1/2, b => 1/2, c => 1}))
  References
    Most algorithms in this package can be found in the book
    {\em Gröbner deformations of Hypergeometric Differential Equations} by Saito, Sturmfels and Takayama,
    cited here as [@HREF("https://mathscinet.ams.org/mathscinet/pdf/1734566.pdf","SST")@].
  SeeAlso
    "Dmodules :: Dmodules"
    "HolonomicSystems :: HolonomicSystems"
  Subnodes
    normalForm
    standardMonomials
    connectionMatrices
    connectionMatrix
    gaugeMatrix
    gaugeTransform
    isEpsilonFactorized
    isIntegrable
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
    v = {1,1};
    D = makeWA(QQ[x,y],v);
    P = dx^2 ; Q = x*dx+1;
    normalForm(P, Q)
References
  See Theorem 1.1.7 [@HREF("https://mathscinet.ams.org/mathscinet/pdf/1734566.pdf","SST")@, pp. 7].
Caveat
  Currently, the output lives in the fraction field of the variables, but this is not encoded as an element of the rational Weyl algebra
SeeAlso

///

doc ///
Key
    baseFractionField
   (baseFractionField, PolynomialRing)
Headline
    extracts the fraction field of the base polynomial ring of a Weyl algebra
Usage
    baseFractionField D
Inputs
    D:PolynomialRing
        a Weyl algebra
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
    Let $I$ be a $D_n$-ideal, $G$ a Gröbner basis of $I$ with respect to some elimination term order on $D_n$ and a $B$ basis of $R_n/R_nI$.
    This methods computes the matrix which encodes the change of basis from the set of standard monomials of $R_nI$ to the basis $B$.
  Example
    w1 = {2,1}; D1 = makeWeylAlgebra(QQ[x,y],w1);
    I = sub(ideal(x*dx^2-y*dy^2+2*dx-2*dy,x*dx+y*dy+1),D1);
    SM1 = standardMonomials(I);
    w2 = {1,2}; D2 = makeWeylAlgebra(QQ[x,y],w2);
    SM2 = standardMonomials(sub(I,D2));
    G = flatten entries gens gb I;
    gaugeMatrix(G,SM2)
SeeAlso

///

doc ///
Key
    connectionMatrices
    (connectionMatrices, Ideal)
    (connectionMatrices, Ideal, List)
Headline
    computes the connection matrices of a $D$-ideal $I$ for a choosen basis
Usage
    connectionMatrices(I)
    connectionMatrices(I, B)
Inputs
    I:Ideal
      $D_n$-ideal
    B:List
      a basis of $R_n/R_nI$
Outputs
    A:List
      the system of connection matrices of $I$ over @TO2{baseFractionField, TT "baseFractionField(ring I)"}@
Description
  Text
    Let $I$ be an ideal in the Weyl algebra $D_n$ and $B$ a basis over @TO2{baseFractionField, TT "baseFractionField(D)"}@ for $R_n/R_nI$.

    If no basis is provided by the user, the basis is chosen to be the set of standard monomials of a Gröbner basis on $R_nI$ with regards to the weighted
    Lex order $(\partial_1 > \cdots > \partial_n > x_1 > \cdots > x_n)$ on the Weyl algebra.
  Example
    D = makeWeylAlgebra(QQ[x,y], v = {2,1})
    I = ideal (x*dx^2-y*dy^2+2*dx-2*dy, x*dx+y*dy+1)
    A = connectionMatrices(I)
  -- Text
  --   More interesting examples arising from physics can also be handled. For example, the connection matrices of the annihilating $D_3$-ideal of a correlation function in cosmology can be computed as follows.
  -- Example
  --   D = makeWeylAlgebra(frac(QQ[eps,DegreeRank=>0])[x,y,z],{1,1,1});
  --   delta1 = (x^2-z^2)*dx^2+2*(1-eps)*x*dx-eps*(1-eps);
  --   delta2 = (y^2-z^2)*dy^2+2*(1-eps)*y*dy-eps*(1-eps);
  --   delta3 = (x+z)*(y+z)*dx*dy-eps*(x+z)*dx-eps*(y+z)*dy+eps^2;
  --   h = x*dx+y*dy+z*dz-2*eps;
  --   I = ideal(delta1+delta3, delta2+delta3,h);
  --   A = connectionMatrices(I, {1,dx,dy,dx*dy})
References
  For more details, see [@HREF("https://mathscinet.ams.org/mathscinet/pdf/1734566.pdf","SST")@, pp. 37-40].
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
    This method computes the gauge transform of the system of connection matrices for a given change of basis.
  Example
    D = makeWeylAlgebra(QQ[x,y]);
    I = ideal(x*dx^2-y*dy^2+2*dx-2*dy, x*dx+y*dy+1);
    A = connectionMatrices(I);
    F = baseFractionField D;
    M = matrix(F, {{x,0},{0,y}});
    gaugeTransform(M,A,D)
  -- Text
  --   It is also possible to compute the gauge transform of a
  --   system of connection matrices contatining paramenters.
  --   The following example comes from the annihilating $D_n$-ideal
  --   of a correlation function in cosmology.
  -- Example
  --   D = makeWeylAlgebra(frac(QQ[eps,DegreeRank=>0])[x,y,z],{1,1,1})
  --   delta1 = (x^2-z^2)*dx^2+2*(1-eps)*x*dx-eps*(1-eps);
  --   delta2 = (y^2-z^2)*dy^2+2*(1-eps)*y*dy-eps*(1-eps);
  --   delta3 = (x+z)*(y+z)*dx*dy-eps*(x+z)*dx-eps*(y+z)*dy+eps^2;
  --   h = x*dx+y*dy+z*dz-2*eps;
  --   I = ideal(delta1+delta3, delta2+delta3,h)
  --   A = connectionMatrices(I, {1,dx,dy,dx*dy})
  --   R = baseFractionField D
  --   changeVar = transpose((1/(2*z*eps^2))*matrix({{2*z*eps^2, -eps^2*(x-z), -eps^2*(y-z), -eps^2*(x+y)},{0,eps*(x^2-z^2),0,eps*(x+y)*(x+z)},{0,0,eps*(y^2-z^2),eps*(x+y)*(y+z)},{0,0,0,-(x+y)*(x+z)*(y+z)}}));
  --   A2 = gaugeTransform(changeVar,A,D)
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
    computes the standard monomials for a $D_n$-ideal
Usage
    standardMonomials(I)
    standardMonomials(G)
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
      @TT "true"@ or @TT "false"@
Description
  Text
    This method returns true if the system of connection matrices factors out (a power of) @TT "eps"@, such that the resulting matrices are independent of @TT "eps"@.
  Example
    D = makeWeylAlgebra(frac(QQ[eps,DegreeRank=>0])[x]);
    I = ideal(x*(1-x)*dx^2 - eps*(1-x)*dx);
    B = {sub(1,D),sub(1/eps,D)*dx};
    A = connectionMatrices(I, B)
    isEpsilonFactorized(A,eps)
  Text
  --   In many examples coming from physics it is important to check
  --   this factorization property. The following example arises as
  --   the annihilating $D_n$-ideal
  --   of a correlation function in cosmology.
  -- Example
  --   D = makeWeylAlgebra(frac(QQ[eps,DegreeRank=>0])[x,y,z],{1,1,1})
  --   delta1 = (x^2-z^2)*dx^2+2*(1-eps)*x*dx-eps*(1-eps);
  --   delta2 = (y^2-z^2)*dy^2+2*(1-eps)*y*dy-eps*(1-eps);
  --   delta3 = (x+z)*(y+z)*dx*dy-eps*(x+z)*dx-eps*(y+z)*dy+eps^2;
  --   h = x*dx+y*dy+z*dz-2*eps;
  --   I = ideal(delta1+delta3, delta2+delta3,h)
  --   A = connectionMatrices(I, {1,dx,dy,dx*dy})
  --   R = baseFractionField D
  --   changeVar = transpose((1/(2*z*eps^2))*matrix({{2*z*eps^2, -eps^2*(x-z), -eps^2*(y-z), -eps^2*(x+y)},{0,eps*(x^2-z^2),0,eps*(x+y)*(x+z)},{0,0,eps*(y^2-z^2),eps*(x+y)*(y+z)},{0,0,0,-(x+y)*(x+z)*(y+z)}}));
  --   A2 = gaugeTransform(changeVar,A,D)
  --   isEpsilonFactorized(A2,eps)
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
    This is the case, in particular, when they come from a $D_n$-module, respectively from a $D_n$-ideal.
  Example
    D = makeWeylAlgebra(QQ[x,y], v = {1,2});
    I = ideal(x*dx^2 - y*dy^2 + dx-dy, x*dx+y*dy+1);
    A = connectionMatrices I;
    assert(isIntegrable(D,A))
    assert(isIntegrable(A))
Caveat
  The matrices need to be defined over the field of rational functions @TO2{baseFractionField, TT "baseFractionField(D)"}@.
SeeAlso

///

end--

uninstallPackage "ConnectionMatrices"
restart
installPackage "ConnectionMatrices"
viewHelp "ConnectionMatrices"
