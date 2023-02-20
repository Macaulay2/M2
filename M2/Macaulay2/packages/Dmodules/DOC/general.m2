doc ///
  Key
    makeWeylAlgebra
    (makeWeylAlgebra,PolynomialRing)
    [makeWeylAlgebra,SetVariables]
  Headline
    Weyl algebra corresponding to a polynomial ring
  Usage
    makeWeylAlgebra R
    makeWA R
  Inputs
    R:PolynomialRing
      the (commutative) polynomial ring
    SetVariables=>Boolean
      indicates whether the generators should be assigned to global variables
  Outputs
    :PolynomialRing
      the (non-commutative) Weyl algebra
  Description
    Text
      Given a polynomial ring @EM "R"@ with variables @EM "x_1,..,x_n"@,
      this routine returns a Weyl algebra with variables @EM "x_1,..,x_n"@
      and @EM "dx_1,..,dx_n"@.
    Example
      R = QQ[x,y,z]
      D = makeWeylAlgebra R
    Text
      To skip naming the ring, use parentheses.
    Example
      makeWA(QQ[x,y,z])
  Caveat
    The polynomial ring R must be commutative.
  SeeAlso
    WeylAlgebra
///

doc ///
  Key
    isHolonomic
    (isHolonomic, Module)
    (isHolonomic, Ideal)
  Headline
    determines whether a D-module (or ideal in Weyl algebra) is holonomic
  Usage
    isHolonomic M
    isHolonomic I
  Inputs
    M:Module
      over the Weyl algebra @EM "D"@
    I:Ideal
      which represents the module @EM "M = D/I"@
  Outputs
    :Boolean
  Description
    Text
      Let $D$ be the Weyl algebra
      with generators $x_1,\dots,x_n$ and $\partial_1,\dots,\partial_n$.
      over a field.
      A $D$-module is holonomic if it has dimension $n$.
      For more details see [@HREF("https://mathscinet.ams.org/mathscinet/pdf/1734566.pdf","SST")@, Section 1.4].

    Example
--      needsPackage "HolonomicSystems"
      D = makeWA(QQ[x_1..x_3])
      A = matrix{{1,1,1},{0,1,2}}
      b = {3,4}
--      I = gkz(A,b,D)
--      isHolonomic I
  SeeAlso
    Ddim
    holonomicRank
///

doc ///
  Key
    DsingularLocus
    (DsingularLocus,Module)
    (DsingularLocus,Ideal)
  Headline
    singular locus of a D-module
  Usage
    DsingularLocus M
    DsingularLocus I
  Inputs
    M:Module
      over the Weyl algebra @EM "D"@
    I:Ideal
      which represents the module @EM "M = D/I"@
  Outputs
    :Ideal
      the singular locus of @EM "M"@
  Description
    Text
      The singular locus of the system of PDE's given by @EM "I"@
      generalizes the notion of singular point of an ODE.
      Geometrically, the singular locus of a D-module @EM "M"@
      equals the projection
      of the characteristic variety of @EM "M"@ minus the zero section
      of the cotangent bundle to the base affine space @BOLD "C"@ @EM "^n"@.
      @BR{}@ 
      More details can be found in 
      [@HREF("https://mathscinet.ams.org/mathscinet/pdf/1734566.pdf","SST")@, Section 1.4].
    Example
      makeWA(QQ[x,y])
      I = ideal (x*dx+2*y*dy-3, dx^2-dy)
      DsingularLocus I
  SeeAlso
    characteristicIdeal
    holonomicRank
    Ddim
///

doc ///
  Key
    characteristicIdeal
    (characteristicIdeal,Ideal)
    (characteristicIdeal,Module)
  Headline
    characteristic ideal of a D-module
  Usage
    characteristicIdeal M, characteristicIdeal I
  Inputs
    M:Module
      over the Weyl algebra @EM "D"@
    I:Ideal
      which represents the module @EM "M = D/I"@
  Outputs
    :Ideal
      the characteristic ideal of @EM "M"@
  Description
    Text
      
      The characteristic ideal of @EM "M"@ is the annihilator of
      @EM "gr(M)"@ under a good filtration with respect to the order
      filtration. 
      If $D$ is the Weyl algebra over &#x2102;
      with generators $x_1,\dots,x_n$ and $\partial_1,\dots,\partial_n$,
      then the order filtration corresponds to the weight vector $(0,...,0,1,...,1)$.
      The characteristic ideal lives in the associated graded ring of $D$ with respect to
      the order filtration, and this is a commutative polynomial ring
      &#x2102;$[x_1,\dots,x_n,\xi_1,\dots,\xi_n]$.
      Here the $\xi_i$ is the principal symbol of $\partial_i$, that is, the image of $\partial_i$
      in the associated graded ring.
      The zero locus of the characteristic ideal is equal to the {\em characteristic variety}
      of @EM "D/I"@ which is an invariant of a D-module.
   Text
      The algorithm to compute the characteristic ideal consists of computing
      the initial ideal of I with respect to the weight vector
      $(0,...,0,1...,1)$.  
      More details can be found in 
      [@HREF("https://mathscinet.ams.org/mathscinet/pdf/1734566.pdf","SST")@, Section 1.4].
    
   Example
      makeWA(QQ[x,y])
      I = ideal (x*dx+2*y*dy-3, dx^2-dy)
      characteristicIdeal I
  SeeAlso
    Ddim
    DsingularLocus
    holonomicRank
///

doc ///
  Key
    holonomicRank
    (holonomicRank,Module)
    (holonomicRank,Ideal)
  Headline
    rank of a D-module
  Usage
    holonomicRank M
    holonomicRank I
  Inputs
    M:Module
      over the Weyl algebra @EM "D"@
    I:Ideal
      which represents the module @EM "M = D/I"@
  Outputs
    :ZZ
      the rank of @EM "M"@
  Description
    Text
      The {\em holonomic rank} of a D-module @EM "M = D^r/N"@ provides analytic information
      about the system of PDE's given by @EM "N"@. By the Cauchy-Kovalevskii-Kashiwara Theorem,
      the dimension of the space of germs of holomorphic solutions to @EM "N"@ in a
      neighborhood of a nonsingular point is equal to the holonomic rank of @EM "M"@.
    
    Text
      The holonomic rank of a D-module is defined algebraically as follows.
      Let $D$ be the Weyl algebra
      with generators $x_1,\dots,x_n$ and $\partial_1,\dots,\partial_n$ over &#x2102;.
      and let $R$ denote the ring of differential operators
      &#x2102;$(x_1,\dots,x_n)<\partial_1,\dots,\partial_n>$
--      @BOLD "C"@ @TT "("@ @EM "x_1,...,x_n"@ @TT ")"@ @TT "<"@ @EM "d_1,...,d_n"@ @TT ">"@
      with rational function coefficients.
      Then the holonomic rank of $M = D^r/N$ is equal to the dimension of
      $R^r/RN$ as a vector space over
      &#x2102;$[x_1,\dots,x_n]$.
      More details can be found in 
      [@HREF("https://mathscinet.ams.org/mathscinet/pdf/1734566.pdf","SST")@, Section 1.4].
--    
--       @BOLD "C"@ ( @EM "x_1,...,x_n"@ ).
--      @BR{}@
--      See the book 'Groebner deformations of hypergeometric differential equations' by
--      Saito-Sturmfels-Takayama (1999) for more details of the algorithm.
    
    Example
      makeWA(QQ[x,y])
      I = ideal (x*dx+2*y*dy-3, dx^2-dy)
      holonomicRank I
  SeeAlso
    characteristicIdeal
    DsingularLocus
    Ddim
///

doc ///
  Key
    Ddim
    (Ddim,Ideal)
    (Ddim,Module)
  Headline
    dimension of a D-module
  Usage
    Ddim M
    Ddim I
  Inputs
    M:Module
      over the Weyl algebra $D$
    I:Ideal
      which represents the module $M=D/I$
  Outputs
    :ZZ
      the dimension of $M$
  Description
    Text
      The dimension of $M$ is equal to the dimension of
      the associated graded module with respect to the Bernstein
      filtration. 
      If $D$ is the Weyl algebra over &#x2102;
      with generators $x_1,\dots,x_n$ and $\partial_1,\dots,\partial_n$,
      then the Bernstein filtration corresponds to the weight vector $(1,...,1,1,...,1)$.
    Example
      makeWA(QQ[x,y])
      I = ideal (x*dx+2*y*dy-3, dx^2-dy)
      Ddim I
  SeeAlso
    characteristicIdeal
    holonomicRank
    DsingularLocus
///
