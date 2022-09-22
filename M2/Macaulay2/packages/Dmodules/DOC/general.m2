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
    SetVariables:Boolean
      whether to set variables of the created algebra to be global
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
    WeylClosure
    (WeylClosure, Ideal)
    (WeylClosure, Ideal, RingElement)
  Headline
    Weyl closure of an ideal
  Usage
    WeylClosure I
    WeylClosure(I,f)
  Inputs
    I:Ideal
      a left ideal of the Weyl Algebra
    f:RingElement
      a polynomial
  Outputs
    :Ideal
      the Weyl closure (w.r.t. $f$) of $I$
  Description
    Text
     Let $D$ be the Weyl algebra 
     with generators $x_1,\dots,x_n$ and $\partial_1,\dots,\partial_n$ over a field $K$
     of characteristic zero, and denote
     $R = K(x_1..x_n)<\partial_1..\partial_n>$, 
     the ring of differential
     operators with rational function coefficients. The {\em Weyl closure}
     of an ideal $I$ in $D$ is the intersection of the extended ideal
     $R I$ with $D$.  It consists of all operators which vanish on the common
     holomorphic solutions of $D$ and is thus analogous to the radical
     operation on a commutative ideal.
     
     The 
     {\em partial Weyl closure} of $I$ with respect to a polynomial $f$
     is the intersection of the extended ideal $D[f^{-1}] I$ with $D$.
     
     The Weyl closure is computed by localizing $D/I$ with respect to
     a polynomial $f$ vanishing on the singular locus, and computing
     the kernel of the map $D \to D/I \to (D/I)[f^{-1}]$.
    Example
      makeWA(QQ[x])
      I = ideal(x*dx-2)
      holonomicRank I
      WeylClosure I
  Caveat
    The ideal I should be of finite holonomic rank, which can be tested
    manually by using the function holonomicRank. The Weyl closure of non-finite rank
    ideals or arbitrary submodules has not been implemented.
  SeeAlso
    Dlocalize
    singLocus
    holonomicRank
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
      D = makeWA(QQ[x_1..x_3])
      A = matrix{{1,1,1},{0,1,2}}
      b = {3,4}
      I = gkz(A,b,D)
      isHolonomic I
  SeeAlso
    Ddim
    holonomicRank
///

doc ///
  Key
    singLocus
    (singLocus,Module)
    (singLocus,Ideal)
  Headline
    singular locus of a D-module
  Usage
    singLocus M
    singLocus I
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
      singLocus I
  SeeAlso
    charIdeal
    holonomicRank
    Ddim
///

doc ///
  Key
    charIdeal
    (charIdeal,Ideal)
    (charIdeal,Module)
  Headline
    characteristic ideal of a D-module
  Usage
    charIdeal M, charIdeal I
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
      charIdeal I
  SeeAlso
    Ddim
    singLocus
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
    charIdeal
    singLocus
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
    charIdeal
    holonomicRank
    singLocus
///

doc ///
  Key
    PolySols
    (PolySols,Module)
    (PolySols,Ideal,List)
    (PolySols,Module,List)
    (PolySols,Ideal)
  Headline
    polynomial solutions of a holonomic system
  Usage
    PolySols I
    PolySols M
    PolySols(I,w)
    PolySols(M,w)
  Inputs
    M:Module
      over the Weyl algebra $D$
    I:Ideal
      holonomic ideal in the Weyl algebra $D$
    w:List
      a weight vector
  Outputs
    :List
      a basis of the polynomial solutions of $I$
      (or of $D$-homomorphisms between $M$ and the polynomial ring)
      using $w$ for Groebner deformations
  Description
    Text
      The polynomial solutions of a holonomic system form a
      finite-dimensional vector space.
      There are two algorithms implemented to get these solutions.
      The first algorithm is based on Gr\"obner deformations and
      works for ideals $I$ of PDE's - see the paper {\em Polynomial
      and rational solutions of a holonomic system} by
      Oaku, Takayama and Tsai (2000).  The second algorithm is based
      on homological algebra - see the paper {\em Computing
      homomorphisms between holonomic D-modules} by Tsai and Walther (2000).
    Example
      makeWA(QQ[x])
      I = ideal(dx^2, (x-1)*dx-1)
      PolySols I
  SeeAlso
    RatSols
    Dintegration
///

document { Key => Alg }
document {
     Key => [PolySols, Alg],
     Headline => "algorithm for finding polynomial solutions",
     UL {
	  {BOLD "GD", " -- uses Groebner deformations"},
	  {BOLD "Duality", " -- uses homological duality"}
	  }
     }
document {
     Key => GD,
     Headline => "an option for PolySols=>Alg",
     SeeAlso => "PolySols"
     }
document {
     Key => Duality,
     Headline => "an option for PolySols=>Alg",
     SeeAlso => "PolySols"
     }

doc ///
  Key
    RatSols
    (RatSols,Ideal,List,List)
    (RatSols,Ideal,RingElement,List)
    (RatSols,Ideal,List)
    (RatSols,Ideal,RingElement)
    (RatSols,Ideal)
  Headline
    rational solutions of a holonomic system
  Usage
    RatSols I
    RatSols(I,f)
    RatSols(I,f,w)
    RatSols(I,ff)
    RatSols(I,ff,w)
  Inputs
    I:Ideal
      holonomic ideal in the Weyl algebra @EM "D"@
    f:RingElement
      a polynomial
    ff:List
      a list of polynomials
    w:List
      a weight vector
  Outputs
    :List
      a basis of the rational solutions of @EM "I"@ with poles along @EM "f"@
      or along the polynomials in @TT "ff"@ using @EM "w"@
      for Groebner deformations
  Description
    Text
      The rational solutions of a holonomic system form a
      finite-dimensional vector space.
      The only possibilities for the poles of a rational solution
      are the codimension one components of the singular locus.
      An algorithm to compute rational solutions
      is based on Gr\"obner deformations and
      works for ideals $I$ of PDE's - see the paper {\em Polynomial
      and rational solutions of a holonomic system} by
      Oaku, Takayama and Tsai (2000).
    Example
      makeWA(QQ[x])
      I = ideal((x+1)*dx+5)
      RatSols I
  Caveat
    The most efficient method to find rational solutions of a system of differential
    equations is
    to find the singular locus, then try to find its irreducible
    factors.  With these, call RatSols(I, ff, w), where w
    should be generic enough so that the PolySols routine
    will not complain of a non-generic weight vector.
  SeeAlso
    PolySols
    RatExt
    DHom
///

doc ///
  Key
    diffOps
    (diffOps, RingElement, ZZ)
    (diffOps, Ideal, ZZ)
  Headline
    differential operators of up to the given order for a quotient polynomial ring
  Usage
    diffOps (I, k)
    diffOps (f, k)
  Inputs
    I:Ideal
      contained in a polynomial ring @EM "R"@
    f:RingElement
      an element of a polynomial ring @EM "R"@
    k:ZZ
      which is nonnegative
  Outputs
    :HashTable
      the differential operators of order at most @EM "k"@
      of the quotient ring @EM "R/I"@ (or @EM "R/(f)"@)
  Description
    Text
      Given an ideal $I$ of a polynomial ring $R$ the set of
      differential operators of the quotient ring $R/I$ having order
      less than or equal to $k$ forms a finitely generated module over
      $R/I$. This routine returns its generating set.
      
    Text 
      The output is in the form of a hash table.
      The key @TT "BasisElts"@ is a row vector of basic differential operators.
      The key @TT "PolyGens"@ is a matrix over @EM "R"@ whose column vectors represent
      differential operators of @EM "R/I"@ in the following way.  For each column
      vector, consider its image in @TT "R/I"@ then take its dot product with
      the @TT "BasisElts"@. This gives a differential operator, and
      the set of these operators generates the differential operators of
      @EM "R/I"@ of order @EM "k"@ or less as an @EM "(R/I)"@-module.
    Example
      R = QQ[x,y,z]
      I = ideal(x^2-y*z)
      diffOps(I, 3)
  SeeAlso
    putWeylAlgebra
///

document {
     Key => PolyGens,
     Headline => "a key of the hashtable generated by diffOps",
     SeeAlso => "diffOps"
     }

document {
     Key => BasisElts,
     Headline => "a key of the hashtable generated by diffOps",
     SeeAlso => "diffOps"
     }
