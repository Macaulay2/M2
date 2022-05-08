--- status: converted November 2021
--- author(s): 
--- notes: heft is defined in hilbert.m2

-- This is to be deprecated
undocumented (heft, Module)

doc ///
Node
  Key
    "heft vectors"
  Description
    Text
      A @EM "heft vector"@ for a polynomial ring is a vector with integer entries, of the same
      length as the degree vectors of the variables of the ring, whose dot product with each of
      them is (strictly) positive. Unless one is specified explicitly, then a good one will be
      found automatically. The heft vector is used in various internal algorithms, such as the
      one in @TO "basis"@, as a way of organizing the sequence of steps, proceeding incrementally
      to larger values of the dot product of the degree of a monomial with the heft vector.
    Example
      R = QQ[a..d];
      degrees R
      heft R
      S = QQ[a..d, DegreeRank => 4];
      degrees S
      heft S
      T = QQ[a,b, Degrees => {1,-1}]
      degrees T
      heft T
      U = QQ[a..d, Degrees => {{2,0}, {1,-1}, {0,-2}, {-1,-3}}]
      degrees U
      heft U
    Text
      The heft vector, multiplied by -1, is used as the weight vector in the monomial ordering of
      the degrees ring, and the @EM "order"@ of the series expansions of the Hilbert series refers
      to the weight formed with respect to that weight vector.
    Example
      hilbertSeries U
      describe ring numerator oo
      hilbertSeries(U, Order => 8)
    Text
      The heft vector is used in the computation of degrees of modules over a polynomial ring @TT "R"@,
      because it gives a homomorphism from the degrees ring of @TT "R"@ to the Laurent polynomial
      ring in one variable @TT "T"@ that sends monomials corresponding to the degrees of variables
      of @TT "R"@ to positive powers of @TT "T"@. See @TO (degree, Module)@.
    Example
      R = QQ[x,y, Heft => {3}];
      degree ideal x
  SeeAlso
    [monoid, Heft]
    degreesRing
    multidegree
    [hilbertSeries, Order]
  Subnodes
    heft
    findHeft

Node
  Key
     heft
    (heft, Ring)
    (heft, Monoid)
    (heft, PolynomialRing)
    (heft, QuotientRing)
  Headline
    heft vector of ring or monoid
  Usage
    heft R
  Inputs
    R:{Ring,Monoid}
  Outputs
    :List
      a weight co-vector of integers with length the same as @TO2 {degreeLength, "degree length"}@ of @TT "R"@, or
    :Nothing
      @TO "null"@ if there is no heft vector.
  Description
    Text
      @TT "heft"@ returns the @TO2 {"heft vectors", "heft vector"}@ in use for @TT "R"@, 
    Example
      S = QQ[a..d, DegreeRank => 4];
      degrees S
      heft S
  SeeAlso
    findHeft

Node
  Key
     findHeft
    (findHeft, List)
    [findHeft, DegreeRank]
  Headline
    find a heft vector for a list of degrees
  Usage
    findHeft(degs, DegreeRank => r)
  Inputs
    degs:List
      of multi-degrees, each of which is a list of integers of fixed length {\tt r}
    DegreeRank=>ZZ
  Outputs
    :List
      of integers of length {\tt r}, or
    :Nothing
      @TO "null"@, if there is none
  Description
    Text
      @TT "findHeft"@ returns a weight co-vector whose dot product with each member of @TT "degs"@ is strictly positive.
    Example
      findHeft({{-1,0}, {2,1}}, DegreeRank => 2)
      findHeft({}, DegreeRank => 0)
  SeeAlso
    heft
///
