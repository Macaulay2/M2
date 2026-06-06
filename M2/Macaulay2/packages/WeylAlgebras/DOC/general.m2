doc ///
  Key
    makeWeylAlgebra
    (makeWeylAlgebra,PolynomialRing)
    [makeWeylAlgebra,SetVariables]
    SetVariables
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
    [monoid, WeylAlgebra]
  Subnodes
    createDpairs
    extractDiffsAlgebra
    extractVarsAlgebra
    -- createThetaRing
///

-*
doc ///
  Key
    createThetaRing
   (createThetaRing, PolynomialRing)
    ThetaRing
    isGeneric
    WtoT
  Headline
  Usage
  Inputs
  Outputs
  Description
    Text
    Example
      n = 4
      W = QQ[t_0..t_n, x_0..x_n, dx_0..dx_n, WeylAlgebra => {x_0..x_n => dx_0..dx_n}]
      createThetaRing W
      W.ThetaRing
///
*-

document {
     Key => {createDpairs, (createDpairs, PolynomialRing), dpairInds, dpairVars},
     Headline => "pairs coordinate and derivation variables in a Weyl algebra",
     Usage => "createDpairs A",
     Inputs => {
	  "A" => "the Weyl algebra"
	  },
     Consequences => {
	  {"attaches to ", TT "A", " a pair of keys to help distinguish the
	  coordinate variables from the derivation variables."}
	  },
     "Since the Weyl algebra has commutation rules, this routine
     attaches to the Weyl algebra two keys to organize the
     variables.  The first key 'dpairVars' contains 3 lists: a list of the coordinate
     variables, a list of the derivative variables, and a list
     of the central variables.  The second key 'dpairInds' also contains 3 lists
     of the corresponding indices to 'dpairVars'.",
     EXAMPLE lines ///
	     W = QQ[x,y,Dx,Dy, WeylAlgebra => {x=>Dx,y=>Dy}]
	     createDpairs W
	     W.dpairVars
	     W.dpairInds
	     ///,
     SeeAlso => {[monoid, WeylAlgebra]}
     }

doc ///
  Key
    extractVarsAlgebra
    (extractVarsAlgebra, PolynomialRing)
  Headline
   underlying polynomial ring in the ordinary variables of a Weyl algebra
  Usage
    extractVarsAlgebra D
  Inputs
    D:PolynomialRing
       a Weyl algebra
  Outputs
    :PolynomialRing
  Description
    Text
      Extracts from a Weyl algebra the polynomial ring in its ordinary variables.
    Example
      D = makeWA(QQ[x,y])
      R = extractVarsAlgebra D
      describe R
  SeeAlso
    extractDiffsAlgebra
///

doc ///
  Key
    extractDiffsAlgebra
    (extractDiffsAlgebra, PolynomialRing)
  Headline
   underlying polynomial ring in the differentials of a Weyl algebra
  Usage
    extractDiffsAlgebra D
  Inputs
    D:PolynomialRing
      a Weyl algebra
  Outputs
    :PolynomialRing
  Description
    Text
      Extracts from a Weyl algebra the polynomial ring in its differentials only.
    Example
      D = makeWA(QQ[x,y])
      S = extractDiffsAlgebra D
      describe S
  SeeAlso
    extractVarsAlgebra
///
