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
      if @TT "true"@ (default), brings the Weyl algebra generators into
      scope as global variables; set to @TT "false"@ to avoid global name
      assignment.
  Outputs
    :PolynomialRing
      the (non-commutative) Weyl algebra
  Description
    Text
      Given a polynomial ring $R$ with variables $x_1,\ldots,x_n$,
      this routine returns a Weyl algebra with variables $x_1,\ldots,x_n$
      and $dx_1,\ldots,dx_n$.
    Example
      R = QQ[x,y,z]
      D = makeWeylAlgebra R
    Text
      To skip naming the ring, use parentheses.
    Example
      makeWA(QQ[x,y,z])
  Caveat
    The polynomial ring $R$ must be commutative.
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

--- old format (commented out) ---
-- document {
--      Key => {createDpairs, (createDpairs, PolynomialRing), dpairInds, dpairVars},
--      Headline => "pairs coordinate and derivation variables in a Weyl algebra",
--      Usage => "createDpairs A",
--      Inputs => {
-- 	  "A" => "the Weyl algebra"
-- 	  },
--      Consequences => {
-- 	  {"attaches to ", TT "A", " a pair of keys to help distinguish the
-- 	  coordinate variables from the derivation variables."}
-- 	  },
--      "Since the Weyl algebra has commutation rules, this routine
--      attaches to the Weyl algebra two keys to organize the
--      variables.  The first key 'dpairVars' contains 3 lists: a list of the coordinate
--      variables, a list of the derivative variables, and a list
--      of the central variables.  The second key 'dpairInds' also contains 3 lists
--      of the corresponding indices to 'dpairVars'.",
--      EXAMPLE lines ///
-- 	     W = QQ[x,y,Dx,Dy, WeylAlgebra => {x=>Dx,y=>Dy}]
-- 	     createDpairs W
-- 	     W.dpairVars
-- 	     W.dpairInds
-- 	     ///,
--      SeeAlso => {[monoid, WeylAlgebra]}
--      }

doc ///
  Key
    createDpairs
    (createDpairs, PolynomialRing)
    dpairInds
    dpairVars
  Headline
    pairs coordinate and derivation variables in a Weyl algebra
  Usage
    createDpairs A
  Inputs
    A:PolynomialRing
      the Weyl algebra
  Consequences
    Item
      attaches to @TT "A"@ a pair of keys to help distinguish the
      coordinate variables from the derivation variables
  Description
    Text
      Since the Weyl algebra has commutation rules, this routine attaches
      to the Weyl algebra two keys to organize the variables. The first key
      @TT "dpairVars"@ contains 3 lists: the coordinate variables, the
      derivative variables, and the central variables (i.e. variables that
      commute with everything in the Weyl algebra). The second key
      @TT "dpairInds"@ contains the corresponding lists of indices into
      the ring's variable list.
    Example
      W = QQ[x,y,Dx,Dy, WeylAlgebra => {x=>Dx,y=>Dy}]
      createDpairs W
      W.dpairVars
      W.dpairInds
  SeeAlso
    [monoid, WeylAlgebra]
///

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
      the polynomial ring in $x_1,\dots,x_n$ (the coordinate variables of $D$)
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
      the polynomial ring in $dx_1,\dots,dx_n$ (the differentials of $D$)
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
