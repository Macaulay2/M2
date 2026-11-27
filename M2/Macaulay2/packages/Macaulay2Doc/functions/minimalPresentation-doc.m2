--- status: Draft
--- author(s): Amelia Taylor
--- notes: This is a new file bringing together both prune and minPres as a 
---        complete minimal presentation collection.  trim is inherently 
---        different as it minimizes to the extent possible while preserving
---        the ambient module.

doc ///
  Key
    minimalPresentation
  Headline
    compute a minimal presentation
  SeeAlso
    trim
///

doc ///
  Key
    prune
  Headline
    prune, e.g., compute a minimal presentation
  SeeAlso
    minimalPresentation
  Subnodes
    "pruningMap"
///

doc ///
  Key
    (minimalPresentation,Ring)
    (prune,Ring)
    minimalPresentationMap
    minimalPresentationMapInv
    [minimalPresentation,Exclude]
    [prune,Exclude]
  Headline
    compute a minimal presentation of a quotient ring
  Usage
    S = minimalPresentation R
    S = prune R
  Inputs
    R:
      a quotient ring
  Outputs
    S:
      a quotient ring, minimally presented if @TT "R"@ is homogeneous,
      isomorphic to @TT "R"@
  Consequences
    Item
      the isomorphism from @TT "R"@ to @TT "S"@ is stored as
      @TT "R.minimalPresentationMap"@ and the inverse of this map is stored as
      @TT "R.minimalPresentationMapInv"@
  Description
    Text
      The computation is accomplished by considering the relations of @TT "R"@.
      If a variable occurs as a term of a relation of @TT "R"@ and in no other
      terms of the same polynomial, then the variable is replaced by the
      remaining terms and removed from the ring. A minimal generating set for
      the resulting defining ideal is then computed and the new quotient ring
      is returned. If @TT "R"@ is not homogeneous, then an attempt is made to
      improve the presentation.
    Example
      R = ZZ/101[x,y,z,u,w]/ideal(x-x^2-y,z+x*y,w^2-u^2);
      minimalPresentation(R)
      R.minimalPresentationMap
      R.minimalPresentationMapInv
    Text
      If the Exclude option is present, then those variables with the given
      indices are not simplified away (remember that ring variable indices
      start at 0).
    Example
      R = ZZ/101[x,y,z,u,w]/ideal(x-x^2-y,z+x*y,w^2-u^2);
      minimalPresentation(R, Exclude=>{1})
  SeeAlso
    (minimalPresentation, Ideal)
    (prune, Ideal)
    (trim, Ring)
    (trim, QuotientRing)
///
doc ///
  Key
    (minimalPresentation,Ideal)
    (prune,Ideal)
  Headline
    compute a minimal presentation of the quotient ring defined by an ideal
  Usage
    J = minimalPresentation I
  Inputs
    I:
      in a ring @TT "S"@
  Outputs
    J:Ideal
      in a new ring @TT "Q"@ with @TT "S/I"@ isomorphic to @TT "Q/J"@
  Consequences
    Item
      @TT "minimalPresentation"@ stores the isomorphism from @TT "S/I"@ to
      @TT "Q/J"@ as @TT "I.cache.minimalPresentationMap"@ , where @TT "I"@ is
      the input ideal and the inverse of this map as
      @TT "I.cache.minimalPresentationMapInv"@
  Description
    Text
      If the ideal @TT "I"@ is homogeneous, then the ideal @TT "J"@, in a new
      ring @TT "Q"@ is the defining ideal for a minimal presentation of the
      ring @TT "S/I"@ where @TT "S"@ is the ring of @TT "I"@. This is
      accomplished as follows. If a variable occurs as a term of a generator of
      @TT "I"@ and in no other terms of the same polynomial, then the variable
      is replaced by the remaining terms and removed from the ring. A minimal
      generating set for the resulting ideal is then computed. If @TT "I"@ is
      not homogeneous, then an attempt is made to improve the presentation of
      @TT "S/I"@.
    Example
      C = ZZ/101[x,y,z,u,w];
      I = ideal(x-x^2-y,z+x*y,w^2-u^2);
      minPres I
      I.cache.minimalPresentationMap
      I.cache.minimalPresentationMapInv
    Text
      If the Exclude option is present, then those variables with the given
      indices are not simplified away (remember that ring variable indices
      start at 0).
    Example
      R = ZZ/101[x,y,z,u,w];
      I = ideal(x-x^2-y,z+x*y,w^2-u^2);
      minimalPresentation(I, Exclude=>{1})
  SeeAlso
    (minimalPresentation,Ring)
    (trim, Ideal)
///

doc ///
  Key
    (minimalPresentation,Matrix)
    (prune,Matrix)
  Headline
    minimally present source and target of a matrix
  Usage
    minimalPresentation f
  Inputs
    f:
  Outputs
    g:Matrix
  Description
    Text
      If the source and target of @TT "f"@ are graded, then minimal
      presentations of the source and target modules for @TT "f"@ are computed
      using @TO (minimalPresentation, Module)@ and @TT "g"@ is the matrix
      corresponding to @TT "f"@ with source and target the minimally presented
      source and target. If either the source or target of @TT "f"@ is not
      graded then an attempt is made to improve their presentations and
      @TT "g"@is the matrix with resulting source and target. An example
      follows.
    Example
      R = ZZ/32003[a..d];
      f = inducedMap(coker matrix {{a,1,b},{c,3,b+d}},R^2)
      g = prune f
      source g
      target g
    Text
      This function does not remove elements from the base field from the
      matrix, but rather minimally presents the source and target and gives the
      corresponding new map. For example:
    Example
      m = matrix{{a,1,b},{c,3,b+d}}
      prune m
    Text
      Unlike above, nothing changes.
  Caveat
    Check out the example just above.
  SeeAlso
    (minimalPresentation, Module)
///

doc ///
  Key
    pruningMap
  Description
    Text
      This symbol is used as a key for storing results in modules, by
      @TO "minimalPresentation"@.
///
doc ///
  Key
    (minimalPresentation,Module)
    (prune,Module)
  Headline
    minimal presentation of a module
  Usage
    N = minimalPresentation M
  Inputs
    M:
  Outputs
    N:Module
      isomorphic to @TT "M"@
  Consequences
    Item
      @TT "The isomorphism from "@@TT "N"@ to @TT "M"@ as
      @TT "g = N.cache.pruningMap"@ unless @TT "M.cache.pruningMap"@ already
      exists, in which case @TT "N"@ is the same as @TT "M"@. The inverse
      isomorphism can be obtained as @TT "g^-1"@
  Description
    Text
      If the Module @TT "M"@ is graded then the module @TT "N"@ is a minimal
      presentation of @TT "M"@. If not, then an attempt is made to improve the
      presentation of @TT "M"@. An example follows.
    Example
      R = ZZ/32003[a..d];
      M = coker matrix {{a,1,b},{c,3,b+d}}
      N = minimalPresentation M
      peek N.cache
      g = N.cache.pruningMap
      g^-1
  SeeAlso
    (minimalPresentation, Matrix)
    (trim, Module)
    (mingens,Module)
///

-- document { 
--      Key => {[minimalPresentation, Variable],[prune, Variable]},
--      Headline => "specify the variable for the new isomorphic ring",
--      Usage => "minimalPresentation(..., Variable => u)",
--      Inputs => {
-- 	  "u" => Symbol
-- 	  },
--      Consequences => {
-- 	  {"variables in the resulting ring are ", TT "u_0, u_1, ..."}
-- 	  },     
--      EXAMPLE {
-- 	  "C = ZZ/101[x,y,z,u,w]/ideal(x-x^2-y,z+x*y,w^2-u^2);",
-- 	  "minimalPresentation(C,Variable => a)",
-- 	  },
--      Caveat => {"If the symbol, ", TT "u", " is used as a variable in 
-- 	  the original ring an error is returned."}
--      }
--  
--  ///
--  I = ideal(x*y, u^2-y^2, x+y+z, v^2-u^2-x^2) 
--  -- trim returns x+y+z, y^2+yz, u^2+yz, v^2-z^2
--  -- minpres returns y^2+yz, u^2+yz, v^2-z^2
--  -- prune does nothing - as a module this is 
--  -- minimal.
--  
--  R = ZZ/101[a,b,w,x,y,z]
--  I = ideal(a*w, a*x, a*y, a*z, b*w, b*x, b*y, b*z)
--  M = koszul(2,gens I)
--  M = matrix{{a*w, a*x, 3, x+w},{a*z, a*w, 2, z+w}}
--  C = chainComplex M
--  C.dd
--  prune oo
--  prune coker M
--  
--  ///
--  
--  
