document {
     Key => {putWeylAlgebra, (putWeylAlgebra, HashTable)},
     Headline => "transforms output of diffOps into elements of Weyl algebra",
     Usage => "putWeylAlgebra m",
     Inputs => {
     	  "the output of diffOps"
	  },
     Outputs => {
	  "the differential operators as elements of the Weyl algebra"
	  },
     "If I is an ideal of the polynomial ring R and m is the output of ", 
     TT "diffOps(I, k)", " then this routine returns elements of the Weyl
     algebra ", TT "W", " corresponding to ", TT "R", " whose images in ", TT "W/IW", 
     " are an ", TT "R/I", "-generating set for the differential operators of order at most ", 
     TT "k", ".",
     EXAMPLE lines ///
	R = QQ[x,y,z]
     	I = ideal(x^2-y*z) 
     	m = diffOps(I, 3)
     	putWeylAlgebra m
	///,
     SeeAlso => {"diffOps"}
     }

doc ///
  Key
    diffOps
    (diffOps, RingElement, ZZ)
    (diffOps, Ideal, ZZ)
    PolyGens
    BasisElts
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
      D = diffOps(I, 3)
      D.BasisElts
      D.PolyGens
  Subnodes
    putWeylAlgebra
///
