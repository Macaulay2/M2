--- old format (commented out) ---
-- document {
--      Key => {putWeylAlgebra, (putWeylAlgebra, HashTable)},
--      Headline => "transforms output of diffOps into elements of Weyl algebra",
--      Usage => "putWeylAlgebra m",
--      Inputs => {
--      	  "the output of diffOps"
-- 	  },
--      Outputs => {
-- 	  "the differential operators as elements of the Weyl algebra"
-- 	  },
--      "If I is an ideal of the polynomial ring R and m is the output of ",
--      TT "diffOps(I, k)", " then this routine returns elements of the Weyl
--      algebra ", TT "W", " corresponding to ", TT "R", " whose images in ", TT "W/IW",
--      " are an ", TT "R/I", "-generating set for the differential operators of order at most ",
--      TT "k", ".",
--      EXAMPLE lines ///
-- 	R = QQ[x,y,z]
--      	I = ideal(x^2-y*z)
--      	m = diffOps(I, 3)
--      	putWeylAlgebra m
-- 	///,
--      SeeAlso => {"diffOps"}
--      }

doc ///
  Key
    putWeylAlgebra
    (putWeylAlgebra, HashTable)
  Headline
    transforms output of diffOps into elements of Weyl algebra
  Usage
    putWeylAlgebra m
  Inputs
    m:HashTable
      the output of @TO "diffOps"@
  Outputs
    :List
      the differential operators as elements of the Weyl algebra
  Description
    Text
      If $I$ is an ideal of the polynomial ring $R$ and $m$ is the output of
      @TT "diffOps(I, k)"@ then this routine returns elements of the Weyl
      algebra $W$ corresponding to $R$ whose images in $W/IW$
      are an $R/I$-generating set for the differential operators of order at most $k$.
    Example
      R = QQ[x,y,z]
      I = ideal(x^2-y*z)
      m = diffOps(I, 3)
      putWeylAlgebra m
  SeeAlso
    diffOps
///

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
      contained in a polynomial ring $R$
    f:RingElement
      an element of a polynomial ring $R$
    k:ZZ
      which is nonnegative
  Outputs
    :HashTable
      the differential operators of order at most $k$
      of the quotient ring $R/I$ (or $R/(f)$)
  Description
    Text
      Given an ideal $I$ of a polynomial ring $R$ the set of
      differential operators of the quotient ring $R/I$ having order
      less than or equal to $k$ forms a finitely generated module over
      $R/I$. This routine returns its generating set.
      
    Text 
      The output is in the form of a hash table.
      The key @TT "BasisElts"@ is a row vector of basic differential operators.
      The key @TT "PolyGens"@ is a matrix over $R$ whose column vectors represent
      differential operators of $R/I$ in the following way.  For each column
      vector, consider its image in $R/I$ then take its dot product with
      the @TT "BasisElts"@. This gives a differential operator, and
      the set of these operators generates the differential operators of
      $R/I$ of order $k$ or less as an $(R/I)$-module.
    Example
      R = QQ[x,y,z]
      I = ideal(x^2-y*z)
      D = diffOps(I, 3)
      D.BasisElts
      D.PolyGens
  Subnodes
    putWeylAlgebra
///
