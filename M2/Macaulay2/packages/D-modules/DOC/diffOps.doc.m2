needs "D-modules.m2"

document { diffOps,
     TT "diffOps (I, k)", " -- 
     compute differential operators of order less than or equal to k 
     of the quotient ring R/I",
     BR, NOINDENT,
     TT "diffOps (f, k)", " -- 
     compute differential operators of order less than or equal to k 
     of the quotient ring R/(f)",

     PARA,
     "Given an ideal I of a polynomial ring R, the set of
     differential operators of the quotient ring R/I having order 
     less than or equal to k forms a finitely generated module over R/I.  
     This routine returns a generating set.",
     
     PARA,
     "The output is in the form of a hash table.
     The key 'BasisElts' is a row vector of basic differential operators.
     The key 'PolyGens' is a matrix over R whose column vectors represent 
     differential operators of R/I in the following way.  For each column
     vector, consider its image in R/I, then take its dot product with
     the 'BasisElts' row vector.  This gives a differential operator, and
     the set of these operators generates the differential operators of
     R/I of order k or less as an (R/I)-module.",

     PARA,
     "A simple example:",
     EXAMPLE ///needs "D-modules.m2"///,
     EXAMPLE "R = QQ[x,y,z]",
     EXAMPLE "I = ideal(x^2-y*z)", 
     EXAMPLE "diffOps(I, 3)",

     PARA,
     SEEALSO {"putWeylAlgebra"}

     },

document { putWeylAlgebra,
     TT "putWeylAlgebra m", " -- 
     given the output m of diffOps, represents
     the differential operators as elements of a Weyl algebra.",

     PARA,
     "If I is an ideal of the polynomial ring R and m is the output of 
     diffOps(I, k), then this routine returns elements of the Weyl
     algebra W corresponding to R whose images in W/IW are an
     R/I-generating set for the order k or less differential operators.",

     PARA,
     "A simple example:",
     EXAMPLE ///needs "D-modules.m2"///,
     EXAMPLE "R = QQ[x,y,z]",
     EXAMPLE "I = ideal(x^2-y*z)", 
     EXAMPLE "m = diffOps(I, 3)",
     EXAMPLE "putWeylAlgebra m",

     PARA,
     SEEALSO {"diffOps"}

     }
