-- Copyright 1998 by Michael E. Stillman

-- Engine documentation

document { quote ggadd,
     TT "ggadd(f:ERingElement,g:ERingElement):ERingElement", "-- yields the ring element sum f+g.",BR,NOINDENT,
     TT "ggadd(f:EVector,g:EVector):EVector", "               -- yields the vector sum f+g.",BR,NOINDENT,
     TT "ggadd(f:EMatrix,g:EMatrix):EMatrix", "               -- yields the matrix sum f+g.",BR,NOINDENT,
     TT "ggadd(F:EFreeModule,G:EFreeModule):EFreeModule", "   -- yields the direct sum F ++ G.",
     PARA,
     "In each case, the base ring of the two arguments must be the same.  
      For vector addition, the two vectors must be in free modules with 
      the same rank.  The result is a vector in the free module of ", TT "f", ".
      In the matrix version, both arguments must have the same number of rows. 
      The resulting matrix has the same target as ", TT "f", ". For free 
      modules, if either ", TO "F", " or ", TO "G", "has an induced monomial 
      order, then the result will as well.",
     PARA,
      "Known problems, restrictions or caveats: The direct sum of free modules 
       should have a different name, e.g. ggdirectsum.",
     SEEALSO "EVector", 
     SEEALSO "EMatrix",
     SEEALSO "EFreeModule"
     }

document { quote ggsubtract,
TT "ggsubtract(f:ERingElement,g:ERingElement):ERingElement", "-- yields the difference f-g.",BR,NOINDENT,
TT "ggsubtract(f:EVector,g:EVector):EVector", "               -- yields the vector difference f-g.",BR,NOINDENT,
TT "ggsubtract(f:EMatrix,g:EMatrix):EMatrix", "               -- yields the matrix difference f-g.",
     PARA,
     "In each case, the base ring of the two arguments must be the same.  
      For vector subtraction, the two vectors must be in free modules with 
      the same rank.  The result is a vector in the free module of ", TT "f", ".
      In the matrix version, both arguments must have the same number of rows. 
      The resulting matrix has the same target as ", TT "f", ". ",
     PARA,
      "Known problems, restrictions or caveats: None.",
     SEEALSO "EVector", 
     SEEALSO "EMatrix",
     SEEALSO "EFreeModule"
     }

document { quote ggiszero,
     TT "ggiszero(f:ERingElement):Boolean", " -- is ", TO "f", " zero?", BR,NOINDENT,
     TT "ggiszero(f:EVector):Boolean", "      -- is ", TO "f", " zero?", BR,NOINDENT,
     TT "ggiszero(f:EMatrix):Boolean", "      -- is ", TO "f", " zero?",
     PARA,
     "In each case, true is returned exactly when ", TO "f", " is zero.",
     PARA,
      "Known problems, restrictions or caveats: None.",
     }

document { quote ggisequal,
     TT "ggisequal(f:ERingElement,g:ERingElement):Boolean", " -- is ", TT "f", " = ", TT "g", "?", BR,NOINDENT,
     TT "ggisequal(f:EVector,g:EVector):Boolean", "           -- is ", TT "f", " = ", TT "g", "?", BR,NOINDENT,
     TT "ggisequal(f:EFreeModule,g:EFreeModule):Boolean", "   -- is ", TT "f", " = ", TT "g", "?", BR,NOINDENT,
     TT "ggisequal(f:EMatrix,g:EVector):Boolean", "           -- is ", TT "f", " = ", TT "g", "?",
     PARA,
     "In each case, true is returned exactly when ", TT "f", " and ", TT "g", " are equal.
      Equality is a very strong notion: for vectors, it means that their respective free 
      modules are equal.  Two matrices are equal only if all entries are the same, 
      and the target and source of each is the same.  For free modules, both arguments 
      must have the same rank, and the degree of each basis element must be the same, 
      and also the monomial ordering must be the same (see ", TO "???", ").",
     PARA,
      "Known problems, restrictions or caveats: Equality checks using ", TO "ggiszero", "
       are often less strict.  For example, if ", TT "m", " and ", TT "n", " are two
       matrices, then it is possible for ", TT "ggiszero(ggsubtract(m,n))", " to yield true,
       but to have ", TT "ggisequal(m,n)", " return false."
     }

document { quote EMonomialOrder,
     TT "EMonomialOrder", " -- An engine type describing a monomial order",
     PARA,
     "This is a hard one to document: Include how to create a monomial order"
     }

document { quote ggEZZ,
     TT "ggEZZ():ERing", " -- yields the ring ZZ of arbitrary precision integers",
     PARA,
      "Known problems, restrictions or caveats: For a VERY SHORT TIME, 
       this ring cannot yet handle arbitrary precision integers."
     }

document { quote ggEcharp,
     TT "ggEZZ(p:int):ERing", " -- yields the quotient ring ZZ/p",
     PARA,
      "Known problems, restrictions or caveats: Currently, the integer ", TT "p", "
       must be a small (< 2^16-1) positive integer.  This restriction 
       will be removed soon."
     }
     
