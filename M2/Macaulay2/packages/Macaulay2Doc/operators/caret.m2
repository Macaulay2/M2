-- -*- coding: utf-8 -*-
--		Copyright 1993-2002 by Daniel R. Grayson

undocumented {
     (symbol ^, InfiniteNumber, ZZ),
     (symbol ^, InfiniteNumber, QQ),
     (symbol ^, InfiniteNumber, RR),
     (symbol ^, InfiniteNumber, InfiniteNumber),
     (symbol ^, ZZ, InfiniteNumber),
     (symbol ^, QQ, InfiniteNumber),
     (symbol ^, RR, InfiniteNumber),
     (symbol ^, Constant, Constant),
     (symbol ^, Constant, RingFamily),
     (symbol ^, Constant, InexactNumber),
     (symbol ^, Constant, Number),
     (symbol ^, InexactNumber, Constant),
     (symbol ^, Number, Constant),
     }

document {
    Key => power,
    Headline => "power",
    Usage => "(x,n)",
    TT "power(x,n)", " yields the ", TT "n", "-th power of ", TT "x", ".",
    PARA{},
    SeeAlso => "^"
}

document {
     Key => {symbol ^},
     Headline => "a binary operator, usually used for powers",
     Usage => "x ^ y",
     PARA{},
     "This operator is used for exponentiation, making free modules and sheaves, 
     for shifting complexes left or right, for projection maps involving direct sums, 
     and for making nets.",
     }

document {
     Key => {
	 (symbol ^, Ring,       List),
	 (symbol ^, RingFamily, List)
	 },
     Headline => "make a free module",
     Usage => "M = R^{i,j,k,...}",
     Inputs => {"R", 
	  Nothing => {TT "{i,j,k, ...}", ", ", ofClass List, ", of integers or lists of integers"}},
     Outputs => {
          Module => {
	       {", a free module over ", TT "R", " whose generators have degrees ", TT "-i", ", ", TT "-j", ", ", TT "-k", ", ..."}}},
     EXAMPLE lines ///
     	  R = QQ[a..d]
	  R^{-1}
	  R^{-1,2:-2,-3}
     	  ///,
     PARA{},
     "If ", TT "i", ", ", TT "j", ", ... are lists of integers, then
     they represent multi-degrees, as in ", TO "graded and multigraded polynomial rings", ".",
     EXAMPLE lines ///
     	  R = QQ[x,y,z,Degrees=>{{1,0},{1,-1},{1,-2}}]
	  R^{{1,2}}
     	  ///,
     SeeAlso => {"degrees", "free modules", "graded and multigraded polynomial rings"}}
document {
     Key => (symbol ^,Module,ZZ),
     Headline => "direct sum",
     Usage => "M^n",
     Inputs => {"M", "n"},
     Outputs => {{"the direct sum of ", TT "n", " copies of ", TT "M"}},
     EXAMPLE lines ///
     	  M = coker matrix{{1,2,3}}
	  M^3
	  directSum(3:M)
     ///,
     SeeAlso => {directSum, symbol++}
     }
document {
     Key => {
	 (symbol ^, Ring,       ZZ),
	 (symbol ^, RingFamily, ZZ)
	 },
     Headline => "make a free module",
     Usage => "R^n",
     Inputs => {"R", "n"},
     Outputs => {{"a new free ", TT "R", "-module of rank ", TT "n", "." }},
     "The new free module has basis elements of degree zero.  To specify the
     degrees explicitly, see ", TO (symbol ^,Ring,List), ".",
     EXAMPLE lines ///
     	  R = ZZ[x,y,z]/(x^2-y*x)
	  F = R^4
	  degrees F
     	  ///,
     SeeAlso => {(degrees,Module), (symbol^,Ring,List), "graded and multigraded polynomial rings"}
     }

document {
     Key => {(symbol ^, RingElement, ZZ), (symbol ^, MonoidElement, ZZ)},
     Headline => "power",
     Usage => "f^n",
     Inputs => {"f", "n"},
     Outputs => {
     	  RingElement => TT "f^n"
	  },
     EXAMPLE lines ///
     	  R = ZZ/7[x]/(x^46-x-1);
	  (x+4)^(7^100)
     	  ///,
     PARA{},
     "If the ring allows inverses, negative values may be used.",
     EXAMPLE lines ///
     	  S = ZZ[t,Inverses=>true,MonomialOrder=>RevLex];
	  t^-1
	  T = frac(ZZ[a,b,c]);
	  (a+b+c)^-1
     	  ///,
     SeeAlso => {frac, "polynomial rings"}
     }

document {
     Key => (symbol ^, RingMap, ZZ),
     Headline => "power",
     Usage => "x^n",
     Inputs => { 
	  "x" => { ofClass{RingMap} },
	  "n" => ZZ
	  },
     Outputs => { {"the ", TT "n", "-th power of ", TT "x"} }
     }
document {
     Key => {(symbol ^, Matrix, ZZ), (symbol ^, MutableMatrix, ZZ)},
     Headline => "power",
     Usage => "f^n",
     Inputs => {"f", "n"},
     Outputs => {
     	  Matrix => TT "f^n"
	  },
     EXAMPLE lines ///
     	  R = ZZ/7[x]/(x^6-3*x-4)
	  f = matrix{{x,x+1},{x-1,2*x}}
	  f^2
	  f^1000
     	  ///,
     PARA{},
     "If the matrix is invertible, then f^-1 is the inverse.",
     EXAMPLE lines ///
     	  M = matrix(QQ,{{1,2,3},{1,5,9},{8,3,1}})
	  det M
	  M^-1
	  M^-1 * M
	  R = QQ[x]
	  N = matrix{{x^3,x+1},{x^2-x+1,1}}
	  det N
	  N^-1
	  N^-1 * N
     	  ///,
     SeeAlso => {det}
     }


document {
     Key => {(symbol ^,Module,Array)},
     Headline => "projection onto summand",
     Usage => "M^[i,j,...,k]",
     Inputs => {"M",
	  Nothing => {TT "[i,j,...,k]", ", an array of indices"}},
     Outputs => {
     	  Nothing => {ofClass Matrix}
	  },
     PARA{},
     "The module ", TT "M", " should be a direct sum, and the result is the map
     obtained by projection onto the sum of the components numbered or named
     ", TT "i, j, ..., k", ".  Free modules are regarded as direct sums of modules.",
     PARA{},
     EXAMPLE lines ///
	  M = ZZ^2 ++ ZZ^3
      	  M^[0]
      	  M^[1]
      	  M^[1,0]
	  ///,
     PARA{},
     "If the components have been given names (see ", TO directSum, "), use those instead.",
     EXAMPLE lines ///
	  R = QQ[a..d];
	  M = (a => image vars R) ++ (b => coker vars R)
	  M^[a]
	  isWellDefined oo
	  M^[b]
	  isWellDefined oo
	  isWellDefined(M^{2})
	  ///,
     PARA{},
     "This works the same way for chain complexes.",
     EXAMPLE lines ///
	  C = res coker vars R
	  D = (a=>C) ++ (b=>C)
	  D^[a]
	  ///,
     SeeAlso => {directSum, (symbol ^,Matrix,Array), (symbol _,Module,Array),(symbol ^,Module,List)}
     }

document { 
     Key => {
	  (symbol ^, Matrix, Array),
	  },
     Headline => "component of map corresponding to summand of target",
     Usage => "F^[i,j,...,k]",
     Inputs => {"F",
	  Nothing => {TT "[i,j,...,k]", ", an array of indices"}},
     Outputs => {
     	  Nothing => ofClass{Matrix}
	  },
     "The target of the matrix ", TT "F", " should be a 
     direct sum, and the result is the component of this map 
     corresponding to the sum of the components numbered or named
     ", TT "i, j, ..., k", ".  Free modules are regarded as direct sums of modules.
     In otherwords, this routine returns the map given by certain blocks of columns.",
     EXAMPLE lines ///
          R = ZZ[a..d];
          F = (vars R) ++ ((vars R) ++ matrix{{a-1,b-3},{c,d}})
	  F^[1]
	  F_[1]^[1]
          ///,
     PARA{"If the components have been given names (see ", TO directSum, "), use those instead."},
     EXAMPLE lines ///
          G = (a=>R^2) ++ (b=>R^1)
	  N = map(G,R^2, (i,j) -> (i+37*j)_R)
	  N^[a]
	  N^[b]
     	  N = directSum(x1 => matrix{{a,b-1}}, x2 => matrix{{a-3,b-17,c-35}}, x3 => vars R)
	  N^[x1,x3]
	  ///,
     PARA {"This works the same way for maps between chain complexes."},
     SeeAlso => {(symbol^,Matrix,Array),(symbol_,Module,Array),directSum}
     }

document {
     Key => (symbol ^, Module, List),
     Headline => "projection onto summand",
     TT "M^{i,j,k,...}", " -- provides the projection map from a free module
     ", TT "M", " to the free module corresponding to the basis vectors whose
     index numbers are listed.",
     PARA{},
     EXAMPLE "(ZZ^5)^{2,3}",
     SeeAlso => {"_", Module, List}
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
