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
     (symbol ^, Constant, InexactNumber),
     (symbol ^, Constant, Number),
     (symbol ^, InexactNumber, Constant),
     (symbol ^, Number, Constant),
     }

doc ///
  Key
    power
  Headline
    power
  Usage
    (x,n)
  Description
    Text
      @TT "power(x,n)"@ yields the @TT "n"@-th power of @TT "x"@.
  SeeAlso
    "^"
///

doc ///
  Key
    symbol ^
  Headline
    a binary operator, usually used for powers
  Usage
    x ^ y
  Description
    Text
      This operator is used for exponentiation, making free modules and
      sheaves, for shifting complexes left or right, for projection maps
      involving direct sums, and for making nets.
///

doc ///
  Key
    (symbol ^, Ring, List)
    (symbol ^, RingFamily, List)
  Headline
    make a free module
  Usage
    M = R^{i,j,k,...}
  Inputs
    R:
    :Nothing
      @TT "{i,j,k, ...}"@, @ofClass List@, of integers or lists of integers
  Outputs
    :Module
      , a free module over @TT "R"@ whose generators have degrees @TT "-i"@,
      @TT "-j"@, @TT "-k"@, ...
  Description
    Example
      R = QQ[a..d]
      R^{-1}
      R^{-1,2:-2,-3}
    Text
      If @TT "i"@, @TT "j"@, ... are lists of integers, then they represent
      multi-degrees, as in @TO "graded and multigraded polynomial rings"@.
    Example
      R = QQ[x,y,z,Degrees=>{{1,0},{1,-1},{1,-2}}]
      R^{{1,2}}
  SeeAlso
    "degrees"
    "free modules"
    "graded and multigraded polynomial rings"
///
doc ///
  Key
    (symbol ^,Module,ZZ)
  Headline
    direct sum
  Usage
    M^n
  Inputs
    M:
    n:
  Outputs
    :
      the direct sum of @TT "n"@ copies of @TT "M"@
  Description
    Example
      M = coker matrix{{1,2,3}}
      M^3
      directSum(3:M)
  SeeAlso
    directSum
    symbol++
///
doc ///
  Key
    (symbol ^, Ring, ZZ)
    (symbol ^, RingFamily, ZZ)
  Headline
    make a free module
  Usage
    R^n
  Inputs
    R:
    n:
  Outputs
    :
      a new free @TT "R"@-module of rank @TT "n"@.
  Description
    Text
      The new free module has basis elements of degree zero. To specify the
      degrees explicitly, see @TO (symbol ^, Ring, List)@.
    Example
      R = ZZ[x,y,z]/(x^2-y*x)
      F = R^4
      degrees F
  SeeAlso
    (degrees,Module)
    (symbol^,Ring,List)
    "graded and multigraded polynomial rings"
///

doc ///
  Key
    (symbol ^, RingElement, ZZ)
    (symbol ^, MonoidElement, ZZ)
  Headline
    power
  Usage
    f^n
  Inputs
    f:
    n:
  Outputs
    :RingElement
      @TT "f^n"@
  Description
    Example
      R = ZZ/7[x]/(x^46-x-1);
      (x+4)^(7^100)
    Text
      If the ring allows inverses, negative values may be used.
    Example
      S = ZZ[t,Inverses=>true,MonomialOrder=>RevLex];
      t^-1
      T = frac(ZZ[a,b,c]);
      (a+b+c)^-1
  SeeAlso
    frac
    "polynomial rings"
///

doc ///
  Key
    (symbol ^, RingMap, ZZ)
  Headline
    power
  Usage
    x^n
  Inputs
    x:
      @ofClass{RingMap}@
    n:ZZ
  Outputs
    :
      the @TT "n"@-th power of @TT "x"@
///
doc ///
  Key
    (symbol ^, Matrix, ZZ)
    (symbol ^, MutableMatrix, ZZ)
  Headline
    power
  Usage
    f^n
  Inputs
    f:
    n:
  Outputs
    :Matrix
      @TT "f^n"@
  Description
    Example
      R = ZZ/7[x]/(x^6-3*x-4)
      f = matrix{{x,x+1},{x-1,2*x}}
      f^2
      f^1000
    Text
      If the matrix is invertible, then f^-1 is the inverse.
    Example
      M = matrix(QQ,{{1,2,3},{1,5,9},{8,3,1}})
      det M
      M^-1
      M^-1 * M
      R = QQ[x]
      N = matrix{{x^3,x+1},{x^2-x+1,1}}
      det N
      N^-1
      N^-1 * N
  SeeAlso
    det
///


doc ///
  Key
    (symbol ^,Module,Array)
  Headline
    projection onto summand
  Usage
    M^[i,j,...,k]
  Inputs
    M:
    :Nothing
      @TT "[i,j,...,k]"@, an array of indices
  Outputs
    :Nothing
      @ofClass Matrix@
  Description
    Text
      The module @TT "M"@ should be a direct sum, and the result is the map
      obtained by projection onto the sum of the components numbered or named
      @TT "i, j, ..., k"@. Free modules are regarded as direct sums of modules.
    Example
      M = ZZ^2 ++ ZZ^3
      M^[0]
      M^[1]
      M^[1,0]
    Text
      If the components have been given names (see @TO directSum@), use those
      instead.
    Example
      R = QQ[a..d];
      M = (a => image vars R) ++ (b => coker vars R)
      M^[a]
      isWellDefined oo
      M^[b]
      isWellDefined oo
      isWellDefined(M^{2})
    Text
      This works the same way for chain complexes.
    Example
      C = res coker vars R
      D = (a=>C) ++ (b=>C)
      D^[a]
  SeeAlso
    directSum
    (symbol ^,Matrix,Array)
    (symbol _,Module,Array)
    (symbol ^,Module,List)
///

doc ///
  Key
    (symbol ^, Matrix, Array)
  Headline
    component of map corresponding to summand of target
  Usage
    F^[i,j,...,k]
  Inputs
    F:
    :Nothing
      @TT "[i,j,...,k]"@, an array of indices
  Outputs
    :Nothing
      @ofClass{Matrix}@
  Description
    Text
      The target of the matrix @TT "F"@ should be a direct sum, and the result
      is the component of this map corresponding to the sum of the components
      numbered or named @TT "i, j, ..., k"@. Free modules are regarded as
      direct sums of modules. In otherwords, this routine returns the map given
      by certain blocks of columns.
    Example
      R = ZZ[a..d];
      F = (vars R) ++ ((vars R) ++ matrix{{a-1,b-3},{c,d}})
      F^[1]
      F_[1]^[1]
    Text
      If the components have been given names (see @TO directSum@), use those
      instead.
    Example
      G = (a=>R^2) ++ (b=>R^1)
      N = map(G,R^2, (i,j) -> (i+37*j)_R)
      N^[a]
      N^[b]
      N = directSum(x1 => matrix{{a,b-1}}, x2 => matrix{{a-3,b-17,c-35}}, x3 => vars R)
      N^[x1,x3]
    Text
      This works the same way for maps between chain complexes.
  SeeAlso
    (symbol^,Matrix,Array)
    (symbol_,Module,Array)
    directSum
///

doc ///
  Key
    (symbol ^, Module, List)
  Headline
    projection onto summand
  Description
    Text
      @TT "M^{i,j,k,...}"@ -- provides the projection map from a free module
      @TT "M"@ to the free module corresponding to the basis vectors whose
      index numbers are listed.
    Example
      (ZZ^5)^{2,3}
  SeeAlso
    "_"
    Module
    List
///

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
