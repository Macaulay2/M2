undocumented {
    (random, GaloisField),
    (random, QuotientRing),
    (random, RingFamily),
    }

doc ///
Node
  Key
    random
  Headline
    get a random object
  Description
    Text
      This function can be used to get randomized objects of various sorts.
  Subnodes
    (random, ZZ, ZZ)
    (random, QQ)
    (random, Type)
    (random, List)
    (random, ZZ, Ideal)
    (random, ZZ, Ring)
    (random, Module)
    (random, Module, Module)
    setRandomSeed
    randomSubset

Node
  Key
    (random, ZZ, ZZ)
    (random, ZZ)
    (random, RR, RR)
    (random, RR)
  Headline
    get a random integer or real number
  Synopsis
    Usage
      random(low, high)
      random high
    Inputs
      low:{ZZ,RR}
      high:{ZZ,RR}
    Outputs
      :ZZ
        a random integer or real number of the same @TO precision@
  Description
    Text
      If only @TT "high"@ is given, the output will be in the range @TT "[0, high)"@.
    Example
      random 57
      random 10^50
      tally apply(100, i -> random 7)
      random 3.14
      random 3p200
    Text
      If, in addition, @TT "low"@ is given, the output will be in the range @TT "[low, high]"@
    Example
      for i to 10 list random(100,200)
      tally apply(100, i -> random(10,15))
      random(10.,20.)
      random(10p100,20p100)
  SeeAlso
    setRandomSeed

Node
  Key
    (random, QQ)
  Headline
    get a random rational number
  Usage
    random x
  Inputs
    x:QQ
    Height => ZZ
  Outputs
    :QQ -- randomly chosen from the interval $[0, x]$
  Description
    Text
      A random number is chosen from the uniform distribution on the interval
      $[0, x]$ and then rounded (using the @wikipedia "Farey sequence"@) to the
      nearest rational number with denominator bounded by the @CODE "Height"@
      option.
    Example
      apply(10, i -> random(7_QQ, Height => 5))
  SeeAlso
    setRandomSeed

Node
  Key
    (random, Type)
  Headline
    get a random object of a type
  Usage
    random T
  Inputs
    T:Type
    Height => ZZ
  Outputs
    :Thing
      a random instance of the type @TT "T"@
  Description
    Text
      If the @TT "Height"@ option specifies a number @TT "h"@ and @TT "T"@
      is @TO "ZZ"@ then the integers returned are in the range @TT "[0, h)"@;
      for @TO "QQ"@ the numerator and denominator are in the range @TT "[1, h]"@.
    Example
      random RR
      random CC_100
      kk = GF 11
      tally for i to 100 list random kk
      random GF(2,40)
  SeeAlso
    setRandomSeed

Node
  Key
    (random, ZZ,   Ring)
    (random, List, Ring)
    [random, Height]
  Headline
    get a random homogeneous element from a graded ring
  Usage
    random(d, R)
  Inputs
    d:{ZZ,List} -- the degree or multi-degree to use
    R:Ring
  Outputs
    :RingElement
      a random homogeneous element of the ring @TT "R"@ of degree @TT "d"@
  Description
    Example
      R = ZZ[x,y]
      random(5, R)
      R = GF(25, Variable => a)[x, y];
      VerticalList for i to 6 list random(3, R)
    Text
      The length of @TT "d"@, if it's a list, should be the same as @TO2 (degreeLength, "degree rank")@ of $R$.
  SeeAlso
    setRandomSeed

Node
  Key
    (random, ZZ,   Ideal)
    (random, List, Ideal)
  Headline
    get a random homogeneous element from a graded ideal
  Usage
    random(d, I)
    random(L, I)
  Inputs
    d:{ZZ,List} -- the degree, multi-degree, or a list of degrees
    I:Ideal -- in a graded ring
  Outputs
    :{RingElement,List} -- homogeneous element(s) with prescribed degrees
  Description
    Text
      This function produces one or more homogeneous elements of an ideal.
    Example
      S = ZZ/101[x, y]
      I = ideal"x2, y2"
      random(2, I)
      random({2}, I)
      random({2, 3}, I)
      random({{2}, {3}}, I)
      R = ZZ/101[x, y, z, Degrees => {{1,0}, {-1,1}, {0,1}}]
      J = ideal"x2, y2, z2"
      random({2, 2}, J)
      random({{2, 2}}, J)
      random(toList(3:{1, 1}), J)
  Caveat
    Note that in the single graded case, the input @TT "{d}"@ is treated as a multidegree
    of length one rather than a list of length one, hence the output is one polynomial,
    while @TT "{{d}}"@ is treated as a list containing a single multidegree of length one
    and therefore the output is a list containing a single polynomial.
  SeeAlso
    (random, ZZ,   Ring)
    (random, List, Ring)

Node
  Key
    (random, List)
  Headline
    shuffle a list randomly
  Usage
    random L
  Inputs
    L:List
  Outputs
    :List
      a new list containing the elements of @TT "L"@ in a shuffled random order
  Description
    Example
      random toList (0 .. 12)
  SeeAlso
    setRandomSeed

Node
  Key
    (random, Module, Module)
    [random, Density]
    [random, MaximalRank]
    [random, UpperTriangular]
  Headline
    get a random map of module
  Usage
    f = random(F, G)
  Inputs
    F:Module
    G:Module
    Density=>RR
      the proportion of entries to set
    MaximalRank=>Boolean
      whether to ensure that the resulting map has maximal rank: designed mostly
      for use with matrices of numbers: for polynomial rings, returns inhomogeneous results
    UpperTriangular=>Boolean
      whether to set just entries strictly above the diagonal
  Outputs
    f:Matrix
      a random, graded, degree @TT "0"@ map $f: G \to F$
  Description
    Example
      R = ZZ/101[x,y];
      random(R^{1,2,3}, R^{1,2,3})
      random(ZZ^3, ZZ^10, Density => .3)
      random(ZZ^3, ZZ^6, MaximalRank => true)
      random(ZZ^6, ZZ^6, UpperTriangular => true)
  Caveat
    Over a polynomial ring, specifying @TT "MaximalRank=>true"@ will yield a non-homogeneous matrix.
  SeeAlso
    (random, Module)
    setRandomSeed

Node
  Key
    (random, List, Module)
    (random, ZZ,   Module)
    (random,       Module)
  Headline
    get a random vector in the module
  Usage
    v = random(d, M)
    v = random M
  Inputs
    d:{ZZ,List} -- a degree or degree vector, assumed to be zero if omitted
    M:Module
  Outputs
    v:Vector
  Description
    Example
      R = ZZ/7[x,y]
      E = End R^{-1,1}
      h = homomorphism random E
      h = homomorphism random(0, E)
      h = homomorphism random(1, E)
      h = homomorphism random(-1, E)
      h = homomorphism random(-2, E)
  SeeAlso
    (Hom, Module, Module)
    (random, Module, Module)
    setRandomSeed
///

document { Key => {(randomMutableMatrix, ZZ, ZZ, RR, ZZ),
	  [randomMutableMatrix,Dense],
	  randomMutableMatrix},
     Headline => "a random mutable matrix of integers",
     Usage => "randomMutableMatrix(nrows,ncols,zerof,max)",
     Inputs => {
	  "nrows",
	  "ncols",
	  "zerof" => { "between 0 and 1" },
	  "max",
	  Dense => {"whether the encoding of the matrix should be dense or not: see ", TO MutableMatrix}
	  },
     Outputs => {
	  {"a random mutable ", TT "nrows", " by ", TT "ncols", " matrix of integers.
	       The absolute value of the
	       entries is less than ", TT "max", ", and
	       the frequency of entries equal to zero is given by ", TT "zerof", "." }
	  },
     "This function has been superseded by ", TO fillMatrix, ", which works over
     more rings, is much faster for large matrices, and is more flexible.",
     EXAMPLE lines ///
       randomMutableMatrix(10,15,.9,100)
     ///,
     SeeAlso => {mutableMatrix, fillMatrix, setRandomSeed, random}
     }

document {
     Headline => "fill a mutable matrix with random numbers",
     Key => {fillMatrix,(fillMatrix, MutableMatrix),(fillMatrix, MutableMatrix, ZZ),
	  [fillMatrix, Height],[fillMatrix,Density],[fillMatrix,UpperTriangular]},
     Usage => "fillMatrix M\nfillMatrix(M,n)",
     BaseFunction => fillMatrix,
     Inputs => {
	  "M"=>MutableMatrix,
	  "n" => ZZ => {"if specified, the maximum number of entries to replace"},
	  Density => RR => {"the fraction of entries of ", TT "M", " to be replaced, if ", TT "n", " is
	       not specified"},
	  UpperTriangular => Boolean => "whether to fill entries only above the diagonal",
	  Height => ZZ => "a bound on the absolute values of the generated random numbers"
	  },
     Outputs => {"M"},
     Consequences => {{ "some entries of M are replaced with randomly generated numbers, whose
	       size depends on the value of the option ", TT "Height" }},
     EXAMPLE lines ///
	  printingPrecision = 2
	  fillMatrix(mutableMatrix(RR,5,10))
	  fillMatrix(mutableMatrix(ZZ,5,10),UpperTriangular=>true)
	  fillMatrix(mutableMatrix(QQ,5,10),Density=>.2,Height=>1000)
	  fillMatrix(mutableMatrix(ZZ,5,10),25,Height=>1000)
	  ///,
     SeeAlso => {setRandomSeed, random, mutableMatrix}
     }
