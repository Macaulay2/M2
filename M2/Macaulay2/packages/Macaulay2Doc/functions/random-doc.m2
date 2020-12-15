document {
     Key => random,
     Headline => "get a random element",
     "This function can be used to get random elements of various sorts.",
     SeeAlso => {"setRandomSeed"}
     }
document {
     Key => (random, ZZ),
     Headline => "random integer",
     Usage => "random n",
     Inputs => {"n"=> {}},
     Outputs => {ZZ => {"a random integer in the range ", TT "0 .. n-1"}},
     EXAMPLE lines ///
     random 57
     random 10^50
     tally apply(100, i -> random 7)
     ///,
     SeeAlso => {setRandomSeed, tally}
     }

document {
     Key => (random, ZZ,ZZ),
     Headline => "random integer in a range",
     Usage => "random(min,max)",
     Inputs => {"min","max"},
     Outputs => {ZZ => {"a random integer in the range ", TT "min .. max"}},
     EXAMPLE lines ///
     for i to 10 list random(100,200)
     tally apply(100, i -> random(10,15))
     ///,
     SeeAlso => {"setRandomSeed"}
     }

document {
     Key => (random, RR),
     Headline => "random real number",
     Usage => "random x",
     Inputs => {"x"},
     Outputs => { RR=> {"a random positive real number less than ", TT "x", ", of the same precision"}},
     EXAMPLE lines ///
     random 3.14
     random 3p200
     ///,
     SeeAlso => {"setRandomSeed", (random,RR,RR)}
     }

document {
     Key => (random, RR,RR),
     Headline => "random real number",
     Usage => "random(x,y)",
     Inputs => {"x","r"},
     Outputs => { RR=> {"a random real number between ", TT "x", " and ", TT "y"}},
     EXAMPLE lines ///
     random(10.,20.)
     random(10p100,20p100)
     ///,
     SeeAlso => {"setRandomSeed", (random,RR)}
     }

document {
     Key => {(random, Type),(random, RingFamily),(random, QuotientRing),(random, GaloisField),
	  (random, ZZ, Ring),(random, List, Ring),[random,Height]
	  },
     Headline => "random element of a type",
     SYNOPSIS (
	  Usage => "random T",
	  Inputs => {
	       "T" => {ofClass Type},
	       Height => ZZ
	       },
	  Outputs => { { "a random instance of the type ", TT "T", ".  If the Height option specifies a number ", TT "h", "
		    and ", TT "T", " is ", TO "ZZ", " and , then the integers
		    returned are in the range ", TT "0 .. h-1", "; for ", TO "QQ", "
		    the numerator and denominator are in the range ", TT "1 .. h", "." } },
	  EXAMPLE lines ///
	  random RR
	  random CC_100
	  tally for i to 100 list random GF 11
	  random GF(2,40)
	  ///
	  ),
     SYNOPSIS (
	  Usage => "random(d,R)",
	  Inputs => {
	       "d" => {ofClass{ZZ,List}, ", the degree or multi-degree to use"},
	       "R" => Ring
	       },
	  Outputs => { { "a random homogeneous element of the ring ", TT "R", " of degree ", TT "d" } },
	  EXAMPLE lines ///
	  R = ZZ[x,y];
	  random(5,R)
	  R = GF(25,Variable=>a)[x,y];
	  VerticalList for i to 6 list random(3,R)
	  ///,
	  "The length of ", TT "d", ", if it's a list, should be the same as ", TT "degreeLength R", ".",
	  ),
     SeeAlso => {setRandomSeed}
     }

document {
     Key => {(random, Module, Module),[random, MaximalRank],[random, Density],[random, UpperTriangular]},
     Headline => "make a random module map",
     Usage => "f = random(F,G)",
     Inputs => {
	  "F" => {"a free module"},
	  "G" => {"a free module"},
	  MaximalRank => {"whether to ensure that the resulting map has maximal rank: designed mostly
	       for use with matrices of numbers: for polynomial rings, returns inhomogeneous results"},
	  Density => RR => {"the proportion of entries to set"},
	  UpperTriangular => Boolean => {"whether to set just entries strictly above the diagonal"}
	  },
     Outputs => {"f" => {"a random, graded, degree ", TT "0", " map, from ", TT "G", " to ", TT "F"}},
     EXAMPLE lines ///
	  R = ZZ/101[x,y];
	  random(R^{1,2,3},R^{1,2,3})
	  random(ZZ^3,ZZ^6,MaximalRank=>true)
	  random(ZZ^3,ZZ^10,Density=>.3)
	  random(ZZ^6,ZZ^6,UpperTriangular=>true)
	  ///,
     Caveat => {
	  "Over a polynomial ring, specifying ", TT "MaximalRank=>true", " will yield a non-homogeneous matrix."
	  },
     SeeAlso => {"setRandomSeed"}
     }

document {
     Key => (random,List),
     Headline => "shuffle a list randomly",
     Usage => "random x",
     Inputs => { "x" },
     Outputs => { List => {"a new list containing the elements of ", TT "x", " in a shuffled random order"} },
     EXAMPLE lines ///
	  random toList (0 .. 12)
     ///
     }

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
     "This function has been superceded by ", TO fillMatrix, ", which works over
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
