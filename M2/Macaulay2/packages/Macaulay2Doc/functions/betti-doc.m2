--- status: DRAFT
--- author(s): caviglia, kummini
--- notes: functions below are all defined in betti.m2

document { Key => {BettiTally,
	  (symbol ++,BettiTally,BettiTally), (symbol SPACE,BettiTally,ZZ),
	  (symbol **,BettiTally,BettiTally), (symbol *,QQ,BettiTally),(symbol *,ZZ,BettiTally),(lift, BettiTally, ZZ),
	  (symbol SPACE,BettiTally,Array),
	  (dual,BettiTally),(degree, BettiTally),(codim, BettiTally),(regularity,BettiTally),(hilbertPolynomial, ZZ, BettiTally),
	  (hilbertSeries, ZZ, BettiTally),(pdim, BettiTally),(poincare, BettiTally)
	  },
     Headline => "the class of all Betti tallies",
     "A Betti tally is a special type of ", TO "Tally", " that is printed as a display of
     graded Betti numbers.  The class was created
     so the function ", TO "betti", " could return something that both prints nicely and
     from which information can be extracted.  The keys
     are triples ", TT "(i,d,h)", ", where ", TT "i", " is the homological degree, ",
     TT "d", " is a list of integers giving a multidegree,
     and ", TT "h", " is the result of applying a weight covector to ", TT "d", ".
     Only ", TT "i", " and ", TT "h", " are used in printing.",
     EXAMPLE lines ///
	  t = new BettiTally from { (0,{0},0) => 1, (1,{1},1) => 2, (2,{3},3) => 3, (2,{4},4) => 4 }
	  peek oo
     ///,
     "For convenience, the operations of direct sum (", TO "++", "), tensor product (", TO "**", "), ",
     TO codim, ", ",
     TO degree, ", ",
     TO dual, ", ",
     TO hilbertPolynomial, ", ",
     TO hilbertSeries, ", ",
     TO pdim, ", ",
     TO poincare, ", ",
     TO regularity, ", and degree shifting (numbers in brackets or parentheses), have
     been implemented for Betti tallies.  These operations mimic the corresponding operations on chain complexes.",
     EXAMPLE lines ///
	  t(5)
	  t[-5]
	  t ++ oo
	  t ** t
	  dual t
	  regularity t
     ///,
     "A Betti tally can be multiplied by an integer or by a rational number, and the values can be lifted
     to integers, when possible.",
     EXAMPLE lines ///
	  (1/2) * t
	  2 * oo
	  lift(oo,ZZ)
     ///,
     "Various combinations of the degree vectors can be displayed by using ", TO (betti,BettiTally), "."
     }

document { Key => {(betti,BettiTally)},
     Headline => "view and set the weights of a betti display",
     Usage => "betti(t, Weights => w)",
     Inputs => {
	  "t",
	  Weights => List => { "a list of integers, ", TT "w", ", with the same degree length as that of ", TT "t"}
	  },
     Outputs => {
	  BettiTally => {"different from the input only in its degree weights.  If ", TT "w", " is
	       non-null, then a new betti tally with new weight values is returned"}
	       },
     EXAMPLE lines ///
	  R = ZZ/101[a..d,Degrees=>{2:{1,0},2:{0,1}}];
	  I = ideal random(R^1, R^{2:{-2,-2},2:{-3,-3}});
	  t = betti res I
	  peek t
	  ///,
     "The following three displays display the first degree, the second degree, and the total
     degree, respectively.",
     EXAMPLE lines ///
	  betti(t,Weights=>{1,0})
	  betti(t,Weights=>{0,1})
	  t1 = betti(t,Weights=>{1,1})
	  peek t1
     ///
     }

document { Key => {
	MultigradedBettiTally,
	(symbol SPACE, MultigradedBettiTally, List)},
    Headline => "the class of all multigraded Betti tallies",
    "A multigraded Betti tally is a special type of ", TO "BettiTally", " that is
     printed as a display of the multigraded Betti numbers.  The class was
     created so that the function ", TO "multigraded", " could return something that
     both prints nicely and from which information could be extracted.  The keys
     are triples ", TT "(i,d,h)", " where ", TT "i", " is the homological
     degree, ", TT "d", " is a list of integers giving a multidegree, and ",
     TT "h", " is the result of applying a weight covector to ", TT "d", ".",
    PARA{},
    "By default the data is presented as a table of polynomials where each column
     corresponds to a given homological degree appearing as the top entry and each
     monomial in the other entries represents the multidegree of a given generator.",
    PARA{},
    "When ", TT "compactMatrixForm", " is set to true, the entries in a
     column correspond to a fixed multidegree, ordered by the ", TT "h",
     ".  The number of summand correspond to a given multidegree appears to
     the left of the multidegree.",
    EXAMPLE lines ///
      B = new MultigradedBettiTally from {(0, {0, 0}, 0) => 1, (1, {0, 2}, 2) => 1, (1, {1, 1}, 2) => 2, (1, {2, 0}, 2) => 1, (2, {1, 2}, 3) => 2, (2, {2, 1}, 3) => 2, (3, {2, 2}, 4) => 1}
      peek oo
    ///,
    "For convenience, most operations on", TT "BettiTally", " such as direct sum
     (", TO "++", "), tensor product (", TO "**", "), ", TO "pdim", " and degree
     shifting (numbers in brackets or lists in parentheses) are automatically
     extended to work with multigraded Betti tables.  These operations mimic the
     corresponding operations on chain complexes.",
    EXAMPLE lines ///
      B({-1,-1})
      B[1]
      B[1] ++ B
      B ** B
      pdim B
      compactMatrixForm = false
      dual B
    ///,
    "A multigraded Betti tally also can multiplied by an integer or by a rational number.",
    EXAMPLE lines ///
      (1/2) * B
      2 * oo
      lift(oo,ZZ)
    ///,
    "This feature was implemented by Mahrud Sayrafi based on earlier work by Gregory G. Smith.",
    SeeAlso => { BettiTally }
    }

document { Key => { multigraded, (multigraded, BettiTally) },
    Headline => "convert a Betti tally into a multigraded Betti tally",
    Usage => "multigraded t",
    Inputs => { "t" => BettiTally },
    Outputs => { MultigradedBettiTally => { "different from the input only in the ordering of each column"} },
    "A multigraded Betti tally is a special type of ", TO "BettiTally", " that both
     prints nicely and from which multigraded Betti numbers could be easily extracted.",
    EXAMPLE lines ///
      R = ZZ/101[a..d, Degrees => {2:{1,0},2:{0,1}}];
      I = ideal random(R^1, R^{2:{-2,-2},2:{-3,-3}});
      t = betti res I
      peek t
      B = multigraded t
      peek B
    ///,
    "By changing the weights, we can reorder the columns of the diagram. The following three
     displays display the first degree, the second degree, and the total degree, respectively.",
    EXAMPLE lines ///
      betti(B, Weights => {1,0})
      betti(B, Weights => {0,1})
      B' = betti(B, Weights => {1,1})
    ///,
    SeeAlso => {
	MultigradedBettiTally,
	(betti, BettiTally)
	}
    }

------------------

doc ///
   Key
     [betti,Minimize]
   Headline
     minimal betti numbers of a non-minimal free resolution
   Usage
     betti(C, Minimize => true)
   Inputs
     C:ChainComplex
       computed using @TO FastNonminimal@ (and therefore a
           non-minimal free resolution of an ideal or
           module in a polynomial ring
           or skew commuting polynomial ring, over a finite prime field)
   Outputs
     :BettiTally
   Description
    Text
      Given a chain complex computed using {\tt res(I, FastNonminimal => true)} (@TO FastNonminimal@),
      returns the minimal graded Betti numbers of this complex.

      To get the actual betti numbers of the non-minimal resolution, use @TO betti@.

      If you simply want the minimal betti numbers of a module or ideal {\tt I},
      use @TO "minimalBetti"@.
    Example
      I = Grassmannian(1,6, CoefficientRing => ZZ/101);
      S = ring I
      elapsedTime C = res(I, FastNonminimal => true)
    Text
      For a non-minimal resolution, @TO betti@ gives the actual ranks of the complex.
      If the option @TT "Minimize => true"@ is given, the minimal Betti numbers are returned.
    Example
      betti C
      betti(C, Minimize=>true)
    Text
      This command is useful if the non-minimal free resolution has already been computed.
      However, as mentioned above, if one wants the minimal betti numbers of an ideal or module,
      it is recommended to use the function @TO "minimalBetti"@ as that avoids much computation,
      and allows the use of length and degree limits.
   Caveat
     Released in M2 1.9, still experimental.  Only works over finite prime field.
     If the complex is the resolution of a non-homogeneous or multi-homogeneous object,
     then this function will result in an error.
   SeeAlso
     minimalBetti
     betti
     resolution
     FastNonminimal
///

document {
     Key => [betti, Weights],
     Usage => "betti(..., Weights => w)",
     Inputs => {
	  "w" => List => { "a list of integers" }
	  },
     PARA {
	  "The module or chain complex provided should be over a ring whose degree length
	  (see ", TO "degreeLength", ") is the same as the length of the list ", TT "w", ".
	  The dot products of w with the multi-degrees occurring will be used to construct
	  the resulting betti tally (see ", TO "BettiTally", ").  If the option is not
	  specified, then the heft vector of the underlying ring is used, see ", TT "heft vectors", ".
	  If the ring has no heft vector, then the weights are taken to be all zero."
	  }
     }

document {
     Key => [regularity, Weights],
     Usage => "regularity(..., Weights => w)",
     Inputs => {
	  "w" => List => { "a list of integers" }
	  },
     PARA {
	  "The module or chain complex provided should be over a ring whose degree length
	  (see ", TO "degreeLength", ") is the same as the length of the list ", TT "w", ".
	  The dot products of w with the multi-degrees occurring will be used in
	  the resulting computation."
	  }
     }

document { 
     Key => {betti},
     Headline => "display degrees",
     "The function ", TT "betti", " displays the degrees of generators and
     relations of graded modules and ideals."
     }

document { 
     Key => (betti,GroebnerBasis),
     Headline => "diagram of the degrees of a groebner basis",
     Usage => "betti G",
     Inputs => { "G" => GroebnerBasis },
	Outputs => { { "a diagram showing the degrees of the generators of the
	source and target modules of the matrix of generators of ", TT "G" } },
	"Here is an example:",
     EXAMPLE {
		   "S = ZZ/10007[x,y];",
		   "G = gb ideal(x^3+y^3, x*y^4);",
		   "gens G",
		   "betti G"
	  },
     "For an explanation of the diagram, see ", TO (betti, GradedModule), ".",
     }
document { 
     Key => (betti,Matrix),
     Headline => "display of the degrees of a map",
     Usage => "betti f",
     Inputs => { "f" => Matrix },
     Outputs => { { "a diagram showing the degrees of the generators of the source and target modules of ", TT "f" } },
     EXAMPLE {
	  "S = ZZ/10007[x,y];",
	  "betti matrix{{x^3,x*y^2},{y*x,y^2}}"
	  },
     "For an explanation of the diagram, see ", TO (betti, GradedModule), ".",
     Caveat => {"The diagram ignores the degree of the map itself."},
     "For an explanation of the diagram, see ", TO (betti, GradedModule), ".",
     }
document { 
     Key => {(betti,Module),(betti,CoherentSheaf)},
     Headline => "show the degrees of the generators and relations of a module or a coherent sheaf",
     Usage => "betti M",
     Inputs => { "M" => Module => " graded." },
     Outputs => {{ "a diagram showing the degrees of the generators and the relations in the module ", TT "M" }
	  },
     "The diagram lists the zeroth and first graded and total Betti numbers of the module.",
     EXAMPLE {
		"S = ZZ/10007[x,y];",
		"betti coker matrix{{x^3,x*y^2},{y*x^2,y^3}}",
	  },
	 "Another example:",
     EXAMPLE {
	 "betti coker map(S^{0,-1},,matrix{{x^2,y},{y^3,x^2}})",
	 },
     "For an explanation of the diagram, see ", TO (betti,GradedModule), ".",
     }

document {
     Key => (betti, GradedModule),
     Headline => "display of degrees in a graded module",
     Usage => "betti C",
     Inputs => {
	  "C" => GradedModule,
	  Weights => List => {
	       "a list of integers whose dot product with the multidegree of a basis
	       element is enumerated in the display returned.  The default is the
	       heft vector of the ring.  See ", TO "heft vectors", "."
	       }
	  },
     Outputs => { { "a diagram showing the degrees of the generators of the modules in ", TT "C"} },
     PARA {
     	  "The diagram can be used to determine the degrees of the entries in the matrices
     	  of the differentials in a chain complex (which is a type of graded module) provided they are homogeneous maps
     	  of degree 0."
     	  },
     PARA{"Here is a sample diagram."},
     EXAMPLE {
	  "R = ZZ/101[a..h]",
      	  "p = genericMatrix(R,a,2,4)",
      	  "q = generators gb p",
      	  "C = resolution cokernel leadTerm q",
      	  "betti C",
	  },
     PARA {
	  "Column ", TT "j", " of the top row of the diagram gives the rank of the
	  free module ", TT "C_j", ". (Columns are numbered from 0.)  The entry in
	  column ", TT "j", " in the row labelled ", TT "i", " is the number
	  of basis elements of (weighted) degree ", TT "i+j", " in the free module", TT " C_j",
	  ".  When the chain complex is the resolution of a module the entries are
	  the total and the graded Betti numbers of the module."
	  },
     PARA{
	  "If the numbers are needed in a program, then they are accessible, because
	  the value returned is ", ofClass BettiTally, ", and the diagram you see
	  on the screen is just the way it prints out."
	  },
     PARA {
	  "The heft vector is used, by default, as the weight vector for weighting the
	  components of the degree vectors of basis elements."
	  },
     EXAMPLE lines ///
     R = QQ[a,b,c,Degrees=>{-1,-2,-3}];
     heft R	  
     betti res coker vars R
     betti(oo, Weights => {1})
     R = QQ[a,b,c,d,Degrees=>{{1,0},{2,1},{0,1},{-2,1}}];
     heft R	  
     b = betti res coker vars R
     betti(b, Weights => {1,0})
     betti(b, Weights => {0,1})
     ///
     }

document { 
     Key => {(betti,Ideal),(betti,MonomialIdeal)},
     Headline => "gives the degrees of generators.",
     Usage => "betti I",
     Inputs => { "I" => Ideal },
     Outputs => { { "a diagram showing the degrees of the generators of the modules in ", TT "C" } },
	"The output is the degrees of generators and relations of the quotient of
	the ambient ring by the ideal.", 
     EXAMPLE {
		"S = ZZ/10007[x,y];",
		"I = ideal(x^2,y^3);",
		"betti I",
	},
     "For an explanation of the diagram, see ", TO (betti,GradedModule), ".",
	}
