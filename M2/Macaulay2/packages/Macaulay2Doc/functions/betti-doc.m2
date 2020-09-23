--- status: DRAFT
--- author(s): caviglia, kummini 
--- notes: 

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
