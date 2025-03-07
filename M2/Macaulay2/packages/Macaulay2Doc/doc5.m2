--		Copyright 1993-2002 by Daniel R. Grayson

document {
     Key => override,
     Headline => "override default values for optional arguments",
     TT "override(defaults,args)", " overrides default values for
     optional arguments present in the argument sequence ", TT "args", ".",
     PARA{
	  "One possibility is for the argument ", TT "defaults", " to be an immutable hash table 
	  (of type ", TO "OptionTable", "), and ", TT "args", " should be
     	  a sequence of arguments, some of which are optional arguments of
     	  the form ", TT "x => v", ".  Each such optional argument
     	  is removed from ", TT "args", ", and the value in ", TT "defaults", "
     	  corresponding to the key ", TT "x", " is replaced by ", TT "v", ".
     	  The value returned is the modified pair ", TT "(defaults, args)", ".
	  An error is signalled if the key ", TT "x", " does not occur in ", TT "defaults", "."
	  },
     PARA {
	  "A second possibility is for the argument ", TT "defaults", " to be ", TO "null", ",
	  in which case the keys x are not checked for validity, and no default values
	  are provided.  The main use of this is to separate the optional arguments from
	  the other arguments, which can then be used for dispatching to the correct method."
	  },
     PARA{
	  "This function is intended for internal use only, and is used in the processing
     	  of optional arguments for method functions that accept them."
	  },
     EXAMPLE {
	  "defs = new OptionTable from { a => 1, b => 2 };",
	  "override(defs, (4,b=>6,5))"
	  }
     }



document {
     Key => Tally,
     Headline => "the class of all tally results",
     TT "Tally", " -- a class designed to hold tally results, i.e., multisets.",
     SeeAlso => {VirtualTally}
     }

document {
     Key => {VirtualTally,(symbol -,VirtualTally,VirtualTally)},
     "The only difference between this class and ", TO "Tally", " is that this class allows negative numbers.",
     EXAMPLE lines ///
     	  x = tally {a,b,b,c,c,c}
     	  y = tally {a,a,a,b,b,c}
     	  x' = new VirtualTally from x
     	  y' = new VirtualTally from y
	  x-y
	  x'-y'
     ///,
     SeeAlso => { BettiTally }
     }

document {
     Key => (symbol **, VirtualTally, VirtualTally),
     Headline => "Cartesian product of tallies",
     TT "x ** y", " -- produces the Cartesian product of two tallies.",
     PARA{},
     "One of the arguments may be a ", TO "Set", ".",
     PARA{},
     EXAMPLE {
	  "x = tally {a,a,b}",
      	  "y = tally {1,2,2,2}",
     	  "x ** y",
	  },
     SeeAlso => {"Tally", "tally"}
     }

document {
     Key => (symbol +, VirtualTally, VirtualTally),
     Headline => "union of tallies",
     TT "x + y", " -- produces the union of two tallies.",
     PARA{},
     "One of the arguments may be a ", TO "Set", ".",
     PARA{},
     EXAMPLE {
	      "x = tally {a,b,b,c,c,c,d,d,d}",
      	  "y = tally {a,a,a,b,b,c,d}",
     	  "x' = new VirtualTally from x",
	 	  "y' = new VirtualTally from y",
	 	  "z' = y' - x'",
	 	  "z' + x'",
	  	  "z' + y'",
	  },
     }
     
document {
     Key => (symbol -, VirtualTally),
     Headline => "negation of a VirtualTally",
     TT "-x", " -- the negation of ", TT "x",
     PARA{},
     EXAMPLE {
      	  "x = tally {a,b,b,c,c,d,d,d}",
	 	  "x' = new VirtualTally from x",
	  	  "- x'",
     },
}     



document {
     Key => (symbol +, Tally, Tally),
     Headline => "union of tallies",
     TT "x + y", " -- produces the union of two tallies.",
     PARA{},
     "One of the arguments may be a ", TO "Set", ".",
     PARA{},
     EXAMPLE {
	  "x = tally {a,a,a,b,b,c}",
      	  "y = tally {b,c,c,d,d,d}",
      	  "x + y",
	  },
     SeeAlso => {"Tally", "tally"}
     }

document {
     Key => (symbol -, Tally, Tally),
     Headline => "difference of tallies",
     Usage => "x - y",
     Inputs => { "x", "y" },
     Outputs => { { "the difference of the two tallies" } },
     "The count associated to an item ", TT "i", " in the result is the difference of the counts in
     ", TT "x", " and in ", TT "y", " if it's positive, otherwise, zero.",
     EXAMPLE "tally {a,a,b,c} - tally {c,d,d}",
     SeeAlso => "Tally"
     }

document {
     Key => (symbol ^**, VirtualTally, ZZ),
     Headline => "Cartesian power of sets and tallies",
     Usage => "B = A^**n",
     Inputs => { "A", "n" },
     Outputs => {"B" => { "the tally of ", TT "n", "-tuples of elements from ", TT "A" }},
     "If ", TT "A", " is ", ofClass Set, ", then so is ", TT "B", ".",
     EXAMPLE lines ///
     	  A = set {1,2}
	  A^**3
     	  A = tally {1,1,2}
	  A^**3
	  ///,
     SeeAlso => {Set, (symbol**,Set,Set)}
     }


document {
     Key => (symbol ^**, Module, ZZ),
     Headline => "tensor power",
     Usage => "M^**i",
     Inputs => { "M", "i" },
     Outputs => {Module => { "the ", TT "i", "-th tensor power of ", TT "M"}},
     "The second symmetric power of the canonical module of the
     rational quartic:",
     EXAMPLE lines ///
         R = QQ[a..d];
         I = monomialCurveIdeal(R,{1,3,4})
	 M = Ext^1(I,R^{-4})
	 M^**2
	 ///
     }

document {
     Key => wrap,
     Usage => "wrap(wid,sep,s)",
     Inputs => {
	  "wid" => ZZ,
	  "sep" => String,
	  "s" => String
	  },
     Outputs => {
	  { "a string obtained by wrapping the string ", TT "s", ", in case it is wider than the number ", TT "wid", ", so that it occupies multiple lines,
	       separated by lines filled with the single character in the string ", TT "sep", ", if provided"}
	  },
     "The inputs ", TT "wid", " and ", TT "sep", " are optional, and can be given in either order.  The default for ", TT "wid", " is ", TT "printWidth", ",
     and the default for ", TT "sep", " is null.",
     EXAMPLE {
	  ///wrap(10,"abcdefghijklmnopqrstuvwxyz")///,
	  ///wrap(10,"-","abcdefghijklmnopqrstuvwxyz")///
	  }
     }

document { Key => {(eagonNorthcott,Matrix),eagonNorthcott},
     Headline => "Eagon-Northcott complex of a matrix of linear forms",
     Usage => "eagonNorthcott f",
     Inputs => { "f" => "a matrix of linear forms" },
     Outputs => { "C" => {"the Eagon-Northcott complex of ", TT "f"} },
     "The Eagon-Northcott complex is an explicit chain complex that gives a minimal projective
     resolution of the cokernel of the matrix maximal minors of a generic matrix of linear forms.",
     EXAMPLE lines ///
     	  R = QQ[a..z]
	  f = genericMatrix(R,3,5)
	  M = coker gens minors_3 f
	  C = res M
	  D = eagonNorthcott f
	  H = prune HH D
	  assert( H_0 == M and H_1 == 0 and H_2 == 0 and H_3 == 0 )
     ///,
     "This function was written by Greg Smith."
     }

document { Key => {(selectVariables,List,PolynomialRing),selectVariables},
     Headline => "make a subring of a polynomial ring generated by selected variables",
     Usage => "(S,F) = selectVariables(v,R)",
     Inputs => {
	  "v" => {"a sorted list of numbers specifying which variables to select"},
	  "R"
	  },
     Outputs => {
	  "S" => PolynomialRing => {"a polynomial ring generated as a subring of R by the variables whose indices
	       occur in the list v, together with the induced monomial ordering"
	       },
	  "F" => RingMap => {"the inclusion map from S to R"}
	  },
     EXAMPLE lines ///
     (S,F) = selectVariables({2,4}, QQ[a..h,Weights=>1..8]);
     describe S
     options S
     F
     ///
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
