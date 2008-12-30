--		Copyright 1993-2002 by Daniel R. Grayson

document {
     Key => html,
     Headline => "convert hypertext to html format",
	Usage => "html x",
	Inputs => {"x" => {}},
	Outputs => {String => {}},
     TT "html x", " converts ", TT "x", " from ", TO "hypertext", " to html format",
     PARA{},
     "The return value is a string which is suitable for use in an
     html file, readable by a world wide web client such as Netscape.",
     SeeAlso => "mathML"
     }

document {
     Key => {EXAMPLE,(EXAMPLE, VisibleList),(EXAMPLE, String)},
     Headline => "hypertext EXAMPLE item",
	Usage => "EXAMPLE x",
	Inputs => {"x" => {}},
	Outputs => {TABLE => {}},
     TT "EXAMPLE x", " evaluates the string or list of strings
     ", TT "x", " as Macaulay 2 code, inserting the results in
     hypertext preformatted ", TO "PRE", " items.",
     PARA{},
     "The evaluation is done by the Makefile at a separate time, and the
     results are left where they can be found the next time the same
     EXAMPLE is encountered.",
     PARA{},
	"For example, the code",
	PRE "EXAMPLE { \"1+1\"}",
     " produces ",
     EXAMPLE {"1+1"},
     SeeAlso => "hypertext"
     }

document {
     Key => {Command,(symbol SPACE,Command,Thing)},
     Headline => "the class of all commands",
     Usage => "Command g",
     Inputs => { "g" => "a function or a string" },
     Outputs => { { "a new command that will evaluate ", TT "g()", " if ", TT "g", " is a function, and will evaluate ", TT "run g", " if ", TT "g", " is a string" } },
     "A command behaves as a function does if it is followed by an adjacent
     expression which can serve as its argument or argument list.  In addition,
     if it appears as the value of an expression typed by the user at top
     level (i.e., not in a file), then it gets executed with empty argument list.",
     EXAMPLE {
	  "(f = Command ( () -> 2^30 );)",
	  "f",
	  "(c = Command \"date\";)",
	  "c"
	  },
     SeeAlso => {"run", "AfterEval"}
     }


document {
     Key => monomialCurveIdeal, 
     Headline => "make the ideal of a monomial curve",
	Usage => "I = monomialCurveIdeal(R,a)",
	Inputs => {
		"R" => Ring => {},
		"a" => {"a list of integers to be used as exponents in the parametrization of a rational curve"}
		},
	Outputs => {"I" => Ideal => {}},
     TT "monomialCurveIdeal(R,a)", " yields the defining ideal of the projective
     curve given parametrically on an affine piece by 
     t |---> (t^a1, ..., t^an).",
     PARA{},
     "The ideal is defined in the polynomial ring R,
     which must have at least n+1 variables, preferably all of equal 
     degree.  The first n+1 variables in the ring are used",
     "For example, the following defines a plane quintic curve of genus 6.",
     EXAMPLE {
	  "R = ZZ/101[a..f]",
	  "monomialCurveIdeal(R,{3,5})",
	  },
     "Here is a genus 2 curve with one singular point.",
     EXAMPLE "monomialCurveIdeal(R,{3,4,5})",
     "Here is one with two singular points, genus 7.",
     EXAMPLE "monomialCurveIdeal(R,{6,7,8,9,11})",
     "Finally, here is the smooth rational quartic in P^3.",
     EXAMPLE "monomialCurveIdeal(R,{1,3,4})"
     }

TEST ///
    R := ZZ/101[a..f];
    -- plane quintic, genus=6
    I1 := monomialCurveIdeal(R,{3,5});
    assert(I1 == image matrix{{b^5-a^2*c^3}});

    -- one singular point, g=2
    I2 := monomialCurveIdeal(R,{3,4,5});
    assert(I2 == image matrix {{c^2-b*d, b^2*c-a*d^2, b^3-a*c*d}});

    -- two singular points, g=7
    I3 := monomialCurveIdeal(R,{6,7,8,9,11});
    assert(I3 == image matrix {{
               d*e-b*f, e^2-c*f, c*d-b*e, d^2-c*e, 
               c^2-b*d, b*c*e-a*f^2, b^2*d-a*e*f, b^2*c-a*d*f, b^3-a*c*f}});

    -- smooth rational quartic in P^3
    I4 := monomialCurveIdeal(R,{1,3,4});
    assert(I4 == image matrix {{b*c-a*d, c^3-b*d^2, a*c^2-b^2*d, b^3-a^2*c}});
///

document {
     Key => Fano, 
     Headline => "Fano scheme"
     }

document {
	Key => (Fano,ZZ,Ideal),
	Headline => "Fano scheme",
	Usage => "Fano(k,I)",
	Inputs => {
		"k" => {"a positive integer less than ", TT "r"},
		"I" => {"an ideal representing a variety in in projective ", TT "r", "-space"}, 
		},
	Outputs => {"the ideal of a Fano scheme in the Grassmannian"},
	  "Given an ideal ", TT "I", " representing a projective variety ", TT "X", "
     in ", TT "P^r", ", a positive integer k<r, and optionally a 
     ring ", TT "GR", " with (exactly) ", TT "r+1", " choose ", TT "k+1", " variables, 
     representing the ambient space of the Grassmannian of 
     k-planes in ", TT "P^r", ", this routine returns the ideal in
     ", TT "GR", " of the Fano scheme that parametrizes the k-planes 
     lying on ", TT "X", ".  If the optional third argument is not 
     present, the routine fabricates its own ring, 
     and returns an ideal in it.",
	SeeAlso => (Fano,ZZ,Ideal,Ring)
	}

document {
	Key => (Fano,ZZ,Ideal,Ring),
	Headline => "Fano scheme",
	Usage => "Fano(k,I,GR)",
	Inputs => {
		"k" => {"a positive integer less than ", TT "r"},
		"I" => {"an ideal representing a variety in in projective ", TT "r", "-space"},
		"GR" => {} 
		},
	Outputs => {"the ideal of a Fano scheme in the Grassmannian"},
	  "Given an ideal ", TT "I", " representing a projective variety ", TT "X", "
     in ", TT "P^r", ", a positive integer k<r, and optionally a 
     ring ", TT "GR", " with (exactly) ", TT "r+1", " choose ", TT "k+1", " variables, 
     representing the ambient space of the Grassmannian of 
     k-planes in ", TT "P^r", ", this routine returns the ideal in
     ", TT "GR", " of the Fano scheme that parametrizes the k-planes 
     lying on ", TT "X", ".  If the optional third argument is not 
     present, the routine fabricates its own ring, 
     and returns an ideal in it.",
	SeeAlso => (Fano,ZZ,Ideal)
	}

undocumented {
	  (code, List),
	  (code, Sequence),
	  (code, Function),
	  (code, Symbol),
	  (code, Command),
	  (code, Pseudocode),
	  (code, Nothing)}

document {
     Key => code,
     Headline => "display source code",
     SYNOPSIS (
	  Usage => "code f",
	  Inputs => {
	       "f" => {ofClass{Function,Command}}
	       },
	  Outputs => {Net => {"the source code of the function or command", TT "f"}},
	  EXAMPLE "code listUserSymbols"
	  ),
     SYNOPSIS {
	  Usage => "code(f,X)",
	  Inputs => {
	       "f" => {ofClass{Function,Keyword}},
	       "X" => Type
	       },
	  Outputs => {Net => {"the source code of method for applying ", TT "f", " to an
		    argument of type ", TT "X"
		    }},
	  EXAMPLE "code(res,Ideal)"
	  },
     SYNOPSIS {
	  Usage => "code(f,X,Y)",
	  Inputs => {
	       "f" => {ofClass{Function,Keyword}},
	       "X" => Type,
	       "Y" => Type
	       },
	  Outputs => {Net => {"the source code of method for applying ", TT "f", " to
		    arguments of type ", TT "X", " and ", TT "Y"
		    }},
	  EXAMPLE "code(symbol :, Ideal, Ideal)"
	  },
     SYNOPSIS {
	  Usage => "code(f,X,Y,Z)",
	  Inputs => {
	       "f" => {ofClass{Function,Keyword}},
	       "X" => Type,
	       "Y" => Type,
	       "Z" => Type
	       },
	  Outputs => {Net => {"the source code of method for applying ", TT "f", " to
		    arguments of type ", TT "X", ", ", TT "Y, and ", TT "Z"
		    }}
	  },
     SYNOPSIS {
	  Usage => "code(f,X,Y,Z,T)",
	  Inputs => {
	       "f" => {ofClass{Function,Keyword}},
	       "X" => Type,
	       "Y" => Type,
	       "Z" => Type,
	       "T" => Type
	       },
	  Outputs => {Net => {"the source code of method for applying ", TT "f", " to
		    arguments of type ", TT "X", ", ", TT "Y, and ", TT "Z", ", and ", TT "T"
		    }}
	  },
     SYNOPSIS {
	  Usage => "code {v,w,...}",
	  Inputs => {
	       "{v,w,...}" => List
	       },
	  Outputs => {Net => {"the source code of the functions or commands", TT "v,w,...", ".  
		    Such a list can be obtained, for example, with ", TO "methods", "."
		    }},
	  EXAMPLE "code methods use"
	  }
     }

document {
     Key => edit,
     Headline => "edit source code",
     SYNOPSIS {
	  Usage => "edit f",
	  Inputs => {
	       "f" => {ofClass{Function,Command}}
	       },
	  Outputs => {Net => {"the editor is started up, pointing at the source code of the function or command", TT "f"}},
	  },
     SYNOPSIS {
	  Usage => "edit(f,X)",
	  Inputs => {
	       "f" => {ofClass{Function,Keyword}},
	       "X" => Type
	       },
	  Outputs => {Net => {"the editor is started up, pointing at the source code of method for applying ", TT "f", " to an
		    argument of type ", TT "X"
		    }},
	  },
     SYNOPSIS {
	  Usage => "edit(f,X,Y)",
	  Inputs => {
	       "f" => {ofClass{Function,Keyword}},
	       "X" => Type,
	       "Y" => Type
	       },
	  Outputs => {Net => {"the editor is started up, pointing at the source code of method for applying ", TT "f", " to
		    arguments of type ", TT "X", " and ", TT "Y"
		    }},
	  },
     SYNOPSIS {
	  Usage => "edit(f,X,Y,Z)",
	  Inputs => {
	       "f" => {ofClass{Function,Keyword}},
	       "X" => Type,
	       "Y" => Type,
	       "Z" => Type
	       },
	  Outputs => {Net => {"the editor is started up, pointing at the source code of method for applying ", TT "f", " to
		    arguments of type ", TT "X", ", ", TT "Y, and ", TT "Z"
		    }}
	  },
     SYNOPSIS {
	  Usage => "edit(f,X,Y,Z,T)",
	  Inputs => {
	       "f" => {ofClass{Function,Keyword}},
	       "X" => Type,
	       "Y" => Type,
	       "Z" => Type,
	       "T" => Type
	       },
	  Outputs => {Net => {"the editor is started up, pointing at the source code of method for applying ", TT "f", " to
		    arguments of type ", TT "X", ", ", TT "Y, and ", TT "Z", ", and ", TT "T"
		    }}
	  },
     PARA{
	  "The name of the user's preferred editor is take from the environment 
	  variable ", TT "EDITOR", ".  If X is running and the editor is not
	  emacs, then the editor is started in a new ", TT "xterm", " window."
	  },
     PARA{
	  "For an interactive example, try ", TT "edit(dim,Module)", ".",
	  },
     PARA{
	  "The value returned is the exit code returned by the editor, as with ", TO "run", ", usually zero."
	  }
     }

document {
     Key => {methods,(methods, Command),(methods, Sequence),(methods, Thing),(methods, Type)},
     Headline => "list methods",
     SYNOPSIS (
	  Usage => "methods x",
	  Inputs => {
	       "x" => { ofClass{Function,Type,Keyword} }
	       },
	  Outputs => {{
		    ofClass VerticalList, " of those methods associated with ", TT "x"
		    }},
	  EXAMPLE lines ///
	       methods BettiTally
	       methods resolution
	       methods symbol @@
	  ///
	  ),
     SYNOPSIS (
	  Usage => "methods(s,X)",
	  Inputs => {
	       "s" => Symbol, "X" => Type
	       },
	  Outputs => {{
		    ofClass VerticalList, " of those methods associated with the operator ", TT "s",
		    " and the type ", TT "X"
		    }},
	  EXAMPLE lines ///
	       methods( symbol ++, Module)
	  ///
	  ),
     SYNOPSIS (
	  Usage => "methods(X,Y)",
	  Inputs => {
	       "X" => Type, "Y" => Type
	       },
	  Outputs => {{
		    ofClass VerticalList, " of those methods associated with "
		    }},
	  EXAMPLE lines ///
	       methods( Matrix, Matrix )
	  ///
	  ),
     "This function operates by examining those types that are values of
     global symbols for keys which appear to be storing references to
     methods.  Types which don't appear as values of global variables will
     not be examined, so perhaps not all methods will be found."
     }

document {
     Key => isTable,
     Headline => "whether something is a rectangular list of lists",
     PARA{},
     "Warning: it is intrinsically impossible to represent a ", TT "0", " 
     by ", TT "k", " matrix as a list of lists.",
     EXAMPLE {
	  "isTable {{1,2,3},{4,5}}",
	  "isTable {{1,2,3},{4,5,6}}",
	  }
     }

document {
     Key => Monoid,
     Headline => "the class of all monoids",
     "A monoid is a set with a multiplicative operation on
     it and an identity element.  A typical monoid is the set
     of monomials in a polynomial ring, which we consider to be
     created before the polynomial ring is created."
     }

document {
     Key => {(RLE,VisibleList),RLE},
     Headline => "run length encoding",
     Usage => "RLE x",
     Inputs => { "x" },
     Outputs => {{ "a list equivalent to ", TT "x", ", in which runs and sequences have been expressed 
	       symbolically as ", TO2{Expression,"expressions"}}},
     PARA {"The result is useful in printed displays, as a way of making them more compact.  The original list can
	  be recovered by appying ", TO "value", " to the elements of the result, and then using ", TO "deepSplice", ",
	  provided that ", TT "x", " contains no entries that are sequences."},
     EXAMPLE lines ///
     x = {1,2,3,a,b,c,a,b,c,4,4,4,"asdf"};
     y = RLE x
     peek y
     value \ y
     deepSplice \\ oo
     x === oo
     ///,
     SeeAlso => {BinaryOperation, Holder}
     }

undocumented { 
     (tex,BettiTally),(texMath,BettiTally),
     (toExternalString,RingElement), (toExternalString,RingMap),
     (image,RingMap),					    -- just an error message referring to coimage
     (symbol ==, Constant, RingElement),
     (symbol ==, RingElement, Constant),
     ((symbol SPACE, symbol =), Function, Thing),
     ((symbol _*, symbol =), RingFamily) 
     }

document {
     Key => {(isSorted,VisibleList), isSorted},
     Headline => "whether a list is sorted",
     Usage => "isSorted x",
     Inputs => { "x" },
     Outputs => { Boolean => {"whether the elements of the list ", TT "x", " are in increasing order"}},
     SourceCode => (isSorted,VisibleList),
     EXAMPLE lines ///
     isSorted {1,2,2,3}
     isSorted {1,2,3,2}
     ///
     }     

document {
     Key => {(switch,ZZ,ZZ,VisibleList), switch},
     Headline => "copy a list, switching two elements",
     Usage => "switch(i,j,x)",
     Inputs => {"i","j","x"},
     Outputs => {{"a copy of the list ", TT "x", " in which the elements at positions ", TT "i", " and ", TT "j", " have
	       been interchanged.  A negative value of ", TT "i", " or ", TT "j", " is taken relative to the end of the list."
	       }},
     EXAMPLE lines ///
     switch(3,9,0..10)
     switch(0,-1,0..10)
     ///
     }

document {
     Key => {(insert,ZZ,Thing,VisibleList), insert},
     Headline => "copy a list, inserting an element",
     Usage => "insert(i,a,x)",
     Inputs => {"i","t","x"},
     Outputs => {{"a copy of the list ", TT "x", " in which ", TT "t", " has been inserted
	       into position ", TT "i", " of the result.  A negative value of ", TT "i", " 
	       is taken relative to the end of the list."
	       }},
     EXAMPLE lines ///
     insert(4,t,0..10)
     insert(0,t,0..10)
     insert(11,t,0..10)
     insert(-1,t,0..10)
     ///
     }

document {
     Key => {heft,(heft, Ring),(heft, Module)},
     Headline => "heft vector of ring or module",
     Usage => "heft X",
     Inputs => { "X" => {ofClass{Ring,Module}} },
     Outputs => { List => {"the heft vector in use for ", TT "X", ", if ", TT "X", " is a 
	       ring, or for the ring of ", TT "X", ", if ", TT "X", " is a module.
	       If there is no heft vector, then ", TO "null", " is returned."
	       }},
     EXAMPLE lines ///
     S = QQ[a..d,DegreeRank => 4];
     degrees S
     heft S
     ///,
     SeeAlso => {"heft vectors"}
     }

document {
     Key => "heft vectors",
     PARA {
	  "A ", EM "heft vector", " for a polynomial ring is a vector with integer entries, of the same length
	  as the degree vectors of the variables of the ring, whose dot product with each of them
	  is (strictly) positive.  Unless one is specified explicitly, then a good one will be
	  found automatically.  The heft vector is used in various internal algorithms
	  as a way of organizing the sequence of steps, proceeding incrementally to larger
	  values of the dot product of the degree of a monomial with the heft vector."
	  },
     EXAMPLE lines ///
     R = QQ[a..d];
     degrees R
     heft R
     S = QQ[a..d,DegreeRank => 4];
     degrees S
     heft S
     T = QQ[a,b,Degrees => {1,-1}]
     degrees T
     heft T
     U = QQ[a..d,Degrees => {{2,0},{1,-1},{0,-2},{-1,-3}}]
     degrees U
     heft U
     ///,
     PARA {
	  "The heft vector, multiplied by -1, is used as the weight vector in the monomial ordering of 
	  the degrees ring, and the order of its approximate expansions refers to 
	  the weight formed with respect to that weight vector."
	  },
     EXAMPLE lines ///
     hilbertSeries U
     describe ring numerator oo
     hilbertSeries(U,Order => 8)
     ///,
     SeeAlso => {heft, [monoid,Heft], degreesRing}
     }

document {
     Key => {multidegree,(multidegree,Module), (multidegree,Ideal), (multidegree,Ring)},
     Headline => "multidegree",
     Usage => "multidegree M",
     Inputs => { "M" => {ofClass{Module,Ideal,Ring}} },
     Outputs => { {"the multidegree of ", TT "M", ".  If ", TT "M", " is an ideal, the corresponding quotient ring is used."} },
     PARA {
	  "The multidegree is defined on page 165 of ", EM "Combinatorial Commutative Algebra", ", by
	  Miller and Sturmfels, on page 165.  It is an element of the degrees ring of ", TT "M", ".  Our
	  implementation agrees with their definition provided the heft vector of the ring has every entry equal to 1."
	  },
     EXAMPLE lines ///
     S = QQ[a..d, Degrees => {{2,-1},{1,0},{0,1},{-1,2}}];
     heft S
     multidegree ideal (b^2,b*c,c^2)
     multidegree ideal a
     multidegree ideal (a^2,a*b,b^2)
     describe ring oo
     ///,
     Caveat => {"This implementation is provisional in the case where the heft vector does not have every entry equal to 1."},
     SeeAlso => {"heft vectors", degreesRing}
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
