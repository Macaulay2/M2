-- -*- coding: utf-8 -*-
--		Copyright 1993-2002 by Daniel R. Grayson

document {
     Key => { formation, (formation,Module), (formation, ChainComplex), (formation, ChainComplexMap), (formation, GradedModule), (formation, GradedModuleMap) },
     Headline => "recover the methods used to make a module",
     Usage => "formation M",
     Inputs => { "M" => Module => "a module" },
     Outputs => { Expression => { ofClass Expression, " whose value is the module itself" }},
     PARA {
	  "If the module was created as a direct sum, tensor product, of Hom-module, then the expression will reflect that.
	  In each case, the result is a function application, and the sequence of arguments is easily obtained."
	  },
     EXAMPLE lines ///
	 M = ZZ^2 ++ ZZ^3
	 t = formation M
	 peek t
	 t#1
	 value t
	 M = directSum(ZZ^2, ZZ^3, ZZ^4)
	 t = formation M
	 t#1
	 M = ZZ^2 ** ZZ^3
	 t = formation M
	 t#1
     ///,
     PARA {
	  "If the module was not obtained that way, then ", TO "null", " is returned."
	  },
     EXAMPLE lines ///
         formation ZZ^6
     ///,
     PARA {
	  "The same remarks apply to certain other types of objects, such as chain complexes."
	  },
     EXAMPLE lines ///
          R = QQ[x,y];
	  C = res coker vars R;
	  D = C ++ C
	  formation D
	  ///,
     SeeAlso => { directSum, (symbol ++, Module, Module), (symbol **, Module, Module), (Hom,Module,Module), Expression, FunctionApplication}
     }
TEST ///
     assert( (     M = ZZ^2 ++ ZZ^3) === ZZ^5 );
     assert( (     formation M) === new FunctionApplication from {directSum,(ZZ^2,ZZ^3)} );
     assert( (     M = directSum(ZZ^2, ZZ^3, ZZ^4)) === ZZ^9 );
     assert( (     formation M) === new FunctionApplication from {directSum,(ZZ^2,ZZ^3,ZZ^4)} );
     assert( (     M = ZZ^2 ** ZZ^3) === ZZ^6 );
     assert( (     formation M) === new FunctionApplication from {tensor,(ZZ^2,ZZ^3)} );
///

document {
     Key => html,
     Headline => "convert to html format",
	Usage => "html x",
	Inputs => {"x" => {}},
	Outputs => {String => {}},
     TT "html x", " converts ", TT "x", " from ", TO "hypertext", " to html format",
     PARA{},
     "The return value is a string that is suitable for use in an
     html file, readable by a world wide web client.
     When no html conversion is available,", TO "tex", "is called.
     (La)TeX can be rendered in the browser using MathJax or Katex.
     ",
     SeeAlso => "mathML"
     }

document {
     Key => {EXAMPLE,(EXAMPLE, VisibleList),(EXAMPLE, String)},
     Headline => "hypertext EXAMPLE item",
     Usage => "EXAMPLE x",
     Inputs => {"x" => {"a string or list of strings or objects of class ", TO "PRE", "."}},
     Outputs => {TABLE => {"a table containing the examples.  Each string
	       will be interpreted by ", TO "installPackage", ", if the table is included in the input provided to ", TO "document", ",
	       as example input to be evaluated so the result can be displayed
	       in the documentation.  Each object of class ", TO "PRE", " will be inserted unchanged into the documentation
	       as example output."}},
     "For example, the code", PRE ///EXAMPLE { "1+1"}///, "produces a display that looks like this:",
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
     expression that can serve as its argument or argument list.  In addition,
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
		    arguments of type ", TT "X", ", ", TT "Y", ", and ", TT "Z"
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
		    arguments of type ", TT "X", ", ", TT "Y", ", ", TT "Z", ", and ", TT "T"
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
     global symbols for keys that appear to be storing references to
     methods.  Types that don't appear as values of global variables will
     not be examined, so perhaps not all methods will be found."
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
     Key => {(runLengthEncode,VisibleList),runLengthEncode},
     Headline => "run length encoding",
     Usage => "runLengthEncode x",
     Inputs => { "x" },
     Outputs => {{ "a list equivalent to ", TT "x", ", in which runs and sequences have been expressed 
	       symbolically as ", TO2{Expression,"expressions"}}},
     PARA {"The result is useful in printed displays, as a way of making them more compact.  The original list can
	  be recovered by appying ", TO "value", " to the elements of the result, and then using ", TO "deepSplice", ",
	  provided that ", TT "x", " contains no entries that are sequences."},
     EXAMPLE lines ///
     x = {1,2,3,a,b,c,a,b,c,4,4,4,"asdf"};
     y = runLengthEncode x
     peek y
     value \ y
     deepSplice \\ oo
     x === oo
     ///,
     SeeAlso => {BinaryOperation, Holder}
     }

undocumented { 
     (texMath,BettiTally),
     (toExternalString,RingElement), (toExternalString,RingMap),
     (symbol ==, Constant, RingElement),
     (symbol ==, RingElement, Constant),
     ((symbol SPACE, symbol =), Function, Thing),
     ((symbol _*, symbol =), RingFamily) 
     }

document {
     Key => {heft,(heft, Ring),(heft, Module),(heft,GradedModule),(heft,Resolution)},
     Headline => "heft vector of ring, module, graded module, or resolution",
     Usage => "heft X",
     Inputs => { "X" => {ofClass{Ring,Module,GradedModule,Resolution}} },
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
	  found automatically.  The heft vector is used in various internal algorithms, such as the one
	  in ", TO "basis", ", as a way of organizing the sequence of steps, proceeding incrementally to larger
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
	  the degrees ring, and the ", EM "order", " of the series expansions of the Hilbert series refers to 
	  the weight formed with respect to that weight vector."
	  },
     EXAMPLE lines ///
     hilbertSeries U
     describe ring numerator oo
     hilbertSeries(U,Order => 8)
     ///,
     PARA {
	  "The heft vector is used in the computation of degrees of modules over a polynomial ring ", TT "R", ", because it
	  gives a homomorphism from the degrees ring of ", TT "R", " to the Laurent
	  polynomial ring in one variable ", TT "T", " that sends monomials corresponding to the degrees of
	  variables of ", TT "R", " to positive powers of ", TT "T", ".  See ", TO "degree(Module)", "."
	  },
     EXAMPLE lines ///
     R = QQ[x,y,Heft=>{3}];
     degree ideal(x)
     ///,
     SeeAlso => {heft, [monoid,Heft], degreesRing, multidegree}
     }

document {
     Key => {multidegree,(multidegree,Module), (multidegree,Ideal), (multidegree,Ring)},
     Headline => "multidegree",
     Usage => "multidegree M",
     Inputs => { "M" => {ofClass{Module,Ideal,Ring}} },
     Outputs => { {"the multidegree of ", TT "M", ".  If ", TT "M", " is an ideal, the corresponding quotient ring is used."} },
     PARA {
	  "The multidegree is defined on page 165 of ", EM "Combinatorial Commutative Algebra", ", by
	  Miller and Sturmfels.  It is an element of the degrees ring of ", TT "M", ".  Our
	  implementation agrees with their definition provided the heft vector of the ring has every entry equal to 1.
	  See also ", EM "Gröbner geometry of Schubert polynomials", ", by Allen Knutson and Ezra Miller."
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

document {
     Key => "division in polynomial rings with monomials less than 1",
     PARA {
	  "Starting with version 1.2, a new division algorithm has been implemented in 
	  rings with inverses, where the monomials can involve negative exponents, and hence
	  do not form a well-ordered set.  The ring should have a monomial ordering whose
	  first test involves at least one weight vector, explicitly, or perhaps implicitly, as with
	  ", TO "GRevLex", ".  The algorithm will work when dividing by
	  a polynomial that is ", EM "monic", " in the sense that its lead monomial has coefficient 1,
	  and all other terms have smaller weight, where the weight is computed with
	  respect to just the first weight vector.  When we say the algorithm works, we
	  mean: (1) that it terminates; and (2) that the remainder is zero if and only if the denominator
	  divides the numerator."
	  },
     PARA {
	  "Define the length of a nonzero ring element to be the weight of the first term minus
	  the weight of the last term.  The length is greater than or equal to 0, because
	  the terms in a sorted polynomial are decreasing in weight."
	  },
     PARA {
	  "We refuse to start dividing unless the denominator is monic in the sense defined above.
	  When dividing, we keep subtracting monomial multiples of the denominator
	  from the numerator to eliminate the lead term of the numerator, which is always possible
	  because the ring contains the reciprocals of its variables.  We stop
	  when we get a remainder whose length is strictly less than the length of the denominator."
	  },
     PARA {
	  "This algorithm works because, in an integral domain, the length of a product is
	  the sum of the lengths of the factors.  Thus the remainder, if it is not zero, can
	  not be a multiple of the denominator."
	  },
     PARA {
	  "This will be good enough for applications to Hilbert series, because in our degrees rings, the denominator of a
	  Hilbert series will be a product of terms ", TT "1-T", ", where ", TT "T", " is a monomial of
	  strictly negative weight.  That's because the weight vector is minus the heft
	  vector of the original ring, and ", TT "T", " is the monomial constructed from the degree
	  vector of one of the variables in the original ring.  Note that any divisor of
	  such a product will also be 1 plus terms of negative weight."
	  },
     EXAMPLE lines ///
     R = QQ[x,y, Inverses => true, MonomialOrder => Lex, Weights => {1,2}]
     quotientRemainder(x^100 - x^89, x^5 - 1)
     quotientRemainder(x^100 - y^61, x^5 - 1)
     ///,
     SeeAlso => {"heft vectors", "polynomial rings", degreesRing}
     }

document {
     Key => inversePermutation,
     Headline => "inverse permutation",
     Usage => "y = inversePermutation x",
     Inputs => {
	  "x" => List => {"a list of length ", TT "n", " whose elements are the numbers 0, 1, 2, ..., ", TT "n-1", ", in some order,
	       representing the permutation defined by sending ", TT "i", " to ", TT "x#i"
	       }
	  },
     Outputs => {
	  "y" => List => {"the list representing the inverse permutation of ", TT "x" }
	  },
     EXAMPLE lines ///
     x = {1,2,3,4,5,0}
     y = inversePermutation x
     all(#x, i -> x#(y#i) == i)
     all(#x, i -> y#(x#i) == i)
     ///,
     PARA {
	  "We compose permutations with ", TT "_", "; see ", TO (symbol_, VisibleList, List), "."
	  },
     EXAMPLE lines ///
     x_x_x
     x_x_x_x_x_x
     x_y
     y_x
     ///
     }

document {
     Key => (symbol -, Vector),
     Headline => "negation of a Vector",
     TT "-v", " -- the negation of ", TT "v",
     PARA{},
     EXAMPLE lines ///
       v = vector {2,3,5,7}
	   - v
     ///
}   


multidoc ///
Node
     Key
     	  (matrix,Vector)
     Usage
     	  matrix v
     Inputs
     	  v:
     Outputs
     	  :Vector
	   the matrix with a single column containing the vector {\tt v}
     Description
     	  Example
	       v = vector {1,2,3}
	       matrix v
Node
     Key
     	  limitFiles
     Usage
     	  limitFiles n
     Inputs
     	  n:ZZ
     Consequences
     	  Item
	       the number of open file descriptors for the current process will be limited to {\tt n}
Node
     Key
     	  limitProcesses
     Usage
     	  limitProcesses n
     Inputs
     	  n:ZZ
     Consequences
     	  Item
	       the number of simultaneous processes for the current user will be limited to {\tt n}
Node
 Key
  (fileWritable,String)
  fileWritable
 Usage
  fileWritable n
 Inputs
  n:
 Outputs
  :
   whether the file whose name is given by the string {\tt n} is writable
 Description
  Example
   fileWritable "."
 SeeAlso
  fileReadable
  fileExecutable
Node
 Key
  (fileReadable,String)
  fileReadable
 Usage
  fileReadable n
 Inputs
  n:
 Outputs
  :
   whether the file whose name is given by the string {\tt n} is readable
 Description
  Example
   fileReadable "."
 SeeAlso
  fileWritable
  fileExecutable
Node
 Key
  (fileExecutable,String)
  fileExecutable
 Usage
  fileExecutable n
 Inputs
  n:
 Outputs
  :
   whether the file whose name is given by the string {\tt n} is executable
 Description
  Example
   fileExecutable "."
 SeeAlso
  fileWritable
  fileReadable
///

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
