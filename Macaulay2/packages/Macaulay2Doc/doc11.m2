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
     TT "code f", " -- prints out the source code of the function or command", TT "f", ".",
     BR{},
     TT "code(f,X)", " -- prints out the source code of the particular 
     method that would be applied if ", TT "f", " were applied to an argument of 
     class ", TT "X", ".",
     BR{},
     TT "code(f,X,Y)", " -- prints out the source code of the particular 
     method that would be applied if ", TT "f", " were applied to arguments of
     classes ", TT "X", " and ", TT "Y", ".",
     BR{},
     TT "code(f,X,Y,Z)", " -- prints out the source code of the 
     particular method that would be applied if ", TT "f", " were applied to 
     arguments of classes ", TT "X", ", ", TT "Y", ", and ", TT "Z", ".",
     BR{},
     TT "code {v,w,...}", " -- prints out the source code for each
     of the items listed.",
     PARA{},
     EXAMPLE "code methods use",
     SeeAlso => "methods"
     }

document {
     Key => edit,
     Headline => "edit source code",
     TT "edit", " -- a command which starts the text editor",
     BR{},
     TT "edit f", " -- starts the text editor at the source code of the
     function ", TT "f", ".",
     BR{},
     TT "edit(f,X)", " -- edits the source code of the particular 
     method that would be applied if ", TT "f", " were applied to an argument of 
     class ", TT "X", ".",
     BR{},
     TT "edit(f,X,Y)", " -- edits the source code of the particular 
     method that would be applied if ", TT "f", " were applied to arguments of
     classes ", TT "X", " and ", TT "Y", ".",
     BR{},
     TT "edit(f,X,Y,Z)", " -- edits the source code of the 
     particular method that would be applied if ", TT "f", " were applied to 
     arguments of classes ", TT "X", ", ", TT "Y", ", and ", TT "Z", ".",
     PARA{},
     "The name of the user's preferred editor is take from the environment 
     variable ", TT "EDITOR", ".  If X is running and the editor is not
     emacs, then the editor is started in a new ", TT "xterm", " window.",
     PARA{},
     "For an interactive example, try ", TT "edit(dim,Module)", ".",
     PARA{},
     "The value returned is the exit code returned by the editor, as with
     ", TO "run", ", usually zero."
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
     (image,RingMap),					    -- just an error message referring to coimage
     (symbol ==, Constant, RingElement),
     (symbol ==, RingElement, Constant),
     ((symbol SPACE, symbol =), Function, Thing),
     ((symbol _*, symbol =), RingFamily) 
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
