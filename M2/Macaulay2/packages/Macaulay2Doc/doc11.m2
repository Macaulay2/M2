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

document {
     Key => html,
     Headline => "convert to html format",
	Usage => "html x",
	Inputs => {"x" => {}},
	Outputs => {String => {}},
     TT "html x", " converts ", TT "x", " to HTML format",
     PARA{
     "The return value is a string that is suitable for use in an
     html file, readable by a world wide web client. ",
     TO "hypertext", " elements are translated into the corresponding HTML elements. ",
     "When no HTML conversion is available, ", TO "tex", " is called.
     (La)TeX can be rendered in the browser using MathJax or KaTeX.
     "},
     SeeAlso => "mathML"
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
     Key => {(runLengthEncode,VisibleList),runLengthEncode},
     Headline => "run length encoding",
     Usage => "runLengthEncode x",
     Inputs => { "x" },
     Outputs => {{ "a list equivalent to ", TT "x", ", in which runs and sequences have been expressed 
	       symbolically as ", TO2{Expression,"expressions"}}},
     PARA {"The result is useful in printed displays, as a way of making them more compact.  The original list can
	  be recovered by applying ", TO "value", " to the elements of the result, and then using ", TO "deepSplice", ",
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

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
