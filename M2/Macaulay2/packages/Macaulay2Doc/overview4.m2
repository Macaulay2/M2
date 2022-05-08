-- -*- coding: utf-8 -*-
--		Copyright 1993-2002 by Daniel R. Grayson


undocumented (hypertext, Hypertext)

document {
     Key => {hypertext,(hypertext, List),(hypertext, Sequence)},
     Headline => "prepare hypertext for display",
     Usage => "hypertext x",
     Inputs => {
	  "x" => List => {"a list of strings and hypertext mark-up lists"}
	  },
     Outputs => {
	  Hypertext => {"a new list of mark-up lists obtained from the old by making the format more suitable for
	       ultimate display"}
	  },
     "Here is a list of some of the transformations that are performed.",
     UL {
	  {"occurrences of ", TT "null", ", such as those that might have been produced by the insertion of an extra comma, are removed"},
	  {TO2 {Sequence,"sequences"}, ", such as those produced by extra sets of parentheses, are ", TO2{ "splice","spliced"}, " into the lists containing them"},
	  {"the contents of lists and sequences are merged into the mark-up lists containing them" },
	  {"mark-up lists of type ", TO "TO", " occurring a mark-up list of type ", TO "UL", " are converted to lists of type ", TO "TOH", ", so the headlines of 
	       the items will appear in the resulting menu"},
	  {"the targets of the links in mark-up lists of type TO, TOH, and TO2, are converted into ", TO2 {"DocumentTag", "document tags"}},
	  {"mark-up types, such as ", TO "PARA", ", are converted into lists of length 0 of that type"},
	  {"strings spanning multiple lines are wrapped into one long line after appropriately trimming the spaces at the beginning and end of each line"},
	  {"an error message is produced if something not recognizable as documentation is encountered"},
	  },
     "We may phase out this function in favor of performing the listed transformations automatically when hypertext elements are created.",
     SeeAlso => {"Hypertext"}
     }

document {
     Key => "w3",
     "You may download the package ", TT "w3", ", by William M. Perry, from 
     ", TT "http://www.cs.indiana.edu/elisp/w3/docs.html", ".  It is an
     emacs package that implements a web browser that displays web pages
     within emacs."
     }

document {
     Key => "engine", 
     "The engine is the part of the program that is dedicated to
     performing the computation of Gröbner bases with Buchberger's
     algorithm.  It is coded directly in C++ for speed.",
     PARA{},
     "The Macaulay2 engine provides fast polynomial and matrix operations,
     and Gröbner bases, syzygies, Hilbert functions, resolutions and
     other operations that we feel need to be implemented directly for
     efficiency reasons.",
     }

document {
     Key => "diff and contract",
     "We may use the function ", TO "diff", " to differentiate polynomials:
     the first argument is the variable to differentiate with respect to,
     and the second argument is the polynomial to be differentiated.",
     EXAMPLE {
	  "R = QQ[a,b,t,x,y,z];",
	  "f = x^7 * y^11;",
	  "diff(x,f)",
	  "diff(y,f)",
	  },
     "We indicate higher derivatives by simply multiplying the variables
     to differentiate by.",
     EXAMPLE {
	  "diff(x^2,f)",
	  "diff(x*y,f)",
	  "diff(y^2,f)",
     	  },
     "The first argument can also be a sum, in which case the sum of
     the answers provided by each of its terms is returned.",
     EXAMPLE {
	  "diff(x+y,f)",
	  "diff(x^2+x*y+y^2,f)",
	  },
     "Remark: the operation ", TT "diff", " is useful, but it's not a 
     natural one: it's not invariant under linear coordinate changes;
     in effect, we've identified the a free module with its dual.",
     PARA{},
     "The second argument can be a matrix, in which case each of
     its entries gets differentiated.",
     EXAMPLE {
	  "m = matrix {{x^3, x^4},{x^5,x^6}}",
	  "diff(x,m)",
	  "diff(x^2,m)",
	  },
     "The first argument can also be a matrix, in which case
     the matrices obtained from each of its entries, acting upon
     the second argument, are concatenated.  Thus the shape of
     the first matrix plays the major role.",
     EXAMPLE {
	  "diff(matrix {{x,x^2,x^3,x^4}}, m)",
	  "diff(matrix {{x,x^2},{x^3,x^4}}, m)",
	  },
     PARA{},
     "Perhaps the most common usage of ", TO "diff", " is when one argument
     has a single column and the other column has a single row.  For example,
     the Jacobian matrix can be computed as follows.",
     EXAMPLE {
	  "diff(matrix {{x},{y}}, matrix {{x^2, x*y, y^2}})",
	  },
     HR{},
     "We can also compute the Hessian matrix of a quadratic form using ", TO "diff", ",
     as follows.",
     EXAMPLE {
	  "v = matrix {{x,y}}",
	  "diff(v ** transpose v, 3*x^2 + 5*x*y + 11*y^2)"
	  },
     HR{},
     "As another example, we show how to compute the Wronskian of a
     polynomial ", TT "f", ".",
     EXAMPLE {
      	  "f = x^3 + y^3 + z^3 - t*x*y*z",
      	  "v = matrix {{x,y,z}}",
      	  "det diff(transpose v * v, f)",
	  },
     HR{},
     "The function ", TO "contract", " is the same as ", TO "diff", ",
     except the multiplication by integers that occurs during
     differentiation is omitted.",
     EXAMPLE {
	  "contract(x,m)",
	  "contract(x^2,m)",
	  "contract(matrix {{x,x^2,x^3,x^4}}, m)",
	  "contract(matrix {{x,x^2},{x^3,x^4}}, m)",
	  },
     "One use is for picking out coefficients of homogeneous polynomials.",
     EXAMPLE {
	  "f",
	  "v3 = symmetricPower(3,matrix{{x,y,z}})",
	  "contract(v3, f)",
	  },
     HR{},
     "As an example, the Sylvester resultant between homogeneous polynomials
     ", TT "f(x,y)", " and ", TT "g(x,y)", " can be found in the following way.",
     EXAMPLE {
      	  "f = a * x^3 + b * x^2 * y + y^3",
      	  "g = b * x^3 + a * x * y^2 + y^3",
	  },
     "Multiply each of these by all quadrics, obtaining a set of elements in
     degree 5.",
     EXAMPLE {
	  "n = matrix {{f,g}} ** symmetricPower(2,matrix {{x,y}})",
	  },
     "Now create the matrix of coefficients by using contract against all
     monomials of degree 5 in ", TT "x", " and ", TT "y", ", and
     compute its determinant.",
     EXAMPLE {
	  "M = contract(transpose symmetricPower(5,matrix {{x,y}}), n)",
      	  "det M",
          --
          --                5    2 3    3     2 2       3    4    3     2        2    3
          --       ideal(- a  - a b  - a b - a b  + 2a*b  - b  + a  - 3a b + 3a*b  - b )
          --   
	  },
     HR{},
     "The function ", TO "diff'", " is the same as ", TO "diff", ",
     except that the first argument is differentiated by the second;
     the shape of the first argument still plays the major role.",
     EXAMPLE {
	  "diff'(m, matrix {{x,x^2,x^3,x^4}})",
	  "diff'(m, matrix {{x,x^2},{x^3,x^4}})",
	  },
     "The function ", TO "contract'", " is the same as ", TO "contract", ",
     except that the first argument is contracted by the second;
     the shape of the first argument still plays the major role.",
     EXAMPLE {
	  "contract'(m, matrix {{x,x^2,x^3,x^4}})",
	  "contract'(m, matrix {{x,x^2},{x^3,x^4}})",
	  },
     HR{},
     "All four of these operators are engineered so that the result is
     a homogeneous matrix if the arguments are.  The operations ", TO "diff", "
     and ", TO "contract", " are essentially partially defined division operations,
     so it should come as no surprise that the source and target of
     ", TT "diff(m,n)", " are the same as those we would get from
     the tensor product ", TT "transpose m^-1 ** n", ", if 
     only ", TT "m", " were invertible.",
     SeeAlso => { (diff,Matrix,Matrix), (contract,Matrix,Matrix), "reshape", "adjoint" }
     }

document {
     Key => "computing syzygies",
     "A syzygy among the columns of a matrix is, by definition, an
     element of the kernel of the corresponding map between free modules,
     and the easiest way to compute the syzygies applying the 
     function ", TO "kernel", ".",
     EXAMPLE {
	  "R = QQ[x..z];",
	  "f = vars R",
	  "K = kernel f",
	  },
     "The answer is provided as a submodule of the source of ", TT "f", ".  The
     function ", TO "super", " can be used to produce the module that ", TT "K", " is
     a submodule of; indeed, this works for any module.",
     EXAMPLE {
	  "L = super K",
	  "L == source f",
	  },
     "The matrix whose columns are the generators of ", TT "K", ", lifted to
     the ambient free module of ", TT "L", " if necessary, can be obtained 
     with the function ", TO "generators", ", an abbreviation for which is
     ", TT "gens", ".",
     EXAMPLE {
	  "g = generators K",
	  },
     "We can check at least that the columns of ", TT "g", " are syzygies 
     of the columns of ", TT "f", " by checking that ", TT "f*g", " is zero.",
     EXAMPLE {
	  "f*g",
	  "f*g == 0",
	  },
     "Use the function ", TO "syz", " if you need detailed control over the
     extent of the computation."
     }

document {
     Key => "computing resolutions",
     "Use the function ", TO "resolution", ", often abbreviated as ", TT "res", ",
     to compute a free resolution of a module.",
     EXAMPLE {
	  "R = QQ[x..z];",
	  "M = cokernel vars R",
	  "C = res M",
	  },
     "See ", TO "chain complexes", " for further details about how to handle
     and examine the result.",
     PARA{},
     "A reference to the result is stored within the module ", TT "M", ", so that
     requesting a computation of ", TT "res M", " a second time yields the formerly
     computed result immediately.",
     PARA{},
     "If the computation is interrupted or discontinued after the skeleton 
     has been successfully computed, then the partially completed
     resolution is available as ", TT "M.cache.resolution", ", and can be
     examined with ", TO "status", ".  The computation can be continued
     with ", TT "res M", ".  Here is an example, with an alarm interrupting
     the computation several times before it's complete.  (On my machine, 
     the computation takes a total of 14 seconds.)  (Example code, such as
     the code below, is run in such a way that interrupts stop the program,
     so to prevent that, we set ", TO "handleInterrupts", " to ", TO "true", ".)",
     PARA{},
     EXAMPLE {
	  "R = ZZ/2[a..d];",
	  "M = coker random(R^4, R^{5:-3,6:-4});",
	  "handleInterrupts = true",
///(<< "-- computation started: " << endl;
 while true do try (
     alarm 1;
     time res M;
     alarm 0;
     << "-- computation complete" << endl;
     status M.cache.resolution;
     << res M << endl << endl;
     break;
     ) else (
     << "-- computation interrupted" << endl;
     status M.cache.resolution;
     << "-- continuing the computation" << endl;
     ))///
	  },
     "If the user has a chain complex in hand that is known to be a
     projective resolution of ", TT "M", ", then it can be installed
     with ", TT "M.cache.resolution = C", ".",
     PARA{},
     "There are various optional arguments associated with ", TO2(resolution, "res"), "
     which allow detailed control over the progress of the computation."
     }

document { Key => "the debugger",
     "We have a Macaulay2 source file with a pair of functions in it that
     we can use for demonstrating the debugger.  Let's load it so we can
     run the functions in it.",
     EXAMPLE "load \"Macaulay2Doc/demo1.m2\"",
     "We can see what functions were provided to us with ", TO "listUserSymbols", ".",
     EXAMPLE "listUserSymbols",
     "Let's peek at the code of the function ", TT "g", ".",
     EXAMPLE "code g",
     "We see that the function g calls a function ", TT "f", ", but ", TT "f", " is not visible to us
     (because ", TT "f", " is a local variable).  In emacs' ", EM "Macaulay2 Interaction Mode", ", pressing
     return (", TT "RET", " or ", TT "enter", ") after positioning the cursor on the output line displaying the file name and line number
     will bring up the source code in a new buffer.",
     PARA{"The first few times we use ", TT "g", ", it seems to work."},
     EXAMPLE {"g 4", "g 3"},
     "However, the following attempt results in an error, and the debugger starts up automatically.",
     EXAMPLE "g 2",
     "You may use ", TO "help", ", as instructed, to view the commands available in the debugger.
     As suggested by the help display, we can use ", TO "listLocalSymbols", " to list the local symbols and their values.",
     EXAMPLE "listLocalSymbols",
     "We see the the value of ", TT "x", " is 0, and that explains the error message about division by zero.
     The other local symbols are the ones defined in the body of the function ", TT "f", ", whose
     code can now be displayed with ", TO "code", ".",
     EXAMPLE "code f",
     "We can use ", TO "step", " with argument 0 to bypass the current expression.",
     EXAMPLE "step 0",
     "If we decide the problem is one level up, we can use ", TT "end", " or the end-of-file character
     (which often is CTRL-D) to quit this instance of the debugger.  In this case, the debugger will 
     be entered again (triggered by the same error indication that caused it to be entered originally)
     at the point inside the function ", TT "g", " from which the function ", TT "f", " was called.",
     EXAMPLE "end",
     "We can use ", TO "listLocalSymbols", " again to see the local variables of ", TT "g", ".",
     EXAMPLE "listLocalSymbols",
     "After we are done debugging, we can quit the debugger entirely and return to top level
     with ", TO "break", ".",
     EXAMPLE "break",
     "The variable ", TO "errorDepth", " can be used to control how deep inside the code the debugger should be activated.",
     SeeAlso => { "break", "end", "step", "continue", "return", "listLocalSymbols", "listUserSymbols", "code", "value", "disassemble", "errorDepth" }
     }     

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
