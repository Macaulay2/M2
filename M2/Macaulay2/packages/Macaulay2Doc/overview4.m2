-- -*- coding: utf-8 -*-
--		Copyright 1993-2002 by Daniel R. Grayson


undocumented methods hypertext

document {
     Key => hypertext,
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

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
