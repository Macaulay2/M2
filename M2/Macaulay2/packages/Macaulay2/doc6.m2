--		Copyright 1993-2002 by Daniel R. Grayson
document {
     Key => odd,
     Headline => "tell whether an integer is odd",
     TT "odd x", " -- returns true or false, tells whether x is an odd integer.",
     PARA,
     "See also ", TO "even", "."}
document {
     Key => even,
     Headline => "tell whether an integer is even",
     TT "even x", " -- returns true or false, tells whether x is an even integer.",
     PARA,
     "See also ", TO "odd", "."}
document {
     Key => numeric,
     Headline => "convert to floating point",
     TT "numeric x", " -- yields the expression obtained from x by converting the 
     integers and rational numbers within to double precision floating 
     point numbers.",
     PARA,
     EXAMPLE "numeric {1,2,3}",
     PARA,
     "See also ", TO "RR", "."}
document {
     Key => "pi",
     Headline => "the number 'pi'",
     TT "pi", " the numerical value of the arithmetic quantity pi.",
	EXAMPLE {
		"pi"
		}
	}
document {
     Key => Engine,
     Headline => "specify whether a ring is handled by the engine",
     TT "Engine", " -- a key for rings which yields the value ", TT "true", " if this
     ring is supported by the ", TO "engine", "."}


document {
     Key => coefficientRing,
     Headline => "get the coefficient ring",
     TT "coefficientRing R", " -- yields the coefficient ring of the ring ", TT "R", ".",
     PARA,
     "If ", TT "R", " is a polynomial ring, then the coefficient ring is
     the base ring from which the coefficients are drawn.  If ", TT "R", " is
     constructed from a polynomial ring as a quotient ring or a fraction ring
     or a sequence of such operations, then the original coefficient ring
     is returned.",
     EXAMPLE {
	  "coefficientRing(ZZ/101[a][b])",
      	  "ultimate(coefficientRing,ZZ/101[a][b])"
	  }}
document {
     Key => baseRings,
     Headline => "store the list of base rings of a ring",
     TT "baseRings", " -- a symbol used as a key in a ring ", TT "R", " under which is
     stored a list of base rings for ", TT "R", ".",
     PARA,
     "A base ring ", TT "A", " of ", TT "R", " is one of the rings involved in the
     construction of ", TT "R", ".  The natural ring homomorphism from ", TT "A", "
     to ", TT "R", " is implemented with ", TO "promote", ".",
     PARA,
     "The base rings are presented in chronological order."}
document {
     Key => lift,
     Headline => "lift to another ring",
     TT "lift(f,R)", " -- promotes a ring element ", TT "f", " to 
     the ring ", TT "R", ".",
     PARA,
     "The ring ", TT "R", " should be one of the base rings associated with the
     ring of ", TT "f", ".",
     SeeAlso => "baseRings"}
document {
     Key => liftable,
     Headline => "whether a ring element can be lifted to another ring",
     TT "liftable(f,R)", " -- tells whether a ring element ", TT "f", " can be
     lifted to the ring ", TT "R", ".",
     PARA,
     "The ring ", TT "R", " should be one of the base rings associated with the
     ring of ", TT "f", ".",
     EXAMPLE {
	  "R = ZZ[x]",
	  "liftable ((x-1)*(x+1)-x^2, ZZ)",
	  },
     SeeAlso => {"lift"}}
document {
     Key => promote,
     Headline => "promote to another ring",
     TT "promote(f,R)", " -- promotes a ring element ", TT "f", " to 
     the ring ", TT "R", ".",
     PARA,
     "The element ", TT "f", " should be an element of some base ring of ", TT "R", ".",
     PARA,
     "A special feature is that if ", TT "f", " is rational, and ", TT "R", " is not
     an algebra over ", TO "QQ", ", then an element of ", TT "R", " is provided
     by attempting the evident division.",
     SeeAlso => "baseRings"}
document {
     Key => RingElement,
     Headline => "the class of all ring elements handled by the engine",
     SeeAlso => "engine"}
document {
     Key => EngineRing,
     Headline => "the class of rings handled by the engine",
     "The ", TO "engine", " handles most of the types of rings in the
     system.",
     PARA,
     "The command ", TT "new Engine from x", " is not meant for general 
     users, and provides the developers with a way to create top-level 
     rings corresponding to rings implemented in the engine.  Here ", TT "x", "
     may be:",
     UL {
	  "commands for the engine, as a string, or a sequence or list
	  of strings, which cause a ring to be placed on the top of the
	  engine's stack.",
	  "a ring, in which case another top-level ring is formed as
	  an interface to the same underlying engine ring.",
	  "the handle of on engine ring"
	  }}
document {
     Key => fraction,
     TT "fraction(f,g)", " -- manufactures the fraction ", TT "f/g", " in the fraction
     field of the ring containing ", TT "f", " and ", TT "g", " without reducing
     it to lowest terms."}
TEST ///
if getenv "USER" == "dan" then exit 0
frac(QQ[a,b])
assert ( a == denominator(b/a) )
assert ( b == numerator(b/a) )
assert ( 1 == numerator(b/b) )
///
document {
     Key => FractionField,
     Headline => "the class of all fraction fields",
     "Note: there is no way to reduce an element of an arbitrary
     fraction field to a normal form.  In other words, fractions
     may be equal without displaying the same numerator and denominator."}
document {
     Key => ZZ,
     Headline => "the class of all integers" }
document {
     Key => numerator,
     Headline => "numerator of a fraction",
     TT "numerator x", " -- provides the numerator of a fraction.",
     PARA,
     EXAMPLE "numerator (4/6)"}
document {
     Key => denominator,
     Headline => "denominator of a fraction",
     TT "denominator x", " -- provides the denominator of a fraction.",
     PARA,
     EXAMPLE "denominator (4/6)"}
document {
     Key => QQ,
     Headline => "the class of all rational numbers",
     EXAMPLE "1/2 + 3/5"}
TEST ///
     assert( net (2/1) === "2" )
     assert( net (1/1) === "1" )
///
document {
     Key => RR,
     Headline => "the class of all real numbers",
     "A real number is entered as a sequence of decimal digits with a point.",
     EXAMPLE "3.14159",
     PARA,
     SeeAlso => {"basictype"}}
document {
     Key => CC,
     Headline => "the class of all complex numbers",
     "The symbol ", TO "ii", " represents the square root of -1.",
     PARA, 
     EXAMPLE {
	  "z = 3-4*ii",
      	  "z^5",
      	  "1/z",
	  }}
document {
     Key => ii,
     Headline => "square root of -1"}
document {
     Key => realPart,
     Headline => "real part",
     TT "realPart z", " -- return the real part of a complex number z."}
document {
     Key => imaginaryPart,
     Headline => "imaginary part",
     TT "imaginaryPart z", " -- return the imaginary part of a complex number z."}
document {
     Key => conjugate,
     Headline => "complex conjugate",
     TT "conjugate z", " -- the complex conjugate of the complex number z."}
document {
     Key => gcdCoefficients,
     Headline => "gcd with coefficients",
     TT "gcdCoefficients(a,b)", " -- returns ", TT "{r,s}", " so that
     ", TT"a*r + b*s", " is the greatest common divisor of ", TT "a", "
     and ", TT "b", ".",
     PARA,
     "Works for integers or elements of polynomial rings.",
     SeeAlso => "gcd"}
document {
     Key => mod,
     Headline => "reduce modulo an integer",
     TT "mod(i,n)", " -- reduce the integer ", TT "i", " modulo ", TT "n", ".",
     PARA,
     "The result is an element of ", TT "ZZ/n", "."}
document {
     Key => OrderedMonoid,
     Headline => "the class of all ordered monoids",
     "An ordered monoid is a multiplicative monoid together with an ordering of 
     its elements.  The ordering is required to be compatible with the 
     multiplication in the sense that if x < y then x z < y z.  The class
     of all ordered monomials is ", TO "OrderedMonoid", ".",
     PARA,
     "The reason for making a separate class for ordered monoids is that monoid
     rings can be implemented more efficiently for them - an element of the 
     monoid ring can be stored as a sorted list, each element of which is
     a pair consisting of an element of the monoid and a coefficient.
     See ", TO "PolynomialRing", ".",
     PARA,
     "A free commutative ordered monoid can be created with ", TO "monoid", ".",
     SeeAlso =>  {"Monoid"}}
document {
     Key => PolynomialRing,
     Headline => "the class of all ordered monoid rings",
     "Every element of a polynomial ring is also a ", TO "RingElement", ".",
     SeeAlso => "polynomial rings"}
document {
     Key => exponents,
     Headline => "list the exponents in a polynomial",
     TT "exponents m", " -- for a monomial ", TT "m", " provides the list
     of exponents.",
     BR, NOINDENT,
     TT "exponents f", " -- for a polynomial ", TT "f", " provides a list
     whose elements are the lists of exponents of the terms of ", TT "f", ".",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[x,y,z];",
      	  "exponents (f = x^2 - 7 + x*y*z^11 + y)",
	  "leadMonomial f",
	  "exponents leadMonomial f"
	  }}
document {
     Key => standardForm,
     Headline => "convert to standard form",
     TT "standardForm f", " -- converts a polynomial or monomial to a
     form involving hash tables.",
     PARA,
     "A polynomial is represented by hash tables in which the keys are
     hash tables representing the monomials and the values are the 
     coefficients.",
     PARA,
     "The monomials themselves are represented by hash tables 
     in which the keys are the variables and the values are the 
     corresponding exponents.",
     EXAMPLE {
	  "R = ZZ/101[x,y,z];",
      	  "standardForm (x^2 - 7 + x*y*z^11 + y)"
	  }}
document {
     Key => listForm,
     Headline => "convert to list form",
     TT "listForm f", " -- converts a polynomial or monomial to a form
     represented by nested lists.",
     PARA,
     "A monomial is represented by the list of its exponents.",
     PARA,
     "A polynomial is represented by lists of pairs (m,c), one for each
     term, where m is a list of exponents for monomial, and c is the
     coefficient.",
     EXAMPLE {
	  "R = ZZ/101[x,y,z];",
      	  "listForm (x^2 - 7 + x*y*z^11 + y)"
	  }}
document {
     Key => WeylAlgebra,
     Headline => "make a Weyl algebra",
     TT "WeylAlgebra", " -- an option used when creating a polynomial ring
     to specify that a Weyl algebra is to be produced.",
     PARA,
     "A Weyl algebra is an algebra in which some of the variables behave
     as derivatives with respect to the other variables.",
     PARA,
     EXAMPLE "R = ZZ/101[x,dx,y,dy,WeylAlgebra => {x=>dx, y=>dy}];",
     "The list ", TT "{x=>dx, y=>dy}", " indicates that the variable ", TT "dx", "
     is to play the role of the derivative with respect to ", TT "x", ", and
     that ", TT "y", " is to play the role of the derivative with respect
     to ", TT "y", ".",
     EXAMPLE {
	  "dx*x",
      	  "dx*x^10",
      	  "dx*y^10"
	  }}
document {
     Key => (symbol _, RingElement, RingElement),
     Headline => "get a coefficient",
     TT "f_m", " -- provide the coefficient of the monomial m in the polynomial f.",
     PARA,
     EXAMPLE {
	  "ZZ[y];",
      	  "((1+y)^5) _ (y^2)",
	  },
     SeeAlso => {"_"}}
document {
     Key => (symbol _, Ring, String),
     Headline => "get a variable by name",
     TT "R_\"x\"", " -- produce the variable of the polynomial ring R 
     whose name is ", TT "x", ".",
     PARA,
     EXAMPLE {
	  "R = ZZ[x,y,z];",
      	  ///R_"x"///,
	  },
     PARA,
     "Eventually we will implement this for monoids, too."}
document {
     Key => (symbol _, Ring, ZZ),
     Headline => "get a variable by number",
     TT "R_i", " -- produce the ", TT "i", "-th generator of a ring ", TT "R", ".",
     PARA,
     "The indexing of generators is based on 0, so ", TT "R_0", " would be
     the first one, and so on.",
     PARA,
     EXAMPLE {
	  "R = ZZ[a..d]",
      	  "R_2"
	  }}
document {
     Key => (symbol _, Ring, List),
     Headline => "make a monomial from a list of exponents",
     TT "R_w", " -- produce the monomial of the ring ", TT "R", " by using the 
     integers in the list ", TT "w", " as exponents of the variables.",
     PARA,
     EXAMPLE {
	  "R = ZZ[a..d]",
      	  "R_{1,2,3,4}"
	  }}
TEST "
-- test name
R = ZZ/101[a..e]
f = symmetricPower(2,vars R)
assert( f == value toExternalString f )
assert( f == value toString f )
"
document {
     Key => assign,
     Headline => "assign a value",
     TT "assign(x,v)", " -- assigns v as the value of x.",
     PARA,
     "If the value of x is a symbol or indexed variable, then it
     can be assigned the value v with ",
     PRE "          assign(x,v)",
     "When the value of x is an indexed variable y_i then what happens
     is that the i-th element of the list y is replaced by v.",
     PARA,
     "Differs from x=v in that here x is evaluated.",
     PARA,
     "Note: it would be better if we could arrange for ",
     PRE "          x <- v",
     "to work with indexed variables.  See ", TO "<-", "."}
document {
     Key => IndexedVariable,
     Headline => "the class of all indexed variables",
     "Indexed variables provide the possibility of producing 
     polynomial rings ", TT "R[x_0, x_1, ..., x_(n-1)]", " in n variables,
     where n is not known in advance.  If ", TT "x", " is an symbol,
     and i is an integer, then ", TT "x_i", " produces an indexed variable.
     After this has been done, an assignment ", TT "x_i=v", " will assign another
     value to it.  A new sequence of indexed variables of
     length n assigned to the symbol ", TT "x", " can be produced with ",
     TT "x_1 .. x_n", " and that sequence can be used in constructing
     a polynomial ring.",
     EXAMPLE {
	  "ZZ/101[t_0 .. t_4]",
      	  "(t_0 -  2*t_1)^3",
	  }}
document {
     Key => MonoidElement,
     Headline => "the class of all monoid elements",
     SeeAlso => "monoid"}
document {
     Key => Degrees,
     Headline => "specify the degrees",
     TT "Degrees", " -- an option which specifies the degrees of the generators.",
     PARA,
     "Used as an option to ", TO "monoid", ", or when a polynomial ring
     is created.",
     PARA,
     "See ", TO "monoid", " for details."}
document {
     Key => SkewCommutative,
     Headline => "make a skewcommutative (alternating) ring",
     TT "SkewCommutative", " -- name for an optional argument for monoids
     that specifies that monoid rings created from them will be skewcommutative.",
     PARA,
     "The default value is false.",
     EXAMPLE {
	  "R = ZZ[x,y,SkewCommutative=>true]",
      	  "x*y",
      	  "y*x"
	  }}
document {
     Key => MonomialSize,
     Headline => "specify maximum exponent size",
     TT "MonomialSize => n", " -- an option which determines the maximum 
     exponent size.",
     PARA,
     "Used as an option to ", TO "monoid", ", or when a polynomial ring
     is created.  Setting 'MonomialSize=>n' specifies that monomial exponents 
     may be as large as 2^(n-1) - 1.  
     The default value is 8, allowing for exponents up to 127.  Currently
     the maximum value is 16, allowing for exponents up to 32767.",
     PARA,
     "See ", TO "monoid", " for details."}
document {
     Key => Inverses,
     Headline => "specify whether generators are invertible",
     TT "Inverses", " -- an option used in creating a monoid which tells
     whether negative exponents will be allowed, making the monoid into
     a group.",
     SeeAlso => "monoid"}
document {
     Key => GeneralOrderedMonoid,
     Headline => "the class of all ordered free commutative monoids",
     "This is the class of free monoids that can be handled by 
     the ", TO "engine", ".  Elements of such monoids are implemented
     as instances of ", TO "MonoidElement", ".",
     PARA,
     SeeAlso => { "monoid" }
     }     
document {
     Key => (symbol _, Monoid, ZZ),
     Headline => "get a generator of a monoid",
     TT "M_i", " -- produces the i-th generator of a monoid ", TT "M", ".",
     PARA,
     SeeAlso => { "Monoid", "_" }}
document {
     Key => degreesMonoid,
     Headline => "get the monoid of degrees",
     TT "degreesMonoid n", " -- returns the monoid whose elements correspond
     to the multi-degrees of monomials in another monoid.",
     PARA,
     "Also used as a key under which to store the result."}
document {
     Key => VariableBaseName,
     Headline => "base name for variables",
     TT "VariableBaseName => x", " -- an optional argument used when creating
     monoids or rings to specify that the variables should be ",
     TT "x_0, ..., x_n", "."}
document {
     Key => Variables,
     Headline => "specify the variable names",
     TT "Variables", " -- a key used with monoids to indicate the list of 
     variable names, or the number of variables.",
     PARA,
     "This option is useful for those situations when one doesn't care about the
     names of the variables in a ring or monoid, or when one is creating a 
     tensor product ring, symmetric algebra, or other ring, and one wants control
     over the names of the ring variables."}
document {
     Key => VariableOrder,
     TT "VariableOrder", " -- an option used when creating a monoid.",
     PARA,
     "Not implemented yet.",
     SeeAlso => "monoid"}
document {
     Key => (monoid, Array),
     Headline => "make a polynomial ring or monoid ring",
     TT "monoid [a,b,c,...]", " -- makes a free ordered commutative monoid on the variables listed.",
     PARA,
     "Optional arguments (placed between the brackets):",
     UL (TO \ keys value Macaulay2Core#"private dictionary"#"monoidDefaults"),
     SeeAlso => {(symbol " ", Ring, Array)}}
document {
     Key => (symbol " ", Ring, Array),
     Headline => "the standard way to make a polynomial ring",
     TT "R[...]", " -- produces the monoid ring from a ring ", TT "R", " and the
     ordered monoid specified by ", TT "[...]", ".",
     PARA,
     "This is the customary way to make a polynomial ring.",
     PARA,
     "Optional arguments (placed inside the array):",
     UL (TO \ keys value Macaulay2Core#"private dictionary"#"monoidDefaults"),
     SeeAlso => "polynomial rings"}
document {
     Key => (symbol " ",Ring, OrderedMonoid),
     Headline => "make a polynomial ring",
     TT "R M", " -- produces the monoid ring from a ring ", TT "R", " and an ordered monoid
     ", TT "M", ".",
     SeeAlso => "polynomial rings"}
document {
     Key => (monoid, Ring),
     Headline => "get the monoid from a monoid ring",
     TT "monoid R", " -- yields the underlying monoid of polynomial ring
     or monoid ring.",
     EXAMPLE {
	  "R = QQ[x,y]",
	  "monoid R"
	  }}
document {
     Key => monoid,
     Headline => "make a monoid",
     TT "monoid [a,b,c,Degrees=>{2,3,4}]", " -- makes a free ordered commutative monoid on the
	     variables listed, with degrees 2, 3, and 4, respectively.",
     PARA,
     NOINDENT,
     TT "monoid [a,b,c,Degrees=>{{1,2},{3,-3},{0,4}}]", " -- makes a free ordered
     commutative monoid on the variables listed, with multi-degrees as listed.",
     PARA,
     NOINDENT,
     TT "monoid [a,b,c,Degrees=>{{},{},{}}]", " -- makes a free ordered commutative monoid on the
	     variables listed, ungraded.",
     PARA,
     "The variables listed may be symbols or indexed variables.
     The values assigned to these variables (with ", TO "assign", ") are
     the corresponding monoid generators.  The function ", TO "baseName", "
     may be used to recover the original symbol or indexed variable.",
     PARA,
     "The class of all monoids created this way is ", TO "GeneralOrderedMonoid", ".",
     PARA,
     SeeAlso => {"OrderedMonoid","IndexedVariable","Symbol"}}
document {
     Key => (symbol **, Monoid, Monoid),
     Headline => "tensor product of monoids",
     TT "M ** N", " -- tensor product of monoids.",
     PARA,
     "For complete documentation, see ", TO "tensor", "."}
document {
     Key => tensor,
     Headline => "tensor product",
     TT "tensor(M,N)", " -- tensor product of rings or monoids.",
     PARA,
     "This method allows all of the options available for monoids, see
     ", TO "monoid", " for details.  This routine essentially combines the 
     variables of M and N into one monoid.",
     PARA,
     "For rings, the rings should be quotient rings of polynomial rings over the same
     base ring.",
     PARA,
     "Here is an example with monoids.",
     EXAMPLE {
	  "M = monoid[a..d, MonomialOrder => Eliminate 1]",
	  "N = monoid[e,f,g, Degrees => {1,2,3}]",
	  "P = tensor(M,N,MonomialOrder => GRevLex)",
	  "describe P",
	  "tensor(M,M,Variables => {t_0 .. t_7}, MonomialOrder => ProductOrder{4,4})",
	  "describe oo",
	  },
     "Here is a similar example with rings.",
     EXAMPLE "tensor(ZZ/101[x,y], ZZ/101[r,s], MonomialOrder => Eliminate 2)",
     SeeAlso => "**"}
document {
     Key => table,
     Headline => "make a table (nested list)",
     TT "table(u,v,f)", " -- yields a table m in which m_i_j is f(u_i,v_j).",
     PARA,
     "A table is a list of lists, all of the same length.  The entry m_i_j is 
     computed as f(u_i,v_j).",
     PARA,
     "table(m,n,f) -- yields, when m and n are integers, a table of size m by n
     whose entries are obtained by evaluating f() repeatedly.",
     PARA,
     "See also ", TO "isTable", ", and ", TO "subtable", "."}
document {
     Key => applyTable,
     Headline => "apply a function to elements of a table",
     TT "applyTable(m,f)", " -- applies the function f to each element of the table m.",
     PARA,
     "It yields a table of the same shape as m containing the resulting values.",
     PARA,
     "See also ", TO "table", "."}
document {
     Key => subtable,
     Headline => "extract a subtable from a table",
     TT "subtable(u,v,m)", " -- yields the subtable of the table m obtained from the
     list u of row numbers and the list v of column numbers.",
     PARA,
     EXAMPLE {
	  "m = table(5,5,identity)",
      	  "subtable({1,3,4},toList(2..4), m)"
	  }}
document {
     Key => vector,
     Headline => "make a vector",
     TT "vector {a,b,c,...}", " -- produces an element of a free module from a list.",
     PARA,
     "The elements a,b,c,... must be elements of the same ring, or be
     convertible to elements of the same ring."}
document {
     Key => relations,
     Headline => "the defining relations",
     TT "relations M", " -- produce the relations defining a module M.",
     PARA,
     "The relations are represented as a matrix, and if not stored
     in the module under M.relations, the matrix is understood to be
     empty.",
     PARA,
     SeeAlso => {"generators","subquotient"}}
document {
     Key => (symbol ==, Module, Module),
     TT "M == N", " -- test whether two modules are equal.",
     PARA,
     "Two modules are equal if they are isomorphic as subquotients of the
     same ambient free module.",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[x]",
      	  "image matrix {{2,x},{1,5}} == R^2",
      	  "image matrix {{2,x},{0,5}} == R^2"
	  }}
TEST "
R = ZZ/101[a,b,c]
M = cokernel matrix {{a,b^2,c^3}}
N = image M_{0}
assert( M == N )
"
document {
     Key => Vector, 
     Headline => "the class of all elements of free modules which are handled by the engine",
     "If ", TT "R", " is a ring handled by the engine, and ", TT "M", " is a free
     module over ", TT "R", ", then M is a subclass of Vector.",
     PARA,
     SeeAlso => {"engine", "Module"}}
document {
     Key => (symbol _, Vector, ZZ),
     Headline => "get a component",
     TT "v_i", " -- produce the i-th entry of a vector or module element v.",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[a..f]",
      	  "v = vector {a,b,c}",
      	  "v_1",
	  },
     SeeAlso => {"_"}}
document { Key => degrees, Headline => "degrees of generators" }
document {
     Key => (degrees, Ideal),
     Usage => "degrees I",
     Inputs => {"I" => null},
     Outputs => {{ "the list of multi-degrees for the generators of the module ", TT "I" }}}
document {
     Key => (degrees, Matrix),
     Usage => "degrees f",
     Inputs => {"f" => null},
     Outputs => {{ "a list ", TT "{x,y}", " where ", TT "x", " is the list
	       of degrees of the target of ", TT "f", " and ", TT "y", " is the
	       list of degrees of the source of ", TT "f", "." }}}
document {
     Key => (degrees, Module),
     Usage => "degrees M",
     Inputs => {"M" => null},
     Outputs => {{ "the list of multi-degrees for the generators of the module ", TT "M" }},
     EXAMPLE {
	  "R = ZZ/101[x,y,z]",
      	  "E = R^5",
      	  "degrees E",
      	  "F = R^{1,2,3,4}",
      	  "degrees F" } }
document {
     Key => (degrees, Ring),
     Usage => "degrees R",
     Inputs => {"R" => null},
     Outputs => {},
     Consequences => {{ "the list of multi-degrees for the generators (variables) of the ring ", TT "R"}},
     EXAMPLE {
	  "R = ZZ/101[x,y,z]",
      	  "degrees R",
	  "S = ZZ/101[x,y,z,Degrees => {{2,3},{1,2},{2,0}}]",
      	  "degrees S" } }
document {
     Key => (symbol ^, Ring, List),
     Headline => "make a free module",
     Usage => "M = R^{i,j,k,...}",
     Inputs => {"R" => null,"{i,j,k, ...}" => {"a list of integers or a list of lists of integers"}},
     Outputs => {{"a free module over ", TT "R", " whose generators have degrees ", TT "-i", ", ", TT "-j", ", ", TT "-k", ", ..."}},
     "If ", TT "i", ", ", TT "j", ", ... are lists of integers, then
     they represent multi-degrees, as in ", TO "multi-graded polynomial rings", ".",
     SeeAlso => {"degrees", "free modules"}}
document {
     Key => components,
     Headline => "list the components of a direct sum",
     TT "components x", " -- produces a list of the components of an element of a 
     free module.",
     BR,NOINDENT,
     TT "components M", " -- the list of components for a module ", TT "M", " which was
     formed as a direct sum, or ", TT "{M}", " if ", TT "M", " was not formed as a 
     direct sum.  Works also for homomorphism, chain complexes, and graded modules.",
     SeeAlso => {"vector", "directSum", "++"}}
document {
     Key => (symbol ^,Module,ZZ),
     Headline => "make a direct sum of several copies of a module",
     Usage => "M^n",
     Inputs => {"M" => {"a module"}, "n" => null},
     Outputs => {{"the direct sum of ", TT "n", " copies of ", TT "M"}}}
document {
     Key => (symbol ^,Ring,ZZ),
     Headline => "make a free module",
     Usage => "R^n",
     Inputs => {"R" => {"a ring"}, "n" => null},
     Outputs => {{"a new free ", TT "R", "-module of rank ", TT "n", "." }},
     "The new free module has basis elements of degree zero.  To specify the
     degrees explicitly, see ", TO (symbol ^,Ring,List), "." }
document {
     Key => rank,
     Headline => "compute the rank",
     TT "rank M", " -- computes the rank of the module M.",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[x,y,z]",
      	  "p = vars R;",
      	  "rank kernel p",
      	  "rank cokernel p"
	  }}
document {
     Key => coverMap,
     Headline => "get the map to the module given by the generators of a module",
     Usage => "coverMap M",
     Inputs => {"M" => null},
     Outputs => {{ "the map from a free module to ", TT "M", " given by the generators of ", TT "M"}},
     SeeAlso => { "cover" }}
document {
     Key => cover,
     Headline => "get the covering free module",
     TT "cover M", " -- yields the free module whose basis elements correspond
     to the generators of M.",
     SeeAlso => {"ambient", "super"}}
document {
     Key => (cover,Module),
     Usage => "F = cover M",
     Inputs => {"M" => null},
     Outputs => {"F" => {"the free module whose basis elements correspond to the generators of ", TT "M", "."}},
     "The free module ", TT "F", " is the source of the generator matrix 
     of ", TT "M", ".",
     EXAMPLE {
	  "R = QQ[a..f];",
	  "g = matrix{{a,b},{c,d},{e,f}}",
	  "M = subquotient(g,matrix{{b},{c},{d}})",
	  "cover M",
	  "cover M == source generators M"},
     SeeAlso => {(ambient,Module), (super,Module)}}
document {
     Key => super,
     Headline => "get the ambient module",
     TT "super M", " -- yields the module which the module ", TT "M", " is a submodule of.",
     BR, NOINDENT,
     TT "super f", " -- if ", TT "f", " is a map whose target is a submodule 
     of ", TT "M", ", yields the composite of ", TT "f", " with the inclusion into ", TT "M", ".",
     PARA,
     SeeAlso => { "cover", "ambient" }}
document {
     Key => End,
     Headline => "module of endomorphisms",
     TT "End M", " -- constructs the module of endomorphisms of ", TT "M", "."}
document {
     Key => ModuleMap,
     Headline => "the class of all maps between modules",
     "This class is experimental, designed to support graded modules.",
     SeeAlso => {"Matrix"}}
document {
     Key => (symbol *, Matrix, Matrix),
     Headline => "matrix multiplication",
     "Multiplication of matrices corresponds to composition of maps, and when
     ", TT "f", " and ", TT "g", " are maps so that the target ", TT "Q", "
     of ", TT "g", " equals the source ", TT "P", " of ", TT "f", ", the
     product ", TT "f*g", " is defined, its source is the source of ", 
     TT "g", ", and its target is the target of ", TT "f", ".  The degree of ",
     TT "f*g", " is the sum of the degrees of ", TT "f", " and of ", TT "g",
     ".  The product is also defined when ", TT "P", " != ", TT "Q", ",
     provided only that ", TT "P", " and ", TT "Q", " are free modules of the
     same rank.  If the degrees of ", TT "P", " differ from the corresponding
     degrees of ", TT "Q", " by the same degree ", TT "d", ", then the degree
     of ", TT "f*g", " is adjusted by ", TT "d", " so it will have a good
     chance to be homogeneous, and the target and source of ", TT "f*g", "
     are as before."}
     
document {
     Key => Matrix,
     Headline => "the class of all matrices",
     "A matrix is a homomorphism between two modules, together with
     an integer (or vector of integers) called its degree, which is
     used when determining whether the map is homogeneous.  The matrix
     is stored in the usual way as a rectangular array of ring elements.
     When the source or target modules are not free, the matrix is
     interpreted as a linear transformation in terms of the generators
     of the modules.",
     SeeAlso => "matrices",
     PARA,
     "A matrix ", TT "f", " is an immutable object, so if you want to 
     cache information about it, put it in the hash table ", TT "f.cache", ".",
     PARA,
     "Common ways to make a matrix:",
     UL {
	  TO "map",
	  TO "matrix",
	  },
     "Common ways to get information about matrices:",
     UL {
	  TO (degree, Matrix),
	  TO (isHomogeneous, Matrix),
	  TO (matrix, Matrix),
	  },
     "Common operations on matrices:",
     UL {
	  TO (symbol +, Matrix, Matrix),
	  TO (symbol -, Matrix, Matrix),
	  TO (symbol *, RingElement, Matrix),
	  TO (symbol *, Matrix, Matrix),
	  TO (symbol ==, Matrix, Matrix),
	  TO (symbol ++, Matrix, Matrix),
	  TO (symbol **, Matrix, Matrix),
	  TO (symbol %, Matrix, Matrix),
	  TO (symbol //, Matrix, Matrix),
	  TO (symbol |, Matrix, Matrix),
	  TO (symbol ||, Matrix, Matrix),
	  TO (symbol ^, Matrix, List),
	  TO (symbol _, Matrix, List)
	  },
     "Common ways to use a matrix:",
     UL {
	  TO (cokernel, Matrix),
	  TO (image, Matrix),
	  TO (kernel, Matrix),
	  TO (homology, Matrix, Matrix),
	  }}
document {
     Key => getMatrix,
     Headline => "get a matrix from the engine's stack",
     TT "getMatrix R", " -- pops a matrix over ", TT "R", " from the top of 
     the engine's stack and returns it.",
     PARA,
     "Intended for internal use only."}
document {
     Key => (symbol _, Matrix, Sequence),
     Headline => "get an entry",
     TT "f_(i,j)", " -- provide the element in row ", TT "i", " and
     column ", TT "j", " of the matrix ", TT "f", ".",
     SeeAlso => {"_", "Matrix"}}
document {
     Key => (symbol _, Matrix, ZZ),
     Headline => "get a column from a matrix",
     TT "f_i", " -- provide the ", TT "i", "-th column of a matrix ", TT "f", " as a vector.",
     PARA,
     "Vectors are disparaged, so we may do away with this function in the future.",
     SeeAlso => "_"}
document {
     Key => isWellDefined,
     Headline => "whether a map is well defined" }
document {
     Key => isDirectSum,
     Headline => "whether something is a direct sum",
     "Works for modules, graded modules, etc.  The components of the sum
     can be recovered with ", TO "components", "."}
TEST "
assert isDirectSum (QQ^1 ++ QQ^2)
assert isDirectSum (QQ^1 ++ QQ^2)
"
document {
     Key => youngest,
     Headline => "the youngest member of a sequence",
     TT "youngest s", " -- return the youngest mutable hash table in the sequence
     ", TT "s", ", if any, else ", TO "null", "."}
document {
     Key => (symbol ++,Module,Module),
     Headline => "direct sum of modules",
     TT "M++N", " -- computes the direct sum of two modules.",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[a..c];",
      	  "image vars R ++ kernel vars R",
	  },
     "Projection and inclusion maps for direct sums:",
     UL {
	  TO (symbol ^,Module,Array),
	  TO (symbol _,Module,Array)
	  },
     SeeAlso => directSum}
document {
     Key => (symbol ++,Matrix,Matrix),
     Headline => "direct sum of maps",
     TT "f++g", " -- computes the direct sum of two maps between modules.",
     PARA,
     "If an argument is a ring element or integer, it is promoted
     to a one by one matrix.",
     EXAMPLE {
	  "R = ZZ/101[a..c];",
      	  "vars R ++ transpose vars R",
      	  "oo^[1]",
      	  "a++b++c",
	  },
     "Selecting rows or columns of blocks:",
     UL {
	  TO (symbol ^,Matrix,Array),
	  TO (symbol _,Matrix,Array)
	  },
     SeeAlso => {directSum, (symbol |, Matrix, Matrix), (symbol ||, Matrix, Matrix)}}
document {
     Key => directSum,
     Headline => "direct sum of modules or maps",
     TT "directSum(M,N,...)", " -- forms the direct sum of matrices or modules.",
     PARA,
     "The components can be recovered later with ", TO "components", ".",
     PARA,
     "Projection and inclusion maps for direct sums:",
     UL {
	  TO (symbol ^,Module,Array),
	  TO (symbol _,Module,Array),
	  TO (symbol ^,Matrix,List),
	  TO (symbol _,Matrix,List)
	  },
     PARA,
     "It sometimes happens that the user has indices for the components of
     a direct sum preferable to the usual consecutive small integers.  In 
     this case the preferred indices can be specified with code
     like ", TT "directSum(a=>M,b=>N,...)", ", as in the following example.",
     EXAMPLE {
	  ///F = directSum(a=>ZZ^1, b=>ZZ^2, c=>ZZ^3)///,
	  ///F_[b]///,
	  ///F^[c]///,
	  },
     "Similar syntax works with ", TO "++", ".",
     EXAMPLE {
	  ///F = (a => ZZ^1) ++ (b => ZZ^2)///,
	  ///F_[b]///,
	  },
     SeeAlso => {"++", "components", "indexComponents", "indices"}}
document {
     Key => indexComponents,
     Headline => "specify keys for components of a direct sum",
     TT "indexComponents", " -- a symbol used as a key in a direct sum
     under which to store a hash table in which to register preferred keys used
     to index the components of the direct sum.",
     PARA,
     SeeAlso => {"directSum", "components", "indices"}}
-- MES: "indices" moved to functions/indices-doc.m2

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
