-- add f%g and f//g

-- overview.m2

-- A mathematical overview of Macaulay 2 functions.

document { "mathematical overview",
     "In this section we give a comprehensive overview of the main 
     functions of Macaulay 2.",
     PARA,
     MENU {
     	  (
	       "making rings",
	       MENU {
		    TO "basic rings",
		    TO "polynomial rings",
		    TO "manipulating polynomials",
		    TO "polynomial rings with other monomial orderings",
		    TO "polynomial rings with multi-degrees",
		    TO "maps between rings",
		    TO "quotient rings",
		    TO "finite fields",
		    TO "fraction fields",
		    TO "tensor products of rings",
		    TO "exterior algebras",
		    TO "symmetric algebras",
		    TO "Weyl algebras",
		    TO "associative algebras"
		    }
	       ),
	  (
	       "ideals",
	       MENU {
		    TO "making ideals",
		    TO "quotient rings",
		    TO "algebraic operations on ideals"
		    }
	       ),
	  (
	       "matrices",
	       MENU {
		    TO "making matrices",
		    TO "manipulating matrices",-- extracting entries, leading terms, columns, submatrices, etc.
		    TO "Groebner bases"
		    }
	       ),
	  (
	       "modules",
	       MENU {
		    TO "free modules",
		    TO "making modules from matrices", -- coker, ker, image, etc.
		    TO "manipulating modules",
		    TO "maps between modules",
		    TO "bases of parts of modules"
		    }
	       ),
	  (
	       "chain complexes",
	       MENU {
		    TO "free resolutions of modules",
		    TO "making chain complexes by hand",
		    TO "extracting information from chain complexes",
		    TO "manipulating chain complexes",
		    TO "maps between chain complexes"
		    }
	       )
	  }
     }

document { "basic rings",
     "The following rings are initially present in every session with
     Macaulay 2.",
     MENU {
	  (TO "ZZ", ", the ring of integers."),
	  (TO "QQ", ", the field of rational numbers."),
	  (TO "RR", ", the field of real (floating point) numbers."),
	  (TO "CC", ", the field of complex (floating point) numbers.")
	  },
     NOINDENT,
     "(The names of these rings are double letters so the corresponding symbols
     with single letters can be used as variables in rings.)  Entries of these
     rings are constructed as follows, and the usual arithmetic operations apply.",
     EXAMPLE "1234",
     EXAMPLE "123/4",
     EXAMPLE "123.4",
     EXAMPLE "123+4*ii",
     "The usual arithmetic operations are available.",
     EXAMPLE "4/5 + 2/3",
     EXAMPLE "10^20",
     EXAMPLE "3*5*7",
     EXAMPLE "5!",
     "An additional pair of division operations which produce integral quotients
     and remainders is available.",
     EXAMPLE "1234//100",
     EXAMPLE "1234%100"
     }

document { "polynomial rings",
     "A polynomial ring can be created with the usual mathematical notation.",
     EXAMPLE "ZZ[x,y,z]",
     "If you try to construct this ring again, you will get a different
     answer.  We use the strict comparisoin operator ", TO "===", "to
     demonstrate this.",
     EXAMPLE "ZZ[x,y,z]===ZZ[x,y,z]",
     "Thus it is a good idea to assign a new ring to a variable for
     future reference.",
     EXAMPLE "R = QQ[a,b,c,d,e,f]",
     "Notice that after assignment to a global variable, the ring knows its
     name, and the name is used when printing the ring.",
     EXAMPLE "R",
     "The original description of the ring can be recovered
     with ", TO "describe", ".",
     EXAMPLE "describe R",
     "Subscript notation can be used to obtain the zero element and the unit
     element of a ring.",
     EXAMPLE "0_R",
     EXAMPLE "1_R",
     "Subscript notation (the other way around) can be used to obtain the
     variables (generators) from the ring.  The first available index is 0.",
     EXAMPLE "R_0^10+R_1^3+R_2",
     "The number of variables is provided by ", TO "numgens", ".",
     EXAMPLE "numgens R",
     EXAMPLE "apply(numgens R, i -> R_i^i)",
     EXAMPLE "sum(numgens R, i -> R_i^i)",
     "The index corresponding to a given variable can be obtained
     with ", TO "index", ".",
     EXAMPLE "index a",
     EXAMPLE "index f",
     "We may construct polynomial rings over polynomial rings.",
     EXAMPLE "ZZ[a,b,c][d,e,f]",
     "When displaying an element of an iterated polynomial ring,
     parentheses may be used to organize the coefficients, which
     may themselves be polynomials (sums).",
     EXAMPLE "(a+d+1)^2",
     "Variables names may be words.",
     EXAMPLE "QQ[rho,sigma,tau]",
     EXAMPLE "(rho - sigma)^2",
     "There are various other ways to specify the variables to be used in a polynomial
     ring.  A sequence of variables can be obtained as follows.",
     EXAMPLE "ZZ[b..k]",
     "The single-letter variables can be obtained with ", TO "vars", ".",
     EXAMPLE "vars (0..4)",
     EXAMPLE "ZZ[vars (0..4),vars(26..30),vars 51]",
     "Subscripted variables can be used.",
     EXAMPLE "ZZ[t,p_0,p_1,q_0,q_1]",
     "Sequences of subscripted variables can be obtained.",
     EXAMPLE "ZZ[t,p_0..p_5,q_0..q_5]",
     EXAMPLE "ZZ[p_(0,0) .. p_(2,1)]",
     "The subscripts can be much more general, but care is required when using
     symbols as subscripts, for the symbols may acquire values later which would
     interfere with your original use of them as symbols.  Thus you should
     protect symbols which will be used in this way.",
     EXAMPLE "protect xx; protect yy; protect zz;",
     EXAMPLE "ZZ[ee_[xx],ee_[yy],ee_[zz]]"
     }

document { "polynomial rings with other monomial orderings",
     "We can make polynomial rings with various other orderings of the
     monomials used in storing and displaying the polynomials.  The
     choice of ordering can make a difference in the time taken in
     various computations.",
     PARA,
     "The material in this section will be completely redone soon.",
     PARA,
     "The default is to use the graded reverse lexicographic ordering 
     of monomials.
     This means that terms of higher total degree come first;
     and for two terms of the same degree, the term with the higher
     power of the last variable comes last; for terms with the same 
     power of the last variable, the exponent on the next to last 
     variable is consulted, and so on.",
     EXAMPLE "R=ZZ/101[a,b,c]",
     EXAMPLE "(a+b+c+1)^2",
     "Explicit comparison of monomials with respect to the chosen
     ordering is possible.",
     EXAMPLE "b^2 > a*c",
     "The comparison operator ", TO "?", " returns a symbol indicating
     the result of the comparison: the convention is that the larger
     monomials are printed first (leftmost).",
     EXAMPLE "b^2 ? a*c",
     "The monomial ordering is also used when sorting lists with ", TO "sort", ".",
     EXAMPLE "sort {1_R, a, a^2, b, b^2, a*b, a^3, b^3}",
     "The next ring uses the reverse lexicographic ordering.  This means
     that the term with 
     the higher power of the last variable comes last; for terms with the same 
     power of the last variable, the exponent on the next to last variable 
     is consulted, and so on.  Under this ordering the monomials are not
     well ordered.",
     EXAMPLE "R=ZZ/101[x,y,z,MonomialOrder=>RevLex]",
     "We currently get a monomial overflow if we try to compute anything
     in this ring, sigh.",
     -- EXAMPLE "(x+y+z+1)^2",
     PARA,
     "The next ring uses graded lexicographic ordering.  This means that
     terms of higher total degree come first; for two terms of the
     same degree, the term with the higher power of the first variable comes
     first: for terms with the same power of the first variable the
     power of the second variable is consulted, and so on.",
     EXAMPLE "R=ZZ/101[a,b,c,MonomialOrder=>GLex]",
     EXAMPLE "(a+b+c+1)^2",
     NOINDENT,
     "(Notice how similar the result above is to the one obtained when
     graded reverse lexicographic ordering is used.)",
     PARA,
     "The next ring uses lexicographic ordering.  This means that 
     terms with the highest power of the first variable come
     first: for two terms with the same power of the first variable the
     power of the second variable is consulted, and so on.",
     EXAMPLE "R=ZZ/101[a,b,c,MonomialOrder=>Lex]",
     EXAMPLE "(a+b+c+1)^2",
     "The next ring uses an elimination order suitable for eliminating
     the first two variables, ", TT "a", " and ", TT "b", ".  In such an 
     ordering we want all terms in which either of the first two
     variables appears to come before all of those terms in which
     the first two variables don't appear.  This particular ordering
     accomplishes this by consulting first the graded reverse lexicographic
     ordering ignoring all variables but the first two, and in case of
     a tie, consulting the graded reverse lexicographic ordering of the
     entire monomials.",
     EXAMPLE "R=ZZ/101[a,b,c,MonomialOrder=>Eliminate 2]",
     EXAMPLE "(a+b+c+1)^2",
     "The next ring uses the product ordering which segregates the
     first variable from the next two.  This means that terms come
     first which would come first in the graded reverse lexicographic
     ordering when their parts involving the
     second two variables are ignored, and in case of equality,
     the graded reverse lexicographic ordering of their parts involving
     just the next two variables is consulted.",
     EXAMPLE "R=ZZ/101[a,b,c,MonomialOrder=>ProductOrder{1,2}]",
     EXAMPLE "(a+b+c+1)^2"
     }

document { "manipulating polynomials",
     "Let's set up some polynomials.",
     EXAMPLE "R = ZZ/10007[a,b]",
     EXAMPLE "f = (2*a+3)^4 + 5",
     EXAMPLE "g = (2*a+b+1)^3",
     "The number of terms in a polynomial is obtained with ", TO "size", ".",
     EXAMPLE "size f", 
     EXAMPLE "size g", 
     "The degree of a polynomial is obtained with ", TO "degree", ".",
     EXAMPLE "degree f", 
     EXAMPLE "degree g",
     NOINDENT,
     "(Notice that the degree is a list containing one integer, rather than
     an integer.  The degree is actually a vector of integers, represented as
     a list, with one component by default.)",
     PARA,
     "A string representing the polynomial, suitable for entry into other programs,
     can be obtained with ", TO "name", ".",
     EXAMPLE "name f",
     EXAMPLE "name g",
     PARA,
     "The usual algebraic operations on polynomials are available, but there
     are some special remarks to make about division.  The result of division
     depends on the ordering of monomials chosen when the ring is created, for
     division of ", TT "f", " by ", TT "g", " proceeds by locating monomials in
     ", TT "f", " divisible by the leading monomial of ", TT "g", ", and
     substituting for it the negation of the rest of ", TT "g", ".  The quotient 
     is provided by the expression ", TT "f//g", ", and the remainder is obtained 
     with ", TT "f%g", ".",
     EXAMPLE "quot = f//g",
     EXAMPLE "rem = f%g",
     EXAMPLE "f == quot * g + rem",
     NOINDENT,
     "(Notice that comparison of polynomials is done with the operator ", TO "==", ".)",
     PARA,
     "Polynomials can be homogenized with respect to one of the variables in the
     ring with ", TO "homogenize", ".",
     EXAMPLE "homogenize(f,b)",
     PARA,
     "Polynomials can be factored with ", TO "factor", ".",
     EXAMPLE "S = factor f",
     EXAMPLE "T = factor g",
     PARA,
     "The results above are represented as products of powers.  (Exponents equal
     to 1 don't appear in the display.)  We can see the internal structure
     to a specified depth (in this case, 2) with ", TO "peek", ".",
     EXAMPLE "peek(S,2)",
     EXAMPLE "peek(T,2)",
     "The components of the expressions above (Products and Powers) are types of
     lists, and the parts can be extracted with ", TO "#", ".",
     EXAMPLE "T#0",
     EXAMPLE "T#0#0",
     EXAMPLE "T#0#1",
     "The ring containing a ring element can be obtained with ", TO "ring", ".",
     EXAMPLE "ring f",
     "You can use this in a program to check whether two ring elements 
     come from the same ring.",
     EXAMPLE "ring f === ring g",
     "The coefficient of a monomial in a polynomial can be obtained with ", TO "_", ".",
     EXAMPLE "f_1",
     EXAMPLE "f_a",
     EXAMPLE "g_(a*b)",
     NOINDENT,
     "(Notice that the coefficients are elements of the coefficient ring.)",
     PARA,
     "We may get parts of the leading term of a polynomial as follows.",
     EXAMPLE "leadTerm g",
     EXAMPLE "leadCoefficient g",
     EXAMPLE "leadMonomial g",
     NOINDENT,
     "Notice that the lead monomial is an element of a monoid whose name is
     ", TT "[a,b]", ".  Its exponents can be extracted with ", TO "exponents", ".",
     EXAMPLE "exponents leadMonomial g",
     "We can get all of the coefficients at once, assembled into matrices.",
     EXAMPLE "coefficients f",
     EXAMPLE "coefficients g",
     "A list of lists of exponents appearing in a polynomial can be obtained with
     ", TO "exponents", ".",
     EXAMPLE "exponents f",
     EXAMPLE "exponents g",
     "The entire structure of a polynomial can be provided in an accessible form
     based on lists with ", TO "listForm", ".",
     EXAMPLE "listForm f",
     EXAMPLE "S = listForm g",
     "The lists above are lists of pairs, where the first member of each pair is
     a list of exponents in a monomial, and the second member is the corresponding
     coefficient.  Standard list operations can be used to manipulate the result.",
     EXAMPLE "S / print;",
     "The structure of a polynomial can also be provided in a form
     based on hash tables with ", TO "standardForm", ".",
     EXAMPLE "S = standardForm f",
     EXAMPLE "standardForm g",
     "The hash tables above present the same information, except that only nonzero
     exponents need to be provided.  The information can be extracted with ", TO "#", ".",
     EXAMPLE "S#(new HashTable from {0 => 2})",
     PARA,
     "Monomials (monoid elements) have an accessible form which is implicitly used
     above.",
     EXAMPLE "listForm leadMonomial g",
     EXAMPLE "standardForm leadMonomial g",
     "Comparison of polynomials is possible, and proceeds by simply examining the
     lead monomials and comparing them.",
     EXAMPLE "f < g",
     EXAMPLE "f ? g",
     EXAMPLE "sort {b^2-1,a*b,a+1,a,b}"
     }

document { "associative algebras",
     "Eventually we will implement associative algebras, not necessarily
     commutative."
     }

document { "polynomial rings with multi-degrees",
     "It is possible to set up a polynomial ring so that the degree of an
     element is a vector of integers.  For this, the option
     ", TO "Degrees", " is used, together with a list of degrees for the
     variables in the ring.  Each degree is itself a list of integers.  The
     degrees given must all be of the same length, and length zero is
     allowed, to get an ungraded ring.",
     EXAMPLE "R = ZZ/101[a,b,Degrees=>{{1,2},{2,1}}]",
     EXAMPLE "describe R",
     "At the moment there is a restriction on the degree vectors: the first
     entry must be greater than zero.  This restriction will be removed soon.",
     EXAMPLE "degree a",
     EXAMPLE "degree b^2",
     EXAMPLE "degree 0_R",
     EXAMPLE "degree 1_R",
     "The degree of a polynomial is the least upper bound of the degrees 
     of its monomials.",
     EXAMPLE "degree (a+b)",
     "We may recover the number of integers in each degree list for our ring
     as follows.",
     EXAMPLE "degreeLength R",
     EXAMPLE "degreeLength ZZ"
     }

document { "maps between rings",
     "The class of all ring homomorphisms is ", TO "RingMap", ".  A ring
     homomorphism from a polynomial ring ", TT "S", " to a ring ", TT "R", "
     is obtained with ", TO "map", " by providing a list of
     elements in ", TT "R", " to which the generators (variables) of
     ", TT "S", " should go, as follows.",
     EXAMPLE "R = ZZ/101[a,b];",
     EXAMPLE "S = ZZ/101[x,y,z];",
     EXAMPLE "f = map(R,S,{a^2,a*b,b^2})",
     NOINDENT,
     "(Notice that the target of ", TT "f", " is the first argument of ", TT "map", "
     and the source of ", TT "f", " is the second argument.)",
     PARA,
     "We can apply this ring map to elements of ", TT "S", " in the usual way.",
     EXAMPLE "f(x+y+z)",
     EXAMPLE "f S_2",
     "Composition of ring maps is possible.",
     EXAMPLE "g = map(S,ZZ/101[t],{x+y+z})",
     EXAMPLE "f * g",
     "The defining matrix of ", TT "f", " can be recovered with the
     key ", TT "matrix", ".",
     EXAMPLE "f.matrix",
     "We can produce the kernel of ", TT "f", " with ", TO "kernel", ".",
     EXAMPLE "kernel f",
     "We can produce the image of ", TT "f", " with ", TO "image", ".  It is
     computed as its source ring modulo its kernel.",
     EXAMPLE "image f",
     "We can check whether the map is homogeneous with ", TO "isHomogeneous", ".",
     EXAMPLE "isHomogeneous f",
     "We can obtain the ring of the graph of ", TT "f", " with ", TO "graphRing", ".",
     EXAMPLE "graphRing f",
     "We can obtain the ideal of the graph of ", TT "f", " with ", TO "graphIdeal", ",
     except that currently the result is a matrix rather than an ideal, sigh.",
     EXAMPLE "graphIdeal f"
     }

document { "quotient rings",
     "The usual notation is used to form quotient rings.  A Groebner basis is computed
     and used to reduce ring elements to normal form after arithmetic operations.",
     EXAMPLE "R = ZZ/11",
     EXAMPLE "6_R + 7_R",
     EXAMPLE "S = QQ[x,y,z]/(x^2-y, y^3-z)",
     EXAMPLE "{1,x,x^2,x^3,x^4,x^5,x^6,x^7,x^8}",
     "In the example above you might have wondered whether typing ", TT "x", "
     would give an element of ", TT "S", " or an element of ", TT "QQ[x,y,z]", ".  Our
     convention is that typing ", TT "x", " gives an element of the 
     quotient ring unless the polynomial ring has been assigned to a global variable.
     Here is an example of the opposite behavior.",
     EXAMPLE "T = QQ[r,s,t]",
     EXAMPLE "U = T/(r^3+s^3+t^3)",
     EXAMPLE "r^3+s^3+t^3",
     "Notice that this time, the variables end up in the ring ", TT "T", ".  The
     command ", TO "use", " will install variables for a specified ring.",
     EXAMPLE "use U",
     EXAMPLE "r^3+s^3+t^3",
     "In a program we can tell whether a ring is a quotient ring.",
     EXAMPLE "isQuotientRing ZZ",
     EXAMPLE "isQuotientRing S",
     "We can recover the coefficient ring.",
     EXAMPLE "coefficientRing S"
     }
     
   