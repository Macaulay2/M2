-- add f%g and f//g

-- overview.m2

-- A mathematical overview of Macaulay 2 functions.

document { "mathematical overview",
     "In this section we give a comprehensive overview of the main 
     functions of Macaulay 2.",
     PARA,
     MENU {
     	  (
	       "rings",
	       MENU {
		    TO "basic rings",
		    TO "polynomial rings",
		    TO "manipulating polynomials",
		    TO "polynomial rings with other monomial orderings",
		    TO "multi-graded polynomial rings",
		    TO "maps between rings",
		    TO "ideals",
		    TO "quotient rings",
		    TO "finite fields",
		    TO "fraction fields",
		    TO "tensor products of rings",
		    TO "exterior algebras",
		    TO "symmetric algebras",
		    TO "Weyl algebras",
		    TO "associative algebras",
		    TO "algebraic varieties",
		    }
	       ),
	  (
	       "matrices",
	       MENU {
		    TO "making matrices",
		    TO "making random matrices",
		    TO "making generic matrices",
		    TO "manipulating matrices",
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
		    TO "bases of parts of modules",
		    TO "coherent sheaves",
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
     EXAMPLE {
	  "1234",
      	  "123/4",
      	  "123.4",
      	  "123+4*ii",
	  },
     "The usual arithmetic operations are available.",
     EXAMPLE {
	  "4/5 + 2/3",
      	  "10^20",
      	  "3*5*7",
      	  "5!",
	  },
     "An additional pair of division operations which produce integral quotients
     and remainders is available.",
     EXAMPLE {
	  "1234//100",
      	  "1234%100"
	  },
     }

document { "polynomial rings",
     "A polynomial ring can be created with the usual mathematical notation.",
     EXAMPLE "ZZ[x,y,z]",
     "If you try to construct this ring again, you will get a different
     answer.  We use the strict comparison operator ", TO "===", " to
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
     element of a ring, or indeed, to obtain any multiple of the unit.",
     EXAMPLE {
	  "0_R",
      	  "1_R",
      	  "11_R",
	  },
     "Subscript notation (the other way around) can be used to obtain the
     variables (generators) from the ring.  The first available index is 0.",
     EXAMPLE "R_0^10+R_1^3+R_2",
     "It is also possible to obtain the variables in a ring from strings
     containing their names.",
     EXAMPLE ///R_"a"^10+R_"b"^3+R_"c"///,
     "The number of variables is provided by ", TO "numgens", ".",
     EXAMPLE {
	  "numgens R",
      	  "apply(numgens R, i -> R_i^i)",
      	  "sum(numgens R, i -> R_i^i)",
	  },
     "The index corresponding to a given variable can be obtained
     with ", TO "index", ".",
     EXAMPLE {
	  "index a, index f",
	  },
     "The coefficient ring can be recovered with ", TO "coefficientRing", ".",
     EXAMPLE "coefficientRing R",
     "An element of the coefficient ring can be promoted to the polynomial ring.",
     EXAMPLE "promote(11/2,R)",
     "Conversely, an element of the polynomial ring which is known to be a scalar
     can be lifted back to the coefficient ring.",
     EXAMPLE {
	  "sc = (a-2)^2-a^2+4*a",
      	  "lift(sc,QQ)",
	  },
     "In programs, the function ", TO "liftable", " can be used to see whether
     this is possible.",
     EXAMPLE {
	  "liftable(sc,QQ)",
      	  "liftable(c^3,QQ)",
	  },
     "A random element of degree ", TT "n", " can be obtained with ", TO "random", ".",
     EXAMPLE "random(2,R)",
     "We may construct polynomial rings over polynomial rings.",
     EXAMPLE "ZZ[a,b,c][d,e,f];",
     "When displaying an element of an iterated polynomial ring,
     parentheses may be used to organize the coefficients, which
     may themselves be polynomials (sums).",
     EXAMPLE "(a+d+1)^2",
     "Variables names may be words.",
     EXAMPLE {
	  "QQ[rho,sigma,tau];",
      	  "(rho - sigma)^2",
	  },
     "There are various other ways to specify the variables to be used in a polynomial
     ring.  A sequence of variables can be obtained as follows.",
     EXAMPLE "ZZ[b..k];",
     "The single-letter variables can be obtained with ", TO "vars", ".",
     EXAMPLE {
	  "vars (0..4)",
      	  "ZZ[vars (0..4),vars(26..30),vars 51]",
	  },
     "Subscripted variables can be used, provided the base for the subscripted
     variable has not bee used for something else.",
     EXAMPLE "ZZ[t,p_0,p_1,q_0,q_1];",
     "Sequences of subscripted variables can be obtained.",
     EXAMPLE {
      	  "ZZ[p_(0,0) .. p_(2,1),q_0..q_5]",
	  "(p_(0,0)+q_2-1)^2",
	  },
     "The subscripts can be much more general, but care is required when using
     symbols as subscripts, for the symbols may acquire values later which would
     interfere with your original use of them as symbols.  Thus you should
     protect symbols which will be used in this way.",
     EXAMPLE {
	  "protect xx; protect yy; protect zz;",
      	  "ZZ[ee_[xx],ee_[yy],ee_[zz]]",
	  },
     "A basis of the part of the ring of a given degree can be obtained in
     matrix form with ", TO "basis", ".",
     EXAMPLE "basis(2,R)",
     "The Hilbert series of a polynomial ring can be obtained.  Its power
     series expansion is the generating function for the dimensions of the
     degree ", TT "n", " parts.",
     EXAMPLE "hilbertSeries R",
     "We may use the option ", TO "Degrees", " to produce rings where the
     generators have degrees other than 1.",
     EXAMPLE {
	  "S = ZZ/101[a,b,c,d,Degrees=>{1,2,3,4}]",
      	  "random(5,S)",
      	  "hilbertSeries S"
	  },
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
     EXAMPLE {
	  "R=ZZ/101[a,b,c]",
      	  "(a+b+c+1)^2",
	  },
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
     EXAMPLE "R=ZZ/101[x,y,z,MonomialOrder=>RevLex];",
     "We currently get a monomial overflow if we try to compute anything
     in this ring, sigh.",
     -- EXAMPLE "(x+y+z+1)^2",
     PARA,
     "The next ring uses graded lexicographic ordering.  This means that
     terms of higher total degree come first; for two terms of the
     same degree, the term with the higher power of the first variable comes
     first: for terms with the same power of the first variable the
     power of the second variable is consulted, and so on.",
     EXAMPLE {
	  "R=ZZ/101[a,b,c,MonomialOrder=>GLex];",
      	  "(a+b+c+1)^2",
	  },
     NOINDENT,
     "(Notice how similar the result above is to the one obtained when
     graded reverse lexicographic ordering is used.)",
     PARA,
     "The next ring uses lexicographic ordering.  This means that 
     terms with the highest power of the first variable come
     first: for two terms with the same power of the first variable the
     power of the second variable is consulted, and so on.",
     EXAMPLE {
	  "R=ZZ/101[a,b,c,MonomialOrder=>Lex];",
      	  "(a+b+c+1)^2",
	  },
     "The next ring uses an elimination order suitable for eliminating
     the first two variables, ", TT "a", " and ", TT "b", ".  In such an 
     ordering we want all terms in which either of the first two
     variables appears to come before all of those terms in which
     the first two variables don't appear.  This particular ordering
     accomplishes this by consulting first the graded reverse lexicographic
     ordering ignoring all variables but the first two, and in case of
     a tie, consulting the graded reverse lexicographic ordering of the
     entire monomials.",
     EXAMPLE {
	  "R=ZZ/101[a,b,c,MonomialOrder=>Eliminate 2];",
      	  "(a+b+c+1)^2",
	  },
     "The next ring uses the product ordering which segregates the
     first variable from the next two.  This means that terms come
     first which would come first in the graded reverse lexicographic
     ordering when their parts involving the
     second two variables are ignored, and in case of equality,
     the graded reverse lexicographic ordering of their parts involving
     just the next two variables is consulted.",
     EXAMPLE {
	  "R=ZZ/101[a,b,c,MonomialOrder=>ProductOrder{1,2}];",
      	  "(a+b+c+1)^2"
	  },
     }

document { "manipulating polynomials",
     "Let's set up some polynomials.",
     EXAMPLE {
	  "R = ZZ/10007[a,b];",
      	  "f = (2*a+3)^4 + 5",
      	  "g = (2*a+b+1)^3",
	  },
     "The number of terms in a polynomial is obtained with ", TO "size", ".",
     EXAMPLE "size f, size g", 
     "The degree of a polynomial is obtained with ", TO "degree", ".",
     EXAMPLE {
	  "degree f", 
      	  "degree g",
	  },
     NOINDENT,
     "(Notice that the degree is a list containing one integer, rather than
     an integer.  The degree is actually a vector of integers, represented as
     a list, with one component by default.)",
     PARA,
     "The list of terms of a polynomial is obtained with ", TO "terms", ".",
     EXAMPLE "terms g",
     "We may combine that with ", TO "select", " to select terms satisfying certain 
     conditions.  Here we select the terms of degree 2, subsequently summing them,
     keeping in mind that the degree is a list of integers.",
     EXAMPLE {
	  "select(terms g, i -> degree i == {2})",
      	  "sum oo",
	  },
     "A string representing the polynomial, suitable for entry into other programs,
     can be obtained with ", TO "name", ".",
     EXAMPLE {
	  "name f",
      	  "name g",
	  },
     PARA,
     "The usual algebraic operations on polynomials are available, but there
     are some special remarks to make about division.  The result of division
     depends on the ordering of monomials chosen when the ring is created, for
     division of ", TT "f", " by ", TT "g", " proceeds by locating monomials in
     ", TT "f", " divisible by the leading monomial of ", TT "g", ", and
     substituting for it the negation of the rest of ", TT "g", ".  The quotient 
     is provided by the expression ", TT "f//g", ", and the remainder is obtained 
     with ", TT "f%g", ".",
     EXAMPLE {
	  "quot = f//g",
      	  "rem = f%g",
      	  "f == quot * g + rem",
	  },
     NOINDENT,
     "(Notice that comparison of polynomials is done with the operator ", TO "==", ".)",
     PARA,
     "Polynomials can be homogenized with respect to one of the variables in the
     ring with ", TO "homogenize", ".",
     EXAMPLE "homogenize(f,b)",
     PARA,
     "Polynomials can be factored with ", TO "factor", ".",
     EXAMPLE {
	  "S = factor f",
      	  "T = factor g",
	  },
     PARA,
     "The results above are represented as products of powers.  (Exponents equal
     to 1 don't appear in the display.)  We can see the internal structure
     to a specified depth (in this case, 2) with ", TO "peek2", ".",
     EXAMPLE {
	  "peek2(S,2)",
      	  "peek2(T,2)",
	  },
     "The components of the expressions above (Products and Powers) are types of
     lists, and the parts can be extracted with ", TO "#", ".",
     EXAMPLE {
	  "T#0",
      	  "T#0#0",
      	  "T#0#1",
	  },
     "The ring containing a ring element can be obtained with ", TO "ring", ".",
     EXAMPLE "ring f",
     "You can use this in a program to check whether two ring elements 
     come from the same ring.",
     EXAMPLE "ring f === ring g",
     "The coefficient of a monomial in a polynomial can be obtained with ", TO "_", ".",
     EXAMPLE {
	  "f_1",
      	  "f_a",
      	  "g_(a*b)",
	  },
     NOINDENT,
     "(Notice that the coefficients are elements of the coefficient ring.)",
     PARA,
     "We may get parts of the leading term of a polynomial as follows.",
     EXAMPLE {
	  "leadTerm g",
      	  "leadCoefficient g",
      	  "leadMonomial g",
	  },
     NOINDENT,
     "Notice that the lead monomial is an element of a monoid whose name is
     ", TT "[a,b]", ".  Its exponents can be extracted with ", TO "exponents", ".",
     EXAMPLE "exponents leadMonomial g",
     "We can get all of the coefficients at once, assembled into matrices.",
     EXAMPLE {
	  "coefficients f",
      	  "coefficients g",
	  },
     "A list of lists of exponents appearing in a polynomial can be obtained with
     ", TO "exponents", ".",
     EXAMPLE {
	  "exponents f",
      	  "exponents g",
	  },
     "The entire structure of a polynomial can be provided in an accessible form
     based on lists with ", TO "listForm", ".",
     EXAMPLE {
	  "listForm f",
      	  "S = listForm g",
	  },
     "The lists above are lists of pairs, where the first member of each pair is
     a list of exponents in a monomial, and the second member is the corresponding
     coefficient.  Standard list operations can be used to manipulate the result.",
     EXAMPLE "S / print;",
     "The structure of a polynomial can also be provided in a form
     based on hash tables with ", TO "standardForm", ".",
     EXAMPLE {
	  "S = standardForm f",
      	  "standardForm g",
	  },
     "The hash tables above present the same information, except that only nonzero
     exponents need to be provided.  The information can be extracted with ", TO "#", ".",
     EXAMPLE "S#(new HashTable from {0 => 2})",
     PARA,
     "Monomials (monoid elements) have an accessible form which is implicitly used
     above.",
     EXAMPLE {
	  "listForm leadMonomial g",
      	  "standardForm leadMonomial g",
	  },
     "Comparison of polynomials is possible, and proceeds by simply examining the
     lead monomials and comparing them.",
     EXAMPLE {
	  "f < g",
      	  "f ? g",
      	  "sort {b^2-1,a*b,a+1,a,b}"
	  },
     }

document { "multi-graded polynomial rings",
     "It is possible to set up a polynomial ring so that the degree of an
     element is a vector of integers.  For this, the option
     ", TO "Degrees", " is used, together with a list of degrees for the
     variables in the ring.  Each degree is itself a list of integers.  The
     degrees given must all be of the same length, and length zero is
     allowed, to get an ungraded ring.",
     EXAMPLE {
	  "R = ZZ/101[a,b,c,Degrees=>{{1,2},{2,1},{1,0}}]",
      	  "describe R",
	  },
     "At the moment there is a restriction on the degree vectors: the first
     entry must be greater than zero.  This restriction will be removed soon.",
     EXAMPLE {
	  "degree a",
      	  "degree b^2",
      	  "degree 0_R",
      	  "degree 1_R",
	  },
     "A random element of bi-degree ", TT "{m,n}", " can be obtained with
     ", TO "random", ".",
     EXAMPLE "random({15,15},R)",
     "The function ", TO "degree", " applied to a polynomial will
     return the least upper bound of the degrees of its monomials.",
     EXAMPLE "degree (a+b)",
     "We may recover the number of integers in each degree list for our ring
     as follows.",
     EXAMPLE {
	  "degreeLength R",
      	  "degreeLength ZZ"
	  },
     }

document { "maps between rings",
     "The class of all ring homomorphisms is ", TO "RingMap", ".  A ring
     homomorphism from a polynomial ring ", TT "S", " to a ring ", TT "R", "
     is obtained with ", TO "map", " by providing a list of
     elements in ", TT "R", " to which the generators (variables) of
     ", TT "S", " should go, as follows.",
     EXAMPLE {
	  "R = ZZ/101[a,b];",
      	  "S = ZZ/101[x,y,z];",
      	  "f = map(R,S,{a^2,a*b,b^2})",
	  },
     NOINDENT,
     "(Notice that the target of ", TT "f", " is the first argument of ", TT "map", "
     and the source of ", TT "f", " is the second argument.)",
     PARA,
     "We can apply this ring map to elements of ", TT "S", " in the usual way.",
     EXAMPLE {
	  "f(x+y+z)",
      	  "f S_2",
	  },
     "Composition of ring maps is possible.",
     EXAMPLE {
	  "g = map(S,ZZ/101[t],{x+y+z})",
      	  "f * g",
	  },
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
     EXAMPLE {
	  "R = ZZ/11",
      	  "6_R + 7_R",
	  },
     EXAMPLE {
	  "S = QQ[x,y,z]/(x^2-y, y^3-z)",
      	  "{1,x,x^2,x^3,x^4,x^5,x^6,x^7,x^8}",
	  },
     "In the example above you might have wondered whether typing ", TT "x", "
     would give an element of ", TT "S", " or an element of ", TT "QQ[x,y,z]", ".  Our
     convention is that typing ", TT "x", " gives an element of the 
     last ring which has been assigned to a global variable.  Here is another
     example.",
     EXAMPLE {
	  "T = ZZ/101[r,s,t]",
      	  "T/(r^3+s^3+t^3)",
      	  "r^3+s^3+t^3",
	  },
     "Notice that this time, the variables end up in the ring ", TT "T", ", because
     we didn't assign the quotient ring to a global variable.  The
     command ", TO "use", " would install the variables for us, or we could
     assign the ring to a global variable.",
     EXAMPLE {
	  "U = ooo",
      	  "r^3+s^3+t^3",
	  },
     "The functions ", TO "lift", " and ", TO "promote", " can be used to transfer
     elements between the polynomial ring and its quotient ring.",
     EXAMPLE {
	  ///lift(U_"r",T)///,
      	  ///promote(T_"r",U)///,
	  },
     "A random element of degree ", TT "n", " can be obtained with ", TO "random", ".",
     EXAMPLE "random(2,S)",
     "In a program we can tell whether a ring is a quotient ring.",
     EXAMPLE {
	  "isQuotientRing ZZ",
      	  "isQuotientRing S",
	  },
     "We can recover the ring of which a given ring is a quotient.",
     EXAMPLE "ambient S",
     "We can also recover the coefficient ring, as we could for the original 
     polynomial ring.",
     EXAMPLE "coefficientRing S",
     "Here's how we can tell whether the defining relations of a quotient
     ring were homogeneous.",
     EXAMPLE {
	  "isHomogeneous S",
      	  "isHomogeneous U",
	  },
     "We can obtain the characteristic of a ring with ", TO "char", ".",
     EXAMPLE {
	  "char (ZZ/11)",
      	  "char S",
      	  "char U",
	  },
     "The presentation of the quotient ring can be obtained as a matrix
     with ", TO "presentation", ".",
     EXAMPLE "presentation S",
     "If a quotient ring has redundant defining relations, a new ring can
     be made in which these are eliminated with ", TO "trim", ".",
     EXAMPLE {
	  "R = ZZ/101[x,y,z]/(x-y,y-z,z-x)",
      	  "trim R"
	  },
     }

document { "finite fields",
     "The prime finite fields can be made easily as quotient rings of ", TT "ZZ", ".",
     EXAMPLE "ZZ/101",
     "In general, to make a finite field with ", TT "q", " elements, we use
     ", TO "GF", ".",
     EXAMPLE "k = GF 81",
     "The generator of the field can be obtained as usual.",
     EXAMPLE "k_0",
     "Notice that the name of the generator is displayed with a ", TT "$", " in it
     to indicate that it is not accessible by typing.  Of course, you could assign the
     generator to the symbol of your choice, but it will still print the same way.",
     EXAMPLE {
	  "a = k_0",
      	  "a^20+1",
	  },
     "To specify a name for the generator when the field is created, use the ", TO "Variable", "
     option.",
     EXAMPLE {
	  "F = GF(16, Variable => b)",
      	  "b^20 + 1",
      	  "random F",
	  },
     "Finite fields can be used as base rings for polynomial rings.",
     EXAMPLE {
	  "R = F[x,y,z]",
      	  "random(2,R)",
	  },
     "If you have a quotient ring that you know is a finite field, then you can
     convert it to ring which is known by the system to be a finite field.",
     EXAMPLE "GF (ZZ/2[T]/(T^9+T+1), Variable => T)",
     "You may also provide your own choice of primitive element.  Internally,
     elements of the finite field are stored as powers of the primitive element.
     First we assign our quotient ring to a global variable to ensure that
     ", TT "T", " gets set to a value in the quotient ring, and then we
     call ", TT "GF", ".",
     EXAMPLE {
	  "A = ZZ/2[T]/(T^9+T+1)",
      	  "k = GF (A, PrimitiveElement => T^3+1)",
	  },
     "Notice that ", TT "T", " is still recorded as an element of its
     quotient ring, rather than this finite field.",
     EXAMPLE "T",
     "Use ", TO "promote", " to see how the generator ", TT "T", " appears as
     an element of the finite field.",
     EXAMPLE "promote(T,k)",
     "Conversely, a given element of the finite field can be transferred back
     to the quotient ring with ", TO "lift", ".",
     EXAMPLE "lift(k_0, ring T)",
     "We can even lift it back to the polynomial ring.",
     EXAMPLE "lift(k_0, ambient ring T)"
     }

document { "fraction fields",
     "The fraction field of a ring (which must be an integral domain) is obtained
     with the function ", TO "frac", ".",
     EXAMPLE {
	  "frac ZZ",
      	  "R = ZZ/101[x,y]/(x^3 + 1 + y^3)",
      	  "frac R",
	  },
     "At the moment, it is also required that the coefficient ring be finite.",
     PARA,
     "After defining a ring such as ", TT "R", ", fractions in it can be obtained 
     by writing them explicitly.",
     EXAMPLE {
	  "x",
      	  "1/x",
      	  "x/1",
	  },
     "Alternatively, after applying the function ", TO "use", ", or assigning the
     fraction ring to a global variable, the symbols you used
     become associated with the corresponding elements of the fraction field.",
     EXAMPLE {
	  "use frac R",
      	  "x",
	  },
     "Fractions are reduced to the extent possible.  This is done by computing the
     syzygies between the numerator and denominator, and picking one of low degree.",
     EXAMPLE {
	  "f = (x-y)/(x^6-y^6)",
      	  "(x^3 - y^3) * f",
	  },
     "The parts of a fraction may be extracted.",
     EXAMPLE {
	  "numerator f",
      	  "denominator f",
	  },
     "Alternatively, the functions ", TO "lift", " and ", TO "liftable", " can
     be used.",
     EXAMPLE {
	  "liftable(1/f,R)",
      	  "liftable(f,R)",
      	  "lift(1/f,R)"
	  },
     }

document { "tensor products of rings",
     "The operator ", TO "**", " or the function ", TO "tensor", " can be
     used to construct tensor products of rings.",
     EXAMPLE "ZZ/101[x,y]/(x^2-y^2) ** ZZ/101[a,b]/(a^3+b^3)",
     "Other monomial orderings can be specified.",
     EXAMPLE "T = tensor(ZZ/101[x,y], ZZ/101[a,b], MonomialOrder => Eliminate 2)",
     "The options to ", TT "tensor", " can be discovered with ", TO "options", ".",
     EXAMPLE "options tensor"
     }

document { "exterior algebras",
     "Create an exterior algebra with explicit generators by creating a polynomial
     ring with the option ", TO "SkewCommutative", ".",
     EXAMPLE {
	  "R = ZZ/101[x,y,z, SkewCommutative => true]",
      	  "y*x",
      	  "(x+y+z)^2",
      	  "basis R",
      	  "basis(2,R)",
	  },
     "At the moment, there is no way to construct an exterior algebra from a free module, but
     you can take quotient rings of exterior algebras.",
     }

document { "symmetric algebras",
     "Polynomial rings are symmetric algebras with explicit generators, and we have
     already seen how to construct them.  But if you have a module, then its symmetric
     algebra can be constructed with ", TO "symmetricAlgebra", ".",
     EXAMPLE {
	  "R = QQ[a..d];",
      	  "S = symmetricAlgebra R^3",
      	  "describe S",
	  },
     "The dollar signs used in displaying the names of the variables indicate that
     the names were invented for us, and are not available by typing them, but you can
     get them in the usual way by indexing.",
     EXAMPLE {
	  "S_0+S_4",
      	  ///S_"$x_0"///,
	  },
     "To specify the names of the variables when creating the ring, use the 
     ", TO "Variables", " option.",
     EXAMPLE "S = symmetricAlgebra(R^3, Variables => {t,u,v})",
     "We can construct the symmetric algebra of a module that isn't
     necessarily free.",
     EXAMPLE "symmetricAlgebra(R^1/(R_0,R_1^3), Variables => {t})"
     }

document { "Weyl algebras",
     "A Weyl algebra is the non-commutative algebra of algebraic differential 
     operators on a polynomial ring.  To each variable ", TT "x", " corresponds 
     the operator ", TT "dx", " which differentiates with respect to that 
     variable.  The evident commutation relation takes the form 
     ", TT "dx*x == x*dx + 1", ".",
     PARA,
     "We can give any names we like to the variables in a Weyl algebra, provided
     we specify the correspondence between the variables and the derivatives, which
     we do with the ", TO "WeylAlgebra", " option, as follows.",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[x,dx,t,WeylAlgebra => {x=>dx}]",
	  "dx*x",
	  "dx*x^5"
	  }
     }

document { "associative algebras",
     "Eventually we will implement associative algebras, not necessarily
     commutative."
     }

document { "ideals",
     "The ideal generated by a list of ring elements can be constructed with the function
     ", TO "ideal", ".",
     EXAMPLE {
	  "R = ZZ/101[a..d];",
      	  "I = ideal (a^2*b-c^2, a*b^2-d^3, c^5-d)",
	  },
     "If you have a matrix, then ", TT "ideal", " will produce the ideal generated
     by the entries of the matrix.",
     EXAMPLE {
	  "f = matrix {{a^2,b^2},{c^2,d^2}}",
      	  "J = ideal f",
	  },
     "An interesting class of ideals can be obtained as the defining ideals in 
     projective space of monomial curves.  The twisted cubic is the closure of the
     set of points ", TT "(1,t^1,t^2,t^3)", " in projective space.  We use a list of
     the exponents and ", TO "monomialCurve", " to get the ideal.",
     EXAMPLE "monomialCurve(R,{1,2,3})",
     "The command ", TO "substitute", " can be used to transfer an ideal to another
     ring.  You may want to do this because another ring has a monomial ordering
     more suitable for the computations you are about to do, or it may have
     additional variables in it, one of which you wish to use for homogenization.
     Here is an example of the latter.  We make another ring with a new variable ", TT "t", "
     in it, transfer the ideal, and then homogenize the ideal.",
     EXAMPLE {
	  "S = ZZ/101[a..d,t];",
      	  "substitute(I,S)",
      	  "homogenize(oo,t)",
	  },
     NOINDENT, "In this case, the substitution was done according to the names of
     the variables in the two rings.  There are more explicit ways to specify the
     substitution to be performed.  Here is one where we list the new values for
     all the variables.",
     EXAMPLE {
	  "T = ZZ/101[x,y,z,t];",
      	  "substitute(I,{a=>x^10,b=>y^10,c=>z^10,d=>t^10})",
	  },
     "Now notice that the variable ", TT "a", " appears to be an element of ", TT "S", ".
     The creation of the ring ", TT "S", " supplanted the earlier value.",
     EXAMPLE "a",
     "We restore the variables of ", TT "R", " to visibility.",
     EXAMPLE "use R",
     "To recover the generators of an ideal as a matrix, use ", TO "generators", ".",
     EXAMPLE "generators J",
     "Use the operator ", TT "%", " to reduce a ring element with respect to a
     Groebner basis of the ideal.",
     EXAMPLE "(1+a+a^3+a^4) % J",
     "Membership in the ideal may be tested by comparing the answer to 0 with ", TT "==", ".",
     EXAMPLE {
	  "(1+a+a^3+a^4) % J == 0",
      	  "a^4 % J == 0",
	  },
     PARA,
     "The usual algebraic operations on ideals are available.",
     EXAMPLE {
	  "I+J",
      	  "intersect(I,J)",
      	  "I*J",
      	  "J:I",
      	  "radical J",
	  },
     "We may ask whether one ideal is contained in another.",
     EXAMPLE {
	  "isSubset(I,J)",
      	  "isSubset(I,I+J)",
      	  "isSubset(I+J,J)",
	  },
     "Once you have an ideal, then you may construct the quotient ring or the quotient
     module (there is a difference).  Here is the quotient ring.",
     EXAMPLE "R/I",
     "Here is the quotient module.",
     EXAMPLE "M = R^1/I",
     "And if you want the module underlying ", TT "I", " itself, you can get it with
     ", TO "module", ".",
     EXAMPLE "module I",
     "In general, when an ideal is used as an argument to a function which usually
     would be given a module, we try to make an informed choice about whether the user
     intends the ideal to be used as a module directly, or whether the quotient module
     is more suitable.  In homological functions such as ", TO "Ext", " and ", TO "Tor", "
     the underlying module is used.  Here are some examples where the quotient 
     module is used.",
     PARA,
     "A free resolution of ", TT "R^1/I", " can be obtained with ", TO "resolution", ".",
     EXAMPLE "resolution I",
     "The Krull dimension or codimension of the support of the quotient module can
     be obtained.",
     EXAMPLE {
	  "dim I",
      	  "dim J",
      	  "codim I",
	  },
     NOINDENT, "(Beware that for a homogeneous ideal the
     dimension of its projective variety is one less than the number provided by
     ", TO "dim", ".)",
     PARA,
     "If the dimension of the quotient module as a vector space is needed,
     use ", TO "basis", " to get a matrix whose columns form a basis, and compute
     the dimension from it.",
     EXAMPLE {
	  "basis (R^1/J)",
      	  "rank source oo",
	  },
     NOINDENT, "(Here ", TO "oo", " refers to the result on the previous line."
     }

document { "making matrices",
     "The simplest way to make a matrix is to give a doubly nested list of ring
     elements to the ", TO "matrix", " command.",
     EXAMPLE {
	  "R = ZZ/101[x,y,z];",
      	  "f = matrix {{x,0,y*z},{0,y^2,x^2}}",
	  },
     "One way to construct a doubly nested
     list of ring elements is with the ", TO "table", " command.",
     EXAMPLE {
	  "p = matrix table(3,3,(i,j) -> R_i^j)",
      	  "q = matrix table(3,3,(i,j) -> R_j^i)",
	  },
     "The usual arithmetic operations among matrices are available, including
     direct sum (", TO "++", ") and tensor product (", TO "**", ").  Scalars 
     are converted to scalar matrices when necessary.",
     EXAMPLE {
	  "x*p",
      	  "11-p",
      	  "p*q",
      	  "p++q",
      	  "r = p++x",
      	  "x ++ y ++ z ++ x*y*z",
      	  "p**p",
	  },
     "The components of a direct sum can be recovered later.",
     EXAMPLE "components r",
     "There are commands for horizontal and vertical concatenation of matrices,
     and again, scalars are converted to scalar matrices when necessary.",
     EXAMPLE {
	  "p|q",
      	  "p||q",
      	  "p|1",
      	  "x^3||p",
	  },
     "The identity matrix can be obtained as the identity map on a free module.",
     EXAMPLE "id_(R^3)",
     "A matrix is regarded as a homomorphism between two free modules, its
     source and target.",
     EXAMPLE {
	  "M = target f",
      	  "N = source f",
	  },
     "Free modules are actually graded free modules, with the same sort
     of grading that the ring comes with.  The degrees of the basis vectors
     of the target are always zero.",
     EXAMPLE {
	  "degree M_0",
      	  "degree M_1",
	  },
     "If possible, the degrees of the basis vectors of the source are set so 
     that the map ", TT "f", " turns out to a homogeneous map of degree zero.
     This opportunism isimportant because certain algorithms will run faster 
     on homogeneous maps.",
     EXAMPLE {
	  "degree N_0",
      	  "degree N_1",
      	  "degree N_2",
      	  "isHomogeneous f",
	  },
     "A list of the degrees of all the basis vectors can be obtained with
     ", TO "degrees", ".",
     EXAMPLE "degrees N",
     "It may happen that the matrix can not be made homogeneous.  In that
     case, the degree of a basis vector is currently set to the degree of the
     largest monomial occurring in the corresponding column of the matrix.  In
     a future version of the program it might be more sensible to set
     the degrees of the basis vectors all to zero.",
     EXAMPLE {
	  "g = matrix {{x,0,y*z},{y^2,x^2,0}}",
      	  "isHomogeneous g",
      	  "degrees source g",
	  },
     "Suppose we multiply a homogeneous polynomial by a homogeneous matrix.
     The result ought to be homogeneous, but how can we arrange that?  Scalar
     multiplication should not change the source or target of a map!  Instead,
     we introduce one final complication: each matrix records a degree of its own,
     which is normally zero, and is used when deciding whether the matrix is
     homogeneous.",
     EXAMPLE {
	  "degree matrix {{x^10}}",
      	  "degree f",
	  },
     "Multiplying a matrix by a homogeneous polynomial adds the degree of
     the polynomial to the degree of the map.",
     EXAMPLE {
	  "h = x^10 * f",
      	  "degree h",
      	  "degrees source h",
      	  "isHomogeneous h",
	  },
     "If you don't like this, you have an alternative.  The degree of a tensor 
     product of two matrices is the sum of the degrees, and its source module is
     the tensor product of the source modules.",
     EXAMPLE {
	  "h = x^10 ** f",
      	  "degree h",
      	  "degrees source h",
      	  "isHomogeneous h"
	  },
     }

document { "making random matrices",
     "The ", TO "random", " command can be used in various ways to make random
     matrices.  We could assemble random polynomials into a doubly nested list
     and use ", TO "matrix", " to enter them into a matrix.",
     EXAMPLE {
	  "R = ZZ/101[x,y,z];",
      	  "matrix table(3,3,(i,j)->random(2,R))",
      	  "matrix table(3,3,(i,j)->random(i-j,R))",
	  },
     NOINDENT, "As usual, the degrees of the basis elements of the source and target
     of the map are determined from the polynomials' degrees.  Alternatively,
     one could generate a free module with the appropriate degrees and use
     ", TO "random", " to insert random matrix entries of the appropriate
     degree so as to make the matrix homogeneous.",
     EXAMPLE {
	  "random(R^3,R^{1,0,-1,-2})",
      	  "random(R^{1,0,-1},R^{1,0,-1})"
	  },
     }

document { "making generic matrices",
     "Here a few commands for making various sorts of generic matrices.  A generic
     matrix is one whose entries are independent variables from the ring, subject
     to certain relations.",
     PARA,
     "We begin by making a ring with enough variables to accomodate
     the examples.",
     EXAMPLE "R = ZZ/101[a..z];",
     "We can make a general generic matrix.  We specify the ring, the starting 
     variable and the dimensions.",
     EXAMPLE "genericMatrix(R,c,3,5)",
     "We can also make a skew symmetric matrix or a symmetric matrix.",
     EXAMPLE {
	  "R = ZZ/101[a..i];",
      	  "genericSkewMatrix(R,c,3)",
      	  "gs = genericSymmetricMatrix(R,a,3)",
	  },
     "Suppose we need a random symmetric matrix of linear forms in three variables.
     We can use ", TO "random", " and ", TO "substitute", " to obtain it.",
     EXAMPLE {
	  "S = ZZ/101[x,y,z];",
      	  "rn = random(S^1, S^{9:-1})",
      	  "substitute(gs, rn)"
	  },
     }

document { "manipulating matrices",
     "This node has not been  written yet."
     }

document { "Groebner bases",
     EXAMPLE {
	  "R = ZZ/101[a..d];",
      	  "f = matrix {{a^2*b-c^2, a*b^2-d^3, c^5-d}}",
	  },
     "The Groebner basis of the columns of a matrix can be obtained with
     ", TO "gb", ", and its generators can assembled into a matrix with ", TO "generators", ", 
     or its abbreviation, ", TO "gens", ".",
     EXAMPLE {
	  "gb f",
      	  "generators gb f",
	  },
     NOINDENT, "We intend to change the behavior of ", TO "gb", " so that it
     will return a matrix immediately rather than a special Groebner basis object.
     For now, beware that a Groebner basis object prints out just like a matrix, but
     can't be used as one."
     }

document { "free modules",
     "This node has not been  written yet."
     }

document { "making modules from matrices",
     "This node has not been  written yet."
     }

document { "manipulating modules",
	  -- document the way to get maps between a module M and its
	  -- version as a cokernel in the overview
     "This node has not been  written yet."
     }

document { "maps between modules",
     	  -- (R^5)_{0}
     "This node has not been  written yet."
     }

document { "bases of parts of modules",
     "This node has not been  written yet."
     }

document { "free resolutions of modules",
     "This node has not been  written yet."
     }

document { "making chain complexes by hand",
     "This node has not been  written yet."
     }

document { "extracting information from chain complexes",
     "This node has not been  written yet."
     }

document { "manipulating chain complexes",
     "This node has not been  written yet."
     }

document { "maps between chain complexes",
     "This node has not been  written yet."
     }

document { "algebraic varieties",
     "This node has not been  written yet."
     }

document { "coherent sheaves",
     "This node has not been  written yet."
     }
