--		Copyright 1993-1998 by Daniel R. Grayson


///
document { "rings",
     Headline => "an overview",
     "In this section we present an overview of rings.",
     see ", TO "Ring", ".",
     MENU {
	  TO "basic rings",
	  TO "polynomial rings",
	  TO "manipulating polynomials",
	  TO "polynomial rings with other monomial orderings",
	  TO "multi-graded polynomial rings",
	  TO "quotient rings",
	  TO "finite fields",
	  TO "fraction fields",
	  TO "tensor products of rings",
	  TO "exterior algebras",
	  TO "symmetric algebras",
	  TO "Weyl algebras",
	  TO "associative algebras",
	  }
     }
///



document { "basic rings",
     "The following rings are initially present in every session with
     Macaulay 2.",
     UL { 
	  TO "ZZ", 
	  TO "QQ", 
	  TO "RR", 
	  TO "CC" 
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
     "An additional pair of division operations that produce integral quotients
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
     "Conversely, an element of the polynomial ring that is known to be a scalar
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
     "A random homogeneous element can be obtained with ", TO "random", ".",
     EXAMPLE "random(2,R)",
     "We may construct polynomial rings over polynomial rings.",
     EXAMPLE "ZZ[a,b,c][d,e,f];",
     "When displaying an element of an iterated polynomial ring,
     parentheses are used to organize the coefficients recursively, which
     may themselves be polynomials.",
     EXAMPLE "(a+d+1)^2",
     "Variable names may be words.",
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
     variable has not been used for something else.",
     EXAMPLE "ZZ[t,p_0,p_1,q_0,q_1];",
     "Sequences of subscripted variables can be obtained.",
     EXAMPLE {
      	  "ZZ[p_(0,0) .. p_(2,1),q_0..q_5]",
	  "(p_(0,0)+q_2-1)^2",
	  },
     "The subscripts can be much more general, but care is required when using
     symbols as subscripts, for the symbols may acquire values later that would
     interfere with your original use of them as symbols.  Thus you should
     protect symbols that will be used in this way.",
     EXAMPLE {
	  "protect xx; protect yy; protect zz;",
      	  "ZZ[ee_[xx],ee_[yy],ee_[zz]]",
	  },
     "A basis of the subspace of ring elements of a given degree can be obtained
     in matrix form with ", TO "basis", ".",
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
     SEEALSO { "polynomial rings with other monomial orderings",  "PolynomialRing"}
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
     "The next ring uses ", TO "MonomialOrder", " to specify reverse
     lexicographic ordering.  This means that the term with 
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
     "The next ring uses the product ordering that segregates the
     first variable from the next two.  This means that terms come
     first that would come first in the graded reverse lexicographic
     ordering when their parts involving the
     second two variables are ignored, and in case of equality,
     the graded reverse lexicographic ordering of their parts involving
     just the next two variables is consulted.",
     EXAMPLE {
	  "R=ZZ/101[a,b,c,MonomialOrder=>ProductOrder{1,2}];",
      	  "(a+b+c+1)^2"
	  },
     PARA,
     "See ", TO "MonomialOrder", " for further details."
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
     "We may combine that with ", TO "select", " to select terms satisfying 
     certain conditions.  Here we select the terms of degree 2, subsequently 
     summing them, keeping in mind that the degree of a polynomial is always a 
     list of integers.",
     EXAMPLE {
	  "select(terms g, i -> degree i == {2})",
      	  "sum oo",
	  },
     "Of course, if the list of selected terms is empty, the sum would turn out
     to be the zero integer, rather than the zero element of the ring ", TT "R", ".
     Fortunately, we have another way to select the elements of given degree
     or multi-degree (see ", TO (symbol _, RingElement, ZZ), ").",
     EXAMPLE {
	  "g_0",
	  "g_1",
	  "g_2",
	  "g_3"
	  },
     "A string representing the polynomial, suitable for entry into other programs,
     can be obtained with ", TO "toString", ".",
     EXAMPLE {
	  "toString f",
      	  "toString g",
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
     "Notice that as in the example above, comparison of polynomials is done
     with the operator ", TO "==", ".",
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
     "Notice that in the comparison above, the strict equality operator ", TO "===", "
     is used.",     
     PARA,
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
     "We can get all of the coefficients at once, assembled a one-rowed matrix,
     along with a matrix containing the corresponding monomials.",
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
     "Monomials (monoid elements) have an accessible form that is implicitly used
     above.",
     EXAMPLE {
	  "listForm leadMonomial g",
      	  "standardForm leadMonomial g",
	  },
     "Comparison of polynomials is possible, and proceeds by simply examining the
     lead monomials and comparing them.",
     EXAMPLE {
	  "f < g",
      	  "sort {b^2-1,a*b,a+1,a,b}"
	  },
     "The comparison operator ", TO "?", " returns a symbol indicating how two
     polynomials, or rather, their lead monomials, stand with respect to each 
     other in the monomial ordering.",
     EXAMPLE "f ? g",
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
     Headline => "an overview",
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
     last ring that has been assigned to a global variable.  Here is another
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
     "For more information see ", TO "QuotientRing", "."
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
     "You may use ", TO "ambient", " to see the quotient ring the field is made from.",
     EXAMPLE "ambient k",
     "Use ", TO "ideal", " to see the ideal that defined that quotient ring.",
     EXAMPLE "ideal oo",
     "Finally, you may use ", TO "_", " to recover the generator of the ideal.",
     EXAMPLE "oo_0",
     "To specify a different name for the generator when the field is created, 
     use the ", TO "Variable", " option.",
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
     convert it to ring that is known by the system to be a finite field.",
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
     EXAMPLE "lift(k_0, ambient ring T)",
     "For more information see ", TO "GaloisField", "."
     }

document { "fraction fields",
     "The fraction field of a ring (which must be an integral domain) is obtained
     with the function ", TO "frac", ".",
     EXAMPLE {
	  "frac ZZ",
      	  "R = ZZ/101[x,y]/(x^3 + 1 + y^3)",
      	  "frac R",
	  },
     "After defining a ring such as ", TT "R", ", fractions in its fraction field
     can be obtained by writing them explicitly.",
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
     "For more information see ", TO "FractionField", "."
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
     the operator ", TT "dx", " that differentiates with respect to that 
     variable.  The evident commutation relation takes the form 
     ", TT "dx*x == x*dx + 1", ".",
     PARA,
     "We can give any names we like to the variables in a Weyl algebra, provided
     we specify the correspondence between the variables and the derivatives,
     with the ", TO "WeylAlgebra", " option, as follows.",
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

document { "algebraic varieties",
     "We may use ", TO "Spec", " to create an affine scheme (or algebraic variety) with
     a specified coordinate ring and ", TO "ring", " to recover the ring.",
     EXAMPLE {
	  "R = ZZ/2[x,y,z]",
	  "X = Spec R",
	  "ring X",
	  "dim X",
	  },
     "The variety ", TT "X", " is a 3-dimensional affine space.",
     PARA,
     "We may form products.",
     EXAMPLE {
	  "X * X",
	  "dim oo",
	  },
     PARA,
     "We may use ", TO "Proj", " to create a projective scheme (or algebraic variety)
     with a specified homogeneous coordinate ring.",
     EXAMPLE {
	  "Y = Proj R",
	  "ring Y",
	  "dim Y",
	  },
     "The most important reason for introducing the notion of algebraic variety into a computer
     algebra system is to support the notion of coherent sheaf.  See ", TO "coherent sheaves", "
     for information about that.",
     PARA,
     "For more details about varieties, see ", TO "Variety", "."
     }

document { "matrices--old",
     Headline => "an overview",
     "In this section we present an overview of matrices.
     For details, see ", TO "Matrix", ".",
     MENU {
	  TO "making matrices",
	  TO "making random matrices",
	  TO "making generic matrices",
	  TO "displaying matrices",
	  TO "manipulating matrices",
	  TO "determinants",
	  TO "diff and contract",
	  }
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
	  "table(3,3,(i,j) -> R_i^j)",
	  "p = matrix oo",
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
     "An identity matrix can be obtained with ", TO "id", " as the identity map
     on a free module.",
     EXAMPLE "id_(R^3)",
     "A matrix is regarded as a homomorphism between two free modules, its
     ", TO "source", " and ", TO "target", ".",
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
     This opportunism is important because certain algorithms will run faster 
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
     "For more information about matrices, see ", TO "Matrix", "."
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
     "We begin by making a ring with enough variables to accommodate all
     the examples.",
     EXAMPLE "R = ZZ/101[a..z];",
     "We can make a general generic matrix with ", TO "genericMatrix", ".  We specify
     the ring, the starting variable and the dimensions.",
     EXAMPLE "genericMatrix(R,c,3,5)",
     "We can also make a skew symmetric matrix with ", TO "genericSkewMatrix", " or
     a symmetric matrix with ", TO "genericSymmetricMatrix", ".",
     EXAMPLE {
	  "R = ZZ/101[a..i];",
      	  "genericSkewMatrix(R,c,3)",
      	  "gs = genericSymmetricMatrix(R,a,3)",
	  },
     "Suppose we need a random symmetric matrix of linear forms in three variables.
     We can use ", TO "random", " and ", TO "substitute", " to obtain it from the
     generic symmetric matrix ", TT "gs", " above.",
     EXAMPLE {
	  "S = ZZ/101[x,y,z];",
      	  "rn = random(S^1, S^{9:-1})",
      	  "substitute(gs, rn)"
	  },
     }

document { "displaying matrices",
     "Normally matrices are displayed in compact notation that originated 
     with Macaulay.",
     EXAMPLE {
	  "R = ZZ/101[x,y];",
	  "f = random(R^1,R^{2:-2})",
	  },
     "Setting the global flag ", TO "compactMatrixForm", " to ", TO "false", "
     will modify that behavior so that matrices are displayed more clearly
     and less compactly.",
     EXAMPLE {
	  "compactMatrixForm = false",
	  "f",
	  },
     "We may use ", TO "toString", " to produce a string, useful as input into
     other programs, or into Macaulay 2 at another time.",
     EXAMPLE "toString f",
     "The function ", TO "toExternalString", " tries harder: it gives
     complete information about the source, target, and degree of the map.",
     EXAMPLE "toExternalString f"
     }

document { "determinants",
     Headline => "an overview of using determinants",
     "The determinant of a square matrix is obtained using ", TO "det", ":",
     EXAMPLE {
	  "R = ZZ[a..i];",
	  "m = genericMatrix(R,a,3,3)",
	  "det m",
	  },
     "Use the ", TO "minors", " function to obtain the ideal generated by all of the 
     determinants of a matrix of a given size.",
     EXAMPLE "transpose generators minors(2,m)",
     "The exterior power of a matrix is obtained using ", TO "exteriorPower", ".",
     EXAMPLE "exteriorPower(2,m)",
     "The signs are chosen so that this operation commutes with multiplication:",
     EXAMPLE {
	  "S = QQ[x_(1,1) .. x_(3,5), y_(1,1) .. y_(5,4)]",
	  "M = transpose genericMatrix(S,x_(1,1),5,3)",
	  "N = transpose genericMatrix(S,y_(1,1),4,5)",
	  "exteriorPower(3,M*N) == exteriorPower(3,M) * exteriorPower(3,N)"
	  },
     "One can also compute the exterior power of more general maps between modules.  See ",
     TO "exteriorPower", " for details and an example.",
     PARA,
     "Macaulay 2 can use two different algorithms to compute determinants:
     the ", TO "Cofactor", " method, which expands a determinant using the standard cofactor
     approach, and ", TO "Bareiss", " which uses a fraction-free variant of Gaussian elimination
     to compute a determinant.  The algorithm to use may be chosen using the optional ",
     TO "Strategy", " argument:",
     EXAMPLE {
	  "m = matrix{{0,a,b},{a+b,a,d},{e,f,g}}",
	  "det(m, Strategy => Cofactor)",
	  "minors(2,m, Strategy => Bareiss)",
	  "exteriorPower(2,m, Strategy => Bareiss)",
	  },
     "One warning is in order here: the Bareiss algorithm requires division in the base ring,
     and so can yield the INCORRECT answer if the base ring contains zero divisors.  However,
     the Bareiss algorithm is often dramatically faster than the cofactor method, unless the
     matrix is particularly sparse.  Consequently, the default strategy for rings which are fields or are
     not quotients of polynomial rings is ", TO "Bareiss", ", while the default for quotients of polynomial
     rings that are not (declared to be) fields is ", TO "Cofactor", ".",
     PARA,
     "Sometimes finer control is needed when one is computing the ideal of minors of a larger
     matrix.  Compute the ideal of some determinants using ", TO "minors", " with optional
     arguments as in",
     EXAMPLE {
	  "M = genericMatrix(R,a,3,3);",
	  "minors(2,M,First => {{0,1},{1,2}}, Limit => 3)"
	  },
     "The argument to the optional argument ", TO "First", " is the list of row and column positions
     to use for the first minor.  Starting at this first minor, we then compute three minors."
     }
     
     
document { "manipulating matrices",
     "Some simple information about matrices:",
     MENU {
	  TO (degree,Matrix),
	  TO (degrees,Matrix),
	  TO (entries,Matrix),
	  TO (isHomogeneous,Matrix),
	  TO (ring,Matrix),
	  TO (source,Matrix),
	  TO (target,Matrix),
     	  TO (trace,Matrix),
	  TO (symbol _, Matrix, Sequence),
	  TO (symbol ==, Matrix, ZZ),
	  },
     "Some common algebraic operations on matrices:",
     MENU {
	  TO (symbol +,Matrix,Matrix),
	  TO (symbol -,Matrix),
	  TO (symbol -,Matrix,Matrix),
	  TO (symbol *,Matrix,Matrix),
	  TO (symbol ^,Matrix,ZZ),
	  TO (symbol ==,Matrix,Matrix),
	  TO (symbol **,Matrix,Matrix),
	  TO (symbol //,Matrix,Matrix),
	  TO (symbol %,Matrix,Matrix),
	  TO (symbol ++,Matrix,Matrix),
	  TO (symbol |,Matrix,Matrix),
	  TO (symbol ||,Matrix,Matrix),
	  TO (symbol ^,Matrix,List),
	  TO (symbol _,Matrix,List),
     	  TO (sortColumns,Matrix),
	  TO (submatrix,Matrix,List,List),
	  TO (substitute,Matrix,List),
	  TO (substitute,Matrix,Matrix),
	  TO (transpose,Matrix),
	  },
     "Some computations on matrices:",
     MENU {
	  TO (exteriorPower,ZZ,Matrix),
	  TO (isInjective,Matrix),
	  TO (isSurjective,Matrix),
	  TO (isIsomorphism,Matrix),
	  TO (pfaffians,ZZ,Matrix),
	  TO (rank,Matrix),
	  TO (symmetricPower,ZZ,Matrix),
	  },
     }

document { "computations",
     Headline => "an overview",
     "In this section we present an overview of the major computations.",
     MENU {
	  TO "Groebner bases",
	  TO "computing Groebner bases",
	  TO "computing syzygies",
	  TO "computing resolutions",
	  }
     }

document { "Groebner bases",
     "Computations in a quotient ring R/I of a polynomial ring R can be done with
     the aid of a Groebner basis for the ideal I.  One must begin by choosing
     a way to order the monomials of R, so that each polynomial has a designated
     leading term.  A Groebner basis is then a generating set for I whose leading
     terms generate the ideal of leading terms of all the elements of I.  See
     ", TO "polynomial rings with other monomial orderings", " for information about
     setting up the ordering of the monomials.",
     PARA,
     "Many routines in Macaulay 2 will compute and use Groebner bases silently, so
     ordinarily the user will not have to explicitly compute them.  Recomputation 
     is avoided, in that if a Groebner basis has already been partially
     computed, then it will be used whenever possible.  Occasionally, the
     user will need to exercise closer control over the computations, and
     may need to compute the Groebner bases directly.",
     PARA,
     "To demonstrate, we set up a polynomial ring.  (The default ordering used here
     of the monomials is the graded reverse lexicographic ordering.)",
     EXAMPLE {
	  "R = ZZ/101[a..d];",
      	  "I = ideal (a^2*b-c^2, a*b^2-d^3, c^5-d)",
	  },
     "The Groebner basis can be obtained with ", TO "gb", ".",
     EXAMPLE {
	  "gb I",
	  },
     "The result obtained above is an object of class ", TO "GroebnerBasis", "
     that encapsulates the basis, the state of the computation, in case
     it's incomplete, and several associated matrices.",
     PARA,
     "The basis can be assembled into a matrix with ", TO "generators", ", or its
     abbreviation, ", TO "gens", ".",
     EXAMPLE {
      	  "transpose generators gb I",
	  },
     "We transposed the matrix above, simply because column vectors of polynomials 
     tend to fit on the page more easily than row vectors.",
     PARA,
     "Here is another example, which illustrates the use of another monomial
     ordering convenient for the elimination of variables.",
     EXAMPLE {
	  "A = ZZ/101[t];",
	  "f = t^3 + t^2 + 1;",
	  "g = t^4 - t;"
	  },
     "We wish to find a polynomial relation among ", TT "f", " and ", TT "g", ",
     so we set up a new ring with a monomial ordering tailored for the
     elimination of its first variable.  (See ", TO "Eliminate", ").",
     EXAMPLE {
	  "B = ZZ/101[t,F,G,MonomialOrder => Eliminate 1];",
	  "I = ideal(F - (t^3 + t^2 + 1), G - (t^4 - t))",
	  "transpose gens gb I",
	  },
     "The entry in the matrix above not involving ", TT "t", " is the desired relation.",
     PARA,
     "Computations in a quotient module ", TT "M/N", " can be done with the aid of a
     Groebner basis for the submodule ", TT "N", ".",
     EXAMPLE {
	  "clearAll",
	  "R = ZZ/101[a..f];",
	  "N = image matrix {{a,b,c},{d,e,f}}",
	  "gb N",
	  },
     "Notice the appearance of the 2 by 2 minors.",
     PARA,
     "Groebner bases can be computed in various types of rings: polynomial rings 
     over fields, skew-commutative-polynomial rings, Weyl algebras, and quotient rings of
     any such rings.  Groebner bases in rings over the integers can be computed
     for homogeneous ideals.  In the future Groebner bases will be provided
     for nonhomogeneous ideals in rings over the integers, in polynomial rings
     over polynomial rings, and in general noncommutative algebras."
     }

     
     
     

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2"
-- End:
