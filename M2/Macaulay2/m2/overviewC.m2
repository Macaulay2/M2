----------- File Mike is Working on! -------------------------

document { "basic rings",
     "The following rings are initially present in every session with
     Macaulay 2.",
     SHIELD MENU { 
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

document { "finite fields",
     -- Also include: getting the variable, its equation.
     -- Current restrictions on p, p^n.
     -- example should include: making these, simple arithmetic
     -- Pointer to finite fields II.
     MENU {
	  TO "ZZ/p",
	  TO "GF(p^n)"
	  },
     "Create a finite field with q = p^n elements using",
     EXAMPLE "F = GF(81,Variable=>a)",
     "This creates the ring of characteristic 3, having 3^4 = 81 elements.  The elements
     of this ring are 0, a, a^2, a^3, ..., a^80.",
     EXAMPLE {
	  "a^80",
	  "a^40"
	  },
     "Note that, except for 0 and 1, every element is displayed as a power 
     of the variable ", TT "a", 
     ".  Use ", TO "ambient", " to see the quotient ring the field is made from.",
     EXAMPLE "ambient F",
     "Now check that ", TT "a", " satisfies this equation.",
     EXAMPLE "a^4 + a - 1",
     "It is often preferable to view elements of ", TT "F", " as polynomials
     in ", TT "a", " rather than as powers of ", TT "a", ".  This can be accomplished
     by lifting the elements back to this ambient ring.",
     EXAMPLE {
	  "lift(a^20, ambient F)",
	  "apply({20,40,80}, i -> lift(a^i, ambient F))",
	  },
     "(for more details on lift, see , ", TO "working with multiple rings", ").",
     PARA,
     "Finite fields can be used as base rings for polynomial rings.",
     EXAMPLE {
	  "R = F[x,y,z]",
      	  "f = random(2,R)",
	  "f = (leadCoefficient f)^(-1) * f"
	  },
     "Groebner bases, and all related computations work in these rings.",
     PARA,
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


document { "polynomial rings",
     "Create a polynomial ring using the usual mathematical notation.",
     EXAMPLE {
	  "R = QQ[x,y,z];",
          "R",
	  },
     "Notice that after assignment to a global variable, Macaulay 2
     knows the ring's name, and this name is used when printing the ring.",
     "The original description of the ring can be recovered
     with ", TO "describe", ".",
     EXAMPLE "describe R",
     "Use the following subscript notation to obtain 0,1, or any multiple of 1,
     as elements in the ring.",
     EXAMPLE {
	  "0_R",
      	  "1_R",
      	  "11_R",
	  },
     "Obtain the variables (generators) of the ring by subscripting the name of 
     the ring.  As always in Macaulay 2, indexing starts at 0.",
     EXAMPLE "R_0^10+R_1^3+R_2",
     "The number of variables is provided by ", TO "numgens", ".",
     EXAMPLE {
	  "numgens R",
      	  "apply(numgens R, i -> R_i^i)",
      	  "sum(numgens R, i -> R_i^i)",
	  },
     "(for more information, see ", TO "apply", " and ", TO "sum", ".",

     "Use ", TO "generators", " to obtain a list of the variables of the ring.",
     EXAMPLE "gens R",
     "A (one row) matrix containing the variables of the ring can be obtained
     using ", TO (vars,Ring), ".",
     EXAMPLE "vars R",
     "The ", TO "index", " of a variable:",
     EXAMPLE {
	  "index x, index y, index z",
	  },

     "The coefficient ring can be recovered with ", TO "coefficientRing", ".",
     EXAMPLE "coefficientRing R",

     "A random homogeneous element can be obtained with ", TO "random", ".",
     EXAMPLE "random(2,R)",

     "A basis of the subspace of ring elements of a given degree can be obtained
     in matrix form with ", TO "basis", ".",
     EXAMPLE "basis(2,R)",

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
     "There are various other ways to specify the variables in a polynomial
     ring.  A sequence of variables can be obtained as follows.",
     EXAMPLE "ZZ[b..k];",
     "In this example, if you had previously assigned either b or k a value that
     was not a ring generator, then Macaulay 2 would complain about this: it would
     no longer understand what variables you wanted.  To get around this, we could
     either do",
     EXAMPLE "ZZ[symbol b .. symbol k];",
     "or we may obtain the single-letter variables with ", TO "vars", ".",
     EXAMPLE {
	  "vars (0..4)",
      	  "ZZ[vars (0..4),vars(26..30),vars 51]",
	  },
     "Subscripted variables can be used, provided the base for the subscripted
     variable has not been used for something else.",
     EXAMPLE "ZZ[t,p_0,p_1,q_0,q_1];",
     "Sequences of subscripted variables can also be used.",
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
     PARA,NOINDENT,
     "Some things to watch out for when using polynomial rings:",
     MENU {
	  ("Defining a ring twice gives different rings, as far as Macaulay 2 is concerned:
     	       We use the strict comparison operator ", TO "===", " to	    
     	       demonstrate this.",     
     	       EXAMPLE "ZZ[a,b,c] === ZZ[a,b,c]",
     	       "Thus it is a good idea to assign a new ring to a variable for
     	       future reference."),
	  ("Variables in monomials are compacted into a smaller space in the machine, for
	       efficiency reasons.  If your exponents will be larger than 127, then use
	       the ", TO "MonomialSize", "  option to increase the amount of space.",
	       EXAMPLE {
		    "R = QQ[a..d];",
		    "a^127",
		    "S = QQ[a..d,MonomialSize=>16];",
		    "a^32767"
		    },
	       "The value 16 is the largest value that you may currently give.  Giving the value
	       k to MonomialSize, allows exponents with maximum value 2^(k-1)-1."),
	  ("Polynomial rings whose coefficient rings are polynomial rings can be very useful
	       for organizing and extracting coefficients easily, but currently most computations
	       cannot be done for these rings. This includes Groebner bases, and therefore all
	       of the applications of Groebner bases.")
	       }
     }

document { "monomial orderings",
     -- Defining the orders
     -- MonomialOrder option.  Currently: Weights option.
     -- Philosophy: each ring comes equipped with a monomial order.
     -- ProductOrder, Eliminate, Lex, RevLex, GRevLex...
     -- What are these order in free modules.  Point to the Schreyer order
     -- information, either in the manual, or in the overview.
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

document { "quasi- and multi-graded polynomial rings",
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

document { "quotient rings",
     -- R/I.  GB of I is needed for arithmetic.
     -- The variables get set?  Doing a quotient ring twice: ie. R/I, then R/I
     -- gives DIFFERENT rings.  Pointer to working with multiple rings.
     "The usual notation is used to form quotient rings.  For quotients of
     polynomial rings, a Groebner basis is computed
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

document { "manipulating polynomials",
     MENU {
	  TO "+",
	  TO "-",
	  TO "*",
	  TO "^",
	  TO "//",
	  TO "%",
	  TO "terms",
	  TO "diff",
	  TO "f _ ZZ",
	  TO "f _ monomial",
	  TO "listForm f",
	  TO "degree f",
	  TO "homogenize",
	  TO "exponents f",
	  TO "leadCoefficient f",
	  TO "leadTerm f",
	  TO "size f",
	  },
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


document { "factoring polynomials",
     "Polynomials can be factored with ", TO "factor", ".  Factorization
     works in polynomial rings over prime finite fields, ZZ, or QQ.",
     EXAMPLE {
	  "R = ZZ/10007[a,b];",
	  "f = (2*a+3)^4 + 5",
	  "g = (2*a+b+1)^3",
	  "S = factor f",
      	  "T = factor g",
	  },
     PARA,
     "The results have been packaged for easy viewing.  The number of
     factors is obtained using",
     EXAMPLE "#T",
     "Each factor is represented as a power (exponents equal
     to 1 don't appear in the display.)  The parts can be 
     extracted with ", TO "#", ".",
     EXAMPLE {
	  "T#0",
      	  "T#0#0",
      	  "T#0#1",
	  }
     }

document { "finite fields, part II",
     -- options to GF command.  Other forms of the GF command.
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
     "Note that computations, such as Groebner bases, over fraction fields can be quite slow."
     }

document { "finite field extensions",
     MENU {
	  TO "toField",
	  -- writeup under "toField" is a good start,
	  -- needs an example
	  }
     }

document { "exterior algebras",
     -- making one, making quotients,
     -- using it.
     -- modules are right-modules, example of multiplication.
     "An exterior algebra is a polynomial ring where multiplication is
     mildly non-commutative, in that, for every x and y in the ring,
     y*x = (-1)^(deg(x) deg(y)) x*y, and that for every x of odd degree,
     x*x = 0.",
     "In Macaulay 2, deg(x) is the degree of x, or the first degree of x, in case 
     a multi-graded ring is being used.  The default degree for each variable is 1, so
     in this case, y*x = -x*y, if x and y are variables in the ring.",
     PARA,
     "Create an exterior algebra with explicit generators by creating a polynomial
     ring with the option ", TO "SkewCommutative", ".",
     EXAMPLE {
	  "R = QQ[x,y,z, SkewCommutative => true]",
      	  "y*x",
      	  "(x+y+z)^2",
      	  "basis R",
      	  "basis(2,R)",
	  },
     EXAMPLE {
	  "S = QQ[a,b,r,s,t, SkewCommutative=>true, Degrees=>{2,2,1,1,1}];",
	  "r*a == a*r",
	  "a*a",
	  "f = a*r+b*s; f^2",
	  "basis(2,S)",
	  },
     "All modules over exterior algebras are right modules.  This means that matrices 
     multiply from the opposite side:",
     EXAMPLE {
	  "x*y",
	  "matrix{{x}} * matrix{{y}}"
	  },
     "You may compute Groebner bases, syzygies, and form quotient rings of these skew
     commutative rings."
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

document { "tensor products of rings",
     -- **, tensor.  Options for changing monomial orders.
     -- What is the default monomial order
     -- What if the names of the variables clash.
     -- The tensor product of two quotients of poly rings is
     -- a quotient of another polynomial ring.
     "The operator ", TO "**", " or the function ", TO "tensor", " can be
     used to construct tensor products of rings.",
     EXAMPLE "ZZ/101[x,y]/(x^2-y^2) ** ZZ/101[a,b]/(a^3+b^3)",
     "Other monomial orderings can be specified.",
     EXAMPLE "T = tensor(ZZ/101[x,y], ZZ/101[a,b], MonomialOrder => Eliminate 2)",
     "The options to ", TT "tensor", " can be discovered with ", TO "options", ".",
     EXAMPLE "options tensor",
     "Given two (quotients of) polynomial rings, say, R = A[x1, ..., xn]/I, S = A[y1,...,yn]/J,
     then R ** S = A[x1,...,xn,y1, ..., yn]/(I + J).  The variables in the two rings are
     always considered as different.  If they have name conflicts, you may still use
     the variables with indexing, but the display will be confusing:",
     EXAMPLE {
	  "R = QQ[x,y]/(x^3-y^2);",
	  "T = R ** R",
	  "generators T",
	  "{T_0 + T_1, T_0 + T_2}"
	  },
     "We can change the variable names with the ", TO "Variables", " option.",
     EXAMPLE {
	  "U = tensor(R,R,Variables => {x,y,x',y'})",
	  "x + y + x' + y'"
	  }
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
	  "R = QQ[x,y,dx,dy,t,WeylAlgebra => {x=>dx, y=>dy}]",
	  "dx*dy*x*y",
	  "dx*x^5"
	  },
     "All modules over Weyl algebras are, in Macaulay 2, right modules.  This means that 
     multiplication of matrices is from the opposite side:",
     EXAMPLE {
	  "dx*x",
	  "matrix{{dx}} * matrix{{x}}"
	  },
     "All Groebner basis and related computations work over this ring.  For an extensive
     collection of D-module routines (A D-module is a module over a Weyl algebra), see ",
     TO "D-modules", "."
     }

document { "Schur rings",
     "Given a positive integer ", TT "n", ", 
     we may define a polynomial ring over ", TO "ZZ", " in ", TT "n", " variables, whose
     monomials correspond to the irreducible representations of GL(n), and where 
     multiplication is given by the decomposition of the tensor product of representations",
     PARA,
     "We create such a ring in Macaulay 2 using the ", TO "Schur", " function:",
     EXAMPLE "R = Schur 4;",
     "A monomial represents the irreducible representation with a given highest weight. 
     The standard 4 dimensional representation is",
     EXAMPLE "V = R_{1}",
     "We may see the dimension of the corresponding irreducible representation using ", TO "dim",
     ":",
     EXAMPLE "dim V",
     "The third symmetric power of V is obtained by",
     EXAMPLE "W = R_{3}",
     EXAMPLE "dim W",
     "and the third exterior power of V can be obtained using",
     EXAMPLE "U = R_{1,1,1}",
     EXAMPLE "dim U",
     "Multiplication of elements corresponds to tensor product of representations.  The 
     value is computed using a variant of the Littlewood-Richardson rule.",
     EXAMPLE "V * V",
     EXAMPLE "V^3",
     "One cannot make quotients of this ring, and Groebner bases and related computations
     do not work, but I'm not sure what they would mean..."
     }

document { "associative algebras",
     "Eventually we will implement associative algebras, not necessarily
     commutative."
     }

///
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
///


-------------------
-- module nodes ---
-------------------

document { "construction of free modules",
     "We use ", TO (symbol ^,Ring,ZZ), " to make a new free module.",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[x,y,z];",
	  "M = R^4"
	  },
     "Such modules are often made as a side effect when creating matrices,
     to serve as the source and target of the corresponding homomorphism.",
     PARA,
     "For graded free modules, and finding the degrees of the 
     generators, see ", TO "graded modules", ".",
     }

document { "matrices to and from modules",
     TOC {
	  SECTION { "matrices to modules (kernel, image, cokernel)",
     	       "Let's make a matrix.",
     	       EXAMPLE {
	  	    "R = ZZ/101[a..c];",
	  	    "f = vars R",
	  	    },
     	       "We can easily compute a ", TO "kernel", ", ", TO "image", "
     	       or ", TT "cokernel", ".",
     	       EXAMPLE {
	  	    "ker f",
	  	    "coker f",
	  	    "image f",
	  	    },
	       },
	  SECTION { "modules to matrices",
	       "gens, mingens, presentation"
	       }
     }
}

document { "Hilbert functions and free resolutions",
     "In this section, we give examples of common operations
     involving modules.  Throughout this section, we suppose that the base
     ring ", TT "R", " is graded, with each variable having degree one, and that  ",
     TT "M", " is a graded ", TT "R", "-module.  If the ring is not graded, or is multi-graded,
     or if ", TT "M", " is not graded, some of these functions still work, but
     care must be taken in interpreting the output.  Here, we just consider the
     standard grading case.",
     TOC {
	  SECTION { "checking homogeniety",
	       "Let's start by making a module over a ring with 18 variables",
	       EXAMPLE {
		    "R = ZZ/32003[vars(0..17)];",
		    "M = coker genericMatrix(R,a,3,6)"
		    },
	       "Use ", TO "isHomogeneous", " to check whether a given module is
	       graded.",
	       EXAMPLE "isHomogeneous M"
	       },
	  SECTION { "codimension, degree, and sectional arithmetic genera",
	       "Use ", TO (codim,Module), ", ", TO (degree,Module), ", and ", TO (genera,Module), " for some basic 
	       numeric information about a module.",
	       EXAMPLE {
		    "codim M",
		    "degree M",
		    "genera M"
		    },
	       "The last number in the list of genera is the degree minus one.  The second to last
	       number is the genus of the generic linear section curve, ..., and the first
	       number is the arithmetic genus",
	       },
	  SECTION { "the Hilbert series",
	       "The Hilbert series (", TO (hilbertSeries, Module), ") of ", TT "M", " is by definition the formal power series ",
	       TT "H(t) = sum(d in ZZ) dim(M_d) t^d", ".  This is a rational function with 
	       denominator ", TT "(1-t)^n", ", where ", TT "n", " is the number of variables
	       in the polynomial ring.  The numerator of this rational function is called
	       the poincare polynomial, and is obtained by the ", TO (poincare,Module), " function.",
	       EXAMPLE "poincare M",
	       EXAMPLE "hilbertSeries M",
	       "Notice that the variable is written as ", TT "$T", ".  This indicates that
	       the variable cannot be typed in directly.",
	       PARA,
	       "It is often useful to divide the poincare polynomial by ", TT "(1-t)", " as many
	       times as possible.  This can be done by the following function:",
	       EXAMPLE {
		    "poincare' = (M) -> (
	H := poincare M;
	t := (ring H)_0;  -- The variable t above
	while H % (1-t) == 0 do H = H // (1-t);
	H)",
                    "poincare' M",
		    }
	       },
	  SECTION { "free resolutions",
	       "The minimal free resolution ", TT "C", " is computed using ", TO (resolution,Module), ".  
	       The specific matrices are obtained by indexing ", TT "C.dd", ".",
	       EXAMPLE {
		    "C = resolution M",
		    "C.dd_3"
		    },
	       "For more information about chain complexes and resolutions, see ", TO "chain complexes",
	       " and ", TO "computing resolutions", "."
	       },
	  SECTION { "betti numbers",
	       "Use ", TO (betti,ChainComplex), " to display the graded betti numbers of ", TT "M", ".",
	       EXAMPLE "betti C",
	       "This table should be interpreted as follows: the number in the ", 
	       TT "i", "th row and ", TT "j", "th column (indices starting at 0),
	       is the number of ", TT "j", "th syzygies in degree ", TT "i+j", ".
	       In the above example, there are 15 second syzygies of degree 4, and the entries
	       of the maps ",
	       TT "CC.d_1, CC.d_3, CC.d_4", " are all linear."
	       }
	  }
     }

document { "operations on modules",
     }

document { "homomorphisms (maps) between modules",
     }

document { "subquotient modules",
     }


document { "extracting elements",
     "If M is an R-module, the best way to think of an element v of M
     in Macaulay 2 is as a map of the ring into M, mapping 1 to v."
     }

document { "equality and containment of modules",
     "==, isSubset"
     }

document { "minimal presentations and generators",
     "prune, trim"
     }

document { "annihilator of a module",
     "The annihilator of a module M over a ring R, ann(M) = { f in R | fM = 0 }, is computed
     using the ", TO "annihilator", " function.",
     EXAMPLE {
	  "R = QQ[a..i];",
	  "M = cokernel genericMatrix(R,a,3,3)",
	  "annihilator M"
	  },
     "You may also use the abbreviation ", TO "ann",
     EXAMPLE {
	  "ann (M/(a*M))"
	  }
     }

document { "constructing maps between modules",
     "The standard way to define a map from an R-module M to an 
     R-module N is to give a matrix whose columns are the image vectors
     of the generators of M.",
     EXAMPLE {
	  "R = QQ[x,y,z];",
	  "m = cokernel vars R",
	  "--F = map(m/m^2, R^1/m, {{x*y*z}})"
	  }
     }

document { "information about a map of modules",
     "usual information: source, target, ring.",
     TOC {
     	  }
     }


document { "kernel, cokernel and image of a map of modules",
     }

document { "degree and multiplicity of a module",
     }

document { "Hilbert functions and polynomials",
     }

document { "homogenization",
     }

document { "truncation and homogeneous components of a graded module",
     }

document { "what is a subquotient module?",
     "There are two basic types of modules over a ring R: submodules of R^n
     and quotients of R^n.  Macaulay 2's notion of a module includes both
     of these.  Macaulay 2 represents every module as a quotient image(f)/image(g),
     where f and g are both homomorphisms from free modules to F: 
     f : F --> G, and g : H --> G.  The columns of f represent the generators of
     M, and the columns of g represent the relations of the module M.",
     EXAMPLE {
	  "R = ZZ/32003[a,b,c,d,e];",
	  },
     "Include here: generators, relations."


     }

document { "extracting parts of a subquotient module",
     "Include: "
     }

document { "quotients of modules",
     }

document { "direct sums of modules",
     }

document { "exterior power of a module",
     }

document { "Fitting ideals",
     }

document { "adjoints of maps",
     }

document { "free resolutions",
     }

document { "Hom module",
     }

document { "tensor products of modules",
     }

document { "Tor and Ext",
     }

-------------------
-- GB nodes -------
-------------------


document { "monomial orderings v1.0",
     "This section is only valid for Macaulay2, versions 1.0 and higher.",
     PARA,
     "Each ring in Macaulay2 comes equipped with an ordering on the
monomials.  This orering is used in the display and storing of polynomials.
The choice of ordering can make a difference in the time taken in various
computations.  Groebner bases performed on ideals and modules will use the
chosen monomial ordering.",
     PARA,
     "The default is to use the graded lexicographic order.  This order is defined 
     as follows: x^A > x^B "
     }


document { "what is a Groebner basis?",
     "A Groebner basis is a specific generating set
     of an ideal or submodule over a polynomial ring, not usually minimal, 
     which has extremely nice properties, from which 
     it is reasonably easy to extract information about the ideal or submodule.",
     "We first define and describe Groebner bases in the important special case
     of an ideal in a polynomial ring.  We then
     describe Groebner bases of submodules, and over more general rings.",
     PARA,
     TEX "Let $R = k[x_1, ..., x_n]$ be a polynomial ring, over a field k,
     and let I \\subset R be an ideal.  A term order on R is, by definition, a total
     order, >,  on the monomials of R, which satsifies two conditions: (1) 
     m > 1, for every monomial m \\neq 1, and (2) the order is multiplicative:
     m > n implies that mp > np, for all monomials m,n,p.",
     PARA,
     "In Macaulay 2, each ring has a multiplicative order associated with it.
     The default is the graded reverse lexicographic order:",
     EXAMPLE "R = QQ[a..d,MonomialOrder=>GRevLex]",
     EXAMPLE "F = a^3 + d^2 + a*d + b*c + 1",
     EXAMPLE "-- R = QQ[a..d,MonomialOrder=>RevLex] -- THIS FAILS",
     EXAMPLE "substitute(F,R)",
     EXAMPLE "R = QQ[a..d,MonomialOrder=>Lex]",
     EXAMPLE "substitute(F,R)",
     EXAMPLE "R = QQ[a..d,Weights=>{1,1,0,0}]",
     EXAMPLE "substitute(F,R)",
     EXAMPLE "R = QQ[a..d,Weights=>{-1,0,0,0}]",
     EXAMPLE "substitute(F,R)",
     EXAMPLE "R = QQ[a..d,Weights=>{-1,-1,-1,-1}]",
     EXAMPLE "substitute(F,R)",
     EXAMPLE "R = QQ[a..d,MonomialOrder=>ProductOrder{1,3}]",
     EXAMPLE "substitute(F,R)",

     "Given a term order, the lead monomial is the term whose monomial is greatest
     in this order.",
     EXAMPLE "leadTerm F"          
     }

document { "finding a Groebner basis",
     }

document { "elimination of variables",
     }

document { "Hilbert functions",
     }

document { "syzygies",
     }

document { "saturation",
     }

document { "fibers of maps",
     }

document { "solving systems of polynomial equations",
     }

/// 
Plan for the next node:
-- groebner basis object
-- getting it, 'snapshot'
-- information about a GB computation:
--   verbose
--   summary
-- using the Hilbert function
-- computing only partial gbs
-- tricks which might help
--   change monomial order
--   homogenize
--   compact monomials
--   remove linear equations, and the corresponding variable.
--   computing up to a given degree
///
document { "fine control of a Groebner basis computation",
     "Sometimes a Groebner basis computation doesn't finish quickly enough.  If so
     then this section might be of use. THIS PAGE IS UNDER CONSTRUCTION.",
     TOC {
	  SECTION { "Partially computed Groebner bases",
	       "Suppose that you have computed part of a Groebner basis.  For
	       example, you may have interrupted the computation using CTRL-C 
	       (typing 'c' while holding the CTRL key down, in emacs, you have to 
	       type CTRL-C twice), or you may have given options requesting only
	       partial computation.",
     	       EXAMPLE "R = ZZ/32003[a..e];",
	       EXAMPLE "I = ideal(random(3,R),random(3,R),random(3,R))",
	       EXAMPLE "gens gb(I,PairLimit=>7);",
	       "Get the Groebner basis object:",
	       EXAMPLE "g = gb(I,StopBeforeComputation => true);",
	       EXAMPLE "leadTerm gens g",
	       "We can make a Groebner basis snapshot by using StopBeforeComputation:",
	       EXAMPLE "gbSnapshot = (I) -> gens gb(I,StopBeforeComputation => true);",
	       EXAMPLE "leadTerm gbSnapshot(I)"
	       }
--	  SECTION { ""
--	       }
	 }
     }

TEST ///
-- document these routines DO THIS

-- Create a free module with an induced (Schreyer) order
Ring ^ Matrix := (R,m) -> (sendgg(ggPush m, ggfree); new Module from R)
-- schreyerMatrix F -- DO THIS

leadTerm(ZZ,RingElement) := (n,f) -> (leadTerm(n,matrix{{f}}))_(0,0)
  -- leadTerm should call a ggleadterm routine?  DO THIS
  
gbSnapshot = (obj) -> (m := gens gb(obj,StopBeforeComputation => true);
     map(target m, source m, entries m))
     -- PROBLEM: have 'gens gb' return a snapshot matrix: not the live one.
     -- same with syz, change...
     -- DO THIS
     
installHilbertFunction = method()
installHilbertFunction(Module,RingElement) := (M,hf) -> (
     -- we need to place hf into the degree ring of M.
     hf = substitute(hf,degreesRing M);
     M.poincare = hf;
     )

installGroebner = method()
-- DO THIS

gbRemove = method()
gbRemove Module := (M) -> remove((generators M).cache, {false,0})
gbRemove Ideal := (I) -> remove((generators I).cache, {false,0})
  -- PROBLEM: what about the other GB
  
R = QQ[a..d,Weights=>{-1,0,0,0}]
f = a+b^2+c^3-2*d^4+1+a*b*c*d
leadTerm f
leadCoefficient f
leadTerm(1,f)

M = image vars R
gbSnapshot(M)
gb(M,PairLimit=>2)
m1 = gbSnapshot(M)
--gb(M,PairLimit=>4)
--m1  -- This has changed!  We probably don't want that
    -- BUG: segmentation fault!!


///
