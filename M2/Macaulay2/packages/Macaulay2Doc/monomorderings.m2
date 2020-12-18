-- -*- coding: utf-8 -*-
----------- File Irena is Working on!  Taken from Mike.  ---------------------


document {
     Key => "polynomial rings",
     "Create a polynomial ring using the usual mathematical notation.",
     EXAMPLE {
	  "R = QQ[x,y,z];",
          "R",
	  },
     "Notice that after assignment to a global variable, Macaulay2
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
     the ring.  As always in Macaulay2, indexing starts at 0.",
     EXAMPLE "R_0^10+R_1^3+R_2",
     "The number of variables is provided by ", TO "numgens", ".",
     EXAMPLE {
	  "numgens R",
      	  "apply(numgens R, i -> R_i^i)",
      	  "sum(numgens R, i -> R_i^i)"
	  },
     "(See ", TO "apply", " and ", TO "sum", ".)  ",
     "Use ", TO "generators", " to obtain a list of the variables of the ring.",
     EXAMPLE "gens R",
     "A matrix (with one row) containing the variables of the ring can be obtained
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
     was not a ring generator, then Macaulay2 would complain about this: it would
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
     "Polynomial rings over polynomial rings work:",
     EXAMPLE lines ///
     R = QQ[a,b][x]
     (a+b+x)^3
     ///,
     PARA {
	  "Internally, the polynomials in such towers are expressed in terms of a flattened monoid
	  containing all the variables, obtainable with the key ", TO "FlatMonoid", "."
	  },
     EXAMPLE lines ///
     R.FlatMonoid
     ///,
     "Some things to watch out for when using polynomial rings:",
     UL {
	  LI ("Defining a ring twice gives different rings, as far as Macaulay2 is concerned:
     	       We use the strict comparison operator ", TO "===", " to demonstrate this.",     
     	       EXAMPLE "ZZ[a,b,c] === ZZ[a,b,c]",
     	       "Thus it is a good idea to assign a new ring to a variable for future reference."
	       )
       	  },
     SeeAlso => {"heft vectors", "division in polynomial rings with monomials less than 1"}
     }

document {
     Key => FlatMonoid,
     Usage => "R.FlatMonoid",
     Inputs => {
	  "R" => PolynomialRing
	  },
     Outputs => {
	  GeneralOrderedMonoid => { "the flattened monoid in terms of which the polynomials 
	       are expressed when the coefficient ring of R is itself a polynomial ring"
	       }
	  },
     EXAMPLE lines ///
     R = QQ[a,b][x]
     R.FlatMonoid
     ///,
     SeeAlso => { flattenRing }
     }

document {
     Key => "monomial orderings", 
     "Every polynomial ring in Macaulay2 comes equipped with an ordering on
     the monomials.  See below for the definitions of all implemented
     orderings.  The default ordering is GRevLex, the graded reverse
     lexicographic order.",
     PARA{},
     "Polynomials are displayed by ordering the monomials in decreasing order.
     The choice of monomial order can make a difference in the
     time and space required for various computations,
     especially Gröbner basis computations.",
     PARA{},
     Subnodes => {
	  TO "examples of specifying alternate monomial orders",
	  TO "monomial orders for free modules",
	  TO "packing monomials for efficiency",
      "Definitions of the specific monomial orders",
	  TO "GRevLex",
	  TO "Lex",
	  TO "GLex",
	  TO "Weights",
	  TO "Eliminate",
	  TO "GroupLex",
	  TO "GroupRevLex",
	  TO "definition of product (block) orders",
	  TO "RevLex",
	  TO "NCLex",
      "A succinct summary",
          TO "MonomialOrder",
      "Developers' corner",
	  TO "obtaining the monomial order of a ring"
	  },
     SeeAlso => { "QthPower::weightGrevlex", "QthPower::grevlexWeight" }
     }

document {
     Key => "examples of specifying alternate monomial orders",
     "For definitions of these monomial orders, see ", 
     TO "GRevLex", ", ",
     TO "Lex", ", ",
     TO "Weights", ", ",
     TO "Eliminate", ", ",
     TO "GroupLex", ", ",
     TO "GroupRevLex", ", ",
     -- Mike wanted this: TO "product orders", ", ",
     TO "RevLex", ", and ",
     TO "NCLex", ".",
     HEADER2 "Graded reverse lexicographic order",
     EXAMPLE {
	  "R = ZZ[a..d];",
	  "a+b^100+c*d"
	  },
     HEADER2 "Lexicographic order",
     EXAMPLE {
	  "R = ZZ[a..d, MonomialOrder=>Lex];",
	  "a+b^100+c*d"
	  },
     HEADER2 "Weight order",
     EXAMPLE {
	  "R = ZZ[a..d, MonomialOrder => Weights => {201,2}];",
	  "a+b^100+c*d"
	  },
     HEADER2 "Graded lexicographic order",
     EXAMPLE {
	  "R = ZZ[a..d, MonomialOrder=>{Weights=>4:1,Lex}];",
	  "a+b^100+c*d"
	  },
     HEADER2 "Elimination order",
     "To use an elimination order, which eliminates the first 2 variables,
     use",
     EXAMPLE {
	  "R = ZZ[a..f, MonomialOrder=>Eliminate 2];",
	  "a+b^100+c*d"
	  },
     "Alternatively, use a weight vector",
     EXAMPLE {
	  "R = ZZ[a..f, MonomialOrder=>Weights=>2:1];",
	  "a+b^100+c*d"
	  },
     HEADER2 "Product (block) order",
     "To make a product order where each block has the GRevLex order:",
     EXAMPLE {
	  "R = ZZ[a..f, MonomialOrder=>{2,4}];",
	  "a^2*(c+d) + b*(c^100+d^100)*(c + e + f)"
	  },
     "The orders in each block can be other orders as well.",
     EXAMPLE {
	  "R = ZZ[a..f, MonomialOrder=>{Weights=>2:1,Lex}]",
	  "a^2*(c+d) + b*(c^100+d^100)*(c + e + f)"
	  },
     HEADER2 "GroupLex",
     "This order is useful for making degree rings, and allows some
     variables to appear with negative exponent.",
     EXAMPLE {
	  "R = ZZ[a..f, MonomialOrder => GroupLex => 3];",
	  "a^-2*(c+d) + b*(c^100+d^100)*(c + e + f)"
	  },
     HEADER2 "GroupRevLex",
     "This order is useful for making degree rings, and allows some
     variables to appear with negative exponent.  Not implemented yet.",
     HEADER2 "RevLex",
     "Warning: this is a local ordering, not a global ordering.",
     EXAMPLE {
	  "R = ZZ[a..f, MonomialOrder=>RevLex, Global=>false];",
	  "a^2*(c+d) + b*(c^100+d^100)*(c + e + f)"
	  },
     HEADER2 "NCLex",
     "For non-commutative Gröbner bases.  Not implemented yet.",
     SeeAlso => {"Singular Book 1.2.13"}
     }

document {
     Key => "packing monomials for efficiency",
     "Sometimes for efficiency reasons, it is important to pack exponent vectors 
     several exponents per machine word.  Polynomials take less space, and monomial 
     operations such as comparison and multiplication become faster.",
     PARA{},
     "The monomial order keys ", TO "Lex", " and ", TO "GRevLex", " allow packing.  The ",
     TT "MonomialSize => n", " option allows one to set the minimum packing size, in number of bits.
     Monomials are stored as signed exponent vectors, so maximum exponents of 2^(n-1)-1 are possible
     for packed variables.  Useful values include 8, 16, 32, and (on 64-bit machines) 64.  The default
     monomial size is 32.",
     EXAMPLE lines ///
     	  A = QQ[a..d,MonomialSize=>8]
     	  B = QQ[x,y,z,w,MonomialSize=>16,MonomialOrder=>Lex]	  
	  ///,
     "The maximum degree for monomials in A is 127.  Monomials of higher degree
     will encounter a monomial overflow.  In the second example, the maximum exponent
     is 32767 (2^15-1).",
     PARA{},
     "It is possible to pack different parts of the monomial with different sizes.
     For example, the following order has two blocks: a graded reverse lexicographic block of 3 variables,
     packed into one 32-bit word, and a second lexicographic block for 4 variables, taking 4 32-bit words.
     Each monomial will be packed into 5 32-bit words (on a computer with a 32-bit word size).",
     EXAMPLE lines ///
     	  C = QQ[a,b,c,x,y,z,w,MonomialOrder=>{MonomialSize=>8,3,MonomialSize=>32,Lex=>4}];
          ///,
     PARA{},
     EXAMPLE {
	  "D = QQ[a..d,MonomialOrder=>Lex];",
	  "a^1000000000",
	  },
     PARA{},     
     "This exponent would give a monomial overflow error in the next two rings.",
     EXAMPLE lines ///
	  E = QQ[a..d,MonomialSize=>16,MonomialOrder=>Lex];
  	  F = QQ[a..d,MonomialSize=>8,MonomialOrder=>Lex];
	  ///
     }

document { 
     Key => "monomial orders for free modules",
     TEX ///In Macaulay2, each free module $F = R^s$ over a ring $R$ has a basis
     of unit column vectors $F_0, F_1, ..., F_(s-1)$.  The monomials of $F$
     are the elements $m F_i$, where $m$ is a monomial of the ring $R$.
     In Macaulay2, orders on the monomials of $F$ are used for computing Gröbner bases and
     syzygies, and also to determine the initial, or lead term of elements of $F$.///,
     PARA{},
     TEX ///The ring $R$ comes equipped with a total order on the monomials of $R$.
     A total order on the monomials of $F$ is called {\bf compatible} (with the order
     on $R$), if $m F_i > n F_i$ (in $F$) whenever $m > n$ (in $R$). There are many types of
     compatible orders, but several stand out: term over position up (the default in Macaulay2), 
     term over position down,
     position up over term, position down over term, and Schreyer orders.///,
     PARA{},
     TEX "term over position up:   $m F_i > n F_j$ iff $m>n$ or $m==n$ and $i>j$",
     PARA{},
     TEX "term over position down: $m F_i > n F_j$ iff $m>n$ or $m==n$ and $i<j$",
     PARA{},
     TEX "position up over term:   $m F_i > n F_j$ iff $i>j$ or $i==j$ and $m>n$",
     PARA{},
     TEX "position down over term: $m F_i > n F_j$ iff $i<j$ or $i==j$ and $m>n$",
     PARA{},
     "Induced monomial orders are another class of important orders on ", TT "F", ", see ",
     TO "Schreyer orders", " for their definition and use in Macaulay2.",
     PARA{},
     "In Macaulay2, free modules come equipped with a compatible order.  The default
     order is: term over position up.
     This is called Position=>Up.  In the following example, the lead term is ",
     TEX "$a F_1$, since $a > b$.",
     EXAMPLE {
	  "R = ZZ[a..d];",
	  "F = R^3",
	  "f = b*F_0 + a*F_1",
	  "leadTerm f"
	  },
     "This is the same as giving the monomial order as:",
     EXAMPLE {
	  "R = ZZ[a..d, MonomialOrder => {GRevLex => 4, Position => Up}];",
	  "F = R^3",
	  "leadTerm(a*F_0 + a*F_1)"
	  },
     "Giving Position=>Down instead switches the test above to i < j.  In this case the 
     monomial order on F is:
     m*F_i > n*F_j if m>n or m==n and i<j.",
     EXAMPLE {
	  "R = ZZ[a..d, MonomialOrder => {GRevLex => 4, Position => Down}];",
	  "F = R^3",
	  "leadTerm(a*F_0 + a*F_1)"
	  },
     "If one gives Position=>Up or Position=>Down earlier, then the position will be 
     taken into account earlier. For example",
     EXAMPLE {
	  "R = ZZ[a..d, MonomialOrder => {GRevLex => 2, Position => Down, GRevLex => 2}];",
	  "F = R^3",
	  "leadTerm(a*F_0 + a*F_1)",
	  "leadTerm(b*F_0 + c^4*F_1)",	  
	  "leadTerm(c*F_0 + d^2*F_1)"	  
	  },
     "If one wants Position over Term (POT), place the Position element first",
     EXAMPLE {
	  "R = ZZ[a..d, MonomialOrder => {Position => Down}];",
	  "F = R^3",
	  "leadTerm(a*F_0 + a*F_1)",
	  "leadTerm(b*F_0 + c^4*F_1)",	  
	  "leadTerm(c*F_0 + d^2*F_1)"	  
	  },
     Subnodes => {
	  TO "Schreyer orders"
	  }
     }
     
document {
     Key => "Schreyer orders",
     Headline => "induced monomial order on a free module",
     "The Schreyer order is a monomial order on a free module that is particularly
     efficient for computing Gröbner bases and syzygies.  The size of Gröbner bases
     of submodules using such orders is often much much smaller than if a position over term
     or term over position order would be used.  We call these Schreyer orders, after
     Frank-Olaf Schreyer, who used them to give an algorithm for syzygies, and who also
     recognized many of their beneficial properties.  See [S1] and [S2] for the algorithm,
     and [LS] for improvements and details on the implementation in Macaulay2",
     BR{},
     UL {
	 LI {"[LS] ", EM "Strategies for computing minimal free resolutions.", "(R. LaScala and M. Stillman, J. Symb. Comp. 26, 409-431, 1998).\n"},
	 LI {"[S1] ", EM "Die Berechnung von Syzygien mit dem verallgemeinerten Weierstrassschen Divisionssatz.", "(F.-O. Schreyer, Diplomarbeit, Hamburg, 1980).\n"},
	 LI {"[S2] ", EM "A standard basis approach to syzygies of canonical curves.", "F.-O. Schreyer, J. reine angew. Math. 421, 83-123 (1991)"}},
     PARA{},
     TEX /// Given a free $R$-module $G$, a set of monomials $m_0, \ldots, m_{s-1}$ of $G$,
     and a monomial order on the monomials of $G$, the induced order, or, Schreyer
     order on $F = R^s$ is defined by:
     $a F_i > b F_j$ (in $F$) iff $a m_i > b m_j$ (in $G$), or $a m_i and b m_j$
     are scalar multiples of each other, and $i>j$, where $F_i$ are the unit column vectors of $F$.
     Typically the monomials $m_0, \ldots, m_{s-1}$ are the initial monomials of a Gröbner basis
     of a submodule of $G$.
     ///,
     PARA{},
     "In Macaulay2, free modules with a Schreyer order on them can be created using ", 
     TO (schreyerOrder,Matrix), ".",
     EXAMPLE lines ///
	  R = ZZ/101[a..f];
	  m = matrix{{a,b,c,d}};
	  m1 = schreyerOrder m
	  F = source m1
	  g = syz m1
	  leadTerm g
	  ///,
     "In Macaulay2, free modules are displayed without any indication of whether they are
     endowed with a Schreyer order or not.  To determine whether one is, use ", 
     TO (schreyerOrder,Module), ".  If the result is the zero matrix, then the monomial order
     associated with this free module is not a Schreyer order.  In that case, the monomial order
     for the free module is the one determined directly from the ring.",
     EXAMPLE lines ///
	  schreyerOrder target m
	  schreyerOrder source g
	  ///,
     TEX "Over quotient rings, the multiplications $a m_i$ and $b m_j$ are over the ambient polynomial
     ring, not the quotient.",
     PARA{},
     "It is fine for the free module ", TT "G", " above to be endowed with a Schreyer order too.",
     PARA{},
     "The only places that Schreyer orders are considered is in computation of Gröbner bases,
     syzygies, and free resolutions, and with the ", TO leadTerm, " routine.",
     PARA{},
     "The size of the Gröbner bases of syzygy modules is often dramatically smaller if
     the monomial order is the Schreyer order, as in the following example.",
     EXAMPLE lines ///
          R = QQ[a..f];
     	  I = ideal"abc-def,a2c-d2f,aef-bcd,a3-d3,af2-cd2"
	  F = syz gens I
	  betti gens gb syz F
	  G = schreyerOrder F
	  betti gens gb syz G	  
     ///,
     SeeAlso => {
	  leadTerm,
	  (schreyerOrder,Matrix),
	  (schreyerOrder,Module),
	  gb,
	  syz,
	  resolution,
	  betti
	  }
     }

document {
     Key => RevLex,
     Headline => "reverse lexicographic ordering",
     "The reverse lexicographic order is defined by: $x^A > x^B$ if
     the FIRST non-zero entry of the vector of integers ", TT "A-B", " is NEGATIVE.
     This is a local order, not a global order.  Therefore Gröbner bases over this
     ring only give generators over the local ring whose fractions are all elements 
     not in the ideal generated by the variables.",
     EXAMPLE {
	  "R = QQ[a..d,MonomialOrder => RevLex, Global => false];",
	  "a^3 + b^2 + b*c + a*c^2 + b^2*c + a + b + c",
	  },
     "Computations of Gröbner bases for local orders are done using Mora's algorithm.",
     SeeAlso => {GRevLex,Global}
     }

document {
     Key => GRevLex,
     Headline => "graded reverse lexicographical monomial order.",
     TEX /// The graded reverse lexicographic order is defined by: $x^A > x^B$ if either
     $degree(x^A) > degree(x^B)$ or $degree(x^A) = degree(x^B)$ and
     the LAST non-zero entry of the vector of integers $A-B$ is NEGATIVE. ///,
     PARA{},
     TEX /// This is the default order in Macaulay2, in large part because it is often
     the most efficient order for use with Gröbner bases.  By giving GRevLex
     a list of integers, one may change the definition of the order: $degree(x^A)$ is
     the dot product of $A$ with the argument of GRevLex.///,
     EXAMPLE {
	  "R = QQ[a..d];",
	  "a^3 + b^2 + b*c",
	  "S = QQ[a..d, MonomialOrder => GRevLex => {1,2,3,4}];",
	  "a^3 + b^2 + b*c"
	  },
     "The largest possible exponent of variables in the ", TT "GRevLex",
     " order is 2^31-1.  For efficiency reasons, it is sometimes useful to
     limit the size of monomials (this often makes computations more efficient).",
     "Use ", 
     TT "MonomialSize => 16", ", which allows maximal exponent 2^15-1, 
     or ", TT "MonomialSize => 8", ", which allows maximal exponent 2^7-1.", 
     EXAMPLE {
	  "B1 = QQ[a..d,MonomialSize=>16,MonomialOrder=>GRevLex];",
	  "B = QQ[a..d,MonomialSize=>16];",	  
	  "a^(2^15-1)",
  	  "C = QQ[a..d,MonomialSize=>8,MonomialOrder=>GRevLex];",
	  "try a^(2^15-1) else \"failed\"",
	  "a^(2^7-1)"
	  },
     SeeAlso => {"packing monomials for efficiency"}
     }

document {
     Key => Lex,
     Headline => "lexicographical monomial order.",
     "The lexicographic order is defined by: $x^A > x^B$ if the FIRST
     non-zero entry of the vector of integers ", TT "A-B", " is POSITIVE.",
     EXAMPLE {
	  "R = QQ[a..d, MonomialOrder => Lex];",
	  "a^3 + a^2*b^2 + b*c"
	  },
     "The largest possible exponent of variables in ", TT "Lex",
     " order is 2^31-1.  For efficiency reasons, the size of the exponents
     of variables may be restricted.  Then instead of ", TT "Lex", ", one can use ",
     TT " MonomialSize=>16", ", which allows maximal exponent 2^15-1, 
     or ", TT "MonomialSize=>8", ", which allows maximal exponent 2^7-1.", 
     EXAMPLE {
	  "B = QQ[a..d,MonomialOrder=>Lex,MonomialSize=>16];",
	  "a^(2^15-1)",
  	  "C = QQ[a..d,MonomialOrder=>Lex,MonomialSize=>8];",
	  "try a^(2^15-1) else \"failed\"",
	  "a^(2^7-1)"
	  },
     "Any of these versions of ", TT "Lex", " order may be combined, for example, with
     a weight order given by a weight vector: x^A > x^B if
     weight(x^A) > weight(x^B) or if weight(x^A) = weight(x^B)
     and if the FIRST non-zero entry of the vector of integers A-B is POSITIVE.",
     EXAMPLE {
	  "B = QQ[a..d,MonomialSize=>16,MonomialOrder=>{Weights => {1,2,3,4}, Lex}];",
	  "a^2 + b+ c + b*d"
	  },
     SeeAlso => {Weights}
     }

document {
     Key => GLex,
     Headline => "graded lexicographic ordering",
     "The option ", TT "GLex => n",
     " is a shortcut for ", TT "Weights => n:1, Lex=>n", " in creating a ", TO2("monomial orderings", "monomial ordering"),
     EXAMPLE lines ///
     	  R = QQ[a..d,MonomialOrder=>GLex]
	  a^3*b+a^4+b^100
	  ///,
     SeeAlso => MonomialOrder
     }

document {
     Key => Weights,
     Headline => "assigning weights to the variables",
     "Given a list L of n integers, the weight order on a polynomial ring
     in n variables is defined by: x^A > x^B if A_1 L_1 + ... + A_n L_n
     > B_1 L_1 + ... + B_n L_n.",
     PARA{},
     "The leading component of a polynomial
     under a weight order need not be a monomial.  When two monomials
     have the same weight, by default they are further distinguished
     with the GRevLex order.",
     EXAMPLE {
	  "R = QQ[a..d,MonomialOrder=>{Weights => {-1,2,3,4}},Global=>false];",
	  "f = a^2 + b+ c^2 + b*d",
	  "leadTerm f",
	  },
     "However, we can retrieve the entire leading component with
     the command ", TT "leadTerm(1,f)",
     ". The plain ", TT "leadTerm f", " is in this case the same as ",
     TT "leadTerm(2,f)", ": they both use the full specification of
     the monomial ordering, first by
     weight and then by the ", TT "GRevLex", " order.  In contrast, ",
     TT "leadTerm(1,f)", "only distinguishes monomials by the first,
     i.e., weight, specification.",
     EXAMPLE {
	  "leadTerm(1,ideal(f))"
	  },
     "The weight order may be combined with further ordering elements to break ties.
     In the following example, we use a second weight vector to break
     ties under first weight vector.",
     EXAMPLE {
	  "R = QQ[a..d,MonomialOrder=>{Weights => {1,2,3,4}, Weights => {2,4,2,1}}];",
	  "f = a^6 + b^3+ c^2",
	  "leadTerm(f)",
	  "leadTerm(1, ideal(f))",
	  "leadTerm(2, ideal(f))",
	  "leadTerm(3, ideal(f))",
	  },
     TT "leadTerm(3, ideal(f))", " uses both the specified weights and the ",
     TT "GRevLex", " order to calculate the leading component.",
     PARA{},
     "When the number of weights is smaller than the number of variables,
     the remaining variables are given weight 0.
     In the following example, ", TT "c", " and ", TT "d",
     " have weight 0.",
     EXAMPLE {
	  "R = QQ[a..d,MonomialOrder=>{Weights => {1,2}, Lex}];",
	  "f = a^2 + b+ c^2 + b*d",
	  },
     SeeAlso => {leadTerm}
     }

document {
     Key => "Eliminate",
     Headline => "elimination order",
     "The option ", TT "Eliminate => n",
     " is a shortcut for ", TT "Weights => {n:1}",
     " The remaining variables are given weight 0.",
     "The monomial order is the elimination order eliminating the
     first n variables, refined by the graded reverse lexicographic order.",
     EXAMPLE {
       "R = QQ[a..i, MonomialOrder => Eliminate 3];",
       },
     "This order enables intersections with the subring
     consisting of all but the first 3 variables.
     For this, use the command ", TO "selectInSubring", ".",
     EXAMPLE {
       "I = ideal(a^2, b-f, d^4, i - b);",
       "selectInSubring(1, gens gb I)",
       },
     "Eliminate may be combined with block orders as well.",
     EXAMPLE {
       "R = QQ[a..i, MonomialOrder => {Eliminate 3,4,2}];",
       "d^3 - a*e^4 + b^2*i + a*c*d*f +a*c^2*g + a*c*g"
       },
     "In the last example, the order is block order:
     the first four variables are in the first block, the
     subsequent two variables are in the second block,
     the remaining variables are in the third block,
     and the weights of the variables are 1,1,1,0,..., 0.
     We illustrate the usage of ", TO "selectInSubring", ".",
     EXAMPLE {
         "I = ideal(a..i)",
	 "selectInSubring(1, gens gb I);",
	 "selectInSubring(2, gens gb I);",
	 "selectInSubring(3, gens gb I);",
	 },
     Caveat => "If the number of degree vectors is greater than one, this
     is currently only graded using the first degree vector.  This will 
     eventually change.",
     SeeAlso => {Weights, "definition of product (block) orders",
     	selectInSubring}
     }

document {
     Key => ProductOrder,
     Headline => "product ordering",
     TT "ProductOrder", "{n1, ..., nr} -- an optional argument of
     ", TO "MonomialOrder", " in monoids handled by the ", TO "engine", " to
     indicate that the monomial order is the product of r graded reverse lex
     orders, each with n1, n2, ..., nr variables.",
     PARA{},
     "This syntax is left here for compatibility with previous
     Macaulay2 versions.",
     EXAMPLE {
     	"R = ZZ[a..e, MonomialOrder => ProductOrder {3,2}];"
	},
     "The new syntax is ",
     TT "R = ZZ[a..e, MonomialOrder => {3,2}];", ".",
     SeeAlso => {"definition of product (block) orders"}
     }

document {
     Key => "definition of product (block) orders",
     TT "MonomialOrder => {n_1, ..., n_l}", " divides the variables of the
     ring into ", TT "l", " blocks, the first block consisting of the first ",
     TT "n_1", " variables, the second block consisting of the subsequent ",
     TT "n_2", " variables, and so on.  For each block of variables,
     we can compute the total degree of a monomial with respect to the
     variables in that block.  This gives a length ", TT "l",
     " vector of total degrees for each monomial.  We say
     x^A > x^B if the total degree vector of x^A is lexicographically
     greater than the total degree vector of x^B, or if the two
     total degree vectors are equal and if in the first block of
     variables where A and B differ, A > B in GRevLex order.",
     EXAMPLE {
	  "R = QQ[a..l, MonomialOrder => {3,3,3,3}];",
	  "a*e^3 + a^2*c*i + a*b^2*i + b^2*e*i"
	  },
     "We may replace ", TT "MonomialOrder => {3,3,3,3}",
     " with the shorter ", TT "MonomialOrder => {4:3}",
     PARA{},
     "The default ", TT "GRevLex", " order on any block may be
     changed to other orders, as follows.",
     EXAMPLE {
	     "R = QQ[a..i, MonomialOrder => {Lex =>3,3:1,3}];",
	     "a*e^3 + a^2*c*i + a*b^2*i + b^2*e*i + d^2*f*h + d*e^2*h",
	  },
     "Note: ", TT "Weights", " and ", TT "Eliminate",
     " do not create blocks, they only assign weights to the
     variables.",
     SeeAlso => {Weights, Eliminate}
     }

document {
     Key => "GroupRevLex",
     TT "MonomialOrder => GroupRevLex => n", " inverts the first ",
     TT "n", " variables in the polynomial ring.
     In the following example, ", TT "a^-1", " is in the ring,
     but ", TT "c^-1", " is not.",
     EXAMPLE {
	  "R = QQ[a..d, MonomialOrder=>GroupRevLex=>2, Global=>false];",
	  "a^-1",
	  "try c^(-1) else \"failed\"",
	  },
     Caveat => { "This feature has not been implemented yet."}
     }

document {
     Key => GroupLex,
     Headline => "defines a ring where some variables are inverted",
     TT "MonomialOrder => GroupLex => n", " inverts the first ",
     TT "n", " variables in the polynomial ring.
     In the following example, ", TT "a^-1", " is in the ring,
     but ", TT "c^-1", " is not.",
     EXAMPLE {
	  "R = QQ[a..d, MonomialOrder=> GroupLex=>2];",
	  "a^-1",
	  "try c^(-1) else \"failed\"",
	  },
     Caveat => {"The element ", TT "a/b", " is in the fraction ring,
        while ", TT "a*b^(-1)", " belongs to ", TT "R", ".",
	PARA{},
	"Currently, one cannot compute Gröbner bases in this ring."}
     }

document {
     Key => NCLex,
     Headline => "Non-commutative lexicographical order.",
     "This feature has not been implemented yet."
     }

document {
     Key => "obtaining the monomial order of a ring",
     "The monomial order of a ring is stored as an option.",
     EXAMPLE lines ///
	  R = QQ[x_1 .. x_10, MonomialOrder=>{4,6}];
	  options R
	  (options R).MonomialOrder
	  S = QQ[a..d];
	  (options S).MonomialOrder
     ///
     }

document {
     Key => "graded and multigraded polynomial rings",
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
     "One restriction on degrees of variables is that the entries be small integer values, possibly
     zero or negative.  The notion of small depends on the size of exponents one wants: the degree
     of each monomial occurring should fit in a 32 bit integer (or 64 bit integer, on 64 bit machines).",
     PARA{
	 "Another restriction on degrees, at least if all the computational facilities of Macaulay2 are
	 needed, is that a heft vector exists for them.  A heft vector is a list of integers whose length is
	 the same as the length of the degrees (see ", TO degreeLength, "), such that its dot product with
	 the degree of each variable is positive.  Heft vectors are computed automatically for you,
	 as in the following example, or they may be provided by the user (see ", TO "Heft", ")."
	 },
    EXAMPLE lines ///
	 R = QQ[a,b,c,Degrees=>{{1,0},{-2,1},{-3,1}}];
	 random({1,1},R)
	 basis({1,1},R)
	 ///,
     PARA {
	  "The heft vector computed behind the scenes is available to the user."
	  },
     EXAMPLE lines ///
     (options R).Heft
     ///,
     PARA {
     	  "If the heft vector is not provided, many computations will work (e.g., Gröbner bases and computation of resolutions),
	  but certain other operations (such as ", TT "basis", " and ", TT "random", ") will raise errors."
     	  }
     }

document {
     Key => "graded modules",
     -- Mike must have wanted a node with this name...
     }

document {
     Key => {MonomialOrder,Position,Up,Down,Global},
     Headline => "monomial ordering",
     TT "MonomialOrder", " -- an optional argument used with polynomial rings and monoids
     to indicate a
     monomial ordering other than the default (graded reverse lexicographic).",
     PARA{},
     "In Macaulay2, each polynomial ring (and also each monoid) is equipped with a monomial order,
     which is used for display of polynomials (terms are listed in descending monomial order),
     and also for Gröbner basis computations.",
     PARA{},
     "In the most general setting, a monomial ordering is given by a list of
     ", EM "ordering tests", ", of various types listed and described below, each of which provides a partial ordering
     on the monomials.  The ordering tests are applied sequentially, starting with the first one, until 
     one monomial is judged greater than the other.
     At the end, if necessary, the graded reverse lexicographic order is used to compare the monomials.
     For examples, see below, or see ", TO "monomial orderings", ".",
     PARA{},
     "Permissible elements:",
     UL {
	  (TO "GRevLex", " => n -- A graded reverse lexicographic block of variables"),
	  (TO "Lex", " => n"),
	  (TO "Weights", " => {...}"),
	  (TO "Position", " => Up  or  Position => Down"),
	  (TO "RevLex", " => n"),
     	  (TO "GroupLex", " => n"),
	  (TO "GroupRevLex", " => n"),
	  (TO "MonomialSize", " => n, n being 8,16,32, or 64.  Set the packing size for exponents for further variables")
          },
     PARA{},
     "Some examples of monomial orders.  Note that if only one item is in the list, 
     we can dispense with the list.",
     UL {
	  (TT "MonomialOrder => {GRevLex=>2, GRevLex=>3}", " -- a product order"),
	  (TT "MonomialOrder => {2, 3}", " -- same"),
	  (TT "MonomialOrder => {Weights=>{1,13,6,2}}", " -- a weight order"),
	  (TT "MonomialOrder => Weights=>{1,13,6,2}", " -- same"),
	  },
     "If any monomials will be less than 1 in the ordering, then the option ", TT "Global => false", "
     should be used.",
     EXAMPLE lines ///
     	  QQ[x,y, Weights => {-1,1}, Global => false]
	  x<1
	  y<1
     ///,
     SeeAlso => {"monomial orderings","QthPower::weightGrevlex", "QthPower::grevlexWeight"}}



-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
