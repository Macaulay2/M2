undocumented {
     (symbol _, ZZ, EngineRing),
     (symbol _, EngineRing, ZZ),
     (symbol _, RR, EngineRing),
     (symbol _, RRi, EngineRing),
     (symbol _, ZZ, Monoid),
     (symbol _, Vector, ZZ),
     (symbol _, Monoid, List),
     (symbol _, Symbol, Monoid),
     (symbol _, IndexedVariable, Monoid),
     (symbol _, Pseudocode, ZZ),
     (symbol _, RingElement, Monoid),
     (symbol _, MonoidElement, Ring),
     (symbol _, RingElement, Thing),
     (symbol _, MonoidElement, Thing),
     (symbol _, MonoidElement, Monoid),
     (symbol _, RingElement, MonoidElement),
     (symbol _, PolynomialRing, List),
     (symbol _, RingElement, RingElement), --coeff of monomials in polynomial -- deprecate or obsolete
     (symbol _, Ring, String), -- these three are kept for backwards compatibility
     (symbol _, Ring, Symbol), -- maybe should be deprecated?
     (symbol _, Ring, IndexedVariable) -- this should be deprecated?
     }

document {
     Key => symbol _,
     Headline => "a binary operator, used for subscripting and access to elements"
     }

document { 
     Key => (symbol _, VisibleList, ZZ),
     Headline => "get element from list",
     Usage => "x_i",
     Inputs => { "x", "i" },
     Outputs => { { "the ", TT "i", "-th element of the list ", TT "x" }},
     PARA{
     	  "The entries of the list, sequence or array are numbered starting with 0.  If  ", TT "i", " 
          is negative, then the ", TT "i", "-th entry counting from the end is provided.
          If ", TT "i", " is out of range, an error is signaled."  },
     EXAMPLE lines ///
     	  x = a..t
	  #x
	  x_-1, x_19, x_0
	  ///,
     Caveat => { "Lists are immutable objects, i.e. you cannot assign to them.  See ", 
	   TO MutableList, " for lists that you may modify."},
     SeeAlso => {(symbol #, BasicList, ZZ)},
     }
document {
     Key => (symbol _, VisibleList, List),
     Headline => "get some entries of a list",
     Usage => "w_{i,j,...}",
     Inputs => {
	  "w",
	  Nothing => { TT "{i,j,...}", ", a list of integers to serve as indices" }
	  },
     Outputs => {{ "the list of entries ", TT "{w_i, w_j, ...}" }},
     PARA{
     	  "The entries of ", TT "w", " are numbered starting with 0.  If  ", TT "i", " 
          is negative, then the ", TT "i", "-th entry counting from the end is provided.
          If ", TT "i", " is out of range, an error is signaled."  },
     EXAMPLE lines ///
	  w = {a,b,c,d,e,f,g,h}
      	  w_{1,3,4}
	  w_{-1,-2,-3}
	  ///,
     PARA{},
     "We can use this operation to compute composition of permutations represented as lists.",
     EXAMPLE "{4,2,3,1,0} _ {2,1,3,4,0}",
     PARA{},
     "Remark: any subscripts that are sequences will have their elements spliced into the rest of the list.",
     EXAMPLE "{a,b,c,d,e}_{2..4}",
     SeeAlso => {"lists and sequences", (symbol _, VisibleList, ZZ)}
     }

document { 
     Key => (symbol _, String, ZZ),
     Headline => "get element from string",
     Usage => "s_i",
     Inputs => { "s", "i" },
     Outputs => { { "the ", TT "i", "-th character of the string ", TT "s" }},
     PARA{
     	  "The characters are numbered starting with 0.  If  ", TT "i", " 
          is negative, then the ", TT "i", "-th entry counting from the end is provided.
          If ", TT "i", " is out of range, an error is signaled."  },
     EXAMPLE lines ///
     	  s = "what are you thinking?"
	  #s
	  s_-1, s_19, s_0
	  ///,
     SeeAlso => {"strings and nets",(symbol #, String, ZZ)},
     }
document { 
     Key => (symbol _, String, Sequence),
     Headline => "substring",
     Usage => "s_(i,j)",
     Inputs => {"s",
	  Nothing => {TT "(i,j)", ", a pair of integers"}
	  },
     Outputs => {
	  String => {"the substring of s having length j and starting at the i-th character"}
	  },
     EXAMPLE lines ///
     	  s = "I did not do that!"
	  "." | s_(6,7) | "."
	  ///,
     SeeAlso => {"strings and nets", substring, (symbol|,String,String)}
     }
document { 
     Key => (symbol _, Partition, ZZ),        
     Headline => "get element",
     Usage => "p_i",
     Inputs => {
	  "p", "i"
	  },
     Outputs => { ZZ => { "the ", TT "i", "-th element of the partition ", TT "p" }},
     EXAMPLE lines ///
     	  partitions 3
	  p = new Partition from {2,1}
	  p_0
     	  w = toList p
	  ///,
     SeeAlso => {"lists and sequences", partitions, toList}
     }
document { 
    Key => {
	"get a ring variable by index",
	(symbol _, Ring, ZZ),
	(symbol _, Monoid, ZZ),
    },
     Headline => "get a ring variable by index",
     Usage => "R_i",
     Inputs => {
	  "R", "i"
	  },
     Outputs => {
     	  RingElement => {"the ", TT "i", "-th generator of the ring ", TT "R"},
	  },
     "The indexing of generators is based on 0, so ", TT "R_0", " would be
     the first one, and so on.",
     EXAMPLE lines ///
     	  R = ZZ[a..d]
	  R_2
	  ///,
     PARA{},
     "If the coefficient ring is a polynomial ring, then the
     numbering matches that in the list returned by ", TO "generators", ".", 
     EXAMPLE lines ///
	  S = R[x,y,z]
	  generators(S,CoefficientRing=>ZZ)
	  S_2
	  S_6
	  ///,
     SeeAlso => {generators, (symbol _, String, Ring)}
     }
document { 
    Key => {
	"get a ring variable by name",
	(symbol _, String, Ring),
	(symbol _, String, Monoid),
    },
     Headline => "get a ring variable by name",
     Usage => ///"x"_R///,
     Inputs => {
	  "R",
	  Nothing => {TT ///"x"///, ", ", ofClass String}
	  },
    Outputs => {
	  RingElement => {"the variable of ", TT "R", " whose name is ", TT "x"}
	  },
     EXAMPLE lines ///
     	  R = ZZ[x,y,z];
	  use R;
     	  S = ZZ[x,t];
	  x
	  ///,
     "The symbol ", TT "x", " now refers to the variable ", TT "x", " in the ring ", TT "S", ".
     There are several ways of now referring to ", TT "x", " in the ring ", TT "R", ".",
     EXAMPLE lines ///
	  R_0
	  "x"_R
	  use R;
	  x
	  ///,
     SeeAlso => { (symbol _, Ring, ZZ), (use, Ring) },
     Subnodes => {
	 TO (symbol _, Symbol, Ring),
	 TO (symbol _, IndexedVariable, Ring),
         }
     }
document { 
     Key => (symbol _, Symbol, Ring),
     Headline => "get a ring variable by name",
     Usage => ///x_R///,
     Inputs => {
	  "x" => {"or ", ofClass RingElement},
	  "R",
	  },
    Outputs => {
	  RingElement => {"the variable of ", TT "R", " whose name is ", TT "x"}
	  },
     EXAMPLE lines ///
     	  R = ZZ[x,y,z];
	  use R;
     	  S = ZZ[x,t];
	  x
	  ///,
     "The symbol ", TT "x", " now refers to the variable ", TT "x", " in the ring ", TT "S", ".
     There are various ways of now referring to ", TT "x", " in the ring ", TT "R", ".",
     EXAMPLE lines ///
	  R_0
	  symbol x
	  oo_R
	  use R;
	  x
	  ///,
     SeeAlso => {(symbol _, Ring, ZZ), (symbol _, String, Ring), (use,Ring)}
     }
document { 
     Key => {
	 "get a monomial by exponent vector",
	 (symbol _, Ring, List),
     },
     Headline => "make a monomial from a list of exponents",
     Usage => "R_w",
     Inputs => {
	  "R",
	  "w" => "of integers"
	  },
     Outputs => {
	  {"the monomial of the ring ", TT "R", " obtained by using the 
     integers in the list ", TT "w", " as exponents of the variables."},
	  },
     EXAMPLE lines ///
          R = ZZ[a..d]
	  R_{3,1,5}
	  R_{1,1,1,1}
	  S = R[x,y,z]
	  S_{1,1,1}
	  S_{1,1,1,4}
	  ///,
     }
document { 
     Key => (symbol _, IndexedVariable, Ring), -- ring variable by name
     Headline => "get a ring variable by name",
     Usage => "v_R",
     Inputs => {
	  "v",
	  "R"
	  },
     Outputs => {
	  RingElement => {"the variable of ", TT "R", " whose name is the indexed variable ", TT "v"}
	  },
     EXAMPLE lines ///
     R = QQ[t_1..t_4]
     symbol t_1
     oo_R
     ///,
     SeeAlso => {(symbol _, Ring , ZZ), (symbol _, String, Ring)}
     }

document { 
     Key => {"generators of rings, ideals, and modules",
	  (symbol _, Ideal, ZZ),
	  (symbol _, Module, ZZ),
	  (symbol _, Matrix, ZZ),
	  (symbol _*, Ring),
	  (symbol _*, Ideal),
	  (symbol _*, Module),
	  (symbol _*, Monoid),
      },
     Headline => "",
     SYNOPSIS {
	  Heading => "get a single generator of a ring, ideal, or module",
     	  Usage => "L_i",
     	  Inputs => {
	       "L" => ofClass{Ring,Ideal,Module,Matrix},
	       "i" => ZZ
	       },
     	  Outputs => {
	       {ofClass{RingElement,Vector}, " the ", TT "i", "-th generator or column of ", TT "L"}
	       },
	  },
     SYNOPSIS {
	  Heading => "get the list of generators of a ring, ideal, or module",
	  Usage => "M_*",
	  Inputs => { "M" => ofClass{Ring,Ideal,Module} },
	  Outputs => { List }
	  },
     	  "As usual in Macaulay2, the first generator has index zero.",
	  PARA{},
     	  EXAMPLE lines ///
	       R = QQ[a..d];
	       numgens R
	       R_2
	       R_*
	       I = ideal(a^3, b^3-c^3, a^4, a*c);
	       numgens I
	       I_0, I_2
	       I_*
	  ///,
	  PARA{},
	  "Notice that the generators are the ones provided.  Alternatively we can
	  minimalize the set of generators.",
	  EXAMPLE lines ///
	       J = trim I
	       J_0
	  ///,
	  PARA{},
	  "Elements of modules are useful for producing submodules or quotients.",
	  EXAMPLE lines ///
	       M = cokernel matrix{{a,b},{c,d}}
	       M_0
	       M_*
	       M/M_0
	       N = M/(a*M + R*M_0)
     	       N_0 == 0_N
	       ///,
	  "Columns of matrices may also be used as vectors in the target module.",
	  EXAMPLE lines ///
	       M = matrix{{a,b,c},{c,d,a},{a-1,b-3,c-13}}
	       M_0
	       prune((image M_{1,2})/(R*M_1))
	       ///,
     	  Caveat => {"Fewer methods exist for manipulating vectors 
	       than other types, such as modules and matrices"},
     	  SeeAlso => {}
     }
document { 
     Key => {(symbol _, Matrix, Sequence),
	  (symbol _, MutableMatrix, Sequence)},
     Headline => "get entry of matrix",
     Usage => "M_(i,j)",
     Inputs => {
	  "M" => {"or ", ofClass MutableMatrix},
	  "i" => "both integers"
	  },
     Outputs => {
	  RingElement => "the (i,j)-th entry of the matrix M, where M_(0,0) is the 
	   top left entry"
	  },
     "All indexing in Macaulay2 is zero-based, so the indices start at zero.",
     EXAMPLE lines ///
     	  M = matrix{{1,2,3},{0,5,6}}
	  M_(1,2)
	  ///,
     "The matrix may be a ", ofClass MutableMatrix, " too:",
     EXAMPLE lines ///
	  N = mutableMatrix M
	  N_(1,0)
          ///,
     "Entries of mutable matrices can be changed:",
     EXAMPLE lines ///
	  N_(1,0) = 37
	  N
	  ///,
     SeeAlso => {Matrix, MutableMatrix}
     }

document {
     Key => (symbol _,Function,Thing),
     Headline => "attach the first argument to a function of two or more arguments",
     Usage => "g = f_x",
     Inputs => {
	  "f" => Function => "a function of two or more arguments",
	  "x" => Thing
	  },
     Outputs => {
	  "g" => Function => {
	       "a new function with the property that ", TT "g(y)", "
	       returns the value of  ", TT "f(x,y)", ", that
	       ", TT "g(y,z)", " returns the value of ", TT "f(x,y,z)", ", and
	       so on.  If ", TT "x", " is itself a sequence, its members will be spliced
	       into the sequence of arguments of ", TT "f", "."
	       }
	  },
     PARA {
     	  "This abbreviation allows us to save a bit of typing, and in some
     	  cases, agrees with standard mathematical notation."
	  },
     PARA {
	  "We use the identity function in the following examples, so we can exactly what
	  sequence of arguments is constructed."
	  },
     EXAMPLE lines ///
     identity_a x
     identity_a (x,y)
     identity_(a,b) x
     identity_(a,b) (x,y)
     ///,
     PARA {
	  "In the following examples, we show more typical uses of this notation."
	  },
     EXAMPLE lines ///
     R = ZZ[a .. i];
     f = genericMatrix(R,a,3,3)
     exteriorPower(2,f)
     exteriorPower_2 f
     p = prepend_7
     p {8,9,10}
     ///
     }

document {
     Key => {(symbol _, Module, List), (symbol _, Ideal, List)},
     Headline => "map from free module to some generators",
     Usage => "M_p",
     Inputs => {
	  "M" => {"or ", ofClass Ideal},
	  "p" => "of integers"
	  },
     Outputs => {
	  "f" => { "a map from a free module to the module ", TT "M", "
	       which sends the basis vectors to the generators of ", TT "M", "
     	       whose index numbers are listed."
	       }
	  },
     "If ", TT "M", " is an ideal, then the map maps to ", TT "module M", ".",
     EXAMPLE lines ///
	  R = QQ[x,y,z]
	  I = ideal vars R
	  f = I_{0,2}
	  image f
	  M = image syz vars R
	  g = M_{1}
	  source g
	  target g
	  ///,
     SeeAlso => { (module, Ideal), (symbol _, Module, ZZ) }
     }

document { 
     Key => {(symbol _, ZZ, Module)},
     Headline => "get the zero vector in a module",
     Usage => "0_M",
     TT "0_M", " provides the zero element of the module ", TT "M", ".",
     PARA{
	  "The return value is either ", ofClass RingElement, ", or ", ofClass Vector, "."
	  },
     EXAMPLE lines ///
	  0_(ZZ^3)
	  ///
     }

document {
     Key => {(symbol _,Module,Array)},
     Headline => "inclusion from summand",
     Usage => "M_[i,j,...,k]",
     Inputs => {"M" => {ofClass Module},
	  Nothing => {TT "[i,j,...,k]", ", an array of indices"}},
     Outputs => {
     	  Nothing => {ofClass Matrix}
	  },
     PARA{},
     "The module or chain complex ", TT "M", " should be a direct sum, and the result is the map
     corresponding to inclusion from the sum of the components numbered or named
     ", TT "i, j, ..., k", ".  Free modules are regarded as direct sums of modules.",
     PARA{},
     EXAMPLE lines ///
	  M = ZZ^2 ++ ZZ^3
      	  M_[0]
      	  M_[1]
      	  M_[1,0]
	  ///,
     PARA{},
     "If the components have been given names (see ", TO directSum, "), use those instead.",
     EXAMPLE lines ///
	  R = QQ[a..d];
	  M = (a => image vars R) ++ (b => coker vars R)
	  M_[a]
	  isWellDefined oo
	  M_[b]
	  isWellDefined oo
	  ///,
     PARA{},
     "This works the same way for chain complexes.",
     EXAMPLE lines ///
	  C = res coker vars R
	  D = (a=>C) ++ (b=>C)
	  D_[a]
	  ///,
     SeeAlso => {directSum, (symbol ^,Matrix,Array), (symbol ^,Module,Array),(symbol _,Module,List)}
     }

document { 
     Key => {
	  (symbol _, Matrix, Array),
	  },
     Headline => "component of map corresponding to summand of source",
     Usage => "F_[i,j,...,k]",
     Inputs => {"F",
	  Nothing => {TT "[i,j,...,k]", ", an array of indices"}},
     Outputs => {
     	  Nothing => ofClass{Matrix}
	  },
     "The source of the matrix ", TT "F", " should be a 
     direct sum, and the result is the component of this map 
     corresponding to the sum of the components numbered or named
     ", TT "i, j, ..., k", ".  Free modules are regarded as direct sums of modules.
     In otherwords, this routine returns the map given by certain blocks of columns.",
     EXAMPLE lines ///
          R = ZZ[a..d];
          F = (vars R) ++ ((vars R) ++ matrix{{a-1,b-3}})
	  F_[1]
	  F_[1]^[1]
          ///,
     PARA{"If the components have been given names (see ", TO directSum, "), use those instead."},
     EXAMPLE lines ///
	  N = (a=>vars R) ++ (b=>vars R)
	  N_[a]
     	  N = directSum(x1 => matrix{{a,b-1}}, x2 => matrix{{a-3,b-17,c-35}}, x3 => vars R)
	  N_[x1,x3]
	  ///,
     PARA {"This works the same way for maps between chain complexes."},
     SeeAlso => {(symbol^,Matrix,Array),(symbol_,Module,Array),directSum}
     }

document { 
     Key => (symbol_,Symbol,Thing),
     Headline => "index variable",
     Usage => "s_v",
     Inputs => {
	  "s", "v"
	  },
     Outputs => {
	  IndexedVariable
	  },
     "The subscript can be anything, even a mutable object.  Often, the subscripts are numbers or sequences.
     Sequences usually print nicer than lists.",
     EXAMPLE lines ///
          x_(1,3)
	  x_{1,3}
          ///,
     "Indexed variables can be used for ring variables.",
     EXAMPLE lines ///
	  R = ZZ[x_(1,1)..x_(2,3),y_a..y_f]
	  gens R
	  ///
     }
