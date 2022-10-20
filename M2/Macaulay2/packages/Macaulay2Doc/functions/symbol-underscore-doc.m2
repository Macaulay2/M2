undocumented {
     (symbol _, ZZ, EngineRing),
     (symbol _, EngineRing, ZZ),
     (symbol _, RR, EngineRing),
     (symbol _, RRi, EngineRing),
     (symbol _, ZZ, Monoid),
     (symbol _, Monoid, ZZ),
     (symbol _, Vector, ZZ),
     (symbol _, Monoid, List),
     (symbol _, QuotientRing, String),
     (symbol _, Symbol, Monoid),
     (symbol _, IndexedVariable, Monoid),
     (symbol _, MonoidElement, Monoid),
     (symbol _, RingElement, MonoidElement),
     (symbol _, PolynomialRing, List),
     (symbol _, RingElement, RingElement), --coeff of monomials in polynomial -- deprecate or obsolete
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
     Key => (symbol _, VirtualTally, Thing),
     Headline => "get a count from a tally",
     Usage => "t_x",
     Inputs => {
	  "t","x"
	  },
     Outputs => {
	  ZZ => {"the number of times ", TT "x", " is counted by ", TT "t"}
	  },
     EXAMPLE lines ///
     	  t = tally apply(1..10000, i -> # factor i)
	  t_5
	  t_6
	  ///,
     SeeAlso => {Tally}
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
     Key => (symbol _, Ring, ZZ),             -- ring variable by index
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
     SeeAlso => {generators, (symbol _, Ring, String)}
     }
document { 
     Key => (symbol _, Ring, String), -- ring variable by name
     Headline => "get a ring variable by name",
     Usage => ///R_"x"///,
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
	  R_"x"
	  use R;
	  x
	  ///,
     SeeAlso => {(symbol _, Ring, ZZ), (symbol _, Symbol, Ring), (use,Ring)}
     }
document { 
     Key => {(symbol _, Symbol, Ring) },
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
	  R_"x"
	  use R;
	  x
	  ///,
     SeeAlso => {(symbol _, Ring, ZZ), (symbol _, Ring, String), (use,Ring)}
     }
document { 
     Key => (symbol _, Ring, List),           -- make monomial
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
     SeeAlso => {}
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
     SeeAlso => {(symbol _, Ring , ZZ), (symbol _, Ring, String)}
     }
document { 
     Key => {
	  (symbol _, ChainComplex, ZZ),     -- component
	  (symbol _, GradedModule, ZZ)},     -- component
     Headline => "component",
     Usage => "C_i",
     Inputs => {
	  "C" => {"or ", ofClass GradedModule},
	  "i"
	  },
     Outputs => {
	  Module => { "the ", TT "i", "-th component of ", TT "C"}
	       },
     EXAMPLE lines ///
     	  R = QQ[x,y,z]/(x^3,y^3,z^3,x*y*z);
	  C = res(coker vars R, LengthLimit=>8)
     	  rank C_7
	  C.dd_3
	  ///,
     SUBSECTION "Programming hint",
	  "The chain complex ", TT "C", " is implemented as a hash table, 
	  but since the computation of a projective resolution 
	  can be stopped prematurely, Macaulay2 doesn't bother
	  populating the hash table with the relevant free modules 
	  until explicitly requested by the user, for example, in response to the
	  command ", TT "C_i", " described above.  The hash table ", 
	  TT "C", " can be examined directly with code like ", TT "C#i", ", but in order to populate 
	  the hash table completely, use ", TO (complete, ChainComplex), ".",
     SeeAlso => {resolution,
	  (symbol ^, ChainComplex, ZZ),
	  (symbol _, ChainComplexMap, ZZ)}
     }

document {
     Key => {(symbol _, ChainComplexMap, ZZ),
	  (symbol _, GradedModuleMap, ZZ)},
     Headline => "component map",
     Usage => "p_i",
     Inputs => {
	  "p" => ("a map ", TT "D", " <- ", TT "C", " of chain complexes, of degree ", TT "d", ", say"),
	  "i"
	  },
     Outputs => { Matrix => {"the component ", TT "D_(i+d) <- C_i"} },
     EXAMPLE lines ///
	  R = ZZ/101[a..c];
	  I = image vars R
	  J = image symmetricPower (2,vars R)
	  g = extend( resolution (R^1/I), resolution (R^1/J), id_(R^1))
	  g_1
	  g_2
	  ///,
     "The map ", TT "p", " may also be ", ofClass GradedModuleMap, ".",
     SeeAlso => { (symbol _, ChainComplex, ZZ), extend, resolution, image, vars, symmetricPower }
     }

document { 
     Key => {"generators of ideals and modules",
	  (symbol _, Ideal, ZZ),
	  (symbol _, MonomialIdeal, ZZ),
	  (symbol _, Module, ZZ),
	  (symbol _, Matrix, ZZ)},
     Headline => "",
     SYNOPSIS {
	  Heading => "Synopsis",
     	  Usage => "L_i",
     	  Inputs => {
	       "L" => ofClass{Ideal,MonomialIdeal,Module,Matrix},
	       "i" => ZZ
	       },
     	  Outputs => {
	       {ofClass{RingElement,Vector}, " the ", TT "i", "-th generator or column of ", TT "L"}
	       },
	  },
     	  "As usual in Macaulay2, the first generator has index zero.",
	  PARA{},
     	  EXAMPLE lines ///
	       R = QQ[a..d];
	       I = ideal(a^3, b^3-c^3, a^4, a*c);
	       numgens I
	       I_0, I_2
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
     Headline => "integers or zero element",
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
     Key => ((symbol _,symbol =),ChainComplex,ZZ),
     Headline => "install component of chain complex",
     Usage => "C_i = M",
     Inputs => { "C" , "i", "M" },
     Outputs => {{"install ", TT "M", " as the ", TT "i", "-th module of the chain complex ", TT "C"}},
     EXAMPLE lines ///
	  R = ZZ[x..z]
	  C = chainComplex R
	  C_2 = R^11
	  C_4 = R^13
	  C
     ///,
     SeeAlso => {((symbol _,symbol =),ChainComplexMap,ZZ)}}

document {
     Key => ((symbol _,symbol =),ChainComplexMap,ZZ),
     Headline => "install component of chain complex map",
     Usage => "f_i = g",
     Inputs => { "f" , "i", "g" },
     Outputs => {{"install ", TT "g", " as the ", TT "i", "-th module of the chain complex map ", TT "f"}},
     EXAMPLE lines ///
	  R = ZZ[x..z]
	  C = chainComplex R
	  C.dd
	  C.dd_1 = vars R
	  C.dd_3 = transpose vars R
	  C.dd
	  C
	  HH C
	  prune HH C
     ///,
     SeeAlso => {((symbol _,symbol =),ChainComplex,ZZ)}}

document {
     Key => {(symbol _,Module,Array),
       (symbol _,ChainComplex,Array)},
     Headline => "inclusion from summand",
     Usage => "M_[i,j,...,k]",
     Inputs => {"M" => {"or ", ofClass ChainComplex},
	  Nothing => {TT "[i,j,...,k]", ", an array of indices"}},
     Outputs => {
     	  Nothing => {ofClass Matrix, ", or ", ofClass ChainComplexMap}
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
	  (symbol _, ChainComplexMap, Array),
	  (symbol _, GradedModuleMap, Array)
	  },
     Headline => "component of map corresponding to summand of source",
     Usage => "F_[i,j,...,k]",
     Inputs => {"F" => {"or ", ofClass{ChainComplexMap,GradedModuleMap}},
	  Nothing => {TT "[i,j,...,k]", ", an array of indices"}},
     Outputs => {
     	  Nothing => ofClass{Matrix, ChainComplexMap, GradedModuleMap}
	  },
     "The source of the module or chain complex ", TT "F", " should be a 
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
