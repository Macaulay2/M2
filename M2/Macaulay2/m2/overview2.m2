--		Copyright 1993-1998 by Daniel R. Grayson

///

this is an old node, apparently

document { "ideals and modules",
     Headline => "an overview",
     "In this section we present an overview of ideals and modules.
     For details, see ", TO "Ideal", " and ", TO "Module", ".",
     PARA,
     "The most general module ", TT "M", " is represented as a submodule of a 
     quotient module of a free module ", TT "F", ".  The quotient module is
     presented internally by a matrix whose columns generate the relations, 
     and the submodule is represented internally by a matrix whose columns
     generate the submodule.  The two matrices the same number of rows, namely,
     the rank of ", TT "F", ".",
     MENU {
	  TO "ideals",
	  TO "free modules",
	  TO "making modules from matrices", -- coker, ker, image, etc.
	  TO "manipulating modules",
	  TO "maps between modules",
	  TO "bases of parts of modules",
	  }
     }

///

document { "free modules",
     "We use ", TO (symbol ^,Ring,ZZ), " to make a new free module.",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[x,y,z];",
	  "M = R^4"
	  },
     "Such modules are often made as a side effect when creating matrices,
     to serve as the source and target of the corresponding homomorphism.",
     PARA,
     "When the ring is graded, so are its free modules.  By default,
     the degrees of the basis elements are taken to be 0.",
     EXAMPLE {
	  "degrees M",
	  },
     "We can use ", TO (symbol ^, Ring, List), " to specify other degrees,
     or more precisely, their additive inverses.",
     EXAMPLE {
	  "F = R^{1,4:2,3,3:4}",
      	  "degrees F",
	  },
     "Notice the use of ", TO ":", " above to indicate repetition.",
     PARA,
     "If the variables of the ring have multi-degrees represented by
     lists (vectors) of integers, as described in
     ", TO "multi-graded polynomial rings", ", then the degrees of a
     free module must also be multi-degrees.",
     EXAMPLE {
	  "S = ZZ[a,b,c, Degrees=>{{1,2},{2,0},{3,3}}]",
	  "N = S ^ {{-1,-1},{-4,4},{0,0}}",
	  "degree N_0",
	  "degree (a*b*N_1)",
	  }
     }

document { "making modules from matrices",
     "Let's make some matrices.",
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
     "Every module is represented internally as a submodule of
     a quotient module.  Such modules often appear in computations,
     for example, when taking the direct sum of a quotient module and
     a submodule.",
     EXAMPLE {
	  "image f ++ coker f"
	  },
     "We may use ", TO "subquotient", " to make such modules directly,
     although it's usually more convenient to use other operations.  The
     first argument is the matrix whose columns are the generators, and
     the second argument is the matrix whose columns are the relations.",
     EXAMPLE {
	  "M = subquotient(f, matrix {{a}})",
	  "prune M",
	  }
     }

document { "manipulating modules",
     "Suppose we have a module which is represented as an image of a
     matrix, and we want to represent it as a cokernel of a matrix.
     This task may be accomplished with ", TO "prune", ".",
     EXAMPLE {
	  "R = QQ[x,y];",
	  "I = ideal vars R",
	  "M = image vars R",
	  "N = prune M",
	  },
     "The isomorphism between them may be found under the key
     ", TT "pruningMap", ".",
     EXAMPLE {
	  "f = N.pruningMap",
	  "isIsomorphism f",
	  "f^-1",
	  },
     "The matrix form of ", TT "f", " looks nondescript, but the map
     knows its source and target",
     EXAMPLE {
	  "source f",
	  "target f",
	  },
     "It's a 2 by 2 matrix because ", TT "M", " and ", TT "N", " are
     both represented as modules with two generators.",
     PARA,
     "Functions for finding related modules:",
     SHIELD MENU {
	  TO "ambient",
	  TO "cover",
	  TO "super",
	  },
     EXAMPLE {
	  "super M",
	  "cover N",
	  },
     "Some simple operations on modules:",
     SHIELD MENU {
	  TO (symbol ^, Module, ZZ),
	  TO (symbol ++, Module, Module),
	  TO (symbol **, Module, Module),
	  },
     EXAMPLE {
	  "M ++ N",
	  "M ** N",
	  },
     "Ideals and modules behave differently when making powers:",
     EXAMPLE {
	  "M^3",
	  "I^3"
	  }
     }

document { "maps between modules",			    -- map
     "Maps between free modules are usual specified as matrices, as
     described in the section on ", TO "matrices", ".  In this section 
     we cover a few other techniques.",
     PARA,
     "Let's set up a ring, a matrix, and a free module.",
     EXAMPLE {
	  "R = ZZ/101[x,y,z];",
	  "f = vars R",
	  "M = R^4",
	  },
     "We can use ", TO (symbol ^, Module,List), " and ", TO (symbol _, Module,List), "
     to produce projection maps to quotient modules and injection maps 
     from submodules corresponding to specified basis vectors.",
     EXAMPLE {
     	  "M^{0,1}",
     	  "M_{2,3}",
	  },
     PARA,
     "Natural maps between modules can be obtained with ", TO "map", "; the
     first argument is the desired target, and the second is the source.",
     EXAMPLE {
	  "map(source f, ker f)",
	  "map(coker f, target f)",
	  },
     }

document { "bases of parts of modules",
     "The function ", TO "basis", " can be used to produce bases (over the
     ground field) of parts of modules (and rings) of a specified degree.",
     EXAMPLE {
	  "R = ZZ/101[a..c];",
      	  "basis(2, R)",
	  "M = ideal(a,b,c)/ideal(a^2,b^2,c^2);",
      	  "f = basis(2,M)",
	  },
     "Notice that the matrix of ", TT "f", " is expressed in terms of the
     generators of ", TT "M", ".  The reason is that ", TT "M", " is the target
     of ", TT "f", ", and matrices such as ", TT "f", " are always expressed 
     in terms of the generators of the source and target.",
     EXAMPLE "target f",
     "The command ", TO "super", " is useful for getting around this.",
     EXAMPLE "super f",
     "When a ring is multi-graded, we specify the degree as a list of
     integers.",
     EXAMPLE {
      	  "S = ZZ/101[x,y,z,Degrees=>{{1,3},{1,4},{1,-1}}];",
      	  "basis({7,24}, S)",
	  },
     }


document { "free resolutions of modules",
     "The function ", TO "resolution", " (also called ", TO "res", "),
     can be used to produce a free resolution of a module.",
     EXAMPLE {
	  "R = ZZ/101[x,y];",
	  "m = ideal vars R",
	  "M = m/m^3",
      	  "C = resolution M",
	  },
     "The default display for a chain complex shows the modules and
     the number of the stage at which they appear.  See the
     documentation of ", TO "resolution", " for details on
     the options which can be used to control the computation.",
     PARA,
     "The same function, applied to a map ", TT "f", ", will produce a map
     from a free resolution of the source of ", TT "f", " to a free resolution of
     the target of ", TT "f", ".",
     EXAMPLE {
	  "h = resolution map(M, m^2/m^4)"
	  }
     }

document { "extracting information from chain complexes",
     "Let's make a chain complex.",
     EXAMPLE {
	  "R = ZZ/101[x,y,z];",
      	  "C = res coker matrix {{x,y^2,z^3}};",
	  },
     "Some simple functions for discovering the shape of ", TT "C", ".",
     SHIELD MENU {
	  TO (length, ChainComplex),
	  TO (max, ChainComplex),
	  TO (min, ChainComplex),
	  },
     EXAMPLE {
	  "length C",
	  "max C",
	  "min C",
	  },
     "In order to see the matrices of the differential maps in a
     chain complex, examine ", TT "C.dd", ".",
     EXAMPLE {
      	  "C.dd",
	  },
     "If ", TT "C", " is a chain complex, then ", TT "C_i", " will produce 
     the ", TT "i", "-th module in the complex, ", TT "C^i", " will produce
     the ", TT "-i", "-th module in it, and ", TT "C.dd_i", " will 
     produce the differential whose source is ", TT "C_i", ".",
     EXAMPLE {
	  "C_1",
	  "C^-1",
	  "C.dd_2"
	  },
     "The function ", TO "betti", " can be used to display the ranks of the
     free modules in ", TT "C", ", together with the distribution of the basis 
     elements by degree, at least for resolutions of homgeneous modules.",
     EXAMPLE {
	  "betti C"
	  },
     "The ranks are displayed in the top row, and below that
     in row ", TT "i", " column ", TT "j", " is displayed the number of
     basis elements of degree ", TT "i+j", " in ", TT "C_j", "."
     }

document { "making chain complexes by hand",
     "A new chain complex can be made with ", TT "C = new ChainComplex", ".  This will
     automatically initialize ", TT "C.dd", ", in which the differentials are stored.
     The modules can be installed with statements like ", TT "C#i=M", " and the 
     differentials can be installed with statements like ", TT "C.dd#i=d", ".
     The ring is installed with ", TT "C.ring = R", ".  It's up to the
     user to ensure that the composite of consecutive differential maps is zero.",
     EXAMPLE {
	  "R = QQ[x,y,z];",
	  "d1 = matrix {{x,y}};",
	  },
     "We take care to use ", TO "map", " to ensure that the target of ", TT "d2", " is
     exactly the same as the source of ", TT "d1", ".",
     EXAMPLE {
	  "d2 = map(source d1, ,{{y*z},{-x*z}});",
	  "d1 * d2 == 0",
	  },
     "Now we make the chain complex, as explained above.",
     EXAMPLE {
	  "C = new ChainComplex; C.ring = R;",
	  "C#0 = target d1; C#1 = source d1; C#2 = source d2;",
	  "C.dd#1 = d1; C.dd#2 = d2;",
	  },
     "Our complex is ready to use.",
     EXAMPLE {
	  "C",
	  "HH_0 C",
	  "prune HH_1 C",
	  },
     "The chain complex we've just made is simple, in the sense that it's a homological
     chain complex with nonzero modules in degrees 0, 1, ..., n.  Such a chain
     complex can be made also with ", TO "chainComplex", ".  It goes to a bit
     of extra trouble to adjust the differentials to match the degrees of the
     basis elements.",
     EXAMPLE {
	  "D = chainComplex(matrix{{x,y}}, matrix {{y*z},{-x*z}})",
	  "degrees source D.dd_2",
	  }
     }

document { "manipulating chain complexes",
     "There are several natural ways to handle chain complexes; for
     details, see ", TO "ChainComplex", ".  Let's illustrate by
     making two chain complexes.",
     EXAMPLE {
	  "R = QQ[x,y];",
	  "M = coker vars R",
	  "N = coker matrix {{x}}",
	  "C = res M",
	  "D = res N",
	  },
     "We can form the direct sum as follows.",
     EXAMPLE {
	  "C ++ D"
	  },
     "We can shift the degree, using the traditional notation.",
     EXAMPLE {
	  "E = C[5]",
	  "E_-4 == C_1"
	  },
     "The same syntax can be used to make a chain complex from a
     single module.",
     EXAMPLE {
	  "R^4[1]"
	  },
     "We can form various tensor products with ", TO "**", ", and
     compute ", TO "Tor", " using them.",
     EXAMPLE {
	  "M ** D",
	  "C ** D",
	  "prune HH_1(C ** D)",
	  "prune HH_1(M ** D)",
	  "prune HH_1(C ** N)",	  
	  },
     "Of course, we can use ", TO "Tor", " to get the same result.",
     EXAMPLE {
	  "prune Tor_1(M,N)"
	  },
     }

document { "maps between chain complexes",
     "One way to make maps between chain complexes is by lifting maps between
     modules to resolutions of those modules.  First we make some modules.",
     EXAMPLE {
	  "R = QQ[x,y];",
	  "M = coker vars R",
	  "N = coker matrix {{x}}",
	  },
     "Let's construct the natural map from ", TT "N", " to ", TT "M", ".",
     EXAMPLE {
	  "f = map(M,N)"
	  },
     "Let's lift the map to a map of free resolutions.",
     EXAMPLE {
	  "g = res f"
	  },
     "We can check that it's a map of chain complexes this way.",
     EXAMPLE {
	  "g * (source g).dd == (target g).dd * g"
	  },
     "We can form the mapping cone of ", TT "g", ".",
     EXAMPLE {
	  "F = cone g"
	  },
     "Since ", TT "f", " is surjective, we know that ", TT "F", "
     is quasi-isomorphic to ", TT "(kernel f)[-1]", ".  Let's
     check that.",
     EXAMPLE {
	  "prune HH_0 F",
	  "prune HH_1 F",
	  "prune kernel f",
	  },
     "There are more elementary ways to make maps between chain
     complexes.  The identity map is available from ", TO "id", ".",
     EXAMPLE {
	  "C = res M",
	  "id_C",
	  "x * id_C",
	  },
     "We can use ", TO "map", " or ", TT "**", " to construct natural maps 
     between chain complexes.",
     EXAMPLE {
	  "map(C ** R^1/x,C)",
	  "g ** R^1/x"
	  },
     "There is a way to make a chain complex map by calling a function
     for each spot that needs a map.",
     EXAMPLE {
	  "q = map(C,C,i -> (i+1) * id_(C_i))"
	  },
     "Of course, the formula we used doesn't yield a map of chain
     complexes.",
     EXAMPLE {
	  "C.dd * q == q * C.dd"
	  }
     }

document { "coherent sheaves",
     "The main reason to implement algebraic varieties is support the
     computation of sheaf cohomology of coherent sheaves, which doesn't
     have an immediate description in terms of graded modules.",
     PARA,
     "In this example, we use ", TO "cotangentSheaf", " to produce
     the cotangent sheaf on a K3 surface and compute its sheaf
     cohomology.",
     EXAMPLE {
	  "R = QQ[a,b,c,d]/(a^4+b^4+c^4+d^4);",
	  "X = Proj R",
	  "Omega = cotangentSheaf X",
	  "HH^1(Omega)",
	  },
     "Use the function ", TO "sheaf", " to convert a graded module to 
     a coherent sheaf, and ", TO "module", " to get the graded module
     back again.",
     EXAMPLE {
	  "F = sheaf coker matrix {{a,b}}",
	  "module F",
	  },
     SEEALSO {
	  (cohomology, ZZ, CoherentSheaf),
	  (cohomology, ZZ, SumOfTwists)
	  }
     }

document { "Language and Programming Overview",
     "In this section we give a comprehensive overview of the user
     language and the main programming features of Macaulay 2.",
     PARA,
     MENU {
	  TO "syntax",
	  TO "variables and symbols",
	  TO "functions",
     	  TO "basic types",
	  TO "control structures",
	  TO "classes and types",
	  TO "input and output",
	  TO "combinatorial functions",
	  TO "system",
	  TO "debugging",
	  TO "operators",
	  TO "executing other programs",
	  }
     }

document { "variables and symbols",
     Headline => "an overview",
     MENU {
	  TO "valid names",
	  TO "assigning values",
	  TO "local variables in a file",
	  TO "viewing the symbols defined so far",
	  TO "subscripted variables",
	  TO "numbered variables",
	  }
     }

document { "functions",
     Headline => "an overview",
     "In this section we present an overview of functions.  For details, 
     see ", TO "Function", ".",
     PARA,
     MENU {
	  TO "using functions",
	  TO "making functions",
	  TO "local variables in a function",
	  TO "making functions with a variable number of arguments",
	  TO "using functions with optional arguments",
	  TO "making new functions with optional arguments",
	  }
     }

document { "basic types",
     Headline => "an overview",
     "The basic type of an object is the way the object is
     essentially implemented internally.  It is not possible for
     the user to create new basic types.  For details, see
     ", TT "basictype", ".",
     PARA,
     "Some common basic types:",     
     MENU {
	  TO "strings",
	  TO "nets",
	  TO "lists",
	  TO "sequences",
	  TO "hash tables",
	  }
     }

document { "control structures",
     Headline => "an overview",
     MENU {
	  TO "loops",
	  TO "mapping over lists",
	  TO "mapping over hash tables",
	  TO "conditional execution",
	  TO "alarm",
	  TO "error handling",
	  }
     }

document { "input and output",
     Headline => "an overview",
     MENU {
	  TO "printing to the screen",
	  TO "reading files",
	  TO "getting input from the user",
	  TO "creating and writing files",
	  TO "two dimensional formatting",
	  TO "communicating with programs",
	  TO "using sockets",
	  }
     }

document { "classes and types",
     Headline => "an overview",
     MENU {
	  TO "what a class is",
	  TO "installing methods",
	  TO "binary methods",
	  TO "making new classes",
	  TO "inheritance from parents",
	  TO "printing and formatting for new classes",
	  (
	       "method functions",
	       MENU {
		    TO "making a new method function",
		    TO (method => SingleArgumentDispatch),
		    TO (method => Options),
		    }
	       ),
	  }
     }

document { "valid names",
     "Valid names for symbols may be constructed using letters, digits, and
     the apostrophe, and should not begin with a digit.",
     EXAMPLE {
	  "x",
	  "q0r55",
	  "f'"
	  },
     "Some symbols have preassigned meanings and values.  For example, symbols
     consisting of the letter ", TT "o", " followed by a number are used 
     to store output values.",
     EXAMPLE {
	  "o2",
	  },
     "Other symbols refer to functions built in to Macaulay 2 that provide
     much of its functionality, as we will learn.",
     PARA,
     "The class of all symbols is ", TO "Symbol", "."
     }

document { "assigning values",
     "Use an equal sign to assign values to variables.",
     EXAMPLE {
	  "x = \"abcde\"",
	  "x"
	  },
     "Before assignment, any reference to a variable provides the symbol
     with that name.  After assignment, the assigned value is provided.
     The variable created is global, in the sense that any code placed
     elsewhere that contains a reference to a variable called ", TT "x", "
     will refer to the one we just set.",
     PARA,
     "It is important to distinguish between a symbol and its value.  The
     initial value of a global symbol is the symbol itself, and the initial
     value of a local variable is ", TO "null", ".  One possibility for
     confusion comes from the possibility of having a symbol whose value is
     another symbol; it's even more confusing if the two symbols have the
     same name but different scopes, for example, if one of them is global
     and the other is local.",
     EXAMPLE {
	  "y",
	  "y = z",
	  "y",
	  "z = 444",
	  "z",
	  "y"
	  },
     "In the example above, the final value of ", TT "y", " is the
     symbol ", TT "z", ", even though the symbol z has acquired a 
     value of its own.  The function ", TT "value", " can be used to
     get the value of a symbol.",
     EXAMPLE {
	  "value y"
	  },
     "The operator ", TO "<-", " can be used to set the value of a
     symbol.  This operator differs from ", TO "=", " in that
     the symbol or expression on the left hand side is evaluated.",
     EXAMPLE {
	  "y <- 555",
	  "y",
	  "z",
	  "y = 666",
	  "y",
	  "z"
	  },
     "One reason the user needs to understand this concept is that
     assignments with the operator ", TO "<-", " are occasionally done
     on the user's behalf by bits of code already in the system,
     for example, when creating a polynomial ring the prospective
     variables are given new values which are the generators of the 
     polynomial ring.",
     PARA,
     SEEALSO { "GlobalAssignHook", "GlobalReleaseHook" }
     }

document { "local variables in a file",
     "There is a way to construct variables that can be used within a given
     source file, and are invisible to code placed in other files.  We use
     ", TO ":=", " for this.  Assume the code below is placed in a file, and
     that the file is loaded with the ", TO "load", " command.",
     EXAMPLE {
	  "ff := 5",
	  "ff"
	  },
     "The variable above is a local one.  Its value is not available to code
     in other files.",
     PARA,
     "Assume the code above is entered directly by the user into Macaulay 2.  Then
     the variable is still a local one and is not available to code previously loaded
     or to functions previously defined, but it will be available to code 
     loaded subsequently.  We illustrate this below with the variable ", TT "jj", ".",
     PARA,
     EXAMPLE {
	  "hh = () -> jj",
	  "hh()",
	  "jj = 444",
	  "hh()",
	  "jj := 555",
	  "hh()",
	  "jj"
	  }
     }

document { "viewing the symbols defined so far",
     "After using Macaulay 2 for a while, you may have stored data in
     several variables.  The system also stores output values for you
     in output variables.  You may wish to free the memory space occupied
     by that data, or you may simply wish to remind yourself where you
     stored a previously computed value.",
     PARA,
     "We may use the command ", TO "listUserSymbols", " to obtain a list of the user's
     symbols encountered so far.",
     EXAMPLE {
	  "ff = 20:4; hh = true; kk",
	  "listUserSymbols"
	  },
     "The symbols are listed in chronological order, and the type of value stored
     under it is displayed.",
     PARA,
     "We can clear the output symbols with ", TO "clearOutput", ".",
     EXAMPLE {
     	  "clearOutput",
	  "listUserSymbols"
	  },
     "We can clear all the symbols with ", TO "clearAll", ".",
     EXAMPLE {
     	  "clearAll",
	  "listUserSymbols"
	  },
     }

document { "subscripted variables",
     "It is common in mathematics to use subscripted variables.  We use the underscore
     to represent subscripts.  If we haven't assigned a value to ", TT "x", "
     we may simply start using it as a subscripted variable.  The subscripts can be
     anything.",
     EXAMPLE {
	  "x",
	  "x_4",
	  "x_(2,3)",
	  },
     "The ", TO "..", " operator knows what to do with subscripted variables.",
     EXAMPLE {
	  "x_10 .. x_20",
	  "x_(1,1) .. x_(2,3)",
	  },
     "Values can be assigned to these variables with ", TO "#", ".",
     EXAMPLE {
	  "x#10 = 555;",
	  "x_10",
	  },
     "Be careful not to assign a value to ", TT "x", " itself if you wish to continue
     using it as a subscripted variable.",
     SEEALSO {"IndexedVariable","IndexedVariableTable"}
     }

document { "using functions",
     "There are many functions in Macaulay 2 that do various things.  You can
     get a brief indication of what a function does by typing its name.",
     EXAMPLE "sin",
     "In this case, we see that the function ", TO "sin", " takes a single argument
     ", TT "x", ".  We apply a function to its argument by typing them in adjacent
     positions.  It is possible but not necessary to place parentheses around
     the argument.",
     EXAMPLE {
	  "sin 1.2",
	  "sin(1.2)",
	  "sin(1.0+0.2)",
	  },
     "In parsing the operator ", TO "^", " takes precedence over adjacency, so the
     function is applied after the power is computed in the following code.  This
     may not be what you expect.",
     EXAMPLE "print(10 + 1)^2",
     "Some functions take more than one argument, and the arguments are separated
     by a comma, and then parentheses are needed.",
     EXAMPLE {
	  "append",
	  "append({a,b,c},d)"
	  },
     "Some functions take a variable number of arguments.",
     EXAMPLE {
	  "join",
	  "join({a,b},{c,d},{e,f},{g,h,i})"
	  },
     "Functions, like anything else, can be assigned to variables.  You may do this
     to provide handy private abbreviations.",
     EXAMPLE {
	  "ap = append;",
	  "ap({a,b,c},d)"
	  },
     }

document { "making functions",
     "The operator ", TO "->", " is used to make new functions.  On its left
     we provide the names of the parameters to the function, and to the 
     right we provide the body of the function, an expression involving
     those parameters whose value is to be computed when the function 
     is applied.  Let's illustrate this by makint a function for squaring 
     numbers and calling it ", TT "sq", ".",
     EXAMPLE {
	  "sq = i -> i^2",
	  "sq 10",
	  "sq(5+5)",
	  },
     "When the function is evaluated, the argument is evaluated and assigned
     temporarily as the value of the parameter ", TT "i", ".  In the example
     above, ", TT "i", " was assigned the value ", TT "10", ", and then the 
     body of the function was evaluated, yielding ", TT "100", ".",
     PARA,
     "Here is how we make a function with more than one argument.",
     EXAMPLE {
	  "tm = (i,j) -> i*j",
	  "tm(5,7)",
	  },
     "Functions can be used without assigning them to variables.",
     EXAMPLE {
	  "(i -> i^2) 7",
	  },
     "Another way to make new functions is to compose two old ones
     with the operator ", TO "@@", ".",
     EXAMPLE {
	  "sincos = sin @@ cos",
	  "sincos 2.2",
	  "sin(cos(2.2))",
	  },
     "Code that implements composition of functions is easy to write, because
     functions can create new functions and return them.  We illustrate
     this by writing a function called ", TT "compose", " that will
     compose two functions, just as the operator ", TO "@@", " did
     above.",
     EXAMPLE {
	  "compose = (f,g) -> x -> f(g(x))",
	  "sincos = compose(sin,cos)",
	  "cossin = compose(cos,sin)",
	  "sincos 2.2",
	  "cossin 2.2",
	  },
     "We created two composite functions in the example above to illustrate an
     important point.  The parameters ", TT "f", " and ", TT "g", " acquire
     values when ", TT "sincos", " is created, and they acquire different values when
     ", TT "cossin", " is created.  These two sets of values do not interfere 
     with each other, and the memory they occupy will be retained as long 
     as they are needed.  Indeed, the body of both functions is
     ", TT "x -> f(g(x))", ", and the only difference between them is the
     values assigned to the parameters ", TT "f", " and ", TT "g", ".",
     PARA,
     "The class of all functions is ", TO "Function", "."
     }

document { "making functions with a variable number of arguments",
     "It is easy to write a function with a variable number of arguments.
     Define the function with just one parameter, with no parentheses around
     it.  If the function is called with several arguments, the value of the
     single parameter will be a sequence containing the several arguments;
     if the function is called with one argument, the value of the parameter
     will be that single argument.",
     EXAMPLE {
	  "f = x -> {class x, if class x === Sequence then #x};",
	  "f()",
	  "f(3)",
	  "f(3,4)",
	  "f(3,4,5)",
	  },
     "We could use the function ", TO "sequence", " to bring the case where there
     is just one argument into line with the others.  It will enclose anything
     that is not a sequence in a sequence of length one.",
     EXAMPLE {
	  "f = x -> (
     x = sequence x;
     {class x, #x});",
	  "f()",
	  "f(3)",
	  "f(3,4)",
	  "f(3,4,5)",
	  },
     "As an aside, we reveal that there is a way to define a function of one argument
     that will signal an error if it's given more than one argument: put
     parentheses around the single parameter in the definition of the function.
     As a side effect it can be used to extract the single element from a
     singleton sequence.",
     EXAMPLE {
	  "((x) -> x) 3",
	  "singleton 3",
	  "((x) -> x) oo",
	  }
     }

document { "using functions with optional arguments",
     "Some functions accept optional arguments.  Each of these optional arguments
     has a name.  For example, one of the optional arguments for ", TO "gb", "
     is named ", TO "DegreeLimit", "; it can be used to specify that the computation
     should stop after a certain degree has been reached.  Values for optional
     arguments are specified by providing additional arguments of the form ", TT "B=>v", "
     where ", TT "B", " is the name of the optional argument, and ", TT "v", " is
     the value provided for it.",
     EXAMPLE {
     	  "R = ZZ/101[x,y,z,w];",
     	  "gb ideal(x*y-z^2,y^2-w^2)",
	  "gb(ideal(x*y-z^2,y^2-w^2),DegreeLimit => 2)",
	  },
     "The names and default values of the optional arguments for a function can
     be obtained with ", TO "options", ", as follows.",
     EXAMPLE {
	  "o = options res"
	  },
     "The value returned is a type of hash table, and can be used to obtain particular
     default values.",
     EXAMPLE "o.SortStrategy",
     "The entry ", TT "DegreeLimit => 2", " is called an option.  Internally it is
     represented as a type of list of length 2.",
     EXAMPLE {
	  "DegreeLimit => 2",
	  "peek oo"
	  },
     }

document { "making new functions with optional arguments",
     "Let's consider an example where we wish to construct a linear function of ", TT "x", " 
     called ", TT "f", ", with the slope and y-intercept of the graph being optional
     arguments of ", TT "f", ".  We use the ", TO "==>", " operator to attach the default
     values to our function, coded in a special way.",
     EXAMPLE {
	  "opts = {Slope => 1, Intercept => 1}",
	  "f = opts ==> o -> x -> x * o.Slope + o.Intercept",
	  "f 5",
	  "f(5, Slope => 100)",
	  "f(5, Slope => 100, Intercept => 1000)",
	  },
     "In the example the function body is the code ", TT "x * opts.Slope + opts.Intercept", ".
     When it is evaluated, a hash table is assigned to ", TT "opts", "; its
     keys are the names of the optional arguments, and the values
     are the corresponding current values, obtained either from the default values 
     specified in the definition of ", TT "f", ", or from the options specified at 
     the time ", TT "f", " is called.",
     PARA,
     "In the example above, the inner function has just one argument, ", TT "x", ",
     but handling multiple arguments is just as easy.  Here is an example with two
     arguments.",
     EXAMPLE {
	  "f = {a => 1000} ==> o -> (x,y) -> x * o.a + y;",
	  "f(3,7)",
	  "f(5,11,a=>10^20)",
	  },
     }

document { "conditional execution",
     Headline => "execute some code if a condition is true",
     "The basic way to control the execution of code is with the ", TO "if", "
     expression.  Such an expression typically has the form
     ",  PRE "if X then Y else Z", "
     and is evaluated as follows.  First ", TT "X", " is evaluated.  If the 
     result is ", TT "true", ", then the value of ", TT "Y", " is provided, 
     and if the result is ", TT "false", ", then the value of ", TT "Z", "
     is provided.  An error is signalled if the value of ", TT "X", " is 
     anything but ", TT "true", " or ", TT "false", ".",
     EXAMPLE {
	  ///(-4 .. 4) / 
     (i -> if i < 0 then "neg" 
          else if i == 0 then "zer" 
          else "pos")///
	  },
     "The else clause may be omitted from an ", TT "if", " expression.  In that case, 
     if value of the predicate ", TT "X", " is false, then ", TO "null", " is provided 
     as the value of the ", TT "if", " expression.",
     EXAMPLE {
	  ///(-4 .. 4) / 
     (i -> if i < 0 then "neg" 
	  else if i == 0 then "zer")///
	  },
     "There are a variety of predicate functions (such as ", TT "<", ", used above)
     that yield ", TT "true", " or ", TT "false", " and can be used as the predicate 
     in an ", TT "if", " expression.  For a list, see ", TO "Boolean", ".  Boolean
     results may be combined with ", TO "not", ", ", TO "and", ", and ", TO "or", "."
     }

document { "loops",
     Headline => "evaluate code repeatedly",
     "One good way to perform an operation several times is with the
     keyword ", TO "while", ".  An expression of the form
     ", TT "while X do Y", " operates by evaluating
     ", TT "X", " repeatedly.  Each time the value of ", TT "X", " is true, 
     ", TT "Y", " is evaluated and its value is discarded.  When finally
     the value of ", TT "X", " is false the special value ", TO "null", " is 
     returned as the value of the ", TT "while", " expression.",
     EXAMPLE {
	  "i = 0;",
	  ///while i < 20 do (<< " " << i; i = i + 1)///
	  },
     "In the example above, ", TT "X", " is the predicate ", TT "i < 20", " and ", TT "Y", " is
     the code ", TT ///(<< " " << i; i = i + 1)///, ".  Notice the use of the
     semicolon within ", TT "Y", " to separate two expressions.",
     PARA,
     "The semicolon can also be used within the predicate ", TT "X", " to do other 
     things before the test is performed.  This works because the value of an expression
     of the form ", TT "(A;B;C;D;E)", " is obtained by evaluating each of the
     parts, and providing the value of the last part (in this case, ", TT "E", "),
     as the value of the whole expression.  Thus, if the value of ", TT "E", " is always
     true or false, the expression ", TT "(A;B;C;D;E)", " can be used as
     the predicate ", TT "X", ".  We illustrate this in the following example.",
     EXAMPLE {
	  "i = 0;",
	  ///while (<< " " << i; i < 20) do i = i+1///
	  },
     "If we use the form ", TT "while X list Y", " then the final value
     of the expression is a list of all the values of ", TT "Y", " encountered.",
     EXAMPLE {
	  ///i = 1; while (i = 2*i; i < 100) list i///
	  },
     "The two keywords can be combined in an expression of the form
     ", TT "while X list Y do Z", ", in which case Y and Z are both evaluated
     each time, and the final value is a list of all the values 
     of ", TT "Y", " encountered.",
     EXAMPLE {
	  ///i = 1; while i < 100 list i do i = 2*i///,
	  ///i = List; while i =!= Thing list i do i = parent i///
	  },
     "The keyword ", TO "break", " can be used to terminate a loop early,
     and optionally to specify a return value for the while-expression.",
     EXAMPLE {
	  "i = 0; while true do (j = i!; if j > 1000000 then break j else i = i+1)"
	  },
     "Another good way to perform an operation several times is with
     the keyword ", TO "for", ", especially when we are looping over
     consecutive integers, as with the variable ", TT "i", " in the
     previous example.  Here is the same computation, implemented
     with ", TO "for", ".",
     EXAMPLE "for i do (k := i!; if k > 1000000 then break k)",
     "Note: a for-loop starts a new lexical scope for local variables,
     and hence the value of ", TT "k", " is not known outside the 
     loop; see ", TO ":=", ".",
     PARA,
     "The keyword ", TO "when", " can be used with ", TO "for", " to
     specify a predicate which must remain true for execution to
     continue, and the keyword ", TO "list", " can be used to
     specify values which should be accumulated into a list and
     return as the value of the for-loop.  The keywords ", TO "from", " and 
     ", TO "to", " can be used to specify numerical limits for the loop
     variable.  Here is an example that illustrate all of these keywords
     at once.",
     EXAMPLE {
	  "for i from 10 to 30 when i<15 list 100*i do print i"
	  }
     }

document { "numbered variables",
     "One way to get many variables suitable for use as indeterminates in
     a polynomial ring is with the function ", TO "vars", ".  It converts 
     a list or sequence of integers into symbols.  It prefers to hand out
     symbols whose name consists of a single letter, but there are only 52 
     such symbols, so it also uses symbols such as ", TT "x55", " 
     and ", TT "X44", ".",
     EXAMPLE {
	  "vars (0 .. 9,40,100,-100)"
	  },
     "These variables can be used to make polynomial rings.",
     EXAMPLE {
	  "ZZ[vars(0 .. 10)]"
	  }
     }

document { "local variables in a function",
     "A local variable in a function is one that is not visible to
     code in other locations.  Correct use of local variables is
     important, for data stored in global variables will stay around
     forever, needlessly occupying valuable memory space.  For recursive
     functions especially, it is important to use local variables so that
     one invocation of the function doesn't interfere with another.",
     PARA,
     "The simplest way to create a local variable in a function is with
     the assignment operator ", TO ":=", ".  The expression ", TT "X := Y", "
     means that the value of ", TT "Y", " will be assigned to a newly created local
     variable whose name is ", TT "X", ".  Once ", TT "X", " has been created, new values can
     be assigned to it with expressions like ", TT "X = Z", ".",
     EXAMPLE {
	  "i = 22;",
	  ///f = () -> (i := 0; while i<9 do (<< i << " "; i=i+1); <<endl;)///,
	  "f()",
	  "i"
	  },
     "In the example above, we see that the function ", TT "f", " does 
     its work with a local variable ", TT "i", " that is unrelated to the global 
     variable ", TT "i", " introduced on the first line.",
     PARA,
     "In the next example, we show that the local variables in two
     invocations of a function don't interfere with each other.  We
     write a function ", TT "f", " that returns a newly created counting function.  
     The counting function simply returns the number of times it has 
     been called.",
     EXAMPLE {
	  "f = () -> (i := 0; () -> i = i+1)",
	  },
     "Let's use ", TT "f", " to create counting functions and show that they operate
     independently.",
     EXAMPLE {
	  "p = f()",
	  "q = f()",
	  "p(),p(),p(),p(),q(),p(),p(),q(),p(),p()"
	  }
     }

document { "strings",
     Headline => "an overview",
     "A string is a sequence of characters.  Strings can
     be manipulated in various ways to produce printed output.
     One enters a string by surrounding a sequence of characters with
     quotation marks.",
     EXAMPLE {
	  ///"abcdefghij"///,
	  },
     "Strings may contain newline characters.",
     EXAMPLE ///"abcde
fghij"///,
     "Strings, like anything else, can be assigned to variables.",
     EXAMPLE ///x = "abcdefghij"///,
     "There are escape sequences that make it possible to
     enter special characters:",
     PRE "      \\n             newline
      \\f             form feed
      \\r             return
      \\\\             \\ 
      \\\"             \"
      \\t             tab
      \\xxx           ascii character with octal value xxx",
     EXAMPLE ///y = "abc\101\102\n\tstu"///,
     "We can use ", TO "peek", " to see what characters are in the string.",
     EXAMPLE "peek y",
     "Another way to enter special characters into strings is to use ", TO "///", -- ///
										  "
     as the string delimiter.",
     EXAMPLE ("///" | ///a \ n = "c"/// | "///"),
     "The function ", TO "ascii", " converts strings to lists of
     ascii character code, and back again.",
     EXAMPLE {
      	  "ascii y",
      	  "ascii oo",
	  },
     "We may use the operator ", TO "|", " to concatenate strings.",
     EXAMPLE "x|x|x",
     "The operator ", TO "#", " computes the length of a string.",
     EXAMPLE "#x",
     "We may also use the operator ", TO "#", " to extract single characters from
     a string.  Warning: we number the characters starting with 0.",
     EXAMPLE "x#5",
     "The function ", TO "substring", " will extract portions of a string
     for us.",
     EXAMPLE {
	  "substring(5,x)",
	  "substring(5,2,x)",
	  },
     "The class of all strings is ", TO "String", "."
     }

document { "nets",
     Headline => "an overview",
     "A net is a rectangular two-dimensional array of characters, together
     with an imaginary horizontal baseline that allows nets to be assembled
     easily into lines of text.  A string is regarded as a net with one row.",
     PARA,
     "Nets are used extensively for such things as formatting polynomials for
     display on ascii terminals.  Use ", TO "net", " to obtain such nets directly.",
     EXAMPLE {
	  "R = ZZ[x,y];",
	  "(x+y)^2",
	  "n = net oo",
	  },
     "The net ", TT "n", " above looks exactly the same as the original polynomial -
     that's because a polynomial is printed by printing its net.  But the net 
     ", TT "n", " is no longer usable as a polynomial; it's just an 
     array of characters.  We may use ", TO "peek", " to clarify the extent of 
     a net.",
     EXAMPLE "peek n",
     "One way to create nets directly is with the operator ", TT "||", ", 
     which concatenates strings or nets vertically.",
     EXAMPLE ///x = "a" || "bb" || "ccc"///,
     "We may use the operator ", TO "^", " to raise or lower a net with 
     respect to its baseline.  Look carefully to see how the baseline has
     moved - it is aligned with the equal sign.",
     EXAMPLE "x^2",
     "Nets may appear in lists, and elsewhere.",
     EXAMPLE {
	  "{x,x^1,x^2}",
	  },
     "Nets and strings can be concatenated horizontally with the operator ", TO "|", ".",
     EXAMPLE ///x^2 | "-------" | x///,
     "Each net has a width, a depth, and a height.  The width is the number
     of characters in each row.  The depth is the number of rows below
     the baseline, and the height is the number of rows above the baseline.
     The depth and the height can be negative.",
     EXAMPLE "width x, height x, depth x",
     "We may extract the rows of a net as strings with ", TO "netRows", ".",
     EXAMPLE {
	  "netRows x",
	  "peek oo"
	  },
     "The class of all nets is ", TO "Net", "."
     }

document { "lists",
     Headline => "an overview",
     "A list is a handy way to store a series of things.  We create one
     by separating the elements of the series by commas and surrounding 
     the series with braces.",
     EXAMPLE "x = {a,b,c,d,e}",
     "We retrieve the length of a list with the operator ", TO "#", ".",
     EXAMPLE "#x",
     "We use it also to obtain a particular element.  The elements are 
     nubered consecutively starting with 0.",
     EXAMPLE "x#2",
     "The elements of a list are stored in consecutive memory locations,
     so the operation above is fast.",
     PARA,
     "The functions ", TO "first", " and ", TO "last", " retrieve the
     first and last elements of a list.",
     EXAMPLE "first x, last x",
     "Omitting an element of a list causes the symbol ", TO "null", " to 
     be inserted in its place.",
     EXAMPLE {
	  "g = {3,4,,5}",
	  "peek g",
	  },
     "Lists can be used as vectors, provided their elements are the sorts of
     things that can be added and mutliplied.",
     EXAMPLE "10000*{3,4,5} + {1,2,3}",     
     "If the elements of a list are themselves lists, we say that we have
     a nested list.",
     EXAMPLE {
	  "y = {{a,b,c},{d,{e,f}}}",
	  "#y"
	  },
     "One level of nesting may be eliminated with ", TO "flatten", ".",
     EXAMPLE "flatten y",
     "A table is a list whose elements are lists all of the same length.  
     The inner lists are regarded as rows when the table is displayed as a
     two-dimensional array with ", TO "MatrixExpression", ".",
     EXAMPLE {
	  "z = {{a,1},{b,2},{c,3}}",
	  "isTable z",
      	  "MatrixExpression z",
	  },
     "Various other functions for manipulating lists include ",
     TO (symbol |, List, List), ", ",
     TO "append", ", ",
     TO "between", ", ",
     TO "delete", ", ",
     TO "drop", ", ",
     TO "join", ", ",
     TO "mingle", ", ",
     TO "pack", ", ",
     TO "prepend", ", ",
     TO "reverse", ", ",
     TO "rsort", ", ",
     TO "sort", ", ",
     TO "take", ", and ",
     TO "unique", ".",
     PARA,
     "The class of all lists is ", TO "List", ", and the class of all
     basic lists, useful for deriving news types of list that do not
     inherit methods for treating lists as vectors, is ", TO "BasicList", "."
     }

document { "sequences",
     Headline => "an overview",
     "A sequence is like a list, except that parentheses are used
     instead of braces to create them and to print them.  Sequences
     are implemented in a more efficient way than lists, since a sequence is 
     created every time a function is called with more than one argument.  
     Another difference is that new types of list can be created by the user, 
     but not new types of sequence.",
     EXAMPLE "x = (a,b,c,d,e)",
     "It is a bit harder to create a sequence of length 1, since no comma
     would be involved, and parentheses are also used for simple grouping
     of algebraic expressions.",
     EXAMPLE "(a)",
     "We provide the function ", TO "singleton", ", which can be used to 
     create a sequence of length 1.  Its name appears when a sequence 
     of length 1 is displayed.",
     EXAMPLE {
	  "singleton a",
	  },
     "Most of the functions that apply to lists also work with sequences.  We
     give just one example.",
     EXAMPLE "append(x,f)",
     "The operator ", TO "..", " can be used to create sequences of numbers,
     sequences of subscripted variables, or sequences of those particular 
     symbols that are known to ", TO "vars", ", and so on.",
     EXAMPLE {
	  "-3 .. 3",
	  "y_1 .. y_10",
	  "a .. p",
	  "(1,1) .. (2,3)",
	  "{a,1} .. {c,2}",
	  },
     "The operator ", TO (symbol :, ZZ, Thing), " can be used to create sequences
     by replicating something a certain number of times.",
     EXAMPLE "12:a",
     "Notice what happens when we try to construct a list using ", TO "..", " 
     or ", TO ":", ".",
     EXAMPLE {
	  "z = {3 .. 6, 9, 3:12}",
	  },
     "The result above is a list of length 3 some of whose elements are sequences.
     This may be a problem if the user intended to produce the list 
     ", TT "{3, 4, 5, 6, 9, 12, 12, 12}", ".  The function ", TO "splice", " can
     be used to flatten out one level of nesting - think of it as removing those
     pairs of parentheses that are one level in.",
     EXAMPLE "splice z",
     "The difference between ", TO "splice", " and ", TO "flatten", " is that
     ", TO "flatten", " removes pairs of braces.",
     PARA,
     "The functions ", TO "toList", " and ", TO "toSequence", " are provided
     for converting between lists to sequences.",
     EXAMPLE {
	  "toList x",
	  "toSequence oo",
	  },
     "Other functions for dealing especially with sequences
     include ", TO "sequence", " and ", TO "deepSplice", ".  The class of all
     sequences is ", TO "Sequence", "."
     }

document { "hash tables",
     Headline => "an overview",
     "A hash table is a data structure that can implement a function
     whose domain is a finite set.  An element of the domain is called
     a key.  The hash table stores the key-value pairs in such a way
     that when presented with a key, the corresponding value can be
     quickly recovered.",
     PARA,
     "A dictionary could be implemented as a hash table: the keys would
     be the words in the language, and the values could be the definitions
     of the words.",
     PARA,
     "A phone book could also be implemented as a hash table: the keys would
     be the names of the subscribers, and the values could be the corresponding
     phone numbers.  (We exclude the possibility of two subscribers with
     the same name.)",
     PARA,
     "As an example we implement a phone book.",
     EXAMPLE {
	  ///book = new HashTable from {
     "Joe" => "344-5567",
     "Sarah" => "567-4223",
     "John" => "322-1456"}///,
     	  },
     "We use the operator ", TO "#", " to obtain values from the phone book.",
     EXAMPLE ///book#"Sarah"///,
     "The operator ", TO "#?", " can be used to tell us whether a given key
     has an entry in the hash table.",
     EXAMPLE ///book#?"Mary"///,
     "We have implemented the notion of set via hash tables in which every value
     is the number 1.",
     EXAMPLE {
	  "x = set {a,b,c,r,t}",
	  "peek x",
	  "x#?a",
	  "x#?4",
	  },
     "There is a type of hash table that is mutable, i.e., a hash table
     whose entries can be changed.  They are changed with assignment 
     statements of the form ", TT "x#key=value", ".",
     EXAMPLE {
	  ///x = new MutableHashTable;///,
	  ///x#"Joe" = "344-5567";///,
	  ///x#3 = {a,b,c};///,
	  ///x#{1,2} = 44;///,
	  ///x#3///,
	  ///x#?4///,
	  },
     "When a mutable hash table is printed, its contents are not displayed.  
     This prevents infinite loops in printing routines.",
     EXAMPLE "x",
     "Use ", TO "peek", " to see the contents of a mutable hash table.",
     EXAMPLE "peek x",
     "A variant of ", TO "#", " is ", TO ".", ".  It takes only global symbols
     as keys, and ignores their values.",
     EXAMPLE {
	  "p=4;",
	  "x.p = 444;",
	  "x.p",
	  "x#?4",
	  },
     "Other functions for manipulating hash tables include ",
     TO "browse", ", ",
     TO "copy", ", ",
     TO "hashTable", ", ",
     TO "keys", ", ",
     TO "mutable", ", ",
     TO "pairs", ", ",
     TO "remove", ", and ",
     TO "values", ".",
     PARA,
     "For details of the mechanism underlying hash tables, see ", TO "hashing", ".
     The class of all hash tables is ", TO "HashTable", ", and the class of all
     mutable hash tables is ", TO "MutableHashTable", ".",
     }

document { "hashing",
     "A hash table contains a set of key-value pairs.  The access
     functions for hash tables accept a key and retrieve the
     corresponding value.  Here are the details, together with a
     discussion of how we designed the hash table system seen in
     Macaulay 2.",
     PARA,
     "The keys and values are stored in the hash table.  The hash table consists
     of a certain number of ", ITALIC "buckets", ", each of which can hold an
     arbitrary number of key-value pairs.  The number of buckets is chosen
     to be large enough that typically one may expect each bucket to hold fewer than
     three key-value pairs.  The key is used  as follows to determine in which
     bucket the key-value pair is to be stored.  The function ", TO "hash", " 
     is applied to the key to produce, in a deterministic way, an integer called
     the hash code, and the remainder of the hash code upon division by the
     number of buckets tells which bucket will be used.",
     PARA,
     "It is ", BOLD "essential", " that the
     hash code of a key never change, for otherwise the next 
     attempt to find the key in the hash table will have an unpredictable 
     result - the new hash code of the key may or may not lead to 
     the same bucket, and the value may or may not be located.",
     PARA,
     "Some hash tables and lists are ", TO "mutable", ", i.e., their 
     contents can be altered by assignment statements.  As explained
     above, the hash code of a mutable thing is not permitted to
     change when the contents of the thing change.  Hence, the 
     algorithm for computing the hash code may not refer to the
     contents of a mutable thing.",
     PARA,
     "The strong comparison operator ", TO "===", " is provided to 
     parrot the equality testing that occurs implicitly when 
     accessing a key in a hash table.  The fundamental requirement for this
     strong comparison operator is that things with different hash codes must also
     turn out to be different when tested with this comparison operator.",
     PARA,
     "Here we come to a question of design.  As discussed above, we must assign
     hash codes to mutable things in such a way that the hash codes don't depend
     on their contents.  We can do this in various ways.",
     MENU {
	  {
     	       "One way to assign hash codes to mutable things is to give 
     	       the same hash code, say 1000000, to every mutable thing.  We
	       could then implement a strong comparison operator for mutable
	       things that would proceed by examining the contents of the
	       things, so that two mutable things would be equal if and only
	       if their contents were equal.  A
	       disadvantage of this approach would be that a hash table in
	       which many mutable things appear as keys would have all of those
	       key-value pairs appearing in the same bucket, so that access
	       to this hashtable would be slow.  (Each bucket is implemented
	       as a linear list, and searching a long linear list is slow.)"
	       },
	  {
     	       "Another way to assign hash codes to mutable things is to
     	       give different hash codes to each mutable thing; for example, the 
	       first mutable thing could receive hash code 1000000, the second
	       could receive the hash code 1000001, and so on.  (Another
     	       choice for such a hash code is the 
     	       address in memory of the thing.  But this address can change
     	       depending on environmental factors not under the control of the
     	       interpreter, and thus its use as a hash code would lead 
	       to unpredictable behavior.)  A disadvantage
	       of this approach is that the strong comparison operator could not
	       examine the contents of mutable objects!  (Remember that
	       if the hash codes are different, the strong comparison must declare
	       the things to be different, too.)  The offsetting advantage is
	       that a hash table in which many mutable things appear as keys would
	       typically have the key-value pairs distributed among the buckets,
	       so that access to this hashtable would be fast."
	       }
	  },
     PARA,
     "In Macaulay 2, we chose the second approach listed above; we expect to
     have many mutable things appearing as keys in hash tables, and we need
     the speed.  A counter with initial value 1000000 is incremented each time 
     a mutable thing is created, and its value is taken as the hash code of the
     thing and stored within it.  The strong comparison test cannot depend on 
     the contents of mutable things, and thus such things appear to be 
     containers with opaque walls.  For mutable things, the test for equality 
     must be the same as equality of the hash codes.",
     PARA,
     "It is essential to have some hash tables for which equality amounts
     to equality of the contents.  This cannot be achieved for mutable
     hash tables, but we do achieve it for non-mutable hash tables -- the
     hash code is computed directly from the contents
     of the thing in a deterministic way.  This allows us to
     implement the notion of polynomial, say, as a hash table -- the 
     keys can be the monomials (necessarily non-mutable) and the 
     values can be the coefficients.  The notion of monomial can be
     implemented as a hash table where the keys are the variables and the
     values are the corresponding exponents.",
     PARA,
     "One further comforting remark: the routines that compute hash 
     codes or strong equality do not get into infinite loops, despite 
     the existence of circular structures: any circular structure 
     must come into being by means of changing something, and
     so the circular loop in the structure necessarily involves a 
     mutable thing, whose contents are not examined by the routines.
     This provides another argument in favor of taking the second approach listed
     above.",
     SEEALSO "HashTable"
     }

document { "mapping over lists",
     Headline => "apply a function to each element of a list",
     "In programming, loops that operate on consecutive elements of a
     list are common, so we offer various ways to apply functions to
     each of the elements of a list, along with various ways to treat the
     returned values.",
     PARA,
     "The most basic operation is provided by ", TO "scan", ", which applies
     a function consecutively to each element of a list, discarding the
     values returned.",
     EXAMPLE "scan({a,b,c}, print)",
     "The keyword ", TO "break", " can be used to terminate the scan
     prematurely, and optionally to specify a return value for the
     expression.  Here we use it to locate the first even number in
     a list.",
     EXAMPLE {
	  "scan({3,5,7,11,44,55,77}, i -> if even i then break i)"
	  },
     "The function ", TO "apply", " is similar to ", TO "scan", " but
     will produce a list containing the values returned.",
     EXAMPLE "apply({1,2,3,4}, i -> i^2)",
     "This operation is so common that we offer two shorthand notations for
     it, one with the function on the right and one with the function on
     the left.",
     EXAMPLE {
	  ///{1,2,3,4} / (i -> i^2)///,
	  ///(i -> i^2) \ {1,2,3,4}///,
	  },
     "The associativity of these operators during parsing is set up so the 
     following code works as one would wish.",
     EXAMPLE {
	  ///{1,2,3,4} / (i -> i^2) / (j -> 1000*j)///,
	  ///(j -> 1000*j) \ (i -> i^2) \ {1,2,3,4}///,
	  ///(j -> 1000*j) @@ (i -> i^2) \ {1,2,3,4}///,
	  },
     "The function ", TO "apply", " can also be used with two lists of the same
     length, in which case it will apply the function consecutively to
     corresponding elements of the two lists.",
     EXAMPLE {
	  "apply({1,2,3}, {7,8,9}, (i,j) -> 1000*i+j)"
	  },
     "The function ", TO "table", " can be used to create a table (doubly
     nested list) from two lists and a function of two arguments.  It applies
     the function consecutively to each element from the first list paired
     with each element from the second list, so the total number of evaluations
     of the function is the product of the lengths of the two lists.",
     EXAMPLE {
	  "table({1,2,3},{7,8},(i,j) -> 1000*i+j)"
	  },
     "The function ", TO "applyTable", " can be used to apply a function to 
     each element of table.",
     EXAMPLE {
	  "applyTable( {{1,2,3},{4,5}}, i -> i^2)"
	  },
     "We may use ", TO "select", " to select those elements from a list
     that satisfy some condition.  In the next example, we use the function
     ", TO "even", " to select the even numbers from a list.",
     EXAMPLE "select({1,2,3,4,5,6,7,8,9,10}, even)",
     "An optional first argument to ", TO "select", " allows us to specify the
     maximum number of elements selected.",
     EXAMPLE "select(2,{1,2,3,4,5,6,7,8,9,10}, even)",
     "We may use ", TO "any", " to tell whether there is at least one element of 
     a list satisfying a condition, and ", TO "all", " to tell whether all 
     elements satisfy it.",
     EXAMPLE {
	  "any({1,2,3,4,5,6,7,8,9,10}, even)",
	  "all({1,2,3,4,5,6,7,8,9,10}, even)",
	  },
     "We can use ", TO "position", " to tell us the position of the first element
     in a list satisfying a condition.",
     EXAMPLE {
	  "position({1,3,5,7,8,9,11,13,15,16},even)",
	  },
     "The functions ", TO "fold", " and ", TO "accumulate", " provide various
     ways to apply a function of two arguments to the elements of a list.  One
     of the arguments is the next element from the list, and the other argument
     is the value returned by the previous application of the function.  As an
     example, suppose we want to convert the list ", TT "{7,3,5,4,2}", " of digits
     into the corresponding number ", TT "73542", ".  The formula
     ", TT "(((7*10+3)*10+5)*10+4)+2", " is a fast way to do it that doesn't
     involve computing high powers of 10 separately.  We can do this with
     ", TO "fold", " and the following code.",
     EXAMPLE {
	  "fold((i,j) -> i*10+j, {7,3,5,4,2})",
	  },
     "It is possible to give an additional argument to ", TO "fold", " so
     that lists of length 0 can be handled correctly."
     }

document { "mapping over hash tables",
     Headline => "apply a function to each element of a hash table",
     "Each entry in a hash table consists of a key and a value.  We provide
     three ways to map functions over a hash table, depending on whether the
     function is to receive a value and return a new value, to receive a key
     and return a new key, or to receive a key-value pair and return a new
     key-value pair.  The corresponding functions, ", TO "applyValues", ",
     ", TO "applyKeys", ", and ", TO "applyPairs", " are illustrated in the
     next example.",
     EXAMPLE {
	  "x = new HashTable from {a=>1, b=>2}",
	  "applyValues(x, value -> 1000*value)",
	  "applyKeys(x, key -> {key})",
	  "applyPairs(x, (key,value) -> (value,key))",
	  },
     "The functions, ", TO "scanValues", ", ", TO "scanKeys", ", and 
     ", TO "scanPairs", " are similar, but the values returned are discarded
     instead of being assembled into a new hash table.",
     EXAMPLE {
	  "x = new HashTable from {a=>1, b=>2}",
	  "scanValues(x, print)",
	  "scanKeys(x, print)",
	  "scanPairs(x, print)",
	  },
     "The function ", TO "merge", " can be used to merge two hash tables.  The
     result is a hash table whose keys are those occuring in one of the two
     incoming hash tables.  We must provide a function of two arguments
     that is used to combine the values when a key occurs in both hash tables.",
     EXAMPLE {
	  "y = new HashTable from {b=>200, c=>300}",
	  "merge(x, y, plus)",
	  },
     "The function ", TO "combine", " can be used to combine two hash tables ", TT "x", "
     and ", TT "y", " into a new hash table.  Three functions must be provided.  The first 
     one produces new keys from a key of ", TT "x", " and a key of ", TT "y", ".  The
     second one produces a new values from a value of ", TT "x", " and a value
     of ", TT "y", ".  The third one is used to combine values when two new keys
     turn out to be the same.",
     EXAMPLE {
     	  "combine(x,y,identity,times,plus)",
	  },
     }

document { "error handling",
     Headline => "signalling and trapping errors",
     "When an error occurs in your program, an error message will appear that
     gives the name of the file, the line number, and the column number of
     the code that provoked the error.",
     PARA,
     "You may use the function ", TO "error", " in your programs to signal
     errors.  Your error message will appear on the screen and execution
     will be stopped.",
     PARA,
     "The function ", TO "try", " can be used to catch an error before
     execution is stopped and to continue or to try something else."
     }

document { "printing to the screen",
     "Use the operator ", TO "<<", " to print something to the screen.",
     EXAMPLE {
	  "<< 2^100"
	  },
     "Notice that the value returned is the standard input output file
     ", TO "stdio", ".  We can also use ", TO "<<", " as a binary
     operator to print explicitly to this file, and the output will 
     appear on the screen.",
     EXAMPLE {
	  "stdio << 2^100"
	  },
     "The associativity of the operator ", TO "<<", " during parsing is
     arranged so that the following code will work in a useful way.",
     EXAMPLE ///<< "The answer is " << 2^100 << ".";///,
     "Printing works fine with nets, too.",
     EXAMPLE ///<< "The answer is " << "aaa"||"bb"||"c" << ".";///,
     "In the presence of nets, which are used whenever you print something
     that is formatted two-dimensionally, the only correct way to terminate a line
     is with ", TO "endl", ", even though a string called ", TO "newline", "
     is available to you.",
     EXAMPLE {
	  ///R = ZZ[x,y];///,
	  ///f = (x+y+1)^2; g = (x-y+1)^2///,
	  ///<< "f = " << f << endl << "g = " << g << endl;///,
	  },
     "Output is saved in a buffer until the end of the line is encountered.
     If nets are not involved, you may use ", TO "flush", " to force
     the output to appear on the screen before the end of the line is
     encountered.  This may be useful in displaying some tiny indication
     of computational progress to the user.",
     EXAMPLE ///scan(0 .. 20, i -> << "." << flush)///,
     PARA,
     "If long lines get displayed too slowly, such as in emacs, then the user may choose
     to put a line such as ", TT "truncateOutput 100", " into an ", TO "initialization file", ".
     Time is still spent creating the wide output which is eventually truncated.",
     EXAMPLE {
	  "truncateOutput 50",
	  "41!",
	  "42!",
	  "43!",
	  "truncateOutput infinity",
	  "43!"
	  }
     }

document { "reading files",
     "Sometimes a file will contain a single expression whose value you wish
     to have access to.  For example, it might be a polynomial produced by
     another program.  The function ", TO "get", " can be used to obtain 
     the entire contents of a file as a single string.  We illustrate this
     here with a file whose name is ", TT "expression", ".",
     EXAMPLE ///get "expression"///,
     "This isn't quite what you want, because a string containing a description
     of a polynomial is not the same as a polynomial.  We may use
     ", TO "value", " to evaluate the string and produce the corresponding
     polynomial, after first setting up a polynomial ring to contain it.",
     EXAMPLE {
	  ///R = ZZ/101[x,y,z]///,
	  ///f = value get "expression"///,
	  ///factor f///,
	  },
     "Often a file will contain code written in the Macaulay 2 language.
     We have a file called ", TT "sample", " with two lines of code.",
     EXAMPLE ///<< get "sample";///,
     "To load and execute that code, use ", TO "load", ".",
     EXAMPLE ///load "sample"///,
     "The command ", TO "needs", " can be used to load a file only if
     it hasn't already been loaded.",
     EXAMPLE ///needs "sample"///,
     "For debugging or display purposes, it is sometimes useful to be able 
     to simulate entering the lines of a file one by one, so they appear
     on the screen along with prompts and output lines.  We use
     ", TO "input", " for
     this.",
     EXAMPLE ///input "sample"///,
     "There are other ways to manipulate the contents of a file with
     multiple lines.  First, let's use ", TO "peek", " to observe the 
     extent of this string returned by ", TO "get", ".",
     EXAMPLE ///peek get "sample"///,
     "The resulting string has newlines in it; we can use ", TO "lines", "
     to break it apart into a list of strings, with one row in each string.",
     EXAMPLE ///lines get "sample"///,
     "We may use ", TO "peek", " to observe the extent of these strings.",
     EXAMPLE ///peek lines get "sample"///,
     "We could even use ", TO "stack", " to assemble the lines of the
     file into a net.",
     EXAMPLE ///stack lines get "sample"///
     }

document { "getting input from the user",
     "The function ", TO "read", " can be used to prompt the user and obtain
     a line of input as a string.  In response to the prompt, the user enters
     ", TT "sample", " and press return.",
     EXAMPLE {
	  ///filename = read "file name : "
sample///,
     	  },
     "Let's use ", TO "peek", " to verify that this string contains no newline
     characters.",
     EXAMPLE ///peek filename///,
     "If necessary, we may use ", TO "value", " to evaluate the string provided
     by the user.  In this example, we enter ", TT "(x+y)^2", " in response to
     the prompt.",
     EXAMPLE {
	  ///R = ZZ[x,y];///,
	  ///s = read "polynomial : "
(x+y)^2///,
     	  ///value s///
	  }
     }

document { "creating and writing files",
     "We can print to a file in essentially the same way we print to the screen.
     In the simplest case, we create the entire file with one command; we
     give the file name as the initial left hand operand of ", TO "<<", ",
     and we close the file with ", TO "close", ".  Files must be closed
     before they can be used for something else.",
     EXAMPLE {
	  ///"testfile" << 2^100 << endl << close///,
	  ///value get "testfile"///,
	  },
     "More complicated files may require printing to the file multiple times.  One
     way to handle this is to assign the open file created the first time we
     use ", TO "<<", " to a variable, so we can use it for subsequent print operations
     and for closing the file.",
     EXAMPLE {
	  ///f = "testfile" << ""///,
	  ///f << "hi" << endl///,
	  ///f << "ho" << endl///,
	  ///f << close///,
	  ///get "testfile"///,
	  }
     }

document { "two dimensional formatting",
     "We have seen that ", TO "nets", " are potentially useful for two
     dimesional formatting of output to an ascii terminal with limited
     graphical ability.  We present now a few more hints about putting
     this idea into practice.  Nets are used extensively in Macaulay 2
     for formatting, for example, for formatting of polynomials and
     matrices.",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[x,y,z];",
	  "f = random(R^1,R^{5:-3})",
	  },
     PARA,
     "Output of routines such as ", TO "betti", " and ", TO "net", " that
     return nets can be easily incorporated into more complex displays 
     using standard operations on nets (see ", TO "Net", ").",
     EXAMPLE {
	  "C = resolution cokernel f",
	  "be = betti C",
	  ///"Betti numbers of " | net C | " are " | be^2///,
	  },
     "You could even learn how to display algebraic expressions with nets.",
     EXAMPLE ///"x" | "2"^1///,
     "There is an easier way to display algebraic expressions, using a
     type of thing called an ", TO "Expression", ".  It allows you
     to set up things that print out as powers, sums, products,
     matrices, and so on.  There are various types of expression, such as
     ", TO "Power", ", ", TO "Sum", ", ", TO "Divide", ", ", TO "Minus", ",
     and ", TO "Product", " that we can use for this.",
     EXAMPLE {
	  "Divide(Minus a,b)",
	  "Power(Sum(3,4,5),7)",
	  "Sum(1,2, Minus 3, 4,5)",
     	  },
     "Actually, the formation of such expressions is contagious, in the sense
     that the basic algebraic operations will construct expressions for you if
     one of their two operands is already an expression.",
     EXAMPLE {
	  "Minus a / b",
	  "(Sum(3,4,5))^7",
	  "1 + 2 + Minus 3 + 4 + 5",
	  },
     "In the last example above, ", TT "1 + 2", " was evaluated first, so it
     yielded ", TT "3", " but after that the contagion set in.",
     PARA,
     "The function ", TO "expression", " can be used to prepare things such
     as polynomials for formatting using the mechanism introduced above.",
     EXAMPLE {
	  "g = (x+y)^2",
	  "e = expression g",
	  "peek e",
	  },
     "In the example above, we see that ", TO "peek", " extracts only one
     level of the structure.  We may use ", TO "peek2", " to display
     the structure of ", TT "e", " to depth 2.",
     EXAMPLE {
	  "peek2(e,2)",
	  },
     "Other types of ", TO "Expression", " that can be used for formatting
     nested lists as two dimensional arrays are ", TO "MatrixExpression", "
     and ", TO "Table", ".",
     EXAMPLE {
	  "Table{{1,2,3},{a,bb,ccc}}",
	  "MatrixExpression{{1,2,3},{a,bb,ccc}}",
	  ///Table{{"Example 1","Example 2"},
      {Table{{1,2},{3,4}},Table{{11,22},{3,444}}}}///
	  },
     }

document { isReady, Headline => "whether a file has data available for reading" }

document { atEndOfFile,
     Headline => "test for end of file",
     TT "atEndOfFile f", " -- tells whether an input file ", TT "f", " is at the end.",
     PARA,
     SEEALSO {"File"}
     }

document { "communicating with programs",
     "The most naive way to interact with another program is simply to run
     it, let it communicate directly with the user, and wait for it to
     finish.  This is done with the ", TO "run", " command.",
     EXAMPLE ///run "uname -a"///,
     "More often, one wants to write Macaulay 2 code to obtain
     and manipulate the output from the other program.  If the program
     requires no input data from us, as in the example above, then we can
     use ", TO "get", " with a file name whose first character is an
     exclamation point; the rest of the file name will be taken as the
     command to run.  In this example, we also peek at the string
     to see whether it includes a newline character.",
     if version#"operating system" === "Windows-95-98-NT"
     then PRE "<< example doesn't work under Windows >>"
     else
     EXAMPLE {
	  ///peek get "!uname -a"///,
	  },
     "Bidirectional communication with a program is also possible.  We use
     ", TO "openInOut", " to create a file that serves as a bidirectional
     connection to a program.  That file is called an input output file.  In
     this example we open a connection to the unix utility ", TT "egrep", "
     and use it to locate the symbol names in Macaulay2 that begin with
     ", TT "in", ".",
     if version#"operating system" === "Windows-95-98-NT"
     then PRE "<< example doesn't work under Windows >>"
     else
     EXAMPLE {
	  ///f = openInOut "!egrep '^in'"///,
	  ///scan(keys symbolTable(), key -> f << key << endl)///,
	  ///f << closeOut///,
	  ///get f///
	  },
     "With this form of bidirectional communication there is always a danger
     of blocking, because the buffers associated with the communication
     channels (pipes) typically hold only 4096 bytes.  In this example we
     succeeded because the entire output from ", TT "egrep", " was smaller
     than 4096 bytes.  In general, one should be careful to arrange things
     so that the two programs take turns using the communication channel, so
     that when one is writing data, the other is reading it.",
     PARA,
     "A useful function in this connection is ", TO "isReady", ", which will
     tell you whether an input file has any input available for reading, or
     whether it has arrived at the end.  We illustrate it in the following
     example by simulating a computation that takes 5 seconds to complete,
     printing one dot per second while waiting.",
     if version#"operating system" === "Windows-95-98-NT"
     then PRE "<< example doesn't work under Windows >>"
     else
     EXAMPLE {
	  ///f = openIn "!sleep 5; echo -n the answer is 4"///,
	  ///isReady f///,
	  ///while not isReady f do (sleep 1; << "." << flush)///,
	  ///read f///,
	  ///isReady f///,
	  ///atEndOfFile f///,
	  ///close f///
	  },
     "We also allow for bidirectional communication
     through sockets over the internet.  See ", TO "openInOut", "
     and ", TO "openListener", ", or the next section.",
     PARA,
     "Another useful function is ", TO "wait", ", which can be used
     to wait for input to be available from any of a list of input
     files."
     }

document { "using sockets",
     "It's easy to use sockets as though they were files.  Simply replace
     the file name by a string of the form ", TT "$host:service", " where
     ", TT "host", " is the name of IP number of host to contact, and
     ", TT "service", " is the port number or name to use.  If ", TT "service", "
     is omitted, then port 2500 is used.  If ", TT "host", " is omitted, then
     an incoming connection will be listened for.",
     PARA,
     "For the demonstration, we use ", TO "fork", " to create a
     separate process which will listen for a connection on port
     7500 and then send us a message.",
     if version#"operating system" === "Windows-95-98-NT"
     then PRE "<< example doesn't work under Windows >>"
     else
     EXAMPLE {
	  ///if (pid = fork()) == 0 then (
     try "$:7500" << "hi there" << close;
     exit 0;
     )///},
     "Let's wait for a while to make sure the child process is listening.",
     if version#"operating system" === "Windows-95-98-NT"
     then PRE "<< example doesn't work under Windows >>"
     else
     EXAMPLE "sleep 2",
     "Now we can use an ordinary input command like ", TO "get", " to obtain 
     the entire message.",
     if version#"operating system" === "Windows-95-98-NT"
     then PRE "<< example doesn't work under Windows >>"
     else
     EXAMPLE ///get "$localhost:7500"///,
     "Finally, we can wait for the child process to finish.",
     if version#"operating system" === "Windows-95-98-NT"
     then PRE "<< example doesn't work under Windows >>"
     else
     EXAMPLE "wait pid",
     PARA,
     SEEALSO { "openInOut", "openListener" }
     }

document { "making new classes",
     "All new classes are made with the operator ", TO "new", ".
     You may choose to implement the instances of your new class
     either as basic lists or as hash tables, or you may even
     base it on a subclass of ", TO "BasicList", " or a subclass of 
     ", TO "HashTable", ", if you find a class that has some
     of the methods you need already implemented.",
     PARA,
     "As an example, we may wish to implement quaternions as lists
     of four real numbers.  We know that lists already have a method
     for addition which treats them as vectors, and we could use
     the same code for addition of quaternions.",
     EXAMPLE {
	  "Qu = new Type of List",
	  "w = new Qu from {1,2,3,4}",
	  "w+w"
	  },
     "Now all we have to do is to install a method for multiplying
     quaternions.",
     EXAMPLE {
	  "Qu * Qu := (x,y) -> new Qu from { 
	  x#0*y#0 - x#1*y#1 - x#2*y#2 - x#3*y#3,
	  x#0*y#1 + x#1*y#0 + x#2*y#3 - x#3*y#2,
	  x#0*y#2 + x#2*y#0 + x#3*y#1 - x#1*y#3,
	  x#0*y#3 + x#3*y#0 + x#1*y#2 - x#2*y#1
	  };",
     	  "w*w"
	  }
     }

document { "making a new method function",
     "The function ", TO "method", " can be used to make new functions
     which execute different bits of code depending on the types
     of the arguments presented.  Our system depends heavily on
     such functions.",
     EXAMPLE "f = method()",
     "We can install a method to be used when ", TT "f", " is applied to a string
     as follows.",
     EXAMPLE {
	  "f String := s -> s|s;",
	  ///f ".abcd."///
	  },
     "We can check for the types of up to three arguments, too.",
     EXAMPLE {
	  "f(ZZ,String) := (n,s) -> concatenate (n:s);",
	  ///f(5,".abcd.")///,
	  }
     }

document { "what a class is",
     "In Macaulay 2 the behavior of a function depends heavily on the types
     of the arguments it's presented with.  For example, the expression ", TT "x+y", "
     means the sum if ", TT "x", " and ", TT "y", " are integers, but it
     means the union if ", TT "x", " and ", TT "y", " are sets.  To implement this
     in a clean fashion, we store the code for doing things with sets
     in something called ", TO "Set", " and store the code for doing things with integers
     in something called ", TO "ZZ", ".  We say that each integer is an ", TO "instance", "
     of ", TO "ZZ", ", and ", TO "ZZ", " is the ", TO "class", " (or type) of each 
     integer.  The function ", TO "class", " provides the class of an object, and
     the function ", TO "instance", " tells whether an given object is an
     instance of a given class, or a subclass of it, and so on.",
     PARA,
     EXAMPLE {
	  "class 33",
	  "instance(33,ZZ)",
	  "instance(33,String)"
	  },
     "The corresponding mathematical idea is that ", TT "ZZ", " is the set of
     all integers.",
     PARA,
     "The class of all classes or types is called ", TO "Type", ".",
     EXAMPLE {
	  "instance(ZZ,Type)",
	  "instance(33,Type)",
	  },
     "The class of all objects in the system is ", TO "Thing", ".",
     EXAMPLE {
	  "instance(33,Thing)",
	  },
     "Everything has a class, and every class has a ", TO "parent", ".  The 
     parent class represents a broader class of objects, and is used to
     contain code that applies to the broader class.  For example, ", TO "ZZ", "
     is a ring, and every ring is also a type.",
     EXAMPLE {
	  "class ZZ",
	  "parent class ZZ"
	  },
     "Types are implemented as hash tables -- it's a versatile way of storing
     bits of code that are needed in various situations; the keys for the
     hash table are constructed in a certain way from the function and the
     types of its arguments whose details the user doesn't need to know.",
     PARA,
     "For more details, see one of the topics below.",
     SHIELD MENU {
	  TO "class",
	  TO "parent",
	  TO "instance",
	  TO "ancestor",
	  TO "newClass",
	  TO "new",
	  },
     "For related topics, see one of the following.",
     SHIELD MENU {
	  TO "uniform",
	  TO "Thing",
	  TO "Nothing",
	  TO "Type",
	  TO "MutableList",
	  TO "MutableHashTable",
	  TO "SelfInitializingType"
	  }
     }

document { "binary methods",
     "The method for computing a sum ", TT "x+y", " depends on the types of ", TT "x", " and ", TT "y", ".
     For example, the method for adding an integer ", TT "x", " and a polynomial 
     ", TT "y", " differs from the method for adding two integers modulo 111.  Because
     both the type of ", TT "x", " and the type of ", TT "y", " must enter into the selection of
     the method, we refer to these methods as binary methods.  Each binary
     method is a function of two variables, and is stored either in the class
     of ", TT "x", " or in the class of ", TT "y", ".",
     PARA,
     "Let's assume that ", TT "X", " is the class (or type) of ", TT "x", ", 
     and that ", TT "Y", " is the class of ", TT "y", ".  The way to install a 
     method for the addition of an instance ", TT "x", " of class ", TT "X", " to 
     an instance ", TT "y", " of class ", TT "Y", " is with a statement of the form ",
     PRE "X + Y := (x,y) -> ( ... )",
     "where ", TT "( ... )", " represents the body of the function, consisting of suitable
     code for the operation at hand.",
     PARA,
     "The method installed by the code above is automatically inherited by 
     subclasses of ", TT "X", " and ", TT "Y", ".  Here is a brief
     description of the way this works.  Suppose ", TT "X", " is the 
     ", TO "parent", " of ", TT "P", " and ", TT "Y", " is the parent of X.  When 
     a sum ", TT "p+q", " is evaluated where the class of ", TT "p", " is 
     ", TT "P", " and the class of ", TT "q", " is ", TT "Q", ", then the binary
     method for ", TT "P+Q", " is applied, unless there isn't one, in which
     case the binary method for ", TT "P+Y", " is applied, unless there isn't
     one, in which case the binary method for ", TT "X+Q", " is applied,
     unless there isn't one, in which case the binary method for ", TT "P+Q", "
     is applied.  In general this search for a binary method continues all
     the way up the chain of parents to the topmost ancestor of everything,
     which is called ", TO "Thing", ".  (See also ", TO "lookup", ".)",
     PARA,
     "As an extreme example of inheritance, the code ", 
     PRE "Thing + Thing := (x,y) -> ( ... )",
     "will install a binary method for adding any two things, which will take
     effect as a last resort whenever more a specifically defined method
     isn't found.",
     PARA,
     "The ", TO "new", " function also uses a ternary lookup table to
     find the initialization function for the new thing, and should
     be thought of as a ternary operator.  The initialization function
     for a new expression created by",
     PRE "new Z of x from y",
     "is obtained as",
     PRE "lookup(NewMethod,Z,X,Y)",
     "Here ", TT "X", " is ", TT "class x", ", and ", TT "Y", " is
     ", TT "class y", ".  The initialization function can be installed 
     with",
     PRE "new Z of X from Y := (z,y) -> ...",
     "where ", TT "z", " denotes the new hash table of class ", TT "Z", " and parent
     ", TT "x", " provided to the routine by the system."
     }

document { "installing methods",
     "The method to be used for computing an expression such as ", TT "-x", " depends 
     on the type of ", TT "x", ".  For example, the method for negating a polynomial
     differs from the method for negating an integer modulo 111.  Each
     method is a function of one variable, and is stored in the class 
     of ", TT "x", " under a key which is referred to as the name of the method.
     For some built-in methods the method name is a symbol, but for
     methods created with ", TO "method", ", the method name is the same
     as the function used for calling it up.",
     PARA,
     "Let's assume that ", TT "X", " is the class of ", TT "x", ".  The way to install a method
     for the negation of an instance ", TT "x", " of ", TT "X", " is with a statement of the 
     following form.",
     PRE "- X := x ->( ... )",
     "Here ", TT "( ... )", " represents the body of the function, consisting of
     suitable code for the operation at hand.",
     PARA,
     "The method installed by the code above is automatically inherited by
     subclasses of X.  Here is a brief description of the way 
     this works.  Suppose ", TT "X", " is the ", TO "parent", " of ", TT "P", ".  When an expression
     ", TT "-p", " is to be evaluated, where the class of ", TT "p", " is ", TT "P", ", then the method for
     ", TT "-P", " is applied, unless there isn't one, in which case the method for
     ", TT "-X", " is applied, and so on, all the way up the chain of parents to the
     topmost ancestor of everything, which is called ", TO "Thing", ".",
     PARA,
     "As an extreme example of inheritance, code like", 
     PRE "- Thing := x -> ...",
     "will install a method for negating anything, which will take
     effect as a last resort whenever a more specifically defined method
     isn't found.  It probably isn't a good idea to install such a method,
     for usually all it can do is to print an error message.",
     PARA,
     "The user may introduce new methods as well as new method names.  So it
     is important to understand how methods are installed and consulted.",
     PARA,
     "Applying a method named ", TT "C", " to a thing ", TT "x", " whose class is ", TT "X", " means that",
     PRE "(lookup(C,X)) x",
     "is evaluated.  In other words, ", TT "C", " is used as a key
     to obtain a function from ", TT "X", " (or its parent, grandparent,
     and so on), and the function is applied to ", TT "x", ".  See ", TO "lookup", ".",
     PARA,
     "Installing a method named ", TT "C", " for the class ", TT "X", " is done with code such
     as ",
     PRE "C X := (x) -> ( ... )",
     "where ", TT "( ... )", " represents suitable code for the operation at hand.",
     PARA,
     "The routine for making new methods is ", TO "method", ".",
     SEEALSO{"binary methods"}
     }

document { "inheritance from parents",
     "Each class has a parent class which can be used as a container
     for bits of code that apply to a more general class of objects.
     In this section we show how this mechanism works in detail.",
     PARA,
     "We begin by creating a new type of basic list.",
     EXAMPLE {
	  "X = new Type of BasicList",
	  "parent X",
	  },
     "The parent of ", TT "X", " is ", TO "BasicList", ", as desired,
     thus methods applicable to basic lists will also apply also
     to instances of ", TT "X", ".  One such method is the method
     for creating a net from a basic list; here is its code:",
     EXAMPLE "code(net,BasicList)",
     "This code is run automatically to display an instance of ", TT "X", ",
     so if we make one, we'll be able to see what it is:",
     EXAMPLE "x = new X from {2,3,4}",
     "Now let's imagine we wish to treat instances of ", TT "X", " as
     vectors, and to negate one by negating its entries.  As it
     happens, no method for this has been installed for basic lists,
     as we can check with ", TO "lookup", ".",
     EXAMPLE "lookup(symbol -, X) === null",
     "We install and test a new method as described in ", TO "installing methods", ".",
     EXAMPLE {
	  "- X := t -> apply(t,i -> -i);",
	  "- x"
	  },
     "This method will apply automatically to subclasses of ", TT "X", ",
     as we see now.",
     EXAMPLE {
	  "Y = new Type of X;",
	  "y = new Y from {4,5,6}",
	  "- y"
	  },
     "For ", TT "binary methods", ", there is an apparent ambiguity in
     deciding exactly how inheritance will work.  Let's illustrate
     by making a new subclass ", TT "Z", " of ", TT "X", ".",
     EXAMPLE {
	  "Z = new Type of X;",
	  "z = new Z from {7,8,9}",
	  },
     "Now let's install two methods, either of which might conceivably
     be applied to evaluate the expression ", TT "y+z", ", and see
     what happens.",
     EXAMPLE {
	  "Y + X := (a,b) -> XY;",
	  "X + Z := (a,b) -> ZX;",
	  "y + z"
	  },
     "The result is the symbol ", TT "XY", ".  The reason is that
     after finding that no method applies directly for adding
     an instance of ", TT "Y", " to an instance of ", TT "Z", ", the 
     search continues: ", TT "Z", " is replaced by its parent ", TT "X", ", 
     and so on.  (After enough unsuccessful iterations of this, the 
     second type is reset to ", TT "Z", ", the first type is replaced 
     by its parent, and the search continues.)",
     PARA,
     "The same search order applies to method functions defined with
     ", TO "method", "."
     }

document { method => Options,
     Headline => "method functions with optional arguments",
     Synopsis => {
	  "f = method(Options => {a=>x, b=>y, ...})",
	  "{a=>x, b=>y, ...}" => {
	       "a list of names ", TT "a", ", ", TT "b", ", ..., for optional
	       arguments with default values ", TT "x", ", ", TT "y", ", ... ."
	       },
	  "f" => "a method function that accepts optional arguments"
	  },
     "The list of options could be replaced by the corresponding ", TT "OptionTable", ".",
     PARA,
     "The methods installed for this method function should be written in
     the form ", TT "opts -> args -> (...)", ".  The argument ", TT "args", "
     will be assigned a hash table of type ", TO "OptionTable", " containing 
     the optional argument names and their current values.  For example,
     in the body of the function, the current value for the argument named
     ", TT "b", " can be recovered with ", TT "opts#b", ", or with ", TT "opts.b", ",
     in case ", TT "b", " is known to be a global symbol.  Warning: be careful 
     not to change the value of ", TT "b", ", or the code will stop working; it
     would be a good idea to protect it.  The default option table for ", TT "f", " 
     can be recovered with the function ", TO "options", ".",
     PARA,
     "In this example we make a linear function of a single real variable whose 
     coefficients are provided as optional arguments.",
     EXAMPLE {
	  "protect Slope; protect Intercept;",
	  "f = method(Options => {Slope => 1, Intercept => 1})",
      	  "f RR := o -> x -> o.Slope * x + o.Intercept",
      	  "f(5.)",
      	  "f(5.,Slope=>100)",
	  "options f",
	  }
     }

document { "printing and formatting for new classes",
     "After making a new type, it's desirable to install methods
     for displaying the instances of the new type in various formats.",
     EXAMPLE {
	  "Qu = new Type of List",
	  "w = new Qu from {1,-2,0,4}",
	  },
     "For example, it's desirable to display the quaternion above
     so it looks like a quaternion.  One way to achieve this is to install
     first a method for creating an ", TO "Expression", " from a
     quaternion, since there are methods already installed for converting
     expressions to common forms of output, such as to nets, which are
     used most commonly.",
     EXAMPLE {
	  ///expression Qu := z -> (
	       expression z#0 +
	       expression z#1 * expression "I" +
	       expression z#2 * expression "J" +
	       expression z#3 * expression "K");///,
	  ///net Qu := z -> net expression z;///,
	  ///toString Qu := z -> toString expression z;///,
	  ///tex Qu := z -> tex expression z;///,
	  ///html Qu := z -> html expression z;///,
	  "w",
	  "toString w",
	  "tex w",
	  "html w",
     	  },
     "Of course, now that we've decided that there should be certain
     quaternions called ", TT "I", ", ", TT "J", ", and ", TT "K", ",
     perhaps we should install them, too.",
     EXAMPLE {
	  "I = new Qu from {0,1,0,0}",
	  "J = new Qu from {0,0,1,0}",
	  "K = new Qu from {0,0,0,1}",
	  "2*I + 5*J",
	  "peek oo"
	  }
     }
