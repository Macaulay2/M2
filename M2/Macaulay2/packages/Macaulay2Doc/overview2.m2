--		Copyright 1993-1998 by Daniel R. Grayson

///

--this is an old node, apparently


///

document {
     Key => "making modules from matrices",
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

document {
     Key => "manipulating modules",
     "Suppose we have a module that is represented as an image of a
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
	  "f = N.cache.pruningMap",
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
     PARA{},
     "Functions for finding related modules:",
     UL {
	  TO "ambient",
	  TO "cover",
	  TO "super",
	  },
     EXAMPLE {
	  "super M",
	  "cover N",
	  },
     "Some simple operations on modules:",
     UL {
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

document {
     Key => "maps between modules",			    -- map
     "Maps between free modules are usually specified as matrices, as
     described in the section on ", TO "matrices", ".  In this section 
     we cover a few other techniques.",
     PARA{},
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
     PARA{},
     "Natural maps between modules can be obtained with ", TO "inducedMap", "; the
     first argument is the desired target, and the second is the source.",
     EXAMPLE {
	  "inducedMap(source f, ker f)",
	  "inducedMap(coker f, target f)",
	  },
     }

document {
     Key => "free resolutions of modules",
     "The function ", TO "resolution", " (also called ", TT "res", "),
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
     the options that can be used to control the computation.",
     PARA{},
     "The same function, applied to a map ", TT "f", ", will produce a map
     from a free resolution of the source of ", TT "f", " to a free resolution of
     the target of ", TT "f", ".",
     EXAMPLE {
	  "h = resolution inducedMap(M, m^2/m^4)"
	  }
     }

document {
     Key => "extracting information from chain complexes",
     "Let's make a chain complex.",
     EXAMPLE {
	  "R = ZZ/101[x,y,z];",
      	  "C = res coker matrix {{x,y^2,z^3}};",
	  },
     "Some simple functions for discovering the shape of ", TT "C", ".",
     UL {
	  TO (length, ChainComplex),
	  TO (max, GradedModule),
	  TO (min, GradedModule),
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
     elements by degree, at least for resolutions of homogeneous modules.",
     EXAMPLE {
	  "betti C"
	  },
     "The ranks are displayed in the top row, and below that
     in row ", TT "i", " column ", TT "j", " is displayed the number of
     basis elements of degree ", TT "i+j", " in ", TT "C_j", "."
     }

document {
     Key => "making chain complexes by hand",
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

document {
     Key => "manipulating chain complexes",
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

document {
     Key => "maps between chain complexes",
     "One way to make maps between chain complexes is by lifting maps between
     modules to resolutions of those modules.  First we make some modules.",
     EXAMPLE {
	  "R = QQ[x,y];",
	  "M = coker vars R",
	  "N = coker matrix {{x}}",
	  },
     "Let's construct the natural map from ", TT "N", " to ", TT "M", ".",
     EXAMPLE {
	  "f = inducedMap(M,N)"
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
     "We can use ", TO "inducedMap", " or ", TT "**", " to construct natural maps 
     between chain complexes.",
     EXAMPLE {
	  "inducedMap(C ** R^1/x,C)",
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

document {
     Key => "coherent sheaves",
     "The main reason to implement algebraic varieties is support the
     computation of sheaf cohomology of coherent sheaves, which doesn't
     have an immediate description in terms of graded modules.",
     PARA{},
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
     SeeAlso => {
	  (cohomology, ZZ, CoherentSheaf),
	  (cohomology, ZZ, SumOfTwists)
	  }
     }


document {
     Key => "hashing",
     "A hash table contains a set of key-value pairs.  The access
     functions for hash tables accept a key and retrieve the
     corresponding value.  Here are the details, together with a
     discussion of how we designed the hash table system seen in
     Macaulay2.",
     PARA{},
     "The keys and values are stored in the hash table.  The hash table consists
     of a certain number of ", ITALIC "buckets", ", each of which can hold an
     arbitrary number of key-value pairs.  The number of buckets is chosen
     to be large enough that typically one may expect each bucket to hold fewer than
     three key-value pairs.  The key is used  as follows to determine in which
     bucket the key-value pair is to be stored.  The function ", TO "hash", " 
     is applied to the key to produce, in a deterministic way, an integer called
     the hash code, and the remainder of the hash code upon division by the
     number of buckets tells which bucket will be used.",
     PARA{},
     "It is essential that the
     hash code of a key never change, for otherwise the next 
     attempt to find the key in the hash table will have an unpredictable 
     result - the new hash code of the key may or may not lead to 
     the same bucket, and the value may or may not be located.",
     PARA{},
     "Some hash tables and lists are ", TO "mutable", ", i.e., their 
     contents can be altered by assignment statements.  As explained
     above, the hash code of a mutable thing is not permitted to
     change when the contents of the thing change.  Hence, the 
     algorithm for computing the hash code may not refer to the
     contents of a mutable thing.",
     PARA{},
     "The strong comparison operator ", TO "===", " is provided to 
     parrot the equality testing that occurs implicitly when 
     accessing a key in a hash table.  The fundamental requirement for this
     strong comparison operator is that things with different hash codes must also
     turn out to be different when tested with this comparison operator.",
     PARA{},
     "Here we come to a question of design.  As discussed above, we must assign
     hash codes to mutable things in such a way that the hash codes don't depend
     on their contents.  We can do this in various ways.",
     UL {
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
     PARA{},
     "In Macaulay2, we chose the second approach listed above; we expect to
     have many mutable things appearing as keys in hash tables, and we need
     the speed.  A counter with initial value 1000000 is incremented each time 
     a mutable thing is created, and its value is taken as the hash code of the
     thing and stored within it.  The strong comparison test cannot depend on 
     the contents of mutable things, and thus such things appear to be 
     containers with opaque walls.  For mutable things, the test for equality 
     must be the same as equality of the hash codes.",
     PARA{},
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
     PARA{},
     "One further comforting remark: the routines that compute hash 
     codes or strong equality do not get into infinite loops, despite 
     the existence of circular structures: any circular structure 
     must come into being by means of changing something, and
     so the circular loop in the structure necessarily involves a 
     mutable thing, whose contents are not examined by the routines.
     This provides another argument in favor of taking the second approach listed
     above.",
     SeeAlso => "HashTable"
     }

document {
     Key => "mapping over hash tables",
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
     result is a hash table whose keys are those occurring in one of the two
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

document {
     Key => "error handling",
     Headline => "signalling and trapping errors",
     "When an error occurs in your program, an error message will appear that
     gives the name of the file, the line number, and the column number of
     the code that provoked the error.",
     PARA{},
     "You may use the function ", TO "error", " in your programs to signal
     errors.  Your error message will appear on the screen and execution
     will be stopped.",
     PARA{},
     "The function ", TO "try", " can be used to catch an error before
     execution is stopped and to continue or to try something else.",
     Subnodes => {
	  TO "error",
	  TO "try"
	  }
     }

document {
     Key => "printing to the screen",
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
     PARA{},
     "If long lines get displayed too slowly, such as in emacs, then the user may choose
     to put a line such as ", TT "truncateOutput 100", " into an ", TO "initialization file", ".
     Time is still spent creating the wide output that is eventually truncated.",
     EXAMPLE {
	  "truncateOutput 50",
	  "41!",
	  "42!",
	  "43!",
	  "truncateOutput infinity",
	  "43!"
	  }
     }

document {
     Key => "reading files",
     "Sometimes a file will contain a single expression whose value you wish
     to have access to.  For example, it might be a polynomial produced by
     another program.  The function ", TO "get", " can be used to obtain 
     the entire contents of a file as a single string.  We illustrate this
     here with a file whose name is ", TT "expression", ".",
     PARA{},
     "First we create the file by writing the desired text to it.",
     EXAMPLE {
	  "fn = temporaryFileName()",
	  "fn << \"z^6+3*x*z^4+6*y*z^4+3*x^2*z^2+12*x*y*z^2+12*y^2*z^2+x^3+6*x^2*y+12*x*y^2+8*y^3\" << endl << close"
	  },
     "Now we get the contents of the file, as a single string.",
     EXAMPLE "get fn",
     "This isn't quite what you want, because a string containing a description
     of a polynomial is not the same as a polynomial.  We may use
     ", TO "value", " to evaluate the string and produce the corresponding
     polynomial, after first setting up a polynomial ring to contain it.",
     EXAMPLE {
	  ///R = ZZ/101[x,y,z]///,
	  ///f = value get fn///,
	  ///factor f///,
	  },
     "Often a file will contain code written in the Macaulay2 language.
     Let's create such a file.",
     EXAMPLE {
	  "fn << \"sample = 2^100\nprint sample\n\" << close"
	  },
     "Now verify that it contains the desired text with ", TO "get", ".",
     EXAMPLE ///get fn///,
     "To load and execute that code, use ", TO "load", ".  This is the function
     you will use most often for using bits of code you have previously written,
     unless you have incorporated them into a package, in which case you would
     use ", TO "loadPackage", ".",
     EXAMPLE ///load fn///,
     "The command ", TO "needs", " can be used to load a file only if
     it hasn't already been loaded.",
     EXAMPLE ///needs fn///,
     PARA {
	  "For debugging or display purposes, it is sometimes useful to be able 
	  to simulate entering the lines of a file one by one, so they appear
	  on the screen along with prompts and output lines.  One may use
	  ", TO "input", " for that."
	  -- we don't illustrate the use of 'input' here, because the documentation example parser can't handle it
	  },
     PARA {
	  "There are other ways to manipulate the contents of a file with
	  multiple lines.  First, let's use ", TO "peek", " to observe the 
	  extent of this string returned by ", TO "get", "."},
     EXAMPLE ///peek get fn///,
     "The resulting string has newlines in it; we can use ", TO "lines", "
     to break it apart into a list of strings, with one row in each string.",
     EXAMPLE ///lines get fn///,
     "We may use ", TO "peek", " to observe the extent of these strings.",
     EXAMPLE ///peek lines get fn///,
     "We could even use ", TO "stack", " to assemble the lines of the
     file into a net.",
     EXAMPLE ///stack lines get fn///,
     "Now let's remove that file.",
     EXAMPLE ///removeFile fn///
     }

document {
     Key => "getting input from the user",
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

document {
     Key => "creating and writing files",
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
	  ///removeFile "testfile"///
	  }
     }

document {
     Key => "two dimensional formatting",
     "We have seen that nets (see ", TO Net, ") are potentially useful for two
     dimensional formatting of output to an ascii terminal with limited
     graphical ability.  We present now a few more hints about putting
     this idea into practice.  Nets are used extensively in Macaulay2
     for formatting, for example, for formatting of polynomials and
     matrices.",
     EXAMPLE lines ///
	  R = ZZ/101[x,y,z];
	  f = random(R^1,R^{5:-3})
     ///,
     "Output of routines such as ", TO "betti", " and ", TO "net", " that
     return nets can be easily incorporated into more complex displays 
     using standard operations on nets (see ", TO "Net", ").",
     EXAMPLE lines ///
	  C = resolution cokernel f
	  be = betti C
	  "Betti numbers of " | net C | " are " | (net be)^2
     ///,
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
     PARA{},
     "The function ", TO "expression", " can be used to prepare things such
     as polynomials for formatting using the mechanism introduced above.",
     EXAMPLE {
	  "g = (x+y)^2",
	  "e = expression g",
	  "peek e",
	  },
     "In the example above, we see that ", TO "peek", " extracts only one
     level of the structure.  We may use ", TO "peek'", " to display
     the structure of ", TT "e", " to depth 2.",
     EXAMPLE {
	  "peek'(2,e)",
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

document {
     Key => {(isReady, File),isReady}, 
     Headline => "whether a file has data available for reading",
     Usage => "isReady f",
     Outputs => { Boolean => { "whether the input file ", TT "f", " has data available for reading" } },
     EXAMPLE lines ///
         f = openInOut "!cat"
     	 isReady f
     	 f << "hi there" << flush;
     	 isReady f
     ///
     }

document {
     Key => {(atEndOfFile, File),atEndOfFile},
     Headline => "test for end of file",
     Usage => "atEndOfFile f",
     Inputs => { "f" },
     Outputs => { Boolean => { "whether the input file ", TT "f", " is at the end" } },
     EXAMPLE lines ///
         f = openInOut "!cat"
     	 f << "hi there" << closeOut;
     	 atEndOfFile f
	 peek read f
     	 atEndOfFile f
     ///
     }

document {
     Key => "communicating with programs",
     "The most naive way to interact with another program is simply to run
     it, let it communicate directly with the user, and wait for it to
     finish.  This is done with the ", TO "run", " command.",
     EXAMPLE ///run "uname -a"///,
     "To run a program and provide it with input, one way is use the operator ", TO "<<", ",
     with a file name whose first character is an
     exclamation point; the rest of the file name will be taken as the
     command to run, as in the following example.",
     EXAMPLE ///"!grep a" << " ba \n bc \n ad \n ef \n" << close///,
     "More often, one wants to write Macaulay2 code to obtain
     and manipulate the output from the other program.  If the program
     requires no input data, then we can use ", TO "get", " with a file name whose first character is an
     exclamation point.  In the following example, we also peek at the string
     to see whether it includes a newline character.",
     EXAMPLE {
	  ///peek get "!uname -a"///,
	  },
     "Bidirectional communication with a program is also possible.  We use
     ", TO "openInOut", " to create a file that serves as a bidirectional
     connection to a program.  That file is called an input output file.  In
     this example we open a connection to the unix utility ", TT "egrep", "
     and use it to locate the symbol names in Macaulay2 that begin with
     ", TT "in", ".",
     EXAMPLE {
	  ///f = openInOut "!egrep '^in'"///,
	  ///scan(keys Core.Dictionary, key -> f << key << endl)///,
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
     PARA{},
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
     PARA{},
     "Another useful function is ", TO "wait", ", which can be used
     to wait for input to be available from any of a list of input
     files."
     }

document {
     Key => "using sockets",
     PARA{
	  "It's easy to use sockets as though they were files.  Simply replace
	  the file name by a string of the form ", TT "$host:service", " where
	  ", TT "host", " is the name of IP number of host to contact, and
	  ", TT "service", " is the port number or name to use.  If ", TT "service", "
	  is omitted, then port 2500 is used.  If ", TT "host", " is omitted, then
	  an incoming connection will be listened for."
	  },
     PARA{
	  "The following code will illustrate two-way communication using sockets
	  similar to the interaction used by web servers,
	  and you may try it out on your machine, unless a firewall prevents it."
	  },
     PRE ///if (pid = fork()) == 0 then (
     try "$:7500" << "hi there" << close;
     exit 0;
     )
sleep 2
get "$localhost:7500"
wait pid///,
     PARA {
	  "The code uses ", TO "fork", " to create a separate process that will listen for a connection on port
	  7500 and then send us a message.  The ", TO "sleep", " command pauses
	  for a while to make sure the child process has had time to start listening.
	  Then we use an ordinary input command, namely ", TO "get", ", to obtain the message.
	  Finally, we ", TO "wait", " for the child process to finish, as we should."
	  },
     SeeAlso => { "openInOut", "openListener", "getWWW" }
     }

document {
     Key => "making new classes",
     "All new classes are made with the operator ", TO "new", ".
     You may choose to implement the instances of your new class
     either as basic lists or as hash tables, or you may even
     base it on a subclass of ", TO "BasicList", " or a subclass of 
     ", TO "HashTable", ", if you find a class that has some
     of the methods you need already implemented.",
     PARA{},
     "As an example, we may wish to implement quaternions as lists
     of four real numbers.  We know that lists already have a method
     for addition that treats them as vectors, and we could use
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

document {
     Key => "making a new method function",
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
	  },
     Subnodes => {
	  TO "method"
	  }
     }

document {
     Key => "what a class is",
     "In Macaulay2 the behavior of a function depends heavily on the types
     of the arguments it's presented with.  For example, the expression ", TT "x+y", "
     means the sum if ", TT "x", " and ", TT "y", " are integers, but it
     means the union if ", TT "x", " and ", TT "y", " are sets.  To implement this
     in a clean fashion, we store the code for doing things with sets
     in something called ", TO "Set", " and we store the code for doing things with integers
     in something called ", TO "ZZ", ".  We say that each integer is an ", TO "instance", "
     of ", TO "ZZ", ", and ", TO "ZZ", " is the ", TO "class", " (or type) of each 
     integer.  The function ", TO "class", " provides the class of an object, and
     the function ", TO "instance", " tells whether a given object is an
     instance of a given class, or a subclass of it, and so on.",
     PARA{},
     EXAMPLE {
	  "class 33",
	  "instance(33,ZZ)",
	  "instance(33,String)"
	  },
     "The corresponding mathematical idea is that ", TO "ZZ", " is the set of
     all integers.",
     PARA{},
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
     PARA{},
     Subnodes => {
	  TO "class",
	  TO "parent",
	  TO "instance",
	  TO "ancestor",
	  },
     SeeAlso => { "uniform", "Thing", "Nothing", "Type", "MutableList", "MutableHashTable", "SelfInitializingType" }
     }

document {
     Key => "binary methods",
     "The method for computing a sum ", TT "x+y", " depends on the types of ", TT "x", " and ", TT "y", ".
     For example, the method for adding an integer ", TT "x", " and a polynomial 
     ", TT "y", " differs from the method for adding two integers modulo 111.  Because
     both the type of ", TT "x", " and the type of ", TT "y", " must enter into the selection of
     the method, we refer to these methods as binary methods.  Each binary
     method is a function of two variables, and is stored either in the class
     of ", TT "x", " or in the class of ", TT "y", ".",
     PARA{},
     "Let's assume that ", TT "X", " is the class (or type) of ", TT "x", ", 
     and that ", TT "Y", " is the class of ", TT "y", ".  The way to install a 
     method for the addition of an instance ", TT "x", " of class ", TT "X", " to 
     an instance ", TT "y", " of class ", TT "Y", " is with a statement of the form ",
     PRE "X + Y := (x,y) -> ( ... )",
     "where ", TT "( ... )", " represents the body of the function, consisting of suitable
     code for the operation at hand.",
     PARA{},
     "The method installed by the code above is automatically inherited by 
     subclasses of ", TT "X", " and ", TT "Y", ".  Here is a brief
     description of the way this works.  Suppose ", TT "X", " is the 
     ", TO "parent", " of ", TT "P", " and ", TT "Y", " is the parent of ", TT "X", ".  When 
     a sum ", TT "p+q", " is evaluated where the class of ", TT "p", " is 
     ", TT "P", " and the class of ", TT "q", " is ", TT "Q", ", then the binary
     method for ", TT "P+Q", " is applied, unless there isn't one, in which
     case the binary method for ", TT "P+Y", " is applied, unless there isn't
     one, in which case the binary method for ", TT "X+Q", " is applied,
     unless there isn't one, in which case the binary method for ", TT "P+Q", "
     is applied.  In general this search for a binary method continues all
     the way up the chain of parents to the topmost ancestor of everything,
     which is called ", TO "Thing", ".  (See also ", TO "lookup", ".)",
     PARA{},
     "As an extreme example of inheritance, the code ", 
     PRE "Thing + Thing := (x,y) -> ( ... )",
     "will install a binary method for adding any two things, which will take
     effect as a last resort whenever more a specifically defined method
     isn't found.",
     PARA{},
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

document {
     Key => "installing methods",
     "The method to be used for computing an expression such as ", TT "-x", " depends 
     on the type of ", TT "x", ".  For example, the method for negating a polynomial
     differs from the method for negating an integer modulo 111.  Each
     method is a function of one variable, and is stored in the class 
     of ", TT "x", " under a key that is referred to as the name of the method.
     For some built-in methods the method name is a symbol, but for
     methods created with ", TO "method", ", the method name is the same
     as the function used for calling it up.",
     PARA{},
     "Let's assume that ", TT "X", " is the class of ", TT "x", ".  The way to install a method
     for the negation of an instance ", TT "x", " of ", TT "X", " is with a statement of the 
     following form.",
     PRE "- X := x ->( ... )",
     "Here ", TT "( ... )", " represents the body of the function, consisting of
     suitable code for the operation at hand.",
     PARA{},
     "The method installed by the code above is automatically inherited by
     subclasses of X.  Here is a brief description of the way 
     this works.  Suppose ", TT "X", " is the ", TO "parent", " of ", TT "P", ".  When an expression
     ", TT "-p", " is to be evaluated, where the class of ", TT "p", " is ", TT "P", ", then the method for
     ", TT "-P", " is applied, unless there isn't one, in which case the method for
     ", TT "-X", " is applied, and so on, all the way up the chain of parents to the
     topmost ancestor of everything, which is called ", TO "Thing", ".",
     PARA{},
     "As an extreme example of inheritance, code like", 
     PRE "- Thing := x -> ...",
     "will install a method for negating anything, which will take
     effect as a last resort whenever a more specifically defined method
     isn't found.  It probably isn't a good idea to install such a method,
     for usually all it can do is to print an error message.",
     PARA{},
     "The user may introduce new methods as well as new method names.  So it
     is important to understand how methods are installed and consulted.",
     PARA{},
     "Applying a method named ", TT "C", " to a thing ", TT "x", " whose class is ", TT "X", " means that",
     PRE "(lookup(C,X)) x",
     "is evaluated.  In other words, ", TT "C", " is used as a key
     to obtain a function from ", TT "X", " (or its parent, grandparent,
     and so on), and the function is applied to ", TT "x", ".  See ", TO "lookup", ".",
     PARA{},
     "Installing a method named ", TT "C", " for the class ", TT "X", " is done with code such
     as ",
     PRE "C X := (x) -> ( ... )",
     "where ", TT "( ... )", " represents suitable code for the operation at hand.",
     PARA{},
     "The routine for making new methods is ", TO "method", ".",
     SeeAlso =>{"binary methods"}
     }

document {
     Key => "inheritance",
     "Each class has a parent class that can be used as a container
     for bits of code that apply to a more general class of objects.
     In this section we show how this mechanism works in detail.",
     PARA{},
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
     so trying to negate ", TT "x", " results in an error.",
     EXAMPLE {
	 "stopIfError = false;",
	 "- x",
	 },
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
	  "Y + X := (a,b) -> \"Y + X\";",
	  "X + Z := (a,b) -> \"X + Z\";",
	  "y + z"
	  },
     "The result is the string ", TT "Y + X", ".  The reason is that
     after finding that no method applies directly for adding
     an instance of ", TT "Y", " to an instance of ", TT "Z", ", the 
     search continues: ", TT "Z", " is replaced by its parent ", TT "X", ", 
     and so on.  (After enough unsuccessful iterations of this, the 
     second type is reset to ", TT "Z", ", the first type is replaced 
     by its parent, and the search continues.)",
     PARA{},
     "The same search order applies to method functions defined with
     ", TO "method", "."
     }

document {
     Key => "printing and formatting for new classes",
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

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
