--		Copyright 1993-1998 by Daniel R. Grayson

document { "free modules",
     "This node has not been written yet."
     }

document { "making modules from matrices",
     "This node has not been written yet."
     }

document { "manipulating modules",
	  -- document the way to get maps between a module M and its
	  -- version as a cokernel in the overview
     "This node has not been written yet."
     }

document { "maps between modules",
     	  -- (R^5)_{0}
     "This node has not been written yet."
     }

document { "bases of parts of modules",
     "This node has not been written yet."
     }

document { "free resolutions of modules",
     "This node has not been written yet."
     }

document { "making chain complexes by hand",
     "This node has not been written yet."
     }

document { "extracting information from chain complexes",
     "This node has not been written yet."
     }

document { "manipulating chain complexes",
     "This node has not been written yet."
     }

document { "maps between chain complexes",
     "This node has not been written yet."
     }

document { "coherent sheaves",
     "This node has not been written yet."
     }

document { "language and programming overview",
     "In this section we give a comprehensive overview of the user
     language and the main programming features of Macaulay 2.",
     PARA,
     MENU {
     	  (
	       "variables and symbols",
	       MENU {
     	       	    TO "valid names",
		    TO "assigning values",
		    TO "local variables in a file",
		    TO "viewing the symbols defined so far",
		    TO "subscripted variables",
		    TO "numbered variables",
		    }
	       ),
	  (
	       "overview of functions",
	       MENU {
		    TO "using functions",
		    TO "making functions",
		    TO "local variables in a function",
		    TO "making functions with a variable number of arguments",
		    TO "using functions with optional arguments",
		    TO "making new functions with optional arguments",
		    }
	       ),
	  (
	       "basic types",
	       MENU {
		    TO "strings",
		    TO "nets",
		    TO "lists",
		    TO "sequences",
		    TO "hash tables",
		    }
	       ),
	  (
	       "control structures",
	       MENU {
		    TO "loops",
		    TO "mapping over lists",
		    TO "mapping over hash tables",
		    TO "conditional execution",
		    TO "error handling",
		    }
	       ),
	  (
	       "input and output",
	       MENU {
		    TO "printing to the screen",
		    TO "reading files",
		    TO "getting input from the user",
		    TO "creating and writing files",
		    TO "two dimensional formatting",
		    TO "communicating with programs",
		    TO "using sockets",
		    }
	       ),
	  (
	       "interfacing with the system",
	       MENU {
		    TO "running programs",
		    }
	       ),
	  (
	       "classes and types",
	       MENU {
		    TO "what a class is",
		    TO "installing methods",
		    TO "inheritance from parents",
		    TO "making new types",
		    TO "printing and formatting for new types",
		    (
			 "method functions",
			 MENU {
			      TO "making a new method function",
			      TO "method functions with a variable number of arguments",
			      TO "method functions with optional arguments",
			      }
			 ),
		    },
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
     "Other symbols refer to functions built in to Macaulay 2 which provide
     much of its functionality, as we will learn.",
     PARA,
     "The class of all symbols is ", TO "Symbol", "."
     }

document { "assigning values",
     "Use an equal sign to assign values to variables.",
     EXAMPLE {
	  "x",
	  "x = \"abcde\"",
	  "x"
	  },
     "Before assignment, any reference to a variable provides the symbol
     with that name.  After assignment, the assigned value is provided.
     The variable created is global, in the sense that any code placed
     elsewhere which contains a reference to a variable called ", TT "x", "
     will refer to the one we just set.",
     }

document { "local variables in a file",
     "There is a way to construct variables which can be used within a given
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
     this by writing a function called ", TT "compose", " which will
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
     which each other, and the memory they occupy will be retained as long 
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
     which will signal an error if it's given more than one argument: put
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
     arguments of ", TT "f", ".  We use the ", TT "@", " operator to attach the default
     values to our function, coded in a special way.",
     EXAMPLE {
	  "f = {Slope => 1, Intercept => 1} @ 
    (opts -> x -> x * opts.Slope + opts.Intercept)",
	  "f 5",
	  "f(5,Slope => 100)",
	  "f(5,Slope => 100, Intercept => 1000)",
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
	  "f = {a => 1000} @ (o -> (x,y) -> x * o.a + y);",
	  "f(3,7)",
	  "f(5,11,a=>10^20)",
	  },
     }

document { "conditional execution",
     "The basic way to control the execution of code is with the ", TO "if", "
     expression.  Such an expression typically has the form ", TT "if X then Y else Z", "
     and is evaluated as follows.  First ", TT "X", " is evaluated.  If the result is ", TT "true", ",
     then the value of ", TT "Y", " is provided, and if the result is ", TT "false", ", then the value of ", TT "Z", "
     is provided.  An error is signalled if the value of ", TT "X", " is anything but ", TT "true", " or
     ", TT "false", ".",
     EXAMPLE {
	  ///(-4 .. 4) / 
     (i -> if i < 0 then "neg" 
          else if i == 0 then "zer" 
          else "pos")///
	  },
     "The else clause may be omitted from an ", TT "if", " expression.  In that case, 
     if value of the predicate ", TT "X", " is false, then ", TT "null", " is provided 
     as the value of the ", TT "if", " expression.",
     EXAMPLE {
	  ///(-4 .. 4) / 
     (i -> if i < 0 then "neg" 
	  else if i == 0 then "zer")///
	  },
     "There is a variety of predicate functions (such as ", TT "<", ") which yield
     ", TT "true", " or ", TT "false", " and can be used as the predicate in 
     an ", TT "if", " expression. They include ",
     TO "==", ", ",
     TO "!=", ", ",
     TO "===", ", ",
     TO "=!=", ", ",
     TO "<", ", ",
     TO ">", ", ",
     TO "<=", ", ",
     TO ">=", ", ",
     TO ".?", ", ",
     TO "#?", ", ",
     TO "even", ", ",
     TO "odd", ", ",
     TO "member", ", ",
     TO "mutable", ", ",
     TO "isAffineRing", ", ",
     TO "isBorel", ", ",
     TO "isCommutative", ", ",
     TO "isDirectSum", ", ",
     TO "isField", ", ",
     TO "isFreeModule", ", ",
     TO "isHomogeneous", ", ",
     TO "isIdeal", ", ",
     TO "isInjective", ", ",
     TO "isIsomorphism", ", ",
     TO "isModule", ", ",
     TO "isPolynomialRing", ", ",
     TO "isPrime", ", ",
     TO "isPrimitive", ", ",
     TO "isQuotientModule", ", ",
     TO "isQuotientOf", ", ",
     TO "isQuotientRing", ", ",
     TO "isRing", ", ",
     TO "isSubmodule", ", ",
     TO "isSubset", ", ",
     TO "isSurjective", ", ",
     TO "isTable", ", ",
     TO "isUnit", ", and ",
     TO "isWellDefined", ".  Results of these tests may be combined with 
     ", TO "not", ", ", TO "and", ", and ", TO "or", "."
     }

document { "loops",
     "An expression of the form ", TT "while X do Y", " operates by evaluating
     ", TT "X", " repeatedly.  Each time the value of ", TT "X", " is true, 
     ", TT "Y", " is evaluated and its value is discarded.  When finally
     the value of ", TT "X", " is false the special value ", TT "null", " is 
     returned as the value of the ", TT "while", " expression.",
     EXAMPLE {
	  "i = 0;",
	  ///while i < 20 do (<< " " << i; i = i + 1); << endl;///
	  },
     "In the example above, ", TT "X", " is the predicate ", TT "i < 20", " and ", TT "Y", " is
     the code ", TT ///(<< " " << i; i = i + 1)///, ".  Notice the use of the
     semicolon within Y which separates two expression.",
     PARA,
     "The semicolon can also be used with the predicate ", TT "X", " to do other things
     before the test is performed.  This works because the value of an expression
     of the form ", TT "(A;B;C;D;E)", " is obtained by evaluating each of the
     parts, and providing the value of the last part (in this case, ", TT "E", "),
     as the value of the whole expression.  Thus, if the value of E is always
     true or false, the expression ", TT "(A;B;C;D;E)", " can be used as
     the predicate ", TT "X", ".  We illustrate this in the following example.",
     EXAMPLE {
	  "i = 0;",
	  ///while (<< " " << i; i < 20) do i = i+1; << endl///
	  },
     "As a further indication of the power of this construction, consider
     an expression of the following form.", 
     PRE "     while (A; not B) and (C; not D) do (E; F)",
     "It is the Macaulay 2 equivalent of C code that looks like this:",
     PRE "     while (TRUE) { A; if (B) break; C; if (D) break; E; F; }",
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
     "A local variable in a function is one which is not visible to
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
     its work with a local variable ", TT "i", " which is unrelated to the global 
     variable ", TT "i", " introduced on the first line.",
     PARA,
     "In the next example, we show that the local variables in two
     invocations of a function don't interfere with each other.  We
     write a function f which returns a newly created counting function.  
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
     "A string is a sequence of characters.  These strings can
     be manipulated in various ways to produce printed output.
     One enters a string by surrounding them with quotation marks.",
     EXAMPLE {
	  ///"abcdefghij"///,
	  },
     "Strings may contain newline characters.",
     EXAMPLE ///"abcde
fghij"///,
     "Strings, like anything else, can be assigned to variables.",
     EXAMPLE ///x = "abcdefghij"///,
     "There are escape sequences which make it possible to
     enter special characters:",  PRE 
"      \\n             newline
      \\f             form feed
      \\r             return
      \\\\             \\ 
      \\\"             \"
      \\t             tab
      \\xxx           ascii character with octal value xxx",
     EXAMPLE ///y = "abc\101\102\n\tstu"///,
     "We can use ", TO "peek", " to see what characters are in the string.",
     EXAMPLE "peek y",
     "Another way to enter special characters into strings is to use ", TO "///", "
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
	  "substring(x,5)",
	  "substring(x,5,2)",
	  },
     "The class of all strings is ", TO "String", "."
     }

document { "nets",
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
     "Each net has a width, depth, and height.  The width is the number
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
     TO (quote |, List, List), ", ",
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
     basic lists, useful for deriving news types of list which do not
     inherit methods for treating lists as vectors, is ", TO "BasicList", "."
     }

document { "sequences",
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
     "The operator ", TO (quote :, ZZ, Thing), " can be used to create sequences
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
     pairs of parentheses which are one level in.",
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
	       things which would proceed by examining the contents of the
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
     the speed.  A counter
     with initial value 1000000 is incremented each time a mutable thing is
     created, and its value is taken as the hash code of the thing and stored
     within it.  The strong comparison test cannot depend on the contents of
     mutable things, and thus such things appear to be containers with opaque
     walls.  For mutable things, the test for equality must be the same as 
     equality of the hash codes.",
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
     "In programming, loops which operate on consecutive elements of a
     list are common, so we offer various ways to apply functions to
     each of the elements of a list, along with various ways to treat the
     returned values.",
     PARA,
     "The most basic operation is provided by ", TO "scan", ", which applies
     a function consecutively to each element of a list, discarding the
     values returned.",
     EXAMPLE "scan({a,b,c}, print)",
     "By contrast, ", TO "apply", " will produced a list containing the
     values returned.",
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
     the function consecutively to an element from the first list and an
     element from the second list.",
     EXAMPLE {
	  "table({1,2,3},{7,8},(i,j) -> 1000*i+j)"
	  },
     "The function ", TO "applyTable", " can be used to apply a function to 
     each element of a doubly nested list (table).  Occasionally matrices
     are represented as a table, so this is a useful facility.",
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
     which is used to combine the values when a key occurs in both hash tables.",
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
     "Notice that the value returned is the standard output file
     ", TO "stdout", ".  We can also use ", TO "<<", " as a binary
     operator to print explicitly to this file, and the output will 
     appear on the screen.",
     EXAMPLE {
	  "stdout << 2^100"
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
     }

document { "reading files",
     "Typically a file will contain code written in the Macaulay 2 language.
     To load and execute that code, use ", TO "load", ".",
     EXAMPLE ///load "sample"///,
     "The command ", TO "needs", " can be used to load a file only if
     it hasn't already been loaded.",
     EXAMPLE ///needs "sample"///,
     "For debugging purposes, it is sometimes useful to be able to simulate
     entering the lines of a file one by one.  We use ", TO "input", " for
     this.",
     EXAMPLE ///input "sample"///,
     "The functions above look for files in every directory listed by
     the variable ", TO "path", ".  Our sample file is located in the
     directory called ", TT "packages", ", which is on the path.  Another
     copy of the sample file is located in our current directory.",
     EXAMPLE "path",
     "The function ", TO "get", " can be used to obtain the entire contents
     of a file as a single string.",
     EXAMPLE ///x = get "sample"///,
     "Use ", TO "peek", " to observe the extent of this string.",
     EXAMPLE ///peek x///,
     "The resulting string has newlines in it; we can use ", TO "lines", "
     to break it apart into a list of strings, with one row in each string.",
     EXAMPLE ///y = lines x///,
     "Use ", TO "peek", " to observe the extent of these strings.",
     EXAMPLE ///peek y///,
     "We could even use ", TO "verticalJoin", " to assemble the lines of the
     file into a net.",
     EXAMPLE ///verticalJoin y///,
     }

document { "getting input from the user",
     "The function ", TO "read", " can be used to prompt the user and obtain
     a line of input as a string.  In response to the prompt, we enter
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
	  ///"testfile" << "print " << 2^100 << endl << close///,
	  ///get "testfile"///,
	  ///load "testfile"///,
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
	  ///peek get "testfile"///,
	  },
     }

document { "two dimensional formatting",
     "We have seen that ", TO "nets", " are potentially useful for two
     dimesional formatting of output to an ascii terminal with limited
     graphical ability.  We present now a few more hints about putting
     this idea into practice.  Nets are used extensively in Macaulay 2
     for formatting, for example, for formatting of polynomials.",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[x,y,z];",
	  "f = random(R^1,R^{5:-3})",
	  },
     PARA,
     "Normally matrices such as the one above are displayed in compact
     notation that originated with Macaulay.  Setting the global flag
     ", TO "compactMatrixForm", " to ", TO "false", " will modify that
     behavior.",
     EXAMPLE {
	  "compactMatrixForm = false",
	  "f",
	  },
     "Output of routines such as ", TO "betti", " and ", TO "net", " that
     return nets can be easily incorporated into more complex displays 
     using standard operations on nets.",
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
     and ", TO "Product", " which we can use for this.",
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
     "Other types of ", TO "Expression", " which can be used for formatting
     nested lists are ", TO "MatrixExpression", " and ", TO "Table", ".",
     EXAMPLE {
	  "Table{{1,2,3},{a,bb,ccc}}",
	  "MatrixExpression{{1,2,3},{a,bb,ccc}}",
	  ///Table{{"Example 1","Example 2"},
      {Table{{1,2},{3,4}},Table{{11,22},{3,444}}}}///
	  },
     }

document { "making new types",
     "This node has not been written yet."
     }

document { "communicating with programs",
     "This node has not been written yet."
     }

document { "making a new method function",
     "This node has not been written yet."
     }

document { "what a class is",
     "This node has not been written yet."
     }

document { "installing methods",
     "This node has not been written yet."
     }

document { "method functions with a variable number of arguments",
     "This node has not been written yet."
     }

document { "inheritance from parents",
     "This node has not been written yet."
     }

document { "running programs",
     "This node has not been written yet."
     }

document { "using sockets",
     "This node has not been written yet."
     }

document { (quote @, List, Function),
     "This node has not been written yet."
     }

document { "method functions with optional arguments",
     "This node has not been written yet."
     }

document { (quote @, OptionTable, Function),
     "This node has not been written yet."
     }

document { "printing and formatting for new types",
     "This node has not been written yet."
     }
     