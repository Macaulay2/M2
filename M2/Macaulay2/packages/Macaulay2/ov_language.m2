document {
     Key => "The Macaulay2 language",
     "The Macaulay2 language is a simple yet powerful interpreted language.  Every 
     object has a type, such as Type, ZZ (integer), String, HashTable, Ring, Matrix,
     Function, etc.  User defined variables are hold values of any type.",
     PARA,
     "Expressions in Macaulay2 include function calls,
     control structures (e.g., ", TO "for", " and ", TO "while", " loops), function definitions,
     and operator expressions.
     Every expression returns an object, although ", TO "null", " is often returned if the 
     expression is only being used to produce some side effect.",
     PARA,

     "There is special syntax for creating and accessing strings, lists, sequences, and
     hashtables.  These are the key data types underlying many new types.  The Macaulay2
     engine implements rings, ring elements, and matrices, as instances of low-level types, 
     and various high-level types, visible to the user, are based on them.  Examples include
     ", TO "Ring", ", ", TO "RingElement", ", ", TO "Matrix", ", ", TO "Ideal", ", ", TO "RingMap", ", ", TO "Module", ", and ", TO "ChainComplex", ".",

     Subnodes => {
	  TO "overview of the Macaulay2 language",
	  TO "variables",
	  "basic data types",
	      TO "numeric types",
	      TO "strings",
	      TO "nets",
	      TO "lists",
	      TO "sequences",
	      TO "hash tables",
     	  "expressions",
	      TO "assignment statements",
	      TO "function calls",
	      TO "function definitions",
	      TO "if statements",
	      TO "while loops",
	      TO "for loops",
	      TO "try clauses",
	      TO "catch and throw",
	      TO "operators",
	  "input and output",
	      TO "printing to the screen",
	      TO "reading files",
	      TO "getting input from the user",
	      TO "creating and writing files",
	      TO "saving polynomials and matrices in files",
	      TO "two dimensional formatting",
	  "classes and types",
	      TO "a sample user defined type: Quaternions",
     	  "packages",
	      TO "packages",
	      TO "writing documentation",
	      TO "a sample package: Quaternions",
	  "other topics",
	      TO "interacting with the system",
	      TO "advanced input and output",
	      TO "debugging",
	  }
     }
------------------------------------------------------------------
document {
     Key => "variables",
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
     "Other symbols refer to functions built into Macaulay 2 that provide
     much of its functionality.",
     PARA,
     "Sometimes, one wants the symbol, rather than the particular value it
     happens to have at the moment.  For example after setting the value of
     ", TT "x", " to 5, the value of the variable ", TT "x", " is no longer the 
     symbol ", TT "x", ".
     Use ", TO "symbol", " to recover the symbol itself.",
     EXAMPLE {
	  "x=5",
	  "x",
	  "symbol x"
	  },
     SeeAlso => {
	  "symbol",
	  "local",
	  "global",
	  listUserSymbols,
	  clearOutput,
	  clearAll,
	  Symbol
	  }
     }
document {
     Key => "numeric types",
     HEADER3 "Integers and rational numbers",
     "In Macaulay2, integers and rational numbers have any number of digits
     (up to memory limits at least).",
     EXAMPLE {
	  "21672378126371263123123",
	  "3748568762746238746278/5876584978947",
	  },
     "Integers are elements of the ring ", TO ZZ, " of integers, and rational
     numbers are elements of the ring ", TO QQ, " of rational numbers.",
     PARA,
     "One point to notice is that there are two kinds of division, ", TO symbol/,
     " and ", TO symbol//, ".  The first
     returns a rational number (element of ", TO QQ, 
     "), while the second does division in ", TO ZZ, ".",
     EXAMPLE {
	  "6/3",
	  "7//3"
	  },
     HEADER3 "Real and complex numbers",
     "Real and complex numbers are approximate numbers, 
     implemented using the machine's double
     precision arithmetic.",
     EXAMPLE {
	  "1.372489274987",
	  "1.3454353 * 10^20",
	  "sqrt 4.5"
	  },
     EXAMPLE {
	  "1/(1+ii)",
	  },
     "There are also arbitrary precision real and complex numbers.  See
     ", TO "RRR", " or ", TO "CCC", " for more details.",
     SeeAlso => {
	  ZZ,
	  QQ,
	  RR,
	  CC,
	  RRR,
	  CCC
	  }
     }



------------------------------------------------------------------


document {
     Key => "expressions",
     "All expressions in Macaulay2 return a value, including if, while and for statements.
     This value may be ", TO null, ", which usually indicates that the expression is mainly 
     being used for side effects.",
     Subnodes => {
	  TO "assignment statements",
	  TO "function calls",
	  TO "function definitions",
	  TO "conditional execution",
	  TO "while loops",
	  TO "for loops",
	  TO "try clauses",
	  TO "catch and throw",
	  TO "operators"
	  }
     }

document {
     Key => "variables and symbols",
     Subnodes => {
	  TO "valid names",
	  TO "assigning values",
	  TO "local variables in a file",
	  TO "viewing the symbols defined so far",
	  TO "subscripted variables",
	  TO "numbered variables",
	  }
     }

document {
     Key => "functions",
     "In this section we present an overview of functions.",
     PARA,
     Subnodes => {
	  TO "using functions",
	  TO "using functions with optional inputs",
	  TO "making functions",
	  TO "local variables in a function",
	  TO "making functions with a variable number of arguments",
	  TO "making new functions with optional arguments",
	  TO "Function"					    -- should become a cross-reference
	  }
     }

document {
     Key => "basic types",
     "The basic type of an object is the way the object is
     essentially implemented internally.  It is not possible for
     the user to create new basic types.  For details, see
     ", TT "basictype", ".",
     SeeAlso => { "strings", "nets", "lists", "sequences", "hash tables" }
     }

document {
     Key => "control structures",
     Subnodes => {
	  TO "assignment",
	  TO "loops",
	  TO "mapping over lists",
	  TO "mapping over hash tables",
	  TO "conditional execution",
	  TO "setting an alarm",
	  TO "error handling",
	  }
     }

document {
     Key => "input and output",
     Subnodes => {
	  TO "printing to the screen",
	  TO "reading files",
	  TO "getting input from the user",
	  TO "creating and writing files",
	  TO "saving polynomials and matrices in files",
	  TO "two dimensional formatting",
	  }
     }




document {
     Key => "valid names",
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
     "Other symbols refer to functions built into Macaulay 2 that provide
     much of its functionality, as we will learn.",
     PARA,
     "The class of all symbols is ", TO "Symbol", "."
     }

document {
     Key => "assigning values",
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
     SeeAlso => { "GlobalAssignHook", "GlobalReleaseHook" }
     }

document {
     Key => "local variables in a file",
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
     loaded subsequently.  We illustrate this below with the variable ", TT "j", ".",
     PARA,
     EXAMPLE {
	  "h = () -> j",
	  "h()",
	  "j = 444",
	  "h()",
	  "j := 555",
	  "h()",
	  "j"
	  }
     }

document {
     Key => "viewing the symbols defined so far",
     "After using Macaulay 2 for a while, you may have stored data in
     several variables.  The system also stores output values for you
     in output variables.  You may wish to free the memory space occupied
     by that data, or you may simply wish to remind yourself where you
     stored a previously computed value.",
     PARA,
     "We may use the command ", TO "listUserSymbols", " to obtain a list of the user's
     symbols encountered so far.",
     EXAMPLE {
	  "ff = 20:4; h = true; kk",
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

document {
     Key => "subscripted variables",
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
     "Values can be assigned to these variables with ", TO "_", ".",
     EXAMPLE {
	  "y_10 = 555;",
	  "y_10",
	  },
     SeeAlso => {"IndexedVariable"}
     }

document {
     Key => "numbered variables",
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

document {
     Key => "using functions",
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

document {
     Key => "making functions",
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

document {
     Key => "making functions with a variable number of arguments",
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
	  "1 : 3",
	  "((x) -> x) oo",
	  }
     }

document {
     Key => "making new functions with optional arguments",
     "Let's consider an example where we wish to construct a linear function of ", TT "x", " 
     called ", TT "f", ", with the slope and y-intercept of the graph being optional
     arguments of ", TT "f", ".  We use the ", TO ">>", " operator to attach the default
     values to our function, coded in a special way.",
     EXAMPLE {
	  "opts = {Slope => 1, Intercept => 1}",
	  "f = opts >> o -> x -> x * o.Slope + o.Intercept",
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
	  "f = {a => 1000} >> o -> (x,y) -> x * o.a + y;",
	  "f(3,7)",
	  "f(5,11,a=>10^20)",
	  },
     }

document {
     Key => "conditional execution",
     Headline => "if-then-else statements",
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

document {
     Key => "loops",
     Headline => "for and while loops",
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
	  },
     UL {
	  TOH "while",
	  TOH "for"
	  }
     }


document {
     Key => "local variables in a function",
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

document {
     Key => "strings",
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
     Subnodes => {
     	  TO "String"
	  }
     }

document {
     Key => "nets",
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
     "We may extract the rows of a net as strings with ", TO "unstack", " and put
     them back together again with ", TO "stack", ".",
     EXAMPLE {
	  "v = unstack x",
	  "peek oo",
	  "stack v"
	  },
     Subnodes => {
	  TO "Net"
	  }
     }

document {
     Key => "lists",
     "A list is a handy way to store a series of things.  We create one
     by separating the elements of the series by commas and surrounding 
     the series with braces.",
     EXAMPLE "x = {a,b,c,d,e}",
     "We retrieve the length of a list with the operator ", TO "#", ".",
     EXAMPLE "#x",
     "We use it also to obtain a particular element.  The elements are 
     numbered consecutively starting with 0.",
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
     Subnodes => {
	  TOH "List",
	  TOH "VisibleList",
	  TOH "BasicList",
	  TOH "Array"
	  }
     }

document {
     Key => "sequences",
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
     "Replicating something once results in a sequence that cannot be represented
     in the usual way with commas.",
     EXAMPLE "1:a",
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
     include ", TO "sequence", " and ", TO "deepSplice", ".",
     Subnodes => {
	  TOH "Sequence"
	  }
     }

document {
     Key => "hash tables",
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

document {
     Key => "operators",
     "Here we list all of the unary and binary operators in the language.",
     HEADER3 "assignment",
     UL {
	  TOH symbol=,
	  TOH symbol:=,
	  TOH symbol<-
	  },
     HEADER3 "equality testing",
     UL {
	  TOH symbol==,
	  TOH symbol!=,
	  TOH symbol===,
	  TOH symbol=!=
	  },
     HEADER3 "comparison of objects",
     UL {
	  TOH symbol<,
	  TOH symbol<=,
	  TOH symbol>,
	  TOH symbol>=,
	  TOH symbol?
	  },
     HEADER3 "predicates",
     UL {
	  TOH symbol and,
	  TOH symbol or,
	  TOH symbol not
	  },
     HEADER3 "functions",
     UL {
          SEQ (TO " ", " -- function application"),
	  TOH symbol->,
	  TOH symbol=>,
          SEQ (TO "@@", " -- composing functions"),
          SEQ (TO "\\", " -- applying a function to elements of a list"),
          SEQ (TO ">>", " -- bit shifting and attaching optional inputs to functions"),
	  },
     HEADER3 "subscripting and object access",
     UL {
          SEQ (TO "_", " -- subscripting"),
          SEQ (TO ".", " -- hash table access or assignment"),
          SEQ (TO ".?", " -- test for hash table access"),
          SEQ (TO "#", " -- hash table access; length of a list, sequence or hash table"),
          SEQ (TO "#?", " -- test for hash table access"),
	  },
     HEADER3 "arithmetic operators",
     UL {
          SEQ (TO "!", " -- factorial"),
	  TOH symbol+,
	  TOH symbol-,
	  TOH symbol*,
	  SEQ (TO "/", " -- division, or applying a function to elements of a list"),
	  TOH symbol//,
	  TOH symbol%,
          SEQ (TO "\\\\", " -- left quotient"),
	  TOH symbol^,
	  TOH symbol\\,
          SEQ (TO "&", " -- bit-wise and"),
	  HR,
	  TOH symbol++,
	  TOH symbol**,
          SEQ (TO "^**", " -- tensor power"),
          SEQ (TO "~", " -- making a coherent sheaf"),
	  TOH symbol(*),
          SEQ (TO ":", " -- ideal quotient, repetitions"),
     	  },
     HEADER3 "miscellaneous",
     UL {
          SEQ (TO "..", " -- sequence builder"),
          SEQ (TO "<<", " -- file output, bit shifting"),
          SEQ (TO ">>", " -- bit shifting and attaching optional inputs to functions"),
          SEQ (TO "|", " -- horizontal concatenation of strings or matrices"),
          SEQ (TO "||", " -- vertical concatenation of strings or matrices"),
          SEQ (TO "@"),
          SEQ (TO "^^"),
          SEQ (TO "|-"),
          SEQ (TO "<==>"),
          SEQ (TO "===>"),
          SEQ (TO "==>"),
     	  },
     HEADER3 "Overloadable unary prefix and postfix operators",
     HEADER3 "Overloadable binary operators",
     "Except for the builtin operators, each operator may be overloaded: i.e.
     have methods
     installed for handling arguments of specific types.  Here is an example.",
     HEADER3 "built-in operators",
     "These operators are builtin and cannot be overloaded.",
     UL {
	  TOH symbol=,
	  TOH symbol:=,
	  TOH symbol<-,
	  TOH symbol===,
	  TOH symbol=!=,
	  TOH symbol->,
	  TOH symbol=>,
	  TOH symbol.,
	  TOH symbol#,
	  TOH symbol.?,
	  TOH symbol#?,
          SEQ (TO ",", " -- separates elements of lists or sequences"),
          SEQ (TO ";", " -- statement separator"),
          SEQ (TO "in"),
	  },
     
     }

document {
     Key => "syntax",
     "A newline ends a statement if it can, otherwise it acts like any
     white space.",
     EXAMPLE "2+\n3+\n4",
     PARA,
--     "Parsing is determined by a triple of numbers attached to each token.
--     The following table (produced by the command ", TO "seeParsing", "), 
--     displays each of these numbers.",
--     EXAMPLE {
--	  "debug Macaulay2Core",
--	  "seeParsing()"
--	  },
--     "Here is the way these numbers work.  The parser maintains a number
--     which we will call the current parsing level, or simply, the level.
--     The parser builds up an expression until it encounters an input token
--     whose parsing precedence is less than or equal to the current level.
--     The tokens preceding the offending token are bundled into an expression
--     appropriately and incorporated into the containing expression.",
     PARA,
     "When an operator or token is encountered, its binding strength serves
     as the level for parsing the subsequent expression, unless the current level
     is higher, in which case it is used.",
     PARA,
     "Consider a binary operator such as ", TT "*", ".  The relationship between
     its binary binding strength and its parsing precedence turns out to determine
     whether ", TT "a*b*c", " is parsed as ", TT "(a*b)*c", " or as ", TT "a*(b*c)", ".
     When the parser encounters the second ", TT "*", ", the current parsing level 
     is equal to the binding strength of the first ", TT "*", ".  If the binding 
     strength is less than the precedence, then the second ", TT "*", " becomes
     part of the right hand operand of the first ", TT "*", ", and the
     expression is parsed as ", TT "a*(b*c)", ".  Otherwise, the expression is
     parsed as ", TT "(a*b)*c", ".",
     PARA,
     "For unary operators, the unary binding strength is used instead of the binary
     binding strength to reset the current level.  The reason for having both numbers 
     is that some operators can be either unary or binary, depending on the context.
     A good example is ", TO "#", " which binds as tightly as ", TO ".", "
     when used as an infix operator, and binds as loosely as adjacency or
     function application when used as a prefix operator.",
     PARA,
     "To handle expressions like ", TT "b c d", ", where there are no tokens present
     which can serve as a binary multiplication operator, after parsing ", TT "b", ",
     the level will be set to 1 less than the precedence of an identifier,
     so that ", TT "b c d", " will be parsed as ", TT "b (c d)", ".",
     PARA,
     "The comma and semicolon get special treatment; the empty expression can
     occur to the right of the comma or semicolon or to the left of the comma."
     }
