document {
     Key => "The Macaulay2 language",
     
     PARA {
	  "The Macaulay2 language is a simple yet powerful interpreted language.  Every object has a type, such as ", TO "Type", ", ", TO "ZZ", "
	  (integer), ", TO "String", ", ", TO "HashTable", ", ", TO "Ring", ", ", TO "Matrix", ", ", TO "Function", ", etc.  User defined variables may
	  hold values of any type."
	  },
     
     PARA{
	  "The Macaulay2 language is based on expressions, which, generally speaking, are used to create new objects from old ones.
	  Other computer languages have things called statements, but Macaulay2 does not.
	  Other computer languages use newline characters and even indentation to indicate separate and nesting of statements, but Macaulay2 does not.
	  Expressions in Macaulay2 include function calls, control structures (e.g., ", TO "for", " and ", TO "while", " loops), function definitions, and
	  operator expressions.  Every expression returns an object, although the basic object ", TO "null", " is often returned if the expression is being 
	  used only to produce some side effect."
	  },

     PARA{
	  "There is special syntax for creating and accessing strings, lists, sequences, and hash tables.  These are the key data types underlying many new
	  types.  The Macaulay2 engine implements rings, ring elements, and matrices, as instances of low-level types, and various high-level types,
	  visible to the user, are based on them.  Examples include ", TO "Ring", ", ", TO "RingElement", ", ", TO "Matrix", ", ", TO "Ideal", ", ", 
	  TO "RingMap", ", ", TO "Module", ", and ", TO "Complexes::Complex", "."
	  },

     PARA{
	  "The user can create new types of higher level mathematical objects, whose types form a hierarchy that facilitates the installation and use of
	  appropriate methods for computing with them."
	  },

     Subnodes => {
	      TO "variables",
	      TO "comments",
	  "basic data types",
	      TO "numeric types",
	      TO "strings and nets",
	      TO "lists and sequences",
	      TO "hash tables",
     	  "expressions",
	      TO "operators",
	      TO "parsing precedence, in detail",
	      TO "conditional execution",
	      TO "control flow statements",
	      TO "error handling",
	  "functions",
	      TO "using functions",
	      TO "using functions with optional inputs",
	      TO "making functions",
	      TO "local variables in a function",
	      TO "making functions with a variable number of arguments",
	      TO "making functions with multiple return values",
	      TO "making new functions with optional arguments",
	      TO "caching computation results",
	      TO "using hooks",
	      --TO "code",
	  "classes and types",
	      TO "what a class is",
	      TO "installing methods",
	      TO "inheritance",
	      TO "making new classes",
	      TO "making a new method function",
	      --TO "methods",
	  "debugging Macaulay2 programs",
	      TO "debugging",
     	  "packages",
	      TO "packages",
	      TO "creating a package",
	      TO "writing documentation",
	  "parallel programming ",
	      TO "parallel programming with threads and tasks",
     	  "system facilities",
	      TO "system facilities",
	  "developer's corner",
	      TO "Core",
	      TO "the engine of Macaulay2",
	      TO "the interpreter of Macaulay2",
	  "reference material",
	      TO Thing,
	      TO Type,
	      TO ImmutableType,
	      TO Function,
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
     "Other symbols refer to functions built into Macaulay2 that provide
     much of its functionality.",
     PARA{},
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
	  },
     Subnodes => {
	  TO "valid names",
	  TO "assigning values",
	  TO "local variables in a file",
	  TO "viewing the symbols defined so far",
	  TO "subscripted variables",
	  TO "numbered variables",
	  TO "augmented assignment"
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
     PARA{},
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
     implemented using the ", TO "MPFR", " library.",
     EXAMPLE {
	  "1.372489274987",
	  "1.3454353 * 10^20",
	  "sqrt 4.5",
	  "toRR_200 4.5",
	  "sqrt oo"
	  },
     EXAMPLE {
	  "1/(1+ii)",
	  },
     SeeAlso => { Number, ZZ, QQ, RR, CC, toRR, toCC }
     }

------------------------------------------------------------------

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
     "Other symbols refer to functions built into Macaulay2 that provide
     much of its functionality, as we will learn.",
     PARA{},
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
     PARA{},
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
     variables are given new values that are the generators of the 
     polynomial ring.",
     PARA{},
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
     PARA{},
     "Assume the code above is entered directly by the user into Macaulay2.  Then
     the variable is still a local one and is not available to code previously loaded
     or to functions previously defined, but it will be available to code 
     loaded subsequently.  We illustrate this below with the variable ", TT "j", ".",
     PARA{},
     EXAMPLE {
	  "j",
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
     "After using Macaulay2 for a while, you may have stored data in
     several variables.  The system also stores output values for you
     in output variables.  You may wish to free the memory space occupied
     by that data, or you may simply wish to remind yourself where you
     stored a previously computed value.",
     PARA{},
     "We may use the command ", TO "listUserSymbols", " to obtain a list of the user's
     symbols encountered so far.",
     EXAMPLE {
	  "ff = 20:4; h = true; kk",
	  "listUserSymbols"
	  },
     "The symbols are listed in chronological order, and the type of value stored
     under it is displayed.",
     PARA{},
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
     "Values can be assigned to these variables with ", TO "=", ".",
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
     "There are many functions in Macaulay2 that do various things.  You can
     get a brief indication of what a function does by typing a ", TT "?", " before
     its name.  In this case, one would see that the function ", TO "sin", " takes a single argument
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
     is applied.  Let's illustrate this by making a function for squaring 
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
     PARA{},
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
     this by writing a function called ", TT "comp", " that will
     compose two functions, just as the operator ", TO "@@", " did
     above.",
     EXAMPLE {
	  "comp = (f,g) -> x -> f(g x)",
	  "sincos = comp(sin,cos)",
	  "cossin = comp(cos,sin)",
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
     PARA{},
     "The class of all functions is ", TO "Function", "."
     }

document {
     Key => "making functions with multiple return values",
     PARA {
	  "A function may return multiple values by returning ", ofClass Sequence, ".
	  Multiple assignment (see ", TO "=", ") or multiple local assignment (see ", TO ":=", ")
	  can be used to assign the values to separate variables, if the number of values to be returned
	  is known."
	  },
     EXAMPLE lines ///
     	  f = i -> (i, i^2, i^3);
	  (x,y,z) = f 3
	  x
	  y
	  z
     ///,
     PARA {
	  "Simple assignment may be used if the number of values to be returned is unknown."
	  },
     EXAMPLE lines ///
     	  s = f 4
	  #s
	  s_0
	  s_1
	  s_2
     ///
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
     PARA{},
     "In the example above, the inner function has just one argument, ", TT "x", ",
     but handling multiple arguments is just as easy.  Here is an example with two
     arguments.",
     EXAMPLE {
	  "f = {a => 1000} >> o -> (x,y) -> x * o.a + y;",
	  "f(3,7)",
	  "f(5,11,a=>10^20)",
	  },
     Subnodes => {
         TO "symbols used as the name or value of an optional argument",
         },
     }

document {
    Key => "control flow statements",
    Subnodes => {
	TO "for",
	TO "while",
	TO "list",
	TO "do",
	TO "break",
	TO "continue",
	TO "return",
    }
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
     results may be combined with ", TO "not", ", ", TO "and", ", and ", TO "or", ".",
     Subnodes => TO "if"
     }
document {
    Key => {"throw", "catch"},
    Headline => "throw and catch exceptions",
    Usage => "catch c\nthrow x",
    Outputs => {{
	    TT "catch", " returns the value obtained by evaluating the code ", TT "c",
	    ", or, if a ", TT "throw", " was executed during the evaluation of ", TT "c",
	    ", the argument given to ", TT "throw", "."}},
    Consequences => {{
	    TT "throw", " passes the flow of control to the surrounding ", TT "catch",
	    ", and ", TT "x", " is returned as its value"}},
     EXAMPLE lines ///
          catch scan(0..10, i -> if i == 5 then throw 18 else print i)
     ///}
document { Key => "continue",
     Headline => "continue with the next iteration of a loop",
     Usage => "continue x",
     Inputs => {"x"},
     Consequences => {
	  {"the currently executing ", TT "list", "-clause of a ", TO "for", "-loop or ", TO "while", "-loop is 
	       finished, and iteration continues with the ", TO "do", "-clause or the next iteration of
	       the loop, if any.  The value ", TT "x", " is added to the list being accumulated.
	       If ", TT "x", " is omitted, then no value is added to the list, and the statement may be used in a ", TT "do", "-clause."
	       },
	  {"Alternatively, as a debugger command, causes execution to be resumed, starting with the current expression."}
	  },
     EXAMPLE lines ///
          for i from 1 to 4 list (continue 4; print ho) do print hi
          for i from 1 to 4 list (continue ; 14) do print hi
          for i from 1 to 4 list 14 do print hi
	  i = 0 ; while i < 10 do ( i = i+1; if i == 5 then continue ; print i )
     ///,
     PARA {
	  "Here is an example of the use of ", TO "continue", " in the debugger after altering a value so continuation will not cause the
	  error to recur."
	  },
     EXAMPLE lines ///
     load "Macaulay2Doc/demos/demo1.m2"
     code g
     g 2
     code f
     x
     x = 11
     continue
     ///
     }
document {
     Key => {"if", "then", "else"},
     Headline => "condition testing",
     TT "if p then x else y", " computes ", TT "p", ", which must yield the value ", TO "true", " 
     or ", TO "false", ".  If true, then the value of ", TT "x", " is provided,
     else the value of ", TT "y", " is provided.",
     PARA{},
     TT "if p then x", " computes ", TT "p", ", which must yield the value ", TO "true", " 
     or ", TO "false", ".  If true, then the value of ", TT "x", " is provided,
     else ", TO "null", " is provided."
     }

document {
     Key => "return",
     Headline => "return from a function",
     Usage => "return x",
     Inputs => {
	  "x" => {"If ", TT "x", " is omitted, then ", TO "null", " is used."} 
	  },
     Consequences => {
	  {"Returns ", TT "x", " as the value of the function currently being evaluated."},
	  {"Alternatively, as a debugger command, returns ", TT "x", " as the value of the
	       current expression and stops execution at the next possible point, entering
	       the debugger again."}
	  },
     EXAMPLE {
	  "f = x -> (
     if x == 3 then return;
     if x > 3 then return x^2;
     5);",
	  "f 2",
	  "f 3",
	  "f 4"
	  },
     PARA {
	  "Here is an example of the use of ", TO "return", " as a debugger command."
	  },
     EXAMPLE lines ///
     load "Macaulay2Doc/demos/demo1.m2"
     code g
     g 2
     code f
     return 1/11
     continue
     ///,
     SeeAlso => { "break" }
     }

document {
     Key => "list",
     Headline => "loop control",
     TT "list", " a keyword used with ", TO "while", ", and ", TO "for", "."
     }

document {
     Key => "do",
     Headline => "loop control",
     TT "do", " a keyword used with ", TO "while", ", and ", TO "for", "."
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
	  TO "try",
	  TO "throw",
	  }
     }

document {
     Key => "try",
     Headline => "catch an error",
     Usage => "try x then y else z",
     Inputs => {
	  "x" => "code",
	  "y" => "code",
	  "z" => "code"
	  },
     Consequences => {
	  {"the code ", TT "x", " is run; if no error or ", TO "alarm", " occurs, then the code ", TT "y", " is run; otherwise, the code ", TT "z", " is run"}
	  },
     "The return value is the value returned by ", TT "y", " or ", TT "z", ", as the case may be.",
     PARA{},
     "The clause '", TT "then y", "' may be omitted, in which case the return value is the value returned by ", TT "x", ", if there is no error or alarm.",
     PARA{},
     "The clause '", TT "else z", "' may be omitted,
     in which case the return value is the value returned by ", TT "y", ",
     unless an error or alarm occurs, in which case ", TO "null", " is returned.",
     PARA{},
     "The clauses '", TT "then y else z", "' may both be omitted, in which case the return value is the value returned by ", TT "x", ", unless an error or
     alarm occurs, in which case ", TO "null", " is returned.",
     PARA{},
     "The behavior of interrupts (other than alarms) is unaffected.",
     EXAMPLE "apply(-3..3,i->try 1/i else infinity)",
     Caveat => "We will change the behavior of this function soon so that it will be possible to catch errors of a particular type.  Meanwhile, users are
     recommended to use this function sparingly, if at all."
     }

document {
     Key => "break",
     Headline => "break from a loop",
     Usage => "break x",
     Consequences => {
	  { "this interrupts execution of a loop controlled by ", TO "for", ", ", TO "while", ", ", TO "apply", ", or ", TO "scan", ", returning ",
     	       TT "x", " as the value of the loop currently being evaluated."
	       },
	  { "Alternatively, as a top level command in the debugger, it leaves the debugger, returning the user to top level." }
	  },
     PARA {
	  "Omitting ", TT "x", ", and executing ", TT "break", ", interrupts execution of a loop as above, returning
     	  ", TO "null", " as the value of the function currently being evaluated, except, in the case of 
	  a ", TO "for", " loop or a ", TO "while", " loop with a list clause, the list accumulated so far is returned as the value."
	  },
     EXAMPLE "for i from 1 to 10 do if i == 7 then break 12345",
     PARA {
     	  "Warning: trying to break from a loop controlled by ", TO "table", " will probably not do what you
	  expect, since ", TO "table", " is implemented by two nested
     	  loops controlled by ", TO "apply", ", and only the inner one will stop, as in the following example."
	  },
     EXAMPLE lines ///
     table(3,3,(i,j) -> if i == 1 then break 3 else "hi")
     table(3,3,(i,j) -> if j == 1 then break 3 else "hi")
     ///,
     PARA {
	  "Here is an example as a debugger command."
	  },
     EXAMPLE lines ///
     load "Macaulay2Doc/demos/demo1.m2"
     g 2
     x
     break
     g 3
     ///,
     SeeAlso => { "apply", "scan", "while", "for" }
     }

document {
     Key => "while",
     Headline => "while loops",
     Usage => "while p list x do z",
     Consequences => {
	  { "The expression ", TT "p", " is repeatedly evaluated.  As long as the result is true, ", TT "x", " is evaluated and its value is saved, and
	       ", TT "z", " is evaluated and its value is discarded.  When the value of ", TT "p", " is false, then the loop terminates, and the list
	       of values of ", TT "x", " is returned as the value of the entire expression."
	       }},
     EXAMPLE lines ///
         i = 0 ; while i < 10 list i^2 do i = i+1
     ///,
     PARA { "The ", TT "list x", " clause may be omitted, in which case no list is accumulated, and ", TO "null", " is returned as the
	  value of the expression." },
     EXAMPLE lines ///
         i = 0 ; while i < 4 do (print i; i = i+1)
     ///,
     PARA { "Alternatively, the ", TT "do z", " clause may be omitted." },
     EXAMPLE lines ///
         i = 0 ; while i < 10 list (i = i+1; i^2)
     ///,
     PARA {
	  "Observe the use of the semicolon (see ", TT ";", ") in the expression above."
	  },
     PARA { "If ", TO "continue", " is executed by ", TT "x", " then execution of ", TT "x", " is interrupted, no value is added to the list, and iteration of the loop
     	  continues." 
	  },
     EXAMPLE lines ///
         i = 0 ; while i < 10 list (i = i+1; if odd i then continue; i^2)
     ///,
     PARA { "If ", TT "continue w", " is executed by ", TT "x", " then execution of ", TT "x", " is interrupted, the value of ", TT "w", " is added to the list, 
     	  and iteration of the loop continues."
	  },
     EXAMPLE lines ///
         i = 0 ; while i < 10 list (i = i+1; if odd i then continue 1234; i^2)
     ///,
     PARA { "  If ", TT "break v", " is executed by ", TT "x", ", then the loop is stopped and ", TT "v", " is returned as its value." },
     EXAMPLE lines ///
         i = 0 ; while i < 10 list (i = i+1; if i == 5 then break i; i^2)
     ///,
     PARA { "If ", TO "break", " is executed by ", TT "x", ", then the loop is stopped and the list accumulated so far is returned as the value." },
     EXAMPLE lines ///
         i = 0 ; while i < 10 list (i = i+1; if i == 5 then break; i^2)
     ///,
     }

document { Key => {"for", "in", "from", "to", "when"},
     Headline => "for loops",
     SYNOPSIS (
     	  Usage => "for i from m to n when p list x do z", 
     	  Inputs => { "m" => ZZ, "n" => ZZ },
     	  Outputs => {{"the list of values of the clause ", TT "x", ", as described above"}},
	  Consequences => { 
	       {"The numbers ", TT "m", " and ", TT "n", " must be small integers that fit into a single word.
		 The variable ", TT "i", " is initialized to ", TT "m", ".  
		 As long as ", TT "i", " is not greater than ", TT "n", ", evaluation of the loop continues.  
		 First ", TT "p", " is evaluated;
		 as long as the value of ", TT "p", " is true, evaluation of the loop continues.  
		 Next ", TT "x", " is evaluated and its value is saved, and ", TT "z", " is evaluated and its value is discarded.  
		 Then ", TT "i", " is incremented by 1, and the loop repeats. 
		 When the value of ", TT "p", " is false, then the loop terminates, and the list of values of ", TT "x", " is returned as the value of the entire expression."
		 }}),
     SYNOPSIS (
	  Usage => "for i in v when p list x do z",
	  Inputs => {"v" => {ofClass{BasicList},
		  " or an instance of a class with the ", TO iterator,
		  " method installed"}},
	  Consequences => {{"The variable ", TT "i", " is set to consecutive values of the list ", TT "v", ".  First ", TT "p", " is evaluated.  
	       As long as the value of ", TT "p", " is true, evaluation of the loop continues.  Next ", TT "x", " is evaluated, and its value is saved.  Then
	       ", TT "z", " is evaluated and its value is discarded.  Then the loop repeats with the next element of ", TT "v", ".  When the value of ", TT "p", " is false,
	       then the loop terminates, and the list of values of ", TT "x", " is returned as the value of the entire expression."
	       }},
	  ),
     Caveat => {
     	  "The variable ", TT "i", " is a new local variable whose scope includes only the expressions ", TT "p", ", ", TT "x", ",
	  and ", TT "z", " in the body of the loop.  Moreover, new local variables defined inside the body of the loop will not
	  be visible outside it."
	  },
     SUBSECTION "examples",
     EXAMPLE lines ///
     	  for i from 1 to 5 when i < 15 list i^2 do print i
     	  for i from 1 to 5 when i^2 < 15 list i^2 do print i
     ///,
     PARA {
	  "The expressions in this construction may be arbitrarily complicated.
	  Here is an example where ", TT "z", " is a sequence of expressions separated by semicolons (see ", TO ";", ")."
	  },
     EXAMPLE ///for i from 1 to 3 do (
     print "The value of i is : ";
     print i
     )
     ///,
     PARA { "The ", TT "do z", " clause may be omitted." },
     EXAMPLE "for i from 1 to 5 when i < 15 list i^2",
     PARA { "The ", TT "from m", " clause may be omitted, in which case ", TT "i", " starts with ", TT "0", "." },
     EXAMPLE "for i to 5 when i < 15 list i^2",
     PARA { "The ", TT "when p", " clause may be omitted." },
     EXAMPLE "for i to 5 list i^2",
     PARA { "The ", TT "to n", " clause may be omitted." },
     EXAMPLE "for i when i < 15 list i^2",
     PARA { "The ", TT "list x", " clause may be omitted." },
     EXAMPLE "for i when i^2 < 15 do print i",
     PARA { "If ", TO "continue", " is executed by ", TT "x", " then execution of ", TT "x", " is interrupted, no value is added to the list, and iteration of the loop
     	  continues." 
	  },
     EXAMPLE "for i from 0 when i < 10 list (if odd i then continue; i^2)",
     PARA { "If ", TT "continue w", " is executed by ", TT "x", " then execution of ", TT "x", " is interrupted, the value of ", TT "w", " is added to the list, 
     	  and iteration of the loop continues."
	  },
     EXAMPLE "for i from 0 when i < 10 list (if odd i then continue 4567; i^2)",
     PARA { "  If ", TT "break v", " is executed by ", TT "x", ", then the loop is stopped and ", TT "v", " is returned as its value." },
     EXAMPLE lines "for i from 0 when i < 10 list (if i== 5 then break i; i^2)",
     PARA { "If ", TO "break", " is executed by ", TT "x", ", then the loop is stopped and the list accumulated so far is returned as the value." },
     EXAMPLE "for i from 0 when i < 10 list (if i== 5 then break; i^2)",
     EXAMPLE "for i in 0..3 list i^2",
     PARA { "If ", TT "v", " is an instance of any class with the ",
	 TO iterator, " method installed (e.g., a string), then the values of ",
	  TT "i", " are obtained by repeatedly calling ", TO next,
	  " on the output of ", TT "iterator v", " until ", TO StopIteration,
	 " is returned."},
     EXAMPLE ///for i in "foo" do print i///
     }

document {
     Key => "local variables in a function",
     "A local variable in a function is one that is not visible to
     code in other locations.  Correct use of local variables is
     important, for data stored in global variables will stay around
     forever, needlessly occupying valuable memory space.  For recursive
     functions especially, it is important to use local variables so that
     one invocation of the function doesn't interfere with another.",
     PARA{},
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
     PARA{},
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
    Key => Symbol,
    Headline => "the class of all symbols",
    "Symbols are entered as an alphabetic character followed by a
    sequence of alphanumeric characters; case is significant.
    The single symbol character ' is regarded as alphabetic, so that
    symbols such as ", TT "x'", " may be used.",
    PARA{},
    "Symbols are used as names for values to be preserved, as indeterminates
    in polynomial rings, and as keys in hash tables.  They may have
    global scope, meaning they are visible from every line of code,
    or local scope, with visibility restricted to a single file or
    function body.",
    EXAMPLE {
	"x",
	"ab12"
    },
    SeeAlso => {
	":=",
	"<-",
	"threadLocal",
    },
    Subnodes => {
	TO Keyword,
	TO SymbolBody,
	TO "symbol",
	TO "global",
	TO "local",
	TO getSymbol,
	TO getGlobalSymbol,
	TO isGlobalSymbol,
	TO erase,
	TO protect,
	TO (value, Symbol),
	TO (findSynonyms, Symbol),
	TO "globalAssign",
	TO "globalAssignFunction",
	TO "GlobalAssignHook",
	TO "globalAssignment",
	TO "globalAssignmentHooks",
	TO "globalReleaseFunction",
	TO "GlobalReleaseHook",
	TO ((symbol _, symbol =), Symbol, Thing),
	TO (symbol .., Symbol, Symbol),
	TO (symbol ..<, Symbol, Symbol),
	-- TO (symbol _, Symbol, Ring),
	TO (symbol _, Symbol, Thing),
    },
}

document {
    Key => Keyword,
    Headline => "the class of all keywords",
    PARA {
	"Keywords are symbols that are treated specially by the system while parsing user input.  Some of them,
	such as ", TO "and", ", consist of alphanumeric characters and look just like
	ordinary symbols.  Others, such as ", TO "==>", ", consist of special characters
	and are called operators."
    },
    SeeAlso => {"precedence of operators"}
}

binaryOperators := core "binaryOperators"
prefixOperators := core "prefixOperators"
postfixOperators := core "postfixOperators"
flexibleBinaryOperators := core "flexibleBinaryOperators"
flexiblePrefixOperators := core "flexiblePrefixOperators"
flexiblePostfixOperators := core "flexiblePostfixOperators"
augmentedAssignmentOperators := core "augmentedAssignmentOperators"
allOperators := core "allOperators"
getParsing := core "getParsing"

document {
     Key => "operators",
     PARA {
	  "Here we present all of the unary and binary operators in the language.  They are members of the class ", TO "Keyword", ".
	  The binary operators are ",
	  between_" " apply(sort toList binaryOperators, s -> TO {s}),
	  " , the unary prefix operators are ",
	  between_" " apply(sort toList prefixOperators, s -> TO {s}),
	  " , and the unary postfix operators are ",
	  between_" " apply(sort toList postfixOperators, s -> TO {s}),
	  " ."
	  },
     PARA {
	  "Of those, the ones for which users may install new methods are ",
	  "the binary operators ",
	  between_" " apply(sort toList join(flexibleBinaryOperators,
		  augmentedAssignmentOperators), s -> TO {s}),
	  " , the unary prefix operators ",
	  between_" " apply(sort toList flexiblePrefixOperators, s -> TO {s}),
	  " , and the unary postfix operators ",
	  between_" " apply(sort toList flexiblePostfixOperators, s -> TO {s}),
	  " ."},
     Subnodes => splice {
     "assignment",
	  TO symbol = ,
	  TO symbol := ,
	  TO symbol <- ,
     "equality testing",
	  TO symbol == ,
	  TO symbol != ,
	  TO symbol === ,
	  TO symbol =!= ,
     "comparison of objects",
	  TO symbol < ,
	  TO symbol <= ,
	  TO symbol > ,
	  TO symbol >= ,
	  TO symbol ? ,
     "predicates",
	  TO symbol and ,
	  TO symbol or ,
	  TO symbol xor ,
	  TO symbol not ,
     "functions",
          TO symbol SPACE,
	  TO symbol -> ,
          TO symbol @@ ,
          TO symbol \\ ,
	  TO symbol \ ,
     "subscripting and object access",
          TO symbol _ ,
          TO symbol . ,
          TO symbol .? ,
          TO symbol # ,
          TO symbol #? ,
     "arithmetic operators",
          TO symbol ! ,
	  TO symbol + ,
	  TO symbol - ,
	  TO symbol * ,
	  TO symbol / ,
	  TO symbol // ,
	  TO symbol % ,
	  TO symbol ^ ,
          TO symbol & ,
	  TO symbol ++ ,
	  TO symbol ** ,
          TO symbol ^** ,
          TO symbol ~ ,
	  TO symbol (*) ,
          TO symbol : ,
     "miscellaneous operators",
          TO symbol .. ,
          TO symbol ..< ,
          TO symbol << ,
          TO symbol >> ,
          TO symbol | ,
          TO symbol || ,
          TO symbol @ ,
          TO symbol ^^ ,
          TO symbol ?? ,
          TO symbol |- ,
          TO symbol <==> ,
          TO symbol ===> ,
          TO symbol ==> ,
          TO symbol <=== ,
          TO symbol <== ,
          TO symbol ^<,
          TO symbol ^<=,
          TO symbol ^>,
          TO symbol ^>=,
          TO symbol _<,
          TO symbol _<=,
          TO symbol _>,
          TO symbol _>=,
          TO symbol _~,
     "operators used for functors",
          TO symbol ^~,
          TO symbol ^!,
          TO symbol ^*,
          TO symbol _!,
          TO symbol _*,
          TO symbol |_,
     "built in operators" ,
	  TO symbol => ,
          TO symbol , ,
          TO symbol ;,
     "augmented assignment",
	 toSequence apply(sort toList augmentedAssignmentOperators,
	     op -> TO op),
     "further information",
          TO "precedence of operators",
	  TO "operatorAttributes"
	  }
     }

document {
     Key => "precedence of operators",
     SeeAlso => { "parsing precedence, in detail" },
     PARA {
	  "One aspect of parsing precedence is associativity.  A ", EM "left-associative", " operator is one, such as ", TO "*", ", with
	  the property that ", TT "x * y * z", " is parsed as ", TT "(x * y) * z", ".
	  A ", EM "right-associative", " operator is one, such as ", TO "=", ", with the property that ", TT "x = y = z", " is parsed
	  as ", TT "x = (y = z)", ".  These operators are left associative: ",
	  between_" " apply(sort select(allOperators, s -> (getParsing s)#0 == (getParsing s)#1), s -> TO {s}),
	  ", and these operators are right associative: ",
	  between_" " apply(sort select(allOperators, s -> (getParsing s)#0 == (getParsing s)#1 + 1), s -> TO {s}),
	  " ."
	  },
     PARA {
	  "Here are the operators arranged in order of increasing parsing precedence.  For example,
	  ", TT "*", " has higher parsing precedence than ", TT "+", ", and hence ", TT "2+3*5", " is
	  parsed as though it had been written as ", TT "2+(3*5)", ".  The symbol ", TO "SPACE", " represents the operator that is used when
	  two things are adjacent in program code."
	  },
     TABLE { "class" => "examples",  TR TD PRE net (core "seeOperatorPrecedence")() }
     }

sp := seeParsing()

document {
     Key => seeParsing,
     Usage => "seeParsing()",
     Headline => "display parsing precedence table for Macaulay2 operators",
     PARA {
	  "We intend to eliminate this function in favor of ", TO "operatorAttributes", "."
	  },
     SeeAlso => {"parsing precedence, in detail"}
     }

document {
     Key => "parsing precedence, in detail",
     "A newline ends a statement if it can, otherwise it acts like any
     white space.",
     EXAMPLE "2+\n3+\n4",
     PARA{},
     "Parsing is determined by a triple of numbers attached to each token.
     The following table (produced by the command ", TO "seeParsing", "), 
     displays each of these numbers.",
     TABLE { "class" => "examples", TR TD PRE { toString net sp} },
     "Here is the way these numbers work.  The parser maintains a number
     which called the current parsing level, or simply, the level.
     The parser builds up an expression until it encounters an input token
     whose parsing precedence is less than or equal to the current level.
     The tokens preceding the offending token are bundled into an expression
     appropriately and incorporated into the containing expression.",
     PARA{},
     "When an operator or token is encountered, its binding strength serves
     as the level for parsing the subsequent expression, unless the current level
     is higher, in which case it is used.",
     PARA{},
     "Consider a binary operator such as ", TT "*", ".  The relationship between
     its binary binding strength and its parsing precedence turns out to determine
     whether ", TT "a*b*c", " is parsed as ", TT "(a*b)*c", " or as ", TT "a*(b*c)", ".
     When the parser encounters the second ", TT "*", ", the current parsing level 
     is equal to the binding strength of the first ", TT "*", ".  If the binding 
     strength is less than the precedence, then the second ", TT "*", " becomes
     part of the right hand operand of the first ", TT "*", ", and the
     expression is parsed as ", TT "a*(b*c)", ".  Otherwise, the expression is
     parsed as ", TT "(a*b)*c", ".",
     PARA{},
     "For unary operators, the unary binding strength is used instead of the binary
     binding strength to reset the current level.  The reason for having both numbers 
     is that some operators can be either unary or binary, depending on the context.
     A good example is ", TO "#", " which binds as tightly as ", TO ".", "
     when used as an infix operator, and binds as loosely as adjacency or
     function application when used as a prefix operator.",
     PARA{},
     "To handle expressions like ", TT "b c d", ", where there are no tokens present
     which can serve as a binary multiplication operator, after parsing ", TT "b", ",
     the level will be set to 1 less than the precedence of an identifier,
     so that ", TT "b c d", " will be parsed as ", TT "b (c d)", ".",
     PARA{},
     "The comma and semicolon get special treatment: the empty expression can
     occur to the right of the comma or semicolon or to the left of the comma.",
     PARA {
	  "One of the most unusual aspects of the parsing precedence table above is that ", TT "[", " is
	  assigned a precedence several steps lower than the precedence of symbols and adjacency, and also
	  lower than the precedence of ", TT "/", ".  This was done so expressions like ", TT "R/I[x]", "
	  would be parsed according to mathematical custom, but it implies that expressions like ", TT "f g [x]", "
	  will be parsed in a surprising way, with ", TT "f g", " being evaluated first, even if ", TT "f", " and ", TT "g", "
	  are both functions.  Suitably placed parentheses can help, as illustrated in the next example."
	  },
     EXAMPLE lines ///
     	  f = x -> (print x; print)
	  f f [1,2,3]
	  f f ([1,2,3])
	  f (f [1,2,3])
     ///,
     Subnodes => TO seeParsing
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
