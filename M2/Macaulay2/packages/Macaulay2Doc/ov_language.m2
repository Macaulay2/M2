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
	  "There is special syntax for creating and accessing strings, lists, sequences, and hashtables.  These are the key data types underlying many new
	  types.  The Macaulay2 engine implements rings, ring elements, and matrices, as instances of low-level types, and various high-level types,
	  visible to the user, are based on them.  Examples include ", TO "Ring", ", ", TO "RingElement", ", ", TO "Matrix", ", ", TO "Ideal", ", ", 
	  TO "RingMap", ", ", TO "Module", ", and ", TO "ChainComplex", "."
	  },

     PARA{
	  "The user can create new types of higher level mathematical objects, whose types form a hierarchy that facilitates the installation and use of
	  appropriate methods for computing with them."
	  },

     Subnodes => {
	      TO "variables",
	  "basic data types",
	      TO "numeric types",
	      TO "strings and nets",
	      TO "lists and sequences",
	      TO "hash tables",
     	  "expressions",
	      TO "operators",
	      TO "conditional execution",
	      TO "while",
	      TO "for",
	      TO "mapping over hash tables",
	      TO "error handling",
	      TO "catch",
	  "functions",
	      TO "using functions",
	      TO "using functions with optional inputs",
	      TO "making functions",
	      TO "local variables in a function",
	      TO "making functions with a variable number of arguments",
	      TO "making functions with multiple return values",
	      TO "making new functions with optional arguments",
	      TO "using hooks",
	  "classes and types",
	      TO "what a class is",
	      TO "installing methods",
	      TO "binary methods",
	      TO "inheritance",
	      TO "making new classes",
	      TO "new",
	      TO "printing and formatting for new classes",
	      TO "making a new method function",
	  "input and output",
	      TO "printing to the screen",
	      TO "reading files",
	      TO "getting input from the user",
	      TO "creating and writing files",
	      TO "saving polynomials and matrices in files",
	      TO "two dimensional formatting",
	      TO "file manipulation",
	      TO "communicating with programs",
	      TO "using sockets",
     	  "packages",
	      TO "packages",
	      TO "creating a package",
	      TO "writing documentation",
	  "parallel programming ",
	      TO "parallel programming with threads and tasks",
     	  "system facilities",
	      TO "system facilities",
	  "debugging",
	      TO "debugging",
	  }
     }
------------------------------------------------------------------
document {
     Key => "saving polynomials and matrices in files",
     "Use the function ", TO "toString", " to convert polynomials, matrices, and other mathematical
     objects to a linear format that can be saved in a file.",
     EXAMPLE lines ///
          R = QQ[x..z]
	  p = (x-y-1)^3
	  m = matrix {{x^2, x^2-y^2, x*y*z^7 }}
	  M = image m
	  f = temporaryFileName()
	  f << toString (p,m,M) << close
	  get f
	  (p',m',M') = value get f
	  p == p'
	  m == m'
	  M == M'
	  removeFile f
     ///
     }
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
     }

document { Key => "using hooks", 
     PARA {"Hooks are a way to provide different implementations of functions and methods.
     The user can attach multiple hooks to a function via ", TO "addHook", "."},
     
     PARA {"Hooks can be functions or methods, and they can accept optional arguments." },
     EXAMPLE lines ///
     f = {a=>3, c=>12} >> opts -> val -> if val == 1 then opts.a + opts.c
     g = method(Options => {b => 5})
     g ZZ := opts -> val -> if val == 2 then opts.b + 1
     h = val -> if val == 3 then 24
     addHook(ZZ, symbol foo, f)
     addHook(ZZ, symbol foo, g)
     addHook(ZZ, symbol foo, h)
     ///,
     PARA {"The command ", TO "hooks", " lists all hooks attached to a symbol."},
     EXAMPLE {"hooks(ZZ, symbol foo)"},
     PARA {"The method ", TO "runHooks", " runs all the hooks until one of them returns a non-null value.
     Hooks are run in order starting from the most recently added hook. Because of this, 
     hooks should be able to decide quickly if they should be used or not."},
     PARA {"Any optional argument passed to ", TT "runHooks", " that matches a key in the ", TO "OptionTable", 
     " of a hook will be passed on to it. Otherwise it will be ignored." },
     EXAMPLE lines ///
     foo = true >> opts -> args -> runHooks(ZZ, symbol foo, args, opts)
     assert( foo 1 == 15 )
     assert( foo(2, b => 9) == 10 )
     assert( foo 3 == 24 )
     ///,
     SeeAlso => {addHook, removeHook, runHooks, hooks}
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
     load "Macaulay2Doc/demo1.m2"
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

document { Key => "in",
     Headline => "a keyword used in for-loops",
     SeeAlso => {"for"}
     }

document { Key => "for",
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
	  Inputs => { "v" => BasicList },
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
     EXAMPLE "for i in 0..3 list i^2"
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
     Key => "hash tables",
     "A hash table is a data structure that can implement a function
     whose domain is a finite set.  An element of the domain is called
     a key.  The hash table stores the key-value pairs in such a way
     that when presented with a key, the corresponding value can be
     quickly recovered.",
     PARA{},
     "A dictionary could be implemented as a hash table: the keys would
     be the words in the language, and the values could be the definitions
     of the words.",
     PARA{},
     "A phone book could also be implemented as a hash table: the keys would
     be the names of the subscribers, and the values could be the corresponding
     phone numbers.  (We exclude the possibility of two subscribers with
     the same name.)",
     PARA{},
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
	  "x#?4"
	  },
     Subnodes => {
	  "functions for manipulating hash tables",
	  TO "copy",
	  TO "hashTable",
	  TO "keys",
	  TO "values",
	  TO "pairs",
	  TO "mutable",
	  TO "remove",
	  TO "applyKeys",
	  TO "applyValues",
	  TO "applyPairs",
	  TO "merge",
	  TO "combine",
	  "more information",
	  TO "hashing",
	  TO "HashTable",
	  TO "MutableHashTable"
	  }
     }

debug Core

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
	  between_" " apply(sort toList flexibleBinaryOperators, s -> TO {s}),
	  " , the unary prefix operators ",
	  between_" " apply(sort toList flexiblePrefixOperators, s -> TO {s}),
	  " , and the unary postfix operators ",
	  between_" " apply(sort toList flexiblePostfixOperators, s -> TO {s}),
	  " ."},
     Subnodes => {
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
          TO symbol |- ,
          TO symbol <==> ,
          TO symbol ===> ,
          TO symbol ==> ,
          TO symbol <=== ,
          TO symbol <== ,
     "built in operators" ,
	  TO symbol => ,
          TO symbol , ,
          TO symbol ;,
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
     TABLE { "class" => "examples",  TR TD PRE net (value Core#"private dictionary"#"seeOperatorPrecedence")() }
     }

sp := seeParsing()

dictionaryPath = select(dictionaryPath, d -> d =!= Core#"private dictionary")

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
     ///
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
