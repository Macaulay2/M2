document {
     Key => "top level loop",
     "The top level evaluation loop of the interpreter contains hooks so the user can
     control how printing of the results of evaluation is done.  If the result is
     ", TO "null", " then nothing is printed.  Otherwise, the appropriate method
     associated with the symbol ", TO "Print", " is applied to perform the printing,
     unless the printing is to be suppressed, as indicated by a semicolon at the end
     of the statement, in which case the ", TO "NoPrint", " method is applied.",
     Subnodes => {TO "capture"},
     SeeAlso => {
	  "Print",
	  "NoPrint",
	  "BeforePrint",
	  "AfterEval",
	  "AfterPrint",
	  "AfterNoPrint"
	  }
     }

document {
     Key => {";"},
     Headline => "expression separator",
     Usage => "(e;f;...;g;h)",
     PARA {
	  "The semicolon can be used for evaluating a sequence of
	  expressions.  The value of the sequence is the value of its
	  last expression, unless it is omitted, in which case the value
	  is ", TO "null", "."
	  },
     EXAMPLE {
	  "(3;4;5)",
	  "(3;4;5;)"
	  },
     PARA {
	  "Putting expressions on separate lines is not a substitute for
	  the semicolons."
	  }
     }

document {
     Key => "--",
     Headline => "comment",
     Consequences => {"Macaulay2 ignores commented text"},
     "Use a double hyphen ", TT "--", " to introduce a comment in the text
     of a program.  The comment runs from to the end of the line.",
     PARA{},
     "Emacs does a good job displaying the comments in a different color
     for visibility.",
     EXAMPLE {
	  "x = 1 -- this is a comment",
	  }
     }

document {
     Key => {"operatorAttributes", Flexible, Binary, Prefix, Postfix},
     Headline => "a hashtable with information about Macaulay2 operators",
     Usage => "operatorAttributes",
     Outputs => {{ "an experimental hash table that give information about ", TO "operators", " in the Macaulay2 language" }},
     "Meanings of the symbols used:",
     UL {
	  LI { TO "Flexible", " -- user defined methods may be installed" },
	  LI { TO "Binary", " -- it's a binary operator" },
	  LI { TO "Prefix", " -- it's a prefix unary operator" },
	  LI { TO "Postfix", " -- it's a postfix unary operator" }
	  },
     "We intend to add parsing precedences to this table and eliminate ", TO "seeParsing", "."
     }

undocumented {(value, RingElement), (value, Nothing), (value, IndexedVariableTable)}

document {
     Key => value,
     Headline => "evaluate"
     }

document {
     Key => (value,Symbol),
     Headline => "retrieve the value of a symbol",
     Usage => "value s",
     Inputs => { "s" },
     Outputs => { {"the value of ", TT "s" } },
     EXAMPLE {
	  "x = s",
	  "s = 11111111111",
	  "x",
	  "value x"
	  }
     }

document {
     Key => (value, IndexedVariable),
     Headline => "retrieve the value of an indexed variable",
     Usage => "value s",
     Inputs => { "s" },
     Outputs => { {"the value of ", TT "s" } },
     EXAMPLE lines ///
	  y = x_3
	  x_3 = 4
	  x_3
	  y
	  value y
     ///
     }

document {
     Key => {(value,String),"currentString"},
     Headline => "evaluate a string",
     Usage => "value s",
     Inputs => { "s" },
     Outputs => { {"the value obtained by evaluating the code in ", TT "s" } },
     "The contents of ", TT "s", " are treated as code in the
     Macaulay2 language, parsed it in its own scope (the same way a file is)
     and evaluated.  The string may contain multiple lines.",
     {
	  EXAMPLE {
	       ///value "2 + 2"///,
	       ///value "a := 33
a+a"///,
	       ///a///
	       },
	  "Since the local assignment to ", TT "a", " above occurred in a new scope,
	  the value of the global variable ", TT "a", " is unaffected."
	  },
     PARA{
	  "During evaluation of the string, error messages will refer to the location of the error
	  as ", TT "currentString", " with a line number and a column number,
	  and the value of the variable ", TO "currentString", " is set to the string, to aid in debugging."
	  },
     EXAMPLE lines ///
     debuggingMode = stopIfError = false;
     value "1/0"
     debuggingMode = true;
     value "1/0"
     break
     ///,
     SeeAlso => {"debugging", "stopIfError", "debuggingMode"}
     }

document {
     Key => "global",
     Headline => "get a global symbol",
     TT "global s", " provides the global symbol s, even if s currently has
     a value.",
     EXAMPLE {
	  "num",
	  "num = 5",
	  "num",
	  "global num",
	  },
     SeeAlso => {"local", "symbol" }
     }

document {
     Key => erase,
     Headline => "remove a global symbol",
     TT "erase s", " removes the global symbol ", TT "s", " from the
     symbol table."
     }

document {
     Key => "local",
     Headline => "get a local symbol",
     TT "local s", " provides the local symbol ", TT "s", ", creating
     a new symbol if necessary.  The initial value of a local
     symbol is ", TO "null", ".",
     EXAMPLE {
	  "f = () -> ZZ[local t]",
	  "f()",
	  "t",
	  },
     SeeAlso => {"global", "symbol"
     }
     }

document {
     Key => "symbol",
     Headline => "get a symbol",
     TT "symbol s", " provides the symbol ", TT "s", ", even if ", TT "s", " currently has a value.",
     PARA{},
     {EXAMPLE {
	       "num",
	       "num = 5",
	       "num",
	       "symbol num",
	       },
	  PARA{},
	  "If ", TT "s", " is an operator, then the corresponding symbol is provided.  This
	  symbol is used by the interpreter in constructing keys for methods
	  associated to the symbol.",
	  EXAMPLE "symbol +"
	  },
     SeeAlso => {"local", "global", "value" }
     }

document {
     Key => protect,
     Headline => "protect a symbol",
     Usage => "protect s",
     Inputs => { "s" => Symbol },
     Outputs => { "s" => Symbol },
     Consequences => {
	  { "the symbol ", TT "s", " is protected from having its value changed" }
	  },
     PARA {
	  "There is no function for unprotecting symbols.",
	  },
     EXAMPLE lines ///
     mutable symbol s
     s = 5
     s
     protect symbol s
     mutable symbol s
     try s = 7
     s
     ///,
     SeeAlso => {mutable, "try"}
     }
document { Key => {frames,(frames, Symbol), (frames, Sequence), (frames, Pseudocode), (frames, Function)},
     Headline => "get the frames associated to a closure",
     Usage => "frames f",
     Inputs => { "f" => {"() or ", ofClass{Symbol,Function,Pseudocode}}},
     Outputs => {{"a list of mutable lists, the frames attached to the closure ", TT "f", " or, if ", TT "f", " is ", TT "()", ", then
	       the frames attached to the current lexical scope"
	       }},
     "This function is occasionally useful as a debugging tool.",
     EXAMPLE lines ///
	  f = (x,y,z) -> t -> t
	  g = f(111,222,"hi there")
	  frames g
	  peek first oo
     ///}


document { Key => "globalAssignmentHooks",
     Headline => "assignment hooks for global symbols",
     Usage => "globalAssignmentHooks#s = f",
     Inputs => {
	  "s" => Symbol => "a global symbol",
	  "f" => Function => {"a function of two arguments, ", TT "f = (sym,val) -> ...", "; the argument ", TT "sym", " is the symbol whose
	       value is about to be assigned to, and ", TT "val", " is the value about be assigned to it"
	       }
	  },
     Consequences => {
	  {"whenever an assignment statement of the form ", TT "s=e", " is done, the expression ", TT "f(s,e)", " is first evaluated"}
	  },
     SeeAlso => {globalAssignFunction}
     }

document {
     Key => globalAssignFunction,
     Headline => "the standard method for the global assignment hook",
     TT "globalAssignFunction", " -- the standard function that can be used
     as a method for ", TO GlobalAssignHook, " so that certain types of
     things, when assigned to a global variable, will acquire
     the name of the global variable as their name.  The companion function
     ", TO "globalReleaseFunction", " is used to release the name when the
     global variable gets reassigned.",
     PARA{},
     "Another thing done by this function is to apply ", TO use, " to the thing.
     This is used for polynomial rings to assign values to the symbols representing
     the variables (indeterminates) in the ring.",
     PARA{},
     EXAMPLE {
	  "X = new Type of MutableHashTable",
	  "x = new X",
	  "X.GlobalAssignHook = globalAssignFunction",
	  "X.GlobalReleaseHook = globalReleaseFunction",
	  "x' = new X",
	  "t = {x,x'}",
	  "x = x' = 44",
	  "t",
	  "code globalAssignFunction",
	  },
     SeeAlso => { "symbol", "SelfInitializingType" }
     }

document {
     Key => globalReleaseFunction,
     Headline => "the standard method for the global variable release hook",
     TT "globalReleaseFunction", " -- the standard function that can be used as
     a method for ", TO GlobalReleaseHook, " so that certain types of things, which
     have acquired as their name the name of a global variable to which they have
     been assigned, will lose that name when a different value is assigned to
     the variable.",
     PARA{},
     SeeAlso => "globalAssignFunction"
     }

document { Key => globalAssignment,
     Headline => "install standard global assignment method",
     Usage => "globalAssignment X",
     Inputs => { "X" => Type },
     Consequences => {{"the functions ", TO "globalAssignFunction", " and ", TO "globalReleaseFunction", " are installed in the type ", TT "X", "
	       under ", TO "GlobalAssignHook", " and ", TO "GlobalReleaseHook", ", respectively.  The effect is that when an object of type ", TT "X", " is
	       assigned to a global variable, the function ", TO "use", " is called on it, and thereafter that object will print out as the name of the variable."
	       }},
     "One type for which this has been done is ", TO "Ring", ", as illustrated in the following example.",
     EXAMPLE lines ///
	  S := QQ[x]
	  S
	  S^3
	  R = S
	  S
	  S^3
     ///}

document {
     Key => GlobalAssignHook,
     Headline => "hook for assignment to global variables",
     Usage => "X.GlobalAssignHook = f",
     Inputs => {
	  "X" => Type,
	  "f" => Function => " of two variables: the symbol to which a value is about to be assigned, and the value being assigned",
	  },
     Consequences => {
	  {TT "f", " will be called just before an assignment to a global variable of an instance of class ", TT "X", " occurs"}
	  },
     PARA {
	  "This technique is used, for example, for instances of ", TO "Type", " and ", TO "Ring", "
	  to arrange for the name of the type or ring to be set to the name
	  of the global variable to which it is first assigned.  The functions
	  ", TO "globalAssignFunction", " and ", TO "globalReleaseFunction", " may installed
	  as methods for this purpose."},
     EXAMPLE {
	  ///RR.GlobalAssignHook = (sym,val) -> << "--about to assign " << val << " to " << sym << endl;///,
	  "a = 4.5",
	  },
     SeeAlso => {"GlobalReleaseHook"}
     }
document {
     Key => GlobalReleaseHook,
     TT "GlobalReleaseHook", " -- a method name that is consulted when an
     assignment to a global variable is about to occur.",
     PARA{},
     "The method should be a function of two variables: the symbol to which
     a value is being assigned, and the old value about to be overwritten.
     The method should be stored under the name ", TT "GlobalReleaseHook", " in the
     class of the old value.  It is executed before the assignment occurs,
     and before the execution of ", TO "GlobalAssignHook", ".",
     PARA{},
     EXAMPLE {
	  ///RR.GlobalReleaseHook = (sym,val) -> << concatenate (
     "assigning ", toString val, " to ", toString sym
     ) << endl///,
	  "a=4.5",
	  "a=5.4",
	  },
     SeeAlso => "GlobalAssignHook"
     }

document {
     Key => Print,
     Headline => "top level method for printing results",
     Usage => "X#{Standard,Print} = f",
     Inputs => {
	  "X" => Type,
	  "f" => Function => { " that can print something of type ", TT "X"}
	  },
     Consequences => {
	  { "at top level, whenever it is time to print an output value of type ", TT "X", ", the function ", TT "f", " will be called" }
	  },
     "The function ", TT "f", " is responsible for printing the output prompt and for applying the ", TO "BeforePrint", " and ", TO "AfterPrint", " methods, if desired.",
     EXAMPLE "code Thing#{Standard,Print}"
     }
document {
     Key => NoPrint,
     Headline => "top level method for non-printing results",
     Usage => "X#{Standard,NoPrint} = f",
     Inputs => {
	  "X" => Type,
	  "f" => Function => { " that can accept something of type ", TT "X"}
	  },
     Consequences => {
	  {
	       "At top level, whenever it is time, as indicated by a semicolon at the end of an input line,
	       to suppress printing of an output value of type ", TT "X", ", the function ", TT "f", " will be called." }
	  }
     }
document {
     Key => BeforePrint,
     Headline => "top level method applied before printing results",
     Usage => "X#{Standard,BeforePrint} = f",
     Inputs => {
	  "f" => { "a function to be applied before printing a top-level evaluation result ", TT "r", " of type ", TT "X", "." },
	  },
     Consequences => {
	  {"The value returned by ", TT "f", " is printed instead."}
	  }
     }
document {
     Key => AfterEval,
     Headline => "top level method applied after evaluation",
     Usage => "X#AfterEval = f",
     Inputs => {
	  "f" => { "a function to be applied after evaluating a top-level evaluation result ", TT "r", " of type ", TT "X", "."},
	  },
     Consequences => {
	  "The value returned result replaces the original for storing in the output variables and for printing"
	  }
     }
document {
     Key => AfterPrint,
     Headline => "top level method applied after printing",
     Usage => "X#{Standard,AfterPrint} = f",
     Inputs => {
	  "f" => { "a function to be applied after printing a top-level evaluation result ", TT "r", " of type ", TT "X", "."}
	  },
     Outputs => {
	  {"The value returned by ", TT "f", " is discarded."}
	  },
     "This method is used to print the type of the result of a computation.",
     EXAMPLE {
	  "3/4"
	  },
     "We could suppress that output for a single type as follows.",
     EXAMPLE {
	  "QQ#{Standard,AfterPrint} = r -> r;",
	  "3/4"
	  }
     }
document {
     Key => AfterNoPrint,
     Headline => "top level method applied after not printing",
     Usage => "X#{Standard,AfterNoPrint} = f",
     Inputs => {
	  "f" => { "a function to be applied after not printing a top-level evaluation result ", TT "r", " of type ", TT "X", "." }
	  },
     Consequences => {
	  {
	       "The function ", TT "f", " will be applied at top level to the
	       result of an evaluation when printing of the result has
	       been suppressed by a semicolon."
	       }
	  }
     }


document {
     Key => "topLevelMode",
     Headline => "the current top level mode",
     Usage => "topLevelMode = x",
     Inputs => {
	  "x" => Symbol => {TO "TeXmacs", ", or ", TO "Standard"}
	  },
     Consequences => {
	  {"the interpreter will produce input and output prompts appropriate for the mode, and will
	       format output values appropriately"}
	  },
     PARA "This variable is intended for internal use only."
     }
document {
     Key => Standard,
     Headline => "the standard top level printing method",
     "This symbol is used (tentatively) as the first element of a pair to specify various top level interpreter methods.",
     SeeAlso => { TeXmacs, Print, NoPrint, BeforePrint, AfterPrint,AfterNoPrint}
     }
document {
     Key => TeXmacs,
     Headline => "the TeXmacs top level printing method",
     "This symbol is used (tentatively) as the first element of a pair to specify various top level interpreter methods, in connection with
     the use of TeXmacs as front end.",
     SeeAlso => { Standard, Print, NoPrint, BeforePrint, AfterPrint,AfterNoPrint}
     }
document {
     Key => WebApp,
     Headline => "the web app top level printing method",
     "This symbol is used (tentatively) as the first element of a pair to specify various top level interpreter methods, in connection with
     the use of the (currently developed) web app with (Ka)TeX output as front end.",
     SeeAlso => { Standard, Print, NoPrint, BeforePrint, AfterPrint,AfterNoPrint}
     }
document {
    Key => Jupyter,
    Headline => "top level printing method used in the Jupyter kernel",
    "The mode allows for a more semantic output, with input, STDOUT, value and class clearly delimited.",
    SeeAlso => {Standard, TeXmacs, WebApp}
    }

document {
     Key => "shield",
     Headline => "shield evaluation from interrupts",
     TT "shield x", " -- executes the expression ", TT "x", ", temporarily
     ignoring interrupts."
     }

document {
     Key => symbol lineNumber,
     Headline => "current line number",
     TT "lineNumber", " -- the current line number.",
     BR{},
     TT "lineNumber = n", " -- sets the line number to ", TT "n", ".",
     }

document { Key => GlobalDictionary,
     Headline => "the class of all global dictionaries",
     SeeAlso => { "dictionaryPath", LocalDictionary }
     }
document { Key => LocalDictionary,
     Headline => "the class of all local dictionaries",
     "A local dictionary is one used in connection with a local scope, such as one that is bounded by the body of a function closure.
     A local dictionary is created on the fly by the interpreter, and after the scope has been closed, the dictionary can be enlarged no further.
     Accessing local dictionaries can be a useful debugging tool.  The local dictionaries accessible to the user come with frames, so their symbols
     have values; thus they may be referred to as dictionary closures.",
     SeeAlso => { localDictionaries, GlobalDictionary }
     }
document { Key => {localDictionaries,(localDictionaries, Symbol), (localDictionaries, Pseudocode), (localDictionaries, Dictionary), (localDictionaries, Function)},
     Headline => "get local dictionaries",
     Usage => "localDictionaries f",
     Inputs => {
	  "f" => {"() or ", ofClass{Function,Symbol,Pseudocode,Dictionary}}
	  },
     Outputs => {
	  List => {"a list of the local dictionaries associated with the lexical scopes containing ", TT "f"}
	  },
     EXAMPLE lines ///
	  f := x -> y -> z -> 11;
	  d := localDictionaries ((f 22) 33)
	  peek d
	  d#0#"y"
	  value d#0#"y"
	  peek localDictionaries()
     ///
     }

document { Key => {listSymbols,(listSymbols, Dictionary), (listSymbols, List)},
     Headline => "compact display of symbols and their values",
     Usage => "listSymbols v",
     Inputs => { "v" => {ofClass{List,Dictionary}, "; if it's a list, it should be a list of symbols"}},
     Outputs => { Net => {"a compact display of the symbols in ", TT "v", " and their values"}},
     EXAMPLE lines ///
	  x:=3; y:="hi there"; z:=2^30; f = x->x;
	  listSymbols { symbol x, symbol y }
	  listSymbols first localDictionaries()
     ///}

document { Key => listLocalSymbols,
     Headline => "display of local symbols and their values",
     SYNOPSIS (
	  Usage => "listLocalSymbols f",
	  Inputs => { "f" => {ofClass{Pseudocode,Symbol,Dictionary,Function}}},
	  Outputs => { Net => {"a compact display of the symbols in the local dictionaries attached to the closure ", TT "f", ", and their values"}},
	  EXAMPLE lines ///
	       x:=3; y:="hi there"; z:=2^30; f = x->x;
	       listLocalSymbols f
	       listLocalSymbols symbol x
	  ///
	  ),
     SYNOPSIS (
	  Usage => "listLocalSymbols",
	  Outputs => { Net => {"a compact display of the symbols in the local dictionaries attached to ", TO "current"}},
	  PARA {
	       "This usage works only in the debugger, where ", TO "current", " has a non-null value."
	       },
	  EXAMPLE lines ///
	  load "Macaulay2Doc/demo1.m2"
	  g 2
	  listLocalSymbols
	  ///,
	  ),
     SYNOPSIS (
	  Usage => "listLocalSymbols(X,f)",
	  Inputs => { "X" => Type, "f" => {ofClass{Pseudocode,Symbol,Dictionary,Function}}},
	  Outputs => { Net => {"a compact display of the symbols in the local dictionaries attached to the closure ", TT "f", ", and their values, provided their
		    values are instances of the type ", TT "X"}},
	  EXAMPLE lines ///
	  h := x -> y -> y+1;
	  listLocalSymbols(ZZ,h 11)
	  ///
	  ),
     SYNOPSIS (
	  Usage => "listLocalSymbols X",
	  Outputs => { Net => {"a compact display of the symbols in the local dictionaries attached to ", TO "current", " whose values
		    have type ", TT "X", "."}},
	  PARA {
	       "This usage works only in the debugger, where ", TO "current", " has a non-null value."
	       },
	  EXAMPLE lines ///
	  load "Macaulay2Doc/demo1.m2"
	  g 2
	  listLocalSymbols ZZ
	  ///
	  )
     }

document {
     Key => lookupCount,
     Headline => "reference count for a symbol",
     TT "lookupCount s", " -- the number of times the symbol ", TT "s", " has been
     encountered in source code presented to the interpreter."
     }

document {
     Key => symbol oo,
     Headline => "the last output value",
     TT "oo", " -- denotes the value of the expression on the previous output
     line.",
     SeeAlso => { "oo", "ooo", "oooo" }
     }

document {
     Key => symbol ooo,
     Headline => "the next to the last output value",
     TT "ooo", " -- denotes the value of the expression on the output line
     two lines above.",
     SeeAlso => { "oo", "oooo" }
     }

document {
     Key => symbol oooo,
     Headline => "the third to the last output value",
     TT "oooo", " -- denotes the value of the expression on the output line
     three lines above.",
     SeeAlso => { "oo", "ooo" }
     }

-- document {
--      Key => precedence,
--      Headline => "parsing precedence",
--      TT "precedence x", " -- returns the parsing precedence of ", TT "x", " for use in
--      the printing routines.",
--      PARA{},
--      SeeAlso => {"Expression", "net", "toString"}
--      }

document {
     Key => userSymbols,
     Headline => "a list of the user's symbols",
	Usage => "userSymbols ()",
	Outputs => {List => {" a list of symbols"}},
     TT "userSymbols ()", " provides a list of symbols defined by
     the user.",
     BR{},
     TT "userSymbols X", " limits the list to those symbols whose
     values are instances of the ", TO "class", " ", TT "X", ".",
     PARA{},
     "Protected symbols are excluded from the list.",
     SeeAlso => "listUserSymbols"
     }

document {
     Key => listUserSymbols,
     Headline => "display the user's symbols",
     SYNOPSIS {
	  Usage => "listUserSymbols",
	  Outputs => {
	       {"a display of the symbols defined and given values by the user, along with their types and values, in abbreviated form"}
	       },
	  PARA {
	       "A symbol is considered to have been give a value, if it's current value is not equal to itself."
	       },
	  EXAMPLE lines ///
	  t=3;
	  R=QQ[x];
	  listUserSymbols
	  ///
	  },
     SYNOPSIS {
	  Usage => "listUserSymbols X",
	  Inputs => {
	       "X" => Type
	       },
	  Outputs => {
	       {"a display of the symbols of type ", TT "X", " defined and given values by the user,
		    along with their types and values, in abbreviated form"}
	       },
	  EXAMPLE lines ///
	  listUserSymbols ZZ
	  ///
	  },
     SeeAlso => {"userSymbols"}
     }


document {
     Key => clearOutput,
     Headline => "forget output values",
	Usage => "clearOutput",
     TT "clearOutput", " is a command that attempts to release memory by
     clearing the values retained by the output line symbols.",
     SeeAlso => { "clearAll" }
     }

document {
     Key => clearAll,
     Headline => "forget everything",
	Usage => "clearAll",
     TT "clearAll", " is a command that attempts to release memory by clearing
     the values retained by the output line symbols and all the user symbols.",
     SeeAlso => {"userSymbols", "clearOutput"}
     }

document {
     Key => {memoize,(memoize, Function),(memoize, Function, List), memoizeClear, memoizeValues},
     Headline => "record results of function evaluation for future use",
     TT "memoize f", " -- produces, from a function ", TT "f", ", a new function that
     behaves the same as ", TT "f", ", but remembers previous answers to be provided
     the next time the same arguments are presented.",
     PARA{},
     EXAMPLE lines ///
     fib = n -> if n <= 1 then 1 else fib(n-1) + fib(n-2)
     time fib 28
     fib = memoize fib
     time fib 28
     time fib 28
     ///,
     PARA{
	  "An optional second argument to memoize provides a list of initial values,
	  each of the form ", TT "x => v", ", where ", TT "v", " is the value to
	  be provided for the argument ", TT "x", "."
	  },
     PARA{
	  "The function ", TT "memoize", " operates by constructing
	  a ", TO "MutableHashTable", ", in which the arguments are used
	  as keys for accessing the return value of the function.  This mutable hash table
	  can be obtained using the function ", TT "memoizeValues", ", as follows."
	  },
     EXAMPLE "peek memoizeValues fib",
     PARA {
	  "That hash table can be replaced by an empty one with the function ", TT "memoizeClear", "."
	  },
     EXAMPLE lines ///
     memoizeClear fib
     peek memoizeValues fib
     ///,
     PARA{
	  "Warning: the new function created by ", TT "memoize", " will save
	  references to all arguments and values it encounters, and this will
	  often prevent those arguments and values from being garbage-collected
	  as soon as they might have been.  If the arguments are
	  implemented as mutable hash tables (modules, matrices and rings are
	  implemented this way) then a viable strategy is to stash computed
	  results in the arguments themselves.  See also ", TT "CacheTable", "."
	  },
     }

document {
     Key => {truncateOutput,(truncateOutput, ZZ),(truncateOutput, InfiniteNumber)},
     Usage => "truncateOutput w",
     Inputs => {"w" => ZZ },
     Consequences => {{
	  "The maximum output line width is set to ", TT "w", ", which should be an integer or ", TO "infinity", ".
	  This function works by assigning a value to ", TT "Thing#{Standard,BeforePrint}", ", which
	  may conflict with other ", TO "BeforePrint", " methods installed by the user, or those installed by the system that do line wrapping."
	  }}
     }


document {
     Key => "printWidth",
     Usage => "printWidth = n",
     Inputs => {
	  "n" => ZZ => "the width to use for wrapping printed output"
	  },
     Consequences => {
	  "The function ", TO "wrap", " will use ", TT "n", " as the window width when wrapping
	  certain types of output."
	  }
     }

document {
     Key => symbol dictionaryPath,
     "The value of ", TO "dictionaryPath", " is the list of global dictionaries whose symbols are visible.",
     EXAMPLE { "dictionaryPath" },
     SeeAlso => { Dictionary }
     }

document {
     Key => Dictionary,
     Headline => "the class of all dictionaries",
     "A dictionary is a special sort of hash table whose keys are the strings, and whose values are the
     corresponding symbols.",
     EXAMPLE {
	  "Core.Dictionary # \"sin\"",
	  "Core.Dictionary #? \"sin\""
	  }
     }

document {
     Key => {dictionary,(dictionary, Keyword),(dictionary, Symbol),(dictionary, Thing)},
     Headline => "determine the dictionary to which a symbol belongs",
     Usage => "dictionary x",
     Inputs => {
	  "x" => Thing
	  },
     Outputs => {
	  { "the dictionary to which the symbol ", TT "x", " belongs"}
	  },
     "If ", TT "x", " is the value of a symbol recorded in the internal table used to recover
     global symbol from the values assigned to them, then that symbol is used."
     }

document {
     Key => youngest,
     Headline => "the youngest member of a sequence",
     TT "youngest s", " -- return the youngest mutable hash table in the sequence
     ", TT "s", ", if any, else ", TO "null", "."}

document {
     Key => symbol compactMatrixForm,
     Headline => "global flag for compact printing",
	Usage => "compactMatrixForm = x",
	Consequences => {"changes the display of matrices"},
     TT "compactMatrixForm", " is a global flag that specifies whether to display
     matrices in compact form.",
     PARA{},
     "The default value is ", TT "true", ".  The compact form is the form used by
     ", ITALIC "Macaulay", ", in which the multiplication and exponentiation operators
     are suppressed from the notation.",
     EXAMPLE {
	  "R = ZZ[x,y];",
	  "f = random(R^{2},R^2)",
	  "compactMatrixForm = false;",
	  "f"
	  }
     }

document {
     Key => globalAssign,
     Headline => "global assignment, in function form",
     Usage => "globalAssign(s,v)",
     Inputs => {
	  "s" => Symbol => "the symbol whose value is to be set",
	  "v" => Thing => {"the value to be assigned to ", TT "s"},
	  },
     Outputs => {
	  Thing => "v"
	  },
     PARA {
	  "This function mimics what happens in the interpreter when an assignment to a
	  global variable occurs, and can be useful if the name of the symbol is not
	  known when the code is written.  If the value changes,
	  then ", TO "GlobalReleaseHook", " and ", TO "GlobalAssignHook", " are called appropriately."
	  },
     EXAMPLE lines ///
	  x = y
	  globalAssign(x,4)
	  x
	  y
     ///
     }

document {
     Key => getSymbol,
     Headline => "make a global user symbol from a string",
     Usage => "getSymbol s",
     Inputs => {
	  "s" => String
	  },
     Outputs => {
	  Symbol => {"a global symbol whose name is provided by the string ", TT "s", "
	       in the private dictionary for the package ", TO "User", "."
	       }
	  },
     EXAMPLE lines ///
	  x = "aaaa"
	  s = getSymbol x
	  dictionary s
	  s === getSymbol x
	  keys User#"private dictionary"
     ///,
     Caveat => {
	  "The old behavior, up to version 1.3.1, was to provide a previously existing global symbol, if
	  one exists and is visible in one of the dictionaries in ", TO "dictionaryPath", ", or, if not, to create a new
	  global symbol in the first mutable dictionary listed in ", TO "dictionaryPath", "."
	  },
     SeeAlso => { getGlobalSymbol }
     }

document { Key => symbol currentLineNumber,
     Headline => "current line number of the current input file",
     Usage => "currentLineNumber()",
     Outputs => { ZZ => "the current line number of the current input file" },
     EXAMPLE "currentLineNumber()",
     SeeAlso => "currentFileName" }
document { Key => symbol currentFileDirectory,
     Headline => "the directory containing the current input file",
     Usage => "currentFileDirectory",
     Outputs => { String => "the directory containing the current input file" },
     EXAMPLE "currentFileDirectory",
     SeeAlso => "currentFileName" }
document { Key => symbol currentFileName,
     Headline => "the current source file",
     Usage => "currentFileName",
     Outputs => { String => "the name of the current source file" },
     EXAMPLE "currentFileName",
     SeeAlso => "currentLineNumber" }

document { Key => functionBody,
     Headline => "get the body of a function",
     Usage => "functionBody f",
     Inputs => { "f" => Function },
     Outputs => { FunctionBody => { "the body of the function ", TT "f" }},
     PARA { "The body of ", TT "f", " is essentially just the source code of ", TT "f", ", with no frames providing bindings for
	  the local variables in scopes enclosing the scope of ", TT "f", ".  Function bodies cannot act as functions, but they can be tested for
	  equality (", TO "===", "), and they can be used as keys in hash tables."
	  },
     EXAMPLE lines ///
	  f = a -> b -> a+b+a*b
	  functionBody f 1
	  f 1 === f 2
	  functionBody f 1 === functionBody f 2
     ///,
     SeeAlso => FunctionBody }
document { Key => FunctionBody,
     Headline => "the class of function bodies",
     SeeAlso => functionBody }

document { Key => symbol OutputDictionary,
     Headline => "the dictionary for output values",
     "The symbols ", TT "o1", ", ", TT "o2", ", ", TT "o3", ", etc., are used to store the output values arising from interaction with the user,
     one line at a time.  The dictionary ", TT "OutputDictionary", " is the dictionary in which those symbols reside.",
     EXAMPLE lines ///
	  2+2
	  "asdf" | "qwer"
	  value \ values OutputDictionary
	  dictionaryPath
	  peek OutputDictionary
     ///,
     SeeAlso => { "dictionaryPath" }
     }
document { Key => Pseudocode,
     Headline => "the class of pseudocodes",
     "The Macaulay2 interpreter compiles its language into pseudocode, which is evaluated later, step by step.  At each
     step, the evaluator is considering a pseudocode item.  These pseudocode items are normally not available to the user, but
     the internal function ", TO "disassemble", " can display their contents, the function ", TO "pseudocode", " can convert
     a function closure to pseudocode, the function ", TO "value", " can evaluate it (bindings of values to local symbols
     are enclosed with the pseudocode), the operator ", TO "===", " can be used for equality testing,
     and when the debugger is activated after an error, the variable ", TO "current", " contains the pseudcode step whose execution produced the error.",
     }
document { Key => pseudocode,
     Headline => "produce the pseudocode for a function",
     Usage => "pseudocode f",
     Inputs => { "f" => FunctionClosure },
     Outputs => { Pseudocode => { "the pseudocode of the function ", TT "f"} },
     SeeAlso => { disassemble },
     EXAMPLE lines ///
	  pseudocode resolution
	  disassemble oo
     ///
     }
document { Key => disassemble,
     Headline => "disassemble pseudocode or a function",
     Usage => "disassemble c",
     Inputs => { "c" => Pseudocode },
     Outputs => { String => {"the disassembled form of ", TT "c"} },
     SeeAlso => { pseudocode },
     EXAMPLE lines ///
     disassemble res
     ///,
     PARA {
	  "It may be useful to disassemble code during debugging, as in the following demonstration."
	  },
     EXAMPLE lines ///
     load "Macaulay2Doc/demo1.m2"
     code g
     g 2
     code current
     disassemble current
     ///
     }
document { Key => "current",
     Headline => "the pseudocode that produced an error",
     Usage => "current",
     Outputs => { Pseudocode => { "the pseudocode that produced an error, or ", TO "null", ", if none" } },
     "Use ", TO "value", " to evaluate the code again, for debugging purposes.",
     EXAMPLE lines ///
     load "Macaulay2Doc/demo1.m2"
     code g
     g 2
     code current
     disassemble current
     value current
     x = 11
     value current
     continue
     ///
     }
document { Key => (value, Pseudocode),
     Headline => "execute pseudocode",
     Usage => "value p",
     Inputs => { "p" },
     Outputs => {{ "the value returned by evaluation of ", TT "p" }},
     PARA {
	  "Here is an example of its use in the debugger, to see whether modifying the value of a local
	  variable fixed the problem in the code, by executing just the offending line."
	  },
     EXAMPLE lines ///
     load "Macaulay2Doc/demo1.m2"
     g 2
     value current
     x = 11
     value current
     ///,
     SeeAlso => { "current", pseudocode }
     }
document { Key => getGlobalSymbol,
     Headline => "create a global symbol in a global dictionary",
     Usage => "getGlobalSymbol(dict,nam)\ngetGlobalSymbol nam",
     Inputs => {
	  "dict" => GlobalDictionary,
	  "nam" => String
	  },
     Outputs => {
	  { "a global symbol in the dictionary ", TT "dict", " whose name is the string ", TT "nam", ", which will be created, if necessary" }
	  },
     Consequences => {
	  { "if a new symbol is created, it is stored under the name ", TT "nam", " in the dictionary ", TT "dict" }
	  },
     PARA {
	  "If ", TT "dict", " is omitted, then the first symbol found in the dictionaries listed in ", TO "dictionaryPath", " will be returned.
	  If none is found, one will be created in the first dictionary listed in ", TO "dictionaryPath", ", unless it is not mutable, in
	  which case an error will be signalled; perhaps that behavior should be changed."
	  },
     EXAMPLE lines ///
	  d = new Dictionary
	  sym = getGlobalSymbol(d,"foo")
	  d
	  peek d
	  d#"foo" === sym
	  d#"asfd" = sym
	  peek d
     ///
     }
