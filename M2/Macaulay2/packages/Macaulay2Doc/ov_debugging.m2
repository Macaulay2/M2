-- TODO: (debug, ZZ)

document {
    Key => "debugging",
    "Macaulay2 has a debugger.",
    UL{
	TO "the debugger",
    },
    "Here are some other debugging tools.",
    Subnodes => {
	TO "the debugger",
	TO "debug",
	TO "assert",
	TO "generateAssertions",
	TO "benchmark",
	TO "Browse::browse",
	TO "code",
	TO "current",
	TO "currentPosition",
	TO "lineNumber", -- TODO: rename to currentLineNumber
	TO "currentRowNumber",
	TO "currentColumnNumber",
	TO "currentFileName",
	TO "currentFileDirectory",
	-- TO "currentDirectory",
	TO "pseudocode",
	TO "disassemble",
	TO "frames",
	TO functionBody,
	TO dictionary,
	TO "dictionaryPath",
	TO localDictionaries,
	TO "fileDictionaries",
	TO "edit",
	--TO "error",
	TO "backtrace",
	TO "stopIfError",
	TO "handleInterrupts",
	TO "debuggingMode",
	TO "debugError",
	TO "errorDepth",
	TO "debugLevel",
	TO "engineDebugLevel",
	TO "loadDepth",
	TO "interpreterDepth",
	TO "recursionDepth",
	TO "recursionLimit",
	TO "flagLookup",
	TO "listLocalSymbols",
	TO "listUserSymbols",
	TO "commandInterpreter",
	TO "locate",
	TO "methods",
	TO "on",
	TO "peek",
	TO "peek'",
	TO "profile",
	TO "shield",
	TO "showStructure",
	TO "showClassStructure",
	TO "showUserStructure",
	TO "userSymbols",
	TO "listSymbols",
	TO "lookupCount",
	TO "serialNumber",
    },
    SeeAlso => {
	"error handling"
    }
}

document {
    Key => "the debugger",
    "We have a Macaulay2 source file with a pair of functions in it that
    we can use for demonstrating the debugger.  Let's load it so we can
    run the functions in it.",
    EXAMPLE "load \"Macaulay2Doc/demos/demo1.m2\"",
    "We can see what functions were provided to us with ", TO "listUserSymbols", ".",
    EXAMPLE "listUserSymbols",
    "Let's peek at the code of the function ", TT "g", ".",
    EXAMPLE "code g",
    "We see that the function g calls a function ", TT "f", ", but ", TT "f", " is not visible to us
    (because ", TT "f", " is a local variable).  In Emacs' ", EM "Macaulay2 Interaction Mode", ", pressing
    return (", TT "RET", " or ", TT "enter", ") after positioning the cursor on the output line displaying the file name and line number
    will bring up the source code in a new buffer.",
    PARA{"The first few times we use ", TT "g", ", it seems to work."},
    EXAMPLE {"g 4", "g 3"},
    "However, the following attempt results in an error, and the debugger starts up automatically.",
    EXAMPLE "g 2",
    "You may use ", TO "help", ", as instructed, to view the commands available in the debugger.
    As suggested by the help display, we can use ", TO "listLocalSymbols", " to list the local symbols and their values.",
    EXAMPLE "listLocalSymbols",
    "We see that the value of ", TT "x", " is 0, and that explains the error message about division by zero.
    The other local symbols are the ones defined in the body of the function ", TT "f", ", whose
    code can now be displayed with ", TO "code", ".",
    EXAMPLE "code f",
    "We can use ", TO "step", " with argument 0 to bypass the current expression.",
    EXAMPLE "step 0",
    "If we decide the problem is one level up, we can use ", TT "end", " or the end-of-file character
    (which often is CTRL-D) to quit this instance of the debugger.  In this case, the debugger will 
    be entered again (triggered by the same error indication that caused it to be entered originally)
    at the point inside the function ", TT "g", " from which the function ", TT "f", " was called.",
    EXAMPLE "end",
    "We can use ", TO "listLocalSymbols", " again to see the local variables of ", TT "g", ".",
    EXAMPLE "listLocalSymbols",
    "After we are done debugging, we can quit the debugger entirely and return to top level
    with ", TO "break", ".",
    EXAMPLE "break",
    "The variable ", TO "errorDepth", " can be used to control how deep inside the code the debugger should be activated.",
    SeeAlso => { "break", "end", "step", "continue", "return", "listLocalSymbols", "listUserSymbols", "code", "value", "pseudocode", "disassemble", "errorDepth" },
    Subnodes => {
	TO "step",
    },
}     

doc ///
  Key
    (debug, Package)
  Headline
    open the private dictionary of a package
  Usage
    debug pkg
  Inputs
    pkg:Package
  Consequences
    Item
      the private dictionary of the package @TT "pkg"@ is added to @TO "dictionaryPath"@
      so its non-exported symbols are visible to the user
  Description
    Text
       For example, the private dictionary for Macaulay2 may be opened using
    Example
      debug Core
    Text
      This allows access to the low level ("raw") routines implemented by the Macaulay2 engine,
      although this is mainly useful for debugging Macaulay2 itself.
    Example
      R = QQ[a..d];
      raw R
  SeeAlso
    export
    exportFrom
    importFrom
    "dictionaryPath"
    (debug, GlobalDictionary)
///

doc ///
Node
  Key
   "profile"
    profileSummary
   (profileSummary, Thing)
   (profileSummary, String)
    coverageSummary
   (coverageSummary, Thing)
   (coverageSummary, String)
  Headline
    profile a computation
  Usage
    profile f()
    profileSummary f
    coverageSummary f
  Consequences
    Item
      The hash table @TT "ProfileTable"@ is updated with the profiling measurement data.
  Description
    Text
      Running a computation with the @TT "profile"@ keyword enables the Macaulay2 profiler,
      which measures and stores the frequency of particular instructions for aid in
      analyzing and optimizing the computation.
    Example
      profile matrix table(4, 5, (i,j) -> i^j)
    Text
      Afterwards, running @TT "profileSummary"@ and @TT "coverageSummary"@ produces
      easy to read tables summarizing the accumulated data so far in different ways.
    Example
      profileSummary
      coverageSummary
///

document {
     Key => "step",
     Headline => "step by single lines in the debugger",
     Usage => "step n",
     Inputs => { "n" => ZZ },
     Consequences => {
	  {"This command is active within the debugger.  The current expression is executed and execution
	       is continued until ", TT "n", " lines of source code whose load depth is as large as ", TO "errorDepth", " have
	       been encountered.  If ", TT "n", " is omitted, then 1 is used.  If ", TT "n", " is negative then
	       instead, ", TT "n", " microsteps in the inner interpreter are executed and traced." }
	  },
     PARA {
	  "One useful way to debug code suspected of being in error is to insert an explicit error message, such
	  as ", TT ///error "debug me"///, ", and start stepping from there, as in the following demonstration."
	  },
     EXAMPLE lines ///
     load "Macaulay2Doc/demos/demo2.m2"
     code f
     f 0
     return
     current
     disassemble oo
     step(-3)
     step
     step
     t
     ///,
     SeeAlso => { "debugging" }
     }

document { Key => "engineDebugLevel",
     Headline => "current engine debugging level",
     "This variable is in place, but the corresponding code is not implemented yet."
     }

document { Key => "debuggingMode",
     Headline => "whether to enter the debugger when an error occurs",
     Usage => "debuggingMode = true",
     Consequences => {{"the debugger will be entered when an error occurs"}}}
document { Key => "debugLevel",
     Headline => "current debugging level",
     Usage => "debugLevel = n",
     Inputs => {"n" => ZZ },
     "Some M2 routines will display debugging information if ", TO "debugLevel", " is set to a value greater than 0.",
    SeeAlso => { "gbTrace", "engineDebugLevel" },
    }
document { Key => "debugError",
     Headline => "a function to debug",
     Usage => "debugError()",
     "In certain situations, after an error occurs, the offending code, in the form of a function, will be stored in the
     variable ", TO "debugError", ", so the user can debug it by running it."
     }

document { Key => "stopIfError",
     Headline => "whether to stop the program when an error occurs",
     PARA {
	  "This can be useful for showing code in documentation examples that results
	  in an error being raised.  For an example, see ", TO "assert", "."
	  }     
     }
document { Key => "interpreterDepth",
     Headline => "nesting depth of the interpreter",
     SeeAlso => {commandInterpreter}}

document { Key => "notify",
     Headline => "whether to notify the user when a file is loaded",
     Usage => "notify = true\nnotify = false",
     Consequences => {
	  {"If ", TO "notify", " is set to ", TO "true", ", then each time a file or a package is loaded, a message will be displayed."}
	  },
     EXAMPLE lines ///
     notify = true
     loadPackage "FirstPackage"
     ///,
     SeeAlso => {load, needs, loadPackage, needsPackage}
     }

document { Key => commandInterpreter,
     Headline => "the top level command interpreter",
     Usage => "commandInterpreter f",
     Inputs => {
	  "f" => { TT "()", " or ", ofClass{Dictionary, Symbol, Pseudocode, Function} }
	  },
     Consequences => {{"the top level command interpreter will be called with the symbols in the lexical scope of ", TT "f", " visible to the user."}},
     "One more ", TT "i", " will be added to prompt each time the command interpreter is entered.  To leave it, type ", TT "end", " or the end of file character.",
     EXAMPLE lines ///
	  f = (x -> y -> 11) 13
	  commandInterpreter f
	  x
	  end
	  x
     ///,
     "This facility is useful as a debugging tool, and is used by the standard debugger."
     }


document {
     Key => flagLookup,
     Headline => "flag a symbol",
     Usage => "flagLookup x",
     Inputs => { "x" => Symbol },
     Outputs => {
	  Boolean => {"whether each subsequent reference to the symbol ", TT "x", " will result in an error message"}
	  },
     Consequences => {{
	       "The first use arranges for each subsequent reference to the symbol ", TT "x", " to result in an error message.
	       The second use cancels the arrangement.  Each subsequent use toggles the state."
	       }},
     PARA {
	  "To get access to a flagged symbol without signalling an error, use ", TO "getGlobalSymbol", "."
	  }
     }

document {
     Key => error,
     Headline => "deliver error message",
	Usage => "error s",
	Inputs => {
		"s" => String => "a string or a sequence of things that can be converted to strings"
		},
	Consequences => {
		"an error message is displayed"
		},
     TT "error s", " causes an error message s to be displayed.",
     PARA{},
     "The error message ", TT "s", " (which should be a string or a sequence of
     things that can be converted to strings and concatenated) is printed.
     Execution of the code is interrupted, and control is returned
     to top level. Here is an explicit example showing how this command is used:",
     PRE M2CODE///exampleCode = p -> (
     if not isPrime p
     then error "expected a prime integer";
     if p == 2
     then error "expected an odd prime";
     lift((p+1)/2, ZZ)
     );///,
     PARA {
	  "Eventually we will have a means of ensuring that the line
	  number printed out with the error message will have more
	  significance, but currently it is the location in the code of
	  the error expression itself."
	  },
     PARA {
	  "If the error message begins with a hyphen, then the word \"error\" will not be
	  inserted into the error message."
	  }
     }


document {
     Key => "recursionLimit",
     Headline => "set the limit on recursion",
     Usage => "recursionLimit = n",
     Inputs => { "n" => ZZ },
     Consequences => { {"The recursion depth limit for the interpreter is set to ", TT "n", "."} },
     "Each time a function is called, the recursion depth is incremented by
     1, and each time a function returns, the recursion depth is decremented.
     This limit on recursion depth is a way to detect infinite loops."
     }
document {
     Key => recursionDepth,
     Headline => "the current recursion depth",
     Usage => "recursionDepth()",
     Outputs => { ZZ => "the current depth of recursion in the interpreter" },
     EXAMPLE lines ///
	  recursionDepth()
	  f = x -> recursionDepth()
	  f()
	  g = x -> f()
	  g()
     ///,
     "Tail recursion optimization may be in effect, so there the answer may not always be what you expect.",
     EXAMPLE lines ///
	  r = i -> if i == 100 then recursionDepth() else r(i+1)
	  r 0
     ///,
     SeeAlso => { "recursionLimit" }
     }

document {
     Key => FilePosition,
     Headline => "the class of all file positions",
     TT "FilePosition", " -- a type of list designed to represent a position
     in a file.",
     PARA{},
     "It's implemented as a list with 3, 5 or 7 elements. The first part is the file name, then each pair is a row/column position.
     A single pair is a position, two form a range. The last pair is the central point of interest in that range."
     }

undocumented {
    (symbol |, FilePosition, String),
    (symbol |, String, FilePosition),
}

document {
     Key => uncurry,
     Headline => "uncurry a function",
     TT "uncurry(f, (a,b))", "     -- computes ", TT "((f a) b)", ".",
     BR{},
     TT "uncurry(f, (a,b,c))", "   -- computes ", TT "(((f a) b) c)", ".",
     BR{},
     TT "uncurry(f, (a,b,c,d))", " -- computes ", TT "((((f a) b) c) d)", ".",
     BR{},
     "... and so on.",
     EXAMPLE {
	  "f = a -> b -> c -> [a,b,c]",
	  "uncurry(f, (1,2,3))"
	  }
     }


document {
     Key => "handleInterrupts",
     Usage => "handleInterrupts = b",
     Inputs => { "b" => Boolean },
     Outputs => { Boolean => { "the value of ", TT "b" }},
     Consequences => {
	  {
	       "If ", TT "b", " is ", TO "false", ", then the default operating system actions for the signals ", TT "SIGINT", ", and ", TT "SIGALRM", " are restored,
	       and thus typing CTRL-C or the triggering of an ", TO "alarm", " will result in the Macaulay2 process terminating immediately.
	       If ", TT "b", " is ", TO "true", ", then the default Macaulay2 signal handlers are installed,
	       and thus control will be returned to top level after the code currently executing notices that the interrupt flag has been set."
	       }
	  },
     PARA {
	  "The command line option ", TT "--int", " has the same effect as ", TT "handleInterrupts=false", "."
	  }
     }

document {
     Key => {on,[on,CallLimit],[on,Name],[on,GenerateAssertions],GenerateAssertions,CallLimit},
     Headline => "trace a function each time it's run",
     Usage => "on f",
     Inputs => {
	  "f" => Function,
	  CallLimit => ZZ => {"the maximum number of times to permit the function ", TT "f", " to be called"},
	  Name => String => {"the name to use for the function, in case ", TT "f", " is an anonymous function (not assigned to a global variable)"},
	  GenerateAssertions => Boolean => {
	       "whether to print assertion statements that can be used as input to Macaulay2 to
	       check the behavior of the function remains the same.  Arguments and values are prepared
	       with ", TO "toExternalString", ", failure of which is silently ignored."
	       }
	  },
     Outputs => { Function => {"a new function that returns the same values that ", TT "f", " would have returned, but has a few side effects
	       useful for debugging: upon entry, it prints its arguments, and upon exit it prints its return values.  The display includes the name of ", TT "f", ",
	       a sequence number in parentheses that tells how many times the function has been called, and a number in brackets that gives the nesting (recursion) depth.
	       The sequence number allows the entry and exit reports to be connected."
	       }},
     PARA{
	  "Ideally, this function would replace ", TT "f", ", i.e., we would write ", TT "f = on f", ".  Unfortunately, all the pre-installed system functions
	  are write-protected; fortunately, their methods are not, and can be replaced."
	  },
     EXAMPLE lines ///
     ker Matrix := on(lookup(ker,Matrix),GenerateAssertions=>true,Name=>"ker"); -* no-capture-flag *-
     f = x -> kernel (x|x);
     R = QQ[a..c];
     f vars R
     ///,
     SeeAlso => {"lookup"}
     }

doc ///
  Key
    assert
    (assert, Thing)
    (assert, Expression)
  Headline
    check whether something is true, raise an error if not
  Usage
    assert x
  Inputs
    x:Thing
  Description
    Text
      @TT "assert x"@ raises an error if @TT "x"@ isn't true.
    Example
      stopIfError = false;
      assert( (2+2) === 4)
      assert(rank matrix {{1, 2}, {2, 4}} == 2)
    Text
      If @TT "x"@ is an @TO Expression@ that evaluates to false, then
      a partially evaluated form is printed with the error message to
      assist in debugging.
    Example
      assert Equation(rank matrix {{1, 2}, {2, 4}}, 2)
  SeeAlso
    generateAssertions
    "stopIfError"
///

document {
     Key => notImplemented,
     Headline => "print an 'not implemented' error message",
	Usage => "notImplemented()",
     TT "notImplemented()", " prints an error message that
     says \"not implemented yet\"."
     }

document {
     Key => "errorDepth",
     Headline => "set the error printing depth",
     TT "errorDepth = i", " -- sets the error depth to ", TT "i", ", which should be
     a small integer, returning the old value.",
     PARA{
	  "During the backtrace after an error message, a position in interpreted
	  code is displayed and the debugger is entered only if the load depth was at least as large at the
	  time the code was parsed as the error depth is now.
	  The default value is 3, which shows only positions in the user's code and positions
	  inside loaded packages whose debugging mode is true.  Set it to 2 to also debug statements
	  inside loaded packages, except for the package ", TO "Core", ".  Set it to 1 to also
	  debug statements in the core, and set it to 0 to debug statements in the bootstrap code."
	  },
     SeeAlso => { "loadDepth" }
     }

document {
     Key => "loadDepth",
     Headline => "the load depth",
     TT "loadDepth = i", " -- sets the load depth to ", TT "i", ", which should be
     a small integer, returning the old value.",
     PARA{
	  "During the backtrace after an error message, a position in interpreted
	  code is displayed only if the load depth at the
	  time the code was parsed is at least as large as the error depth is now.  The load depth
	  is set to 0 initially, is set to 1 when the files of the ", TO "Core", "
	  package are being loaded, is set to 2 while loading a package with the ", TO "debuggingMode", " option
	  set to ", TO "false", ", and is set to 3 while loading a package with the ", TO "debuggingMode", " option
	  set to ", TO "true", " and for user input."
	  },
     PARA {
	  "The value of ", TO "loadDepth", " active when code is parsed is referred to later when
	  error messages are being handled: see ", TO "errorDepth", ", and it is also displayed, in parentheses,
	  when the error message is printed."
	  },
     Caveat => { "The user should not attempt to adjust the value of ", TO "loadDepth", "." },
     }

document {
     Key => benchmark,
     Headline => "accurate timing of execution",
     Inputs => {
	     "s" => String => "a string containing Macaulay2 code"
	     },
     Outputs => {
	     RR => {"the number of seconds it takes to evaluate the code in ", TT "s"}
	     },
     Usage => "benchmark s",
     "Produces an accurate timing for the code contained in the string ", TT "s", ".  The value returned is the number of seconds.",
     EXAMPLE {
		///benchmark "sqrt 2p100000"///
		},
     "The snippet of code provided will be run enough times to register
     meaningfully on the clock, and the garbage collector will be called
     beforehand.",
     }

undocumented {(code, Nothing)}

document {
  Key => {
    code,
   (code, Symbol),
   (code, Command),
   (code, Function),
   (code, Sequence),
   (code, Pseudocode),
   (code, List),
   (code, ZZ)},
     Headline => "display source code",
     SYNOPSIS (
	  Usage => "code f",
	  Inputs => {
	       "f" => {ofClass{Function,Command}}
	       },
	  Outputs => {Net => {"the source code of the function or command", TT "f"}},
	  EXAMPLE "code listUserSymbols"
	  ),
     SYNOPSIS {
	  Usage => "code(f,X)",
	  Inputs => {
	       "f" => {ofClass{Function,Keyword}},
	       "X" => Type
	       },
	  Outputs => {Net => {"the source code of method for applying ", TT "f", " to an
		    argument of type ", TT "X"
		    }},
	  EXAMPLE "code(res,Ideal)"
	  },
     SYNOPSIS {
	  Usage => "code(f,X,Y)",
	  Inputs => {
	       "f" => {ofClass{Function,Keyword}},
	       "X" => Type,
	       "Y" => Type
	       },
	  Outputs => {Net => {"the source code of method for applying ", TT "f", " to
		    arguments of type ", TT "X", " and ", TT "Y"
		    }},
	  EXAMPLE "code(symbol :, Ideal, Ideal)"
	  },
     SYNOPSIS {
	  Usage => "code(f,X,Y,Z)",
	  Inputs => {
	       "f" => {ofClass{Function,Keyword}},
	       "X" => Type,
	       "Y" => Type,
	       "Z" => Type
	       },
	  Outputs => {Net => {"the source code of method for applying ", TT "f", " to
		    arguments of type ", TT "X", ", ", TT "Y", ", and ", TT "Z"
		    }}
	  },
     SYNOPSIS {
	  Usage => "code(f,X,Y,Z,T)",
	  Inputs => {
	       "f" => {ofClass{Function,Keyword}},
	       "X" => Type,
	       "Y" => Type,
	       "Z" => Type,
	       "T" => Type
	       },
	  Outputs => {Net => {"the source code of method for applying ", TT "f", " to
		    arguments of type ", TT "X", ", ", TT "Y", ", ", TT "Z", ", and ", TT "T"
		    }}
	  },
     SYNOPSIS {
	  Usage => "code {v,w,...}",
	  Inputs => {
	       "{v,w,...}" => List
	       },
	  Outputs => {Net => {"the source code of the functions or commands", TT "v,w,...", ".
		    Such a list can be obtained, for example, with ", TO "methods", "."
		    }},
	  EXAMPLE "code methods use"
	  },
     SeeAlso => {"edit", "methods"}
     }

document {
     Key => edit,
     Headline => "edit source code",
     SYNOPSIS {
	  Usage => "edit f",
	  Inputs => {
	       "f" => {ofClass{Function,Command}}
	       },
	  Outputs => {Net => {"the editor is started up, pointing at the source code of the function or command", TT "f"}},
	  },
     SYNOPSIS {
	  Usage => "edit(f,X)",
	  Inputs => {
	       "f" => {ofClass{Function,Keyword}},
	       "X" => Type
	       },
	  Outputs => {Net => {"the editor is started up, pointing at the source code of method for applying ", TT "f", " to an
		    argument of type ", TT "X"
		    }},
	  },
     SYNOPSIS {
	  Usage => "edit(f,X,Y)",
	  Inputs => {
	       "f" => {ofClass{Function,Keyword}},
	       "X" => Type,
	       "Y" => Type
	       },
	  Outputs => {Net => {"the editor is started up, pointing at the source code of method for applying ", TT "f", " to
		    arguments of type ", TT "X", " and ", TT "Y"
		    }},
	  },
     SYNOPSIS {
	  Usage => "edit(f,X,Y,Z)",
	  Inputs => {
	       "f" => {ofClass{Function,Keyword}},
	       "X" => Type,
	       "Y" => Type,
	       "Z" => Type
	       },
	  Outputs => {Net => {"the editor is started up, pointing at the source code of method for applying ", TT "f", " to
		    arguments of type ", TT "X", ", ", TT "Y, and ", TT "Z"
		    }}
	  },
     SYNOPSIS {
	  Usage => "edit(f,X,Y,Z,T)",
	  Inputs => {
	       "f" => {ofClass{Function,Keyword}},
	       "X" => Type,
	       "Y" => Type,
	       "Z" => Type,
	       "T" => Type
	       },
	  Outputs => {Net => {"the editor is started up, pointing at the source code of method for applying ", TT "f", " to
		    arguments of type ", TT "X", ", ", TT "Y, and ", TT "Z", ", and ", TT "T"
		    }}
	  },
     PARA{
	  "The name of the user's preferred editor is take from the environment
	  variable ", TT "EDITOR", ".  If X is running and the editor is not
	  Emacs, then the editor is started in a new ", TT "xterm", " window."
	  },
     PARA{
	  "For an interactive example, try ", TT "edit(dim,Module)", ".",
	  },
     PARA{
	  "The value returned is the exit code returned by the editor, as with ", TO "run", ", usually zero."
	  }
     }

-- TODO: this needs some upgrades
-- TODO: also show how to use "headlines methods X"
document {
  Key => {
    methods,
   (methods, Command),
   (methods, Manipulator),
   (methods, Package),
   (methods, Sequence),
   (methods, Thing),
   (methods, ScriptedFunctor),
   (methods, Symbol),
   (methods, Type)
   },
     Headline => "list methods",
     SYNOPSIS (
	  Usage => "methods x",
	  Inputs => {
	       "x" => { ofClass{Function,Type,Keyword,Package} }
	       },
	  Outputs => {{
		    ofClass VerticalList, " of those methods associated with ", TT "x"
		    }},
	  EXAMPLE lines ///
	       methods BettiTally
	       methods resolution
	       methods symbol @@
	  ///
	  ),
     SYNOPSIS (
	  Usage => "methods(s,X)",
	  Inputs => {
	       "s" => Symbol, "X" => Type
	       },
	  Outputs => {{
		    ofClass VerticalList, " of those methods associated with the operator ", TT "s",
		    " and the type ", TT "X"
		    }},
	  EXAMPLE lines ///
	       methods( symbol ++, Module)
	  ///
	  ),
     SYNOPSIS (
	  Usage => "methods(X,Y)",
	  Inputs => {
	       "X" => Type, "Y" => Type
	       },
	  Outputs => {{
		    ofClass VerticalList, " of those methods associated with "
		    }},
	  EXAMPLE lines ///
	       methods( Matrix, Matrix )
	  ///
	  ),
     "This function operates by examining those types that are values of
     global symbols for keys that appear to be storing references to
     methods.  Types that don't appear as values of global variables will
     not be examined, so perhaps not all methods will be found.",
     SeeAlso => {
	 (options, ZZ), (code, ZZ), (code, List),
	 (locate, List), (makeDocumentTag, List)
	 }
     }

document {
    Key => "backtrace",
     Headline => "whether a backtrace is displayed following an error message",
     Usage => "backtrace = false",
     Consequences => { "a backtrace will not displayed following an error message" }
     }

document {
    Key => {
	Bag,
	(symbol ?,Bag,Bag)
    },
    Headline => "the class of all bags",
    PARA "A bag can be used for enclosing something in a container to prevent it from being printed, in normal circumstances.
    Any mutable list can be used for this purpose, but bags are designed for this purpose.  In comparison and sorting, they
    are declared to be incomparable.",
    Subnodes => { TO unbag },
}
document {
    Key => {
	unbag,
       (unbag, Bag)
    },
    Usage => "unbag y",
    Inputs => { "y" },
    Outputs => { { "the contents of ", TT "y" }},
    EXAMPLE lines ///
	  x = 100!
	  y = Bag {x}
	  unbag y
     ///
     }
