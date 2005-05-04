--		Copyright 1993-1998 by Daniel R. Grayson

document {
     Key => "timing",
     Headline => "time a computation",
     TT "timing e", " evaluates ", TT "e", " and returns a list of type ", TO "Time", "
     of the form ", TT "{t,v}", ", where ", TT "t", " is the number of seconds
     of cpu timing used, and ", TT "v", " is the value of the the expression.",
     PARA,
     "The default method for printing such timing results is to display the
     timing separately in a comment below the computed value.",
     EXAMPLE {
	  "timing 3^30",
      	  "peek oo",
	  },
     SeeAlso => "Time"
     }

document {
     Key => "time",
     Headline => "time a computation",
	Usage => "time e",
     TT "time e", " evaluates ", TT "e", ", prints the amount of cpu time
     used, and returns the value of ", TT "e", ".",
     EXAMPLE "time 3^30",
     SeeAlso => "timing"
     }

document {
     Key => Time,
     Headline => "the class of all timing results",
     TT "Time", " is the class of all timing results.  Each timing result
     is ", OFCLASS BasicList, " of the form ", TT "{t,v}", ", where ", TT "t", " 
     is the number of seconds of cpu time used, and ", TT "v", " is the value 
     of the the expression."
     }

document {
     Key => "null",
     Headline => "the unique member of the empty class",
     "When it is the value of an expression entered into the interpreter, the
     output line doesn't appear.  Empty spots in a list are represented by
     it.",
     PARA,
     "It is the only member of the class ", TO "Nothing", ", which strictly
     speaking, ought to have no members at all.",
     PARA,
     "An ", TO "if", " expression with no ", TO "else", " clause returns
     ", TO "null", " when the predicate is false.",
     PARA,
     "Various routines that prepare data for printing convert ", TO "null", "
     to an empty string.",
     EXAMPLE {
	  "x = {2,3,,4}",
	  "net x",
      	  "toString x#2",
      	  "peek x",
	  }
     }

document {
     Key => "then",
     Headline => "condition testing",
     TT "then", " a keyword used with ", TO "if", ".",
	EXAMPLE {
		"if 5 > 4 then 8 else 7"
		},
	SeeAlso => {"if", "else"}
     }

document {
     Key => "else",
     Headline => "condition testing",
     TT "else", " a keyword used with ", TO "if", ".",
	EXAMPLE {
		"if 4 > 5 then 8 else 7"
		},
	SeeAlso => {"if", "then"}
     }

document {
     Key => "if",
     Headline => "condition testing",
     TT "if p then x else y", " computes ", TT "p", ", which must yield the value ", TO "true", " 
     or ", TO "false", ".  If true, then the value of ", TT "x", " is provided,
     else the value of ", TT "y", " is provided.",
     PARA,
     TT "if p then x", " computes ", TT "p", ", which must yield the value ", TO "true", " 
     or ", TO "false", ".  If true, then the value of ", TT "x", " is provided,
     else ", TO "null", " is provided."
     }

document {
     Key => "for",
     Headline => "loop control",
     TT "for i from a to b when c list x do y", " repeatedly 
     evaluate ", TT "x", " and ", TT "y", " as long as ", TT "c", " is
     true, letting the variable ", TT "i", " be assigned the values
     ", TT "a", ", ", TT "a+1", ", ", TT "a+2", ", ..., ", TT "b", ",
     returning a list of the values of ", TT "x", " encountered.",
     PARA,
     "Each of the clauses ", TT "from a", ", ", TT "to b", ",
     ", TT "when c", ", ", TT "list x", ", and ", TT "do y", " is optional,
     provided either the ", TT "list x", " or the ", TT "do y", " clause is 
     present.  If the clause ", TT "from a", " is missing, then ", TT "from 0", "
     is assumed.  If the clause ", TT "to b", " is missing, then no upper limit
     is imposed on the value of ", TT "i", ".  If the clause
     ", TT "list x", " is absent, then ", TO "null", " is returned.",
     PARA,
     "The numbers ", TT "a", " and ", TT "b", " must be small integers that fit
     into a single word.",
     PARA,
     "The variable ", TT "i", " is a new local variable whose scope includes 
     only the expressions ", TT "c", ", ", TT "x", ", and ", TT "y", ".",
     EXAMPLE {
	  "for i from 3 to 6 do print i",
	  "for i when i^2 < 90 list i",
	  },
     SeeAlso => { "loops", "while", "return" }
     }     

document {
     Key => "while",
     Headline => "loop control",
     TT "while p do x", " repeatedly evaluates ", TT "x", " as long 
     as the value of ", TT "p", " remains ", TO "true", ", returning 
     ", TO "null", ".",
     BR,NOINDENT,
     TT "while p list x", " repeatedly evaluates ", TT "x", " as long 
     as the value of ", TT "p", " remains ", TO "true", ", returning a
     list of the values of ", TT "x", " encountered.",
     BR,NOINDENT,
     TT "while p list x do z", " repeatedly evaluates ", TT "x", " 
     and ", TT "z", " as long as the value of ", TT "p", " remains
     ", TO "true", ", returning a list of the values of ", TT "x", " 
     encountered.",BR,
     PARA,
     SeeAlso => { "loops", "break", "for" }
     }

document {
     Key => "break",
     Headline => "break from a loop",
     TT "break x", " interrupts execution of a loop controlled by ",
     TO "for", ", ", TO "while", ", ", TO "apply", ", or ", TO "scan", ", returning ",
     TT "x", " as the value of the loop currently being
     evaluated.",BR,
     TT "break", " interrupts execution of a loop, returning
     ", TO "null", " as the value of the function currently being evaluated.",
     PARA,
     "Warning: trying to break from a loop controlled by ", TO "table", " will
     probably not do what you expect, since ", TO "table", " is implemented by two nested
     loops controlled to ", TT "apply", ", and only the inner one will stop.",
     SeeAlso => { "scan", "while", "for" }
     }

document {
     Key => "return",
     Headline => "return from a function",
     TT "return x", " returns ", TT "x", " as the value of the function currently
     being evaluated.",BR,
     TT "return;", " returns ", TO "null", " as the value of the function currently
     being evaluated.",
     PARA,
     EXAMPLE {
	  "f = x -> (
     if x == 3 then return;
     if x > 3 then return x^2;
     5);",
	  "f 2",
	  "f 3",
	  "f 4"
	  },
     SeeAlso => { "break" }
     }

document {
     Key => "list",
     Headline => "loop control",
     TT "list", " a keyword used with ", TO "while", ", and ", TO "for", "."
     }

document {
     Key => "from",
     Headline => "loop control",
     TT "from", " a keyword used with ", TO "for", " and ", TO "new", "."
     }

document {
     Key => "to",
     Headline => "loop control",
     TT "to", " a keyword used with ", TO "for", "."
     }

document {
     Key => "do",
     Headline => "loop control",
     TT "do", " a keyword used with ", TO "while", ", and ", TO "for", "."
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
     PARA,
     "The clause '", TT "then y", "' may be omitted, in which case the return value is the value returned by ", TT "x", ", if there is no error or alarm.",
     PARA,
     "The clauses '", TT "then y else z", "' may both be omitted, in which case the return value is the value returned by ", TT "x", ", unless an error or
     alarm occurs, in which case ", TO "null", " is returned.",
     PARA,
     "The behavior of interrupts (other than alarms) is unaffected.",
     EXAMPLE "apply(-3..3,i->try 1/i else infinity)",
     Caveat => "We will change the behavior of this function soon so that it will be possible to catch errors of a particular type.  Meanwhile, users are
     recommended to use this function sparingly, if at all."
     }

document {
     Key => openFiles,
     Headline => "list the open files",
     TT "openFiles()", " produces a list of all currently open files.",
     PARA,
     SeeAlso => { "File" }
     }

document {
     Key => stdio,
     Headline => "the standard input output file",
     TT "stdio", " the standard input output file.",
     PARA,
     "Use this file to get input from the terminal, or to display information
     on the user's screen.  This is the file always used by ", TO "print", "
     and used ", TO "<<", " if it is not explicitly given a file."
     }

document {
     Key => stderr,
     Headline => "the standard error output file",
     TT "stderr", " the standard error output file.",
     PARA,
     "Use this file to display error messages on the user's screen."
     }

document {
     Key => openListener,
     Headline => "open a port for listening",
     TT "f = openListener \"$:service\"", "  opens a listener on the local
     host at the specified service port.",
     BR,NOINDENT,
     TT "f = openListener \"$\"", "  opens a listener on the local
     host at the Macaulay2 port (2500).",
     PARA,
     "Use ", TT "openInOut f", " to accept an incoming connection on the listener,
     returning a new input output file which serves as the connection.  The function
     ", TT "isReady", " can be used to determine whether an incoming connection has
     arrived, without blocking."
     }

document {
     Key => openIn,
     Headline => "open an input file",
     TT "openIn \"fff\"", " opens an input file whose filename is ", TT "fff", ".",
     PARA,
     "Other options are available.  For details, see ", TO "openInOut", "."
     }

document {
     Key => openOut,
     Headline => "open an output file",
     TT "openOut \"fff\"", " opens an output file whose filename is ", TT "fff", ".",
     PARA,
     "Other options are available.  For details, see ", TO "openInOut", "."
     }

document {
     Key => openInOut,
     Headline => "open an input output file",
     TT "openInOut \"fff\"", " opens an input output file whose 
     filename is ", TT "fff", ".",
     BR,NOINDENT,
     TT "openInOut \"!cmd\"", " opens an input output file which corresponds to a pipe 
     receiving the output from the shell command ", TT "cmd", ".",
     BR,NOINDENT,
     TT "openInOut \"$hostname:service\"", "opens an input output file
     by connecting to the specified service port at the specified host.",
     BR,NOINDENT,
     TT "openInOut \"$:service\"", " opens an input output file by
     listening to the specified service port on the local host, and 
     waiting for an incoming connection.",
     BR,NOINDENT,
     TT "openInOut \"$hostname\"", " opens an input output file
     by connecting to the Macaulay2 service port (2500) at the specified host.",
     BR,NOINDENT,
     TT "openInOut \"$\"", " opens an input output file by listening to the
     Macaulay2 service port (2500) on the local host, and waiting for an
     incoming connection.",
     BR,NOINDENT,
     TT "openInOut f", " opens an input output file by accepting a
     connection to the listener ", TT "f", ", previously created with
     ", TO "openListener", ".",
     PARA,
     "In order to open a socket successfully, there must be a process
     accepting connections for the desired service on the specified host.",
     PARA,
     "Socket connections are not available on Sun computers, because Sun 
     doesn't provide static versions of crucial libraries dealing with 
     network communications, or the static version doesn't provide network 
     name service for looking up hostnames.",
     PARA,
     "The various forms listed above can be used also with all other input
     output operations that open files, such as ", TO "openIn", ",
     ", TO "openOut", ", ", TO "get", ", and ", TO "<<", ", with data transfer 
     possible only in the direction specified.  The only subtlety is that 
     with ", TT ///openIn "!foo"///, " the standard input of the command
     ", TT "foo", " is closed, but with ", TT ///openOut "!foo"///, " the
     standard output of the command ", TT "foo", " is connected to the
     standard output of the parent Macaulay2 process."
     }

document {
     Key => protect,
     Headline => "protect a symbol",
     TT "protect s", " protects the symbol ", TT "s", " from having its value changed.",
     PARA,
     "There is no unprotect function, because we want to allow the compiler
     to take advantage of the unchangeability.",
     PARA,
     "The documentation function ", TO "document", " protects the symbols
     it documents."
     }

document {
     Key => isInputFile,
     Headline => "whether a file is open for input",
     TT "isInputFile f", " whether ", TT "f", " is an input file.",
     PARA,
     "The return value is ", TO "true", " or ", TO "false", "."
     }

document {
     Key => isOutputFile,
     Headline => "whether a file is open for output",
     TT "isOutputFile f", " whether ", TT "f", " is an output file.",
     PARA,
     "The return value is ", TO "true", " or ", TO "false", "."
     }

document {
     Key => isOpen,
     Headline => "whether a file or database is open",
     TT "isOpen f", " whether ", TT "f", " is an open file or an
     open database.",
     PARA,
     "An open file is either an input file, an output file, an
     input output file, or a listener.",
     PARA,
     "The return value is ", TO "true", " or ", TO "false", "."
     }

document {
     Key => isListener,
     Headline => "whether a file is open for listening",
     TT "isListener f", " whether ", TT "f", " is a listener.",
     PARA,
     "The return value is ", TO "true", " or ", TO "false", "."
     }

document {
     Key => symbol <<, Headline => "a binary operator" }
document {
     Key => symbol >>, Headline => "a binary operator" }
document {
     Key => (symbol >>, ZZ, ZZ),
     Headline => "shift bits rightward",
     Usage => "i >> j",
     Inputs => { "i" => null, "j" => null },
     Outputs => {{ "the integer obtained from ", TT "i", " by shifting its binary representation rightward ", TT "j", " places" }},
     EXAMPLE "256 >> 5",
     SeeAlso => {(symbol <<,ZZ, ZZ)}
     }
document {
     Key => (symbol <<, ZZ, ZZ),
     Headline => "shift bits rightward",
     Usage => "i << j",
     Inputs => { "i" => null, "j" => null },
     Outputs => {{ "the integer obtained from ", TT "i", " by shifting its binary representation leftward ", TT "j", " places" }},
     EXAMPLE "256 << 5",
     SeeAlso => {(symbol >>,ZZ, ZZ)}
     }

document {
     Key => (symbol <<, String, Thing),
     Headline => "print to a file",
     TT "\"name\" << x", " prints the expression ", TT "x", " on the output file
     named \"name\".",
     PARA,
     "Returns the newly created ", TO "File", " associated to the given name.
     Parsing associates leftward, so that several expressions may be displayed 
     with something like ", TT "\"name\"<<x<<y<<z", ".  It will often be convenient 
     to let the last output operation close the file, as illustrated below.",
     EXAMPLE {
	  "\"foo\" << 2^30 << endl << close",
      	  "get \"foo\"",
	  "unlink \"foo\""
	  }
     }

document {
     Key => (symbol <<, Thing),
     Headline => "print to a file",
     TT "<< x", " -- prints the expression x on the standard output file ", TO "stdio", ".",
     EXAMPLE "<< \"abcdefghij\" << endl",
     SeeAlso => {"<<"}
     }

document {
     Key => symbol ":",
     Headline => "a binary operator",
     }

document {
     Key => (symbol :, ZZ, Thing),
     Headline => "repeat an item",
     TT "n : x", " repetition ", TT "n", " times of ", TT "x", " in a sequence",
     PARA,
     "If ", TT "n", " is an integer and ", TT "x", " is anything, return a
     sequence consisting of ", TT "x", " repeated ", TT "n", " times.  A negative 
     value for ", TT "n", " will silently be treated as zero.",
     PARA,
     "Warning: such sequences do not get automatically spliced into lists
     containing them.",
     PARA,
     EXAMPLE { "{5:a,10:b}", "splice {5:a,10:b}" }
     }

document {
     Key => getc,
     Headline => "get a byte",
     TT "getc f", " obtains one byte from the input file f and provides it as a 
     string of length 1.  On end of file an empty string of is returned.",
     PARA,
     SeeAlso => { "File" },
     PARA,
     "Bug: the name is cryptic and should be changed."
     }

document {
     Key => symbol "<",
     Headline => "less than",
     TT "x < y", " yields ", TO "true", " or ", TO "false", 
     " depending on whether x < y.",
     PARA,
     "Calls upon ", TO "?", " to perform the comparison, if necessary."
     }

document {
     Key => symbol "<=",
     Headline => "less than or equal",
     TT "x <= y", " yields ", TO "true", " or ", 
     TO "false", " depending on whether x <= y.",
     PARA,
     "Calls upon ", TO "?", " to perform the comparison, if necessary."
     }

document {
     Key => symbol ">",
     Headline => "greater than",
     TT "x > y", " yields ", TO "true", " or ", 
     TO "false", " depending on whether x > y.",
     PARA,
     "Calls upon ", TO "?", " to perform the comparison, if necessary."
     }

document {
     Key => symbol ">=",
     Headline => "greater than or equal",
     TT "x >= y", " yields ", 
     TO "true", " or ", 
     TO "false", " depending on whether x >= y.",
     PARA,
     "Calls upon ", TO "?", " to perform the comparison, if necessary."
     }

protect incomparable
document {
     Key => incomparable,
     Headline => "a result indicating incomparability",
     TT "incomparable", " a symbol which may be returned by ", TO "?", "
     when the two things being compared are incomparable."
     }

document {
     Key => symbol "?",
     Headline => "comparison operator",
     TT "x ? y", " compares x and y, returning ", TT "symbol <", ", ",
     TT "symbol >", ", ", TT "symbol ==", ", or ", TO "incomparable", ".",
     PARA,
     "The user may install additional ", TO "binary methods", " for this 
     operator with code such as ",
     PRE "         X ? Y := (x,y) -> ...",
     "where ", TT "X", " is the class of ", TT "x", " and ", TT "Y", " is the
     class of ", TT "y", ".",
     EXAMPLE {
	  "3 ? 4",
      	  "\"book\" ? \"boolean\"",
      	  "3 ? 3.",
	  }
     }

document {
     Key => ";",
     Headline => "statement separator",
     TT "(e;f;...;g;h)", " the semicolon can be used for evaluating a sequence of 
     expressions.  The value of the sequence is the value of its
     last expression, unless it is omitted, in which case the value
     is ", TO "null", ".",
     EXAMPLE {
	  "(3;4;5)",
      	  "(3;4;5;)"
	  }
     }

document {
     Key => symbol "<-",
     Headline => "assignment with left side evaluated",
     TT "x <- y    ", " assigns the value of y to x, but x is evaluated, too.",
     PARA,
     "If the value of x is a symbol, then the value of y is assigned as the
     value of that symbol.  If the value of x is a hash table, then the value 
     of y must be one, too, and the contents of y bodily replace the contents
     of x.  If the value of x is a list, then the value of y must be a list, 
     and the contents of y replace the contents of x.",
     PARA,
     "Warning: if y is a class with instances, these instances
     will NOT become instances of x.  If instances of x are 
     created later, then they will not be compatible with the
     instances of y.  One should try to avoid using ", TO "<-", " in this
     case.",
     PARA,
     "The value of the expression x <- y is x, with its new contents.",
     PARA,
     SeeAlso => "="
     }

document {
     Key => "=",
     Headline => "assignment",
     TT "x = e", " assigns the value ", TT "e", " to the variable ", TT "x", ".",
     PARA,
     NOINDENT,
     TT "x#i = e", " assigns the value ", TT "e", " to the ", TT "i", "-th member of the array ", TT "x", ".  Here
     ", TT "i", " must be a nonnegative integer.",
     PARA,
     NOINDENT,
     TT "x#k = e", " assigns the value ", TT "e", " to the key ", TT "k", " in the hash table
     ", TT "x", ".  Here ", TT "k", " can be any expression.",
     PARA,
     NOINDENT,
     TT "(a,b,c) = x", " assigns the members of the sequence ", TT "x", " as
     values of the variables ", TT "a", ", ", TT "b", ", ", TT "c", ".  If ", TT "x", "
     has too few elements, then the trailing symbols on the left
     side are given the value ", TO "null", ".  If ", TT "x", " has too
     many elements, then the last symbol on the left hand side is given
     as value a sequence containing the trailing elements of the right hand side.
     If the right hand side is not a sequence, then ", TT "a", " gets the value, and
     ", TT "b", " and ", TT "c", " get ", TO "null", ".",
     SeeAlso => {"HashTable", ":=", "GlobalReleaseHook", "GlobalAssignHook"}
     }


document {
     Key => ":=",
     Headline => "assignment of method or new local variable",
     TT "x := e", " assign the value e to the new local variable x",
     BR,NOINDENT,
     TT "f X := (x) -> ( ... )", " installs a method for the method function
     ", TT "f", " acting on an argument of class ", TT "X", ".",
     BR,NOINDENT,
     TT "X * Y := (x,y) -> ( ... )", " installs a method for the operator
     ", TT "*", " applied to arguments of classes ", TT "X", " and ", TT "Y", ".
     Many other operators are allowed: see ", TO "operators", ".",
     PARA,
     NOINDENT,
     TT "(a,b,c) := x", " assigns the members of the sequence ", TT "x", " as
     values of the local variables ", TT "a", ", ", TT "b", ", ", TT "c", ".  If ", TT "x", "
     has too few elements, then the trailing symbols on the left
     side are given the value ", TO "null", ".  If ", TT "x", " has too
     many elements, then the last symbol on the left hand side is given
     as value a sequence containing the trailing elements of the right hand side.
     If the right hand side is not a sequence, then ", TT "a", " gets the value, and
     ", TT "b", " and ", TT "c", " get ", TO "null", ".",
     PARA,
     "This operator is slightly schizophrenic in its function, as the installation
     of a method has global effect if the classes involved are globally known,
     as is typically the case, whereas the assignment of a value to a local
     variable is never globally known."
     }

document {
     Key => abs,
     Headline => "absolute value function", 
	Usage => "abs x",
	Inputs => {
		"x" => "a number"
		},
	Outputs => {
		{"the absolute value of ", TT "x"}
		},
     TT "abs x", " computes the absolute value of ", TT "x", ".",
	EXAMPLE {
		"abs(-pi)"
		},
     }


-- the next three document nodes illustrate three possibilities for the first entry of the list:
-- string, function, and symbol.

document {
     Key => sin, -- this node is used as an example in the documentation node Inputs and Outputs.
     Headline => "sine function",
     Usage => "sin x",
     Inputs => { "x" => RR => null },
     Outputs => { { "the sine of ", TT "x", "" } },
	EXAMPLE {
		"sin (pi/2)"
		}
	 }
document {
     Key => cos, 
     Headline => "cosine function",
     Usage => "cos x",
     Inputs => { "x" => RR => null },
     Outputs => { { "the cosine of ", TT "x", "" } },
	EXAMPLE {
		"cos 2"
		}
	 }
document {
     Key => symbol tan,
     Headline => "tangent function",
     Usage => "tan x",
     Inputs => { "x" => RR => null },
     Outputs => { { "the tangent of ", TT "x" } },
	EXAMPLE {
		"tan 1"
		}
	 }
document {
     Key => asin,
     Headline => "arcsine function", 
     Usage => "asin x",
     Inputs => { "x" => RR => null },
     Outputs => { { "the arcsine of ", TT "x"} },
	EXAMPLE {
		"asin 1"
		}
	 }
document {
     Key => acos,
     Headline => "arccosine function", 
     Usage => "acos x",
     Inputs => { "x" => RR => null },
     Outputs => { { "the arccosine of ", TT "x"} },
	EXAMPLE {
		"acos .5"
		}
	 }
document {
     Key => atan,
     Headline => "arctangent function",
     Usage => "atan x",
     Inputs => { "x" => RR => null },
     Outputs => { { "the arctangent of ", TT "x"} } }
document {
     Key => (atan,RR,RR),
     Usage => "atan(x,y)",
     Inputs => { "x" => RR => null, "y" => RR => null },
     Outputs => { { "the angle formed with the x-axis by the ray from the origin to the point ", TT "{x,y}" } } }
document {
     Key => sinh,
     Headline => "hyperbolic sine function",
     Usage => "sinh x",
     Inputs => { "x" => RR => null },
     Outputs => { { "the hyperbolic sine of ", TT "x" } } }
document {
     Key => cosh, Headline => "hyperbolic cosine function",
     Usage => "cosh x",
     Inputs => { "x" => RR => null },
     Outputs => { { "the hyperbolic cosine of ", TT "x" } } }
document {
     Key => tanh, Headline => "hyperbolic tangent function",
     Usage => "tanh x",
     Inputs => { "x" => RR => null },
     Outputs => { { "the hyperbolic tangent of ", TT "x" } } }
document {
     Key => exp, Headline => "exponential function",
     Usage => "exp x",
     Inputs => { "x" => RR => null },
     Outputs => { { "the exponential of ", TT "x" } } }
document {
     Key => log, Headline => "logarithm function",
     Usage => "log x",
     Inputs => { "x" => RR => null },
     Outputs => { { "the logarithm of ", TT "x"} } }
document {
     Key => sqrt, Headline => "square root function",
     Usage => "sqrt x",
     Inputs => { "x" => RR => null },
     Outputs => { { "the square root of ", TT "x"} } }
document {
     Key => floor,
     Headline => "floor function",
     Usage => "floor x",
     Inputs => { "x" => RR => null },
     Outputs => { { "the largest integer less than or equal to the number ", TT "x" } } }
document {
     Key => ceiling,
     Headline => "ceiling function",
     Usage => "ceiling x",
     Inputs => { "x" => RR => null },
     Outputs => { { "the largest integer greater than or equal to the number ", TT "x" } } }
document {
     Key => run,
     Headline => "run an external command", 
     Usage => "run s",
     Inputs => { "s" => String => {"a command understandable by your operating system"} },
     Outputs => { "the exit status of the command (a small integer which is normally zero)" } }
document {
     Key => wait,
     Headline => "wait for child process", 
     TT "wait i", " waits for the completion of child process with process id
     ", TT "i", ".",
     BR,NOINDENT,
     TT "wait f", " waits for the input file to have some input ready.",
     BR,NOINDENT,
     TT "wait s", " waits for at least one of the files in the list 
     ", TT "s", " of input files to be ready, and return the list of positions
     corresponding to ready files."
     }

document {
     Key => value,
     Headline => "evaluate"
     }

document {
     Key => (value,Symbol),
     Headline => "retrieve the value of a symbol",
     Usage => "value s",
     Inputs => { "s" => null },
     Outputs => { {"the value of ", TT "s" } },
     EXAMPLE {
	  "x = s",
	  "s = 11111111111",
      	  "x",
      	  "value x"
	  }
     }

document {
     Key => (value,String),
     Headline => "evaluate a string",
     Usage => "value s",
     Inputs => { "s" => null },
     Outputs => { {"the value obtained by evaluating the code in ", TT "s" } },
     "The contents of ", TT "s", " are treated as code in the
     Macaulay 2 language, parsed it in its own scope (the same way a file is)
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
	  }
     }

document {
     Key => (value,Expression),
     Headline => "evaluate an expression",
     Usage => "value s",
     Inputs => { "s" => null },
     Outputs => { "x" => {"the value of ", TT "s" } },
     EXAMPLE {
	  "p = (expression 2)^3 * (expression 3)^2",
      	  "value p"
	  }
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
     PARA,
     {EXAMPLE {
	       "num",
	       "num = 5",
	       "num",
	       "symbol num",
	       },
	  PARA,
	  "If ", TT "s", " is an operator, then the corresponding symbol is provided.  This
	  symbol is used by the interpreter in constructing keys for methods
	  associated to the symbol.",
	  EXAMPLE "symbol +"
	  },
     SeeAlso => {"local", "global", "value" }
     }


document {
     Key => gcd,
     Headline => "greatest common divisor",
	Usage => "gcd(x,y,...)",
	Inputs => {
		"x" => ZZ => {}
		},
	Outputs => {
		ZZ => {}
		},
     TT "gcd(x,y,...)", " yields the greatest common divisor of ", TT "x", ", ", TT "y", ", ... .",
	EXAMPLE {
		gcd(2,4,8)
		},
     SeeAlso => "gcdCoefficients"
     }

document {
     Key => concatenate,
     Headline => "join strings",
     TT "concatenate(s,t,...,u)", " yields the concatenation of the strings s,t,...,u.",
     PARA,
     "The arguments may also be lists or sequences of strings and symbols, in
     which case they are concatenated recursively.  Additionally,
     an integer may be used to represent a number of spaces.",
     EXAMPLE "concatenate {\"a\",(\"s\",3,\"d\"),\"f\"}",
     SeeAlso => { "String"} 
     }

document {
     Key => error,
     Headline => "deliver error message",
	Usage => "error s",
	Inputs => {
		"s" => String => "a string or a sequence which can be converted to strings"
		},
	Consequences => {
		"an error message is displayed"
		},
     TT "error s", " causes an error message s to be displayed.",
     PARA,
     "The error message ", TT "s", " (which should be a string or a sequence of
     things which can be converted to strings and concatenated) is printed.
     Execution of the code is interrupted, and control is returned
     to top level. Here is an explicit example showing how this command is used:",
	PRE///exampleCode = p -> (
		if not isPrime p	
	    	then error "expected a prime integer";
	    	if p == 2
	    	then error "expected an odd prime";
	    	lift((p+1)/2, ZZ)
	    	);///,
	"Eventually we will have a means of ensuring that the line 
     number printed out with the error message will have more 
     significance, but currently it is the location in the code of 
     the error expression itself."
     }

document {
     Key => characters,
     Headline => "get characters from a string",
     TT "characters s", " produces a list of the characters in the string s.",
     PARA,
     "The characters are represented by strings of length 1.",
     PARA,
     EXAMPLE "characters \"asdf\"",
     PARA,
     SeeAlso => "String"
     }

document {
     Key => getenv,
     Headline => "get value of environment variable",
     TT "getenv s", " yields the value associated with the string s in the 
     environment.",
     PARA,
     EXAMPLE {
	  ///getenv "HOME"///
	  }
     }

document {
     Key => "currentDirectory",
     Headline => "current working directory",
     TT "currentDirectory", " is the name of the current directory,
     together with an extra slash (or appropriate path separator)."
     }

document {
     Key => symbol "~",
     Headline => "a unary postfix operator",
     }

document {
     Key => copy,
     Headline => "copy an object",
     TT "copy x", " yields a copy of x.",
     PARA,
     "If x is an hash table, array, list or sequence, then the elements are 
     placed into a new copy. If x is a hash table, the copy is mutable if 
     and only if x is.",
     PARA,
     "It is not advisable to copy such things as modules and rings,
     for the operations which have already been installed for them will return
     values in the original object, rather than in the copy.",
     PARA,
     SeeAlso => { "newClass" }
     }

document {
     Key => mergePairs,
     Headline => "merge sorted lists of pairs",
     TT "mergePairs(x,y,f)", " merges sorted lists of pairs.",
     PARA,
     "It merges ", TT "x", " and ", TT "y", ", which should be lists 
     of pairs ", TT "(k,v)", " arranged in increasing order according
     to the key ", TT "k", ".  The result will be a list of pairs, also
     arranged in increasing order, each of which is either from ", TT "x", "
     or from ", TT "y", ", or in the case where a key ", TT "k", " occurs in
     both, with say ", TT "(k,v)", " in ", TT "x", " and ", TT "(k,w)", "
     in ", TT "y", ", then the result will contain the pair ", TT "(k,f(v,w))", ".
     Thus the function ", TT "f", " is used for combining the values when the keys
     collide.  The class of the result is taken to be the minimal common
     ancestor of the class of ", TT "x", " and the class of ", TT "y", ".",
     PARA,
     SeeAlso => { "merge" }
     }

document {
     Key => merge,
     Headline => "merge hash tables",
     Usage => "z = merge(x,y,g)",
     Inputs => {
	  "x" => {"a hash table"},
	  "y" => {"a hash table"},
	  "g" => {"a function of two variables to be used to combine a value of ", TT "x", " with a value of ", TT "y", " when the 
	       corresponding keys coincide"
	       } },
     Outputs => {
	  "z" => { "a new hash table whose keys are the keys occurring in ", TT "x", "
	       or in ", TT "y", "; the same values are used, except that if if a key ", TT "k", " occurs in both arguments, then
	       ", TT "g(x#k,y#k)", " is used instead." } },
     "If ", TT "x", " and ", TT "y", " have the same class and parent, then so will ", TT "z", ".",
     PARA "This function is useful for multiplying monomials or adding polynomials.",
     SeeAlso => {"combine"}
     }

document {
     Key => combine,
     Headline => "combine hash tables",
     Usage => "z = combine(x,y,f,g,h)",
     Inputs => {
	  "x" => "a hash table",
	  "y" => {"a hash table of the same class as ", TT "x"},
	  "f" => { "a function of two variables to be used for combining a key
	       of ", TT "x", " with a key of ", TT "y", " to make a new key
	       for ", TT "z", "." },
	  "g" => { "a function of two variables to be used for combining a value
	       of ", TT "x", " with a value of ", TT "y", " to make a new value
	       for ", TT "z", "." },
	  "h" => { "a function of two variables to be used for combining two
	       values returned by ", TT "g", " when the corresponding keys
	       returned by ", TT "f", " turn out to be equal.  Its first argument
	       will be the value accumulated so far, and its second argument will
	       be a value just provided by ", TT "g", "."
	       }
	  },
     Outputs => {
	  "z" => {
	       "a new hash table, of the same class as ", TT "x", " and ", TT "y", ",
	       containing the pair ", TT "f(p,q) => g(b,c)", "
	       whenever ", TT "x", " contains the pair ", TT "p => b", "
	       and ", TT "y", " contains the pair ", TT "q => c", ",
	       except that ", TT "h", " is used to combine values when two keys
	       coincide."
	       }
	  },
     "The function ", TT "f", " is applied to every pair ", TT "(p,q)", "
     where ", TT "p", " is a key of ", TT "x", " and ", TT "q", " is a
     key of ", TT "y", ".  The number of times ", TT "f", " is evaluated is thus 
     the product of the number of keys in ", TT "x", " and the number of 
     keys in ", TT "y", ".",
     PARA,
     "The function ", TT "h", " should be an associative function, for otherwise 
     the result may depend on internal details about the implementation of hash 
     tables that affect the order in which entries are encountered.  If ", TT "f", ",
     ", TT "g", ", and ", TT "h", " are commutative functions as well, then the 
     result ", TT "z", " is a commutative function of ", TT "x", " and ", TT "y", ".",
     PARA,
     "The result is mutable if and only if ", TT "x", " or ", TT "y", " is.",
     PARA,
     "This function can be used for multiplying polynomials, where it
     can be used in code something like this:", 
     PRE "     combine(x, y, monomialTimes, coeffTimes, coeffPlus)"
     }

document {
     Key => ancestor,
     Headline => "whether one type is an ancestor of another",
     Usage => "ancestor(x,y)",
     Inputs => { "x" => Type => null, "y" => Type => null },
     Outputs => { {"whether ", TT "y", " is an ancestor of ", TT "x"} },
     "The ancestors of ", TT "x", " are ", TT "x", ", ", TT "parent x", ", ", TT "parent parent x", ", and so on.",
     SeeAlso => "classes and types"
     }

document {
     Key => unique,
     Headline => "eliminate duplicates from a list",
     TT "unique v", " yields the elements of the list ", TT "v", ", without duplicates.",
     PARA,
     EXAMPLE {
	  "unique {3,2,1,3,2,4,a,3,2,3,-2,1,2,4}"
	  },
     "The order of elements is maintained.  For something that might be slightly 
     faster, but doesn't maintain the order of the elements, and may different
     answers, try making a set and then getting its elements.",
     EXAMPLE {
	  "toList set {3,2,1,3,2,4,a,3,2,3,-2,1,2,4}"
	  },
     SeeAlso => {"sort"}
     }

document {
     Key => Ring,
     Headline => "the class of all rings",
     SeeAlso => "rings",
     "Common ways to make a ring:",
     UL {
	  TO (symbol /, Ring, Ideal),
	  TO (symbol " ", Ring, Array),
	  TO "GF",
	  },
     "Common functions for accessing the variables or elements in a ring:",
     UL {
	  TO (use, Ring),
	  TO (generators, Ring),
	  TO (numgens, Ring),
	  TO (symbol _, Ring, ZZ),
	  TO (symbol _, ZZ, Ring)
	  },
     "Common ways to get information about a ring:",
     UL {
	  TO (char, Ring),
	  TO (coefficientRing, Ring),
	  TO (dim, Ring),
	  },
     "Common ways to use a ring:",
     UL {
	  TO (symbol ^, Ring, ZZ),
	  TO (symbol ^, Ring, List),
	  TO (vars, Ring),
	  },
     }

document {
     Key => (symbol _, ZZ, Ring),
     TT "1_R", " provides the unit element of the ring ", TT "R", ".",
     BR, NOINDENT,
     TT "0_R", " provides the zero element of the ring ", TT "R", ".",
     BR, NOINDENT,
     TT "n_R", " promotes the integer ", TT "n", " to the ring ", TT "R", ".",
     }

document {
     Key => applyPairs,
     Headline => "apply a function to pairs in a hash table",
     TT "applyPairs(x,f)", " applies ", TT "f", " to each pair ", TT "(k,v)", " in the 
     hash table ", TT "x", " to produce a new hash table.",
     PARA,
     "It produces a new hash table ", TT "y", " from a hash table ", TT "x", " 
     by applying the function ", TT "f", " to the pair ", TT "(k,v)", " for 
     each key ", TT "k", ", where ", TT "v", " is the value stored in
     ", TT "x", " as ", TT "x#k", ".  Thus ", TT "f", " should be a function of 
     two variables which returns either a pair ", TT "(kk,vv)", " which is 
     placed into ", TT "y", ", or it returns ", TO "null", ", which 
     signifies that no action be performed.",
     PARA,
     "It is an error for the function ", TT "f", " to return two pairs with the 
     same key.",
     PARA,
     "In this example, we show how to produce the hash table corresponding
     to the inverse of a function.",
     EXAMPLE {
	  "x = new HashTable from {1 => a, 2 => b, 3 => c}",
	  "y = applyPairs(x, (k,v) -> (v,k))",
	  "x#2",
	  "y#b",
	  },
     SeeAlso => { "applyValues", "applyKeys", "scanPairs"
     }
     }

document {
     Key => applyKeys,
     Headline => "apply a function to each key in a hash table",
     TT "applyKeys(x,f)", " applies ", TT "f", " to each key ", TT "k", " in the 
     hash table ", TT "x", " to produce a new hash table.",
     PARA,
     "Thus ", TT "f", " should be a function of one variable ", TT "k", " which 
     returns a new key ", TT "k'", " for the value ", TT "v", " in ", TT "y", ".",
     PARA,
     "It is an error for the function ", TT "f", " to return the same key twice.",
     EXAMPLE {
	  "x = new HashTable from {1 => a, 2 => b, 3 => c}",
	  "applyKeys(x, k -> k + 100)",
	  },
     PARA,
     SeeAlso => {"applyValues","applyPairs"
     }
     }

document {
     Key => applyValues,
     Headline => "apply a function to each value",
     TT "applyValues(x,f)", " applies ", TT "f", " to each value ", TT "v", " 
     in the hash table ", TT "x", " to produce a new hash table.",
     PARA,
     "Thus ", TT "f", " should be a function of one variable ", TT "v", " which 
     returns a new value ", TT "v'", " for the key ", TT "k", " in the resulting hash
     table.",
     EXAMPLE {
	  "x = new HashTable from {a => 1, b => 2, c => 3}",
	  "applyValues(x, v -> v + 100)",
	  },
     PARA,
     SeeAlso => {"applyPairs","applyKeys"
     }
     }

document {
     Key => use,
     Headline => "install defaults",
     TT "use S", " installs certain defaults associated with S.",
     PARA,
     "This will install functions or methods which make the use 
     of S the default in certain contexts.  For example, if ", TT "S", " is
     a polynomial ring on the variable ", TT "x", ", then it will set the
     value of the symbol ", TT "x", " to be the corresponding element of
     the ring ", TT "S", ".",
     PARA,
     "Here is another example.  If S is a monoid ring, then the product of an
     element of the base ring of S and an element of the base monoid of S
     will be taken to be an element of S, provided ", TT "use S", " has been
     executed.",
     PARA,
     "The return value is S.",
     PARA,
     "When a ring is assigned to a global variable, this function is
     automatically called for it.",
     SeeAlso => "GlobalAssignHook"
     }

document {
     Key => symbol "=>",
     Headline => "produce an Option",
     TT "x => y", " a binary operator which produces a type of list called
     an ", TO "Option", "."
     }

document {
     Key => (symbol " ", RingElement, Array),
     Headline => "substitution of variables",
     Usage => "f[a,b,c]",
     Inputs => {
	  "f" => null,
	  "[a,b,c]" => { "an array of ring elements" } },
     Outputs => {
	  "r" => { "the result of replacing the variables in ", TT "f", " by the ring elements provided in brackets." } } ,
     EXAMPLE {
	  "R = QQ[x,y];",
	  "f = x^3 + 99*y;",
	  "f[1000,3]"
	  }
     }
     
document {
     Key => (symbol _, Symbol, Ring),
     Headline => "generator of a ring with a given name",
     Usage => "x_R",
     Inputs => {
	  "x" => null,
	  "R" => null },
     Outputs => { { "the generator of the ring ", TT "R", " whose name is ", TT "x" } }
     }
     
document {
     Key => (symbol _, RingElement, Ring),
     Headline => "generator of a ring with a given name",
     Usage => "x_R",
     Inputs => {
	  "x" => null,
	  "R" => null },
     Outputs => { { "the generator of the ring ", TT "R", " whose name is the same as that of ", TT "x" } }
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
