--		Copyright 1993-1998 by Daniel R. Grayson

document { "timing",
     TT "timing e", " -- evaluates e and returns a list of type ", TO "Time", "
     of the form ", TT "{t,v}", ", where ", TT "t", " is the number of seconds
     of cpu timing used, and ", TT "v", " is the value of the the expression.",
     PARA,
     "The default method for printing such timing results is to display the
     timing separately in a comment below the computed value.",
     EXAMPLE {
	  "timing 3^30",
      	  "peek oo",
	  },
     SEEALSO "Time"
     }

document { "time",
     TT "time e", " -- evaluates e, prints the amount of cpu time
     used, and returns the value of e.",
     PARA,
     EXAMPLE "time 3^30",
     SEEALSO "timing"
     }

document { Time,
     TT "Time", " -- is the class of all timing results.  Each timing result
     is a ", TO "BasicList", " of the form ", TT "{t,v}", ", where ", TT "t", " 
     is the number of seconds of cpu time used, and ", TT "v", " is the value 
     of the the expression.",
     SEEALSO "timing"
     }

document { null,
     HEADLINE "nothingness",
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
     PARA,
     EXAMPLE {
	  "x = {2,3,,4}",
	  "net x",
      	  "toString x#2",
      	  "peek x",
	  }
     }

document { "then",
     HEADLINE "condition testing",
     TT "then", " -- a keyword used with ", TO "if", "."
     }

document { "else",
     HEADLINE "condition testing",
     TT "else", " -- a keyword used with ", TO "if", "."
     }

document { "if",
     HEADLINE "condition testing",
     TT "if p then x else y", " -- computes ", TT "p", ", which must yield the value ", TO "true", " 
     or ", TO "false", ".  If true, then the value of ", TT "x", " is provided,
     else the value of ", TT "y", " is provided.",
     PARA,
     TT "if p then x", " --  computes ", TT "p", ", which must yield the value ", TO "true", " 
     or ", TO "false", ".  If true, then the value of ", TT "x", " is provided,
     else ", TO "null", " is provided."
     }

document { "while",
     HEADLINE "loop control",
     TT "while p do x", " -- repeatedly evaluates ", TT "x", " as long 
     as the value of ", TT "p", " remains ", TO "true", ".",
     PARA,
     "The value of the whole expression is always ", TT "null", ".",
     EXAMPLE {
	  ///i = 1; while i < 50000 do (<< i << " "; i = 2*i); << endl///
	  }
     }

document { "do",
     HEADLINE "loop control",
     TT "do", " -- a keyword used with ", TO "while", "."
     }

document { "try",
     HEADLINE "catch an error",
     TT "try x else y ", " -- returns the value of x unless an error or
     ", TO "alarm", " occurs during the evaluation of x, in which case it 
     returns the value of y.", BR, NOINDENT,
     TT "try x ", " -- returns the value of x unless an error or
     ", TO "alarm", " occurs during the evaluation of x, in which case it 
     returns ", TO "null", ".",
     PARA,
     "The behavior of interrupts (other than alarms) is unaffected.",
     EXAMPLE "apply(-3..3,i->try 1/i else infinity)",
     PARA,
     "We will change the behavior of this function soon so that it will be
     possible to catch errors of a particular type.  Meanwhile, users are
     recommended to use this function sparingly, if at all."
     }

document { openFiles,
     TT "openFiles()", " -- produces a list of all currently open files.",
     PARA,
     SEEALSO { "File" }
     }

document { stdio,
     TT "stdio", " -- the standard input output file.",
     PARA,
     "Use this file to get input from the terminal, or to display information
     on the user's screen.  This is the file always used by ", TO "print", "
     and used ", TO "<<", " if it is not explicitly given a file."
     }

document { stderr,
     TT "stderr", " -- the standard error output file.",
     PARA,
     "Use this file to display error messages on the user's screen."
     }

document { openListener,
     TT "f = openListener \"$:service\"", "  -- opens a listener on the local
     host at the specified service port.",
     BR,NOINDENT,
     TT "f = openListener \"$\"", "  -- opens a listener on the local
     host at the Macaulay2 port (2500).",
     PARA,
     "Use ", TT "openInOut f", " to accept an incoming connection on the listener,
     returning a new input output file which serves as the connection."
     }

document { openIn,
     TT "openIn \"fff\"", "  -- opens an input file whose filename is ", TT "fff", ".",
     PARA,
     "Other options are available.  For details, see ", TO "openInOut", "."
     }

document { openOut,
     TT "openOut \"fff\"", "  -- opens an output file whose filename is fff.",
     PARA,
     "Other options are available.  For details, see ", TO "openInOut", "."
     }

document { openInOut,
     TT "openInOut \"fff\"", "  -- opens an input output file whose 
     filename is ", TT "fff", ".",
     BR,NOINDENT,
     TT "openInOut \"!cmd\"", " -- opens an input output file which corresponds to a pipe 
     receiving the output from the shell command ", TT "cmd", ".",
     BR,NOINDENT,
     TT "openInOut \"$hostname:service\"", " -- opens an input output file
     by connecting to the specified service port at the specified host.",
     BR,NOINDENT,
     TT "openInOut \"$:service\"", " -- opens an input output file by
     listening to the specified service port on the local host, and 
     waiting for an incoming connection.",
     BR,NOINDENT,
     TT "openInOut \"$hostname\"", " -- opens an input output file
     by connecting to the Macaulay2 service port (2500) at the specified host.",
     BR,NOINDENT,
     TT "openInOut \"$\"", " -- opens an input output file by listening to the
     Macaulay2 service port (2500) on the local host, and waiting for an
     incoming connection.",
     BR,NOINDENT,
     TT "openInOut f", " -- opens an input output file by accepting a
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
     "The various forms listed above can be used also with ", TO "openIn", "
     and ", TO "openOut", ", with data transfer possible only in the
     direction specified.  The only subtlety is that with ", TT ///openIn "!foo"///, "
     the standard input of the command ", TT "foo", " is closed, but with
     ", TT ///openOut "!foo"///, " the standard output of the command ", TT "foo", "
     is connected to the standard output of the parent Macaulay 2 process."
     }

document { protect,
     TT "protect s", " -- Protects the symbol s from having its value changed.",
     PARA,
     "There is no unprotect function, because we want to allow the compiler
     to take advantage of the unchangeability.",
     PARA,
     "The documentation function ", TO "document", " protects the symbols
     it documents."
     }

document { isInputFile,
     HEADLINE "whether a file is open for input",
     TT "isInputFile f", " -- whether ", TT "f", " is an input file.",
     PARA,
     "The return value is ", TO "true", " or ", TO "false", "."
     }

document { isOutputFile,
     HEADLINE "whether a file is open for output",
     TT "isOutputFile f", " -- whether ", TT "f", " is an output file.",
     PARA,
     "The return value is ", TO "true", " or ", TO "false", "."
     }

document { isOpenFile,
     HEADLINE "whether a file is open",
     TT "isOpenFile f", " -- whether ", TT "f", " is an open file.",
     PARA,
     "An open file is either an input file, an output file, an
     input output file, or a listener.",
     PARA,
     "The return value is ", TO "true", " or ", TO "false", "."
     }

document { isListener,
     HEADLINE "whether a file is open for listening",
     TT "isListener f", " -- whether ", TT "f", " is a listener.",
     PARA,
     "The return value is ", TO "true", " or ", TO "false", "."
     }

document { symbol <<,
     TT "x << y", " -- a binary operator usually used for file output.", BR,
     TT "<< y", " -- a unary operator usually used for output to stdout."
     }

document { (symbol <<,ZZ, ZZ),
     TT "i << j", " -- shifts the bits in the integer i leftward j places.",
     PARA,
     EXAMPLE "2 << 5",
     SEEALSO ">>"
     }

document { (symbol >>, ZZ, ZZ),
     TT "i >> j", " -- shifts the bits in the integer i rightward j places.",
     PARA,
     EXAMPLE "256 >> 5",
     SEEALSO "<<"
     }

document { (symbol <<, String, Thing),
     TT "\"name\" << x", " -- prints the expression x on the output file
     named \"name\".",
     PARA,
     "Returns the newly created ", TO "File", " associated to the given name.
     Parsing associates leftward, so that several expressions may be displayed 
     with something like ", TT "\"name\"<<x<<y<<z", ".  It will often be convenient 
     to let the last output operation close the file, as illustrated below.",
     PARA,
     EXAMPLE {
	  "\"foo\" << 2^30 << endl << close",
      	  "get \"foo\""
	  }
     }

document { (symbol <<, Thing),
     TT "<< x", " -- prints the expression x on the standard output file ", 
     TO "stdio", ".",
     PARA,
     EXAMPLE "<< \"abcdefghij\" << endl",
     SEEALSO {"<<"}
     }

document { symbol ">>",
     TT "i >> j", " -- shifts the bits in the integer i rightward j places."
     }

document { symbol ":",
     }

document { (symbol :, ZZ, Thing),
     TT "n : x", " -- repetition n times of x",
     PARA,
     "If ", TT "n", " is an integer and ", TT "x", " is anything, return a
     sequence consisting of ", TT "x", " repeated ", TT "n", " times.  A negative 
     value for ", TT "n", " will silently be treated as zero.",
     PARA,
     "Warning: such sequences do not get automatically spliced into lists
     containing them.",
     PARA,
     EXAMPLE "{5:a,10:b}"
     }

document { getc,
     TT "getc f", " -- obtains one byte from the input file f and provides it as a 
     string of length 1.  On end of file an empty string of is returned.",
     PARA,
     SEEALSO { "File" },
     PARA,
     "Bug: the name is cryptic and should be changed."
     }

document { symbol "<",
     HEADLINE "less than",
     TT "x < y", " -- yields ", TO "true", " or ", TO "false", 
     " depending on whether x < y.",
     PARA,
     "Calls upon ", TO "?", " to perform the comparison, if necessary."
     }

document { symbol "<=",
     HEADLINE "less than or equal",
     TT "x <= y", " -- yields ", TO "true", " or ", 
     TO "false", " depending on whether x <= y.",
     PARA,
     "Calls upon ", TO "?", " to perform the comparison, if necessary."
     }

document { symbol ">",
     HEADLINE "greater than",
     TT "x > y", " -- yields ", TO "true", " or ", 
     TO "false", " depending on whether x > y.",
     PARA,
     "Calls upon ", TO "?", " to perform the comparison, if necessary."
     }

document { symbol ">=",
     HEADLINE "greater than or equal",
     TT "x >= y", " -- yields ", 
     TO "true", " or ", 
     TO "false", " depending on whether x >= y.",
     PARA,
     "Calls upon ", TO "?", " to perform the comparison, if necessary."
     }

protect incomparable
document { incomparable,
     TT "incomparable", " -- a symbol which may be returned by ", TO "?", "
     when the two things being compared are incomparable."
     }

document { symbol "?",
     TT "x ? y", " -- compares x and y, returning ", TT "symbol <", ", ",
     TT "symbol >", ", ", TT "symbol ==", ", or ", TO "incomparable", ".",
     PARA,
     "The user may install additional ", TO {"binary method", "s"}, " for this 
     operator with code such as ",
     PRE "         X ? Y := (x,y) -> ...",
     "where ", TT "X", " is the class of ", TT "x", " and ", TT "Y", " is the
     class of ", TT "y", ".",
     EXAMPLE {
	  "3 ? 4",
      	  "\"book\" ? \"boolean\"",
      	  "3 ? 3.",
      	  "3 ? \"a\"",
	  },
     "It would be nice to implement an operator like this one for everything
     in such a way that the set of all things in the language would be
     totally ordered, so that it could be used in the implementation of
     efficient hash tables, but we haven't done this.  The methods which have
     been installed for this operator are fairly primitive, and in the end
     often amount to simply comparing hash codes."  
     }

document { ";",
     HEADLINE "statement separator",
     TT "(e;f;...;g;h)", " -- the semicolon can be used for evaluating a sequence of 
     expressions.  The value of the sequence is the value of its
     last expression, unless it is omitted, in which case the value
     is ", TO "null", ".",
     EXAMPLE {
	  "(3;4;5)",
      	  "(3;4;5;)"
	  }
     }

document { symbol "<-",
     HEADLINE "assignment",
     TT "x <- y    ", " -- assigns the value of y to x, but x is evaluated, too.",
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
     SEEALSO "="
     }

document { "=",
     HEADLINE "assignment",
     TT "x = e", "      -- assigns the value e to the variable x.",
     PARA,
     NOINDENT,
     TT "x#i = e", "    -- assigns the value e to the i-th member of the array x.  Here
     i must be a nonnegative integer.",
     PARA,
     NOINDENT,
     TT "x#k = e", "    -- assigns the value e to the key k in the hash table
     x.  Here k can be any expression.",
     SEEALSO {"HashTable", ":=", "GlobalReleaseHook", "GlobalAssignHook"}
     }


document { ":=",
     HEADLINE "assignment to a new local variable",
     TT "x := e", " -- assign the value e to the new local variable x",
     BR,NOINDENT,
     TT "f X := (x) -> ( ... )", " -- install a method for the method function
     ", TT "f", " acting on an argument of class ", TT "X", ".",
     BR,NOINDENT,
     TT "X * Y := (x,y) -> ( ... )", " -- install a method for the operator
     ", TT "*", " applied to arguments of classes ", TT "X", " and ", TT "Y", ".
     Many other operators are allowed: see ", TO "operators", ".",
     PARA,
     "This operator is slightly schizophrenic in its function, as the installation
     of a method has global effect if the classes involved are globally known,
     as is typically the case, whereas the assignment of a value to a local
     variable is never globally known."
     }

document { abs, HEADLINE "absolute value function", 
     TT "abs x", " -- computes the absolute value of ", TT "x", "." }
document { sin, HEADLINE "sine function", 
     TT "sin x", " -- computes the sine of ", TT "x", "." }
document { cos, HEADLINE "cosine function", 
     TT "cos x", " -- computes the cosine of ", TT "x", "." }
document { tan, HEADLINE "tangent function",
     TT "tan x", " -- computes the tangent of ", TT "x", "." }
document { asin, HEADLINE "arcsine function", 
     TT "asin x", " -- computes the arcsine of ", TT "x", "."}
document { acos, HEADLINE "arccosine function", 
     TT "acos x", " -- computes the arccosine of ", TT "x", "."}
document { atan, HEADLINE "arctangent function", 
     TT "atan x", " -- computes the arctangent of ", TT "x", ".",
     BR,NOINDENT,
     TT "atan(x,y)", " -- computes the angle formed with the 
     x-axis by the ray from the origin ", TT "{0,0}", " to the point ", TT "{x,y}", "."
     }
document { sinh, HEADLINE "hyperbolic sine function",
     TT "sinh x", " -- computes the hyperbolic sine of ", TT "x", "."}
document { cosh, HEADLINE "hyperbolic cosine function",
     TT "cosh x", " -- computes the hyperbolic cosine of ", TT "x", "."}
document { tanh, HEADLINE "hyperbolic tangent function",
     TT "tanh x", " -- computes the hyperbolic tangent of ", TT "x", "."}
document { exp, HEADLINE "exponential function",
     TT "exp x", " -- computes the exponential of ", TT "x", "."}
document { log, HEADLINE "logarithm function",
     TT "log x", " -- computes the logarithm of ", TT "x", "."}
document { sqrt, HEADLINE "square root function",
     TT "sqrt x", " -- provides the square root of the number ", TT "x", "."}
document { floor, HEADLINE "floor function",
     TT "floor x", " -- provides the largest integer less than or equal to the number ", TT "x", "."
     }
document { ceiling, HEADLINE "ceiling function",
     TT "ceiling x", " -- provides the largest integer greater than or equal to the number ", TT "x", "."
     }

document { run, HEADLINE "run an external command", 
     TT "run s", " -- runs the command string ", TT "s", "
     by passing it to the operating system.",
     PARA,
     "The return value is the exit status of the command, a small
     integer which is usually zero."
     }

document { wait, HEADLINE "wait for child process", 
     TT "wait i", " -- wait for the completion of child process with process id
     ", TT "i", ".",
     BR,NOINDENT,
     TT "wait f", " -- wait for the input file to have some input ready.",
     BR,NOINDENT,
     TT "wait s", " -- wait for at least one of the files in the list 
     ", TT "s", " of input files to be ready, and return the list of positions
     corresponding to ready files."
     }

document { value, HEADLINE "evaluate", 
     TT "value s", " -- provides the value of ", TT "s", ", which may be a
     symbol, string, or an expression.",
     PARA,
     EXAMPLE {
	  "a = 11111111111",
      	  "x = symbol a",
      	  "x",
      	  "value x",
	  "p = (expression 2)^3 * (expression 3)^2",
      	  "value p",
	  ///value "2 + 2"///,
	  },
     "If ", TT "s", " is a string, its contents are treated as code in the
     Macaulay 2 language, parsed it in its own scope, the same way a file is,
     and evaluated.  The string may even contain multiple lines.",
     EXAMPLE {
      	  ///value "a := 2 \n a+a"///,
	  },
     "Since the local assignment to ", TT "a", " above occurred in a new scope,
     the value of the global variable ", TT "a", " is unaffected.",
     EXAMPLE {
      	  "a"
	  }
     }

document { "global", HEADLINE "get a global symbol", 
     TT "global s", " -- provides the global symbol s, even if s currently has 
     a value.",
     PARA,
     EXAMPLE {
	  "num",
      	  "num = 5",
      	  "num",
      	  "global num",
	  },
     SEEALSO {"local", "symbol"}
     }

document { erase, HEADLINE "remove a global symbol",
     TT "erase s", " -- removes the global symbol ", TT "s", " from the
     symbol table."
     }

document { "local", HEADLINE "get a local symbol",
     TT "local s", " -- provides the local symbol ", TT "s", ", creating
     a new symbol if necessary.  The initial value of a local
     symbol is ", TT "null", ".",
     EXAMPLE {
	  "f = () -> ZZ[local t]",
      	  "f()",
      	  "t",
	  },
     SEEALSO {"global", "symbol"}
     }

document { "symbol", HEADLINE "get a symbol",
     TT "symbol s", " -- provides the symbol ", TT "s", ", even if ", TT "s", " currently has a value.",
     PARA,
     EXAMPLE {
	  "num",
      	  "num = 5",
      	  "num",
      	  "symbol num",
	  },
     PARA,
     "If ", TT "s", " is an operator, then the corresponding symbol is provided.  This
     symbol is used by the interpreter in constructing keys for methods
     associated to the symbol.",
     EXAMPLE "symbol +",
     SEEALSO {"local", "global", "value"}
     }

document { gcd, HEADLINE "greatest common divisor",
     TT "gcd(x,y)", " -- yields the greatest common divisor of x and y.",
     SEEALSO "gcdCoefficients"
     }

document { concatenate, HEADLINE "join strings",
     TT "concatenate(s,t,...,u)", " -- yields the concatenation of the strings s,t,...,u.",
     PARA,
     "The arguments may also be lists or sequences of strings and symbols, in
     which case they are concatenated recursively.  Additionally,
     an integer may be used to represent a number of spaces.",
     PARA,
     EXAMPLE "concatenate {\"a\",(\"s\",3,\"d\"),\"f\"}",
     SEEALSO { "String"} 
     }

document { error, HEADLINE "deliver error message",
     TT "error s", " -- causes an error message s to be displayed.",
     PARA,
     "The error message s (which should be a string or a sequence of
     things which can be converted to strings and concatenated) is printed.
     Execution of the code is interrupted, and control is returned
     to top level.",
     PARA,
     "Eventually we will have a means of ensuring that the line 
     number printed out with the error message will have more 
     significance, but currently it is the location in the code of 
     the error expression itself."
     }

document { characters, HEADLINE "get characters from a string",
     TT "characters s", " -- produces a list of the characters in the string s.",
     PARA,
     "The characters are represented by strings of length 1.",
     PARA,
     EXAMPLE "characters \"asdf\"",
     PARA,
     SEEALSO "String"
     }

document { getenv, HEADLINE "get value of environment variable",
     TT "getenv s", " -- yields the value associated with the string s in the 
     environment.",
     PARA,
     EXAMPLE {
	  ///getenv "HOME"///
	  }
     }

document { currentDirectory, HEADLINE "current working directory",
     TT "currentDirectory()", " -- returns the name of the current directory."
     }

document { symbol "~" }

document { copy,
     TT "copy x", " -- yields a copy of x.",
     PARA,
     "If x is an hash table, array, list or sequence, then the elements are 
     placed into a new copy. If x is a hash table, the copy is mutable if 
     and only if x is.",
     PARA,
     "It is not advisable to copy such things as modules and rings,
     for the operations which have already been installed for them will return
     values in the original object, rather than in the copy.",
     PARA,
     SEEALSO { "newClass" }
     }

document { mergePairs,
     TT "mergePairs(x,y,f)", " -- merges sorted lists of pairs.",
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
     SEEALSO { "merge" }
     }

document { merge,
     TT "merge(x,y,g)", " -- merges hash tables x and y using the function
     g to combine the values when the keys collide.",
     PARA,
     "If ", TT "x", " and ", TT "y", " have the same class and parent, then 
     so will the result.  The merged hash table has as its keys the keys 
     occuring in the arguments.  When a key occurs in both arguments, the 
     corresponding values are combined using the function ", TT "g", ", which 
     should be a function of two arguments.",
     PARA,
     "This function is useful for multiplying monomials or adding polynomials.",
     PARA,
     "See also ", TO "combine", ", which is useful for multiplying polynomials."
     }

document { combine,
     TT "combine(x,y,f,g,h)", " -- yields the result of combining hash tables
     ", TT "x", " and ", TT "y", ", using ", TT "f", " to combine keys, ", TT "g", "
     for values, and ", TT "h", " for collisions.",
     PARA,
     "The objects are assumed to have the same class, and the result will
     have the class of one of them.  The combined object will contain 
     ", TT "f(p,q) => g(b,c)", " when ", TT "x", " contains ", TT "p => b", "
     and ", TT "y", " contains ", TT "q => c", ".  The function ", TT "h", " is 
     used to combine values when key collisions occur in the result, as with
     ", TO "merge", ".  The function ", TT "h", " should be a function of two 
     arguments; it may assume that its first argument will be the value accumulated 
     so far, and its second argument will be the result ", TT "g(b,c)", " from a 
     single pair of values.  Normally ", TT "h", " will be an associative and
     commutative function.",
     PARA,
     "The result is mutable if and only if ", TT "x", " or ", TT "y", " is.",
     PARA,
     "This function can be used for multiplying polynomials,
     where it will be used in code like this.", 
     PRE "     combine(x, y, monomialTimes, coeffTimes, coeffPlus)"
     }

document { ancestor,
     HEADLINE "whether one type is an ancestor of another",
     TT "ancestor(x,y)", " -- tells whether y is an ancestor of x.",
     PARA,
     "The ancestors of x are x, parent x, parent parent x, and so on.",
     PARA,
     SEEALSO "classes"
     }

document { unique,
     TT "unique v", " -- yields the elements of the list v, without duplicates.",
     PARA,
     EXAMPLE {
	  "unique {3,2,1,3,2,4,3,2,3,-2,1,2,4}"
	  },
     "The order of elements is not necessarily maintained.",
     PARA,
     SEEALSO {"sort"}
     }

document { Ring,
     HEADLINE "the class of all rings",
     "A ring is a set together with operations +, -, * and elements 0, 1 
     satisfying the usual rules.  In this system, it is also understood to 
     be a ZZ-algebra, which means that the operations where one argument is 
     an integer are also provided."
     }

document { (symbol _, ZZ, Ring),
     TT "1_R", " -- provides the unit element of a ring R.",
     BR, NOINDENT,
     TT "0_R", " -- provides the zero element of a ring R."
     }


document { SymbolTable, HEADLINE "the class of all symbol tables",
     "In a symbol table, each key is string containing the name of 
     a symbol, and the corresponding value is the symbol itself.",
     SEEALSO "Symbol"
     }

document { symbolTable,
     TT "symbolTable()", " -- constructs an hash table containing the 
     global symbol table.",
     PARA,
     "Each key is a string containing the name of a symbol, and the 
     corresponding value is the symbol itself.",
     PARA,
     SEEALSO "SymbolTable"
     }

document { applyPairs,
     TT "applyPairs(x,f)", " -- applies f to each pair (k,v) in the 
     hash table x to produce a new hash table.",
     PARA,
     "It produces a new hash table ", TT "y", " from a hash table ", TT "x", " 
     by applying the function ", TT "f", " to the pair ", TT "(k,v)", " for 
     each key ", TT "k", ", where ", TT "v", " is the value stored in
     ", TT "x", " as ", TT "x#k", ".  Thus ", TT "f", " should be a function of 
     two variables which returns either a pair ", TT "(kk,vv)", " which is 
     placed into ", TT "y", ", or it returns ", TT "null", ", which 
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
     SEEALSO { "applyValues", "applyKeys", "scanPairs"}
     }

document { applyKeys,
     TT "applyKeys(x,f)", " -- applies ", TT "f", " to each key ", TT "k", " in the 
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
     SEEALSO {"applyValues","applyPairs"}
     }

document { applyValues,
     TT "applyValues(x,f)", " -- applies ", TT "f", " to each value ", TT "v", " 
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
     SEEALSO {"applyPairs","applyKeys"}
     }

document { use,
     TT "use S", " -- installs certain defaults associated with S.",
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
     SEEALSO "GlobalAssignHook"
     }

  --  types := new MutableHashTable
  --  itemize := (sym) -> (
  --       thing := value sym;
  --       type := class thing;
  --       if not types#?type then types#type = new MutableHashTable;
  --       types#type#sym = true;
  --       )
  --  itemize \ values symbolTable()
  --  sortByName := v -> (i -> i#1) \ sort ((i -> (toString i, i)) \ v)
  --  nm := type -> "index for class " | toString type
  --  document { "index of existing objects by class", MENU apply(sortByName keys types, type -> TO nm type) }
  --  scan(sortByName keys types, type -> (
  --  	  if type === Symbol then (
  --  	       op := sym -> value sym === sym and toExternalString sym =!= toString sym;
  --  	       ops := v -> select(v,op);
  --  	       nonops := v -> select(v,i -> not op i);
  --  	       document { nm type,
  --  		    "Operators:",
  --  		    MENU ((i -> TOH i) \ toString \ sortByName    ops keys types#type),
  --  		    "Nonoperators:",
  --  		    MENU ((i -> TOH i) \ toString \ sortByName nonops keys types#type),
  --  		    }
  --  	       )
  --  	  else document { nm type,
  --  	       MENU ((i -> TOH i) \ toString \ sortByName keys types#type),
  --  	       }
  --  	  )
  --       )
  --  types = null

document { setSpin,
     TT "setSpin n", " -- sets the interval between calls to spin the cursor on
     the Macintosh.",
     PARA,
     "The value returned is the previous interval."
     }

document { symbol "=>",
     TT "x => y", " -- a binary operator which produces a type of list called
     an ", TO "Option", "."
     }

