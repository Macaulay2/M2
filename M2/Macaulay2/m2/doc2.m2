--		Copyright 1993-1998 by Daniel R. Grayson

document { quote "timing",
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

document { quote "time",
     TT "time e", " -- evaluates e, prints the amount of cpu time
     used, and returns the value of e.",
     PARA,
     EXAMPLE "time 3^30",
     SEEALSO "timing"
     }

document { quote Time,
     TT "Time", " -- is the class of all timing results.  Each timing result
     is a ", TO "BasicList", " of the form ", TT "{t,v}", ", where ", TT "t", " 
     is the number of seconds of cpu time used, and ", TT "v", " is the value 
     of the the expression.",
     SEEALSO "timing"
     }

document { quote null,
     TT "null", " -- a symbol that represents the presence of nothing.",
     PARA,
     "When it is the value of an expression entered into the interpreter, the
     output line doesn't appear.  Empty spots in a list are represented by
     it.",
     PARA,
     "It is the only member of the class ", TO "Nothing", ".",
     PARA,
     EXAMPLE {
	  "x = {2,3,,4}",
      	  "x#2",
      	  "name x#2",
	  },
     SEEALSO { "Nothing" }
     }

document { quote "then",
     TT "then", " -- a keyword used with ", TO "if", "."
     }

document { quote "else",
     TT "else", " -- a keyword used with ", TO "if", "."
     }

document { quote "if",
     TT "if p then x else y", " -- computes ", TT "p", ", which must yield the value ", TO "true", " 
     or ", TO "false", ".  If true, then the value of ", TT "x", " is provided,
     else the value of ", TT "y", " is provided.",
     PARA,
     TT "if p then x", " --  computes ", TT "p", ", which must yield the value ", TO "true", " 
     or ", TO "false", ".  If true, then the value of ", TT "x", " is provided,
     else the symbol ", TO "null", " is provided.",
     PARA,
     SEEALSO {"then", "else"}
     }

document { quote "while",
     TT "while p do x", " -- repeatedly evaluates ", TT "x", " as long 
     as the value of ", TT "p", " remains ", TO "true", ".",
     PARA,
     "The value of the whole expression is always ", TT "null", ".",
     EXAMPLE {
	  ///i = 1; while i < 50000 do (<< i << " "; i = 2*i); << endl///
	  },
     SEEALSO "do"
     }

document { quote "do",
     TT "do", " -- a keyword used with ", TO "while", "."
     }

document { quote "try",
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

document { quote openFiles,
     TT "openFiles()", " -- produces a list of all currently open files.",
     PARA,
     SEEALSO { "File" }
     }

document { quote stdio,
     TT "stdio", " -- the standard input output file.",
     PARA,
     "Use this file to get input from the terminal, or to display information
     on the user's screen.  This is the file always used by ", TO "print", "
     and used ", TO "<<", " if it is not explicitly given a file.",
     PARA,
     SEEALSO { "File" }
     }

document { quote stderr,
     TT "stderr", " -- the standard error output file.",
     PARA,
     "Use this file to display error messages on the user's screen.",
     PARA,
     SEEALSO { "File" }
     }

document { quote openListener,
     TT "f = openListener \"$:service\"", "  -- opens a listener on the local
     host at the specified service port.",
     BR,NOINDENT,
     TT "f = openListener \"$\"", "  -- opens a listener on the local
     host at the Macaulay2 port (2500).",
     PARA,
     "Use ", TT "openInOut f", " to accept an incoming connection on the listener,
     returning a new input output file which serves as the connection.",
     RETURNS "File"
     }

document { quote openIn,
     TT "openIn \"fff\"", "  -- opens an input file whose filename is ", TT "fff", ".",
     PARA,
     "Other options are available.  For details, see ", TO "openInOut", ".",
     RETURNS "File"
     }

document { quote openOut,
     TT "openOut \"fff\"", "  -- opens an output file whose filename is fff.",
     PARA,
     "Other options are available.  For details, see ", TO "openInOut", ".",
     RETURNS "File"
     }

document { quote openInOut,
     TT "openInOut \"fff\"", "  -- opens an input output file whose 
     filename is ", TT "fff", ".",
     BR,NOINDENT,
     TT "openInOut \"!cmd\"", " -- opens an input file which corresponds to a pipe 
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
     direction specified.",
     RETURNS "File"
     }

document { quote protect,
     TT "protect s", " -- Protects the symbol s from having its value changed.",
     PARA,
     "There is no unprotect function, because we want to allow the compiler
     to take advantage of the unchangeability.",
     PARA,
     "The documentation function ", TO "document", " protects the symbols
     it documents."
     }

document { quote isInputFile,
     TT "isInputFile f", " -- whether ", TT "f", " is an input file.",
     PARA,
     "The return value is ", TO "true", " or ", TO "false", "."
     }

document { quote isOutputFile,
     TT "isOutputFile f", " -- whether ", TT "f", " is an output file.",
     PARA,
     "The return value is ", TO "true", " or ", TO "false", "."
     }

document { quote isOpenFile,
     TT "isOpenFile f", " -- whether ", TT "f", " is an open file.",
     PARA,
     "An open file is either an input file, an output file, an
     input output file, or a listener.",
     PARA,
     "The return value is ", TO "true", " or ", TO "false", "."
     }

document { quote isListener,
     TT "isListener f", " -- whether ", TT "f", " is a listener.",
     PARA,
     "The return value is ", TO "true", " or ", TO "false", "."
     }

document { quote <<,
     TT "x << y", " -- a binary or unary operator used for file output.",
     PARA,
     "The user may install ", TO {"binary method", "s"}, " for this operator 
     with code such as ",
     PRE "         X << Y := (x,y) -> ...",
     "where ", TT "X", " is the class of ", TT "x", " and ", TT "Y", " is the
     class of ", TT "y", ".",
     MENU {
	  TO (quote <<,File,Thing),
	  TO (quote <<,Nothing,Thing),
	  TO (quote <<,Thing),
	  TO (quote <<, String, Thing)
	  }
     }

document { (quote <<,ZZ, ZZ),
     TT "i << j", " -- shifts the bits in the integer i leftward j places.",
     PARA,
     EXAMPLE "2 << 5",
     SEEALSO ">>"
     }

document { (quote >>, ZZ, ZZ),
     TT "i >> j", " -- shifts the bits in the integer i rightward j places.",
     PARA,
     EXAMPLE "256 >> 5",
     SEEALSO "<<"
     }

document { (quote <<, String, Thing),
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

document { (quote <<, Thing),
     TT "<< x", " -- prints the expression x on the standard output file ", 
     TO "stdio", ".",
     PARA,
     EXAMPLE "<< \"abcdefghij\" << endl",
     SEEALSO {"<<"}
     }

document { quote >>,
     TT "i >> j", " -- shifts the bits in the integer i rightward j places."
     }

document { quote :,
     TT "x : y", " -- a binary operator.",
     PARA,
     "The user may install ", TO {"binary method", "s"}, " for this operator 
     with code such as ",
     PRE "         X : Y := (x,y) -> ...",
     "where ", TT "X", " is the class of ", TT "x", " and ", TT "Y", " is the
     class of ", TT "y", ".",
     PARA,
     MENU {
	  }
     }

document { (quote :, ZZ, Thing),
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

document { quote getc,
     TT "getc f", " -- obtains one byte from the input file f and provides it as a 
     string of length 1.  On end of file an empty string of is returned.",
     PARA,
     SEEALSO { "File" },
     PARA,
     "Bug: the name is cryptic and should be changed."
     }

document { quote <,
     TT "x < y", " -- yields ", TO "true", " or ", TO "false", 
     " depending on whether x < y.",
     PARA,
     "Calls upon ", TO "?", " to perform the comparison, if necessary."
     }

document { quote <=,
     TT "x <= y", " -- yields ", TO "true", " or ", 
     TO "false", " depending on whether x <= y.",
     PARA,
     "Calls upon ", TO "?", " to perform the comparison, if necessary."
     }

document { quote >,
     TT "x > y", " -- yields ", TO "true", " or ", 
     TO "false", " depending on whether x > y.",
     PARA,
     "Calls upon ", TO "?", " to perform the comparison, if necessary."
     }

document { quote >=,
     TT "x >= y", " -- yields ", 
     TO "true", " or ", 
     TO "false", " depending on whether x >= y.",
     PARA,
     "Calls upon ", TO "?", " to perform the comparison, if necessary."
     }

protect incomparable
document { quote incomparable,
     TT "incomparable", " -- a symbol which may be returned by ", TO "?", "
     when the two things being compared are incomparable."
     }

document { quote ?,
     TT "x ? y", " -- compares x and y, returning ", TT "quote <", ", ",
     TT "quote >", ", ", TT "quote ==", ", or ", TO "incomparable", ".",
     PARA,
     "The user may install ", TO {"binary method", "s"}, " for this operator with code
     such as ",
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

document { quote ;,
     TT "(e;f;...;g;h)", " -- the semicolon can be used for evaluating a sequence of 
     expressions.  The value of the sequence is the value of its
     last expression, unless it is omitted, in which case the value
     is ", TO "null", ".",
     EXAMPLE {
	  "(3;4;5)",
      	  "(3;4;5;)"
	  }
     }

document { quote <-,
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

document { quote =,
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


document { quote :=,
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

document { quote abs,
     TT "abs x", " -- computes the absolute value of ", TT "x", "." }
document { quote sin,
     TT "sin x", " -- computes the sine of ", TT "x", "." }
document { quote cos,
     TT "cos x", " -- computes the cosine of ", TT "x", "." }
document { quote tan,
     TT "tan x", " -- computes the tangent of ", TT "x", "." }
document { quote asin,
     TT "asin x", " -- computes the arcsine of ", TT "x", "."}
document { quote acos,
     TT "acos x", " -- computes the arccosine of ", TT "x", "."}
document { quote atan,
     TT "atan x", " -- computes the arctangent of ", TT "x", ".",
     BR,NOINDENT,
     TT "atan(x,y)", " -- computes the angle formed with the 
     x-axis by the ray from the origin ", TT "{0,0}", " to the point ", TT "{x,y}", "."
     }
document { quote sinh,
     TT "sinh x", " -- computes the hyperbolic sine of ", TT "x", "."}
document { quote cosh,
     TT "cosh x", " -- computes the hyperbolic cosine of ", TT "x", "."}
document { quote tanh,
     TT "tanh x", " -- computes the hyperbolic tangent of ", TT "x", "."}
document { quote exp,
     TT "exp x", " -- computes the exponential of ", TT "x", "."}
document { quote log,
     TT "log x", " -- computes the logarithm of ", TT "x", "."}
document { quote sqrt,
     TT "sqrt x", " -- provides the square root of the number ", TT "x", "."}
document { quote floor,
     TT "floor x", " -- provides the largest integer less than or equal to the number ", TT "x", "."
     }
document { quote ceiling,
     TT "ceiling x", " -- provides the largest integer greater than or equal to the number ", TT "x", "."
     }

document { quote run,
     TT "run s", " -- runs the command string s by passing it to the operating system.",
     PARA,
     "The return value is the exit status of the command."
     }

document { quote wait,
     TT "wait i", " -- wait for the completion of child process with process id
     ", TT "i", "."
     }

document { quote value,
     TT "value s", " -- provides the value of ", TT "s", ", which may be a
     symbol, string, or an expression.",
     PARA,
     EXAMPLE {
	  "a = 11111111111",
      	  "x = quote a",
      	  "x",
      	  "value x",
	  "p = (expression 2)^3 * (expression 3)^2",
      	  "value p"
	  },
     "If ", TT "s", " is a string, its contents are treated as code in the
     Macaulay 2 language, parsed it in its own scope, and evaluated.",
     EXAMPLE {
	  ///value "2 + 2"///,
      	  ///value "a := 2"///,
	  },
     "Since the local assignment to ", TT "a", " above occurred in a new scope,
     the value of the global variable ", TT "a", " is unaffected.",
     EXAMPLE {
      	  "a"
	  }
     }

document { quote "global",
     TT "global s", " -- provides the global symbol s, even if s currently has 
     a value.",
     PARA,
     EXAMPLE {
	  "num",
      	  "num = 5",
      	  "num",
      	  "global num",
	  },
     SEEALSO {"local", "quote"}
     }

document { quote erase,
     TT "erase s", " -- removes the global symbol ", TT "s", " from the
     symbol table."
     }

document { quote "local",
     TT "local s", " -- provides the local symbol ", TT "s", ", creating
     a new symbol if necessary.  The initial value of a local
     symbol is ", TT "null", ".",
     EXAMPLE {
	  "f = () -> ZZ[local t]",
      	  "f()",
      	  "t",
	  },
     SEEALSO {"global", "quote"}
     }

document { quote quote,
     TT "quote s", " -- provides the symbol s, even if s currently has a value.",
     PARA,
     EXAMPLE {
	  "num",
      	  "num = 5",
      	  "num",
      	  "quote num",
	  },
     PARA,
     "If ", TT "s", " is an operator, then the corresponding symbol is provided.  This
     symbol is used by the interpreter in constructing keys for methods
     associated to the symbol.",
     EXAMPLE "quote +",
     SEEALSO {"local", "global"}
     }

document { quote gcd,
     TT "gcd(x,y)", " -- yields the greatest common divisor of x and y.",
     SEEALSO "gcdCoefficients"
     }

document { quote concatenate,
     TT "concatenate(s,t,...,u)", " -- yields the concatenation of the strings s,t,...,u.",
     PARA,
     "The arguments may also be lists or sequences of strings and symbols, in
     which case they are concatenated recursively.  Additionally,
     an integer may be used to represent a number of spaces.",
     PARA,
     EXAMPLE "concatenate {\"a\",(\"s\",3,\"d\"),\"f\"}",
     SEEALSO { "String"} 
     }

document { quote error,
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

document { quote characters,
     TT "characters s", " -- produces a list of the characters in the string s.",
     PARA,
     "The characters are represented by strings of length 1.",
     PARA,
     EXAMPLE "characters \"asdf\"",
     PARA,
     SEEALSO "String"
     }

document { quote getenv,
     TT "getenv s", " -- yields the value associated with the string s in the 
     environment.",
     PARA,
     EXAMPLE {
	  ///getenv "HOME"///
	  }
     }

document { quote currentDirectory,
     TT "currentDirectory()", " -- returns the name of the current directory."
     }

document { quote ~,
     TT "~ x", " -- a postfix unary operator available to the user.  A method may
     be installed with code such as ", 
     PRE "           X ~ := x -> ... ",
     PARA,
     NOINDENT,
     "Here are the methods associated with this operator:",
     MENU {
	  TO (quote ~, Module),
	  TO (quote ~, Ring),
     	  }
     }

document { quote copy,
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

document { quote mergePairs,
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

document { quote merge,
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

document { quote combine,
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

document { quote ancestor,
     TT "ancestor(x,y)", " -- tells whether y is an ancestor of x.",
     PARA,
     "The ancestors of x are x, parent x, parent parent x, and so on.",
     PARA,
     SEEALSO "classes"
     }

document { quote unique,
     TT "unique v", " -- yields the elements of the list v, without duplicates.",
     PARA,
     EXAMPLE {
	  "unique {3,2,1,3,2,4,3,2,3,-2,1,2,4}"
	  },
     "The order of elements is not necessarily maintained.",
     PARA,
     SEEALSO {"sort"}
     }

document { quote Ring,
     TT "Ring", " -- the class of all rings.",
     PARA,
     "A ring is a set together with operations +, -, * and elements 0, 1 
     satisfying the usual rules.  In this system, it is also understood to 
     be a ZZ-algebra, which means that the operations where one argument is 
     an integer are also provided.",
     PARA,
     "Here are some classes of rings.",
     MENU {
	  TO "Field",
	  TO "FractionField",
	  TO "GaloisField",
	  TO "PolynomialRing",
	  TO "ProductRing",
	  TO "QuotientRing",
	  TO "SchurRing"
	  },
     "Here are some particular rings:",
     MENU {
	  TO "ZZ",
	  TO "QQ"
	  },
     "Tests:",
     MENU {
	  TO "isAffineRing",
	  TO "isCommutative",
	  TO "isField",
	  TO "isPolynomialRing",
	  TO "isQuotientOf",
	  TO "isQuotientRing",
	  TO "isRing"
	  },
     "Here are some functions:",
     MENU {
	  {TO (quote _, ZZ, Ring), " -- get integer elements of a ring."},
	  (TO (quote _,Ring,ZZ), " -- get a generator of a ring."),
	  (TO (quote _,Ring,String), " -- getting generators by name"),
	  (TO (quote _,Ring,List), " -- getting monomials with given exponents"),
	  TO "char",
	  TO "coefficientRing",
	  TO "lift",
	  TO "map",
	  TO "promote",
	  TO "ring"
	  },
     "Ways to create new rings:",
     MENU {
	  (TO (quote **,Ring,Ring), " -- tensor product of rings"),
	  (TO (quote " ", Ring, OrderedMonoid), " -- monoid ring"),
	  (TO "symmetricAlgebra", " -- symmetric algebra")
	  },
     "Here are some keys used in rings:",
     MENU {
	  TO "baseRings",
	  TO "Engine",
	  TO "modulus"
	  }
     }

document { (quote _, ZZ, Ring),
     TT "1_R", " -- provides the unit element of a ring R.",
     BR, NOINDENT,
     TT "0_R", " -- provides the zero element of a ring R."
     }


document { quote SymbolTable,
     TT "SymbolTable", " -- the class of all symbol tables.",
     PARA,
     "In a symbol table, each key is string containing the name of 
     a symbol, and the corresponding value is the symbol itself.",
     PARA,
     SEEALSO "Symbol"
     }

document { quote symbolTable,
     TT "symbolTable()", " -- constructs an hash table containing the 
     global symbol table.",
     PARA,
     "Each key is a string containing the name of a symbol, and the 
     corresponding value is the symbol itself.",
     PARA,
     SEEALSO "SymbolTable"
     }

document { quote applyPairs,
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

document { quote applyKeys,
     TT "applyKeys(x,f)", " -- applies ", TT "f", " to each key ", TT "k", " in the 
     hash table ", TT "x", " to produce a new hash table.",
     PARA,
     "Thus ", TT "f", " should be a function of one variable ", TT "k", " which 
     returns a new key ", TT "k'", " for the value ", TT "v", " in ", TT "y", ".",
     EXAMPLE {
	  "x = new HashTable from {1 => a, 2 => b, 3 => c}",
	  "applyKeys(x, k -> k + 100)",
	  },
     PARA,
     SEEALSO {"applyValues","applyPairs"}
     }

document { quote applyValues,
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

document { quote use,
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

document { "reading the documentation",
     "The documentation for Macaulay 2 is available in several formats.
     The directory ", TT "Macaulay2/html", " contains the documentation in html
     form, suitable for viewing with a web browser such as Netscape, and this
     is the best way to view it.",
     PARA,
     "The directory ", TT "Macaulay2/book", " contains the code for producing
     the documentation in TeX form, which can be printed with ", TT "dvips", " 
     or viewed on your screen with ", TT "xdvi", ".  The hypertext links 
     in the book can be followed if you get ", TT "xdvi", " version 20a
     or later.  (If your old ", TT "xdvi", " complains about the unknown special
     commands that implement hypertext links, you can give it the option 
     ", TT "-hushspecials", " to silence it.)  The source code for ", TT "xdvi", " 
     can be obtained at one of the CTAN mirror sites in the directory
     ", TT "dviware/xdvik", ".  Obtain a list of CTAN mirror sites by
     fingering ", TT "ctan@ftp.tex.ac.uk", ".",
     PARA,
     "Finally, all the documentation can be viewed within the program in
     text form using ", TO "help", "."
     }

document { "Macaulay 2",
     IMG "9planets.gif", PARA,
     "Macaulay 2 is a software system devoted to supporting research in 
     algebraic geometry and commutative algebra.  The current version is 
     ", version#"VERSION", ".  The program is still under development, but
     most of the main features are working.  We are eager to help new users
     get started with it.",
     MENU {
     	  {
     	       H2 "Preface",
	       MENU {
		    TO "how to get this program",
		    TO "resources required",
		    TO "reading the documentation",
		    TO "copyright and license",
		    TO "acknowledgements",
		    TO "the authors",
		    }
	       },
	  {
	       H2 "User's Guide",
	       "Here are the basic concepts needed to use Macaulay 2 effectively.",
	       MENU {
		    TO "getting started",
		    TO "mathematical overview",
		    TO "language and programming overview",
		    },
	       },
	  {
	       H2 "Mathematical Vignettes",
	       "In this section we present some tutorials which aim to introduce
	       the user to some mathematical ways of using Macaulay 2.  The tutorials
	       are relatively independent of each other, and each one introduces the use
	       of some features of Macaulay 2 in a slow and leisurely way, assuming the
	       reader is already familiar with the mathematical concepts involved.  
	       ", SHIELD TO "David Eisenbud", " joins us as a co-author of these tutorials.",
	       MENU {
		    TO "Elementary uses of Groebner bases",
		    TO "Canonical Embeddings of Plane Curves and Gonality",
		    TO "Fano varieties",
		    TO "Divisors",
		    }
	       },
     	  { 
	       H2 "Reference Manual",
	       "This section is intended to offer detailed documentation on
	       every aspect of the system of interest to users.",
	       MENU {
		    TO "invoking the program",
		    TO "classes",
		    TO "operators",
		    TO "Thing", 
		    TO "programming",
		    TO "mathematics", 
		    TO "executing other programs",
		    TO "debugging",
		    TO "system",
	       	    TO "help functions",
		    TO "syntax",
		    TO "replacements for commands and scripts from Macaulay",
		    TO "obsolete functions and symbols",
		    },
     	       },
	  {
	       H2 "Developer's Corner",
	       MENU {
	       	    TO "engine",
		    TO "internals",
	       	    }
	       },
	  }
     }

document { "internals",
     "Here are some functions and classes that are intended for internal use 
     by the developers only.",
     MENU {
	  TO "formatDocumentTag",
	  }
     }

load "tutorials.m2"

document { "acknowledgements",
     "We thank the National Science Foundation for generous funding since
     1993 for this project, Gert-Martin Greuel and Ruediger Stobbe for the
     incorporation of their ", TO "Factory library", ", Michael Messollen for
     the incorporation of his ", TO "Factorization and characteristic sets library", ",
     and David Eisenbud, Wolfram Decker and Sorin Popescu for
     early support, encouragement and suggestions.  We also acknowledge an
     intellectual debt to David Bayer, who, with Michael Stillman,
     wrote Macaulay, a specialized computer algebra system for algebraic
     geometry and the predecessor of this program."
     }

document { "copyright and license",
     "Macaulay 2, its object code and source code, and its documentation,
     are copyright by Daniel R. Grayson and Michael E. Stillman.  We 
     permit you to make copies under the following conditions.",
     PARA,
     -- this paragraph has to be duplicated in licenses/README
     "Provided you are a person (and not a corporate entity), you may make as
     many copies of Macaulay 2 as you like for your personal non-commercial
     use.  You may install copies of Macaulay 2 on computers owned by
     Universities, Colleges, High Schools, and other schools in such a way
     that students and staff of those institutions may use it.  You may
     modify and distribute the source code in the Macaulay 2 language we
     provide you, but you must retain our copyright notices and mark modified
     source code so others will know that it's been modified.  You may print
     out the manual and make copies of it for your personal use.",
     PARA,
     "If your intended use of Macaulay 2 is not covered by the license above,
     please contact us so we can work something out.  Notice that in the
     license above we have not granted you permission to make copies of
     Macaulay 2 to be sold, distributed on media which are sold, or
     distributed along with software which is sold.  We have not granted you
     permission to make derivative works, or to distribute them.  If you
     encounter a copy which appears not to conform to the terms of the
     license above, we would like to hear about it.",
     PARA,
     "Various libraries have been compiled into Macaulay 2.",
     MENU {
	  SHIELD TO "Factory library",
	  SHIELD TO "Factorization and characteristic sets library",
	  SHIELD TO "MP: Multi Protocol",
	  SHIELD TO "GNU MP",
	  SHIELD TO "GC garbage collector"
	  }
     }

document { "GC garbage collector",
     "Macaulay 2 uses the excellent garbage collector 'GC' written by Hans-J. Boehm
     and Alan J. Demers and generously provided to the publinc.  The copyright is 
     contained in its README file which we provide in the 
     file ", TT "Macaulay2/licenses/gc.lic", ".",
     SEEALSO {"collectGarbage", "gcDump"}
     }

document { "Factory library",
     "With the kind permission of the authors of Singular, G.-M. Greuel,
     G. Pfister, H. Schoenemann and R. Stobbe, University of Kaiserslautern,
     Macaulay 2 incorporates 'Factory', a Singular library of polynomial
     routines which provides for factorization of polynomials. That library
     is copyright 1996 by Gert-Martin Greuel and Ruediger Stobbe.  We provide
     a copy of the license in the file ", TT "Macaulay2/licenses/factory.lic", ".",
     SEEALSO {"factor", "gcd"}
     }

document { "Factorization and characteristic sets library",
     "With the kind permission of the author, Michael Messollen, University
     of Saarbruecken, Macaulay 2 incorporates a library of routines which
     provides factorization of multivariate polynomials over finite fields
     and computation of the minimal associated primes of ideals via
     characteristic sets.  That library is copyright 1996 by Michael
     Messollen.  We provide a copy of the license in the file
     ", TT "Macaulay2/licenses/libfac.lic", ".",
     SEEALSO {"factor", "gcd", "decompose", "irreducibleCharacteristicSeries"}
     }

document { "GNU MP",
     "The GNU MP library provides routines for arbitrary precision
     integer and floating point arithmetic.  Version 2.0 of the library
     is provided to us under the GNU LIBRARY GENERAL PUBLIC LICENSE,
     a copy of which is provided to you as part of the Macaulay 2
     package in the file ", TT "Macaulay2/licenses/gnulib.lic", ".  
     Macaulay 2 contains no derivative of GNU MP, and works with it by 
     being linked with it, and hence the Macaulay2 executable is covered 
     by section 6 of the GNU license.  We fulfill the terms of its license 
     by offering you the source code of the program, available at our
     web site and our anonymous ftp site.",
     SEEALSO "how to get this program"
     }

document { "operators",
     "Here is a list of unary and binary operators in the language.  Many
     of them can have methods installed for handling arguments of specific
     types.",
     MENU {
          (TO quote " ", " -- function application"),
          (TO ",", " -- separates elements of lists or sequences"),
          (TO ";", " -- statement separator"),
          (TO "=", " -- assignment"),
          (TO "<-", " -- assignment with left hand side evaluated"),
          (TO ":=", " -- assignment of method or new local variable"),
          (TO "==", " -- equal"),
          (TO "!=", " -- not equal"),
          (TO "===", " -- strictly equal"),
          (TO "=!=", " -- strictly not equal"),
          (TO "<", " -- less than"),
          (TO "<=", " -- less than or equal"),
          (TO "=>", " -- option"),
          (TO ">", " -- greater than"),
          (TO ">=", " -- greater than or equal"),
          (TO "?", " -- comparison"),
	  (TO "or", " -- or"),
	  (TO "and", " -- and"),
          (TO "not", " -- negation"),
          (TO "..", " -- sequence builder"),
          (TO "+", " -- addition"),
          (TO "-", " -- subtraction"),
          (TO "*", " -- multiplication"),
          (TO "/", " -- division"),
          (TO "//", " -- quotient"),
          (TO "\\\\", " -- left quotient"),
          (TO "%", " -- remainder"),
          (TO "^", " -- power"),
          (TO "/^", " -- divided power"),
          (TO "!", " -- factorial"),
          (TO "++", " -- direct sum"),
          (TO "**", " -- tensor product"),
          (TO "<<", " -- file output, bit shifting"),
          (TO ">>", " -- bit shifting"),
          (TO "_", " -- subscripting"),
          (TO ".", " -- hash table access or assignment"),
          (TO ".?", " -- test for hash table access"),
          (TO "#", " -- hash table access; length of a list, sequence or hash table"),
          (TO "#?", " -- test for hash table access"),
          (TO "|", " -- horizontal concatenation of strings or matrices"),
          (TO "||", " -- vertical concatentation of strings or matrices"),
          (TO "&", " -- bit-wise and"),
          (TO ":", " -- ideal quotient, repetitions"),
          (TO quote "\\", " -- applying a function to elements of a list"),
          (TO "/", " -- applying a function to elements of a list"),
          (TO "@", " -- attaching options to a function"),
          (TO "@@", " -- composing functions"),
          -- (TO "::", " -- "),
          (TO "&&", " -- "),
          (TO "^^", " -- "),
          (TO "~", " -- ")
     	  }
     }

document { quote =>,
     TT "x => y", " -- a binary operator.",
     PARA,
     "The user may install ", TO {"binary method", "s"}, " for this operator 
     with code such as ",
     PRE "         X => Y := (x,y) -> ...",
     "where ", TT "X", " is the class of ", TT "x", " and ", TT "Y", " is the
     class of ", TT "y", ".",
     MENU {
	  TO (quote =>, Thing, Thing)
	  }
     }

document { "invoking the program",
     TT "M2", " -- starts the program.",
     PARA,
     TT "M2 file1 file2 ... ", " -- starts the program, reading and 
     executing the specified files.",
     PARA,
     "These are the options that can also be provided on the command
     line.",
     MENU {
	  {TT "--", "       -- ignore previous arguments after reloading data"},
	  {TT "-e x", "     -- evaluates the expression x"},
	  {TT "-h", "       -- displays the usage message"},
	  {TT "-n", "       -- print no input prompts"},
	  {TT "-q", "       -- suppresses loading of init file 'init.m2'"},
	  {TT "-s", "       -- stops execution if an error occurs"},
	  {TT "-silent", "  -- don't print the startup banner"},
	  {TT "-tty", "     -- assume stdin is a tty"},
	  {TT "-x", "       -- special mode for running examples"},
	  },
     TT "M2", " is actually a shell script which calls the executable file
     with appropriate arguments for loading the Macaulay 2 code previously
     compiled.",
     SEEALSO "initialization file"
     }
