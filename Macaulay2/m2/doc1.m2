--		Copyright 1993-1999 by Daniel R. Grayson

TEST ///
assert( null =!= documentation sin)
assert( null =!= documentation "sin")
assert( null =!= documentation symbol sin)
///

document { document,
     TT "document {s, d}", " -- install documentation ", TT "d", " for 
     the topic ", TT "s", ".",
     PARA,
     "The documentation ", TT "d", " should be ", TO "hypertext", ".  The topic
     ", TT "s", " may be one of the special forms useable with ", TO "TO", ".  As
     a convenience, lists and sequences in ", TT "d", " are converted to ", TT "SEQ", "
     mark up items, and instances of ", TO "MarkUpType", ", such as ", TO "PARA", "
     are converted to empty instances of their type.",
     PARA,
     SEEALSO {"help functions"}
     }

document { TEST,
     TT "TEST s", " -- writes the string ", TT "s", " to a new test file.  The
     commands in that file can be run later as a test.",
     PARA,
     "Intended for internal use only."
     }

document { between,
     TT "between(m,v)", " -- inserts ", TT "m", " between each pair of elements 
     of the list or sequence ", TT "v", ", returning a list.",
     PARA,
     EXAMPLE {
	  "between(55,{a,b,c,d})"
	  }
     }

document { SEEALSO,
     TT "SEEALSO {a, b, ...}", " -- inserts, into a documentation page, a sentence
     instructing the reader to see some other topics.",
     PARA,
     "The topics may have the special forms used with ", TO "TO", ".",
     SEEALSO "document"
     }

document { briefDocumentation,
     TT "briefDocumentation s", " -- provides the first paragraph of the online
     documentation for the topic ", TT "s", ", in internal ", TO "hypertext", "
     form, suitable for conversion to text with ", TO "text", " or to html 
     with ", TO "html", ".",
     SEEALSO "documentation"
     }

document { documentation,
     TT "documentation s", " -- provides the online documention for the topic s, in
     internal ", TO "hypertext", " form, suitable for conversion to
     text with ", TO "text", " or to html with ", TO "html", ".",
     PARA,
     EXAMPLE "documentation partitions"
     }

document { help,
     -- no PARA in this documentation, so it all gets displayed.
     TT "help X", " -- displays the online documentation for ", TT "X", ".",
     BR, NOINDENT,
     TT "help \"Macaulay 2\"", " -- displays the base of the online documentation
     tree.",
     BR, NOINDENT,
     TT "help methods X", " -- displays help messages about the methods usable
     with things of type ", TT "X", ".",
     BR, NOINDENT,
     TT "help methods res", " -- displays help messages about the methods 
     usable with the function ", TT "res", ".",
     BR, NOINDENT,
     TT "help methods symbol **", " -- displays help messages about the methods 
     usable with the operator ", TT "**", ".",
     BR, NOINDENT,
     TT "help methods (res, X)", " -- displays help messages about the 
     methods usable with the function ", TT "res", " and a thing of
     class ", TT "X", ".",
     BR, NOINDENT,
     TT "help methods (symbol **, X)", " -- displays help messages about the 
     methods usable with the operator ", TT "**", " and a thing of
     class ", TT "X", ".",
     BR, NOINDENT,
     TT "help methods (X, Y)", " -- displays help messages about the 
     methods usable with a thing of class ", TT "X", " and a thing of class
     ", TT "Y", "."
     }

document { topicList,
     TT "topicList()", " -- provides a complete list of topics on which help 
     is available.",
     PARA,
     "Intended to be used in programs.  Users will prefer 
     to use ", TO "topics", ".",
     PARA,
     SEEALSO "help"
     }

document { topics,
     TT "topics  ", " -- displays a list of topics on which help is available.",
     PARA,
     "topics() -- Does the same in a function or file.",
     PARA,
     SEEALSO "help"
     }

document { apropos,
     TT "apropos s", " -- displays a list of global symbols which match
     the pattern specified by the string ", TT "s", ".",
     PARA,
     "The pattern may contain '*'s as wild card characters.",
     EXAMPLE "apropos \"scan\""
     }

document { printExamples,
     TT "printExamples f", " -- prints out the examples of code using
     the function ", TT "f", " provided in the documentation for
     ", TT "f", ".",
     PARA,
     EXAMPLE "printExamples partition",
     SEEALSO {"examples", "document"}
     }

document { "Symbols",
     TT "Symbols", " -- a hash table which can be used to obtain the global
     symbol with a particular value.",
     PARA,
     "This is useful internally for getting the name of a function, for 
     example."
     }

document { "Documentation",
     TT "Documentation", " -- a hash table which is used to store
     pointers to documentation of functions, symbols, and methods.",
     PARA,
     "This hash table is used by the routines that display documentation,
     is intended for internal use only, and its format may change.",
     PARA,
     "The documentation is stored both in a hash table in memory, and in a 
     database file.",
     SEEALSO {"Symbols", ":="}
     }

document { formatDocumentTag,
     TT "formatDocumentTag x", " -- formats the tags used with ", TO "TO", " for
     display purposes in documents.",
     PARA,
     "This function is intended for internal use only."
     }

document { uniform,
     TT "uniform x", " -- whether all elements of the list x have the same class."
     }
document { newClass,
     TT "newClass(N,m)", " -- makes a copy of m with N as the new class", BR,
     TT "newClass(N,M,m)", " -- makes a copy of m with N as class and M as parent",
     PARA,
     "If m is a list, then BasicList should be an ancestor of N.  If m is 
     a hash table, then ", TT "HashTable", " should be an ancestor of N.",
     PARA,
     "If m is mutable, and instances of class N are also mutable, then
     copying is not required, and is not done.",
     PARA,
     SEEALSO { "copy", "toList" }
     }

document { MutableList,
     TT "MutableList", " -- the class of all mutable Lists.",
     PARA,
     "Normally the entries in a mutable list are not printed, to prevent
     infinite loops in the printing routines.  To print them out, use 
     ", TO "peek", ".",
     PARA,
     EXAMPLE {
	  "s = new MutableList from {a,b,c};",
      	  "s#2 = 1234;",
	  "s",
      	  "peek s",
	  },
     SEEALSO {"BasicList"}
     }

document { lookup,
     TT "lookup", " -- a function for looking up methods.",
     PARA,
     NOINDENT,
     TT "lookup(M,A)", " -- provides the binary method named ", TT "M", " for class ", TT "A", ".
     The first place to look is ", TT "A#M", ".  The search proceeds with
     the parent of ", TT "A", ", and so on.",
     PARA,
     NOINDENT, TT "lookup(M,A,B)", " -- provides the binary method named ", TT "M", " for ", TT "(A,B)", ".
     The first place to look is ", TT "Y#(M,A,B)", " where ", TT "Y", " is the younger
     of ", TT "A", " and ", TT "B", ".  The search proceeds next with the parent of ", TT "B", ", 
     and so on.",
     PARA,
     NOINDENT, TT "lookup(M,A,B,C)", " -- provides the ternary method named ", TT "M", " for
     ", TT "(A,B,C)", ".  The first place to look is ", TT "Y#(M,A,B,C)", " where ", TT "Y", " 
     is the youngest of ", TT "A", ", ", TT "B", ", and ", TT "C", ".  The search proceeds with 
     the parent of ", TT "C", ", and so on.",
     PARA,
     "If no method is found, then ", TT "null", " is returned.",
     PARA,
     SEEALSO {"#", "classes", "installMethod", "youngest"}
     }

document { installMethod,
     TT "installMethod", " -- a function for installing methods.",
     PARA,
     "Most users will use a different way of installing methods.",
     PARA,
     NOINDENT,
     TT "installMethod(M,A,f)", "     -- installs a function ", TT "f", " as a unary method for
     the class ", TT "A", " under the name ", TT "M", ".  This is the same as ", "M A := f", " 
     if ", TT "M", " is a function.  As currently implemented, this is also the same 
     as ", TT "A#M = f", ".",
     PARA,
     NOINDENT,
     TT "installMethod(M,A,B,f)", "   -- installs a function ", TT "f", " as a binary method for
     classes ", TT "A", " and ", TT "B", " under the name ", TT "M", ".  This is the same as 
     ", TT "M(A,B) := f", " if ", TT "M", " is a
     function, or the same as ", TT "A M B := f", " if ", TT "M", " is a binary operator. As currently
     implemented, this is also the same as ", TT "Y#(M,A,B) = f", ", where ", TT "Y", " is 
     the younger of ", TT "A", " and ", TT "B", ".",
     PARA,
     NOINDENT,
     TT "installMethod(M,A,B,C,f)", " -- installs a function ", TT "f", " as a ternary method 
     for classes ", TT "A", ", ", TT "B", ", and ", TT "C", " under the name ", TT "M", ".  
     This is the same as ", TT "M(A,B,C) := f", " if ", TT "f", "
     is a function.  As currently implemented, this is also the same as
     ", TT "Y#(M,A,B,C) = f", ", where ", TT "Y", " is the youngest of ", TT "A", ", ", TT "B", ", 
     and ", TT "C", ".",
     PARA,
     SEEALSO{"#", "lookup",  "new", "classes"}
     }

document { "new",
     TT "new A of b from c", " -- make a hash table of class ", TT "A", " and 
     parent ", TT "b", " initialized from ", TT "c", ".", BR,
     NOINDENT,
     TT "new A of b", " -- make a hash table of class ", TT "A", " 
     and parent ", TT "b", ".", BR,
     NOINDENT,
     TT "new A from c", " -- make a new instance of class ", TT "A", " 
     initialized from ", TT "c", ".", BR,
     NOINDENT,
     TT "new A", " -- makes a new instance ", TT "n", " 
     of class ", TT "A", ".", BR,
     PARA,
     HR,
     NOINDENT,
     TT "new A of b from c", " -- make a hash table ", TT "n", " of
     class ", TT "AA", " and parent ", TT "b", " initialized from ", TT "c", ".",
     PARA,
     "One may use this to model the mathematical notion
     that ", TT "x", " is an element of ", TT "A", " and a subset of ", TT "b", ".
     Here ", TT "A", " and ", TT "b", " are hash tables, and ", TT "c", " is
     any expression.  Let ", TT "b", " be an instance of ", TT "B", ", ", TT "c", "
     be an instance of ", TT "C", ", and let ", TT "AA", " be an
     ancestor of ", TT "A", ".  Then use",
     PRE "          new AA of B from C := (A,b,c) -> ... ",
     "to install the corresponding optional creation routine -- the
     value it returns will be converted so its class is ", TT "A", " and its
     parent is ", TT "b", "; this will involve copying unless the returned value 
     is mutable and objects of class ", TT "A", " are mutable.",
     PARA,
     "If no installation routine has been installed, then ", TT "c", " should be
     a hash table or a list, and it will be converted directly.",
     HR,
     NOINDENT,
     TT "new A of b", " -- make a hash table of class ", TT "A", "
     and parent ", TT "b", ".",
     PARA,
     "Same as above, except ", TT "c", " is missing.  Use ",
     PRE "          new AA of B := (A,b) -> ... ",
     "to install the initialization routine.",
     HR,
     NOINDENT,
     TT "new A from c", " -- make a hash table or list ", TT "n", " of 
     class ", TT "A", " initialized from ", TT "c", ".",
     PARA,
     "The same as above except ", TT "b", " is missing.  Use ",
     PRE "          new AA from C := (A,c) -> ... ",
     "to install the corresponding initialization routine.",
     PARA,
     "Since no parent ", TT "b", " has been provided, the value returned by the
     initialization routine will not have its parent reset.  If there
     is no initialization routine the parent will be set to Nothing.",
     HR,
     NOINDENT,
     TT "new A", " -- make a new instance ", TT "n", " of 
     class ", TT "A", ".",
     PARA,
     "Same as above, except ", TT "b", " and ", TT "c", " are missing.
     Use ", TT "new AA := A -> ... ", " to install the initialization routine.",
     PARA,
     "Since no parent ", TT "b", " has been provided, the value returned by the
     initialization routine will not have its parent reset.  If there
     is no initialization routine the parent will be set to Nothing.",
     PARA,
     "Note that if the ", TT "of", " option is not used, then the class ", TT "A", "
     need not consist of hash tables or lists.  We are using this feature by
     installing a method so that ", TT "new ZZ", " returns an integer popped
     from the top of the engine's stack.",
     PARA,
     "The symbols ", TO "NewMethod", ", ", TO "NewOfMethod", ", ", 
     TO "NewFromMethod", ", and ", TO "NewOfFromMethod", " are used for 
     installation of the initialization routines.",
     SEEALSO {"classes", "of", "from"}
     }

document { "of",
     TT "of", " -- a keyword used with ", TO "new", "."
     }

document { "from",
     TT "from", " -- a keyword used with ", TO "new", "."
     }

document { NewMethod,
     TT "NewMethod", " -- a symbol used as a method name in conjuction with
     the ", TO "new", " operator."
     }

document { NewOfMethod,
     TT "NewOfMethod", " -- a symbol used as a method name in conjuction with
     the ", TO "new", " operator."
     }

document { NewFromMethod,
     TT "NewFromMethod", " -- a symbol used as a method name in conjuction with
     the ", TO "new", " operator."
     }

document { NewOfFromMethod,
     TT "NewOfFromMethod", " -- a symbol used as a method name in conjuction with
     the ", TO "new", " operator."
     }

document { Thing,
     TT "Thing", " -- the class of all things.",
     PARA,
     "Everything in Macaulay 2 is a ", ITALIC "thing", ".  This 
     includes numbers, strings, and lists.  More complicated things such as 
     polynomials, groups, rings, and chain complexes are implemented
     as ", ITALIC "hash tables", ".  The class of all things is ", TO "Thing", ".",
     PARA,
     "The basic types of things are:", 
     MENU {
          TO "BasicList",
          TO "Boolean",
          SHIELD TO "Database",
          TO "File",
          TO "Function",
          SHIELD TO "Handle",
          TO "HashTable",
          TO "Net",
          TO "Nothing",
          TO "QQ",
          TO "RR",
          TO "Sequence",
          TO "String",
          TO "Symbol",
          TO "Thing",
          TO "ZZ"
	  },
     "Operations on things:",
     MENU {
	  TO "comparison",
	  TO "assignment"
	  }
     }

document { Nothing,
     TT "Nothing", " -- the empty class.",
     PARA,
     "This class is useful for representing the class of an argument
     which is missing.  It is also used as the parent for those things which
     are not themselves types, i.e., which do not have instances." 
     }

document { Option,
     TT "Option", " -- the class of all pairs x => y.",
     PARA,
     "Such pairs are used as optional arguments for functions.  There
     is also a way to make new hash tables with ", TO "new", " by
     providing a list of option pairs.",
     PARA,
     EXAMPLE {
	  "a => 5",
      	  "peek (a => 5)",
	  "new HashTable from {a => 5, b => 7}",
	  },
     PARA,
     "These pairs are implemented as lists, so that if ", TT "z", " is ", TT "x => y", ", then 
     ", TT "x", " is ", TT "z#0", " and ", TT "y", " is ", TT "z#1", ".",
     PARA,
     SEEALSO {"classes", "=>"}
     }

document { (NewFromMethod, HashTable, List),
     TT "new HashTable from x", " -- produce a new hash table from a
     list ", TT "x", ".",
     PARA,
     "Elements of ", TT "x", " which are options, ", TT "k => v", " cause
     the value ", TT "v", " to be stored in ", TT "x", " under the key ", TT "k", ".
     Other elements ", TT "s", " cause the value ", TT "true", " to be stored under 
     the key ", TT "s", "."
     }

document { OptionTable,
     TT "OptionTable", " -- the class of those hash tables which are used
     to store optional named parameters to functions.",
     SEEALSO "processArgs"
     }

document { processArgs,
     TT "processArgs(args,defs,fun)", " -- a helper function which processes
     optional arguments.  Intended for internal use only.",
     PARA,
     "Here ", TT "args", " is the sequence of arguments previously passed to 
     some function intended to accept optional arguments, ", TT "defs", " is a
     hash table whose keys are the names of the optional arguments, and 
     whose values are the corresponding default values.
     The return value is obtained by evaluation of ", TT "(fun opts)(args')", ",
     where ", TT "args'", " is obtained from ", TT "args", " by removing the
     options of the form ", TT "X=>A", " (where ", TT "X", " is a
     name of an optional argument), and ", TT "opts", " is a hash table
     of the same form as ", TT "defs", " in which the default
     values have been replaced by the user-supplied values, e.g., the
     value stored under the key ", TT "X", " has been replaced by
     ", TT "A", ".",
     PARA,
     "In the following example we use a simple definition for ", TT "fun", "
     so we can see everything that ", TT "fun", " receives from ", TT "processArgs", ".",
     EXAMPLE {
	  "defs = new OptionTable from {a=>1, b=>2}",
	  "fun = opts -> args -> {opts, args}",
	  "processArgs(x,defs,fun)",
	  "processArgs((x,y,b=>66),defs,fun)",
	  "processArgs((t,u,a=>44,b=>77),defs,fun)",
	  },
     SEEALSO {"making new functions with optional arguments", "OptionTable", "Option", "=>"}
     }

document { "using methods",
     "The method to be used for computing an expression such as ", TT "-x", " depends 
     on the type of ", TT "x", ".  For example, the method for negating a polynomial
     differs from the method for negating an integer modulo 111.  Each
     method is a function of one variable, and is stored in the class 
     of ", TT "x", " under a key which is referred to as the name of the method.
     For some built-in methods the method name is a symbol, but for
     methods created with ", TO "method", ", the method name is the same
     as the function used for calling it up.",
     PARA,
     "Let's assume that ", TT "X", " is the class of ", TT "x", ".  The way to install a method
     for the negation of an instance ", TT "x", " of ", TT "X", " is with a statement of the 
     following form.",
     PRE "- X := x ->( ... )",
     "Here ", TT "( ... )", " represents the body of the function, consisting of
     suitable code for the operation at hand.",
     PARA,
     "The method installed by the code above is automatically inherited by
     ", TO "subclass", "es of X.  Here is a brief description of the way 
     this works.  Suppose ", TT "X", " is the ", TO "parent", " of ", TT "P", ".  When an expression
     ", TT "-p", " is to be evaluated, where the class of ", TT "p", " is ", TT "P", ", then the method for
     ", TT "-P", " is applied, unless there isn't one, in which case the method for
     ", TT "-X", " is applied, and so on, all the way up the chain of parents to the
     topmost ancestor of everything, which is called ", TO "Thing", ".",
     PARA,
     "As an extreme example of inheritance, code like", 
     PRE "- Thing := x -> ...",
     "will install a method for negating anything, which will take
     effect as a last resort whenever more a specifically defined method
     isn't found.",
     PARA,
     "The user may introduce new methods as well as new method names.  So it
     is important to understand how methods are installed and consulted.",
     PARA,
     "Applying a method named ", TT "C", " to a thing ", TT "x", " whose class is ", TT "X", " means that",
     PRE "(lookup(C,X)) x",
     "is evaluated.  In other words, ", TT "C", " is used as a key
     to obtain a function from ", TT "X", " (or its parent, grandparent,
     and so on), and the function is applied to ", TT "x", ".  See ", TO "lookup", ".",
     PARA,
     "Installing a method named ", TT "C", " for the class ", TT "X", " is done with code such
     as ",
     PRE "C X := (x) -> ( ... )",
     "where ", TT "( ... )", " represents suitable code for the operation at hand.",
     PARA,
     "Here is the routine for making new methods.",
     MENU {
	  TO "method"
	  },
     SEEALSO{"binary method", "classes", "lookup"}
     }

document { "binary method",
     "The method for computing a sum ", TT "x+y", " depends on the types of ", TT "x", " and ", TT "y", ".
     For example, the method for adding an integer ", TT "x", " and a polynomial 
     ", TT "y", " differs from the method for adding two integers modulo 111.  Because
     both the type of ", TT "x", " and the type of ", TT "y", " must enter into the selection of
     the method, we refer to these methods as binary methods.  Each binary
     method is a function of two variables, and is stored either in the class
     of ", TT "x", " or in the class of ", TT "y", ".  See also ", TO "lookup", ".",
     PARA,
     "Let's assume that ", TT "X", " is the class (or type) of ", TT "x", ", 
     and that ", TT "Y", " is the class of ", TT "y", ".  The way to install a 
     method for the addition of an instance ", TT "x", " of class ", TT "X", " to 
     an instance ", TT "y", " of class ", TT "Y", " is with a statement of the form ",
     PRE "X + Y := (x,y) -> ( ... )",
     "where ", TT "( ... )", " represents the body of the function, consisting of suitable
     code for the operation at hand.",
     PARA,
     "The method installed by the code above is automatically inherited by 
     ", TO "subclass", "es of ", TT "X", " and ", TT "Y", ".  Here is a brief
     description of the way this works.  Suppose ", TT "X", " is the 
     ", TO "parent", " of ", TT "P", " and ", TT "Y", " is the parent of X.  When 
     a sum ", TT "p+q", " is evaluated where the class of ", TT "p", " is 
     ", TT "P", " and the class of ", TT "q", " is ", TT "Q", ", then the binary
     method for ", TT "P+Q", " is applied, unless there isn't one, in which
     case the binary method for ", TT "P+Y", " is applied, unless there isn't
     one, in which case the binary method for ", TT "X+Q", " is applied,
     unless there isn't one, in which case the binary method for ", TT "P+Q", "
     is applied.  In general this search for a binary method continues all
     the way up the chain of parents to the topmost ancestor of everything,
     which is called ", TO "Thing", ".",
     PARA,
     "As an extreme example of inheritance, the code ", 
     PRE "Thing + Thing := (x,y) -> ( ... )",
     "will install a binary method for adding any two things, which will take
     effect as a last resort whenever more a specifically defined method
     isn't found.",
     PARA,
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

document { "OptionsRegistry",
     TT "OptionsRegistry", " -- a hash table used for recording the tables of
     option names and their default values for those functions which accpet
     optional arguments.",
     PARA,
     "If ", TT "f", " is a function which accepts optional arguments, then
     the ", TO "OptionTable", " for ", TT "f", " is stored as ", 
     TT "OptionsRegistry#f", ".",
     PARA,
     "The function ", TO "method", ", when given a table of options, will
     record them here."
     }

document { SingleArgumentDispatch,
     TT "SingleArgumentDispatch=>true", " -- an option to ", TO "method", "
     which specifies whether the method function should treat several
     arguments as a single argument, i.e., as a sequence.",
     PARA,
     "This allows the user to install a method for handling sequences, whereas
     normally, the types of up to the three arguments are considered.",
     EXAMPLE {
	  "f = method ( SingleArgumentDispatch => true )",
      	  "f Sequence := print",
	  "f (1,2,3)"
	  }
     }

document { "specifying typical values",
     "For the purpose of construction good documentation automatically, it
     is useful to specify the type of value typically returned by a function
     or method.  For example, the function ", TO "isModule", " returns a boolean
     value, and this is specified when creating the method function with the
     option ", TO "TypicalValue", " as follows.",
     PRE ///isModule = method(TypicalValue => Boolean)///,
     PARA,
     "Other functions, such as ", TO "prune", ", return values of various types,
     depending on the type of the arguments provided.  To install a
     function ", TT "f", " as the handler for ", TT "prune", " applied to a matrix,
     we would normally use the following statement.",
     PRE ///prune Matrix := f///,
     "To specify that the value typically returned is a matrix (of class ", TT "Matrix", "),
     we replace ", TT "f", " by ", TT "Matrix => f", ", as follows.",
     PRE ///prune Matrix := Matrix => f///,
     "Here is the way our code looks.",
     EXAMPLE "code(prune, Matrix)",
     "The information is stored in the hash table ", TO "typicalValues", ", and can
     be recovered like this.",
     EXAMPLE "typicalValues#(prune,Matrix)",
     PARA,
     "Warning: don't imagine that a definition of the form ",
     PRE "f = t -> (...)",
     "can be replaced with a declaration of the following form.",
     PRE "f = X => t -> (...)",
     "The difference here is that here we are using simple assignment, rather than
     installing a method.  To document the return type is ", TT "X", " in this case, 
     make an entry in ", TT "typicalValues", " directly.",
     PRE "f = t -> (...)\ntypicalValues#f = X"
     }

document { "typicalValues",
     TT "typicalValues", " -- a hash table used to store information about
     the type of values typically returned by functions and methods.",
     PARA,
     "This information is used only to build documentation automatically.",
     SEEALSO { "specifying typical values" }
     }

document { TypicalValues,
     TT "TypicalValues => X", " -- an option to ", TO "method", "
     which specifies that values returned by the method function will
     typically be of type ", TT "X", ".",
     PARA,
     "This information is used only to build documentation automatically, and
     is stored in the hash table ", TO "typicalValues", ".",
     SEEALSO { "specifying typical values" }
     }

document { method,
     TT "f = method()", " -- creates a method function",
     PARA,
     "Optional arguments:",
     MENU {
	  TO "Associative",
	  -- TO "ClassArgument",
	  TO "Options",
	  TO "SingleArgumentDispatch",
	  TO "TypicalValues"
	  },
     PARA,
     "The code above creates a method function which takes up to three 
     arguments, looking up the appropriate method according to the classes of 
     the arguments, with inheritance.  To install a method for two arguments,
     (x,y), of classes X and Y, use code like this:",
     PRE "     f(X,Y) := (x,y) -> ...",
     "where '...' represents the body of the function you wish to install.
     The syntax for one or three arguments is analogous.  For a single
     argument x of class X, one could also write:",
     PRE "     f X := (x) -> ...",
     "the effect of which happens to be the same as that of",
     PRE "     X#f := (x) -> ...",
     PARA,
     SEEALSO {"Options", "methods", "OptionsRegistry"}
     }

document { Associative,
     TT "Associative", " -- an option name for ", TO "method", " which
     allows associative methods to be created.",
     PARA,
     NOINDENT,
     TT "f = method(Associative=>true)", " -- creates an associative
     method which will call upon the appropriate binary methods for its arguments 
     two at a time.",
     PARA,
     "In the following example we install a method which isn't associative
     to illustrate the order of evaluation.",
     EXAMPLE {
	  "f = method(Associative => true)",
	  ///f(String,String) := (i,j) -> "(" | i | ")," | j;///,
      	  ///f("a","b","c","d")///,
	  },
     SEEALSO "method"
     }

document { size,
     TT "size x", " -- returns the size of ", TT "x", " which usually gives
     a rough indication of memory space required to store the object ", TT "x", ".",
     PARA,
     "For a polynomial, the size is the number of terms."
     }
document { baseName,
     TT "baseName x", " -- returns the variable or symbol upon which a generator of a
     monoid or polynomial ring is based."
     }
document { degree,
     TT "degree X", " -- returns the degree of a polynomial, vector, 
     matrix, monomial, or module.",
     PARA,
     "The degree may be an integer, or a vector of integers.  The length
     of that vector is referred to as the 'number of degrees', and is
     provided by ", TO "degreeLength", ".",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[x,y,z]",
      	  "degree (x^2+y^2)^5",
      	  "F = R^{2,3,4}",
      	  "v = F_2",
      	  "degree v",
	  },
     "The degree of a module of finite length is the same as its length.",
     EXAMPLE "degree cokernel symmetricPower ( 2, vars R )",
     PARA,
     "Implemented with a method of the same name."
     }

document { degreeLength,
     TT "degreeLength x", " -- returns the number of degrees of x.",
     PARA,
     "Here x may be a ring, in which case it returns the number of degrees
     (the length of the degree vector) used in grading the ring.",
     SEEALSO "degree"
     }

document { coefficients,
     TT "coefficients({i,j,...},p)", " -- yields the coefficients and
     monomials of the polynomial or matrix p with respect to variables 
     numbered i, j, ... .  This has to
     be completely redone, so I don't document it further, but it is used in
     the factoring code.",
     BR,NOINDENT,
     TT "coefficients(p)", " -- yields the coefficients and monomials of
     the polynomial or matrix p with respect to all of the variables."
     }

document { isIsomorphism,
     TT "isIsomorphism f", " -- whether the map f of modules is an isomorphism."
     }
document { isHomogeneous,
     TT "isHomogeneous x", " -- whether the polynomial or ideal x is homogeneous."
     }
document { vars, 
     TT "vars R", " -- provides a 1 by n matrix whose entries are the
     variables of the polynomial ring R.",
     BR,
     NOINDENT,
     TT "vars(i .. j)", " -- provides a sequence of symbols which can be used
     as indeterminates in a polynomial ring, the i-th one through the j-th one.
     There is no limit on the size of the integers ", TT "i", " and ", TT "j", ".",
     PARA,
     EXAMPLE {
	  "vars(3 .. 9,1000,-100)",
      	  "R = ZZ/101[vars(3 .. 5)]",
      	  "vars R",
      	  "symmetricPower(2,vars R)"
	  }
     }

document { leadCoefficient,
     TT "leadCoefficient f", " -- return the leading coefficient of the polynomial
     or vector f.",
     PARA,
     SEEALSO {"leadTerm", "leadMonomial", "leadComponent"}
     }

document { leadComponent,
     TT "leadComponent f", " -- return the leading component of the vector f,
     i.e., the integer i so that f_i is the first nonzero component of f.",
     PARA,
     SEEALSO {"leadTerm", "leadCoefficient", "leadMonomial"}
     }

document { leadMonomial,
     TT "leadMonomial f", " -- return the leading monomial of the polynomial
     or vector f.",
     PARA,
     SEEALSO {"leadTerm", "leadCoefficient", "leadCoefficient"}
     }

document { flatten,
     TT "flatten m", " -- produces a new list from m by effectively removing the braces
     surrounding the elements of any elements of m which happen to be
     lists.  Also works for matrices.",
     PARA,
     EXAMPLE "flatten {{2,3,4},{{5}},6}"
     }
document { "coker",
     "An abbreviation for ", TO "cokernel", "."
     }
document { "cokernel",
     TT "cokernel f", " -- produces the cokernel of the module homomorphism f",
     PARA,
     "The result will be a quotient module of the target of f.  If f is
     a ring element, it is interpreted as a one by one matrix.",
     PARA,
     "The generators of the cokernel are provided by the generators of the target
     of ", TT "f", ".  In other words, ", TT "cover target f", " and ", TT "cover cokernel f", " are equal.",
     PARA,
     "For an abbreviation, use ", TO "coker", ".",
     SEEALSO {"kernel", "cover"}
     }

TEST ///
    R = QQ[x,y,z]
    modules = {
	 image matrix {{x^2,x,y}},
	 coker matrix {{x^2,y^2,0},{0,y,z}},
	 R^{-1,-2,-3},
	 image matrix {{x,y}} ++ coker matrix {{y,z}}
	 }
    scan(modules, M -> assert( cover cokernel M_{1} ==  cover M ) )
///


document { image,
     TT "image h", " -- yields the image of the homomorphism h.",
     PARA,
     "The result will be a submodule of the target of h",
     PARA,
     "If h is a ring element, it is interpreted as a one by one matrix."
     }

document { source,
     TT "source h", " -- the source of a morphism h.",
     }

document { target,
     TT "target h", " -- the target of a morphism or Groebner basis.",
     }

document { ambient,
     TT "ambient M", " -- yields the ambient free module for the module M.",
     BR,
     NOINDENT,
     TT "ambient R", " -- yields the ambient ring of the quotient ring ", TT "R", ".
     For a Galois field it yields the ring it was constructed from.",
     PARA,
     EXAMPLE "ambient(ZZ/101[a,b]/b^3/a^3)",
     SEEALSO {"cover", "super"}
     }
     
document { Hom,
     TT "Hom(M,N)", " -- constructs the module of homomorphisms from M to N.",
     PARA,
     "Implemented with a method of the same name.",
     PARA,
     "Use ", TO "homomorphism", " to convert an element of the module of
     homomorphisms to a matrix."
     }
document { "gens",
     "See ", TO "generators", "."
     }
document { "generators",
     TT "generators x", " -- produces the generators of x.",
     PARA,
     "For an abbreviation, use ", TO "gens", ".",
     PARA,
     "Produces the generators of a Groebner basis, a polynomial ring,
     a monoid ring, a free module, a free group, a submodule given by
     means of generators (or for which generators have been computed),
     or a free monoid.",
     PARA,
     "Usually the result is a list of generators, but the generators of
     a module or Groebner basis are provided as the columns in a matrix.  
     The matrix is stored in a module M under M.generators, unless the matrix
     is the identity matrix.",
     SEEALSO {"Monoid", "GroebnerBasis", "Module", "relations", "subquotient"}
     }

document { someTerms,
     TT "someTerms(f,i,n)", " -- selects n terms from the polynomial f, starting
     with the i-th one, and returns the resulting polynomial."
     }

document { scanKeys,
     TT "scanKeys(x,f)", " -- apply the function ", TT "f", " to each key used in the
     hash table or database ", TT "x", ".",
     PARA,
     "This function requires an immutable hash table.  To scan the keys in
     a mutable hash table, use ", TT "scan(keys x, f)", "."
     }

document { scanValues,
     TT "scanValues(x,f)", " -- apply the function ", TT "f", " to each value
     appearing in the hash table ", TT "x", ".",
     PARA,
     "This function requires an immutable hash table.  To scan the values in
     a mutable hash table, use ", TT "scan(values x, f)", "."
     }

document { GlobalAssignHook,
     TT "GlobalAssignHook", " -- a method name which is consulted when an
     assignment to a global variable occurs.",
     PARA,
     "The method should be a function of two variables: the symbol to which
     a value is being assigned, and the value being assigned.  The method
     should be stored under then name ", TT "GlobalAssignHook", " in the
     class of the value.  It will be executed just before the assignment
     occurs.",
     PARA,
     "This method is used for instances of ", TO "Type", " and ", TO "Ring", "
     to arrange for the name of the type or ring to be set to the name
     of the global variable to which it is first assigned.  The functions
     ", TO "globalAssignFunction", " and ", TO "globalReleaseFunction", " may installed
     as methods for this purpose.",
     PARA,
     EXAMPLE {
	  ///GlobalAssignHook RR := (sym,val) -> <<"assigning " <<val <<" to " <<sym <<endl;///,
          "a=4.5",
	  },
     SEEALSO "GlobalReleaseHook"
     }

document { GlobalReleaseHook,
     TT "GlobalReleaseHook", " -- a method name which is consulted when an
     assignment to a global variable is about to occur.",
     PARA,
     "The method should be a function of two variables: the symbol to which
     a value is being assigned, and the old value about to be overwritten.  
     The method should be stored under the name ", TT "GlobalReleaseHook", " in the
     class of the old value.  It is executed before the assignment occurs,
     and before the execution of ", TO "GlobalAssignHook", ".",
     PARA,
     EXAMPLE {
	  ///GlobalReleaseHook RR := (sym,val) -> << concatenate (
     "assigning ", toString val, " to ", toString sym
     ) << endl///,
          "a=4.5",
      	  "a=5.4",
	  },
     SEEALSO "GlobalAssignHook"
     }

document { stats,
     TT "stats g", " -- describe the status of a Groebner basis computation
     or of a resolution computation.",
     PARA,
     EXAMPLE {
	  "ZZ/101[a..f]",
      	  "stats gb matrix {{a*b, b*c},{a^3*f, b^3*e}}",
	  },
     SEEALSO { "GroebnerBasis", "Resolution" }
     }

document { complete,
     TT "complete C", " -- completely fills out the chain complex C by
     calling upon the engine to provide the maps and modules computed
     by ", TO "resolution", ".",
     PARA,
     "This is mainly intended for developers of new routines for chain
     complexes which have to make use of their internal structure.
     Before running this routine, it is not possible to determine which
     spots in a chain complex are actually occupied by modules or maps."
     }

document { drop,
     TT "drop(v,n) ", " -- yields the list obtained from the list v by
     dropping the first n elements.  Also works for sequences.",
     BR, NOINDENT,
     TT "drop(v,-n)", " -- yields the list obtained from the list v by dropping the 
     last n elements.",
     BR, NOINDENT,
     "drop(v,{m,n})", " -- yields the list obtained from the list v by dropping the
     elements at positions m through n.",
     PARA,
     EXAMPLE {
	  "drop({a,b,c,d,e},2)",
	  "drop({a,b,c,d,e},-2)",
	  "drop({a,b,c,d,e},{2,3})",
	  },
     SEEALSO{ "take"}
     }

document { options,
     TT "options f", " -- returns the table of option names and default values
     provided for the function ", TT "f", ", if one has been registered.",
     BR,NOINDENT,
     TT "options X", " -- returns the options used when the monoid or polynomial
     ring X was created.",
     BR,NOINDENT,
     TT "options S", " -- returns a list of those functions which have an
     optional argument named ", TT "S", ".  Here ", TT "S", " is a symbol.",
     PARA,
     SEEALSO {"method", "OptionsRegistry"}
     }
document { (symbol <<, Nothing, Thing),
     "null << x", " -- does nothing and returns ", TT "null", ".",
     PARA,
     "The intention here is that you can use ", TT "null", " as a dummy
     output file."
     }

document { mathML,
     TT "mathML x", " -- converts ", TT "x", " to MathML form.",
     PARA,
     EXAMPLE {
	  "R = ZZ[x,y];",
	  "mathML matrix {{x,y},{x^2+2,0}}"
	  },
     SEEALSO "hypertext"
     }

document { "#",
     TT "#x", " -- provides the length of a list, sequence, array, file, or 
     string, or the number of elements in a hash table or set.",
     BR,NOINDENT,
     TT "x#i", " -- provides the value associated to the key ", TT "i", " in the hash table
     ", TT "x", "; or else the i-th element of ", TT "x", " if ", TT "x", " is a list, array, or 
     sequence; or the i-th character of ", TT "x", " if ", TT "x", " is a string; or the value stored 
     in the database ", TT "x", " under the key ", TT "i", ", which must be a string.",
     PARA,
     "If ", TT "x", " is a string, then ", TT "i", " must be an integer, and ", TT "x#i", " is the i-th
     character in the string, presented as a string of length one, or if 
     ", TT "i", " is out of range, a string of length zero is returned.  If ", TT "i", " is
     negative, then the i-th character from the end is provided.",
     PARA,
     "Assignment to ", TT "x#i", " can change the value if x is mutable.",
     PARA,
     "The precedence of ", TT "#", " when used as a binary operator is high,
     as high as ", TT ".", ", but the precedence when used as a unary operator
     lower, as low as adjacency or function application.",
     PARA,
     EXAMPLE {
	  "x = new MutableHashTable",
	  "x#i = p",
	  "x#i",
	  },
     SEEALSO{ "#?", "#" }
     }

document { "#?",
     TT "x#?i", " -- tells whether there is a value associated to the key ", TT "i", " in 
     the hash table ", TT "x", "; or else whether the i-th element of ", TT "x", " exists if ", TT "x", " is a list, 
     array, or sequence; or else whether the i-th character of ", TT "x", " exists if ", TT "x", "
     is a string; or the value stored in the database ", TT "x", " under the key ", TT "i", ", which
     must be a string.",
     PARA,
     SEEALSO{ "#" }
     }

document { "_",
     TT "x_i", " -- a binary operator which is used for various
     mathematical operations that are customarily written with subscripts.",
     PARA,
     "A ", TO "binary method", " may be installed for ", TT "x_i", " with code like ",
     PRE "          X _ Y := (x,i) -> ...",
     "where X is the prospective class of x and Y is the class of i.",
     PARA,
     "Examples where methods have been installed:",
     MENU {
	  SHIELD (TO (symbol _, List, ZZ), " -- get an entry from a list"),
	  SHIELD (TO (symbol _, Sequence, ZZ), " -- get an entry from a sequence"),
	  SHIELD (TO (symbol _, List, List), " -- get a list of entries from a list or sequence"),
	  SHIELD (TO (symbol _, ChainComplex, ZZ), " -- get a module from a chain complex"),
	  SHIELD (TO (symbol _, Matrix, ZZ), " -- get a column from a matrix"),
	  SHIELD (TO (symbol _, ChainComplexMap, ZZ), " -- get a component from a map of chain complexes"),
	  SHIELD (TO (symbol _, Matrix, Sequence), " -- get an entry from a matrix"),
	  SHIELD (TO (symbol _, Matrix, List), " -- get some columns from a matrix"),
	  SHIELD (TO (symbol _, RingElement, RingElement), " -- get a coefficient from a polynomial"),
	  SHIELD (TO (symbol _, Ring, ZZ), " -- get a generator from a ring"),
	  SHIELD (TO (symbol _, Module, ZZ), " -- get a generator of a module"),
	  SHIELD (TO (symbol _, Monoid, ZZ), " -- get a generator from a monoid"),
	  SHIELD (TO (symbol _, Module, List), " -- get a map onto some generators of a module"),
	  SHIELD (TO "Tor", " -- Tor functor"),
	  SHIELD (TO "HH", " -- homology functor"),
	  SHIELD (TO (symbol _, Vector, ZZ), " -- get an component from a vector"),
	  SHIELD (TO (symbol _, SchurRing, List), " -- make an element of a Schur ring")
	  }
     }

document { (symbol _, List, ZZ),
     TT "w_i", " -- selects an entry from a list."
     }

document { (symbol _, Sequence, ZZ),
     TT "w_i", " -- selects an entry from a sequence."
     }

document { ".",
     TT "x.k", " -- the same as ", TT "x#(global k)", ", i.e., treat ", TT "k", " as
     a global symbol and provide the value stored in the hash table ", TT "x", " 
     under the key ", TT "k", ".",
     PARA,
     "May also be used in an assignment.",
     PARA,
     EXAMPLE {
	  "x = new MutableHashTable;",
      	  "x.k = 444",
      	  "x.k",
      	  "peek x",
	  },
     SEEALSO {"#", ".?", "global"}
     }

document { ".?",
     TT "x.?k", " -- the same as ", TT "x#?(global k)", ", tells whether a value is
     available with ", TT "x.k", ".",
     PARA,
     SEEALSO{ ".", "#?" }
     }

document { autoload,
     TT "autoload(f,\"x\")", " -- arranges for a function ", TT "f", " to be 
     automatically loaded from the file named ", TT "x", " the first
     time it is used."
     }
