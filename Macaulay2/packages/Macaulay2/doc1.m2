
document {
     Key => between,
     Headline => "insert something between elements of a list",
     TT "between(m,v)", " -- inserts ", TT "m", " between each pair of elements 
     of the list or sequence ", TT "v", ", returning a list.",
     PARA,
     EXAMPLE {
	  "between(55,{a,b,c,d})"
	  }
     }
document {
     Key => briefDocumentation,
     Headline => "get brief documentation",
     TT "briefDocumentation s", " -- provides the first paragraph of the online
     documentation for the topic ", TT "s", ", in internal ", TO "hypertext", "
     form, suitable for conversion to text with ", TO "net", " or to html 
     with ", TO "html", ".",
     SeeAlso => "documentation"
     }
document {
     Key => documentation,
     Headline => "get documentation, unformatted",
     TT "documentation s", " -- provides the online documentation for the topic s, in
     internal ", TO "hypertext", " form, suitable for conversion to
     text with ", TO "net", " or to html with ", TO "html", ".",
     PARA,
     EXAMPLE "documentation partitions"
     }
document {
     Key => "initial help",				    -- display by the help command by default
     "Welcome to Macaulay 2",
     PARA,
     "Try entering '2+2' at your next input prompt, which begins with ", TT "i", ".
     The two output prompts begin with ", TT "o", ".  The first one, with the
     equal sign, '=', gives the value computed from your input, and the second one, with
     the colon, ':', tells what type of thing the value is.",
     PARA,
     "Type one of these commands to get started reading the documentation:",
     UL {
     	  ("copyright", "                        -- the copyright"),
     	  ("help \"Macaulay 2\"", "                -- top node of the documentation."),
     	  ("help \"Reading the documentation\"", " -- "),
     	  ("help \"getting started\"", "           -- "),
     	  ("help \"A first Macaulay 2 session\"", "   -- "),
     	  ("help x", "                           -- display the documentation for ", TT "x"),
	  ("printWidth = 80", "                  -- set the print width to 80 characters"),
     	  ("viewHelp x", "                       -- view documentation for ", TT "x", " in a browser"),
     	  ("viewHelp", "                         -- view all documentation in a browser"),
	  },
     "To read the documentation in info form, in case you happen to be running Macaulay 2 in a 
     terminal window, replace ", TO "help", " by ", TO "infoHelp", " in any of the commands 
     above."
     }
document {
     Key => help,
     Headline => "help command",
     "Various ways to get help:",
     UL {
     	  (TT "help \"Macaulay 2\"", " -- displays the base of the online documentation tree."),
     	  (TT "help X", " -- displays the online documentation for ", TT "X"),
	  (TT "help methods X", " -- displays help messages about the methods usable with things of type ", TT "X"),
	  (TT "help methods res", " -- displays help messages about the methods usable with the function ", TT "res"),
	  (TT "help methods symbol **", " -- displays help messages about the methods usable with the operator ", TT "**"),
	  (TT "help methods (res, X)", " -- displays help messages about the methods usable with the function ", TT "res", " and a thing of class ", TT "X"),
	  (TT "help methods (symbol **, X)", " -- displays help messages about the methods usable with the operator ", TT "**", " and a thing of class ", TT "X"),
	  (TT "help methods (X, Y)", " -- displays help messages about the methods usable with a thing of class ", TT "X", " and a thing of class ", TT "Y")
	  },
     "The ", TT "help", " command is used to display online documentation.  Use ", TO viewHelp, " to display the corresponding
     documentation in your web browser.",
     EXAMPLE {
	  "help",
	  "help ideal",
	  "help (ideal,List)"
	  },
     "Some other potential help topics:",
     UL {
	  TT "help \"monomial orders\"",
	  TT "help \"Groebner bases\"",
	  TT "help \"multigraded polynomial rings\""
	  },
     SeeAlso => {viewHelp, infoHelp,  apropos, code, examples}
     }
document {
     Key => viewHelp,
     Headline => "view online doc with a web browser",
     Usage => {TT "viewHelp", EM " or ", TT "viewHelp s"},
     Inputs => {
	  "s" => "a descriptor for a documentation node (see below for examples)"
	  },
     Consequences => {
	  {"The given documentation page is displayed in your default web browser.  If
	  the browser is not running, it is started.  If no argument is given to ", TT "viewHelp", 
	  "then the top page of your local html documentation is displayed."}},
     "Some example uses:",
     UL {
	  (TT "viewHelp", " -- top of local copy of the documentation, including installed packages"),	  
	  (TT "viewHelp \"Macaulay 2\"", " -- top of Macaulay2 doc"),
	  (TT "viewHelp ideal", " -- online doc for the 'ideal' function"),
	  (TT "viewHelp \"matrices\"", " -- overview of matrices in Macaulay 2"),
	  (TT "viewHelp (ideal,List)", " -- online doc for ideal(List) method"),
	  (TT "viewHelp (diff,Matrix,Matrix)", " -- online doc for the diff function taking two matrices as arguments"),
	  (TT "viewHelp [gb,DegreeLimit]", " -- view doc for the optional argument DegreeLimit to gb function"),
	  (TT "viewHelp (symbol**,Matrix,Matrix)", " -- view doc for Matrix**Matrix")
	  },
     Caveat => {"The ", TO help, " command allows other possible arguments, such as ", 
	  TT "help methods ideal", ", but for ", TT "viewHelp", " the argument ", TT "s",
	  " must refer to ony one web page."},
     SeeAlso => {apropos, examples, help, infoHelp}
     }
document {
     Key => infoHelp,
     Headline => "view online doc with info",
     Usage => "infoHelp s",
     Inputs => {
	  "s" => "a descriptor for a documentation node (see below for examples)"
	  },
     Consequences => {
	  "The given documentation page is displayed using info, if you are running
	  Macaulay2 in a terminal window."},
     "Some example uses:",
     UL {
	  (TT "infoHelp \"Macaulay 2\"", " -- top of Macaulay2 doc"),
	  (TT "infoHelp ideal", " -- online doc for the 'ideal' function"),
	  (TT "infoHelp \"matrices\"", " -- overview of matrices in Macaulay 2"),
	  (TT "infoHelp (ideal,List)", " -- online doc for ideal(List) method"),
	  (TT "infoHelp (diff,Matrix,Matrix)", " -- online doc for the diff function taking two matrices as arguments"),
	  (TT "infoHelp [gb,DegreeLimit]", " -- view doc for the optional argument DegreeLimit to gb function"),
	  (TT "infoHelp (symbol**,Matrix,Matrix)", " -- view doc for Matrix**Matrix")
	  },
     "While in the ", TT "info", " program, there are many ways to navigate and search.
     Besides the arrow keys to move around on the page, here is a list of the most useful key strokes:",
     UL {
	  (TT "?", " -- display information about all of the possible keystrokes"),
	  (TT "q", " -- quit info, return to Macaulay2"),
	  (TT "n", " -- go to the next documentation node"),
	  (TT "p", " -- go to the revious node"),
	  (TT "m", " -- follow the menu link"),
	  (TT "r", " -- follow a cross-reference"),
	  (TT "l", " -- go to the last node visited"),
	  },
     Caveat => {"The ", TO help, " command allows other possible arguments, such as ", 
	  TT "help methods ideal", ", but ", TT "infoHelp", " requires that the argument ", TT "s",
	  " refers to only one documentation page."},
     SeeAlso => {apropos, examples, help, viewHelp}
     }
document {
     Key => uniform,
     Headline => "test whether elements of a list are of the same class",
     TT "uniform x", " -- whether all elements of the list x have the same class."
     }
document {
     Key => newClass,
     Headline => "copy an object, changing the class",
     TT "newClass(N,m)", " -- makes a copy of m with N as the new class", BR, NOINDENT,
     TT "newClass(N,M,m)", " -- makes a copy of m with N as class and M as parent",
     PARA,
     "If m is a list, then BasicList should be an ancestor of N.  If m is 
     a hash table, then ", TT "HashTable", " should be an ancestor of N.",
     PARA,
     "If m is mutable, and instances of class N are also mutable, then
     copying is not required, and is not done.",
     PARA,
     SeeAlso => { "copy", "toList" }
     }
document {
     Key => MutableList,
     Headline => "the class of all mutable lists",
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
     SeeAlso => {"BasicList"}
     }
document {
     Key => lookup,
     Headline => "look up methods",
     NOINDENT,
     TT "lookup(M,A)", " -- provides the binary method named ", TT "M", " for class ", TT "A", ".
     The first place to look is ", TT "A#M", ".  The search proceeds with
     the parent of ", TT "A", ", and so on.",
     PARA,
     NOINDENT, TT "lookup(M,A,B)", " -- provides the binary method named ", TT "M", " for ", TT "(A,B)", ".
     The first place to look is ", TT "Y#(M,A,B)", " where ", TT "Y", " is the younger
     of ", TT "A", " and ", TT "B", ".  The search proceeds next with the parent of ", TT "B", ", 
     and so on. ",
     PARA,
     NOINDENT, TT "lookup(M,A,B,C)", " -- provides the ternary method named ", TT "M", " for
     ", TT "(A,B,C)", ".  The first place to look is ", TT "Y#(M,A,B,C)", " where ", TT "Y", " 
     is the youngest of ", TT "A", ", ", TT "B", ", and ", TT "C", ".  The search proceeds with 
     the parent of ", TT "C", ", and so on.",
     PARA,
     NOINDENT, TT "lookup(M,A,B,C,D)", " -- provides the quaternary method named ", TT "M", " for
     ", TT "(A,B,C,D)", ".  The first place to look is ", TT "Y#(M,A,B,C,D)", " where ", TT "Y", " 
     is the youngest of ", TT "A", ", ", TT "B", ", ", TT "C", ", and ", TT "D", ".  The search proceeds with 
     the parent of ", TT "D", ", and so on.",
     PARA,
     NOINDENT, TT "lookup x", " -- where ", TT "x", " is a symbol or function, returns ", TT "x", ".",
     PARA,
     "If no method is found, then ", TO "null", " is returned.",
     PARA,
     SeeAlso => {"#", "classes and types", "installMethod", "youngest"}
     }
document {
     Key => installMethod,
     Headline => "install methods",
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
     SeeAlso =>{"#", "lookup",  "new", "classes and types"}
     }
document {
     Key => "new",
     Headline => "new objects of various types",
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
     PARA,
     "The class ", TT "A", " should be a type of type, which means that
     ", TT "Type", " is an ancestor of ", TT "A", " and of the class of ", TT "A", ".",
     HR,
     NOINDENT,
     TT "new A of b", " -- make a hash table of class ", TT "A", "
     and parent ", TT "b", ".",
     PARA,
     "Same as above, except ", TT "c", " is missing.  Use ",
     PRE "          new AA of B := (A,b) -> ... ",
     "to install the initialization routine.",
     PARA,
     "The class ", TT "A", " should be a type of type, which means that
     ", TT "Type", " is an ancestor of ", TT "A", " and of the class of ", TT "A", ".",
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
     PARA,
     "The class ", TT "A", " should be a type, which means that
     ", TT "Type", " is an ancestor of the class of ", TT "A", ".",
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
     "The class ", TT "A", " should be a type, which means that
     ", TT "Type", " is an ancestor of the class of ", TT "A", ".",
     HR,
     "Note that if the ", TT "of", " option is not used, then the class ", TT "A", "
     need not consist of hash tables or lists.  We are using this feature by
     installing a method so that ", TT "new ZZ", " returns an integer popped
     from the top of the engine's stack.",
     PARA,
     "The symbols ", TO "NewMethod", ", ", TO "NewOfMethod", ", ", 
     TO "NewFromMethod", ", and ", TO "NewOfFromMethod", " are used internally
     for installation of the initialization routines.",
     SeeAlso => {"classes and types"}
     }
document {
     Key => "of",
     Headline => "a keyword",
     TT "of", " -- a keyword used with ", TO "new", "."
     }
document {
     Key => NewMethod,
     TT "NewMethod", " -- a symbol used as a method name in conjunction with
     the ", TO "new", " operator.",
     PARA,
     "Intended for internal use only."
     }
document {
     Key => NewOfMethod,
     TT "NewOfMethod", " -- a symbol used as a method name in conjunction with
     the ", TO "new", " operator.",
     PARA,
     "Intended for internal use only."
     }
document {
     Key => NewFromMethod,
     TT "NewFromMethod", " -- a symbol used as a method name in conjunction with
     the ", TO "new", " operator.",
     PARA,
     "Intended for internal use only."
     }
document {
     Key => NewOfFromMethod,
     TT "NewOfFromMethod", " -- a symbol used as a method name in conjunction with
     the ", TO "new", " operator.",
     PARA,
     "Intended for internal use only."
     }
document {
     Key => Thing,
     Headline => "the class of all things",
     "Everything in Macaulay 2 is a ", ITALIC "thing", ".  This 
     includes numbers, strings, and lists.  More complicated things such as 
     polynomials, groups, rings, and chain complexes are implemented
     as ", ITALIC "hash tables", ".  See ", TO "Type", " for information 
     about what types of things there are."
     }
document {
     Key => Nothing,
     Headline => "the empty class",
     "This class is useful for representing the class of an argument
     which is missing.  It is also used as the parent for those things which
     are not themselves types, i.e., which do not have instances." 
     }
document {
     Key => Option,
     Headline => "the class of all pairs x => y",
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
     SeeAlso => {"classes and types", "=>"}
     }
document {
     Key => (NewFromMethod, HashTable, List),
     Headline => "make a hash table from a list",
     TT "new HashTable from x", " -- produce a new hash table from a
     list ", TT "x", ".",
     PARA,
     "Elements of ", TT "x", " which are options, ", TT "k => v", " cause
     the value ", TT "v", " to be stored in ", TT "x", " under the key ", TT "k", ".",
     SeeAlso => "hashTable"
     }
document {
     Key => OptionTable,
     Headline => "the class of hash tables for optional arguments",
     SeeAlso => ">>" }
document {
     Key => (symbol ">>", List, Function),
     Headline => "attaching options to a function",
     "See ", TO (symbol ">>", OptionTable, Function), " for details."
     }
document {
     Key => (symbol ">>", OptionTable, Function),
     Headline => "attaching options to a function",
     Usage => "g = defs >> fun",
     Inputs => {
	  "defs" => { 
	       "a hash table whose keys are the names of the optional arguments, and whose values are the
	       corresponding default values"},
	  "fun" => { "a function that expects optional arguments" }
	  },
     Outputs => {
	  "g" => { "a new function that pre-processes the optional arguments and then calls ", TT "fun" }
	  },
     PARA {
	  "The new function ", TT "g", " works as follows.
	  The value of ", TT "g args", ", say, is obtained by evaluation of 
	  ", TT "(fun opts)(args')", ", where ", TT "args'", " is obtained from
	  ", TT "args", " by removing the options of the form ", TT "X=>A", " 
	  (where ", TT "X", " is a name of an optional argument), and ", TT "opts", " 
	  is a hash table of the same form as ", TT "defs", " in which the default
	  values have been replaced by the user-supplied values, e.g., the
	  value stored under the key ", TT "X", " has been replaced by
	  ", TT "A", "."},
     PARA { "Remark: ", TT "defs", " can also be simply a list of options." },
     PARA {
     	  "In the following example we use a simple definition for ", TT "fun", "
     	  so we can see everything that ", TT "fun", " receives."},
     EXAMPLE {
	  "g = {a=>1, b=>2} >> opts -> args -> {args, opts}",
	  "g x",
	  "g(x,y,b=>66)",
	  "g(t,u,a=>44,b=>77)",
	  },
     SeeAlso => {"making new functions with optional arguments", "OptionTable", "Option", "=>"}
     }
document {
     Key => [method, SingleArgumentDispatch],
     Headline => "method functions with a variable number of arguments",
     Usage => "f = method(SingleArgumentDispatch => true)",
     Inputs => {
	  },
     Outputs => {
	  "f" => "a method function that treats several arguments as a single argument, i.e., as a sequence."
	  },
     "Here is an example.",
     EXAMPLE {
	  "f = method(SingleArgumentDispatch=>true);",
	  "f ZZ := i -> -i;",
	  "f Sequence := S -> reverse S;",
	  "f 44",
	  "f(3,4,5,6)"
	  },
     "Normally, at most three arguments could be handled by such a method
     function, and the types would have to be considered separately."
     }
document {
     Key => symbol "typicalValues",
     Headline => "types of values returned by functions",
     "A hash table used to store information about the type of values
     typically returned by functions and methods.",
     PARA,
     "This information is used only to build documentation automatically.",
     EXAMPLE "typicalValues#isRing",
     SeeAlso => { "specifying typical values" }
     }
document {
     Key => [method,TypicalValue],
     Headline => "specify return value type",
     TT "TypicalValue => X", " -- an option to ", TO "method", "
     which specifies that values returned by the method function will
     typically be of type ", TT "X", ".",
     PARA,
     "This information is used only to build documentation automatically, and
     is stored in the hash table ", TO "typicalValues", ".",
     SeeAlso => { "specifying typical values" }
     }
document {
     Key => method,
     Headline => "make a new method function",
     TT "f = method()", " -- creates a method function",
     PARA,
     "The code above creates a method function which takes up to three 
     arguments, looking up the appropriate method according to the classes of 
     the arguments, with inheritance.  To install a method for two arguments,
     (x,y), of classes X and Y, use code like this:",
     PRE "     f(X,Y) := (x,y) -> ...",
     "where '...' represents the body of the function you wish to install.
     The syntax for one or three arguments is analogous.  For a single
     argument ", TT "x", " of class ", TT "X", ", one could omit the parentheses, and write:",
     PRE "     f X := (x) -> ...",
     "the effect of which happens to be the same as that of",
     PRE "     X#f := (x) -> ...",
     PARA,
     SeeAlso => {"methods" }
     }
document {
     Key => [method,Associative],
     Headline => "allows associative methods to be created",
     NOINDENT,
     TT "f = method(Associative=>true)", " -- creates an associative
     method which will call upon the appropriate binary methods for its arguments 
     two at a time.",
     PARA,
     "In the following example we install a method which isn't associative
     to illustrate the order of evaluation.",
     EXAMPLE {
	  ///f = method(Associative => true)///,
	  ///f(String,String) := (i,j) -> "(" | i | ")," | j;///,
      	  ///f("a","b","c","d")///,
	  }
     }
document {
     Key => size,
     Headline => "the size of an object",
     TT "size x", " -- returns the size of ", TT "x", " which usually gives
     a rough indication of memory space required to store the object ", TT "x", ".",
     PARA,
     "For a polynomial, the size is the number of terms.",
     PARA,
     "This function should be replaced by something more generally useful."
     }
document {
     Key => baseName,
     Headline => "the base name of a generator",
     TT "baseName x", " -- returns the variable or symbol upon which a generator of a
     monoid or polynomial ring is based."
     }
document {
     Key => degree,
     Headline => "the degree",
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
document {
     Key => degreeLength,
     Headline => "the number of degrees",
     TT "degreeLength x", " -- returns the number of degrees of x.",
     PARA,
     "Here x may be a ring, in which case it returns the number of degrees
     (the length of the degree vector) used in grading the ring.",
     SeeAlso => "degree"
     }
document {
     Key => isIsomorphism,
     Headline => "whether a map is an isomorphism",
     TT "isIsomorphism f", " -- whether the map f of modules is an isomorphism."
     }
document {
     Key => leadCoefficient,
     Headline => "the leading coefficient",
     TT "leadCoefficient f", " -- return the leading coefficient of the polynomial
     or vector ", TT "f", ".",
     PARA,
     SeeAlso => {"leadTerm", "leadMonomial", "leadComponent"}
     }
document {
     Key => leadComponent,
     Headline => "the leading component of a vector",
     TT "leadComponent f", " -- return the leading component of the vector f,
     i.e., the integer i so that f_i is the first nonzero component of f.",
     PARA,
     SeeAlso => {"leadTerm", "leadCoefficient", "leadMonomial"}
     }
document {
     Key => leadMonomial,
     Headline => "the leading monomial",
     TT "leadMonomial f", " -- return the leading monomial of the polynomial
     or vector f.",
     PARA,
     SeeAlso => {"leadTerm", "leadCoefficient", "leadCoefficient"}
     }
document {
     Key => flatten,
     Headline => "flatten a list by unnesting lists",
     TT "flatten m", " -- produces a new list from ", TT "m", " by
     effectively removing the braces surrounding the elements
     of any elements of m which happen to be lists.  Also works
     for matrices.",
     PARA,
     EXAMPLE "flatten {{2,3,4},{{5}},6}"
     }
document {
     Key => (flatten,Matrix),
     Headline => "puts the columns of a matrix into a single row",
	Usage => "g = flatten f",
	Inputs => {
		"f" => {"a ", TT "m", " by ", TT "n", " matrix."}
		},
	Outputs => {
		"g" => { "a ", TT "1", " by ", TT "m*n", " matrix."}
		},
     TT "flatten f", " produces a new matrix from ", TT "f", " by placing the entries of the columns of ", TT "f", " all in a single row, one after the other.",
     EXAMPLE {
		"R = ZZ/101[x,y,z];",
		"f = matrix {{2, x},{y^2, 23},{z, z^3}}",
		"flatten f"
		},
	"Note that this is the matrix given by unnesting the list which was used to enter the matrix.",
     }
document {
     Key => image,
     Headline => "image of a map",
     TT "image h", " -- yields the image of the homomorphism ", TT "h", ".",
     PARA,
     "The result will be a submodule of the target of h",
     PARA,
     "If h is a ring element, it is interpreted as a one by one matrix."
     }
document {
     Key => Hom,
     Headline => "module of homomorphisms",
     TT "Hom(M,N)", " -- constructs the module of homomorphisms from M to N.",
     PARA,
     "Implemented with a method of the same name.",
     PARA,
     "Use ", TO "homomorphism", " to convert an element of the module of
     homomorphisms to a matrix."
     }
     
document {
     Key => scanKeys,
     Headline => "apply a function to each key in a hash table or database",
     TT "scanKeys(x,f)", " -- apply the function ", TT "f", " to each key used in the
     hash table or database ", TT "x", ".",
     PARA,
     "This function requires an immutable hash table.  To scan the keys in
     a mutable hash table, use ", TT "scan(keys x, f)", "."
     }
document {
     Key => scanValues,
     Headline => "apply a function to each value in a hash table",
     TT "scanValues(x,f)", " -- apply the function ", TT "f", " to each value
     appearing in the hash table ", TT "x", ".",
     PARA,
     "This function requires an immutable hash table.  To scan the values in
     a mutable hash table, use ", TT "scan(values x, f)", "."
     }
document {
     Key => GlobalAssignHook,
     Headline => "hook for assignment to global variables",
     Usage => "X.GlobalAssignHook = f",
     Inputs => {
	  "X" => Type => "",
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
	  ///RR.GlobalReleaseHook = (sym,val) -> << concatenate (
     "assigning ", toString val, " to ", toString sym
     ) << endl///,
          "a=4.5",
      	  "a=5.4",
	  },
     SeeAlso => "GlobalAssignHook"
     }
document {
     Key => complete,
     TT "complete C", " -- completely fills out the chain complex C by
     calling upon the engine to provide the maps and modules computed
     by ", TO "resolution", ".",
     PARA,
     "This is mainly intended for developers of new routines for chain
     complexes which have to make use of their internal structure.
     Before running this routine, it is not possible to determine which
     spots in a chain complex are actually occupied by modules or maps."
     }
document {
     Key => (drop, BasicList, List),
     Usage => "drop(v,{m,n})",
     Inputs => {
	  "v" => null,
	  "{m,n}" => "a pair of natural numbers"
	  },
     Outputs => {
	  { "the list by omitting those elements of the list ", TT "v", " in positions ", TT "m", " through ", TT "n", "." }
	  },
     EXAMPLE "drop({a,b,c,d,e},{2,4})"
     }
document {
     Key => (drop, BasicList, ZZ),
     Usage => "w = drop(v,n)",
     Inputs => {
	  "v" => null,
	  "n" => null
	  },
     Outputs => {
	  {"the list obtained by omitting the first ", TT "n", " elements of the list ", TT "v", " if ", TT "n", " is positive, or
	       the last ", TT "-n", " elements if ", TT "n", " is negative."}
	  },
     EXAMPLE { "drop({a,b,c,d,e},2)", "drop({a,b,c,d,e},-2)" }
     }
document {
     Key => drop,
     Headline => "drop some elements", 
     SeeAlso => "take" }
document {
     Key => (options, Function),
     Headline => "get optional arguments and default values for a function which accepts them",
     Usage => "options f",
     Inputs => {
	  "f" => ""
	  },
     Outputs => {
	  { "a hash table whose keys are the names of the optional arguments accepted by the function ", TT "f", " and whose values are the corresponding default values" }
	  },
     EXAMPLE {
	  "options res",
	  }
     }
document {
     Key => (options, Ring),
     Headline => "get values used for optional arguments",
     TT "options R", " -- returns the options used when the polynomial
     ring ", TT "R", " was created."
     }
document {
     Key => (options, Monoid),
     Headline => "get values used for optional arguments",
     TT "options M", " -- returns the options used when the monoid ", TT "M", " 
     was created."
     }
document {
     Key => options, Headline => "get options" }
document {
     Key => (symbol <<, Nothing, Thing),
     Headline => "dummy file output",
     "null << x", " -- does nothing and returns ", TO "null", ".",
     PARA,
     "The intention here is that you can use ", TO "null", " as a dummy
     output file, but a lot of time may be wasted converting ", TT "x", " to
     a net."
     }
document {
     Key => mathML,
     Headline => "convert to MathML format",
     TT "mathML x", " -- converts ", TT "x", " to MathML form.",
     PARA,
     EXAMPLE {
	  "R = ZZ[x,y];",
	  "mathML matrix {{x,y},{x^2+2,0}}"
	  },
     SeeAlso => "hypertext"
     }
document {
     Key => symbol "#",
     Headline => "length, or access to elements",
     "The precedence of ", TT "#", " when used as a binary operator is high,
     as high as ", TT ".", ", but the precedence when used as a unary operator
     lower, as low as adjacency or function application.",
     SeeAlso =>{ "#?" }
     }
document {
     Key => (symbol #, BasicList),
     Headline => "length",
     TT "#x", " -- provides the length of a list.",
     }
document {
     Key => (symbol #, Sequence),
     Headline => "length",
     TT "#x", " -- provides the length of a sequence.",
     }
document {
     Key => (symbol #, HashTable),
     Headline => "length",
     TT "#x", " -- provides the number of key-value pairs recorded
     in a hash table.",
     }
document {
     Key => (symbol #, Set),
     Headline => "cardinality",
     TT "#x", " -- provides the number of elements in the set ", TT "x", "."
     }
document {
     Key => (symbol #, String),
     Headline => "length",
     TT "#x", " -- provides the length of a string.",
     }
document {
     Key => (symbol #, File),
     Headline => "length",
     TT "#x", " -- provides the length of a file.",
     }
document {
     Key => (symbol #, HashTable, Thing),
     Headline => "get value from hash table",
     TT "x#i", " -- provides the value associated to the key ", TT "i", " in the hash table
     ", TT "x", ".",
     PARA,
     "Assignment to ", TT "x#i", " can change the value if ", TT "x", " is mutable.",
     EXAMPLE {
	  "x = new MutableHashTable",
	  "x#i = p",
	  "x#i",
	  },
     SeeAlso => {(symbol #?, HashTable, Thing), "hashing"}
     }
document {
     Key => (symbol #, Database, String),
     Headline => "get value from database",
     TT "x#i", " -- provides the value associated to the key ", TT "i", " in the database
     ", TT "x", ".",
     SeeAlso => {(symbol #?, Database, String)}
     }
document {
     Key => (symbol #, String, ZZ),
     Headline => "get character from string",
     TT "x#i", " -- provides the ", TT "i", "-th character of the string ", TT "x", ",
     as a string of length 1, if there is one.",
     PARA,
     "If ", TT "i", " is out of range, a string of length 0 is returned.
     If  ", TT "i", " is negative, then the ", TT "i", "-th character
     from the end is provided.",
     SeeAlso => {(symbol #?, String, ZZ)}
     }
document {
     Key => (symbol #, BasicList, ZZ),
     Headline => "get element from list",
     TT "x#i", " -- provides the ", TT "i", "-th element of the list ", TT "x", ".",
     PARA,
     "If ", TT "i", " is out of range, an error is signaled. If  ", TT "i", " 
     is negative, then the ", TT "i", "-th entry counting from the end is provided.",
     PARA,
     "Assignment to ", TT "x#i", " can change the value if ", TT "x", " is mutable."
     }
document {
     Key => (symbol #, Sequence, ZZ),
     Headline => "get element from sequence",
     TT "x#i", " -- provides the ", TT "i", "-th element of the sequence ", TT "x", ".",
     PARA,
     "If ", TT "i", " is out of range, an error is signaled. If  ", TT "i", " 
     is negative, then the ", TT "i", "-th entry counting from the end is provided."
     }
document {
     Key => (symbol #?, HashTable, Thing),
     Headline => "check for value in hash table",
     TT "x#?i", " -- tells whether there is a value associated to the
     key ", TT "i", " stored in the hash table ", TT "x", ".",
     SeeAlso => {(symbol #, HashTable, Thing), "hashing"}
     }
document {
     Key => (symbol #?, Database, String),
     Headline => "check for value in database",
     TT "x#?i", " -- tells whether there is a value associated to the string
     ", TT "i", " in the database ", TT "x", ".",
     SeeAlso => {(symbol #, Database, String)}
     }
document {
     Key => (symbol #?, String, ZZ),
     Headline => "check for character in string",
     TT "x#?i", " -- tells whether there is an ", TT "i", "-th character in
     the string ", TT "x", ".",
     EXAMPLE {
	  ///"asdf" #? 2///,
	  ///"asdf" #? 22///
	  },
     SeeAlso => {(symbol #, String, ZZ)}
     }
document {
     Key => (symbol #?, BasicList, ZZ),
     Headline => "check for element in list",
     TT "x#?i", " --  tells whether there is an ", TT "i", "-th element in
     the list ", TT "x", ".",
     EXAMPLE {
	  ///{a,b,c} #? 2///,
	  ///{a,b,c} #? 22///
	  },
     SeeAlso => {(symbol #, BasicList, ZZ)}
     }
document {
     Key => (symbol #?, Sequence, ZZ),
     Headline => "check for element in sequence",
     TT "x#?i", " --tells whether there is an ", TT "i", "-th element in
     the sequence ", TT "x", ".",
     SeeAlso => {(symbol #, Sequence, ZZ)}
     }
document {
     Key => symbol "#?",
     Headline => "check for presence of elements",
     SeeAlso =>{ "#" }
     }
document {
     Key => symbol "_",
     "A binary operator which is used for various mathematical operations
     that are customarily written with subscripts."
     }
document {
     Key => (symbol _, List, ZZ),
     Headline => "get element from list",
     TT "x_i", " -- provides the ", TT "i", "-th element of the list ", TT "x", ".",
     PARA,
     "This is merely a convenient synonym for ", TT "x#i", ".",
     PARA,
     SeeAlso => {(symbol #, BasicList, ZZ)}
     }
document {
     Key => (symbol _, Sequence, ZZ),
     Headline => "get element from list",
     TT "x_i", " -- provides the ", TT "i", "-th element of the sequence ", TT "x", ".",
     PARA,
     "This is merely a convenient synonym for ", TT "x#i", ".",
     SeeAlso => {(symbol #, BasicList, ZZ)}
     }
document {
     Key => ".",
     Headline => "access to elements whose key is a symbol",
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
     SeeAlso => {"#", ".?", "global"}
     }
document {
     Key => ".?",
     Headline => "check for presence of elements whose key is a symbol",
     TT "x.?k", " -- the same as ", TT "x#?(global k)", ", tells whether a value is
     available with ", TT "x.k", ".",
     PARA,
     SeeAlso =>{ ".", "#?" }
     }
document {
     Key => autoload,
     Headline => "arrange for a function to be loaded automatically",
     TT "autoload(f,\"x\")", " -- arranges for a function ", TT "f", " to be 
     automatically loaded from the file named ", TT "x", " the first
     time it is used."
     }


TEST ///
     k = ZZ/101
     R = k[a,b,c,d]/(a^4+b^4+c^4+d^4)
     X = Proj R
     result = table(3,3,(p,q) -> timing ((p,q) => rank HH^q(cotangentSheaf(p,X))))
     assert( {{1, 0, 1}, {0, 20, 0}, {1, 0, 1}} === applyTable(result,last@@last) )
     print new MatrixExpression from result
     ///

-- Example 4.1: the bounds can be sharp.
TEST ///
     S = QQ[w,x,y,z];
     X = Proj S;
     I = monomialCurveIdeal(S,{1,3,4})
     N = S^1/I;
     assert(Ext^1(OO_X,N~(>= 0)) == prune truncate(0,Ext^1(truncate(2,S^1),N)))
     assert(Ext^1(OO_X,N~(>= 0)) != prune truncate(0,Ext^1(truncate(1,S^1),N)))
     ///

-- Example 4.2: locally free sheaves and global Ext.
TEST ///
     S = ZZ/32003[u,v,w,x,y,z];
     I = minors(2,genericSymmetricMatrix(S,u,3));
     X = variety I;
     R = ring X;
     Omega = cotangentSheaf X;
     OmegaDual = dual Omega;
     assert(Ext^1(OmegaDual, OO_X^1(>= 0)) == Ext^1(OO_X^1, Omega(>= 0)))
     ///

-- Example 4.3: Serre-Grothendieck duality.
TEST ///
     S = QQ[v,w,x,y,z];
     X = variety ideal(w*x+y*z,w*y+x*z);
     R = ring X;
     omega = OO_X^{-1};
     G = sheaf cokernel genericSymmetricMatrix(R,R_0,2);
     assert(Ext^2(G,omega) == dual HH^0(G))
     assert(Ext^1(G,omega) == dual HH^1(G))
     assert(Ext^0(G,omega) == dual HH^2(G))
     ///


-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
