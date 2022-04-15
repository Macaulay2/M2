document {
     Key => "new",
     Headline => "new objects and new types",
     PARA {
	  "In this reference section we discuss how to make new types of object and new objects of those types."
	  },
     SYNOPSIS (
	  Heading => "installing a new method for new-of-from",
	  Usage => "new AA of BB from C := (A,B,c) -> ...",
	  Inputs => {
	       "AA" => HashTable,
	       "BB" => HashTable,
	       "C" => HashTable,
	       { TT "(A,B,c) -> ...", ", a function of 3 arguments: ", TT "AA", " will be an ancestor of ", TT "A", ",
		    ", TT "BB", " will be an ancestor of ", TT "B", ", and ", TT "C", " will be an ancestor of the class of ", TT "c", ".
		    Alternatively, ", TT "A", " will be a type of ", TT "AA", ", ", TT "B", " will be a type of ", TT "BB", ", and ", TT "c", " will be an instance of ", TT "C", "." }
	       },
	  Consequences => {
	       { "the function will be installed as the method for ", TT "new AA of BB from C", ".  It will be stored under the key ", TT "(NewOfFromMethod,AA,BB,C)", "
		    in the youngest of the hash tables ", TT "AA", ", ", TT "BB", ", and ", TT "C", "." }
	       },
	  Outputs => {
	       { "the function is returned as the value of the expression" }
	       },
	  PARA { "In this example we install a creation method for new types of types from functions.  The function is used to construct the method for converting
	       instances of the new type to nets for display."
	       },
	  EXAMPLE lines ///
	       new Type of BasicList from Function := (A,B,f) -> hashTable { net => f, html => f }; -* no-capture-flag *-
	  ///,
	  PARA {
	       "The hash tables ", TT "AA", ", ", TT "BB", ", and ", TT "C", " will normally be instances of ", TO "Type", "."
	       }
	  ),
     SYNOPSIS (
	  Heading => "new-of-from",
	  Usage => "new A of B from c",
	  Inputs => {
	       "A" => HashTable,
	       "B" => HashTable,
	       "c" => { "an instance of ", TT "C" }
	       },
	  Consequences => {
	       { "the function previously installed as the method for ", TT "new A of B from C", " will be called with arguments ", TT "(A,B,c)", "." },
	       { "if no such method has been installed, then ancestors of ", TT "A", ", ", TT "B", ", and ", TT "C", ", will be consulted,
		    searching lexicographically for a method; see ", TO "inheritance", "." },
	       { "if no method is found by searching the ancestors, then the function ", TT "(A,B,c) -> c", " will be used" },
	       { "the value returned by the function, (or ", TT "c", ", if no method was found), will have
		    its class set to ", TT "A", " and its parent set to ", TT "B", "; see ", TO "newClass", "."
		    },
	       },
	  Outputs => {
	       { "the new object of class ", TT "A", " and with parent ", TT "B", " described above" }
	       },
	  PARA "We use the creation method installed above to create a new type of list.",
	  EXAMPLE lines ///
	       f = s -> "--list of type X--"
	       X = new Type of List from f
	       class X
	       parent X
	       peek X
	  ///,
	  PARA { "Now we use ", TO "new", " to create a new list of type ", TT "X", " from a list.  The system knows how to convert lists to lists of type ", TT "X", ", so no creation
	       method need be installed for ", TT "new X from List", "."
	       },
	  EXAMPLE lines ///
	       x = new X from {1,3,11,12}
	       class x
	       parent x
	       peek x
	  ///
	  ),
     SYNOPSIS (
	  Heading => "installing a new method for new-of",
	  Usage => "new AA of BB := (A,B) -> ...",
	  Inputs => {
	       "AA" => HashTable,
	       "BB" => HashTable,
	       { TT "(A,B) -> ...", ", a function of 2 arguments: ", TT "AA", " will be an ancestor of ", TT "A", ",
		    and ", TT "BB", " will be an ancestor of ", TT "B", ".
		    Alternatively, ", TT "A", " will be a type of ", TT "AA", ", and ", TT "B", " will be a type of ", TT "BB", "." }
	       },
	  Consequences => {
	       { "the function will be installed as the method for ", TT "new AA of BB", ".  It will be stored under the key ", TT "(NewOfMethod,AA,BB)", "
		    in the younger of the hash tables ", TT "AA", " and ", TT "BB", "." }
	       },
	  Outputs => {
	       { "the function is returned as the value of the expression" }
	       },
	  PARA {
	       "This operation turns out to be needed infrequently, because there is no ", TT "from", "-clause to provide data for initializing the instance of ", TT "A", "."
	       },
	  EXAMPLE ///new Type of BasicList := (type,array) -> (
		    stderr << "--new " << type << " of "
			   << array << " being made" << endl;
		    new MutableHashTable)///
	  ),
     SYNOPSIS (
	  Heading => "new-of",
	  Usage => "new A of B",
	  Inputs => {
	       "A" => HashTable,
	       "B" => HashTable,
	       },
	  Consequences => {
	       { "the function previously installed as the method for ", TT "new A of B", " will be called with arguments ", TT "(A,B)", "." },
	       { "if no such method has been installed, then ancestors of ", TT "A", " and ", TT "B", " will be consulted,
		    searching lexicographically for a method; see ", TO "inheritance", "." },
	       { "the value returned by the function will have its class set to ", TT "A", " and its parent set to ", TT "B", "; see ", TO "newClass", "." },
	       { "if no method is found by searching the ancestors, then a new empty instance of ", TT "A", " with parent ", TT "B", " will be created" }
	       },
	  Outputs => {
	       { "the new object of class ", TT "A", " and parent ", TT "B", " described above" }
	       },
	  PARA {
	       "We illustrate this operation by making a new type of basic list, and then by making a list of that type."
	       },
	  EXAMPLE lines ///
	       M = new Type of BasicList
	       m = new M from {3,4,5}
	       class m
	       m#1
	  ///,
	  PARA {
	       "Now let's define a method for reversing the elements of a list of class ", TT "M", ", using the unary operator ", TT "-", "."
	       },
	  EXAMPLE lines ///
	       - M := reverse
	       - m
	  ///
	  ),
     SYNOPSIS (
	  Heading => "installing a new method for new-from",
	  Usage => "new AA from C := (A,c) -> ...",
	  Inputs => {
	       "AA" => HashTable,
	       "C" =>Type,
	       { TT "(A,c) -> ...", ", a function of 2 arguments: ", TT "AA", " will be an ancestor of ", TT "A", ",
		    and ", TT "C", " will be an ancestor of the class of ", TT "c", ".
		    Alternatively, ", TT "A", " will be a type of ", TT "AA", " and ", TT "c", " will be an instance of ", TT "C", "." }
	       },
	  Consequences => {
	       { "the function will be installed as the method for ", TT "new AA from C", ".  It will be stored under the key ", TT "(NewFromMethod,AA,C)", "
		    in the younger of the hash tables ", TT "AA", " and ", TT "C", "." }
	       },
	  Outputs => {
	       { "the function is returned as the value of the expression" }
	       },
	  PARA {
	       "Let's use the class ", TT "M", " defined above, and introduce a method for creating lists of class ", TT "M", " from integers.  Then we use it
	       in the subsection below."
	       },
	  EXAMPLE lines ///
	       new M from ZZ := (M',i) -> 0 .. i
	  ///
	  ),
     SYNOPSIS (
	  Heading => "new-from",
	  Usage => "new A from c",
	  Inputs => {
	       "A" => HashTable,
	       "c" => { "an instance of ", TT "C" }
	       },
	  Consequences => {
	       { "the function previously installed as the method for ", TT "new A from C", " will be called with arguments ", TT "(A,c)", "." },
	       { "if no such method has been installed, then ancestors of ", TT "A", " and ", TT "C", ", will be consulted, searching
		    lexicographically for a method; see ", TO "inheritance", "." },
	       { "if no method is found by searching the ancestors, then the function ", TT "(A,c) -> c", " will be used" },
	       { "the value returned by the function, (or ", TT "c", ", if no method was found), will have
		    its class set to ", TT "A", " and its parent retained; see ", TO "newClass" }
	       },
	  Outputs => {
	       { "the new object of class ", TT "A", " initialized from ", TT "c", " described above" }
	       },
	  PARA {
	       "We use the new-from method for ", TT "new M from ZZ", " installed above."
	       },
	  EXAMPLE lines ///
	       n = new M from 13
	       - n
	  ///
	  ),
     SYNOPSIS (
	  Heading => "installing a new method for new",
	  Usage => "new AA := (A) -> ...",
	  Inputs => {
	       "AA" => HashTable,
	       { TT "(A) -> ...", ", a function of 1 argument: ", TT "AA", " will be an ancestor of ", TT "A", ".
		    Alternatively, ", TT "A", " will be a type of ", TT "AA", "." }
	       },
	  Consequences => {
	       { "the function will be installed as the method for ", TT "new AA", ".  It will be stored under the key ", TT "NewMethod", "
		    in the hash table ", TT "AA", "." }
	       },
	  Outputs => {
	       { "the function is returned as the value of the expression" }
	       },
	  PARA {
	       "We use the class ", TT "M", " introduced above, and install a method for ", TT "new M", ", and we use it in the next subsection."
	       },
	  EXAMPLE lines ///
	       new M := (M') -> {"a","b","c"}
	       ///
	  ),
     SYNOPSIS (
	  Heading => "new",
	  Usage => "new A",
	  Inputs => { "A" => HashTable },
	  Consequences => {
	       { "the function previously installed as the method for ", TT "new A", " will be called with argument ", TT "A", "." },
	       { "if no such method has been installed, then ancestors of ", TT "A", " will be consulted, searching for a method; see ", TO "inheritance", "." },
	       { "the value returned by the function will have its class set to ", TT "A", "; see ", TO "newClass" },
	       { "if no method is found by searching the ancestors, then a new empty instance of ", TT "A", " will be created, if possible" },
	       },
	  Outputs => {
	       { "the new object of class ", TT "A", " described above" }
	       },
	  PARA {
	       "We use the method for ", TT "new M", " installed above."
	       },
	  EXAMPLE lines ///
	       new M
	  ///
	  ),
     Subnodes => {
	  TO "newClass"
	  }
     }

document {
     Key => newClass,
     Headline => "set the class and parent of an object",
     SYNOPSIS (
	  Heading => "setting the class and parent",
	  Usage => "newClass(A,B,x)",
	  Inputs => { "A" => HashTable, "B" => HashTable, "x" },
	  Outputs => {{"a copy (possibly) of ", TT "x", " with ", TT "A", " as class and ", TT "B", " as parent"}},
	  ),
     SYNOPSIS (
	  Heading => "setting the class",
	  Usage => "newClass(A,x)",
	  Inputs => { "A" => HashTable, "x" },
	  Outputs => {{"a copy (possibly) of ", TT "x", " with ", TT "A", " as the new class"}},
	  ),
     SUBSECTION "common remarks",
     PARA{
	  "If ", TT "x", " is a basic list or sequence, then ", TO "BasicList", " should be an ancestor of ", TT "A", " and ", TT "B", " should be ", TO "Nothing", ".
	  If ", TT "x", " is a hash table, then ", TO "HashTable", " should be an ancestor of ", TT "A", "."
	  },
     PARA {
	  "If the class (and parent) of x are already equal to A (and B, respectively), then copying of the elements of ", TT "x", " is not required, and is not done."
	  },
     PARA{
	  "If ", TT "x", " is mutable, and instances of class ", TT "A", " are also mutable, then copying of the elements of ", TT "x", " is not required, and is not done."
	  },
     PARA{
	  "If ", TT "x", " is not a hash table, basic list, or sequence, then its class will be set to ", TT "A", " internally, essentially by wrapping it
	  in a special kind of object designed solely to indicate the new class.  The new class ", TT "A", " must be a specialization of the class of ", TT "x", ".
	  The parent cannot be reset this way.
	  Not all of the internal code of Macaulay2 is ready to recognize such wrapped objects, which are part of a new feature, except for the code that handles functions."
	  },
     EXAMPLE lines ///
	  t = 1..4
	  newClass(Array,t)
	  x = new HashTable from { a => 1, b => 2 }
	  z = newClass(ImmutableType,Vector,x)
	  parent z
     ///,
     PARA {
	  "The difference between ", TT "new A of B from x", " and ", TT "newClass(A,B,x)", " is that the methods installed for ", TO "new", " are not used."
	  },
     EXAMPLE {
	  ///new Thing of Thing from Thing := (A,B,c) -> ( -* no-capture-flag *-
       << "-- new " << A << " of " << B
       << " from " << toString c << endl;
       c);///,
	  "new ImmutableType of Vector from x",
	  "newClass(ImmutableType,Vector,x)"
	  },
     SeeAlso => { "new", "copy", "toList" }
     }

document {
     Key => parent,
     Headline => "parent type of an object",
     Usage => "X = parent x",
     Inputs => {
	  "x"
	  },
     Outputs => {
	  "X" => { "the parent class of ", TT "x" }
	  },
     "Methods for the instances of ", TT "X", " which are not found
     in ", TT "X", " itself are sought in ", TT "P", ", and its parent, and so on.
     Thus the mathematical notion of a set ", TT "P", " and a subset ", TT "X", "
     can modeled in this way.",
     PARA{},
     "Things that don't have instances have the empty class, called
     ", TO "Nothing", " as their parent.  The parent of ", TO "Thing", "
     is ", TO "Thing", " itself (because every thing is a thing).",
     PARA{},
     "The entire structure of types and their parents can be displayed
     with ", TO "showStructure", "."
     }

document {
     Key => {(ancestor, Type, Type),ancestor},
     Headline => "whether one type is an ancestor of another",
     Usage => "ancestor(x,y)",
     Inputs => { "x" => Type, "y" => Type },
     Outputs => { {"whether ", TT "x", " is an ancestor of ", TT "y"} },
     PARA {
	  "The ancestors of ", TT "y", " are ", TT "y", ", ", TT "parent y", ", ", TT "parent parent y", ", and so on."
	  },
     PARA {
	  "If ", TT "x", " is an ancestor of ", TT "y", ", then we also say that ", TT "y", " is a ", EM "specialization", " of ", TT "x", "."
	  },
     EXAMPLE lines ///
     parent String
     parent parent String
     parent parent parent String
     ///,
     SeeAlso => {ancestors}
     }

document {
     Key => ancestors,
     Headline => "the ancestors of something",
     Usage => "ancestors x",
     Inputs => {"x"},
     Outputs => {{"the list of ancestors of ", TT "x"}},
     EXAMPLE lines ///
     ancestors String
     ancestors class 3
     ancestors class 3.
     ancestors ring 3.
     ancestors class ring 3.
     ancestors 3
     ///,
     SeeAlso => {ancestor, showStructure}
     }

document {
     Key => Type,
     Headline => "the class of all types",
     "Everything in the system is classified, and the class that a thing
     belongs to is a type.  A type is implemented as a hash table containing
     method functions for its instances.",
     PARA{},
     "The list of types known to the system is displayed below."
     }

document {
     Key => Thing,
     Headline => "the class of all things",
     "Everything in Macaulay2 is a ", ITALIC "thing", ".  This
     includes numbers, strings, and lists.  More complicated things such as
     polynomials, groups, rings, and chain complexes are implemented
     as ", ITALIC "hash tables", ".  See ", TO "Type", " for information
     about what types of things there are."
     }
document {
     Key => Nothing,
     Headline => "the empty class",
     "This class is useful for representing the class of an argument
     that is missing.  It is also used as the parent for those things that
     are not themselves types, i.e., which do not have instances."
     }

document {
     Key => null,
     Headline => "the unique member of the empty class",
     "When it is the value of an expression entered into the interpreter, the
     output line doesn't appear.  Empty spots in a list are represented by
     it.",
     PARA{},
     "It is the only member of the class ", TO "Nothing", ", which strictly
     speaking, ought to have no members at all.",
     PARA{},
     "An ", TO "if", " expression with no ", TO "else", " clause returns
     ", TO "null", " when the predicate is false.",
     PARA{},
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
     Key => copy,
     Headline => "copy an object",
     TT "copy x", " yields a copy of x.",
     PARA{},
     "If x is an hash table, array, list or sequence, then the elements are
     placed into a new copy. If x is a hash table, the copy is mutable if
     and only if x is.",
     PARA{},
     "It is not advisable to copy such things as modules and rings,
     for: (1) the operations that have already been installed for them will return
     values in the original object, rather than in the copy; and (2) the copy
     operation is shallow, not copying keys and values that happen to be hash tables.",
     PARA{},
     SeeAlso => { "newClass" }
     }

document {
     Key => {instance,(instance, Thing, Type)},
     Headline => "whether something has a certain type",
     TT "instance(x,X)", " -- tells whether ", TT "x", " is an instance
     of the type ", TT "X", ".",
     PARA{},
     "We say that x is an instance of X if X is the class of x, or a parent
     of the class of x, or a grandparent, and so on.",
     PARA{},
     SeeAlso => { "class", "parent" }
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
     SeeAlso => {":="}
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

document { Key => ImmutableType,
     Headline => "the class of immutable types",
     "All types are implemented as hash tables.  Most types are mutable, so that additional methods for handling their instances can be added
     at any time.  However, if a type has an ancestor where the methods can be stored, then mutability is not needed.",
     PARA{},
     "When a type is used to represent a mathematical object, then immutability is desirable, in order to make the strict equality operator work on it.  For example, a
     module ", TT "M", " is a type, with its elements are its instances, but we would like to be able to compare two modules quickly, and form sets of modules.  This
     is possible, because we have implemented modules as immutable types, and we have put the methods for adding and subtracting elements of ", TT "M", " into the
     class ", TO "Vector", ".",
     EXAMPLE lines ///
	  F = ZZ^3
	  class F
	  parent class F
	  showStructure class F
	  showStructure F
	  v = F_0 + 3*F_2
	  F === ZZ^3
	  set (ZZ^3, ZZ^2, ZZ^3)
	  peek F
     ///,
     "Another advantage of immutability of modules is that there is no particular reason, aside from efficiency, to avoid creating a given module multiple times, as
     one copy of the module is as good as another.",
     EXAMPLE lines ///
	  ZZ^3_0 + ZZ^3_2
     ///,
     SeeAlso => {showStructure,parent,class}
     }

document {
     Key => serialNumber,
     Headline => "serial number of a dictionary, task, symbol, mutable hash table, or mutable list, ",
     Usage => "serialNumber x",
     Inputs => {"x"},
     Outputs => { ZZ => { "the serial number of ", TT "x" } },
     EXAMPLE lines ///
	  serialNumber asdf
	  serialNumber foo
	  serialNumber ZZ
     ///
     }

document { Key => "synonym",
     Headline => "synonym for members of a class",
     Usage => "synonym X",
     Inputs => { "X" => Type },
     Outputs => { String => {"a synonym for members of the class ", TT "X" }},
     "A synonym can be installed with the assignment statement ", TT "X.synonym=t", ".  The synonym is used by ", TO "ofClass", ".",
     EXAMPLE lines ///
	  synonym ZZ
	  Stack = new Type of HashTable
	  synonym Stack
	  Stack.synonym = "Deligne-Mumford stack"
	  ofClass Stack
     ///}


document {
     Key => {SelfInitializingType,
	  (symbol SPACE, SelfInitializingType, Thing)},
     Headline => "the class of all self initializing types",
     "A self initializing type ", TT "X", " will produce an instance of X from
     initial data ", TT "v", " with the expression ", TT "X v", ".",
     PARA{},
     EXAMPLE {
	  "X = new SelfInitializingType of BasicList",
	  "x = X {1,2,3}",
	  "class x",
	  },
     PARA{},
     TO "Command", " is an example of a self initializing type.",
     SeeAlso => {"HeaderType", "WrapperType"}
     }


document {
     Key => MutableHashTable,
     Headline => "the class of all mutable hash tables",
     PARA{},
     "A mutable hash table is a type of hash table whose entries can be changed.",
     PARA{},
     "Normally the entries in a mutable hash table are not printed, to prevent
     infinite loops in the printing routines.  To print them out, use
     ", TO "peek", ".",
     EXAMPLE {
	  "x = new MutableHashTable",
	  "scan(0 .. 30, i -> x#i = i^2)",
	  "x # 20",
	  "x #? 40",
	  },
     SeeAlso => "HashTable"
     }

document {
     Key => {hashTable,(hashTable, List)},
     Headline => "make a hash table",
     TT "hashTable(h,v)", " -- produce a hash table from a list ", TT "v", " of key-value pairs, with an optional collision handler function ", TT "h", ".",
     PARA{},
     "The pairs may be of the form ", TT "a=>b", ", ", TT "{a,b}", ",
     or ", TT "(a,b)", ".",
     PARA{},
     "Missing entries in the list, represented by ", TO "null", ", will be silently
     ignored.",
     PARA{},
     EXAMPLE {
	  "x = hashTable {a=>b, c=>d, }",
	  "x#a",
	  "hashTable(plus, {(a,3),(b,4),(a,10)})"
	  },
     }

document {
     Key => RingElement,
     Headline => "the class of all ring elements handled by the engine",
     SeeAlso => "engine"}
document {
     Key => EngineRing,
     Headline => "the class of rings handled by the engine",
     "The ", TO "engine", " handles most of the types of rings in the
     system.",
     PARA{},
     "The command ", TT "new Engine from x", " is not meant for general
     users, and provides the developers with a way to create top-level
     rings corresponding to rings implemented in the engine.  Here ", TT "x", "
     may be:",
     UL {
	  "commands for the engine, as a string, or a sequence or list
	  of strings, which cause a ring to be placed on the top of the
	  engine's stack.",
	  "a ring, in which case another top-level ring is formed as
	  an interface to the same underlying engine ring.",
	  "the handle of on engine ring"
	  }}
document {
     Key => FractionField,
     Headline => "the class of all fraction fields",
     "Macaulay2 provides for fraction fields of integral domains.",
     PARA{},
     "In some cases, normal forms of fractions makes sense, but in general
     for fraction fieldss of quotient rings, there is no notion of
     normal form for a fraction.
     In other words, fractions
     may be equal without displaying the same numerator and denominator.",
     PARA{},
     "Computations over fraction fields, or polynomial rings over fraction fields,
     especially Gröbner basis computations, are much slower than over prime fields.
     Still, an attempt is made to speed up these computations as much as possible, and
     more is planned in the future.",
     PARA{},
     "For an overview, see ", TO "fraction fields", " and  ", TO frac, ".",
     HEADER4 "Useful functions for use with fractions and fraction fields include:",
     UL {
	  TO frac,
	  TO numerator,
	  TO denominator,
	  TO liftable,
	  TO lift
	  }
     }
document {
     Key => ZZ,
     Headline => "the class of all integers" }

document {
     Key => QQ,
     Headline => "the class of all rational numbers",
     EXAMPLE "1/2 + 3/5"}

document {
     Key => RR,
     Headline => "the class of all real numbers",
     "A real number is entered as a sequence of decimal digits with a point.  It is stored internally
     as an arbitrary precision floating point number, using the ", TO "MPFR", " library.",
     EXAMPLE "3.14159",
     "The precision is measured in bits, is visible in the ring displayed on
     the second of each pair of output lines, and can be recovered using ", TO "precision", ".",
     EXAMPLE "precision 3.14159",
     "For real numbers, the functions ", TO "class", " and ", TO "ring", " yield different
     results.  That allows numbers of various precisions
     to be used without creating a new ring for each precision.",
     EXAMPLE {"class 3.1", "ring 3.1"},
     "The precision can be specified on input by appending the letter ", TT "p", " and a positive number.",
     EXAMPLE "3p300",
     "An optional exponent (for the power of ten to multiply by) can be specified on input
     by appending the letter ", TT "e", " and a number.",
     EXAMPLE {"3e3", "-3e-3", "-3p111e-3"},
     "Numbers that appear alone on an output line are displayed with all their meaningful digits.
     (Specifying 100 bits of precision yields about 30 decimal digits of precision.)",
     EXAMPLE {"1/3.","1/3p100", "100 * log(10,2)"},
     "Numbers displayed inside more complicated objects are printed with the number of digits
     specified by ", TO "printingPrecision", ".",
     EXAMPLE {"printingPrecision","{1/3.,1/3p100}"},
     "The notion of equality tested by ", TO "==", " amounts to equality of the internal binary digits.",
     EXAMPLE {".5p100 == .5p30", ".2p100 == .2p30"},
     "The notion of (strict) equality tested by ", TO "===", " also takes the precision into account.",
     EXAMPLE {".5p100 === .5p30", ".2p100 === .2p30"},
     "Perhaps surprisingly, the IEEE floating point standard also specifies that every number, including 0,
     has a sign bit, and strict equality testing takes it into account, as it must do, because some arithmetic
     and transcendental functions take it into account.",
     EXAMPLE lines ///
     0.
     -0.
     1/0.
     1/-0.
     log 0
     csc (0.)
     csc (-0.)
     ///,
     "Use ", TO "toExternalString", " to produce something that, when encountered as input, will reproduce
     exactly what you had before.",
     EXAMPLE lines ///
	  x = {1/3.,1/3p100}
	  x == {.333333, .333333}
	  y = toExternalString x
	  x === value y
     ///,
     "Transcendental constants and functions are available to high precision, with ", TO "numeric", ".",
     EXAMPLE lines ///
	  numeric pi
	  numeric_200 pi
	  Gamma oo
	  ///,
     SeeAlso => {toRR, numeric, precision, format, "printingPrecision", "printingAccuracy",
	  "printingLeadLimit", "printingTrailLimit", "printingSeparator",
	  "maxExponent", "minExponent"
	  }
     }

document {
     Key => CC,
     Headline => "the class of all complex numbers",
     "In Macaulay2, complex numbers are represented as floating point numbers, and so are
     only approximate.  The symbol ", TO "ii", " represents the square root of -1 in many numeric
     contexts.  A complex number is obtained by using the symbolic constant ", TO "ii", " or the conversion
     functions ", TO "toCC", " and ", TO "numeric", ", in combination with real numbers (see ", TO "RR", ").
     It is stored internally as a pair of arbitrary precision floating point real numbers, using
     the ", TO "MPFR", " library.",
     EXAMPLE {
	  "z = 3-4*ii",
	  "z^5",
	  "1/z",
	  "+ii",
	  "numeric_200 ii",
	  },
     "Complex numbers are ordered lexicographically, mingled with real numbers.",
     EXAMPLE {
	  "sort {1+ii,2+ii,1-ii,2-ii,1/2,2.1,7/5}"
	  },
     "The precision is measured in bits, is visible in the ring displayed on
     the second of each pair of output lines, and can be recovered using ", TO "precision", ".",
     EXAMPLE "precision z",
     "For complex numbers, the functions ", TO "class", " and ", TO "ring", " yield different
     results.  That allows numbers of various precisions
     to be used without creating a new ring for each precision.",
     EXAMPLE {"class z", "ring z"},
     "A computation involving numbers of different precisions has a result with the minimal precision occurring.
     Numbers that appear alone on an output line are displayed with all their meaningful digits.
     (Specifying 100 bits of precision yields about 30 decimal digits of precision.)",
     EXAMPLE "3p100+2p90e3*ii",
     "Numbers displayed inside more complicated objects are printed with the number of digits
     specified by ", TO "printingPrecision", ".",
     EXAMPLE {"printingPrecision","x = {1/3.*ii,1/3p100*ii}"},
     "Use ", TO "toExternalString", " to produce something that, when encountered as input, will reproduce
     exactly what you had before.",
     EXAMPLE lines ///
	  y = toExternalString x
	  value y === x
     ///,
     Caveat => { "Currently, most transcendental functions are not implemented for complex arguments." },
     SeeAlso => {"ii", toCC, toRR, numeric, precision, format, "printingPrecision", "printingAccuracy", "printingLeadLimit", "printingTrailLimit", "printingSeparator"}
     }

undocumented {RRi'}

document {
     Key => RRi,
     Headline => "the class of all real intervals",
     "A real interval is entered as a pair of real numbers to the interval function.  It is stored internally as an arbitrary precision interval using the ", TO "MPFI", " library.",
     EXAMPLE "interval(3.1415,3.1416)",
     "The precision is measured in bits, is visible in the ring displayed on
     the second of each pair of output lines, and can be recovered using ", TO "precision", ".",
     EXAMPLE "precision interval(3.1415,3.1416)",
     "For real intervals, the functions ", TO "class", " and ", TO "ring", " yield different
     results.  That allows numbers of various precisions
     to be used without creating a new ring for each precision.",
     EXAMPLE {"class interval(3.1,3.5)", "ring interval(3.1,3.5)"},
     "The precision can be specified on input by specifying the precision of both input ", TO "RR", " numbers.",
     "Alternatively, the precision can be specified by including the option ", TT "Precision", ".",
     EXAMPLE {"interval(2.5p100,3.2p1000)","interval(2.5,3.2,Precision=>200)"},
     "Intervals can also be created using ", TO (span,Sequence), " to create the smallest interval containing the inputs.",
     EXAMPLE {"span(2,Precision=>100)","span(2,3,interval(-1.5,-0.5),73)"},
     "Operations using intervals are computed as sets so that the resulting intervals contain all possible outputs from pairs of points in input intervals.",
     EXAMPLE {"interval(1,3)+interval(2,4)","interval(-1,1)*interval(2,3)","interval(0,1)-interval(0,1)","interval(1,2)/interval(1,2)"},
     "The notion of equality tested by ", TO "==", " amounts to checking the equality of the endpoints of intervals.",
     "The notion of equality tested by ", TO "===", " takes into account the precision of the inputs as well.",
     EXAMPLE {"interval(1,3) == interval(1,3,Precision=>100)","interval(1,3) === interval(1,3,Precision=>100)","interval(1/3,1,Precision=>100)==interval(1/3,1,Precision=>1000)"},
     "The notion of inequalities for intervals amounts to testing the inequality for all points in the intervals.  In particular, ",TO "<=", " is not the same as ",TO "<"," or ",TO "==",".",
    EXAMPLE {"interval(1,2)<=interval(2,3)","interval(1,2)<=interval(1,2)", "interval(1,2)<interval(2,3)","interval(1,2)<interval(3,4)"},
     "Transcendental functions on intervals produce intervals containing the image of the function on the interval.",
     EXAMPLE {"exp(interval(2,4))","cos(interval(1,1.3))","sqrt(interval(2))"},
     "Transcendental functions are available to high precision, with ", TO "numericInterval", ".",
    EXAMPLE {"numericInterval(100,pi)","numericInterval_200 EulerConstant"},
    SeeAlso => {toRRi, numericInterval, precision, interval, (span,Sequence), (span,List)}
	  }

document {
     Key => OrderedMonoid,
     Headline => "the class of all ordered monoids",
     "An ordered monoid is a multiplicative monoid together with an ordering of
     its elements.  The ordering is required to be compatible with the
     multiplication in the sense that if x < y then x z < y z.  The class
     of all ordered monomials is ", TO "OrderedMonoid", ".",
     PARA{},
     "The reason for making a separate class for ordered monoids is that monoid
     rings can be implemented more efficiently for them - an element of the
     monoid ring can be stored as a sorted list, each element of which is
     a pair consisting of an element of the monoid and a coefficient.
     See ", TO "PolynomialRing", ".",
     PARA{},
     "A free commutative ordered monoid can be created with ", TO "monoid", ".",
     SeeAlso =>  {"Monoid"}}
document {
     Key => PolynomialRing,
     Headline => "the class of all ordered monoid rings",
     "Every element of a polynomial ring is also a ", TO "RingElement", ".",
     SeeAlso => "polynomial rings"}
document {
     Key => IndexedVariable,
     Headline => "the class of all indexed variables",
     "Indexed variables provide the possibility of producing
     polynomial rings ", TT "R[x_0, x_1, ..., x_(n-1)]", " in n variables,
     where n is not known in advance.  If ", TT "x", " is an symbol,
     and i is an integer, then ", TT "x_i", " produces an indexed variable.
     After this has been done, an assignment ", TT "x_i=v", " will assign another
     value to it.  A new sequence of indexed variables of
     length n assigned to the symbol ", TT "x", " can be produced with ",
     TT "x_1 .. x_n", " and that sequence can be used in constructing
     a polynomial ring.",
     EXAMPLE {
	  "ZZ/101[t_0 .. t_4]",
	  "(t_0 -  2*t_1)^3",
	  },
     "Warning: the values of the indexed variables ", TT "t_i", " are stored in a global location,
     behind the scenes, so may not get garbage collected, even if ", TT "t", " is a local variable."
     }

undocumented {(NewFromMethod,IndexedVariableTable,Symbol)}
document {
     Key => {IndexedVariableTable,((symbol _,symbol =),IndexedVariableTable,Thing),(symbol _,IndexedVariableTable,Thing)},
     "This class is used as part of the implementation of indexed variables.  Objects of this class contain
     the values of the indexed variables that share the same base.",
     EXAMPLE lines ///
	  p_1 = a
	  p_2 = b
	  p
	  peek p
     ///,
     SeeAlso => {IndexedVariable}
     }
document {
     Key => MonoidElement,
     Headline => "the class of all monoid elements",
     SeeAlso => "monoid"}
document {
     Key => GeneralOrderedMonoid,
     Headline => "the class of all ordered free commutative monoids",
     "This is the class of free monoids that can be handled by
     the ", TO "engine", ".  Elements of such monoids are implemented
     as instances of ", TO "MonoidElement", ".",
     PARA{},
     SeeAlso => { "monoid" }
     }
document {
     Key => Vector,
     Headline => "the class of all elements of free modules that are handled by the engine",
     "If ", TT "R", " is a ring handled by the engine, and ", TT "M", " is a free
     module over ", TT "R", ", then M is a subclass of Vector.",
     PARA{},
     SeeAlso => {"engine", "Module"}}
document {
     Key => Matrix,
     Headline => "the class of all matrices",
     "A matrix is a homomorphism between two modules, together with
     an integer (or vector of integers) called its degree, which is
     used when determining whether the map is homogeneous.  The matrix
     is stored in the usual way as a rectangular array of ring elements.
     When the source or target modules are not free, the matrix is
     interpreted as a linear transformation in terms of the generators
     of the modules.",
     SeeAlso => "matrices",
     PARA{},
     "A matrix ", TT "f", " is an immutable object, so if you want to
     cache information about it, put it in the hash table ", TT "f.cache", ".",
     PARA{},
     "Common ways to make a matrix:",
     UL {
	  TO "map",
	  TO "matrix",
	  },
     "Common ways to get information about matrices:",
     UL {
	  TO (degree, Matrix),
	  TO (isHomogeneous, Matrix),
	  TO (matrix, Matrix),
	  },
     "Common operations on matrices:",
     UL {
	  TO (symbol +, Matrix, Matrix),
	  TO (symbol -, Matrix, Matrix),
	  TO (symbol *, RingElement, Matrix),
	  TO (symbol *, Matrix, Matrix),
	  TO (symbol ==, Matrix, Matrix),
	  TO (symbol ++, Matrix, Matrix),
	  TO (symbol **, Matrix, Matrix),
	  TO (symbol %, Matrix, Matrix),
	  TO (symbol //, Matrix, Matrix),
	  TO (symbol |, Matrix, Matrix),
	  TO (symbol ||, Matrix, Matrix),
	  TO (symbol ^, Matrix, List),
	  TO (symbol _, Matrix, List)
	  },
     "Common ways to use a matrix:",
     UL {
	  TO (cokernel, Matrix),
	  TO (image, Matrix),
	  TO (kernel, Matrix),
	  TO (homology, Matrix, Matrix),
	  }}

document {
     Key => QuotientRing,
     Headline => "the class of all quotient rings"
     }

document {
     Key => Descent,
     "A type of mutable hash table used by ", TO "showUserStructure", ", ", TO "showClassStructure", ",
     and ", TO "showStructure", " to display their tree of results conveniently."
     }

document {
     Key => showUserStructure,
     Headline => "show parent structure for those types defined by user",
     Usage => "showUserStructure",
     Outputs => {{ "a display of the parent structure of the types defined by the user and assigned to global variables" }},
     PARA{"Each type is displayed to the right of its parent."},
     PARA{"A type is an instance of the class ", TO "Type", "."},
     EXAMPLE lines ///
     R = QQ[x,y]
     X = new Type of List
     Y = new Type of X
     Z = new Type of X
     showUserStructure
     ///,
     SeeAlso => { showStructure, parent, ancestors}
     }


document {
     Key => showStructure,
     Headline => "display parent structure",
     SYNOPSIS (
	  Usage => "showStructure",
	  Outputs => {{ "a display of the parent structure of all types assigned to global variables" }}
	  ),
     SYNOPSIS (
	  Usage => "showStructure (X,Y,...)",
	  Inputs => {"X" => Type,"Y" => Type},
	  Outputs => {{ "a display of the class structure of the types specified" }}
	  ),
     PARA{"Each such type is displayed to the right of its ", TO "parent", "."},
     PARA{"A type is an instance of ", TO "Type", ", by definition."},
     EXAMPLE {
	  "showStructure",
	  "showStructure(ZZ,QQ,RR,RR_200,QQ[x],Ring)"
	  },
     SeeAlso => { "showClassStructure", "showUserStructure", ancestors }
     }

document {
     Key => showClassStructure,
     Headline => "display class structure",
     SYNOPSIS (
	  Usage => "showClassStructure",
	  Outputs => {{ "a display of the class structure of all objects assigned to global variables" }}
	  ),
     SYNOPSIS (
	  Usage => "showClassStructure (x,y,...)",
	  Inputs => {"x","y"},
	  Outputs => {{ "a display of the class structure of objects specified" }}
	  ),
     PARA{"Each object is displayed to the right of its ", TO "class", "."},
     EXAMPLE lines ///
     showClassStructure
     ///,
     SeeAlso => { "showStructure", "showUserStructure" }
     }

document {
     Key => {ultimate,(ultimate, Function, Thing)},
     Headline => "ultimate value for an iteration",
     TT "ultimate(f,x)", " -- yields the value ultimately obtained by
     applying the function ", TT "f", " to ", TT "x", ".",
     PARA{},
     "Iteration ceases when an error occurs during application of the
     function, or the result is the same.  Errors are not reported.",
     PARA{},
     "It's a bad idea to use this function, because unexpected errors will
     produce unexpected results silently."
     }
