document {
     Key => "what a class is",
     "In Macaulay2 the behavior of a function depends heavily on the types
     of the arguments it's presented with.  For example, the expression ", TT "x+y", "
     means the sum if ", TT "x", " and ", TT "y", " are integers, but it
     means the union if ", TT "x", " and ", TT "y", " are sets.  To implement this
     in a clean fashion, we store the code for doing things with sets
     in something called ", TO "Set", " and we store the code for doing things with integers
     in something called ", TO "ZZ", ".  We say that each integer is an ", TO "instance", "
     of ", TO "ZZ", ", and ", TO "ZZ", " is the ", TO "class", " (or type) of each 
     integer.  The function ", TO "class", " provides the class of an object, and
     the function ", TO "instance", " tells whether a given object is an
     instance of a given class, or a subclass of it, and so on.",
     PARA{},
     EXAMPLE {
	  "class 33",
	  "instance(33,ZZ)",
	  "instance(33,String)"
	  },
     "The corresponding mathematical idea is that ", TO "ZZ", " is the set of
     all integers.",
     PARA{},
     "The class of all classes or types is called ", TO "Type", ".",
     EXAMPLE {
	  "instance(ZZ,Type)",
	  "instance(33,Type)",
	  },
     "The class of all objects in the system is ", TO "Thing", ".",
     EXAMPLE {
	  "instance(33,Thing)",
	  },
     "Everything has a class, and every class has a ", TO "parent", ".  The 
     parent class represents a broader class of objects, and is used to
     contain code that applies to the broader class.  For example, ", TO "ZZ", "
     is a ring, and every ring is also a type.",
     EXAMPLE {
	  "class ZZ",
	  "parent class ZZ"
	  },
     "Types are implemented as hash tables -- it's a versatile way of storing
     bits of code that are needed in various situations; the keys for the
     hash table are constructed in a certain way from the function and the
     types of its arguments whose details the user doesn't need to know.",
     PARA{},
     Subnodes => {
	  TO "class",
	  TO "synonym",
	  TO "parent",
	  TO "instance",
	  TO "instances",
	  TO "ancestor",
	  TO "ancestors",
	  },
     SeeAlso => { "uniform", "Thing", "Nothing", "Type", "MutableList", "MutableHashTable", "SelfInitializingType" }
     }

document {
     Key => "making new classes",
     "All new classes are made with the operator ", TO "new", ".
     You may choose to implement the instances of your new class
     either as basic lists or as hash tables, or you may even
     base it on a subclass of ", TO "BasicList", " or a subclass of 
     ", TO "HashTable", ", if you find a class that has some
     of the methods you need already implemented.",
     PARA{},
     "As an example, we may wish to implement quaternions as lists
     of four real numbers.  We know that lists already have a method
     for addition that treats them as vectors, and we could use
     the same code for addition of quaternions.",
     EXAMPLE {
	  "Qu = new Type of List",
	  "w = new Qu from {1,2,3,4}",
	  "w+w"
	  },
     "Now all we have to do is to install a method for multiplying
     quaternions.",
     EXAMPLE {
	  "Qu * Qu := (x,y) -> new Qu from { 
	  x#0*y#0 - x#1*y#1 - x#2*y#2 - x#3*y#3,
	  x#0*y#1 + x#1*y#0 + x#2*y#3 - x#3*y#2,
	  x#0*y#2 + x#2*y#0 + x#3*y#1 - x#1*y#3,
	  x#0*y#3 + x#3*y#0 + x#1*y#2 - x#2*y#1
	  };",
     	  "w*w"
	  },
     Subnodes => {
	 TO "new",
	 TO "printing and formatting for new classes",
     }
}

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
	       "C" => {Type, Sequence} => {},
	       { TT "(A,c) -> ...", ", a function of 2 arguments: ", TT "AA", " will be an ancestor of ", TT "A", ",
		    and ", TT "C", " will be an ancestor of the class of ", TT "c", ".
		    Alternatively, ", TT "A", " will be a type of ", TT "AA", " and ", TT "c", " will be an instance of ", TT "C", ".
		    If ", CODE "C", " is a sequence, then it must contain 2 or 3 types and the given function must have the form ",
		    CODE "(A,c,d) -> ...", " or ", CODE "(A,c,d,e) -> ...", ", where ", CODE "c", " is an instance of ", CODE "C#0", ", ",
		    CODE "d", " is an instance of ", CODE "C#1", ", and (if applicable) ", CODE "e", " is an instance of ",
		    CODE "C#2", "."
		    }
	       },
	  Consequences => {
	       { "the function will be installed as the method for ", TT "new AA from C", ".  It will be stored under the key ", TT "(NewFromMethod,AA,C)", "
		    in the younger of the hash tables ", TT "AA", " and ", TT "C", ".
		    If ", CODE "C", " is a sequence, then it will be stored under the key ", CODE "(NewFromMethod,AA,C#0,C#1)",
		    " or ", CODE "(NewFromMethod,AA,C#0,C#1,C#2)", ", depending on the length of ", CODE "C", " in the youngest of the hash tables."
		    }
	       },
	  Outputs => {
	       { "the function is returned as the value of the expression" }
	       },
	  PARA {
	       "Let's use the class ", TT "M", " defined above, and introduce methods for creating lists of class ", TT "M", " from integers.  Then we use them
	       in the subsection below."
	       },
	  EXAMPLE lines ///
	       new M from ZZ := (M',i) -> 0 .. i
	       new M from (ZZ,ZZ) := (M',i,j) -> splice(i:0 .. j)
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
	       { "if ", CODE "C", " is a sequence, then this method will be called with arguments ",
		   CODE "(A,c#0,c#1)", " or ", CODE "A(c#0,c#1,c#2)", ", depending on its length."},
	       { "if no such method has been installed, then ancestors of ", TT "A", " and ", TT "C", " (or its elements if it is a sequence),
		   will be consulted, searching lexicographically for a method; see ", TO "inheritance", "." },
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
	       new M from (3,2)
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
	TO "of",
	TO "newClass",
	TO NewMethod,
	TO NewOfMethod,
	TO NewFromMethod,
	TO NewOfFromMethod,
        },
     }

document {
     Key => "printing and formatting for new classes",
     "After making a new type, it's desirable to install methods
     for displaying the instances of the new type in various formats.",
     EXAMPLE {
	  "Qu = new Type of List",
	  "w = new Qu from {1,-2,0,4}",
	  },
     "For example, it's desirable to display the quaternion above
     so it looks like a quaternion.  One way to achieve this is to install
     first a method for creating an ", TO "Expression", " from a
     quaternion, since there are methods already installed for converting
     expressions to common forms of output, such as to nets, which are
     used most commonly.",
     EXAMPLE {
	  ///expression Qu := z -> (
	       expression z#0 +
	       expression z#1 * expression "I" +
	       expression z#2 * expression "J" +
	       expression z#3 * expression "K");///,
	  ///net Qu := z -> net expression z;///,
	  ///toString Qu := z -> toString expression z;///,
	  ///tex Qu := z -> tex expression z;///,
	  ///html Qu := z -> html expression z;///,
	  "w",
	  "toString w",
	  "tex w",
	  "html w",
     	  },
     "Of course, now that we've decided that there should be certain
     quaternions called ", TT "I", ", ", TT "J", ", and ", TT "K", ",
     perhaps we should install them, too.",
     EXAMPLE {
	  "I = new Qu from {0,1,0,0}",
	  "J = new Qu from {0,0,1,0}",
	  "K = new Qu from {0,0,0,1}",
	  "2*I + 5*J",
	  "peek oo"
	  },
     Subnodes => {
	 TO expression,
	 TO describe,
         },
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
     Key => {parent, (parent, Thing)},
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
     Headline => "the class of all mutable types",
     "Everything in the system is classified, and the class that a thing belongs to is a type. ",
     "A type is implemented as a ", TO2(MutableHashTable, "mutable hash table"),
     " containing method functions for its instances.",
     PARA{},
     "The list of types known to the system is displayed below.",
     Subnodes => {
	 "mathematical mutable types",
	 TO Monoid,
	 TO Ring,
	 TO RingFamily,
	 "other mutable types",
	 TO SelfInitializingType,
	 TO WrapperType,
	 TO HeaderType,
         },
     }

document {
     Key => Thing,
     Headline => "the class of all things",
     "Everything in Macaulay2 is a ", ITALIC "thing", ".  This
     includes numbers, strings, and lists.  More complicated things such as
     polynomials, groups, rings, and chain complexes are implemented
     as ", ITALIC "hash tables", ".  See ", TO "Type", " for information
     about what types of things there are.",
     Subnodes => TO \ {
	 --Net,
	 NetFile,
	 Boolean,
	 Dictionary,
	 Nothing,
	 --Database,
	 --HashTable,
	 --Task,
	 --SymbolBody,
	 --BasicList,
	 Number,
	 File,
	 --Function,
	 AtomicInt,
	 Symbol,
	 Pseudocode
	 --FunctionBody
     }
}
document {
     Key => Nothing,
     Headline => "the empty class",
     "This class is useful for representing the class of an argument
     that is missing.  It is also used as the parent for those things that
     are not themselves types, i.e., which do not have instances.",
     Subnodes => { TO "null" },
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

document { Key => ImmutableType,
     Headline => "the class of all immutable types",
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
     SeeAlso => {showStructure,parent,class},
     -- PARA{},
     -- "The list of immutable types known to the system is displayed below.",
     -- Subnodes => {
     -- 	 "mathematical immutable types",
     -- 	 TO Module,
     --     },
     }

document {
     Key => serialNumber,
     Headline => "serial number of a dictionary, task, symbol, mutable hash table, or mutable list",
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
