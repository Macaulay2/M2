--		Copyright 1995 by Daniel R. Grayson

Sequence _ ZZ := List _ ZZ := (s,i) -> s#i

String _ ZZ := (s,i) -> s#i
String _ Sequence := (s,i) -> ((j,k) -> substring(s,j,k)) i

document { quote #,
     TT "#x", " -- provides the length of a list, sequence, array or 
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
     EXAMPLE "x = new MutableHashTable",
     EXAMPLE "x#i = p",
     EXAMPLE "x#i",
     SEEALSO{ "#?", "#" }
     }

document { quote #?,
     TT "x#?i", " -- tells whether there is a value associated to the key ", TT "i", " in 
     the hash table ", TT "x", "; or else whether the i-th element of ", TT "x", " exists if ", TT "x", " is a list, 
     array, or sequence; or else whether the i-th character of ", TT "x", " exists if ", TT "x", "
     is a string; or the value stored in the database ", TT "x", " under the key ", TT "i", ", which
     must be a string.",
     PARA,
     SEEALSO{ "#" }
     }

document { quote _,
     TT "x_i", " -- a binary operator which is used for various
     mathematica operations that are customarily written with subscripts.",
     PARA,
     "A ", TO "binary method", " may be installed for ", TT "x_i", " with code like ",
     PRE "          X _ Y := (x,i) -> ...",
     "where X is the prospective class of x and Y is the class of i.",
     PARA,
     "Examples where methods have been installed:",
     SHIELD MENU {
	  (TO (quote _, List, ZZ), " -- get an entry from a list"),
	  (TO (quote _, Sequence, ZZ), " -- get an entry from a sequence"),
	  (TO (quote _, List, List), " -- get a list of entries from a list or sequence"),
	  (TO (quote _, ChainComplex, ZZ), " -- get a module from a chain complex"),
	  (TO (quote _, Matrix, ZZ), " -- get a column from a matrix"),
	  (TO (quote _, ChainComplexMap, ZZ), " -- get a component from a map of chain complexes"),
	  (TO (quote _, Matrix, Sequence), " -- get an entry from a matrix"),
	  (TO (quote _, Matrix, List), " -- get some columns from a matrix"),
	  (TO (quote _, RingElement, RingElement), " -- get a coefficient from a polynomial"),
	  (TO (quote _, Ring, ZZ), " -- get a generator from a ring"),
	  (TO (quote _, Module, ZZ), " -- get a generator of a module"),
	  (TO (quote _, Monoid, ZZ), " -- get a generator from a monoid"),
	  (TO (quote _, Module, List), " -- get a map onto some generators of a module"),
	  (TO "Tor", " -- Tor functor"),
	  (TO "HH", " -- homology functor"),
	  (TO (quote _, Vector, ZZ), " -- get an component from a vector"),
	  (TO (quote _, SchurRing, List), " -- make an element of a Schur ring")
	  }
     }

document { (quote _, List, ZZ),
     TT "w_i", " -- selects an entry from a list."
     }

document { (quote _, Sequence, ZZ),
     TT "w_i", " -- selects an entry from a sequence."
     }

document { quote .,
     TT "x.k", " -- the same as ", TT "x#(global k)", ", i.e., treat ", TT "k", " as
     a global symbol and provide the value stored in the hash table ", TT "x", " 
     under the key ", TT "k", ".",
     PARA,
     "May also be used in an assignment.",
     PARA,
     EXAMPLE "x = new MutableHashTable;",
     EXAMPLE "x.k = 444",
     EXAMPLE "x.k",
     EXAMPLE "peek x",
     SEEALSO {"#", ".?", "global"}
     }

document { quote .?,
     TT "x.?k", " -- the same as ", TT "x#?(global k)", ", tells whether a value is
     available with ", TT "x.k", ".",
     PARA,
     SEEALSO{ ".", "#?" }
     }

