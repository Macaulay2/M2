--		Copyright 1995 by Daniel R. Grayson

List _ ZZ := (s,i) -> s#i

String _ ZZ := (s,i) -> s#i
String _ Sequence := (s,i) -> ((j,k) -> substring(s,j,k)) i

document { quote #,
     TT "#x", " -- provides the length of a list, sequence, array or 
     string, or the number of elements in a hash table or set.",
     BR,NOINDENT,
     TT "x#i", " -- provides the value associated to the key i in the hash table
     x; or else the i-th element of x if x is a list, array, or sequence; or
     the i-th character of x if x is a string; or the value stored in the
     database x under the key i, which must be a string.",
     PARA,
     "If x is a string, then i must be an integer, and x#i is the i-th
     character in the string, presented as a string of length one, or if 
     i is out of range, a string of length zero is returned.  If i is
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
     SEEALSO( "#?", "#" )
     }

document { "#?",
     TT "x#?i", " -- tells whether there is a value associated to the key i in 
     the hash table x; or else whether the i-th element of x exists if x is a list, 
     array, or sequence; or else whether the i-th character of x exists if x
     is a string; or the value stored in the database x under the key i, which
     must be a string.",
     SEEALSO( "#" )
     }

document { "_",
     TT "x_i", " -- a binary operator which is used for various
     mathematica operations that are customarily written with subscripts.",
     PARA,
     "A ", TO "binary method", " may be installed for ", TT "x_i", " with code like ",
     PRE "          X _ Y := (x,i) -> ...",
     "where X is the prospective class of x and Y is the class of i.",
     PARA,
     "Examples where methods have been installed:",
     MENU {
	  (TO "w_i", "        -- get an entry from a list or sequence"),
	  (TO "w_{i,j}", "    -- get entries from a list or sequence"),
	  (TO "C_i", "        -- get a module from a chain complex"),
	  (TO "f_i", "        -- get a column from a matrix"),
	  (TO "p_i", "        -- get a component from a map of chain complexes"),
	  (TO "f_(i,j)", "    -- get an entry from a matrix"),
	  (TO "f_{i,j}", "    -- get some columns from a matrix"),
	  (TO "f_m", "        -- get a coefficient from a polynomial"),
	  (TO "M_i", "        -- get a generator in a module or monoid"),
	  (TO "M_{i}", "      -- get a generator mapping to a module"),
	  (TO "G_i", "        -- get a generator from a monoid"),
	  (TO "R_i", "        -- get a generator from a ring"),
	  (TO "Tor", "_i      -- Tor functor"),
	  (TO "HH", "_i       -- homology functor"),
	  (TO "v_i", "        -- get an entry from a vector"),
	  (TO "S_v", "        -- make an element of a Schur ring")
	  }
     }

document { "w_i",
     TT "w_i", " -- selects an entry from a list or sequence.",
     PARA,
          
     }

document { "w_{i,j}",
     TT "w_{i,j,...}", " -- selects entries from a list or sequence w.",
     PARA,

     }

document { ".",
     TT "x.k", " -- the same as x#(global k), i.e., treat ", TT "k", " as
     a global symbol and provide the value stored in the hash table ", TT "x", " 
     under the key ", TT "k", ".",
     PARA,
     "May also be used in an assignment.",
     PARA,
     EXAMPLE "x = new MutableHashTable;",
     EXAMPLE "x.k = 444",
     EXAMPLE "x.k",
     EXAMPLE "peek x",
     SEEALSO ("#", ".?", "global")
     }

document { ".?",
     TT "x.?k", " -- the same as x#?(global k), tells whether a value is
     available with ", TT "x.k", ".",
     PARA,
     SEEALSO( ".", "#?" )
     }

