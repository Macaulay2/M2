StateTable = new Type of HashTable
StateTable.synonym = "state table"

fixStateTableEntry := method()
fixStateTableEntry Option := (v) -> fixStateTableEntry(v#0,v#1)
fixStateTableEntry(Thing,Thing) := (a,s) -> {a,s}

new StateTable from List := (StateTable,v) -> hashTable splice (fixStateTableEntry \ v)
names := new MutableHashTable
StateTable.GlobalAssignHook = (X,x) -> names#x = toString X
StateTable.GlobalReleaseHook = (X,x) -> remove(names,x)

net StateTable := x -> if names#?x then names#x else (lookup(net,parent StateTable)) x

stateTable = method(SingleArgumentDispatch => true)
stateTable List := x -> new StateTable from x
stateTable Sequence := x -> stateTable toList x
stateTable Option := x -> stateTable {x}
document { stateTable,
     TT "stateTable {a => f, b => g, ...}", " -- create a state table that specifies
     that upon input ", TT "a", " the function ", TT "f", " should be called, etc.",
     PARA,
     SEEALSO "StateTable"
     }

StateTable || StateTable := (x,y) -> merge(x,y,(f,g) -> c -> (f c; g c;))
StateTable || List := (x,o) -> x || stateTable o
List || StateTable := (o,x) -> stateTable o || x
document { (symbol ||, StateTable, StateTable),
     TT "x || y", " -- merges two state tables.  If there is a key that appears in
     both tables, the two functions are merged into one which calls both.",
     PARA,
     SEEALSO "StateTable"
     }

StateTableAlternative = new Type of BasicList
StateTableAlternative.synonym = "state table alternative list"
fixStateTableEntry(StateTableAlternative,Thing) := (a,s) -> apply(toSequence a, i -> {i,s})
document { StateTableAlternative,
     Headline => "the class of all state table alternative lists",
     "These are lists of keys, which when encountered by ", TO "stateTable", " to
     make a state table, will create an entry for each of the keys in the list.",
     EXAMPLE {
	  "load \"StateTable.m2\"",
	  "a = new StateTableAlternative from {1,2,3}",
	  "stateTable { a => c -> null }",
	  },
     PARA,
     SEEALSO {"StateTable"}
     }

space = new StateTableAlternative from characters " \f\n\r\t"
document { "space",
     TT "stateTable { space => f , ...}", " -- a list of alternative keys which
     specifies that the function ", TT "f", " should be run for each white
     space ASCII character.",
     PARA,
     EXAMPLE {
	  "load \"StateTable.m2\"",
	  "toExternalString space",
	  },
     SEEALSO "stateTable"
     }

digit = new StateTableAlternative from characters "0123456789"
document { "digit",
     TT "stateTable { digit => f , ...}", " -- a list of alternative keys which
     specifies that the function ", TT "f", " should be run for each ASCII character
     which is a decimal digit.",
     PARA,
     EXAMPLE {
	  "load \"StateTable.m2\"",
	  "digit",
	  "stateTable{ digit => c -> null }"
	  },
     SEEALSO "stateTable"
     }

upper = new StateTableAlternative from characters "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
document { "upper",
     TT "stateTable { upper => f , ...}", " -- a list of alternative keys which
     specifies that the function ", TT "f", " should be run for each upper case
     ASCII character.",
     PARA,
     EXAMPLE {
	  "load \"StateTable.m2\"",
	  "upper",
	  },
     SEEALSO "stateTable"
     }

lower = new StateTableAlternative from characters "abcdefghijklmnopqrstuvwxyz"
document { "lower",
     TT "stateTable { lower => f , ...}", " -- a list of alternative keys which
     specifies that the function ", TT "f", " should be run for each lower case
     ASCII character.",
     PARA,
     EXAMPLE {
	  "load \"StateTable.m2\"",
	  "lower",
	  },
     SEEALSO "stateTable"
     }

alpha = join(upper,lower)
document { "alpha",
     TT "stateTable { alpha => f , ...}", " -- a list of alternative keys which
     specifies that the function ", TT "f", " should be run for each alphabetic
     ASCII character.",
     PARA,
     EXAMPLE {
	  "load \"StateTable.m2\"",
	  "alpha",
	  },
     SEEALSO "stateTable"
     }

alphanum = join(alpha,digit)
document { "alphanum",
     TT "stateTable { alphanum => f , ...}", " -- a list of alternative keys which
     specifies that the function ", TT "f", " should be run for each  
     ASCII character.",
     PARA,
     EXAMPLE {
	  "load \"StateTable.m2\"",
	  "alphanum",
	  },
     SEEALSO "stateTable"
     }

protect default
document { "default",
     TT "stateTable { default => f , ...}", " -- a symbol to be used as a key in a
     state table to specify that the function ", TT "f", " should be run if the current
     input doesn't appear elsewhere in the table.",
     PARA,
     SEEALSO "stateTable"
     }

ignore = c -> null
document { ignore,
     TT "stateTable { a => ignore , ...}", " -- a function to be used as a value in a
     state table to indicate that no action should be performed for the corresponding
     key.",
     PARA,
     SEEALSO "stateTable"
     }

done = stateTable { }
protect symbol done
document { done,
     TT "done", " -- an empty state table that can be used to indicate that a finite
     state machine has reached a terminal state, and can stop scanning its input.",
     PARA,
     "We should replace this by a general way of specifying that a state is a
     terminal state.",
     PARA,
     SEEALSO "StateTable"
     }


document { StateTable, Headline => "the class of all state tables",
     "State tables are hash tables designed to be used in the implementation
     of finite state machines.  The state of such a machine can be mostly
     encapsulated in a state table whose keys are the possible inputs and whose
     values are the functions to be applied to the current key.  A side effect
     of those functions may be to change the current state.",
     PARA,
     "Useful keys for finite state machines:",
     MENU {
	  TO "alpha",
	  TO "alphanum",
	  TO "default",
	  TO "digit",
	  TO "lower",
	  TO "space",
	  TO "upper",
	  },
     "Useful values for finite state machines:",
     MENU {
	  TO "ignore"
	  },
     "Useful state tables:",
     MENU {
	  TO "done"
	  },
     "Methods for creating or combining state tables:",
     MENU {
	  TO "stateTable",
	  TO (symbol ||, StateTable, StateTable),
	  }
     }
