--		Copyright 1994 by Daniel R. Grayson

Option = new Type of BasicList
ListHead Option := (x,y) -> x {y}
html Option := x -> name x
text Option := x -> name x

name Option := z -> concatenate splice (
     if precedence z > precedence z#0 then ("(",name z#0,")") else name z#0,
     " => ",
     if precedence z > precedence z#1 then ("(",name z#1,")") else name z#1
     )

document { quote Option,
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

Thing => Thing := (x,y) -> new Option from {x,y}

new HashTable from List := (O,v) -> hashTable v
-- erase quote hashTable
document { (NewFromMethod, HashTable, List),
     TT "new HashTable from x", " -- produce a new hash table from a
     list ", TT "x", ".",
     PARA,
     "Elements of ", TT "x", " which are options, ", TT "k => v", " cause
     the value ", TT "v", " to be stored in ", TT "x", " under the key ", TT "k", ".
     Other elements ", TT "s", " cause the value ", TT "true", " to be stored under 
     the key ", TT "s", "."
     }

OptionTable = new Type of HashTable
document { quote OptionTable,
     TT "OptionTable", " -- the class of those hash tables which are used
     to store optional named parameters to functions.",
     SEEALSO "processArgs"
     }
processArgs = (args,defaults,function) -> (
     defaults = new MutableHashTable from defaults;
     op := (nam,value) -> (
	  if defaults#?nam 
	  then defaults#nam = value
	  else error("unrecognized option '", name nam, "'");
	  false);
     args = select(deepSplice sequence args,
	  a -> (
	       if class a === Option then op toSequence a
	       else if class a === OptionTable then scanPairs(a, op)
	       else true
	       )
	  );
     defaults = new OptionTable from defaults;
     function(args, defaults))
document { quote processArgs,
     TT "processArgs(args,defaults,function)", " -- Here ", TT "args", " 
     is the sequence of arguments previously passed to some function 
     intended to accept optional arguments, ", TT "defaults", " is a
     hash table whose keys are the names of the optional arguments, and 
     whose values are the corresponding default values.
     The return value is obtained by evaluation of
     ", TT "function(newargs,options)", ",
     where newargs is obtained from args by removing the
     options of the form ", TT "X=>A", " (where ", TT "X", " is a
     name of an optional argument), and ", TT "options", " is a hash table
     of the same form as ", TT "defaults", " in which the default
     values have been replaced by the user-supplied values, e.g., the
     value stored under the key ", TT "X", " has been replaced by
     ", TT "A", ".  As shorthand for the option ", TT "X=>true", " the
     one may use ", TT "X", ".",
     PARA,
     EXAMPLE "processArgs((t,u,a=>4,c), new HashTable from {a=>1,b=>2,c=>false},identity)",
     SEEALSO {"OptionTable", "Option", "=>"}
     }
