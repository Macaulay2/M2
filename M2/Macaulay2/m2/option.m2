--		Copyright 1994 by Daniel R. Grayson

Option = new Type of BasicList
html Option := x -> name x
text Option := x -> name x

name Option := z -> concatenate splice (
     if precedence z > precedence z#0 then ("(",name z#0,")") else name z#0,
     " => ",
     if precedence z > precedence z#1 then ("(",name z#1,")") else name z#1
     )

Thing => Thing := (x,y) -> new Option from {x,y}

new HashTable from List := (O,v) -> hashTable v
-- erase quote hashTable

OptionsRegistry = new MutableHashTable
OptionTable = new Type of HashTable
Function @ OptionTable := (f,o) -> x -> processArgs(x,o,f);
Function @ List := (f,o) -> f @ new OptionTable from o

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
     if #args === 1 then args = args#0;
     defaults = new OptionTable from defaults;
     (function defaults) args)

