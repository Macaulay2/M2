--		Copyright 1994 by Daniel R. Grayson

Option = new Type of BasicList
html Option := x -> toString x
text Option := x -> toString x

toString Option := z -> concatenate splice (
     if precedence z > precedence z#0 then ("(",toString z#0,")") else toString z#0,
     " => ",
     if precedence z > precedence z#1 then ("(",toString z#1,")") else toString z#1
     )

Thing => Thing := (x,y) -> new Option from {x,y}

new HashTable from List := (O,v) -> hashTable v
-- erase quote hashTable

OptionsRegistry = new MutableHashTable
OptionTable = new Type of HashTable
OptionTable @ Function := (o,f) -> x -> processArgs(x,o,f);
List @ Function := (o,f) -> (new OptionTable from o) @ f

processArgs = (args,defaults,function) -> (
     defaults = new MutableHashTable from defaults;
     op := (nam,value) -> (
	  if defaults#?nam 
	  then defaults#nam = value
	  else error("unrecognized option '", toString nam, "'");
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

