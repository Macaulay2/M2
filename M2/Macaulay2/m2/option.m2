--		Copyright 1994 by Daniel R. Grayson

html Option := x -> toString x
text Option := x -> toString x

new HashTable from List := HashTable => (O,v) -> hashTable v
-- erase quote hashTable

OptionsRegistry = new MutableHashTable
OptionTable = new Type of HashTable
OptionTable @ Function := Function => (o,f) -> x -> processArgs(x,o,f);
List        @ Function := Function => (o,f) -> (new OptionTable from o) @ f

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

