--		Copyright 1993-1999 by Daniel R. Grayson


String << Thing := File => (filename,x) -> openOut filename << x

counter := 0

temporaryFileName = () -> (
     counter = counter + 1;
     "/tmp/M2-" | toString processID() | "-" | toString counter
     )
-----------------------------------------------------------------------------
fourDigits := i -> (
     s := toString i;
     concatenate(4-#s:"0", s)
     )
-----------------------------------------------------------------------------
cacheFileName = method()

changed := new MutableHashTable

indexTable := memoize(
     prefix -> (
	  fn := prefix | "Macaulay2-index-cache";
	  tb := new MutableHashTable from try value get fn else {};
	  addEndFunction( () -> (
		    if changed#?tb then (
			 try fn << toExternalString pairs tb << endl << close
			 else stderr << "warning: cache file '" << fn << "' not created" << endl;
			 remove(changed,tb);
			 )
		    )
	       );
	  tb
	  )
     )

counterVariable := memoize( prefix -> ( counter := 0; symbol counter ) )

cacheFileName(String) := (prefix) -> (
     cv := counterVariable prefix;
     prefix | toString (cv <- value cv + 1)
     )

cacheFileName(String,Thing) := (prefix,key) -> (
     tb := indexTable prefix;
     prefix | if tb#?key then tb#key else (
	  changed#tb = true;
	  tb#key = fourDigits(#tb)
	  )
     )

cacheFileName(String,Thing,String) := (prefix,key,suffix) -> (
     tb := indexTable prefix;
     changed#tb = true;
     prefix | (tb#key = suffix)
     )

cacheFileNameKeys = method()
cacheFileNameKeys String := prefix -> keys indexTable prefix
