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
queryFun := symbol queryFun
getFun := symbol getFun
setFun := symbol setFun
sizeFun := symbol sizeFun

LAST := "-- last key assigned --"

indexTable := memoize(
     prefix -> (
	  fn := prefix | "Macaulay2-index-cache.db";
	  local tb;
	  local val;
     	  next := 0;
	  try (
	       tb = openDatabaseOut fn;
	       addEndFunction( () -> (
			 if val =!= null then tb#LAST = val;
			 close tb;
			 ));
	       ) 
	  else try (
	       tb = openDatabase fn;
	       )
	  else new HashTable;
	  if tb#?LAST then next = value tb#LAST + 1;
	  new HashTable from {
	       queryFun => key -> tb#?key,
	       getFun => key -> prefix | if tb#?key then tb#key else (
		    if mutable tb
		    then (
			 val = tb#key = fourDigits next;
			 next = next+1;
			 val)
		    else if class tb === HashTable 
		    then error ("failed to created database file ", fn)
		    else error ("database file ", fn, " is read-only")
		    )
	       }
	  )
     )

cacheFileName = method()
cacheFileName(String,Thing) := (prefix,key) -> (indexTable prefix)#getFun toExternalString key
cacheFileName(List,Thing) := (path,key) -> (
     key = toExternalString key;
     apply(
	  select(path, prefix -> (indexTable prefix)#queryFun key),
	  prefix -> (indexTable prefix)#getFun key
	  )
     )
