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
	  store := (key,val) -> (
	       if not mutable tb then error (
		    if class tb === HashTable 
	       	    then ("failed to create database file ", fn)
	       	    else ("database file ", fn, " is read-only")
		    );
	       tb#key = val
	       );
	  new HashTable from {
	       queryFun => key -> tb#?key,
	       getFun => key -> prefix | if tb#?key then tb#key else (
		    val := fourDigits next;
		    next = next+1;
		    store(key,val)
		    ),
	       setFun => (key,val) -> prefix | (
		    if tb#?key then error("key ",key," already has a value");
		    store(key,val)
		    )
	       }
	  )
     )

cacheFileName = method()
cacheFileName(String,Thing) := (prefix,key) -> (
     (indexTable prefix)#getFun toExternalString key
     )
cacheFileName(String,Thing,String) := (prefix,key,val) -> (
     (indexTable prefix)#setFun(toExternalString key,val)
     )
cacheFileName(List,Thing) := (path,key) -> (
     key = toExternalString key;
     apply(
	  select(path, prefix -> (indexTable prefix)#queryFun key),
	  prefix -> (indexTable prefix)#getFun key
	  )
     )
