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
tt := new MutableHashTable from toList apply(0 .. 255, i -> (
	  c := ascii i;
	  c => c
	  ));

tt#"/" = "%sl"				  -- can't occur in a file name: unix
tt#"?" = "%qu"					     -- has a meaning in URLs
tt#"#" = "%sh"					     -- has a meaning in URLs
tt#":" = "%co"				    -- has a meaning for shells: unix
tt#"\""= "%qu"				    -- has a meaning for shells: unix
tt#";" = "%sc"				    -- has a meaning for shells: unix
tt#"\\"= "%bs"				    -- has a meaning for shells: unix
tt#" " = "%sp"				    -- has a meaning for shells: unix
tt#"%" = "%pc"					      -- our escape character
tt#"$" = "%do"				    -- has a meaning for shells: unix
tt#"|" = "%vb"				    -- has a meaning for shells: unix
tt#"&" = "%am"				    -- has a meaning for shells: unix
tt#"<" = "%lt"				    -- has a meaning for shells: unix
tt#">" = "%gt"				    -- has a meaning for shells: unix
tt#"!" = "%ep"				    -- has a meaning for shells: unix

uu := new HashTable from {
     "." => ".%",
     ".." => "..%",
     "index" => "index%"
     }

toFilename = method()
toFilename String := s -> (
     -- Convert a string to a new string usable as a file name.
     -- avoid ".", "..", and any string with "/" in it.
     s = concatenate(apply(characters s, c -> tt#c));
     if uu#?s then s = uu#s;
     s)
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
	  makeName := key -> (
	       if true			    -- I don't know how to decide yet
	       then (
	       	    toFilename key	    -- for OSes with long file names
	       	    )
	       else (			    -- for OSes with short file names
	       	    val := fourDigits next;
	       	    next = next+1;
	       	    val
	       	    )
	       );
	  new HashTable from {
	       queryFun => key -> tb#?key,
	       getFun => key -> prefix | if tb#?key then tb#key else store(key,makeName key),
	       setFun => (key,val) -> prefix | (
		    if tb#?key then error("key ",key," already has a value");
		    store(key,val)
		    )
	       }
	  )
     )

cacheFileName = method()
cacheFileName(String,Thing) := (prefix,key) -> (
     (indexTable prefix)#getFun toString key
     )
cacheFileName(String,Thing,String) := (prefix,key,val) -> (
     (indexTable prefix)#setFun(toString key,val)
     )
cacheFileName(List,Thing) := (path,key) -> (
     key = toString key;
     apply(
	  select(path, prefix -> (indexTable prefix)#queryFun key),
	  prefix -> (indexTable prefix)#getFun key
	  )
     )
