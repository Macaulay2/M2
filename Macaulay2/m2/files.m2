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

tt#"/" = "_sl_"				  -- can't occur in a file name: unix
tt#"%" = "_pc_"					     -- has a meaning in URLs
tt#"?" = "_qu_"					     -- has a meaning in URLs
tt#"#" = "_sh_"					     -- has a meaning in URLs
tt#"\\"= "_bs_"				 -- can't occur in a file name: MSDOS
tt#" " = "_sp_"					      -- can't occur in a URL
tt#"_" = "_us_"					      -- our escape character

tt#":" = "_co_"					-- has a meaning for gnu make
tt#"$" = "_do_"					-- has a meaning for gnu make
tt#";" = "_se_"					-- has a meaning for gnu make

toFilename = method()
toFilename String := s -> (
     -- Convert a string to a new string usable as a file name, and with
     -- at least one special character prefixed, to avoid collisions with
     -- package names and with index.html.
     s = concatenate("_",apply(characters s, c -> tt#c));
     s)
-----------------------------------------------------------------------------
queryFun := symbol queryFun
getFun := symbol getFun
setFun := symbol setFun
keysFun := symbol keysFun

LAST := "-- last key assigned --"

indexTable := memoize(
     prefix -> (
	  fn := prefix | "Macaulay2-index-cache.db";
	  local tb;
	  try (
	       tb = openDatabaseOut fn;
	       ) 
	  else try (
	       tb = openDatabase fn;
	       )
	  else tb = new HashTable;
	  store := (key,val) -> (
	       if #key > 300 then error "suspiciously long key";
	       if not mutable tb then error (
		    if class tb === HashTable 
	       	    then ("failed to open database file for writing or reading: ", fn)
	       	    else ("database file ", fn, " is read-only or in use by another process")
		    );
	       tb#key = val
	       );
     	  next := null;
	  makeName := key -> (
	       if true
	       then (
	       	    toFilename key	    -- for OSes with long file names
	       	    )
	       else (			    -- for OSes with short file names
     	       	    if next === null then next = #(keys tb);
	       	    val := fourDigits next;
	       	    next = next+1;
	       	    val
	       	    )
	       );
	  new HashTable from {
	       queryFun => key -> tb#?key,
	       getFun => key -> prefix | if tb#?key then tb#key else store(key,makeName key),
     	       keysFun => () -> keys tb,
	       setFun => (key,val) -> prefix | (
		    if tb#?key 
		    then (
			 if tb#key =!= val then error("key ",key," already has a different value");
			 )
		    else store(key,val);
		    val
		    )
	       }
	  )
     )

cacheFileName = method()
-- cacheFileName(String) := (prefix) -> (
--      (indexTable prefix)#keysFun ()
--      )
cacheFileName(String,String) := (prefix,key) -> (
     (indexTable prefix)#getFun key
     )
cacheFileName(String,String,String) := (prefix,key,val) -> (
     (indexTable prefix)#setFun(key,val)
     )
cacheFileName(List,String) := (path,key) -> (
     apply(
	  select(path, prefix -> (indexTable prefix)#queryFun key),
	  prefix -> (indexTable prefix)#getFun key
	  )
     )
