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

readPairs := fn -> (
     data := "{}";
     if fileExists fn then data = get fn;
     try data = value data else error( "couldn't evaluate contents of file: ", fn);
     if class data =!= List or not all(data, x -> class x === Sequence and #x === 2)
     then error("expected file '", fn, "' to contain a list of pairs");
     new MutableHashTable from data)

queryFun := symbol queryFun
getFun := symbol getFun
setFun := symbol setFun
sizeFun := symbol sizeFun

indexTable := memoize(
     prefix -> (
	  fn := prefix | "Macaulay2-index-cache";
	  tb := readPairs fn;
	  changed := false;
	  addEndFunction (
	       () -> if changed then (
	       	    fn << toExternalString pairs tb << endl << close;
	       	    changed = false;
	       	    )
	       );
	  new HashTable from {
	       queryFun => key -> tb#?key,
	       getFun => key -> tb#key,
	       setFun => key -> (
     	       	    if not changed and not fileExists fn then (
		    	 -- initialize the file now to ensure we can write to it later
		    	 fn << "{}" << close;
		    	 );
	       	    changed = true;
	       	    tb#key = fourDigits(#tb)
	       	    )
	       }
	  )
     )

cacheFileName(String,Thing) := (prefix,key) -> (
     tb := indexTable prefix;
     prefix | if tb#queryFun key then tb#getFun key else tb#setFun key)

cacheFileName(List,Thing) := (path,key) -> (
     apply(
	  select(path, prefix -> (indexTable prefix)#queryFun key),
	  prefix -> prefix | (indexTable prefix)#getFun key
	  )
     )

-- tt := new MutableHashTable from toList apply(0 .. 255, i -> (
-- 	  c := ascii i;
-- 	  c => c
-- 	  ));
-- 
-- tt#"/" = "%sl"
-- tt#":" = "%co"
-- tt#";" = "%sc"
-- tt#"\\" = "%bs"
-- tt#" " = "%_"
-- tt#"%" = "%%"
-- tt#"$" = "%do"
-- tt#"<" = "%lt"
-- tt#">" = "%gt"
-- 
-- uu := new HashTable from {
--      "." => "%pe",
--      ".." => "%pe%pe"
--      }
-- 
-- toFilename = method()
-- toFilename String := s -> (
--      -- Convert a string to a new string usable as a file name.
--      -- avoid ".", "..", and any string with "/" in it.
--      s = concatenate(apply(characters s, c -> tt#c));
--      if uu#?s then s = uu#s;
--      s)
