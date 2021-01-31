-- -*- coding: utf-8 -*-
newPackage(
     "CacheFileName", 
     Version => "1.0",
     Headline => "creating filenames for files to be used for caching information")

export {"cacheFileName"}

queryFun := symbol queryFun
getFun := symbol getFun
setFun := symbol setFun
keysFun := symbol keysFun

LAST := "-- last key assigned --"

indexTable := memoize(
     prefix -> (
	  fn := prefix | "Macaulay2-index-cache.db";
	  tb := try openDatabaseOut fn else try openDatabase fn else new HashTable ;
	  store := (key,val) -> (
	       if #key > 300 then error "suspiciously long key";
	       if not mutable tb then error (
		    if class tb === HashTable 
	       	    then if fileExists fn 
		    then ("failed to open existing database file for writing or reading: ", fn)
		    else ("failed to create database file: ", fn)
	       	    else ("failed to write to database file ", fn, ", read-only or in use by another process")
		    );
	       tb#key = val
	       );
     	  next := null;
	  makeName := key -> (			    -- for short file names, assigned sequentially
	       if next === null then next = #(keys tb);
	       val := toString next;
	       next = next+1;
	       val
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
cacheFileName(String) := (prefix) -> (
     (indexTable prefix)#keysFun ()
     )
cacheFileName(String,String) := (prefix,key) -> (
     (indexTable prefix)#getFun key
     )
cacheFileName(String,String,String) := (prefix,key,val) -> (
     (indexTable prefix)#setFun(key,val)
     )
cacheFileName(List,String) := (path,key) -> apply(
     select(path, prefix -> (indexTable prefix)#queryFun key),
     prefix -> (indexTable prefix)#getFun key
     )
cacheFileName(String,List,String) := (head,path,key) -> (
     w := cacheFileName(path,key);
     if #w === 0 then {cacheFileName(head, key)} else w
     )
cacheFileName(Nothing,List,String) := (head,path,key) -> cacheFileName(path,key)

beginDocumentation()

document {
     Key => CacheFileName
     }

document {
     Key => cacheFileName,
     Headline => "produce the name of a cache file",
     "Macaulay2 needs to remember some bits of data from one invocation to the next,
     so it stores this data in cache files.  The name and directory of the cache file
     depend on the data being stored.  For example, the input code for examples associated
     with one of the documentation node will be stored in a file whose name depends on
     the name of the node."
     }

document {
     Key => (cacheFileName, String, String),
     Usage => ///fn = cacheFileName(prefix,key)///,
     Inputs => {
	  "prefix" => "the prefix from which to construct the file name",
	  "key" => "a key, which can be anything"
	  },
     "A file name is returned that depends only on the prefix and the key.  The
     prefix should be the path to a directory, together with a terminating path component
     separator.  When the program terminates, a file called
     ", TT "Macaulay2-index-cache", " will be created or updated, if
     necessary, in which to store the table of correspondences between keys
     and filenames.",
     EXAMPLE {
	  ///cacheFileName("/tmp/","algebra")///,
	  ///cacheFileName("/tmp/","14")///,
	  ///cacheFileName("/tmp/","14")///
	  }
     }

document {
     Key => (cacheFileName, String, String, String),
     Usage => ///fn = cacheFileName(prefix,key,base)///,
     Inputs => {
	  "prefix" => "the prefix from which to construct the file name",
	  "key" => "a key, which can be anything",
	  "base" => "the base part of the file name"
	  },
     Outputs => {
	  "fn" => "a new file name"
	  },
     "A file name ", TT "fn", " is constructed by concatenating ", TT "prefix", " and
     ", TT "base", " and associated with ", TT "key", " for future retrieval with
     ", TT "cacheFileName", ".  The prefix should be the path to a directory,
     together with a terminating path component
     separator.  When the program terminates, a file called
     ", TT "Macaulay2-index-cache", " will be created or updated, if
     necessary, in which to store the table of correspondences between keys
     and filenames.",
     EXAMPLE {
	  ///cacheFileName("/tmp/","K-theory","motives")///,
	  ///cacheFileName("/tmp/","K-theory")///,
	  }
     }

document {
     Key => (cacheFileName, List, String),
     Usage => ///fn = cacheFileName(path,key)///,
     Inputs => {
	  "path" => "a search path (list) of prefixes from which to construct the
	        file name",
	  "key" => "a key"
	  },
     Outputs => {
	  "fn" => "a new file name"
	  },
     "The path should be a list of prefixes that correspond to existing
     directories.  A list of those file names for the given key that have already been
     assigned (see ", TO (cacheFileName, String, String), ") in one of
     the directories on the path will be returned.  In case no previous assignments
     to this key have occurred yet, one will be made using the first element of the
     search path."
     }


-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=CacheFileName pre-install"
-- End:
