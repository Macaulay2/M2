--		Copyright 1993-1999 by Daniel R. Grayson

makeDir := name -> if not fileExists name then mkdir name

makeDirectory = method()
makeDirectory String := name -> (
     name = minimizeFilename name;
     parts := separate("/", name);
     if last parts === "" then parts = drop(parts,-1);
     if first parts === "" then parts = prepend("/"|first parts, drop(parts,1));
     makeDir fold((a,b) -> ( makeDir a; a|"/"|b ), parts))

fileOptions := new OptionTable from { 
     Exclude => set {},	-- eventually we change from a set to a regular expression or a list of them
     Verbose => false 
     }

copyFile = method(Options => fileOptions)
copyFile(String,String) := opts -> (src,tar) -> (
     if opts.Verbose then stderr << "--copying: " << src << " -> " << tar << endl;
     tar << get src << close;
     )

baseFilename = fn -> (
     fn = separate("/",fn);
     while #fn > 0 and fn#-1 === "" do fn = drop(fn,-1);
     last fn)

findFiles = method(Options => fileOptions)
findFiles String := opts -> name -> (
     ex := opts.Exclude;
     if class ex =!= Set then (
     	  if class ex =!= List then ex = {ex};
     	  ex = set ex;
	  opts = merge(opts, new OptionTable from {Exclude => ex}, last);
	  );
     if ex#?(baseFilename name) or not fileExists name then return {};
     if not isDirectory name then return {name};
     if not name#-1 === "/" then name = name | "/";
     prepend(name,flatten apply(drop(readDirectory name,2), f -> findFiles(name|f,opts)))
     )

copyDirectory = method(Options => fileOptions)
-- The unix 'cp' command is confusing when copying directories, because the
-- result depends on whether the destination exists:
--    % ls
--    % mkdir -p a/bbbb
--    % mkdir t
--    % cp -a a t
--    % cp -a a u
--    % ls t
--    a
--    % ls u
--    bbbb
-- One way to make it less confusing is to name '.' as the source, but the
-- definition of recursive copying is still unclear.
--    % mkdir v
--    % cp -a a/. v
--    % cp -a a/. w
--    % ls v
--    bbbb
--    % ls w
--    bbbb
-- One result of the confusion is that doing the command twice can result in
-- something different the second time.  We wouldn't want that!
--    % cp -a a z
--    % ls z
--    bbbb
--    % cp -a a z
--    % ls z
--    a  bbbb
-- So we make our 'copyDirectory' function operate like 'cp -a a/. v'.
-- For safety, we insist the destination directory already exist.
-- Normally the base names of the source and destination directories will be
-- the same.
copyDirectory(String,String) := opts -> (src,dst) -> (
     if not fileExists src then error("directory not found: ",src);
     if not isDirectory src then error("file not a directory: ",src);
     if not src#-1 === "/" then src = src | "/";
     if not dst#-1 === "/" then dst = dst | "/";
     transform := fn -> dst | substring(fn,#src);
     scan(findFiles(src,opts), 
	  srcf -> (
	       tarf := transform srcf;
	       if tarf#-1 === "/" 
	       then (
		    if not isDirectory tarf then mkdir tarf 
		    )
	       else (
     		    if not isRegularFile srcf 
		    then stderr << "--skipping: non regular file: " << srcf << endl
		    else copyFile(srcf,tarf,opts)
		    )
	       )
	  )
     );

-----------------------------------------------------------------------------

String << Thing := File => (filename,x) -> openOut filename << x

counter := 0

temporaryFileName = () -> (
     counter = counter + 1;
     "/tmp/M2-" | toString processID() | "-" | toString counter
     )
-----------------------------------------------------------------------------
tt := new MutableHashTable from toList apply(0 .. 255, i -> (
	  c := ascii i;
	  c => c
	  ));

tt#" " = "_sp_"           -- can't occur in a URL and has a meaning for xargs
tt#"#" = "_sh_"					     -- has a meaning in URLs
tt#"$" = "_do_"					-- has a meaning for gnu make
tt#"%" = "_pc_"					     -- has a meaning in URLs
tt#"'" = "_sq_"					   -- has a meaning for xargs
tt#"/" = "_sl_"				  -- can't occur in a file name: unix
tt#":" = "_co_"					-- has a meaning for gnu make
tt#";" = "_se_"					-- has a meaning for gnu make
tt#"?" = "_qu_"					     -- has a meaning in URLs
tt#"\""= "_dq_"					 -- " has a meaning for xargs
tt#"\\"= "_bs_"				 -- can't occur in a file name: MSDOS
tt#"_" = "_us_"					      -- our escape character
for i from 1 to 26 do (			    -- some OSes are case insensitive
     cap := ascii (64 + i);
     low := ascii (96 + i);
     tt#cap = concatenate("_", low, "_");
     )

toFilename = method()
toFilename String := s -> (
     -- Convert a string to a new string usable as a file name, and with
     -- at least one special character "_" prefixed, to avoid collisions with
     -- other file names such as "index.html".
     -- Notice that the prefix character _ prevents the "!" character
     -- from occuring in the first position, where it would have a special
     -- meaning to Macaulay 2.
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
