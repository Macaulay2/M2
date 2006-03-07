--		Copyright 1993-1999 by Daniel R. Grayson

printpass := ID -> x -> (stderr << ID << ": " << x << endl; x)

makeDir := name -> if name != "" and (not fileExists name or not isDirectory (name | "/.")) then mkdir name

length File := f -> #f

makeDirectory = method()
makeDirectory String := name -> (			    -- make the whole path, too
     name = minimizeFilename name;
     parts := separate("/", name);
     if last parts === "" then parts = drop(parts,-1);
     makeDir fold((a,b) -> ( makeDir a; a|"/"|b ), parts))

fileOptions := new OptionTable from { 
     Exclude => {},
     Verbose => false,
     Undo => false,
     UpdateOnly => false
     }

copyFile = method(Options => fileOptions)
copyFile(String,String) := opts -> (src,tar) -> (
     if opts.UpdateOnly and fileExists tar and fileTime src <= fileTime tar then (
     	  if opts.Verbose then stderr << "--skipping: " << src << " not newer than " << tar << endl;
	  )
     else (
     	  if opts.Verbose then stderr << "--copying: " << src << " -> " << tar << endl;
     	  tar << get src << close;
     	  fileTime(tar,fileTime src);
     	  fileMode(tar,fileMode src);
	  )
     )

moveFile = method(Options => fileOptions)
moveFile(String,String) := opts -> (src,tar) -> (
     if opts.Verbose then stderr << "--moving: " << src << " -> " << tar << endl;
     if not fileExists src then error("file '",src,"' doesn't exist");
     if fileExists tar then unlinkFile tar;
     linkFile(src,tar);
     unlinkFile src;
     )

baseFilename = fn -> (
     fn = separate("/",fn);
     while #fn > 0 and fn#-1 === "" do fn = drop(fn,-1);
     last fn)

findFiles = method(Options => fileOptions)
findFiles String := opts -> name -> (
     excludes := opts.Exclude;
     if class excludes =!= List then (
     	  excludes = {excludes};
	  opts = merge(opts, new OptionTable from {Exclude => excludes}, last);
	  );
     bn := baseFilename name;
     if any(excludes, pattern -> match(pattern, bn)) then return {};
     if not fileExists name then return {};
     if not isDirectory name then return {name};
     if not name#-1 === "/" then name = name | "/";
     prepend(name,flatten apply(readDirectory name, 
	       f -> if f === "." or f === ".." then {} else findFiles(name|f,opts)))
     )

backupFileRegexp = "\\.~[0-9.]+~$"					    -- we don't copy backup files.

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
		    if not isDirectory tarf then makeDirectory tarf 
		    )
	       else (
     		    if not isRegularFile srcf 
		    then (if opts.Verbose then stderr << "--  skipping: non regular file: " << srcf << endl)
		    else if match(backupFileRegexp,srcf)
		    then (if opts.Verbose then stderr << "--  skipping: backup file: " << srcf << endl)
		    else copyFile(srcf,tarf,opts)))));
symlinkDirectory = method(Options => fileOptions)
symlinkDirectory(String,String) := opts -> (src,dst) -> (
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
		    then (if opts.Verbose then stderr << "--  skipping: non regular file: " << srcf << endl)
		    else if match(backupFileRegexp,srcf)
		    then (if opts.Verbose then stderr << "--  skipping: backup file: " << srcf << endl)
		    else (
			 tardir := concatenate between("/",drop(separate("/",tarf),-1)); -- directory part of file name
			 relsrcf := relativizeFilename(tardir,srcf);
			 if not opts.Undo then (
			      if opts.Verbose then stderr << "--symlinking: " << relsrcf << " -> " << tarf << endl;
			      if fileExists tarf then (
				   if readlink tarf === relsrcf
				   then (
					-- stderr << "--  skipping: link already exists" << endl;
					null
					)
				   else if readlink tarf =!= null then (
					unlinkFile tarf;    -- silently unlink a symbolic link
					symlinkFile(relsrcf,tarf);
					)
				   else error("file ", tarf, " already exists, not what we want");
				   )
			      else symlinkFile(relsrcf,tarf))
			 else (
			      if opts.Verbose then stderr << "--unsymlinking: " << relsrcf << " -> " << tarf << endl;
			      if not fileExists tarf then (
				   -- stderr << "--  skipping: link does not exist" << endl;
				   )
			      else if relsrcf =!= readlink tarf then (
				   stderr << "--  skipping: unexpected file " << tarf << endl;
				   )
			      else removeFile tarf))))))

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

tt#" " = "_sp"            -- can't occur in a URL and has a meaning for xargs
tt#"*" = "_st"					       -- has a meaning in sh
tt#"|" = "_vb"					       -- has a meaning in sh
tt#"(" = "_lp"					       -- has a meaning in sh
tt#")" = "_rp"					       -- has a meaning in sh
tt#"<" = "_lt"					       -- has a meaning in sh
tt#">" = "_gt"					       -- has a meaning in sh
tt#"&" = "_am"				   -- has a meaning in sh and in URLs
tt#"@" = "_at"					     -- has a meaning in URLs
tt#"=" = "_eq"					     -- has a meaning in URLs
tt#"," = "_cm"					     -- has a meaning in URLs
tt#"#" = "_sh"					     -- has a meaning in URLs
tt#"+" = "_pl"					     -- has a meaning in URLs
tt#"$" = "_do"		       -- has a meaning for gnu make, sh, and in URLs
tt#"%" = "_pc"					     -- has a meaning in URLs
tt#"'" = "_sq"					   -- has a meaning for xargs
tt#"/" = "_sl"			   -- has a meaning in file names and in URLs
tt#":" = "_co"			    -- has a meaning for gnu make and in URLs
tt#";" = "_se"			    -- has a meaning for gnu make and in URLs
tt#"?" = "_qu"				      -- has a meaning in URLs and sh
tt#"\""= "_dq"					 -- " has a meaning for xargs
tt#"\\"= "_bs"			  -- can't occur in a file name: MSDOS and sh
tt#"_" = "_us"					      -- our escape character

-- some OSes are case insensitive:
apply(characters "ABCDEFGHIJKLMNOPQRSTUVWXYZ", cap -> tt#cap = concatenate("__", cap))

toFilename = method()
toFilename String := s -> (
     -- Convert a string to a new string usable as a file name, and with
     -- at least one special character "_" prefixed, to avoid collisions with
     -- other file names such as "index.html".
     -- Notice that the prefix character _ prevents the "!" character
     -- from occuring in the first position, where it would have a special
     -- meaning to Macaulay 2.
     -- We should check which characters are allowed in URLs.
     s = concatenate("_",apply(characters s, c -> tt#c));
     s)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
