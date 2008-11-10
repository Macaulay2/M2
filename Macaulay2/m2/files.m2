--		Copyright 1993-1999 by Daniel R. Grayson

printpass := ID -> x -> (stderr << ID << ": " << x << endl; x)
fold3 := (f,x,v) -> (scan(v, y -> x = f(x,y)); x)
fold2 := (f,v) -> fold3(f,v#0,drop(v,1))
mergeopts := x -> fold2((a,b) -> merge(a,b,last), x)
makeDir := name -> if name != "" and (not fileExists name or not isDirectory (name | "/.")) then mkdir name

searchPath = method()
searchPath(List,String) := List => (pth,fn) -> select(apply(pth, dir -> dir | fn), fileExists)
searchPath(String) := List => (fn) -> searchPath(path,fn)

makeDirectory = method()
makeDirectory String := name -> (			    -- make the whole path, too
     name = minimizeFilename name;
     parts := separate("/", name);
     if last parts === "" then parts = drop(parts,-1);
     makeDir fold((a,b) -> ( makeDir a; a|"/"|b ), parts))

copyFile = method(Options => new OptionTable from { Verbose => false, UpdateOnly => false })
copyFile(String,String) := opts -> (src,tar) -> (
     if opts.UpdateOnly and fileExists tar and fileTime src <= fileTime tar then (
     	  if opts.Verbose then stderr << "--skipping: " << src << " not newer than " << tar << endl;
	  )
     else (
     	  if opts.Verbose then stderr << "--copying: " << src << " -> " << tar << endl;
     	  tar << get src << close;
     	  fileTime(fileTime src,tar);
     	  fileMode(fileMode src,tar);
	  )
     )

moveFile = method(Options => new OptionTable from { Verbose => false })
moveFile(String,String) := opts -> (src,tar) -> (
     if opts.Verbose then stderr << "--moving: " << src << " -> " << tar << endl;
     if not fileExists src then error("file '",src,"' doesn't exist");
     if fileExists tar then removeFile tar;
     linkFile(src,tar);
     removeFile src;
     )

moveFile String := opts -> src -> if fileExists src or readlink src =!= null then (
     bak := src | ".bak";
     for i from 1 do (
	  try linkFile(src,bak) else ( 
	       if not fileExists bak then error("failed to create backup file: ", bak);
	       bak = src | ".bak-" | toString i;
	       continue
	       );
	  if opts.Verbose then stderr << "--backup file created: " << bak << endl;
     	  removeFile src;
	  return bak))

baseFilename = fn -> (
     fn = separate("/",fn);
     while #fn > 0 and fn#-1 === "" do fn = drop(fn,-1);
     last fn)

findFiles = method(Options => new OptionTable from { Exclude => {}, FollowLinks => false })
findFiles String := opts -> name -> (
     excludes := opts.Exclude;
     if class excludes =!= List then (
     	  excludes = {excludes};
	  opts = mergeopts(opts, new OptionTable from {Exclude => excludes});
	  );
     bn := baseFilename name;
     if any(excludes, pattern -> match(pattern, bn)) then return {};
     if not fileExists name and readlink name === null then return {};
     if not (isDirectory name or opts.FollowLinks and isDirectory realpath name) then return {name};
     if not name#-1 === "/" then name = name | "/";
     prepend(name,flatten apply(readDirectory name, 
	       f -> if f === "." or f === ".." then {} else findFiles(name|f,opts)))
     )

backupFileRegexp = "\\.~[0-9.]+~$"					    -- we don't copy backup files.

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
copyDirectory = method(Options => mergeopts( options copyFile, options findFiles, new OptionTable from { Verbose => false }))
copyDirectory(String,String) := opts -> (src,dst) -> (
     if not fileExists src then error("directory not found: ",src);
     if not isDirectory src then error("file not a directory: ",src);
     if not src#-1 === "/" then src = src | "/";
     if not dst#-1 === "/" then dst = dst | "/";
     transform := fn -> dst | substring(fn,#src);
     scan(findFiles(src,applyPairs(options findFiles, (k,v) -> (k,opts#k))),
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
		    else copyFile(srcf,tarf,applyPairs(options copyFile, (k,v) -> (k,opts#k)))))));
symlinkDirectory = method( Options => mergeopts(options findFiles, new OptionTable from { Verbose => false, Undo => false }))
symlinkDirectory(String,String) := opts -> (src,dst) -> (
     if not fileExists src then error("directory not found: ",src);
     if not isDirectory src then error("file not a directory: ",src);
     if not src#-1 === "/" then src = src | "/";
     if not dst#-1 === "/" then dst = dst | "/";
     transform := fn -> dst | substring(fn,#src);
     scan(findFiles(src,applyPairs(options findFiles, (k,v) -> (k,opts#k))), 
	  srcf -> (
	       tarf := transform srcf;
	       if tarf#-1 === "/" 
	       then (
		    tarf' := substring(tarf, 0, #tarf-1);
		    if readlink tarf' =!= null then removeFile tarf'; -- don't traverse symbolic links in the target: and remove them.
		    if not isDirectory tarf then mkdir tarf;
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
					if opts.Verbose then stderr << "--  skipping: link already exists" << endl;
					null
					)
				   else if readlink tarf =!= null then (
					removeFile tarf;    -- silently unlink a symbolic link
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

String << Thing := File => (filename,x) -> (
     files := select(openFiles(), f -> toString f === filename);
     (
	  if #files === 0
     	  then openOut filename 
	  else if #files === 1 then first files
	  else error("multiple files already open with name ", format filename)	       
     	  ) << x)

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
     -- from occurring in the first position, where it would have a special
     -- meaning to Macaulay 2.
     -- We should check which characters are allowed in URLs.
     s = concatenate("_",apply(characters s, c -> tt#c));
     s)

regexpString := s -> replace(///([][\.^$+*{()}])///,///\\1///,s)

mungeFile = (filename, headerline, trailerline, text) -> (
     oldcontents := if fileExists filename then get filename else "";
     headerline = headerline | newline;
     trailerline = trailerline | newline;
     text = replace("/PREFIX/",prefixDirectory,text);
     insert := headerline | text | trailerline;
     hdr := "^" | regexpString headerline;
     tlr := "^" | regexpString trailerline;
     regexp := hdr | "(.|\n)*" | tlr ;
     if match(regexp,oldcontents) then (
     	  excerpt := first select(regexp, oldcontents);
	  if 1 < length select(hdr,excerpt) or 1 < length select(tlr,excerpt) then (
	       error("multiple Macaulay 2 insertion markers encountered in file: ", filename);
	       );
     	  newcontents := replace(regexp, insert, oldcontents);
     	  if oldcontents == newcontents then (
	       stderr << "--initialization text already in file, no changes needed: " << filename << endl;
	       return;
	       );
	  )
     else (
	  newcontents = oldcontents | newline | insert; 
	  );
     filename = realpath filename;			    -- no other editor does this, but it seems like a good idea...
     tmp := filename | ".Macaulay2.tmp";
     tmp << newcontents << close;
     if fileExists filename then (
	  stderr << "--initialization text about to be added to file: " << filename << endl;
	  moveFile(filename,Verbose=>true);		    -- move file out of way
	  )
     else (
	  stderr << "--file about to be created for initialization text: " << filename << endl;
	  );
     linkFile(tmp,filename);
     removeFile tmp;
     stderr << "--operation complete" << endl;
     )

dotemacsFix = ///
(setq load-path
       (append
        '( "/PREFIX/share/emacs/site-lisp/" )
        load-path))

;; this version will give an error if M2-init.el is not found:
(load "M2-init")

;; this version will not give an error if M2-init.el is not found:
;(load "M2-init" t)

; comment out the following line with an initial semicolon if you 
; want to use your f12 key for something else
(global-set-key [ f12 ] 'M2)
///

bashtempl := ///
case "$VAR" in 
     "/PREFIX/DIR"|"/PREFIX/DIR:"*|*":/PREFIX/DIR"|*":/PREFIX/DIR:"*) ;;
     "") VAR="/PREFIX/DIR" ; export VAR ;;
     *) VAR="/PREFIX/DIR:$VAR" ; export VAR ;;
esac
///

cshtempl := ///
if ( $?VAR == 1 ) then
  switch ("$VAR")
    case "/PREFIX/DIR":
    case "/PREFIX/DIR:*":
    case "*:/PREFIX/DIR":
    case "*:/PREFIX/DIR:*":
       breaksw
    case "":
       setenv VAR "/PREFIX/DIR"
       breaksw
    default:
       setenv VAR "/PREFIX/DIR:$VAR"
       breaksw
  endsw
else
  setenv VAR "/PREFIX/DIR"
endif
///
stripdir := dir -> if dir === "/" then dir else replace("/$","",dir)
fixes := { ("PATH", LAYOUT#"bin"), ("MANPATH", LAYOUT#"man"), ("INFOPATH", LAYOUT#"info" ), ("LD_LIBRARY_PATH", LAYOUT#"lib" )}
fix := (var,dir,templ) -> replace_("VAR",var) replace_("DIR",stripdir dir) templ
dotprofileFix = dotbashrcFix = concatenate apply(fixes, (var,dir) -> fix(var,dir,bashtempl))
dotloginFix = dotcshrcFix = concatenate apply(fixes, (var,dir) -> fix(var,dir,cshtempl))

setupEmacs = method()
setup = method()
installMethod(setupEmacs, () -> (
     if prefixDirectory === null then error "can't determine Macaulay 2 prefix (prefixDirectory not set)";
     mungeFile( homeDirectory | ".emacs", ";; Macaulay 2 start", ";; Macaulay 2 end", dotemacsFix );
     ))
installMethod(setup, () -> (
     if prefixDirectory === null then error "can't determine Macaulay 2 prefix (prefixDirectory not set)";
     ---- from bash info:
     -- After reading that file, it looks for `~/.bash_profile',
     -- `~/.bash_login', and `~/.profile', in that order, and reads and
     -- executes commands from the first one that exists and is readable.
     if fileExists(homeDirectory | ".bash_profile") then mungeFile( homeDirectory | ".bash_profile", "## Macaulay 2 start", "## Macaulay 2 end", dotprofileFix );
     if fileExists(homeDirectory | ".bash_login"  ) then mungeFile( homeDirectory | ".bash_login"  , "## Macaulay 2 start", "## Macaulay 2 end", dotprofileFix );
     mungeFile( homeDirectory | ".profile", "## Macaulay 2 start", "## Macaulay 2 end", dotprofileFix );
     mungeFile( homeDirectory | ".login", "## Macaulay 2 start", "## Macaulay 2 end", dotloginFix );
     mungeFile( homeDirectory | ".cshrc", "## Macaulay 2 start", "## Macaulay 2 end", dotcshrcFix );
     -- tcsh reads .tcshrc, or, if that doesn't exists, .cshrc
     if fileExists(homeDirectory | ".tcshrc"  ) then mungeFile( homeDirectory | ".tcshrc"  , "## Macaulay 2 start", "## Macaulay 2 end", dotcshrcFix );
     mungeFile( homeDirectory | ".bashrc", "## Macaulay 2 start", "## Macaulay 2 end", dotbashrcFix );
     setupEmacs();
     ))


scanLines = method()
ifbrk := x -> if x =!= null then break x
scanLines(Function,String) := (p,inf) -> (		    -- the function p can use "break x" with a non-null value for x to return prematurely, with x as value
     inf = openIn inf;
     tail := "";
     ret := while not atEndOfFile inf do (
	  r := separate read inf;
	  front := concatenate(tail,r#0);
	  if #r === 1 then tail = front
	  else (
	       p front;
 	       ifbrk for i from 1 to #r-2 do p r#i;
	       );
	  tail = if #r > 1 then r#-1 else front
	  );
     close inf;
     if #tail > 0 then p tail;
     ret)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
