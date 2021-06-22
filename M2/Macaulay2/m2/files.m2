--		Copyright 1993-1999 by Daniel R. Grayson

needs "methods.m2"

printpass := ID -> x -> (stderr << ID << ": " << x << endl; x)
fold3 := (f,x,v) -> (scan(v, y -> x = f(x,y)); x)
fold2 := (f,v) -> fold3(f,v#0,drop(v,1))
mergeopts := x -> fold2((a,b) -> merge(a,b,last), x)
makeDir := name -> if name != "" and (not fileExists name or not isDirectory (name | "/.")) then mkdir name

searchPath = method()
searchPath(List,String) := List => (pth,fn) -> searchPath'(pth,fn)
searchPath(String) := List => (fn) -> searchPath(path,fn)

makeDirectory = method()
makeDirectory String := name -> (			    -- make the whole path, too
     name = minimizeFilename name;
     parts := separate("/", name);
     if last parts === "" then parts = drop(parts,-1);
     makeDir fold((a,b) -> ( makeDir a; a|"/"|b ), parts))

copyFile = method(Options => new OptionTable from { Verbose => false, UpdateOnly => false })
copyFile(String,String) := opts -> (src,tar) -> (
     if src === tar then (
     	  if opts.Verbose then printerr("skipping: " | src | " the same as " | tar);
	  )
     else if opts.UpdateOnly and fileExists tar and fileTime src <= fileTime tar then (
     	  if opts.Verbose then printerr("skipping: " | src | " not newer than " | tar)
	  )
     else (
     	  if opts.Verbose then printerr("copying: " | src | " -> " | tar);
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
     bn := baseFilename name;
     if match(opts.Exclude, bn) then return {};
     if not fileExists name and readlink name === null then return {};
     if not (isDirectory name or opts.FollowLinks and isDirectory (name|"/.")) then return {name};
     if not name#-1 === "/" then name = name | "/";
     prepend(name,flatten apply(readDirectory name, 
	       f -> if f === "." or f === ".." then {} else findFiles(name|f,opts)))
     )
findFiles List := opts -> names -> flatten apply(names,findFiles)

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

copyDirectory = method( Options =>
    options copyFile ++ options findFiles ++ { Exclude => "\\.~[0-9.]+~$", Verbose => false })
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
		    else if match(opts.Exclude, srcf)
		    then (if opts.Verbose then stderr << "--  skipping: excluded file: " << srcf << endl)
		    else copyFile(srcf,tarf,applyPairs(options copyFile, (k,v) -> (k,opts#k)))))));

symlinkDirectory = method( Options =>
    options findFiles ++ { Exclude => "\\.~[0-9.]+~$", Undo => false, Verbose => false })
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
		    else if match(opts.Exclude, srcf)
		    then (if opts.Verbose then stderr << "--  skipping: excluded file: " << srcf << endl)
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

temporaryDirectoryName = null
temporaryDirectoryCounter = 0
-- Track the process ID: if we fork, then the child should (lazily) get a new temp dir and not clean up the parent's temp dir
temporaryDirectoryProcessID = -1
temporaryFilenameCounter = 0
addStartFunction( () -> (
	  temporaryDirectoryCounter = 0;
	  temporaryFilenameCounter = 0;
	  temporaryDirectoryName = null;
	  temporaryDirectoryProcessID = -1;
	  ))
addEndFunction( () -> (
	  if (temporaryDirectoryName =!= null and temporaryDirectoryProcessID === processID())
	  then scan(reverse findFiles temporaryDirectoryName, 
	       fn -> (
		    if isDirectory fn then (
			 if debugLevel === 111 then stderr << "-- removing directory " << fn << endl;
			 removeDirectory fn;
			 )
		    else (
			 if debugLevel === 111 then stderr << "-- removing file " << fn << endl;
			 removeFile fn;
			 )))))

temporaryDirectory = () -> (
     if (temporaryDirectoryName === null or temporaryDirectoryProcessID =!= processID())
     then temporaryDirectoryName = (
	  tmp := (
	       if getenv "TMPDIR" === ""
	       then "/tmp/"			     -- unix dependency here...
	       else getenv "TMPDIR"
	       );
	  if not match("/$",tmp) then tmp = tmp | "/";
	  if not isDirectory tmp then error("expected a directory: ", tmp);
	  if not fileExecutable tmp then error("expected a executable directory: ", tmp);
	  if not fileWritable tmp then error("expected a writable directory: ", tmp);
	  temporaryDirectoryProcessID=processID();
	  while true do (
	       fn := tmp | "M2-" | toString temporaryDirectoryProcessID | "-" | toString temporaryDirectoryCounter | "/";
	       temporaryDirectoryCounter = temporaryDirectoryCounter + 1;
	       try mkdir fn else continue;
	       break fn
	       )
	  )
     else temporaryDirectoryName
     )

temporaryFileName = () -> (
     fn := temporaryDirectory() | toString temporaryFilenameCounter;
     temporaryFilenameCounter = temporaryFilenameCounter + 1;
     fn
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
tt#"\\"= "_bs"			 -- can't occur in a file name: MS-DOS and sh
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
     -- meaning to Macaulay2.
     -- We should check which characters are allowed in URLs.
     s = concatenate("_",apply(characters s, c -> tt#c));
     s)

regexpString := s -> replace(///([][\.^$+*{()}])///,///\\1///,s)

supplantFileFile = (tmp,filename,backupq) -> (
     msg :=
     if fileExists filename then (
	  if backupq
	  then moveFile(filename,Verbose=>true)		    -- move file out of way
	  else removeFile filename;
	  "initialization text placed in file"
	  )
     else "file created for initialization text";
     linkFile(tmp,filename);
     removeFile tmp;
     stderr << "--" << msg << ": " << filename << endl;
     )

supplantStringFile = (text,filename,backupq) -> (
     text = replace("/PREFIX/",prefixDirectory,text);
     if fileExists filename and text === get filename then (
	  stderr << "--initialization text already in file: " << filename << endl;
	  return;
	  );
     tmp := filename | ".Macaulay2.tmp";
     tmp << text << close;
     supplantFileFile(tmp,filename,backupq);
     )

promptUser := true
mungeFile = (filename, headerline, trailerline, text) -> (
     headerline = headerline | newline;
     trailerline = trailerline | newline;
     text = replace("/PREFIX/",prefixDirectory,text);
     insert := headerline | text | trailerline;
     local action;
     if fileExists filename then (
     	  filename = realpath filename;	-- if filename is a symbolic link, we want to preserve that link and modify the underlying file
	  hdr := "^" | regexpString headerline;
	  tlr := "^" | regexpString trailerline;
     	  regexp := hdr | "(.|\n)*" | tlr ;
     	  oldcontents := get filename;
	  if match(regexp,oldcontents) then (
	       action = "insert text in";
	       excerpt := first select(regexp, oldcontents);
	       if 1 < length select(hdr,excerpt) or 1 < length select(tlr,excerpt) then (
		    error("multiple Macaulay2 insertion markers encountered in file: ", filename);
		    );
	       newcontents := replace(regexp, insert, oldcontents);
	       if oldcontents == newcontents then (
		    stderr << "--initialization text already in file, no changes needed: " << filename << endl;
		    return false;
		    );
	       )
	  else (
	       action = "append text to";
	       newcontents = oldcontents | newline | insert; 
	       )
	  )
     else (
	  action = "create";
	  newcontents = insert;
	  );
     tmp := filename | ".Macaulay2.tmp";
     tmp << newcontents << close;
     if promptUser then while true do (
	  response := toLower read concatenate(action, " ", format filename, " ? [y/n/r/q/!/?]: ");
	  if response == "y" then break else
	  if response == "n" then (removeFile tmp; return false) else
	  if response == "q" then (removeFile tmp; return true ) else
	  if response == "!" then (promptUser = false; break) else
	  if response == "?" then (
	       << ///   y     yes
   n     no
   r     review
   q     quit
   !     yes to this and to future questions
   ?     this explanation
///;	      ) else
     	  if response == "r" then (
	       << "new text proposed for file " << format filename << ":" << endl
	       << "  " << stack lines insert << endl;
	       if excerpt =!= null then
	       << "replacing existing text:" << endl
	       << "  " << stack lines excerpt << endl
	       )
	  else << "unrecognized response" << endl;
	  );
     supplantFileFile(tmp,filename,true);
     false)

emacstempl := ///
;; add "/PREFIX/DIR" to VAR if it isn't there
(add-to-list 'VAR "/PREFIX/DIR")
///

emacsenvtempl := ///
;; add "/PREFIX/DIR" to VAR if it isn't there
(if (not (string-match "/PREFIX/DIR" (getenv "VAR")))
     (setenv "VAR" "/PREFIX/DIR:$VAR" t))
///

dotemacsFix0 = ///
;; this version will give an error if M2-init.el is not found:
(load "M2-init")

;; this version will not give an error if M2-init.el is not found:
;(load "M2-init" t)

; You may comment out the following line with an initial semicolon if you 
; want to use your f12 key for something else.  However, this action
; will be undone the next time you run setup() or setupEmacs().
(global-set-key [ f12 ] 'M2)

; Prevent Emacs from inserting a superfluous "See" or "see" in front
; of the hyperlinks when reading documentation in Info mode.
(setq Info-hide-note-references 'hide)
///

emacsHeader := ";-*-emacs-lisp-*-\n"
shHeader := "#-*-sh-*-\n"

bashtempl := ///
## add "/PREFIX/DIR" to the environment variable VAR
case "$VAR" in 
     "/PREFIX/DIR"|"/PREFIX/DIR:"*|*":/PREFIX/DIR"|*":/PREFIX/DIR:"*) ;;
     "") VAR="/PREFIX/DIR:REST" ; export VAR ;;
     *) VAR="/PREFIX/DIR:$VAR" ; export VAR ;;
esac
///
cshtempl := ///
## add "/PREFIX/DIR" to the environment variable VAR
if ( $?VAR == 1 ) then
  switch ("$VAR")
    case "/PREFIX/DIR":
    case "/PREFIX/DIR:*":
    case "*:/PREFIX/DIR":
    case "*:/PREFIX/DIR:*":
       breaksw
    case "":
       setenv VAR "/PREFIX/DIR:REST"
       breaksw
    default:
       setenv VAR "/PREFIX/DIR:$VAR"
       breaksw
  endsw
else
  setenv VAR "/PREFIX/DIR"
endif
///

shellfixes := {
     ("PATH", currentLayout#"bin",""),
     ("MANPATH", currentLayout#"man",":"),
     ("INFOPATH", currentLayout#"info",":"),
     ("LD_LIBRARY_PATH", currentLayout#"lib","")}
emacsfixes := {
     ("load-path", currentLayout#"emacs", emacstempl),
     -- the exec-path fix is not needed, because we exec the shell and ask it to find M2
     -- ("exec-path", currentLayout#"bin", emacstempl),
     ("Info-default-directory-list", currentLayout#"info", emacstempl),
     ("PATH", currentLayout#"bin", emacsenvtempl)}

stripdir := dir -> if dir === "/" then dir else replace("/$","",dir)
fix := (var,dir,rest,templ) -> replace_(":REST",rest) replace_("VAR",var) replace_("DIR",stripdir dir) templ

startToken := "## Macaulay 2 start"
endToken := "## Macaulay 2 end"
M2profile := ".profile-Macaulay2"
M2login   := ".login-Macaulay2"
M2emacs   := ".emacs-Macaulay2"
M2profileRead := replace("filename",M2profile,
///if [ -f ~/filename ]
then . ~/filename
fi
///)
M2loginRead   := replace("filename",M2login,
///if ( -e ~/filename ) source ~/filename
///)
M2emacsRead   := replace("filename",format ("~/"|M2emacs),
///(load filename t)
///)

local dotprofileFix
local dotloginFix
local dotemacsFix

setupEmacs = method()
setup = method()
mungeEmacs = () -> (
     dotemacsFix = concatenate(emacsHeader, apply(emacsfixes, (var,dir,templ) -> fix(var,dir,"",templ)), dotemacsFix0);
     supplantStringFile(dotemacsFix,"~/"|M2emacs,false);
     mungeFile("~/"|".emacs", ";; Macaulay 2 start", ";; Macaulay 2 end", M2emacsRead )
     )
prelim := () -> (
     promptUser = true;
     if prefixDirectory === null then error "can't determine Macaulay 2 prefix (prefixDirectory not set)";
     )
installMethod(setupEmacs, () -> ( prelim(); mungeEmacs(); ))
installMethod(setup, () -> (
     prelim();
     dotprofileFix = concatenate(shHeader, apply(shellfixes, (var,dir,rest) -> fix(var,dir,rest,bashtempl)));
     dotloginFix = concatenate(shHeader,apply(shellfixes, (var,dir,rest) -> fix(var,dir,rest,cshtempl)));
     supplantStringFile(dotprofileFix,"~/"|M2profile,false);
     supplantStringFile(dotloginFix,"~/"|M2login,false);
     -- bash:
     --   from bash info:
     --     After reading that file, it looks for `~/.bash_profile',
     --     `~/.bash_login', and `~/.profile', in that order, and reads and
     --     executes commands from the first one that exists and is readable.
     mungeFile(     if fileExists "~/.bash_profile" then "~/.bash_profile"
	       else if fileExists "~/.bash_login"   then "~/.bash_login"
	       else "~/.profile",
	       startToken,endToken,M2profileRead) or
     -- zsh:
     mungeFile("~/.zprofile",startToken,endToken,M2profileRead) or
     -- csh and tcsh:
     mungeFile("~/.login",startToken,endToken,M2loginRead) or
     -- emacs:
     mungeEmacs(); ))

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
scanLines(Function,List) := (p,infs) -> scan(infs,inf->scanLines(p,inf))

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
