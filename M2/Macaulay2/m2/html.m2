--		Copyright 1993-2002 by Daniel R. Grayson

-- produce html form of documentation, for Macaulay 2 and for packages

local prefix, local topNodeName, local topFileName, local topNodeButton
local haderror, local databaseFileName, local nullButton, local masterIndexButton, local masterFileName
local NEXT, local PREV, local UP
local nextButton, local prevButton, local upButton
local lastKey, local thisKey
local linkFollowedTable, local masterIndex
local docdatabase

linkFilename := s -> first cacheFileName(first documentationPath, documentationPath, s) | ".html"

documentationMemo := memoize documentation		    -- for speed

BUTTON := (s,alt) -> (
     if not fileExists s then error ("file ", s, " doesn't exist");
     s = relativizeFilename(prefix,s);
     if alt === null
     then LITERAL concatenate("<IMG src=\"",s,"\" border=0 align=center>")
     else LITERAL concatenate("<IMG src=\"",s,"\" border=0 align=center alt=\"[", alt, "]\">")
     )

upAncestors := key -> reverse (
     n := 0;
     while UP#?key and n < 20 list (n = n+1; key = UP#key)
     )

next := key -> if NEXT#?key then HREF { linkFilename NEXT#key, nextButton } else nullButton
prev := key -> if PREV#?key then HREF { linkFilename PREV#key, prevButton } else nullButton
up   := key -> if   UP#?key then HREF { linkFilename   UP#key,   upButton } else nullButton

scope := method(SingleArgumentDispatch => true)
scope2 := method(SingleArgumentDispatch => true)
scope1 := method(SingleArgumentDispatch => true)

follow := key -> (
     fkey := formatDocumentTag key;
     if not linkFollowedTable#?fkey then (
	  fn := linkFilename fkey;
	  if prefix == substring(fn,0,#prefix) then (	    -- don't stray outside this package
	       linkFollowedTable#fkey = true;
	       if class key =!= Option and class key =!= Sequence then masterIndex#(fkey,key) = true;
	       saveThisKey := thisKey;
	       saveLastKey := lastKey;
	       thisKey = fkey;
	       lastKey = null;
	       scope documentationMemo key;
	       thisKey = saveThisKey;
	       lastKey = saveLastKey;
	       )
	  else (
	       linkFollowedTable#fkey = false;
	       )
	  )
     )

-- scanning at top level
scope Thing := x -> null
scope Sequence := scope BasicList := x -> scan(x,scope)
scope SHIELD := x -> scan(x,scope1)
scope MENU := x -> scan(x,scope2)
scope TO := scope TOH := x -> follow x#0

-- scanning inside a SHIELD
scope1 Thing := x -> null
scope1 Sequence := scope1 BasicList := x -> scan(x,scope1)
scope1 TO := scope1 TOH := x -> follow x#0

-- scanning inside a MENU not inside a SHIELD
scope2 Thing := scope
scope2 SEQ := x -> scan(x,scope2)
scope2 SHIELD := x -> scan(x,scope1)
scope2 TO := scope2 TOH := x -> (
     -- here we construct the ordered tree needed for presentation in book format,
     -- with UP, NEXT, and PREVIOUS pointers.
     key := formatDocumentTag x#0;
     if UP#?key 
     then (
	  stderr << "error: links to '" << key << "' from two nodes: '"
	  << UP#key << "' and '" << thisKey << "'" << endl
	  )
     else if key == thisKey then stderr << "error: node " << key << " links to itself" << endl
     else (
	  UP#key = thisKey;
	  if lastKey =!= null then (
	       PREV#key = lastKey;
	       NEXT#lastKey = key;
	       );
	  lastKey = key;
	  );
     follow x#0;
     )

buttonBar := (key) -> CENTER {
     next key,
     prev key, 
     up key,
     if key =!= topNodeName then topNodeButton else nullButton,
     masterIndexButton,
     LITERAL concatenate (///
     <form action="///,					    -- "
     if getenv "SEARCHENGINE" === "" 
     then "http://rhenium.math.uiuc.edu:7003/" else getenv "SEARCHENGINE",
     ///">
	search:
	<input type="text"   name="words">
	<input type="hidden" name="method"   value="boolean">
	<input type="hidden" name="format"   value="builtin-short">
	<input type="hidden" name="sort"     value="score">
	<input type="hidden" name="config"   value="htdig-M2">
     </form>
     ///)						    -- "
     }

pass1 := () -> (
     << "pass 1, finding undocumented symbols" << endl;
     scan(undocumentedSymbols(), 
	  x -> (
	       haderror = true;
	       follow toString x;
	       )
	  )
     )

pass2 := () -> (
     << "pass 2, descending through documentation tree" << endl;
     follow topNodeName;
     )

pass3 := () -> (
     << "pass 3, checking for unreachable documentation nodes" << endl;
     scan(keys docdatabase,
	  key -> (
	       if not linkFollowedTable#?key then (
		    haderror = true;
		    stderr << "error: documentation node '" << key << "' not reachable" << endl ) ) ) )

pass4 := () -> (
     << "pass 4, writing html files" << endl;
     scan(keys linkFollowedTable, fkey -> if linkFollowedTable#fkey then (
	       linkFilename fkey
	       << html HTML { 
		    HEAD TITLE {fkey, headline fkey},
		    BODY { 
			 buttonBar fkey,
			 if UP#?fkey then SEQ {
			      "Parent headings:",
			      MENU apply(upAncestors fkey, i -> TOH i)
			      },
			 HR{}, 
			 documentationMemo fkey,
			 }
		    }
	       << endl << close ) ) )

-----------------------------------------------------------------------------

alpha := characters "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
anchorPoint := 0
anchor := entry -> if alpha#?anchorPoint and entry >= alpha#anchorPoint then (
     s := select(drop(alpha,anchorPoint), c -> entry >= c);
     anchorPoint = anchorPoint + #s;
     SEQ apply(s, c -> ANCHOR {c, ""})
     )

pass5 := () -> (
     << "pass 5, creating the master index" << endl;
     masterNodeName := topNodeName | " Index";
     masterFileName << html HTML {
	  HEAD { TITLE masterNodeName },
	  BODY {
	       HEADER2 masterNodeName, PARA,
	       CENTER topNodeButton, PARA,
	       CENTER between(LITERAL "&nbsp;&nbsp;&nbsp;",apply(alpha, c -> HREF {"#"|c, c})), PARA,
	       MENU apply(sort keys masterIndex, (fkey,key) -> SEQ { anchor fkey, TOH key })
	       }
	  } << endl << close
     )

-----------------------------------------------------------------------------
-- making the html pages
-----------------------------------------------------------------------------

checkDirectory := path -> (
     if not fileExists (path | ".")		     -- what about Macintosh?
     then (
	  if fileExists path 
	  then error ("directory ", path, " doesn't end with a path separator")
	  else error ("directory ", path, " doesn't exist")
	  )
     )

checkFile := filename -> (
     if not fileExists filename then error ("file ", filename, " doesn't exist");
     filename)

cacheVars := varlist -> (
     valuelist := apply(varlist, x -> value x);
     () -> apply(varlist, valuelist, (x,n) -> x <- n))

makeHTML = (topnode,docdatabase0) -> (
     restore := cacheVars{symbol documentationPath};
     gifpath := minimizeFilename( getenv "M2HOME" | "/html/" );
     prefix = "cache/doc/";
     docdatabase = docdatabase0;
     checkDirectory prefix;
     checkDirectory gifpath;
     documentationPath = unique prepend(prefix,documentationPath);
     topNodeName = topnode;
     topFileName = cacheFileName(prefix,topNodeName) | ".html";
     topNodeButton = HREF { topFileName, BUTTON (checkFile(gifpath|"top.gif"),"top") };
     databaseFileName = "../cache/Macaulay2-doc";
     nullButton = BUTTON(checkFile(gifpath|"null.gif"),null);
     masterFileName = prefix | "master.html";
     masterIndexButton = HREF { masterFileName, BUTTON(checkFile(gifpath|"index.gif"),"index") };
     nextButton = BUTTON(checkFile(gifpath|"next.gif"),"next");
     prevButton = BUTTON(checkFile(gifpath|"previous.gif"),"previous");
     upButton = BUTTON(checkFile(gifpath|"up.gif"),"up");
     sav := if htmlDefaults#?"BODY" then htmlDefaults#"BODY";
     htmlDefaults#"BODY" = concatenate(
	  "BACKGROUND=\"",				    -- "
	  relativizeFilename(prefix,checkFile(gifpath|"recbg.jpg")),
	  "\""						    -- "
	  );
     lastKey = null;
     thisKey = null;
     linkFollowedTable = new MutableHashTable;
     masterIndex = new MutableHashTable;
     NEXT = new MutableHashTable;
     PREV = new MutableHashTable;
     UP   = new MutableHashTable;
     haderror = false;
     setrecursionlimit first (
	  setrecursionlimit 4000,
     	  time pass1(),
     	  time pass2(),
     	  time pass3(),
     	  time pass4(),
     	  time pass5()
	  );
     if sav =!= null then htmlDefaults#"BODY" = sav;
     if haderror then (
	  stderr << "error: ignoring documentation errors" << endl;
	  -- error "documentation errors occurred";
	  );
     restore();
     )     
