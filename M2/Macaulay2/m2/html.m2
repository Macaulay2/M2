--		Copyright 1993-2002 by Daniel R. Grayson

-- produce html form of documentation, for Macaulay 2 and for packages

local prefix, local topNodeName, local topFileName, local topNodeButton
local haderror, local databaseFileName, local nullButton, local masterIndexButton, local masterFileName
local NEXT, local PREV, local UP
local nextButton, local prevButton, local upButton
local lastKey, local thisKey
local linkFollowedTable, local masterIndex

linkFilename := s -> cacheFileName(prefix,s) | ".html";

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
scope3 := method(SingleArgumentDispatch => true)

follow := key -> (
     fkey := formatDocumentTag key;
     if not linkFollowedTable#?fkey then (
	  if class key =!= Option and class key =!= Sequence then masterIndex#(fkey,key) = true;
	  linkFollowedTable#fkey = true;
	  linkFilename fkey;
	  saveThisKey := thisKey;
	  saveLastKey := lastKey;
	  thisKey = fkey;
	  lastKey = null;
	  scope documentationMemo key;
	  thisKey = saveThisKey;
	  lastKey = saveLastKey;
	  )
     )

scope Thing := x -> null
scope Sequence := scope BasicList := x -> scan(x,scope)
scope SHIELD := x -> scan(x,scope3)
scope MENU := x -> scan(x,scope2)
scope TO := scope TOH := x -> follow x#0

scope3 Thing := x -> null
scope3 Sequence := scope3 BasicList := x -> scan(x,scope3)
scope3 TO := scope3 TOH := x -> follow x#0

scope2 Thing := scope
scope2 SEQ := x -> scan(x,scope2)
scope2 TO := scope2 TOH := x -> (
     key := formatDocumentTag x#0;
     if not UP#?key and key != thisKey then (
	  UP#key = thisKey;
	  if lastKey =!= null then (
	       PREV#key = lastKey;
	       NEXT#lastKey = key;
	       );
	  lastKey = key;
	  )
     else (
	  << "links to '" << key << "' from two nodes: '" << UP#key << "' and '" << thisKey << "'" << endl;
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
     << "pass 1, descending through documentation tree" << endl;
     follow topNodeName;
     )

pass2 := () -> (
     << "pass 2, checking for unreachable documentation nodes" << endl;
     scanKeys(DocDatabase,
	  key -> (
	       if not linkFollowedTable#?key then (
		    haderror = true;
		    stderr << "documentation node '" << key << "' not reachable" << endl ) ) ) )

pass3 := () -> (
     << "pass 3, writing html files" << endl;
     scan(keys linkFollowedTable, fkey -> (
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

alpha := characters "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
anchorPoint := 0
anchor := entry -> if alpha#?anchorPoint and entry >= alpha#anchorPoint then (
     s := select(drop(alpha,anchorPoint), c -> entry >= c);
     anchorPoint = anchorPoint + #s;
     SEQ apply(s, c -> ANCHOR {c, ""})
     )

pass4 := () -> (
     << "pass 4, creating the master index" << endl;
     masterNodeName := "master index";
     masterFileName << html HTML {
	  HEAD { TITLE masterNodeName },
	  BODY {
	       HEADER2 masterNodeName, PARA,
	       CENTER topNodeButton, PARA,
	       CENTER between(LITERAL "&nbsp;&nbsp;&nbsp;",apply(alpha, c -> HREF {"#"|c, c})), PARA,
	       MENU apply(sort keys masterIndex, (fkey,key) -> SEQ { anchor fkey, TO key }), PARA,
	       CENTER between(LITERAL "&nbsp;&nbsp;&nbsp;",apply(alpha, c -> HREF {"#"|c, c})), PARA,
	       CENTER topNodeButton
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

makeHTML = (topnode,gifpath,prefix0) -> (
     prefix = prefix0;
     checkDirectory prefix;
     checkDirectory gifpath;
     documentationPath = unique prepend(prefix,documentationPath);
     topNodeName = topnode;
     topFileName = cacheFileName(prefix,topNodeName) | ".html";
     topNodeButton = HREF { topFileName, BUTTON(gifpath|"top.gif","top") };
     databaseFileName = "../cache/Macaulay2-doc";
     nullButton = BUTTON(gifpath|"null.gif",null);
     masterFileName = prefix | "master.html";
     masterIndexButton = HREF { masterFileName, BUTTON(gifpath|"index.gif","index") };
     nextButton = BUTTON(gifpath|"next.gif","next");
     prevButton = BUTTON(gifpath|"previous.gif","previous");
     upButton = BUTTON(gifpath|"up.gif","up");
     lastKey = null;
     thisKey = null;
     linkFollowedTable = new MutableHashTable;
     masterIndex = new MutableHashTable;
     NEXT = new MutableHashTable;
     PREV = new MutableHashTable;
     UP   = new MutableHashTable;
     sav := if htmlDefaults#?"BODY" then htmlDefaults#"BODY";
     htmlDefaults#"BODY" = concatenate(
	  "BACKGROUND=\"",				    -- "
	  relativizeFilename(prefix,"recbg.jpg"),
	  "\""						    -- "
	  );
     haderror = false;
     setrecursionlimit first (
	  setrecursionlimit 4000,
     	  time pass1(),
     	  time pass2(),
     	  time pass3(),
     	  time pass4()
	  );
     if sav =!= null then htmlDefaults#"BODY" = sav;
     if haderror then error "documentation errors occurred";
     )     
