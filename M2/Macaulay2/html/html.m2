--		Copyright 1993-1999 by Daniel R. Grayson

htmlDefaults#"BODY" = "BACKGROUND='recbg.jpg'"

setrecursionlimit 4000

documentationMemo := memoize documentation

databaseFileName = "../cache/Macaulay2-doc"
errorDepth 0

BUTTON = (s,alt) -> (
     if alt === null
     then LITERAL concatenate("<IMG src=\"",s,"\" border=0 align=center>")
     else LITERAL concatenate("<IMG src=\"",s,"\" border=0 align=center alt=\"[", alt, "]\">")
     )

prefix = ""
topNodeName = "Macaulay 2"
topFileName = cacheFileName(prefix,topNodeName,"index") | ".html"
linkFilename = s -> cacheFileName(prefix,s) | ".html"

topNodeButton = HREF { topFileName, BUTTON("top.gif","top") }

nullButton = BUTTON("null.gif",null)

masterFileName = "master.html"
masterNodeName = "master index"
masterIndexButton = HREF { masterFileName, BUTTON("index.gif","index") }


htmlLiteralTable := new MutableHashTable
scan(characters ascii(0 .. 255), c -> htmlLiteralTable#c = c)
htmlLiteralTable#"\"" = "&quot;"
htmlLiteralTable#"<" = "&lt;"
htmlLiteralTable#"&" = "&amp;"
htmlLiteralTable#">" = "&gt;"
htmlLiteral := s -> concatenate apply(characters s, c -> htmlLiteralTable#c)

htmlExtraLiteralTable := copy htmlLiteralTable
htmlExtraLiteralTable#" " = "&nbsp;"
htmlExtraLiteral := s -> concatenate apply(characters s, c -> htmlExtraLiteralTable#c)

html TO := x -> (
     fkey := formatDocumentTag x#0;
     concatenate("<A HREF=\"", linkFilename fkey, "\">", htmlExtraLiteral fkey, "</A>", drop(toList x,1))
     )

NEXT = new MutableHashTable
PREV = new MutableHashTable
UP   = new MutableHashTable

nextButton = BUTTON("next.gif","next")
prevButton = BUTTON("previous.gif","previous")
upButton = BUTTON("up.gif","up")

next = key -> if NEXT#?key then HREF { linkFilename NEXT#key, nextButton } else nullButton
prev = key -> if PREV#?key then HREF { linkFilename PREV#key, prevButton } else nullButton
up   = key -> if   UP#?key then HREF { linkFilename   UP#key,   upButton } else nullButton

scope := method(SingleArgumentDispatch => true)
scope2 := method(SingleArgumentDispatch => true)
scope3 := method(SingleArgumentDispatch => true)

lastKey := null
thisKey := null

linkFollowedTable := new MutableHashTable
masterIndex = new MutableHashTable
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

scope3 Thing := scope
scope3 MENU := x -> scan(x,scope)

scope2 Thing := scope
scope2 TO := scope2 TOH := x -> (
     key := formatDocumentTag x#0;
     if not UP#?key then (
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

--      LITERAL ///
--      <form action="/SFgate/cgi-bin/SFgate">
-- 	<input type="hidden" name="database" value="localhost:2200/Macaulay2">
-- 	search for: <input name="text">
--      </form>
--      ///


buttonBar = (key) -> CENTER {
     next key,
     prev key, 
     up key,
     if key =!= topNodeName then topNodeButton else nullButton,
     masterIndexButton,
     LITERAL concatenate (///
     <form action="///,
     if getenv "SEARCHENGINE" === "" then "/cgi-bin/htsearch" else getenv "SEARCHENGINE",
     ///">
	search for:
	<input type="text"   name="words">
	<input type="hidden" name="method"   value="boolean">
	<input type="hidden" name="format"   value="builtin-short">
	<input type="hidden" name="sort"     value="score">
	<input type="hidden" name="config"   value="htdig-M2">
     </form>
     ///)
     }
	  
haderror = false

<< "pass 1, descending through documentation tree" << endl
time follow topNodeName

<< "pass 2, checking for unreachable documentation nodes" << endl
docFile := openDatabase databaseFileName
time scanKeys(docFile,
     key -> (
	  if not match(docFile#key,"goto *") then (
	       fkey := formatDocumentTag value key;
	       if not linkFollowedTable#?fkey then (
	       	    haderror = true;
	       	    stderr << "documentation node '" << key << "' not reachable" << endl;
	       	    )
	       )
	  )
     )
close docFile

<< "pass 3, writing html files" << endl
time scan(keys linkFollowedTable, fkey -> (
	  linkFilename fkey
	  << html HTML { 
	       HEAD TITLE {fkey, headline fkey},
	       BODY { 
		    buttonBar fkey, 
		    HR{}, 
		    documentationMemo fkey,
		    HR{}, 
		    buttonBar fkey 
		    }
	       }
	  << endl
	  << close
	  )
     )

-- create the master index
al := characters "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
i := 0
anchor := entry -> if al#?i and entry >= al#i then (
     s := select(drop(al,i), c -> entry >= c);
     i = i + #s;
     SEQ apply(s, c -> ANCHOR {c, ""})
     )

masterFileName << html HTML {
     HEAD { TITLE masterNodeName },
     BODY {
	  HEADER2 masterNodeName, PARA,
	  CENTER topNodeButton, PARA,
	  CENTER between(LITERAL "&nbsp;&nbsp;&nbsp;",apply(al, c -> HREF {"#"|c, c})), PARA,
	  MENU apply(sort keys masterIndex, (fkey,key) -> SEQ { anchor fkey, TO key }), PARA,
	  CENTER between(LITERAL "&nbsp;&nbsp;&nbsp;",apply(al, c -> HREF {"#"|c, c})), PARA,
	  CENTER topNodeButton
	  }
     } << endl << close

if haderror then (
     stderr << "exiting after having encountered some documentation errors" << endl;
     stderr << "ignoring the errors" << endl;
     -- exit 1
     )
