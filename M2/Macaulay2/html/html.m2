--		Copyright 1993-1999 by Daniel R. Grayson

databaseFileName = "../cache/Macaulay2-doc"
errorDepth 0

BUTTON = (s,alt) -> LITERAL concatenate("<IMG src=\"",s,"\" border=0 align=center alt=\"[", alt, "]\">")

topFileName = "index.html"
topNodeName = "Macaulay 2"
topNodeButton = HREF { topFileName, BUTTON("top.gif","top") }

masterIndex = new MutableHashTable
masterFileName = "master.html"
masterNodeName = "master index"
masterIndexButton = HREF { masterFileName, BUTTON("index.gif","index") }

linkFilenameTable = new MutableHashTable from { topNodeName => topFileName }
linkFilenameCounter = 0

missing = false
missed = memoize(
     key -> (
	  missing = true;
	  stderr << "Documentation for '" << toExternalString key << "' missing, needed by '" << thisKey << "'." << endl;
	  )
     )

fourDigits = i -> (
     s := toString i;
     concatenate(4-#s:"0", s)
     )

linkFilename = s -> (
     if linkFilenameTable#?s 
     then linkFilenameTable#s
     else linkFilenameTable#s = (
	  linkFilenameCounter = linkFilenameCounter + 1;
	  fourDigits linkFilenameCounter | ".html"))

html TO   := x -> (
     key := formatDocumentTag x#0;
     if linkFilenameTable#?key
     then concatenate("<A HREF=\"", linkFilenameTable#key, "\">", html key, "</A>", drop(toList x,1))
     else (
	  missed key;
	  concatenate(html key, drop(toList x,1))))

html BODY := x -> concatenate(
     "<BODY BACKGROUND='recbg.jpg'>", newline,
     apply(x, html), newline,
     "</BODY>", newline
     )

NEXT = new MutableHashTable
PREV = new MutableHashTable
UP   = new MutableHashTable

nextButton = BUTTON("next.gif","next")
prevButton = BUTTON("previous.gif","previous")
upButton = BUTTON("up.gif","up")

next = key -> if NEXT#?key then HREF { linkFilename NEXT#key, nextButton }
prev = key -> if PREV#?key then HREF { linkFilename PREV#key, prevButton }
up   = key -> if   UP#?key then HREF { linkFilename   UP#key,   upButton }

scope := method(SingleArgumentDispatch => true)
scope2 := method(SingleArgumentDispatch => true)

lastKey = null
thisKey = null

scope Thing := x -> null
scope Sequence := scope BasicList := x -> scan(x,scope)
scope MENU := x -> scan(x,scope2)
scope TO := scope TOH := x -> (
     key := formatDocumentTag x#0;
     linkFilename key;
     )

scope2 Thing := x -> null
scope2 Sequence := scope2 BasicList := x -> scan(x,scope2)
scope2 SHIELD := x -> scan(x,scope)
scope2 TO := scope2 TOH := x -> (
     key := formatDocumentTag x#0;
     linkFilename key;
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
	  )
     )

buttonBar = (key) -> CENTER {
     next key,
     prev key, 
     up key,
     if key =!= topNodeName then topNodeButton,
     masterIndexButton,
     }
	  
-- get all documentation entries
allDoc = new MutableHashTable
docFile = openDatabase databaseFileName
<< "loading documentation" << endl
time scanKeys(docFile,
     key -> (
	  doc := docFile#key;
	  if not match(doc,"goto *") then (
     	       fkey := formatDocumentTag value key;
	       nkey := toExternalString fkey;
	       if not allDoc#?fkey then(
		    doc = (
			 try value doc
			 else error ("error evaluating documentation string for ", toExternalString key)
			 );
		    linkFilename fkey;
		    allDoc#fkey = doc))))
allDocPairs = pairs allDoc
close docFile

-- create one web page for each documentation entry
<< "pass 1" << endl
time scan(allDocPairs, (key,doc) -> (
     	  thisKey = key;
     	  lastKey = null;
     	  scope documentation key)) 
<< "pass 2" << endl
time scan(pairs linkFilenameTable, (key,filename) -> (
     	  thisKey = key;
     	  masterIndex#key = filename;
     	  filename << html HTML { 
	       HEAD TITLE key,
	       BODY { buttonBar key, HR{}, documentation key, HR{}, buttonBar key }
	       } << endl << close)) 

-- create the master index
masterFileName << html HTML {
     HEAD { TITLE masterNodeName },
     BODY {
	  H2 masterNodeName,
	  CENTER topNodeButton,
	  MENU apply(sort pairs masterIndex, (key, fname) -> HREF {fname, formatDocumentTag key}),
	  CENTER topNodeButton
	  }
     } << endl << close

if missing then error "missing some nodes"
