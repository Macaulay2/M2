--		Copyright 1994 by Daniel R. Grayson


linkFilenameTable := new MutableHashTable
linkFilenameCounter := 0
linkFilenameKeys = () -> keys linkFilenameTable

fourDigits := i -> (
     s := string i;
     concatenate(4-#s:"0", s)
     )

linkFilename = s -> (
     if linkFilenameTable#?s 
     then linkFilenameTable#s
     else (
	  n := fourDigits linkFilenameCounter;
	  linkFilenameTable#s = n;
	  linkFilenameCounter = linkFilenameCounter + 1;
	  n)
     ) | ".html"

html TO   := x -> concatenate (
     "<A HREF=\"", linkFilename getDocumentationTag x#0, "\">", html formatDocumentTag x#0, "</A>", 
     drop(toList x,1)
     )

-- make an html file for each documentation node

BUTTON = (s,alt) -> LITERAL concatenate("<IMG src=\"",s,"\" border=0 align=center alt=\"[", alt, "]\">")

html BODY := x -> concatenate(
     "<BODY BACKGROUND='recbg.jpg'>",
     newline,
     apply(x, html),
     newline,
     "</BODY>",
     newline
     )


masterIndex = new MutableHashTable
masterFileName = "master.html"
masterNodeName = "master index"
masterIndexButton := HREF { masterFileName, BUTTON("index.gif","index") }

topFileName = "index.html"
topNodeName = "\"Macaulay 2\""
topNodeAlias = "table of contents"
topNodeButton := HREF { topFileName, BUTTON("top.gif","top") }

databaseFileName = "../cache/Macaulay2-doc"
errorDepth 0

scandb = (db,f) -> scanKeys(db,k->f(k,db#k))

NEXT = new MutableHashTable
PREV = new MutableHashTable
UP   = new MutableHashTable

nextButton := BUTTON("next.gif","next")
prevButton := BUTTON("previous.gif","previous")
upButton := BUTTON("up.gif","up")

next = key -> if NEXT#?key then HREF { linkFilename NEXT#key, nextButton }
prev = key -> if PREV#?key then HREF { linkFilename PREV#key, prevButton }
up   = key -> if   UP#?key then HREF { linkFilename   UP#key,   upButton }

scope = method(SingleArgumentDispatch => true)
scope2 = method(SingleArgumentDispatch => true)

lastKey := null
thisKey := null

scope Sequence := scope BasicList := x -> scan(x,scope)
scope Thing := x -> null
scope MENU := x -> scan(x,scope2)
scope2 Thing := x -> null
scope2 Sequence := scope2 BasicList := x -> scan(x,scope2)
scope2 SHIELD := x -> null
scope2 TO := x -> (
     key := getDocumentationTag x#0;
     if UP#?key then (
	  stderr
	  << "key '" << key << "' already encountered" << endl
	  << "    previous menu was in '" << UP#key << "'" << endl
	  << "         this menu is in '" << thisKey << "'" << endl
	  )
     else (
	  UP#key = thisKey;
	  if lastKey =!= null then (
	       PREV#key = lastKey;
	       NEXT#lastKey = key;
	       );
	  lastKey = key;
	  )
     )
	  
preprocess = (key,doc) -> (
     thisKey = key;
     lastKey = null;
     scope try value doc else error ("value ", doc);
     )

docFile := openDatabase databaseFileName

scandb(docFile, preprocess) 

buttonBar := (key) -> CENTER {
     if key =!= topNodeName then topNodeButton,
     masterIndexButton,
     prev key, up key, next key,
     }

process := (key,doc) -> (
     -- stderr << key << endl;
     filename := linkFilename key;
     masterIndex#key = filename;
     vkey := value key;
     title := formatDocumentTag key;
     filename << html HTML { 
	  HEAD TITLE title,
	  BODY {
	       buttonBar key,
	       HR,
	       H2 title,
	       try value doc else error ("value ", doc),
	       HR,
	       buttonBar key
	       }
	  } << endl << close)
scandb(docFile, process) 

close docFile

missing := false;

scan(linkFilenameKeys(),
     key -> if not masterIndex#?key then (
	  title := formatDocumentTag key;
	  missing = true;
	  stderr << "Documentation for '" << key << "' missing." << endl;
	  linkFilename key << html HTML { 
	       HEAD TITLE title,
	       BODY {
		    H2 title,
		    PARA{
		    	 "The text for this node has not been written yet.",
			 },
		    if key =!= "index" then topNodeButton,
		    masterIndexButton
		    }
	       } << endl << close;
	  ))

masterFileName << html HTML {
     HEAD {
	  TITLE masterNodeName
	  },
     BODY {
	  H2 masterNodeName,
	  MENU apply(sort pairs masterIndex, (key, fname) -> HREF {fname, formatDocumentTag key}),
	  topNodeButton
	  }
     } << endl << close

run concatenate ( 
     if version#"operating system" === "Windows-95-98-NT" then "copy" else "ln -f",
     " ",
     masterIndex#(format "Macaulay 2"),
     " index.html")

if missing then error "missing some nodes"


-- 
-- document { quote linkFilename,
--      TT "linkFilename s", " -- convert a string ", TT "s", " into a string 
--      which can be used as a file name to contain HTML about the topic 
--      referred to by ", TT "s", ".",
--      PARA,
--      "The value returned is a sequence number, and hence may not be
--      the same in subsequent sessions.  Hence this is mainly useful
--      for creating html for all the online documentation, and the general 
--      user will not find it useful.",
--      PARA,
--      SEEALSO "linkFilenameKeys"
--      }
-- 
-- document { quote linkFilenameKeys,
--      TT "linkFilenameKeys()", " -- returns a list of the strings which
--      have been given to ", TO "linkFilename", ".",
--      PARA,
--      "This function is intended mainly for internal use in generating
--      the documentation for Macaulay 2."
--      }
