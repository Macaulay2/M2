--		Copyright 1994 by Daniel R. Grayson

-- make an html file for each documentation node

BUTTON = (s,alt) -> LITERAL concatenate("<IMG src=\"",s,"\" border=0 align=center alt=\"", alt, "\">")

masterIndex = new MutableHashTable
masterFileName = "master.html"
masterNodeName = "master index"
masterIndexButton := HREF { masterFileName, BUTTON("redsq_info.gif","index") }

topFileName = "index.html"
topNodeName = "Macaulay 2"
topNodeAlias = "table of contents"
topNodeButton := HREF { topFileName, BUTTON("star_home.gif","top") }

databaseFileName = "../cache/Macaulay2.doc"
errorDepth 0

scandb = (db,f) -> scanKeys(db,k->f(k,db#k))

NEXT = new MutableHashTable
PREV = new MutableHashTable
UP   = new MutableHashTable

nextButton := BUTTON("redsq_next.gif","next")
prevButton := BUTTON("redsq_back.gif","previous")
upButton := BUTTON("up.gif","up")

next = key -> if NEXT#?key then HREF { linkFilename NEXT#key, nextButton } else nextButton
prev = key -> if PREV#?key then HREF { linkFilename PREV#key, prevButton } else prevButton
up   = key -> if   UP#?key then HREF { linkFilename   UP#key,   upButton } else   upButton

scope = method()
scope2 = method()

this := null
last := null

scope Sequence := scope BasicList := x -> scan(x,scope)
scope Thing := x -> null
scope MENU := x -> scan(x,scope2)
scope2 Thing := x -> null
scope2 Sequence := scope BasicList := x -> scan(x,scope2)
scope2 SHIELD := x -> null
scope2 TO := x -> (
     key := getDocumentationTag x#0;
     if last =!= null then (
	  PREV#key = last;
	  NEXT#last = key;
	  );
     UP#key = this;
     last = key;
     )
	  
preprocess = (key,doc) -> (
     doc = try value doc else error ("value ", doc);
     this = key;
     last = null;
     scope doc;
     )
scandb(openDatabase databaseFileName, preprocess) 

error ""

process := (key,doc) -> (
     -- stderr << key << endl;
     filename := linkFilename key;
     masterIndex#key = filename;
     vkey := value key;
     title := formatDocumentTag vkey;
     filename << html HTML { 
	  HEAD TITLE title,
	  BODY {
	       H2 title,
	       try value doc else error ("value ", doc),
	       HR,
	       CENTER {
		    if vkey =!= topNodeName then topNodeButton,
		    masterIndexButton,
		    prev key, up key, next key,
		    }
	       }
	  } << endl << close)
scandb(openDatabase databaseFileName, process) 

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
	  MENU apply(sort pairs masterIndex, (key, fname) -> HREF {fname, formatDocumentTag value key}),
	  topNodeButton
	  }
     } << endl << close

OS := "operating system"

run concatenate ( 
     if version#OS === "MS-DOS" then "copy" else 
     if version#OS === "Windows NT" then "copy" else 
     if version#OS === "CYGWIN32-95" then "copy" else 
     if version#OS === "CYGWIN32-NT" then "copy" else 
	 "ln -f",
     " ",
     masterIndex#(format "Macaulay 2"),
     " index.html")

if missing then error "missing some nodes"

