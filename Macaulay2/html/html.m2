--		Copyright 1994 by Daniel R. Grayson

-- make an html file for each documentation node

masterIndex = new MutableHashTable
masterFileName = "master.html"
masterNodeName = "master index"
masterIndexButton := HREF { 
     masterFileName,
     LITERAL "<IMG src=\"redsq_info.gif\" border=0 align=center alt=index>" 
     }

topFileName = "index.html"
topNodeName = "Macaulay 2"
topNodeAlias = "table of contents"
topNodeButton := HREF { 
     topFileName,
     LITERAL "<IMG src=\"star_home.gif\" border=0 align=center alt=top>"
     }

databaseFileName = "../cache/Macaulay2.doc"
errorDepth 0

scandb = (db,f) -> scanKeys(db,k->f(k,db#k))

next := key -> LITERAL "<IMG src=\"redsq_next.gif\" border=0 align=center alt=next>"
prev := key -> LITERAL "<IMG src=\"redsq_back.gif\" border=0 align=center alt=previous>"
up   := key -> LITERAL "<IMG src=\"up.gif\" border=0 align=center alt=up>"

process := (key,doc) -> (
     -- stderr << key << endl;
     filename := linkFilename key;
     masterIndex#key = filename;
     key = value key;
     title := formatDocumentTag key;
     filename << html HTML { 
	  HEAD TITLE title,
	  BODY {
	       H2 title,
	       try value doc else error ("value ", doc),
	       HR{},
	       if key =!= topNodeName then topNodeButton,
	       masterIndexButton,
	       prev key, up key, next key,
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

if missing then exit 1
