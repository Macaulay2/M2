--		Copyright 1994 by Daniel R. Grayson

-- make an html file for each documentation node

masterIndex = new MutableHashTable
masterFileName = "master.html"
masterNodeName = "master index"
topFileName = "index.html"
topNodeName = "Macaulay 2"
topNodeAlias = "table of contents"
databaseFileName = "../cache/Macaulay2.doc"

errorDepth 0

scandb = (db,f) -> scanKeys(db,k->f(k,db#k))

process := (key,doc) -> (
     -- stderr << key << endl;
     filename := linkFilename key;
     masterIndex#key = filename;
     key = evaluate key;
     title := formatDocumentTag key;
     filename << html HTML { 
	  HEAD TITLE title,
	  BODY {
	       H2 title,
	       try evaluate doc else error ("evaluate ", doc),
	       if key =!= topNodeName then SEQ {
		    PARA{
		    	 "Go to ", HREF {topFileName, topNodeAlias}, "."
			 },
		    },
	       PARA{
	       	    "Go to ", HREF {masterFileName, masterNodeName}, "."
		    },
	       }
	  } << endl << close)

scandb(openDatabase databaseFileName, process) 

scan(linkFilenameKeys(),
     key -> if not masterIndex#?key then (
	  title := formatDocumentTag key;
	  stderr << "Documentation for '" << key << "' missing." << endl;
	  linkFilename key << html HTML { 
	       HEAD TITLE title,
	       BODY {
		    H2 title,
		    PARA{
		    	 "The text for this node has not been written yet.",
			 },
		    if key =!= "index" then SEQ {
			 PARA{
			      "Go to ", HREF {topFileName, topNodeAlias}, "."
			      },
			 },
		    PARA{
		    	 "Go to ", HREF {masterFileName, masterNodeName}, "."
			 },
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
     	  PARA{
	       "Go to ", HREF {topFileName, topNodeAlias}, "."
	       },
	  }
     } << endl << close

OS := "operating system"

run concatenate ( 
     if version#OS === "MS-DOS" then "copy" else 
     if version#OS === "Windows NT" then "copy" else 
     if version#OS === "CYGWIN32-95" then "copy" else 
     if version#OS === "CYGWIN32-NT" then "copy" else 
	 "ln -s",
     " ",
     masterIndex#(format "Macaulay 2"),
     " index.html")
