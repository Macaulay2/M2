--		Copyright 1994 by Daniel R. Grayson

-- make an html file for each documentation node

errorDepth 0

conceptTable = new MutableHashTable

scandb = (db,f) -> scanKeys(db,k->f(k,db#k))
scandb(openDatabase "../cache/Macaulay2.doc",
     (key,doc) -> (
	  filename := linkFilename key;
	  conceptTable#key = filename;
	  openOut filename << html SEQ { 
	       TITLE key, H1 key,
	       try evaluate doc else error ("evaluate ", doc),
	       if key != "index" then SEQ {
		    PARA,
		    "Go to ", HREF {"index.html", "main index"}, "."
		    },
	       PARA,
	       "Go to ", HREF {"concepts.html", "concepts index"}, "."
	       } << endl << close))

concepts = openOut "concepts.html";
concepts << html SEQ { TITLE "Concepts Index", H1 "Concepts Index" }
concepts << "<MENU>\n";
scan(sort keys conceptTable,
     key -> (
	  filename := conceptTable#key;
	  concepts
	  << "<LI> <A href=\"" 
	  << filename 
	  << "\">" 
	  << key 
	  << "</A>\n"))
concepts << "</MENU>\n"
concepts << html {
     PARA, "Go to ", HREF {"index.html", "main index"}, "."
     } << endl << close

run concatenate ( 
     if version#"OS" === "MS-DOS" then "copy" else "ln -s",
     " ",
     conceptTable#"Macaulay 2", 
     " index.html")
