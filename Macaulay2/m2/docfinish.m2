--		Copyright 2002 by Daniel R. Grayson


topNodeName = "Macaulay 2"

-----------------------------------------------------------------------------
-- produce html page
-----------------------------------------------------------------------------
writeHtmlPage := nodeName -> (
     nodeBaseFilename | ".html" << html HTML { 
	  HEAD TITLE {nodeName, headline nodeName},
	  BODY { 
	       -- buttonBar key, 
	       HR{}, 
	       documentation nodeName,
	       HR{}, 
	       -- buttonBar key 
	       }
	  }
     << close)

-----------------------------------------------------------------------------
-- find items to document
-----------------------------------------------------------------------------

prefix := first documentationPath

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
	  cacheFileName(prefix,fkey);
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

<< "pass 1, descending through documentation tree" << endl
time follow topNodeName

scan(keys linkFollowedTable, print)
