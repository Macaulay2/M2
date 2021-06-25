---------------------------------------------------------------------------
-- PURPOSE : Visualize package for Macaulay2 provides the ability to 
-- visualize various algebraic objects in javascript using a 
-- modern browser.
--
-- Copyright (C) 2017 Brett Barwick, Thomas Enkosky, Branden Stone and Jim Vallandingham
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License version 2
-- as published by the Free Software Foundation.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--------------------------------------------------------------------------


newPackage(
	"Visualize",
    	Version => "1.5", 
    	Date => "May 24, 2019",
    	Authors => {       
     	     {Name => "Brett Barwick", Email => "bbarwick@uscupstate.edu", HomePage => "http://faculty.uscupstate.edu/bbarwick/"},	     
	     {Name => "Thomas Enkosky", Email => "tomenk@bu.edu", HomePage => "http://math.bu.edu/people/tomenk/"},	     
	     {Name => "Branden Stone", Email => "bstone@adelphi.edu", HomePage => "http://math.adelpi.edu/~bstone/"},
	     {Name => "Jim Vallandingham", Email => "vlandham@gmail.com", HomePage => "http://vallandingham.me/"}
-- Contributing Author	     {Name => "Ata Firat Pir", Email => "atafirat@math.tamu.edu"},	     
-- Contributing Author	     {Name => "Elliot Korte", Email => "ek2872@bard.edu"},	     
-- Contributing Author	     {Name => "Will Smith", Email => "smithw12321@gmail.com"},		
-- Contributing Author	     {Name => "Julio Urenda", Email => "jcurenda@nmsu.edu"},	     
	     },
    	Headline => "interactive visualization and manipulation of combinatorial objects in a browser",
	Keywords => {"Graphics"},
	PackageExports => {"Graphs", "Posets", "SimplicialComplexes"},
	AuxiliaryFiles => true,
	Configuration => {"DefaultPath" => null } 
    	)

export {
    
    -- Options
     "VisPath",
     "VisTemplate",
     "Warning",
     "FixExtremeElements",
    
    -- Methods
     "visualize",
     
    -- Helpers 
--     "toArray", -- Don't need to export?
--     "getCurrPath", -- Don't need to export?
--     "copyTemplate",-- Don't need to export?
--     "replaceInFile",-- Don't need to export?
--     "heightFunction",
--     "relHeightFunction",
     
    -- Server
     "openPort",
     "closePort"
}


------------------------------------------------------------
-- Global Variables
------------------------------------------------------------

defaultPath = (options Visualize).Configuration#"DefaultPath"
basePath = currentFileDirectory -- created this because copyJS would not handle 
    	    	    	    	-- currentFileDirectory for some reason.
commonVisOpts = {VisPath => defaultPath, Warning => true, Verbose => false} -- list of common options for visualize method

-- (options Visualize).Configuration

portTest = false -- Used to test if ports are open or closed.
inOutPort = null -- Actual file the listener is opened to.
inOutPortNum = null -- The port number that is being opened. This is passed to the browser.


------------------------------------------------------------
-- METHODS
------------------------------------------------------------

-- Input: None.
-- Output: String containing current path.

getCurrPath = method()
installMethod(getCurrPath, () -> (local currPath; currPath = get "!pwd"; substring(currPath,0,(length currPath)-1)|"/"))

--input: A list of lists
--output: an array of arrays

toArray = method() 
toArray(List) := L -> (
     return new Array from apply(L, i -> new Array from i);
     )

--replaceInFile
--	replaces a given pattern by a given pattern in a file
--	input: string containing the pattern
--	       string containing the replacement
--	       string containing the file name, 
replaceInFile = method()
replaceInFile(String, String, String) := (patt, repl, fileName) -> (
		local currFile; 
		local currStr; 
		
		currFile = openIn fileName; 
		currStr = get currFile;
	      
		
		currStr = replace(patt, repl, currStr);

		currFile = openOut fileName; 

		currFile << currStr << close;
		
		return fileName;
)	

-- input: path to an html file
-- output: a copy of the input file in a temporary folder
--
copyTemplate = method()
copyTemplate String := src -> (
    local fileName; local dirPath;
    
    fileName = (toString currentTime() )|".html";
    
    dirPath = temporaryFileName();
    makeDirectory dirPath;
    dirPath = concatenate(dirPath,"/",fileName);
    
    copyFile( src, dirPath);
    
    return dirPath;
)

-- input: A source path to an html file and a destination directory
-- output: a copy of the source file in the destination directory
--
copyTemplate(String,String) := (src,dst) -> (
    local fileName; local dirPath;
    
    fileName = (toString currentTime() )|".html";
    
--    dirPath = temporaryFileName();
--    makeDirectory dirPath;
    dirPath = concatenate(dst,fileName);

-- test to see if users directory exists    
    if (fileExists dst) 
    then (
	copyFile( src, dirPath);
	return dirPath;
	)
    else error "Path does not exist. Please check the path and try again.";

)

-- input: three strings
-- output:
searchReplace = method() --Options => {VisPath => currentDirectory()}) -- do we use VisPath?
searchReplace(String,String,String) := (oldString,newString,visSrc) -> (
    local visFilePathTemp;
    
    visFilePathTemp = temporaryFileName();
    copyFile(visSrc,visFilePathTemp);
    openOut visSrc << 
    	replace(oldString, newString , get visFilePathTemp) << 
	close;
	
    return visSrc;
    )

-- Input: Poset
-- Output: A list where the ith element is the height of the ith element
-- of P.GroundSet as it appears in a filtration of the poset.
heightFunction = method()
heightFunction(Poset) := P -> (
    local F; local G; local tempList;
    F = filtration P;
    G = P.GroundSet;
    tempList = new MutableList from apply(#G,i -> null);
    -- The next line creates a list in which the ith entry is the
    -- height of the ith element in P.GroundSet.
    scan(#F, i -> scan(F#i, j -> (tempList#(position(G, k -> k == j)) = i)));
    return toList tempList;
)

-- Input: Poset
-- Output: A list where the ith element is the 'relative height' of the ith
-- element of P.GroundSet, which helps to evenly distribute the elements
-- of the poset evenly based on how they appear within the maximal chains
-- in the poset.
relHeightFunction = method()
relHeightFunction(Poset) := P -> (
    local nodes; local maxChains;
    local heightList; local maxChainList; local chainLengthList;
    local relHeightList; local totalHeight;
    
    nodes = P_*;
    maxChains = maximalChains P;
    heightList = heightFunction P;
    maxChainList = apply(nodes, j -> delete(null,apply(maxChains, L -> (if any(L, i -> i == j) == true then L else null))));
    chainLengthList = apply(maxChainList, i -> apply(i, j -> #j));
    relHeightList = apply(chainLengthList, i -> max i - 1);
    totalHeight = lcm relHeightList;
    return apply(#nodes, i -> (totalHeight / relHeightList#i) * heightList#i);    
)

--input: A monomial ideal of a polynomial ring in 2 or 3 variables.
--output: The newton polytope of the of the ideal.
--
visualize = method(Options => true)

--{VisPath => defaultPath, Warning => true, VisTemplate => basePath |"Visualize/templates/visIdeal/visIdeal"} >> opts -> J -> (
visualize(Ideal) := commonVisOpts|{VisTemplate => basePath |"Visualize/templates/visIdeal/visIdeal"} >> opts -> J -> ( 
    local R; local arrayList; local arrayString; local numVar; local visTemp;
    local varList; local newArrayList; local newArrayString;    
        
    R = ring J;
    numVar = rank source vars R;
    varList = flatten entries vars R;
        
    if ((numVar != 2) and (numVar != 3)) then (error "Ring needs to have either 2 or 3 variables.");
    
    if numVar == 2 
    then (
	if opts.VisPath =!= null 
	then (
	    	visTemp = copyTemplate(opts.VisTemplate|"2D.html",opts.VisPath);
	    	copyJS(opts.VisPath, Warning => opts.Warning);	    
	    )
	else (
	    	visTemp = copyTemplate(opts.VisTemplate|"2D.html");
	    	copyJS(replace(baseFilename visTemp, "", visTemp), Warning => opts.Warning);	    
	    );
	
	-- changed gens to leadTerm so if there's a non monomial ideal
	-- it will return the initial ideal
	arrayList = apply( flatten entries leadTerm J, m -> flatten exponents m);	
	arrayList = toArray arrayList;
	arrayString = toString arrayList;
	
	searchReplace("visArray",arrayString, visTemp);
--	searchReplace("XXX",toString(varList_0), visTemp);
--	searchReplace("YYY",toString(varList_1), visTemp);
--	searchReplace("ZZZ",toString(varList_2), visTemp)
    )
    else (
	
	if opts.VisPath =!= null 
	then (
	    	visTemp = copyTemplate(opts.VisTemplate|"3D.html",opts.VisPath);
	    	copyJS(opts.VisPath, Warning => opts.Warning);	    
	    )
	else (
	    	visTemp = copyTemplate(opts.VisTemplate|"3D.html");
	    	copyJS(replace(baseFilename visTemp, "", visTemp), Warning => opts.Warning);	    
	    );
	    
    	arrayList = apply(flatten entries basis(0,infinity, R/J), m -> flatten exponents m );
    	arrayList = toArray arrayList;
	newArrayList = apply(flatten entries leadTerm J, m -> flatten exponents m );
    	newArrayList = toArray newArrayList;
    	arrayString = toString arrayList;
     	newArrayString = toString newArrayList;
	
	searchReplace("visArray",arrayString, visTemp);
	searchReplace("newVisArray",newArrayString, visTemp);
	searchReplace("XXX",toString(varList_0), visTemp);
	searchReplace("YYY",toString(varList_1), visTemp);
	searchReplace("ZZZ",toString(varList_2), visTemp)
    );
    
    show new URL from { "file://"|visTemp };
    
    return visTemp;--opts.VisPath|A_1;
    )

--input: A graph
--output: A visualization of the graph in the browser
--{VisPath => defaultPath, VisTemplate => basePath | "Visualize/templates/visGraph/visGraph-template.html", Warning => true, Verbose => false} >> opts -> G -> (
visualize(Graph) := commonVisOpts|{VisTemplate => basePath | "Visualize/templates/visGraph/visGraph-template.html"} >> opts -> G -> (
    local A; local arrayString; local vertexString; local visTemp;
    local keyPosition; local vertexSet; local browserOutput;
    
    openPortTest();
        
    A = adjacencyMatrix G;
    arrayString = toString toArray entries A; -- Turn the adjacency matrix into a nested array (as a string) to copy to the template html file.
    
    -- Add this back in when we figure out how to deal with the old
    -- Graphs package not knowing what G.vertexSet means.

-- !!!! Need to deal with version numbers here    
--  if value((options Graphs).Version) == 0.3.1 then (
    if .1 == 1 then (	
	 vertexString = toString new Array from apply(keys(G#graph), i -> "\""|toString(i)|"\""); -- Create a string containing an ordered list of the vertices in the older Graphs package.
    ) else (
    
    	 -- This is a workaround for finding and referring to the key vertexSet in the hash table for G.
         -- Would be better to be able to refer to G.vertexSet, but the package
	 -- seems not to load if we try this.
	 keyPosition = position(keys G, i -> toString i == "vertexSet");
	 vertexString = toString new Array from apply((values G)#keyPosition, i -> "\""|toString(i)|"\""); -- Create a string containing an ordered list of the vertices in the newer Graphs package
	 
	 --vertexSet = symbol vertexSet;
	 --vertexString = toString new Array from apply(G.vertexSet, i -> "\""|toString(i)|"\""); -- Create a string containing an ordered list of the vertices in the newer Graphs package.
	 -- vertexString = toString new Array from apply((values G)#0, i -> "\""|toString(i)|"\""); -- Create a string containing an ordered list of the vertices in the newer Graphs package.
    );

    if opts.VisPath =!= null 
    then (
	visTemp = copyTemplate(opts.VisTemplate, opts.VisPath); -- Copy the visGraph template to a temporary directory.
    	copyJS(opts.VisPath, Warning => opts.Warning); -- Copy the javascript libraries to the temp folder.
      )
    else (
	visTemp = copyTemplate(opts.VisTemplate); -- Copy the visGraph template to a temporary directory.
    	copyJS(replace(baseFilename visTemp, "", visTemp), Warning => opts.Warning); -- Copy the javascript libraries to the temp folder.
      );
    
    searchReplace("visArray",arrayString, visTemp); -- Replace visArray in the visGraph html file by the adjacency matrix.
    searchReplace("visLabels",vertexString, visTemp); -- Replace visLabels in the visGraph html file by the ordered list of vertices.
    searchReplace("visPort",inOutPortNum, visTemp); -- Replace visPort in the visGraph html file by the user port number.
    
    show new URL from { "file://"|visTemp };
    
    browserOutput = openServer(inOutPort, Verbose => opts.Verbose);
        
    return browserOutput;
)

-- Input: A digraph
-- Output: A visualization of the digraph in the browser
--{Verbose => false, VisPath => defaultPath, VisTemplate => basePath |"Visualize/templates/visDigraph/visDigraph-template.html", Warning => true} >> opts -> G -> (
visualize(Digraph) := commonVisOpts|{VisTemplate => basePath |"Visualize/templates/visDigraph/visDigraph-template.html"} >> opts -> G -> (
    local A; local arrayString; local vertexString; local visTemp;
    local keyPosition; local vertexSet; local browserOutput;

    openPortTest();    
    
    A = adjacencyMatrix G;
    arrayString = toString toArray entries A; -- Turn the adjacency matrix into a nested array (as a string) to copy to the template html file.
    
    -- Add this back in when we figure out how to deal with the old
    -- Graphs package not knowing what G.vertexSet means.
    
    --if value((options Graphs).Version) == 0.1 then (
    if 1 == .1 then (
	 vertexString = toString new Array from apply(keys(G#graph), i -> "\""|toString(i)|"\""); -- Create a string containing an ordered list of the vertices in the older Graphs package.
    ) else (
    
    	 -- This is a workaround for finding and referring to the key vertexSet in the hash table for G.
         -- Would be better to be able to refer to G.vertexSet, but the package
	 -- seems not to load if we try this.
	 keyPosition = position(keys G, i -> toString i == "vertexSet");
	 vertexString = toString new Array from apply((values G)#keyPosition, i -> "\""|toString(i)|"\""); -- Create a string containing an ordered list of the vertices in the newer Graphs package
	 
	 --vertexSet = symbol vertexSet;
	 --vertexString = toString new Array from apply(G.vertexSet, i -> "\""|toString(i)|"\""); -- Create a string containing an ordered list of the vertices in the newer Graphs package.
	 -- vertexString = toString new Array from apply((values G)#0, i -> "\""|toString(i)|"\""); -- Create a string containing an ordered list of the vertices in the newer Graphs package.
    );
    
--    visTemp = copyTemplate(currentDirectory()|"Visualize/templates/visDigraph/visDigraph-template.html"); -- Copy the visDigraph template to a temporary directory.
--    copyJS(replace(baseFilename visTemp, "", visTemp)); -- Copy the javascript libraries to the temp folder.    

    if opts.VisPath =!= null 
    then (
	visTemp = copyTemplate(opts.VisTemplate, opts.VisPath); -- Copy the visDigraph template to a temporary directory.
    	copyJS(opts.VisPath, Warning => opts.Warning); -- Copy the javascript libraries to the temp folder.
      )
    else (
	visTemp = copyTemplate(opts.VisTemplate); -- Copy the visDigraph template to a temporary directory.
    	copyJS(replace(baseFilename visTemp, "", visTemp), Warning => opts.Warning); -- Copy the javascript libraries to the temp folder.
      );

    searchReplace("visArray",arrayString, visTemp); -- Replace visArray in the visDigraph html file by the adjacency matrix.
    searchReplace("visLabels",vertexString, visTemp); -- Replace visLabels in the visDigraph html file by the ordered list of vertices.
    searchReplace("visPort",inOutPortNum, visTemp); -- Replace visPort in the visGraph html file by the user port number.

    show new URL from { "file://"|visTemp };
    
    browserOutput = openServer(inOutPort, Verbose => opts.Verbose);
        
    return browserOutput;
)


-- Input: A poset
-- Output: A visualization of the poset in the browser
--{Verbose=>false,FixExtremeElements => false, VisPath => defaultPath, VisTemplate => basePath | "Visualize/templates/visPoset/visPoset-template.html", Warning => true} >> opts -> P -> (
visualize(Poset) := commonVisOpts|{FixExtremeElements => false, VisTemplate => basePath | "Visualize/templates/visPoset/visPoset-template.html"} >> opts -> P -> (

    local labelList; local groupList; local relList; local visTemp;
    local numNodes; local labelString; local nodeString; local relationString;
    local relMatrixString; local fixExtremeEltsString; local browserOutput;

    openPortTest();
        
    labelList = apply(P_*, i -> "'"|(toString i)|"'");
    
    labelString = toString new Array from labelList;
    relMatrixString = toString toArray entries P.RelationMatrix;
    fixExtremeEltsString = toString opts.FixExtremeElements;
  
    if opts.VisPath =!= null 
    then (
	visTemp = copyTemplate(opts.VisTemplate, opts.VisPath); -- Copy the visPoset template to a temporary directory.
    	copyJS(opts.VisPath, Warning => opts.Warning); -- Copy the javascript libraries to the temp folder.
      )
    else (
	visTemp = copyTemplate(opts.VisTemplate); -- Copy the visPoset template to a temporary directory.
    	copyJS(replace(baseFilename visTemp, "", visTemp), Warning => opts.Warning); -- Copy the javascript libraries to the temp folder.
    );
    
    searchReplace("visLabels",labelString, visTemp); -- Replace visLabels in the visPoset html file by the labels of the nodes in the same order as the ground set of P.
    searchReplace("visRelMatrix",relMatrixString, visTemp); -- Replace visRelMatrix in the visPoset html file by the relation matrix of the poset.
    searchReplace("visPort",inOutPortNum, visTemp); -- Replace visPort in the visGraph html file by the user port number.
    searchReplace("visExtremeElts",fixExtremeEltsString, visTemp); -- Replace visExtremeElts in the visPoset html file by the option passed by the user of whether to fix the extremal elements at the minimum or maximum levels.
    
    show new URL from { "file://"|visTemp };
 
    browserOutput = openServer(inOutPort, Verbose => opts.Verbose);
    
    return browserOutput; 
)

-- Input: A simplicial complex
-- Output: A visualization of the simplicial complex in the browser
--{Verbose => false, VisPath => defaultPath, VisTemplate => basePath | "Visualize/templates/visSimplicialComplex/visSimplicialComplex2d-template.html", Warning => true} >> opts -> D -> (
visualize(SimplicialComplex) := commonVisOpts|{VisTemplate => basePath | "Visualize/templates/visSimplicialComplex/visSimplicialComplex2d-template.html"} >> opts -> D -> (
    local vertexSet; local edgeSet; local face2Set; local face3Set; local visTemp;
    local vertexList; local edgeList; local face2List; local face3List;
    local vertexString; local edgeString; local face2String; local face3String;
    local visTemplate; local browserOutput;

    openPortTest();
        
    vertexSet = flatten entries faces(0,D);
    edgeSet = flatten entries faces(1,D);
    face2Set = flatten entries faces(2,D);
    vertexList = apply(vertexSet, v -> apply(new List from factor v, i -> i#0));
    edgeList = apply(edgeSet, e -> apply(new List from factor e, i -> i#0));
    face2List = apply(face2Set, f -> apply(new List from factor f, i -> i#0));

    vertexString = toString new Array from apply(vertexSet, i -> "'"|toString(i)|"'");
    edgeString = toString new Array from apply(#edgeList, i -> new Array from {position(vertexSet, j -> j === edgeList#i#1),position(vertexSet, j -> j === edgeList#i#0)});
    face2String = toString new Array from apply(#face2List, i -> new Array from {position(vertexSet, j -> j == face2List#i#2),position(vertexSet, j -> j == face2List#i#1),position(vertexSet, j -> j == face2List#i#0)});
  
    if dim D>2 then (
	error "3-dimensional simplicial complexes not implemented yet.";
 	visTemplate = basePath | "Visualize/templates/visSimplicialComplex/visSimplicialComplex3d-template.html"
    )
    else (
	visTemplate = basePath | "Visualize/templates/visSimplicialComplex/visSimplicialComplex2d-template.html"
    );
   
    if opts.VisPath =!= null 
    then (
	visTemp = copyTemplate(visTemplate, opts.VisPath); -- Copy the visSimplicialComplex template to a temporary directory.
    	copyJS(opts.VisPath, Warning => opts.Warning); -- Copy the javascript libraries to the temp folder.
      )
    else (
	visTemp = copyTemplate(visTemplate); -- Copy the visSimplicialComplex template to a temporary directory.
    	copyJS(replace(baseFilename visTemp, "", visTemp), Warning => opts.Warning); -- Copy the javascript libraries to the temp folder.
    );
    
    searchReplace("visNodes",vertexString, visTemp); -- Replace visNodes in the visSimplicialComplex html file by the ordered list of vertices.
    searchReplace("visEdges",edgeString, visTemp); -- Replace visEdges in the visSimplicialComplex html file by the list of edges.
    searchReplace("vis2Faces",face2String, visTemp); -- Replace vis2Faces in the visSimplicialComplex html file by the list of faces. 
    searchReplace("visPort",inOutPortNum, visTemp); -- Replace visPort in the visGraph html file by the user port number.
        
    if dim D>2 then (
	error "3-dimensional simplicial complexes not implemented yet.";
	face3Set = flatten entries faces(3,D);
	face3List = apply(face3Set, f -> apply(new List from factor f, i -> i#0));
       	face3String = toString new Array from apply(#face3List, i -> {"\"v1\": "|toString(position(vertexSet, j -> j == face3List#i#3))|",\"v2\": "|toString(position(vertexSet, j -> j == face3List#i#2))|",\"v3\": "|toString(position(vertexSet, j -> j == face3List#i#1))|",\"v4\": "|toString(position(vertexSet, j -> j == face3List#i#0))});
	searchReplace("vis3Faces",face3String, visTemp); -- Replace vis3Faces in the visSimplicialComplex html file by the list of faces. 
    );
    
    show new URL from { "file://"|visTemp };
 
    browserOutput = openServer(inOutPort, Verbose => opts.Verbose);
    
    return browserOutput; 
)

-*
--input: A parameterized surface in RR^3
--output: The surface in the browser
--
visualize(List) := {VisPath => defaultPath, VisTemplate => basePath | "Visualize/templates/visSurface/Graphulus-Surface.html", Warning => true} >> opts -> P -> (
    local visTemp; local stringList;
        
    if opts.VisPath =!= null 
    then (
	visTemp = copyTemplate(opts.VisTemplate, opts.VisPath); -- Copy the visSimplicialComplex template to a temporary directory.
    	copyJS(opts.VisPath, Warning => opts.Warning); -- Copy the javascript libraries to the temp folder.
      )
    else (
	visTemp = copyTemplate(opts.VisTemplate); -- Copy the visSimplicialComplex template to a temporary directory.
    	copyJS(replace(baseFilename visTemp, "", visTemp), Warning => opts.Warning); -- Copy the javascript libraries to the temp folder.
    );
    
    stringList = apply(P, i -> "\""|toString i|"\"");

    searchReplace("visP1", stringList#0, visTemp); -- Replace visNodes in the visSimplicialComplex html file by the ordered list of vertices.
    searchReplace("visP2", stringList#1, visTemp); -- Replace visEdges in the visSimplicialComplex html file by the list of edges.
    searchReplace("visP3", stringList#2, visTemp); -- Replace vis2Faces in the visSimplicialComplex html file by the list of faces. 
    
    show new URL from { "file://"|visTemp };
    
    return visTemp;
)
*-

-- Input: A string of a path to a directory
-- Output: Copies the needed files and libraries to path
--
-- Caveat: Checks to see if files exist. If they do exist, the user
--        must give permission to continue. Continuing will overwrite
--        current files and cannot be undone.
copyJS = method(Options => {Warning => true})
copyJS(String) := opts -> dst -> (
    if not match("/$", dst) then dst = dst | "/";
    if not fileExists dst then makeDirectory dst;

    dirs := {"js", "css", "fonts", "images"};
    existingDirs := select(dirs, dir -> fileExists concatenate(dst, dir));

    if #existingDirs > 0 and opts.Warning == true then (
	print concatenate(
	    " -- Note: You can suppress this message with the 'Warning => false' option.\n",
	    " -- The following folders on the path ",dst," have some files that will be overwritten: ",
	    demark(", ", existingDirs), newline,
	    " -- This action cannot be undone.", newline);
	ans := read " Would you like to continue? (y or n):  ";
	while (ans != "y" and ans != "n") do (
	    ans = read " Would you like to continue? (y or n):  ";
	    );
	if ans == "n" then (
	    error "Process was aborted.";
	    );
	);

    for dir in dirs do (
	makeDirectory(dst | dir);
	for file in readDirectory(basePath | "Visualize/" | dir) do (
	    if not member(file, {".", ".."}) then (
		copyFile(realpath(basePath | "Visualize/" | dir | "/" | file),
		    dst | dir | "/" | file)))
    );

    return "Created directories at "|dst;
)


-- The server workflow is as follows.
-- 0. Load Visualize.m2
-- 1. User opens port :: openPort("8000")
--                    :: If any port is open an error occurs.
--                    :: Sometimes the error is thrown when no port
--                    :: is open. This usually occurs right after a
--                    :: port has been closed. It takes a bit of time
--                    :: for M2 to realize no port is open. 
--                    :: Maybe this is an issue with the garbage collector?
-- 2. Define graph or other object :: G = graph(....)
-- 3. Run visualize :: H = visualize G
--                  :: This will open the website and start
--                  :: communication with the server. 
--                  :: When the user ends session, output is 
--                  :: sent back to M2 and assigned to H.
-- 4. End session to export info to browser;
-- 5. Keep working and visualizing objects;
-- 6. When finished, user closes port :: closePort() (or restart M2).


-- Input: String, a port number the user wants to open.
-- Output: None, a port is open and a message is displayed.
--
openPort = method()
openPort String := F -> (    
    if (portTest == true)
    then (
	error ("--Port "| toString inOutPort | " is currently open. To use a different port, you must first close this port with closePort().");
	)
    else(
	portTest = true;
	inOutPortNum = F;
	F = "$localhost:"|F;
	inOutPort = openListener F;
	print("--Port " | toString inOutPort | " is now open.");    
	);
)

--getCurrPath = method()
--installMethod(getCurrPath, () -> (local currPath; currPath = get "!pwd"; substring(currPath,0,(length currPath)-1)|"/"))

closePort = method()
installMethod(closePort, () -> (
     portTest = false;
     close inOutPort;
     print("--Port " | toString inOutPort | " is now closing. This could take a few seconds.");
     )
)

-- Input: none
-- Output: Error is user trying to run 'visualize' without first opening a port.
--
openPortTest = method()
installMethod(openPortTest, () -> (
    	if (portTest == false ) then error("-- You must open a port with 'openPort()' before you can communicate with the browser.");
     )
)


-- Input: File, an in-out port for communicating with the browser
-- Output: Whatever the browser sends
--
openServer = method(Options =>{Verbose => true})
openServer File := opts -> S -> (
 
local server; local fun; local listener; 
local httpHeader; local testKey;
local u; local data; local dataValue;

testKey = " ";
listener = S;

server = () -> (
    stderr << "-- Visualizing. Your browser should open automatically." << endl <<  "-- Click 'End Session' in the browser when finished." << endl;
    while true do (
        wait {listener};
        g := openInOut listener; -- this should be interruptible! (Dan's Comment, not sure what it means)
        r := read g;
        if opts.Verbose then stderr << "request: " << stack lines r << endl << endl;
        r = lines r;
	
        if #r == 0 then (close g; continue);
	
	data := last r;
        dataValue = value data;
	r = first r;
	
	-- Begin handling requests from browser
	---------------------------------------
	
	-- hasEulerianTrail
	if match("^POST /hasEulerianTrail/(.*) ",r) then (
	    -- testKey = "hasEulerianTrail";
	    fun = identity;
	    u = toString( hasEulerianTrail dataValue );
	)	
	
	-- hasOddHole
	else if match("^POST /hasOddHole/(.*) ",r) then (
	    -- testKey = "hasOddHole";
	    fun = identity;
	    u = toString( hasOddHole dataValue );
	)	
	
	-- isCM
	else if match("^POST /isCM/(.*) ",r) then (
	    -- testKey = "isCM";
	    fun = identity;
	    u = toString( isCM dataValue );
	)	

	-- isBipartite
	else if match("^POST /isBipartite/(.*) ",r) then (
	    -- testKey = "isBipartite";
	    fun = identity;
	    u = toString( isBipartite dataValue );
	)	

	-- isChordal
	else if match("^POST /isChordal/(.*) ",r) then (
	    -- testKey = "isChordal";
	    fun = identity;
	    u = toString( isChordal dataValue );
	)	
	
	-- isComparabilityGraph
	else if match("^POST /isComparabilityGraph/(.*) ",r) then (
	    -- testKey = "isComparabilityGraph";
	    fun = identity;
	    u = toString( isComparabilityGraph dataValue );
	)	
	
	-- isConnected
	else if match("^POST /isConnected/(.*) ",r) then (
	    -- testKey = "isConnected";
	    fun = identity;
	    print"isConnected else if in M2";
	    u = toString( isConnected dataValue );
	)	

    	-- isCyclic
	else if match("^POST /isCyclic/(.*) ",r) then (
	    -- testKey = "isCyclic";
	    fun = identity;
	    u = toString( isCyclic dataValue );
	)		

    	-- isEulerian
	else if match("^POST /isEulerian/(.*) ",r) then (
	    -- testKey = "isEulerian";
	    fun = identity;
	    u = toString( isEulerian dataValue );
	)		

    	-- isForest
	else if match("^POST /isForest/(.*) ",r) then (
	    -- testKey = "isForest";
	    fun = identity;
	    u = toString( isForest dataValue );
	)		

    	-- isPerfect
	else if match("^POST /isPerfect/(.*) ",r) then (
	    -- testKey = "isPerfect";
	    fun = identity;
	    u = toString( isPerfect dataValue );
	)		

    	-- isRegular
	else if match("^POST /isRegular/(.*) ",r) then (
	    -- testKey = "isRegular";
	    fun = identity;
	    u = toString( isRegular dataValue );
	)		

    	-- isSimple
	else if match("^POST /isSimple/(.*) ",r) then (
	    -- testKey = "isSimple";
	    fun = identity;
	    u = toString( isSimple dataValue );
	)		

    	-- isStronglyConnected
	else if match("^POST /isStronglyConnected/(.*) ",r) then (
	    -- testKey = "isStronglyConnected";
	    fun = identity;
	    u = toString( isStronglyConnected dataValue );
	)	
	
	-- isTree
	else if match("^POST /isTree/(.*) ",r) then (
	    -- testKey = "isTree";
	    fun = identity;
	    u = toString( isTree dataValue );
	)

	-- isRigid
	else if match("^POST /isRigid/(.*) ",r) then (
	    -- testKey = "isRigid";
	    fun = identity;
	    u = toString( isRigid dataValue );
	)    
    
        -- isWeaklyConnected
	else if match("^POST /isWeaklyConnected/(.*) ",r) then (
	    -- testKey = "isWeaklyConnected";
	    fun = identity;
	    u = toString( isWeaklyConnected dataValue );
	)	
    
        -- chromaticNumber
	else if match("^POST /chromaticNumber/(.*) ",r) then (
	    -- testKey = "chromaticNumber";
	    fun = identity;
	    u = toString( chromaticNumber dataValue );
	)			
	
	-- independenceNumber
	else if match("^POST /independenceNumber/(.*) ",r) then (
	    -- testKey = "independenceNumber";
	    fun = identity;
	    u = toString( independenceNumber dataValue );
	)			
	
	-- cliqueNumber
	else if match("^POST /cliqueNumber/(.*) ",r) then (
	    -- testKey = "cliqueNumber";
	    fun = identity;
	    u = toString( cliqueNumber dataValue );
	)			
	
	-- degeneracy
	else if match("^POST /degeneracy/(.*) ",r) then (
	    -- testKey = "degeneracy";
	    fun = identity;
	    u = toString( degeneracy dataValue );
	)			
	
	-- density
	else if match("^POST /density/(.*) ",r) then (
	    -- testKey = "density";
	    fun = identity;
	    u = toString( density dataValue );
	)			
	
	-- diameter
	else if match("^POST /diameter/(.*) ",r) then (
	    -- testKey = "diameter";
	    fun = identity;
	    u = toString( diameter dataValue );
	)			
	
	-- edgeConnectivity
	else if match("^POST /edgeConnectivity/(.*) ",r) then (
	    -- testKey = "edgeConnectivity";
	    fun = identity;
	    u = toString( edgeConnectivity dataValue );
	)			
	
	-- minimalDegree
	else if match("^POST /minimalDegree/(.*) ",r) then (
	    -- testKey = "minimalDegree";
	    fun = identity;
	    u = toString( minimalDegree dataValue );
	)			
	
	-- numberOfComponents
	else if match("^POST /numberOfComponents/(.*) ",r) then (
	    -- testKey = "numberOfComponents";
	    fun = identity;
	    u = toString( numberOfComponents dataValue );
	)			
	
	-- numberOfTriangles
	else if match("^POST /numberOfTriangles/(.*) ",r) then (
	    -- testKey = "numberOfTriangles";
	    fun = identity;
	    u = toString( numberOfTriangles dataValue );
	)			
	
	-- radius
	else if match("^POST /radius/(.*) ",r) then (
	    -- testKey = "radius";
	    fun = identity;
	    if not isConnected dataValue then u = "Not connected." else u = toString( radius dataValue );
	)			
	
	-- vertexConnectivity
	else if match("^POST /vertexConnectivity/(.*) ",r) then (
	    -- testKey = "";
	    fun = identity;
	    u = toString( vertexConnectivity dataValue );
	)			
	
	-- vertexCoverNumber
	else if match("^POST /vertexCoverNumber/(.*) ",r) then (
	    -- testKey = "vertexCoverNumber";
	    fun = identity;
	    u = toString( vertexCoverNumber dataValue );
	)			
	
	----------------------
	-- Tests for posets --
	----------------------
	
	-- isAtomic
	else if match("^POST /isAtomic/(.*) ",r) then (
	    -- testKey = "isAtomic";
	    fun = identity;
	    if not isLattice dataValue then u = "Not lattice." else u = toString( isAtomic dataValue );
	)
    
    	-- isBounded
	else if match("^POST /isBounded/(.*) ",r) then (
	    -- testKey = "isBounded";
	    fun = identity;
	    u = toString( isBounded dataValue );
	)
    
    	-- isDistributive
	else if match("^POST /isDistributive/(.*) ",r) then (
	    -- testKey = "isDistributive";
	    fun = identity;
	    if not isLattice dataValue then u = "Not lattice." else u = toString( isDistributive dataValue );
	)
    
    	-- isGeometric
	else if match("^POST /isGeometric/(.*) ",r) then (
	    -- testKey = "isGeometric";
	    fun = identity;
	    if not isLattice dataValue then u = "Not lattice." else u = toString( isGeometric dataValue );
	)
    
    	-- isGraded
	else if match("^POST /isGraded/(.*) ",r) then (
	    -- testKey = "isGraded";
	    fun = identity;
	    u = toString( isGraded dataValue );
	)
    
    	-- isLattice
	else if match("^POST /isLattice/(.*) ",r) then (
	    -- testKey = "isLattice";
	    fun = identity;
	    u = toString( isLattice dataValue );
	)
    
    	-- isLowerSemilattice
	else if match("^POST /isLowerSemilattice/(.*) ",r) then (
	    -- testKey = "isLowerSemilattice";
	    fun = identity;
	    u = toString( isLowerSemilattice dataValue );
	)
    
    	-- isLowerSemimodular (only applies to ranked posets)
	else if match("^POST /isLowerSemimodular/(.*) ",r) then (
	    -- testKey = "isLowerSemimodular";
	    fun = identity;
	    if not (isRanked dataValue and isLattice dataValue) then u = "Not ranked lattice." else u = toString( isLowerSemimodular dataValue );
	)
    
    	-- isModular
	else if match("^POST /isModular/(.*) ",r) then (
	    -- testKey = "isModular";
	    fun = identity;
	    if not isLattice dataValue then u = "Not lattice." else u = toString( isModular dataValue );
	)
    
    	-- isRanked
	else if match("^POST /isRanked/(.*) ",r) then (
	    -- testKey = "isRanked";
	    fun = identity;
	    u = toString( isRanked dataValue );
	)
    
    	-- isSperner (only applies to ranked posets)
	else if match("^POST /isSperner/(.*) ",r) then (
	    -- testKey = "isSperner";
	    fun = identity;
	    if not isRanked dataValue then u = "Not ranked." else u = toString( isSperner dataValue );
	)
    
    	-- isStrictSperner (only applies to ranked posets)
	else if match("^POST /isStrictSperner/(.*) ",r) then (
	    -- testKey = "isStrictSperner";
	    fun = identity;
	    if not isRanked dataValue then u = "Not ranked." else u = toString( isStrictSperner dataValue );
	)
    
    	-- isUpperSemilattice
	else if match("^POST /isUpperSemilattice/(.*) ",r) then (
	    -- testKey = "isUpperSemilattice";
	    fun = identity;
	    u = toString( isUpperSemilattice dataValue );
	)
    
    	-- isUpperSemimodular (only applies to ranked posets)
	else if match("^POST /isUpperSemimodular/(.*) ",r) then (
	    -- testKey = "isUpperSemimodular";
	    fun = identity;
	    if not(isRanked dataValue and isLattice dataValue) then u = "Not ranked lattice." else u = toString( isUpperSemimodular dataValue );
	)
    
    	-- dilworthNumber
	else if match("^POST /dilworthNumber/(.*) ",r) then (
	    -- testKey = "dilworthNumber";
	    fun = identity;
	    u = toString( dilworthNumber dataValue );
	)
    	
	-- End Session   
	else if match("^POST /end/(.*) ",r) then (
	    R := dataValue;
	    return R;
	)
	
	-- Error to catch typos and bad requests
	else (
	    error ("There was no match to the request: "|r);
	    );	   
	
	-- Determines the output based on the testKey
--	if (testKey == "isCM") then ( u = toString( cmTest value data ) );
--	if (testKey == "isBipartite") then ( u = toString( isBipartite value data ) );	
--	if (testKey == "isChordal") then ( u = toString( isChordal value data ) );	
--	if (testKey == "isConnected") then ( u = toString( isConnected value data ) );	
--	if (testKey == "isCyclic") then ( u = toString( isCyclic value data ) );			
--	if (testKey == "isEulerian") then ( u = toString( isEulerian value data ) );			
--	if (testKey == "isForest") then ( u = toString( isForest value data ) );			
--	if (testKey == "isPerfect") then ( u = toString( isPerfect value data ) );			
--	if (testKey == "isRegular") then ( u = toString( isRegular value data ) );			
--	if (testKey == "isSimple") then ( u = toString( isSimple value data ) );			
--	if (testKey == "isTree") then ( u = toString( isTree value data ) );			
	
	send := httpHeader fun u; 
	
	if opts.Verbose then stderr << "response: " << stack lines send << endl << endl;	  
	
	g << send << close;
	);
    );

httpHeader = ss -> concatenate(
     -- for documentation of http protocol see http://www.w3.org/Protocols/rfc2616/rfc2616.html
     -- This header is not up to the standards, but I am not sure it matters for local transmissions.
     -- I believe you are supposed to have a different header for different requests.
     "HTTP/1.1 200 OK
Server: Macaulay2
Access-Control-Allow-Origin: *
Connection: close
Content-Length: ", toString length ss, "
Content-type: text/html; charset=utf-8

", ss);

H := server();

return H;
)
 

--------------------------------------------------
-- DOCUMENTATION
--------------------------------------------------

beginDocumentation()

reldir = replace("PKG","Visualize",Layout#2#"package")	    -- this works just for installation in layouts of type 2, too bad.

document {
     Key => Visualize,
     Headline => "A package to help visualize algebraic objects in the browser using javascript",
     
     PARA "Using JavaScript, this package creates interactive visualizations of a variety of objects 
     in a modern browser. While viewing the object, the user has the ability to manipulate and 
     run various tests. Once finished, the user can export the finished result back to the 
     Macaulay2 session.",
     
     SUBSECTION "Javascript Packages Used", 
     "Built on the shoulders of giants, this package utilizes a variety of existing open-source javascript packages.",
     
     UL {
	 {HREF("https://github.com/AndreaLombardo/BootSideMenu","BootSideMenu.js")},
	 {HREF("https://github.com/dataarts/webgl-globe/blob/master/globe-vertex-texture/third-party/Three/Detector.js","Detectors.js")},
	 {HREF("http://getbootstrap.com/","Bootstrap.js")},
	 {HREF("https://clipboardjs.com/","clipboard.js")},
	 {HREF("https://d3js.org/","D3.js")},
	 {HREF("https://jquery.com/","jQuery")},
	 {HREF("http://refreshless.com/nouislider/","noUiSlider.js")},
	 {HREF("https://github.com/mrdoob/three.js/","Three.js")},
	 {HREF("http://underscorejs.org/","Underscore.js")}
	},
         
     
     SUBSECTION "Contributors",     
     "The following people have generously contributed code or worked on our code at various
     Macaulay2 workshops.",
     
     UL {
	 "Ata Firat Pir",
	 "Elliot Korte",
	 "Will Smith",
	 "Julio Urenda"	 	 
	},
     
     "In particular we are thankful to Dan Grayson and Mike Stillman for their help in creating 
     communication between Macaulay2 and the browser.",
     
     SUBSECTION "Interactive Examples",
     
     "The following links are interactive examples without the communication between Macaulay2
     and the browser. The editing, manipulation, and TikZ should work. Depending on the browser, 
     some features may require you to open the links in a new tab.",
     
     UL {
	 {HREF(reldir|"graph-example.html","Visualize Graphs example")},
	 {HREF(reldir|"digraph-example.html","Visualize Digraphs example")},	 
	 {HREF(reldir|"poset-example.html","Visualize Posets example")},	  
	 {HREF(reldir|"simplicial-complex-example.html","Visualize Simplicial Complexes example")},	 
	 {HREF(reldir|"ideal2d-example.html","Visualize Ideals in 2 variables example")},	 
	 {HREF(reldir|"ideal3d-example.html","Visualize Ideals in 3 variables example")}
        },

     SUBSECTION "Methods and Workflow",
     
     UL {
	 TO "Basic Workflow for Visualize",
	 TO (visualize,Graph),	 
	 TO (visualize,Digraph),	 	 
	 TO (visualize,Poset),	 	 
	 TO (visualize,SimplicialComplex),	 	 
	 TO (visualize,Ideal)
        }
    }

document {
    Key => "Basic Workflow for Visualize",
    PARA {"The basic workflow this package requires the user to open a port for communication between
    Macaulay2 and the web browser. Once the port is open, the ", TO "visualize", " method can be used
    freely. In particular, the workflow is as follows:"},
     
     UL{ "1. Load or install the package."},
     
     UL{{"2. Open a port with ", TO "openPort", " method for communication with the browser. 
	     It is up to the user to choose port and also to close the port when finished."}},
     
     UL{"3. Define an object you wish to visualize. For example, a graph, poset, digraph, etc."},
     
     UL{{"4. Run the ", TO "visualize", " method. This will open the browser with an interactive
	     interface. This session is in communication with Macaulay2 through the open port above.
	     At this point you can edit and manipulate the created object."}},
     
     UL{"5. End the session and export work back to Macaulay2."},
     
     UL{"6. Continue manipulating the object and repeat steps 3-5 as necessary."},
     
     UL{{"7. When finished, close the port with ", TO "closePort", " or restart Macaulay2."}},
     
    SeeAlso => {
	 "Basic Workflow for Visualize",
	 (visualize,Graph),	 
	 (visualize,Digraph),	 	 
	 (visualize,Poset),	 	 
	 (visualize,SimplicialComplex),	 	 
	 (visualize,Ideal)
	}

    }        

-*
document {
    Key => "Visualizing Graphs",	 
    
    
    SeeAlso => {
	"Basic Workflow for Visualize",
--	"Visualizing Graphs",	 
	"Visualizing Digraphs",	 	 
	"Visualizing Posets",	 	 
	"Visualizing Simplicial Complexes",	 	 
	"Visualizing Ideals"
	}
    
    }

document {
    Key => "Visualizing Digraphs",    
    SeeAlso => {
	"Basic Workflow for Visualize",
	"Visualizing Graphs",	 
--	"Visualizing Digraphs",	 	 
	"Visualizing Posets",	 	 
	"Visualizing Simplicial Complexes",	 	 
	"Visualizing Ideals"
	}
    
    }

document {
    Key => "Visualizing Posets",	 	    
    SeeAlso => {
	"Basic Workflow for Visualize",
	"Visualizing Graphs",	 
	"Visualizing Digraphs",	 	 
--	"Visualizing Posets",	 	 
	"Visualizing Simplicial Complexes",	 	 
	"Visualizing Ideals"
	}
    
    }


document {
    Key => "Visualizing Simplicial Complexes",	 	      
    SeeAlso => {
	"Basic Workflow for Visualize",
	"Visualizing Graphs",	 
	"Visualizing Digraphs",	 	 
	"Visualizing Posets",	 	 
--	"Visualizing Simplicial Complexes",	 	 
	"Visualizing Ideals"
	}
    
    }


document {
    Key => "Visualizing Ideals",

    SeeAlso => {
	"Basic Workflow for Visualize",
	"Visualizing Graphs",	 
	"Visualizing Digraphs",	 	 
	"Visualizing Posets",	 	 
	"Visualizing Simplicial Complexes",	 	 
--	"Visualizing Ideals"
	}
    
    }
*-

document {
     Key => visualize,
     Headline => "creates an interactive object in a modern browser",
     
     PARA "Given an open port, this method will create an interactive visualization of a variety of objects 
     in a modern browser. While viewing the object, the user has the ability to manipulate the 
     object, and run various tests. Once finished, the user can export the finished result back to the 
     Macaulay2 session.",

    SeeAlso => {
	 "Basic Workflow for Visualize",
	 (visualize,Graph),	 
	 (visualize,Digraph),	 	 
	 (visualize,Poset),	 	 
	 (visualize,SimplicialComplex),	 	 
	 (visualize,Ideal)
	}

    
     }


-- (visualize,Graph)
document {
     Key => (visualize,Graph),
     Headline => "visualizes a graph in a modern browser",
     Usage => " H = visualize G",
     Inputs => {
	 "G" => Graph => " a graph",
--	 Verbose => Boolean => " prints server communication in the M2 buffer",
--	 VisPath => String => " a path where the visualization will be created and saved",
--	 VisTemplate => String => " a path to a user created/modified template",
--	 Warning => Boolean => " gives a warning if files will be overwritten when using VisPath"
	 },
     
     PARA "Using JavaScript, this method creates an interactive visualization of a graph
     in a modern browser. While viewing the graph, the user has the ability to manipulate and 
     run various tests. Once finished, the user can export the finished result back to the 
     Macaulay2 session.",
     
     PARA {"The workflow for this package is as follows. Once we have loaded the package, we first 
     open a port with ", TO "openPort", " for Macaulay2 to communicate with the browser. Once a 
     port is established, define an object to visualize. In this example we could use the command ", 
     TT "openPort\"8080\""," before or after we define the following graph."},

     EXAMPLE {
--	 "openPort \"8080\"",
	 "G = graph({{0,1},{0,3},{0,4},{1,3},{2,3}},Singletons => {5})"
	 },
     
     PARA {"At this point we wish to visualize ", TT "G", ". To do this simply execute ", TT "H = visualize G", " and 
     browser will open with interactive image. You can view this image in the link below."},
     
     PARA {HREF(reldir|"graph-example.html","Visualize Graphs example")},
     
     -- make sure this image matches the graph in the example. 
--     PARA IMG ("src" => reldir|"images/Visualize/Visualize_Graph1.png", "alt" => "Original graph entered into M2"), 
     
     
     PARA {"Once finished with a session, you can keep visualizing. For example if you were to say ", TT "H = visualize G", ", once you 
	 ended the session, the last graph on the screen would be assigned to ", TT "H", ". After running various computations on this graph, 
	 you can then visualize it once more with the ", TO "visualize", " method. You can keep using this method until the port is closed with ",
	 TO "closePort", " or Macaulay2 is restarted."},
     
     
     SUBSECTION "Browser Menu Options",
     
     PARA {"On the side of the browser will be several options to interact with the graph using the ", TO "Graphs", " package. 
     Below is a brief overview of the options."},     
     
     
     UL { 
	 {BOLD "Force Variables: ", "The 'charge' slider will force the vertices to repel or attract each other while the 'links' slider
	     increases and decreases the length of the edges.", BR{}, BR{}}, 
	     
	 {BOLD "Enable Editing: ", "In the browser, you can edit the graph (add/delete vertices or edges) by clicking ", TT "Enable Editing",
	      ".  For example, in order to remove the edges ", TT "{0,1}", " and ", TT "{1,3}", " click on 'Enable Editing' and select 
	      the edges and press delete on the keyboard. You may also add vertices and edges with the mouse/trackpad. When editing is enabled,
	      you can move the vertices around by holding down the shift key.", BR{}, BR{}}, 
	      
	 {BOLD "Hide Labels: ", "Removes the labels from the vertices.", BR{}, BR{}},  	      
	 
	 {BOLD "Highlight Neighbors: ", "Allows you to see the neighbors of a vertex when selected.", BR{}, BR{}}, 
	 
	 {BOLD "Reset Nodes: ", "When you move a vertex, it will pin it to the canvas. If you have pinned a node to the canvas, you can 
	     undo this process by reseting the nodes. Clicking this will reset all nodes.", BR{}, BR{}}, 
	     
	 {BOLD "Turn off force: ", "The force is what creates the charges on the nodes. Turning this off will make the vertices 
	     not repel each other.", BR{}, BR{}}, 
	     
	 {BOLD "Generate TikZ code: ", "Clicking this will generate the TikZ code for a black and white version of the graph that is
	     being viewed. You will then have to copy this to the clipboard by clicking a new button. The TeX file will only need 
	     '\\usepackage{tikz}' in the preamble. If you decide you want to modify the graph, feel free. All you need do in 
	     order to get a new TikZ code is click on 'Generate TikZ code' once more, and then click the button. This button will look
	     the same, but it will be a new code.", BR{}, BR{}}, 
	     
	 {BOLD "Boolean tests: ", "Clicking this will pull up a submenu of boolean tests supported by the ", TO "Graphs", " package. 
	     Clicking on these tests will send a request to Macaulay2 to calculate the answer for the current graph on the screen. ",
	     BOLD "Warning:", " If your graph is too large, clicking a menu item could take a very long time. In order to cancel the 
	     process you need to kill the session of Macaulay2 with 'Ctrl+c+c' in the instance of Macaulay2.", BR{}, BR{}}, 
	     
	 {BOLD "Numerical invariants: ", "Clicking this will pull up a submenu of numerical invariants supported by the ", TO "Graphs", " package. 
	     Clicking on these will send a request to Macaulay2 to calculate the answer for the current graph on the screen. ",
	     BOLD "Warning:", " If your graph is too large, clicking a menu item could take a very long time. In order to cancel the 
	     process you need to kill the session of Macaulay2 with 'Ctrl+c+c' in the instance of Macaulay2.", BR{}, BR{}},  	      	     
	     
	 {BOLD "End session: ", "When you are finished editing, you can end the session and send the current graph to Macaulay2. The 
	     output of ", TT "visualize G", " is the graph on the screen when end session is clicked."} 	      	     

	 },
	
     
--     PARA IMG ("src" => reldir|"images/Visualize/Visualize_Graph2.png", "alt" => "Original graph entered into M2"), 

    Caveat => {"When the graph is exported back to Macaulay2 after ending the visualization session, all vertices are represented as strings.  To recover the values of these labels (for example, if they have numeric values or represent ring variables), use the command ", TT "value first toString G", "."},
     
    SeeAlso => {
	 "Basic Workflow for Visualize",
--	 (visualize,Graph),	 
	 (visualize,Digraph),	 	 
	 (visualize,Poset),	 	 
	 (visualize,SimplicialComplex),	 	 
	 (visualize,Ideal)
	},
     
     }
 
 
-- (visualize,Ideal)
document {
     Key => (visualize,Ideal),
     Headline => "visualizes an ideal in the browser",
     
     PARA {"Using JavaScript, this method creates an interactive visualization of an ideal, in either 2 or 3
     variables, in a modern browser. Unlike other ", TO "Type","s, when visualizing an ideal you 
     do not need to open a port as there is no communication between Macaulay2 and the browser. 
     We hope to add more functionality to this method in future versions. The ideal being visualized
     is actually the ideal of leading terms."},

     EXAMPLE {
	 "S = QQ[x,y]",
	 "I = ideal\"x4,xy3,y5\""
	 },
     
     PARA {"In order to visualize this, simply type ", TT "visualize I", " and the following example will appear in the browser."},
     
     PARA {HREF(reldir|"ideal2d-example.html","Visualize ideal in 2 variables example")},     
     
     EXAMPLE {
	 "R = QQ[x,y,z]",
	 "J = ideal\"x4,xyz3,yz2,xz3,z6,y5\""
	 },
     
     PARA {"In order to visualize this, simply type ", TT "visualize J", " and the following example will appear in the browser."},  
     
     PARA {HREF(reldir|"ideal3d-example.html","Visualize ideal in 3 variables example")},             
     
     Caveat => {"Visualizing ideals is still in development so please be gentle with it."},     
     
    SeeAlso => {
	 "Basic Workflow for Visualize",
	 (visualize,Graph),	 
	 (visualize,Digraph),	 	 
	 (visualize,Poset),	 	 
	 (visualize,SimplicialComplex)	 	 
--	 (visualize,Ideal)
	}
     
     }

-- (visualize,Digraph)
document {
     Key => (visualize,Digraph),
     Headline => "visualizes a digraph in the browser",
     
     PARA "Using JavaScript, this method creates an interactive visualization of a digraph
     in a modern browser. While viewing the digraph, the user has the ability to manipulate and 
     run various tests. Once finished, the user can export the finished result back to the 
     Macaulay2 session.",
     
     PARA {"The workflow for this package is as follows. Once we have loaded the package, we first 
     open a port with ", TO "openPort", " for Macaulay2 to communicate with the browser. Once a 
     port is established, define an object to visualize. In this example we could use the command ", 
     TT "openPort\"8080\""," before or after we define the following digraph."},

     EXAMPLE {
--	 "openPort \"8080\"",
	 "D = digraph {{1,{2,3}}, {2,{4,5}}, {3,{5,6}}, {4,{7}}, {5,{7}},{6,{7}},{7,{}}}"
	 },
     
     PARA {"At this point we wish to visualize ", TT "D", ". To do this simply execute ", TT "H = visualize D", " and 
     browser will open with interactive image. You can view this image in the link below."},
     
     PARA {HREF(reldir|"digraph-example.html","Visualize Digraphs example")},
     
     -- make sure this image matches the graph in the example. 
--     PARA IMG ("src" => reldir|"images/Visualize/Visualize_Graph1.png", "alt" => "Original graph entered into M2"), 
     
     
     PARA {"Once finished with a session, you can keep visualizing. For example if you were to say ", TT "H = visualize D", ", once you 
	 ended the session, the last digraph on the screen would be assigned to ", TT "H", ". After running various computations on this digraph, 
	 you can then visualize it once more with the ", TO "visualize", " method. You can keep using this method until the port is closed with ",
	 TO "closePort", " or Macaulay2 is restarted."},
     
     
     SUBSECTION "Browser Menu Options",
     
     PARA {"On the side of the browser will be several options to interact with the digraph using the ", TO "Graphs", " package. 
     Below is a brief overview of the options."},     
     
     
     UL { 
	 {BOLD "Force Variables: ", "The 'charge' slider will force the vertices to repel or attract each other while the 'links' slider
	     increases and decreases the length of the edges.", BR{}, BR{}}, 
	     
	 {BOLD "Enable Editing: ", "In the browser, you can edit the digraph (add/delete vertices or edges) by clicking ", TT "Enable Editing",
	      ".  For example, in order to remove the edges ", TT "{0,1}", " and ", TT "{1,3}", " click on 'Enable Editing' and select 
	      the edges and press delete on the keyboard. You may also add vertices and edges with the mouse/trackpad. When editing is enabled,
	      you can move the vertices around by holding down the shift key. Further, to create a loop, select a node and press the 'r' key.
	      When an edge is selected, you may press the 'l', 'r', and 'b' keys to change the edge to a left-, right-, or two-sided edge, respectively.", 
	      BR{}, BR{}}, 
	      
	 {BOLD "Hide Labels: ", "Removes the labels from the vertices.", BR{}, BR{}},  	      
	 
	 {BOLD "Highlight Neighbors: ", "Allows you to see the neighbors of a vertex when selected.", BR{}, BR{}}, 
	 
	 {BOLD "Reset Nodes: ", "When you move a vertex, it will pin it to the canvas. If you have pinned a node to the canvas, you can 
	     undo this process by reseting the nodes. Clicking this will reset all nodes.", BR{}, BR{}}, 
	     
	 {BOLD "Turn off force: ", "The force is what creates the charges on the nodes. Turning this off will make the vertices 
	     not repel each other.", BR{}, BR{}}, 
	     
	 {BOLD "Generate TikZ code: ", "Clicking this will generate the TikZ code for a black and white version of the digraph that is
	     being viewed. You will then have to copy this to the clipboard by clicking a new button. The TeX file will only need 
	     '\\usepackage{tikz}' in the preamble. If you decide you want to modify the digraph, feel free. All you need do in 
	     order to get a new TikZ code is click on 'Generate TikZ code' once more, and then click the button. This button will look
	     the same, but it will be a new code.", BR{}, BR{}}, 
	     
	 {BOLD "Boolean tests: ", "Clicking this will pull up a submenu of boolean tests supported by the ", TO "Graphs", " package. 
	     Clicking on these tests will send a request to Macaulay2 to calculate the answer for the current digraph on the screen. ",
	     BOLD "Warning:", " If your digraph is too large, clicking a menu item could take a very long time. In order to cancel the 
	     process you need to kill the session of Macaulay2 with 'Ctrl+c+c' in the instance of Macaulay2.", BR{}, BR{}}, 
	     
	 {BOLD "Numerical invariants: ", "Clicking this will pull up a submenu of numerical invariants supported by the ", TO "Graphs", " package. 
	     Clicking on these will send a request to Macaulay2 to calculate the answer for the current digraph on the screen. ",
	     BOLD "Warning:", " If your digraph is too large, clicking a menu item could take a very long time. In order to cancel the 
	     process you need to kill the session of Macaulay2 with 'Ctrl+c+c' in the instance of Macaulay2.", BR{}, BR{}},  	      	     
	     
	 {BOLD "End session: ", "When you are finished editing, you can end the session and send the current digraph to Macaulay2. The 
	     output of ", TT "visualize D", " is the digraph on the screen when end session is clicked."} 	      	     

	 },
	
    Caveat => {"When the digraph is exported back to Macaulay2 after ending the visualization session, all vertices are represented as strings.  To recover the values of these labels (for example, if they have numeric values or represent ring variables), use the command ", TT "value first toString G", "."},
     
    SeeAlso => {
	 "Basic Workflow for Visualize",
	 (visualize,Graph),	 
--	 (visualize,Digraph),	 	 
	 (visualize,Poset),	 	 
	 (visualize,SimplicialComplex),	 	 
	 (visualize,Ideal)
	}
     
     }


-- (visualize,Poset)
document {
     Key => (visualize,Poset),
     Headline => "visualizes a poset in the browser",

     
     PARA "Using JavaScript, this method creates an interactive visualization of a poset
     in a modern browser. While viewing the poset, the user has the ability to manipulate and 
     run various tests. Once finished, the user can export the finished result back to the 
     Macaulay2 session.",
     
     PARA {"The workflow for this package is as follows. Once we have loaded the package, we first 
     open a port with ", TO "openPort", " for Macaulay2 to communicate with the browser. Once a 
     port is established, define an object to visualize. In this example we could use the command ", 
     TT "openPort\"8080\""," before or after we define the following poset."},

     EXAMPLE {
--	 "openPort \"8080\"",
    	 "P = poset {{1,2},{2,3},{3,4},{5,6},{6,7},{3,6}}"
	 },
     
     PARA {"At this point we wish to visualize ", TT "P", ". To do this simply execute ", TT "H = visualize P", " and 
     browser will open with interactive image. You can view this image in the link below."},
     
     PARA {HREF(reldir|"poset-example.html","Visualize Posets example")},
     
     -- make sure this image matches the graph in the example. 
--     PARA IMG ("src" => reldir|"images/Visualize/Visualize_Graph1.png", "alt" => "Original graph entered into M2"), 
     
     
     PARA {"Once finished with a session, you can keep visualizing. For example if you were to say ", TT "H = visualize P", ", once you 
	 ended the session, the last poset on the screen would be assigned to ", TT "H", ". After running various computations on this poset, 
	 you can then visualize it once more with the ", TO "visualize", " method. You can keep using this method until the port is closed with ",
	 TO "closePort", " or Macaulay2 is restarted."},
     
     
     SUBSECTION "Browser Menu Options",
     
     PARA {"On the side of the browser will be several options to interact with the poset using the ", TO "Posets", " package. 
     Below is a brief overview of the options."},     
     
     
     UL { 
	 {BOLD "Force Variables: ", "The 'charge' slider will force the vertices to repel or attract each other while the 'links' slider
	     increases and decreases the length of the edges.", BR{}, BR{}}, 
	     
	 {BOLD "Enable Editing: ", "In the browser, you can edit the poset (add/delete vertices or edges) by clicking ", TT "Enable Editing",
	      ".  For example, in order to remove the edge ", TT "{1,2}" , ", click on 'Enable Editing' and select 
	      the edge and press delete on the keyboard.  When deleting an edge, the minimal covering relations and visual display are automatically
	      updated to reflect the new poset structure.  You may also add vertices and edges with the mouse/trackpad. When adding a new edge
	      (relation), you must drag from the smaller vertex in the relation to the larger vertex.  Each time a new edge is added, a check is
	      performed to determine whether the new edge would violate antisymmetry, and if so then an alert is displayed.  Also, each time a new
	      edge is added, the transitive closure is computed, the rank/height of each poset element is recomputed, and the visual display of the
	      poset is updated.  When editing is enabled, you can move the vertices around by holding down the shift key.  ", 
	      BR{}, BR{}}, 
	      
	 {BOLD "Hide Labels: ", "Removes the labels from the vertices.", BR{}, BR{}},  	      
	 
	 {BOLD "Highlight comparable elements: ", "Allows you to see the comparable elements when a vertex is selected.", BR{}, BR{}}, 
	 
	 {BOLD "Fix extremal nodes: ", "Forces extremal nodes to be placed at the top and bottom of the image.", BR{}, BR{}}, 	 
	 
	 {BOLD "Reset Nodes: ", "When you move a vertex, it will pin it to the canvas. If you have pinned a node to the canvas, you can 
	     undo this process by reseting the nodes. Clicking this will reset all nodes.", BR{}, BR{}}, 
	     
	 {BOLD "Turn off force: ", "The force is what creates the charges on the nodes. Turning this off will make the vertices 
	     not repel each other.", BR{}, BR{}}, 
	     
	 {BOLD "Generate TikZ code: ", "Clicking this will generate the TikZ code for a black and white version of the poset that is
	     being viewed. You will then have to copy this to the clipboard by clicking a new button. The TeX file will only need 
	     '\\usepackage{tikz}' in the preamble. If you decide you want to modify the poset, feel free. All you need do in 
	     order to get a new TikZ code is click on 'Generate TikZ code' once more, and then click the button. This button will look
	     the same, but it will be a new code.", BR{}, BR{}}, 
	     
	 {BOLD "Boolean tests: ", "Clicking this will pull up a submenu of boolean tests supported by the ", TO "Posets", " package. 
	     Clicking on these tests will send a request to Macaulay2 to calculate the answer for the current poset on the screen. ",
	     BOLD "Warning:", " If your poset is too large, clicking a menu item could take a very long time. In order to cancel the 
	     process you need to kill the session of Macaulay2 with 'Ctrl+c+c' in the instance of Macaulay2.", BR{}, BR{}}, 
	     
	 {BOLD "Numerical invariants: ", "Clicking this will pull up a submenu of numerical invariants supported by the ", TO "Posets", " package. 
	     Clicking on these will send a request to Macaulay2 to calculate the answer for the current poset on the screen. ",
	     BOLD "Warning:", " If your poset is too large, clicking a menu item could take a very long time. In order to cancel the 
	     process you need to kill the session of Macaulay2 with 'Ctrl+c+c' in the instance of Macaulay2.", BR{}, BR{}},  	      	     
	     
	 {BOLD "End session: ", "When you are finished editing, you can end the session and send the current poset to Macaulay2. The 
	     output of ", TT "visualize P", " is the poset on the screen when end session is clicked."} 	      	     

	 },
	
    Caveat => {"When the poset is exported back to Macaulay2 after ending the visualization session, all vertices are represented as strings.  To recover the values of these labels (for example, if they have numeric values or represent ring variables), use the command ", TT "poset(value toString vertices P,value toString coveringRelations P)", "."},

    SeeAlso => {
	 "Basic Workflow for Visualize",
	 (visualize,Graph),	 
	 (visualize,Digraph),	 	 
--	 (visualize,Poset),	 	 
	 (visualize,SimplicialComplex),	 	 
	 (visualize,Ideal)
	}
     
     }

-- (visualize,SimplicialComplex)
document {
     Key => (visualize,SimplicialComplex),
     Headline => "visualizes a simplicial complex in the browser",
     
     PARA "Using JavaScript, this method creates an interactive visualization of a simplicial complex
     in a modern browser. While viewing the simplicial complex, the user has the ability to manipulate and 
     run various tests. Once finished, the user can export the finished result back to the 
     Macaulay2 session.",
     
     PARA {"The workflow for this package is as follows. Once we have loaded the package, we first 
     open a port with ", TO "openPort", " for Macaulay2 to communicate with the browser. Once a 
     port is established, define an object to visualize. In this example we could use the command ", 
     TT "openPort\"8080\""," before or after we define the following simplicial complex."},

     EXAMPLE {
--	 "openPort \"8080\"",
    	 "R = ZZ[a..g]",
    	 "D = simplicialComplex {a*b*c,a*b*d,a*e*f,a*g}"
	 },
     
     PARA {"At this point we wish to visualize ", TT "D", ". To do this simply execute ", TT "H = visualize D", " and 
     browser will open with interactive image. You can view this image in the link below."},
     
     PARA {HREF(reldir|"simplicial-complex-example.html","Visualize Simplicial Complex example")},
     
     -- make sure this image matches the graph in the example. 
--     PARA IMG ("src" => reldir|"images/Visualize/Visualize_Graph1.png", "alt" => "Original graph entered into M2"), 
     
     
     PARA {"Once finished with a session, you can keep visualizing. For example if you were to say ", TT "H = visualize D", ", once you 
	 ended the session, the last simplicial complex on the screen would be assigned to ", TT "H", ". After running various computations on this simplicial complex, 
	 you can then visualize it once more with the ", TO "visualize", " method. You can keep using this method until the port is closed with ",
	 TO "closePort", " or Macaulay2 is restarted."},
     
     
     SUBSECTION "Browser Menu Options",
     
     PARA {"On the side of the browser will be several options to interact with the simplicial complex using the ", TO "SimplicialComplex", " package. 
     Below is a brief overview of the options."},     
     
     
     UL { 
	 {BOLD "Force Variables: ", "The 'charge' slider will force the vertices to repel or attract each other while the 'links' slider
	     increases and decreases the length of the edges.", BR{}, BR{}}, 
	     
	 {BOLD "Enable Editing: ", "In the browser, you can edit the simplicial complex (add/delete vertices or edges) by clicking ", TT "Enable Editing",
	      ".  For example, in order to remove the edges ", TT "{0,1}", " and ", TT "{1,3}", " click on 'Enable Editing' and select 
	      the edges and press delete on the keyboard. You may also add vertices and edges with the mouse/trackpad. When editing is enabled,
	      you can move the vertices around by holding down the shift key. Further, to create a face, hold down the 'f' key and select 
	      three distinct nodes.", 
	      BR{}, BR{}}, 
	      
	 {BOLD "Hide Labels: ", "Removes the labels from the vertices.", BR{}, BR{}},  	      
	 
	 {BOLD "Highlight faces: ", "Allows you to see the connecting faces when a vertex is selected.", BR{}, BR{}}, 
	 
	 {BOLD "Reset Nodes: ", "When you move a vertex, it will pin it to the canvas. If you have pinned a node to the canvas, you can 
	     undo this process by reseting the nodes. Clicking this will reset all nodes.", BR{}, BR{}}, 
	     
	 {BOLD "Turn off force: ", "The force is what creates the charges on the nodes. Turning this off will make the vertices 
	     not repel each other.", BR{}, BR{}}, 
	     
	 {BOLD "Generate TikZ code: ", "Clicking this will generate the TikZ code for a black and white version of the simplicial complex that is
	     being viewed. You will then have to copy this to the clipboard by clicking a new button. The TeX file will only need 
	     '\\usepackage{tikz}' in the preamble. If you decide you want to modify the simplicial complex, feel free. All you need do in 
	     order to get a new TikZ code is click on 'Generate TikZ code' once more, and then click the button. This button will look
	     the same, but it will be a new code.", BR{}, BR{}}, 
	     
	 {BOLD "Boolean tests: ", "Clicking this will pull up a submenu of boolean tests supported by the ", TO "SimplicialComplex", " package. 
	     Clicking on these tests will send a request to Macaulay2 to calculate the answer for the current simplicial complex on the screen. ",
	     BOLD "Warning:", " If your simplicial complex is too large, clicking a menu item could take a very long time. In order to cancel the 
	     process you need to kill the session of Macaulay2 with 'Ctrl+c+c' in the instance of Macaulay2.", BR{}, BR{}}, 
	     
--	 {BOLD "Numerical invariants: ", "Clicking this will pull up a submenu of numerical invariants supported by the ", TO "SimplicialComplex", " package. 
--	     Clicking on these will send a request to Macaulay2 to calculate the answer for the current simplicial complex on the screen. ",
--	     BOLD "Warning:", " If your simplicial complex is too large, clicking a menu item could take a very long time. In order to cancel the 
--	     process you need to kill the session of Macaulay2 with 'Ctrl+c+c' in the instance of Macaulay2.", BR{}, BR{}},  	      	     
	     
	 {BOLD "End session: ", "When you are finished editing, you can end the session and send the current simplicial complex to Macaulay2. The 
	     output of ", TT "visualize D", " is the simplicial complex on the screen when end session is clicked."} 	      	     

	 },
	
     

    SeeAlso => {
	 "Basic Workflow for Visualize",
	 (visualize,Graph),	 
	 (visualize,Digraph),	 	 
	 (visualize,Poset),	 	 
--	 (visualize,SimplicialComplex),	 	 
	 (visualize,Ideal)
	}

     }

 
document {
     Key => VisPath,
     Headline => "an option to define a path to save visualizations",
          
     PARA {"The default nature of the Visualize package is to open the visualization in a temporary file. Use the ", TT "VisPath", 
	 " option if you wish to save the visualization to a given directory. If the process will overwrite files, a warning appears
	 asking the user if they would like to proceed. You can squelch this warning with the ", TO "Warning", " option."},

    SeeAlso => {
	 "Basic Workflow for Visualize",
	 (visualize,Graph),	 
	 (visualize,Digraph),	 	 
	 (visualize,Poset),	 	 
	 (visualize,SimplicialComplex),	 	 
	 (visualize,Ideal)
	}
	 
     }

document {
     Key => VisTemplate,
     Headline => "an option defining the template path",
     
     PARA {"This option is used internally to pass paths from one method to another. A savvy user would be able to use this option 
     to create a personal template and pass the path for ", TT "Visualize.m2", " to use."},
     
    SeeAlso => {
	 "Basic Workflow for Visualize",
	 (visualize,Graph),	 
	 (visualize,Digraph),	 	 
	 (visualize,Poset),	 	 
	 (visualize,SimplicialComplex),	 	 
	 (visualize,Ideal)
	}
     
     }
 
document {
     Key => Warning,
     Headline => "an option to squelch warnings",
     
     PARA {"When using the option ", TO "VisPath", " a warning is produced if files will be overwritten. ", TT "Warning", " is 
	 used to squelch these warnings. The default value is true, meaning the warning will be displayed."},
	 
    SeeAlso => {
	 "Basic Workflow for Visualize",
	 (visualize,Graph),	 
	 (visualize,Digraph),	 	 
	 (visualize,Poset),	 	 
	 (visualize,SimplicialComplex),	 	 
	 (visualize,Ideal)
	}
	 
     }

document {
     Key => {FixExtremeElements,[(visualize,Poset),FixExtremeElements]},
     Headline => "an option to fix extreme elements of a poset",
     
     PARA "When using this option, the visualized poset will be drawn such that the extreme elements are placed 
     at the top and bottom of the drawing.",
     
    SeeAlso => {
	 "Basic Workflow for Visualize",
	 (visualize,Poset)
	}
     
     }

document {
     Key => [(visualize,Digraph),VisPath],
     Headline => "an option to define a path to save visualizations of Digraphs",
     Usage => "H = visualize (D, VisPath => \"/PATH/TO/DIRECTORY/\")",
     Inputs => {"D" => Digraph => "a Digraph"},
     
     PARA {"The default nature of the Visualize package is to open the visualization in a temporary file. Use the ", TT "VisPath", 
	 " option if you wish to save the visualization to a given directory. If the process will overwrite files, a warning appears
	 asking the user if they would like to proceed. You can squelch this warning with the ", TO "Warning", " option."},
	 
    SeeAlso => {
	 "Basic Workflow for Visualize",
	 (visualize,Graph),	 
	 (visualize,Digraph),	 	 
	 (visualize,Poset),	 	 
	 (visualize,SimplicialComplex),	 	 
	 (visualize,Ideal)
	}
	 
     }

document {
     Key => [(visualize,Graph),VisPath],
     Headline => "an option to define a path to save visualizations of Graphs",
     Usage => "H = visualize (G, VisPath => \"/PATH/TO/DIRECTORY/\")",
     Inputs => {"G" => Graph => "a graph"},
     
     PARA {"The default nature of the Visualize package is to open the visualization in a temporary file. Use the ", TT "VisPath", 
	 " option if you wish to save the visualization to a given directory. If the process will overwrite files, a warning appears
	 asking the user if they would like to proceed. You can squelch this warning with the ", TO "Warning", " option."},

    SeeAlso => {
	 "Basic Workflow for Visualize",
	 (visualize,Graph),	 
	 (visualize,Digraph),	 	 
	 (visualize,Poset),	 	 
	 (visualize,SimplicialComplex),	 	 
	 (visualize,Ideal)
	}
	 
     }


document {
     Key => [(visualize,Ideal),VisPath],
     Headline => "an option to define a path to save visualizations of Ideals",
     Usage => "H = visualize (I, VisPath => \"/PATH/TO/DIRECTORY/\")",
     Inputs => {"I" => Ideal => "an ideal"},
     
     PARA {"The default nature of the Visualize package is to open the visualization in a temporary file. Use the ", TT "VisPath", 
	 " option if you wish to save the visualization to a given directory. If the process will overwrite files, a warning appears
	 asking the user if they would like to proceed. You can squelch this warning with the ", TO "Warning", " option."},
	 
    SeeAlso => {
	 "Basic Workflow for Visualize",
	 (visualize,Graph),	 
	 (visualize,Digraph),	 	 
	 (visualize,Poset),	 	 
	 (visualize,SimplicialComplex),	 	 
	 (visualize,Ideal)
	}
	 
     }

document {
     Key => [(visualize,Poset),VisPath],
     Headline => "an option to define a path to save visualizations of Posets",
     Usage => "H = visualize (P, VisPath => \"/PATH/TO/DIRECTORY/\")",
     Inputs => {"P" => Poset => "a poset"},
     
     PARA {"The default nature of the Visualize package is to open the visualization in a temporary file. Use the ", TT "VisPath", 
	 " option if you wish to save the visualization to a given directory. If the process will overwrite files, a warning appears
	 asking the user if they would like to proceed. You can squelch this warning with the ", TO "Warning", " option."},
	 
    SeeAlso => {
	 "Basic Workflow for Visualize",
	 (visualize,Graph),	 
	 (visualize,Digraph),	 	 
	 (visualize,Poset),	 	 
	 (visualize,SimplicialComplex),	 	 
	 (visualize,Ideal)
	}
	 
     }


document {
     Key => [(visualize,SimplicialComplex),VisPath],
     Headline => "an option to define a path to save visualizations of simplicial complexes",
     Usage => "H = visualize (S, VisPath => \"/PATH/TO/DIRECTORY/\")",
     Inputs => {"S" => SimplicialComplex => "a simplicial complex"},
     
     PARA {"The default nature of the Visualize package is to open the visualization in a temporary file. Use the ", TT "VisPath", 
	 " option if you wish to save the visualization to a given directory. If the process will overwrite files, a warning appears
	 asking the user if they would like to proceed. You can squelch this warning with the ", TO "Warning", " option."},
	 
    SeeAlso => {
	 "Basic Workflow for Visualize",
	 (visualize,Graph),	 
	 (visualize,Digraph),	 	 
	 (visualize,Poset),	 	 
	 (visualize,SimplicialComplex),	 	 
	 (visualize,Ideal)
	}
	 
     }


document {
     Key => [(visualize,Digraph),VisTemplate],
     Headline => "an option defining the template path",
     Usage => "H = visualize (D, VisPath => \"/PATH/TO/TEMPLATE/\")",
     Inputs => {"D" => Digraph => "a digraph"},
     
     PARA {"This option is used internally to pass paths from one method to another. A savvy user would be able to use this option 
     to create a personal template and pass the path for ", TT "Visualize.m2", " to use."},
     
    SeeAlso => {
	 "Basic Workflow for Visualize",
	 (visualize,Graph),	 
	 (visualize,Digraph),	 	 
	 (visualize,Poset),	 	 
	 (visualize,SimplicialComplex),	 	 
	 (visualize,Ideal)
	}
     

     }

document {
     Key => [(visualize,Graph),VisTemplate],
     Headline => "an option defining the template path",
     Usage => "H = visualize (G, VisPath => \"/PATH/TO/TEMPLATE/\")",
     Inputs => {"G" => Graph => "a graph"},
     
     PARA {"This option is used internally to pass paths from one method to another. A savvy user would be able to use this option 
     to create a personal template and pass the path for ", TT "Visualize.m2", " to use."},
     
    SeeAlso => {
	 "Basic Workflow for Visualize",
	 (visualize,Graph),	 
	 (visualize,Digraph),	 	 
	 (visualize,Poset),	 	 
	 (visualize,SimplicialComplex),	 	 
	 (visualize,Ideal)
	}
     
     }

document {
     Key => [(visualize,Ideal),VisTemplate],
     Headline => "an option defining the template path",
     Usage => "H = visualize (I, VisPath => \"/PATH/TO/TEMPLATE/\")",
     Inputs => {"I" => Ideal => "an ideal"},
     
     PARA {"This option is used internally to pass paths from one method to another. A savvy user would be able to use this option 
     to create a personal template and pass the path for ", TT "Visualize.m2", " to use."},
     
    SeeAlso => {
	 "Basic Workflow for Visualize",
	 (visualize,Graph),	 
	 (visualize,Digraph),	 	 
	 (visualize,Poset),	 	 
	 (visualize,SimplicialComplex),	 	 
	 (visualize,Ideal)
	}
     

     }

document {
     Key => [(visualize,Poset),VisTemplate],
     Headline => "an option defining the template path",
     Usage => "H = visualize (P, VisPath => \"/PATH/TO/TEMPLATE/\")",
     Inputs => {"P" => Poset => "a poset"},
     
     PARA {"This option is used internally to pass paths from one method to another. A savvy user would be able to use this option 
     to create a personal template and pass the path for ", TT "Visualize.m2", " to use."},
     
    SeeAlso => {
	 "Basic Workflow for Visualize",
	 (visualize,Graph),	 
	 (visualize,Digraph),	 	 
	 (visualize,Poset),	 	 
	 (visualize,SimplicialComplex),	 	 
	 (visualize,Ideal)
	}
     

     }

document {
     Key => [(visualize,SimplicialComplex),VisTemplate],
     Headline => "an option defining the template path",
     Usage => "H = visualize (S, VisPath => \"/PATH/TO/TEMPLATE/\")",
     Inputs => {"S" => SimplicialComplex => "a simplicial complex"},
     
     PARA {"This option is used internally to pass paths from one method to another. A savvy user would be able to use this option 
     to create a personal template and pass the path for ", TT "Visualize.m2", " to use."},
     
    SeeAlso => {
	 "Basic Workflow for Visualize",
	 (visualize,Graph),	 
	 (visualize,Digraph),	 	 
	 (visualize,Poset),	 	 
	 (visualize,SimplicialComplex),	 	 
	 (visualize,Ideal)
	}
     

     }

document {
     Key => [(visualize,Digraph),Warning],
     Headline => "an option to squelch warnings",
     Usage => "H = visualize (D, VisPath => \"/PATH/TO/DIRECTORY/\", Warning => false)",     
     Inputs => {"D" => Digraph => "a digraph"},
     
     PARA {"When using the option ", TO "VisPath", " a warning is produced if files will be overwritten. ", TT "Warning", " is 
	 used to squelch these warnings. The default value is true, meaning the warning will be displayed."},
	 
    SeeAlso => {
	 "Basic Workflow for Visualize",
	 (visualize,Graph),	 
	 (visualize,Digraph),	 	 
	 (visualize,Poset),	 	 
	 (visualize,SimplicialComplex),	 	 
	 (visualize,Ideal)
	}
	 

     }

document {
     Key => [(visualize,Graph),Warning],
     Headline => "an option to squelch warnings",
     Usage => "H = visualize (G, VisPath => \"/PATH/TO/DIRECTORY/\", Warning => false)",     
     Inputs => {"G" => Graph => "a graph"},
     
     PARA {"When using the option ", TO "VisPath", " a warning is produced if files will be overwritten. ", TT "Warning", " is 
	 used to squelch these warnings. The default value is true, meaning the warning will be displayed."},
	 
    SeeAlso => {
	 "Basic Workflow for Visualize",
	 (visualize,Graph),	 
	 (visualize,Digraph),	 	 
	 (visualize,Poset),	 	 
	 (visualize,SimplicialComplex),	 	 
	 (visualize,Ideal)
	}
	 

     }


document {
     Key => [(visualize,Ideal),Warning],
     Headline => "an option to squelch warnings",
     Usage => "H = visualize (I, VisPath => \"/PATH/TO/DIRECTORY/\", Warning => false)",     
     Inputs => {"I" => Ideal => "an ideal"},
     
     PARA {"When using the option ", TO "VisPath", " a warning is produced if files will be overwritten. ", TT "Warning", " is 
	 used to squelch these warnings. The default value is true, meaning the warning will be displayed."},
	 
    SeeAlso => {
	 "Basic Workflow for Visualize",
	 (visualize,Graph),	 
	 (visualize,Digraph),	 	 
	 (visualize,Poset),	 	 
	 (visualize,SimplicialComplex),	 	 
	 (visualize,Ideal)
	}
	 

     }


document {
     Key => [(visualize,Poset),Warning],
     Headline => "an option to squelch warnings",
     Usage => "H = visualize (P, VisPath => \"/PATH/TO/DIRECTORY/\", Warning => false)",     
     Inputs => {"P" => Poset => "a poset"},
     
     PARA {"When using the option ", TO "VisPath", " a warning is produced if files will be overwritten. ", TT "Warning", " is 
	 used to squelch these warnings. The default value is true, meaning the warning will be displayed."},
	 
    SeeAlso => {
	 "Basic Workflow for Visualize",
	 (visualize,Graph),	 
	 (visualize,Digraph),	 	 
	 (visualize,Poset),	 	 
	 (visualize,SimplicialComplex),	 	 
	 (visualize,Ideal)
	}
	 

     }

document {
     Key => [(visualize,SimplicialComplex),Warning],
     Headline => "an option to squelch warnings",
     Usage => "H = visualize (S, VisPath => \"/PATH/TO/DIRECTORY/\", Warning => false)",     
     Inputs => {"S" => SimplicialComplex => "a simplicial complex"},
     
     PARA {"When using the option ", TO "VisPath", " a warning is produced if files will be overwritten. ", TT "Warning", " is 
	 used to squelch these warnings. The default value is true, meaning the warning will be displayed."},
	 
    SeeAlso => {
	 "Basic Workflow for Visualize",
	 (visualize,Graph),	 
	 (visualize,Digraph),	 	 
	 (visualize,Poset),	 	 
	 (visualize,SimplicialComplex),	 	 
	 (visualize,Ideal)
	}
	 

     }




document {
     Key => [(visualize,Graph),Verbose],
     Headline => "an to view communication between Macaulay2 server and browser ",
     Usage => "H = visualize (G, Verbose => true)",     
     Inputs => {"G" => Graph => "a graph"},

     PARA {"When this option is used, the user can view the communication between the Macaulay2 server and the browser.
	 The communication will be displayed in the instance of Macaulay2."},
	 
    SeeAlso => {
	 "Basic Workflow for Visualize",
	 (visualize,Graph),	 
	 (visualize,Digraph),	 	 
	 (visualize,Poset),	 	 
	 (visualize,SimplicialComplex),	 	 
	 (visualize,Ideal)
	}
	 
     }

document {
     Key => [(visualize,Digraph),Verbose],
     Headline => "an to view communication between Macaulay2 server and browser ",
     Usage => "H = visualize (D, Verbose => true)",     
     Inputs => {"D" => Digraph => "a digraph"},

     PARA {"When this option is used, the user can view the communication between the Macaulay2 server and the browser.
	 The communication will be displayed in the instance of Macaulay2."},
	 
    SeeAlso => {
	 "Basic Workflow for Visualize",
	 (visualize,Graph),	 
	 (visualize,Digraph),	 	 
	 (visualize,Poset),	 	 
	 (visualize,SimplicialComplex),	 	 
	 (visualize,Ideal)
	}
	 

     }


document {
     Key => [(visualize,Poset),Verbose],
     Headline => "an to view communication between Macaulay2 server and browser ",
     Usage => "H = visualize (P, Verbose => true)",     
     Inputs => {"P" => Poset => "a poset"},

     PARA {"When this option is used, the user can view the communication between the Macaulay2 server and the browser.
	 The communication will be displayed in the instance of Macaulay2."},
	 
    SeeAlso => {
	 "Basic Workflow for Visualize",
	 (visualize,Graph),	 
	 (visualize,Digraph),	 	 
	 (visualize,Poset),	 	 
	 (visualize,SimplicialComplex),	 	 
	 (visualize,Ideal)
	}
	 
     }

document {
     Key => {openPort,(openPort,String)},
     Headline => "opens a port",
     Usage => "openPort N",
     Inputs => {"N" => String => "a port number"},
     
     PARA {"In order to use the ", TO "visualize", " method, the user must first open a port on their 
	 machine. This can be dangerous as someone could potentially be listening on that port. The port 
	 being open is only accessible to the localhost and is most vulnerable if the package is being
	 run on a server. If running on a personal computer/laptop, then the risk of cyber attack is 
	 very small as the attacker would have to have access to your local machine. Because of this risk, 
	 and the fact that the examples will throw errors if the user is using the example port, 
	 the documentation will not actually run examples of ", TT "openPort", "."},
	 
-- adding an example can cause installation to fail if user has port 8080 open
--     EXAMPLE {
--	 "openPort \"8080\"",
--	 },
     
     PARA {"The user can use any port number they wish. Common ports are '8080' and '8081'. For example, ",
     TT "openPort\"8080\"", " will open port '8080' for communication."},
     
     PARA {"Once a port is open, another instance of ", TO "openPort", " will not be allowed to open another port. 
	 Hence the user can only open one port at a time with this method. Once finished visualizing, you can close the 
	 port with the ", TO "closePort", " method."},
	 
     EXAMPLE {
	 "closePort()",
	 },	 

    PARA "Restarting Macaulay2 will also close the port.",
    
    SeeAlso => {
	 "Basic Workflow for Visualize",
	 (visualize,Graph),	 
	 (visualize,Digraph),	 	 
	 (visualize,Poset),	 	 
	 (visualize,SimplicialComplex),	 	 
	 (visualize,Ideal)
	}
    
     }


document {
     Key => (closePort),
     Headline => "closes and open port",
     
     PARA {"When a port is opened with the ", TO "openPort", " method, ", TT "closePort()", 
	 " is used to close the port. If the user forgets to close the port, closing or restarting
	 Macaulay2 will automatically close the port."},
     
      EXAMPLE {
--	 "openPort \"8080\"",
	 "closePort()"
	 },

    SeeAlso => {
	 "Basic Workflow for Visualize",
	 (visualize,Graph),	 
	 (visualize,Digraph),	 	 
	 (visualize,Poset),	 	 
	 (visualize,SimplicialComplex),	 	 
	 (visualize,Ideal)
	},
    
    Caveat => {"This method does not have an input. In order for it to run, you must write the '()' at the end as follows: ", TT "closePort()", "."}
    

     }


-------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------

end

-------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------


restart
uninstallPackage"Visualize"
restart
path = {"~/GitHub/Visualize-M2/"}|path
installPackage"Visualize"
viewHelp Visualize
viewHelp doc

-----------------------------
-----------------------------
-- Tests
-----------------------------
-----------------------------

-- brett

-- Graphs
restart
path = {"~/GitHub/Visualize-M2/"}|path
loadPackage"Graphs"
loadPackage"Visualize"
openPort"8080"
G = graph({{x_0,x_1},{x_0,x_3},{x_0,x_4},{x_1,x_3},{x_2,x_3}},Singletons => {x_5})
visualize G
G2 = cocktailParty 10
visualize G2

G = graph({},Singletons => {x_1,x_2})
visualize G

G1 = barbellGraph 6
visualize G1
G3 = barycenter completeGraph 6
visualize G3

-- Converting graphs with String vertices back to their values.
R = QQ[x]
V = {"1","0100","1/2","a","x"}

G = graph({{V#0,V#1},{V#1,V#0},{V#0,V#2},{V#1,V#3}},Singletons => {V#4})
G2 = value first toString G

D = digraph({{V#0,{V#0,V#1}},{V#1,{V#2,V#3}},{V#2,{V#3,V#4,V#2}}})
class last vertices D
D2 = value first toString D
class last vertices D2

P = poset({{V#0,V#1},{V#0,V#2},{V#0,V#3},{V#4,V#2},{V#2,V#3}})
apply(vertices P, i -> class i)
P2 = poset(value toString vertices P,value toString coveringRelations P)
apply(vertices P2, i -> class i)
areIsomorphic(P,P2)

-- Digraphs
restart
loadPackage"Graphs"
loadPackage"Visualize"
openPort"8080"
G = digraph({ {1,{2,3}} , {2,{3}} , {3,{1}}})
visualize G

D1 = digraph ({{a,{b,c,d,e}}, {b,{d,e}}, {e,{a}}}, EntryMode => "neighbors")
visualize D1
D2 = digraph {{1,{2,3}}, {2,{4,5}}, {3,{5,6}}, {4,{7}}, {5,{7}},{6,{7}},{7,{}}}
visualize D2

D = digraph({})
visualize D

-- Posets
restart
loadPackage"Posets"
loadPackage"Visualize"
openPort"8080"
P = divisorPoset(258)
P = divisorPoset(2)
visualize P

P = poset({1,2,3,4,5},{{1,2},{1,3},{2,4},{3,5},{4,5}})
visualize P

-- The following poset demonstrates a bug in the rankFunction method of the Posets package.
-- Vertex 4 covers vertex 1, but they are assigned the same rank by rankFunction.  In fact the poset is not ranked.
P = poset({0,1,2,3,4},{{0,2},{2,3},{1,4},{0,4},{1,3}})
isRanked P
vertices P
rankFunction P -- Notice that 1 and 4 both have rank 1.
coveringRelations P -- But 4 covers 1.
visualize P

-- Simplicial Complexes
restart
loadPackage"SimplicialComplexes"
loadPackage"Visualize"
openPort"8080"
R=ZZ[x_0..x_5]
C = simplicialComplex apply({{x_0, x_1, x_2}, {x_1, x_2, x_3}, {x_0, x_1, x_4}, {x_0, x_3, x_4}, {x_2, x_3, x_4}, {x_0, x_2, x_5}, {x_0, x_3, x_5}, {x_1, x_3, x_5}, {x_1, x_4, x_5}, {x_2, x_4, x_5}},face)
visualize C

R = ZZ[a..f]
visualize simplicialComplex monomialIdeal(a*f, b*d, c*e)

-- tom examples
-- Posets
restart
path = {"~/GitHub/Visualize-M2/"}|path
loadPackage "Visualize"
openPort "8081"
--P = poset {{abc,2}, {1,3}, {3,4}, {2,5}, {4,5}}
--visualize(P, Verbose=>true)
P2 = poset {{1,2},{2,3},{3,4},{5,6},{6,7},{3,6}}
visualize(P2,FixExtremeElements => true,Verbose=>true)
visualize P2
visualize(oo, Verbose=>true)

closePort()
-- ideals
restart
path = {"~/GitHub/Visualize-M2/"}|path
loadPackage "Visualize"
R=ZZ/101[x,y]
I=ideal(x^5,x^2*y,y^4)
visualize I


G=graph({})
visualize(G,Verbose=>true)
visualize G

R = QQ[x,y,z]
I = ideal(x*y^2,x^2*z,y*z^2)
P = lcmLattice I
visualize P

P = diamondProduct(chain 4, chain 4)
visualize P

-- Simplicial Complexes
restart
loadPackage "SimplicialComplexes"
loadPackage "Visualize"
openPort "8081"
R = ZZ[a..f]
D = simplicialComplex monomialIdeal(a*b*c,a*b*f,a*c*e,a*d*e,a*d*f,b*c*d,b*d*e,b*e*f,c*d*f,c*e*f)
visualize D
visualize(D,Verbose => true)

R = ZZ[a..g]
D2 = simplicialComplex {a*b*c,a*b*d,a*e*f,a*g}
visualize D2

R = ZZ[a..f]
L =simplicialComplex {d*e*f, b*e*f, c*d*f, b*c*f, a*d*e, a*b*e, a*c*d, a*b*c}
visualize L

----------------

-----------------------------
-----------------------------
-- Stable Tests
-----------------------------
-----------------------------

-- branden
-- (options Visualize).Configuration

--Graphs test
restart

loadPackage"Visualize"
openPort "8081"
G = graph({{0,1},{0,3},{0,4},{1,3},{2,3}},Singletons => {5})

installPackage"Visualize"
viewHelp Visualize

uninstallPackage"Visualize"
restart
path = path|{"~/GitHub/Visualize-M2/"}
loadPackage"Visualize"
openPort "8081"
G = graph({{0,1},{1,4},{2,4},{0,3},{0,4},{1,3},{2,3}},Singletons => {5})

H = visualize (G, Verbose => true)
K = spanningForest H
J = visualize K
closePort()

G = graph({{x_0,x_1},{x_0,x_3},{x_0,x_4},{x_1,x_3},{x_2,x_3}},Singletons => {x_5})
H = visualize ( G, VisPath => "/Users/bstone/Desktop/Test/",Verbose => true)
y
K = spanningForest H
J = visualize K

G = graph({{x_1, x_0}, {x_3, x_0}, {x_3, x_1}, {x_4, x_0}}, Singletons => {x_2, x_5, 6, cat_sandwich})
H = visualize (G, Verbose => true)
K = spanningForest H
J = visualize K

-- Digraphs
restart
loadPackage"Visualize"
openPort "8081"
G = digraph({ {1,{2,3}} , {2,{3}} , {3,{1}}})
visualize G

D1 = digraph ({{a,{b,c,d,e}}, {b,{d,e}}, {e,{a}}}, EntryMode => "neighbors")
visualize D1
D2 = digraph {{1,{2,3}}, {2,{4,5}}, {3,{5,6}}, {4,{7}}, {5,{7}},{6,{7}},{7,{}}}
visualize D2


closePort()

-- ideal tests
restart
loadPackage"Visualize"
openPort "8080"
R = QQ[a,b,c]
I = ideal"a2,ab,b2c,c5,b4"
-- I = ideal"x4,xyz3,yz,xz,z6,y5"
visualize I
visualize( I, VisPath => "/Users/bstone/Desktop/Test/", Warning => false)
visualize( I, VisPath => "/Users/bstone/Desktop/Test/")

S = QQ[x,y]
I = ideal"x4,xy3,y5"
visualize I
visualize( I, VisPath => "/Users/bstone/Desktop/Test/", Warning => false)
visualize( I, VisPath => "/Users/bstone/Desktop/Test/")
y
closePort()


-- Bug
visualize( G, VisPath => "/Users/bstone/Desktop/Test/")


-- Random Tests

copyTemplate(currentDirectory() | "Visualize/templates/visGraph/visGraph-template.html", "/Users/bstone/Desktop/Test/")



-----------------------------
-----------------------------
-- Demo
-----------------------------
-----------------------------

restart
uninstallPackage"Graphs"
restart
loadPackage"Graphs"
loadPackage"Visualize"

-- Old Graphs
G = graph({{x_0,x_1},{x_0,x_3},{x_0,x_4},{x_1,x_3},{x_2,x_3}},Singletons => {x_5})
visGraph G
H = graph({{Y,c},{1, 0}, {3, 0}, {3, 1}, {4, 0}}, Singletons => {A, x_5, 6, cat_sandwich})
visGraph H

restart
loadPackage"Graphs"
loadPackage"Visualize"
-- New Graphs
G = graph(toList(0..5),{{0,1},{0,3},{0,4},{1,3},{2,3}},Singletons => {5},EntryMode => "edges")
visGraph G
cycleGraph 9
visGraph oo
wheelGraph 8
visGraph oo
generalizedPetersenGraph(3,4)
visGraph oo
completeGraph(70)
visGraph oo
cocktailParty(70)
visGraph oo


R = QQ[a,b,c]
I = ideal"a2,ab,b2c,c5,b4"
I = ideal"x4,xyz3,yz,xz,z6,y5"
visIdeal I
copyJS "/Users/bstone/Desktop/Test/"
yes
visIdeal( I, VisPath => "/Users/bstone/Desktop/Test/")

S = QQ[x,y]
I = ideal"x4,xy3,y5"
visualize I
visualize( I, VisPath => "/Users/bstone/Desktop/Test/")
y

copyJS "/Users/bstone/Desktop/Test/"
yes




restart
uninstallPackage"Graphs"
loadPackage"Graphs"
peek Graphs
loadPackage"Visualize"

-- Creates staircase diagram 
-- 2 variables
S = QQ[x,y]
I = ideal"x4,xy3,y5"
visIdeal I

-- User can choose where to place files
visIdeal( I, VisPath => "/Users/bstone/Desktop/Test/")

-- 3 variables
R = QQ[x,y,z]
J = ideal"x4,xyz3,yz2,xz3,z6,y5"
visIdeal J
visIdeal( J, VisPath => "/Users/bstone/Desktop/Test/")

restart
needsPackage"Graphs"
loadPackage"Visualize"

-- we are also focusing on graphs
G = graph({{x_0,x_1},{x_0,x_3},{x_0,x_4},{x_1,x_3},{x_2,x_3}},Singletons => {x_5})
-- displayGraph A
visGraph G

M = 
A = graph M
visGraph A


-- Utah Demo

restart

uninstallPackage"Graphs"
path = path|{"~/GitHub/Visualize-M2/"}
path = {"~/GitHub/M2/M2/Macaulay2/packages/"}|path

needsPackage "Visualize"
openPort "8080"
closePort()
-- Graphs
G = graph({{0,1},{0,3},{0,4},{1,3},{2,3}},Singletons => {5})
isRigid G

visualize( G, Verbose => true )
visualize(G, VisPath => "/Users/bstone/Desktop/Test/", Warning => true)
y

cycleGraph 9
visualize oo
run"pwd"
wheelGraph 8
visualize oo

generalizedPetersenGraph(3,3)
visualize oo

completeGraph(70)
visualize oo

cocktailParty(70)
visualize oo


-- Digraphs
G = digraph({ {1,{2,3}} , {2,{3}} , {3,{1}}})
visualize G

D1 = digraph ({{a,{b,c,d,e}}, {b,{d,e}}, {e,{a}}}, EntryMode => "neighbors")
visualize D1

D2 = digraph {{1,{2,3}}, {2,{4,5}}, {3,{5,6}}, {4,{7}}, {5,{7}},{6,{7}},{7,{}}}
visualize (D2, VisPath => "/Users/bstone/Desktop/Test/", Warning => false)


-- Posets
restart
path = {"~/GitHub/Visualize-M2/"}|path
loadPackage "Visualize"
openPort "8081"
P2 = poset {{1,2},{2,3},{3,4},{5,6},{6,7},{3,6}}
visualize( P2, VisPath => "/Users/bstone/Desktop/Test/", Warning => false)
visualize(P2,FixExtremeElements => true)
visualize oo

-- Simplicial Complexes
R = ZZ[a..f]
D = simplicialComplex monomialIdeal(a*b*c,a*b*f,a*c*e,a*d*e,a*d*f,b*c*d,b*d*e,b*e*f,c*d*f,c*e*f)
visualize G

R = ZZ[a..g]
D2 = simplicialComplex {a*b*c,a*b*d,a*e*f,a*g}
visualize( D2, VisPath => "/Users/bstone/Desktop/Test/", Warning => false)

R = ZZ[a..f]
L =simplipcialComplex {d*e*f, b*e*f, c*d*f, b*c*f, a*d*e, a*b*e, a*c*d, a*b*c}
visualize L


-- Ideals
S = QQ[x,y]
I = ideal"x4,xy3,y5"
visualize I
visualize( I, VisPath => "/Users/bstone/Desktop/Test/", Warning => false)

viewHelp EdgeIdeals

R = QQ[x,y,z]
J = ideal"x4,xyz3,yz2,xz3,z6,y5"
visualize( J, VisPath => "/Users/bstone/Desktop/Test/", Warning => false)

closePort()

restart
uninstallPackage"Graphs"
installPackage"Graphs"
restart
path ={"~/GitHub/Visualize-M2/"}|path
needsPackage"Graphs"
?isRigid
uninstallPackage"Visualize"
restart
path ={"~/GitHub/Visualize-M2/"}|path
installPackage"Visualize"
viewHelp Visualize

///
restart
makeVisualizeFun = method()
makeVisualizeFun(Function) := g -> (
    return g a == b;
    )
a = "Hello"; b=a;

f := s -> s
userFunction f

m = method()
m(ZZ) := z -> z+1
userFunction m    

restart
path = {"~/GitHub/Visualize-M2/"}|path
loadPackage"Visualize"
///







-- Demo For the paper

restart
-- Workflow
loadPackage "Visualize"
openPort "8080"
openPort "8081"
closePort()
openPort "8081"

-- Case Study: Graphs
restart

needsPackage"Graphs"
G = graph({{1,2},{1,3},{2,3},{3,4}},Singletons=>{5})

loadPackage "Visualize"
openPort "8080"
visualize G


testvisualize = method(Options => true)
testvisualize(ZZ) := {VisPath => defaultPath, Warning => true, Verbose => false}|{VisTemplate => "basePath" | "Visualize/templates/visGraph/visGraph-template.html"} >> opts -> G -> (
    return G;
    )
testvisualize 5

-- fpsac paper

restart

path = path|{"~/GitHub/Visualize-M2/"}

needsPackage "Visualize"
openPort "8080"
openPort "8081"

P = partitionLattice 3
visualize P

H = hasseDiagram P
visualize H

H = K

G = vertices H
R = edges H

Q = poset(G,R, AntisymmetryStrategy => "digraph") -- Correct poset, but ground set has changed. 
visualize Q

closePort()

-- Examples for the M2 documentation --
restart
path = {"~/GitHub/Visualize-M2/"}|path
loadPackage"Graphs"
loadPackage"Visualize"
openPort"8080"
ExamplePath = "~/Desktop/vis_examples/"

-- Graph example
G = graph({{0,1},{0,3},{0,4},{1,3},{2,3}},Singletons => {5})
visualize(G)

-- Digraph example
D = digraph {{1,{2,3}}, {2,{4,5}}, {3,{5,6}}, {4,{7}}, {5,{7}},{6,{7}},{7,{}}}
visualize(D)

-- Poset example
P = poset {{1,2},{2,3},{3,4},{5,6},{6,7},{3,6}}
visualize(P)

-- Simplicial complex example
R = ZZ[a..g]
D = simplicialComplex {a*b*c,a*b*d,a*e*f,a*g}
visualize(D)

-- Monomial ideal staircase diagram 2d example
S = QQ[x,y]
I = ideal"x4,xy3,y5"
visualize(I)


-- Monomial ideal staircase diagram 3d example
R = QQ[x,y,z]
J = ideal"x4,xyz3,yz2,xz3,z6,y5"
visualize(J)
