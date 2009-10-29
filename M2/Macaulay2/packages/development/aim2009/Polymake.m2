needsPackage "Polyhedra"

newPackage(
	"Polymake",
    	Version => "0.2", 
    	Date => "October 28, 2009",
    	Authors => {{Name => "Josephine Yu", 
		  Email => "josephine.yu@gmail.com", 
		  HomePage => "http://www.math.mit.edu/~jyu/"}},
    	Headline => "a package for interfacing with polymake",
    	DebuggingMode => true
    	)

export {PolymakeObject, polymakeObject, toPolymakeFormat, writePolymakeFile, runPolymake,getPropertyNames, getProperty, getMatrixProperty, getListProperty, getVectorProperty, makevec, makemat }


---------------------------------------------------------------------------
-- Code
---------------------------------------------------------------------------

needsPackage "Polyhedra"
needsPackage "SimpleDoc"


PolymakeObject = new Type of MutableHashTable

polymakeObject = method()
polymakeObject(String, List) := (filename, properties) -> (
 --    F := removeComments lines get filename;
     new PolymakeObject from apply(properties, p -> p => getMatrixProperty(filename,p))
     )
polymakeObject(String) := (filename) -> (    
 --    F := removeComments lines get filename;
 --    properties = getPropertyNames(F);
     properties = getPropertyNames(filename);
     new PolymakeObject from apply(properties, p -> p => getMatrixProperty(filename,p))
     )

---------------------------------------------------------------------------
-- toPolymakeFormat

toPolymakeFormat = method(TypicalValue => String)
toPolymakeFormat(String, Matrix) := (propertyname, M) -> (
     if M === null then ""
     else(
     	  S = propertyname|"\n";
     	  if numRows M > 0 then
	     S = S|replace("\\|", "", toString net M);
     	  S
     	  )
     )
toPolymakeFormat(String,Vector) := (propertyname,V) -> (
     if V === null then ""
     else(
     	  S = propertyname|"\n";
     	  if length L > 0 then
              S = S|replace("\\|", "", toString net matrix{L});     
     	  S
     	  )
     )
toPolymakeFormat(String,ZZ) := (propertyname,x) -> (
     if x === null then ""
     else(
     	  S = propertyname|"\n"|x|"\n";
     	  S
     	  )
     )
toPolymakeFormat(PolymakeObject) := (P) -> (
     concatenate apply(pairs P, a -> toPolymakeFormat(a)|"\n\n")
     )
toPolymakeFormat(Polyhedron) := (P) -> (
     if P === null then ""
     else(
	  S = "";
	  S = S|toPolymakeFormat("DIM", P#"dimension of polyhedron")|"\n\n";
	  if(P#"dimension of lineality space" == 0) then (
	      S = S|toPolymakeFormat("VERTICES", transpose(P#"homogenizedVertices"#0))|"\n\n";
	      S = S|toPolymakeFormat("N_VERTICES", P#"number of rays" + P#"number of vertices")|"\n\n";
	      );
	  S = S|toPolymakeFormat("INEQUALITIES", transpose(P#"homogenizedHalfspaces"#0))|"\n\n";
	  S = S|toPolymakeFormat("EQUATIONS", transpose(P#"homogenizedHalfspaces"#1))|"\n\n";
     	  S
	  )
     )

---------------------------------------------------------------------------
-------- Create polymake input files from a Polyhedron

writePolymakeFile = method(TypicalValue => String)
writePolymakeFile(Polyhedron,String) := (P, filename) ->(
     filename << toPolymakeFormat(P) << endl << close;
     filename	  
     )
writePolymakeFile(Polyhedron) := (P) ->(
     filename := temporaryFileName()|currentTime()|".poly";
     << "using temporary file " << filename << endl;
     filename << toPolymakeFormat(P) << endl << close;
     filename	  
     )

---------------------------------------------------------------------------
----- Run Polymake

runPolymake = method(TypicalValue => String)
runPolymake(String,String) := (filename, propertyname)->(
     get("!"|"polymake "|filename|" "|propertyname)
     )
runPolymake(PolymakeObject, String) := (P, propertyname) -> (
     filename := temporaryFileName()|currentTime()|".poly";
     << "using temporary file " << filename << endl;
     filename << toPolymakeFormat(P) << endl << close;
     runPolymake(filename, propertyname)
     )
runPolymake(Polyhedron, String) := (P, propertyname) -> (
     filename := temporaryFileName()|currentTime()|".poly";
     << "using temporary file " << filename << endl;
     filename << toPolymakeFormat(P) << endl << close;
     runPolymake(filename, propertyname)
     )

---------------------------------------------------------------------------
----- Utilities

isBlankLine = (s) -> (
     #s == 0 or
     match("^[[:space:]]*$",s) or
     match("^[[:space:]]*#",s)
     )

makevec = (str) -> (
     t := separateRegexp(" +", removeComment str);
     t = apply(t,value);
     select(t, x-> x =!= null)
     )

makemat = (L) -> (
     if #L == 0 then map(QQ^0,QQ^0,0)
     else matrix(L/makevec)
     )

makelist = (L) -> (
     if #L == 0 then {}
     else apply(removeComments(L), t-> value replace(" ", ",", t)) 
     )

removeComments = (F) -> ( 
     -- F is a list of lines
     -- removes comments beginning with #
     -- also removes trailing blank spaces
		 if F === null then return null
		 else
     apply(F, s-> replace("[[:space:]]*#.*|[[:space:]]+$","" ,s))
     )

removeComment = (str) -> ( 
     -- F is a list of lines
     -- removes comments beginning with #
     -- also removes trailing blank spaces
     replace("[[:space:]]*#.*|[[:space:]]+$","" ,str)
     )

------------------------------------------------------------------------------
-------  Getting Properties of Polyhedra using Polymake

-- This is for Polymake version 2
      -- returns a list of strings, names of known properties
getPropertyNames = method(TypicalValue => String)
getPropertyNames(List) := (F) -> (
      -- F is a list of lines, as from a polymake file
      nameLines = select(F, s-> match("property name", s) );
      apply(nameLines, s->  
	   replace( "\".*$"  , "" ,replace("^.*property name=\"", "", s))
	   )
     )
getPropertyNames(String) := (filename) -> (
      -- filename is the name of a polymake file (XML format)
      F = lines get filename;
      nameLines = select(F, s-> match("property name", s) );
      apply(nameLines, s->  
	   replace( "\".*$"  , "" ,replace("^.*property name=\"", "", s))
	   )
     )

----- Old Version -----
--getPropertyNames(List) := (F) -> (
      -- F is a list of lines, as from a polymake file
      -- returns a list of strings, names of known properties
  --    FF = removeComments(F);
  --    PP = select(FF, s-> match("^[[:space:]]*[[:alpha:]]", s));
  --    apply(PP, s -> toString s)    
  -- )

------------------------------------------------------------------------------

getProperty = method(TypicalValue => List)
getProperty(List, String) := (F, section) -> (
     -- F is a list of lines, as from a polymake file
     n := position(F, s -> match("^"|section|///[:space:]*$///,s) or
	   match("^"|section|///[:space:]*#///,s));
     if(n =!= null) then
     ( 
        p := n+1;
				if isBlankLine F_p then {} 
				else
       	  while p < #F and not isBlankLine F_p  list (p=p+1; F_(p-1))
     )
     else null
     ) 
getProperty(String,String) := (filename, propertyname) -> (
     F := lines runPolymake(filename, propertyname);
     getProperty(F, propertyname)
     )
getProperty(Polyhedron,String) := (P, propertyname) -> (
     F := lines runPolymake(P, propertyname);
     getProperty(F, propertyname)
     )

---------------------------------------------------------------------------

getMatrixProperty = method(TypicalValue => Matrix)
getMatrixProperty(List, String) := (F, section) -> (
     L := getProperty(F,section);
     if L === null then null
     else makemat L
     )
getMatrixProperty(String,String) := (filename, propertyname) -> (
     F := lines runPolymake(filename, propertyname);
     getMatrixProperty(F, propertyname)
     )
getMatrixProperty(Polyhedron,String) := (P, propertyname) -> (
     F := lines runPolymake(P, propertyname);
     getMatrixProperty(F, propertyname)
     )

---------------------------------------------------------------------------


getListProperty = method(TypicalValue => List)
getListProperty(List, String) := (F, section) -> (
		 L := getProperty(F,section);
		 if L === null then null
	   else makelist L
		 )
getListProperty(String,String) := (filename, propertyname) -> (
     F := lines runPolymake(filename, propertyname);
     getListProperty(F, propertyname)
     )
getListProperty(Polyhedron,String) := (P, propertyname) -> (
     F := lines runPolymake(P, propertyname);
     getListProperty(F, propertyname)
     )

---------------------------------------------------------------------------


getVectorProperty = method(TypicalValue => List)
getVectorProperty(List, String) := (F, section) -> (
     L := getProperty(F,section);
     if L === null then null
     else if #L > 1 then error "property is not a vector"
     else if #L == 0 then {} 
     else makevec L_0
     )
getVectorProperty(String,String) := (filename, propertyname) -> (
     F := lines runPolymake(filename, propertyname);
     getVectorProperty(F, propertyname)
     )
getVectorProperty(Polyhedron,String) := (P, propertyname) -> (
     F := lines runPolymake(P, propertyname);
     getVectorProperty(F, propertyname)
     )




---------------------------------------------------------------------------
-----------------------DOCUMENTATION----------------------
---------------------------------------------------------------------------

beginDocumentation()

doc ///
  Key
    Polymake
  Headline
    Interface for Polymake
  Description
   Text 
     {\tt Polymake} is a software for convex polyhedra, simplicial complexes, and other discrete geometric objects, written by Ewgenij Gawrilow and Michael Joswig.  It is available at @HREF "http://www.math.tu-berlin.de/polymake/"@. The user should have {\tt Polymake} installed on their machine.
   Text 
     Warning: this package is not complete, and is mostly undocumented, but it is used in  @TO "gfanInterface::gfanInterface"@. 
   Text
     We can use the interface to get properties of @TO "Polyhedra::Polyhedron"@
   Example
     P = cyclicPolytope(3,5)
     getVectorProperty(P, "F_VECTOR")
     getMatrixProperty(P, "VERTEX_NORMALS")
   Text
     Instead of creating a new temporary file every time, we can reuse one and also save computation time.
   Example
     polyFile = writePolymakeFile(P)
     runPolymake(polyFile, "F_VECTOR")
     runPolymake(polyFile, "VERTEX_NORMALS")
   Text
     Look in the polymake file for the properties already computed.
   Example
     getPropertyNames(polyFile)
     runPolymake(polyFile, "VERTICES_IN_FACETS")
   Text
     We can save the {\tt Polymake} output as a vector, a matrix or a list of things.
   Example
     getVectorProperty(polyFile, "VERTEX_DEGREES")
     getMatrixProperty(polyFile, "VERTEX_NORMALS")
     getListProperty(polyFile, "VERTICES_IN_FACETS")
     getProperty(polyFile, "SIMPLICIAL")
     removeFile(polyFile)
  Caveat
     You may need to manually remove the temporary files created in {\tt /tmp}.
  SeeAlso
     Polyhedra
///;


doc ///
  Key
    PolymakeObject
  Headline

  Usage

  Inputs

  Outputs

  Consequences

  Description
   Text
   Text
   Example
   Text
   Example
  Caveat
  SeeAlso
///


doc ///
  Key
    polymakeObject
    (polymakeObject,String)
    (polymakeObject,String,List)
  Headline
    Makes PolymakeObject either from the name of a polymake file, or the file name and a list of properties.
  Usage

  Inputs

  Outputs

  Consequences

  Description
   Text
   Text
   Example
   Text
   Example
  Caveat
  SeeAlso
///



doc ///
  Key
    getPropertyNames
    (getPropertyNames, String)
    (getPropertyNames, List)
  Headline
     Get the list of known properties in a polymake file or a list containing lines of a polymake (xml) format file.
  Usage

  Inputs

  Outputs

  Consequences

  Description
   Text
   Text
   Example
   Text
   Example
  Caveat
  SeeAlso
///


doc ///
  Key
    getListProperty
    (getListProperty, List,String) 
    (getListProperty, Polyhedron,String)
    (getListProperty, String,String)
  Headline
    get proprety of a polyhedron as a list
  Usage
    getListProperty(dataLines, property)
    getListProperty(P, property)
    getListProperty(polyFileName, property)
  Inputs
    dataLines: List
      list of strings, typically output of polymake
    property: String
      name of polymake property
    polyFileName: String
      file name of {\tt Polymake} file
    P: Polyhedron
  Outputs
    L:List
      whose entries are values of the strings in dataLines or output of , with {\tt Polymake} comments removed.
  Consequences

  Description
   Text
   Text
   Example
     P = cyclicPolytope(3,5)
     getListProperty(P, "VERTICES_IN_FACETS")
   Text
   Example
  Caveat
    A temporary file may get created.
  SeeAlso
///


doc ///
  Key
    getMatrixProperty
    (getMatrixProperty,String,String) 
    (getMatrixProperty,Polyhedron,String)
    (getMatrixProperty,List,String)
  Headline
    Get a property as a matrix, given a @TO Polyhedra::Polyhedron@, a polymake file name, or a list of lines in a polymake output.
  Usage

  Inputs

  Outputs

  Consequences

  Description
   Text
   Text
   Example
   Text
   Example
  Caveat
  SeeAlso
///

doc ///
  Key
    getProperty
    (getProperty,List,String)
    (getProperty,Polyhedron,String)
    (getProperty,String,String)
  Headline

  Usage

  Inputs

  Outputs

  Consequences

  Description
   Text
   Text
   Example
   Text
   Example
  Caveat
  SeeAlso
///


doc ///
  Key
    getVectorProperty
    (getVectorProperty,List,String)
    (getVectorProperty,Polyhedron,String) 
    (getVectorProperty,String,String)
  Headline

  Usage

  Inputs

  Outputs

  Consequences

  Description
   Text
   Text
   Example
   Text
   Example
  Caveat
  SeeAlso
///

doc ///
  Key
    makemat
  Headline

  Usage

  Inputs

  Outputs

  Consequences

  Description
   Text
   Text
   Example
   Text
   Example
  Caveat
  SeeAlso
///

doc ///
  Key
    makevec
  Headline

  Usage

  Inputs

  Outputs

  Consequences

  Description
   Text
   Text
   Example
   Text
   Example
  Caveat
  SeeAlso
///

doc ///
  Key
    runPolymake
    (runPolymake,Polyhedron,String) 
    (runPolymake,PolymakeObject,String) 
    (runPolymake,String,String)
  Headline

  Usage

  Inputs

  Outputs

  Consequences

  Description
   Text
   Text
   Example
   Text
   Example
  Caveat
  SeeAlso
///

doc ///
  Key
    toPolymakeFormat
    (toPolymakeFormat,Polyhedron) 
    (toPolymakeFormat,PolymakeObject)
    (toPolymakeFormat,String,Matrix)
    (toPolymakeFormat,String,Vector)
    (toPolymakeFormat,String,ZZ) 
  Headline

  Usage

  Inputs

  Outputs

  Consequences

  Description
   Text
   Text
   Example
   Text
   Example
  Caveat
  SeeAlso
///

doc ///
  Key
    writePolymakeFile
    (writePolymakeFile,Polyhedron)
    (writePolymakeFile,Polyhedron,String) 
  Headline

  Usage

  Inputs

  Outputs

  Consequences

  Description
   Text
   Text
   Example
   Text
   Example
  Caveat
  SeeAlso
///

---------------------------------------------------------------------------
------------------------- TEST ---------------------------
---------------------------------------------------------------------------


TEST ///

///

end

---------------------------------------------------------------------------
------------------------- END ---------------------------
---------------------------------------------------------------------------

restart
installPackage("Polymake", FileName => "~/Documents/math/M2codes/packageRepository/development/aim2009/Polymake.m2", RemakeAllDocumentation => true)
viewHelp(Polymake)

P = cyclicPolytope(3,5)
Vertices = transpose (matrix {{1, 0, 0, 0}, {1, 1, 1, 1}, {1, 2, 4, 8}, {1, 3, 9, 27}, {1, 4,16, 64}})
Rays = transpose(matrix{{1,2,3,4},{-1,-2,-3,-4}})
P2 = convexHull(Vertices,Rays)
Rays = transpose(matrix{{1,2,3,4},{1,-2,-3,-4}})
P3 = convexHull(Vertices,Rays)
P4 = convexHull(transpose (matrix {{1, 0, 0, -1}, {1, -1,-1,1}, {1, 2, 4, -7}, {1, -10, 9, 0}, {8, 4,-16, 4}}))

runPolymake(P, "FACETS")
filename = writePolymakeFile(P)
runPolymake(filename, "PROPERTY_NAMES")

getProperty(filename, "AMBIENT_DIM")
getProperty(filename, "VERTICES")
getMatrixProperty(filename, "VERTICES")
getVectorProperty(filename, "F_VECTOR")
getProperty(filename, "F_VECTOR")



------------------ TO DO ---------------------
get other properties such as lists, graphs, simplicial complexes
if can't process, just read as list of strings
