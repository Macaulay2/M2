newPackage(
	"Polymake",
    	Version => "0.1", 
    	Date => "March 21, 2008",
    	Authors => {{Name => "Josephine Yu", 
		  Email => "jyu@math.mit.edu", 
		  HomePage => "http://www.math.mit.edu/~jyu/"}},
    	Headline => "interfacing with polymake",
	Keywords => {"Interfaces"},
    	DebuggingMode => false
    	)

export {"PolymakeObject", "polymakeObject",
     "removeComments",
     "getPropertyNames",
     "getProperty", "getMatrixProperty", "getListProperty",
     "getVectorProperty", "makevec", "makemat", 
     "toPolymakeFormat", "runPolymake", "NewtonPolytope"}

PolymakeObject = new Type of MutableHashTable

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
     if #L == 0 then map(ZZ^0,ZZ^0,0)
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

getPropertyNames = method()
getPropertyNames(List) := (F) -> (
      -- F is a list of lines, as from a polymake file
      -- returns a list of strings, names of known properties
      FF := removeComments(F);
      PP := select(FF, s-> match("^[[:space:]]*[[:alpha:]]", s));
      apply(PP, s -> toString s)
      )     
 
getProperty = method()
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
     F := lines get filename;
     getProperty(F, propertyname)
     )

getMatrixProperty = method()
getMatrixProperty(List, String) := (F, section) -> (
     L := getProperty(F,section);
     if L === null then null
     else makemat L
     )
getMatrixProperty(String,String) := (filename, propertyname) -> (
     F := lines get filename;
     getMatrixProperty(F, propertyname)
     )

getListProperty = method()
getListProperty(List, String) := (F, section) -> (
		 L := getProperty(F,section);
		 if L === null then null
	   else makelist L
		 )
getListProperty(String,String) := (filename, propertyname) -> (
     F := lines get filename;
     getListProperty(F, propertyname)
     )


getVectorProperty = method()
getVectorProperty(List, String) := (F, section) -> (
     L := getProperty(F,section);
     if L === null then null
     else if #L > 1 then error "property is not a vector"
     else if #L == 0 then {} 
     else makevec L_0
     )
getVectorProperty(String,String) := (filename, propertyname) -> (
     F := lines get filename;
     getVectorProperty(F, propertyname)
     )


polymakeObject = method()
polymakeObject(String, List) := (filename, properties) -> (
     F := removeComments lines get filename;
     new PolymakeObject from apply(properties, p -> p => getMatrixProperty(F,p))
     )
polymakeObject(String) := (filename) -> (    
     F := removeComments lines get filename;
     properties := getPropertyNames(F);
     new PolymakeObject from apply(properties, p -> p => getMatrixProperty(F,p))
     )

toPolymakeFormat = method()
toPolymakeFormat(String, Matrix) := (propertyname, M) -> (
     if M === null then ""
     else(
     	  S := propertyname|"\n";
     	  if numRows M > 0 then
	     S = S|replace("\\|", "", toString net M);
     	  S
     )
)

toPolymakeFormat(String,Vector) := (propertyname,V) -> (
     if V === null then ""
     else(
          L := entries V;
     	  S := propertyname|"\n";
     	  if length L > 0 then
              S = S|replace("\\|", "", toString net matrix{L});     
     	  S
     )
)
toPolymakeFormat(PolymakeObject) := (P) -> (
     concatenate apply(pairs P, a -> toPolymakeFormat(a)|"\n\n")
     )

runPolymake = method()
runPolymake(String,String) := (filename, propertyname)->(
     ex := "polymake "|filename|" "|propertyname;
     run ex
     )
runPolymake(PolymakeObject, String) := (P, propertyname) -> (
     filename := temporaryFileName();
     << "using temporary file " << filename << endl;
     filename << toPolymakeFormat(P) << endl << close;
     runPolymake(filename, propertyname)
     )

NewtonPolytope = method()
NewtonPolytope(RingElement) := (f) -> (
     new PolymakeObject from {"POINTS" => matrix apply(listForm f, m ->  {1}|m#0)}
     )

-----------------------DOCUMENTATION----------------------

beginDocumentation()
document { 
	Key => Polymake,
	Headline => "a package for interfacing with polymake",
	EM "Polymake", " is a package for interfacing with polymake, and
	for reading and writing files in polymake's format.",
	PARA{},
	"Warning: this package is not complete, and is mostly undocumented, but it is used in ", TO "gfanInterface::gfanInterface", ".
	It is expected that the interface will change in the (near) future."
	}
end


document {
	Key => {(firstFunction,ZZ),firstFunction},
	Headline => "a silly first function",
	Usage => "firstFunction n",
	Inputs => { "n" },
	Outputs => {{ "a silly string, depending on the value of ", TT "n" }},
        SourceCode => {(firstFunction,ZZ)},
	EXAMPLE lines ///
	   firstFunction 1
	   firstFunction 0
     	///
	}
   
document {
	Key => {(firstFunction,ZZ),firstFunction},
	Headline => "a silly first function",
	Usage => "firstFunction n",
	Inputs => { "n" },
	Outputs => {{ "a silly string, depending on the value of ", TT "n" }},
        SourceCode => {(firstFunction,ZZ)},
	EXAMPLE lines ///
	   firstFunction 1
	   firstFunction 0
     	///
	}

document {
	Key => {(firstFunction,ZZ),firstFunction},
	Headline => "a silly first function",
	Usage => "firstFunction n",
	Inputs => { "n" },
	Outputs => {{ "a silly string, depending on the value of ", TT "n" }},
        SourceCode => {(firstFunction,ZZ)},
	EXAMPLE lines ///
	   firstFunction 1
	   firstFunction 0
     	///
	}

------------------------- TEST ---------------------------

TEST ///
    assert ( firstFunction 2 == "D'oh!" )
///

readMat = method(TypicalValue => Matrix)
readMat(String,Ring) := (filename,R) -> (
     ss := select(lines get filename, s -> length s > 0);
     matrix(R, apply(ss, s -> (t := separateRegexp(" +", s); 
                 t = apply(t,value);
                     select(t, x -> class x =!= Nothing))))
)

firstFunction = method(TypicalValue => String)
firstFunction ZZ := String => n -> if n == 1 then "Hello World!" else "D'oh!"

restart
loadPackage "Polymake"
filename =  "/Users/bb/Desktop/3cube.poly";
F = lines get "/Users/bb/Desktop/3cube.poly";

L := getPropertyNames(F)
class L_0
P = polymakeObject(filename);
peek P
getProperty(F, "AMBIENT_DIM")

getProperty(filename, "VERTICES")
getProperty(F, "FACETS")
getProperty(F, "AFFINE_HULL")
getMatrixProperty(F, "VERTICES")
facets = getMatrixProperty(F, "FACETS")
AH = getMatrixProperty(F, "AFFINE_HULL")
fv = getVectorProperty(F, "F_VECTOR")

P = polymakeObject("/Users/bb/Desktop/3cube.poly", {"VERTICES","DIM","FACETS"})
P = polymakeObject(filename, {"VERTICES","DIM","FACETS"})
P = polymakeObject("/Users/bb/Desktop/3cube.poly", {"AMBIENT_DIM"})
peek oo
peek P
toPolymakeFormat(P)

"/Users/bb/Desktop/temp" << toPolymake("FACETS", facets) <<endl<< close;

runPolymake("/Users/bb/Desktop/3cube.poly", "FACETS")
runPolymake(P, "F_VECTOR");
FF = lines get "/tmp/M2-799-1";
getVectorProperty(FF,"F_VECTOR")


viewHelp listForm

ZZ[x,y,z]
f = (1-x)*(1-y)*(1-z)
P = NewtonPolytope f
peek P
runPolymake(P,"FACETS");
getMatrixProperty("/tmp/M2-825-7","VERTICES")


------------------ TO DO ---------------------
get other properties such as lists, graphs, simplicial complexes
if can't process, just read as list of strings
