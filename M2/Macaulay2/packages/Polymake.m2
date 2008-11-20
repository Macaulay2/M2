-- -*- coding: utf-8 -*-
newPackage(
	"Polymake",
    	Version => "0.1", 
    	Date => "March 20, 2008",
    	Authors => {{Name => "Josephine Yu", 
		  Email => "jyu@math.mit.edu", 
		  HomePage => "http://www.math.mit.edu/~jyu/"}},
    	Headline => "a package for interfacing with polymake",
    	DebuggingMode => true
    	)

export {PolymakeObject, polymakeObject,
     getProperty, getMatrixProperty, 
     getVectorProperty, makevec, makemat}

PolymakeObject = new Type of MutableHashTable

isBlankLine = (s) -> (
     #s == 0
     )

makevec = (str) -> (
     t := separateRegexp(" +", str);
     t = apply(t,value);
     select(t, x-> x =!= null)
     )

makemat = (L) -> (
     if #L == 0 then map(ZZ^0,ZZ^0,0)
     else matrix(L/makevec)
     )

getProperty = method()
getProperty(List, String) := (F, section) -> (
     -- F is a list of lines, as from a polymake file
     n := position(F, s -> match("^"|section|///[^_[:alpha:]]*///, s));
     p := n+1;
     while p < #F and not isBlankLine F_p  list (p=p+1; F_(p-1))
     )

getMatrixProperty = method()
getMatrixProperty(List, String) := (F, section) -> makemat getProperty(F,section)

getVectorProperty = method()
getVectorProperty(List, String) := (F, section) -> (
     L := getProperty(F,section);
     if #L > 1 then error "property is not a vector";
     if #L == 0 
       then {} 
       else makevec L_0
     )

polymakeObject = method()
polymakeObject(String, List) := (filename, properties) -> (
     F := lines get filename;
     new PolymakeObject from apply(properties, p -> p => getMatrixProperty(F,p))
     )

beginDocumentation()
document { 
	Key => Polymake,
	Headline => "a package for interfacing with polymake",
	EM "Polymake", " is a package for interfacing wiht polymake, and
	for reading and writing files in polymake's format."
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
F = lines get 

getProperty(F, "VERTICES")
getMatrixProperty(F, "VERTICES")
polymakeObject("Polymake/a.poly", {"VERTICES","FACETS"})

peek oo
matrix apply(oo, s -> makevec s)
select(L, s -> match(///^[[:alpha:]]///,s))
viewHelp regex

map(ZZ^0,ZZ^0,0)
