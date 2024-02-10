newPackage(
        "JuliaInterface",
        Version => "0.1", 
        Date => "July 10, 2018",
        Authors => {
	    {Name => "Tim Duff", 
	     Email => "tduff3@gatech.edu", 
             HomePage => "http://people.math.gatech.edu/~tduff3/"}
	      },
        Headline => "Interfacing Macaulay2 with Julia Homotopy Continuation",
	PackageImports => {},
	PackageExports => {"NumericalAlgebraicGeometry","MonodromySolver"},
        DebuggingMode => true
        )


export{"JuliaProcess", "solveJulia", "restartJulia","importJulia","writeJuliaFile",
    -- exported options below
    "IncludeTemplate","ImportList","WithImports","OnlyReal","OnlyFinite","TemplateText"
    }


----------------------------------------
---GLOBAL VARIABLES / INTERFACE SETUP
----------------------------------------

importJulia = (importList,f) -> f<<concatenate apply(importList,pkg->pkg | "\n")

juliaBinary="/home/tim/julia-d55cadc350/bin/julia"
JuliaProcess=openInOut("!" |juliaBinary)
noProcessError="JuliaProcess not currently open"
importJulia({"using HomotopyContinuation","import DynamicPolynomials: PolyVar"},JuliaProcess)

---------------------
--- PACKAGE IMPORTS
---------------------



--------------
---UNEXPORTED-
--------------

numberRegex="([0-9]|| |-|\\.|e|i|m|\\+|,)*" -- potentially useful

parsePolynomial = p -> replace("ii","im",replace("p[0-9]+","0",toExternalString p))

-- the implementation here may need to change as the output of HomotopyContinuation evolves in future versions
parseSolutions = method(Options=>{OnlyReal=>false})
parseSolutions String := o -> out -> (
    outLines:=select("\\*.*",out);
    M:=new MutableList from {};
    stride:=12;
    numSols:=sub((length outLines)/stride,ZZ);
    for i from 0 to numSols-1 do (
    	s:=first select("\\[([0-9]|| |-|\\.|e|i|m|\\+|,)*\\]",outLines#(1+i*stride));
    	M#i=point{apply(separate(",",replace("\\[|\\]","",replace("im","*ii",s))),x->value x)};
	if match("nonsingular",outLines#(2+i*stride)) then M#i.SolutionStatus = Regular else M#i.SolutionStatus = Singular;
	);
    toList M
    )

--todo: write exported wrapper for first two signatures
writeSys = method(Options=>{IncludeTemplate=>false,WithImports=>false,
	                    ImportList=>{"using HomotopyContinuation",
				"import DynamicPolynomials: PolyVar"},
			    TemplateText=>false})
writeSys (PolySystem, File) := o -> (P,f) -> (
    R:=ring P;
    varString:=apply(gens R,g->(toString g));
    varCommas:=(P.NumberOfVariables-1):", ";
    eqnCommas:=(P.NumberOfPolys-1):", ";
    if o.WithImports then importJulia(o.ImportList,f);
    if o.TemplateText then f << "# We declare the variables of our polynomial system\n";
    f << concatenate(mingle(varString,varCommas))| " = " | "[PolyVar{true}(i) for i in [" | concatenate mingle(apply(varString,g-> "\"" | g | "\""),varCommas)|"]];\n";
    if o.TemplateText then f << "# We define the polynomial system in which solutions we are interested.\n";
    f <<"F = [" | concatenate mingle(
	apply(equations P,e->parsePolynomial e),eqnCommas)| "];\n";
    if o.TemplateText then (
	f<<"# Now we can solve the system\n";
	);
    )
writeSys (PolySystem, String) :=o -> (P,filename) -> (
    f := openOut filename;
    writeSys(P,f);
    close f;
    )
writeSys PolySystem := o -> P -> (
    if not isOpen JuliaProcess then error("JuliaProcess not open");
    writeSys(P,JuliaProcess)
    )


writeJuliaFile = method(Options=>{})
writeJuliaFile (PolySystem,String) := o -> (P,filename) -> (
    f := openOut(filename|".jl");
    f << read openIn "./jlHeader.jl";
    writeSys(P,f,TemplateText=>true);
    f << "result = solve(F)\n";
    f << read openIn"./jlTail.jl";
    close f;
    )    
    

-------------------------------------
--- EXPORTED FUNCTIONS AND METHODS---
-------------------------------------

restartJulia = () -> (
    if (isOpen JuliaProcess) then close JuliaProcess;
    JuliaProcess=openInOut("!" |juliaBinary);
    --needsPackage("JuliaInterface",Reload=>true)
    )


TEST ///
JuliaProcess
close JuliaProcess
assert(not isOpen JuliaProcess)
restartJulia()
assert(isOpen JuliaProcess)
///

-- todo: replace arbitrary variable name "f" for julia system
-- todo: remove bottom two lines from print
-- todo: parse other solutions info
-- todo: finite solutions only
-- todo: check all variables are used
solveJulia = method(Options=>{})
solveJulia PolySystem := o -> P -> (
    if not isOpen JuliaProcess then error(noProcessError);
--    print "Wait for system to be solved:\n";
    writeSys P;
    JuliaProcess<<flush;
    x:=read JuliaProcess;
    JuliaProcess<<"result=solve(F);\n";
--    JuliaProcess<<"x=[s.solution for s in sols];\n";
    JuliaProcess<<"show(IOContext(STDOUT, :compact=>false),[s.solution for s in result])\n";
    finished:="done!!!x";
    JuliaProcess<<"print(\"" | finished |"\")\n";
    out:="";
    cur:="";
    while not match(finished,cur) do (
	JuliaProcess<<flush;
	cur = read JuliaProcess;
    	out = out | cur;	
	);
    endIndex:=regex("Array\\{Complex",out);
    parseSolutions substring(out,0,first first endIndex)
    )

------------------------------------------
------------------------------------------
-- Documentation
------------------------------------------
------------------------------------------

beginDocumentation()


doc ///
    Key
        solveJulia
        (solveJulia, PolySystem)
    Headline
        interface to "solve" function
    Usage
        sols = solve P
        sols = solve L
    Inputs
        P:PolySystem
        L:List
	    containing elements from a common @TO Ring @
    Outputs
        sols:List
            each element of class @TO AbstractPoint @
    Description
        Text
            This is the basic interface to the @HREF("https://www.juliahomotopycontinuation.org/HomotopyContinuation.jl/latest/solving.html","\"solve function")@.
        Example
	    R=CC[x,y]
            importJulia({"using HomotopyContinuation","import DynamicPolynomials: PolyVar"},JuliaProcess)
	    f= x^2+y
	    g=y^2-pi*ii
	    sols=solveJulia polySystem {f,g}
///



end



uninstallPackage packageName
close JuliaProcess
restart
packageName = "JuliaInterface"
path=append(path,currentDirectory())
installPackage("JuliaInterface",MakeDocumentation=>false)

R=CC[x,y]
f= x^2+y
g=y^2-pi*ii
P= polySystem {f,g}
writeJuliaFile(P,"test")
sols=solveJulia P
peek first sols




writeSys(P,"test.jl",WithImports=>true)--option not working?

needs "./ExampleSystems/jointsR6.m2"
Q=polySystem jointsR6(CC_53)
sols=solveJulia Q;
ourSols = solveSystem Q
apply(
    sortSolutions ourSols, 
    sortSolutions sols,
    (a,b) -> areEqual(a,b)
    )
#sols--too many?
#ourSols
