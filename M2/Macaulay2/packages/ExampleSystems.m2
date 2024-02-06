-- -*- coding: utf-8 -*-
newPackage(
	"ExampleSystems",
	AuxiliaryFiles => true,
    	Version => "0.5", 
    	Date => "May 2020",
    	Authors => {
               {Name => "Anton Leykin", 
               Email => "leykin@math.gatech.edu"},
               {Name => "Justin Chen", 
               Email => "justin.chen@math.gatech.edu"},
               {Name => "Kelly Maluccio", 
               Email => "kmaluccio@math.tamu.edu"},
               {Name => "Leah Gold", 
               Email => "L.Gold33@csuohio.edu"}
        },
	PackageExports => {"NumericalAlgebraicGeometry"},
    	Headline => "database of polynomial systems",
	Keywords => {"Examples and Random Objects"},
    	DebuggingMode => false
    	)

examples'names = {
    "bellido",
    "boon",
    "butcher",
    "camera1s",
    "caprasse",
    "cassou",
    "chemequ",
    "cohn3",
    "comb3000s",
    "cyclic",
    "dipole2",
    "eco8",
    "geneig",
    "heart",
    "ipp",
    "katsura",
    "ku10",
    "lorentz",
    "lumped",
    "noon5",
    "proddeco",
    "puma",
    "quadgrid",
    "rabmo",
    "randomGeneralizedEigenvalueProblem",
    "randomSystem",
    "rbpl24s",
    "reimer5",
    "rose",
    -- "scurve726",
    "sendra",
    "trinks",
    "virasoro",
    "wood",
    "wright"
    }
    
for e in examples'names do 
needs("./ExampleSystems/"|e|".m2")

beginDocumentation()

doc ///
     Key
          ExampleSystems
     Headline
          examples of polynomial systems
     Description
          Text
               This package is a database for examples of polynomial systems in Macaulay2,
               including standard systems of interest in various applications, such as 
               engineering, chemistry, robotics, and economics.
               
               {\bf References:} 
          Code
               UL {
                    "http://homepages.math.uic.edu/~jan/demo.html",
                    "http://www-sop.inria.fr/coprin/logiciels/ALIAS/Benches/node1.html",
                    "http://symbolicdata.org/XMLResources/IntPS/",
                    "http://www.cecm.sfu.ca/~rpearcea/polsys.txt"
               }
///

-- multidoc ///
 -- Node
  -- Key
   -- ExampleSystems
  -- Headline
   -- examples of polynomial systems
-- ///

end

-- Here place M2 code that you find useful while developing this
-- package.  None of it will be executed when the file is loaded,
-- because loading stops when the symbol "end" is encountered.
restart
uninstallPackage "ExampleSystems"
installPackage "ExampleSystems"
installPackage("ExampleSystems", RemakeAllDocumentation=>true)
check "ExampleSystems"
help "ExampleSystems"
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=PackageTemplate pre-install"
-- End:

ExampleTable = new Type of HashTable

box = method()
example = method(Options=>{CoefficientRing => ZZ/32003,
	                   Ring => null})

findExamples  = method()
findExamples(ExampleTable, String) := (E,regex) -> (
     K := keys E;
     select(K, k -> match(regex, K#k#0))
     )

show(ExampleTable, ZZ) := (H,x) -> print ("-------------------" || ("-- "|H#x#0) || H#x#1)
show(ExampleTable, List) := (H,x) -> scan(x, x1 -> show(H,x1))
show(ExampleTable, String) := (H,re) -> show(H,findExamples(H,re))
show(ExampleTable) := (H) -> show(H, sort keys H)

box(ExampleTable, ZZ) := (H,x) -> box(H,{x})
box(ExampleTable, List) := (H,x) -> netList apply(x, x1 -> {x1, ("-- "|H#x1#0) || H#x1#1})
box(ExampleTable, String) := (H,re) -> box(H,findExamples(H,re))
box(ExampleTable) := (H) -> box(H, sort keys H)

example(ExampleTable, ZZ) :=
example(ExampleTable, String) := opts -> (H,x) -> (
     R1 := if opts#?Ring then opts#Ring else null;
     kk := if R1 === null 
             then opts.CoefficientRing 
	     else coefficientRing R1; 
     I := value replace("kk", toString kk, H#x#1);
     if R1 =!= null then (
     	  nvars := numgens ring I;
	  if numgens R1 < nvars then 
	    error ("expected ring with at least "|nvars|" variables");
	  substitute(I, (vars R1)_{0..nvars-1})
	  )
     else I)

readExampleFile = method()
-* -- remove this code
readExampleFile(String,Ring) := (filename, coeffring) -> (
     G := separateRegexp("---+", get (ExampleIdeals#"source directory"|filename));
     G = apply(G, s -> select(lines s, t -> #t > 0));
     -- now for each example, find its name/number and make the string
     G = select(G, s -> #s > 0);
     new ExampleTable from apply(#G, i -> (
	       s := substring(2,G#i#0); -- remove the first two -- characters
	       i+1 => s => replace("kk", toString coeffring, demark("\n",drop(G#i,1)))))
     )
readExampleFile(String) := (filename) -> (
     G := separateRegexp("---+", get (ExampleIdeals#"source directory"|filename));
     G = apply(G, s -> select(lines s, t -> #t > 0));
     -- now for each example, find its name/number and make the string
     G = select(G, s -> #s > 0);
     new ExampleTable from apply(#G, i -> (
	       s := substring(2,G#i#0); -- remove the first two -- characters
	       i+1 => s => demark("\n",drop(G#i,1))))
     )
*-
getExampleFile = method()

-* -- remove this code
getExampleFile(String,String) := (filename,kkstring) -> (
     G := separateRegexp("---+", get filename);
     G = apply(G, s -> select(lines s, t -> #t > 0));
     -- now for each example, find its name/number and make the string
     G = select(G, s -> #s > 0);
     new ExampleTable from apply(#G, i -> (
	       s := substring(2,G#i#0); -- remove the first two -- characters
	       i+1 => s => replace("kk", kkstring, demark("\n",drop(G#i,1)))))
     )
getExampleFile(String) := (filename) -> getExampleFile(filename,"")
*-

-- New code
getExampleFile(String) := (filename) -> (
     G := separateRegexp("---+", get filename);
     G = apply(G, s -> select(lines s, t -> #t > 0));
     -- now for each example, find its name/number and make the string
     G = select(G, s -> #s > 0);
     new ExampleTable from apply(#G, i -> (
	       s := substring(2,G#i#0); -- remove the first two -- characters
	       i+1 => s => demark("\n",drop(G#i,1))))
     )
readExampleFile(String) := (filename) -> 
     getExampleFile(ExampleIdeals#"source directory"|"ExampleIdeals/"|filename)
     
replaceStrings = (L, str) -> (scan(L, v -> str = replace(v#0,v#1,str)); str)
substitute(ExampleTable, List) := (E, L) -> (
     -- L is a list of options: str => newstr, (or regex => newstr).
     -- return a new ExampleTable where each final string has the given strings in L
     -- replaced (in order).
     new ExampleTable from apply(pairs E, (k,v) -> (
	       k => (v#0 => replaceStrings(L, v#1))
	       ))
     )
substitute(ExampleTable, Option) := (E,subs) -> substitute(E, {subs})


