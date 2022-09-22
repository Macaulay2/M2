-- -*- coding: utf-8 -*-
-- Copyright (C) 2020 Lily Silverstein and Jay White
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.


newPackage (
  "MonomialIntegerPrograms",
  Version=>"1.0",
  Authors => {
      {Name => "Lily Silverstein", Email => "lsilverstein@cpp.edu", HomePage => "https://cpp.edu/faculty/lsilverstein"},
      {Name => "Jay White", Email => "jay.white@uky.edu", HomePage => "https://math.as.uky.edu/users/jwh245"}
      },
  Headline => "using integer programming for fast computations with monomial ideals",
  Configuration => {
      "CustomPath" => "",
      "CustomScipPrintLevel" => ""
      },
  AuxiliaryFiles => true,
  CacheExampleOutput => true,
  PackageImports => {"LexIdeals","MinimalPrimes"},
  OptionalComponentsPresent => scipPresent := run "type scip >/dev/null 2>&1" === 0,
  DebuggingMode => false
)

-------------
-- exports --
-------------
export {
    "bettiTablesWithHilbertFunction",
    "codimensionIP",    
    "degreeIP",
    "dimensionIP",
    "loadSCIPCodimAndDegree",
    "loadBuiltinCodimAndDegree",
    "monomialIdealsWithHilbertFunction",
    "topMinimalPrimesIP",
    "minimalPrimesIP",
    "BoundGenerators",
    "Count",
    "FirstBetti",
    "GradedBettis",
    "KnownDim",
    "IgnorePrimes",
    "SquareFree"
    }
exportMutable {
    "ScipPrintLevel"
    }

userPath = MonomialIntegerPrograms#Options#Configuration#"CustomPath";

ScipPath = if userPath === "" then(
    if debugLevel > 0 then
    print("Using default executable name \"scip\".\nTo change this, load package using CustomPath option.");
    "scip") else userPath;

userPrintLevel = MonomialIntegerPrograms#Options#Configuration#"CustomScipPrintLevel";

ScipPrintLevel = if userPrintLevel === "" then(
    if debugLevel > 0 then
    print("Current value of ScipPrintLevel is 0.\nTo set a custom default value, load package using CustomScipPrintLevel option.");
    0) else value userPrintLevel;
------------------------
-- codim, dim, degree --
------------------------
codimensionIP = method();
codimensionIP (MonomialIdeal) := I -> (
    if I.cache#?(symbol codim) then return I.cache#(symbol codim);
    (dir, zimplFile, solFile, errorFile, detailsFile) := tempDirectoryAndFiles("codim");
    zimplFile << codimensionIPFormulation(I) << close;
    run(concatenate("(",ScipPath, 
	    " -c 'read ", zimplFile,
	    "' -c 'optimize'",
	    " -c 'display solution ",
	    "' -c quit;) 1>",
	    solFile,
	    " 2>",
	    errorFile));
    printStatement({zimplFile, solFile, errorFile, "Codim", dir});
    readScipSolution(solFile)
    )

dimensionIP = method();
dimensionIP (MonomialIdeal) := I -> (
    n := numgens ring I;
    n - codimensionIP(I)
    )

degreeIP = method( 
    Options => {KnownDim => -1}
    );
degreeIP (MonomialIdeal) := o -> I -> (
    if I.cache#?(symbol degree) then return I.cache#(symbol degree);
    if (cokernel generators I).cache#?(symbol poincare) then return oldDegree I;
    objValue := if o.KnownDim >= 0 then o.KnownDim else dimensionIP(I);
    (dir, zimplFile, solFile, errorFile, detailsFile) := tempDirectoryAndFiles("deg");        
    if not isSquareFree I then (
    	J := polarize(I);
    	newDim := numgens ring J - numgens ring I + objValue;
    	zimplFile << degreeIPFormulation(J, newDim) << close;
  	) else (
    	zimplFile << degreeIPFormulation(I, objValue) << close;
  	);
    run(concatenate("(",ScipPath, 
	    " -c 'set emphasis counter'",
	    " -c 'set constraints countsols collect FALSE'",     	    
	    " -c 'read ", zimplFile, "'",
	    " -c 'count'",
	    " -c quit;)",
	    " 1>",
	    solFile,
	    " 2>",
	    errorFile
	    ));
    printStatement({zimplFile, solFile, errorFile, "Degree", dir});
    readScipCount(solFile)
    )
    


oldCodim = lookup(codim, MonomialIdeal);
oldDegree = lookup(degree, MonomialIdeal);
loadSCIPCodimAndDegree = method();
installMethod(loadSCIPCodimAndDegree,() -> (
  codim MonomialIdeal := {} >> opts -> m -> ((cacheValue symbol codim) codimensionIP) m;
  degree MonomialIdeal := m -> ((cacheValue symbol degree) degreeIP) m;
));
loadBuiltinCodimAndDegree = method();
installMethod(loadBuiltinCodimAndDegree, () -> (
  codim MonomialIdeal := oldCodim;
  degree MonomialIdeal := oldDegree;
));

if scipPresent then loadSCIPCodimAndDegree();
--------------------------
-- betti tables with HF --
--------------------------
bettiTablesWithHilbertFunction = method(
    Options => {
	Count => false,
	BoundGenerators => -1,
	FirstBetti => null,
	GradedBettis => null,
	SquareFree => false
	}
    );
bettiTablesWithHilbertFunction (List, PolynomialRing) := o -> (D, R) -> (
    M := monomialIdealsWithHilbertFunction(D, R, 
	BoundGenerators => o.BoundGenerators, 
	FirstBetti => o.FirstBetti, 
	GradedBettis => o.GradedBettis,
	SquareFree => o.SquareFree);
    if o.Count then(
	tally apply(M, m -> betti res m)
	)
    else(
	unique apply(M, m -> betti res m)
	)
    )
-----------------------
-- monideals with HF
-----------------------
monomialIdealsWithHilbertFunction = method(
    Options => {
	BoundGenerators => -1,
	FirstBetti => null,
	GradedBettis => null,
	SquareFree => false
	}
    );
monomialIdealsWithHilbertFunction (List, PolynomialRing) := o -> (D, R) -> (
    if not isHF D then(if ScipPrintLevel >= 1 then print("Impossible Hilbert function values."); {})
    else(
	if o.FirstBetti =!= null and o.GradedBettis =!= null then error("cannot specify FirstBetti and GradedBettis options simultaneously");
	n := numgens R;
    	Dlist := apply(#D, i -> binomial(n+i-1,i)-D#i);
    	if all(Dlist, d -> d==0) then return {monomialIdeal(0_R)};
    	(dir, zimplFile, solFile, errorFile, detailsFile) := tempDirectoryAndFiles("hilbert");            
    	zimplFile << hilbertIPFormulation(Dlist, n, o) << close;
    	run(concatenate("(",ScipPath, 
	    " -c 'set emphasis counter'",
	    " -c 'set constraints countsols collect TRUE'",     
	    " -c 'read ", zimplFile,
	    "' -c 'count'",
	    " -c 'write allsolutions ",
	    solFile,
	    "' -c quit;)",
	    " 1>",
	    detailsFile,
	    " 2>",
	    errorFile));
        printStatement({zimplFile, solFile, errorFile, "Hilbert", dir});
    	readAllMonomialIdeals(solFile, R)
	)
    )


----------------------
-- topMinimalPrimes --
----------------------

topMinimalPrimesIP = method(
    Options => {KnownDim => -1, IgnorePrimes => {}}
    );
topMinimalPrimesIP (MonomialIdeal) := o -> I -> (
    if I == monomialIdeal(1_(ring I)) then return I;
    R := null;
    squarefree := isSquareFree I;
    ignorePrimes := o.IgnorePrimes;
    if not squarefree then (
      R = ring I;
      I = polarize I;
      polarizedRing := ring I;
      ignorePrimes = apply(ignorePrimes, p ->(
        sub(polarize monomialIdeal p, polarizedRing)
      ));
    );
    ignorecontraints := ignorePrimesConstraints(ignorePrimes, squarefree);
    
    k := if o.KnownDim >= 0 then o.KnownDim else dimensionIPWithConstraints(I, ignorecontraints);
    if k === null then return {};
    (dir, zimplFile, solFile, errorFile, detailsFile) := tempDirectoryAndFiles("comps");
    zimplFile << degreeIPFormulation(I, k) << ignorecontraints << close;
    run(concatenate("(",ScipPath,
	    " -c 'set emphasis counter'",
	    " -c 'set constraints countsols collect TRUE'",
	    " -c 'read ", zimplFile,
	    "' -c 'count'",
	    " -c 'write allsolutions ",
	    solFile,
	    "' -c quit;)",
	    " 1>",
	    detailsFile,
	    " 2>",
	    errorFile));
    printStatement({zimplFile, solFile, errorFile, "Minimal primes of codim " | (numgens ring I - k), dir});
    L := readAllPrimes(solFile, ring I);
    if squarefree then L else unPolarizeSome(L, R)
)

---------------------
-- minimalPrimesIP --
---------------------

minimalPrimesIP = method();
minimalPrimesIP (MonomialIdeal, ZZ) := (I, iterations) -> (
  collectedPrimes := {};
  i := 0;
  while i < iterations or iterations < 0 do (
    newPrimes := topMinimalPrimesIP(I, IgnorePrimes => collectedPrimes);
    if #newPrimes === 0 then break;
    collectedPrimes = collectedPrimes | newPrimes;
    i = i + 1;
  );
  collectedPrimes
)
minimalPrimesIP (MonomialIdeal) := I -> minimalPrimesIP(I, -1);


----------------------
-- internal methods --
----------------------

degreeIPFormulation = method();
degreeIPFormulation (List, ZZ, ZZ) := (A, n, knownDim) -> (
    concatenate(codimensionIPFormulation(A, n),"\n",
	"subto dim: sum <i> in N: X[i] == "|toString(n - knownDim)|";")
    )
degreeIPFormulation (MonomialIdeal, ZZ) := (I, knownDim) -> (
    degreeIPFormulation(monIdealToSupportSets(I), #gens ring I, knownDim)
    )
codimensionIPFormulation = method();
codimensionIPFormulation (List, ZZ) := (A, n) -> (
    concatenate({"set N := {0 .. ",toString(n-1),"};\n",
        "var X[N] binary;\n","minimize obj: sum <i> in N: X[i];\n",
        demark("\n", for i from 0 to #A-1 list(
	concatenate({"subto constraint",toString(i),": ",
	demark("+",apply(A#i, e -> "X["|toString(e)|"]")),
	" >= ",toString(1)|";"})))
    })
)
codimensionIPFormulation (MonomialIdeal) := (I) -> (
    codimensionIPFormulation(monIdealToSupportSets I, #gens ring I)
    )
hilbertIPFormulation = method(
    Options => {
	BoundGenerators => -1,
	FirstBetti => null,
	GradedBettis => null,
	SquareFree => false
	}
    );
hilbertIPFormulation (List, ZZ) := o -> (D, n) -> (
    db := if o.BoundGenerators > 0 then o.BoundGenerators else #D-1;
    varsCommas := demark(",", toList vars(0..n-1));
    varsPluses := demark("+", toList vars(0..n-1));
    altVarsCommas := demark(",", toList vars(n..2*n-1));    
    altVarsPluses := demark("+", toList vars(n..2*n-1));
    bettiLines := "";
    sfLines := "";
    if o.SquareFree then(
	sfLines = concatenate({
	    "\nset F := {0 .. 1};\n",
	    "subto squarefree: forall <",varsCommas,"> in M without ",demark("*", n:"F"), " do\n",
	    "    Y[",varsCommas,"] == 0;"
	    });
    );
    if o.GradedBettis =!= null then (
	G := o.GradedBettis; 
	if #G-1 > db then error("degrees of generators cannot be higher than degree bound");
	bettiLines = concatenate({
	    "\nset E := {0 .. maxGenD};\n",
	    "param Q[<degree> in E] := ", demark(", ",apply(#G, i -> "<"|i|">"|G#i)),";\n",
	    "subto specifiedBettis: forall <degree> in E do\n",
	    "    sum <", varsCommas, "> in M with ", varsPluses, " == degree: Y[",varsCommas,"] == Q[degree];\n"
	    })
    );
    if o.FirstBetti =!= null then (
    	bettiLines = concatenate({"\nsubto totalBetti: sum <", varsCommas, "> in M: Y[",varsCommas,"] == ",toString o.FirstBetti, ";\n"})
	);
    concatenate({
	    "param maxD := ",toString(#D-1),";\n",
	    "param maxGenD := ",toString(db),";\n",
	    "set D := {0 .. maxD};\n",
	    "set M := {<",varsCommas,"> in ", demark("*", n:"D")," with ", varsPluses," <= maxD};\n",
	    "set BELOW[<",varsCommas,"> in M] := {<",altVarsCommas,"> in M with ",
	    demark(" and ", apply(n, i -> (toString vars(n+i))|"<="|(toString vars(i)))),
	    " and (",altVarsPluses," == ",varsPluses,"-1)};\n",
	    "set ABOVE[<",varsCommas,"> in M] := {<",altVarsCommas,"> in M with ",
	    demark(" and ", apply(n, i -> (toString vars(n+i))|">="|(toString vars(i)))),
	    " and (",altVarsPluses," == ",varsPluses,"+1)};\n",
	    "set ALLABOVE[<",varsCommas,"> in M] := {<",altVarsCommas,"> in M with ",
	    demark(" and ", apply(n, i -> (toString vars(n+i))|">="|(toString vars(i)))),
	    " and (",altVarsPluses," >= ",varsPluses,"+1)};\n",
	    "param P[<degree> in D] := ", demark(", ",apply(#D, i -> "<"|i|">"|D#i)),";\n",
	    "var X[M] binary;\n",
	    "var Y[M] binary;\n",
	    "minimize obj: X[", demark(",", n:"0"), "];\n",
	    "subto h: forall <degree> in D do\n",
	    "    sum <", varsCommas, "> in M with ", varsPluses, " == degree: X[",varsCommas, "] == P[degree];\n",
	    "subto ideal: forall <",varsCommas,"> in M with ",varsPluses," <= maxD-1 do\n",    
	    "sum <",altVarsCommas,"> in ABOVE[",varsCommas,"]: X[",altVarsCommas,"] - ",toString n,"*X[",varsCommas,"] >= 0;\n",
	    "subto gensInIdeal: forall <",varsCommas,"> in M do\n X[",varsCommas,"] - Y[",varsCommas,"] >= 0;\n",
	    "subto mingens: forall <",varsCommas,"> in M with ",varsPluses," <= maxD-1 do\n",
    	    "    forall <",altVarsCommas,"> in ALLABOVE[",varsCommas,"] do\n",
	    "        Y[",varsCommas,"] + Y[",altVarsCommas,"] <=1;\n",
	    "subto markGens: forall <",varsCommas,"> in M with ",varsPluses," <= maxD do\n",
	    "    sum <",altVarsCommas,"> in BELOW[",varsCommas,"]: X[",altVarsCommas,"] + Y[",varsCommas,"] - X[",varsCommas,"] >= 0;\n",
	    "subto genDegreeBound: forall <",varsCommas,"> in M with ",varsPluses," >= maxGenD+1 do\n",
	    "    Y[",varsCommas,"] == 0;",
	    sfLines,
	    bettiLines
	    })
    )
monIdealToSupportSets = method()
monIdealToSupportSets (MonomialIdeal) := (I) -> (
    apply(first entries mingens I, m -> apply(support m, r -> index r))
    )
printStatement = method();
printStatement (List) := L -> (
    (zimplFile, solFile, errorFile, nickname, dir) := toSequence L;
    if ScipPrintLevel >= 1 then print(nickname|" files saved in directory: "|dir);
    if ScipPrintLevel >= 2 then try print(get errorFile) else print("No SCIP errors reported.");
    if ScipPrintLevel >= 3 then try print(get zimplFile) else print("ZIMPL file not found! This might be an error or bug.");
    if ScipPrintLevel >= 4 then try print(get solFile) else print("SCIP solution file not found! This is probably due to an infeasible IP.");
)
readAllMonomialIdeals = method()
readAllMonomialIdeals (String, PolynomialRing) := (solFile, R) -> (
    n := numgens R;
    try(L := lines get solFile;
	L = apply(L, l -> separate(",",l));
	yIndices := positions(L#0, a -> select("Y",a)=!={});
    	exps := for y in yIndices list drop(separate("#",L#0#y),1);
    	mons := apply(exps, e -> product apply(n, i-> R_i^(value e_i)));
    	L = drop(L, 1);
    	apply(L, ln -> monomialIdeal mons_(positions(ln_yIndices, i -> value i == 1)))
	)
    else(if ScipPrintLevel >= 1 then print("Infeasible IP; no solutions"); {} )
    )
readAllPrimes = method()
readAllPrimes (String, PolynomialRing) := (solFile, R) -> (
  n := numgens R;
  L := lines get solFile;
  mons := apply(select("X#([[:digit:]]+)", L#0), a -> R_(value substring(a, 2)));
  L = drop(L, 1);
  allSolutions := apply(L, ln -> (
    l := value replace("\\(", "-(", ln);  --faster parse: offloads parsing to value
                                          --replaces "24(24)" with "24-(24)" which allows value to parse the line as a sequence.
    monomialIdeal for i from 1 to #l-1 list if l#i===1 then mons#(i-1) else continue
  ))
)
readScipSolution = method();
readScipSolution (String) := solFile -> (
    solContents := get solFile;
    allSolutions := select(///objective value.[[:space:]]+([[:digit:]]+)///, ///\1///, solContents);
    if #allSolutions == 0 then null else value first allSolutions
)
readScipCount = method();
readScipCount (String) := solFile -> (
    solContents := get solFile;
    value first select(///Feasible Solutions[[:space:]]+:[[:space:]]+([[:digit:]]+)///, ///\1///, solContents)
)
tempDirectoryAndFiles = method()
tempDirectoryAndFiles (String) := (bname) -> (
    dir := temporaryFileName();
    makeDirectory(dir);
    (dir, dir|"/"|bname|".zpl", dir|"/"|bname|".sol", dir|"/"|bname|".errors", dir|"/"|bname|".details")
)

-----------------------------------------------------
-- internal methods related to IgnorePrimes option --
-----------------------------------------------------

ignorePrimesConstraints = method();
ignorePrimesConstraints (List, Boolean) := (L, squarefree) -> (
  concatenate(apply(#L, i -> (
    primeVars := first entries mingens(L#i);
    sumBound := #primeVars - 1;
    
    if not squarefree then (
      polarizedVariables := (ring(L#i))_*;
      varindices := first@@last@@baseName \ primeVars;
      primeVars = select(polarizedVariables, v->member(first@@last@@baseName v, varindices));
    );
    
    concatenate(
      "\n",
      "subto ignore",
      toString(i),
      ": ",
      demark("+", apply(primeVars, e -> "X["|toString(index e)|"]")),
      " <= ",
      toString(sumBound),
      ";"
    )
  )))
)




codimensionIPWithConstraints = method();
codimensionIPWithConstraints (MonomialIdeal, String) := (I, constraints) -> (
    (dir, zimplFile, solFile, errorFile, detailsFile) := tempDirectoryAndFiles("codim");
    zimplFile << codimensionIPFormulation(I) << constraints << close;
    run(concatenate("(",ScipPath, 
	    " -c 'read ", zimplFile,
	    "' -c 'optimize'",
	    " -c 'display solution ",
	    "' -c quit;) 1>",
	    solFile,
	    " 2>",
	    errorFile));
    printStatement({zimplFile, solFile, errorFile, "Codim", dir});
    readScipSolution(solFile)
)

dimensionIPWithConstraints = method();
dimensionIPWithConstraints (MonomialIdeal, String) := (I, constraints) -> (
    n := numgens ring I;
    cdim := codimensionIPWithConstraints(I, constraints);
    if cdim === null then null else (n - cdim)
)

---------------------
-- un-polarization --
---------------------

unPolarize = method();
unPolarize (MonomialIdeal, PolynomialRing) := (I, R) -> (
  --This reverses the effect of polarize.
  --I is the ideal we wish to unpolarize.
  --R is the ring that we want to map I too.

  polarizedVariables := (ring I)_*;         --This gets all the variable names in I.
  substitutions := polarizedVariables / (   --We get a list of all the substitutions.
    v -> v => R_(first@@last@@baseName v)            --All the substitutions look like z_{i, j} => R_i.
                                            --first@@indices would not work because z_{i,j} is not the ith variable in the ring containing I
  );
  monomialIdeal substitute(I, substitutions)              --Finally, we apply all these substitutions to I.
)


unPolarizeSome = method();
unPolarizeSome (List, PolynomialRing) := (L, R) -> (
  --This applies unPolarize to the ideals in L where all the last indices are 0.
  for I in L list (                                               --loop through the list
    if not all(I_*, zero@@last@@last@@baseName) then continue;    --If one of the last indices is zero, we skip this and go to the next ideal and add nothing.
    unPolarize(I, R)                                --Otherwise, we unPolarize the ideal and add it to the list
  )
)



-------------------
-- documentation --
-------------------

beginDocumentation()

doc ///
 Key
  MonomialIntegerPrograms
 Headline
  A package for fast monomial ideal computations using constraint integer programming
 Description
  Text
   {\bf Installation and licensing information.}
   
   This package relies on the constraint integer program solver SCIP, which
   is available at @HREF"https://scip.zib.de/"@. This software is free for
   for academic, non-commercial purposes. Notice that SCIP is not distributed 
   under GPL, but under the ZIB Academic License (@HREF"https://scip.zib.de/academic.txt"@).
  
   To install SCIP, click the {\bf Download} tab on the left-hand side of the
   SCIP home page. The easiest method is to install prebuilt binaries (look for the heading
   {\em Installers (install the scipoptsuite in your computer, without source files)}.
   Choose the appropriate Linux, Windows, or MacOS file. The download is free,
   but you will be asked to submit your name and academic institution, to conform to
   the ZIB Academic License requirements, before the download begins.
  
   Under the heading {\em Source Code}, you can find the files for building
   from source. If building from source, you MUST include the source files for
   the modeling language Zimpl in order to use the Monomial Integer 
   Programs package. This will be included if you choose the download named 
   SCIP Optimization Suite, rather than the one named SCIP. Alternatively, download
   SCIP and then follow the {\em ZIMPL} link at the top of the home page to
   download the source files for Zimpl. When building SCIP, you will have to set
   a flag indicating that Zimpl should be built as well. For more information about
   building SCIP visit their online documentation (@HREF "https://scip.zib.de/doc-6.0.0/html/"@)
   and click on {\em Overview} -> {\em Getting started} ->
   {\em Installing SCIP}.
  
   An excellent user guide to using Zimpl can be found at 
   @HREF"https://zimpl.zib.de/download/zimpl.pdf"@. The author, Thorsten
   Koch, requests that research making use of this software please
   cite his 2004 PhD thesis, {\em Rapid Mathematical Programming}. The
   appropriate BibTeX entry can be found here: @HREF"https://zimpl.zib.de/download/zimpl.bib"@.
   Zimpl is distributed under GPL.
  
   Additionally, any research that uses SCIP needs a proper citation. See the
   {\bf How to Cite} tab on their home page.  
     
   Finally, because this package relies on temporary files, Windows users must
   ensure the directory @TT"/tmp"@ exists as stated in the documentation of
   @TO temporaryFileName@.
     
   {\bf Behavior of package on load.}
   
   The value of @TO symbol ScipPrintLevel@ determines the verbosity.
   It is set to @TT"0"@ when the package is loaded.
   
   The functions @TO codim@, and @TO degree@ are overwritten for inputs with
   type @TO MonomialIdeal@. Specifically, @TO loadSCIPCodimAndDegree@ is run
   when the package is loaded.
 Caveat
  These methods are designed and tested for ideals and polynomial rings with a
  field as the coefficients ring. For other rings, the behavior of these methods
  is unspecified.
 SeeAlso
  codimensionIP
  degreeIP
  dimensionIP
  monomialIdealsWithHilbertFunction
  topMinimalPrimesIP
  symbol ScipPrintLevel
///

doc ///
 Key
  symbol ScipPrintLevel
 Headline
  adjust how much solving information is displayed in MonomialIntegerPrograms
 Description
  Text
   @TT"ScipPrintLevel"@ is a global symbol defined in Monomial Integer Programs 
   using @TO exportMutable@. After the package has been loaded, 
   the user can change the value of ScipPrintLevel at any time, 
   and the specified behavior will immediately apply to all 
   methods implemented in the package.

   Meaningful options for ScipPrintLevel are:
   
   {\bf 0} return the answers to computations only,  suppressing all other printing  
   
   {\bf 1} return the answer, and print to screen the location of the
   temporary directory which contains all the files related to the computation. By 
   default this is a subdirectory of @TT"/tmp/"@, see @TO temporaryFileName@.

   {\bf 2} all the above, plus display any error or warning messages 
   generated by SCIP during the computation, i.e. anything sent by SCIP to stderr.
   See note below about warning messages.

   {\bf 3} all the above, plus print the problem file generated with 
   this package, used as the input to SCIP
      
   {\bf 4} all the above, plus print the solution file generated 
   by SCIP after solving the IP
   
   {\bf 5} all the above, plus print any other information sent by 
   SCIP to stdout during the solve, if any

   The default value of ScipPrintLevel is 0. To load the package with a different
   default value for ScipPrintLevel, imitate the following example.
  Example
   loadPackage("MonomialIntegerPrograms", Configuration => {"CustomScipPrintLevel" => "2"}, Reload => true);
  Text
   Replace "2" above  with any custom choice. Note that the string "2" is used, not an integer.
   You may or may not have to include the "Reload => true" option depending on if the package is already loaded.
      
   
   {\bf Why am I getting warnings/why does the solver report infeasibility for
   the degree count?}

   This often happens when ScipPrintLevel is set to 2 or above and is not an error, but a consequence
   of the normal solving behavior of SCIP. 
   Computing the degree of a monomial ideal is done by counting the number of feasible solutions
   to a certain integer program. SCIP is generally programmed to find a single optimal or feasible
   solution and then terminate, so to count them it uses the following "trick": Every time SCIP encounters a feasible
   solution or branch, it is recorded, then a constraint is added to make the new solution/branch 
   infeasible, so the search can continue. Eventually, all the solutions are recorded and the entire problem has been 
   made "infeasible." Thus the solving details for the degree problem print a final result of 
   "problem is infeasible," but the correct count has been taken. For more details, see this
   specific SCIP documentation page: @HREF"https://scip.zib.de/doc/html/COUNTER.php"@.

 SeeAlso
  MonomialIntegerPrograms
///



doc ///
 Key
  degreeIP
  (degreeIP, MonomialIdeal)
  [degreeIP, KnownDim]
 Headline
  compute the degree of a monomial ideal using integer programming
 Usage
  d = degreeIP(I)
  d = degreeIP(I, KnownDim => k)
 Inputs
  I : MonomialIdeal
  KnownDim => ZZ
   the dimension, @TT"k"@, of the ideal
 Outputs
  d:ZZ
   the degree of @TT"I"@. That is, if @TT"k"@ is the maximum dimension of
   a coordinate subspace in the variety of @TT"I"@, then @TT"degree(I)"@ is
   the number of @TT"k"@-dimensional subspaces in the variety.
 Description
  Text
   If a @TT"KnownDim"@ is not provided, @TT"degreeIP"@ will first
   call @TO dimensionIP@ to compute the dimension.
   Additionally, before doing any computation, the cache of
   @TT"cokernel generators I"@ is checked to see if the degree has been previously computed.
   
   An integer programming formulation of the degree problem is
   written to a temporary file directory, then the SCIP
   Optimization Suite is used to solve the IP. Solving details
   are written to a second file in the temporary directory, before
   outputting the answer.
  Example
   R = QQ[x,y,z,w,v];
   I = monomialIdeal(x*y*w, x*z*v, y*x, y*z*v);
   degreeIP(I, KnownDim => 3)
   degreeIP(I)
  Text
   The location of the temporary directory is printed to the
   screen.

   For more information about the SCIP warning messages, and related
   info on how SCIP counts solutions, see the very end of the
   @TO symbol ScipPrintLevel@ info page. 
 Caveat
  @TT"degreeIP"@ does not verify that a provided @TT"KnownDim"@ 
  is correct. Providing the wrong dimension will result in an 
  incorrect degree count (and possibly an infeasible program).
 SeeAlso
  (degree, Ideal)
  dimensionIP
  MonomialIntegerPrograms
  symbol ScipPrintLevel
///

doc ///
 Key
  dimensionIP
  (dimensionIP, MonomialIdeal)
 Headline
  compute the dimension of a monomial ideal using integer programming
 Usage
  k = dimensionIP(I)
 Inputs
  I:MonomialIdeal
 Outputs
  k:ZZ
   the dimension of $I$. That is, $k$ is the maximum dimension of
   a coordinate subspace in the variety of $I$.
 Description
  Text
   This function calls @TO codimensionIP@ and then returns $n$-codimensionIP($I$), where 
   $n$ is the number of variables in the polynomial ring where $I$ is defined.
   The integer programming input and output files created will therefore be named
   "codim.zpl", "codim.errors", etc. as with @TO codimensionIP@. 
  Example
   R = QQ[x,y,z,w,v];
   I = monomialIdeal(x*y*w, x*z*v, y*x, y*z*v);
   dimensionIP(I)
  Text   
   The location of input/output files for SCIP solving is printed
   to the screen by default. To change this, see @TO symbol ScipPrintLevel@.
  Example
   ScipPrintLevel = 0;
   J = monomialIdeal(x*y^3*z^7, y^4*w*v, z^2*v^8, x*w^3*v^3, y^10, z^10)
   dimensionIP(J) 
  Text
   The dimension of a monomial ideal is equal to the dimension
   of its radical. Therefore, when looking at the IP formulation written to
   the temporary file "codim.zpl", you will see that exponents are ignored.
 SeeAlso
  (dim, MonomialIdeal)
  codimensionIP
  MonomialIntegerPrograms
  symbol ScipPrintLevel
///

doc ///
 Key
  codimensionIP
  (codimensionIP, MonomialIdeal)
 Headline
  compute the codimension of a monomial ideal using integer programming
 Usage
  c = codimensionIP(I)
 Inputs
  I:MonomialIdeal
 Outputs
  c:ZZ
   the codimension of $I$
 Description
  Text
   The integer programming input and output files created are named
   "codim.zpl", "codim.errors", etc., and saved to a temporary directory.
   By default the location of the temporary directory is printed to the
   screen.
  Example
   R = QQ[x,y,z,w,v];
   I = monomialIdeal(x*y*w, x*z*v, y*x, y*z*v);
   codimensionIP(I)
  Text
   The verbosity of every function in the MonomialIntegerPrograms package is controlled with
   @TO symbol ScipPrintLevel@. For example, to suppress printing the name of
   the directory or any other information and simply return the answer, set
   @TT"ScipPrintLevel"@ to 0.
  Example
   ScipPrintLevel = 0;
   J = monomialIdeal(x*y^3*z^7, y^4*w*v, z^2*v^8, x*w^3*v^3, y^10, z^10)
   codimensionIP(J) 
  Text
   The codimension of a monomial ideal is equal to the codimension
   of its radical. Therefore, when looking at the IP formulation written to
   the temporary file "codim.zpl", you will see that exponents are ignored.
 SeeAlso
  (codim, MonomialIdeal)
  dimensionIP
  MonomialIntegerPrograms
  symbol ScipPrintLevel
///

doc ///
 Key
  monomialIdealsWithHilbertFunction
  (monomialIdealsWithHilbertFunction, List, PolynomialRing)
  [monomialIdealsWithHilbertFunction, BoundGenerators]
  [monomialIdealsWithHilbertFunction, FirstBetti]
  [monomialIdealsWithHilbertFunction, GradedBettis]
  [monomialIdealsWithHilbertFunction, SquareFree]
 Headline
  find all monomial ideals in a polynomial ring with a particular (partial or complete) Hilbert function
 Usage
  monomialIdealsWithHilbertFunction(L, R)
  monomialIdealsWithHilbertFunction(L, R, BoundGenerators => a)
  monomialIdealsWithHilbertFunction(L, R, FirstBetti => b)
  monomialIdealsWithHilbertFunction(L, R, GradedBettis => B)
  monomialIdealsWithHilbertFunction(L, R, SquareFree => x)
 Inputs
  L: List
   $\{h(0), h(1), \ldots, h(d)\}$, the values of a valid Hilbert function for $R$ for degrees $0\ldots d$.
  R: PolynomialRing
  BoundGenerators => ZZ
   a degree bound on the monomial generators
  FirstBetti => ZZ
   a specified first total Betti number
  GradedBettis => List
   if the form $\{b_0, b_1, \ldots, b_d\}$, where $b_i$ is the first graded Betti numbers with degree $i$.
  SquareFree => Boolean
   whether or not to consider squarefree monomial ideals only
 Outputs
   :List
    all ideals $I$ of $R$ that satisfy $HF(R/I, i) = h(i)$ for all $0\le i\le d$
 Description
  Text
   For example, count the monomial ideals in $\mathbb{Q}[x,y,z]$, generated in degrees up to 5, whose Hilbert
   function begins with $\{1, 3, 6, 5, 4, 4\}$:
  Example
   R = QQ[x,y,z]; L = {1, 3, 6, 5, 4, 4};

   M = monomialIdealsWithHilbertFunction(L, R); #M

   netList take(M, 5)
  Text
   By default, the degrees of generators are bounded by the length of $L$. A lower bound can be set
   manually with the BoundGenerators option.
  Example
   M = monomialIdealsWithHilbertFunction(L, R, BoundGenerators => 3); #M
   netList take(M, 5)
  Text 
   There is also an option to enumerate squarefree monomial ideals only.
  Example
   S = QQ[a..f]
   
   I = monomialIdealsWithHilbertFunction({1, 6, 19, 45, 84}, S, SquareFree => true); #I

   first random I
  Text
   To specify the total number of minimal generators, use FirstBetti.
  Example
   #monomialIdealsWithHilbertFunction({1, 3, 6, 5, 4, 4}, R, FirstBetti => 5)

   #monomialIdealsWithHilbertFunction({1, 3, 6, 5, 4, 4}, R, FirstBetti => 6)
  Text
   Alternatively, specify the number of minimal generators in each degree using GradedBettis. The
   length of the list of graded (first) Betti numbers should match the length of the partial
   Hilbert function.
  Example
   #monomialIdealsWithHilbertFunction({1, 3, 4, 2, 1}, R, GradedBettis => {0, 0, 2, 2, 1})
  Text
   Notice that the GradedBettis option totally constrains the degrees of generators already, so do 
   not use it with the BoundGenerators option.

   You can combine BoundGenerators with FirstBetti, however, since FirstBetti does not constrain degrees.
  Example
   #monomialIdealsWithHilbertFunction({1, 3, 6, 7, 6, 5, 4, 4, 4}, R, FirstBetti => 6, BoundGenerators => 5)

   #monomialIdealsWithHilbertFunction({1, 3, 6, 7, 6, 5, 4, 4, 4}, R, FirstBetti => 6, BoundGenerators => 4)
  Text
   The SquareFree option can be used with any of the other options.
  Example
   #monomialIdealsWithHilbertFunction({1, 4, 7, 10, 13}, S, SquareFree => true, FirstBetti => 5)

   #monomialIdealsWithHilbertFunction({1, 4, 7, 10, 13}, S, SquareFree => true, BoundGenerators => 3)

   #monomialIdealsWithHilbertFunction({1, 4, 7, 10, 13}, S, SquareFree => true, GradedBettis => {0, 2, 3, 1, 0})
 SeeAlso
  bettiTablesWithHilbertFunction
  hilbertFunct
  isHF
  isSquareFree
  LexIdeals
///

doc ///
 Key
  bettiTablesWithHilbertFunction
  (bettiTablesWithHilbertFunction, List, PolynomialRing)
  [bettiTablesWithHilbertFunction, BoundGenerators]
  [bettiTablesWithHilbertFunction, FirstBetti]
  [bettiTablesWithHilbertFunction, GradedBettis]
  [bettiTablesWithHilbertFunction, SquareFree]
  [bettiTablesWithHilbertFunction, Count]
 Headline
  list or tally all Betti tables that can be obtained from monomial ideals with a particular (partial or complete) Hilbert function
 Usage
  bettiTablesWithHilbertFunction(L, R)
  bettiTablesWithHilbertFunction(L, R, BoundGenerators => a)
  bettiTablesWithHilbertFunction(L, R, FirstBetti => b)
  bettiTablesWithHilbertFunction(L, R, GradedBettis => B)
  bettiTablesWithHilbertFunction(L, R, SquareFree => x)
  bettiTablesWithHilbertFunction(L, R, Count => y)
 Inputs
  L: List
   $\{h(0), h(1), \ldots, h(d)\}$, the values of a valid Hilbert function for $R$ for degrees $0\ldots d$.
  R: PolynomialRing
  BoundGenerators => ZZ
   a degree bound on the monomial generators
  FirstBetti => ZZ
   a specified first total Betti number
  GradedBettis => List
   if the form $\{b_0, b_1, \ldots, b_d\}$, where $b_i$ is the first graded Betti numbers with degree $i$.
  SquareFree => Boolean
   whether or not to consider squarefree monomial ideals only
  Count => Boolean
   whether or not to return the count of unique tables instead of all values
 Outputs
   :List
    all unique Betti tables that can be obtained from monomial ideals of $R$ with Hilbert function beginning with $L$, or
   :Tally
    the same set of Betti tables, with the number of unique monomial ideals producing each one
 Description
  Text
   This function calls @TO monomialIdealsWithHilbertFunction@ with the specified options, determines the
   Betti table of each feasible monomial ideal, then lists or tallies the Betti tables encountered. 
   The Count option does not fundamentally change how the computation is performed, only how the
   results are reported.
  Example
   R = QQ[x,y,z]; L = {1, 3, 5, 5, 4};

   bettiTablesWithHilbertFunction(L, R) --outputs unique tables matching the criteria

   bettiTablesWithHilbertFunction(L, R, Count => true) --tallies distinct ideals giving each table
   
   bettiTablesWithHilbertFunction(L, R, FirstBetti => 5) --only returns tables whose first total betti number is 5
   
   bettiTablesWithHilbertFunction(L, R, GradedBettis => {0, 0, 2, 2, 1}) --only returns tables whose first graded betti numbers match a given sequence
   
   --constrain the degrees of ideal generators
   --can be used with either false or true Count option
   bettiTablesWithHilbertFunction(L, R, BoundGenerators => 3)
   bettiTablesWithHilbertFunction(L, R, BoundGenerators => 3, Count => true) 
  Text
   These methods also work for squarefree ideals
  Example
   S = QQ[a..f]

   bettiTablesWithHilbertFunction({1, 6, 19, 45, 86}, S, SquareFree => true)
  Text
   The next examples show that options can be combined in many ways
  Example
   bettiTablesWithHilbertFunction({1, 3, 6, 7, 6, 5, 4, 4, 4}, R, FirstBetti => 6, BoundGenerators => 5, Count => true)
   bettiTablesWithHilbertFunction({1, 4, 7, 10, 13}, S, SquareFree => true, FirstBetti => 5)
   bettiTablesWithHilbertFunction({1, 4, 7, 10, 13}, S, SquareFree => true, GradedBettis => {0, 2, 3, 1, 0})
 SeeAlso
  monomialIdealsWithHilbertFunction
  (betti,GradedModule)
  BettiTally
  HashTable
  tally
  hilbertFunct
  isHF
  isSquareFree
  LexIdeals
///


doc ///
 Key
  topMinimalPrimesIP
  (topMinimalPrimesIP, MonomialIdeal)
  [topMinimalPrimesIP, KnownDim]
  KnownDim
 Headline
  compute the minimal primes of maximum dimension using integer programming
 Usage
  topMinimalPrimesIP(I)
  topMinimalPrimesIP(I, KnownDim => k)
  topMinimalPrimesIP(I, IgnorePrimes => P)
 Inputs
  I:MonomialIdeal
  KnownDim => ZZ
   the dimension, @TT"k"@, of the ideal
  IgnorePrimes => List
    a list of primes to not include in the result. See @TO IgnorePrimes@.
 Outputs
  L:List
   all minimal associated primes of dimension $k$
 Description
  Text
   If a @TT"KnownDim"@ is not provided, @TT"topMinimalPrimesIP"@ will first
   call {@TO dimensionIP@}($I$) to compute the dimension.
   
   The IP for this function is similar to the @TO degreeIP@ formulation,
   except that rather than count the number of solutions, SCIP
   uses a sparse data structure to enumerate all feasible solutions.
   
   The location of input/output files for SCIP solving is printed
   to the screen by default. To change this, see @TO symbol ScipPrintLevel@.
  Example
   R = QQ[x,y,z,w,v];
   I = monomialIdeal(y^12, x*y^3, z*w^3, z*v*y^10, z*x^10, v*z^10, w*v^10, y*v*x*z*w);
   ScipPrintLevel = 0;
   minimalPrimes(I)
   apply(oo, p -> dim p)
   topMinimalPrimesIP(I)
  Text
   Notice that if the dimension of a monomial ideal is $k$, each
   of the top minimal primes is generated by $n-k$ variables, where $n$
   is the number of variables in the polynomial ring.
 Caveat
  @TT"topMinimalPrimesIP"@ does not verify that a provided 
  @TT"KnownDim"@ is correct. Providing the wrong dimension will 
  result in an incorrect answer or an error.
 SeeAlso
  "PrimaryDecomposition::topComponents(Ideal)"
  degreeIP
  MonomialIntegerPrograms
  symbol ScipPrintLevel
///

doc ///
 Key
  IgnorePrimes
  [topMinimalPrimesIP, IgnorePrimes]
 Headline
  Ignores certain primes when computing top minimal primes.
 Description
  Text
   The option @TO IgnorePrimes@ should be a list of prime ideals.
   If a @TO IgnorePrimes@ is provided, @TO topMinimalPrimesIP@ will not include
   any primes containing those ideals in the computation and will find the
   minimal primes with maximal dimension other than the ignored ones.
  Example
   R = QQ[x,y,z,w,v];
   I = monomialIdeal(y^12, x*y^3, z*w^3, z*v*y^10, z*x^10, v*z^10, w*v^10, y*v*x*z*w);
   ScipPrintLevel = 0;
   L1 = topMinimalPrimesIP I
   L2 = topMinimalPrimesIP(I, IgnorePrimes=>L1)
   minimalPrimes I
 Caveat
  This may not be faster than simply using @TO minimalPrimes@ and counting generators.
///

doc ///
 Key
  loadBuiltinCodimAndDegree
 Headline
  change codim and degree to use the default, built-in methods.
 Usage
  loadBuiltinCodimAndDegree()
 Description
  Text
   When the package gets loaded, codim and degree are replaced with
   @TT"codimensionIP"@ and @TT"degreeIP"@ respectively for a @TT"MonomialIdeal"@.
   @TT"loadBuiltinCodimAndDegree"@ reloads the built-in methods.
  Example
   R = QQ[a,b,c];
   ScipPrintLevel = 1;
   codim(monomialIdeal(a^2, b*a, c*b))
   degree(monomialIdeal(a^2, b*a, c*b))
   loadBuiltinCodimAndDegree();
   codim(monomialIdeal(a^2, b*a, c*b))
   degree(monomialIdeal(a^2, b*a, c*b))
 SeeAlso
  loadSCIPCodimAndDegree
  codimensionIP
  degreeIP
///
doc ///
 Key
  loadSCIPCodimAndDegree
 Headline
  change codim and degree to use the default, built-in methods.
 Usage
  loadBuiltinCodimAndDegree()
 Description
  Text
   When the package gets loaded, codim and degree are replaced with
   @TT"codimensionIP"@ and @TT"degreeIP"@ respectively for a @TT"MonomialIdeal"@.
   @TT"loadSCIPCodimAndDegree"@ can be used to reload the SCIP methods in the
   event that @TO loadBuiltinCodimAndDegree@ was called.
  Example
   R = QQ[a,b,c];
   ScipPrintLevel = 1;
   loadBuiltinCodimAndDegree();
   codim(monomialIdeal(a^2, b*a, c*b))
   degree(monomialIdeal(a^2, b*a, c*b))
   loadSCIPCodimAndDegree();
   codim(monomialIdeal(a^2, b*a, c*b))
   degree(monomialIdeal(a^2, b*a, c*b))
 SeeAlso
  loadBuiltinCodimAndDegree
  codimensionIP
  degreeIP
///

doc ///
    Key
        minimalPrimesIP
        (minimalPrimesIP, MonomialIdeal)
        (minimalPrimesIP, MonomialIdeal, ZZ)
    Headline
        one line description if different from minimalPrimesIP
    Usage
        minimalPrimesIP I
        minimalPrimesIP (I, iterations)
    Inputs
        I:MonomialIdeal
        iterations:ZZ
            how many iterations of topMinimalPrimesIP should be called
    Outputs
        :List
            the minimal primes of I
    Description
        Text
            This is basically an alternative version of @TO minimalPrimes@.            
            
            This function calls @TO topMinimalPrimesIP@ repeatedly, collecting the primes
            and passing them in with @TO IgnorePrimes@. This is repeated @TT"iterations"@ many times
            or until there are no primes remaining. If @TT"iterations"@ is excluded, all minimal primes are returned.
        Example
            R = QQ[x,y,z,w,v];
            I = monomialIdeal(y^12, x*y^3, z*w^3, z*v*y^10, z*x^10, v*z^10, w*v^10, y*v*x*z*w);
            ScipPrintLevel = 0;
            minimalPrimesIP(I, 1)
            minimalPrimesIP I
            minimalPrimes I
    Caveat
        Warning: more than likely, this with take longer than @TO minimalPrimes@ to return the same output.
        It some situations @TO topMinimalPrimesIP@ is much faster than @TO minimalPrimes@, but not all.
    SeeAlso
        minimalPrimes
        topMinimalPrimesIP
        IgnorePrimes
///






doc ///
  Key
    FirstBetti
  SeeAlso
    [monomialIdealsWithHilbertFunction, FirstBetti]
///
doc ///
  Key
    GradedBettis
  SeeAlso
    [monomialIdealsWithHilbertFunction, GradedBettis]
///
doc ///
  Key
    BoundGenerators
  SeeAlso
    [monomialIdealsWithHilbertFunction, BoundGenerators]
///
doc ///
  Key
    SquareFree
  SeeAlso
    [monomialIdealsWithHilbertFunction, SquareFree]
///
doc ///
  Key
    Count
  SeeAlso
    [bettiTablesWithHilbertFunction, SquareFree]
///


-----------
-- tests --
-----------

TEST /// --dim and codim
loadBuiltinCodimAndDegree();
R = QQ[x_1..x_10];
I = monomialIdeal(x_1*x_4*x_7^3,x_1^2*x_8^3,x_1*x_2*x_8^2*x_9,x_1*x_4^2*x_9^2,x_1*x_7^2*x_9^2);
assert(codimensionIP monomialIdeal(I_*) == codim monomialIdeal(I_*))
assert(dimensionIP monomialIdeal(I_*) == dim monomialIdeal(I_*))
J = monomialIdeal(x_3^2*x_5*x_6*x_8,x_4^4*x_9,x_7^2*x_8^2*x_9,x_4*x_5*x_8*x_9^2,x_2^2*x_4*x_10^2);
assert(codimensionIP monomialIdeal(J_*) == codim monomialIdeal(J_*))
assert(dimensionIP monomialIdeal(J_*) == dim monomialIdeal(J_*))
K = monomialIdeal(x_4^5,x_2*x_3*x_5^2*x_7,x_2*x_5*x_7^3,x_2*x_3^2*x_7*x_8,x_1^4*x_9,x_4*x_6*x_8*x_9^2,x_1*x_4^3*x_10,x_1^2*x_5*x_6*x_10,x_3^3*x_7*x_10,x_1^2*x_7*x_9*x_10,x_1*x_5*x_8*x_10^2,x_2*x_7*x_8*x_10^2,x_3^2*x_10^3,x_3*x_9*x_10^3);
assert(codimensionIP monomialIdeal(K_*) == codim monomialIdeal(K_*))
assert(dimensionIP monomialIdeal(K_*) == dim monomialIdeal(K_*))
///

TEST /// --degree 
loadBuiltinCodimAndDegree();
R = QQ[x_1..x_10];
I = monomialIdeal(x_1*x_4*x_7^3,x_1^2*x_8^3,x_1*x_2*x_8^2*x_9,x_1*x_4^2*x_9^2,x_1*x_7^2*x_9^2);
assert(degreeIP monomialIdeal(I_*) == degree monomialIdeal(I_*))
J = monomialIdeal(x_3^2*x_5*x_6*x_8,x_4^4*x_9,x_7^2*x_8^2*x_9,x_4*x_5*x_8*x_9^2,x_2^2*x_4*x_10^2);
assert(degreeIP monomialIdeal(J_*) == degree monomialIdeal(J_*))
K = monomialIdeal(x_4^5,x_2*x_3*x_5^2*x_7,x_2*x_5*x_7^3,x_2*x_3^2*x_7*x_8,x_1^4*x_9,x_4*x_6*x_8*x_9^2,x_1*x_4^3*x_10,x_1^2*x_5*x_6*x_10,x_3^3*x_7*x_10,x_1^2*x_7*x_9*x_10,x_1*x_5*x_8*x_10^2,x_2*x_7*x_8*x_10^2,x_3^2*x_10^3,x_3*x_9*x_10^3);
assert(degreeIP monomialIdeal(K_*) == degree monomialIdeal(K_*))
///

TEST /// --hilbert
R = QQ[x,y,z];
assert(#monomialIdealsWithHilbertFunction({1,2,1,0}, R) == 9)
assert(#monomialIdealsWithHilbertFunction({1,3,4,2,1,0}, R, FirstBetti => 6) == 72)
assert(b = {0,0,2,3,0,1}; Mb = monomialIdealsWithHilbertFunction({1,3,4,2,1,0}, R, GradedBettis => b); #Mb == 30)
R = QQ[x,y,z,w];
assert(#monomialIdealsWithHilbertFunction({1,4,3,1,0}, R) == 244)
assert(all(monomialIdealsWithHilbertFunction({1,4,10,19,31}, R), I -> numgens I == 1))
///

TEST /// -- squarefree and bound generators options
R = QQ[x,y,z,w];
M = monomialIdealsWithHilbertFunction({1,4,5,6,7}, R, BoundGenerators => 2);
SF = monomialIdealsWithHilbertFunction({1,4,5,6,7}, R, BoundGenerators => 2, SquareFree => true);
assert(#SF == 6)
assert(set select(M, isSquareFree) === set SF) 
-- squarefree and first betti
R = QQ[x,y,z,w,v]
sf = monomialIdealsWithHilbertFunction({1,5,10,15}, R, SquareFree => true);
assert(#sf == 252)
assert(all(sf, isSquareFree))
sf7 = monomialIdealsWithHilbertFunction({1,5,10,15}, R, SquareFree => true, FirstBetti => 7);
assert(set sf7 === set select(sf, m -> numgens m == 7))
assert(member(monomialIdeal (x*y, x*z, y*z, y*w, y*v, x*w*v, z*w*v), sf7))
-- squarefree and graded bettis
sf8 = monomialIdealsWithHilbertFunction({1,5,10,15}, R, SquareFree => true, GradedBettis => {0,0,5,2});
assert(#sf8 == 30)
assert(member(monomialIdeal (x*y, x*z, y*z, y*w, y*v, x*w*v, z*w*v), sf8))
///

TEST /// --bettis
R = QQ[x,y,z];
assert(#bettiTablesWithHilbertFunction({1,2,1,0}, R) == 2)
assert(set values bettiTablesWithHilbertFunction({1,2,1,0}, R, Count => true) === set{3, 6})
R = QQ[x,y,z,w];
assert(bettiTablesWithHilbertFunction({1,4,10,19,31}, R) == {new BettiTally from {(0,{0},0) => 1, (1,{3},3) => 1}})
assert(#bettiTablesWithHilbertFunction({1, 4, 7, 10, 13}, R, SquareFree => true, FirstBetti => 4) == 1)
assert(#(values bettiTablesWithHilbertFunction({1, 2, 2, 1}, R, GradedBettis => {0,2,1,1}, Count => true)) == 2)

///

TEST /// --top min primes
R = QQ[x,y,z,w,v];
I = monomialIdeal(x*y*w, x*z*v, y*x, y*z*v);
assert(set(topMinimalPrimesIP(I))===set(minimalPrimes I))
J = monomialIdeal(x^2*y*w^3*z, x*y*z*w*v, y*x^8*v, y^5*z*v, x^10, z^10, v^10);
assert(topMinimalPrimesIP(J) == {monomialIdeal(x,z,v)})
K = monomialIdeal(x^2*y*w^3*z, x*y*z*w*v, y*x^8*v, y^5*z*v, y*x^10, v*z^10, w*v^10);
assert(set(topMinimalPrimesIP(K))===set(select(minimalPrimes(K), p -> 3 == dim p)))
L = monomialIdeal(y^12, x*y^3, z*w^3, z*v*y^10, z*x^10, v*z^10, w*v^10, y*v*x*z*w);
assert(set(topMinimalPrimesIP(L))===set(select(minimalPrimes(L), p -> 2 == dim p)))
///



TEST /// --min primes
R = QQ[x,y,z,w,v];
I = monomialIdeal(x*y*w, x*z*v, y*x, y*z*v);
assert(set(minimalPrimesIP I) === set(minimalPrimes I))
J = monomialIdeal(x^2*y*w^3*z, x*y*z*w*v, y*x^8*v, y^5*z*v, x^10, z^10, v^10);
assert(set(minimalPrimesIP J) === set(minimalPrimes J) )
K = monomialIdeal(x^2*y*w^3*z, x*y*z*w*v, y*x^8*v, y^5*z*v, y*x^10, v*z^10, w*v^10);
assert(set(minimalPrimesIP K) === set(minimalPrimes K) )
L = monomialIdeal(y^12, x*y^3, z*w^3, z*v*y^10, z*x^10, v*z^10, w*v^10, y*v*x*z*w);
assert(set(minimalPrimesIP L) === set(minimalPrimes L) )
///

TEST /// --infeasible IPs in monomialIdeals...
R = QQ[x,y,z];
assert(monomialIdealsWithHilbertFunction({1, 3, 3}, R, FirstBetti => 200) == {})
///


TEST /// --infeasible IPs in bettiTables...
R = QQ[x,y,z];
assert(bettiTablesWithHilbertFunction({1, 3, 3}, R, FirstBetti => 200) == {})
///

TEST /// --infeasible Hilbert functions
R = QQ[x,y,z];
assert(monomialIdealsWithHilbertFunction({1, 3, 7}, R) == {})
assert(monomialIdealsWithHilbertFunction({1, 3, 7}, R, FirstBetti => 200) == {})
assert(bettiTablesWithHilbertFunction({1, 3, 7}, R) == {})
///


end--


