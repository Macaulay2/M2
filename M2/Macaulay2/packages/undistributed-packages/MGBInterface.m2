newPackage(
        "MGBInterface",
        Version => "0.1", 
        Date => "12 April 2013",
        Authors => {{Name => "Mike Stillman", 
                  Email => "", 
                  HomePage => ""}},
        Headline => "experimental mathicgb interface, not meant for general use",
        PackageExports => {"ExampleIdeals"},
        Configuration => { "path" => ""
	        },
        DebuggingMode => false
        )

debug Core
-- The code here is that part of Issac2012Examples.m2 which
-- deals with actually running and obtaining the examples
--
-- It also has the routines from there which involve writing example
-- files for other computer algebra systems.

export {"write",
     "toClassic",
     "displayit",
     "toABC",
     "makeExampleFiles",
     "createExamples",
     "helpMGB",
     "mgbStr",
     "MGB",
     "MGBF4",
     "runMGB",
     "doMGB",
     "testRawMGB",
     "testMGB"
     }

-- NOTE: the absolute path where 'mgb' lives should be put into the .init file for MGBInterface
--  inside the .Macaulay2 directory (or, on the mac: the ~/Library/Application Support/Macaulay2/ folder)
path'mgb = (options MGBInterface).Configuration#"path"
if path'mgb === "" then path'mgb = prefixDirectory | currentLayout#"programs"

write = method()
write Ring  := (R) -> (
     -- R should be a polynomial ring
     s1 := char R | " " | numgens R | " 1";
     D := apply(degrees R, first);
     s2 := concatenate(for d in D list (" "|d));
     s1 | s2 | "\n"
     )

write Ring  := (R) -> (
     -- R should be a polynomial ring
     debug Core;
     (mo, tiebreak, positionUpDown, componentLocation) := monomialOrderMatrix R;
     wts := entries mo;
     s1 := char R | " " | numgens R;
     s1a := if tiebreak === Lex then " lex " else " revlex ";
     s1b := toString(#wts) | "\n";
     s2 := concatenate for wtvec in wts list (
          "   " | (concatenate between(" ", wtvec/toString)) | "\n"
          );
     s1 | s1a | s1b | s2
     )

write Ring  := (R) -> (
     -- R should be a polynomial ring
     debug Core; -- for monomialOrderMatrix.
     (mo, tiebreak, positionUpDown, componentLocation) := monomialOrderMatrix R;
     wts := entries mo;
     s1 := char R | " " | numgens R;
     s1a := if tiebreak === Lex then " lex " else " revlex ";
     s1b := toString(#wts) | "\n";
     s2 := concatenate for wtvec in wts list (
          "   " | (concatenate between(" ", wtvec/toString)) | "\n"
          );
     s1 | s1a | s1b | s2
     )

toClassic = method()
toClassic Ideal := (I) -> (
     g := concatenate between("\n", apply(numgens I, i -> replace(///[\*\^]///,"",toString I_i)));
     numgens I | "\n" | g
     )

displayit = method()
displayit Ideal := (I) -> (
     s1 := write ring I;
     s1 | toClassic I
     )

toABC = (I) -> (
     R := ring I;
     S := (coefficientRing R)[vars(0..numgens R-1), MonomialOrder=>(monoid ring I).Options.MonomialOrder];
     sub(I, vars S)
     )

makeFGbString = method()
makeFGbString(Ideal, String) := (I1,fileprefix) -> (
     outfile := fileprefix | ".fgb-gb";
     v := for i from 1 to numgens ring I1 list "x"|i|"y";
     R := (coefficientRing ring I1) (monoid[v]);
     J1 := (map(R,ring I1, vars R))(I1);
     str1 := (replace     ("\\)", "];\n",
       replace("ideal\\(", "V := [",
       replace(",", ",\n    ", 
       replace("y", "]",
       replace("x", "x[", 
       toString J1))))));
     str0 := ///
infolevel[Groebner] := 2;  # allows to see if FGb is being called
with(Groebner);
///;
     str3 := "gb := Basis(V, monord, characteristic=" | (char R) | "):\n";
     str2 := "monord := tdeg(" | concatenate between( ",", for i from 1 to numgens R list "x["|i|"]") | "):\n";
     str4 := ///save gb, "/// | outfile | ///";///;
     str0 | str1 | str2 | str3 | str4)



makeExampleFiles = method()

makeExampleFiles(String, Ideal) := (prefix, I1) -> (
     I1 = toABC I1;
     --(prefix | ".m2") << m2GBString I1 << close;
     (prefix | ".magma") << magmaGBString I1 << close;
     (prefix | ".sing") << singularGBString I1 << close;
     (prefix | ".ideal") << displayit I1 << close; 
     --(prefix | "-divmask.ideal") << displayit I1 << close;
     (prefix | ".fgb") << makeFGbString(I1, prefix) << close;
     )

makeExampleFiles(String, List) := (prefixDir, L) -> (
     -- L is a list of (projectName, String evaluating to an Ideal)
     -- This creates the example files in the given directory
     makeDirectory prefixDir;
     for ell in L do (
          example := prefixDir | "/" | ell#0;
          J := value ell#1;
          << "making " << example << endl;
          makeExampleFiles(example, J)
          );
     )

createExamples = method()
createExamples(String, String, String) := (outputPrefixDir, shellSuffix, inputExampleFile) -> (
  -- example uses
  -- outputPrefixDir = "~/src/M2-git/M2/Macaulay2/packages/MGBInterface/examples/foo"
  -- shellSuffix = "-grevlex-level1"
  -- inputExampleFile = "~/src/M2-git/M2/Macaulay2/packages/MGBInterface/gb-examples.m2"

  Hexamples := getExampleFile inputExampleFile;
  makeExampleFiles(outputPrefixDir, values Hexamples);
  

  -- make shell file to run examples in MGB, F4 reduction
  fil := openOut(outputPrefixDir|"/runMGBF4"|shellSuffix|".sh");
  -- fil << "set -x" << endl;
  for v in (values Hexamples)/first do (
       fil << "echo doing example " << v << endl;
       fil << "time mgb gb " << v << " -red 26 -log F4" << endl;
       );
  close fil;

  -- make shell file to run examples in MGB
  fil = openOut(outputPrefixDir|"/runMGB"|shellSuffix|".sh");
  -- fil << "set -x" << endl;
  for v in (values Hexamples)/first do (
       fil << "echo doing example " << v << endl;
       fil << "time mgb gb " << v << " " << endl;
       );
  close fil;

  -- make shell file to run examples
  fil = openOut(outputPrefixDir|"/runMAGMA"|shellSuffix|".sh");
  -- fil << "set -x" << endl;
  for v in (values Hexamples)/first do (
       fil << "echo doing example " << v << endl;
       fil << "time magma <" << v << ".magma" << endl;
       );
  close fil;
  
  fil = openOut(outputPrefixDir|"/runSINGULAR"|shellSuffix|".sh");
  fil << "set -x" << endl;
  for v in (values Hexamples)/first do (
       fil << "echo doing example " << v << endl;
       fil << "time Singular <" << v << ".sing" << endl;
       );
  close fil;
);

createExamples(String, String, String) := (outputPrefixDir, exampleSetName, inputExampleFile) -> (
  -- example uses
  -- outputPrefixDir = "~/src/M2-git/M2/Macaulay2/packages/MGBInterface/examples/foo"
  -- exampleSetName = "grevlex-level1"
  -- inputExampleFile = "~/src/M2-git/M2/Macaulay2/packages/MGBInterface/gb-examples.m2"

  -- This creates, in the given directory, the following files:
  -- exampleSetName-exampleName.magma
  -- exampleSetName-exampleName.sing
  -- exampleSetName-exampleName.mgb
  -- exampleSetName-MAGMA.sh
  -- exampleSetName-SING.sh
  -- exampleSetName-MGB.sh
  -- exampleSetName-MGBF4.sh
  -- exampleSetName-RUN.sh
  -- ./make-timings <exampleSetName>
  
  Hexamples := getExampleFile inputExampleFile;
  makeExampleFiles(outputPrefixDir, values Hexamples);

  -- make shell file to run examples in MGB, F4 reduction
  fil := openOut(outputPrefixDir|"/"|exampleSetName|"-MGBF4.sh");
  -- fil << "set -x" << endl;
  for v in (values Hexamples)/first do (
       eg := exampleSetName|"-"|v;
       fil << "echo doing example " << v << endl;
       fil << "time mgb gb " << v << " -red 26 -log F4" << endl;
       );
  close fil;
  return;
  -- make shell file to run examples in MGB
  shellSuffix := "";
  fil = openOut(outputPrefixDir|"/runMGB"|shellSuffix|".sh");
  -- fil << "set -x" << endl;
  for v in (values Hexamples)/first do (
       fil << "echo doing example " << v << endl;
       fil << "time mgb gb " << v << " " << endl;
       );
  close fil;

  -- make shell file to run examples
  fil = openOut(outputPrefixDir|"/runMAGMA"|shellSuffix|".sh");
  -- fil << "set -x" << endl;
  for v in (values Hexamples)/first do (
       fil << "echo doing example " << v << endl;
       fil << "time magma <" << v << ".magma" << endl;
       );
  close fil;
  
  fil = openOut(outputPrefixDir|"/runSINGULAR"|shellSuffix|".sh");
  fil << "set -x" << endl;
  for v in (values Hexamples)/first do (
       fil << "echo doing example " << v << endl;
       fil << "time Singular <" << v << ".sing" << endl;
       );
  close fil;
);

----------------------------------------
mgbOptions = hashTable {
     "AutoTailReduce" => {"off", "-autoTailReduce "},
     "AutoTopReduce" => {"on", "-autoTopReduce "},
     "BreakAfter" => {0, "-breakAfter "},
     "DivisorLookup" => {2, "-divisorLookup"},
     "Log" => {"", "-log "},
     "MemoryQuantumForReducer" => {1048576, "-memoryQuantumForReducer "},
     "Module" => {"off", "-module"},
     "MonomialTable" => {2, "-monomialTable "},
     "Output" => {"off", "-outputResult "},
     "PreferSparseReducers" => {"on", "-preferSparseReducers "},
     "PrintInterval" => {0, "-printInterval"},
     "Reducer" => {4, "-reducer "},
     "SPairGroupSize" => {0, "-sPairGroupSize "},
     "SPairQueue" => {0, "-spairQueue "},
     "StoreMatrices" => {0, "-storeMatrices "},
     "ThreadCount" => {1, "-threadCount "},
     "TracingLevel" => {0, "-tracingLevel "}
     }

optionsMGB = join(
     apply(keys mgbOptions, k -> k => null),
     {
          "Algorithm" => null, -- default is "gb", other option is "sig"
          "ProjectName" => null -- default is to choose a temporary file name (w/o the ".ideal" on the end)
          }
     )
runMGB = method(Options => optionsMGB)

--helpMGB = () -> get ("!"|(options runMGB)#"Executable"| " help gb");
helpMGB = () -> get ("!"|path'mgb|"mgb help gb");

mgbStr = method(Options => options runMGB)
mgbStr String := opts -> (projectName) -> (
     alg := if opts#"Algorithm" === "sig" then " sig " else " gb ";
     execString := "time "| path'mgb | "mgb " | alg |projectName|" ";
     -- now add in the options
     for k in keys opts do (
          if mgbOptions#?k and opts#k =!= null then (
               execString = execString | " " | mgbOptions#k#1 | toString(opts#k);
               )
          );
     execString
     )

runMGB Ideal := opts -> (J) -> (
     R := ring J;
     projectName := if opts#"ProjectName" === null then temporaryFileName() else opts#"ProjectName";
     (projectName |  ".ideal") << displayit toABC J << endl << close;
     runMGB(projectName, opts)
     )

runMGB String := opts -> (projectName) -> (
     execStr := mgbStr(projectName, opts);
     << "exec string: " << execStr << endl;
     run execStr
     )

-- The following routine HAS NOT BEEN TESTED YET (12 April 2013).  The
-- format has likely changed since the last time we have used this routine.

-- read polynomials from the file: <stem>.gb
-- return a list of these polynomials
-- ASSUMPTION: R is a ring in variables an initial sequence of a-zA-Z
-- CAVEAT: if we have more than 52 variables, this will not work
readPolys = method()
readPolys(String,Ring) := (stem,R) -> (
     use R;
     L := lines get (stem | ".gb");
     if #L === 0 then error "expected some polynomials";
     if match("^--",L#0)
     then (
	  -- signature gb output: first line is ignored, other lines have: <i> <sig:monomial> <poly>
	  -- Here we create the <poly> into a list 
	       L = drop(L, 1);
     	       L1 := apply(L, a -> separateRegexp("[[:space:]]+", a));
     	       L2 := apply(L1, a -> {a#1, value ("poly\""|a#2|"\"")});
	       L2/last)
     else (
         apply(lines get (stem | ".gb"), a -> value ("poly\""|a|"\""))
	 )
     )

doMGB = method(Options => options runMGB)
doMGB Ideal := opts -> (J) -> (
     projectName := temporaryFileName();
     (projectName |  ".ideal") << displayit J << endl << close;
     execStr := mgbStr(projectName, opts);
     << "exec string: " << execStr << endl;
     retval := run execStr;
     if retval != 0 then error "Error running mgb command";
     << "mgb completed, now reading in polynomials" << endl;
     time readPolys(projectName, ring J)
     )

MGB = method(Options => {"Reducer"=>null, "Threads"=>0, "SPairGroupSize"=>0,"Log"=>""})
  -- possible values for Reducer: "Classic", "F4",  (0,1)
  -- see 'mgb help logs' for format of the Logs argument.
MGB Ideal := opts -> (I) -> (
     reducer := if opts#"Reducer" === null then 0
                else if instance(opts#"Reducer", ZZ) then opts#"Reducer"
                else if opts#"Reducer" === "F4" then 1
                else if opts#"Reducer" === "Classic" then 0
                else error ///Expected "F4" or "Classic" as reducer type///;
     spairGroupSize := if instance(opts#"SPairGroupSize", ZZ) then opts#"SPairGroupSize"
                else error "expected an integer for SPairGroupSize";
     nthreads := if instance(opts#"Threads", ZZ) then opts#"Threads"
                else error "expected an integer for number of threads to use";
     log := if instance(opts#"Log", String) then opts#"Log"
                else error "Log expects a string argument, e.g. \"all\" or \"F4\"";
     rawgb := rawMGB(raw gens I, reducer, spairGroupSize, nthreads, log);
     flatten entries map(ring I, rawgb)
     )
     
MGBF4 = method(Options => options MGB)
MGBF4 Ideal := opts -> (I) -> MGB(I, opts, "Reducer"=>"F4")

testRawMGB = method()
testRawMGB Ideal := (I) -> (
     time G2 := flatten entries gens if isHomogeneous I then gb(I, Algorithm=>LinearAlgebra) else gb I;
     time G3 := MGB I; -- flatten entries map(ring I, rawMGB(raw gens I, 0, 1, ""));
     time G4 := MGBF4 I; -- flatten entries map(ring I, rawMGB(raw gens I, 1, 1, ""));
     assert(#G2 == #G3);
     assert(#G2 == #G4);
     lt2 := monomialIdeal(G2/leadTerm);
     lt3 := monomialIdeal(G3/leadTerm);
     lt4 := monomialIdeal(G4/leadTerm);
     assert(#G2 == numgens lt2);
     assert(#G2 == numgens lt3);
     assert(#G2 == numgens lt4);
     assert(lt2 == lt3);
     assert(lt2 == lt4);
     gb2 := forceGB matrix{G2};
     gb3 := forceGB matrix{G3};
     gb4 := forceGB matrix{G4};
     assert((gens gb2) % gb3 == 0);
     assert((gens gb3) % gb2 == 0);
     assert((gens gb2) % gb4 == 0);
     assert((gens gb4) % gb2 == 0);
     assert((gens gb3) % gb4 == 0);
     assert((gens gb4) % gb3 == 0);
     )
testRawMGB List := (L) -> (
     -- L should be a list of (String, String)
     -- where the first string is the name, and the second should evaluate to
     -- an ideal in the variables a,b,c,...
     for e in L do (
          << "testing: " << e#0 << endl;
          J := value e#1;
          testRawMGB J
          );
     )

testMGB = method()
testMGB Ideal := (I) -> (
     time G1 := doMGB I;
     time G2 := flatten entries gens if isHomogeneous I then gb(I, Algorithm=>LinearAlgebra) else gb I;
     --time G2 := flatten entries gens gb I;
     time G3 := flatten entries map(ring I, rawMGB(raw gens I, 0, 1, ""));
     time G4 := flatten entries map(ring I, rawMGB(raw gens I, 1, 1, ""));
     assert(#G1 == #G2);
     assert(#G1 == #G3);
     assert(#G1 == #G4);
     lt1 := monomialIdeal(G1/leadTerm);
     lt2 := monomialIdeal(G2/leadTerm);
     lt3 := monomialIdeal(G3/leadTerm);
     lt4 := monomialIdeal(G4/leadTerm);
     assert(#G1 == numgens lt1);
     assert(#G1 == numgens lt2);
     assert(#G1 == numgens lt3);
     assert(#G1 == numgens lt4);
     assert(lt1 == lt2);
     assert(lt1 == lt3);
     assert(lt1 == lt4);
     gb1 := forceGB matrix{G1};
     gb2 := forceGB matrix{G2};
     gb3 := forceGB matrix{G3};
     gb4 := forceGB matrix{G4};
     assert((gens gb1) % gb2 == 0);
     assert((gens gb2) % gb1 == 0);
     assert((gens gb1) % gb3 == 0);
     assert((gens gb3) % gb1 == 0);
     assert((gens gb1) % gb4 == 0);
     assert((gens gb4) % gb1 == 0);
     )
testMGB List := (L) -> (
     -- L should be a list of (String, String)
     -- where the first string is the name, and the second should evaluate to
     -- an ideal in the variables a,b,c,...
     for e in L do (
          << "testing: " << e#0 << endl;
          J := value e#1;
          testMGB J
          );
     )

--SLOWER = (str) -> TEST str
SLOWER = (str) -> null
BENCHMARK = (str) -> null
-------------------------------------------------------------
TEST ///
  -- test of MGB, MGBF4, for a small example, with a number of monomial orders
  needsPackage "MGBInterface"
  R = ZZ/101[a..d]
  I = ideal(a^2*b-c^2-1, 2*a*d-c, a^4-1)
  g1 = gens gb I
  g2 = gens forceGB matrix {MGB I}
  g3 = gens forceGB matrix {MGBF4 I}
  assert(g1 == g2 and g2 == g3)

  R1 = ZZ/101[a..d, MonomialOrder=>Lex]
  I1 = sub(I,R1)
  g1 = gens gb I1
  g2 = gens forceGB matrix {MGB I1}
  g3 = gens forceGB matrix {MGBF4 I1}
  assert(g1 == g2 and g2 == g3)

  R1 = ZZ/101[a..d, MonomialOrder=>{1,3}]
  I1 = sub(I,R1)
  g1 = gens gb I1
  g2 = gens forceGB matrix {MGB I1}
  g3 = gens forceGB matrix {MGBF4 I1}
  assert(g1 == g2 and g2 == g3)

  R1 = ZZ/101[a..d, MonomialOrder=>{GRevLex=>{1,4,7,10}}]
  I1 = sub(I,R1)
  g1 = gens gb I1
  g2 = gens forceGB matrix {MGB I1}
  g3 = gens forceGB matrix {MGBF4 I1}
  assert(g1 == g2 and g2 == g3)

  R1 = ZZ/101[a..d, MonomialOrder=>{Weights=>{1,3,2,1}, Lex}]
  I1 = sub(I,R1)
  g1 = gens gb I1
  g2 = gens forceGB matrix {MGB I1}
  g3 = gens forceGB matrix {MGBF4 I1}
  assert(g1 == g2 and g2 == g3)

  R1 = ZZ/101[a..d, MonomialOrder=>{
          Weights=>{100,1,1,1}, 
          Weights=>{0,-5,-1,1}, 
          Lex}]
  I1 = sub(I,R1)
  g1 = gens gb I1
  g2 = gens forceGB matrix {MGB I1}
  g3 = gens forceGB matrix {MGBF4 I1}
  assert(g1 == g2 and g2 == g3)
///

TEST ///
  -- test of MGB, MGBF4, for a submodule
  needsPackage "MGBInterface"
  R = ZZ/101[a..d]
  m = matrix"ad-1,ba-c,c;a,d-1,b-1" 
  gens gb m
  leadTerm oo
  debug Core
  rawMGB(raw m, 0, 1, 0, "")
///

TEST ///
-- running some test files, checking results against M2
restart
load "MGBInterface/f5ex.m2"
loadPackage "MGBInterface"
myexamples = {
     {"weispfenning94", "weispfenning94()"},
     {"liu", "liu()"},
     {"buchberger87", "buchberger87()"},
     {"gerdt93","gerdt93()"},
     {"trinks", "trinks()"},
     {"eco6", "eco6()"},
     {"sym33","sym33()"},
     {"hairer1","hairer1()"},
     {"f633", "f633()"},
     {"katsura5","katsura5()"},
     {"katsura6","katsura6()"},
     {"katsura7","katsura7()"},
     {"uteshevBikker","uteshevBikker()"},
     {"hfeSegers","hfeSegers()"},
     {"gonnet83","gonnet83()"},
     {"f744","f744()"},
     {"schransTroost","schransTroost()"}
     }
time testRawMGB myexamples
time for e in myexamples do (
     << "running: " << e#0 << endl;
     J = value e#1;
     MGB J
     )
time for e in myexamples do (
     << "running: " << e#0 << endl;
     J = value e#1;
     MGBF4 J
     )

-- This next one only works currently if mgb is configured to output .gb files
  -*
    for e in myexamples do (
       << "testing: " << e#0 << endl;
       J = value e#1;
       testMGB J
       )
  *-
-- The following need the mgb executable to be installed in the 
-- given place.
-*
for e in myexamples do (
     << "running: " << e#0 << endl;
     J = value e#1;
     runMGB J
     )
for e in myexamples do (
     << "running: " << e#0 << endl;
     J = value e#1;
     runMGB(J, "Reducer" => 26)
     )
*-
///

TEST ///
-*
  restart
  loadPackage "MGBInterface"
*-
  R = ZZ/101[a..d]
  I = ideal"a2-bc,a3-b3,a4-b4-c4"
  G2 = MGB I
  G3 = MGBF4 I
  G4 = MGBF4(I, "Log"=>"all")
  assert(G3 == G4)

  g1 = gens gb I
  g2 = gens forceGB matrix{G2}
  g3 = gens forceGB matrix{G3}
  assert(g1 == g2)
  assert(g1 == g3)

-*
  -- run this if you wish to test the mgb executable
  makeExampleFiles("first-eg", I)
  runMGB I
  runMGB(I, "Reducer"=>26)
*-

///

TEST ///
-*
  restart
  loadPackage "MGBInterface"
*-
  R = ZZ/101[a..d, MonomialOrder=>Eliminate 1]
  I = ideal"a2-bc, ab-cd"
  G2 = MGB I
  G3 = MGBF4 I
  G4 = MGBF4(I, "Log"=>"all")
  assert(G3 == G4)

  g1 = gens gb I
  g2 = gens forceGB matrix{G2}
  g3 = gens forceGB matrix{G3}
  assert(g1 == g2)
  assert(g1 == g3)
///

TEST ///
  -- rational quartic example
-*
  restart
  loadPackage "MGBInterface"
*-
  R = ZZ/101[s,t,a..d, MonomialOrder=>Eliminate 2, Degrees=>{1,1,4,4,4,4}]
  I = ideal"s4-a,s3t-b,st3-c,t4-d"
  
  G2 = MGB I
  G3 = MGBF4 I
  G4 = MGBF4(I, "Log"=>"all")
  assert(G3 == G4)

  g1 = gens gb I
  g2 = gens forceGB matrix{G2}
  g3 = gens forceGB matrix{G3}
  assert(g1 == g2)
  assert(g1 == g3)
///


SLOWER ///
  -- benchmarking example, jason210
-*
  restart
  load "MGBInterface/f5ex.m2"
  loadPackage "MGBInterface"
*-
  J1 = jason210()
  R = ring J1

  time G2 = MGB J1;  -- [mike rMBP; 17 April 2013; 2.9 sec]
  time G3 = MGBF4 J1; -- [mike rMBP; 17 April 2013; 5.7 sec]
  time g1 = gens gb J1; -- [mike rMBP; 17 April 2013; 25.9 sec]
  time G1a = gens gb(ideal J1_*, Algorithm=>LinearAlgebra); -- [mike rMBP; 17 April 2013; 8.0 sec]

  time g1a = gens forceGB G1a;
  assert(g1a == g1)
  time g2 = gens forceGB matrix{G2};
  time g3 = gens forceGB matrix{G3};
  assert(g1 == g2)
  assert(g1 == g3)

-*
  -- To create the example file:  
  prefixDir = "~/src/M2-git/M2/Macaulay2/packages/MGBInterface/examples"
  makeExampleFiles(prefixDir, {{"jason210", "jason210()"}});

  -- run the following in the shell.
  mgb gb jason210
  mgb gb jason210 -reducer 26  -log +all
  mgb gb jason210 -reducer 26 # -log +all
*-
///

TEST ///
  --isaac97-32003 (from bugs/mike/gbB-refactor-gb/gb-elim-examples.m2)
-*
  restart
  loadPackage "MGBInterface"
*-

  R1 = ZZ/32003[w,x,y,z,MonomialOrder => Lex]
  J1 = ideal"
    -2w2+9wx+8x2+9wy+9xy+6y2-7wz-3xz-7yz-6z2-4w+8x+4y+8z+2,
    3w2-5wx+4x2-3wy+2xy+9y2-6wz-2xz+6yz+7z2+9w+7x+5y+7z+5,
    7w2+5wx+2x2+3wy+9xy-4y2-5wz-7xz-5yz-4z2-5w+4x+6y-9z+2,
    8w2+5wx+5x2-4wy+2xy+7y2+2wz-7xz-8yz+7z2+3w-7x-7y-8z+8"
-- UNCOMMENT once Lex is working ok
  MGBF4(J1, "Log"=>"all");  -- FAILS NOW (just doesn't finish)
  time G2 = MGB J1;  -- [mike rMBP; 17 April 2013;   sec]
  time G3 = MGBF4 J1; -- [mike rMBP; 17 April 2013;   sec]
  time g1 = gens gb J1; -- [mike rMBP; 17 April 2013;  sec]

  time g2 = gens forceGB matrix{G2};
  time g3 = gens forceGB matrix{G3};
  assert(g1 == g2)
  assert(g1 == g3)

  R1 = ZZ/32003[w,x,y,z,MonomialOrder => {1,1,1,1}]
  J1 = ideal"
    -2w2+9wx+8x2+9wy+9xy+6y2-7wz-3xz-7yz-6z2-4w+8x+4y+8z+2,
    3w2-5wx+4x2-3wy+2xy+9y2-6wz-2xz+6yz+7z2+9w+7x+5y+7z+5,
    7w2+5wx+2x2+3wy+9xy-4y2-5wz-7xz-5yz-4z2-5w+4x+6y-9z+2,
    8w2+5wx+5x2-4wy+2xy+7y2+2wz-7xz-8yz+7z2+3w-7x-7y-8z+8"
  time G2 = MGB J1;  -- [mike rMBP; 17 April 2013;  .06 sec]
  time G3 = MGBF4 J1; -- [mike rMBP; 17 April 2013;  1.8 sec]
  time g1 = gens gb J1; -- [mike rMBP; 17 April 2013; .009 sec]

  time g2 = gens forceGB matrix{G2};
  time g3 = gens forceGB matrix{G3};
  assert(g1 == g2)
  assert(g1 == g3)
  
  R1 = ZZ/32003[w,x,y,z,MonomialOrder => Eliminate 3]
  J1 = sub(J1, R1)

  time G2 = MGB J1;  -- [mike rMBP; 17 April 2013;  .0003 sec]
  time G3 = MGBF4 J1; -- [mike rMBP; 17 April 2013;  .003 sec]
  time g1 = gens gb J1; -- [mike rMBP; 17 April 2013; .009 sec]

  time g2 = gens forceGB matrix{G2};
  time g3 = gens forceGB matrix{G3};
  assert(g1 == g2)
  assert(g1 == g3)
///

SLOWER ///
  --joswig-101 (from gbB-refactor-gb/gb-elim-examples.m2)
-*
  restart
  loadPackage "MGBInterface"
*-
  R1 = ZZ/101[x4,x3,x2,x1,s,t,MonomialOrder=>Eliminate 4]
  J1 = ideal (
       1 + s^2  * x1 * x3 + s^8 * x2 * x3 + s^19 * x1 * x2 * x4,
       x1 + s^8 * x1 * x2 * x3 + s^19 * x2 * x4,
       x2 + s^10 * x3 * x4 + s^11 * x1 * x4,
       x3 + s^4 * x1 * x2 + s^19 * x1 * x3 * x4 + s^24 * x2 * x3 * x4,
       x4 + s^31 * x1 * x2 * x3 * x4
       )
  time G2 = MGB J1;  -- [mike rMBP; 16 April 2013; SLOW > 14 minutes]
  time G3 = MGBF4 J1; -- [mike rMBP; 17 April 2013; 35 sec]
  time g1 = gens gb J1; -- [mike rMBP; 17 April 2013; 298 sec]

  --time g2 = gens forceGB matrix{G2};  -- add back in once it is faster
  time g3 = gens forceGB matrix{G3};
  --assert(g1 == g2) -- add back in once g2 can be computed
  assert(g1 == g3)
///

SLOWER ///
  -- benchmarking example, hilbertkunz1
-*
  restart
  load "MGBInterface/f5ex.m2"
  loadPackage "MGBInterface"
*-
  J1 = hilbertkunz1() -- #GB=150 #monoms=454535

  time G2 = MGB J1;  -- [mike rMBP; 17 April 2013;  4.9 sec]
  time G3 = MGBF4 J1; -- [mike rMBP; 17 April 2013;  1.7 sec]
  time g1 = gens gb J1; -- [mike rMBP; 17 April 2013; 86.4 sec]
  time G1a = gens gb(ideal J1_*, Algorithm=>LinearAlgebra); -- [mike rMBP; 17 April 2013; 37.5 sec]

  time g1a = gens forceGB G1a;
  time g2 = gens forceGB matrix{G2};
  time g3 = gens forceGB matrix{G3};
  assert(g1a == g1)
  assert(g1 == g2)
  assert(g1 == g3)

-*
  -- to make the example file  
  prefixDir = "~/src/M2-git/M2/Macaulay2/packages/MGBInterface/examples"
  makeExampleFiles(prefixDir, {{"hilbertkunz1", "hilbertkunz1()"}});

  -- run the following in the shell
  mgb gb hilbertkunz1 -reducer 26  -log +all
  mgb gb hilbertkunz1 -reducer 26
  magma <hilbertkunz1.magma [mike rMBP; Magma V2.18-11; 4.3 sec]
  Singular <hilbertkunz1.sing [mike rMBP; Singular 3-1-5; 4.1 sec]
*-
///

SLOWER ///
  -- benchmarking example, hilbertkunz2
-*
  restart
  load "MGBInterface/f5ex.m2"
  loadPackage "MGBInterface"
*-
  -- #vars=4, #gens=5,
  J1 = hilbertkunz2() -- #GB=7471 #monoms=

  time G2 = MGB J1;  -- [mike rMBP; 17 April 2013; > 25 minutes, about 1 GB when killed.]
  time G3 = MGBF4 J1; -- [mike rMBP; 17 April 2013; 398.2 sec] #monoms=54,252,161
  time g1 = gens gb J1; -- [mike rMBP; 17 April 2013;  sec]
  time G1a = gens gb(ideal J1_*, Algorithm=>LinearAlgebra); -- [mike rMBP; 17 April 2013;  sec]

  time g1a = gens forceGB G1a;
  time g2 = gens forceGB matrix{G2};
  time g3 = gens forceGB matrix{G3};
  assert(g1a == g1)
  assert(g1 == g2)
  assert(g1 == g3)

-*
  -- to make the example file  
  prefixDir = "~/src/M2-git/M2/Macaulay2/packages/MGBInterface/examples"
  makeExampleFiles(prefixDir, {{"hilbertkunz2", "hilbertkunz2()"}});

  -- run the following in the shell
  mgb gb hilbertkunz2 -reducer 26  -log +all
  time mgb gb hilbertkunz2 -reducer 26 -log F4 -threa 4 # [mike rMBP; 244.2 sec] about 4 GB
  magma <hilbertkunz2.magma [mike rMBP; Magma V2.18-11; 772.629 sec] # Total memory usage: 2416.66MB
  Singular <hilbertkunz2.sing [mike rMBP; Singular 3-1-5; 3786 sec] #monoms=54,976,568
*-
///

BENCHMARK ///
  -- benchmarking example, hcyclic8
-*
  restart
  load "MGBInterface/f5ex.m2"
  loadPackage "MGBInterface"
*-
  J1 = (hcyclicn 8)() -- #GB: 3626

  -- vars=8, numgens=8, homogeneous.
  -- #gb=1182, topdegree=29, #monoms=676,400
  time G2 = MGB J1;  --    [mike rMBP; 17 April 2013; 86.6 sec] #monoms=757,245
  time G3 = MGBF4 J1; --   [mike rMBP; 17 April 2013; 3.0  sec] #monoms=676,400
  time g1 = gens gb J1; -- [mike rMBP; 17 April 2013;  416 sec] #monoms=676,400
  time G1a = gens gb(ideal J1_*, Algorithm=>LinearAlgebra); -- [mike rMBP; 17 April 2013; 7.6 sec], #monoms=676,400

  time g1a = gens forceGB G1a;
  time g2 = gens forceGB matrix{G2};
  time g3 = gens forceGB matrix{G3};
  assert(g1a == g2)
  assert(g1a == g3)

  -- now try runMGB with the signature based algorithm, f4-parallel
  J2 = trim J1;
  runMGB(J2, "Algorithm"=>"sig") -- [mike rMBP; 17 April 2013; 410 sec] #monoms in sig basis: 3,314,052
  runMGB(J1, "Reducer"=>26) --      [mike rMBP; 17 April 2013; 3.7 sec] #monoms=676,400
-*
  -- to make the example file  
  prefixDir = "~/src/M2-git/M2/Macaulay2/packages/MGBInterface/examples"
  makeExampleFiles(prefixDir, {{"hcyclic8", "(hcyclicn 8)()"}});
  makeExampleFiles(prefixDir, {{"hcyclic8-trimmed", "trim ((hcyclicn 8)())"}});

  -- run the following in the shell
  time mgb gb hcyclic8 -reducer 26 -thread 4 # [mike rMBP;                 2.8 sec]
  magma <hcyclic8.magma                      # [mike rMBP; Magma V2.18-11; 2.4 sec]
  Singular <hcyclic8.sing                    # [mike rMBP; Singular 3-1-5; 32.2 sec] #monoms=757,035

  time mgb gb hcyclic8-trimmed -reducer 26 -thread 4      # [mike rMBP;                 2.7 sec]
  time magma <hcyclic8-trimmed.magma                      # [mike rMBP; Magma V2.18-11; 2.6 sec]
  time Singular <hcyclic8-trimmed.sing                    # [mike rMBP; Singular 3-1-5; 32.3 sec] #monoms=757,035
*-
///

BENCHMARK ///
  -- benchmarking example, yang1
-*
  restart
  load "MGBInterface/f5ex.m2"
  loadPackage "MGBInterface"
*-
  J1 = yang1(); -- #GB: 

  -- vars=48, numgens=66, homogeneous.
  -- #gb=4761, topdegree=5, #monoms=54,324
  time G2 = MGB J1;  --    [mike rMBP; 17 April 2013; 4.1 sec] #monoms=54,324
  time G3 = MGBF4 J1; --   [mike rMBP; 17 April 2013;  sec] #monoms=
  time g1 = gens gb J1; -- [mike rMBP; 17 April 2013; 34.0 sec]
  time G1a = gens gb(ideal J1_*, Algorithm=>LinearAlgebra); -- [mike rMBP; 17 April 2013; 35.1 sec]

  time g1a = gens forceGB G1a;
  time g2 = gens forceGB matrix{G2};
  time g3 = gens forceGB matrix{G3};
  assert(g1a == g2)
  assert(g1a == g3)

  -- now try runMGB with the signature based algorithm, f4-parallel
  runMGB(J1, "Algorithm"=>"sig") -- [mike rMBP; 17 April 2013;  sec] #monoms in sig basis:
  runMGB(J1, "Reducer"=>26) --      [mike rMBP; 17 April 2013;  sec] #monoms=
-*
  -- to make the example file  
  prefixDir = "~/src/M2-git/M2/Macaulay2/packages/MGBInterface/examples"
  makeExampleFiles(prefixDir, {{"yang1", "yang1()"}});

  -- run the following in the shell
  time mgb gb yang1 -reducer 26 -thread 4 # [mike rMBP;                  sec]
  magma <yang1.magma                      # [mike rMBP; Magma V2.18-11; 19.4 sec]
  Singular <yang1.sing                    # [mike rMBP; Singular 3-1-5; 45.6 sec] #monoms=54,324
*-
///

BENCHMARK ///
  -- benchmarking example, cyclic8
-*
  restart
  load "MGBInterface/f5ex.m2"
  loadPackage "MGBInterface"
*-
  J1 = (cyclicn 8)() -- #GB: 

  -- vars=8, numgens=8, inhomogeneous.
  -- #gb=372, topleaddegree=14, #monoms=93,490
  time G2 = MGB J1;  --    [mike rMBP; 17 April 2013;  sec] #monoms=
  time G3 = MGBF4 J1; --   [mike rMBP; 17 April 2013; 4.1 sec] #monoms=
  time g1 = gens gb J1; -- [mike rMBP; 17 April 2013;  sec] #monoms=

  time g2 = gens forceGB matrix{G2};
  time g3 = gens forceGB matrix{G3};
  assert(g1 == g2)
  assert(g1 == g3)

  -- now try runMGB with the signature based algorithm, f4-parallel
  J2 = trim J1;
  runMGB(J2, "Algorithm"=>"sig") -- [mike rMBP; 17 April 2013;  sec] #monoms in sig basis: 
  runMGB(J1, "Reducer"=>26) --      [mike rMBP; 17 April 2013;  sec] #monoms=
-*
  -- to make the example file  
  prefixDir = "~/src/M2-git/M2/Macaulay2/packages/MGBInterface/examples"
  makeExampleFiles(prefixDir, {{"cyclic8", "(cyclicn 8)()"}});

  -- run the following in the shell
  time mgb gb cyclic8 -reducer 26 -thread 4 # [mike rMBP;                  sec]
  magma <cyclic8.magma                      # [mike rMBP; Magma V2.18-11;  sec]
  Singular <cyclic8.sing                    # [mike rMBP; Singular 3-1-5;  sec] #monoms=
*-
///

BENCHMARK ///
  -- This one is way too easy over grevlex, so do it over Lex
  -- sottile-Y^8Y2 (although the desired one is over QQ)
  -- comes from Sottile's large Schubert problem program
-*
  restart
  loadPackage "MGBInterface"
*-

  R1 = ZZ/32003[x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, MonomialOrder=>Lex];
  J1 = ideal(
       -10675*x2*x3+10675*x1*x4+7190*x2*x5-110*x4*x5-7190*x1*x6+110*x3*x6+8758*x2*x7+11841*x4*x7-1650*x6*x7-8758*x1*x8-11841*x3*x8+1650*x5*x8+1417*x2*x9+3358*x4*x9-14403*x6*x9+7253*x8*x9-1417*x1*x10-3358*x3*x10+14403*x5*x10-7253*x7*x10+8660*x1+7805*x2+10937*x3+10748*x4-1205*x5+13636*x6-12522*x7-7976*x8-8372*x9-12786*x10+228,
       -9601*x2*x3+9601*x1*x4+7361*x2*x5+2560*x4*x5-7361*x1*x6-2560*x3*x6-2977*x2*x7-7936*x4*x7+6144*x6*x7+2977*x1*x8+7936*x3*x8-6144*x5*x8-11638*x2*x9+12057*x4*x9+6556*x6*x9+8345*x8*x9+11638*x1*x10-12057*x3*x10-6556*x5*x10-8345*x7*x10+9792*x1-4819*x2+5165*x3+15130*x4-4309*x5-9735*x6-8639*x7+9868*x8-10882*x9+11975*x10-3263,
       8001*x2*x3-8001*x1*x4-8001*x2*x5-625*x4*x5+8001*x1*x6+625*x3*x6+8626*x2*x7+625*x4*x7-5647*x6*x7-8626*x1*x8-625*x3*x8+5647*x5*x8-9251*x2*x9+5022*x4*x9+5647*x6*x9+4177*x8*x9+9251*x1*x10-5022*x3*x10-5647*x5*x10-4177*x7*x10+6440*x1-4229*x2-11510*x3+10669*x4+14001*x5+9824*x6+13699*x7+4177*x8+9522*x9+9522*x10+5232,
       -x1+9144*x3+11103*x5-12316*x7-1053*x9+4271,
       -x2+9144*x4+11103*x6-12316*x8-1053*x10-10364,
       9144*x2*x3-9144*x1*x4+11103*x2*x5-11103*x1*x6-12316*x2*x7+12316*x1*x8-1053*x2*x9+1053*x1*x10+10364*x1+4271*x2,
       -x2*x3+x1*x4-11103*x4*x5+11103*x3*x6+12316*x4*x7-12316*x3*x8+1053*x4*x9-1053*x3*x10-10364*x3-4271*x4,
       x2*x5-9144*x4*x5-x1*x6+9144*x3*x6-12316*x6*x7+12316*x5*x8-1053*x6*x9+1053*x5*x10+10364*x5+4271*x6,
       -x2*x7+9144*x4*x7+11103*x6*x7+x1*x8-9144*x3*x8-11103*x5*x8+1053*x8*x9-1053*x7*x10-10364*x7-4271*x8,
       x2*x9-9144*x4*x9-11103*x6*x9+12316*x8*x9-x1*x10+9144*x3*x10+11103*x5*x10-12316*x7*x10+10364*x9+4271*x10,
       4800*x2*x3-4800*x1*x4-240*x2*x5-240*x4*x5+240*x1*x6+240*x3*x6+252*x2*x7+12*x4*x7+12*x6*x7-252*x1*x8-12*x3*x8-12*x5*x8+6376*x2*x9+6388*x4*x9+6400*x6*x9+6400*x8*x9-6376*x1*x10-6388*x3*x10-6400*x5*x10-6400*x7*x10-11507*x1-12495*x2+7376*x3+13120*x4+656*x5+6720*x6+336*x7+320*x8+16*x9+320*x10-16,
       10312*x2*x3-10312*x1*x4-15239*x2*x5+2821*x4*x5+15239*x1*x6-2821*x3*x6+3800*x2*x7-5283*x4*x7-4079*x6*x7-3800*x1*x8+5283*x3*x8+4079*x5*x8-10512*x2*x9+10921*x4*x9+12449*x6*x9-4539*x8*x9+10512*x1*x10-10921*x3*x10-12449*x5*x10+4539*x7*x10-5949*x1+10010*x2-11335*x3+13502*x4+7634*x5+3539*x6-4503*x7-5097*x8+13141*x9-6098*x10+1727,
       14287*x2*x3-14287*x1*x4+15481*x2*x5-9705*x4*x5-15481*x1*x6+9705*x3*x6+9001*x2*x7-12942*x4*x7-8989*x6*x7-9001*x1*x8+12942*x3*x8+8989*x5*x8+2459*x2*x9+4304*x4*x9-937*x6*x9-6588*x8*x9-2459*x1*x10-4304*x3*x10+937*x5*x10+6588*x7*x10+418*x1-13102*x2-8181*x3+9320*x4+3074*x5+8452*x6-12788*x7+3008*x8-14199*x9-5799*x10+15641,
       15545*x2*x3-15545*x1*x4-12852*x2*x5+15545*x4*x5+12852*x1*x6-15545*x3*x6+8487*x2*x7-12852*x4*x7+15545*x6*x7-8487*x1*x8+12852*x3*x8-15545*x5*x8-15837*x2*x9+8487*x4*x9-12852*x6*x9+15545*x8*x9+15837*x1*x10-8487*x3*x10+12852*x5*x10-15545*x7*x10+6492*x1-4662*x2+4662*x3+15837*x4-15837*x5-8487*x6+8487*x7+12852*x8-12852*x9-15545*x10-15545,
       -8001*x2*x3+8001*x1*x4-10002*x2*x5+12000*x4*x5+10002*x1*x6-12000*x3*x6-6504*x2*x7-13006*x4*x7-10006*x6*x7+6504*x1*x8+13006*x3*x8+10006*x5*x8-14636*x2*x9+2735*x4*x9+5479*x6*x9-13024*x8*x9+14636*x1*x10-2735*x3*x10-5479*x5*x10+13024*x7*x10-2105*x1+932*x2-4194*x3+1856*x4-8352*x5+3694*x6+15380*x7-8654*x8+6940*x9-5398*x10+7712);
  R2 = (ZZ/32003)[gens R1, MonomialOrder=>{10:1}];
  J2 = sub(J1, R2)

  -- vars=10, numgens=15, inhomogeneous.
  -- #gb=, topdegree=, #monoms=
  time G2 = MGB J1;  --    [mike rMBP; 17 April 2013;  sec] #monoms=
  time G3 = MGBF4 J1; --   [mike rMBP; 17 April 2013;  sec] #monoms=
  time g1 = gens gb J1; -- [mike rMBP; 17 April 2013;  sec] #monoms=

  time g2 = gens forceGB matrix{G2};
  time g3 = gens forceGB matrix{G3};
  assert(g1 == g2)
  assert(g1 == g3)

  -- now try runMGB with the signature based algorithm, f4-parallel
  J1t = trim J1;
  runMGB(J1t, "Algorithm"=>"sig") -- [mike rMBP; 17 April 2013;  sec] #monoms in sig basis: 
  runMGB(J1, "Reducer"=>26) --       [mike rMBP; 17 April 2013;  sec] #monoms=
-*
  -- to make the example file  
  prefixDir = "~/src/M2-git/M2/Macaulay2/packages/MGBInterface/examples"
  makeExampleFiles(prefixDir | "/sottile-Y8Y2-lex", J1)

  -- run the following in the shell [I'm not sure how to get lex in these systems!!]
  time mgb gb sottile-Y8Y2-lex -reducer 26 -thread 4 # [mike rMBP;                  sec]
  magma <sottile-Y8Y2-lex.magma                      # [mike rMBP; Magma V2.18-11;  sec]
  Singular <sottile-Y8Y2-lex.sing                    # [mike rMBP; Singular 3-1-5;  sec] #monoms=
*-
///




end
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
TEST ///
-- running some test files, checking results against M2
restart
loadPackage "MGBInterface"
myexamples = {
     {"weispfenning94", "weispfenning94()"},
     {"liu", "liu()"},
     {"buchberger87", "buchberger87()"},
     {"gerdt93","gerdt93()"},
     {"trinks", "trinks()"},
     {"eco6", "eco6()"},
     {"sym33","sym33()"},
     {"hairer1","hairer1()"},
     {"f633", "f633()"},
     {"katsura5","katsura5()"},
     {"katsura6","katsura6()"},
     {"katsura7","katsura7()"},
     {"uteshevBikker","uteshevBikker()"},
     {"hfeSegers","hfeSegers()"},
     {"gonnet83","gonnet83()"},
     {"f744","f744()"},
     {"schransTroost","schransTroost()"}
     }
prefixDir = "~/src/M2-git/M2/Macaulay2/packages/MGBInterface/examples"
eg = (prefixDir|"/"|(myexamples#0#0))
runMGB (prefixDir|"/"|(myexamples#0#0))
///




TEST ///
  -- sottile-Y^8Y2 (although the desired one is over QQ
  R1 = QQ[x1, x2, x3, x4, x5, x6, x7, x8, x9, x10]
  J1 = ideal(
       -(22/3)*x2*x3+(22/3)*x1*x4+(704/9)*x2*x5-110*x4*x5-(704/9)*x1*x6+110*x3*x6-(19558/27)*x2*x7+(3520/3)*x4*x7-1650*x6*x7+(19558/27)*x1*x8-(3520/3)*x3*x8+1650*x5*x8+(530816/81)*x2*x9-(97790/9)*x4*x9+17600*x6*x9-24750*x8*x9-(530816/81)*x1*x10+(97790/9)*x3*x10-17600*x5*x10+24750*x7*x10+(387404864/729)*x1+(14345782/243)*x2-(71728910/81)*x3-(2654080/27)*x4+(13270400/9)*x5+(488950/3)*x6-2444750*x7-264000*x8+3960000*x9+371250*x10+5568750,
       -(1/10)*x2*x3+(1/10)*x1*x4+(31/100)*x2*x5-(6/25)*x4*x5-(31/100)*x1*x6+(6/25)*x3*x6-(721/1000)*x2*x7+(93/125)*x4*x7-(72/125)*x6*x7+(721/1000)*x1*x8-(93/125)*x3*x8+(72/125)*x5*x8+(14911/10000)*x2*x9-(2163/1250)*x4*x9+(1116/625)*x6*x9-(864/625)*x8*x9-(14911/10000)*x1*x10+(2163/1250)*x3*x10-(1116/625)*x5*x10+(864/625)*x7*x10+(5386591/1000000)*x1+(289201/100000)*x2-(867603/125000)*x3-(44733/12500)*x4+(134199/15625)*x5+(12978/3125)*x6-(155736/15625)*x7-(13392/3125)*x8+(160704/15625)*x9+(10368/3125)*x10+124416/15625,
       (1/4)*x2*x3-(1/4)*x1*x4-(1/4)*x2*x5+(15/256)*x4*x5+(1/4)*x1*x6-(15/256)*x3*x6+(49/256)*x2*x7-(15/256)*x4*x7+(225/16384)*x6*x7-(49/256)*x1*x8+(15/256)*x3*x8-(225/16384)*x5*x8-(17/128)*x2*x9+(735/16384)*x4*x9-(225/16384)*x6*x9+(3375/1048576)*x8*x9+(17/128)*x1*x10-(735/16384)*x3*x10+(225/16384)*x5*x10-(3375/1048576)*x7*x10-(931/16384)*x1-(1441/16384)*x2+(21615/1048576)*x3+(255/8192)*x4-(3825/524288)*x5-(11025/1048576)*x6+(165375/67108864)*x7+(3375/1048576)*x8-(50625/67108864)*x9-(50625/67108864)*x10-759375/4294967296,
       -x1+(2/7)*x3-(4/49)*x5+(8/343)*x7-(16/2401)*x9-32/16807,
       -x2+(2/7)*x4-(4/49)*x6+(8/343)*x8-(16/2401)*x10+64/117649,
       (2/7)*x2*x3-(2/7)*x1*x4-(4/49)*x2*x5+(4/49)*x1*x6+(8/343)*x2*x7-(8/343)*x1*x8-(16/2401)*x2*x9+(16/2401)*x1*x10-(64/117649)*x1-(32/16807)*x2,
       -x2*x3+x1*x4+(4/49)*x4*x5-(4/49)*x3*x6-(8/343)*x4*x7+(8/343)*x3*x8+(16/2401)*x4*x9-(16/2401)*x3*x10+(64/117649)*x3+(32/16807)*x4,
       x2*x5-(2/7)*x4*x5-x1*x6+(2/7)*x3*x6+(8/343)*x6*x7-(8/343)*x5*x8-(16/2401)*x6*x9+(16/2401)*x5*x10-(64/117649)*x5-(32/16807)*x6,
       -x2*x7+(2/7)*x4*x7-(4/49)*x6*x7+x1*x8-(2/7)*x3*x8+(4/49)*x5*x8+(16/2401)*x8*x9-(16/2401)*x7*x10+(64/117649)*x7+(32/16807)*x8,
       x2*x9-(2/7)*x4*x9+(4/49)*x6*x9-(8/343)*x8*x9-x1*x10+(2/7)*x3*x10-(4/49)*x5*x10+(8/343)*x7*x10-(64/117649)*x9-(32/16807)*x10,
       -(9/20)*x2*x3+(9/20)*x1*x4+(9/400)*x2*x5+(9/400)*x4*x5-(9/400)*x1*x6-(9/400)*x3*x6-(189/8000)*x2*x7-(9/8000)*x4*x7-(9/8000)*x6*x7+(189/8000)*x1*x8+(9/8000)*x3*x8+(9/8000)*x5*x8+(369/160000)*x2*x9+(189/160000)*x4*x9+(9/160000)*x6*x9+(9/160000)*x8*x9-(369/160000)*x1*x10-(189/160000)*x3*x10-(9/160000)*x5*x10-(9/160000)*x7*x10+(11529/64000000)*x1+(4149/3200000)*x2+(4149/64000000)*x3+(369/3200000)*x4+(369/64000000)*x5+(189/3200000)*x6+(189/64000000)*x7+(9/3200000)*x8+(9/64000000)*x9+(9/3200000)*x10-9/64000000,
       -(7/90)*x2*x3+(7/90)*x1*x4-(329/8100)*x2*x5-(7/1350)*x4*x5+(329/8100)*x1*x6+(7/1350)*x3*x6-(11683/729000)*x2*x7-(329/121500)*x4*x7-(7/20250)*x6*x7+(11683/729000)*x1*x8+(329/121500)*x3*x8+(7/20250)*x5*x8-(371441/65610000)*x2*x9-(11683/10935000)*x4*x9-(329/1822500)*x6*x9-(7/303750)*x8*x9+(371441/65610000)*x1*x10+(11683/10935000)*x3*x10+(329/1822500)*x5*x10+(7/303750)*x7*x10-(323420489/531441000000)*x1+(11148907/5904900000)*x2-(11148907/88573500000)*x3+(371441/984150000)*x4-(371441/14762250000)*x5+(11683/164025000)*x6-(11683/2460375000)*x7+(329/27337500)*x8-(329/410062500)*x9+(7/4556250)*x10+7/68343750,
       -(3/56)*x2*x3+(3/56)*x1*x4-(135/3136)*x2*x5-(27/3136)*x4*x5+(135/3136)*x1*x6+(27/3136)*x3*x6-(4563/175616)*x2*x7-(1215/175616)*x4*x7-(243/175616)*x6*x7+(4563/175616)*x1*x8+(1215/175616)*x3*x8+(243/175616)*x5*x8-(137295/9834496)*x2*x9-(41067/9834496)*x4*x9-(10935/9834496)*x6*x9-(2187/9834496)*x8*x9+(137295/9834496)*x1*x10+(41067/9834496)*x3*x10+(10935/9834496)*x5*x10+(2187/9834496)*x7*x10-(105336855/30840979456)*x1+(3878523/550731776)*x2-(34906707/30840979456)*x3+(1235655/550731776)*x4-(11120895/30840979456)*x5+(369603/550731776)*x6-(3326427/30840979456)*x7+(98415/550731776)*x8-(885735/30840979456)*x9+(19683/550731776)*x10+177147/30840979456,
       (24/35)*x2*x3-(24/35)*x1*x4+(1776/1225)*x2*x5+(24/35)*x4*x5-(1776/1225)*x1*x6-(24/35)*x3*x6+(102024/42875)*x2*x7+(1776/1225)*x4*x7+(24/35)*x6*x7-(102024/42875)*x1*x8-(1776/1225)*x3*x8-(24/35)*x5*x8+(5374176/1500625)*x2*x9+(102024/42875)*x4*x9+(1776/1225)*x6*x9+(24/35)*x8*x9-(5374176/1500625)*x1*x10-(102024/42875)*x3*x10-(1776/1225)*x5*x10-(24/35)*x7*x10+(13597146576/1838265625)*x1-(272709624/52521875)*x2+(272709624/52521875)*x3-(5374176/1500625)*x4+(5374176/1500625)*x5-(102024/42875)*x6+(102024/42875)*x7-(1776/1225)*x8+(1776/1225)*x9-(24/35)*x10-24/35,
       -(1/4)*x2*x3+(1/4)*x1*x4-(17/16)*x2*x5-(9/8)*x4*x5+(17/16)*x1*x6+(9/8)*x3*x6-(217/64)*x2*x7-(153/32)*x4*x7-(81/16)*x6*x7+(217/64)*x1*x8+(153/32)*x3*x8+(81/16)*x5*x8-(2465/256)*x2*x9-(1953/128)*x4*x9-(1377/64)*x6*x9-(729/32)*x8*x9+(2465/256)*x1*x10+(1953/128)*x3*x10+(1377/64)*x5*x10+(729/32)*x7*x10-(269297/4096)*x1+(26281/1024)*x2-(236529/2048)*x3+(22185/512)*x4-(199665/1024)*x5+(17577/256)*x6-(158193/512)*x7+(12393/128)*x8-(111537/256)*x9+(6561/64)*x10+59049/128  );
  loadPackage "MGBInterface"
  R = ZZ/32003[gens R1, MonomialOrder=>{10:1}]
     
  R = ZZ/32003[gens R1, MonomialOrder=>Lex]
  R = ZZ/32003[gens R1]
  J = sub(J1, R)
  debug Core
  time G3 = flatten entries map(ring J, rawMGB(raw gens J, 0, 1, ""));  -- takes > 14 minutes (16 April 2013)
  time G4 = flatten entries map(ring J, rawMGB(raw gens J, 1, 1, "F4"));  -- 36 sec
  
///

TEST ///
  R1 = ZZ/2003 [X_0..X_35, Degrees => {{1}, {1}, {1}, {1}, {1}, {1}, {1}, {1}, {1}, {1}, {1}, {1}, {1}, {1}, {1}, {1}, {1}, {1}, 
	                        {2}, {2}, {2}, {2}, {2}, {2}, {2}, {2}, {2}, {2}, {2}, {2}, {2}, {2}, {2}, {2}, {2}, {2}}, 
    MonomialOrder => {Weights=>{18:1, 18:2}, Weights=>{18:1}, RevLex=>36},
	MonomialSize => 8]
  J1 = ideal (-X_0*X_9-X_1*X_12+X_18,
     -X_0*X_10-X_1*X_13+X_19,
     -X_0*X_11-X_1*X_14+X_20,-X_3*X_9-X_4*X_12+X_21,-X_3*X_10-X_4*X_13+X_22,-X_3*X_11-X_4*X_14+X_23,-X_6*X_9-X_7*X_12+X_24,
     -X_6*X_10-X_7*X_13+X_25,-X_6*X_11-X_7*X_14+X_26,-X_2*X_9-X_1*X_12-X_2*X_12-X_0*X_15-X_1*X_15-X_2*X_15+X_27,
     -X_2*X_10-X_1*X_13-X_2*X_13-X_0*X_16-X_1*X_16-X_2*X_16+X_28,-X_2*X_11-X_1*X_14-X_2*X_14-X_0*X_17-X_1*X_17-X_2*X_17+X_29,
     -X_5*X_9-X_4*X_12-X_5*X_12-X_3*X_15-X_4*X_15-X_5*X_15+X_30,-X_5*X_10-X_4*X_13-X_5*X_13-X_3*X_16-X_4*X_16-X_5*X_16+X_31,
     -X_5*X_11-X_4*X_14-X_5*X_14-X_3*X_17-X_4*X_17-X_5*X_17+X_32,-X_8*X_9-X_7*X_12-X_8*X_12-X_6*X_15-X_7*X_15-X_8*X_15+X_33,
     -X_8*X_10-X_7*X_13-X_8*X_13-X_6*X_16-X_7*X_16-X_8*X_16+X_34,
     -X_8*X_11-X_7*X_14-X_8*X_14-X_6*X_17-X_7*X_17-X_8*X_17+X_35)

  debug Core
  monomialOrderMatrix R1
  time G3 = flatten entries map(ring J1, rawMGB(raw gens J1, 0, 1, ""));  -- 
  time G4 = flatten entries map(ring J1, rawMGB(raw gens J1, 1, 1, "F4Detail"));  -- 
  time g1 = MGB J1;
///


TEST ///
  -- benchmarking example, cyclic8
  restart
  load "MGBInterface/f5ex.m2"
  loadPackage "MGBInterface"
  debug Core
  I = (cyclicn 8)() -- #GB: 
  gbTrace = 1
  time gb(I, Strategy=>LongPolynomial);  364.35 sec, #monoms=668573, #nonminGB=1175
  time gb(I, Algorithm=>Sugarless); -- >500 sec
  time gb(I, Algorithm=>Sugarless, Strategy=>LongPolynomial); -- >500 sec
  time G3 = flatten entries map(ring I, rawMGB(raw gens I, 0, 1, "")); -- 250.77 sec
  time G4 = flatten entries map(ring I, rawMGB(raw gens I, 1, 1, ""));  -- 4.3 sec

  time gens forceGB(matrix{G3}) == gens forceGB(matrix{G4})
  
  prefixDir = "~/src/M2-git/M2/Macaulay2/packages/MGBInterface/examples"
  makeExampleFiles(prefixDir, {{"cyclic8", "(cyclicn 8)()"}});
  -- run the following:
  mgb gb cyclic8 -reducer 26 -thread 1 # -log +all
  mgb gb cyclic8  # -log +all
///

TEST ///
  -- benchmarking example, bayes148
  restart
  load "MGBInterface/f5ex.m2"
  loadPackage "MGBInterface"
  debug Core
  I = bayes148() -- #GB: 3626
  time gb I;  -- 24 seconds, < 80 MB real memory
  time gb(I, Algorithm=>LinearAlgebra); -- 36 seconds, about 774 MB, real memory
  time G3 = flatten entries map(ring I, rawMGB(raw gens I, 0, 1, "")); -- 4.24 sec, < 98 MB
  time G4 = flatten entries map(ring I, rawMGB(raw gens I, 1, 1, "")); -- 176 sec, 2.4 GB
  
  prefixDir = "~/src/M2-git/M2/Macaulay2/packages/MGBInterface/examples"
  makeExampleFiles(prefixDir, {{"bayes148", "bayes148()"}});
  -- run the following:
  mgb gb bayes148 -reducer 26 -thread 1 # -log +all
  mgb gb bayes148  # -log +all
///

TEST ///
  -- benchmarking example, bayes148
  restart
  load "MGBInterface/f5ex.m2"
  loadPackage "MGBInterface"
  debug Core
  I = bayes148() -- #GB: 3626
  time gb I;  -- 24 seconds, < 80 MB real memory
  time gb(I, Algorithm=>LinearAlgebra); -- 36 seconds, about 774 MB, real memory
  time G3 = flatten entries map(ring I, rawMGB(raw gens I, 0)); -- 4.24 sec, < 98 MB
  time G4 = flatten entries map(ring I, rawMGB(raw gens I, 1)); -- 176 sec, 2.4 GB
  
  prefixDir = "~/src/M2-git/M2/Macaulay2/packages/MGBInterface/examples"
  makeExampleFiles(prefixDir, {{"bayes148", "bayes148()"}});
  -- run the following:
  mgb gb bayes148 -reducer 26 -sP 100000000 -log +all
///


TEST ///
  -- benchmarking example, hilbertkunz2
  restart
  load "MGBInterface/f5ex.m2"
  loadPackage "MGBInterface"
  debug Core
  I = hilbertkunz2() -- #GB: 
  time gb I;  -- 
  time gb(I, Algorithm=>LinearAlgebra); -- 
  time G3 = flatten entries map(ring I, rawMGB(raw gens I, 0, 1, "")); -- sec
  time G4 = flatten entries map(ring I, rawMGB(raw gens I, 1, 1, "F4")); --  sec
  time G4 = flatten entries map(ring I, rawMGB(raw gens I, 1, 1, "")); -- sec
  
  prefixDir = "~/src/M2-git/M2/Macaulay2/packages/MGBInterface/examples"
  makeExampleFiles(prefixDir, {{"hilbertkunz2", "hilbertkunz2()"}});
  -- run the following:
  mgb gb hilbertkunz2 -reducer 26  -log # 464.007s, approx 3 GB
  mgb gb hilbertkunz2 -reducer 26 -thre 4 -log # 265.818s
  time mgb gb hilbertkunz2 -reducer 26 -thre 8 -log # 275.4s
  time mgb gb hilbertkunz2 -reducer 26 -thre 1 # 432.3 sec
  magma <hilbertkunz2.magma  # Total time: 772.629 seconds, Total memory usage: 2416.66MB
  
  time mgb gb hilbertkunz2 -log  # seems slow...
  Singular <hilbertkunz2.sing # time=3786 sec, #gb=7471 #monoms=54976568
///

TEST ///
  -- benchmarking example, yang1
  restart
  load "MGBInterface/f5ex.m2"
  loadPackage "MGBInterface"
  debug Core
  I = yang1() -- #GB: 
  time G2 = MGB I;
  time G3 = MGBF4(I, "SPairGroupSize"=>1, "Log"=>"F4");
  time gb I;  -- 37.2 sec
  time gb(I, Algorithm=>LinearAlgebra); -- 36 sec
  time G3 = flatten entries map(ring I, rawMGB(raw gens I, 0, 1, "")); -- 4.02  sec
  time G4 = flatten entries map(ring I, rawMGB(raw gens I, 1, 1, "F4")); -- 171.7 sec (at 1000 spairs per group, time goes way down, I think, to about 24 sec?)
  
  prefixDir = "~/src/M2-git/M2/Macaulay2/packages/MGBInterface/examples"
  makeExampleFiles(prefixDir, {{"yang1", "yang1()"}});
  -- run the following:
  mgb gb yang1 -reducer 26 -sP 100000000 -log +all
  mgb gb yang1 -reducer 26 -sP 1000 # -log +all
///

TEST ///
  -- benchmarking example, random5556 (gb-one-minute.m2)
  restart
  load "MGBInterface/f5ex.m2"
  loadPackage "MGBInterface"
  debug Core
  kk = ZZ/101;
  R1 = kk[a..g, MonomialSize=>8];
  J1 = ideal(50*a^5+12*a^4*b-45*a^3*b^2+29*a^2*b^3+7*a*b^4-8*b^5+26*a^4*c+7*a^3*b*c+21*a^2*b^2*c+48*a*b^3*c+22*b^4*c+28*a^3*c^2-23*a^2*b*c^2-34*a*b^2*c^2-19*b^3*c^2-46*a^2*c^3-14*a*b*c^3+46*b^2*c^3+16*a*c^4+4*b*c^4+36*c^5+37*a^4*d-30*a^3*b*d-11*a^2*b^2*d+7*a*b^3*d+22*b^4*d+34*a^3*c*d-7*a^2*b*c*d+36*a*b^2*c*d+19*b^3*c*d-38*a^2*c^2*d+16*a*b*c^2*d-6*b^2*c^2*d-37*a*c^3*d-27*b*c^3*d-27*c^4*d-50*a^3*d^2-33*a^2*b*d^2+31*a*b^2*d^2-34*b^3*d^2-11*a^2*c*d^2+31*a*b*c*d^2+17*b^2*c*d^2-48*a*c^2*d^2+b*c^2*d^2+17*c^3*d^2+20*a^2*d^3-43*a*b*d^3+30*a*c*d^3+21*b*c*d^3+34*c^2*d^3+18*a*d^4+17*b*d^4-43*c*d^4-25*d^5-28*a^4*e+34*a^3*b*e-44*a^2*b^2*e-8*a*b^3*e+10*b^4*e+32*a^3*c*e-a^2*b*c*e+29*a*b^2*c*e-15*b^3*c*e+42*a^2*c^2*e+28*a*b*c^2*e-23*b^2*c^2*e-27*a*c^3*e+29*b*c^3*e+38*c^4*e-29*a^3*d*e-25*a^2*b*d*e-38*a*b^2*d*e-43*b^3*d*e+30*a^2*c*d*e-34*a*b*c*d*e-31*b^2*c*d*e+35*a*c^2*d*e+49*b*c^2*d*e+10*c^3*d*e-20*a^2*d^2*e+20*a*b*d^2*e+40*b^2*d^2*e-46*a*c*d^2*e-25*b*c*d^2*e-20*c^2*d^2*e-23*a*d^3*e+36*b*d^3*e-38*c*d^3*e-3*d^4*e-31*a^3*e^2+22*a^2*b*e^2-a*b^2*e^2+32*b^3*e^2-9*a^2*c*e^2-2*a*b*c*e^2+44*b^2*c*e^2-40*a*c^2*e^2+44*b*c^2*e^2+7*c^3*e^2+32*a^2*d*e^2+11*a*b*d*e^2-3*b^2*d*e^2-11*a*c*d*e^2-47*b*c*d*e^2+6*c^2*d*e^2-16*a*d^2*e^2-3*b*d^2*e^2-12*c*d^2*e^2+35*d^3*e^2+10*a^2*e^3-48*a*b*e^3+44*b^2*e^3+31*a*c*e^3-47*b*c*e^3-40*c^2*e^3-50*a*d*e^3+18*b*d*e^3-33*c*d*e^3+5*d^2*e^3+43*a*e^4-3*b*e^4+35*c*e^4+34*d*e^4+27*e^5+33*a^4*f+35*a^3*b*f-41*a^2*b^2*f-7*a*b^3*f+26*b^4*f+48*a^3*c*f+30*a^2*b*c*f+16*a*b^2*c*f-13*b^3*c*f-40*a^2*c^2*f-33*a*b*c^2*f-33*b^2*c^2*f+23*a*c^3*f-34*b*c^3*f+42*c^4*f-36*a^3*d*f+2*a^2*b*d*f-23*a*b^2*d*f-13*b^3*d*f-19*a^2*c*d*f+21*a*b*c*d*f-15*b^2*c*d*f+31*a*c^2*d*f-22*b*c^2*d*f-41*c^3*d*f-29*a^2*d^2*f+47*a*b*d^2*f+14*b^2*d^2*f-46*a*c*d^2*f-28*b*c*d^2*f+42*c^2*d^2*f+9*a*d^3*f+35*b*d^3*f-25*c*d^3*f+22*d^4*f-16*a^3*e*f-35*a^2*b*e*f+50*a*b^2*e*f-45*b^3*e*f-14*a^2*c*e*f+11*a*b*c*e*f+14*b^2*c*e*f-7*a*c^2*e*f-37*b*c^2*e*f-34*c^3*e*f-23*a^2*d*e*f-27*a*b*d*e*f-24*b^2*d*e*f+50*a*c*d*e*f-33*b*c*d*e*f+37*c^2*d*e*f+45*a*d^2*e*f+12*b*d^2*e*f+4*c*d^2*e*f-37*d^3*e*f+41*a^2*e^2*f+12*a*b*e^2*f+39*b^2*e^2*f-3*a*c*e^2*f-36*b*c*e^2*f-8*c^2*e^2*f-14*a*d*e^2*f-28*b*d*e^2*f+25*c*d*e^2*f-22*d^2*e^2*f+43*a*e^3*f+6*b*e^3*f+29*c*e^3*f-13*d*e^3*f+44*e^4*f-29*a^3*f^2+20*a^2*b*f^2+28*a*b^2*f^2-45*b^3*f^2-28*a^2*c*f^2+16*a*b*c*f^2+24*b^2*c*f^2-48*a*c^2*f^2+48*b*c^2*f^2+7*c^3*f^2+15*a^2*d*f^2+14*a*b*d*f^2-21*b^2*d*f^2+18*a*c*d*f^2-3*b*c*d*f^2-47*c^2*d*f^2-15*a*d^2*f^2-11*b*d^2*f^2-5*c*d^2*f^2-27*d^3*f^2+28*a^2*e*f^2-21*a*b*e*f^2+4*b^2*e*f^2+32*a*c*e*f^2-15*b*c*e*f^2-43*c^2*e*f^2+26*a*d*e*f^2-42*b*d*e*f^2-46*c*d*e*f^2+32*d^2*e*f^2-35*a*e^2*f^2+b*e^2*f^2-4*c*e^2*f^2-36*d*e^2*f^2-36*e^3*f^2+48*a^2*f^3+42*a*b*f^3+6*b^2*f^3+36*a*c*f^3-14*b*c*f^3+37*c^2*f^3-25*a*d*f^3-9*b*d*f^3-37*c*d*f^3+49*d^2*f^3+40*a*e*f^3+49*b*e*f^3-15*c*e*f^3+49*d*e*f^3-43*e^2*f^3-17*a*f^4+31*b*f^4-23*c*f^4-50*d*f^4+50*e*f^4+23*f^5+44*a^4*g+5*a^3*b*g+22*a^2*b^2*g+38*a*b^3*g-14*b^4*g-7*a^3*c*g+19*a^2*b*c*g-41*a*b^2*c*g-14*b^3*c*g-7*a^2*c^2*g+50*a*b*c^2*g+17*b^2*c^2*g-17*a*c^3*g+b*c^3*g+26*c^4*g-26*a^3*d*g+6*a^2*b*d*g-19*a*b^2*d*g-15*b^3*d*g+32*a^2*c*d*g-43*a*b*c*d*g-41*b^2*c*d*g+35*a*c^2*d*g+26*b*c^2*d*g+46*c^3*d*g+31*a^2*d^2*g+41*a*b*d^2*g-14*b^2*d^2*g-30*a*c*d^2*g+28*b*c*d^2*g+12*c^2*d^2*g-44*b*d^3*g+5*c*d^3*g-18*d^4*g-9*a^3*e*g-19*a^2*b*e*g+4*a*b^2*e*g-13*b^3*e*g+7*a^2*c*e*g-2*a*b*c*e*g-13*b^2*c*e*g+16*a*c^2*e*g+4*b*c^2*e*g-11*c^3*e*g+28*a^2*d*e*g-33*a*b*d*e*g+27*b^2*d*e*g+26*a*c*d*e*g-20*b*c*d*e*g-18*c^2*d*e*g-14*a*d^2*e*g+21*b*d^2*e*g+46*c*d^2*e*g-32*d^3*e*g+2*a^2*e^2*g-3*a*b*e^2*g-45*b^2*e^2*g+17*b*c*e^2*g+20*c^2*e^2*g+26*a*d*e^2*g+42*b*d*e^2*g+29*c*d*e^2*g+5*d^2*e^2*g+5*a*e^3*g+30*b*e^3*g-17*c*e^3*g-6*d*e^3*g-25*e^4*g+33*a^3*f*g+44*a^2*b*f*g+13*a*b^2*f*g+48*b^3*f*g+17*a^2*c*f*g-37*a*b*c*f*g-10*b^2*c*f*g-27*a*c^2*f*g+23*b*c^2*f*g-46*c^3*f*g+26*a^2*d*f*g-29*a*b*d*f*g+17*b^2*d*f*g+49*a*c*d*f*g+44*b*c*d*f*g-49*c^2*d*f*g-12*a*d^2*f*g-30*b*d^2*f*g+30*c*d^2*f*g-13*d^3*f*g+12*a^2*e*f*g+4*a*b*e*f*g+12*b^2*e*f*g-47*a*c*e*f*g-38*b*c*e*f*g-24*c^2*e*f*g+21*a*d*e*f*g-36*b*d*e*f*g+14*c*d*e*f*g-18*d^2*e*f*g-15*a*e^2*f*g-7*b*e^2*f*g-44*c*e^2*f*g-9*d*e^2*f*g-14*e^3*f*g+5*a^2*f^2*g-27*b^2*f^2*g+11*a*c*f^2*g+25*b*c*f^2*g-2*c^2*f^2*g+30*a*d*f^2*g-33*b*d*f^2*g-33*c*d*f^2*g-5*d^2*f^2*g+38*a*e*f^2*g-24*b*e*f^2*g-12*c*e*f^2*g+40*d*e*f^2*g-3*e^2*f^2*g+36*a*f^3*g+4*b*f^3*g-45*c*f^3*g+45*d*f^3*g-41*e*f^3*g-50*f^4*g-45*a^3*g^2+5*a^2*b*g^2-47*a*b^2*g^2-12*b^3*g^2-44*a^2*c*g^2+35*a*b*c*g^2+27*b^2*c*g^2-a*c^2*g^2-42*b*c^2*g^2-39*c^3*g^2-23*a^2*d*g^2+16*a*b*d*g^2+3*b^2*d*g^2+39*a*c*d*g^2-46*b*c*d*g^2+42*c^2*d*g^2+8*a*d^2*g^2+19*b*d^2*g^2-14*c*d^2*g^2+8*d^3*g^2+14*a^2*e*g^2+44*a*b*e*g^2+3*b^2*e*g^2+45*a*c*e*g^2+36*b*c*e*g^2-50*c^2*e*g^2+13*a*d*e*g^2+24*b*d*e*g^2-21*c*d*e*g^2+d^2*e*g^2-2*a*e^2*g^2+11*b*e^2*g^2+7*c*e^2*g^2+45*d*e^2*g^2+e^3*g^2-38*a^2*f*g^2-33*a*b*f*g^2-25*b^2*f*g^2+21*a*c*f*g^2+32*b*c*f*g^2+42*c^2*f*g^2-15*a*d*f*g^2+14*b*d*f*g^2+28*c*d*f*g^2+12*d^2*f*g^2+50*a*e*f*g^2+16*b*e*f*g^2+38*c*e*f*g^2-10*d*e*f*g^2-18*e^2*f*g^2-24*a*f^2*g^2+48*b*f^2*g^2+20*c*f^2*g^2-48*d*f^2*g^2-31*e*f^2*g^2-12*f^3*g^2-5*a^2*g^3+28*a*b*g^3-42*b^2*g^3+35*a*c*g^3-27*b*c*g^3+26*c^2*g^3-33*a*d*g^3-26*b*d*g^3-21*c*d*g^3+47*d^2*g^3+14*a*e*g^3+24*c*e*g^3+4*d*e*g^3-40*e^2*g^3-3*a*f*g^3-30*b*f*g^3+38*c*f*g^3-2*d*f*g^3-46*e*f*g^3+16*f^2*g^3-28*a*g^4-9*b*g^4+35*c*g^4-13*d*g^4-23*e*g^4+30*f*g^4-16*g^5,29*a^5+47*a^4*b+2*a^3*b^2+40*a^2*b^3+5*a*b^4-22*b^5+41*a^4*c-41*a^3*b*c+22*a^2*b^2*c+46*a*b^3*c+33*b^4*c-8*a^3*c^2-45*a^2*b*c^2-a*b^2*c^2+30*b^3*c^2+39*a^2*c^3+4*a*b*c^3-11*b^2*c^3+19*a*c^4+22*b*c^4-44*c^5+46*a^4*d-10*a^3*b*d-37*a^2*b^2*d-14*a*b^3*d-15*b^4*d-42*a^3*c*d+43*a^2*b*c*d-44*a*b^2*c*d+39*b^3*c*d+36*a^2*c^2*d+48*a*b*c^2*d-30*b^2*c^2*d+9*a*c^3*d-44*b*c^3*d-40*c^4*d-12*a^3*d^2-13*a^2*b*d^2+15*a*b^2*d^2-19*b^3*d^2-33*a^2*c*d^2+11*a*b*c*d^2+15*b^2*c*d^2+48*a*c^2*d^2-44*b*c^2*d^2-32*c^3*d^2-38*a^2*d^3-18*a*b*d^3-45*b^2*d^3+44*a*c*d^3+48*b*c*d^3-45*c^2*d^3+12*a*d^4-23*b*d^4-17*c*d^4-14*d^5+29*a^3*b*e-26*a^2*b^2*e-13*a*b^3*e+46*b^4*e+43*a^3*c*e+6*a^2*b*c*e+43*a*b^2*c*e-28*b^3*c*e-45*a^2*c^2*e-29*a*b*c^2*e+12*b^2*c^2*e-22*a*c^3*e+6*b*c^3*e-35*c^4*e+4*a^3*d*e-35*a^2*b*d*e+39*a*b^2*d*e+36*b^3*d*e-20*a^2*c*d*e+3*a*b*c*d*e-7*b^2*c*d*e+44*a*c^2*d*e+9*b*c^2*d*e+22*c^3*d*e+5*a^2*d^2*e-41*a*b*d^2*e-43*b^2*d^2*e-8*a*c*d^2*e+44*b*c*d^2*e-24*c^2*d^2*e+49*a*d^3*e-14*b*d^3*e-31*c*d^3*e-42*d^4*e+9*a^3*e^2+39*a^2*b*e^2+35*a*b^2*e^2-46*b^3*e^2-40*a^2*c*e^2-19*a*b*c*e^2+32*b^2*c*e^2-6*a*c^2*e^2+6*b*c^2*e^2-2*c^3*e^2+29*a^2*d*e^2-16*a*b*d*e^2+22*b^2*d*e^2+19*a*c*d*e^2+5*b*c*d*e^2+11*c^2*d*e^2-34*b*d^2*e^2+25*c*d^2*e^2+21*d^3*e^2-23*a^2*e^3+35*a*b*e^3+6*b^2*e^3-39*a*c*e^3-5*b*c*e^3+7*c^2*e^3-41*a*d*e^3+35*b*d*e^3-42*c*d*e^3+26*d^2*e^3-15*a*e^4+26*b*e^4+17*c*e^4-7*d*e^4-48*e^5+33*a^4*f+17*a^2*b^2*f-32*a*b^3*f-8*b^4*f-21*a^2*b*c*f+16*a*b^2*c*f+36*b^3*c*f+34*a^2*c^2*f-36*a*b*c^2*f+19*b^2*c^2*f-50*a*c^3*f+34*b*c^3*f+3*c^4*f-23*a^3*d*f+45*a^2*b*d*f-19*a*b^2*d*f+38*b^3*d*f-43*a^2*c*d*f-49*a*b*c*d*f-31*b^2*c*d*f-43*a*c^2*d*f+9*b*c^2*d*f-23*c^3*d*f-23*a^2*d^2*f-23*a*b*d^2*f+37*b^2*d^2*f+10*a*c*d^2*f+48*b*c*d^2*f-9*c^2*d^2*f+27*a*d^3*f+41*b*d^3*f+40*c*d^3*f+50*d^4*f-7*a^2*b*e*f-24*a*b^2*e*f-3*b^3*e*f+12*a^2*c*e*f+6*a*b*c*e*f+40*b^2*c*e*f-30*a*c^2*e*f+40*b*c^2*e*f-9*c^3*e*f-22*a^2*d*e*f+6*a*b*d*e*f-30*b^2*d*e*f-2*a*c*d*e*f-42*b*c*d*e*f-18*c^2*d*e*f+8*a*d^2*e*f-50*b*d^2*e*f+5*c*d^2*e*f+10*d^3*e*f+8*a^2*e^2*f-10*a*b*e^2*f-29*b^2*e^2*f-28*a*c*e^2*f+26*b*c*e^2*f-42*c^2*e^2*f-29*a*d*e^2*f+23*b*d*e^2*f-38*c*d*e^2*f+47*d^2*e^2*f-40*a*e^3*f-14*b*e^3*f-46*c*e^3*f+29*d*e^3*f-19*e^4*f-12*a^3*f^2-17*a^2*b*f^2-36*a*b^2*f^2+29*b^3*f^2+17*a^2*c*f^2-23*a*b*c*f^2+36*b^2*c*f^2+28*a*c^2*f^2+b*c^2*f^2+21*c^3*f^2+50*a^2*d*f^2+43*a*b*d*f^2-27*b^2*d*f^2+42*a*c*d*f^2-22*b*c*d*f^2-38*c^2*d*f^2+27*a*d^2*f^2-20*b*d^2*f^2-13*c*d^2*f^2+38*d^3*f^2+27*a^2*e*f^2+15*a*b*e*f^2-46*b^2*e*f^2+3*a*c*e*f^2+45*b*c*e*f^2-9*c^2*e*f^2+19*a*d*e*f^2-14*b*d*e*f^2+6*c*d*e*f^2+34*d^2*e*f^2+42*a*e^2*f^2-36*b*e^2*f^2+c*e^2*f^2-7*d*e^2*f^2+32*e^3*f^2+30*a^2*f^3+49*a*b*f^3+28*b^2*f^3+11*a*c*f^3-17*b*c*f^3-40*c^2*f^3-12*a*d*f^3-14*b*d*f^3-21*c*d*f^3-29*d^2*f^3-44*a*e*f^3-2*b*e*f^3-32*c*e*f^3-2*d*e*f^3-13*e^2*f^3-30*a*f^4-46*b*f^4-39*c*f^4-36*d*f^4-5*e*f^4-26*f^5-34*a^4*g+50*a^3*b*g-33*a^2*b^2*g-6*a*b^3*g-41*b^4*g-40*a^3*c*g-2*a^2*b*c*g-40*a*b^2*c*g-8*b^3*c*g-26*a^2*c^2*g-8*a*b*c^2*g-38*b^2*c^2*g+47*a*c^3*g-49*b*c^3*g-40*c^4*g+6*a^3*d*g-28*a^2*b*d*g+35*a*b^2*d*g-20*b^3*d*g-43*a^2*c*d*g+24*a*b*c*d*g+28*b^2*c*d*g+8*a*c^2*d*g-16*b*c^2*d*g+2*c^3*d*g+5*a^2*d^2*g-21*a*b*d^2*g-25*b^2*d^2*g-15*a*c*d^2*g+30*b*c*d^2*g-43*c^2*d^2*g-25*a*d^3*g-49*b*d^3*g+24*c*d^3*g+28*d^4*g+46*a^3*e*g+38*a^2*b*e*g-47*a*b^2*e*g-42*b^3*e*g-25*a^2*c*e*g-22*a*b*c*e*g-45*b^2*c*e*g+32*a*c^2*e*g+31*b*c^2*e*g+25*c^3*e*g+9*a*b*d*e*g-27*b^2*d*e*g+7*a*c*d*e*g-43*b*c*d*e*g-30*c^2*d*e*g-19*a*d^2*e*g+27*b*d^2*e*g+17*c*d^2*e*g+38*d^3*e*g-6*a^2*e^2*g-37*a*b*e^2*g-28*a*c*e^2*g-30*b*c*e^2*g+8*c^2*e^2*g-34*a*d*e^2*g-11*b*d*e^2*g+39*c*d*e^2*g-31*d^2*e^2*g-5*a*e^3*g-42*b*e^3*g+12*c*e^3*g+46*d*e^3*g+30*e^4*g-8*a^3*f*g+20*a^2*b*f*g+42*a*b^2*f*g-43*b^3*f*g+13*a^2*c*f*g+38*a*b*c*f*g+46*b^2*c*f*g+15*a*c^2*f*g-8*b*c^2*f*g-3*c^3*f*g-21*a^2*d*f*g-41*a*b*d*f*g+34*b^2*d*f*g-15*a*c*d*f*g-28*b*c*d*f*g-18*c^2*d*f*g-21*a*d^2*f*g+35*b*d^2*f*g-31*c*d^2*f*g+47*d^3*f*g+37*a^2*e*f*g-35*a*b*e*f*g-18*b^2*e*f*g-34*a*c*e*f*g-24*b*c*e*f*g+27*c^2*e*f*g+45*a*d*e*f*g+29*b*d*e*f*g-41*c*d*e*f*g+16*d^2*e*f*g+44*a*e^2*f*g+26*b*e^2*f*g-49*c*e^2*f*g+26*d*e^2*f*g+44*e^3*f*g+50*a^2*f^2*g-21*a*b*f^2*g-21*b^2*f^2*g-42*a*c*f^2*g-35*b*c*f^2*g-8*c^2*f^2*g-42*a*d*f^2*g-3*b*d*f^2*g+7*c*d*f^2*g+9*d^2*f^2*g-23*a*e*f^2*g-35*b*e*f^2*g-12*c*e*f^2*g+38*d*e*f^2*g-34*e^2*f^2*g+31*a*f^3*g+41*b*f^3*g+19*c*f^3*g-41*d*f^3*g-35*e*f^3*g-37*f^4*g-6*a^3*g^2+49*a^2*b*g^2+27*a*b^2*g^2-46*b^3*g^2-35*a^2*c*g^2+12*a*b*c*g^2-39*b^2*c*g^2+12*a*c^2*g^2+23*b*c^2*g^2-48*c^3*g^2+42*a^2*d*g^2+20*a*b*d*g^2-29*b^2*d*g^2+25*a*c*d*g^2+15*b*c*d*g^2+22*c^2*d*g^2+15*a*d^2*g^2+21*b*d^2*g^2+39*c*d^2*g^2+18*d^3*g^2-12*a^2*e*g^2+4*a*b*e*g^2+13*b^2*e*g^2-33*a*c*e*g^2+15*b*c*e*g^2+c^2*e*g^2-14*a*d*e*g^2+20*b*d*e*g^2-49*c*d*e*g^2+d^2*e*g^2-18*a*e^2*g^2+10*b*e^2*g^2-47*c*e^2*g^2+28*d*e^2*g^2-19*e^3*g^2+38*a^2*f*g^2-17*a*b*f*g^2+29*b^2*f*g^2+26*a*c*f*g^2+18*b*c*f*g^2-12*c^2*f*g^2+13*a*d*f*g^2+38*b*d*f*g^2-21*c*d*f*g^2+18*a*e*f*g^2-46*b*e*f*g^2+31*c*e*f*g^2-33*d*e*f*g^2-49*e^2*f*g^2+15*a*f^2*g^2+39*b*f^2*g^2-9*c*f^2*g^2-49*d*f^2*g^2+32*e*f^2*g^2+19*f^3*g^2+33*a^2*g^3-2*a*b*g^3+39*b^2*g^3+38*a*c*g^3-46*b*c*g^3+29*a*d*g^3+44*b*d*g^3+22*c*d*g^3+42*d^2*g^3+17*a*e*g^3+50*b*e*g^3-37*c*e*g^3-21*d*e*g^3+8*e^2*g^3+25*a*f*g^3+28*b*f*g^3+29*d*f*g^3-23*e*f*g^3-19*f^2*g^3-14*a*g^4+4*b*g^4-18*c*g^4+47*d*g^4-13*e*g^4-36*f*g^4+13*g^5,-16*a^5-48*a^4*b+14*a^3*b^2+46*a^2*b^3-42*a*b^4-43*b^5-49*a^4*c-7*a^3*b*c+38*a^2*b^2*c+9*a*b^3*c+33*b^4*c+47*a^3*c^2+38*a^2*b*c^2+5*a*b^2*c^2+45*b^3*c^2+8*a^2*c^3-12*a*b*c^3-44*b^2*c^3-36*a*c^4+12*b*c^4-32*c^5-22*a^4*d-30*a^3*b*d-36*a^2*b^2*d+40*a*b^3*d-8*b^4*d-34*a^3*c*d+29*a^2*b*c*d-31*a*b^2*c*d-29*b^3*c*d-17*a^2*c^2*d-18*a*b*c^2*d+20*b^2*c^2*d+8*a*c^3*d-14*b*c^3*d-20*c^4*d-28*a^3*d^2+42*a^2*b*d^2-14*a*b^2*d^2+38*b^3*d^2+47*a^2*c*d^2+27*a*b*c*d^2-49*b^2*c*d^2+39*a*c^2*d^2-42*b*c^2*d^2-48*c^3*d^2-3*a^2*d^3-25*a*b*d^3-5*b^2*d^3+48*a*c*d^3+48*b*c*d^3+12*c^2*d^3+21*a*d^4+49*b*d^4+41*c*d^4-46*d^5-30*a^4*e-26*a^3*b*e-9*a^2*b^2*e+19*a*b^3*e+34*b^4*e+16*a^3*c*e+13*a^2*b*c*e+7*a*b^2*c*e+49*b^3*c*e+40*a^2*c^2*e+37*a*b*c^2*e-14*b^2*c^2*e+22*a*c^3*e-45*b*c^3*e+20*c^4*e+40*a^3*d*e-41*a^2*b*d*e+5*a*b^2*d*e-20*b^3*d*e-41*a^2*c*d*e-4*a*b*c*d*e+15*b^2*c*d*e+14*a*c^2*d*e+35*b*c^2*d*e+6*c^3*d*e-2*a^2*d^2*e+26*a*b*d^2*e-18*b^2*d^2*e+6*a*c*d^2*e+39*b*c*d^2*e+50*c^2*d^2*e-49*a*d^3*e-36*b*d^3*e+37*c*d^3*e+6*d^4*e+9*a^3*e^2-18*a^2*b*e^2+46*a*b^2*e^2-29*b^3*e^2-25*a^2*c*e^2-7*a*b*c*e^2-26*b^2*c*e^2-10*a*c^2*e^2-45*b*c^2*e^2-18*c^3*e^2+19*a^2*d*e^2+35*a*b*d*e^2+44*b^2*d*e^2+37*a*c*d*e^2-50*c^2*d*e^2-42*a*d^2*e^2+21*b*d^2*e^2-16*c*d^2*e^2-11*d^3*e^2+20*a^2*e^3-15*a*b*e^3+41*b^2*e^3-30*a*c*e^3-25*b*c*e^3-39*c^2*e^3+42*a*d*e^3+31*b*d*e^3+15*c*d*e^3+13*d^2*e^3-44*a*e^4-50*b*e^4+46*c*e^4+41*d*e^4-30*e^5+6*a^4*f-a^3*b*f-39*a^2*b^2*f-7*a*b^3*f-18*b^4*f+23*a^3*c*f+20*a^2*b*c*f+2*a*b^2*c*f-21*b^3*c*f-40*a^2*c^2*f-17*a*b*c^2*f-28*b^2*c^2*f+26*a*c^3*f-34*b*c^3*f+8*c^4*f+31*a^3*d*f+7*a^2*b*d*f+38*a*b^2*d*f+26*b^3*d*f-34*a^2*c*d*f-11*a*b*c*d*f+35*b^2*c*d*f-33*a*c^2*d*f-15*b*c^2*d*f-6*c^3*d*f+45*a^2*d^2*f-37*a*b*d^2*f+11*b^2*d^2*f+2*a*c*d^2*f+30*b*c*d^2*f+33*c^2*d^2*f+32*a*d^3*f-46*b*d^3*f-39*c*d^3*f-19*d^4*f-44*a^3*e*f+31*a^2*b*e*f-6*a*b^2*e*f+47*b^3*e*f-35*a^2*c*e*f-10*a*b*c*e*f+33*b^2*c*e*f+33*a*c^2*e*f-17*b*c^2*e*f-13*c^3*e*f-49*a^2*d*e*f-26*a*b*d*e*f-17*b^2*d*e*f-25*a*c*d*e*f-47*b*c*d*e*f-6*c^2*d*e*f-36*a*d^2*e*f+44*b*d^2*e*f-8*c*d^2*e*f-47*d^3*e*f-42*a^2*e^2*f-31*a*b*e^2*f+11*b^2*e^2*f+38*a*c*e^2*f-10*b*c*e^2*f-21*c^2*e^2*f-3*a*d*e^2*f-5*b*d*e^2*f+9*c*d*e^2*f+31*d^2*e^2*f-28*a*e^3*f-3*b*e^3*f-14*c*e^3*f+17*d*e^3*f-39*e^4*f-44*a^3*f^2+a^2*b*f^2+21*a*b^2*f^2-19*b^3*f^2+20*a^2*c*f^2+5*a*b*c*f^2+18*b^2*c*f^2+6*a*c^2*f^2-5*b*c^2*f^2-13*c^3*f^2-33*a^2*d*f^2-20*a*b*d*f^2-10*b^2*d*f^2+50*b*c*d*f^2-6*c^2*d*f^2+8*a*d^2*f^2-11*b*d^2*f^2+24*c*d^2*f^2+6*d^3*f^2-6*a^2*e*f^2-6*a*b*e*f^2-20*b^2*e*f^2-30*a*c*e*f^2+23*b*c*e*f^2-13*c^2*e*f^2+46*a*d*e*f^2-38*b*d*e*f^2-36*c*d*e*f^2-17*d^2*e*f^2+14*a*e^2*f^2+35*b*e^2*f^2+48*c*e^2*f^2-36*d*e^2*f^2+3*e^3*f^2-8*a^2*f^3+9*a*b*f^3-34*b^2*f^3-31*a*c*f^3+26*b*c*f^3-c^2*f^3+49*a*d*f^3+5*b*d*f^3+30*c*d*f^3+49*d^2*f^3-6*a*e*f^3-31*b*e*f^3+4*c*e*f^3+18*d*e*f^3+26*e^2*f^3+8*a*f^4-44*b*f^4-8*c*f^4-17*d*f^4-30*e*f^4+7*f^5+13*a^4*g-36*a^3*b*g-30*a^2*b^2*g-19*a*b^3*g+27*b^4*g+3*a^3*c*g-4*a^2*b*c*g+9*a*b^2*c*g+15*b^3*c*g+45*a^2*c^2*g+2*a*b*c^2*g+3*b^2*c^2*g-6*a*c^3*g+41*b*c^3*g+11*c^4*g+37*a^3*d*g-3*a^2*b*d*g+50*a*b^2*d*g-44*b^3*d*g+23*a^2*c*d*g+30*a*b*c*d*g+45*b^2*c*d*g+21*a*c^2*d*g-11*b*c^2*d*g+22*c^3*d*g+36*a^2*d^2*g-2*a*b*d^2*g+9*b^2*d^2*g-22*a*c*d^2*g-34*b*c*d^2*g-2*c^2*d^2*g-21*a*d^3*g-28*b*d^3*g+11*c*d^3*g-48*d^4*g-14*a^3*e*g-22*a^2*b*e*g+17*a*b^2*e*g-14*b^3*e*g-21*a^2*c*e*g-26*a*b*c*e*g-21*b^2*c*e*g-19*a*c^2*e*g+26*b*c^2*e*g-c^3*e*g-a^2*d*e*g-28*a*b*d*e*g+6*b^2*d*e*g+8*a*c*d*e*g+40*b*c*d*e*g+22*c^2*d*e*g-36*a*d^2*e*g-32*b*d^2*e*g-4*c*d^2*e*g+41*d^3*e*g+5*a^2*e^2*g-12*a*b*e^2*g-23*b^2*e^2*g-3*a*c*e^2*g+29*b*c*e^2*g+10*c^2*e^2*g-5*a*d*e^2*g-32*b*d*e^2*g-48*c*d*e^2*g+34*d^2*e^2*g+12*a*e^3*g-40*b*e^3*g-15*c*e^3*g-49*d*e^3*g-35*e^4*g+44*a^3*f*g+13*a^2*b*f*g+26*a*b^2*f*g-38*b^3*f*g-49*a^2*c*f*g-6*a*b*c*f*g+4*b^2*c*f*g+25*a*c^2*f*g-45*b*c^2*f*g+13*c^3*f*g-17*a^2*d*f*g-48*a*b*d*f*g-33*b^2*d*f*g+27*a*c*d*f*g-33*b*c*d*f*g+38*c^2*d*f*g-41*a*d^2*f*g+36*b*d^2*f*g-13*c*d^2*f*g-47*d^3*f*g-2*a^2*e*f*g+15*a*b*e*f*g+32*b^2*e*f*g-27*a*c*e*f*g-14*b*c*e*f*g-37*a*d*e*f*g+33*b*d*e*f*g+15*c*d*e*f*g-38*d^2*e*f*g-47*a*e^2*f*g-35*b*e^2*f*g+22*c*e^2*f*g+4*d*e^2*f*g+4*e^3*f*g+39*a^2*f^2*g+26*a*b*f^2*g-36*b^2*f^2*g+22*a*c*f^2*g+8*b*c*f^2*g-3*c^2*f^2*g-30*a*d*f^2*g-33*b*d*f^2*g-14*c*d*f^2*g+38*d^2*f^2*g-11*a*e*f^2*g+36*b*e*f^2*g+31*c*e*f^2*g-7*d*e*f^2*g+e^2*f^2*g-12*a*f^3*g-37*b*f^3*g-c*f^3*g+12*d*f^3*g+20*e*f^3*g-16*f^4*g+46*a^3*g^2+35*a^2*b*g^2-46*a*b^2*g^2-34*b^3*g^2+5*a^2*c*g^2+17*a*b*c*g^2+6*b^2*c*g^2+41*a*c^2*g^2+28*b*c^2*g^2+7*c^3*g^2+36*a^2*d*g^2+33*a*b*d*g^2-32*b^2*d*g^2-3*a*c*d*g^2+19*b*c*d*g^2+11*c^2*d*g^2-28*a*d^2*g^2-21*b*d^2*g^2+26*c*d^2*g^2-41*d^3*g^2+27*a^2*e*g^2-21*a*b*e*g^2-7*b^2*e*g^2-13*a*c*e*g^2+32*b*c*e*g^2-38*c^2*e*g^2-17*a*d*e*g^2-39*b*d*e*g^2-36*c*d*e*g^2+42*d^2*e*g^2+43*a*e^2*g^2+39*b*e^2*g^2-27*c*e^2*g^2+5*d*e^2*g^2+11*e^3*g^2+49*a^2*f*g^2+14*a*b*f*g^2+41*b^2*f*g^2-24*a*c*f*g^2+25*b*c*f*g^2-23*c^2*f*g^2+45*a*d*f*g^2-12*b*d*f*g^2+49*c*d*f*g^2+48*d^2*f*g^2-26*a*e*f*g^2+23*b*e*f*g^2-38*c*e*f*g^2-18*d*e*f*g^2-3*e^2*f*g^2+26*a*f^2*g^2+16*b*f^2*g^2-24*c*f^2*g^2-47*d*f^2*g^2+38*e*f^2*g^2+11*f^3*g^2+6*a^2*g^3-14*a*b*g^3-14*b^2*g^3-18*a*c*g^3+20*b*c*g^3-25*c^2*g^3+11*a*d*g^3-34*b*d*g^3-37*c*d*g^3+41*d^2*g^3-43*a*e*g^3-33*b*e*g^3-30*c*e*g^3-21*d*e*g^3+29*e^2*g^3-40*a*f*g^3-18*b*f*g^3-24*c*f*g^3-50*d*f*g^3-2*e*f*g^3+27*f^2*g^3+a*g^4-23*b*g^4-3*c*g^4-35*d*g^4-13*e*g^4+6*f*g^4-36*g^5,-37*a^6+17*a^5*b+49*a^4*b^2+35*a^3*b^3+34*a^2*b^4+32*a*b^5-21*b^6+2*a^5*c+24*a^4*b*c+27*a^3*b^2*c+45*a^2*b^3*c+16*a*b^4*c+b^5*c+35*a^4*c^2+33*a^3*b*c^2+31*a^2*b^2*c^2-5*a*b^3*c^2+41*b^4*c^2-11*a^3*c^3+36*a^2*b*c^3+10*a*b^2*c^3-49*b^3*c^3-41*a^2*c^4+29*a*b*c^4+32*b^2*c^4+33*a*c^5+17*b*c^5+45*c^6-24*a^5*d-12*a^4*b*d-13*a^3*b^2*d-19*a^2*b^3*d-28*a*b^4*d+23*b^5*d+32*a^4*c*d+22*a^3*b*c*d+20*a^2*b^2*c*d+9*a*b^3*c*d+50*b^4*c*d+39*a^3*c^2*d-46*a^2*b*c^2*d-28*a*b^2*c^2*d+b^3*c^2*d-32*a^2*c^3*d-34*a*b*c^3*d+2*b^2*c^3*d-24*a*c^4*d-49*b*c^4*d+46*c^5*d+33*a^4*d^2+40*a^3*b*d^2+9*a^2*b^2*d^2+22*a*b^3*d^2+12*b^4*d^2-32*a^3*c*d^2-18*a^2*b*c*d^2+26*a*b^2*c*d^2+37*b^3*c*d^2-46*a^2*c^2*d^2-47*a*b*c^2*d^2-28*b^2*c^2*d^2+48*b*c^3*d^2-11*c^4*d^2-8*a^3*d^3+22*a^2*b*d^3-3*a*b^2*d^3+8*b^3*d^3-21*a^2*c*d^3-50*a*b*c*d^3-b^2*c*d^3-48*a*c^2*d^3+23*b*c^2*d^3-c^3*d^3+37*a^2*d^4-49*a*b*d^4-50*b^2*d^4+21*a*c*d^4+18*b*c*d^4+26*c^2*d^4+23*a*d^5-25*b*d^5+14*c*d^5-29*d^6+45*a^5*e-22*a^4*b*e+12*a^3*b^2*e+18*a^2*b^3*e+34*a*b^4*e-35*b^5*e+9*a^4*c*e+18*a^3*b*c*e-15*a^2*b^2*c*e-a*b^3*c*e-17*b^4*c*e+42*a^3*c^2*e+44*a^2*b*c^2*e-25*a*b^2*c^2*e-17*b^3*c^2*e+44*a^2*c^3*e+6*a*b*c^3*e-5*b^2*c^3*e-15*a*c^4*e+37*b*c^4*e-32*c^5*e+46*a^4*d*e-18*a^3*b*d*e+44*a^2*b^2*d*e-45*a*b^3*d*e+8*b^4*d*e-44*a^3*c*d*e-20*a^2*b*c*d*e-17*a*b^2*c*d*e+5*b^3*c*d*e-29*a*b*c^2*d*e+26*b^2*c^2*d*e-34*a*c^3*d*e-9*b*c^3*d*e+21*c^4*d*e+10*a^3*d^2*e-43*a^2*b*d^2*e+36*a*b^2*d^2*e-6*b^3*d^2*e+43*a^2*c*d^2*e-24*a*b*c*d^2*e+10*b^2*c*d^2*e-4*a*c^2*d^2*e-16*b*c^2*d^2*e-37*c^3*d^2*e+46*a^2*d^3*e+37*a*b*d^3*e+27*b^2*d^3*e+16*a*c*d^3*e+31*b*c*d^3*e+26*c^2*d^3*e+27*a*d^4*e-27*b*d^4*e-38*c*d^4*e-24*d^5*e-27*a^4*e^2-27*a^3*b*e^2+9*a^2*b^2*e^2-50*a*b^3*e^2+14*b^4*e^2-31*a^3*c*e^2+36*a^2*b*c*e^2-18*a*b^2*c*e^2+38*b^3*c*e^2+32*a^2*c^2*e^2+34*a*b*c^2*e^2+18*b^2*c^2*e^2-13*a*c^3*e^2+49*b*c^3*e^2+30*c^4*e^2-14*a^3*d*e^2+13*a^2*b*d*e^2+27*a*b^2*d*e^2-7*b^3*d*e^2-21*a^2*c*d*e^2-26*a*b*c*d*e^2+32*b^2*c*d*e^2-27*a*c^2*d*e^2+3*b*c^2*d*e^2-46*c^3*d*e^2-42*a^2*d^2*e^2+2*a*b*d^2*e^2-10*b^2*d^2*e^2+31*a*c*d^2*e^2+9*b*c*d^2*e^2-40*c^2*d^2*e^2+5*a*d^3*e^2+24*b*d^3*e^2-5*c*d^3*e^2+15*d^4*e^2+43*a^3*e^3+6*a^2*b*e^3-22*a*b^2*e^3+49*b^3*e^3-47*a^2*c*e^3-35*a*b*c*e^3+29*b^2*c*e^3+26*a*c^2*e^3+34*b*c^2*e^3+50*c^3*e^3+39*a^2*d*e^3-40*a*b*d*e^3-19*b^2*d*e^3+14*a*c*d*e^3+44*b*c*d*e^3+33*c^2*d*e^3+27*a*d^2*e^3-40*b*d^2*e^3-37*c*d^2*e^3-47*a^2*e^4-44*a*b*e^4-19*b^2*e^4+9*a*c*e^4+7*b*c*e^4+29*c^2*e^4-22*a*d*e^4+44*b*d*e^4-19*c*d*e^4+7*d^2*e^4-15*a*e^5+36*b*e^5-15*c*e^5-24*d*e^5-12*e^6-35*a^5*f+39*a^4*b*f+35*a^3*b^2*f+33*a^2*b^3*f+44*a*b^4*f+17*b^5*f+31*a^4*c*f+41*a^3*b*c*f-48*a^2*b^2*c*f-26*a*b^3*c*f-35*b^4*c*f+47*a^3*c^2*f-36*a^2*b*c^2*f+37*a*b^2*c^2*f-16*b^3*c^2*f-14*a^2*c^3*f+43*a*b*c^3*f-4*b^2*c^3*f-23*a*c^4*f+8*b*c^4*f+18*c^5*f+8*a^4*d*f+21*a^3*b*d*f-2*a^2*b^2*d*f-43*a*b^3*d*f-18*b^4*d*f+21*a^3*c*d*f+31*a^2*b*c*d*f-31*a*b^2*c*d*f-5*b^3*c*d*f+8*a^2*c^2*d*f-37*a*b*c^2*d*f+3*b^2*c^2*d*f+7*a*c^3*d*f-22*b*c^3*d*f+34*c^4*d*f+17*a^3*d^2*f+19*a^2*b*d^2*f+35*a*b^2*d^2*f-6*b^3*d^2*f-31*a^2*c*d^2*f+6*a*b*c*d^2*f+35*b^2*c*d^2*f+28*a*c^2*d^2*f-10*b*c^2*d^2*f+2*c^3*d^2*f+49*a^2*d^3*f+16*a*b*d^3*f+b^2*d^3*f-15*a*c*d^3*f-27*b*c*d^3*f+7*c^2*d^3*f-11*a*d^4*f-50*b*d^4*f-8*c*d^4*f+32*d^5*f+22*a^4*e*f+29*a^3*b*e*f+46*a^2*b^2*e*f-49*a*b^3*e*f-45*b^4*e*f+32*a^3*c*e*f-32*a^2*b*c*e*f-35*a*b^2*c*e*f-23*b^3*c*e*f-17*a^2*c^2*e*f+7*a*b*c^2*e*f+16*b^2*c^2*e*f-49*a*c^3*e*f-15*b*c^3*e*f-48*c^4*e*f-33*a^3*d*e*f+23*a^2*b*d*e*f-2*a*b^2*d*e*f+46*b^3*d*e*f-33*a^2*c*d*e*f+48*a*b*c*d*e*f-49*b^2*c*d*e*f+7*a*c^2*d*e*f-25*b*c^2*d*e*f+26*c^3*d*e*f+19*a^2*d^2*e*f+16*a*b*d^2*e*f-28*b^2*d^2*e*f-15*a*c*d^2*e*f-47*b*c*d^2*e*f-32*c^2*d^2*e*f-39*a*d^3*e*f-41*b*d^3*e*f-20*c*d^3*e*f+50*d^4*e*f+10*a^3*e^2*f-17*a^2*b*e^2*f-5*a*b^2*e^2*f+48*b^3*e^2*f-16*a^2*c*e^2*f+20*a*b*c*e^2*f-3*b^2*c*e^2*f+13*a*c^2*e^2*f+43*b*c^2*e^2*f+3*c^3*e^2*f-16*a^2*d*e^2*f-18*a*b*d*e^2*f-42*b^2*d*e^2*f+11*b*c*d*e^2*f+41*c^2*d*e^2*f+3*a*d^2*e^2*f+47*b*d^2*e^2*f-44*c*d^2*e^2*f-11*d^3*e^2*f-46*a^2*e^3*f-24*a*b*e^3*f-39*b^2*e^3*f+22*a*c*e^3*f-30*b*c*e^3*f+45*c^2*e^3*f-9*a*d*e^3*f-27*b*d*e^3*f+17*c*d*e^3*f-9*d^2*e^3*f+49*a*e^4*f-31*b*e^4*f-20*c*e^4*f-20*d*e^4*f+17*e^5*f+10*a^4*f^2-15*a^3*b*f^2-16*a^2*b^2*f^2+31*a*b^3*f^2+35*b^4*f^2-28*a^3*c*f^2+19*a^2*b*c*f^2+11*a*b^2*c*f^2+39*b^3*c*f^2+16*a^2*c^2*f^2-22*a*b*c^2*f^2-30*b^2*c^2*f^2+44*a*c^3*f^2+18*b*c^3*f^2+14*c^4*f^2-49*a^3*d*f^2+23*a^2*b*d*f^2-39*a*b^2*d*f^2-21*b^3*d*f^2-41*a^2*c*d*f^2-32*a*b*c*d*f^2+24*b^2*c*d*f^2-37*a*c^2*d*f^2+24*b*c^2*d*f^2+47*c^3*d*f^2-12*a^2*d^2*f^2+2*a*b*d^2*f^2-10*b^2*d^2*f^2-42*a*c*d^2*f^2-2*b*c*d^2*f^2-8*c^2*d^2*f^2+48*a*d^3*f^2-21*b*d^3*f^2-40*c*d^3*f^2-33*d^4*f^2+47*a^3*e*f^2-31*a^2*b*e*f^2+9*a*b^2*e*f^2+7*b^3*e*f^2+24*a^2*c*e*f^2-14*a*b*c*e*f^2-15*b^2*c*e*f^2+48*a*c^2*e*f^2-2*b*c^2*e*f^2-43*c^3*e*f^2+13*a^2*d*e*f^2+25*a*b*d*e*f^2+26*b^2*d*e*f^2+7*a*c*d*e*f^2+6*b*c*d*e*f^2+16*c^2*d*e*f^2+4*a*d^2*e*f^2+39*b*d^2*e*f^2+2*c*d^2*e*f^2-32*d^3*e*f^2+33*a^2*e^2*f^2+21*a*b*e^2*f^2+40*b^2*e^2*f^2+a*c*e^2*f^2-36*b*c*e^2*f^2+11*c^2*e^2*f^2-45*a*d*e^2*f^2-32*b*d*e^2*f^2-6*c*d*e^2*f^2+27*d^2*e^2*f^2-6*a*e^3*f^2+3*b*e^3*f^2-35*c*e^3*f^2+45*d*e^3*f^2-21*e^4*f^2-34*a^3*f^3-18*a^2*b*f^3+30*a*b^2*f^3+26*b^3*f^3+6*a^2*c*f^3-38*a*b*c*f^3+8*b^2*c*f^3+40*a*c^2*f^3+41*b*c^2*f^3+30*c^3*f^3-31*a^2*d*f^3+40*a*b*d*f^3+18*b^2*d*f^3-11*a*c*d*f^3+28*b*c*d*f^3+13*c^2*d*f^3+34*a*d^2*f^3+9*b*d^2*f^3-10*c*d^2*f^3-18*d^3*f^3+15*a^2*e*f^3-26*a*b*e*f^3-23*b^2*e*f^3-15*a*c*e*f^3+11*b*c*e*f^3-6*c^2*e*f^3+8*a*d*e*f^3+b*d*e*f^3-45*c*d*e*f^3-50*d^2*e*f^3+21*a*e^2*f^3-22*b*e^2*f^3+34*c*e^2*f^3+46*d*e^2*f^3+34*e^3*f^3+27*a^2*f^4+42*a*b*f^4-10*b^2*f^4-22*a*c*f^4-30*b*c*f^4-50*c^2*f^4-21*a*d*f^4-22*b*d*f^4+24*c*d*f^4+13*d^2*f^4-26*a*e*f^4+28*b*e*f^4+33*c*e*f^4-4*d*e*f^4-11*e^2*f^4+7*a*f^5-19*b*f^5+17*c*f^5-12*d*f^5-e*f^5+35*f^6-31*a^5*g+28*a^4*b*g+25*a^3*b^2*g+17*a^2*b^3*g-35*a*b^4*g-18*b^5*g+30*a^4*c*g+3*a^3*b*c*g+4*a^2*b^2*c*g-22*a*b^3*c*g-4*b^4*c*g+18*a^3*c^2*g+25*a^2*b*c^2*g+7*a*b^2*c^2*g-10*b^3*c^2*g+15*a^2*c^3*g+50*a*b*c^3*g+8*b^2*c^3*g+21*a*c^4*g+10*b*c^4*g-9*c^5*g+18*a^4*d*g-10*a^3*b*d*g-32*a^2*b^2*d*g-4*a*b^3*d*g+43*b^4*d*g-4*a^3*c*d*g+32*a^2*b*c*d*g+35*a*b^2*c*d*g+35*b^3*c*d*g-48*a^2*c^2*d*g+40*a*b*c^2*d*g-28*b^2*c^2*d*g-16*a*c^3*d*g-11*b*c^3*d*g-22*c^4*d*g-12*a^3*d^2*g-16*a^2*b*d^2*g+21*a*b^2*d^2*g-19*b^3*d^2*g-32*a^2*c*d^2*g+25*a*b*c*d^2*g+28*b^2*c*d^2*g+25*a*c^2*d^2*g+21*b*c^2*d^2*g-42*c^3*d^2*g-27*a^2*d^3*g-47*a*b*d^3*g-13*b^2*d^3*g+18*a*c*d^3*g+34*b*c*d^3*g-20*c^2*d^3*g+34*a*d^4*g+4*b*d^4*g+47*c*d^4*g-25*d^5*g+14*a^4*e*g+31*a^3*b*e*g-42*a^2*b^2*e*g+12*a*b^3*e*g+46*b^4*e*g-19*a^3*c*e*g+34*a^2*b*c*e*g-25*a*b^2*c*e*g-39*b^3*c*e*g+46*a^2*c^2*e*g-27*a*b*c^2*e*g-45*b^2*c^2*e*g-44*a*c^3*e*g-11*b*c^3*e*g+22*c^4*e*g+45*a^3*d*e*g+39*a^2*b*d*e*g-44*a*b^2*d*e*g-16*b^3*d*e*g+36*a^2*c*d*e*g+36*a*b*c*d*e*g-10*b^2*c*d*e*g+39*a*c^2*d*e*g-49*b*c^2*d*e*g-7*c^3*d*e*g+5*a^2*d^2*e*g+9*a*b*d^2*e*g+2*b^2*d^2*e*g+25*a*c*d^2*e*g+15*b*c*d^2*e*g-14*c^2*d^2*e*g+18*a*d^3*e*g+5*b*d^3*e*g+36*c*d^3*e*g-13*d^4*e*g-36*a^3*e^2*g-43*a^2*b*e^2*g-19*a*b^2*e^2*g+25*b^3*e^2*g-36*a^2*c*e^2*g+29*a*b*c*e^2*g-5*b^2*c*e^2*g-2*a*c^2*e^2*g-22*b*c^2*e^2*g-17*c^3*e^2*g-28*a^2*d*e^2*g-11*a*b*d*e^2*g+15*b^2*d*e^2*g+22*a*c*d*e^2*g-50*b*c*d*e^2*g-40*c^2*d*e^2*g-11*a*d^2*e^2*g+36*b*d^2*e^2*g-19*c*d^2*e^2*g-30*d^3*e^2*g+26*a^2*e^3*g-47*a*b*e^3*g-50*b^2*e^3*g+22*a*c*e^3*g-18*b*c*e^3*g+11*c^2*e^3*g-37*a*d*e^3*g+42*b*d*e^3*g-41*c*d*e^3*g-12*d^2*e^3*g+23*a*e^4*g+47*b*e^4*g+c*e^4*g-19*d*e^4*g+4*e^5*g+32*a^4*f*g+45*a^3*b*f*g-30*a^2*b^2*f*g-48*a*b^3*f*g-28*b^4*f*g-a^3*c*f*g-34*a^2*b*c*f*g+8*a*b^2*c*f*g+22*b^3*c*f*g+5*a^2*c^2*f*g-7*a*b*c^2*f*g+16*b^2*c^2*f*g+26*a*c^3*f*g-37*b*c^3*f*g+7*c^4*f*g-18*a^3*d*f*g-30*a^2*b*d*f*g-a*b^2*d*f*g+5*b^3*d*f*g-43*a^2*c*d*f*g+18*a*b*c*d*f*g-24*b^2*c*d*f*g-23*a*c^2*d*f*g-10*b*c^2*d*f*g-33*c^3*d*f*g-13*a^2*d^2*f*g-50*a*b*d^2*f*g+11*b^2*d^2*f*g-2*a*c*d^2*f*g-48*b*c*d^2*f*g+33*c^2*d^2*f*g-23*a*d^3*f*g+37*b*d^3*f*g-29*c*d^3*f*g+37*d^4*f*g+50*a^3*e*f*g-21*a^2*b*e*f*g+25*a*b^2*e*f*g+36*b^3*e*f*g+45*a^2*c*e*f*g-8*a*b*c*e*f*g+27*b^2*c*e*f*g-a*c^2*e*f*g-25*b*c^2*e*f*g-21*c^3*e*f*g+12*a^2*d*e*f*g-49*a*b*d*e*f*g-36*b^2*d*e*f*g+45*a*c*d*e*f*g-42*b*c*d*e*f*g-17*a*d^2*e*f*g+36*b*d^2*e*f*g-7*c*d^2*e*f*g+37*d^3*e*f*g-13*a^2*e^2*f*g-21*a*b*e^2*f*g-26*b^2*e^2*f*g+44*a*c*e^2*f*g-7*b*c*e^2*f*g+32*c^2*e^2*f*g-20*a*d*e^2*f*g+19*b*d*e^2*f*g+49*c*d*e^2*f*g+41*d^2*e^2*f*g-27*a*e^3*f*g+26*b*e^3*f*g+16*c*e^3*f*g-26*d*e^3*f*g+30*e^4*f*g-26*a^3*f^2*g-38*a^2*b*f^2*g+46*a*b^2*f^2*g-14*b^3*f^2*g-36*a^2*c*f^2*g+20*a*b*c*f^2*g+39*b^2*c*f^2*g+33*a*c^2*f^2*g+45*b*c^2*f^2*g-38*c^3*f^2*g+42*a^2*d*f^2*g+25*a*b*d*f^2*g+6*b^2*d*f^2*g+35*a*c*d*f^2*g-32*b*c*d*f^2*g+23*c^2*d*f^2*g+29*a*d^2*f^2*g-22*b*d^2*f^2*g+15*c*d^2*f^2*g+10*d^3*f^2*g+29*a^2*e*f^2*g+20*a*b*e*f^2*g-17*b^2*e*f^2*g+35*a*c*e*f^2*g-8*b*c*e*f^2*g+26*c^2*e*f^2*g+49*a*d*e*f^2*g+49*b*d*e*f^2*g+2*c*d*e*f^2*g-45*d^2*e*f^2*g-22*a*e^2*f^2*g-27*b*e^2*f^2*g-4*c*e^2*f^2*g-8*d*e^2*f^2*g+45*e^3*f^2*g+7*a^2*f^3*g+30*a*b*f^3*g-35*b^2*f^3*g-45*a*c*f^3*g+5*b*c*f^3*g-18*c^2*f^3*g-46*a*d*f^3*g-28*b*d*f^3*g+45*c*d*f^3*g-3*d^2*f^3*g-48*a*e*f^3*g-37*b*e*f^3*g-24*c*e*f^3*g-37*d*e*f^3*g-13*e^2*f^3*g-21*a*f^4*g+22*b*f^4*g-41*c*f^4*g+11*d*f^4*g+8*e*f^4*g+29*f^5*g-35*a^4*g^2-44*a^3*b*g^2-a^2*b^2*g^2+47*a*b^3*g^2-11*b^4*g^2+18*a^3*c*g^2-18*a^2*b*c*g^2-2*a*b^2*c*g^2-6*b^3*c*g^2+34*a^2*c^2*g^2-20*a*b*c^2*g^2+50*b^2*c^2*g^2+9*a*c^3*g^2-20*b*c^3*g^2+27*c^4*g^2-8*a^3*d*g^2+36*a^2*b*d*g^2+10*a*b^2*d*g^2+19*b^3*d*g^2+24*a^2*c*d*g^2+3*a*b*c*d*g^2+4*b^2*c*d*g^2+11*c^3*d*g^2-40*a^2*d^2*g^2+34*a*b*d^2*g^2+32*b^2*d^2*g^2+37*a*c*d^2*g^2-5*b*c*d^2*g^2+32*c^2*d^2*g^2+19*a*d^3*g^2-9*b*d^3*g^2-11*d^4*g^2-45*a^3*e*g^2+47*a^2*b*e*g^2-22*a*b^2*e*g^2-2*b^3*e*g^2+8*a^2*c*e*g^2-32*a*b*c*e*g^2+32*b^2*c*e*g^2-36*a*c^2*e*g^2+21*b*c^2*e*g^2-15*c^3*e*g^2+50*a^2*d*e*g^2-35*a*b*d*e*g^2-12*b^2*d*e*g^2-8*a*c*d*e*g^2+50*b*c*d*e*g^2+11*c^2*d*e*g^2+9*a*d^2*e*g^2-42*b*d^2*e*g^2-22*c*d^2*e*g^2-50*d^3*e*g^2-19*a^2*e^2*g^2-42*a*b*e^2*g^2-47*b^2*e^2*g^2+29*a*c*e^2*g^2+27*b*c*e^2*g^2+26*c^2*e^2*g^2-23*a*d*e^2*g^2+5*b*d*e^2*g^2-9*c*d*e^2*g^2+39*d^2*e^2*g^2+42*a*e^3*g^2+6*b*e^3*g^2-9*c*e^3*g^2-26*d*e^3*g^2-34*e^4*g^2+10*a^3*f*g^2+33*a^2*b*f*g^2+23*a*b^2*f*g^2-24*b^3*f*g^2+a^2*c*f*g^2-16*a*b*c*f*g^2-14*b^2*c*f*g^2+13*a*c^2*f*g^2+30*b*c^2*f*g^2+2*c^3*f*g^2-8*a^2*d*f*g^2-29*a*b*d*f*g^2-10*b^2*d*f*g^2-16*a*c*d*f*g^2+49*b*c*d*f*g^2+48*c^2*d*f*g^2+48*a*d^2*f*g^2+14*b*d^2*f*g^2+5*c*d^2*f*g^2+5*d^3*f*g^2-3*a^2*e*f*g^2-a*b*e*f*g^2-25*b^2*e*f*g^2-25*a*c*e*f*g^2+42*b*c*e*f*g^2+49*c^2*e*f*g^2+3*a*d*e*f*g^2-26*b*d*e*f*g^2-29*c*d*e*f*g^2-28*d^2*e*f*g^2+43*a*e^2*f*g^2+29*b*e^2*f*g^2+19*c*e^2*f*g^2-35*d*e^2*f*g^2+17*e^3*f*g^2+49*a^2*f^2*g^2-22*a*b*f^2*g^2+46*b^2*f^2*g^2+4*a*c*f^2*g^2+28*b*c*f^2*g^2-16*c^2*f^2*g^2+39*a*d*f^2*g^2+10*b*d*f^2*g^2+6*c*d*f^2*g^2-14*d^2*f^2*g^2+17*a*e*f^2*g^2-49*b*e*f^2*g^2+31*c*e*f^2*g^2+20*d*e*f^2*g^2+7*e^2*f^2*g^2+15*a*f^3*g^2+45*b*f^3*g^2-25*c*f^3*g^2-25*d*f^3*g^2+45*e*f^3*g^2+17*f^4*g^2-34*a^3*g^3+35*a^2*b*g^3-32*a*b^2*g^3+24*b^3*g^3-4*a^2*c*g^3-50*a*b*c*g^3-47*b^2*c*g^3+11*a*c^2*g^3-24*b*c^2*g^3+40*c^3*g^3+24*a^2*d*g^3+40*a*b*d*g^3+7*b^2*d*g^3-23*a*c*d*g^3+48*b*c*d*g^3-27*c^2*d*g^3-47*a*d^2*g^3+5*b*d^2*g^3+49*c*d^2*g^3-23*d^3*g^3-42*a^2*e*g^3-19*a*b*e*g^3+25*b^2*e*g^3+26*a*c*e*g^3+34*b*c*e*g^3-22*c^2*e*g^3+37*a*d*e*g^3-33*b*d*e*g^3-15*c*d*e*g^3+29*d^2*e*g^3-39*a*e^2*g^3+21*b*e^2*g^3-5*c*e^2*g^3-44*d*e^2*g^3+12*e^3*g^3-48*a^2*f*g^3+24*a*b*f*g^3-45*b^2*f*g^3-25*a*c*f*g^3+49*b*c*f*g^3+18*c^2*f*g^3-2*a*d*f*g^3+10*b*d*f*g^3-19*c*d*f*g^3+47*d^2*f*g^3+15*a*e*f*g^3+13*b*e*f*g^3-25*c*e*f*g^3+33*d*e*f*g^3+47*e^2*f*g^3-47*a*f^2*g^3-34*b*f^2*g^3+47*c*f^2*g^3-10*d*f^2*g^3-36*e*f^2*g^3-32*f^3*g^3-3*a^2*g^4+34*a*b*g^4-16*b^2*g^4+32*a*c*g^4+47*b*c*g^4-31*c^2*g^4+22*a*d*g^4-32*b*d*g^4-21*c*d*g^4+42*d^2*g^4+a*e*g^4-40*b*e*g^4+19*c*e*g^4-25*d*e*g^4+40*e^2*g^4+43*a*f*g^4-30*b*f*g^4-14*c*f*g^4-7*d*f*g^4-e*f*g^4+7*f^2*g^4+10*a*g^5+14*b*g^5+36*c*g^5-27*d*g^5+47*e*g^5-47*f*g^5-14*g^6);
  I = J1; -- #GB: 
  time gb J1;  --  sec
  time gb(J1, Algorithm=>LinearAlgebra); -- 27.0 sec
  time G3 = flatten entries map(ring J1, rawMGB(raw gens J1, 0, 1, "")); --   sec
  time G4 = flatten entries map(ring J1, rawMGB(raw gens J1, 1, 1, "")); -- 14.6 sec
  
  prefixDir = "~/src/M2-git/M2/Macaulay2/packages/MGBInterface/examples"
  makeExampleFiles(prefixDir, {{"random5556", "random5556()"}});
  -- run the following:
  mgb gb random5556 -reducer 26  -log +all
  mgb gb random5556 -reducer 26 # -log +all
///


--franzi-siphon-naive
  -- has 131 components, same as franzi-siphon-nonnaive
TEST ///
  -- benchmarking example, random5556 (gb-one-minute.m2)
  restart
  load "MGBInterface/f5ex.m2"
  loadPackage "MGBInterface"
  debug Core
  R1 = ZZ/101[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,A,B,C,D,E,F,G,H,I,J,K,L,M,MonomialSize => 8]
  J1 = ideal ( I*K-K^2, r*G-G^2, A*D-D^2, k*z-z^2, m*w-w^2, j^2-j*t, d*f-f^2, p*y*M-p^2, k*w*M-w^2, j*r*M-j*t, 
      I*J*L-J^2, B*H*L-H^2, A*E*L-E^2, b*C*L-C^2, b*B*L-b^2, t*x*L-x^2, a*u*L-a^2, m*q*L-q^2, d*g*L-g^2, 
      o*J*K-J^2, k*s*K-K^2, i*B*H-H^2, l*F*G-G^2, v*D*F-D^2, e*z*F-z^2, r*y*F-r^2, f*s*F-f^2, j*p*F-j*t, 
      o*D*E-E^2, r*s*D-D^2, b*i*C-C^2, s*v*y-v^2, i*j*x-x^2, i*q*w-q^2, a*o*u-a^2, i*k*r-r^2, f*g*o-g^2, 
      h*i*n-h^2, e*i*l-l^2, c*h*i-h^2, y^2*M^2-p^2, r^2*M^2-j*t, k^2*M^2-w^2, I^2*L^2-J^2, B^2*L^2-b^2, 
      A^2*L^2-E^2, t^2*L^2-x^2, m^2*L^2-q^2, d^2*L^2-g^2, y^2*F^2-r^2, v^2*F^2-D^2, s^2*F^2-f^2, 
      p^2*F^2-j*t, l^2*F^2-G^2, e^2*F^2-z^2, i^2*B^2-H^2, s^2*y^2-v^2, o^2*u^2-a^2, r^2*s^2-D^2, 
      k^2*s^2-K^2, i^2*k^2-r^2, e^2*i^2-l^2, c^2*i^2-h^2, b^2*i^2-C^2)

  -- The only one of the following which finish is G3.
  time gb J1;  --  sec
  time gb(J1, Algorithm=>LinearAlgebra); --  sec
  time G3 = flatten entries map(ring J1, rawMGB(raw gens J1, 0, 1, "")); -- 183.6 sec, mbg version: 156 sec (28,880 GB elements)
    -- note: mgb version does not actually pass over the answer.
  time G4 = flatten entries map(ring J1, rawMGB(raw gens J1, 1)); --  sec
  
  prefixDir = "~/src/M2-git/M2/Macaulay2/packages/MGBInterface/examples"
  makeExampleFiles(prefixDir, {{"franzi-naive", "franzi-naive()"}});
  makeExampleFiles(prefixDir|"/"|"franzi-naive",J1)
  -- run the following:
  mgb gb franzi-naive -reducer 26  -log +all
  mgb gb franzi-naive  -log +all
///

///  -- Creation of test files 
  -- file containing the examples
  restart
  needsPackage "MGBInterface"
  
  inputDir = "~/src/M2-git/M2/Macaulay2/packages/MGBInterface/"
  prefixDir = "~/src/M2-git/M2/Macaulay2/packages/MGBInterface/examples/foo"
  createExamples(prefixDir, "-grevlex-level1", inputDir|"/gb-grevlex-level1.m2")

///


///  -- Creation of test files 
  -- file containing the examples
  restart
  needsPackage "MGBInterface"
  
  inputDir = "~/src/M2-git/M2/Macaulay2/packages/MGBInterface/"
  prefixDir = "~/src/M2-git/M2/Macaulay2/packages/MGBInterface/examples/foo2"
  createExamples(prefixDir, "grevlex-level1", inputDir|"/gb-grevlex-level1.m2")

///
beginDocumentation()

doc ///
Key
  MGBInterface
Headline
Description
  Text
  Example
Caveat
SeeAlso
///

doc ///
Key
Headline
Usage
Inputs
Outputs
Consequences
Description
  Text
  Example
  Code
  Pre
Caveat
SeeAlso
///

TEST ///
-- test code and assertions here
-- may have as many TEST sections as needed
///

