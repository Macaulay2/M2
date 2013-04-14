newPackage(
        "MGBInterface",
        Version => "0.1", 
        Date => "12 April 2013",
        Authors => {{Name => "Mike Stillman", 
                  Email => "", 
                  HomePage => ""}},
        Headline => "Experimental package for mathicgb interface.  Not meant for general use",
        PackageExports => {"ExampleIdeals"},
        DebuggingMode => true
        )

debug Core
-- The code here is that part of Issac2012Examples.m2 which
-- deals with actually running and obtaining the examples
--
-- It also has the routines from there which involve writing example
-- files for other computer algebra systems.

export {write,
     toClassic,
     displayit,
     toABC,
     makeExampleFiles,
     helpMGB,
     mgbStr,
     runMGB,
     doMGB,
     testMGB
     }

write = method()
write Ring  := (R) -> (
     -- R should be a polynomial ring
     s1 := char R | " " | numgens R | " 1";
     D := apply(degrees R, first);
     s2 := concatenate(for d in D list (" "|d));
     s1 | s2 | "\n"
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
----------------------------------------
mgbOptions = hashTable {
     "AutoTailReduce" => {"off", "-autoTailReduce "},
     "AutoTopReduce" => {"on", "-autoTopReduce "},
     "BreakAfter" => {0, "-breakAfter "},
     "DivisorLookup" => {2, "-divisorLookup"},
     "Log" => {"", "-log"},
     "MemoryQuantumForReducer" => {1048576, "-memoryQuantumForReducer "},
     "MonomialTable" => {2, "-monomialTable "},
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
          "Executable" => "~/src/github/mathicgb/rel/mgb"
          }
     )
runMGB = method(Options => optionsMGB)

{*
runMGB = method(Options => {
	  Order => 4, 
	  Reducer => 4, 
	  DivLookup => 2, 
	  SyzTable => 2,
	  Verbose => 0, 
	  FullReduction=>true, 
	  Algorithm => 0,
	  FileName => "/tmp/foo1",
	  Executable => "~/src/github/mathicgb/rel/mgb"})
*}

helpMGB = () -> get ("!"|(options runMGB)#"Executable"| " help gb");

mgbStr = method(Options => options runMGB)
mgbStr String := opts -> (projectName) -> (
     execString := opts#"Executable" | " gb "|projectName|" ";
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
     projectName := temporaryFileName();
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

testMGB = method()
testMGB Ideal := (I) -> (
     time G1 := doMGB I;
     time G2 := flatten entries gens if isHomogeneous I then gb(I, Algorithm=>LinearAlgebra) else gb I;
     --time G2 := flatten entries gens gb I;
     time G3 := flatten entries map(ring I, rawMGB(raw gens I, 0));
     time G4 := flatten entries map(ring I, rawMGB(raw gens I, 1));
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

end

TEST ///
-- crerating some test files
restart
load "MGBInterface/f5ex.m2"
loadPackage "MGBInterface"
myexamples = {
     {"trinks", "trinks()"},
     {"eco6", "eco6()"}
     }
prefixDir = "~/src/M2-git/M2/Macaulay2/packages/MGBInterface/examples"
makeExampleFiles(prefixDir, myexamples)
///

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
testMGB myexamples
for e in myexamples do (
     << "testing: " << e#0 << endl;
     J = value e#1;
     testMGB J
     )
///

///
  restart
  loadPackage "MGBInterface"
  R = ZZ/101[a..d]
  I = ideal"a2-bc,a3-b3,a4-b4-c4"
  debug Core
  rawMGB raw gens I
  makeExampleFiles("first-eg", I)
  runMGB I
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
  -- benchmarking example, hilbertkunz1
  restart
  load "MGBInterface/f5ex.m2"
  loadPackage "MGBInterface"
  debug Core
  I = hilbertkunz1() -- #GB: 150
  time gb I;  -- 
  time gb(I, Algorithm=>LinearAlgebra); -- 36.2 sec, 440 MB real memory, 725 MB virtual
  time G3 = flatten entries map(ring I, rawMGB(raw gens I, 0)); -- 5.5 sec
  time G4 = flatten entries map(ring I, rawMGB(raw gens I, 1)); -- 2.3 sec
  
  prefixDir = "~/src/M2-git/M2/Macaulay2/packages/MGBInterface/examples"
  makeExampleFiles(prefixDir, {{"hilbertkunz1", "hilbertkunz1()"}});
  -- run the following:
  mgb gb hilbertkunz1 -reducer 26 -sP 100000000 -log +all
///

TEST ///
  -- benchmarking example, yang1
  restart
  load "MGBInterface/f5ex.m2"
  loadPackage "MGBInterface"
  debug Core
  I = yang1() -- #GB: 
  time gb I;  -- 37.2 sec
  time gb(I, Algorithm=>LinearAlgebra); -- 36 sec
  time G3 = flatten entries map(ring I, rawMGB(raw gens I, 0)); -- 4.02  sec
  time G4 = flatten entries map(ring I, rawMGB(raw gens I, 1)); -- 171.7 sec (at 1000 spairs per group, time goes way down, I think, to about 24 sec?)
  
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
  time G3 = flatten entries map(ring J1, rawMGB(raw gens J1, 0)); --   sec
  time G4 = flatten entries map(ring J1, rawMGB(raw gens J1, 1)); -- 14.6 sec
  
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
  time G3 = flatten entries map(ring J1, rawMGB(raw gens J1, 0)); -- 183.6 sec, mbg version: 156 sec (28,880 GB elements)
    -- note: mgb version does not actually pass over the answer.
  time G4 = flatten entries map(ring J1, rawMGB(raw gens J1, 1)); --  sec
  
  prefixDir = "~/src/M2-git/M2/Macaulay2/packages/MGBInterface/examples"
  makeExampleFiles(prefixDir, {{"franzi-naive", "franzi-naive()"}});
  makeExampleFiles(prefixDir|"/"|"franzi-naive",J1)
  -- run the following:
  mgb gb franzi-naive -reducer 26  -log +all
  mgb gb franzi-naive  -log +all
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

