newPackage(
        "NCAlgebraV2",
        Version => "0.1", 
        Date => "16 Feb 2016",
        Authors => {{Name => "Frank Moore", 
                  Email => "", 
                  HomePage => ""},
              {Name => "Mike Stillman", 
                  Email => "", 
                  HomePage => ""}
              },
        Headline => "Package for computations with noncommutative algebras"
        )

export {
    "NCFreeAlgebra",
    "sequenceToVariableSymbols",
    "NCQuotientRing",
    "NCIdeal",
    "ncIdeal",
    "NCGroebnerBasis",
    "ncGroebnerBasis",
    "InstallGB",
    "ReturnIdeal",
    "CacheBergmanGB",
    "NumModuleVars",
    "MakeMonic"
}

exportMutable {
    "bergmanPath"
    }

debug Core

NCFreeAlgebra = new Type of EngineRing
NCFreeAlgebra.synonym = "noncommutative free algebra"
NCQuotientRing = new Type of Ring
NCQuotientRing.synonym = "quotient of a noncommutative free algebra"
NCIdeal = new Type of HashTable
NCGroebnerBasis = new Type of HashTable

protect BergmanRing
protect MaxNCGBDegree
protect MinNCGBDegree
protect MaxCoeffDegree
protect MinCoeffDegree
protect ComputeNCGB
protect DontUse

new NCFreeAlgebra from List := (EngineRing, inits) -> new EngineRing of RingElement from new HashTable from inits

-------------------------------------------------
--- Helpful general-purpose functions
-------------------------------------------------

minUsing = (xs,f) -> (
   n := min (xs / f);
   first select(1,xs, x -> f x == n)
)

sortUsing = (xs,f) -> (sort apply(xs, x -> (f x, x))) / last

getNameIfAny = (k) -> expression if hasAttribute(k,ReverseDictionary) then getAttribute(k,ReverseDictionary) else k

-- MES.  do we really need all 5 of these?
expression NCFreeAlgebra := R -> (
     if hasAttribute(R,ReverseDictionary) then return expression getAttribute(R,ReverseDictionary);
     k := last R.baseRings;
     (getNameIfAny k) (R.generatorSymbols)
     )
net NCFreeAlgebra := R -> (
     if hasAttribute(R,ReverseDictionary) then toString getAttribute(R,ReverseDictionary)
     else net expression R)
describe NCFreeAlgebra := R -> (
     k := last R.baseRings;
     net ((getNameIfAny k) R.generatorSymbols)
     )
toExternalString NCFreeAlgebra := R -> (
    --toString describe R
     k := last R.baseRings;
     toString ((getNameIfAny k) R.generatorSymbols)
     )
toString NCFreeAlgebra := R -> (
    toString expression R
    -- 
    -- if hasAttribute(R,ReverseDictionary) then toString getAttribute(R,ReverseDictionary)
    -- else toString expression R
    )

-- Currently, we are using findSymbols in m2/ofcm.m2 instead of this.
sequenceToVariableSymbols = args -> (
    variables := splice args;
    v := flatten toList apply(variables, x -> if class x === MutableList then toList x else x);
    for v0 in v list (
	    try baseName v0
	    else if instance(v0,String) and match("[[:alnum:]$]+",v0) then getSymbol v0
	    else error ("name " | v0 | " not usable as a variable")
       )
    )

Ring List := (A, args) -> (
   -- get the symbols associated to the list that is passed in, in case the variables have been used earlier.
   opts := new OptionTable from {Degrees=>null, DegreeRank=>null};
   (opts,args) = override(opts,toSequence args);
   varList := args;
   if not (A.?Engine and A.Engine) then
       error "expected coefficient ring handled by the engine";
   --varSymbols := sequenceToVariableSymbols toSequence varList;
   varSymbols := findSymbols toSequence varList;
   if #varSymbols == 0 then error "Expected at least one variable.";
   (degs,degrk) := processDegrees( opts.Degrees, opts.DegreeRank, length varSymbols);
   rawR := rawNCFreeAlgebra(raw A, toSequence(varSymbols/toString), raw degreesRing degrk, flatten degs);
   R := new NCFreeAlgebra from {
       (symbol RawRing) => rawR,
       (symbol generators) => {},
       (symbol generatorSymbols) => varSymbols,
       --(symbol generatorExpressions) => hashTable apply(#varList, i -> (i,expression varList#i)),
       (symbol generatorExpressions) => for v in varSymbols list if instance(v,Symbol) then v else expression v,
       (symbol degreesRing) => degreesRing degrk,
       (symbol degreeLength) => degrk,
       (symbol degrees) => degs,
       (symbol CoefficientRing) => A,
       (symbol cache) => new CacheTable from {},
       (symbol BergmanRing) => false,
       (symbol baseRings) => append(A.baseRings,A)
       };
   newGens := for i from 0 to #varSymbols-1 list varSymbols#i <- new R from R.RawRing_i;
   R.generators = newGens;
   commonEngineRingInitializations R;
   -- tell Macaulay2 that this is ring the Bergman understands
   if A === QQ or A === ZZ/(char A) then R#BergmanRing = true;
   --- need to fix net of an RingElement coming from a NCPolynomial ring.
   processFactor := (k,v) -> if v =!= 1 then Power{R.generatorExpressions#k, v} else R.generatorExpressions#k;
   processFactors := facs -> (
	  if #facs  === 1
	  then processFactor facs#0
	  else new Product from apply(facs, processFactor));
   -- TODO:
   -- In order to get expression R to display monomials correctly, one must:
   -- 1. Create a new function which returns (coeff, a list of integers representing the monomial)
   -- 2. Use this function below, together with a modified version of the code above to
   --    create an expression of a monomial.
   expression R := f -> (
	       (
		    rawP := rawPairs(raw coefficientRing R, raw f);
		    -- apply the following function to the output of rawPairs
		    (coeffs,monoms) -> (
			 if #coeffs === 0
			 then expression 0
			 else sum(coeffs,monoms, (a,m) -> expression (if a == 1 then 1 else promote(a,A)) * expression (if m == {} then 1 else processFactors m))
			 )
		    ) (rawP#0, rawP#1 / rawSparseListFormMonomial)
        );
   net R := f -> net expression f;
   R
   );

NCFreeAlgebra _ ZZ := (R, n) -> (R.generators)#n
coefficientRing NCFreeAlgebra := R -> last R.baseRings

degreesRing NCFreeAlgebra := PolynomialRing => R -> (
   if R#?(symbol degreesRing) then R#(symbol degreesRing)
   else error "no degreesRing for this ring"
   )

isWellDefined NCFreeAlgebra := Boolean => R -> (
    -- an internal check that R is defined properly.
    isbad := str -> (if debugLevel > 0 then << str; false);
    if # R.generators =!= numgens R then 
        return isbad "# R.generators =!= numgens R";
    if # R.generatorExpressions =!= numgens R then 
        return isbad "# R.generatorExpressions =!= numgens R";
    if # R.generatorSymbols =!= numgens R then 
        return isbad "# R.generatorSymbols =!= numgens R";
    if not all(R.generators, x -> class x === R) then 
        return isbad "generators are not all in the ring";
    if not all(R.generatorExpressions, x -> instance(x,Expression) or instance(x,Symbol)) then
        return isbad "generatorExpressions are not all expressions";
    if not all(R.generatorSymbols, x -> instance(x, Symbol) or instance(x, IndexedVariable)) then
        return isbad "generatorSymbols are not all symbols or indexed variables";
    if not instance(expression R, Expression) then
        return isbad "expression R should be an expression";
    if not instance(describe R, Net) then
        return isbad "'describe R' should be a Net";
    if not instance(net R, Net) then
        return isbad "'net R' should be a Net";
    if not instance(toString R, String) then
        return isbad "'toString R' should be a string";
    if not instance(toExternalString R, String) then
        return isbad "'toExternalString R' should be a string";
    true
    )

-------------------------------------------
--- NCIdeal functions ---------------------
-------------------------------------------

ncIdeal = method()
ncIdeal List := idealGens -> (
   if #idealGens == 0 then error "Expected at least one generator.";
   new NCIdeal from new HashTable from {(symbol ring) => ring idealGens#0,
                                        (symbol generators) => idealGens,
                                        (symbol cache) => new CacheTable from {}}
)

ncIdeal RingElement := f -> ncIdeal {f}

generators NCIdeal := opts -> I -> I.generators;

isHomogeneous NCIdeal := I -> all(gens I, isHomogeneous)

net NCIdeal := I -> "Two-sided ideal " | net (I.generators);

ring NCIdeal := Ring => I -> I.ring

NCIdeal + NCIdeal := (I,J) -> (
    if ring I =!= ring J then error "Expected ideals over the same ring.";
    ncIdeal (gens I | gens J)
)

------------------------------------------------------
---- RingElement code for noncommutative polynomials
------------------------------------------------------

--- a bit of a hack for now, but the below function creates
--- a copy of the monList which was part of the old NCMonomial type
--- which the below functions will work on.
monList = method()
monList RingElement := f -> (
   genSymbols := (ring f).generatorSymbols;
   sparseListForm := last rawPairs(raw coefficientRing ring f, raw leadTerm f) / rawSparseListFormMonomial // first;
   flatten apply(sparseListForm, p -> genSymbols_(toList (p#1:p#0)))
)

--- putInRing is also a function used before, and this places a list of symbols
--- back as a monomial in the ring over which we are working, together with
--- a coefficient.
putInRing = method()
putInRing (List, NCFreeAlgebra, ZZ) :=
putInRing (List, NCFreeAlgebra, QQ) :=
putInRing (List, NCFreeAlgebra, RingElement) := (m,A,c) -> (
   if not A.cache#?"generatorSymbolHash" then (
      A.cache#"generatorSymbolHash" = hashTable apply(numgens A, i -> (A.generatorSymbols#i,i));
   );
   genSymbolHash := A.cache#"generatorSymbolHash";
   (product apply(m, v -> A_(genSymbolHash#v)))*c
)


tensorLength = method()
tensorLength RingElement := f -> (
   R := ring f;
   rawP := rawPairs(raw coefficientRing R, raw f);
   max apply(last rawP / rawSparseListFormMonomial, t -> sum apply(t, p -> last p))
) 

leadMonomialForGB = f -> (
   R := coefficientRing ring f;
   if isField R then
      (ncLeadMonomial f, 1_R)
   else
      (ncLeadMonomial f, leadMonomial leadCoefficient f)
)

clearDenominators = method()
clearDenominators RingElement := f -> (
   if coefficientRing ring f =!= QQ then (f,f) else (
      coeffDens := apply(flatten entries last coefficients f, p -> if class p === QQ then denominator p else 1);
      myLCM := lcm coeffDens;
      (f*myLCM,myLCM)
   )
)

getMinMaxDegrees = gensList -> (
   minNCGBDeg := minCoeffDeg := infinity;
   maxNCGBDeg := maxCoeffDeg := -infinity;
   scan(gensList, f -> (degf := tensorLength f;  
                        degLeadCoeff := if isField coefficientRing ring f then 0 else first degree leadCoefficient f;
                        if degf > maxNCGBDeg then maxNCGBDeg = degf;
                        if degf < minNCGBDeg then minNCGBDeg = degf;
                        if degLeadCoeff > maxCoeffDeg then maxCoeffDeg = degLeadCoeff;
                        if degLeadCoeff < minCoeffDeg then minCoeffDeg = degLeadCoeff;));
   (minNCGBDeg,maxNCGBDeg,minCoeffDeg,maxCoeffDeg)
)

ncLeadMonomial = f -> first flatten entries first coefficients leadTerm f


--- looks for noncommutative substrings of length at least m and at most n
ncSubstrings = method()
ncSubstrings (RingElement,ZZ,ZZ) := (mon,m,n) -> (
   monLen := #(mon#monList);
   flatten apply(toList(1..(monLen)), i -> if i > n or i < m then 
                                            {}
                                         else
                                            apply(monLen-i+1, j -> (mon_{0..(j-1)},mon_{j..j+i-1},mon_{j+i..(monLen-1)})))
)

--- looks for commutative 'substrings'
cSubstrings = method()
cSubstrings (List,ZZ,ZZ) := (exps,m,n) -> (
   if #exps == 1 then 
      apply(toList (m..(min(exps#0,n))),l -> {l})
   else
      flatten for i from 0 to min(exps#0,n) list apply(cSubstrings(drop(exps,1),max(0,m-i),max(0,n-i)), l -> {i} | l)
)


------------------------------------------------------------------
------- NCGroebnerBasis methods
------------------------------------------------------------------
ring NCGroebnerBasis := Igb -> ring first gens Igb

generators NCGroebnerBasis := opts -> ncgb -> (pairs ncgb.generators) / last

ncGroebnerBasis = method(Options => {DegreeLimit => 20,
                                     InstallGB => false})
ncGroebnerBasis List := opts -> fList -> (
   if opts#InstallGB then (
      -- eliminate repeats
      fList = unique fList;
      -- make monic
      fList = apply(fList, f -> (coeff := leadCoefficient f; if isUnit coeff then (coeff)^(-1)*f else (leadCoefficient coeff)^(-1)*f));
      (minNCGBDeg,maxNCGBDeg,minCoeffDeg,maxCoeffDeg) := getMinMaxDegrees(fList);
      new NCGroebnerBasis from hashTable {(symbol generators) => hashTable apply(fList, f -> (leadMonomialForGB f,f)),
                                          (symbol cache) => new CacheTable from {},
                                          MaxNCGBDegree => maxNCGBDeg,
                                          MinNCGBDegree => minNCGBDeg,
                                          MaxCoeffDegree => maxCoeffDeg,
                                          MinCoeffDegree => minCoeffDeg}
   )
   else ncGroebnerBasis(ncIdeal fList,opts)
)

ncGroebnerBasis NCIdeal := opts -> I -> (
   if I.cache#?gb then return I.cache#gb;
   ncgb := if opts#InstallGB then (
              gensI := apply(gens I, f -> (coeff := leadCoefficient f; if isUnit coeff then (leadCoefficient f)^(-1)*f else promote((leadCoefficient coeff)^(-1), coefficientRing ring f)*f));
              (minNCGBDeg,maxNCGBDeg,minCoeffDeg,maxCoeffDeg) := getMinMaxDegrees(gensI);
              new NCGroebnerBasis from hashTable {(symbol generators) => hashTable apply(gensI, f -> (leadMonomialForGB f,f)),
                                                  (symbol cache) => new CacheTable from {},
                                                  MaxNCGBDegree => maxNCGBDeg,
                                                  MinNCGBDegree => minNCGBDeg,
                                                  MaxCoeffDegree => maxCoeffDeg,
                                                  MinCoeffDegree => minCoeffDeg}
   )
   else twoSidedNCGroebnerBasisBergman(I, DegreeLimit => opts#DegreeLimit);
   I.cache#gb = ncgb;
   ncgb   
)
net NCGroebnerBasis := ncgb -> (
   stack apply(sort pairs ncgb.generators, (lt,pol) -> (net pol) | net "; Lead Term = " | (net lt))
)

ZZ % NCGroebnerBasis := (n,ncgb) -> n
QQ % NCGroebnerBasis := (n,ncgb) -> n

-------------------------------------------------------
---- Reduction functions   ----------------------------
-------------------------------------------------------

RingElement % NCGroebnerBasis := (f,ncgb) -> (
   divisionAlgorithm(f,ncgb)
   -- don't put in normalFormBergman just yet
   --if (degree f <= MAXDEG and size f <= MAXSIZE) or not f.ring#BergmanRing then
   --   remainderFunction(f,ncgb)
   --else
   --   first normalFormBergman({f},ncgb)
)

--- these two functions make up the Knuth-Morris-Pratt prefix
--- string searching algorithm.  KMP uses a prefix function
--- built from the string we are searching for.  We cache this
--- information since we use it often.
prefixFunction = method()
prefixFunction RingElement := m -> (
   --m = (first first pairs m.terms)#monList;
   m = monList m;
   prefixHash := new MutableHashTable from apply(#m, i -> (i+1,0));
   prefixHash#1 = 0;
   k := 0;
   for q from 2 to #m do (
      while k > 0 and m#k =!= m#(q-1) do k = prefixHash#k;
      if m#k === m#(q-1) then k = k + 1;
      prefixHash#q = k;
   );
   prefixHash
)

isSubwordKMP = method()
isSubwordKMP (RingElement, RingElement) := (g,f) -> (
   A := ring g;
   --- build prefix table for g
   --- not sure if caching this saves any time.
   --if not g.cache#?"LeadTermPrefixTable" then g.cache#"LeadTermPrefixTable" = prefixFunction leadMonomial g;
   --pref := g.cache#"LeadTermPrefixTable";
   --m1 := (first first pairs (leadMonomial g).terms)#monList;
   --m2 := (first first pairs (leadMonomial f).terms)#monList;
   pref := prefixFunction ncLeadMonomial g;
   m1 := monList ncLeadMonomial g;
   m2 := monList ncLeadMonomial f;
   deg1 := #m1;
   deg2 := #m2;
   if deg1 > deg2 then return null;
   k := 0;
   shift := -1;
   for q from 1 to deg2 do (
      while k > 0 and m1#k =!= m2#(q-1) do k = pref#k;
      if m1#k === m2#(q-1) then k = k + 1;
      if k == deg1 then ( shift = q - deg1; break; );      
   );
   if shift == -1 then null
   else (putInRing(m2_{0..shift-1},A,1), putInRing(m2_{(shift+deg1)..(deg2-1)},A,1))
)

isDivisible = method()
isDivisible (RingElement, List) := (f, L) -> (
   --- this function returns either null if no match is found
   --- or a triple of the form (leftQ, g, rightQ) where the lead
   --- term of f is (leftQ)*(leadterm g)*(rightQ)
   for g in L do (
      subwd := isSubwordKMP(g, f);
      if subwd =!= null then return (subwd#0, g, subwd#1);
   );
   return null;
)

divisionAlgorithm = method()
divisionAlgorithm (RingElement, NCGroebnerBasis) := (f,L) -> (
   --    f % L
   --- as it stands currently, this code does not yet handle coefficient rings
   --- that are not fields.
   A := ring f; -- should be a ring with a multiplicative basis
   curPoly := f;
   gensL := gens L;
   leftQuotients := new MutableHashTable from apply(gensL, g -> (g,0_A));
   rightQuotients := new MutableHashTable from apply(gensL, g -> (g,0_A));
   rem := 0_A;
   while true do (
      isDiv := isDivisible(curPoly,gensL);
      if isDiv === null then (
         ltf := leadTerm curPoly;
	 rem = rem + ltf;
	 curPoly = curPoly - ltf;
      )
      else (
	 lc := leadCoefficient curPoly;
	 curPoly = curPoly - lc*isDiv#0*isDiv#1*isDiv#2;
	 leftQuotients#(isDiv#1) = leftQuotients#(isDiv#1) + lc*isDiv#0;
	 rightQuotients#(isDiv#1) = rightQuotients#(isDiv#1) + isDiv#2;
      );
      if curPoly == 0 then break;
   );
   rem
)

----------------------------------------
--- Bergman related functions
----------------------------------------

runCommand = cmd -> (
   --- comment this line out eventually, or add a verbosity option
   stderr << "--running: " << cmd << " ... " << flush;
   r := run cmd;
   if r != 0 then (
      << "Failed!" << endl;
      error("--command failed, error return code ",r);
   )
   else stderr << "Complete!" << endl;
)

makeVarListString = B -> (
   varListString := "vars ";
   --- note the reverse here.  This is because in Macaulay2, by convention
   --- when you list the variables in order a,b,c this means a > b > c,
   --- while in Bergman this means a < b < c.
   gensB := reverse gens B;
   lastVar := last gensB;
   varListString = varListString | (concatenate apply(drop(gensB,-1), x -> (toString x) | ","));
   varListString | (toString lastVar) | ";"
)

makeGenListString = genList -> (
   lastGen := last genList;
   genListString := concatenate apply(drop(genList,-1), f -> toString first clearDenominators f | ",\n");
   genListString | (toString first clearDenominators lastGen) | ";\n"
)

makeVarWeightString = (B,n) -> (
   gensB := reverse gens B;
   numgensB := #gensB;
   adjust := if n == 0 then 0 else (
      minDeg := min (drop(gensB,numgensB - n) / degree);
      if minDeg <= 0 then 1-minDeg else 0
   );
   concatenate apply(#gensB,i -> (if i >= numgensB - n then toString ((first degree gensB#i) + adjust) else toString first degree gensB#i) | " ")
)

writeBergmanInputFile = method(Options => {ComputeNCGB => true,
                                           DegreeLimit => 10,
                                           NumModuleVars => 0})
writeBergmanInputFile (List, String) := opts -> (genList, tempInput) -> (
   B := ring first genList;
   charB := char coefficientRing B;
   degList := (genList / degree / sum) | {0};
   maxDeg := max(opts#DegreeLimit, 2*(max degList));
   genListString := makeGenListString genList;
   writeBergmanInputFile(B,
                         genListString,
                         tempInput,
                         NumModuleVars => opts#NumModuleVars,
                         DegreeLimit => maxDeg,
                         ComputeNCGB => opts#ComputeNCGB);
)

writeBergmanInputFile (Ring,String,String) := opts -> (B,genListString,tempInput) -> (
   if class B =!= NCFreeAlgebra then error "Expected a noncommutative free algebra.";
   if opts#DegreeLimit == -infinity then error "Unknown error.  Degree limit is -infinity.";
   fil := openOut tempInput;
   varListString := makeVarListString B;
   charB := char coefficientRing B;
   weightString := makeVarWeightString(B,opts#NumModuleVars);
   -- print the setup of the computation
   if not opts#ComputeNCGB then
   (
      -- if we don't want to recompute the GB, we need to tell Bergman that there are no
      -- Spairs to work on for twice the max degree of the gens we send it so it
      -- doesn't try to create any more Spairs.
      fil << "(load \"" << bergmanPath << "/lap/clisp/unix/hseries.fas\")" << endl;
      -- This is trying to get the 'bmload' environment variable to load correctly.
      -- fil << "(load (mkbmpathexpand \"$bmload/hseries.fas\"))" << endl;
      -- or is it this:?
      -- (from master) fil << "(load (mkbmpathexpand \"$bmload/lap/clisp/unix/hseries.fas\"))" << endl;
      fil << "(setinterruptstrategy minhilblimits)" << endl;
      fil << "(setinterruptstrategy minhilblimits)" << endl;
      fil << "(sethseriesminima" << concatenate(opts#DegreeLimit:" skipcdeg") << ")" << endl;
   );
   fil << "(noncommify)" << endl;
   fil << "(setmodulus " << charB << ")" << endl;
   fil << "(setweights " << weightString << ")" << endl;
   fil << "(setmaxdeg " << opts#DegreeLimit << ")" << endl;
   
   if opts#NumModuleVars != 0 then
      fil << "(setq nmodgen " << opts#NumModuleVars << ")" << endl;
   
   fil << "(algforminput)" << endl;
   
   -- print out the list of variables we are using
   fil << varListString << endl;
   
   --- print out the generators of ideal
   fil << genListString << endl << close;
)

------------------------------------------------------
----- Bergman GB commands
------------------------------------------------------

writeGBInitFile = method()
writeGBInitFile (String, String, String) := (tempInit, tempInput, tempOutput) -> (
   fil := openOut tempInit;
   fil << "(simple \"" << tempInput << "\" \"" << tempOutput << "\")" << endl;
   fil << "(quit)" << endl << close;   
)

gbFromOutputFile = method(Options => {ReturnIdeal => false,
                                      CacheBergmanGB => true,
                                      MakeMonic => true})
gbFromOutputFile(NCFreeAlgebra,String) := opts -> (A,tempOutput) -> (
   fil := openIn tempOutput;
   totalFile := get fil;
   fileLines := drop(select(lines totalFile, l -> l != "" and l != "Done"),-1);
   gensString := select(fileLines, s -> s#0#0 != "%");
   numLines := #fileLines;
   fileLines = concatenate for i from 0 to numLines-1 list (
                              if i != numLines-1 then
                                 fileLines#i | "\n"
                              else
                                 replace(",",";",fileLines#i) | "\n"
                           );
   -- save the 'old' state to move to tensor algebra (should I be doing all this with dictionaries?)
   oldVarSymbols := A.generatorSymbols;
   oldVarValues := oldVarSymbols / value;
   use A;
   -- load dictionary on dictionaryPath if present.
   if A.cache#?Dictionary then dictionaryPath = prepend(A.cache#Dictionary,dictionaryPath);
   -- switch to tensor algebra
   gensList := select(gensString / value, f -> class f === Sequence) / first;
   -- roll back dictionaryPath, if present
   if A.cache#?Dictionary then dictionaryPath = drop(dictionaryPath,1);
   -- roll back to old variables (maybe no longer in tensor algebra)
   scan(oldVarSymbols, oldVarValues, (sym,val) -> sym <- val);
   if opts#MakeMonic then
      gensList = apply(gensList, f -> (leadCoefficient f)^(-1)*f);
   (minNCGBDeg,maxNCGBDeg,minCoeffDeg,maxCoeffDeg) := getMinMaxDegrees(gensList);
   ncgb := new NCGroebnerBasis from hashTable {(symbol generators) => hashTable apply(gensList, f -> (leadMonomialForGB f,f)),
                                               (symbol cache) => new CacheTable from {},
                                               MaxNCGBDegree => maxNCGBDeg,
                                               MinNCGBDegree => minNCGBDeg,
                                               MaxCoeffDegree => maxCoeffDeg,
                                               MinCoeffDegree => minCoeffDeg};
   -- now write gb to file to be used later, and stash answer in ncgb's cache
   if opts#CacheBergmanGB then (
      cacheGB := temporaryFileName() | ".bigb";
      B := if #gensList > 0 then ring first gensList
           else A;
      maxDeg := 2*(max((gensList / degree / first)|{0}));
      writeBergmanInputFile(B,
                            fileLines,
                            cacheGB,
                            ComputeNCGB=>false,
                            DegreeLimit=>maxDeg);
      ncgb.cache#"bergmanGBFile" = cacheGB;
   );
   if opts#ReturnIdeal then (
      I := ncIdeal gensList;
      I.cache#gb = ncgb;
      I
   )
   else
      ncgb
)

twoSidedNCGroebnerBasisBergman = method(Options=>{DegreeLimit=>10,
                                                  NumModuleVars=>0,
                                                  MakeMonic=>true,
                                                  CacheBergmanGB=>true})
twoSidedNCGroebnerBasisBergman List := opts -> fList -> twoSidedNCGroebnerBasisBergman(ncIdeal fList,opts)
twoSidedNCGroebnerBasisBergman NCIdeal := opts -> I -> (
  local phi;
  oldI := I;
  if not (ring I)#BergmanRing then
  (
     -- If the coefficients of the gens of I only use QQ or ZZ/p, then compute
     -- gb over the ring with QQ or ZZ/p coefficients, then sub back into the
     -- right ring.
     k := bergmanCoefficientRing gens I;
     if k === null then error "Bergman interface can only handle coefficients over QQ or ZZ/p at the present time.";
     I = newBergmanIdealRing(I,k);
     phi = map(ring oldI, ring I, gens ring oldI);
  );
  -- call Bergman for this, at the moment
  tempInit := temporaryFileName() | ".init";      -- init file
  tempInput := temporaryFileName() | ".bi";       -- gb input file
  tempOutput := temporaryFileName() | ".bo";      -- gb output goes here
  tempTerminal := temporaryFileName() | ".ter";   -- terminal output goes here
  gensI := gens I;
  writeBergmanInputFile(gensI,
                        tempInput,
                        DegreeLimit=>opts#DegreeLimit,
                        NumModuleVars=>opts#NumModuleVars);
  writeGBInitFile(tempInit,tempInput,tempOutput);
  stderr << "--Calling Bergman for NCGB calculation." << endl;
  runCommand("bergman -i " | tempInit | " -on-error exit --silent > " | tempTerminal);
  retVal := gbFromOutputFile(ring I,
                             tempOutput,
                             MakeMonic=>opts#MakeMonic,
                             CacheBergmanGB=>opts#CacheBergmanGB);
  -- at this point, if we are not a BergmanRing, then we could compute over QQ or ZZ/p
  -- sub the retVal back to the original ring.
  if not oldI.ring#BergmanRing then phi retVal else retVal
)

bergmanCoefficientRing = method()
bergmanCoefficientRing List := L -> (
   --- this function returns true if all the coefficients
   --- of all the elements in the list L are in either QQ or FF_p
   coeffs := flatten apply(L, f -> (pairs f.terms) / last);
   if all(coeffs, c -> sub(sub(c,QQ),ring c) == c) then
   (
      QQ
   )
   else
   (
      p := char ring first coeffs;
      if all(coeffs, c -> sub(sub(c,ZZ/p),ring c) == c) then ZZ/p else null
   )
)

newBergmanIdealRing = method()

newBergmanIdealRing (NCIdeal,Ring) := (I,k) -> (
   if k =!= QQ and k =!= ZZ/(char k) then
      error "Expected a coefficient ring of QQ or ZZ/p for some p";
   n := numgens ring I;
   XX := getSymbol("XX");
   tempRing := k{XX_1..XX_n};
   phi := map(tempRing,ring I, gens tempRing);
   phi I
)

-*

-------------------------------------------
--- NCQuotientRing functions --------------
-------------------------------------------

new NCQuotientRing from List := (NCQuotientRing, inits) -> new NCQuotientRing of RingElement from new HashTable from inits

NCFreeAlgebra / NCIdeal := (A, I) -> (
   ncgb := ncGroebnerBasis I;
   B := new NCQuotientRing from {(symbol generators) => {},
                                 (symbol generatorSymbols) => A.generatorSymbols, 
                                 (symbol generatorExpressions) => for v in varSymbols list if instance(v,Symbol) then v else expression v,
                                 (symbol CoefficientRing) => A.CoefficientRing,
                                 BergmanRing => false,
		        	 (symbol degreesRing) => A.degreesRing,
       				 (symbol degreeLength) => A.degreeLength,
       				 (symbol degrees) => A.degrees,
                                 (symbol degreesRing) => degreesRing 1,
				 (symbol ambient) => A,
                                 (symbol cache) => new CacheTable from {},
          		         (symbol baseRings) => append(A.baseRings,A),    -- this will be for quotients of quotients
                                 (symbol ideal) => I};
   
   commonEngineRingInitializations B;
   
   error "err";
   
   --newGens := apply(B.generatorSymbols, v -> v <- new B from {(symbol ring) => B,
   --                                                           (symbol cache) => new CacheTable from {("isReduced",true)},
   --                                                           (symbol terms) => new HashTable from {(ncMonomial({v},B),1_(coefficientRing A))}});
   newGens := for i from 0 to #varSymbols-1 list varSymbols#i <- new R from R.RawRing_i;
   R.generators = newGens;

   
   B#(symbol weights) = A.weights;      
   B#(symbol generators) = newGens;
   
   R := A.CoefficientRing;
   
   if A#BergmanRing then B#BergmanRing = true;

   --- all these promotes will need to be written between this ring and all base rings.
   promote (A,B) := (f,B) -> new B from {(symbol ring) => B,
                                         (symbol cache) => new CacheTable from {("isReduced",f.cache#"isReduced")},
                                         (symbol terms) => promoteHash((f % ncgb).terms,B)};
                                     
   promote (B,A) := (f,A) -> new A from {(symbol ring) => A,
                                         (symbol cache) => new CacheTable from {("isReduced",f.cache#"isReduced")},
                                         (symbol terms) => promoteHash(f.terms,A)};

   promote (ZZ,B) := (n,B) -> putInRing({},B,promote(n,A.CoefficientRing));

   promote (QQ,B) := (n,B) -> putInRing({},B,promote(n,A.CoefficientRing));
   
   promote (R,B) := (n,B) -> putInRing({},B,promote(n,A.CoefficientRing));
   
   promote (B,B) := (f,B) -> f;

   promote (NCMatrix,B) := (M,B) -> (
      if M.source == {} or M.target == {} then
         ncMatrix(B,M.target,M.source)
      else (
         promEntries := applyTable(M.matrix, e -> promote(e,B));
         prom := ncMatrix promEntries;
         if isHomogeneous M then
            assignDegrees(prom,M.target,M.source);
         prom
      )
   );

   lift B := opts -> f -> promote(f,A);
   push := f -> (
      temp := if f.cache#"isReduced" then f else f % ncgb;
      new B from {(symbol ring) => B,
                  (symbol cache) => new CacheTable from {("isReduced",f.cache#"isReduced")},
                  (symbol terms) => promoteHash(temp.terms,B)}
   );
   B * B := (f,g) -> push((lift f)*(lift g));
   B ^ ZZ := (f,n) -> product toList (n:f);
   B + B := (f,g) -> push((lift f)+(lift g));
   R * B := (r,f) -> push(r*(lift f));
   B * R := (f,r) -> r*f;
   A * B := (f,g) -> push(f*(lift g));
   B * A := (f,g) -> push((lift f)*g);
   QQ * B := (r,f) -> push(r*(lift f));
   B * QQ := (f,r) -> r*f;
   ZZ * B := (r,f) -> push(r*(lift f));
   B * ZZ := (f,r) -> r*f;
   B - B := (f,g) -> f + (-1)*g;
   - B := f -> (-1)*f;
   B + ZZ := (f,r) -> push((lift f) + r);
   ZZ + B := (r,f) -> f + r;
   B + QQ := (f,r) -> push((lift f) + r);
   QQ + B := (r,f) -> f + r;

   B ? B := (f,g) -> (
      m := first pairs (leadMonomial f).terms;
      n := first pairs (leadMonomial g).terms;
      m ? n
   );

   B == B := (f,g) -> (lift(f - g) % ncgb) == 0;
   B == ZZ := (f,n) -> (
       f = push(lift f);
       (#(f.terms) == 0 and n == 0) or 
       (#(f.terms) == 1 and ((first pairs f.terms)#0#monList === {}) and ((first pairs f.terms)#1 == n))
   );
   ZZ == B := (n,f) -> f == n;
   B == QQ := (f,n) -> (
       f = push(lift f);
       (#(f.terms) == 0 and n == 0) or 
       (#(f.terms) == 1 and ((first pairs f.terms)#0#monList === {}) and ((first pairs f.terms)#1 == n))
   );
   QQ == B := (n,f) -> f == n;
   B == R := (f,n) -> (
      f = push(lift f);
      (#(f.terms) == 0 and n == 0) or 
      (#(f.terms) == 1 and ((first pairs f.terms)#0#monList === {}) and ((first pairs f.terms)#1 == n))
   );
   R == B := (n,f) -> f == n;
   B
)

net NCQuotientRing := B -> (
    hasAttribute := value Core#"private dictionary"#"hasAttribute";
    getAttribute := value Core#"private dictionary"#"getAttribute";
    ReverseDictionary := value Core#"private dictionary"#"ReverseDictionary";
    if hasAttribute(B,ReverseDictionary) then toString getAttribute(B,ReverseDictionary)
    else (
       net (B.ambient) |
       net " / " |
       net take(B.ideal.generators,10) |
       net if (#(B.ideal.generators) > 10) then " + More..." else ""
    )
)

ideal NCQuotientRing := NCIdeal => B -> B.ideal;
ambient NCQuotientRing := B -> B.ambient;
ambient NCFreeAlgebra := identity
*-

end

------
restart
needsPackage "PolynomialAlgebra"
debug needsPackage "NCAlgebraV2"
A = QQ{b,c,d}
f = 2*b^2*c*b + b^4
I = ncIdeal f
Igb = ncGroebnerBasis I
mon = monList leadTerm f
putInRing(mon,A,1)
g = 3*b^2*c*b + b^4
g % Igb

restart
needsPackage "NCAlgebraV2"
A = QQ{b,c,d}
f = 2*b^2*c*b + b^4
I = ncIdeal f
Igb = ncGroebnerBasis(I, DegreeLimit => 20)
fg = 3*b^2*c*b + b^4
g % Igb

--- TODO: discuss variable order with Mike
restart
needsPackage "NCAlgebraV2"
A = QQ{b,c,d}
I = ncIdeal {b*c - c*b, b*d + d*b, c*d + d*c}
Igb = ncGroebnerBasis(I, DegreeLimit => 20)
g = product(apply(50, i -> A_(random 3)))
time (g % Igb)

--- can try to speed up the new way, but not sure...
restart
debug needsPackage "NCAlgebra"
A = QQ{b,c,d}
I = ncIdeal {b*c - c*b, b*d + d*b, c*d + d*c}
Igb = ncGroebnerBasis(I, DegreeLimit => 20)
g = product(apply(50, i -> A_(random 3)))
time remainderFunction(g,Igb)

restart
needsPackage "PolynomialAlgebra"
debug Core
A = QQ[x,y]
R = A{b,c,d}
f = 3*x*y*b^2*c*b + 2*b^4
rawP = rawPairs(raw coefficientRing R, raw f)
(rawP#0, rawP#1 / rawSparseListFormMonomial)
-- this code can be used, for example, to get the 'monomial part' of terms.
toList apply(last rawP / rawSparseListFormMonomial, t -> product(apply(t, p -> R_(p#0)^(p#1))))
--- this is how terms is computed.  rawTerm calls IM2_RingElement_term in the engine.
--- Q: Do we change this code to work for PolynomialAlgebra objects as well, or will they
---    get their own function?  (We added a separate function for these types of things in the past).
apply(rawP#0,rawP#1,(c,m) -> new R from rawTerm(raw R, c, m))
