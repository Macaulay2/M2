newPackage("NCAlgebra",
     Headline => "data types for noncommutative algebras",
     Version => "1.0",
     Date => "July 2, 2016",
     Authors => {
	  {Name => "Frank Moore",
	   HomePage => "http://www.math.wfu.edu/Faculty/Moore.html",
	   Email => "moorewf@wfu.edu"},
	  {Name => "Andrew Conner",
	   HomePage => "http://www.stmarys-ca.edu/math-and-computer-science/faculty?facid=142546",
	   Email => "abc12@stmarys-ca.edu"},
	  {Name => "Courtney Gibbons",
	   HomePage => "http://people.hamilton.edu/cgibbons/index.html",
	   Email => "crgibbon@hamilton.edu"}},
     Keywords => {"Noncommutative Algebra", "Interfaces"},
     AuxiliaryFiles => true,
     CacheExampleOutput => true,
     PackageExports =>{"IntegralClosure"},
     OptionalComponentsPresent => bergmanPresent := run "type bergman >/dev/null 2>&1" === 0
     )

export { "NCRing", "NCQuotientRing", "NCPolynomialRing",
         "NCRingMap", "NCRingElement",
         "NCGroebnerBasis", "ncGroebnerBasis",
         "NCIdeal", "NCLeftIdeal", "NCRightIdeal",
         "ncIdeal", "ncLeftIdeal", "ncRightIdeal",
         "twoSidedNCGroebnerBasisBergman",
         "gbFromOutputFile",
         "CacheBergmanGB",
         "setWeights",
	 "MakeMonic",
	 "Derivation",
         "NumModuleVars",
	 "InstallGB",
         "ReturnIdeal",
         "NumberOfBins",
         "normalFormBergman",
         "hilbertBergman",
         "rightKernelBergman",
	 "rightKernelDegreeLimit",
	 "isLeftRegular",
         "isRightRegular",
         "centralElements",
         "normalElements",
	 "assignDegrees",
         "normalAutomorphism",
         "leftMultiplicationMap",
         "rightMultiplicationMap",
         "rightKernel",
         "NCMatrix",
	 "ncMatrix",
         "isCentral",
         "ncMap",
         "functionHash",
         "oreExtension",
	 "oreIdeal",
         "endomorphismRing",
	 "endomorphismRingGens",
         "minimizeRelations",
         "skewPolynomialRing",
	 "threeDimSklyanin",
	 "fourDimSklyanin",
	 "oppositeRing",
         "quadraticClosure",
	 "homogDual",
	 "toM2Ring",
	 "toNCRing",
	 "isExterior",
	 "coordinates",
	 "Basis",
	 "sparseCoeffs",
	 "kernelComponent",
	 "gddKernel",
	 "freeProduct",
	 "qTensorProduct",
	 "envelopingAlgebra",
	 "NCChainComplex"
}

--- symbols in hash tables of exported types
protect generatorSymbols
protect weights
protect monList
protect functionHash
--- options for non-exported functions
protect BergmanRing
protect MaxLoops
protect CheckPrefixOnly
protect ComputeNCGB
protect MaxNCGBDegree
protect MinNCGBDegree
protect MaxCoeffDegree
protect MinCoeffDegree
protect UsePreviousGBOutput
protect DontUse
protect CumulativeBasis

MAXDEG = 40
MAXSIZE = 1000

NCRing = new Type of Ring
NCQuotientRing = new Type of NCRing
NCPolynomialRing = new Type of NCRing
NCRingElement = new Type of HashTable
NCGroebnerBasis = new Type of HashTable
NCMatrix = new Type of MutableHashTable
NCMonomial = new Type of HashTable
NCIdeal = new Type of HashTable
NCLeftIdeal = new Type of HashTable
NCRightIdeal = new Type of HashTable
NCRingMap = new Type of HashTable

globalAssignment NCRing

---------------------------------------------------------------
--- Helpful general-purpose functions
---------------------------------------------------------------

removeNulls = xs -> select(xs, x -> x =!= null)

removeZeroes = myHash -> select(myHash, c -> c != 0)

minUsing = (xs,f) -> (
   n := min (xs / f);
   first select(1,xs, x -> f x == n)
)

sortUsing = (xs,f) -> (sort apply(xs, x -> (f x, x))) / last
reverseSortUsing = (xs, f) -> (reverse sort apply(xs, x -> (f x, x))) / last

-- get base m representation of an integer n
rebase = (m,n) -> (
   if (n < 0) then error "Must provide an integer greater than or equal to 1";
   if n == 0 then return {};
   if n == 1 then return {1};
   k := floor (log_m n);
   loopn := n;
   reverse for i from 0 to k list (
      digit := loopn // m^(k-i);
      loopn = loopn % m^(k-i);
      digit
   )   
)

-- list-based mingle
functionMingle = method()
functionMingle (List, List, FunctionClosure) := (xs,ys,f) -> (
   -- This function merges the lists xs and ys together, but merges them according to the function f.
   -- If f(i) is true, then the function takes the next unused element from xs.  Otherwise, the next
   -- unused element of ys is taken.  If either list is 'asked' for an element and has run out,
   -- then the list built so far is returned.
   xlen := #xs;
   ylen := #ys;
   ix := -1;
   iy := -1;
   for ians from 0 to xlen+ylen-1 list (
      if f ians then (
         ix = ix + 1;
         if ix >= xlen then break;
         xs#ix
      )
      else (
         iy = iy + 1;
         if iy >= ylen then break;
         ys#iy
      )
   )
)

-------------------------------------------
--- NCRing methods ------------------------
-------------------------------------------
coefficientRing NCRing := A -> A.CoefficientRing

generators NCRing := opts -> A -> (
    if A.?generators then A.generators else {}
)

numgens NCRing := A -> #(A.generators)

use NCRing := A -> (scan(A.generatorSymbols, A.generators, (sym,val) -> sym <- val); A)

setWeights = method()
setWeights (NCRing,List) := (A,weightList) -> (
   gensA := A.generatorSymbols;
   A#(symbol weights) = new HashTable from apply(#gensA, i -> (gensA#i,weightList#i));
   A
)

promoteHash = (termHash,A) -> (
   hashTable apply(pairs termHash, p -> (ncMonomial(p#0#monList,A),p#1))
)

isHomogeneous NCRing := A -> (
   if instance(A,NCPolynomialRing) then true
   else isHomogeneous ideal A
)

hilbertSeries NCRing := RingElement => opts -> A -> (
   if not isHomogeneous A then error "Expected a homogeneous NCAlgebra.";
   if opts#Order == infinity then error "Expected a maximum degree.  Use the Order=>ZZ option.";
   deg := opts#Order;
   if A.BergmanRing and ((gens A) / degree // unique === {1}) and class A === NCQuotientRing then
      hilbertBergman A
   else if class A === NCPolynomialRing then (
      -- use the known Hilbert series of the algebra
      -- Hilb(T(V)) = (1-Hilb(V))^(-1) where V is a positively graded vector space
      R := A.degreesRing;
      T := R_0;
      hsV := sum apply(numgens A, i -> (R_0)^(degree (A_i)));
      tensorHS := sum apply(deg + 1, i -> hsV^i);
      tensorCoeffs := coefficients(tensorHS, Monomials=>apply(deg + 1, i -> T^i));
      tensorHS = first flatten entries ((tensorCoeffs#0)*(tensorCoeffs#1));
      tensorHS
   )
   else if isField coefficientRing A then (
      monBasis := flatten entries cumulativeBasis(deg,A);
      R = A.degreesRing;
      T = R_0;
      degTally := tally (monBasis / degree);
      sum apply(deg+1, i -> degTally#i*T^i)
   )
   else error "Expected a NCAlgebra whose coefficient ring is a field."
)

-------------------------------------------
--- NCPolynomialRing methods --------------
-------------------------------------------

new NCPolynomialRing from List := (NCPolynomialRing, inits) -> new NCPolynomialRing of NCRingElement from new HashTable from inits

Ring List := (R, varList) -> (
   -- get the symbols associated to the list that is passed in, in case the variables have been used earlier.
   if #varList == 0 then error "Expected at least one variable.";
   if #varList == 1 and class varList#0 === Sequence then varList = toList first varList;
   varList = varList / baseName;
   A := new NCPolynomialRing from {(symbol generators) => {},
                                   (symbol generatorSymbols) => varList,
                                   (symbol degreesRing) => degreesRing 1,
				   (symbol CoefficientRing) => R,
                                   (symbol cache) => new CacheTable from {},
				   (symbol baseRings) => {ZZ},
                                   BergmanRing => false};
   newGens := apply(varList, v -> v <- putInRing({v},A,1));

   if R === QQ or R === ZZ/(char R) then A#BergmanRing = true;

   A#(symbol generators) = newGens;
   
   setWeights(A, toList (#(gens A):1));
      
   --- all these promotes will need to be written between this ring and all base rings.
   promote (ZZ,A) := (n,A) -> putInRing({},A,promote(n,R));


   promote (QQ,A) := (n,A) ->  putInRing({},A,promote(n,R));
        
   promote (R,A) := (n,A) -> putInRing({},A,n);
      
   promote (NCMatrix,A) := (M,A) -> (
       if M.source == {} or M.target == {} then
          ncMatrix(A,M.target,M.source)
       else (
          prom := ncMatrix apply(M.matrix, row -> apply(row, entry -> promote(entry,A)));
          if isHomogeneous M then
             assignDegrees(prom,M.target,M.source);
          prom
       )
   );

   promote (A,A) := (f,A) -> f;
   
   addVals := (c,d) -> (
      e := c+d;
      if e == 0 then continue else e
   );

   multVals := (c,d) -> c*d;
      
   multKeys := (m,n) -> m | n;

   A + A := (f,g) -> (
      -- new way
      newHash := removeZeroes merge(f.terms,g.terms,addVals);
      -- old way
      --newHash := new MutableHashTable from pairs f.terms;
      --for s in pairs g.terms do (
      --   newMon := s#0;
      --   if newHash#?newMon then newHash#newMon = newHash#newMon + s#1 else newHash#newMon = s#1;
      --);
      --newHash = removeZeroes hashTable pairs newHash;
      if newHash === hashTable {} then newHash = (promote(0,f.ring)).terms;
      new A from hashTable {(symbol ring, f.ring),
                            (symbol cache, new CacheTable from {("isReduced",false)}),
                            (symbol terms, newHash)}   
   );

   A ? A := (f,g) -> (
      m := first pairs (leadMonomial f).terms;
      n := first pairs (leadMonomial g).terms;
      m ? n
   );

   A * A := (f,g) -> (
      -- new way
      -- time not very predictable...
      newHash := removeZeroes combine(f.terms,g.terms,multKeys,multVals,addVals);
      -- old way
      --newHash := new MutableHashTable;
      --for t in pairs f.terms do (
      --   for s in pairs g.terms do (
      --      newMon := t#0 | s#0;
      --      newCoeff := (t#1)*(s#1);
      --      if newHash#?newMon then newHash#newMon = newHash#newMon + newCoeff else newHash#newMon = newCoeff;
      --   );
      --);
      --newHash = removeZeroes hashTable pairs newHash;
      if newHash === hashTable {} then newHash = (promote(0,f.ring)).terms;
      new A from hashTable {(symbol ring, f.ring),
                            (symbol cache, new CacheTable from {("isReduced",false)}),
                            (symbol terms, newHash)}
   );

   A ^ ZZ := (f,n) -> product toList (n:f);

   R * A := (r,f) -> promote(r,A)*f;
   A * R := (f,r) -> r*f;
   QQ * A := (r,f) -> promote(r,A)*f;
   A * QQ := (f,r) -> r*f;
   ZZ * A := (r,f) -> promote(r,A)*f;

   A * ZZ := (f,r) -> r*f;
   A - A := (f,g) -> f + (-1)*g;
   - A := f -> (-1)*f;
   A + ZZ := (f,r) -> f + promote(r,A);
   ZZ + A := (r,f) -> f + r;
   A + QQ := (f,r) -> f + promote(r,A);
   QQ + A := (r,f) -> f + r;
   A + R  := (f,r) -> f + promote(r,A);
   R + A  := (r,f) -> f + r;
   
   A ? A := (f,g) -> (leadNCMonomial f) ? (leadNCMonomial g);

   A == A := (f,g) -> #(f.terms) == #(g.terms) and (sort pairs f.terms) == (sort pairs g.terms);
   A == ZZ := (f,n) -> (#(f.terms) == 0 and n == 0) or (#(f.terms) == 1 and ((first pairs f.terms)#0#monList === {}) and ((first pairs f.terms)#1 == n));
   ZZ == A := (n,f) -> f == n;
   A == QQ := (f,n) -> (#(f.terms) == 0 and n == 0) or (#(f.terms) == 1 and ((first pairs f.terms)#0#monList === {}) and ((first pairs f.terms)#1 == n));
   QQ == A := (n,f) -> f == n;
   A == R := (f,n) -> (#(f.terms) == 0 and n == 0) or (#(f.terms) == 1 and ((first pairs f.terms)#0#monList === {}) and ((first pairs f.terms)#1 == n));
   R == A := (n,f) -> f == n;

   A
)

net NCRing := A -> (
    hasAttribute := value Core#"private dictionary"#"hasAttribute";
    getAttribute := value Core#"private dictionary"#"getAttribute";
    ReverseDictionary := value Core#"private dictionary"#"ReverseDictionary";
    if hasAttribute(A,ReverseDictionary) then toString getAttribute(A,ReverseDictionary)
    else net A.CoefficientRing | net A.generators
)

ideal NCPolynomialRing := NCIdeal => A ->
   new NCIdeal from new HashTable from {(symbol ring) => A,
                                        (symbol generators) => {promote(0,A)},
                                        (symbol cache) => new CacheTable from {}}

-------------------------------------------

-------------------------------------------
--- NCQuotientRing functions --------------
-------------------------------------------

new NCQuotientRing from List := (NCQuotientRing, inits) -> new NCQuotientRing of NCRingElement from new HashTable from inits

NCPolynomialRing / NCIdeal := (A, I) -> (
   ncgb := ncGroebnerBasis I;
   B := new NCQuotientRing from {(symbol generators) => {},
                                 (symbol generatorSymbols) => A.generatorSymbols,
                                 (symbol CoefficientRing) => A.CoefficientRing,
                                 BergmanRing => false,
                                 (symbol degreesRing) => degreesRing 1,
				 (symbol ambient) => A,
                                 (symbol cache) => new CacheTable from {},
          		         (symbol baseRings) => {ZZ},    -- this will be for quotients of quotients
                                 (symbol ideal) => I};
   newGens := apply(B.generatorSymbols, v -> v <- new B from {(symbol ring) => B,
                                                              (symbol cache) => new CacheTable from {("isReduced",true)},
                                                              (symbol terms) => new HashTable from {(ncMonomial({v},B),1_(coefficientRing A))}});
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
ambient NCPolynomialRing := identity

quadraticClosure = method()
quadraticClosure NCQuotientRing := B -> (
   I:=B.ideal;
   J:=quadraticClosure(I);
   A:=B.ambient;
   A/J
)


homogDual = method()
homogDual NCQuotientRing := B -> (
   I:=B.ideal;
   J:=homogDual(I);
   A:=J.ring;
   A/J
)

homogDual Ring := B -> homogDual toNCRing B

-------------------------------------------------------
--- Commands to compute endomorphism ring presentations
-------------------------------------------------------

endomorphismRing = method()
endomorphismRing (Module,Symbol) := (M,X) -> (
   R := ring M;
   endM := End M;
   N := numgens endM;
   gensEndMaps := apply(N, i -> homomorphism endM_{i});
   W := coker relations endM;
   gensEndM := map(W,source gens endM,gens endM);
   endMVars := apply(N, i -> X_i);
   A := R endMVars;
   gensA := basis(1,A);
   -- build the squarefree relations
   twoSubsets := subsets(N,2);
   mons := twoSubsets | (twoSubsets / reverse) | apply(N, i -> {i,i});
   relsHomM := getHomRelations(gensA,gensEndM,mons,gensEndMaps);
   -- should now make some attempt at finding a minimal set of algebra generators
   I := ncIdeal relsHomM;
   Igb := ncGroebnerBasis(I,InstallGB=>true);
   B := A/I;
   B.cache#(symbol endomorphismRingGens) = gensEndMaps;
   B
)

getHomRelations = method()
getHomRelations (NCMatrix,Matrix,List,List) := (gensA,gensEndM,mons,gensEndMaps) -> (
   monMatrix := matrix {for m in mons list (
      comp := gensEndMaps#(m#0)*gensEndMaps#(m#1);
      matrix entries transpose flatten matrix comp
   )};
   monMatrix = map(target gensEndM,source monMatrix, monMatrix);
   linearParts := (monMatrix // gensEndM);
   linearParts = flatten entries (gensA*linearParts);
   varsA := flatten entries gensA;
   apply(#mons, i -> varsA#(mons#i#0)*varsA#(mons#i#1) - linearParts#i)
)

findLinearRelation = (x,relList) -> (
    xmon := (first pairs x.terms)#0#monList;
    i := 0;
    while i < #relList do (
        monMatches := select(2,pairs (relList#i).terms, m -> member(first xmon,m#0#monList));
        if #monMatches == 1 and monMatches#0#0#monList === xmon and isUnit monMatches#0#1 then
           return (i,monMatches#0#1);
        i = i + 1;
    );
)

removeConstants = f -> (
    coeff := leadCoefficient f;
    if isUnit coeff then
       (coeff)^(-1)*f
    else if isUnit leadCoefficient coeff then
       (leadCoefficient coeff)^(-1)*f
    else
       f
)

partialInterreduce = method(Options => {Verbosity => 0, MaxLoops => infinity})
partialInterreduce List := opts -> relList -> (
   redList := relList;   
   oldListLen := -1;
   numLoops := 0;
   while #redList != oldListLen and numLoops < opts#MaxLoops do
   (
      oldListLen = #redList;
      tempGb := ncGroebnerBasis(redList,InstallGB=>true);
      redList = for i from 0 to #redList-1 list (
         if opts#Verbosity > 0  then (
            << "Reducing " << toString redList#i << endl
            << "  (" << i+1 << " of " << #redList << ")" << endl
            << "  (Pass " << numLoops+1 << ")" << endl;
         );
         redListRem := remainderFunction(redList#i,tempGb,DontUse=>redList#i);
	 if redListRem != 0 then redListRem
      );
      redList = (unique removeNulls redList) / removeConstants;
      numLoops = numLoops + 1;
   );
   redList
)

eliminateLinearVariables = method(Options => {Verbosity => 0})
eliminateLinearVariables List := opts -> rels -> (
   A := ring first rels;
   R := coefficientRing A;
   gensA := gens A;
   curRels := rels;
   linearRel := null;
   elimVars := 1;
   i := 0;
   while (i < #gensA) do (
      linearRel = findLinearRelation(gensA#i,curRels);
      if linearRel =!= null then break;
      i = i + 1;
   );
   while i != #gensA do (
      relIndex := first linearRel;
      relCoeff := last linearRel;
      phi := ncMap(A,A,apply(gensA, x -> if x === gensA#i then x - (relCoeff^(-1))*(curRels#relIndex) else x));
      elimVars = elimVars + 1;
      if opts#Verbosity > 0 then << "Eliminating variable " << gensA#i << endl;
      curRels = select(curRels / phi, f -> f != 0);
      i = 0;
      while (i < #gensA) do (
         linearRel = findLinearRelation(gensA#i,curRels);
         if linearRel =!= null then break;
         i = i + 1;
      );
   );
   unique curRels
)

minimizeRelations = method(Options => {Verbosity => 0})
minimizeRelations List := opts -> rels -> (
   numOldRels := -1;
   --- make the input monic
   loopRels := apply(rels, f -> (leadCoefficient f)^(-1)*f);
   while numOldRels != #loopRels do (
      numOldRels = #loopRels;
      loopRels = eliminateLinearVariables(loopRels,opts);
      loopRels = partialInterreduce(loopRels,opts);
   );
   loopRels
)

checkHomRelations = method(Options => {Verbosity => 0})
checkHomRelations (List,List) := opts -> (rels,homGens) -> (
   if rels == {} then return null;
   A := ring first rels;
   gensA := A.generatorSymbols;
   genHash := new HashTable from pack(2,mingle(gensA,homGens));
   for f in rels do (
      relMatrix := sum for p in pairs (f.terms) list (
         prod := product apply(p#0#monList, x -> genHash#x);
         (p#1)*prod
      );
      if relMatrix != 0 then error "Endomorphism relations do not hold.";
   );
)

------------------------------------------------

------------------------------------------------
------------- NCIdeal --------------------------
------------------------------------------------

ncIdeal = method()
ncIdeal List := idealGens -> (
   if #idealGens == 0 then error "Expected at least one generator.";
   new NCIdeal from new HashTable from {(symbol ring) => (idealGens#0).ring,
                                        (symbol generators) => idealGens,
                                        (symbol cache) => new CacheTable from {}}
)

ncIdeal NCRingElement := f -> ncIdeal {f}

generators NCIdeal := opts -> I -> I.generators;

isHomogeneous NCIdeal := I -> all(gens I, isHomogeneous)

net NCIdeal := I -> "Two-sided ideal " | net (I.generators);

ring NCIdeal := NCRing => I -> I.ring

NCIdeal + NCIdeal := (I,J) -> (
    if ring I =!= ring J then error "Expected ideals over the same ring.";
    ncIdeal (gens I | gens J)
)

quadraticClosure NCIdeal := I -> (
   J:=select(I.generators, g-> (degree g)<=2);
   ncIdeal J
)

homogDual NCIdeal := I -> (
   if class I.ring =!= NCPolynomialRing then
      error "Expected an ideal in the tensor algebra.";
   d:=degree I.generators_0;
   if not all(I.generators, g->(degree g)==d)
      then error "Expected a pure ideal.";
   A:=I.ring;
   bas := basis(d,A);
   mat := transpose sparseCoeffs(I.generators,Monomials=>flatten entries bas);
   dualGens := bas*(gens ker mat);
   J:=ncIdeal flatten entries dualGens
)

basis(ZZ,NCIdeal) := NCMatrix => opts -> (n,I) ->
   newBasis(n,I,CumulativeBasis=>false)
   
cumulativeBasis = method()
cumulativeBasis(ZZ,NCIdeal) := NCMatrix => (n,I) ->
   newBasis(n,I,CumulativeBasis=>true)

newBasis = method(Options => {CumulativeBasis => false})   
newBasis (ZZ,NCIdeal) := NCMatrix => opts -> (n,I) -> (
   R:=ring I;
   idealGens:=select(I.generators, g->degree g <= n);
   doneToDeg := min (idealGens / degree);  
   if n < doneToDeg then error "The requested degree must be at least as large as the degree of a minimal generator.";
   activeGensList:=select(gens R, g-> degree g <= n-doneToDeg);
   minGenDeg := min (activeGensList / degree);
      
-- compute a spanning set for the ideal in degrees <= n
   newElts := idealGens;
   doneBasis := if opts#CumulativeBasis then newElts else select(newElts,x->(degree x)==n);
   while doneToDeg <= n-minGenDeg do (
        -- compute all products of qualified generators with most recently added elements
	newElts = flatten apply(activeGensList, v-> flatten{v*newElts,newElts*v});
	-- select only those of the desired degrees
	newElts = select(newElts, x-> degree x<=n);
	if opts#CumulativeBasis then
	   -- add them to the cumulative list
	   doneBasis = doneBasis | newElts
	else
	   doneBasis = doneBasis | select(newElts, x->(degree x) == n);
	-- increment the degree to which the spanning set is fully computed
	doneToDeg = doneToDeg + minGenDeg;
	-- update the qualified generators 
	-- note that the minimum degree won't change unless we're done
	activeGensList = select(activeGensList, g-> degree g <= n - doneToDeg);
   );

   -- now we need to minimize the spanning set
   terms := flatten entries newBasis(n,R,CumulativeBasis=>opts#CumulativeBasis);
   asCoeffs := sparseCoeffs(doneBasis, Monomials=>terms);  
   minGens := mingens image asCoeffs;
   ncMatrix{terms}*minGens 
)

------------------------------------------------

------------------------------------------------
------------- NCRightIdeal ---------------------
------------------------------------------------

ncRightIdeal = method()
ncRightIdeal List := idealGens -> (
   if #idealGens == 0 then error "Expected at least one generator.";
   new NCRightIdeal from new HashTable from {(symbol ring) => (idealGens#0).ring,
                                             (symbol generators) => idealGens,
                                             (symbol cache) => new CacheTable from {}}
)

ncRightIdeal NCRingElement := f -> ncRightIdeal {f}

generators NCRightIdeal := opts -> I -> I.generators;

isHomogeneous NCRightIdeal := I -> all(gens I, isHomogeneous)

net NCRightIdeal := I -> "Right ideal " | net (I.generators);

ring NCRightIdeal := NCRing => I -> I.ring

NCRightIdeal + NCRightIdeal := (I,J) -> (
   if ring I =!= ring J then error "Expected ideals over the same ring.";
   ncRightIdeal (gens I | gens J)
)

basis(ZZ,NCRightIdeal) := NCMatrix => opts -> (n,I) ->
   newBasis(n,I,CumulativeBasis=>false)
   
cumulativeBasis(ZZ,NCRightIdeal) := NCMatrix => (n,I) ->
   newBasis(n,I,CumulativeBasis=>true)

newBasis (ZZ,NCRightIdeal) := NCMatrix => opts -> (n,I) -> (
   R:=ring I;
   idealGens:=select(I.generators, g->degree g <= n);
   doneToDeg := min (idealGens / degree);  
   if n < doneToDeg then error "The requested degree must be at least as large as the degree of a minimal generator.";
   activeGensList:=select(gens R, g-> degree g <= n-doneToDeg);
   minGenDeg := min (activeGensList / degree);
      
-- compute a spanning set for the ideal in degrees <= n
   newElts := idealGens;
   doneBasis := if opts#CumulativeBasis then newElts else select(newElts,x->(degree x)==n);
   while doneToDeg <= n-minGenDeg do (
        -- compute all products of qualified generators with most recently added elements
	newElts = flatten apply(activeGensList, v-> newElts*v);
	-- select only those of the desired degrees
	newElts = select(newElts, x-> degree x<=n);
	if opts#CumulativeBasis then
	   -- add them to the cumulative list
	   doneBasis = doneBasis | newElts
	else
	   doneBasis = doneBasis | select(newElts, x->(degree x) == n);
	-- increment the degree to which the spanning set is fully computed
	doneToDeg = doneToDeg + minGenDeg;
	-- update the qualified generators 
	-- note that the minimum degree won't change unless we're done
	activeGensList = select(activeGensList, g-> degree g <= n - doneToDeg);
   );

-- now we need to minimize the spanning set
   terms := flatten entries newBasis(n,R,CumulativeBasis=>opts#CumulativeBasis);
   asCoeffs := sparseCoeffs(doneBasis, Monomials=>terms);  
   minGens := mingens image asCoeffs;
   ncMatrix{terms}*minGens
)


------------------------------------------------

------------------------------------------------
------------- NCLeftIdeal ---------------------
------------------------------------------------

ncLeftIdeal = method()
ncLeftIdeal List := idealGens -> (
   if #idealGens == 0 then error "Expected at least one generator.";
   new NCLeftIdeal from new HashTable from {(symbol ring) => (idealGens#0).ring,
                                            (symbol generators) => idealGens,
                                            (symbol cache) => new CacheTable from {}}
)

ncLeftIdeal NCRingElement := f -> ncLeftIdeal {f}

generators NCLeftIdeal := opts -> I -> I.generators;

isHomogeneous NCLeftIdeal := I -> all(gens I, isHomogeneous)

net NCLeftIdeal := I -> "Left ideal " | net (I.generators);

ring NCLeftIdeal := NCRing =>I -> I.ring

NCLeftIdeal + NCLeftIdeal := (I,J) -> (
   if ring I =!= ring J then error "Expected ideals over the same ring.";
   ncLeftIdeal (gens I | gens J)
)

basis(ZZ,NCLeftIdeal) := NCMatrix => opts -> (n,I) ->
   newBasis(n,I,CumulativeBasis=>false)
   
cumulativeBasis(ZZ,NCLeftIdeal) := NCMatrix => (n,I) ->
   newBasis(n,I,CumulativeBasis=>true)

newBasis (ZZ,NCLeftIdeal) := NCMatrix => opts -> (n,I) -> (
   R:=ring I;
   idealGens:=select(I.generators, g->degree g <= n);
   doneToDeg := min (idealGens / degree);  
   if n < doneToDeg then error "The requested degree must be at least as large as the degree of a minimal generator.";
   activeGensList:=select(gens R, g-> degree g <= n-doneToDeg);
   minGenDeg := min (activeGensList / degree);
      
-- compute a spanning set for the ideal in degrees <= n
   newElts := idealGens;
   doneBasis := if opts#CumulativeBasis then newElts else select(newElts,x->(degree x)==n);
   while doneToDeg <= n-minGenDeg do (
        -- compute all products of qualified generators with most recently added elements
	newElts = flatten apply(activeGensList, v-> v*newElts);
	-- select only those of the desired degrees
	newElts = select(newElts, x-> degree x<=n);
	if opts#CumulativeBasis then
	   -- add them to the cumulative list
	   doneBasis = doneBasis | newElts
	else
	   doneBasis = doneBasis | select(newElts, x->(degree x) == n);
	-- increment the degree to which the spanning set is fully computed
	doneToDeg = doneToDeg + minGenDeg;
	-- update the qualified generators 
	-- note that the minimum degree won't change unless we're done
	activeGensList = select(activeGensList, g-> degree g <= n - doneToDeg);
   );

-- now we need to minimize the spanning set
   myTerms := flatten entries newBasis(n,R,CumulativeBasis=>opts#CumulativeBasis);
   asCoeffs := sparseCoeffs(doneBasis, Monomials=>myTerms);  
   minGens := mingens image asCoeffs;
   ncMatrix{myTerms}*minGens
)

------------------------------------------------

-------------------------------------------
--- NCMonomial functions ------------------
-------------------------------------------

ncMonomial = method()
ncMonomial (List,NCRing) := (monL,B) -> (
   newMon := new NCMonomial from {(symbol monList) => monL,
                                  (symbol ring) => B};
   newMon
)

NCMonomial | List := (mon,symList) -> ncMonomial(mon#monList | symList, mon.ring)
List | NCMonomial := (symList,mon) -> ncMonomial(symList | mon#monList, mon.ring)

NCMonomial | NCMonomial := (mon1,mon2) -> ncMonomial(mon1#monList | mon2#monList, mon1.ring)

net NCMonomial := mon -> (
   mon = mon#monList;
   if mon === {} then return net "";
   myNet := net "";
   tempVar := first mon;
   curDegree := 0;
   for v in mon do (
      if v === tempVar then curDegree = curDegree + 1
      else (
          myNet = myNet | (net tempVar) | if curDegree == 1 then (net "") else ((net curDegree)^1);
          tempVar = v;
          curDegree = 1;
      );
   );
   myNet | (net tempVar) | if curDegree == 1 then (net "") else ((net curDegree)^1)
)

toString NCMonomial := mon -> (
   mon = mon#monList;
   if mon === {} then return "";
   myNet := "";
   tempVar := first mon;
   curDegree := 0;
   for v in mon do (
      if v === tempVar then curDegree = curDegree + 1
      else (
          myNet = myNet | (toString tempVar) | if curDegree == 1 then "*" else "^" | curDegree | "*";
          tempVar = v;
          curDegree = 1;
      );
   );
   myNet | (toString tempVar) | if curDegree == 1 then "" else "^" | curDegree
)

degree NCMonomial := mon -> sum apply(#(mon#monList), i -> ((mon.ring).weights)#(mon#monList#i))

tensorLength = method()
tensorLength NCMonomial := mon -> #(mon#monList)

putInRing = method()
putInRing (NCMonomial, ZZ) := 
putInRing (NCMonomial, QQ) :=
putInRing (NCMonomial, RingElement) := (mon,coeff) ->
      new (mon.ring) from {(symbol ring) => mon.ring,
			   (symbol cache) => new CacheTable from {("isReduced",false)},
                           (symbol terms) => new HashTable from {(mon,promote(coeff,coefficientRing (mon.ring)))}}
putInRing (List, NCRing, ZZ) := 
putInRing (List, NCRing, QQ) :=
putInRing (List, NCRing, RingElement) := (monList,A,coeff) -> (
    mon := ncMonomial(monList,A);
    new A from {(symbol ring) => A,
                (symbol cache) => new CacheTable from {("isReduced",false)},
                (symbol terms) => new HashTable from {(mon,promote(coeff,coefficientRing A))}}
)

NCMonomial _ List := (mon,substr) -> ncMonomial((mon#monList)_substr,mon.ring)

findSubstring = method(Options => {CheckPrefixOnly => false})
findSubstring (NCMonomial,NCMonomial) := opts -> (lt, mon) -> (
   mon = mon#monList;
   lt = lt#monList;
   deg := length lt;
   if opts#CheckPrefixOnly and take(mon, deg) === lt then
      return true
   else if opts#CheckPrefixOnly then
      return false;
   if not isSubset(lt,mon) then return null;
   substrIndex := null;
   for i from 0 to #mon-1 do (
      if #mon - i < deg then break;
      if lt === mon_{i..i+deg-1} then (
         substrIndex = i;
         break;
      );
   );
   if substrIndex =!= null then (take(mon,substrIndex),take(mon,-#mon+deg+substrIndex)) else null
)

NCMonomial ? NCMonomial := (m,n) -> if (m#monList) === (n#monList) then symbol == else
                                    if #m < #n then symbol < else
                                    if #m > #n then symbol > else
                                    if (#m == #n and (toIndexList m) < (toIndexList n)) then symbol > else symbol <

NCMonomial == NCMonomial := (m,n) -> (m#monList) === (n#monList)

toIndexList = method()
toIndexList NCMonomial := mon -> (
   apply(mon#monList, x -> position(mon.ring.generatorSymbols, y -> x === y))
)

-----------------------------------------

-----------------------------------------
--- NCRingElement methods ---------------
-----------------------------------------

net NCRingElement := f -> (
   if #(f.terms) == 0 then return net "0";
   
   firstTerm := true;
   myNet := net "";
   isZp := (class coefficientRing ring f === QuotientRing and ambient coefficientRing ring f === ZZ);
   for t in sort pairs f.terms do (
      tempNet := net t#1;
      printParens := ring t#1 =!= QQ and
  		     ring t#1 =!= ZZ and
                     not isZp and
		     (size t#1 > 1 or (isField ring t#1 and 
			               numgens coefficientRing ring t#1 > 0 and
				       size sub(t#1, coefficientRing ring t#1) > 1));
      myNet = myNet |
              (if isZp and tempNet#0#0 != "-" and not firstTerm then net "+"
	       else if not firstTerm and t#1 > 0 then
                 net "+"
               else 
                 net "") |
              (if printParens then net "(" else net "") | 
              (if t#1 != 1 and t#1 != -1 then
                 tempNet
               else if t#1 == -1 then net "-"
               else net "") |
              (if printParens then net ")" else net "") |
              (if t#0#monList === {} and (t#1 == 1 or t#1 == -1) then net "1" else net t#0);
      firstTerm = false;
   );
   myNet
)

toStringMaybeSort := method(Options => {Sort => false})
toStringMaybeSort NCRingElement := opts -> f -> (
   sortFcn := if opts#Sort then sort else identity;
   isZp := (class coefficientRing ring f === QuotientRing and ambient coefficientRing ring f === ZZ);
   if #(f.terms) == 0 then "0" else (
      firstTerm := true;
      myNet := "";
      for t in sortFcn pairs f.terms do (
         tempNet := toString t#1;
         printParens := ring t#1 =!= QQ and ring t#1 =!= ZZ and not isZp and size t#1 > 1;
         myNet = myNet |
                 (if isZp and tempNet#0#0 != "-" and not firstTerm then "+"
		  else if not firstTerm and t#1 > 0 then
                    "+"
                  else 
                    "") |
                 (if printParens then "(" else "") |
                 (if t#1 != 1 and t#1 != -1 and t#0#monList =!= {} then
                    tempNet | "*"
                  else if t#1 == -1 and t#0#monList =!= {} then "-"
                  else if t#0#monList === {} then tempNet
		  else "") |
                 (if printParens then ")" else "") |
                 (if t#0#monList =!= {} then toString t#0 else "");
         firstTerm = false;
      );
      myNet
   )
)

clearDenominators = method()
clearDenominators NCRingElement := f -> (
   if coefficientRing ring f =!= QQ then (f,f) else (
      coeffDens := apply(values (f.terms), p -> if class p === QQ then denominator p else 1);
      myLCM := lcm coeffDens;
      (f*myLCM,myLCM)
   )
)

baseName NCRingElement := x -> (
   A := class x;
   pos := position(gens A, y -> y == x);
   if pos === null then error "Expected a generator";
   A.generatorSymbols#pos
)

ring NCRingElement := NCRing => f -> f.ring

flagReduced := f -> new (ring f) from {(symbol ring) => ring f,
                                       (symbol cache) => new CacheTable from {("isReduced",true)},
                                       (symbol terms) => f.terms}


coordinates = method(Options =>{Basis=>null})

coordinates NCRingElement := opts -> f -> (
  if opts#Basis === null then
     sparseCoeffs({f})
  else
    coordinates({f},Basis=>opts#Basis)
)

coordinates List := opts -> L -> (
   if opts#Basis === null then
      sparseCoeffs L
   else
      bas := opts#Basis;
      R := ring bas#0;
      firstNonzero := select(1,bas,b -> b != 0);
      d := if firstNonzero == {} then 0 else degree first firstNonzero;
      if not all(L, m-> (isHomogeneous(m) and ((degree m) == d or m == 0))) then
	error "Expected homogeneous elements of the same degree.";
      mons := flatten entries basis(d,R);
      M := sparseCoeffs(bas, Monomials=>mons);
      N := sparseCoeffs(L, Monomials=>mons);
      if rank (M|N) != rank M then error "Expected elements in the span of given basis.";
      I := id_(target M);
      T := I // M;
      T*N
)


sparseCoeffs = method(Options => {Monomials => null})
sparseCoeffs NCRingElement := opts -> f -> (
  if opts#Monomials === null then
     sparseCoeffs({f})
  else
    sparseCoeffs({f},Monomials=>opts#Monomials)
)

sparseCoeffs List := opts -> L -> (
  d := if all(L, m -> m == 0) then 0 else L#(position(L,m->m!=0));
  -- trying to fix to allow for inhomogeneous input.
  --if not all(L, m-> (isHomogeneous(m) and (m == 0 or (degree m)==(degree d)))) then 
  --	error "Expected homogeneous elements of the same degree.";
  B := (L#0).ring;
  R := coefficientRing B;
  mons := if opts#Monomials === null then (
              unique flatten apply(L, e-> flatten entries monomials e)) 
          else opts#Monomials;
  
  m := #mons;
  
  mons =  (mons / (m -> first keys m.terms));
  mons =  hashTable apply(m, i -> (mons#i,i));
   
  termsF := pairs (L#0).terms;
  
  coeffs := if (L#0)!=0 then (apply(termsF, (k,v) -> if v!=0 then (mons#k,0) => promote(v,R))) else {};

  l:=length L;
  if l>1 then
     for i from 1 to l-1 do (
        --if not isHomogeneous L#i then error "Expected a homogeneous element.";
        if (L#i) !=0 then (
        termsF = pairs (L#i).terms;
        newCoeffs := (apply(termsF, (k,v) -> if v!=0 then
            if not mons#?k then error "Expected polynomial to be in span of input monomials."
            else (mons#k,i) => promote(v,R)));
	coeffs = coeffs | newCoeffs;);
     ); 
  map(R^m , R^l, coeffs)
-*  d := if all(L, m -> m == 0) then 0 else L#(position(L,m->m!=0));
  if not all(L, m-> (isHomogeneous(m) and (m == 0 or (degree m)==(degree d)))) then 
	error "Expected homogeneous elements of the same degree.";
  B := (L#0).ring;
  R := coefficientRing B;
  mons := if opts#Monomials === null then (
              unique flatten apply(L, e-> flatten entries monomials e)) 
          else opts#Monomials;
  
  m := #mons;
  
  mons =  (mons / (m -> first keys m.terms));
  mons =  hashTable apply(m, i -> (mons#i,i));
   
  termsF := pairs (L#0).terms;
  
  coeffs := if (L#0)!=0 then (apply(termsF, (k,v) -> if v!=0 then (mons#k,0) => promote(v,R))) else {};

  l:=length L;
  if l>1 then
     for i from 1 to l-1 do (
        if not isHomogeneous L#i then error "Expected a homogeneous element.";
        if (L#i) !=0 then (
        termsF = pairs (L#i).terms;
        newCoeffs := (apply(termsF, (k,v) -> if v!=0 then (mons#k,i) => promote(v,R)));
       
	coeffs = coeffs | newCoeffs;);
     ); 
   map(R^m , R^l, coeffs)*-
)

monomials NCRingElement := opts -> f -> (
    ncMatrix {apply(sort keys f.terms, mon -> putInRing(mon,1))}
)
toString NCRingElement := f -> toStringMaybeSort(f,Sort=>true)
degree NCRingElement := f -> (
    fkeys := (keys f.terms);
    max (fkeys / degree)
)
tensorLength NCRingElement := f -> (
    ltf := leadTerm f;
    ltfkeys := (keys (ltf.terms));
    first (ltfkeys / tensorLength)
)
size NCRingElement := f -> #(f.terms)
leadTerm NCRingElement := f -> (
    if f.cache.?leadTerm then return f.cache.leadTerm;
    degf := degree f;
    lt := first sort select((pairs f.terms), p -> degree p#0 == degf);
    retVal := putInRing(lt#0,lt#1);
    f.cache.leadTerm = retVal;
    retVal
)
leadMonomial NCRingElement := f -> putInRing(leadNCMonomial f,1);
leadNCMonomial = f -> first keys (leadTerm f).terms;
leadMonomialForGB = f -> (
   R := coefficientRing ring f;
   if isField R then
      (leadNCMonomial f, 1_R)
   else
      (leadNCMonomial f, leadMonomial leadCoefficient f)
)
leadCoefficient NCRingElement := f -> if size f == 0 then 0 else (pairs (leadTerm f).terms)#0#1;
isConstant NCRingElement := f -> f.terms === hashTable {} or (#(f.terms) == 1 and f.terms#?(ncMonomial({},ring f)))
isHomogeneous NCRingElement := f -> (
    B := ring f;
    if f == promote(0,B) then true
    else (
    fTerms := keys f.terms;
    degf := degree first fTerms;
    all(fTerms, g -> degree g == degf))
)
terms NCRingElement := f -> (
    for p in pairs (f.terms) list (
       putInRing(p#0,p#1)
    )
)
support NCRingElement := f -> (
   varSymbols := unique flatten apply(pairs (f.terms), (m,c) -> unique toList m#monList);
   apply(varSymbols, v -> putInRing({v},f.ring,1))
)

NCRingElement * List := List => (f,xs) -> apply(xs, x -> f*x);
List * NCRingElement := List => (xs,f) -> apply(xs, x -> x*f);

isCentral = method()
isCentral (NCRingElement, NCGroebnerBasis) := (f,ncgb) -> (
   varsList := gens f.ring;
   all(varsList, x -> (f*x - x*f) % ncgb == 0)
)

isCentral NCRingElement := f -> (
   varsList := gens f.ring;
   all(varsList, x -> (f*x - x*f) == 0)   
)

isCommutative NCRing := A -> all(gens A, x -> isCentral x)

isExterior = method()
isExterior NCRing := 
isExterior Ring := A -> (
   sc := apply(subsets(gens A,2), s-> s_0*s_1+s_1*s_0);
   sq := apply(gens A, g->g^2);
   all(sc|sq, x-> x==0)
)

toM2Ring = method(Options => {SkewCommutative => false})
toM2Ring NCRing := opts -> B -> (
   gensB := gens B;
   R := coefficientRing B;
   gensI := gens ideal B;
   abB := R [ gens B , SkewCommutative => opts#SkewCommutative];
   if gensI == {} then
      abB
   else (
      phi := ambient ncMap(abB,B,gens abB);
      abI := ideal flatten entries mingens ideal ((gensI) / phi);
      if abI == 0 then
         abB
      else
         abB/abI
   )
)

toNCRing = method()
toNCRing Ring := R -> (
   isComm := isCommutative R;
   isExter := isExterior R;
   if not isComm and not isExter then error "Input ring must be either strictly (-1)-skew commutative or commutative.";
   --- generate the (skew)commutativity relations
   Q := coefficientRing R;
   A := Q (gens R);
   phi := ncMap(A,ambient R,gens A);
   commRelations := apply(subsets(gens A,2), s-> s_0*s_1+(-1)^(if isComm then -1 else 0)*s_1*s_0);
   extRelations := if isExter then apply(gens A, s -> s^2) else {};
   --- here is the defining ideal of the commutative algebra, inside the tensor algebra
   I := ncIdeal (commRelations | extRelations | ((flatten entries gens ideal R) / phi));
   commIgb := gb ideal R;
   maxDeg := (((flatten entries gens commIgb) / degree) | {{0}}) / sum // max;
   Igb := ncGroebnerBasis(I, DegreeLimit => 2*maxDeg);
   A/I
)

isNormal NCRingElement := f -> (
   if not isHomogeneous f then error "Expected a homogeneous element.";
   all(gens ring f, x -> findNormalComplement(f,x) =!= null)
)

normalAutomorphism = method()
normalAutomorphism NCRingElement := f -> (
   B := ring f;
   normalComplements := apply(gens B, x -> findNormalComplement(f,x));
   if any(normalComplements, f -> f === null) then error "Expected a normal element.";
   ncMap(B, B, normalComplements)
)

findNormalComplement = method()
findNormalComplement (NCRingElement,NCRingElement) := (f,x) -> (
   B := ring f;
   if B =!= ring x then error "Expected elements from the same ring.";
   if not isHomogeneous f or not isHomogeneous x then error "Expected homogeneous elements";
   n := degree f;
   m := degree x;
   leftFCoeff := sparseCoeffs(f*x,Monomials=>flatten entries basis(n+m,B));
   rightMultF := rightMultiplicationMap(f,m);
   factorMap := (leftFCoeff // rightMultF);
   if rightMultF * factorMap == leftFCoeff then
      first flatten entries (basis(m,B) * factorMap)
   else
      null
)

getMinMaxDegrees = gensList -> (
   minNCGBDeg := minCoeffDeg := infinity;
   maxNCGBDeg := maxCoeffDeg := -infinity;
   scan(gensList, f -> (degf := tensorLength f;  --- had to change this to tensor length for GB reduction...
                        degLeadCoeff := if isField coefficientRing ring f then 0 else first degree leadCoefficient f;
                        if degf > maxNCGBDeg then maxNCGBDeg = degf;
                        if degf < minNCGBDeg then minNCGBDeg = degf;
                        if degLeadCoeff > maxCoeffDeg then maxCoeffDeg = degLeadCoeff;
                        if degLeadCoeff < minCoeffDeg then minCoeffDeg = degLeadCoeff;));
   (minNCGBDeg,maxNCGBDeg,minCoeffDeg,maxCoeffDeg)
)

----------------------------------------

----------------------------------------
--- Bergman related functions
----------------------------------------

runCommand = cmd -> (
   --- comment this line out eventually, or add a verbosity option
   -- stderr << "--running: " << cmd << " ... " << flush;
   r := run cmd;
   if r != 0 then (
      << "Failed!" << endl;
      error("--command failed, error return code ",r);
   )
   else stderr << "Complete!" << endl;
)

makeVarListString = B -> (
   varListString := "vars ";
   gensB := gens B;
   lastVar := last gensB;
   varListString = varListString | (concatenate apply(drop(gensB,-1), x -> (toStringMaybeSort x) | ","));
   varListString | (toStringMaybeSort lastVar) | ";"
)

makeGenListString = genList -> (
   lastGen := last genList;
   genListString := concatenate apply(drop(genList,-1), f -> toStringMaybeSort first clearDenominators f | ",\n");
   genListString | (toStringMaybeSort first clearDenominators lastGen) | ";\n"
)

makeVarWeightString = (B,n) -> (
   gensB := gens B;
   numgensB := #gensB;
   adjust := if n == 0 then 0 else (
      minDeg := min (drop(gensB,numgensB - n) / degree);
      if minDeg <= 0 then 1-minDeg else 0
   );
   concatenate apply(#gensB,i -> (if i >= numgensB - n then toString ((degree gensB#i) + adjust) else toString degree gensB#i) | " ")
)

writeBergmanInputFile = method(Options => {ComputeNCGB => true,
                                           DegreeLimit => 10,
                                           NumModuleVars => 0})
writeBergmanInputFile (List, String) := opts -> (genList, tempInput) -> (
   B := ring first genList;
   charB := char coefficientRing B;
   degList := (genList / degree) | {0};
   maxDeg := max(opts#DegreeLimit, 2*(max degList));
   genListString := makeGenListString genList;
   writeBergmanInputFile(B,
                         genListString,
                         tempInput,
                         NumModuleVars => opts#NumModuleVars,
                         DegreeLimit => maxDeg,
                         ComputeNCGB => opts#ComputeNCGB);
)

writeBergmanInputFile (NCRing,String,String) := opts -> (B,genListString,tempInput) -> (
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
      fil << "(load (mkbmpathexpand \"$bmload/hseries.fas\"))" << endl;
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
gbFromOutputFile(NCPolynomialRing,String) := opts -> (A,tempOutput) -> (
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
      maxDeg := 2*(max((gensList / degree)|{0}));
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
  if not I.ring#BergmanRing then
  (
     -- If the coefficients of the gens of I only use QQ or ZZ/p, then compute
     -- gb over the ring with QQ or ZZ/p coefficients, then sub back into the
     -- right ring.
     k := bergmanCoefficientRing gens I;
     if k === null then error "Bergman interface can only handle coefficients over QQ or ZZ/p at the present time.";
     I = newBergmanIdealRing(I,k);
     phi = ncMap(ring oldI, ring I, gens ring oldI);
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
  runCommand("bergman -on-error exit --silent < " | tempInit | " > " | tempTerminal);
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
   phi := ncMap(tempRing,ring I, gens tempRing);
   phi I
)

------------------------------------------------------
----- Bergman Normal Form commands
------------------------------------------------------

writeNFInputFile = method(Options => {UsePreviousGBOutput => true})
writeNFInputFile (List,NCGroebnerBasis, List, ZZ) := opts -> (fList,ncgb, inputFileList, maxDeg) -> (
   genList := (pairs ncgb.generators) / last;
   --- set up gb computation
   -- need to also test if ncgb is in fact a gb, and if so, tell Bergman not to do the computation
   if opts#UsePreviousGBOutput then
      writeBergmanInputFile(genList,inputFileList#0,DegreeLimit=>maxDeg,ComputeNCGB=>false);
   --- now set up the normal form computation
   fil := openOut inputFileList#1;
   for f in fList do (
      fil << "(readtonormalform)" << endl;
      fil << toStringMaybeSort f << ";" << endl;
   );
   fil << close;
)
writeNFInputFile (NCRingElement, NCGroebnerBasis, List, ZZ) := (f, ncgb, inputFileList, maxDeg) ->
   writeNFInputFile({f},ncgb,inputFileList,maxDeg)

writeNFInitFile = method()
writeNFInitFile (String, String, String, String) := (tempInit, tempGBInput, tempNFInput, tempGBOutput) -> (
   fil := openOut tempInit;
   fil << "(simple \"" << tempGBInput << "\" \"" << tempGBOutput << "\")" << endl;
   fil << "(simple \"" << tempNFInput << "\")" << endl;
   fil << "(quit)" << endl << close;   
)

nfFromTerminalFile = method()
nfFromTerminalFile (NCRing,String) := (A,tempTerminal) -> (
   fil := openIn tempTerminal;
   outputLines := lines get fil;
   eltPositions := positions(outputLines, l -> l == "is reduced to");
   -- remember previous setup
   oldVarSymbols := A.generatorSymbols;
   oldVarValues := oldVarSymbols / value;
   -- switch to tensor algebra
   use A;
   retVal := outputLines_(apply(eltPositions, i -> i + 1)) / value / first;
   -- roll back to old variables
   scan(oldVarSymbols, oldVarValues, (sym,val) -> sym <- val);
   -- return normal form, promoted to A (in case there are any ZZ or QQ)
   retVal / (f -> promote(f,A))
)

normalFormBergman = method(Options => {})
normalFormBergman (List, NCGroebnerBasis) := opts -> (fList, ncgb) -> (
   -- don't send zero elements to bergman, or an error occurs
   oldFList := fList;
   fListLen := #fList;
   nonConstantIndices := positions(fList, f -> not isConstant f);
   fList = fList_nonConstantIndices;
   maxDeg := 2*max((gens ncgb) / degree);
   -- if there are no nonzero entries left, then return
   if fList == {} then return fList;
   nonConstantIndices = set nonConstantIndices;
   numConstantIndices := fListLen - #nonConstantIndices;
   A := (first fList).ring;
   if not A#BergmanRing then 
      error "Bergman interface can only handle coefficients over QQ or ZZ/p at the present time." << endl;
   -- prepare the call to Bergman
   tempInit := temporaryFileName() | ".init";            -- init file
   usePreviousGBOutput := ncgb.cache#?"bergmanGBFile";
   tempGBInput := if usePreviousGBOutput then    -- gb input file
                     ncgb.cache#"bergmanGBFile"
                  else
                     temporaryFileName() | ".bigb";
   tempOutput := temporaryFileName() | ".bo";            -- gb output goes here
   tempTerminal := temporaryFileName() | ".ter";         -- terminal output goes here
   tempNFInput := temporaryFileName() | ".binf";         -- nf input file
   -- (**) clear denominators first, since Bergman doesn't like fractions
   newFList := apply(fList, f -> clearDenominators f);
   << "Writing bergman input file." << endl;
   writeNFInputFile(newFList / first,ncgb,{tempGBInput,tempNFInput},maxDeg,UsePreviousGBOutput=>usePreviousGBOutput);
   << "Writing bergman init file." << endl;
   writeNFInitFile(tempInit,tempGBInput,tempNFInput,tempOutput);
   stderr << "--Calling Bergman for NF calculation for " << #nonConstantIndices << " elements." << endl;
   runCommand("bergman -on-error exit --silent < " | tempInit | " > " | tempTerminal);
   -- these are now the nfs of the nonzero entries.  Need to splice back in
   -- the zeros where they were.
   nfList := nfFromTerminalFile(A,tempTerminal);
   -- (**) then put denominators back
   nfList = apply(#nfList, i -> ((newFList#i#1)^(-1))*nfList#i);
   functionMingle(nfList,toList(numConstantIndices:0), i -> member(i,nonConstantIndices))
)

normalFormBergman (NCRingElement, NCGroebnerBasis) := opts -> (f,ncgb) ->
   first normalFormBergman({f},ncgb,opts)

------------------------------------------------------
----- Bergman Hilbert commands
------------------------------------------------------

-- can't be a method.  Too many arguments
--writeHSInitFile = method()
writeHSInitFile := (tempInit,
                    tempInput,
		    tempGBOutput,
		    tempPBOutput,
		    tempHSOutput) -> (
   fil := openOut tempInit;
   fil << "(setf (getenv \"bmload\") (mkbmpathexpand \"$bmload\"))" << endl;
   fil << "(ncpbhgroebner " 
       << "\"" << tempInput << "\" "
       << "\"" << tempGBOutput << "\" "
       << "\"" << tempPBOutput << "\" "
       << "\"" << tempHSOutput << "\")" << endl;
   fil << "(calctolimit (getmaxdeg))" << endl;
   fil << "(tdegreehseriesout (getmaxdeg))" << endl;
   fil << "(quit)" << endl << close;   
)

hsFromOutputFiles = method()
hsFromOutputFiles (NCQuotientRing,String,String) := (B,tempOutput,tempTerminal) -> (
   fil1 := openIn tempOutput;
   file1Lines := lines get fil1;
   fil2 := openIn tempTerminal;
   file2Lines := lines get fil2;
   dims := select(file1Lines | file2Lines, l -> #l > 0 and l#0 == "+");
   dims = apply(dims, d -> first select("[0-9][0-9]*",d)) / value;
   T := first gens B.degreesRing;
   1 + sum apply(gens B, x -> T^(degree x)) + sum apply(#dims, i -> (dims#i)*T^(i+2))
)

hilbertBergman = method(Options => {DegreeLimit => 10  -- DegreeLimit = 0 means return rational function (if possible, not yet functional)
                                                        -- else return as a power series
                                   })
-- will add this later
-- hilbertBergman NCIdeal := opts -> I -> (
hilbertBergman NCQuotientRing := opts -> B -> (
  -- Is the result already here? Check the cache and return if so.
  if not B#BergmanRing then 
     error "Bergman interface can only handle coefficients over QQ or ZZ/p at the present time." << endl;
  if (gens B) / degree // max != 1 or (gens B) / degree // min != 1 then
     error "Bergman currently only computes Hilbert series correctly for standard graded algebras.  Use hilbertSeries." << endl;
  -- prepare the call to bergman
  tempInit := temporaryFileName() | ".init";      -- init file
  tempInput := temporaryFileName() | ".bi";       -- gb input file
  tempGBOutput := temporaryFileName() | ".bgb";   -- gb output goes here
  tempHSOutput := temporaryFileName() | ".bhs";   -- hs output goes here
  tempPBOutput := temporaryFileName() | ".bpb";   -- pb 'output' goes here (?)
  tempTerminal := temporaryFileName() | ".ter";   -- terminal output goes here
  I := ideal B;
  gensI := gens I;
  writeBergmanInputFile(gensI,
                        tempInput,
			DegreeLimit=>opts#DegreeLimit);
  writeHSInitFile(tempInit,tempInput,tempGBOutput,tempPBOutput,tempHSOutput);
  stderr << "--Calling bergman for HS computation." << endl;
  runCommand("bergman -on-error exit --silent < " | tempInit | " > " | tempTerminal);
  I.cache#gb = gbFromOutputFile(ring I,tempGBOutput);
  hsFromOutputFiles(B,tempHSOutput,tempTerminal)
)

-------------------------------------------------
--- Bergman Kernel computations -----------------
-------------------------------------------------

buildMatrixRing = method()
buildMatrixRing NCMatrix := M -> (
   rowsM := #(M.target);
   colsM := #(M.source);
   B := ring M;
   kk := coefficientRing B;
   gensB := gens B;
   weightsB := gensB / degree;
   --- make the dictionary for the variables
   varsDic := new GlobalDictionary;
   RoW := getGlobalSymbol(varsDic, "RoW");
   CoL := getGlobalSymbol(varsDic, "CoL");
   --- create the variables
   rowGens := toList (RoW_0..RoW_(rowsM-1));
   colGens := toList (CoL_0..CoL_(colsM-1));
   --- store old values of B gens
   oldVarSymbols := B.generatorSymbols;
   oldVarValues := oldVarSymbols / value;
   --- create the ring
   gensC := gensB | colGens | rowGens;
   C := kk gensC;
   --- roll back B gens
   scan(oldVarSymbols, oldVarValues, (sym,val) -> sym <- val);
   --- fill the cache
   C.cache#Dictionary = varsDic;
   C.cache#"MatrixRingOver" = B;
   C.cache#"rows" = rowsM;
   C.cache#"cols" = colsM;
   -- set weights
   setWeights(C,weightsB | (M.source) | (M.target))
)

buildMatrixRelations = method()
buildMatrixRelations NCMatrix := M -> (
   --- if done before, no need to recreate
   if M.cache#?"BergmanRelations" then return M.cache#"BergmanRelations";
   C := buildMatrixRing M;
   B := ring M;
   I := ideal B;
   rowsM := #(M.target);
   colsM := #(M.source);
   gensB := gens B;
   gensC := gens C;
   phi := ncMap(C,B,gensC_{0..(#gensB-1)});
   colVars := gensC_{(#gensB)..(#gensB+colsM-1)};
   rowVars := gensC_{(#gensB+colsM)..(#gensC-1)};
   rowVarMatr := ncMatrix {rowVars};
   colVarMatr := ncMatrix {colVars};
   bergmanRels := (flatten entries (rowVarMatr*(phi M) - colVarMatr));
   phi = if class B === NCQuotientRing then ambient phi else phi;
   M.cache#"BergmanRelations" = bergmanRels;
   ((gens ideal B) / phi) | bergmanRels
)

rightKernelBergman = method(Options=>{DegreeLimit=>10})
rightKernelBergman (NCMatrix) := opts -> (M) -> (
   B := ring M;
   if not B#BergmanRing then 
      error "Bergman interface can only handle coefficients over QQ or ZZ/p at the present time.";
   if not isHomogeneous M then
      error "Expected a homogeneous matrix.";
   maxKerDeg := max M.source + opts#DegreeLimit;
   if M.cache#?rightKernel and M.cache#rightKernelDegreeLimit >= maxKerDeg then
      return M.cache#rightKernel;
   matrRels := buildMatrixRelations M;
   C := ring first matrRels;
   kerGBGens := getMatrixGB(M, DegreeLimit => maxKerDeg);
   kerM := minimizeMatrixKerGens(C,M,gens kerGBGens, DegreeLimit=>maxKerDeg);
   M.cache#rightKernel = kerM;
   M.cache#rightKernelDegreeLimit = maxKerDeg;
   kerM
)

getKernelElements = (kerGens,gensBinC,colGens) -> (
   select(kerGens, f -> isKernelElement(f,gensBinC,colGens))
)

isKernelElement = (f, gensBinC, colGens) -> (
   fsupp := support f;
   -- isSubset uses === and not ==, so can't use it (did this break recently?)
   all(fsupp, f -> any(gensBinC | colGens, g -> f == g)) and 
   any(fsupp, f -> all(gensBinC, g -> f != g))
   --isSubset(fsupp,gensBinC | colGens) and not isSubset(fsupp,gensBinC)
)

getModuleCoefficients = method()
getModuleCoefficients (List, List) := (modElts, modVars) -> (
   --- this is for the case of an ideal, where there is not a 'module' element.
   if modVars == {} then return ncMatrix {modElts};
   --- the rest is for honest module coefficients
   -- if the map is injective, then return zero.  Will make more robust later when
   -- modules are implemented
   if modElts == {} then return null;
   C := ring first modElts;
   B := C.cache#"MatrixRingOver";
   cols := C.cache#"cols";
   rows := C.cache#"rows";
   bGensList := apply(gens B | toList (cols:0) | toList (rows:0), f -> promote(f,B));
   CtoB := ncMap(B,C,bGensList);
   modVarSymbols := modVars / baseName;
   -- finally, build the matrix corresponding to the kernel
   MkerC := ncMatrix {for f in modElts list (
      -- need to get the coefficients of each element as a column vector.
      -- the entry in row i of this column vector will be the coefficient of Col_i
      -- need to be careful, however, since there may be several entries with Col_i
      -- as a leading coefficient.
      eltCoeffs := new MutableHashTable from {};
      -- put in all module variables with zeroes
      scan(modVarSymbols, c -> eltCoeffs#c = promote(0,C));
      for p in pairs f.terms do (
         modvar := first p#0#monList;
         theRest := putInRing(drop(p#0#monList,1),C,1);
         if eltCoeffs#?modvar then
            eltCoeffs#modvar = eltCoeffs#modvar + theRest*(p#1);
      );
      ncMatrix apply(modVarSymbols, c -> {eltCoeffs#c})
   )};
   --- make homogeneous
   if all(modElts, isHomogeneous) then
      assignDegrees(MkerC,modVars / degree,modElts / degree);
   CtoB MkerC
)

rightMingens = method()
rightMingens NCMatrix := M -> (
   --- returns a minimal generating set of the right column space
   --- of the input matrix 
   --- Not yet complete...
   B := ring M;
   if not B#BergmanRing then 
      error "Bergman interface can only handle coefficients over QQ or ZZ/p at the present time.";
   if not isHomogeneous M then
      error "Expected a homogeneous matrix.";
   minDeg := min M.source;
   minDegPos := positions(M.source, m -> m == minDeg);
   otherPos := positions(M.source, m -> m != minDeg);
   minGens := M_minDegPos;
   loopM := M;
   --- rewrite using module elements rather than matrices?  Probably faster.
   while otherPos != {} do (
      loopM = loopM_otherPos;
      facMatr := loopM // minGens;
      remainders := loopM - minGens*facMatr;
      possGens := positions(toList(0..#otherPos-1), i -> remainders_{i} != 0);
      if possGens != {} then (
         minGens = minGens | loopM_{first possGens};
         otherPos = drop(possGens, 1);
      )
      else
         otherPos = possGens;
   );
   minGens
)

minimizeMatrixKerGens = method(Options => options rightKernelBergman)
minimizeMatrixKerGens(NCPolynomialRing,NCMatrix,List) := opts -> (C,M,kerGens) -> (
   -- first select only the generators that involve just the columns and the
   -- ring variables alone.
   B := ring M;
   cols := #(M.source);
   rows := #(M.target);
   gensC := gens C;
   gensBinC := take(gensC, numgens B);
   ambientBtoC := if class B === NCQuotientRing then ambient ncMap(C,B,gensBinC) else ncMap(C,B,gensBinC);
   gbIdealB := (gens ncGroebnerBasis ideal B) / ambientBtoC;
   colGens := gensC_{(numgens B)..(numgens B+cols-1)};
   colGenSymbols := (C.generatorSymbols)_{(numgens B)..(numgens B+cols-1)};
   kerGensElim := getKernelElements(kerGens,gensBinC,colGens);
   sortGens := sortUsing(kerGensElim,f -> (degree f, #(terms f)));
   kernelMatrix := getModuleCoefficients(sortGens,colGens);
   -- this is the case when the map is injective
   if kernelMatrix === null then return ncMatrix {{promote(0,B)}};
   minMker := rightMingens kernelMatrix;
   minMker
)

------------------------------------------------------------------
--- End Bergman interface code
------------------------------------------------------------------

------------------------------------------------------------------
------- NCGroebnerBasis methods
------------------------------------------------------------------
ring NCGroebnerBasis := Igb -> ring first gens Igb

generators NCGroebnerBasis := opts -> ncgb -> (pairs ncgb.generators) / last

leftNCGroebnerBasis = method()
leftNCGroebnerBasis List := genList -> (
)

rightNCGroebnerBasis = method()
rightNCGroebnerBasis List := genList -> (
)

ncGroebnerBasis = method(Options => {DegreeLimit => 100,
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
   stack apply(pairs ncgb.generators, (lt,pol) -> (net pol) | net "; Lead Term = " | (net lt))
)

ZZ % NCGroebnerBasis := (n,ncgb) -> n
QQ % NCGroebnerBasis := (n,ncgb) -> n

basis(ZZ,NCRing) := NCMatrix => opts -> (n,B) ->
   newBasis(n,B,CumulativeBasis=>false)
   
cumulativeBasis(ZZ,NCRing) := NCMatrix => (n,B) ->
   newBasis(n,B,CumulativeBasis=>true)

newBasis(ZZ,NCRing) := NCMatrix => opts -> (n,B) -> (
   if n == 0 then return ncMatrix {{promote(1,B)}};
   ncgbGens := if class B === NCQuotientRing then pairs (ncGroebnerBasis B.ideal).generators else {};
   basisList := {ncMonomial({},B)};
   doneList := if opts#CumulativeBasis then basisList else {};
   varsList := apply(B.generatorSymbols, v -> ncMonomial({v},B));
   numVars := #varsList;
   degreeList := gens B / degree;
   mindeg := min degreeList;
   lastTerms := ncgbGens / first / first;
   while basisList =!= {} do (
      -- build the next tensor weight monomials
      newBasisList := flatten apply(basisList, mon -> removeNulls apply(#varsList, i -> if degreeList#i + degree mon <= n then (varsList#i) | mon));
      -- select only those that are not a lead term of a GB element
      if ncgbGens =!= {} then
         newBasisList = select(newBasisList, b -> all(lastTerms, mon -> not findSubstring(mon,b,CheckPrefixOnly=>true)));
      -- add qualifying monomials to done list
      if opts#CumulativeBasis then
         doneList = doneList | newBasisList
      else
         doneList = doneList | select(newBasisList, b -> degree b == n);
      -- now only select those that will be needed to build next step
      basisList = select(newBasisList, b -> degree b + mindeg <= n);
   );
   if doneList != {} then 
      ncMatrix {apply(doneList, mon -> putInRing(mon,1))}   
   else
      ncMatrix(B,{},{})
)

if bergmanPresent then TEST ///
debug NCAlgebra						   -- to get "newBasis"
A = QQ{a,b,c,d}
setWeights(A,{1,1,2,3})
time b1 = flatten entries basis(8,A);
time b2 = flatten entries newBasis(8,A);
B = skewPolynomialRing(QQ,(-1)_QQ,{x,y,z,w})
setWeights(B,{1,1,2,3})
time b1 = flatten entries basis(15,B);
time b2 = flatten entries newBasis(15,B);
///

leftMultiplicationMap = method()
leftMultiplicationMap(NCRingElement,ZZ) := (f,n) -> (
   B := f.ring;
   m := degree f;
   if m === -infinity then m = 0;
   nBasis := flatten entries basis(n,B);
   nmBasis := flatten entries basis(n+m,B);
   leftMultiplicationMap(f,nBasis,nmBasis)
)

leftMultiplicationMap(NCRingElement,ZZ,ZZ) := (f,n,m) -> (
   B := f.ring;
   nBasis := flatten entries basis(n,B);
   mBasis := flatten entries basis(m,B);
   leftMultiplicationMap(f,nBasis,mBasis)
)

leftMultiplicationMap(NCRingElement,List,List) := (f,fromBasis,toBasis) -> (
   local retVal;
   A := ring f;
   R := coefficientRing A;
   if not isHomogeneous f then error "Expected a homogeneous element.";
   if fromBasis == {} and toBasis == {} then (
      retVal = map(R^0,R^0,0);
      retVal
   )
   else if fromBasis == {} then (
      retVal = map(R^(#toBasis), R^0,0);
      retVal
   )
   else if toBasis == {} then (
      retVal = map(R^0,R^(#fromBasis),0);
      retVal
   )
   else (
      sparseCoeffs(f*fromBasis, Monomials=>toBasis)
   )
)

if bergmanPresent then TEST ///
A = QQ{a,b}
I = ncIdeal {a*a*a,a*a*b,a*b*a,a*b*b,b*a*a,b*a*b,b*b*a,b*b*b}
B = A/I
basis(0,B)
basis(1,B)
basis(2,B)
basis(3,B)
leftMultiplicationMap(a,-1,0)
leftMultiplicationMap(a,-1,0)
leftMultiplicationMap(a,-1,0)
///

rightMultiplicationMap = method()
rightMultiplicationMap(NCRingElement,ZZ) := (f,n) -> (
   B := f.ring;
   m := degree f;
   if m === -infinity then m = 0;
   nBasis := flatten entries basis(n,B);
   nmBasis := flatten entries basis(n+m,B);
   rightMultiplicationMap(f,nBasis,nmBasis)
)

rightMultiplicationMap(NCRingElement,ZZ,ZZ) := (f,n,m) -> (   
   if f != 0 and degree f != m-n then error "Expected third argument to be the degree of f, if nonzero.";
   B := f.ring;
   nBasis := flatten entries basis(n,B);
   mBasis := flatten entries basis(m,B);
   rightMultiplicationMap(f,nBasis,mBasis)
)

rightMultiplicationMap(NCRingElement,List,List) := (f,fromBasis,toBasis) -> (
   local retVal;
   A := ring f;
   R := coefficientRing A;
   if not isHomogeneous f then error "Expected a homogeneous element.";
   if fromBasis == {} and toBasis == {} then (
      retVal = map(R^0,R^0,0);
      retVal
   )
   else if fromBasis == {} then (
      retVal = map(R^(#toBasis), R^0,0);
      retVal
   )
   else if toBasis == {} then (
      retVal = map(R^0,R^(#fromBasis),0);
      retVal
   )
   else (
      sparseCoeffs(fromBasis*f, Monomials=>toBasis)
   )
)



centralElements = method()
centralElements(NCRing,ZZ) := (B,n) -> (
   idB := ncMap(B,B,gens B);
   normalElements(idB,n)
)

normalElements = method()
normalElements(NCRingMap,ZZ) := (phi,n) -> (
   if source phi =!= target phi then error "Expected an automorphism.";
   B := source phi;
   ringVars := gens B;
   diffMatrix := matrix apply(ringVars, x -> {leftMultiplicationMap(phi x,n) - rightMultiplicationMap(x,n)});
   nBasis := basis(n,B);
   kerDiff := ker diffMatrix;
   R := ring diffMatrix;
   if kerDiff == 0 then sub(matrix{{}},R) else nBasis * (gens kerDiff)
)

normalElements (NCQuotientRing, ZZ, Symbol, Symbol) := (R,n,x,y) -> (
   -- Inputs: An NCQuotientRing R, a degree n, and two symbols to use for indexed variables.
   -- Outputs: (1) A list of normal monomials in degree n 
   --          (2) the components of the variety of normal elements (excluding the normal monomials) 
   --              expressed as ideals in terms of coefficients of non-normal monomial basis elements
   -- 
   -- The variety also contains information about the normalizing automorphism. This information is not displayed.
   -- The user can obtain the normalizing automorphism of a normal element via the normalAutomorphism function
   fromBasis := flatten entries basis(n,R);
   toBasis := flatten entries basis(n+1,R);
   pos := positions(fromBasis,m->isNormal(m));
   normalBasis := apply(pos, i-> fromBasis#i);
   nonNormalBasis := fromBasis-set normalBasis;
   -- print a list of normal monomials
      << "Normal monomials of degree " << n << ":" << endl;
      for i from 0 to ((length normalBasis) - 1) do (
        << normalBasis#i << endl;
      );
      if (length normalBasis)==0 then << "none" << endl;
   numGens := numgens R;
   leftMaps := apply(gens R, x->leftMultiplicationMap(x,nonNormalBasis,toBasis));
   rightMaps := apply(gens R, x->rightMultiplicationMap(x,nonNormalBasis,toBasis));
   -- make a polynomial ring with (fromDim) - (number of normal monomials)  + (numgens R)^2 variables
   -- need to hide variables from user
   xvars := apply(nonNormalBasis, i->x_i);
   yvars := table(numGens, numGens, (i,j) -> y_(i,j));
   cRing := R.CoefficientRing[(flatten yvars) | xvars,MonomialOrder=>Eliminate numGens^2];
   xvars = xvars / value;
   yvars = applyTable(yvars,value);
   leftCoeff := apply(leftMaps, L-> L*transpose matrix {xvars});
   rightCoeff := apply(rightMaps, R-> R*transpose matrix {xvars});
   idealGens := flatten apply(numGens,g-> rightCoeff#(g-1) - sum(numGens,j->(yvars#g#j)*leftCoeff#(j-1)));
   I:=ideal idealGens;
   -- the next line throws away information, including automorphism data
   << "Components of the normal variety, excluding normal monomials:" << endl;
   unique(select(apply(xvars, x-> (J:=saturate(I,ideal(x));
                                   if J!=cRing then 
                                      selectInSubring(1, gens gb J)
                                   else
                                      0)),c->c!=0))
)

--- This is the command that computes the kernel via linear
--- algebra over the base ring.  It can only handle
--- a matrix of homogeneous entries all of the same degree
rightKernel = method(Options=>{NumberOfBins => 1, Verbosity=>0})
rightKernel(NCMatrix,ZZ):= opts -> (M,deg) -> (
   -- Assume (without checking) that the entries of M are homogeneous of the same degree n
   -- This function takes a NCMatrix M and a degree deg and returns the right kernel in degree deg
   -- Increasing bins can provide some memory savings if the degree deg part of the ring is large. Optimal bin size seems to be in the 1000-2000 range.
   bins := opts#NumberOfBins;
   rows := # entries M;
   cols := # first M.matrix;
   MasList := flatten entries M;
   n := max apply(MasList, i->degree i);

   bas := basis(deg,M.ring);
   fromBasis := flatten entries bas;
   toBasis := flatten entries basis(deg+n,M.ring);

   fromDim := #fromBasis; --the number of rows of K is dim*cols
   toDim := #toBasis; --the number of rows in multiplication map

   -- packing variables
   if toDim % bins != 0 then error "Basis doesn't divide evenly into that many bins";
   pn := toDim//bins; -- denominator is number of bins
   pB := pack(toBasis,pn);

   -- zero vectors
   fromZeros := apply(toList(0..(fromDim-1)),i->0);
   toZeros := transpose matrix{apply(toList(0..(#(pB#0)-1)),i->0)};
   zeroMat := matrix{apply(fromZeros, i-> toZeros)};
 
   --initialize 
   Kscalar := (coefficientRing M.ring)^(fromDim*cols);
   nextKer := 0;  
   U:= 0;
   Lmat:=0;
   for row from 0 to (rows-1) when Kscalar!=0 do (
       if opts#Verbosity > 0 then 
          << "Computing kernel of row " << row+1 << " of " << rows << endl; 
         U = ncMatrix{ (M.matrix)#row };
         Lmat = (transpose U)*bas;
       for ind from 0 to (#pB-1) when Kscalar!=0 do (
       	   if opts#Verbosity > 0 then
              << "Converting to coordinates" << endl;

           coeffs:= sparseCoeffs(flatten entries Lmat, Monomials=>pB#ind);

       	   nextKer = sub(ker coeffs, coefficientRing M.ring);
	   if opts#Verbosity > 0 then
              << "Updating kernel" << endl;
	   Kscalar = intersect(Kscalar,nextKer);
	   );
   );

   if Kscalar == 0 then
      return 0
   else
      if opts#Verbosity > 0 then << "Kernel computed. Reverting to ring elements." << endl;
   retVal := ncMatrix apply(toList(0..(cols-1)), k-> {bas*submatrix(gens Kscalar,{k*fromDim..(k*fromDim+fromDim-1)},)});
   --- now need to assign degrees.
   assignDegrees(retVal,M.source, toList ((#(first entries retVal)):((first M.source)+deg)));
   retVal
)

isLeftRegular = method()
isLeftRegular (NCRingElement, ZZ) := (f,d) -> (
   A := ring f;
   if not isHomogeneous f then error "Expected a homogeneous element.";
   r := rank rightMultiplicationMap(f,d);
   s := #(flatten entries basis(d,A));
   r == s
)

isRightRegular = method()
isRightRegular (NCRingElement, ZZ) := (f,d) -> (
   A := ring f;
   if not isHomogeneous f then error "Expected a homogeneous element.";
   r := rank leftMultiplicationMap(f,d);
   s := #(flatten entries basis(d,A));
   r == s
)

NCRingElement % NCGroebnerBasis := (f,ncgb) -> (
   if (degree f <= MAXDEG and size f <= MAXSIZE) or not f.ring#BergmanRing then
      remainderFunction(f,ncgb)
   else
      first normalFormBergman({f},ncgb)
)

--- need to make this work for non-standard gradings
ncSubstrings = method()
ncSubstrings (NCMonomial,ZZ,ZZ) := (mon,m,n) -> (
   monLen := #(mon#monList);
   flatten apply(toList(1..(monLen)), i -> if i > n or i < m then 
                                            {}
                                         else
                                            apply(monLen-i+1, j -> (mon_{0..(j-1)},mon_{j..j+i-1},mon_{j+i..(monLen-1)})))
--   flatten apply(toList(1..(monLen)), i -> if i > n or i < m then 
--                                            {}
--                                         else
--                                            apply(monLen-i+1, j -> (mon_{0..(j-1)},mon_{j..j+i-1},mon_{j+i..(monLen-1)})))
)

--- this one is ok
cSubstrings = method()
cSubstrings (List,ZZ,ZZ) := (exps,m,n) -> (
   if #exps == 1 then 
      apply(toList (m..(min(exps#0,n))),l -> {l})
   else
      flatten for i from 0 to min(exps#0,n) list apply(cSubstrings(drop(exps,1),max(0,m-i),max(0,n-i)), l -> {i} | l)
)

commDivides = (x,y) -> (y // x)*x == y

List ** List := (xs,ys) -> flatten for y in ys list apply(xs, x -> {x,y})

remainderFunction = method(Options => {DontUse => 0})
remainderFunction (NCRingElement,NCGroebnerBasis) := opts -> (f,ncgb) -> (
   if #(gens ncgb) == 0 then return f;
   if f == 0 then return f;
   if ((gens ncgb)#0).ring =!= f.ring then error "Expected GB over the same ring.";
   dontUse := opts#DontUse;
   ncgbHash := ncgb.generators;
   maxNCGBDeg := ncgb#MaxNCGBDegree;
   minNCGBDeg := ncgb#MinNCGBDegree;
   maxCoeffDeg := ncgb#MaxCoeffDegree;
   minCoeffDeg := ncgb#MinCoeffDegree;
   newf := f;
   R := f.ring.CoefficientRing;
   hasCoeffs := not (isField R);
   pairsf := sort select(pairs newf.terms, s-> s#1 != promote(0,R));
--   pairsf := sort pairs newf.terms;
   foundSubstr := {};
   coeff := null;
   gbHit := null;
   ncSubstrs := null;
   cSubstrs := null;
   for p in pairsf do (
      ncSubstrs = ncSubstrings(p#0,minNCGBDeg,maxNCGBDeg);
      cSubstrs = if hasCoeffs then
                    apply(cSubstrings(first exponents leadMonomial promote(p#1,R),minCoeffDeg,maxCoeffDeg), s -> R_s)
                 else
                    {1_R};
      foundSubstr = select(ncSubstrs ** cSubstrs, s -> ncgbHash#?(s#0#1,s#1) and
                                                       ncgbHash#(s#0#1,s#1) != dontUse);
      coeff = p#1;
      if foundSubstr =!= {} then (
         foundSubstr = minUsing(foundSubstr, s -> size ncgbHash#(s#0#1,s#1));
         gbHit = ncgbHash#(foundSubstr#0#1,foundSubstr#1);
         break;
      );
   );
   while foundSubstr =!= {} do (
      pref := putInRing(foundSubstr#0#0,1);
      suff := putInRing(foundSubstr#0#2,1);
      newf = newf - (promote(coeff,R)//promote(foundSubstr#1,R))*pref*gbHit*suff;
      pairsf = sort select(pairs newf.terms, s-> s#1 != promote(0,R));
      foundSubstr = {};
      gbHit = null;
      coeff = null;   
      for p in pairsf do (
         ncSubstrs = ncSubstrings(p#0,minNCGBDeg,maxNCGBDeg);
         cSubstrs = if hasCoeffs then 
                       apply(cSubstrings(first exponents leadMonomial promote(p#1,R),minCoeffDeg,maxCoeffDeg), s -> R_s)
                    else
                       {1_R};
         foundSubstr = select(ncSubstrs ** cSubstrs, s -> ncgbHash#?(s#0#1,s#1) and
                                                          ncgbHash#(s#0#1,s#1) != dontUse);
         coeff = p#1;
         if foundSubstr =!= {} then (
            foundSubstr = minUsing(foundSubstr, s -> size ncgbHash#(s#0#1,s#1));
            gbHit = ncgbHash#(foundSubstr#0#1,foundSubstr#1);
            break;
         );
      );
   );
   newf
)

---------------------------------------
----NCRingMap Commands -----------------
---------------------------------------

ncMap = method(Options => {Derivation=>false})
--- ncMap from Ring to NCRing not implemented.
ncMap (Ring,NCRing,List) := 
ncMap (NCRing,Ring,List) := 
ncMap (NCRing,NCRing,List) := opts -> (B,C,imageList) -> (
   genCSymbols := (gens C) / baseName;
   if opts#Derivation and B=!=C then error "Source and target of a derivation must be the same.";
   if not all(imageList / class, r -> r === B) then error "Expected a list of entries in the target ring.";
   new NCRingMap from hashTable {(symbol functionHash) => hashTable apply(#genCSymbols, i -> (genCSymbols#i,imageList#i)),
                                 (symbol source) => C,
                                 (symbol target) => B,
				 (symbol Derivation) => opts#Derivation,
				 (symbol cache) => new CacheTable from {}}
)

source NCRingMap := f -> f.source
target NCRingMap := f -> f.target
matrix NCRingMap := opts -> f -> (
     if member(NCRing, ancestors class f.target) then
        ncMatrix {(gens source f) / f}
     else
        matrix {(gens source f) / f}
)
--id _ NCRing := B -> ncMap(B,B,gens B)

NCRingMap NCRingElement := (f,x) -> (
   if x == 0 then return promote(0, target f);
   if ring x =!= source f then error "Ring element not in source of ring map.";
   C := ring x;
   if f.Derivation then
      sum for t in pairs x.terms list(   
         mon := t#0#monList;
         monImage := sum apply(# mon, j-> 
	  product apply(replace(j,f.functionHash#(mon_j),mon), s-> 
		 if class s===Symbol or class s===IndexedVariable then putInRing({s},C,1) else s
		 )
	    );
         promote(sub(t#1,coefficientRing target f)*monImage,C) -- an empty sum is 0, which we need to promote 
      )
   else
      sum for t in pairs x.terms list (
         monImage := promote(product apply(t#0#monList, v -> f.functionHash#v),target f);
         sub(t#1,coefficientRing target f)*monImage
      )
)


NCRingMap RingElement := (f,x) -> (
   if x == 0 then return promote(0, target f);
   if ring x =!= source f then error "Ring element not in source of ring map.";
   C := ring x;

   if f.Derivation then
      sum for t in terms x list (
         coeff := leadCoefficient t;
         mon := leadMonomial t;
         supp := support leadMonomial t;
         monImage := sum apply(# supp, j -> (
		    d:=degree(supp_j,mon);
		    product apply(# supp, k-> 
			 if k==j then  
			 -- chain rule
                            d*(f.functionHash#(baseName supp_k))^(d-1)
		      	 else (supp_k)^(degree(supp_k,mon))))
	            );
         promote(coeff*monImage,C) -- an empty sum is 0, which we need to promote
      )    
   else
      sum for t in terms x list (
         coeff := leadCoefficient t;
         mon := leadMonomial t;
         monImage := promote(product apply(support mon, y -> (f.functionHash#(baseName y))^(degree(y,mon))),target f);
         coeff*monImage
      )
)

NCRingMap NCMatrix := (f,M) -> (
   newMatr := applyTable(M.matrix, x -> f x);
   newM := ncMatrix newMatr;
   if isHomogeneous M and isHomogeneous f then
      assignDegrees(newM,M.target,M.source)
   else
      assignDegrees newM;
   newM
)

List / NCRingMap := (xs, f) -> apply(xs, x -> f x)

net NCRingMap := f -> (
   net "NCRingMap " | (net target f) | net " <--- " | (net source f)
)

ambient NCRingMap := f -> (
--- check whether the source or target is an NCRing
   C := source f;
   ambC := ambient C;
   genCSymbols := (gens C) / baseName;
   ncMap(target f, ambC, apply(genCSymbols, c -> f.functionHash#c))
)

isWellDefined NCRingMap := f -> (
--- check whether the source or target is an NCRing
   defIdeal := ideal source f;
   liftf := ambient f;
   all(gens defIdeal, x -> liftf x == 0)
)

isHomogeneous NCRingMap := f -> (
   gensB := gens source f;
   gensC := gens target f;
   all(gensB, x -> degree (f x) == degree x or f x == 0)
)

NCRingMap _ ZZ := (f,n) -> (
   if not isHomogeneous f then error "Expected a homogeneous NCRingMap.";
   if f.cache#?"DegreeMatrices" and f.cache#"DegreeMatrices"#?n then
      return f.cache#"DegreeMatrices"#n;
   B := source f;
   C := target f;
   srcBasis := flatten entries basis(n,B);
   tarBasis := flatten entries basis(n,C);
   imageList := srcBasis / f;
   if #(unique (select(imageList, g -> g != 0) / degree)) != 1 then
      error "Expected the image of degree " << n << " part of source to lie in single degree." << endl;
   retVal := sparseCoeffs(imageList,Monomials=> tarBasis);
   if not f.cache#?"DegreeMatrices" then f.cache#"DegreeMatrices" = new CacheTable from {};
   f.cache#"DegreeMatrices"#n = retVal;
   retVal
)

NCRingMap ? NCRingMap := (f,g) -> (matrix f) ? (matrix g)

NCRingMap @@ NCRingMap := (f,g) -> (
   if target g =!= source f then error "Expected composable maps.";
   ncMap(target f, source g, apply(gens source g, x -> f g x))
)

NCRingMap NCIdeal := (phi, I) -> ncIdeal ((gens I) / phi)

NCRingMap NCGroebnerBasis := (phi,Igb) -> (
   I := ncIdeal gens Igb;
   Iphi := phi I;
   ncGroebnerBasis(Iphi, InstallGB => true)
)

NCRingMap + NCRingMap := (f,g) -> (
    if source f =!= source g and target f =!= target g then
       error "Expected maps between the same source and target.";
    ncMap(target f, source f, apply(gens source f, x -> f x + g x))
)

ZZ * NCRingMap := 
QQ * NCRingMap := 
RingElement * NCRingMap := (a,f) -> (
    ncMap(target f, source f, apply(gens source f, x -> a*(f x)))
)

NCRingMap ^ ZZ := (f,n) -> (
   if source f =!= target f then
      error "Expected a ring endomorphism.";
   A := source f;
   if n < 0 then (
      M := f_1;
      if (rank M != numgens A) then 
         error "Expected an invertible ring map.";
      gensA := ncMatrix {gens A};
      g := ncMap(A, A, flatten entries (gensA*(M^(-1))));
      fold(abs(n):g, (a,b) -> a @@ b)
   )
   else if n == 0 then ncMap(A, A, gens A)
   else fold(n:f, (a,b) -> a @@ b)
)

kernelComponent = method()
kernelComponent (ZZ,NCRingMap) := (d,f) -> (
   -- computes the kernel of a homogeneous ring map in a specified degree
   if not isHomogeneous f then error "Expected degree 0 map.";
   R := source f;
   bas := basis(d,R);
   K := mingens ker f_d;
   if K == 0 then return ncMatrix{{promote(0,R)}} else bas*K
)

gddKernel = method()
gddKernel (ZZ,NCRingMap) := (d,f) -> (
  -- computes a generating set for the kernel of a homogeneous ring map up to a specified degree
  K := {};
  for i from 1 to d do (
     << "Computing kernel in degree " << i << endl;
     K = K | flatten entries kernelComponent(i,f);
  );
  minimizeRelations(select(K,r-> r!=0))
)

----------------------------
----- NCRing constructions
----------------------------

oppositeElement = method()
oppositeElement NCRingElement := f -> (
   new (ring f) from hashTable {
          (symbol ring, f.ring),
          (symbol cache, new CacheTable from {}),
          (symbol terms, hashTable apply(pairs f.terms, p -> (
               newMon := new NCMonomial from {(symbol monList) => reverse p#0#monList,
                                              (symbol ring) => ring f};
               (newMon,p#1))))}
)

oppositeRing = method()
oppositeRing NCRing := B -> (
   gensB := gens B;
   R := coefficientRing B;
   oppA := R gensB;
   if class B === NCPolynomialRing then return oppA;
   idealB := gens ideal B;
   phi := ncMap(oppA,ambient B, gens oppA);
   oppIdealB := idealB / oppositeElement / phi // ncIdeal;
   oppIdealBGB := ncGroebnerBasis(oppIdealB, InstallGB=>not B#BergmanRing);
   oppA / oppIdealB
)

validSkewMatrix = method()
validSkewMatrix Matrix := M -> (
   rows := numgens source M;
   cols := numgens target M;
   if rows != cols then return false;
   invOffDiag := all(apply(rows, i -> all(apply(toList(i..cols-1), j -> isUnit M_i_j and isUnit M_j_i and (M_j_i)^(-1) == M_i_j),b->b==true)),c->c==true);
   oneOnDiag := all(rows, i -> M_i_i == 1);
   invOffDiag and oneOnDiag
)

skewPolynomialRing = method()
skewPolynomialRing (Ring,Matrix,List) := (R,skewMatrix,varList) -> (
   if not validSkewMatrix skewMatrix then
      error "Expected a matrix M such that M_ij = M_ji^(-1).";
   if ring skewMatrix =!= R then error "Expected skewing matrix over base ring.";
   A := R varList;
   gensA := gens A;
   I := ncIdeal apply(subsets(numgens A, 2), p -> 
            (gensA_(p#0))*(gensA_(p#1)) - (skewMatrix_(p#0)_(p#1))*(gensA_(p#1))*(gensA_(p#0)));
   Igb := ncGroebnerBasis(I, InstallGB=>(not A#BergmanRing));
   B := A/I;
   B
)

skewPolynomialRing (Ring,ZZ,List) := 
skewPolynomialRing (Ring,QQ,List) := 
skewPolynomialRing (Ring,RingElement,List) := (R,skewElt,varList) -> (
   if class skewElt =!= R then error "Expected ring element over base ring.";
   A := R varList;
   gensA := gens A;
   I := ncIdeal apply(subsets(numgens A, 2), p ->
            (gensA_(p#0))*(gensA_(p#1)) - skewElt*(gensA_(p#1))*(gensA_(p#0)));
   Igb := ncGroebnerBasis(I, InstallGB=>(not A#BergmanRing));
   B := A/I;
   B
)

threeDimSklyanin = method(Options => {DegreeLimit => 5})
threeDimSklyanin (Ring, List, List) := opts -> (R, params, varList) -> (
   if #params != 3 or #varList != 3 then error "Expected lists of length 3.";
   if instance(varList#0, R) or instance(varList#0,ZZ) or instance(varList#0,QQ) then
      error "Expected list of variables in third argument.";
   A := R varList;
   gensA := gens A;
   I := ncIdeal {params#0*gensA#1*gensA#2+params#1*gensA#2*gensA#1+params#2*(gensA#0)^2,
                 params#0*gensA#2*gensA#0+params#1*gensA#0*gensA#2+params#2*(gensA#1)^2,
		 params#0*gensA#0*gensA#1+params#1*gensA#1*gensA#0+params#2*(gensA#2)^2};
   --Igb := ncGroebnerBasis(I, InstallGB=>(not A#BergmanRing), DegreeLimit=>opts#DegreeLimit);
   installGB := not (A#BergmanRing or bergmanCoefficientRing gens I =!= null);
   Igb := ncGroebnerBasis(I, InstallGB=>installGB, DegreeLimit=>opts#DegreeLimit);
   B := A/I;
   B
)
threeDimSklyanin (Ring, List) := opts -> (R, varList) -> (
   if char R =!= 0 then error "For random Sklyanin, QQ coefficients are required.";
   threeDimSklyanin(R,{random(QQ),random(QQ), random(QQ)}, varList)
)

fourDimSklyanin = method(Options => {DegreeLimit => 5})
fourDimSklyanin (Ring, List, List) := opts -> (R, params, varList) -> (
   if #params != 3 or #varList != 4 then error "Expected three parameters and four variables.";
   if instance(varList#0, R) or instance(varList#0,ZZ) or instance(varList#0,QQ) then
      error "Expected list of variables in third argument.";
   A := R varList;
   gensA := gens A;
   varList = gens A;
   f1 := (varList#0*varList#1 - varList#1*varList#0) - params#0*(varList#2*varList#3 + varList#3*varList#2);
   f2 := (varList#0*varList#2 - varList#2*varList#0) - params#1*(varList#3*varList#1 + varList#1*varList#3);
   f3 := (varList#0*varList#3 - varList#3*varList#0) - params#2*(varList#1*varList#2 + varList#2*varList#1);
   g1 := (varList#0*varList#1 + varList#1*varList#0) - (varList#2*varList#3 - varList#3*varList#2);
   g2 := (varList#0*varList#2 + varList#2*varList#0) - (varList#3*varList#1 - varList#1*varList#3);
   g3 := (varList#0*varList#3 + varList#3*varList#0) - (varList#1*varList#2 - varList#2*varList#1);

   I := ncIdeal {f1,f2,f3,g1,g2,g3};
   installGB := not (A#BergmanRing or bergmanCoefficientRing gens I =!= null);
   Igb := ncGroebnerBasis(I, InstallGB=>installGB, DegreeLimit=>opts#DegreeLimit);
   B := A/I;
   B
)
fourDimSklyanin (Ring, List) := opts -> (R, varList) -> (
   if char R != 0 and char R < 100 then error "For random Sklyanin, QQ coefficients or characteristic larger than 100 are required.";
   --- generate a generic four dimensional Sklyanin that is AS regular
   alpha := random(QQ);
   while (alpha == 0) do alpha = random(QQ);
   beta := random(QQ);
   while (beta == 0 or (1 + alpha*beta) == 0) do beta = random(QQ);
   gamma := (-alpha-beta)/(1 + alpha*beta);
   fourDimSklyanin(R,{alpha,beta,gamma}, varList)
)

oreIdeal = method(Options => {Degree => 1})
oreIdeal (NCRing,NCRingMap,NCRingMap,NCRingElement) := 
oreIdeal (NCRing,NCRingMap,NCRingMap,Symbol) := opts -> (B,sigma,delta,X) -> (
   -- This version assumes that the derivation is zero on B
   -- Don't yet have multiple rings with the same variables names working yet.  Not sure how to
   -- get the symbol with the same name as the variable.
   X = baseName X;
   kk := coefficientRing B;
   varsList := ((gens B) / baseName) | {X};
   C := kk varsList;
   A := ambient B;
   fromBtoC := ncMap(C,B,drop(gens C, -1));
   fromAtoC := ncMap(C,A,drop(gens C, -1));
   X = value X;
   setWeights(C,degrees A | {opts.Degree});
   ncIdeal (apply(gens B.ideal, f -> fromAtoC promote(f,A)) |
            apply(gens B, x -> X*(fromBtoC x) - (fromBtoC sigma x)*X - (fromBtoC delta x)))
)

oreIdeal (NCRing,NCRingMap,Symbol) := 
oreIdeal (NCRing,NCRingMap,NCRingElement) := opts -> (B,sigma,X) -> (
   zeroMap := ncMap(B,B,toList ((numgens B):promote(0,B)));
   oreIdeal(B,sigma,zeroMap,X,opts)
)

oreExtension = method(Options => options oreIdeal)
oreExtension (NCRing,NCRingMap,NCRingMap,Symbol) := 
oreExtension (NCRing,NCRingMap,NCRingMap,NCRingElement) := opts -> (B,sigma,delta,X) -> (
   X = baseName X;
   I := oreIdeal(B,sigma,delta,X,opts);
   C := ring I;
   C/I
)

oreExtension (NCRing,NCRingMap,Symbol) := 
oreExtension (NCRing,NCRingMap,NCRingElement) := opts -> (B,sigma,X) -> (
   X = baseName X;
   I := oreIdeal(B,sigma,X,opts);
   C := ring I;
   C/I
)

freeProduct = method()
freeProduct (NCRing,NCRing) := (A,B) -> (
   R := coefficientRing A;
   if R =!= (coefficientRing B) then error "Input rings must have same coefficient ring.";
   gensA := gens A;
   gensB := gens B;
   newgens := gensA | gensB;     
   if #unique(newgens) != (#gensA + #gensB) then error "Input rings have a common generator.";

   I := gens ideal A;
   J := gens ideal B;
   
   A' := if class A === NCPolynomialRing then A else ambient A;
   B' := if class B === NCPolynomialRing then B else ambient B;
    
   C := R newgens;
   gensAinC := take(gens C, #gensA);
   gensBinC := drop(gens C, #gensA);
   incA := ncMap(C,A',gensAinC);
   incB := ncMap(C,B',gensBinC);
   IinC := I / incA;
   JinC := J / incB;
   newIdealGens := select( (IinC | JinC), x -> x!=0);       
   if newIdealGens == {} then C 
   else C/(ncIdeal newIdealGens)
)

qTensorProduct = method()
qTensorProduct (NCRing, NCRing, ZZ) :=
qTensorProduct (NCRing, NCRing, QQ) :=
qTensorProduct (NCRing, NCRing, RingElement) := (A,B,q) -> (
   -- this is the q-commuting tensor product of rings
   R := coefficientRing A;
   if class q =!= QQ and q.ring =!= R then error "Twisting parameter must belong to coefficient ring.";
   F := freeProduct(A,B);
   gensAinF := take(gens F, #gens A);
   gensBinF := drop(gens F, #gens A);   
   -- create the commutation relations among generators of A and B
   K := flatten apply( gensAinF, g-> apply( gensBinF, h-> h*g-q*g*h));

   if class F === NCPolynomialRing then F/(ncIdeal K)
   else (
      I := gens ideal F;
      C := ambient F;
      newI := ncIdeal select( (I | K), g -> g!=0);
      C/newI
   )
     
)

NCRing ** NCRing := (A,B) -> (
   qTensorProduct(A,B,promote(1,coefficientRing A))
)

envelopingAlgebra = method()
envelopingAlgebra (NCRing, Symbol) := (A,x) -> (
   --  want to add an option to index op variables by number rather than a ring element?
   R := coefficientRing A;
   Aop := oppositeRing A;
   B := R apply(#gens A, g-> x_g);  -- remove # once indexing works without printing ( ) 
   if class A === NCPolynomialRing then (A ** B) 
   else (
      A' := ambient Aop;   
      f := ncMap(B,A',gens B);
      J := ncIdeal (gens ideal Aop / f);
      A ** (B/J)
   )
)

---------------------------------------
----NCMatrix Commands -----------------
---------------------------------------
ncMatrix = method()
ncMatrix List := ncEntries -> (
   if #ncEntries == 0 then error "Expected a nonempty list.";
   if not isTable ncEntries then error "Expected a rectangular matrix.";
   rows := #ncEntries;
   cols := #(ncEntries#0);
   if cols == 0 then error "Use ncMatrix(NCRing,List,List) for maps to/from the zero module.";
   --- here, we need to find a common ring to promote all the entries to before checking anything else.
   ringList := (flatten ncEntries) / ring;
   B := (ringList)#(position(ringList, r -> ancestor(NCRing,class r)));
   ncEntries = applyTable(ncEntries, e -> promote(e,B));
   types := ncEntries // flatten / class // unique;
   if #types != 1 then error "Expected a table of either NCRingElements over the same ring or NCMatrices.";
   local retVal;
   if ancestor(NCRingElement,types#0) then (
      retVal = new NCMatrix from hashTable {(symbol ring, (ncEntries#0#0).ring), 
                                            (symbol matrix, ncEntries),
                                   	    (symbol cache, new CacheTable from {})};
    
   )
   else if types#0 === NCMatrix then (
      -- this block of code handles a matrix of matrices and creates a large matrix from that
      blockEntries := applyTable(ncEntries, entries);
      -- this is a hash table with the sizes of the matrices in the matrix
      sizeHash := new HashTable from flatten apply(rows, i -> apply(cols, j -> (i,j) => (#(blockEntries#i#j), #(blockEntries#i#j#0))));
      -- make sure the blocks are of the right size, and all matrices are defined over same ring.
      if not all(rows, i -> #(unique apply(select(pairs sizeHash, (e,m) -> e#0 == i), (e,m) -> m#0)) == 1) then
         error "Expected all matrices in a row to have the same number of rows.";
      if not all(cols, j -> #(unique apply(select(pairs sizeHash, (e,m) -> e#1 == j), (e,m) -> m#1)) == 1) then
         error "Expected all matrices in a column to have the same number of columns.";
      rings := unique apply(flatten ncEntries, m -> m.ring);
      if #rings != 1 then error "Expected all matrices to be defined over the same ring.";
      -- now we may perform the conversion.
      newEntries := flatten for i from 0 to rows-1 list
                            for k from 0 to (sizeHash#(i,0))#0-1 list (
                               flatten for j from 0 to cols-1 list
                                       for l from 0 to (sizeHash#(0,j))#1-1 list blockEntries#i#j#k#l
                            );
      retVal = new NCMatrix from hashTable {(symbol ring, (ncEntries#0#0).ring), 
	                                    (symbol matrix, newEntries),
                                   	    (symbol cache, new CacheTable from {})};
   );
   assignDegrees retVal
)

--- adding new constructors for ncMatrix for maps to/from the zero module
ncMatrix(NCRing,List,List) := (A,tar,src) -> (
   --- this function defines the map to/from the zero module from/to a free module with the given degree vector
   if tar != {} and src != {} then error "This ncMatrix constructor is used to create maps to/from the zero module.";
   ncEntries := toList ((#tar):{});
   M := new NCMatrix from hashTable {(symbol ring, A),
                                         (symbol matrix, ncEntries),
                                         (symbol cache, new CacheTable from {})};
   M#(symbol source) = src;
   M#(symbol target) = tar;
   setIsHomogeneous M;
   M
)

ring NCMatrix := NCRing => M -> M.ring

lift NCMatrix := NCMatrix => opts -> M -> (
   M' := if length M.source == 0 or length M.target == 0 then
            ncMatrix(M.ring.ambient, M.target, M.source)
         else ncMatrix applyTable(M.matrix, entry -> promote(entry,(M.ring.ambient)));
   if isHomogeneous M then assignDegrees(M',M.target,M.source);
   M'
)

NCMatrix ? NCMatrix := (M,N) -> (flatten entries M) ? (flatten entries N)

NCMatrix * NCMatrix := (M,N) -> (
   if M.ring =!= N.ring then error "Expected matrices over the same ring.";
   B := M.ring;
   colsM := length M.source;
   rowsN := length N.target;
   if colsM != rowsN then error "Maps not composable.";
   rowsM := length M.target;
   colsN := length N.source;
   -- lift entries of matrices to tensor algebra
   local prod;
   if class B === NCQuotientRing then (
      MoverTens := lift M;
      NoverTens := lift N;
      prodOverTens := MoverTens*NoverTens;
      prod = if prodOverTens.source == {} or prodOverTens.target == {} then
                promote(prodOverTens,B)
             else (
         	  ncgb := B.ideal.cache#gb;
         	  reducedMatr := prodOverTens % ncgb;
	 	  flagReducedMatrix(reducedMatr);
         	  promote(reducedMatr,B)
		  );
--  No need to assignDegrees again. All operations preserve degrees if possible
--      if isHomogeneous M and isHomogeneous N then
--	    assignDegrees(prod,M.target,N.source);
      prod
   )
   
   else
   (
      if rowsM == 0 or colsN == 0 then (
--         if (M.source != N.target) then << "Warning: Graded composition undefined."<<endl; 
         ncMatrix(M.ring,M.target,N.source))
      else if colsM == 0 then (
         prod = ncMatrix table(rowsM,colsN, i-> promote(0,B));
	 if isHomogeneous M and isHomogeneous N then
            assignDegrees(prod,M.target,N.source);
	 prod
      )
      else ( 
         -- not sure which of the below is faster
         -- ncMatrix apply(toList (0..(rowsM-1)), i -> apply(toList (0..(colsN-1)), j -> sum(0..(colsM-1), k -> ((M.matrix)#i#k)*((N.matrix)#k#j))))
         prod = ncMatrix table(toList (0..(rowsM-1)), toList (0..(colsN-1)), (i,j) -> sum(0..(colsM-1), k -> ((M.matrix)#i#k)*((N.matrix)#k#j)));
         if isHomogeneous M and isHomogeneous N then (
	    if (M.source == N.target) then
               assignDegrees(prod,M.target,N.source);
--	    else << "Warning: Graded composition undefined." << endl;
	 ); 
         prod
      )
   )
)

NCMatrix * Matrix := (M,N) -> (
   N' := sub(N,coefficientRing M.ring);
   rkNs := rank N.source;
   rkNt := rank N.target;
   N'':= if (rkNs == 0) or (rkNt == 0) then ncMatrix(M.ring, toList(rkNt:0), toList(rkNs:0))
         else ncMatrix applyTable(entries N, e -> promote(e,M.ring));
   M*N''
)

Matrix * NCMatrix := (N,M) -> (
   N' := sub(N,coefficientRing M.ring);
   rkNs := rank N.source;
   rkNt := rank N.target;
   N'':= if (rkNs == 0) or (rkNt == 0) then ncMatrix(M.ring, toList(rkNt:0), toList(rkNs:0))
         else ncMatrix applyTable(entries N, e -> promote(e,M.ring));
   N''*M
)

NCMatrix % NCGroebnerBasis := (M,ncgb) -> (
   -- this function should be only one call to bergman
   -- the nf for a list is there already, just need to do entries, nf, then unpack.
   coeffRing := coefficientRing M.ring;
   colsM := #(first M.matrix);
   rowsM := #(M.matrix);
   entriesM := flatten M.matrix;
   maxDeg := max(entriesM / degree);
   maxSize := max(entriesM / size);
   -- this code does not yet handle zero entries correctly when sending them to the bergman interface.
   entriesMNF := if ((rowsM*colsM > MAXSIZE) or
                     (maxDeg > MAXDEG or maxSize > MAXSIZE)) or
                     (coeffRing === QQ and coeffRing === ZZ/(char coeffRing)) then 
   		    normalFormBergman(entriesM, ncgb)
                 else
                    apply(entriesM, f -> f % ncgb);
   redMat := ncMatrix pack(colsM,entriesMNF);
   if isHomogeneous M then assignDegrees(redMat,M.target,M.source);
   redMat
)

-- need to make this more intelligent(hah!) via repeated squaring and binary representations.
NCMatrix ^ ZZ := (M,n) -> product toList (n:M)

NCMatrix + NCMatrix := (M,N) -> (
   if M.ring =!= N.ring then error "Expected matrices over the same ring.";
   colsM := length M.source;
   rowsN := length N.target;
   rowsM := length M.target;
   colsN := length N.source;
   if colsM != colsN or rowsM != rowsN then error "Matrices not the same shape.";
   if any({colsM,rowsN,rowsM,colsN}, n-> n==0) then M
   else (
      MpN :=  ncMatrix apply(toList(0..(rowsM-1)), i -> apply(toList(0..(colsM-1)), j -> M.matrix#i#j + N.matrix#i#j));
      if isHomogeneous M and isHomogeneous N and M.target == N.target and M.source == N.source then
      assignDegrees(MpN,M.target,M.source);
      MpN
   )
)

NCMatrix - NCMatrix := (M,N) -> (
   if M.ring =!= N.ring then error "Expected matrices over the same ring.";
   colsM := length M.source;
   rowsN := length N.target;
   rowsM := length M.target;
   colsN := length N.source;
   if colsM != colsN or rowsM != rowsN then error "Matrices not the same shape.";
   if any({colsM,rowsN,rowsM,colsN}, n-> n==0) then M
   else (
      MmN :=  ncMatrix apply(toList(0..(rowsM-1)), i -> apply(toList(0..(colsM-1)), j -> M.matrix#i#j - N.matrix#i#j));
      if isHomogeneous M and isHomogeneous N and M.target == N.target and M.source == N.source then
      assignDegrees(MmN,M.target,M.source);
      MmN
   )
)

NCMatrix | NCMatrix := (M,N) -> (
   if M.ring =!= N.ring then error "Expected matrices over the same ring.";
   rowsN := length N.target;
   rowsM := length M.target;
   if rowsN != rowsM then error "Expected matrices with the same number of rows.";
   if rowsN == 0 then
   -- if both map to the zero module, the direct sum does as well
      ncMatrix(M.ring,M.target,M.source|N.source)
   else if M.source == {} then N
   else if N.source == {} then M
   -- if both map to a nonzero module and one is from a zero module, return the other
   else(
   -- otherwise, do the usual thing 
      pipe := ncMatrix apply(rowsN, i -> (M.matrix)#i | (N.matrix)#i);
      if isHomogeneous M and isHomogeneous N and M.target == N.target then
      assignDegrees(pipe,M.target, M.source | N.source);
      pipe
   )
)

NCMatrix || NCMatrix := (M,N) -> (
   if M.ring =!= N.ring then error "Expected matrices over the same ring.";
   colsN := length N.source;
   colsM := length M.source;
   if colsN != colsM then error "Expected matrices with the same number of columns.";
   if colsN == 0 then
   -- if both map from the zero module, the direct sum does as well
      ncMatrix(M.ring, M.target|N.target, M.source)
   else if M.target == {} then N 
   else if N.target == {} then M
   -- if both map from a nonzero module and one goes to a zero module, return the other
   else (
   --  otherwise do the usual thing
      pipe := ncMatrix (M.matrix | N.matrix);
      if isHomogeneous M and isHomogeneous N and M.source == N.source then
      assignDegrees(pipe,M.target | N.target, M.source);
      pipe
   )
)

NCMatrix * ZZ := (M,r) -> (
   if M.source == {} or M.target == {} then
      ncMatrix(M.ring, M.target, M.source)
   else (
      newM := ncMatrix apply(M.matrix, row -> apply(row, entry -> entry*sub(r,M.ring.CoefficientRing)));
      if isHomogeneous M then assignDegrees(newM,M.target,M.source);
      newM
   )
)
ZZ * NCMatrix := (r,M) -> M*r;
NCMatrix * QQ := (M,r) -> (
   if M.source == {} or M.target == {} then
      ncMatrix(M.ring, M.target, M.source)
   else (
      newM := ncMatrix apply(M.matrix, row -> apply(row, entry -> entry*sub(r,M.ring.CoefficientRing)));
      if isHomogeneous M then assignDegrees(newM,M.target,M.source);
      newM
   )
)
QQ * NCMatrix := (r,M) -> M*r
- NCMatrix := M -> (-1)*M;
NCMatrix * RingElement := (M,r) -> M*(promote(r,M.ring))
RingElement * NCMatrix := (r,M) -> (promote(r,M.ring)*M)

NCMatrix * NCRingElement := (M,r) -> (
   B := M.ring;
   s := promote(r,B);
   -- lift entries of matrices to tensor algebra
   Mr := if class B === NCQuotientRing then (
            MOverTens := lift M;
            sOverTens := lift s;
            prodOverTens := MOverTens*sOverTens;
            if M.source != {} and M.target != {} then (
               ncgb := B.ideal.cache#gb;
               reducedMatr := prodOverTens % ncgb;
	       flagReducedMatrix(reducedMatr);
               promote(reducedMatr,B)
            ) else promote(prodOverTens, B)
         )
         else if M.source == {} or M.target == {} then 
            ncMatrix(B,M.target,M.source)	 
         else
      	    ncMatrix applyTable(M.matrix, m -> m*s);
   --- now set the degrees properly
   
   if isHomogeneous M and isHomogeneous r then
      assignDegrees(Mr, M.target, apply(M.source, d -> d + degree r));
   Mr
)

NCRingElement * NCMatrix := (r,M) -> (
   B := M.ring;
   s := promote(r,B);
   -- lift entries of matrices to tensor algebra
   rM := if class B === NCQuotientRing then (
      MOverTens := lift M;
      sOverTens := lift s;
      prodOverTens := sOverTens*MOverTens;
      if M.source != {} and M.target != {} then (
         ncgb := B.ideal.cache#gb;
         reducedMatr := prodOverTens % ncgb;
         flagReducedMatrix(reducedMatr);
         promote(reducedMatr,B)
      ) else promote(prodOverTens, B)
   )
   else if M.source == {} or M.target == {} then
      ncMatrix(B,M.target,M.source)
   else
      ncMatrix applyTable(M.matrix, m -> s*m);
      
   if isHomogeneous M and isHomogeneous r then
      assignDegrees(rM, apply(M.target, d -> d - degree r), M.source);
   rM   
)

entries NCMatrix := M -> M.matrix
transpose NCMatrix := M -> (
    if M.source == {} or M.target == {} then 
       ncMatrix(M.ring,-M.source,-M.target)
    else ( 
       Mtrans := ncMatrix transpose M.matrix;
       assignDegrees(Mtrans,-M.source,-M.target);
       Mtrans)
)

--NCMatrix ** Matrix := 
--Matrix ** NCMatrix := 
-- what does this matrix represent?
NCMatrix ** NCMatrix := (M,N) -> (
   entriesM := entries M;
   MtensN := ncMatrix applyTable(entriesM, e -> e*N);
   --- now we must assignDegrees to make make them compatible
   --- with the maps M and N
   newSource := flatten apply(#(M.source), i ->
         apply(#(N.source), j -> ((M.source)#i)+((N.source)#j)));
   newTarget := flatten apply(#(M.target), i ->
         apply(#(N.target), j -> ((M.target)#i)+((N.target)#j)));
   assignDegrees(MtensN,newTarget,newSource)
)

NCMatrix ++ NCMatrix := (M,N) -> (
   B := ring M;
   Mtar := M.target;
   Msrc := M.source;
   Ntar := N.target;
   Nsrc := N.source;
   urZero := zeroMap(Mtar,Nsrc,B);
   lrZero := zeroMap(Ntar,Msrc,B);
   -- this is a hack until ncMatrix can take lists of empty matrices
   if Msrc == {} and Mtar == {} then N
   else if Msrc == {} and Nsrc == {} then ncMatrix(B,Mtar|Ntar,{})
   else if Msrc == {} and Ntar == {} then urZero
   else if Msrc == {} then ncMatrix{{urZero},{N}}
   else if Mtar == {} and Nsrc == {} then lrZero
   else if Mtar == {} and Ntar == {} then ncMatrix(B,{},Msrc|Nsrc)
   else if Mtar == {} then ncMatrix{{lrZero,N}}
   else if Nsrc == {} and Ntar == {} then M
   else if Nsrc == {} then ncMatrix{{M},{lrZero}}
   else if Ntar == {} then ncMatrix{{M,urZero}}
   else (
      ds := ncMatrix {{M,urZero},{lrZero,N}};
      assignDegrees(ds,M.target | N.target, M.source | N.source)
   )
)

--- flag an entire matrix as having reduced entries
--- internal routine
flagReducedMatrix = method()
flagReducedMatrix NCMatrix := M ->
   applyTable(M.matrix, entry -> flagReduced(entry))

--- for printing out the matrices; taken from the core M2 code for
--- usual matrix printouts (though simplified)
net NCMatrix := M -> net expression M
expression NCMatrix := M -> MatrixExpression applyTable(M.matrix, expression)

assignDegrees = method()
assignDegrees NCMatrix := M -> (
   rowsM := #(M.matrix);
   colsM := #(first M.matrix);
   if rowsM == 0 or colsM == 0 then error "Use assignDegrees(NCMatrix, List, List) to change degrees of maps to/from the zero module.";
   sourceDeg := toList (colsM:0);
   targetDeg := toList (rowsM:0);
   entriesM := select(flatten entries M, f -> f != 0);
   -- if the zero matrix or entries not homogeneous, then just assign zero degrees.
   if entriesM == {} or any(entriesM, f -> not isHomogeneous f) then
      return assignDegrees(M,targetDeg,sourceDeg);
   colDegs := apply(transpose M.matrix, col -> unique apply(select(col, f -> f != 0), f -> degree f));
   if not all(colDegs, coldeg -> #coldeg == 1) then
      return assignDegrees(M,targetDeg,sourceDeg);
   sourceDeg = apply(colDegs, coldeg -> if coldeg == {} then 0 else first coldeg);
   -- assign the degrees
   assignDegrees(M,targetDeg,sourceDeg);
   -- set the isHomogeneous flag
   setIsHomogeneous M;
   -- for matrices that are not homogeneous with these degrees, the user may use assignDegrees
   -- below.  An attempt to find a (nontrivial) set of degrees for which the map is homogeneous requires
   -- integer programming.  I may implement this in the future.
   M
)

assignDegrees (NCMatrix, List, List) := (M,targetDeg,sourceDeg) -> (
   -- this function is for manual assignment of degrees
   -- it's also called by the method trying to assign degrees automatically
   -- this means an input matrix may not have "source" and "target" keys
   -- but it will if it's a map to the zero module
   if (#(targetDeg) != #(entries M)) then error "Target degree list does not match number of rows of matrix";
   if #(entries M) !=0 and (#(sourceDeg) != #(first entries M)) then error "Source degree list does not match number of columns of matrix";   
   if #(entries M) == 0 and (#(sourceDeg) != #(M.source)) then error "Source degree list does not match number of columns of matrix";
   M#(symbol source) = sourceDeg;
   M#(symbol target) = targetDeg;
   -- set the isHomogeneous flag.
   setIsHomogeneous M;
   M
)

setIsHomogeneous = method()
setIsHomogeneous NCMatrix := M -> (
   targetDeg := M.target;
   sourceDeg := M.source;
   retVal := all(#(M.matrix), i -> all(#((M.matrix)#i), j -> (M.matrix)#i#j == 0 or sourceDeg#j - degree ((M.matrix)#i#j) == targetDeg#i));
   M#(symbol isHomogeneous) = retVal;
   retVal      
)

isHomogeneous NCMatrix := M -> M.?isHomogeneous and M.isHomogeneous

NCMatrix _ List := (M,ns) -> (
    if M.source == {} then error "There are no columns to select";
    if M.target == {} then
       ncMatrix(M.ring, {}, (M.source)_ns)
    else (
       M' := ncMatrix (transpose (transpose (M.matrix))_ns);
       assignDegrees(M',M.target, (M.source)_ns);
       M'
    )
)

NCMatrix ^ List := (M,ns) -> (
    if M.target == {} then error "There are no rows to select";
    if M.source == {} then
       ncMatrix(M.ring, (M.target)_ns, {})
    else (
       M' := ncMatrix ((M.matrix)_ns);
       assignDegrees(M',(M.target)_ns, M.source);
       M'
    )
    
)

NCMatrix // NCMatrix := (N,M) -> (
   -- The function factors a map through another.  However, if no such lift is possible,
   -- the function finds the 'closest' to a lift that it can by reducing the columns of N
   -- modulo a GB for the image of M.  The answer satisfies M - N * (M // N) is equal to the
   -- columns of N reduced modulo the image of M (similar to the // command for ordinary Matrices)
   if not (isHomogeneous M and isHomogeneous N) then
      error "Expected homogeneous maps.";
   if M.target != N.target then
      error "Expected maps with the same target.";
   -- handle trivial cases now:
   
   -------------------------------
   gbDegree := max N.source + 1;   -- what is the right degree here?
   matrixRelsM := buildMatrixRelations M;
   CM := ring first matrixRelsM;
   matrixRelsN := buildMatrixRelations N;
   CN := ring first matrixRelsN;
   matrixGBM := getMatrixGB(M, DegreeLimit => gbDegree, MakeMonic => true);
   B := ring M;
   colsN := #(N.source);
   rowsN := #(N.target);
   colsM := #(M.source);
   rowsM := #(M.target);
   phi := ncMap(CM,CN,take(gens CM, numgens B) | toList(colsN:promote(0,CM)) | take(gens CM, -rowsM));
   colVarsCM := (gens CM)_{(numgens B)..(numgens B+colsM-1)};
   reducedN := apply(N.cache#"BergmanRelations" / phi, f -> f % matrixGBM);
   factorMap := getModuleCoefficients(reducedN,colVarsCM);
   assignDegrees(factorMap, M.source, N.source);
   factorMap
)

getMatrixGB = method(Options => {DegreeLimit => 10, MakeMonic => false})
getMatrixGB NCMatrix := opts -> M -> (
   if M.cache#?"matrixGB" and M.cache#"gbDegree" >= opts#DegreeLimit then return M.cache#"matrixGB";
   
   matrRels := buildMatrixRelations M;
   C := ring first matrRels;
   B := ring M;
   BtoC := ncMap (C, B, take(gens C, numgens B));
   ambBtoC := if class B === NCQuotientRing then ambient BtoC else BtoC;
   mGB := twoSidedNCGroebnerBasisBergman(matrRels | ((gens ideal B) / ambBtoC),
                                         NumModuleVars => numgens C - numgens B,
                                         DegreeLimit => opts#DegreeLimit,
                                         MakeMonic=>opts#MakeMonic,
                                         CacheBergmanGB=>false);
   M.cache#"matrixGB" = mGB;
   M.cache#"gbDegree" = opts#DegreeLimit;
   mGB
)

NCMatrix == NCMatrix := (M,N) -> (
    if isHomogeneous M and isHomogeneous N then
       (M.source == N.source) and (M.target == N.target) and (M.matrix) == (N.matrix)
    else
       (M.matrix) == (N.matrix)
)

NCMatrix == ZZ := (M,n) -> (
   if n != 0 then error "Expected comparison to zero.";
   all(flatten entries M, f -> f == 0)
)

ZZ == NCMatrix := (n,M) -> M == n

--- graded shift
NCMatrix Array := (M,n) -> (
    if #n != 1 then return "Error: Please enter a single integer" else
    M**(assignDegrees(ncMatrix {{promote(1,M.ring)}},{-1*n#0},{-1*n#0}))
    )

------------------------
--- NCChainComplex code
------------------------

-------------------------------------------
--- NCChainComplex Methods ----------------
-------------------------------------------
NCChainComplex = new Type of HashTable

resolution NCMatrix := opts -> M -> (
   i := 0;
   numSyz := if opts#LengthLimit === infinity then numgens ring M - 1 else opts#LengthLimit;
   currentM := M;
   syzList := {M} | while (i < numSyz and currentM != 0) list (
      newM := rightKernelBergman currentM;
      currentM = newM;
      currentM
   ) do i = i+1;
   new NCChainComplex from apply(#syzList, i -> (i,syzList#i))
)

betti NCChainComplex := opts -> C -> (
    len := #C;
    firstbettis := flatten apply(
    	keys (tally (C#0).target), 
    	i -> {(0,{i},i) => (tally (C#0).target)_i}
    );
    if C#(len-1) == 0 then len = len - 1;
    lastbettis := flatten flatten apply(len, j -> 
	apply(
    	    keys (tally (C#j).source), 
    	    i -> {(j+1,{i},i) => (tally (C#j).source)_i}
	    )
	);
    L := firstbettis | lastbettis;
    B := new BettiTally from L
)

spots = C -> select(keys C, i -> class i === ZZ and C#i != 0)

net NCChainComplex := C -> (
   s := sort spots C;
   if # s === 0 then "0"
   else (
      a := s#0;
      b := s#-1;
      A := ring C#a;
      mostOfThem := horizontalJoin between(" <-- ", apply(a .. b, i -> stack ((net A) | (net (#(C#i.target)))^1," ",net i)));
      mostOfThem | " <-- " |  stack ((net A) | (net (#(C#b.source)))^1," ",net (b+1))
   )
)

---------------
--- Hom code
--------------

identityMap = method()
identityMap (List, NCRing) := (L,R) -> (
   n := #L;
   B := coefficientRing R;
   I := ncMatrix applyTable(entries id_(B^n), e -> promote(e,R));
   assignDegrees(I,L,L)
)

identityMap (ZZ,NCRing) := (n,R) -> identityMap(toList(n:0),R)

zeroMap = method()
zeroMap (List, List, NCRing) := (tar,src,B) -> (
   R := coefficientRing B;
   if tar == {} or src == {} then
      ncMatrix(B,tar,src)
   else (
      myZero := ncMatrix applyTable(entries map(R^#tar,R^#src,0), e -> promote(e,B));
      assignDegrees(myZero,tar,src);
      myZero
   )
)

NCMatrix _ ZZ := (M,d) -> (
   entryTable := apply(#(M.target), i -> apply(#(M.source), j -> (i,j)));
   multTable := applyTable(entryTable, e -> leftMultiplicationMap(M#(e#0)#(e#1), 
	                                                d - (M.source)#(e#1),
                                                        d - (M.target)#(e#0)));
   matrix multTable
)

Hom (ZZ,NCMatrix,NCMatrix) := (d,M,N) -> (
   -- This method uses Boehm's Algorithm 6.5.1 from "Computer Algebra: Lecture Notes" 
   -- http://www.mathematik.uni-kl.de/~boehm/lehre/1213_CA/ca.pdf
   --
   -- That algorithm applies to modules over a commutative ring R, 
   -- taking advantage of the fact that Hom(M,N) is an R-module in that case.
   -- In our case, we must work on the level of graded vector spaces, as 
   -- Hom(M,N) need not be an R-module if R is noncommutative
   --
   -- The setup here:
   --
   -- M and N are presentation matrices for a pair of graded modules over an NCRing 
   -- We compute graded homomorphisms only; a degree d homomorphism M --> N is the
   -- same as a degree 0 homomorphism M --> N[d]. The method returns a basis for 
   -- the space of degree d homomorphisms. Each map is represented by a matrix.
   -- The matrix is a lift of the homomorphism to a map between targets of presentation
   -- matrices. That is, if B is the NCRing, a homomorphism from coker M to coker N is
   -- a pair of maps completing the commutative diagram below. We return f as a matrix.
   -- 
   -- coker M <--- B^{s0} <-- M --- B^{s1}
   --    |          |                 |
   --    |          f                 g
   --    |          |                 |
   --	 v	    v                 v
   -- coker N <--- B^{t0} <-- N --- B^{t1} 
   --
   -- The key to implementing Boehm's algorithm is the identification
   -- Hom(B^n, B^m) = B^m \tensor (B^n)*
   -- This identification is valid in the category of locally finite graded modules.
   B := ring M;
   -- it might be cleaner to shift N and set d=0

   -- Step 1: We need the first syzygy module of N to deal with homotopy.
   --
   -- coker M <--- B^{s0} <-- M --- B^{s1}
   --    |          |                 |
   --    |          f                 g
   --    |          |                 |
   --	 v	    v                 v
   -- coker N <--- B^{t0} <-- N --- B^{t1} <---- Nsyz ---- B^{t2}
   
   Nsyz := rightKernelBergman N; 
   if Nsyz == ncMatrix{{promote(0,B)}} then Nsyz = zeroMap(N.source,{0},B);

   -- Step 2: Compute the map "\delta"
   --      Hom(B^{s0},B^{t0}) ++ Hom(B^{s1},B^{t1}) ----> Hom(B^{s1},B^{t0}) 
   --               ( f, g ) |--->  f*M - N*g
   -- since every homomorphism belongs to the kernel of this map. 
   --
   -- After the identifications mentioned above, the map is given by
   --      id \tensor M* - N \tensor id*

   L1 := identityMap(N.target,B); -- identity on B^t0
   L2 := identityMap(M.source,B); -- identity on B^s1
   K1 := L1 ** (transpose M);    
   K2 := N ** (transpose L2);
   
   -- Step 3: Compute the kernel of the map \delta in the prescribed degree
   --
   -- See below.
   --
   -- Step 4: Compute the map "\rho"
   --      Hom(B^{s0},B^{t1}) ++ Hom(B^{s1},B^{t2}) ----> Hom(B^{s0},B^{t0}) ++ Hom(B^{s1},B^{t1}) 
   --                ( h, k ) |--->  ( N*h, h*M - Nsyz*k )
   --
   -- We will ultimately return the quotient ker(\delta) / im(\rho)

   L3 := identityMap(M.target,B); -- identity on B^s0
   L4 := identityMap(N.source,B); -- identity on B^t1
   K3 := N ** (transpose L3);
   K4 := L4 ** (transpose M);
   K5 := Nsyz ** (transpose L2);
   myZeroMap := zeroMap(K3.target,K5.source,B); -- this is a zero block of the appropriate size

   -- Computing Hom in degree d
   --
   -- To do this at the vector space level, we replace every entry in the matrices constructed
   -- above with a matrix representing right or left multiplication, as required by the formulas.
   -- Some care must be taken with degree shifts in the source and target. In particular, some
   -- columns will simply vanish for degree reasons. We must keep track of these, at least for K1.
   
   K1ent := entries K1;
   K2ent := entries K2;
   K3ent := entries K3;
   K4ent := entries K4;
   K5ent := entries K5;
   myZeroMapEnt := entries myZeroMap;
   
   -- The following tracks the source degrees of K1 where the conversion returns the empty matrix.
   -- At the end, we'll go back and insert zeros if needed.
   
   sourceBasisSize := apply(#(K1.source),i-> # flatten entries basis(d-(K1.source)#i,B));
   empties := positions(sourceBasisSize, i-> i==0);

   -- Converting all matrices as described above.
   
   K1' := matrix apply(#(K1.target), i -> apply(#(K1.source), j -> 
	rightMultiplicationMap(K1ent#i#j, d - (K1.source)#j, d - (K1.target)#i)));
   K2' := matrix apply(#(K2.target), i -> apply(#(K2.source), j -> 
	leftMultiplicationMap(-K2ent#i#j, d - (K2.source)#j, d - (K2.target)#i)));
   K3' := matrix apply(#(K3.target), i -> apply(#(K3.source), j -> 
	leftMultiplicationMap(K3ent#i#j, d - (K3.source)#j, d - (K3.target)#i)));
   K4' := matrix apply(#(K4.target), i -> apply(#(K4.source), j -> 
	rightMultiplicationMap(K4ent#i#j, d - (K4.source)#j, d - (K4.target)#i)));
   K5' := matrix apply(#(K5.target), i -> apply(#(K5.source), j -> 
	leftMultiplicationMap(-K5ent#i#j, d - (K5.source)#j, d - (K5.target)#i)));
   myZeroMap' := matrix apply(#(myZeroMap.target), i -> apply(#(myZeroMap.source), j -> 
        leftMultiplicationMap(-myZeroMapEnt#i#j, d - (myZeroMap.source)#j, d - (myZeroMap.target)#i)));
   
   -- The following are the matrix representations of the degree d part of \delta and \rho respectively.
      
   K' := K1'|K2';
   H' := matrix {{K3',myZeroMap'},{K4',K5'}};

   -- Compute the subquotient. 
   
   myHom := prune ((ker K') / (image H')); 

   -- Now reformat to output the map f. 
   -- First minimize the generators and trim down so only f is returned. 
   homGens := mingens image(gens image myHom.cache.pruningMap)^(toList(0..(numgens source K1' - 1)));
   
   -- Convert back to NCMatrices (flattened into a vector at this point)
   basisMatr := fold(apply(#(K1.source), i -> basis(d-(K1.source)#i,B)), (a,b) -> a ++ b);
   flattenedMatrs := basisMatr * homGens;
   
   -- Finally, insert zeros where needed.
   toMingleZeros := zeroMap((K1.source)_empties,flattenedMatrs.source,B);
   if flatten entries flattenedMatrs == {} then {}
   else (
      mingledFlatMatrs := ncMatrix functionMingle(entries flattenedMatrs,entries toMingleZeros,i->sourceBasisSize#i!=0); 
      retVal := apply(apply(#(mingledFlatMatrs.source), i -> flatten entries mingledFlatMatrs_{i}), L -> ncMatrix pack(#(M.target),L));
      retVal
   )
)

-------------------------------------------------------------
------- end package code ------------------------------------

-------------------- timing code ---------------------------
wallTime = Command (() -> value get "!date +%s.%N")  --- %N doesn't work on Mac
wallTiming = f -> (
    a := wallTime(); 
    r := f(); 
    b := wallTime();  
    << "wall time : " << b-a << " seconds" << endl;
    r);
------------------------------------------------------------

--- include the documentation
load "./NCAlgebra/NCAlgebraDoc.m2"

end

---- installing and loading the package
restart
uninstallPackage "NCAlgebra"
installPackage "NCAlgebra"
needsPackage "NCAlgebra"
viewHelp "NCAlgebra"

---- checking the package

loadPackage "UnitTestsNCA"
check UnitTestsNCA


