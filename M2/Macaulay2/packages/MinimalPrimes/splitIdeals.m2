-----------------------------------------
--- Begin new nested strategy code
-----------------------------------------

-- format for strategy:
-- a strategy is one of the following:
--  1. Symbol (allowed: Linear, Factorization, ...)
--  2. (strategy, #times)
--  3. list of strategies
-- If no #times is given (e.g. in (1) or (3), then 1 is assumed)

strategySet = strat -> (
    if instance(strat, Symbol) then set {strat}
    else if instance(strat, List) then sum(strat/strategySet)
    else if instance(strat, Sequence) then strategySet first strat
    )

isStrategyDone = method()
isStrategyDone (List,Symbol) := (L,strat) ->
  all(L, I -> I#?strat or (I.?isPrime and I.isPrime === "YES"))

separateDone = (L, strats) -> (
    -- L is a list of annotated ideals
    H := partition(f -> all(strats, s -> isStrategyDone({f}, s)), L);
    (H1,H2) := (if H#?true then H#true else {}, if H#?false then H#false else {});
    indexList := sort(apply(#H2, i -> (codimLowerBound H2#i,i))) / last;
    (H1,H2_indexList)
    )

separatePrime = L -> (
    -- L is a list of annotated ideals
    -- returns (L1,L2), where L1 is the list of elements of L which are known to be prime
    -- and L2 are the rest
    H := partition(I -> (I.?isPrime and I.isPrime === "YES"), L);
    (if H#?true then H#true else {}, if H#?false then H#false else {})
    )

------------------------------------------------------------
-- 'Splitting' commands follow.
-- Not all are genuinely 'splitting' in that sometimes,
-- an operation is performed on the ideal without any
-- splitting occurring.
------------------------------------------------------------

-- splitIdeal code: ???
splitIdeal = method(
    Options => {
	Strategy               => BirationalStrat,
	Verbosity              => 0,
	"PDState"              => null,
	CodimensionLimit       => null,
	"SquarefreeFactorSize" => 1,
	"CheckPrimeOnly"       => false}
    )
splitIdeal AnnotatedIdeal := opts -> I -> splitIdeals({I}, opts.Strategy, opts)
splitIdeal Ideal          := opts -> I -> (
    opts = opts ++ {"PDState" => createPDState I};
    splitIdeals({annotatedIdeal(I, {}, {}, {})}, opts.Strategy, opts))

-- each of the splitIdeals routines:
--  takes a list of annotated ideals, and returns a similar list
splitIdeals = method(Options => options splitIdeal)
splitIdeals(List, Symbol) := opts -> (L, strat) -> (
    -- L is a list of annotated ideals
    -- process each using strategy 'strat'.
    -- return (L1, L2), where L1 consists of the ideals
    --   that are either prime, or are done using this method
    --   (i.e. running it through this strategy again would have no effect).
    -- and L2 are ideals which may or may not be done, but we don't know that yet.
    if not member(strat,{
            Linear,
            Birational,
            Factorization,
            IndependentSet,
            SplitTower,
            CharacteristicSets,
            DecomposeMonomials,
            Trim
            }) then
          error ("Unknown strategy " | toString strat | " given.");
    pdState := opts#"PDState";
    flatten for f in L list (
        if opts.Verbosity >= 2 then (
            << "  Strategy: " << pad(toString strat,18) << flush;
            );
        -- check to see if this ideal is redundant before performing the splitting computation
        if isRedundantIdeal(f,pdState) then (
           if opts.Verbosity >= 2 then << " ** Redundant Ideal found and skipped." << endl;
           continue;
        );
        tim := timing splitFunction#strat(f, opts);
        ans := tim#1;
        numOrig := #ans;
        if opts.CodimensionLimit =!= null then 
            ans = select(ans, i -> codimLowerBound i <= opts.CodimensionLimit);
        (primes,others) := separatePrime(ans);
        updatePDState(pdState,primes,numOrig - #ans);
        if opts.Verbosity >= 2 then << pad("(time " | toString (tim#0) | ") ", 16);
        if opts.Verbosity >= 2 then (
            << " #primes = " << numPrimesInPDState(pdState);
            << " #prunedViaCodim = " << pdState#"PrunedViaCodim" << endl;
            );
        others
        )
    )

splitIdeals(List, Sequence) := opts -> (L, strat) -> (
    (strategy, n) := strat;
    strategies := toList strategySet strat;
    (L1,L2) := separateDone(L, strategies);
    while n > 0 and #L2 != 0 do (
        --<< endl << L2 / codimLowerBound << endl;
        M := splitIdeals(L2, strategy, opts);
        (M1,M2) := separateDone(M, strategies);
        L1 = join(L1, M1);
        L2 = M2;
        n = n-1;
        );
    join(L1,L2)
    )

splitIdeals(List, List) := opts -> (L, strat) -> (
    strategies := toList strategySet strat;
    (L1,L2) := separateDone(L, strategies);
    for s from 0 to #strat-1 do (
         L2 = splitIdeals(L2, strat#s, opts);
         (M1,M2) := separateDone(L2, strategies);
         L1 = join(L1, M1);
         L2 = M2;
         );
    join(L1,L2)
    )

-- doSplitIdeal code
doSplitIdeal = method(Options => (options splitIdeals))
doSplitIdeal(Ideal) := opts -> I -> (
    if opts.CodimensionLimit === null
    then opts = opts ++ {CodimensionLimit => numgens I};
    pdState := createPDState(I);
    opts = opts ++ {"PDState" => pdState};
    M := splitIdeals({annotatedIdeal(I,{},{},{})}, opts.Strategy, opts);
    numRawPrimes := numPrimesInPDState pdState;
    --{pdState, M}
    {getPrimesInPDState pdState, M})

-- possible Strategy values:
--  Linear               -- Eliminates variables where a generator is of the form x - g
                         -- for g not containing x
--  Birational           -- Tries to eliminates variables where a generator is of
                         -- the form g*x - h for g,h not containing x.
                         -- If g is a nzd mod I, this eliminates x.  Else,
                         -- if g is in the radical of I, add in g to I and return
                         -- else, split with g as: (sat(I,g), (I:sat(I,g)))
--  IndependentSet       -- Find an independent set (annotate this), find a flattener,
                         -- and split using flattener
--  SplitTower           -- For an ideal which has LexGBSplit set to true, this splits the
                         -- ideal into prime (annotated) ideals
--  Trim                 -- simply trim I.ideal
--  DecomposeMonomials   -- Finds monomials that appear as generators, and uses monomial
                         -- ideal decomposition on those
--  Factorization        -- Finds a generator that factors, and splits the ideal based on
                         -- these factors.  ***No GB computed using this strategy***
--  SquarefreeGenerators -- appears currently in Factorization. (not individually
                         -- implemented yet)
--  GB                   -- replaces I.ideal with ideal gens gb I.ideal (not yet implemented)
--  CharacteristicSets   -- Use original decompose command (not yet implemented)
--  UseColon             -- Commented out in factorization, but logic is faulty.  Not yet
                         -- implemented, but can we get the logic to work properly?
--  WangSplitTower       -- Worth doing?

splitFunction = new MutableHashTable
-- each function should like like this:
-- splitFunction#MyStrategy = (I, opts) -> ...
    -- I is an AnnotatedIdeal
    -- opts is from options of splitIdeal
    -- return value is tuple (I1s, I2s), where
    --   I1s is a list of AnnotatedIdeal's, known to be prime
    --   I2s is a list of AnnotatedIdeal's, primality unknown

splitFunction#Trim = (I, opts) -> if I.?Trim then {I} else {trim I}

splitFunction#Linear = (I, opts) -> (
    -- no redundancy issues (in fact, ideals are not split at all...)
    if I.?Linear then return {I};
    J := I.Ideal;
    linears := for x in gens ring J list (
        -- find a position k such that x appears linearly, with constant coeff in J_k
        k := position(J_*, f -> f != 0 and degree(x,f) == 1 and support contract(x,f) === {});
        if k === null then continue;
        m := makeLinearElement(x, J_k);
        J = replaceVariable(J,m);
        m);
    newJ := if #linears === 0 then (
              I.Linear = true;
              I
            )
            else
              annotatedIdeal(J, join(I.Linears, linears), I.NonzeroDivisors, I.Inverted);
    {newJ}
    )

------------------------------------------------
---- Birational helper functions
------------------------------------------------
removeNull = (L) -> select(L, x -> x =!= null)
makeLinearElement = (x,f) -> (
    -- x is a variable
    -- f is a polynomial
    -- returns null if f is not linear as a polynomial in x
    -- otherwise returns
    -- (x, g, f1),
    --    where f1 = xg-h, 
    --   g and h do not involve x, and g is monic
    g := contract(x,f);
    if g == 0 then return null;
    if contract(x,g) != 0 then return null;
    c := leadCoefficient g;
    f = 1/c * f;
    g = 1/c * g;
    --h := x*g-f;
    (x, g, f)
    )
replaceVariable = (I, m) -> (
    -- reduce by x-p, p doesn't involve x, but x might not be the lead term of x-p.
    if leadTerm m#2 === m#0
    then ideal compress ((gens I) % m#2)
    else ideal compress sub(gens I, m#0 => m#0 * m#1 - m#2)
    )
eliminateLinear = (I, m) -> (
    -- I is an ideal
    -- m is a list as returned by makeLinearElement
    -- returns the ideal with I eliminated
    if m#1 == 1 
    then replaceVariable(I,m)
    else eliminate(I, m#0)
    )
linears = (x,I) -> I_* / (F -> makeLinearElement(x,F)) // removeNull
findBirationalPoly = (x,I) -> (
    M := linears(x,I);
    if #M === 0 then null
    else (
        M1 := sort apply(M, m -> (size m#1, first degree m#1, m));
        last first M1
        )
    )
findGoodBirationalPoly = (I) -> (
    -- given an ideal I, returns either null or a tuple (x, g, f)
    -- (see makeLinearElement for a description of these items)
    M := removeNull for x in gens ring I list (
        findBirationalPoly(x,I)
        );
    M = sort apply(M, m -> (size m#1, first degree m#1, m));
    if #M === 0 then null else last first M
    )
splitBy = (I, h) -> (
     -- I is an ideal in a poly ring
     -- h is an element in the same ring
     -- computes (I1,I2), where I1 = saturate(I,h), I2=I:I1
     -- except it returns null in some cases:
     --   h == 1, or
     --   I1 == I (in which case, h is a NZD mod I)
     if h == 1 then return null;
     --time Isat := saturate(I, h);
     Isat := mySat(I, h);
     if Isat == I then return null; -- in this case, h is a NZD mod I
     I2 := I : Isat;  -- this line is a killer (sometimes) but doesn't generate redundant components.
     --I2 := (ideal h) + I;  -- this will create (possibly) redundant components.
     (Isat, I2)
     )

splitFunction#Birational = (I, opts) -> (
      if I.?Birational then return {I};
      if I.Ideal == 1 then return {};
      pdState := opts#"PDState";
      m := findGoodBirationalPoly I.Ideal;
        -- either null or a list {x, g, f=xg-h}, with f in ideal
      if m === null then (
          I.Birational = true;
          return {I};
          );
      -- depending on whether splitBy uses colons or adds in generators,
      -- the following line will/will not possibly create redundant components.
      splitt := if member(m#1, I.NonzeroDivisors) then null else splitBy(I.Ideal,m#1);
      if splitt === null then (
          -- in this case, m#1 is a nonzerodivisor
          -- in this case, I may still be prime.
          -- we eliminate m#0
          J := eliminateLinear(I.Ideal, m);
          newI := annotatedIdeal(J,
                                 append(I.Linears, m),
                                 unique append(I.NonzeroDivisors, m#1),
                                 I.Inverted);
          -- if we wanted to, we could also place newI onto the "prime" list
          -- if newI.Ideal is generated by one irreducible element
          return {newI};
          );
      (J1,J2) := splitt;  -- two ideals.  The first has m#1 as a non-zero divisor.
      if J1 == 1 then (
          -- i.e. m#1 is in the radical of I.Ideal
          -- I is not prime if m#1 is not in I.
          -- since m#1 is in the radical, even though this looks like
          -- redundant components are possible, that is not the case here.
          g := m#1//factors/last//product; -- squarefree part of m#1
          if g == 1 then error "also a bad error";
          --if (g % I) != 0 then flagPrimality(pdState, false);
          newI = ideal compress((gens I.Ideal) % g) + ideal g;
          newI = annotatedIdeal(newI, I.Linears, I.NonzeroDivisors, I.Inverted);
          return {newI};
          );

      -- if we make it this far, this is an irredundant split.  Therefore, the original
      -- ideal is not prime.
      --flagPrimality(pdState,false);
      {annotatedIdeal(J1, I.Linears, unique append(I.NonzeroDivisors, m#1), I.Inverted),
       annotatedIdeal(J2, I.Linears, I.NonzeroDivisors, I.Inverted)}
    )

---------------------------------------
-- Factorization helper functions
---------------------------------------
detectMembership = method()
detectMembership (RingElement, Ideal) := (f,I) -> (
   --- The point of this function is to determine if f is in I without
   --- forcing a gb of I to be computed.  If one exists, it is found in the
   --- cache of I with getGB.  If not, then a degree check is made.
   --- If after these checks, one can't determine if f is in I or not,
   --- then null is returned.
   if f == 0 then return true;
   if I == 0 then return false;
   gbI := getGB I;
   if gbI =!= null then return f % I == 0;
   if not I.cache#?detectMembership then (
      canCheck := isHomogeneous I and numgens degreesRing ring I == 1;
      lowestDegree := if canCheck then first min degrees source gens I;
      I.cache#detectMembership = lowestDegree;
   );
   lowestDeg := I.cache#detectMembership;
   if lowestDeg =!= null and isHomogeneous f and first degree f < lowestDeg then
      false
   else
      null   
)

-*
-- Old version - 5/28/2013
-- this function is used in the #Factorization splitting option, followed by a trim.
-- TODO: rethink how the trim is done when variables are added?
squarefreeGenerators = method(Options=>{"SquarefreeFactorSize"=>1})
squarefreeGenerators AnnotatedIdeal := opts -> I -> (
   if I.?Squarefree then return I; 
   nonzeros := set I.Inverted;
   J := I.Ideal;
   n := opts#"SquarefreeFactorSize";
   madeChanges := false;
   J1 := ideal for g in J_* list (
              if size g > n then g
              else (
                nonzeroFacs := set ((factors g) / last) - nonzeros;
                h := product toList nonzeroFacs;
                if g != h then madeChanges = true;
                h
              )
         );
   if madeChanges then
      -- note that the NonzeroDivisor list is empty below since elements
      -- can become zerodivisors when removing powers of generators
      annotatedIdeal(J1,I.Linears,{},unique join(I.NonzeroDivisors,I.Inverted))
   else 
      I
)
*-
-- new version with trim moved to a smaller ideal than the whole input.
squarefreeGenerators = method(Options=>{"SquarefreeFactorSize"=>1})
squarefreeGenerators AnnotatedIdeal := opts -> I -> (
   if I.?Squarefree then return I; 
   nonzeros := set I.Inverted;
   J := I.Ideal;
   n := opts#"SquarefreeFactorSize";
   madeChanges := false;
   L := for g in J_* list (
           if size g > n then (g,false)
           else (
             nonzeroFacs := set ((factors g) / last) - nonzeros;
             h := (1_(ring I))*(product toList nonzeroFacs);
             if g != h then madeChanges = true;
             (h,g != h)
           )
        );
   if madeChanges then
   (
      -- note that the NonzeroDivisor list is empty below since elements
      -- can become zerodivisors when removing powers of generators
      J1 := ideal (L / first);
      J2 := trim ideal (select(L, p -> p#1) / first);  -- note that this trim involves only
                                                       -- 'small' factors
      J1 = ideal compress ((gens J1) % J2);
      J1 = if J1 == 0 then J2 else J1 + J2;
      annotatedIdeal(J1,I.Linears,{},unique join(I.NonzeroDivisors,I.Inverted))
   )
   else 
      I
)

findElementThatFactors = method()
findElementThatFactors List := L -> (
    -- sort L by number of terms first?
    for f in L do (
      -- don't try to factor a large polynomial?
      facs := factors f;
      if #facs > 1 or (#facs == 1 and facs#0#0 > 1) then return (f,facs/last);
    );
    (null, {})
    )

splitFunction#Factorization = (I,opts) -> (
    if I.?Factorization then return {I};
    J := I.Ideal;
    --- originally taken from facGB0 in PD.m2 -- 12/18/2012
    (f, facs) := findElementThatFactors J_*; -- chooses a generator of I that factors
    if #facs == 0 then (
        --<< "no elements found that factor" << endl; << "ideal is " << toString I << endl;
        I.Factorization = true;
        return {I};
    );
    nonzeros := set I.Inverted;
    prev := set{};
    nonzeroFacs := toList(set facs - nonzeros);
    if #nonzeroFacs == 1 and nonzeroFacs#0 != f then
       return {adjoinElement(I,nonzeroFacs#0)};
       -*return {annotatedIdeal(trim(ideal nonzeroFacs#0 + J),
                              I.Linears,
                              I.NonzeroDivisors,
                              I.Inverted)};*-
    L := for g in nonzeroFacs list (
          -- colon or sum?
          -- Try and fix UseColon?  May not be fixable...
          -*if opts#"UseColon" then (
          --   -- TODO: Find the components that are missing when using colons!
          --   --       This process will miss any component for which g is in I for all g.
          --   J = I:(f // g);
          *-
          -*
          J = (ideal(g) + I.Ideal);
          J = trim ideal apply(J_*, f -> (
                product toList (set ((factors f)/last) - nonzeros)
              ));
          *-
          J = adjoinElement(I,g);
          J = squarefreeGenerators(J,  -- there used to be a trim here, but we changed
                                       -- the function so that it was no longer needed.
                     "SquarefreeFactorSize" => opts#"SquarefreeFactorSize");
          J.Inverted = toList (set(J.Inverted) + prev);
          prev = prev + set{g};
          if numgens J.Ideal === 1 and J.Ideal_0 == 1 then continue else J
    );
    L
)

---------------------------------------
-- IndependentSet helper functions
---------------------------------------
--- this function is used in splitFunction#IndependentSet to make the
--- generators of the ideal over the fraction field ready for #SplitTower
splitLexGB = method(Options => options splitIdeal)
splitLexGB AnnotatedIdeal := opts -> I -> (
    if not I.?IndependentSet then return {I};
    if I.?LexGBSplit then return {I};
    IF := ideal I.LexGBOverBase;
    L := IF_*;
    for f in L do (
        facs := factors f;
        if #facs == 1 and facs#0#0 == 1 then continue;
        return flatten for fac in facs list (
               J := ideal gens gb ((ideal fac#1) + IF);
               Jann := new AnnotatedIdeal from I;
               Jann.LexGBOverBase = J_*;
               splitLexGB Jann
            )
        );
    -- At this point, all generators of IF_* are irreducible over the base field
    I.isPrime = if #select(L, f -> sum first exponents leadTerm f > 1) <= 1 then
       "YES"
    else
       "UNKNOWN";
    I.LexGBSplit = true;
    {I}
    )

minimalizeOverFrac = method()
minimalizeOverFrac(Ideal, Ring) := (I, SF) -> (
     -- Input:  I is an ideal in a ring with an elimination order (maybe Lex)
     --         SF is of the form k(basevars)[fibervars].
     --         If G is a GB of I, then G SF is a GB if I S.
     -- Output: A reduced minimal Groebner basis of I SF, as a list
     --         of polynomials (defined over SF).
     -- Caveat: ring I must have either a lex order or a product order, compatible with
     --  fibervars >> basevars, and must have been created with makeFiberRings
     S := ring I;
     G := flatten entries gens gb I;
     phi := S.cache#"StoSF";
     psi := S.cache#"SFtoS";
     sz := G/size; -- number of monomials per poly, used to choose which elem to take
     GS := flatten entries phi gens gb I;
     minG := flatten entries mingens ideal(GS/leadMonomial);
     GF := for mon in minG list (
        z := positions(GS, f -> leadMonomial f == mon);
        i := minPosition (sz_z);
        GS_(z#i)
     );
     -- QUESTION : Do we really wany 'numerator' here instead of psi?
     coeffs := GF/leadCoefficient/psi;
     (flatten entries gens forceGB matrix(SF,{GF}), coeffs)
     )

splitFunction#IndependentSet = (I,opts) -> (
    -- what do we need to stash in the answer from independentSets?
    -- does this really belong in the annotated ideal framework?
    -- create two annotated ideals:
    if isPrime I === "YES" then return {I};
    if I.?IndependentSet then return {I};
    J := I.Ideal;
    if J == 1 then error "Internal error: Input should not be unit ideal.";
    R := ring J;
    hf := if isHomogeneous J then poincare J else null;
    indeps := independentSets(J, Limit=>1);
    basevars := support first indeps;
    if opts.Verbosity >= 3 then
        << "  Choosing: " << basevars << endl;
    (S, SF) := makeFiberRings(basevars,R);
    JS := S.cache#"RtoS" J;
    -- if basevars is empty, then return I, but put in the lex ring.
    -- return value not correct form yet
    if #basevars == 0 then (
        I.IndependentSet = ({},S,SF);
        I.LexGBOverBase = (ideal gens gb JS)_*;
        return splitLexGB I;
    );
    -- otherwise compute over the fraction field.
    if hf =!= null and degreesRing ring JS === ring hf then gb(JS, Hilbert=>hf) else gb JS;
    --gens gb IS;
    (JSF, coeffs) := minimalizeOverFrac(JS, SF);
    if coeffs == {} then (
        I.IndependentSet = (basevars,S,SF);
        I.LexGBOverBase = JSF;
        splitLexGB I
    )
    else (
       facs := (factors product coeffs)/last;
       G := product facs;
       if opts.Verbosity >= 3 then
           << "  the factors of the flattener: " << netList(facs) << endl;
       G = S.cache#"StoR" G;
       J1 := mySat(J, G);
       --J1 := saturate(J, G);
       J1ann := annotatedIdeal(J1,I.Linears,unique join(I.NonzeroDivisors,facs),I.Inverted);
       J1ann.IndependentSet = (basevars,S,SF);
       J1ann.LexGBOverBase = JSF;
       if J1 == J then
          splitLexGB J1ann
       else (
          J2 := trim (J : J1);
          J2ann := annotatedIdeal(J2,I.Linears,I.NonzeroDivisors,I.Inverted);
          join(splitLexGB J1ann,{J2ann})
       )
    )
)

splitFunction#SplitTower = (I,opts) -> (
    -- what do we need to stash in the answer from independentSets?
    -- does this really belong in the annotated ideal framework?
    -- create two annotated ideals:
    if isPrime I === "YES" then return {I};
    if I.?SplitTower then return {I};
    if I.?CharacteristicSets then return {I};
    if not I.?IndependentSet or not I.?LexGBSplit then return {I};
    -- Finally we can try to split this ideal into primes
    L := I.LexGBOverBase;  -- L is the lex GB over the fraction field base
    --facsL := factorTower(L, Verbosity=>opts.Verbosity, "SplitIrred"=>true, "Minprimes"=>true);
    (completelySplit, facsL) := factorTower(L, Verbosity=>opts.Verbosity);
    -- facsL is currently a list of lists:
    --   each list is of the form {exponent, poly}.  Here, we need to remove these exponents.
    if opts.Verbosity >= 4 then (
         << "SplitTower: Input: " << L << endl;
         << "           Output: " << netList facsL << endl;
         );
     for fac in facsL list (
         newI := new AnnotatedIdeal from I;
         newI.LexGBOverBase = flatten entries gens gb ideal (fac/last);
         newI.SplitTower = true;
         if completelySplit then (
             newI.isPrime = "YES";
             );
         newI
         )
     )

---------------------------------------
-- CharacteristicSets helper functions
---------------------------------------
splitViaCharSeries = method()
splitViaCharSeries Ideal := (IF) -> (
    -- variables should be in order used by rawCharSeries.
    SF := ring IF;
    I := ideal (IF_*/numerator);
    S := ring I;
    toSF := S.cache#"StoSF";
    C := rawCharSeries raw gens I;
    C = C/(c -> map(S, c));
    select(for c in C list flatten entries gens gb toSF c, i -> i#0 != 1)
    )

splitFunction#CharacteristicSets = (I,opts) -> (
    -- does this really belong in the annotated ideal framework?
    -- create two annotated ideals:
    if isPrime I === "YES" then return {I};
    if I.?CharacteristicSets then return {I};
    if not I.?IndependentSet or not I.?LexGBSplit then return {I};
    -- Finally we can try to split this ideal into primes
    L := ideal I.LexGBOverBase;  -- L is the lex GB over the fraction field base
    C := splitViaCharSeries L;  -- each of the results is a lex GB which is irred, over the ring SF
    if opts.Verbosity >= 4 then (
         << "CharacteristicSets: Input: " << L << endl;
         << "                   Output: " << netList C << endl;
         );
    for P in C list (
        newI := new AnnotatedIdeal from I;
        newI.LexGBOverBase = P;
        newI.CharacteristicSets = true;
        newI.isPrime = "YES";
        newI
        )
    )

splitFunction#DecomposeMonomials = (I,opts) -> (
    if isPrime I === "YES" then return {I};
    if I.?DecomposeMonomials or I.?IndependentSet then return {I};
    -- get all of the monomial generators of I,
    -- find all minimal primes of those, and return lots of annotated ideals adding these monomial generators
    monoms := select(I.Ideal_*, f -> size f === 1);
    if #monoms === 0 then (
        I.DecomposeMonomials = true;
        return {I};
        );
    comps := decompose monomialIdeal monoms;
    R := ring I;
    for c in comps list (
        newI := flatten entries compress ((gens I.Ideal) % c);
        J := if #newI === 0
             then ideal matrix(R, {{}})
             else trim(ideal newI);
        newlinears := for x in c_* list (x, leadCoefficient x, x);
        -- this is a place where we could be adding in redundancy.
        annJ := annotatedIdeal(J, join(I.Linears, newlinears), I.NonzeroDivisors, I.Inverted);
        if #newI === 0 then annJ.isPrime = "YES";
        annJ
        )
    )

TEST ///
  restart
  debug needsPackage "MinimalPrimes"
  R = ZZ/5[a,b,c]
  I = ideal"a5-c2, b5-c2"
  (S,SF) = makeFiberRings {c}
  L = sub(I, SF)
  splitViaCharSeries L
///
