newPackage(
        "MinimalPrimes",
        Headline => "minimal primes of an ideal",
        Version => "0.9", 
        Date => "Oct 9, 2014",
        Authors => {
            {Name => "Frank Moore",
	            HomePage => "http://users.wfu.edu/moorewf/",
	            Email => "moorewf@wfu.edu"
                },
            {Name => "Mike Stillman", 
                Email => "mike@math.cornell.edu", 
                HomePage => "http://www.math.cornell.edu/~mike"
                },
            {Name => "Franziska Hinkelmann"}
            },
	Keywords => {"Commutative Algebra"},
        AuxiliaryFiles=>true,
        PackageImports => {"Elimination"}
        )

-- << "warning!  This package is experimental.  The interface will change, and although" << endl;
-- << "  it passes its tests, it has not been fully debugged yet!" << endl;
-- << "  In particular, in small characteristic, it *sometimes* might miss a component" << endl;

--USEMGB = true;
USEMGB = false;
--if USEMGB then needsPackage "MGBInterface";

export {
    -- Main functions
    "installMinprimes",
    "minprimes",
    "newIsPrime",
    "Verbosity"
    }

protect symbol IndependentSet
protect symbol Trim
protect symbol Birational
protect symbol Linears
protect symbol DecomposeMonomials
protect symbol Factorization
protect symbol SplitTower
protect symbol CharacteristicSets

protect symbol Inverted
protect symbol LexGBOverBase
protect symbol NonzeroDivisors
protect symbol LexGBSplit
protect symbol Squarefree  -- MES todo: this is never being set but is being tested.

protect symbol toAmbientField
protect symbol fromAmbientField
-*
    --
    -- Support routines
    -- The following functions are used in UnitTestsPD.  They should
    -- removed from the export list upon release
    radicalContainment, 
    factors, 
    findNonMemberIndex, 
    -- Can we use these as keys to a ring's HashTable without exporting them?
    -- It seems awkward to have to export these.
    toAmbientField, 
    fromAmbientField,  
    -- Main functions
    minprimesWithStrategy,
    splitIdeal,
    splitIdeals,
    --- annotated ideal keys
    AnnotatedIdeal,
    Linears,
    NonzeroDivisors,
    Inverted,
    FiberInfo,
    LexGBOverBase,
    nzds,
    ---- Splitting options. Should we be exporting these?  Are we going to be
    ---- allowing the user to split (annotated) ideals using these options?
    Birational,  -- Strategy option for splitIdeal.  Exported now for simplicity
    IndependentSet,
    SplitTower,
    LexGBSplit,
    Factorization,
    DecomposeMonomials,
    Trim,
    CharacteristicSets,
    Minprimes,
    Squarefree
*-

raw  = value Core#"private dictionary"#"raw"
rawGBContains = value Core#"private dictionary"#"rawGBContains"
rawCharSeries = value Core#"private dictionary"#"rawCharSeries"

installMinprimes = () -> (
    minimalPrimes Ideal := decompose Ideal := (cacheValue symbol minimalPrimes) (
     (I) -> minprimes(I, Verbosity=>0));
    --isPrime Ideal := (I) -> newIsPrime I;
    << "minimalPrimes Ideal, decompose Ideal, and isPrime Ideal have been " << endl;
    << "re-installed to use experimental code" << endl;
    )

load "./MinimalPrimes/factorTower.m2"

if USEMGB then (
  myGB = (I) -> (
      L := groebnerBasis(I, Strategy=>"MGB");
      J := ideal L;
      forceGB gens J;
      J
      );
  MGB = (I) -> flatten entries gens myGB I;
) else (
  MGB = (I) -> flatten entries gens gb I;
  myGB = (I) -> ideal gens gb I;
  );

    mySat0 = (I, var) -> (
        if not isHomogeneous I then error "expected homogeneous input";
        if index var === null then error "expected variable";
        i := index var;
        R := ring I;
        n := numgens R - 1;
        phi := map(R,R,sub(vars R, {R_n => var, var => R_n}));
        J := phi I;
        L := MGB J;
        (M1,maxdeg) := divideByVariable(matrix{L}, R_n);
        --<< "maxdeg = " << maxdeg << endl;
        if maxdeg == 0 then I else ideal phi M1
        )

  mySat = (J, G) -> (
      facs := (factors G)/last;
      Jsat := J;
      for f in facs do (
          Jsat = if index f =!= null and isHomogeneous J and char ring J > 0 then 
              mySat0(Jsat, f)
          else 
              saturate(Jsat, f);
          );
      Jsat)
      

---------------------------------
--- Minprimes strategies
---------------------------------
strat0 = ({Linear,DecomposeMonomials},infinity)
strat1 = ({Linear,DecomposeMonomials,(Factorization,3)},infinity)
BirationalStrat = ({strat1, (Birational,infinity)},infinity)
NoBirationalStrat = strat1
stratEnd = {(IndependentSet,infinity),SplitTower, CharacteristicSets}

getMinPrimesStrategy = strat -> (
    -- input: string: String
    -- output: is a strategy list/sequence, etc for use with minprimes
    -- MES
    if not instance(strat, String) then return strat;
    if strat === "NoBirational" then
        NoBirationalStrat
    else if strat === "Birational" then
        BirationalStrat
    else
        error ("unknown strategy: "|strat)
    )

minprimes = method(Options => {
        Verbosity => 0,
        Strategy => "Birational",  -- if null, calls older minprimesWorker code
        "SquarefreeFactorSize" => 1,
        CodimensionLimit => null, -- only find minimal primes of codim <= this bound
        "IdealSoFar" => null,  -- used in inductive setting
        "RadicalSoFar" => null, -- used in inductive setting
        "CheckPrimeOnly" => false
        })

AnnotatedIdeal = new Type of MutableHashTable

-- An "annotated ideal" is a hash table with keys:
--   Ideal:  I
--   Linears: L
--   NonzeroDivisors: NZ
--   Inverted: inverteds
--   where 
--     (a) I is an ideal (in a subset of the variables)
--     (b) L is a list of (x, g, f), where 
--     (c) x is a variable (not appearing in I at all)
--     (d) g is a monic poly not involving x
--     (e) f = xg-h is in the original ideal (leadTerm f is not nec leadTerm(xg))
--     (f) h does not involve x.
--   NZ: list of known nonzero-divisors.  This is used only for performance:
--     once we know that e.g. A.Ideal : f == A.Ideal, then f can be placed on this list.
--   inverteds: Elements that have been 'inverted' in the calculation.  Need to saturate
--     with respect to these when reconstructing the associated ideal, assuming that
--     the ideal is not known to be prime already.
-- HOWEVER, if the keys IndependentSet, LexGBOverBase exist
--   then I is only contained in the associated ideal.
--   These keys contain the following info:
--     A.IndependentSet   This is a triple (basevars,S,SF) where S,SF are returned from makeFiberRings
--     A.LexGBOverBase  GB of ISF over SF
--     If one of these flags is set, both are, and the resulting ideal is equidimensional.
-- Other keys:
--   Finished Flags: if any of these flags exists, then that split
--   technique would have no further effect on the annotated ideal A.
--    A.Birational   
--    A.Linear
--    A.Factorization
--    A.IndependentSet
--    A.SplitTower
--    A.DecomposeMonomials
--    A.Trim
--    A.LexGBSplit is set once LexGBOverBase consists of irred polynomials over the base field.
--   A.isPrime: possible values: "YES", "NO", "UNKNOWN".  Usually "YES" or "UNKNOWN".
-- The associated ideal consists of 3 parts, with a potential saturation step:
--   (a) the linear polynomials in L
--   (b) the ideal I
--   (c) if LexGBOverBase is a key, then the contraction of (ideal A.LexGBOverBase) to the polynomial ring
-- the saturation is with respect to all g for each (x,g,f) in L.
-- See "ideal AnnotatedIdeal" for the exact formula.

---------------------------------------------
--- General AnnotatedIdeal commands ---------
---------------------------------------------

-- this function returns a list of the unique factors (of positive degree)
--  occurring in polyList, made monic.
monicUniqueFactors = polyList -> (
    polyList1 := polyList/factors//flatten;
    polyList2 := select(polyList1, g -> #g > 0);
    polyList2 / last // unique
)

TEST ///
   debug needsPackage "MinimalPrimes"
   R = QQ[a,b,c]
   polyList = {2*a^2,b^2,c^2,a^3,1_R,8*b^3-c^3}
   muf = monicUniqueFactors polyList
   assert(set muf === set {a, b, c, b-(1/2)*c, b^2+(1/2)*b*c+(1/4)*c^2})
///

annotatedIdeal = method()
annotatedIdeal(Ideal, List, List, List) := (I, linears, nzds, inverted) -> (
    -- See above for the definition of an annotated ideal and its keys
    -- The arguments are named the same thing as in that description
    -- nzds is a list of polynomials which are nzds's of the associated ideal
    -- inverted is a list of elements such that we ignore the minimal primes
    --   that contain any of these elements
    -- The associated ideal is:
    --   saturate(I + ideal(linears/last), product unique join((linears / (x -> x#1)),inverted))
    new AnnotatedIdeal from {
        symbol Ideal => I, 
        symbol Linears => linears, 
        symbol NonzeroDivisors => monicUniqueFactors nzds,
        symbol Inverted => monicUniqueFactors inverted
        }
    )

gb AnnotatedIdeal := opts -> (I) -> I.Ideal = ideal gens gb(I.Ideal, opts)

-- getGB is used for finding a lower bound on the codim of the ideal I
-- The idea is that sometimes the GB computation is too huge, and we
-- don't want to undertake that.  But, if it is there, we want to take
-- advantage of it.  It could even be a partial Groebner basis.
-- codimLowerBound below uses whatever lead terms we can find in the ideal
-- to get some lower bound on the codimension.
getGB = method()
getGB Ideal := (I) -> (
     cached := keys I.generators.cache;
     pos := select(1, cached, k -> instance(k, GroebnerBasisOptions));
     if #pos === 0 then null
     else
       I.generators.cache#(first pos)
     )

codim AnnotatedIdeal := options(codim,Ideal) >> opts -> (I) -> (
     if I.?LexGBOverBase then (
         -- this codim is correct in all cases
         S := ring I.LexGBOverBase#0; -- should be of the form kk(indepvars)[fibervars]
         # I.Linears + numgens S
         )
     else ( 
         -- this codim may be only a codimLowerBound if an element in the
         -- inverted list is in all the minimal primes of top dimension
         if not I.?isPrime or I.isPrime =!= "YES" then 
            << "Warning : codim AnnotatedIdeal called on ideal not known to be prime." << endl;
         # I.Linears + codim(I.Ideal)
     )
     )

codimLowerBound = method()
codimLowerBound AnnotatedIdeal := (I) -> (
     if I.?LexGBOverBase then (
         -- again, in this case, this is the actual codimension.
         S := ring I.LexGBOverBase#0; -- should be of the form kk(indepvars)[fibervars]
         # I.Linears + numgens S
         )
     else (
          GB := getGB I.Ideal;
          if GB =!= null then (
             # I.Linears + codim(monomialIdeal leadTerm GB)
             )
          else if numgens I.Ideal === 1 and I.Ideal_0 != 0 then
             # I.Linears + 1
          else
             # I.Linears + codim(monomialIdeal gens I.Ideal)
         )
     )

net AnnotatedIdeal := (I) -> peek I

ring AnnotatedIdeal := (I) -> ring I.Ideal

isSubset (Ideal,AnnotatedIdeal) := (J,I) -> (
   -- naive attempt first...
   isSubset(J,ideal I)
)

-- The associated ideal to an annotated ideal I is
-- defined at the top of this file.
-- TODO Notes (23 April 2013):
--  (1) Possibly split the linears into two groups, and add the ones with denom=1
--      after the saturation is done.
--  (2) Should we be saturating with the I.Inverted \ I.NonzeroDivisors polynomials? 
--      Answer should be yes: but if we know the ideal is prime, then 
--      we think we can avoid this.  BUT: we need to be very precise about
--      this logic.
-*
ideal AnnotatedIdeal := (I) -> (
    --F := product unique join(I.Linears / (x -> x#1),I.Inverted);
    if not I#?"CachedIdeal" then I#"CachedIdeal" = (
       F := product unique (I.Linears / (x -> x#1));
       I1 := ideal(I.Linears/last);
       I2 := if I.?IndependentSet then (
               S := (I.IndependentSet)#1;
               phi := S.cache#"StoR";
               phi contractToPolynomialRing ideal I.LexGBOverBase
             )
             else
               I.Ideal;
       I3 := if numgens I1 === 0 then I2 else if numgens I2 === 0 then I1 else I1+I2;
       --if F == 1 then I3 else saturate(I3, F)
       if F == 1 then I3 else mySat(I3, F)
    );
    I#"CachedIdeal"
)
*-

ideal AnnotatedIdeal := (I) -> (
    --F := product unique join(I.Linears / (x -> x#1),I.Inverted);
    if not I#?"CachedIdeal" then I#"CachedIdeal" = (
       I2 := if I.?IndependentSet then (
               S := (I.IndependentSet)#1;
               phi := S.cache#"StoR";
               phi contractToPolynomialRing ideal I.LexGBOverBase
             )
             else
               I.Ideal;
       for linear in reverse I.Linears do (
           I2 = I2 + ideal last linear;
           if linear#1 == 1 then continue;
           facs := factors linear#1;
           if #facs != 0 then (
               F := facs/last//product;
               I2 = mySat(I2, F);
               );
           I2 = trim I2;
           );
       I2
    );
    I#"CachedIdeal"
)

TEST ///
  debug needsPackage "MinimalPrimes"
  R = QQ[b,s,t,u,v,w,x,y,z]
  I = ideal"su - bv, tv - sw, vx - uy, wy - vz"
  J1 = annotatedIdeal(trim ideal 0_R,{},{},{})
  assert(ideal J1 == 0)
  assert(ring J1 === R)
  assert(codim J1 == 0)
  J2Linears = {(z, v, - w*y + v*z), (y, u, - v*x + u*y), (w, s, - t*v + s*w), (v, b, - s*u + b*v)}
  J2 = annotatedIdeal(trim ideal 0_R, J2Linears,{v, u, s, b},{})
  F = product unique (J2Linears / (x -> x#1))
  assert(ideal J2 == saturate(ideal (J2Linears / last), F))
  assert(ring J2 === R)
  assert(codim J2 == 4)
  J2 = trim J2
  assert(J2.Trim)
  J3 = annotatedIdeal(trim ideal 1_R,{},{},{})
  assert(ideal J3 == 1)
  assert(ring J3 === R)
  assert(codim J3 == infinity)
  --- TODO: An example with IndependentSets/SplitTower
///

-- Note that if I.IndependentSet is set, then I.Ideal is not the entire ideal.
-- However in this case, I.isPrime will (TODO: check this!) have previously
-- been set to "UNKNOWN", or maybe to "YES" during the IndependentSet or
-- SplitTower computatations.
isPrime AnnotatedIdeal := (I) -> (
    if I.?IndependentSet and not I.?isPrime 
      then error "Our isPrime logic is wrong in this case.";
    if not I.?isPrime or I.isPrime === "UNKNOWN" then (
        I.isPrime = if numgens I.Ideal == 0 then "YES" else
                    if I.?Factorization and numgens I.Ideal == 1 then "YES" else
                    "UNKNOWN";
       );
    I.isPrime
    )


--- this is so that we can add in generators to I and keep track of
--- how the annotation changes
adjoinElement = method()
adjoinElement (AnnotatedIdeal,RingElement) := (I,f) -> (
   J := ideal compress ((gens I.Ideal) % f);
   J = if J == 0 then ideal f else (ideal f) + J;
   annotatedIdeal(J,  -- is a trim necessary/desirable here?
                  I.Linears,  -- 'linear' generators
                  {},         -- clear out nonzerodivisor list
                  unique join(I.NonzeroDivisors,I.Inverted)) -- move nonzerodivisors to inverted list
)

trim AnnotatedIdeal := opts -> I -> (
    I.Ideal = trim I.Ideal;
    I.Trim = true;
    I
)

-------------------------------------------
---- 'Splitting' commands follow.  Not all are genuinely 'splitting'
---- in that sometimes, an operation is performed on the ideal without
---- any splitting occurring.
-------------------------------------------

------------------------------------------------------------
-- splitIdeal code
splitIdeal = method(Options => {Strategy=>BirationalStrat,
                                Verbosity=>0,
                                "PDState"=>null,
                                CodimensionLimit => null,
                                "SquarefreeFactorSize" => 1,
                                "CheckPrimeOnly" => false})
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
    if hf =!= null then gb(JS, Hilbert=>hf) else gb JS;
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

TEST ///
  restart
  debug needsPackage "MinimalPrimes"
  R = ZZ/5[a,b,c]
  I = ideal"a5-c2, b5-c2"
  (S,SF) = makeFiberRings {c}
  L = sub(I, SF)
  splitViaCharSeries L
///

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
------------------------------------
--- IndependentSet helper functions
-------------------------------------

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

-----------------------------------------
--- Begin new nested strategy code
-----------------------------------------

-- format for strategy:
-- a strategy is one of the following:
--  1. Symbol (allowed: Linear, Factorization, ...)
--  2. (strategy, #times)
--  3. list of strategies
-- If no #times is given (e.g. in (1) or (3), then 1 is assumed)

-- each of the splitIdeals routines:
--  takes a list of annotated ideals, and returns a similar list
--  
splitIdeals = method(Options => options splitIdeal)

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

separatePrime = (L) -> (
    -- L is a list of annotated ideals
    -- returns (L1,L2), where L1 is the list of elements of L which are known to be prime
    -- and L2 are the rest
    H := partition(I -> (I.?isPrime and I.isPrime === "YES"), L);
    (if H#?true then H#true else {}, if H#?false then H#false else {})
    )

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
splitIdeal(Ideal) := opts -> (I) -> (
    pdState := createPDState(I);
    opts = opts ++ {"PDState" => pdState};
    splitIdeals({annotatedIdeal(I,{},{},{})}, opts.Strategy, opts)
    )
splitIdeal(AnnotatedIdeal) := opts -> (I) -> (
    splitIdeals({I}, opts.Strategy, opts)
    )


--------------------------------
--- PDState commands -----------
--------------------------------
PDState = new Type of MutableHashTable
createPDState = method()
createPDState Ideal := I -> (
   new PDState from {"OriginalIdeal" => I,
                     "PrimesSoFar" => new MutableHashTable from {},
                     "IntersectionSoFar" => ideal (1_(ring I)),
                     "isPrimeIdeal" => true,
                     "isPrimaryIdeal" => true,
                     "PrunedViaCodim" => 0}
)

updatePDState = method()
updatePDState (PDState,List,ZZ) := (pdState,L,pruned) -> (
  -- this function updates the pdState with the new primes in the list L which
  -- consists of annotated ideals, all of which are known to be prime
  ansSoFar := pdState#"PrimesSoFar";
  pdState#"PrunedViaCodim" = pdState#"PrunedViaCodim" + pruned;
  for p in L do (
     I := ideal p;
     --pdState#"IntersectionSoFar" = trim intersect(pdState#"IntersectionSoFar", I);
     c := codim p;
     --<< endl << "   Adding codimension " << c << " prime ideal." << endl;
     if not ansSoFar#?c then
        ansSoFar#c = {(p,I)}
     else
        ansSoFar#c = append(ansSoFar#c,(p,I));
  );
  -*
  -- here we update the isPrime flag if L comes in with more than one
  -- prime, then the ideal is neither prime nor primary.
  -- the reason for this is that no single step will produce multiple redundant
  -- primes.  The only possible redundancy occurs when a prime is
  -- already in pdState and also comes into the list L
  if #L > 1 then (
     pdState#"isPrimeIdeal" = false;
     pdState#"isPrimaryIdeal" = false;
  );
  *-
)

numPrimesInPDState = method()
numPrimesInPDState PDState := pdState -> sum apply(pairs (pdState#"PrimesSoFar"), p -> #(p#1))

getPrimesInPDState = method()
getPrimesInPDState PDState := pdState ->
   flatten apply(pairs (pdState#"PrimesSoFar"), p -> (p#1) / last)

isRedundantIdeal = method()
isRedundantIdeal (AnnotatedIdeal,PDState) := (I,pdState) -> (
   -- the reason for this line is that once IndependentSet has been called, then
   -- I.Ideal no longer reflects the ideal
   if I.?IndependentSet then return false;
   primeList := getPrimesInPDState(pdState);
   -- as of now, all ideals are declared not redundant
   false
   -- this commented line takes too long!
   --any(primeList,p -> isSubset(p,I))
)

flagPrimality = method()
flagPrimality (PDState,Boolean) := (pdState,primality) -> (
   pdState#"isPrime" = primality;
)
--------------------------------
--- Minimal primes
--------------------------------

minprimesWithStrategy = method(Options => (options splitIdeals))
minprimesWithStrategy(Ideal) := opts -> (I) -> (
    newstrat := {opts.Strategy, stratEnd};
    if opts.CodimensionLimit === null then 
      opts = opts ++ {CodimensionLimit => numgens I};
    pdState := createPDState(I);
    opts = opts ++ {"PDState" => pdState};
    M := splitIdeals({annotatedIdeal(I,{},{},{})}, newstrat, opts);
    -- if just a primality/primary check, then return result.
    -- should we cache what we have done somewhere?
    if opts#"CheckPrimeOnly" then return pdState#"isPrime";
    numRawPrimes := numPrimesInPDState pdState;
    --M = select(M, i -> codimLowerBound i <= opts#"CodimensionLimit");
    --(M1,M2) := separatePrime(M);
    if #M > 0 then (
         ( << "warning: ideal did not split completely: " << #M << " did not split!" << endl;);
         error "answer not complete";
         );
    if opts#Verbosity>=2 then (
       << "Converting annotated ideals to ideals and selecting minimal primes..." << flush;
    );
    answer := timing((getPrimesInPDState pdState)//selectMinimalIdeals);
    if opts#Verbosity>=2 then (
       << " Time taken : " << answer#0 << endl;
       if #(answer#1) < numRawPrimes then (
            << "#minprimes=" << #(answer#1) << " #computed=" << numPrimesInPDState pdState << endl;
            );
    );
    answer#1
    )

doSplitIdeal = method(Options => (options splitIdeals))
doSplitIdeal(Ideal) := opts -> (I) -> (
    if opts.CodimensionLimit === null then 
      opts = opts ++ {CodimensionLimit => numgens I};
    pdState := createPDState(I);
    opts = opts ++ {"PDState" => pdState};
    M := splitIdeals({annotatedIdeal(I,{},{},{})}, opts.Strategy, opts);
    numRawPrimes := numPrimesInPDState pdState;
    {getPrimesInPDState pdState, M}
    --{pdState, M}
    )


-----------------------
-- Minimal primes -----
-----------------------
minprimes Ideal := opts -> (I) -> (
    strategy := getMinPrimesStrategy opts#Strategy;
    -- returns a list of ideals, the minimal primes of I
    A := ring I;
    (I',F) := flattenRing I; -- F is not needed
    R := ring I';
    if not isPolynomialRing R then error "expected ideal in a polynomial ring or a quotient of one";
    psi := map(A, R, generators(A, CoefficientRing => coefficientRing R));
    backToOriginalRing := if R === A then 
            identity 
         else (J) -> trim psi J;
    I = I';
    if not isCommutative R then
      error "expected commutative polynomial ring";
    kk := coefficientRing R;
    if kk =!= QQ and not instance(kk,QuotientRing) and not instance(kk, GaloisField) then
      error "expected base field to be QQ or ZZ/p or GF(q)";
    if I == 0 then return {if A === R then I else ideal map(A^1,A^0,0)};
    -- note: at this point, R is the ring of I, and R is a polynomial ring over a prime field
    C := minprimesWithStrategy(I,
                       CodimensionLimit => opts.CodimensionLimit,
                       Strategy=>strategy,
                       "SquarefreeFactorSize"=>opts#"SquarefreeFactorSize",
                       Verbosity=>opts#Verbosity);
    C / backToOriginalRing
)

TEST ///
R1 = QQ[d, f, j, k, m, r, t, A, D, G, I, K];
I1 = ideal ( I*K-K^2, r*G-G^2, A*D-D^2, j^2-j*t, d*f-f^2, d*f*j*k - m*r, A*D - G*I*K);
assert(#(minprimes I1) == 22)
///

------------------------------
--- isPrime ------------------
------------------------------
newIsPrime = method(Options => {
                Verbosity => 0,
                Strategy => BirationalStrat,
                "SquarefreeFactorSize" => 1
                })

newIsPrime Ideal := opts -> I -> (
    C := minprimes(I, opts);
    #C === 1 and C#0 == I
    )

-- This should work, but no time to debug it before version 1.7 (MES)
-- Once it is working, check the logic of newIsPrime replacing isPrime
--newIsPrime Ideal := opts -> I -> (
--   minprimesWithStrategy(I,
--                         Strategy=>opts#Strategy,
--                         "SquarefreeFactorSize"=>opts#"SquarefreeFactorSize",
--                         Verbosity=>opts#Verbosity,
--                         "CheckPrimeOnly"=>true);
--)
------------------------------
-- Radical containment -------
------------------------------
-- helper function for 'radicalContainment'
radFcn = (I) -> (
    if not I.cache#?"RadicalContainmentFunction" then (
        R := ring I;
        n := numgens R;
        S := (coefficientRing R) (monoid[Variables=>n+1,MonomialSize=>16]);
        mapto := map(S,R,submatrix(vars S,{0..n-1}));
        I = mapto I;
        -- here is a GB of I!
        A := S/I;
        rad := (g) -> (g1 := promote(mapto g, A); g1 == 0 or ideal(g1 * A_n - 1) == 1);
        I.cache#"RadicalContainmentFunction" = rad;
        );
    I.cache#"RadicalContainmentFunction"
    )

radicalContainment = method()
-- Returns true if g is in the radical of I.
-- Assumption: I is in a monomial order for which you are happy to compute GB's.x
radicalContainment(RingElement, Ideal) := (g,I) -> (radFcn I) g

-- Returns the first index i such that I_i is not in the radical of J,
-- and null, if none
-- another way to do something almost identical: select(1, I_*, radFcn J)
radicalContainment(Ideal, Ideal) := (I,J) -> (
    rad := radFcn J;
    G := I_*;
    for i from 0 to #G-1 do if not rad G#i then return i;
    null
    )

-----------------------------
-- Redundancy control -------
-----------------------------
-- find, if any, an element of I which is NOT in the ideal J.
-- returns the index x of that element, if any, else returns -1.
findNonMemberIndex = method()
findNonMemberIndex(Ideal,Ideal) := (I,J) -> (
     m := generators I;
     n := gb J;
     rawGBContains(raw n, raw m)
     )
-- The following function removes any elements which are larger than another one.
-- Each should be tagged with its codimension.  For each pair (L_i, L_j), check containment of GB's
selectMinimalIdeals = (L) -> (
    L = L/(i -> (codim i, flatten entries gens gb i))//sort/last/ideal;
    ML := new MutableList from L;
    for i from 0 to #ML-1 list (
        if ML#i === null then continue;
        for j from i+1 to #ML-1 do (
            if ML#j === null then continue;
            if findNonMemberIndex(ML#i, ML#j) === -1 then ML#j = null;
            );
        ML#i
        )
    )

----------------------------------------------
-- Factorization and fraction field commands 
----------------------------------------------
-- setAmbientField:
--   input: KR, a ring of the form kk(t)[u] (t and u sets of variables)
--          RU, kk[u,t] (with some monomial ordering)
--   consequence: sets information in KR so that
--     'factors' and 'numerator', 'denominator' work for elemnts of KR 
--     sets KR.toAmbientField, KR.fromAmbientField
setAmbientField = method()
setAmbientField(Ring, Ring) := (KR, RU) -> (
    -- KR should be of the form kk(t)[u]
    -- RU should be kk[u, t], with some monomial ordering
    KR.toAmbientField = map(frac RU,KR);
    KR.fromAmbientField = (f) -> (if ring f === frac RU then f = numerator f; (map(KR,RU)) f);
    numerator KR := (f) -> numerator KR.toAmbientField f;
    denominator KR := (f) -> denominator KR.toAmbientField f;
    )

-- needs documentation
factors = method()
factors RingElement := (F) -> (
    R := ring F;
    if F == 0 then return {(1,F)};
    facs := if R.?toAmbientField then (
        F = R.toAmbientField F;
        RU := ring numerator F;
        numerator factor F
        )
    else if isPolynomialRing R and instance(coefficientRing R, FractionField) then (
        KK := coefficientRing R;
        A := last KK.baseRings;
        RU = (coefficientRing A) (monoid[generators R, generators KK, MonomialOrder=>Lex]);
        setAmbientField(R, RU);
        F = R.toAmbientField F;
        numerator factor F
        )
    else if instance(R, FractionField) then (
        -- What to return in this case?
        -- WORKING ON THIS MES
        error "still need to handle FractionField case";
        )
    else (
        RU = ring F;
        factor F
        );
    facs = facs//toList/toList; -- elements of facs: {factor, multiplicity}
    facs = select(facs, z -> ring first z === RU);
    facs = apply(#facs, i -> (facs#i#1, (1/leadCoefficient facs#i#0) * facs#i#0 ));
    facs = select(facs, (n,f) -> # support f =!= 0);
    if R.?toAmbientField then apply(facs, (r,g) -> (r, R.fromAmbientField g)) else facs
    )

makeFiberRings = method()
makeFiberRings List := basevars -> if #basevars == 0 then error "Expected at least one variable in the base" else makeFiberRings(basevars,ring (basevars#0))
makeFiberRings(List,Ring) := (basevars,R) -> (
   -- This function takes a (possibly empty) list of variables basevars as input
   -- and returns a pair of matrices (mons, cs) where mons are the monomials in the ideal
   -- of lead terms of a gb of I, and cs are the coefficients, but with respect to
   -- a product order kk[fiberVars][basevars].  See tests for behavior
   -- If basevars does happen to be empty, then the original ring with Lex order is returned.
   local S;
   if #basevars == 0 then (
        -- in this case, we are not inverting any variables.  So, S = SF, and S just has the lex
        -- order.
        S = newRing(R, MonomialOrder=>Lex);
        S#cache = new CacheTable;
        S.cache#"RtoS" = map(S,R,sub(vars R,S));
        S.cache#"StoR" = map(R,S,sub(vars S,R));
        S.cache#"StoSF" = identity;
        S.cache#"SFtoS" = identity;
        numerator S := identity;
        (S,S)
   )
   else
   (
      if any(basevars, x -> ring x =!= R) then error "expected all base variables to have the same ring";
      allVars := set gens R;
      fiberVars := rsort toList(allVars - set basevars);
      basevars = rsort basevars;
      S = (coefficientRing R) monoid([fiberVars,basevars,MonomialOrder=>Lex]);
          --MonomialOrder=>{#fiberVars,#basevars}]);
      KK := frac((coefficientRing R)(monoid [basevars]));
      SF := KK (monoid[fiberVars, MonomialOrder=>Lex]);
      S#cache = new CacheTable;
      S.cache#"StoSF" = map(SF,S,sub(vars S,SF));
      S.cache#"SFtoS" = map(S,SF,sub(vars SF,S));
      S.cache#"StoR" = map(R,S,sub(vars S,R));
      S.cache#"RtoS" = map(S,R,sub(vars R,S));
      setAmbientField(SF, S);
      (S, SF)
   )
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

-- Question: What if we want to contract away only some of the basevars, not all of them?  Will this ever
--           be the case?
-- TODO NOTE: the saturate here should be done in the ring R (grevlex)
contractToPolynomialRing = method(Options => {Verbosity => 0})
contractToPolynomialRing(Ideal) := opts -> (I) -> (
     -- assumes: I is in a ring k(basevars)[fibervars] created with makeFiberRings
     -- returns the intersection of I with k[fibervars,basevars] (also created with makeFiberRing).
     --   note: numerator (and denominator) of element in ring I gives an element in k[fibervars,basevars]
     if not instance(coefficientRing ring I, FractionField) then return I; -- in this case, we are already contracted!
     newI := I_*/numerator//ideal//trim;
     S := ring newI;
     denoms := I_*/denominator;
     denomList := unique flatten for d in denoms list (factors d)/last;
     if opts.Verbosity > 0 then << "denoms = " << denoms << " and denomList = " << denomList << endl;
     Isat := S.cache#"StoR" newI;
     F := S.cache#"StoR" (product denomList);
     Isat = mySat(Isat, F);
     --for f in denomList do Isat = saturate(Isat, S.cache#"StoR" f);
     S.cache#"RtoS" Isat
     )

-- This function determines whether or not the lead term of the input polynomial is linear
--- Doesn't seem to be used here.  Maybe in gbRatRecon?
hasLinearLeadTerm = method()
hasLinearLeadTerm RingElement := (f) -> (
    t := leadTerm f;
    s := support t;
    #s === 1 and s#0 == t
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

-------------------------------------------------------------
load "./MinimalPrimes/tests.m2"
beginDocumentation()

doc ///
Key
  MinimalPrimes
Headline
  experimental package: minimal primes of an ideal
Description
  Text
    Find the minimal primes of an ideal in a polynomial ring over a prime field,
    or a quotient ring of that.  These are the geometric components
    of the corresponding algebraic set.
    
    The main routine is @TO "minprimes"@, although in a future 
    release this will be renamed to {\tt minimalPrimes}.
    
    Use @TO "installMinprimes"@ to replace the system versions of 'decompose Ideal', 
    'minimalPrimes Ideal' and 'isPrime Ideal'.  In many cases the new function is {\it much} faster,
    although there are cases when the older, current, version is faster.
Caveat
  Only works for ideals in (commutative)polynomial rings or quotients of 
    polynomial rings over a prime field, might have bugs in small characteristic and larger degree 
    (although, many of these cases are caught correctly).
SeeAlso
  decompose
  minimalPrimes
  isPrime
///

doc ///
   Key
     minprimes
   Headline
     minimal primes in a polynomial ring over a field
   Usage
     C = minprimes I
   Inputs
     I:Ideal
     Verbosity => ZZ
       A larger number will cause more output during the computation
     Strategy => String
       The default is fine for most things.  If it is slow, try "NoBirational".
         The strategies might change, so is it best to stick with these two
         for now (there are other undocumented strategies)
     CodimensionLimit => ZZ
       Only find components of codimension less than or equal to this value
   Outputs
     C:List
       a list of the minimal primes of I
   Description
    Text
      Given an ideal in a polynomial ring, or a quotient of a polynomial ring
      whose base ring is either {\tt QQ} or {\tt ZZ/p}, return a list
      of minimal primes of the ideal.
    Example
      R = ZZ/32003[a..e]
      I = ideal"a2b-c3,abd-c2e,ade-ce2"
      C = minprimes I;
      netList C
    Example
      C2 = minprimes(I, Strategy=>"NoBirational", Verbosity=>2)
      C1 = minprimes(I, Strategy=>"Birational", Verbosity=>2)
   Caveat
     This will eventually be made to work over GF(q), and over other fields too.
   SeeAlso
///

doc ///
   Key
     installMinprimes
   Headline
     install experimental functions into Macaulay2
   Usage
     installMinprimes()
   Consequences
     Item
       Changes the methods @TO (decompose,Ideal)@, @TO (minimalPrimes, Ideal)@, and 
         @TO (isPrime,Ideal)@
   Description
    Example
      installMinprimes()
      R = ZZ/32003[a..e]
      I = ideal"a2b-c3,abd-c2e,ade-ce2"
      C = minprimes I;
      C1 = minimalPrimes I;
      C == C1
      netList C
      C/isPrime
   Caveat
     This function will eventually go away, once this is no longer experimental code
   SeeAlso
     minprimes
///

doc ///
  Key
    Verbosity
  Headline
    optional argument describing how verbose the output should be
  Description
   Text
     Specifying the optional argument {\tt Verbosity => n}, where $n$ is an integer
     tells the routine how much output should be given.  A value of 0 means be silent.
     The larger the value $n$, the more output one might see.
///


end--

restart
uninstallAllPackages()
installPackage "MinimalPrimes"
installPackage "IntegralClosure"
loadPackage "MinimalPrimes"

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

restart
debug needsPackage "MinimalPrimes"
R1 = QQ[d, f, j, k, m, r, t, A, D, G, I, K];
I1 = ideal ( I*K-K^2, r*G-G^2, A*D-D^2, j^2-j*t, d*f-f^2, d*f*j*k - m*r, A*D - G*I*K);
time minprimes(I1, CodimensionLimit=>6, Verbosity=>2)
#(minprimes I1)
#(oldMinPrimes I1)
time minprimes(I1, CodimensionLimit=>7, Verbosity=>2)
time minprimes(I1, CodimensionLimit=>6, Verbosity=>2);


-- TODO to get this into the system:
-- add documentation
-- tests.  The tests should be in this package.
--   test trivial ideals
--        cases that shouldn't work
--        funny gradings
--        quotient rings
--
-- minprimes: installs itself into decompose. DONE
--   minprimes should stash its answer DONE
--             should work for quotients DONE
--             should give errors for general situations DONE?
--             what about over ZZ?
--             absolute case?
--             factorization over towers?
--  a better inductive system?
--
-- Mike and Frank talking 10/9/2014
-- to do:
-- .  The function 'factors' needs documentation and tests.  In fact, it is failing in some cases (e.g.
--    when 'factor' returns a factor not in the polynomial ring.
-- .  Document minprimes, something about the strategies
-- .  Export only the symbols we want
-- .  
restart
needsPackage "MinimalPrimes"
check oo

R1 = QQ[d, f, j, k, m, r, t, A, D, G, I, K];
I1 = ideal ( I*K-K^2, r*G-G^2, A*D-D^2, j^2-j*t, d*f-f^2, d*f*j*k - m*r, A*D - G*I*K);
-- C = doSplitIdeal(I1, Verbosity=>2)
time minprimes(I1, CodimensionLimit=>6, Verbosity=>2)
C = time minprimes I1
C = time minprimes(I1, Strategy=>"NoBirational", Verbosity=>2)
C = time minprimes(I1, Strategy=>"NoBirationa", Verbosity=>2)

R1 = QQ[a,b,c]
I1 = ideal(a^2-3, b^2-3)
C = doSplitIdeal(I1, Verbosity=>2)
minprimes(I1, Verbosity=>2)

kk = ZZ/7
R = kk[x,y,t]
I = ideal {x^7-t^2,y^7-t^2}
minprimes I
C = doSplitIdeal(I, Verbosity=>2)
