newPackage("Valuations",
        Headline => "implementation of valuations for rings",
        Version => "1.0",
        Date => "June 5, 2023",
        Authors => {
            {Name => "Michael Burr", Email => "burr2@clemson.edu", HomePage => "https://cecas.clemson.edu/~burr2/"},
            {Name => "Colin Alstad", Email => "calstad@clemson.edu", HomePage => "https://colinalstad.com/"},
            {Name => "Michael Byrd", Email => "mbyrd6@clemson.edu", HomePage => "https://michael-byrd.github.io"},
            {Name => "Ethan Partida", Email => "ethan_partida@brown.edu", HomePage => "https://ethanpartida.github.io/"},
            {Name => "Shelby Cox", Email => "spcox@umich.edu"},
            {Name => "Courtney George", Email => "courtney.george@uky.edu"},
            {Name => "Oliver Clarke", Email => "oliver.clarke@ed.ac.uk", HomePage => "https://www.oliverclarkemath.com/"}},
        HomePage => "https://github.com/Macaulay2/Workshop-2023-Minneapolis/tree/valuations",
        Configuration => {},
        PackageExports => {"LocalRings", "SubalgebraBases", "InvariantRing", "gfanInterface", "Binomials"},
	Keywords => {"Commutative Algebra"})

----- Eventually move to other packages
ring Subring := A -> ambient A
ring RingOfInvariants := A -> ambient A
ring LocalRing := A -> A
ambient LocalRing := A -> A

export{"valuation",
       "Valuation",
       "trivialValuation",
       "padicValuation",
       "leadTermValuation",
       "lowestTermValuation",
       "localRingValuation",
       --"getMExponent",
       "valM",
       "primeConesOfIdeal",
       "primeConesOfSubalgebra",
       --"positivity",
       "coneToValuation",
       "OrderedQQn",
       "OrderedQQVector",
       "orderedQQn"}

OrderedQQn = new Type of Module
OrderedQQVector = new Type of Vector

--In the future, this object will be implemented at a deeper level.
--A @TO "Module"@ object does not naturally contain a monomial order.
--We aim to implement this like we see in the object @TO "Ring"@.

--------------------------------------------------------------------------------
-------------------------------- Valuation Type --------------------------------
--------------------------------------------------------------------------------

Valuation = new Type of HashTable

valuation = method()

valuation Function := v -> (
    internalValuation(v, null, null)
)

ourSources := {Ring,Subring,LocalRing,RingOfInvariants}
ourTargets := {Ring,Subring,LocalRing,RingOfInvariants,OrderedQQn}

-- Create different valuation functions for various inputs
-- Add items to the sources and targets above to add types
for i in ourSources do (
    for j in ourTargets do (
        valuation (Function, i, j) := (v, S, T) -> (
            internalValuation(v, S, T)
        )
    )
)

-- Create a valuation from a function, source, and target
-- The source and target may be null (hence the type Thing).
internalValuation = method()
internalValuation (Function, Thing, Thing) := (v, S, T) -> (
    new Valuation from{
        "function" => v,
        source => S,
        target => T,
        cache => new CacheTable
    }
)

ourInputs := {Number, RingElement, Constant}

-- Evaluation of a valuation on an input.
-- The standard types of inputs are in the input list
-- Other inputs will need to be added to that list, if needed
for i in ourInputs do (
    Valuation i := (v,t) -> (
        num := try numerator t then numerator t else t;
        den := try denominator t then denominator t else 1_(ring t);
        if (v#source === null) or (ring t) === v#source then
            v#"function" t
        else if (isMember(ring t, v#source.baseRings)) then
            v#"function" promote(t, v#source)
        else if (ring t) === v#source then
            v#"function" num - v#"function" den
        else if (isMember(ring num, v#source.baseRings)) then
            v#"function" promote(num, v#source) - v#"function" promote(den, v#source)
    )
)

-- Print information about a valuation
net Valuation := v -> (
    s := v#source;
    t := v#target;
    if not ((s === null) or (t === null)) then
        "valuation from " | toString(s) | " to " | toString(t)
    else if not (s === null) then
        "valuation from " | toString(s)
    else if not (t === null) then
        "valuation to " | toString(t)
    else
        "valuation with unspecified source and target"
)

--------------------------------------------------------------------------------
--------------------------- Ordered QQ-module Types ----------------------------
--------------------------------------------------------------------------------

-- Ordered Module based on the monomial order of a polynomial ring
--
-- given two elements a, b in QQ^n they are compared by using the
-- the monomial order of the polynomial ring:
-- 1) clear the denominators of a and b: d*a, d*b \in \ZZ^n
-- 2) clear the negative values: d*a + c, d*b + c \in \NN^n
-- 3) compare x^(d*a + c) and x^(d*b + c) in the polynomial ring

orderedQQn = method()

-- Create a orderedQQn from a ring R, using the monomial order of the ring
orderedQQn(PolynomialRing) := R -> (
    n := numgens R;
    ordMod := new OrderedQQn of OrderedQQVector from QQ^n;
    ordMod.cache.Ring = R;
    ordMod
)

-- Create an orderedQQn from an integer (the rank) and a monomial order
orderedQQn(ZZ, List) := (n, monOrder) -> (
    R := QQ[Variables => n, MonomialOrder => monOrder];
    ordMod := orderedQQn R;
    ordMod
)

-- Two ordered modules are equal iff their cached rings are identical
OrderedQQn == OrderedQQn := (N, M) -> (
    N.cache.Ring === M.cache.Ring
)

-- Informative afterprint identifying the module as an ordered module
OrderedQQn#{Standard,AfterPrint} =
OrderedQQn#{Standard,AfterNoPrint} = M -> (
    << endl; -- double space
    << concatenate(interpreterDepth:"o") << lineNumber;
    << " : Ordered QQ^" | toString (numgens M) | " module" << endl
);

-- An OrderedQQVector is an element of an OrderedQQn
-- This identifies that this comes from an ordered module
OrderedQQVector#{Standard,AfterPrint} =
OrderedQQVector#{Standard,AfterNoPrint} = v -> (
    M := module v;
    << endl; -- double space
    << concatenate(interpreterDepth:"o") << lineNumber;
    << " : Ordered QQ^" | toString (numgens M) | " module" << endl
);

-- comparison of ordered vectors
OrderedQQVector ? OrderedQQVector := (a, b) -> (
    M := class a;
    N := class b;
    assert(M == N);
    assert(instance(M, OrderedQQn));

    d := lcm((entries a | entries b)/denominator);
    aScaled := d*a;
    bScaled := d*b;

    R := M.cache.Ring;
    c := for i from 0 to numgens R-1 list min(aScaled_i, bScaled_i, 0);

    aMonomial := product for i from 0 to numgens R-1 list (R_i)^(sub(aScaled_i - c_i,ZZ));
    bMonomial := product for i from 0 to numgens R-1 list (R_i)^(sub(bScaled_i - c_i,ZZ));

    if aMonomial < bMonomial then symbol <
    else if aMonomial > bMonomial then symbol >
    else symbol ==
)

-- Comparisons with infinity
OrderedQQVector == InfiniteNumber := (a, b) -> false
InfiniteNumber ==  OrderedQQVector := (a, b) -> false

-- A function that takes a monomial and an ordered QQ-module and returns the
-- exponent vector of the monomial as a vector in the passed QQ-module
monomialToOrderedQQVector = method()
monomialToOrderedQQVector (RingElement, OrderedQQn) := (monomial, orderedQQModule) -> (
    exponentVector := vector flatten exponents monomial;
    modGens := gens orderedQQModule;
    modGens*exponentVector
)

--------------------------------------------------------------------------------
------------------------- Built-in Valuation Functions -------------------------
--------------------------------------------------------------------------------

-- Trivial Valuation.  Always returns 0, any input is valid
trivialValuation = valuation (x -> if x == 0 then infinity else 0)

-- Returns the number of times that p divides x
countPrimeFactor = method()
countPrimeFactor (ZZ, ZZ) := (p, x) -> (
    numFactors := 0;
    while x % p == 0 do (
        x = x // p;
        numFactors = numFactors + 1
    );
    numFactors
)

-- p-adic Valuation valuation construction
-- Allows for inputs to be rationals and computes difference of the
-- valuations for the numerator and denominator
padicValuation = method()
padicValuation ZZ := p -> (
    if not isPrime p then error "expected a prime integer";
    func := x -> (
        if x == 0 then infinity
        else countPrimeFactor(p, numerator x_QQ) - countPrimeFactor(p, denominator x_QQ)
    );
    valuation(func,QQ,QQ)
)

-- Leading Term Valuation,
-- returns negative of exponent of leading term of the input polynomial.
leadTermValuation = method()
leadTermValuation PolynomialRing := R -> (
    monOrder := (options R).MonomialOrder;
    orderedMod := orderedQQn(R);
    valFunc := f -> (if f == 0 then infinity else (-1)*monomialToOrderedQQVector(leadTerm f, orderedMod));
    internalValuation(valFunc, R, orderedMod)
)

-- Lowest Term Valuation,
-- returns lowest term of the input polynomial.
lowestTermValuation = method()
lowestTermValuation PolynomialRing := R -> (
    monOrder := (options R).MonomialOrder;
    orderedMod := orderedQQn(R);
    valFunc := f -> (
        if f == 0_R then infinity
        else monomialToOrderedQQVector((sort flatten entries monomials f)_0, orderedMod)
    );
    internalValuation(valFunc, R, orderedMod)
)

-- returns the highest power of the maximal ideal m containing x
getMExponent = method()
getMExponent (Ideal, RingElement) := (m, x) -> (
    numFactors := 0;
    n := m;
    while x % n == 0 do (
        numFactors = numFactors + 1;
        n = n*m;
    );
    numFactors
)

-- constructs the local ring valuation
-- allows for inputs to be in the fraction field of R
localRingValuation = method()
localRingValuation LocalRing := R -> (
    m := R.maxIdeal;
    S := ring m;
    func := x -> (
        if x == 0 then infinity
        else getMExponent(m, sub(x, S))
    );
    valuation(func, R, ZZ)
)

--------------------------------------------------------------------------------
------------------ Kaveh-Manon prime cones valuation ---------------------------
--------------------------------------------------------------------------------
-- internalTropicalVariety -- simple tropical variety computation
-- input:
-- I : Ideal: prime, homogeneous
--
-- output:
-- T : tropical polyhedral fan trop(I) without weights
--     uses max-convention (from gfan)

-- A method for tropical varieties to avoid the tropical package
-- There is a naming conflict with the tropical package, once that is resolved,
-- this method can be merged with that one.
internalTropicalVariety = method(
    Options => {
        "Convention" => "Max"
    }
)

-- Construct a tropical variety from an ideal
-- the convention can be "Max" or "Min"
-- stores the tropical variety in the cache
-- this code could be deleted when the conflict with the Tropical
-- package is resolved
internalTropicalVariety Ideal := opts -> I -> (
    if not I.cache#?("TropicalVariety", opts) then (
        startCone := gfanTropicalStartingCone I;
        T := (gfanTropicalTraverse startCone)_0;
        if opts#"Convention" == "Max" then (
            -- use default output of gfan
        )
        else if opts#"Convention" == "Min" then (
            -- negate the rays
            T = fan(-rays T, linealitySpace T, maxCones T);
        )
        else (
            error("-- Unknown value for option 'Convention', use 'Max' or 'Min'");
        );
        I.cache#("TropicalVariety", opts) = T;
    );
    I.cache#("TropicalVariety", opts)
)

-- Searches through the cones to find the prime cones.
primeConesOfIdeal = method()
primeConesOfIdeal Ideal := I -> (
    -- When the conflict with the Tropical package is resolved,
    -- the following code can be used
    -- F:=tropicalVariety(I, IsHomogeneous=>true,Prime=>true);
    F := internalTropicalVariety(I, "Convention" => "Min");
    r := rays F;
    c := maxCones F;
    cns := for i in c list r_i;
    inCns := for c in cns list (flatten entries(c * transpose matrix{toList(numColumns(c) : 1)}));
    L := for i from 0 to #cns-1 list (
        H := gfanInitialForms(first entries gens I, -1*(inCns#i), "ideal" =>true);
        if binomialIsPrime ideal H then cns#i
    );
    delete(null,L)
)

-- Searches through the prime cones of the kernel of the
-- presentation map.
primeConesOfSubalgebra = method()
primeConesOfSubalgebra Subring := A -> (
    I := ker A#"presentationMap";
    primeConesOfIdeal I
)

-- Given a set of rays of a 2D cone,
-- construct interior points of the cone that span it (as a vector space)
coneToMatrix = method()
coneToMatrix Matrix := coneRays -> (
    independentConeRays := getMaxIndependent(coneRays);
    onevector := matrix {toList ((numcols independentConeRays):1)};
    coeffs := 1 + (transpose onevector * onevector);
    coeffs*(transpose independentConeRays)
)

-- Construct a maximal set of independent columns of a matrix
-- compute the pivot columns to obtain a maximal linearly independent subset of columns of M
getMaxIndependent = method()
getMaxIndependent Matrix := M -> (
    R := reducedRowEchelonForm(sub(M, QQ));
    P := delete(infinity,apply(entries R,i->min(positions(i,j->j!=0))));
    M_P
)

-- Scale the rows of a list of matrices
-- Using a multiple of the lineality space
-- of a tropical variety f
positivity = method()
positivity (Fan, List) := (f, matL) -> (
    l := transpose linealitySpace(f);
    finalScaledMats := {};
    matList := for i from 0 to #matL-1 list entries matL_i;
    for i from 0 to #matList-1 do (
        scaledRows := {};
        for j from 0 to #(matList_i)-1 do (
            coeff := -1*min apply(#(matList_i)_j, k -> (((matList_i)_j)_k)/(flatten entries l)_k);
            scaledRows = append(scaledRows, (1/gcd(flatten entries (coeff*l + matrix{(matList_i)_j})))*(coeff*l + matrix{(matList_i)_j}));
        );
        mat := scaledRows_0;
        for i from 1 to #scaledRows-1 do mat = mat || scaledRows_i;
        finalScaledMats = append(finalScaledMats, mat);
    );
    finalScaledMats
)

-- Construct a quasivaluation from a prime cone using the construction in Kaveh-Manon
-- Technically, the result is only a quasivaluation since it might not be a multiplicative homomorphism
coneToValuation = method()
coneToValuation (Matrix, Subring) := (coneRays, A) -> (coneToValuation(coneRays, A, presentationRing A))
coneToValuation (Matrix, Subring, Ring) := (coneRays, A, S) -> (
    -- When the conflict with the Tropical package is resolved,
    -- the following code can be used, although it should be cached instead of recomputed
    --F := tropicalVariety(I, IsHomogeneous=>true,Prime=>true);
    I := ker A#"presentationMap";
    F := internalTropicalVariety(I, "Convention" => "Min");
    M := coneToMatrix(coneRays);
    scaledM := (positivity(F, {-M}))/(i -> sub(i, ZZ));
    weightList := for row in entries scaledM_0 list Weights => row;
    ncols := numcols scaledM_0;
    e := symbol e;
    T := QQ[e_1..e_ncols, MonomialOrder => weightList];
    val := leadTermValuation(T);
    orderedM := orderedQQn(#weightList, {Lex});
    func := (f -> (
            m := map(T, S, gens T);
            valf := val(m f);
            if valf == infinity then infinity else (
                (gens orderedM)*(scaledM_0)*(valf)
            )
        )
    );
    valS := valuation(func, S, orderedM);
    valS.cache#"Ideal" = I;
    valS.cache#"Subalgebra" = A;
    valS
)

-- construct the new valuation on the quotient by taking min of preimages
-- This turns a quasivaluation into a valuation using construction in Kaveh-Manon
valM = method()
valM (Ring, Valuation) := (T, valMTwiddle) -> (
    valMfunc := (g) -> (
    A := valMTwiddle.cache#"Subalgebra";
    S := valMTwiddle#source;

    numberVariables := numcols vars T;
    numberGenerators := numcols vars S;
    tensorVariables := monoid[Variables => numberVariables + numberGenerators,
                                MonomialOrder => Eliminate numberVariables];
    tensorRing := (coefficientRing T) tensorVariables;

    includeT := map(tensorRing, T, (gens tensorRing)_{0 .. numgens T -1});
    includeS := map(tensorRing, S, (gens tensorRing)_{numberVariables .. numgens tensorRing - 1});

        generatingVariables := (vars tensorRing)_{numberVariables..numberVariables + numberGenerators - 1};
    I := ideal(generatingVariables - includeT gens A); -- need a map to include

    f := includeS (valMTwiddle.cache#"Ideal");

    m := map(S, tensorRing, matrix{{0,0,0}} | matrix {gens S});
        gTwiddle := m ((includeT g) % I);
        RtoS := map(S, tensorRing, {0_S, 0_S, 0_S} | gens S);
        maxTwiddle := gTwiddle % (RtoS f);
        valMTwiddle(maxTwiddle)
    );
    valuation(valMfunc, T, valMTwiddle#target)
)

--------------------------------------------------------------------------------
-------------------------------- Documentation ---------------------------------
--------------------------------------------------------------------------------

beginDocumentation()

doc ///
     Key
         "trivialValuation"
     Headline
         The trivial valuation
     Usage
         v = trivialValuation
     Outputs
         v:Valuation
             the trivial valuation
     Description
       Text
           This valuation returns zero for all nonzero inputs.
       Example
           v = trivialValuation;
           v (-13)
           v 100000000
           v (14/23)
           v 0
     SeeAlso
         valuation
         Valuation
         leadTermValuation
         lowestTermValuation
         localRingValuation
         padicValuation
     ///

doc ///
     Key
         padicValuation
         (padicValuation, ZZ)
     Headline
         The p-adic valuation
     Usage
         v = padicValuation(p)
     Inputs
         p:ZZ
             a prime
     Outputs
         v:Valuation
             p-adic valuation using prime p
     Description
       Text
           This function constructs a valuation which returns
           the number of times that $p$ divides the numerator
           minus the number of times that $p$ divides the denominator.
       Example
           v = padicValuation 7;
           v 98
           v (2/7)
           v 0
           v (-42)
     SeeAlso
         valuation
         Valuation
         leadTermValuation
         lowestTermValuation
         localRingValuation
         "trivialValuation"
     ///

doc ///
     Key
        lowestTermValuation
        (lowestTermValuation, PolynomialRing)
     Headline
        The valuation defined by lowest terms
     Usage
         v = lowestTermValuation
     Inputs
         R:PolynomialRing
            the ring whose term order is used to define the valuation
     Outputs
         v:Valuation
            the lowest term valuation
     Description
       Text
           This function builds a valuation which returns the exponent vector of the
           lead term of a polynomial with respect to the ring's term order.
           The valuation returns vectors in an @TT "ordered $\\QQ$-module"@,
           which respects the monomial order of the
           @TO "PolynomialRing"@. For more details see @TO "Ordered modules"@.
       Example
           R = QQ[a,b,c, MonomialOrder => Lex];
           vR = lowestTermValuation R;
           f = 13*a^2*b + a*c^3;
           g = 5*a^2*c + b^3;
           vR f
           vR f < vR g
           S = QQ[a,b,c, MonomialOrder => RevLex, Global => false];
           vS = lowestTermValuation S;
           f = 13*a^2*b + a*c^3;
           g = 5*a^2*c + b^3;
           vS f
           vS f < vS g
     SeeAlso
         valuation
         Valuation
         leadTermValuation
         localRingValuation
         padicValuation
         "trivialValuation"
     ///

doc ///
     Key
         valuation
         (valuation, Function)
         (valuation, Function, Ring, Ring)
         (valuation, Function, Ring, Subring)
         (valuation, Function, Ring, LocalRing)
         (valuation, Function, Ring, RingOfInvariants)
         (valuation, Function, Ring, OrderedQQn)
         (valuation, Function, Subring, Ring)
         (valuation, Function, Subring, Subring)
         (valuation, Function, Subring, LocalRing)
         (valuation, Function, Subring, RingOfInvariants)
         (valuation, Function, Subring, OrderedQQn)
         (valuation, Function, LocalRing, Ring)
         (valuation, Function, LocalRing, Subring)
         (valuation, Function, LocalRing, LocalRing)
         (valuation, Function, LocalRing, RingOfInvariants)
         (valuation, Function, LocalRing, OrderedQQn)
         (valuation, Function, RingOfInvariants, Ring)
         (valuation, Function, RingOfInvariants, Subring)
         (valuation, Function, RingOfInvariants, LocalRing)
         (valuation, Function, RingOfInvariants, RingOfInvariants)
         (valuation, Function, RingOfInvariants, OrderedQQn)
     Headline
         User-defined valuation object
     Usage
         v = valuation(f)
         v = valuation(f, S, T)
     Inputs
         f:Function
           the valuation function.
         S:{Ring,LocalRing,Subring}
           the source
         T:{Ring,LocalRing,Subring}
           the target
     Outputs
         v:Valuation
            user-defined valuation function
     Description
         Text
             Construct a user defined valuation function.
             User-defined functions are not checked for satisfying the
             properties of a valuation.
             It is not necessary to specify a source or target, but
             if they are provided, then the input is checked to
             be in the source
             (or either promotable to the source or in the fraction field of the source).
             For common use cases, it is suggested to use the
             provided valuations.
         Example
             v = valuation(x -> if x == 0 then infinity else 0)
             v = valuation(x -> if x == 0 then infinity else 0, ZZ, ZZ)
     SeeAlso
          lowestTermValuation
              padicValuation
              "trivialValuation"
              leadTermValuation
              localRingValuation
///

doc ///
     Key
        leadTermValuation
        (leadTermValuation, PolynomialRing)
     Headline
        The valuation defined by leading terms
     Usage
         v = leadTermValuation R
     Inputs
         R:PolynomialRing
            the ring whose term order is used to define the valuation
     Outputs
         v:Valuation
            the lead term valuation
    Description
       Text
           This function constructs a valuation which
           returns the negative exponent vector of the
           lead term of a polynomial with respect to the ring's term order.
           The valuation returns vectors in an @TT "ordered $\\QQ$-module"@,
           which respects the monomial order of the
           @TO "PolynomialRing"@. For more details see @TO "Ordered modules"@.
       Example
           R = QQ[a,b,c, MonomialOrder => Lex];
           v = leadTermValuation R;
           f = 13*a^2*b + a*c^3;
           g = 5*a^2*c + b^3;
           v f
           v g
           v f < v g
    SeeAlso
      valuation
      Valuation
      localRingValuation
      lowestTermValuation
      padicValuation
      "trivialValuation"
///

doc ///
     Key
         localRingValuation
         (localRingValuation, LocalRing)
     Headline
         The valuation defined by a local ring.
     Usage
         v = localRingValuation(R)
     Inputs
         R:LocalRing
             the ring whose maximal ideal determines the order
     Outputs
         v:Valuation
             local ring valuation using a local ring R
     Description
       Text
           This function constructs a valuation which
           returns the largest power of the maximal ideal
               of R that contains the input to the valuation.
       Example
           R = QQ[x,y];
           I = ideal(x,y);
           S = R_I
           localVal = localRingValuation(S)
           localVal(1 + x + y)
           localVal(x^4 + x^2*y^2 + x^7 + y^3)
           localVal(x^2 + x*y + y^2)
       Text
           This valuation may be applied to elements of the fraction field of R,
           where the value of the valuation is the difference
           between the valuations of the numerator and denominator.
       Example
           localVal(1/(x^2+x*y+y^3))
     SeeAlso
         valuation
         Valuation
         leadTermValuation
         lowestTermValuation
         padicValuation
         "trivialValuation"
     ///

     -*
doc ///
     Key
         getMExponent
         (getMExponent, Ideal, RingElement)
     Headline
         Finds the smallest power of an ideal that a given ring element belongs to.
     Usage
         n = getMExponent(I, r)
     Inputs
         I:Ideal
             An ideal of R
         r:RingElement
             An element of R
     Outputs
         n:ZZ
             Smallest n such that r is in I^n
     Description
       Text
           Returns the smallest power of the ideal $I$ that contains $r$
       Example
           R = QQ[x,y];
           I = ideal(x,y);
           getMExponent(I, 1 + x + y)
           getMExponent(I, x^4 + x^2*y^2 + x^7 + y^3)
           getMExponent(I, x^2 + x*y + y^2)
     SeeAlso
        localRingValuation
     ///
     *-

doc ///
      Key
          Valuations
      Headline
          A package for constructing and using valuations.
      Description
        Text
          A valuation is a function $v:R\rightarrow G\cup\{\infty\}$
          where $R$ is a ring and $G$ is a linearly ordered group with
          the following properties:
        Text
          @UL {{"$v(ab)=v(a)+v(b)$,"},
          {"$v(a+b)\\geq\\min\\{v(a),v(b)\\}$, and"},
          {"$v(a)=\\infty$ iff $a=0$."}}@
        Text
          The @TT "Valuations"@ package provides uniform constructions of
          common valuations and also offers user-defined valuations.
          A valuation acts like @ofClass Function@,
          but may contain extra information.
        Example
          pval = padicValuation 3;
          pval(54)
          pval(2)
          R = QQ[x,y];
          leadval = leadTermValuation R;
          leadval(x^3+3*x^3*y^2+2*y^4)
          lowestval = lowestTermValuation R;
          lowestval(x^3+3*x^3*y^2+2*y^4)
          lowestval(0)
      ///

doc ///
      Key
            Valuation
      Headline
            The type of all valuations
      Description
          Text
            @TT "Valuation"@ is a type that contains the data needed
            to evaluate a @TT "valuation"@.
            A valuation is a function $v:R\rightarrow G\cup\{\infty\}$
            where $R$ is a ring and $G$ is a linearly ordered group with
            the following properties:
          Text
            @UL {{"$v(ab)=v(a)+v(b)$,"},
            {"$v(a+b)\\geq\\min\\{v(a),v(b)\\}$, and"},
            {"$v(a)=\\infty$ iff $a=0$."}}@
          Text
            This package provides common valuations and user-defined valuations.
      SeeAlso
          valuation
          leadTermValuation
          lowestTermValuation
          localRingValuation
          padicValuation
          "trivialValuation"
      ///

doc ///
     Key
        "Ordered modules"
        (symbol ==, OrderedQQn, OrderedQQn)
     Headline
         Overview of the ordered module $\QQ^n$
     Description
       Text
           Many standard valuations take values in a totally ordered subgroup $\Gamma \subseteq \QQ^n$.
           These standard valuations implement @ofClass OrderedQQn@, whose order is based on the
           monomial order of a given ring $R$.
           The values in $\QQ^n$ are compared using the monomial order of $R$.
           By default, our valuations use the min convention, that is $v(a + b) \ge \min(v(a), v(b))$.
       Example
           R = QQ[x,y];
           I = ideal(x,y);
           v = leadTermValuation R;
           a = v(x)
           b = v(y)
           c = v(x+y)
           a > b
           a == c
       Text
           Two ordered $\QQ^n$ modules are equal if they are
           built from the same ring. Note that isomorphic rings with the
           same term order may not be equal.
       Example
           M1 = orderedQQn(3, {Lex})
           R = M1.cache.Ring
           M2 = orderedQQn R
           M1 == M2
           S = QQ[x_1 .. x_3, MonomialOrder => {Lex}]
           M3 = orderedQQn S
           M1 == M3
     SeeAlso
         leadTermValuation
         lowestTermValuation
         OrderedQQn
         orderedQQn
///

doc ///
     Key
         valM
         (valM, Ring, Valuation)
     Headline
         Construct a valuation from a (quasi-)valuation
     Usage
          val = valuation(R, v)
     Inputs
          R: Ring
          v: Valuation
     Outputs
          val: Valuation
     Description
       Text
           Constructs a valuation from a (quasi-)valuation following the
           approach in Kaveh and Manon, 2019.
           In particular, the maximum quasi-valuation of all preimages of
           the input is taken as the valuation.
       Example
            R = QQ[x_1, x_2, x_3];
            A = subring {
                x_1 + x_2 + x_3,
                x_1*x_2 + x_1*x_3 + x_2*x_3,
                x_1*x_2*x_3,
                (x_1 - x_2)*(x_1 - x_3)*(x_2 - x_3)
                };
            C = primeConesOfSubalgebra A;
            v = coneToValuation(C#0, A);
            vA = valM(R, v)
            use R;
            vA(x_1^2 + x_2^2 + x_3^2)
            vA((x_1^2 - x_2^2)*(x_1^2 - x_3^2)*(x_2^2 - x_3^2))
            vA(0_R)
       Text
            For elements not in A, the valuation returns unreliable results
            because the valuation does not come from a weight valuation
            on R
       Example
            vA(x_2)
            vA(x_2^2)
            vA(x_2^3)
     SeeAlso
        coneToValuation
        primeConesOfIdeal
        primeConesOfSubalgebra
     References
            @HREF("https://epubs.siam.org/doi/10.1137/17M1160148", {"K. Khovanskii and C. Manon.  ",
                "Khovanskii Bases, Higher Rank Valuations, and Tropical Geometry.",
                EM "SIAM Journal on Applied Algebra and Geometry", ", 3(2), 2019."})@

///

doc ///
     Key
        coneToValuation
        (coneToValuation, Matrix, Subring)
        (coneToValuation, Matrix, Subring, Ring)
     Headline
        Convert a prime cone of a tropical ideal to a (quasi-)valuation
     Description
       Text
            This function constructs a valuation from the prime cone of a tropical variety.
            From a @TO Subring@ and the rays of a prime cone of the kernel of its @TO presentationRing@,
            a (quasi-)valuation is constructed.  A quasivaluation satisfies $\nu(fg)\geq\min\{\nu(f),\nu(g)\},$
            where the inequality replaces the equality.
       Example
            R = QQ[x_1, x_2, x_3];
            A = subring {
                x_1 + x_2 + x_3,
                x_1*x_2 + x_1*x_3 + x_2*x_3,
                x_1*x_2*x_3,
                (x_1 - x_2)*(x_1 - x_3)*(x_2 - x_3)
                };
            C = primeConesOfSubalgebra A
            val = coneToValuation(C#0, A)
            use A#"presentationRing";
            val(p_0^2 + p_1*p_2 - p_3^3)
     SeeAlso
        valM
        Subring
        presentationRing
///

doc ///
     Key
       primeConesOfSubalgebra
       primeConesOfIdeal
       (primeConesOfSubalgebra,Subring)
       (primeConesOfIdeal, Ideal)
     Headline
        Finds the prime cones of the tropicalization of a given subalgebra or ideal.
     Usage
        C = primeConesOfSubalgebra(S)
        C = primeConesOfIdeal(I)
     Inputs
        S: Subring
        I: Ideal
     Outputs
        C: List
            containing the ray-generators of the prime cones.
     Description
       Text
         Let $I \subset k[x]$ be a prime ideal and let $C \subset \mathcal{T}(I)$ be an open cone in
         the tropicalization of $I$.  This function returns all such $C$ where the initial ideal
         $\operatorname{in_{C}}(I)$ is a prime ideal.  When the input is a @TO Subring@ which
         is a domain, then $I$ is the kernel of the presentation map of $S$.
       Example
            R = QQ[x_1, x_2, x_3];
            A = subring {
                x_1 + x_2 + x_3,
                x_1*x_2 + x_1*x_3 + x_2*x_3,
                x_1*x_2*x_3,
                (x_1 - x_2)*(x_1 - x_3)*(x_2 - x_3)
                };
            primeConesOfSubalgebra A
            I = ideal(x_1*x_2+x_2^2+x_3^2);
            primeConesOfIdeal I
     SeeAlso
       coneToValuation
       valM
///

-*
doc ///
     Key
        positivity
        (positivity,Fan,List)
     Headline
        Scale the rows of a list of matrices based on a tropical variety.
     Description
       Text
         Given a list of matrices, this function scales each matrix by a positive
         vector in the lineality space of the a given tropical variety.
///
*-

doc ///
     Key
         OrderedQQn
     Headline
         The class of all ordered modules $\QQ^n$
     Description
       Text
           For an introduction see @TO "Ordered modules"@. Every element of
           an ordered $\QQ^n$ module is @ofClass OrderedQQVector@. A new
           ordered $\QQ^n$ module is created with the function @TO "orderedQQn"@.
       Example
           R = QQ[x_1 .. x_4, MonomialOrder => Lex]
           M = orderedQQn R
           M_0, M_1, M_2, M_3
           M_0 < M_1
           M_0 + M_3 < M_1 + M_2
     SeeAlso
         "Ordered modules"
         orderedQQn
         OrderedQQVector
///

doc ///
     Key
         orderedQQn
         (orderedQQn, PolynomialRing)
         (orderedQQn, ZZ, List)
     Headline
         Construct an ordered module $\QQ^n$
     Usage
         M = orderedQQn R
         M = orderedQQn(n, monomialOrders)
     Inputs
         R:PolynomialRing
             polynomial ring for the construction
         n:ZZ
             rank of the module
         monomialOrder:List
             monomial order for comparison
     Outputs
         M:OrderedQQn
     Description
       Text
           For an overview see @TO "Ordered modules"@.
           Let $R$ be @ofClass PolynomialRing@ with $n$ variables $x_1 \dots x_n$.
           Then the corresponding ordered $\QQ^n$ module has the following
           ordering. Suppose that $v, w \in \QQ^n$.
           Let $d \in \ZZ$ be a positive integer and $c \in \ZZ^n_{\ge 0}$
           be a vector such that $dv + c$ and $dw + c$ have non-negative
           entries. Then $v < w$ if and only if $x^{dv + c} > x^{dw + c}$
           in $R$. Note that this property does not depend on the choices of $c$ and
           $d$, so we obtain a well-defined order on $\QQ^n$.

       Example
           R = QQ[x_1 .. x_3, MonomialOrder => Lex]
           M = orderedQQn R
           v = 1/2 * M_0 - 1/3 * M_1
           w = 1/2 * M_0 + 1/4 * M_2
           v < w

       Text
           Instead of supplying @ofClass PolynomialRing@, we may supply
           the rank $n$ of the module along with a monomial order.
           The constructor creates the ring $R$ with $n$ variables and the
           given monomial order to construct the @TO "OrderedQQn"@ module

       Example
           N = orderedQQn(3, {Lex})
           R = N.cache.Ring
           N' = orderedQQn R
           N == N'

       Text
           $N$ and $N'$ are the same module
           because they are built from the same ring. See @TO (symbol ==, OrderedQQn, OrderedQQn)@.

     SeeAlso
         "Ordered modules"
         OrderedQQn
///

doc ///
     Key
         OrderedQQVector
         "OrderedQQVector ? OrderedQQVector"
         "OrderedQQVector == InfiniteNumber"
         "InfiniteNumber == OrderedQQVector"
         "(symbol ==, OrderedQQVector, InfiniteNumber)"
         "(symbol ==, InfiniteNumber, OrderedQQVector)"
     Headline
         The class of all vectors of an ordered module $\QQ^n$
     Description
       Text
           For an introduction see @TO "Ordered modules"@. Every ordered $\QQ^n$
           vector belongs to @ofClass OrderedQQn@. The ordered $\QQ^n$ vectors
           are most easily accessed though the original module.
       Example
           M = orderedQQn(3, {Lex})
           M_0 + 2 * M_1 + 3 * M_2
       Text
           Any pair of vectors of a module of type @TO "OrderedQQn"@ may be
           compared with <, >, and ==.
       Example
           M = orderedQQn(3, {GLex})
           2*M_1 < M_0 + M_2
           3*M_1 < M_0 + M_2
       Text
           The image of $0$ under a valuation is $\infty$, so it may be
           necessary to test whether an element of an ordered module $\QQ^n$
           is equal to the valuation of $0$.
       Example
            M = orderedQQn(3, {Lex})
            M_0 < infinity
            M_0 == infinity
     SeeAlso
         "Ordered modules"
         OrderedQQn
         orderedQQn
///




--------------------------------------------------------------------------------
------------------------------------ Tests -------------------------------------
--------------------------------------------------------------------------------

-- Trivial Valuation Tests
TEST ///
-- Everything should have valuation 0 except 0
val = trivialValuation
assert(val 0 == infinity)
assert(val 5 == 0)
assert(val (-9/2) == 0)
///

-- p-adic Valuation Tests
TEST ///
val = padicValuation(7)
assert(val 0 == infinity)
assert(val 7 == 1)
assert(val (-7) == 1)
assert(val (-9/7) == -1)
assert(val (7/9) == 1)
///

-- Leading Term Valuation Tests
TEST///
R = QQ[x,y]
val = leadTermValuation(R)
assert(val(x) > val(y^2))
///
TEST///
R = QQ[x,y, MonomialOrder=>Lex]
val = leadTermValuation(R)
assert(val(x) < val(y^2))
///

-- Lowest Term Valuation tests
TEST///
R = QQ[x,y,MonomialOrder => Lex];
val = lowestTermValuation R;
assert(val(x^2 + x*y) > val(y^3 + x*y^4));
///

TEST///
R = QQ[x,y,z, Degrees => {1,2,3}, MonomialOrder => GLex];
val = lowestTermValuation R;
assert(val(x^2*y^2*z + x^7*y) > val(x*y*z^2 + y^3*z))
///

-- Local ring valuation tests
TEST///
R = QQ[x,y,z];
I = ideal (x,y);
S = R_I;
val = localRingValuation S;
assert(val(x+y) == 1)
assert(val(x+y+z) == 0)
assert(val(1/(x+y)) == -1)
///

-- Kaveh-Manon tests
TEST///
R = QQ[x_1, x_2, x_3];
A = subring {
    x_1 + x_2 + x_3,
    x_1*x_2 + x_1*x_3 + x_2*x_3,
    x_1*x_2*x_3,
    (x_1 - x_2)*(x_1 - x_3)*(x_2 - x_3)
    };
C = primeConesOfSubalgebra A;
v = coneToValuation(C#0, A);
vA = valM(R, v);
use presentationRing A;
assert(vA(A#"presentationMap" (p_1^3)) == vA(A#"presentationMap" (p_3^2)))
assert(vA(A#"presentationMap" (p_0^3*p_2)) > vA(A#"presentationMap" (p_3^2)))
///
end--


--------------------------------------------------------------------------------
-------------------------------- Dev Functions ---------------------------------
--------------------------------------------------------------------------------

buildPackage = x ->(
    uninstallPackage("Valuations");
    installPackage("Valuations", RunExamples => false);
    )

testPackage = x -> (
    uninstallPackage("Valuations");
    installPackage("Valuations");
    check Valuations;
    )

---------------------
restart
uninstallPackage "Valuations"
restart
installPackage "Valuations"
needsPackage "Valuations"
