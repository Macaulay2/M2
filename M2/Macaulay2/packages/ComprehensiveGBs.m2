newPackage(
    "ComprehensiveGBs",
    Version => "1.0",
    Date => "",
    Headline => "A package for computing Comprehensive Gröbner Bases (CGBs)",
    Authors => {
        { Name => "Agustina Cagliero", Email => "mariaagustina.cagliero@kuleuven.be", HomePage => "https://sites.google.com/view/mariaagustinacagliero/"},
        { Name => "Oliver Clarke", Email => "oliver.clarke@durham.ac.uk", HomePage => "https://www.oliverclarkemath.com"},
        { Name => "Woody Cohen", Email => "2597103@swansea.ac.uk", HomePage => ""},
        { Name => "Lorenzo De Biase", Email => "lorenzo.debiase@enea.it", HomePage => "https://sites.google.com/view/lorenzodebiase/"},
        { Name => "Angelo El Saliby", Email => "angelo.el.saliby@mis.mpg.de", HomePage => "angeloelsaliby.github.io"},
        { Name => "Giulia Gaggero", Email => "gaggerog@mcmaster.ca", HomePage => "https://www.giuliagaggero.com/"},
        { Name => "Sam Knight", Email => "samdeckardknight@gmail.com", HomePage => ""},
        { Name => "Weijia Wang", Email => "weijia.wang@lip6.fr", HomePage => "https://weijia.perso.lip6.fr/"}
    },
    Keywords => {"Groebner Basis Algorithms"},
    AuxiliaryFiles => false,
    PackageImports => {"MinimalPrimes", "RealRoots"},
    DebuggingMode => false,
    HomePage => "https://github.com/Macaulay2/Workshop-2026-Warwick/tree/CGBs"
    )

export {
    "comprehensiveGBMain",
    "comprehensiveGB",
    "comprehensiveGBOnGraph",
    "ReduceStrata",
    "parametricGBMain",
    "minimalDicksonBasis",
    "Depth",
    "CheckAssumption",
    "ICheckLoops"
} -- functions, objects to export


-- store all the maps and rings of a CGB computation in a object
CGBData = new Type of HashTable


ringOrder = method(); -- returns the monomial order of a polynomial ring
ringOrder PolynomialRing := List => R -> (
    select(toList (options R).MonomialOrder, orderEntry -> not member(first orderEntry, {MonomialSize, Position}))
);

cgbDataFromRings = method();

cgbDataFromRings Ring := CGBData => R -> (
    if R.cache#?"cgbData" then return R.cache#"cgbData";
    KU := coefficientRing R;
    K := coefficientRing KU;
    n := numgens R;
    m := numgens KU;
    x := local x;
    u := local u;
    l := local l;
    RExt := K[l, x_1..x_n, u_1..u_m, MonomialOrder => {Lex => 1} | ringOrder R | ringOrder KU];
    RFlat := K[x_1..x_n, u_1..u_m, MonomialOrder => ringOrder R | ringOrder KU];
    RExt' := KU[l, x_1..x_n, MonomialOrder => {Lex => 1} | ringOrder R];
    RFlatl := RFlat[l];
    RtoRExt := map(RExt, R, drop(gens RExt, 1));
    RExttoRFlatl := map(RFlatl, RExt, gens RFlatl | gens coefficientRing RFlatl);
    RExttoRExt' := map(RExt', RExt, gens RExt'| gens coefficientRing RExt');
    RExttoR := map(R, RExt, {1} | gens R | gens coefficientRing R);
    KUtoRFlat := map(RFlat, KU, take(gens RFlat, -#gens KU));
    RFlattoR := map(R, RFlat, gens R | gens coefficientRing R);
    KUtoR := map(R, KU, gens coefficientRing R);
    RtoRFlat := map(RFlat, R, gens RFlat);
    RFlattoKU := map(KU, RFlat, toList(n : 0_KU) | gens KU); -- used by eleminateVariables

    R.cache#"cgbData" = new CGBData from {
        "R"             => R,
        "RExt"          => RExt,
        "RFlat"         => RFlat,
        "RExt'"         => RExt',
        "KU"            => KU,
        "RFlatl"        => RFlatl,
        "RtoRExt"       => RtoRExt,
        "RExttoRFlatl"  => RExttoRFlatl,
        "RExttoRExt'"   => RExttoRExt',
        "RExttoR"       => RExttoR,
        "KUtoRFlat"     => KUtoRFlat,
        "RFlattoR"      => RFlattoR,
        "KUtoR"         => KUtoR,
        "RtoRFlat"      => RtoRFlat,
        "RFlattoKU"     => RFlattoKU
    }
);


listOfFactors = method() -- returns the list of factors of a ring element
listOfFactors RingElement := h -> (
    hfac := factor h;
    select(first \ toList hfac, t -> not isConstant t)
);

squareFreePart = method() -- returns the square free part of a ring element
squareFreePart RingElement := h -> (
    if zero h then return h;
    R := ring h;
    f := product listOfFactors h;
    return f_R
);



isConsistentRabinowitsch = method(); -- returns whether or not rad(E) intersect N is empty, using the Rabinowitsch trick
isConsistentRabinowitsch (List, List) := (E, N) -> (
    if isEmpty E then return not all(N, zero);
    M := rabinowitschMap ring E_0;
    y := last gens target M;
    any(N, f -> not isMember(1, ideal((M \ E) | {M f * y - 1})))
)

-- cache the map R -> R[y] used by the Rabinowitsch trick in R.cache
rabinowitschMap = R -> (
    if not R.cache#?"rabinowitschMap" then (
        u := local u;
        y := local y;
        S := (baseRing R)[u_1..u_(numgens R), y];
        R.cache#"rabinowitschMap" = map(S, R, drop(gens S, -1));
    );
    R.cache#"rabinowitschMap"
)



isCoveredPrime = method(); -- returns whether V(P) is covered by the strata in memo, with P being a prime ideal
isCoveredPrime (Ideal, List) := (P, memo) -> (
    j := position(memo, t -> all(t#0, s -> s % P == 0) and any(t#1, n -> n % P != 0));
    if j === null then return false;
    all(decompose(P + ideal (memo#j)#1), Q -> isCoveredPrime(Q, memo))
);



comprehensiveGBMain = method(
    Options => {
        ReduceStrata => false,
        Verbose => false,
        Depth => -1,
        CheckAssumption => true
        }
    ); -- Initialises comprehensiveGBMainRec

comprehensiveGBMain List := o -> F -> (
    R := ring first F;
    KU := coefficientRing R;
    S := first entries eliminateVariables(F, cgbDataFromRings R);
    cgs := comprehensiveGBMain(F, S, o ++ {CheckAssumption => false});
    if #S == 0 then return cgs;
    {({0_KU}, S, apply(S, s -> promote(s, R)))} | cgs
    )
comprehensiveGBMain (List, List) := o -> (F, S) -> (
    R := ring first F;
    KU := coefficientRing R;
    S' := apply(S, s -> (
            s' := sub(s, R);
            if not liftable(s', KU) then
            error("S must consist of polynomials in the parameters; found " | toString s);
            lift(s', KU)
            ));
    cgbData := cgbDataFromRings R;
    if o.CheckAssumption then (
        -- V(S) is contained in V(<F> cap k[U]) <=> (S, <F> cap k[U]) is inconsistent
        if isConsistentRabinowitsch(S', first entries eliminateVariables(F, cgbData)) then
        error "V(S) is not contained in V(ideal F cap k[U]); pass CheckAssumption => false to skip this check";
        );
    cgs := comprehensiveGBMainRec(F, S', {}, cgbData,
        ReduceStrata => o.ReduceStrata,
        Verbose => o.Verbose,
        Depth => o.Depth
        );
    apply(cgs, t -> (if #(t#0) == 0 then {0_KU} else t#0, t#1, t#2))
    )

comprehensiveGBMainRec = method(
    Options => {
        ReduceStrata => false,
        Verbose => false,
        Depth => -1
    }
);
comprehensiveGBMainRec (List, List, List, CGBData) := o -> (F, S, memo, cgbData) -> (
    R := cgbData#"R";
    RExt := cgbData#"RExt";
    KU := cgbData#"KU";
    RtoRExt := cgbData#"RtoRExt";
    RExttoRFlatl := cgbData#"RExttoRFlatl";
    RExttoRExt' := cgbData#"RExttoRExt'";
    RExttoR := cgbData#"RExttoR";
    S = first entries gens gb ideal S;
    if o.Verbose then (
        print("Computing CGB for F = " | toString F | " and S = " | toString S);
    );
    if 1 % ideal S == 0 then (
        return {}
    );
    l := first gens RExt;
    G := gens gb ideal join(
        apply(F, f -> l * RtoRExt f),
        apply(S, s -> (l-1) * RtoRExt s));

    n := numgens R;
    pruneIndices := positions(first entries RExttoRFlatl G, g ->
        leadMonomial g != 1 and
        any(support leadCoefficient g, v -> index v < n));
    pruneG := leadCoefficient \ first entries RExttoRExt' G_pruneIndices;
    h := lcm(pruneG | {1_KU});
    hfac := select(toList factor h, p -> not isConstant p#0);
    h = product({1_KU} | apply(hfac, value));
    branch := (S, {h}, first entries compress RExttoR G);
    if o.ReduceStrata then (
        memo |= {branch};
    );
    if pruneG == {} then (
        if o.ReduceStrata then (
            return memo
        ) else (
            return {branch}
        )
    );
    H := first \ hfac;
    if o.ReduceStrata then (
        if o.Depth != 0 then (
            for hi in H do (
                for P in decompose ideal append(S, hi) do (
                    if not isCoveredPrime(P, memo) then (
                        memo = comprehensiveGBMainRec(F, first entries gens gb P, memo, cgbData, o ++ {Depth => o.Depth -1});
                        )
                    )
                )
            );
        return memo
    ) else (
        return {branch} | if o.Depth == 0 then {} else flatten apply(H, hi -> comprehensiveGBMainRec(F, append(S, hi), memo, cgbData, o ++ {Depth => o.Depth -1}))
    );
);


comprehensiveGB = method( Options => {
    ReduceStrata => false,
    Verbose => false,
    Depth => -1
})
comprehensiveGB List := o -> F -> (
    R := ring first F;
    s := first entries eliminateVariables(F, cgbDataFromRings R);
    G := comprehensiveGBMain(F, s,
        ReduceStrata => o.ReduceStrata,
        Verbose => o.Verbose,
        Depth => o.Depth,
        CheckAssumption => false
    );
    -- scale each g so that the leading coefficient
    -- of the leading coefficient is 1 (to remove duplicates)
    normalForm := g -> (1 / leadCoefficient leadCoefficient g) * g;
    result := apply(s, f -> promote(f, R)) | flatten apply(G, t -> t#2);
    unique (normalForm \ select(result, g -> g != 0))
)


eliminateVariables = method()
eliminateVariables (List, CGBData) := (F, cgbData) -> (
    R := cgbData#"R";
    RtoRFlat := cgbData#"RtoRFlat";
    Gflat := gens gb ideal (RtoRFlat \ F);
    variableBlocks := select(ringOrder R, orderEntry -> first orderEntry =!= Weights);
    cgbData#"RFlattoKU" selectInSubring(#variableBlocks, Gflat)
)


comprehensiveGBOnGraph = method(
    Options => {
        ReduceStrata => false,
        Verbose => false,
        Depth => -1,
        CheckAssumption => true
        }
    )
comprehensiveGBOnGraph (List, ZZ) := opts -> (G, d) -> (
    V := sort G_0;
    E := G_1;
    x := local x;
    w := local w;
    SMonoid := monoid [apply(E, l -> w_(toSequence l))];
    S := QQ SMonoid;
    RMonoid := monoid [x_(V_0, 1)..x_(V_(#V-1), d)];
    R := S RMonoid;
    F := for i in E list (
        sum(1..d, k -> (
                R_(d*(i_0-V_0)+k-1)-R_(d*(i_1-V_0)+k-1)
                )^2
            ) - S_(position(E, j -> j === i)
            )
        );
    (F, comprehensiveGBMain(F, opts))
)

-- Given two lists A and B return the list
-- {a*b : a in A,  b in B}
totalListProduct = method();
totalListProduct (List, List) := (A, B) -> flatten table(A, B, times);


--------------------------------------------------
-- Implementing definition 4.1 of
-- "An efficient algorithm for computing a
-- comprehensive Gröbner system of a parametric
-- polynomial system", D. Kapur Y. Sun D. Wang,
-- J. of Symbolic Computation issue 49, 2013
--------------------------------------------------
minimalDicksonBasis = method();
minimalDicksonBasis List := G -> (
    if #G == 0 then return {};
    lpps := leadMonomial \ G;
    minimal := select(G, g -> not any(lpps, m -> m != leadMonomial g and leadMonomial g % m == 0));
    -- Section 7.1, first heuristic (prefer leading coefficients with low degree factors)
    simpler := lc -> max({0} | apply(listOfFactors lc, f -> first degree f));
    -- Section 7.1, second heuristic (prefer leading coefficients shared by many candidates)
    freq := tally apply(minimal, leadCoefficient);
    -- order the input by those two heuristics;
    -- if there are still ties, order by the keys (chosen purely arbitrarily) after them
    candidates := sort(minimal, g -> (lc := leadCoefficient g; (- freq_lc, simpler lc, #terms lc, first degree lc, toString g)));
    -- keep the first candidate for each leading monomial
    seen := new MutableHashTable;
    select(candidates, g -> (
        m := leadMonomial g;
        if seen#?m then false else seen#m = true)
    )
)
-*
Notes: new implementation of minimalDicksonBasis:

Recall that a minimal Dickson basis keeps exactly one polynomial for each leading monomial that is minimal under divisibility.
The previous version has a loop to do this: it adds polynomials one at a time, and later additions replace
earlier ones where the leading monomial is divisible by the new one.

For each element in the resulting list, it ...
>> has a minimal leading monomial, because the others were replaced or skipped once a divisor turns up,
>> is the first polynomial with that leading monomial in the heuristic order, because later ones with the same leading monomial are skipped,
>> appears in the heuristic order, because everything else in the resulting list just appears once, i.e., it's never removed and moved.

Since we already compute `minimal` for the second heuristic, we sort the list and keep the first polynomial of each leading monomial
*-

--------------------------------------------------
-- Implementing algorithm in Section 4.1 of
-- "An efficient algorithm for computing a
-- comprehensive Gröbner system of a parametric
-- polynomial system", D.Kapur Y. Sun D. Wang,
-- J. of Symbolic Computation issue 49, 2013
--------------------------------------------------
parametricGBMain = method(
    Options => {
        Depth => -1,
        ICheckLoops => infinity
        }
    );
parametricGBMain (List, List, List) := o -> (E, N, F) -> (
    if zero length F then error "F must be non-empty";
    R := ring first F;
    if not (instance(R, PolynomialRing) and instance(coefficientRing R, PolynomialRing)) then
    error "F must consist of polynomials in a ring k[U][X]";
    cgbData := cgbDataFromRings R;
    RFlat := cgbData#"RFlat";
    KU := cgbData#"KU";
    KUtoRFlat := cgbData#"KUtoRFlat";
    RFlattoR := cgbData#"RFlattoR";
    RtoRFlat := cgbData#"RtoRFlat";
    RFlattoKU := cgbData#"RFlattoKU";
    toKU := p -> try promote(p, KU) else
    error("E and N must consist of polynomials in the parameters; found " | toString p);
    E = if zero length E then {0_KU} else toKU \ E;
    N = if zero length N then {1_KU} else toKU \ N;
    if not consistencyCheckAllTogether(E, N, ICheckLoops => o.ICheckLoops) then (
        return {} -- The domain is empty
    );
    -- Compute the GB of union(E, F), but viewing the parameters as variables

    G := first entries gens gb ideal ((KUtoRFlat \ E) | (RtoRFlat \ F));
    if member(1_RFlat, G) then (
        return {{E, N, {1_R}}} -- Trivial case where the vanishing set is empty
        );

    -- Split G into the polynomials that only contain the parameters (true)
    -- and the others (false); the parameters are the last variables of RFlat
    n := numgens R;
    parts := partition(g -> all(support g, v -> index v >= n), G);
    Gr := apply(if parts#?true then parts#true else {}, g -> RFlattoKU g);
    -- Elements of GB that do not only contain parameters
    listDiff := apply(if parts#?false then parts#false else {}, g -> RFlattoR g);

    -- By convention, an empty list for a GB means that the
    -- corresponding vanishing set is the whole space,
    -- which is equivalent to only containing the 0 element.
    -- To keep track of the original rings down the line and in the output,
    -- we never return an empty GB.
    if zero length Gr then (
        Gr = {0_KU};
        );
    productList := unique totalListProduct(Gr, N); -- The list obtained by multiplying every element in Gr with every element in N
    if zero length productList then (
        productList = {0_KU};
        );
    PGB := {};
    if consistencyCheckAllTogether(E, productList, ICheckLoops => o.ICheckLoops) then (
        PGB = {{E, productList, {1_R}}};
        );
    if not consistencyCheckAllTogether(Gr, N, ICheckLoops => o.ICheckLoops) then (
        return PGB
        );
    Gm := minimalDicksonBasis listDiff;
    H := unique flatten apply(Gm, g -> listOfFactors leadCoefficient g);
    h := squareFreePart lcm(H | {1_KU});
    productList = unique (squareFreePart \ totalListProduct(N, {h}));
    if consistencyCheckAllTogether(Gr, productList, ICheckLoops => o.ICheckLoops) then (
        PGB = unique(PGB | {{Gr, productList, if length Gm == 0 then {0_R} else Gm}});
        );
    -- Depth controls the number of recursive calls:
    -- if Depth == 0 then we stop here, otherwise recurse with Depth = Depth - 1
    -- (default value for Depth is -1 never hits zero)
    if o.Depth == 0 then return PGB;
    o' := o ++ {Depth => o.Depth - 1};
    for i in 0..length H - 1 do (
        -- Both cCheck and iCheck require E to be a Gröbner basis of <E>
        E' := first entries gens gb ideal unique(Gr | {H_i});
        if length E' == 0 then (
            E' = {0_KU};
            );
        if i == 0 then (
            PGB = unique(PGB | parametricGBMain(
                        E',
                        N,
                        listDiff, o')
                )
            )
        else (
            PGB = unique(PGB | parametricGBMain(
                        E',
                        unique totalListProduct(N, {squareFreePart product H_{0..i-1}}),
                        listDiff, o')
                )
            );
        );
    return PGB
    );

parametricGBMain List := o -> F -> parametricGBMain({}, {}, F, o)
parametricGBMain (List, List) := o -> (E, F) -> parametricGBMain(E, {}, F, o)

-----------------------------
-- zeroDimCheck if for those
-- cases in which <E> is
-- zero dimensional.
-- It uses RealRoots.
-----------------------------

zeroDimCheck = method();
zeroDimCheck (List, RingElement) := (E, f) -> (
    I := ideal E;
    pf := characteristicPolynomial(f, I);
    d := first degree pf;
    lambda := first gens ring pf;
    if pf == lambda^d then
        false -- inconsistent
    else
        true -- consistent
);

-----------------------------
-- cCheck if for those
-- cases in which <E> is
-- of positive dimensional.
-----------------------------

cCheck = method();
cCheck (List, RingElement) := (E, f) -> (
    R := ring f;
    U := gens R;
    supports := apply(select(E, g -> g != 0), g -> (
        e := first exponents leadMonomial g;
        positions(e, k -> k != 0)
    ));

    V := {};

    for i from 0 to #U - 1 do (
        candidate := V | {i};

        if not any(supports, S -> all(S, j -> member(j, candidate))) then (
            V = candidate;
        );
    );
    alpha := apply(numgens R, i -> random(-100, 100));
    phi := map(R, R, for i from 0 to numgens R-1 list if member(i, V) then alpha_i else R_i);
    spE := gb (ideal (phi \ E) + ideal (gens R)_V);
    -- the above is a little different from KSW where they restrict to a smaller ring
    -- and check that spE is zero dimensional there
    -- Here we add in the variables in order to avoid constructing a new polynomial ring
    -- and hopefully the GB computation is just as fast
    fAlpha := phi f;
    GspE := flatten entries gens spE;
    if zero dim ideal GspE and zeroDimCheck(GspE, fAlpha) then (
        return true; -- certifies consistency
    );
    return false -- unknown consistency (i.e., it could still be consistent)
);


iCheck = method(
    Options => {
        ICheckLoops => infinity
    }
);

iCheck (List, RingElement) := o -> (E, f) -> (
    if zero f then (
        return true; -- certifies inconsistency
    );
    -- we do the computation in the quotient ring R/<E>
    -- this avoids extra costs when doing s := p^2 in the main loop
    if o.ICheckLoops === infinity then (
        return not isConsistentRabinowitsch(E, {squareFreePart f}); -- certifies inconsistency/consistency
    );
    Q := ring f / ideal E;
    if zero promote(f, Q) then (
        return true; -- certifies inconsistency
    );
    factors := listOfFactors f;
    promoted := factors / (g -> promote(g, Q));
    -- before performing iCheck on the input f, we first perform iCheck on the factors of f
    -- this is a not-too-expensive heuristic, but it proves very helpful for S5 and P3P when ICheckLoops is set to 4
    candidates := if zero length factors then {promote(f, Q)}
        else if #factors == 1 then promoted
        else sort(promoted, z -> #terms z) | {promote(product factors, Q)};
    for p in candidates do (
        if zero p then (
            return true; -- certifies inconsistency
        );
        for i from 1 to o.ICheckLoops do (
            s := p^2;
            if zero s then (
                return true; -- certifies inconsistency
            );
            if s == p then break;
            p = s;
        );
    );

    return false; -- unknown consistency
);

----------------------------------------
-- The following puts together ZeroDimCheck
-- cCheck AND iCheck
-- 1)check whether f is already in <E>
-- 2)determine the dimension of <E>
-- 3) zeroDimCheck or cCheck
-- 4) if cCheck is not enough, do iCheck
----------------------------------------

consistencyCheckAllTogether = method(
    Options => {
        ICheckLoops => infinity
    }
);

consistencyCheckAllTogether (List, RingElement) := o -> (E, f) -> (
    if zero (f % ideal E) then (
        if debugLevel > 0 then (
            print "inconsistent: direct ideal membership check was used";
            );
        return false; -- inconsistent
    );
    d := dim ideal E;
    if zero d then (
        if debugLevel > 0 then (
            print "zeroDimCheck was used";
            );
        return zeroDimCheck(E, f);
    );
    if cCheck(E, f) then (
        if debugLevel > 0 then (
            print "consistent: cCheck was used";
            );
        return true; -- consistent
    );
    if iCheck(E, f, ICheckLoops => o.ICheckLoops) then (
        if debugLevel > 0 then (
            print "inconsistent: iCheck was used";
            );
        return false; -- inconsistent
    );
    if o.ICheckLoops === infinity then (
        -- iCheck also checks consistency when ICheckLoops is infinity
        -- so if we reach this point, f is not in rad(E)
        return true; -- consistent
    );
    if debugLevel > 0 then (
       print "General check was used";
       );
    return null;
);

-- Note that we cannot have ideal E = (0) or ideal F = (0)
consistencyCheckAllTogether (List, List) := o -> (E, N) -> (
    if all(N, zero) then return false;
    if all(E, zero) then return true;
    undecided := false;
    for f in N do (
        check := consistencyCheckAllTogether(E, f, ICheckLoops => o.ICheckLoops);
        if check === true then (
            return true;
        );
        if check === null then (
            undecided = true;
        );
    );
    if undecided then (
        return isConsistentRabinowitsch(E, N)
    );
    return false
);


-* Documentation section *-

beginDocumentation()

doc ///
  Key
    ComprehensiveGBs
  Headline
    a package for computing Comprehensive Gröbner Bases (CGBs).
  Description
    Text
      This package provides the implementations of two different algorithms for computing a comprehensive Gröbner system and a
      comprehensive Gröbner basis of a parametric ideal. In the following we refer to Section 2 of @HREF("#ref1","[1]")@.

      Let $k$ be a field, let $R$ be the polynomial ring $k[U]$ in the parameters $U=\{u_1,\ldots,u_m\}$, and let $R[X]$ be the polynomial ring
      over $R$ in the variables $X=\{x_1,\ldots,x_n\}$, with $X\cap U=\emptyset$.
      Denote by $L$ the algebraic closure of $k$. Given $a \in L^m$, the specialization homomorphism of $R$ induced by $a$ is the homomorphism
      $\sigma_a:R\rightarrow L$ defined by $\sigma_a(f)=f(a)$.
      The homomorphism $\sigma_a$ extends canonically to a homomorphism $\sigma_a:R[X]\rightarrow L[X]$
      by applying $\sigma_a$ coefficient-wise.
      For $E\subseteq R=k[U]$, the variety defined by $E$ in $L^m$, denoted by $V(E)$, is
      $V(E)=\{a\in L^m :\sigma_a(f)=0 \text{ for all } f \in E \}.$

      A set $A\subseteq L^m$ is called constructible if there exists a pair of finite sets of polynomials $(E,N)$ such that
      $A=V(E)\setminus V(N)$, where $E,N\subseteq k[U]$.

      Let $F$ be a subset of $R[X]$, let $E_1,N_1,\ldots, E_t,N_t$ be subsets of $R=k[U]$, let $G_1,\ldots,G_t$ be subsets of $R[X]$,
      and let S be a subset of $L^m$ such that $S\subseteq (V(E_1)\setminus V(N_1) \cup \ldots \cup V(E_t)\setminus V(N_t)$. A finite set
      $\mathcal{G} = \{(E_1,N_1,G_1),\ldots,(E_t,N_t,G_t)\}$ is called a comprehnsive Gröbner system on $S$ for $F$ if
      $\sigma_a(G_i)$ is a Gröbner basis of the ideal $(\sigma_a(F))\subseteq L[X]$ for $a \in V(E_i) \setminus V(N_i)$ and
      $i=1,\ldots,t$. Each $(E_i,N_i,G_i)$ is called a branch of $\mathcal{G}$. In particular, if $S=L^m$, then $\mathcal{G}$ is
      called a comprehensive Gröbner system for F.

      Given a basis $S$ of the elimination ideal of $(F)\cap k[U]$, a comprehenvive Gröbner basis $\mathcal{B}$ for $F\subseteq R[X]$
      is the union of all the sets $G_i$ in a comprehensive Gröbner system $\mathcal{G}$ for F on $S$. Notice that $(\sigma_a(F))=(1)$
      for any $a \in L^m \setminus V(S)$. Therefore, the set $\mathcal{G} \cup \{(\emptyset,S,S)\}$ is a comprehensive Gröbner system for $F$.

      The function @TO "comprehensiveGBMain"@ is the implementation of Algorithm CGBMain of @HREF("#ref2","[2]")@. The function @TO "parametricGBMain"@
      corresponds to the Algorithm PGBMain of @HREF("#ref1","[1]")@.
  References
      @LABEL("[1]","id" => "ref1")@ Deepak Kapur, Yao Sun, and Dingkang Wang. 2013. An efficient algorithm for computing a comprehensive Gröbner system of a parametric polynomial system. In Journal of Symbolic Computation, 49, 27-44.

      @LABEL("[2]","id" => "ref2")@ Akira Suzuki and Yosuke Sato. 2006. A simple algorithm to compute comprehensive Gröbner bases using Gröbner bases. In Proceedings of the 2006 International Symposium on Symbolic and Algebraic Computation (ISSAC '06), 326–331.
///


doc ///
  Key
    comprehensiveGBMain
    (comprehensiveGBMain, List, List)
    (comprehensiveGBMain, List)
    [comprehensiveGBMain, Verbose]
    [comprehensiveGBMain, ReduceStrata]
    [comprehensiveGBMain, Depth]
    [comprehensiveGBMain, CheckAssumption]
  Headline
    a method that computes a Comprehensive Gröbner System
  Usage
    comprehensiveGBMain(F,S)
    comprehensiveGBMain F
  Inputs
    F :List
       a list of polynomials in a ring $R = k[U][X]$
    S :List
       a list of polynomials in a ring $RU = k[U]$
    CheckAssumption=>Boolean
       check that $V(S)\subseteq V(\langle F\rangle\cap k[U])$
    ReduceStrata=>Boolean
       ignore strata that have already been computed
    Verbose=>Boolean
       print polynomial lists during computation
    Depth=>ZZ
       maximum recursion depth
  Outputs
    G :List
      of Sequences of the form (E,N,G), where G is a Gröbner basis on the set
      $V(E)\setminus V(N)$
  Description
    Text
      Implementation of Algorithm CGBMain of @HREF("#ref1","[1]")@. Given a
      tower polynomial ring $R = k[U][X]$, for $U$ a set of parameters and
      $X$ a set of variables, and a finite set $F\subset R$ of polynomials in the variables and
      parameters, and given a finite set $S\subset k[U]$ satisfying $V(S)\subseteq
      V(\langle F\rangle\cap k[U])$, the method comprehensiveGBMain takes returns a comprehensive Gröbner system of the ideal generated by $F$ on $V(S)$. The ring where the elements of $F$ live must be initialised as a tower ring,
      as in the following example.
    Example
      R1 = QQ[a,b][x,y]
    Text
      Here $X = \{x,y\}$ and $U = \{a,b\}$. To find a
      comprehensive Gröbner system of $F = \langle ax+by\rangle$ over $\mathb{Q}^2$, 
      we input the following.
    Example
      F1 = {a*x+b*y}
      S1 = {}
      comprehensiveGBMain(F1,S1)
    Text
      The function comprehensiveGBMain has several options: ReduceStrata, Verbose, and Depth.

      The symbol ReduceStrata is an option to ignore computations on strata which
      have already been considered. This value is set to false by default.
      For smaller examples, changing this to true can reduce computation
      times, as for the following example. It will also give more easily
      parsable results.
    Example
      R2 = QQ[a,b][x,y,z];
      F2 = {x^2-a,y^3-b,x+y-z};
      S2 = {};
    Text
      Setting @TO "Verbose"@ to @TT "true"@ will display the current
      polynomial lists $F$ and $S$ in the internal computation.
    Example
      comprehensiveGBMain(F1,S1,Verbose=>true)
    Text
      The option @TO "Depth"@ sets a bound for the recursion depth of the
      algorithm. For instance, setting it to zero will return one of the
      generic stratum.
    Example
      for i from 0 to 3 do print (i, netList comprehensiveGBMain(F1, S1, Depth => i))
    Text
      When the function is about to recurse, the
      value of the depth option is checked.  If the value is zero then
      the function stops there. Otherwise, the function decrements the
      depth and recurses. The default value for the depth is minus one,
      which means the depth will never reach zero on recursion.


      The function @TO "comprehensiveGBMain"@ can take in one or two lists as inputs.
      When $S$ is not specified, the function returns a comprehensive
      Gröbner system on the whole parameter space, by computing a basis
      $\{s_1,\dots,s_r\}$ of the elimination ideal
      $\langle F\rangle\cap k[U]$, passing that basis as $S$ to comprehensiveGBMain,
      and prepending the extra segment
      $(\{0\}, \{s_1,\dots,s_r\}, \{s_1,\dots,s_r\})$
      to the output. When $S$ is specified, the function returns a
      comprehensive Gröbner system on $V(S)$, under the assumption that
      $V(S)\subseteq V(\langle F\rangle\cap k[U])$. That assumption is
      verified before the computation starts, and an error is raised
      when it fails. If the assumption is known to be true, the option
      CheckAssumption can be set to false to skip the verification step.
  References
    @LABEL("[1]","id" => "ref1")@ Akira Suzuki and Yosuke Sato. 2006. A simple algorithm to
    compute comprehensive Gröbner bases using Gröbner bases. In Proceedings of the 2006
    International Symposium on Symbolic and Algebraic Computation (ISSAC '06), 326–331.
  SeeAlso
    comprehensiveGB
    ReduceStrata
    Verbose
  ///

doc ///
  Key
    ReduceStrata
  Headline
    ignore computations on strata which have already been considered
  Description
    Text
      The option ReduceStrata allows to ignore computations on strata which
      have already been considered. This value is set to false by default.
      For smaller examples, changing this to true can reduce computation
      times, as for the following example. It will also give more easily
      parseable results.
    Example
      R2 = QQ[a,b][x,y,z];
      F2 = {x^2-a,y^3-b,x+y-z};
      S2 = {};
  SeeAlso
    comprehensiveGBMain
///

doc ///
  Key
    ICheckLoops
  Headline
    number of squarings in the consistency checks of parametricGBMain
  Description
    Text
      The option ICheckLoops sets the number of squarings that
      the heuristic consistency check iCheck tries before it falls back
      on the Rabinowitsch trick. The default value @TT "infinity"@ skips
      the squarings.
  SeeAlso
    parametricGBMain
///

doc ///
  Key
    Depth
  Headline
    bound on the recursion depth of comprehensiveGBMain, comprehensiveGB and parametricGBMain
  Description
    Text
      The option Depth bounds the number of nested recursive calls.
      The default value @TT "-1"@ means that no bound is set,
      and the recursion will continue until the algorithm terminates.
      If Depth is set to a non-negative integer,
      then the recursion will stop when the depth reaches zero.
      In that case, the output may only cover part of the parameter space.
    Example
      R1 = QQ[a,b][x,y];
      F1 = {a*x+b*y};
      netList comprehensiveGBMain(F1, {}, Depth => 0)
      netList parametricGBMain(F1, Depth => 0)
  SeeAlso
    comprehensiveGBMain
    comprehensiveGB
    parametricGBMain
///

doc ///
  Key
    CheckAssumption
  Headline
    check the assumption on S in comprehensiveGBMain
  Description
    Text
      The function @TO "comprehensiveGBMain"@ called on $(F, S)$ requires that
      $V(S)\subseteq V(\langle F\rangle\cap k[U])$.
      The default value of CheckAssumption is @TT "true"@,
      which means that the assumption is verified before
      the computation starts,
      and an error is raised when it fails.
      Setting it to @TT "false"@ skips the verification,
      which is useful when the assumption is known to hold.
    Example
      R1 = QQ[a,b][x,y];
      F1 = {a*x-1, a*b};
    Text
      Here, $\langle F\rangle\cap k[U] = \langle b\rangle$,
      so $S = \{b\}$ satisfies the assumption,
      while $S = \{\}$ does not.
    Example
      netList comprehensiveGBMain(F1, {b}, CheckAssumption => false)
  SeeAlso
    comprehensiveGBMain
///

doc ///
  Key
    Verbose
  Headline
    record comprehensiveGBMain iterations
  Description
    Text
      Setting Verbose to @TT "true"@ will print whatever $F$ and $S$ that
      comprehensiveGBMainRec is currently working on, as in the following example.
    Example
      R1 = QQ[a,b][x,y];
      F1 = {a*x+b*y};
      S1 = {};
      comprehensiveGBMain(F1,S1,Verbose=>true)
  SeeAlso
    comprehensiveGBMain
///


doc ///
  Key
    comprehensiveGBOnGraph
    (comprehensiveGBOnGraph,List,ZZ)
    [comprehensiveGBOnGraph, Verbose]
    [comprehensiveGBOnGraph, ReduceStrata]
    [comprehensiveGBOnGraph, Depth]
    [comprehensiveGBOnGraph, CheckAssumption]
  Headline
    parametrized systems from a graph
  Usage
    (F, GG) = comprehensiveGBOnGraph(G,d)
  Inputs
    G: List
      a list consisting of a list of vertices and a list of edges.
    d: ZZ
      a positive integer
    CheckAssumption=>Boolean
       check that $V(S)\subseteq V(\langle F\rangle\cap k[U])$
    ReduceStrata=>Boolean
       ignore strata that have already been computed
    Verbose=>Boolean
       print polynomial lists during computation
    Depth=>ZZ
       maximum recursion depth
  Outputs
    F: List
      A list of polynomials
    GG: List
      The Comprehensive Gröbner Basis of the polynomials
  Description
    Text
      Let $G=(V,E)$ be graph and fix a positive integer $d$.
      Consider the parameterized polynomial systems
      $F=\{f_e\}_{e\in E}\subseteq K[\lambda_e\:e\in E][x_{v,k}:v\in V,1\leq k \leq d]$,
      where $K\in \{\mathbb{C},\mathbb{R}\}$ and
      \[ f_{ij}=\sum_{k=1}^{d}(x_{i,k}-x_{j,k})^2  -\lambda_{ij} \].
      The function comprehensiveGBOnGraph returns the polynomial systems $F$ and a Combrehensive Gröbner Basis of $F$.
    Example
      (F,GG)=comprehensiveGBOnGraph({{1,2,3},{(1,2),(2,3),(3,1)}},1, ReduceStrata => true);
      netList F
      GG_0
  Caveat
    This function is not set up to take in Type Graph. The user should convert it to a list.
  SeeAlso
    ComprehensiveGBs

///


doc ///
  Key
    minimalDicksonBasis
    (minimalDicksonBasis, List)
  Headline
    a method for computing a minimal Dickson basis
  Usage
    minimalDicksonBasis G
  Inputs
    G :List
      of polynomials in a ring $R = k[U][X]$
  Outputs
    :List
      a minimal Dickson basis of G
  Description
    Text
      Following Definition 4.1 of @HREF("#ref1","[1]")@, a subset $F$ of $G$ is a minimal Dickson
      basis of $G$ if the following two conditions hold.
    Text
      @UL {
          {"For every $g \\in G$, there is some $f \\in F$ such that the leading power product of $g$
          is a multiple of the leading power product of $f$."},
          {"For any two distinct $f_1, f_2 \\in F$, neither leading power product is a multiple of
          the other."}
          }@
    Text
      In other words, $F$ has one polynomial for each leading power product of $G$
      that is minimal under divisibility. The leading power products are taken with respect to the
      variables $X$ only, so the leading coefficients are polynomials in the parameters $U$.
      We use a minimal Dickson basis in @TO "parametricGBMain"@ to determine how the
      parameter space splits.
    Example
      U = QQ[a, MonomialOrder => Lex];
      R = U[x, y, MonomialOrder => Lex];
      G = {a*x^2 - y, a*y^2 - 1, a*x - 1, (a + 1)*x - y, (a + 1)*y - a};
      minimalDicksonBasis G
    Text
      The minimal leading power products here are $x$ and $y$. A minimal Dickson basis is not
      unique: both $a x - 1$ and $(a+1) x - y$ have leading power product $x$. Following the
      heuristics of Section 7.1 of @HREF("#ref1","[1]")@, minimalDicksonBasis prefers polynomials whose leading
      coefficient is shared by many of the candidates, and then those whose leading coefficient
      has irreducible factors of low degree. Here $a + 1$ is the leading coefficient of both
      $(a+1) x - y$ and $(a+1) y - a$, so $(a+1) x - y$ is chosen over $a x - 1$.
  References
    @LABEL("[1]","id" => "ref1")@ Deepak Kapur, Yao Sun, and Dingkang Wang. 2013. An efficient
    algorithm for computing a comprehensive Gröbner system of a parametric polynomial system.
    In Journal of Symbolic Computation, 49, 27-44.
  SeeAlso
    parametricGBMain
///


doc ///
  Key
    "comprehensiveGB"
    (comprehensiveGB, List)
    [comprehensiveGB, Verbose]
    [comprehensiveGB, ReduceStrata]
    [comprehensiveGB, Depth]
  Headline
    a method that computes a Comprehensive Gröbner Basis
  Usage
    G = comprehensiveGB F
  Inputs
    F :List
       a list of polynomials in a ring $R = k[U][X]$
    ReduceStrata=>Boolean
       ignore strata that have already been computed
    Verbose=>Boolean
       print polynomial lists during computation
    Depth=>ZZ
       maximum recursion depth
  Outputs
    G :List
      of polynomials forming a comprehensive Gröbner basis of $\langle F\rangle$
  Description
    Text
      Implementation of the Algorithm proposed by Suzuki and Sato. Given a tower
      polynomial ring $R = k[U][X]$ for $U$ a set of parameters and $X$ a set
      of variables, and $F\subset R$ an ideal of variables and parameters, comprehensiveGB
      takes $F$ as input and returns a comprehensive Gröbner basis of
      $\langle F\rangle$ over the whole parameter space.
      The function computes a comprehensive Gröbner system with comprehensiveGBMain and
      returns the union of its Gröbner bases, together with a basis of the
      elimination ideal $\langle F\rangle\cap k[U]$.
      As above, the ring must be initialised as a tower ring, as in the following example.
    Example
      R1 = QQ[a,b][x,y]
    Text
      Here $X = \{x,y\}$ and $U = \{a,b\}$. If we wanted to find a comprehensive
      Gröbner basis over $\mathb{Q}^2$ for $F = \langle ax+by\rangle$, we input
      the following.
    Example
      F1 = {a*x+b*y}
      comprehensiveGB F1
    Text
      The function comprehensiveGB has several options: ReduceStrata and Verbose. The symbol ReduceStrata is an option
      to ignore computations on strata which have already been considered. This
      value is set to false by default. For smaller examples, changing this to
      true can reduce computation times, as for the following example. It will
      also give more easily parseable results.
    Example
      R2 = QQ[a,b][x,y,z];
      F2 = {x^2-a,y^3-b,x+y-z};
    Text
      Setting Verbose to True will print whatever $F$ and $S$ that comprehensiveGBMainRec is
      currently working on, as in the following example.
    Example
      comprehensiveGB(F1,Verbose=>true)
  SeeAlso
    comprehensiveGBMain
    ReduceStrata
    Verbose
///

doc ///
  Key
    parametricGBMain
    (parametricGBMain, List)
    (parametricGBMain, List, List)
    (parametricGBMain, List, List, List)
    [parametricGBMain, Depth]
    [parametricGBMain, ICheckLoops]
  Headline
    compute a Comprehensive Gröbner System using Kapur-Sun-Wang algorithm
  Usage
    parametricGBMain F
    parametricGBMain(E, F)
    parametricGBMain(E, N, F)
  Inputs
    F :List
      a list of polynomials in a ring $R = k[U][X]$
    E :List
      The strata where to execute the computation
    N :List
      The strata to remove from the vanishing set of $E$
    Depth=>ZZ
      a bound on the recursion depth; a negative value means no bound
    ICheckLoops=>ZZ
      the number of squarings in one of the consistency checks; the value @TO "infinity"@ means skip the squarings
  Outputs
    :List
      of lists $\{E_i, N_i, G_i\}$ such that $G_i$ is a Gröbner basis on $V(E_i)\setminus V(N_i)$
  Description
    Text
      Implementation of Algorithm PGBMain of @HREF("#ref1","[1]")@. Given a polynomial ring
      $R = k[U][X]$, where $U$ is a set of parameters and $X$ a set of variables, and a list $F$ of
      polynomials in $R$, parametricGBMain returns a minimal comprehensive Gröbner system for $F$ on the whole
      parameter space. The strata $V(E_i)\setminus V(N_i)$ are disjoint and nonempty, and the
      leading coefficients of the polynomials in $G_i$ do not vanish on them.
    Example
      U = QQ[a,b,c];
      R = U[x,y];
      F = {a*x - b, b*y - a, c*x^2 - y, c*y^2 - x};
      netList parametricGBMain F
    Text
      To compute a comprehensive Gröbner system on $V(E)\setminus V(N)$ instead, where $E$ and $N$
      are lists of polynomials in $k[U]$, call parametricGBMain on the triple $\{E, N, F\}$.
    Example
      netList parametricGBMain ({a, b}, {c}, F)
    Text
      The option Depth sets a bound on the recursion depth of the algorithm. See also @TO "comprehensiveGBMain"@.
      In each recursive call of the function, the algorithm considers a stratum on for
      which one of the leading coefficients $h_i$ of the minimal Dickson basis vanishes.
      When the function is about to recurse, the implementation checks the value of Depth: if it is
      zero then the function stops and returns the current system, otherwise it decrements Depth
      and recurses. So with Depth => 0, the only the branches found by the first call are returned.
      The default Depth is -1, which never reaches zero, so the full comprehensive Gröbner system is
      computed.
    Example
      for i from 0 to 2 do print (i, netList parametricGBMain(F, Depth => i))
    Text
      Note. If Depth is not negative, then the output is a comprehensive Gröbner system on only
      part of the parameter space.
    Text
      The algorithm discards the strata $V(E)\setminus V(N)$ that are empty, using the consistency
      checks of Section 5 of @HREF("#ref1","[1]")@.
      One of them decides whether $f \in \sqrt{\langle E\rangle}$ for $f \in N$, using the heuristic
      called ICheck in Algorithm 3 of @HREF("#ref1","[1]")@, hence the name of the option.
      If ICheckLoops is a nonnegative integer $k$, then iCheck computes the normal forms
      of $f^{2^i}$ modulo $E$ for $i \le k$, and the algorithm falls back on the Rabinowitsch trick only
      if none of them is zero.
      The default value @TT "infinity"@ uses the Rabinowitsch trick directly.
    Example
      parametricGBMain(F, ICheckLoops => 2) === parametricGBMain F
  References
    @LABEL("[1]","id" => "ref1")@ Deepak Kapur, Yao Sun, and Dingkang Wang. 2013. An efficient
    algorithm for computing a comprehensive Gröbner system of a parametric polynomial system.
    In Journal of Symbolic Computation, 49, 27-44.
  SeeAlso
    comprehensiveGBMain
    minimalDicksonBasis
///





-* Test section *-
TEST ///
-* Testing ringOrder on various polynomial rings *-
debug needsPackage "ComprehensiveGBs";

RGRevLex = QQ[a..d];
RLex = QQ[a..d, MonomialOrder => Lex];
RGLex = QQ[a..d, MonomialOrder => GLex];
RWeights = QQ[a..d, MonomialOrder => {Weights => {1, 3, 2, 4}, Lex}];
REliminate = QQ[a..d, MonomialOrder => Eliminate 2];
RGroupLex = QQ[a..d, MonomialOrder => GroupLex => 2, Global => false];
RGroupRevLex = QQ[a..d, MonomialOrder => GroupRevLex => 2, Global => false];
RProduct = QQ[a..d, MonomialOrder => ProductOrder {2, 2}];
RBlock = QQ[a..d, MonomialOrder => {Lex => 2, GRevLex => 2}];
RRevLex = QQ[a..d, MonomialOrder => RevLex, Global => false];

assert(ringOrder RGRevLex === {GRevLex => {1, 1, 1, 1}});
assert(ringOrder RLex === {Lex => 4});
assert(ringOrder RGLex === {Weights => {1, 1, 1, 1}, Lex => 4});
assert(ringOrder RWeights === {Weights => {1, 3, 2, 4}, Lex => 4});
assert(ringOrder REliminate === {Weights => {1, 1}, GRevLex => {1, 1, 1, 1}});
assert(ringOrder RGroupLex === {GroupLex => 2, GRevLex => {1, 1}});
assert(ringOrder RGroupRevLex === {GroupRevLex => 2, GRevLex => {1, 1}});
assert(ringOrder RProduct === {GRevLex => {1, 1}, GRevLex => {1, 1}});
assert(ringOrder RBlock === {Lex => 2, GRevLex => {1, 1}});
assert(ringOrder RRevLex === {RevLex => 4});
///


TEST ///
-* Testing  comprehensiveGBMain on a*x+b*y *-

Ptest = QQ[a,b];
Rtest = Ptest[x,y, MonomialOrder => Lex];

params = gens Ptest;
variables = gens Rtest

aP = params#0;
bP = params#1;

aR = promote (aP , Rtest);
bR = promote (bP , Rtest);

xR = variables#0;
yR = variables#1;


resultTest = comprehensiveGBMain({aR*xR + bR*yR}, {});

expected1 = ({0_Ptest}, {aP}, {aR*xR + bR*yR});
expected2 = ({aP}, {bP}, {aR^2*xR + aR*bR*yR, aR*xR + bR*yR});
expected3 = ({bP, aP}, {1_Ptest}, {aR*xR + bR*yR});

assert(#resultTest == 3);

assert member(expected1, resultTest);
assert member(expected2, resultTest);
assert member(expected3, resultTest);

///


TEST ///
-* Testing  comprehensiveGB on a*x+b*y  *-
PTest = QQ[aTest,bTest];
RTest = PTest[xTest,yTest, MonomialOrder => Lex];

fTest = aTest*xTest + bTest*yTest;

expected1 = aTest*xTest + bTest*yTest;
expected2 = aTest^2*xTest + aTest*bTest*yTest;

result = comprehensiveGB {fTest};

assert(#result == 2);
assert(result#0 == expected1 or result#1 == expected1);
assert(result#0 == expected1 or result#1 == expected2);
///


TEST ///
-* Testing comprehensiveGBOnGraph  on  E = {(1,2)}, V = {1,2} *-

E = {(1,2)};
V = {1,2};
G = {V,E};

(F,GG) = comprehensiveGBOnGraph(G,2);



Rtest = ring first F;


Stest = coefficientRing Rtest;

x11 = Rtest_0;
x12 = Rtest_1;
x21 = Rtest_2;
x22 = Rtest_3;

w12 = promote(Stest_0,Rtest);

expectedF = {x11^2 - 2*x11*x21 + x21^2 + x12^2 - 2*x12*x22 + x22^2 - w12 };
expectedGG = {
    ({0_Stest}, {1_Stest}, expectedF)
};

assert(F == expectedF);
assert(GG == expectedGG);

///


TEST ///
-* Testing  comprehensiveGBMain on a*x+b*y  with Verbose option *-
Ptest = QQ[a,b];
Rtest = Ptest[x,y, MonomialOrder => Lex];

params = gens Ptest;
variables = gens Rtest

aP = params#0;
bP = params#1;

aR = promote (aP , Rtest);
bR = promote (bP , Rtest);

xR = variables#0;
yR = variables#1;


resultTest = comprehensiveGBMain({aR*xR + bR*yR}, {}, Verbose => true);

expected1 = ({0_Ptest}, {aP}, {aR*xR + bR*yR});
expected2 = ({aP}, {bP}, {aR^2*xR + aR*bR*yR, aR*xR + bR*yR});
expected3 = ({bP, aP}, {1_Ptest}, {aR*xR + bR*yR});

assert(#resultTest == 3);

assert member(expected1, resultTest);
assert member(expected2, resultTest);
assert member(expected3, resultTest);


///


TEST ///
-* Testing  comprehensiveGB on a*x+b*y  with Verbose option  *-
PTest = QQ[aTest,bTest];
RTest = PTest[xTest,yTest, MonomialOrder => Lex];

fTest = aTest*xTest + bTest*yTest;

expected1 = aTest*xTest + bTest*yTest;
expected2 = aTest^2*xTest + aTest*bTest*yTest;

result = comprehensiveGB({fTest}, Verbose=> true);

assert(#result == 2);
assert(result#0 == expected1 or result#1 == expected1);
assert(result#0 == expected1 or result#1 == expected2);

///


TEST ///
-* Testing  comprehensiveGBMain on a*x+b*y  with ReduceStrata => true option *-
Ptest = QQ[a,b];
Rtest = Ptest[x,y, MonomialOrder => Lex];

params = gens Ptest;
variables = gens Rtest

aP = params#0;
bP = params#1;

aR = promote (aP , Rtest);
bR = promote (bP , Rtest);

xR = variables#0;
yR = variables#1;


resultTest = comprehensiveGBMain({aR*xR + bR*yR}, {},  ReduceStrata => true);

expected1 = ({0_Ptest}, {aP}, {aR*xR + bR*yR});
expected2 = ({aP}, {bP}, {aR^2*xR + aR*bR*yR, aR*xR + bR*yR});
expected3 = ({bP, aP}, {1_Ptest}, {aR*xR + bR*yR});

assert(#resultTest == 3);

assert member(expected1, resultTest);
assert member(expected2, resultTest);
assert member(expected3, resultTest);

///

TEST ///
-* Testing  comprehensiveGBMain on a*x+b*y  with ReduceStrata => true option *-
PTest = QQ[aTest,bTest];
RTest = PTest[xTest,yTest, MonomialOrder => Lex];

fTest = aTest*xTest + bTest*yTest;

expected1 = aTest*xTest + bTest*yTest;
expected2 = aTest^2*xTest + aTest*bTest*yTest;

result = comprehensiveGB({fTest}, ReduceStrata=> true);

assert(#result == 2);
assert(result#0 == expected1 or result#1 == expected1);
assert(result#0 == expected2 or result#1 == expected2);

///

-----------------------------
-- TEST for minimalDicksonBasis
-----------------------------
TEST /// -* Testing minimalDicksonBasis on {a*x^2 - y, a*y^2 - 1, a*x - 1, (a + 1)*x - y, (a + 1)*y - a} *-
U = U = QQ[a, MonomialOrder => Lex];
R = U[x, y, MonomialOrder => Lex];
G = {a*x^2 - y, a*y^2 - 1, a*x - 1, (a + 1)*x - y, (a + 1)*y - a}
minimalDicksonBasis G
///

-----------------------------------------------
-- TEST for minimalDicksonBasis (KSW, Section 6)
-----------------------------------------------
TEST /// -* Testing minimalDicksonBasis on {a*x*y + b*x, b*x^2*y + c*z, a*b*x + a*x*y + z, a*y + z, c*x + c*z^2} *-
U = QQ[a, b, c, MonomialOrder => Lex];
R = U[x, y, z, MonomialOrder => Lex];
G = {a*x*y + b*x, b*x^2*y + c*z, a*b*x + a*x*y + z, a*y + z, c*x + c*z^2}
assert(minimalDicksonBasis G == {a*y + z, c*x + c*z^2})
///


------------------------------------------------
-- TEST for parametricGBMain
-- From Example 6.1 of
-- "An efficient algorithm for computing a
-- comprehensive Gröbner system of a parametric
-- polynomial system", D. Kapur Y. Sun D. Wang,
-- J. of Symbolic Computation issue 49, 2013
------------------------------------------------
TEST /// -* Testing parametricGBMain on {a*x-, b*y-a, c*x^2-y, c*y^2-x} *-
U := QQ[a, b, c, MonomialOrder => GRevLex]
R := U[x,y,z, MonomialOrder => GRevLex]
G := {a*x-b, b*y-a, c*x^2-y, c*y^2-x}
L := parametricGBMain({0_U}, {1_U}, G)
U' = ring first first first L
R' = ring first last first L
ExpResult = set{
  {
    set ((f -> sub(f, U')) \ {0}),
    set ((f -> sub(f, U')) \ {b*c^2-b, a*c^2-a, b^3*c-a^3, a^3*c-b^3, a^6-b^6}),
    set ((f -> sub(f, R')) \ {1})
    },
  {
    set ((f -> sub(f, U')) \ {b*c^2-b, a*c^2-a, b^3*c-a^3, a^3*c-b^3, a^6-b^6}),
    set ((f -> sub(f, U')) \ {b}),
    set ((f -> sub(f, R')) \ {b*y-a, b*x-a*c*y})
  },
  {
    set ((f -> sub(f, U')) \ {b, a}),
    set ((f -> sub(f, U')) \ {c}),
    set ((f -> sub(f, R')) \ {c*x^2-y, c*y^2-x})
    },
  {
    set ((f -> sub(f, U')) \ {c, b, a}),
    set ((f -> sub(f, U')) \ {1_U'}),
    set ((f -> sub(f, R')) \ {y, x})
    }
}

assert((set apply(L, r -> set \ r)) == ExpResult )

///

TEST /// -* Testing parametricGBMain overloads*-
U := QQ[a, b, c, MonomialOrder => GRevLex]
R := U[x,y,z, MonomialOrder => GRevLex]
G := {a*x-b, b*y-a, c*x^2-y, c*y^2-x}
L := parametricGBMain({0_U}, {1_U}, G)

assert(parametricGBMain(G) == L)
assert(parametricGBMain({0_U}, G) == L)
assert(parametricGBMain({}, G) == L)
assert(parametricGBMain({}, {}, G) == L)

///


----------------------------------
-- TEST for consistency checks
-- Zeros in E should not break the consistency checks
----------------------------------

TEST ///
debug ComprehensiveGBs
U = QQ[a,b];
assert cCheck({0_U, a}, b)
assert consistencyCheckAllTogether({0_U, a}, {1_U})
R = U[x,y];
L = parametricGBMain({0_U, a}, {1_U}, {a*x+b*y});
assert(set apply(L, t -> set \ t) === set {
    {set {a}, set {b}, set {b*y}},
    {set {b, a}, set {1_U}, set {0_R}}})
///

------------------------------
-- TEST comprehensiveGB returns elements of R only without duplication
------------------------------
TEST ///
U = QQ[a,b];
R = U[x,y];
G = comprehensiveGB {a*x - 1, b*x - 1};
assert all(G, g -> ring g === R)
assert(#G == 3)
assert(set G === set {a_R - b_R, b*x - 1, b^2*x - b})
///

------------------------------
-- Test ICheckLoops option for parametricGBMain
------------------------------
TEST ///
U = QQ[a, b, c];
R = U[x, y, MonomialOrder => Lex];
F = {a*x - b, b*y - a, c*x^2 - y, c*y^2 - x};
L = parametricGBMain F;
assert all({0, 1, 2, 4}, k -> parametricGBMain(F, ICheckLoops => k) === L)
///

-- Test Depth option for parametricGBMain
TEST ///
U = QQ[a, b, c, MonomialOrder => GRevLex];
R = U[x, y, MonomialOrder => GRevLex];
F = {a*x - b, b*y - a, c*x^2 - y, c*y^2 - x};
strata = L -> set apply(L, t -> (set t#0, set t#1));
L = parametricGBMain F;
-- each extra level of recursion adds one branch in this example
assert(apply(0..2, d -> #parametricGBMain(F, Depth => d)) == (2, 3, 4))
-- the default never stops early, and a large depth gives the same result
assert(strata parametricGBMain(F, Depth => 10) === strata L)
-- with a smaller depth we get a subset of the branches
assert all(0..2, d -> isSubset(strata parametricGBMain(F, Depth => d), strata L))
///


end --



-* Development section *-
restart
debug needsPackage "ComprehensiveGBs"
check "ComprehensiveGBs"

uninstallPackage "ComprehensiveGBs"
restart
needsPackage "ComprehensiveGBs"
installPackage "ComprehensiveGBs"
viewHelp "ComprehensiveGBs"


Description
       Text
       Tree
       Example
       CannedExample
     Acknowledgement
     Contributors
     References
     Caveat
     SeeAlso
     Subnodes

doc ///
     Key
     Headline
     Usage
       degreeMap phi
     Inputs
       phi: ...
     Outputs
     Consequences
       Item
     Description
       Text
       Example
       CannedExample
       Code
       Pre
     ExampleFiles
     Contributors
     References
     Caveat
     SeeAlso

     ///
