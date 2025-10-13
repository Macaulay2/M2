-- -*- coding: utf-8 -*-

newPackage(
        "GeometricDecomposability",
        Version => "1.4.2",
        Date => "May 2, 2025",
        Headline => "checking whether ideals are geometrically vertex decomposable",
        Authors => {
                {
                Name => "Mike Cummings",
                Email => "mike.cummings@uwaterloo.ca",
                HomePage => "https://mikecummings.ca"
                },
                {
                Name => "Adam Van Tuyl",
                Email => "vantuyl@math.mcmaster.ca",
                HomePage => "https://ms.mcmaster.ca/~vantuyl/"
                }
                },
        Keywords => {"Commutative Algebra"},
        PackageImports => {"Depth", "PrimaryDecomposition"},
	Certification => {
	    "journal name" => "Journal of Software for Algebra and Geometry",
	    "journal URI" => "https://msp.org/jsag/",
	    "article title" => "The GeometricDecomposability package for Macaulay2",
	    "acceptance date" => "2024-01-23",
	    "published article URI" => "https://msp.org/jsag/2024/14-1/p06.xhtml",
	    "published article DOI" => "10.2140/jsag.2024.14.41",
	    "published code URI" => "https://msp.org/jsag/2024/14-1/jsag-v14-n1-x06-GeometricDecomposability.m2",
	    "release at publication" => "d29b1075986232868a6460344ad708dbddbdd29b",
	    "version at publication" => "1.2",
	    "volume number" => "14",
	    "volume URI" => "https://msp.org/jsag/2024/14-1/"
	    }
        )

export {
        -- methods
        "findLexCompatiblyGVDOrders",
        "findOneStepGVD",
        "getGVDIdeal",
        "initialYForms",
        "isGeneratedByIndeterminates",
        "isGVD",
        "isLexCompatiblyGVD",
        "isUnmixed",
        "isWeaklyGVD",
        "oneStepGVD",
        "oneStepGVDCyI",
        "oneStepGVDNyI",

        -- options
        "CheckCM",
        "CheckDegenerate",
        "CheckUnmixed",
        "IsIdealHomogeneous",
        "IsIdealUnmixed",
        "OnlyDegenerate",
        "OnlyNondegenerate",
        "SquarefreeOnly",
        "UniversalGB"
        };

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--
-- METHODS
--
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

findLexCompatiblyGVDOrders = method(
        Options => {CheckUnmixed => true}
        )
findLexCompatiblyGVDOrders(Ideal) := List => opts -> I -> (
        if isGVDBaseCase I then (
                return permutations gens ring I;
                );
        try (
                orders := sort lexOrderHelper({I}, {}, CheckUnmixed=>opts.CheckUnmixed);
                truncatedOrders := recursiveFlatten orders;
                )
        then (
                allLexOrders := permutations gens ring I;
                validLexOrders := select(allLexOrders, lexOrder -> inTruncatedList(lexOrder, truncatedOrders) );
                return validLexOrders;
                )
        else (
                return {};
        )
        )

--------------------------------------------------------------------------------

findOneStepGVD = method(
        Options => {
                CheckUnmixed => true, 
                OnlyDegenerate => false,
                OnlyNondegenerate => false, 
                SquarefreeOnly => false,
                UniversalGB => false,
                Verbose => false
                }
        )
findOneStepGVD(Ideal) := List => opts -> I -> (
        -- returns a list of indeterminates for which there exists a one-step geometric vertex decomposition

        if opts.OnlyDegenerate and opts.OnlyNondegenerate then (
                error("a geometric vertex decomposition cannot be both degenerate and nondegenerate");
                return {};
                );

        R := ring I;
        indets := support I;

        if opts.SquarefreeOnly then (
                if (opts.CheckUnmixed or opts.OnlyDegenerate or opts.OnlyNondegenerate) then (
                        printIf(opts.Verbose, "ignoring unmixedness/degeneracy checks");
                );
                -- we use [KR, Lemma 2.6] and [KR, Lemma 2.12]

                -- first get the indets with respect to which the ideal is "clearly" squarefree 
                -- the variables y such that y^2 does not divide any term of any generator of I
                gensTerms := flatten apply(I_*, terms);
                isSquarefreeIndet := (termsList, y) -> ( 
                        L := apply(gensTerms, m -> degree(y, m));
                        return max L <= 1;
                        );
                return select(indets, z -> isSquarefreeIndet(gensTerms, z));
                );

        -- in this case, we compute a Gröbner basis for each indeterminate in support I
        oneSteps := apply(indets, y -> join(toSequence {y}, oneStepGVD(I, y, CheckDegenerate=>(opts.OnlyDegenerate or opts.OnlyNondegenerate), CheckUnmixed=>opts.CheckUnmixed, UniversalGB=>opts.UniversalGB, Verbose=>opts.Verbose)));

        -- finish by proceeding by cases on degenerate/nondegenerate checks
        if opts.OnlyDegenerate then (
                return apply(select(oneSteps, o -> o_1 and o_4 == "degenerate"), t -> t_0);
        );
        if opts.OnlyNondegenerate then (
                return apply(select(oneSteps, o -> o_1 and o_4 == "nondegenerate"), t -> t_0);
        );
        return apply(select(oneSteps, o -> o_1), t -> t_0);
        )

--------------------------------------------------------------------------------

getGVDIdeal = method(
        Options => {
                CheckUnmixed => true,
                UniversalGB => false
                }
        )
getGVDIdeal(Ideal, List) := List => opts -> (I, L) -> (
        CNs := new HashTable from {
                "C" => oneStepGVDCyI,
                "N" => oneStepGVDNyI
                };
        return accumulate( (i, j) -> CNs#(j_0)(i, j_1, CheckUnmixed=>opts.CheckUnmixed, UniversalGB=>opts.UniversalGB) , prepend(I, L) );  -- last entry is the desired ideal
        )


--------------------------------------------------------------------------------

-- [KMY, Section 2.1]
initialYForms = method(
        Options => {UniversalGB => false}
        )
initialYForms(Ideal, RingElement) := Ideal => opts -> (I, y) -> (
        givenRing := ring I;

        -- set up the ring
        indeterminates := switch(0, index y, gens ring y);
        cr := coefficientRing ring I;

        initYFormRing := (cr) monoid([indeterminates, MonomialOrder=>ProductOrder{1, #indeterminates - 1}]);

        -- get the ideal of initial y-forms using the product order
        I = sub(I, initYFormRing);
        y = sub(y, initYFormRing);

        -- compute in_y(I) manually if we have a UGB using [KMY, Theorem 2.1(a)]
        if opts.UniversalGB then (
                listOfInitYForms := apply(I_*, f -> leadTerm(1, f));
                return sub(ideal listOfInitYForms, givenRing);
                );

        -- if we don't have a universal Gröbner basis
        inyFormIdeal := ideal leadTerm(1,I);
        return sub(inyFormIdeal, givenRing);
        )


--------------------------------------------------------------------------------

isGeneratedByIndeterminates = method()
isGeneratedByIndeterminates(Ideal) := Boolean => I -> (
        R := ring I;
        indeterminates := gens R;
        gensI := first entries gens I;
        return isSubset(delete(0, gensI), indeterminates);
        )

--------------------------------------------------------------------------------

-- [KR, Definition 2.7]
isGVD = method(
        Options => {
                CheckCM => "always", 
                CheckUnmixed => true, 
                IsIdealHomogeneous => false, 
                IsIdealUnmixed => false, 
                UniversalGB => false,
                Verbose => false
                }
        )
isGVD(Ideal) := Boolean => opts -> I -> (

        if not instance(opts.CheckCM, String) then (
                error "value of CheckCM must be a string";
                ) else (
                if not isSubset({opts.CheckCM}, {"always", "once", "never"}) then error ///unknown value of CheckCM; options are "once" (default), "always", "never"///;
                );

        R := ring I;
        printIf(opts.Verbose, "I = " | toString I);

        if I == 0 then (printIf(opts.Verbose, "-- zero ideal"); return true);
        if I == 1 then (printIf(opts.Verbose, "-- unit ideal"); return true);
        if (isGeneratedByIndeterminates I) then (printIf(opts.Verbose, "-- generated by indeterminates"); return true);

        -- Cohen-Macaulay check when the ideal is homogeneous [KR, Corollary 4.5] (i.e., homogeneous and not CM => not GVD)
        -- (if we are here, the ideal is proper)
        x := opts.IsIdealHomogeneous or isHomogeneous(I);
        checkCohenMacaulay := x and (opts.CheckCM == "once" or opts.CheckCM == "always");
        if checkCohenMacaulay then (
                -- Auslander-Buchsbaum in this case says that Cohen-Macaulay is equivalent to pdim == codim
                if pdim(R^1 / I) != codim(I) then (
                        printIf(opts.Verbose, "-- not Cohen-Macaulay");
                        return false;
                        );
                );
        
        -- Cohen-Macaulay implies unmixed so we need only check unmixed if Cohen-Macaulayness was false or not checked
        if opts.CheckUnmixed and not checkCohenMacaulay then (
                if not opts.IsIdealUnmixed then (
                        if not (isUnmixed I) then (printIf(opts.Verbose, "-- ideal is not unmixed"); return false);
                        );
                );

        -- to get the value of CheckCM in next call of isGVD
        CMTable := new HashTable from {
                "always" => "always",
                "once" => "never",
                "never" => "never"
                };

        -- iterate over all indeterminates, first trying the ones which appear squarefree in the given generators for I
        squarefreeIndets := findOneStepGVD(I, SquarefreeOnly=>true, UniversalGB=>opts.UniversalGB);
        remainingIndets := (support I) - set(squarefreeIndets);
        iterIndets := join(squarefreeIndets, remainingIndets);
        for y in iterIndets do (

                printIf(opts.Verbose, "-- decomposing with respect to " | toString y);

                (isValid, C, N) := oneStepGVD(I, y, CheckUnmixed=>opts.CheckUnmixed, UniversalGB=>opts.UniversalGB, Verbose=>opts.Verbose);
                if not isValid then continue;  -- go back to top of for loop

                printIf(opts.Verbose, "-- C = " | toString C);
                printIf(opts.Verbose, "-- N = " | toString N);

                -- check N first (the link of a Cohen-Macaulay simplicial complex will be Cohen-Macaulay, but the deletion need not be
                -- so probably will be more likely to catch a false in the N branch than the C branch)
                NisGVD := isGVD(N, CheckCM=>CMTable#(opts.CheckCM), CheckUnmixed=>opts.CheckUnmixed, IsIdealHomogeneous=>x, IsIdealUnmixed=>true, UniversalGB=>opts.UniversalGB, Verbose=>opts.Verbose);
                if not NisGVD then continue;
                
                -- if we are here, then NisGVD is true
                CisGVD := isGVD(C, CheckCM=>CMTable#(opts.CheckCM), CheckUnmixed=>opts.CheckUnmixed, IsIdealHomogeneous=>x, IsIdealUnmixed=>true, UniversalGB=>opts.UniversalGB, Verbose=>opts.Verbose);
                if CisGVD and NisGVD then return true;  -- otherwise, try next variable 
                );

        -- if we are here, no choice of y worked
        return false;
        )

--------------------------------------------------------------------------------

-- [KR, Definition 2.11], checked using [KR, Proposition 2.14]
isLexCompatiblyGVD = method(
        Options => {
                CheckCM => "always", 
                CheckUnmixed => true, 
                IsIdealHomogeneous => false, 
                IsIdealUnmixed => false, 
                UniversalGB => false, 
                Verbose => false
                }
        )
isLexCompatiblyGVD(Ideal, List) := Boolean => opts -> (I, indetOrder) -> (
        if not instance(opts.CheckCM, String) then (
                error "value of CheckCM must be a string";
                ) else (
                if not isSubset({opts.CheckCM}, {"always", "once", "never"}) then error ///unknown value of CheckCM, options are "once" (default), "always", "never"///;
                );

        printIf(opts.Verbose, "I = " | toString I);

        -- compute initial ideal with respect to the given lex order
        R := ring I;
        cr := coefficientRing R;
        allVariables := join(indetOrder,  gens R - set indetOrder);  -- extend the lex order by appending any missing variables
        lexRing := (cr) monoid([allVariables, MonomialOrder=>Lex]);  
        initIdeal := ideal leadTerm sub(I, lexRing);

        printIf(opts.Verbose, "initial ideal = " | toString initIdeal);

        -- run recursive definition [KR, Definition 2.11] on the initial ideal [KR, Proposition 2.14]
        recursiveLexGVD(initIdeal, indetOrder, 
                CheckCM=>opts.CheckCM, CheckUnmixed=>opts.CheckUnmixed, 
                IsIdealHomogeneous=>true, IsIdealUnmixed=>opts.IsIdealUnmixed,
                UniversalGB=>true, Verbose=>opts.Verbose
                )
        )

-- recursive definition of <-compatibly geometrically vertex decomposable [KR, Definition 2.11]
recursiveLexGVD = method(
        Options => {
                CheckCM => "always", 
                CheckUnmixed => true, 
                IsIdealHomogeneous => false, 
                IsIdealUnmixed => false, 
                UniversalGB => false, 
                Verbose => false
                }
        )
recursiveLexGVD(Ideal, List) := Boolean => opts -> (I, indetOrder) -> (
        if not instance(opts.CheckCM, String) then (
                error "value of CheckCM must be a string";
                ) else (
                if not isSubset({opts.CheckCM}, {"always", "once", "never"}) then error ///unknown value of CheckCM, options are "once" (default), "always", "never"///;
                );

        R := ring I;
        printIf(opts.Verbose, "I = " | toString I);

        if I == 0 then (printIf(opts.Verbose, "-- zero ideal"); return true);
        if I == 1 then (printIf(opts.Verbose, "-- unit ideal"); return true);
        if (isGeneratedByIndeterminates I) then (printIf(opts.Verbose, "-- generated by indeterminates"); return true);

        supportIndets := support I;
        trimmedOrder := select(indetOrder, i -> member(sub(i, R), supportIndets));

        -- Cohen-Macaulay check when the ideal is homogeneous [KR, Corollary 4.5]
        -- (if we are here, the ideal is proper)
        x := opts.IsIdealHomogeneous or isHomogeneous(I);
        checkCohenMacaulay := x and (opts.CheckCM == "once" or opts.CheckCM == "always");
        if checkCohenMacaulay then (
                -- Auslander-Buchsbaum in this case says that Cohen-Macaulay is equivalent to pdim == codim
                if pdim(R^1 / I) != codim(I) then (
                        printIf(opts.Verbose, "-- not Cohen-Macaulay");
                        return false;
                        );
                );

        -- Cohen-Macaulay implies unmixed so we need only check unmixed if C-M was false or not checked
        if opts.CheckUnmixed and not checkCohenMacaulay then (
                if not opts.IsIdealUnmixed then (
                        if not (isUnmixed I) then (printIf(opts.Verbose, "-- ideal is not unmixed"); return false);
                        );
                );

        -- to get the value of CheckCM in next call of isLexCompatiblyGVD
        CMTable := new HashTable from {
                "always" => "always",
                "once" => "never",
                "never" => "never"
                };

        -- check next indeterminate in list
        y := first trimmedOrder;
        remainingOrder := take(trimmedOrder, {1, #trimmedOrder});

        printIf(opts.Verbose, "-- decomposing with respect to " | toString y);

        (isValid, C, N) := oneStepGVD(I, y, CheckUnmixed=>opts.CheckUnmixed, UniversalGB=>opts.UniversalGB, Verbose=>opts.Verbose);
        if not isValid then return false;  -- order didn't work

        printIf(opts.Verbose, "-- C = " | toString C);
        printIf(opts.Verbose, "-- N = " | toString N);

        -- check N first, same reasoning as in isGVD
        NisGVD := recursiveLexGVD(N, remainingOrder, CheckCM=>CMTable#(opts.CheckCM), CheckUnmixed=>opts.CheckUnmixed, IsIdealHomogeneous=>x, IsIdealUnmixed=>true, UniversalGB=>opts.UniversalGB, Verbose=>opts.Verbose);
        if not NisGVD then return false;

        -- if are here, then NisGVD is true
        CisGVD := recursiveLexGVD(C, remainingOrder, CheckCM=>CMTable#(opts.CheckCM), CheckUnmixed=>opts.CheckUnmixed, IsIdealHomogeneous=>x, IsIdealUnmixed=>true, UniversalGB=>opts.UniversalGB, Verbose=>opts.Verbose);
        return CisGVD;
)

--------------------------------------------------------------------------------

isUnmixed = method()
isUnmixed(Ideal) := Boolean => I -> (
        R := ring I;
        D := primaryDecomposition I;
        
        if #D <= 1 then return true;

        commonDim := dim(R/(D_0));
        remainingPrimes := drop(D, 1);
        for P in remainingPrimes do (
                if dim(P) != commonDim then return false;
        );
        return true;
        )

--------------------------------------------------------------------------------

-- [KR, Definition 4.6]
isWeaklyGVD = method( 
        Options => {
                CheckUnmixed => true, 
                IsIdealUnmixed => false,
                UniversalGB => false,
                Verbose => false
                }
        )
isWeaklyGVD(Ideal) := Boolean => opts -> I -> (
        R := ring I;
        printIf(opts.Verbose, "I = " | toString I);

        if I == 0 then (printIf(opts.Verbose, "-- zero ideal"); return true);
        if I == 1 then (printIf(opts.Verbose, "-- unit ideal"); return true);
        if (isGeneratedByIndeterminates I) then (printIf(opts.Verbose, "-- generated by indeterminates"); return true);

        if opts.CheckUnmixed then (
                if not opts.IsIdealUnmixed then (
                        if not (isUnmixed I) then (printIf(opts.Verbose, "-- ideal is not unmixed"); return false);
                        );
                );

        -- iterate over all indeterminates, first trying the ones which appear squarefree in the given generators for I
        squarefreeIndets := findOneStepGVD(I, SquarefreeOnly=>true, UniversalGB=>opts.UniversalGB);
        remainingIndets := (support I) - set(squarefreeIndets);
        iterIndets := join(squarefreeIndets, remainingIndets);

        -- check all options for y until one works
        for y in iterIndets do (

                printIf(opts.Verbose, "-- decomposing with respect to " | toString y);

                oneStep := oneStepGVD(I, y, CheckDegenerate=>true, CheckUnmixed=>opts.CheckUnmixed, UniversalGB=>opts.UniversalGB, Verbose=>opts.Verbose);
                isValid := oneStep_0;
                if not isValid then continue;  -- go back to top of for loop

                (C, N, degenerateOutput) := (oneStep_1, oneStep_2, oneStep_3);
                isDegenerate := (degenerateOutput == "degenerate");
                degenerateTable := new HashTable from {true => "degenerate", false => "nondegenerate"};

                printIf(opts.Verbose, "-- C = " | toString C);
                printIf(opts.Verbose, "-- N = " | toString N);
                printIf(opts.Verbose, "-- form a " | degenerateTable#isDegenerate | " geometric vertex decomposition");

                if isDegenerate then (
                        -- degenerate case
                        if isWeaklyGVD(N, CheckUnmixed=>opts.CheckUnmixed, IsIdealUnmixed=>true, UniversalGB=>opts.UniversalGB, Verbose=>opts.Verbose) then return true else continue;

                        ) else (
                        -- nondegenerate case
                        NisRadical := (N == radical(N, Unmixed=>true));
                        NisCM := if (isHomogeneous N) then pdim( (ring N)^1/N ) == codim N else isCM(ring N/N);

                        if not (NisRadical and NisCM) then continue;
                        -- otherwise, we need only check that C is weakly GVD
                        if isWeaklyGVD(C, CheckUnmixed=>opts.CheckUnmixed, IsIdealUnmixed=>true, UniversalGB=>opts.UniversalGB, Verbose=>opts.Verbose) then return true else continue;
                        )
                );

        -- if we are here, no choice of y worked
        return false;
        )

--------------------------------------------------------------------------------

oneStepGVD = method( 
        Options => {
                CheckDegenerate => false, 
                CheckUnmixed => true, 
                UniversalGB => false,
                Verbose => false
                }
        )
oneStepGVD(Ideal, RingElement) := Sequence => opts -> (I, y) -> (

        -- set up the rings
        indeterminates := switch(0, index y, gens ring y);
        remainingIndets := drop(gens ring y, {index y, index y});
        cr := coefficientRing ring I;

        givenRing := ring I;
        lexRing := (cr) monoid([indeterminates, MonomialOrder=>Lex]);
        contractedRing := (cr) monoid([remainingIndets]);

        -- pull everything into the new rings and get a (reduced) Gröbner basis
        J := sub(I, lexRing);
        z := sub(y, lexRing);
        G := if opts.UniversalGB then J_* else first entries gens gb J;

        -- get N_{y,I}
        NyI := ideal select(G, g -> degree(z, g) == 0);

        -- get C_{y, I}
        CyI := ideal apply(G, g -> getQ(g, z));

        -- check whether the intersection condition holds
        isValid := if opts.UniversalGB then isValidOneStepFromUGB(G, CyI, NyI, z) else isValidOneStep(G, z);
        if not isValid then (
                printIf(opts.Verbose, "Warning: not a valid geometric vertex decomposition");
                );

        -- sub C and N into original ring
        -- by [CDSRVT, Theorem 2.9] variables in ring not appearing in the ideal do not matter 
        C := sub(CyI, givenRing);
        N := sub(NyI, givenRing);

        -- check unmixed & degenerate as needed, and return
        if opts.CheckUnmixed and opts.CheckDegenerate then (
                unmixedIdeals := unmixedCheck(C, N, opts.Verbose);
                degeneracyStatus := degeneracyCheck(C, N);
                return (isValid and unmixedIdeals, C, N, degeneracyStatus);
                );
        if opts.CheckUnmixed then (  -- not needed to CheckDegenerate
                unmixedIdeals1 := unmixedCheck(C, N, opts.Verbose);
                return (isValid and unmixedIdeals1, C, N);
                );
        if opts.CheckDegenerate then ( -- not needed to CheckUnmixed
                degeneracyStatus1 := degeneracyCheck(C, N);
                return (isValid, C, N, degeneracyStatus1);
                );
        
        -- otherwise, we don't need not check unmixed nor degeneracy
        return (isValid, C, N);
        )

--------------------------------------------------------------------------------

oneStepGVDCyI = method( 
        Options => {
                CheckUnmixed => true,
                UniversalGB => false
                }
        )
oneStepGVDCyI(Ideal, RingElement) := Ideal => opts -> (I, y) -> (oneStepGVD(I, y, CheckUnmixed=>opts.CheckUnmixed, UniversalGB=>opts.UniversalGB))_1;

--------------------------------------------------------------------------------

oneStepGVDNyI = method( 
        Options => {
                CheckUnmixed => true,
                UniversalGB => false
                }
        )
oneStepGVDNyI(Ideal, RingElement) := Ideal => opts -> (I, y) -> (oneStepGVD(I, y, CheckUnmixed=>opts.CheckUnmixed, UniversalGB=>opts.UniversalGB))_2;

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

--** METHODS (Hidden from users, not exported)


areGensSquarefreeInY = method()
areGensSquarefreeInY(List, RingElement) := Boolean => (L, y) -> (
        -- L a list of polynomials (e.g., generators of some ideal), and y an indeterminate in the ring
        -- returns true if and only if ideal(L) is squarefre in y, that is, if y^2 does not divide any term of any of the polynomials

        return all( apply(L, m -> isSquarefreeInY(m, y)), i->i );
        )


-- check if C_{y, I} and N_{y, I} form a degenerate (or not) geometric vertex decomposition
degeneracyCheck = method()
degeneracyCheck(Ideal, Ideal) := String => (C, N) -> (
        -- degenerate if C == 1 or radical C == radical N
        if C == 1 then return "degenerate";

        radC := radical(C, Unmixed=>true);
        radN := radical(N, Unmixed=>true);
        if (radC == radN) then return "degenerate";

        -- if we are here, we are nondegenerate
        return "nondegenerate";
        )


getQ = method()
getQ(RingElement, RingElement) := RingElement => (f, y) -> (
        -- f is of the form q*y^d+r, return q  (where y^d does not divide any term of r and y does not divide any term of q)
        L := terms f;  -- this preserves the coefficient (and hence sign) of each term
        yDegrees := L / degree_y;
        q := apply(select(L, m -> degree(y, m) == max yDegrees), n -> sub(n, y=>1));
        return sum q;
        )


isGVDBaseCase = method()
isGVDBaseCase(Ideal) := Boolean => I -> (
        return (I == 1 or I == 0 or isGeneratedByIndeterminates(I));
        )


isIdealSquarefreeInY = method()
isIdealSquarefreeInY(Ideal, RingElement) := Boolean => (I, y) -> (
        -- returns true if and only if I is squarefree in y, that is: if and only if
        -- y^2 does not divide any term of a Grobner basis of I with respect to a y-compatible monomial order 
        -- we use lex with y > all other variables

        R := ring I;
        cr := coefficientRing R;
        indeterminates := switch(0, index y, gens ring y);  -- assumes that y and I are "from" the same ring

        S := (cr) monoid([indeterminates, MonomialOrder=>Lex]);  -- ring that has lex order with y > all other variables
        J := sub(I, S);
        z := sub(y, S);
        grobnerLeadTerms := first entries gens leadTerm J;
        return areGensSquarefreeInY(grobnerLeadTerms, z);
        )


intersectLists = method()
intersectLists(List) := List => L -> (
        -- L is a list of lists
        S := for l in L list (set l);
        return toList fold(intersectSets, S)
        )


intersectSets = method()
intersectSets(Set, Set) := Set => (S1, S2) -> (
        return S1 * S2;
        )


inTruncatedList = method()
inTruncatedList(List, List) := Boolean => (L, LL) -> (
        -- LL is a list of lists
        -- return True if: for some list l of length n in LL, the first n terms of L are exactly l
        for l in LL do (
                n := #l ;
                for i from 0 to n-1 do (
                        if L_i != l_i then break;
                        return true;
                        );
                );
        return false;
        )


isSquarefreeInY = method()
isSquarefreeInY(RingElement, RingElement) := (m, y) -> (
        -- m a monomial, y an indeterminate
        -- returns true if and only if m is squarefree in y
        return not (m % y^2 == 0)
        )


-- determine whether the one-step geometric vertex decomposition holds
-- uses [KR, Lemmas 2.6 and 2.12]
isValidOneStep = method()
isValidOneStep(List, RingElement) := Boolean => (G, y) -> (
        -- G is a list, whose elements form a reduced Gröbner basis

        -- analyze the powers of y appearing in the Gröbner basis
        gbTerms := G / terms;
        yDegreesByTerm := apply(gbTerms, L -> apply(L, m -> degree(y, m)));
        yDegrees := unique flatten yDegreesByTerm;
        yMaxDegree := max yDegrees;
        return yMaxDegree <= 1;
        )


isValidOneStepFromUGB = method()
isValidOneStepFromUGB(List, Ideal, Ideal, RingElement) := Boolean => (G, C, N, y) -> (
        -- G is a UGB for the ideal I it generates; C = C_{y, I} and N_{y, I}
        -- the previous check may not work for UGBs because it requires the GB to be reduced
        currentRing := ring y;
        C1 := sub(C, currentRing);
        N1 := sub(N, currentRing);

        initYForms := sub(initialYForms(ideal G, y, UniversalGB=>true), currentRing);
        return initYForms == intersect(C1, N1 + ideal(y));
        )


lexOrderHelper = method(Options => {CheckUnmixed => true})
lexOrderHelper(List, List) := List => opts -> (idealList, order) -> (
        -- remove ideals that are trivially GVD
        nontrivialIdeals := select(idealList, i -> not isGVDBaseCase i);
        -- if there are none left, return the order
        if (#nontrivialIdeals) == 0 then (
                return order;
                );

        -- for each ideal, get the indets which form a oneStepGVD
        possibleIndets := apply(nontrivialIdeals, i -> findOneStepGVD(i, CheckUnmixed=>opts.CheckUnmixed));
        commonPossibleIndets := intersectLists possibleIndets;
        if commonPossibleIndets == {} then return;

        -- for each variable, compute the C and N ideals
        nextIdeals := for y in commonPossibleIndets list (
                flatten apply( nontrivialIdeals, i -> (
                        oneStep := oneStepGVD(i, y);
                        {oneStep_1, oneStep_2}
                        ))
                );

        L := for m from 0 to (#commonPossibleIndets)-1 list (
                lexOrderHelper(nextIdeals#m, append(order, commonPossibleIndets#m))
                );
        return L;
        )


sumGenerators = method()
sumGenerators(Ideal) := List => I -> (
        -- returns a list {I1, I2} where I = I1 + I2 and (support I1) and (support I2) are disjoint,
        -- if no such I1 and I2 exist, then {I} is returned
        -- only uses the given generators of I (does not compute a minimal generating set)

        supp := support I;
        suppSubsets := delete(supp, drop(subsets supp, {0, 0}));

        -- nested list; each inner list has two lists that are disjoint sets of variables, whose union is support I
        suppPartitions := unique apply(suppSubsets, s -> sort{s, supp - set(s)});

        -- filter to those that partition the generators
        gensPartitions := select(suppPartitions, L -> all(
                        I_*, f -> (isSubset(support f, L_0) xor isSubset(support f, L_1))
                        )
                );

        if #gensPartitions == 0 then return (I, ideal 0);

        -- not sure what the "best" partition would be (computationally)
        -- for now, just pick the first one
        P := gensPartitions_0;
        return (
                ideal select(I_*, f -> isSubset(support f, P_0)),
                ideal select(I_*, f -> isSubset(support f, P_1))
        );
        )


printIf = method()
printIf(Boolean, String) := (bool, str) -> (
        if bool then print str;
        )

        recursiveFlatten = method(TypicalValue => List)
        recursiveFlatten(List) := L -> (
                Lstr := toString L;
                if Lstr#2 == "{" then (
                        return recursiveFlatten flatten L;
                        )
                else (
                        return L;
                        )
                )


unmixedCheck = method()
unmixedCheck(Ideal, Ideal, Boolean) := Boolean => (C, N, verb) -> (

        CisCM := isHomogeneous C and (pdim((ring C)^1/C) == codim C);
        NisCM := isHomogeneous N and (pdim((ring N)^1/N) == codim N);

        isUnmixedC := CisCM or isUnmixed C;
        isUnmixedN := NisCM or isUnmixed N;

        bothUnmixed := (isUnmixedC and isUnmixedN);

        if not isUnmixedC then (
                printIf(verb, "Warning: CyI is not unmixed");
                );
        if not isUnmixedN then (
                printIf(verb, "Warning: NyI is not unmixed");
                );

        return bothUnmixed;
        )


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--
-- DOCUMENTATION
--
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

beginDocumentation()

--******************************************************************************
-- Documentation for package
--******************************************************************************


doc///
        Node
                Key
                        GeometricDecomposability

                Headline
                        a package to check whether ideals are geometrically vertex decomposable

                Description
                        Text

                                This package includes routines to check whether an ideal is
                                geometrically vertex decomposable.

                                Geometrically vertex
                                decomposable ideals can be viewed as a generalization of the properties
                                of the Stanley-Reisner ideal of a vertex decomposable simplicial complex.
                                This family of ideals is based upon the geometric vertex
				decomposition property defined by Knutson, Miller, and Yong [KMY]. Klein and Rajchgot
                                then gave a recursive definition for
				geometrically vertex decomposable ideals in [KR] using this notion.

                                An unmixed ideal $I$ in a polynomial ring $R$ is geometrically vertex
                                decomposable if it is the zero ideal, the unit ideal, an ideal generated
                                by indeterminates, or if there is a indeterminate $y$ of $R$ such that
                                two ideals $C_{y,I}$ and $N_{y,I}$ constructed from $I$ are
                                both geometrically vertex decomposable. For the complete definition, see
                                @TO isGVD@.

                                Observe that a geometrically vertex decomposable ideal is recursively
                                defined. The complexity of verifying that an ideal is geometrically
                                vertex decomposable will increase as the number of indeterminates
                                appearing in the ideal increases.

                Acknowledgement
                        We thank Sergio Da Silva, Megumi Harada, Patricia Klein, and Jenna Rajchgot for feedback and suggestions. 
                        Additionally, we thank the anonymous referees of the paper [CVT] for their concrete 
                        suggestions that significantly improved that manuscript and this package.
                        Cummings was partially supported by an NSERC USRA and CGS-M and a Milos Novotny Fellowship. 
                        Van Tuyl's research is partially supported by NSERC Discovery Grant 2019-05412.

                References

                        [CDSRVT] Mike Cummings, Sergio Da Silva, Jenna Rajchgot, and Adam Van Tuyl.
                        Geometric vertex decomposition and liaison for toric ideals of
                        graphs. Algebr. Comb., 6 (2023), no. 4, 965--997.

                        [CVT] Mike Cummings and Adam Van Tuyl.
                        The GeometricDecomposability package for Macaulay2.
                        J. Softw. Algebra Geom., 14 (2024), no. 1, 41--50.

                        [DSH] Sergio Da Silva and Megumi Harada. Geometric vertex decomposition, Gröbner bases, and Frobenius 
                        splittings for regular nilpotent Hessenberg Varieties. 
                        Transform. Groups, 2023.

                        [KMY] Allen Knutson, Ezra Miller, and Alexander Yong. Gröbner geometry of vertex
                        decompositions and of flagged tableaux. J. Reine Angew. Math. 630 (2009), 1–-31.

                        [KR] Patricia Klein and Jenna Rajchgot. Geometric vertex decomposition and
                        liaison. Forum Math. Sigma, 9 (2021) Paper No. e70, 23pp.

                        [SM] Hero Saremi and Amir Mafi. Unmixedness and arithmetic properties of
                        matroidal ideals. Arch. Math. 114 (2020), no. 3 299–-304.

                Subnodes
                        CheckCM
                        CheckDegenerate
                        CheckUnmixed
                        findLexCompatiblyGVDOrders
                        findOneStepGVD
                        getGVDIdeal
                        initialYForms
                        isGeneratedByIndeterminates
                        isGVD
                        IsIdealHomogeneous
                        IsIdealUnmixed
                        isLexCompatiblyGVD
                        isUnmixed
                        isWeaklyGVD
                        oneStepGVD
                        oneStepGVDCyI
                        oneStepGVDNyI
                        OnlyDegenerate
                        OnlyNondegenerate
                        SquarefreeOnly
                        UniversalGB
///


--******************************************************************************
-- Documentation for functions
--******************************************************************************


doc///
        Node
                Key
                        findLexCompatiblyGVDOrders
                        (findLexCompatiblyGVDOrders, Ideal)
                Headline
                        finds all lexicographic monomial orders $<$ such that the ideal is $<$-compatibly geometrically vertex decomposable
                Usage
                        findLexCompatiblyGVDOrders I
                Inputs
                        I:Ideal
                Outputs
                        :List
                                list containing all the lexicographical orders $<$ with respect to which 
                                {\tt I} is $<$-compatibly geometrically vertex decomposable

                Description

                        Text

                                An ideal $I$ is $<$-compatibly geometrically vertex decomposable if
                                there exists a (lexicographic) order $<$ such that $I$ is geometrically vertex
                                decomposable and for every (one-step) geometric vertex decomposition, we
                                pick $y$ to be the most expensive indeterminate remaining in the ideal according
                                to $<$ [KR, Definition 2.11].
                                For the definition of a (one-step) geometric vertex decomposition, see @TO oneStepGVD@.

                                This method computes all possible lex orders $<$ for which the ideal $I$ is $<$-compatibly
                                geometrically vertex decomposable.

			Example
                                R = QQ[x,y,z];
                                I = ideal(x-y, x-z);
                                findLexCompatiblyGVDOrders I

                        Text
                                The ideal in the following example is not square-free with respect to
                                any indeterminate, so no one-step geometric vertex decomposition exists.

                        Example
			        R = QQ[x,y];
                                I = ideal(x^2-y^2);
				findLexCompatiblyGVDOrders I

                Caveat
                        In the ring $k[x_1, \ldots, x_n]$, there are $n!$ possible lexicographic
                        monomial orders, so this function can be computationally expensive.


		References
		        [KR] Patricia Klein and Jenna Rajchgot. Geometric vertex decomposition and
                        liaison. Forum Math. Sigma, 9 (2021) Paper No. e70, 23pp.

                SeeAlso
                        CheckUnmixed
                        isLexCompatiblyGVD
///


doc///
        Node
                Key
                        findOneStepGVD
                        (findOneStepGVD, Ideal)
                        [findOneStepGVD, Verbose]
                Headline
                        for which indeterminates does there exist a geometric vertex decomposition
                Usage
                        findOneStepGVD I
                Inputs
                        I:Ideal
                Outputs
                        :List

                Description
                        Text
                                Returns a list containing the $y$ for which there exists a @TO oneStepGVD@.  In other words, a list
				of all the variables $y$ that satisfy ${\rm in}_y(I) = C_{y,I} \cap (N_{y,I} + \langle y \rangle)$.
                                All indeterminates $y$ which appear in the ideal are checked.

                                The results [KR, Lemma 2.6] and [KR, Lemma 2.12] are used to check whether $I$ has a geometric vertex 
                                decomposition with respect to each indeterminate $y$.
                                First, for each indeterminate $y$ appearing in the ideal, we check whether the given generators of the ideal
                                are squarefree in $y$.
                                Note that this is a sufficient but not necessary condition.
                                For the indeterminates $z$ that do not satisfy this sufficient condition, we compute a Gröbner of $I$ 
                                with respect to a $z$-compatible monomial order, and repeat the squarefree-check for the entries of this
                                Gröbner basis.

                                {\bf Warning:} if {\tt SquarefreeOnly=>true}, then the options @TO CheckUnmixed@, @TO OnlyDegenerate@, and 
                                @TO OnlyNondegenerate@ are ignored.

                        Example
                                R = QQ[x,y,z]
                                I = ideal(x-y, x-z)
                                findOneStepGVD I

                        Text
                                The following example is [KR, Example 2.16]. The variable $b$ is
                                the only indeterminate for which there exists a geometric vertex decomposition.

                        Example
                                R = QQ[a..f]
                                I = ideal(b*(c*f - a^2), b*d*e, d*e*(c^2+a*c+d*e+f^2))
                                findOneStepGVD I

                References
		        [KR] Patricia Klein and Jenna Rajchgot. Geometric vertex decomposition and
                        liaison. Forum Math. Sigma, 9 (2021) Paper No. e70, 23pp.

                SeeAlso
                        CheckUnmixed
                        oneStepGVD
                        OnlyDegenerate
                        OnlyNondegenerate
                        SquarefreeOnly
                        UniversalGB
///


doc///
        Node
                Key
                        getGVDIdeal
                        (getGVDIdeal, Ideal, List)
                Headline
                        computes the $C_{y,I}$ or $N_{y,I}$ ideal at any point in the GVD recursion tree
                Usage
                        getGVDIdeal(I, L)
                Inputs
                        I:Ideal
                        L:List
                                a nested list where each list within {\tt L} is of length two, the
                                first entry is either "C" or "N" and the second entry is an
                                indeterminate in the ring
                Outputs
                        :List
                Description
                        Text
                                The purpose of {\tt getGVDIdeal} is to return the ideal generated
                                by a sequence of choices of $C$ or $N$ ideals and corresponding
                                choices of indeterminates $y$.

				Given an ideal $I$ and variable $y_1$ in $R = k[x_1,\ldots,x_n]$, we can compute the ideals
				$C_{y_1,I}$ and $N_{y_1,I}$ (see @TO isGVD@ for the definition of these ideals).  But
				then for each of these ideals in the ring $R = k[x_1,\ldots,\hat{y_1},\ldots,x_n]$, we can
				then pick a new variable $y_2$ to form the ideals $C_{y_2,C_{y_1,I}}$, $C_{y_2,N_{y_1,I}}$,
				$N_{y_2,C_{y_1,I}}$ or $N_{y_2,N_{y_1,I}}$.  This process can be continued by now picking a new
				variable $y_3$, and finding either the $C$ or $N$ ideals of these ideals.

				The input syntax is best explained via example. The following is
                                [KR, Example 2.16]. We are given the ideal $I$.  The input
				tells us to first find $C_{y,I}$ of $I$.  Then we find $N_{s,C_{y,I}}$.

                        Example
                                R = QQ[x,y,z,w,r,s]
                                I = ideal(y*(z*s - x^2), y*w*r, w*r*(z^2+z*x+w*r+s^2))
                                getGVDIdeal(I, {{"C", y}, {"N", s}})
                References
		        [KR] Patricia Klein and Jenna Rajchgot. Geometric vertex decomposition and
                        liaison. Forum Math. Sigma, 9 (2021) Paper No. e70, 23pp.

                SeeAlso
                        CheckUnmixed
                        oneStepGVD
                        oneStepGVDCyI
                        oneStepGVDNyI
                        UniversalGB
///


doc///
       Node
                Key
                        initialYForms
                        (initialYForms, Ideal, RingElement)
                Headline
                        computes the ideal of initial y-forms
                Usage
                        initialYForms(I, y)
                Inputs
                        I:Ideal
                        y:RingElement
                                an indeterminate in the ring
                Outputs
                        :Ideal
		Description
			 Text
                                Let $y$ be a variable of the polynomial ring $R = k[x_1,\ldots,x_n]$. A monomial ordering $<$ on $R$ is said to be
			       	{\it $y$-compatible} if the initial term of $f$ satisfies ${\rm in}_<(f) = {\rm in}_<({\rm in}_y(f))$ for all $f \in R$.  Here,
			       	${\rm in}_y(f)$ is the {\it initial $y$-form} of $f$, that is, if $f = \sum_i \alpha_iy^i$ and $\alpha_d \neq 0$
			       	but $\alpha_t = 0$ for all $t >d$, then ${\rm in}_y(f) = \alpha_d y^d$.
			       	We set ${\rm in}_y(I) = \langle {\rm in}_y(f) ~|~ f \in I \rangle$ to be the ideal generated by all the initial $y$-forms in $I$

			        This routine computes the ideal of initial $y$-forms ${\rm in}_y(I)$.

                                For more on the definition of initial $y$-forms or their corresponding ideals, see [KMY, Section 2.1]. The following example is
                                [KR, Example 2.16].

                        Example
                                R = QQ[x,y,z,w,r,s]
                                I = ideal(y*(z*s - x^2), y*w*r, w*r*(z^2 + z*x + w*r + s^2))
                                initialYForms(I, y)


		References
                        [KMY] Allen Knutson, Ezra Miller, and Alexander Yong. Gröbner geometry of vertex
                        decompositions and of flagged tableaux. J. Reine Angew. Math. 630 (2009), 1–-31.

                        [KR] Patricia Klein and Jenna Rajchgot. Geometric vertex decomposition and
                        liaison. Forum Math. Sigma, 9 (2021) Paper No. e70, 23pp.
		SeeAlso
                        oneStepGVD
                        UniversalGB
///


doc///
        Node
                Key
                        isGeneratedByIndeterminates
                        (isGeneratedByIndeterminates, Ideal)
                Headline
                        checks whether the ideal is generated by indeterminates
                Usage
                        isGeneratedByIndeterminates I
                Inputs
                        I:Ideal
                Outputs
                        :Boolean
                Description
                        Text
                                An ideal is generated by indeterminates if the generators are a
                                (possibly empty) subset of the indeterminates in the ring.

                        Example
                                R = QQ[x,y]
                                isGeneratedByIndeterminates ideal 0
                                isGeneratedByIndeterminates ideal 1
                                isGeneratedByIndeterminates ideal(x,y)
                                isGeneratedByIndeterminates ideal(x*y)

                SeeAlso
                        isGVD
                        isLexCompatiblyGVD
                        isWeaklyGVD
///


doc///
        Node
                Key
                        isGVD
                        (isGVD, Ideal)
                        [isGVD, Verbose]
                Headline
                        checks whether an ideal is geometrically vertex decomposable
                Usage
                        isGVD I
                Inputs
                        I:Ideal
                Outputs
                        :Boolean
                Description
                        Text
                                This function tests whether a given ideal is geometrically vertex decomposable.
		                Geometrically vertex decomposable ideals are based upon the geometric vertex
				decomposition defined by Knutson, Miller, and Yong [KMY].  Using geometric
				vertex decomposition, Klein and Rajchgot gave a recursive definition for
				geometrically vertex decomposable ideals in [KR, Definition 2.7].  This definition generalizes the properties
				of a square-free monomial ideal whose associated simplicial complex is vertex decomposable.

                                We include the definition here.
				Let $y$ be a variable of the polynomial ring $R = k[x_1,\ldots,x_n]$. A monomial ordering $<$ on $R$ is said to be
                                {\it $y$-compatible} if the initial term of $f$ satisfies ${\rm in}_<(f) = {\rm in}_<({\rm in}_y(f))$ for all $f \in R$.  Here,
				${\rm in}_y(f)$ is the {\it initial $y$-form} of $f$, that is, if $f = \sum_i \alpha_iy^i$ and $\alpha_d \neq 0$
				but $\alpha_t = 0$ for all $t >d$, then ${\rm in}_y(f) = \alpha_d y^d$.
				We set ${\rm in}_y(I) = \langle {\rm in}_y(f) ~|~ f \in I \rangle$ to be the ideal generated by all the initial $y$-forms in $I$.

                                Given an ideal $I$ and a $y$-compatible monomial ordering $<$, let $G(I) = \{ g_1,\ldots,g_m\}$ be a Gröbner basis of $I$ with respect to this
                                ordering.  For $i=1,\ldots,m$, write $g_i$ as $g_i = y^{d_i}q_i + r_i$, where $y$ does not divide any term of $q_i$;
                                that is, ${\rm in}_y(g_i) = y^{d_i}q_i$.   Given this setup, we define two ideals:
                                $$C_{y,I} = \langle q_1,\ldots,q_m\rangle$$
                                and
                                $$N_{y,I} = \langle q_i ~|~ d_i = 0 \rangle.$$
                                Recall that an ideal $I$ is {\it unmixed} if all of the associated primes of $I$ have the same height.

                                An ideal $I$ of $R =k[x_1,\ldots,x_n]$ is {\it geometrically vertex decomposable} if $I$ is unmixed and

                                (1)  $I = \langle 1 \rangle$, or $I$ is generated by a (possibly empty) subset of variables of $R$, or

                                (2) there is a variable $y = x_i$ in $R$ and a $y$-compatible monomial ordering $<$ such that
                                        $${\rm in}_y(I) = C_{y,I} \cap (N_{y,I} + \langle y \rangle),$$
                                        and the contractions of the
                                        ideals $C_{y,I}$ and $N_{y,I}$ to the ring
                                        $k[x_1,\ldots,\hat{y},\ldots,x_n]$ are geometrically
                                        vertex decomposable.

                        	{\it NOTE:}  The ideals $C_{y,I}$ and $N_{y,I}$ do not depend upon the choice of the Gröbner basis or
                        	a particular $y$-compatible order (see comment after [KR, Definition 2.3]).
                        	When computing $C_{y,I}$ and $N_{y,I}$ we use a lexicographical ordering
                        	on $R$ where $y > x_j$ for all $i \neq j$ if $y = x_i$ since this gives us a $y$-compatible order.

                        Example
                	        R = QQ[a,b,c,d]
                		f = 3*a*b + 4*b*c+ 16*a*c + 18*d
                		i = ideal f
                		isGVD i

                        Text
                	        Square-free monomial ideals that are geometrically vertex decomposable are precisely those square-free monomial ideals
                		whose associated simplicial complex are vertex decomposable [KR, Proposition 2.9].
				The edge ideal of a chordal graph corresponds to a simplicial
                		complex that is vertex decomposable (for more, see the EdgeIdeals package).  The option {\tt Verbose} shows the intermediate steps; in particular, {\tt Verbose}
				displays what variable is being used to test a decomposition, as well as the ideals
				$C_{y,I}$ and $N_{y,I}$.


                        Example
                                R = QQ[a,b,c,d]
                                i = ideal(a*b, a*c, a*d, b*c, b*d, c*d) -- edge ideal of a complete graph K_4, a chordal graph
                                isGVD(i, Verbose=>true)

                        Text
                                The following is an example of a toric ideal of graph that is geometrically vertex decomposable, and another example
                		of a toric ideal of a graph that is not geometrically vertex decomposable. The second ideal is not Cohen-Macaulay, so it
                		cannot be geometrically vertex decomposable [KR, Corollary 4.5].
                                For background on toric ideals of graphs, see [CDSRVT, Section 3].

                        Example
                	        R = QQ[e_1..e_7]
                		i = ideal(e_2*e_7-e_5*e_6, e_1*e_4-e_2*e_3) -- the toric ideal of a graph
                		isGVD i
                	        R = QQ[e_1..e_10]
                		i = ideal(e_1*e_4-e_2*e_3, e_2^2*e_7*e_8*e_9-e_4^2*e_5*e_6*e_10, e_1*e_2*e_7*e_8*e_9-e_3*e_4*e_5*e_6*e_10, e_1^2*e_7*e_8*e_9-e_3^2*e_5*e_6*e_10)
                		isGVD i
		References
                        [CDSRVT] Mike Cummings, Sergio Da Silva, Jenna Rajchgot, and Adam Van Tuyl.
                        Geometric vertex decomposition and liaison for toric ideals of
                        graphs. Algebr. Comb., 6 (2023), no. 4, 965--997.

                        [KMY] Allen Knutson, Ezra Miller, and Alexander Yong. Gröbner geometry of vertex
                        decompositions and of flagged tableaux. J. Reine Angew. Math. 630 (2009), 1–-31.

                        [KR] Patricia Klein and Jenna Rajchgot. Geometric vertex decomposition and
                        liaison. Forum Math. Sigma, 9 (2021) Paper No. e70, 23pp.

                SeeAlso
                        CheckCM
                        CheckUnmixed
                        isGeneratedByIndeterminates
                        IsIdealHomogeneous
                        IsIdealUnmixed
                        isLexCompatiblyGVD
                        isUnmixed
                        isWeaklyGVD
                        oneStepGVD
                        UniversalGB
                        Verbose
///


doc///
        Node
                Key
                        isLexCompatiblyGVD
                        (isLexCompatiblyGVD, Ideal, List)
                        [isLexCompatiblyGVD, Verbose]
                Headline
                        checks whether an ideal is <-compatibly geometrically vertex decomposable for a given order
                Usage
                        isLexCompatiblyGVD(I, L)
                Inputs
                        I:Ideal
                        L:List
                Outputs
                        :Boolean
                Description
		 	Text
                                An ideal $I$ is $<$-compatibly geometrically vertex decomposable if
                                there exists a (lexicographic) order $<$ such that $I$ is geometrically vertex
                                decomposable and for every (one-step) geometric vertex decomposition, we
                                pick $y$ to be the most expensive indeterminate remaining in the ideal according
                                to $<$ [KR, Definition 2.11].
                                For the definition of a (one-step) geometric vertex decomposition, see @TO oneStepGVD@.

                                This method returns a Boolean value depending upon whether or not
				the given ideal is $<$-compatibly geometrically vertex decomposable with
				respect to a given ordering lex ordering of the indeterminates.
				Compare this function to the command @TO findLexCompatiblyGVDOrders@ which checks all possible lex
				orders of the variables in order to find at least one $<$-compatibly lex order.

				Below is [KR, Example 2.16], which is an example of an ideal that is not $<$-compatibly geometrically
				vertex decomposable. Any permutation of the variables we give in this example will result in {\tt false}.
			Example
			        R = QQ[x,y,z,w,r,s];
                                I = ideal(y*(z*s - x^2), y*w*r, w*r*(z^2 + z*x + w*r + s^2));
				isLexCompatiblyGVD(I, {x,y,z,w,r,s})
				isLexCompatiblyGVD(I, {s,x,w,y,r,z}, Verbose=>true)
                        Text
                                In view of [KR, Proposition 2.14], we check whether the initial ideal ${\rm in}_<(I)$ is $<$-compatibly
                                geometrically vertex decomposable.
                                Heuristically, one should expect this check to be quicker than the definition [KR, Definition 2.11] 
                                since checking unmixedness (that is, computing a primary decomposition) is in general faster for monomial ideals.
                                
                        
                References
		        [KR] Patricia Klein and Jenna Rajchgot. Geometric vertex decomposition and
                        liaison. Forum Math. Sigma, 9 (2021) Paper No. e70, 23pp.


                SeeAlso
                        CheckCM
                        CheckUnmixed
                        isGeneratedByIndeterminates
                        isGVD
                        IsIdealHomogeneous
                        IsIdealUnmixed
                        isUnmixed
                        isWeaklyGVD
                        oneStepGVD
                        UniversalGB
                        Verbose
///


doc///
        Node
                Key
                        isUnmixed
                        (isUnmixed, Ideal)
                Headline
                        checks whether an ideal is unmixed
                Usage
                        isUnmixed I
                Inputs
                        I:Ideal
                Outputs
                        :Boolean
                Description
		        Text
			        A function that checks whether an ideal $I \subseteq R$ is unmixed, that is, whether the ideal $I$
                                satisfies $\dim(R/I) = \dim(R/P)$ for all associated primes $P \in {\rm Ass}_R(R/I)$.

			        The following example uses  [SM, Example 1.6].
		        Example
			        R = QQ[x_1..x_5];
                                I = ideal(x_1*x_3, x_1*x_4, x_1*x_5, x_2*x_3, x_2*x_4, x_2*x_5);
				isUnmixed I
		References
		        [SM] Hero Saremi and Amir Mafi. Unmixedness and arithmetic properties of
                        matroidal ideals. Arch. Math. 114 (2020), no. 3 299–-304.
                SeeAlso
                        CheckUnmixed
                        isGVD
                        IsIdealUnmixed
                        isLexCompatiblyGVD
                        isWeaklyGVD
///


doc///
        Node
                Key
                        isWeaklyGVD
                        (isWeaklyGVD, Ideal)
                        [isWeaklyGVD, Verbose]
                Headline
                        checks whether an ideal is weakly geometrically vertex decomposable
                Usage
                        isWeaklyGVD I
                Inputs
                        I:Ideal
                Outputs
                        :Boolean
                Description
		        Text
			        This function tests whether an ideal $I \subseteq k[x_1,\ldots,x_n]$ is weakly geometrically vertex decomposable [KR, Definition 4.6].

				See @TO isGVD@ for the definition of the ideals $C_{y,I}$ and $N_{y,I}$ used below. We say that a geometric
				vertex decomposition is {\it degenerate} if $C_{y,I} = \langle 1 \rangle$ or if $\sqrt{C_{y,I}} = \sqrt{N_{y,I}}$.
                                The geometric vertex decomposition is {\it nondegenerate} otherwise.

				An ideal $I \subseteq R = k[x_1, \ldots, x_n]$ is {\it weakly geometrically vertex decomposable} if $I$ is unmixed and

                                (1) $I = \langle 1 \rangle$, or $I$ is generated by a (possibly empty) subset of variables of $R$, or

				(2) (Degenerate Case) for some variable $y = x_j$ of $R$, ${\rm in}_y(I) = C_{y,I} \cap (N_{y,I} + \langle y \rangle)$ is
				a degenerate geometric vertex decomposition and the contraction of $N_{y,I}$ to the ring $k[x_1,\ldots,\hat{y},\ldots,x_n]$
				is weakly geometrically vertex decomposable, or

				(3) (Nondegenerate Case) for some variable $y = x_j$ of $R$,  ${\rm in}_y(I) = C_{y,I} \cap (N_{y,I} + \langle y \rangle)$ is
				a nondegenerate geometric vertex decomposition, the contraction of $C_{y,I}$ to the ring  $k[x_1,\ldots,\hat{y},\ldots,x_n]$
				is weakly geometrically vertex decomposable, and $N_{y,I}$ is radical and Cohen-Macaulay.

		                The following example is [KR, Example 4.10]. It is an example of an ideal that is weakly geometrically
				vertex decomposable, but not geometrically vertex decomposable.
		        Example
                	        R = QQ[x,y,z,w,r,s];
                                I = ideal(y*(z*s - x^2), y*w*r, w*r*(x^2 + s^2 + z^2 + w*r));
				isWeaklyGVD I
				isGVD I

                References
        	        [KR] Patricia Klein and Jenna Rajchgot. Geometric vertex decomposition and
                        liaison. Forum Math. Sigma, 9 (2021) Paper No. e70, 23pp.

                SeeAlso
                        CheckUnmixed
                        isGeneratedByIndeterminates
                        isGVD
                        IsIdealUnmixed
                        isLexCompatiblyGVD
                        isUnmixed
                        oneStepGVD
                        UniversalGB
                        Verbose
///


doc///
       Node
                Key
                        oneStepGVD
                        (oneStepGVD, Ideal, RingElement)
                        [oneStepGVD, Verbose]
                Headline
                        computes a geometric vertex decomposition
                Usage
                        oneStepGVD(I, y)
                Inputs
                        I:Ideal
                        y:RingElement
                                an indeterminate in the ring
                Outputs
                        :Sequence
                                containing whether the $C_{y,I}$ and $N_{y,I}$ ideals form
                                a valid geometric vertex decomposition, the ideals $C_{y,I}$ and $N_{y,I}$, and if
                                {\tt CheckDegenerate=>true}, whether the one-step decomposition
                                is degenerate or nondegenerate
		Description
			 Text
                                This function computes a geometric vertex decomposition of an ideal based upon work of Knutson,
				Miller, and Yong [KMY, Theorem 2.1].  Geometric vertex decomposition is the key step in the recursive
			        definition of geometrically vertex decomposable ideals.  The function {\tt oneStepGVD} is repeatedly used by @TO isGVD@ to determine
				if an ideal is a geometrically vertex decomposable ideal.

				Let $y$ be a variable of the polynomial ring $R = k[x_1,\ldots,x_n]$. A monomial ordering $<$ on $R$ is said to be
                                {\it $y$-compatible} if the initial term of $f$ satisfies ${\rm in}_<(f) = {\rm in}_<({\rm in}_y(f))$ for all $f \in R$.  Here,
				${\rm in}_y(f)$ is the {\it initial $y$-form} of $f$, that is, if $f = \sum_i \alpha_iy^i$ and $\alpha_d \neq 0$
				but $\alpha_t = 0$ for all $t >d$, then ${\rm in}_y(f) = \alpha_d y^d$.
				We set ${\rm in}_y(I) = \langle {\rm in}_y(f) ~|~ f \in I \rangle$ to be the ideal generated by all the initial $y$-forms in $I$.

                                Given an ideal $I$ and a $y$-compatible monomial ordering $<$, let $G(I) = \{ g_1,\ldots,g_m\}$ be a Gröbner basis of $I$ with respect to this
                                ordering.  For $i=1,\ldots,m$, write $g_i$ as $g_i = y^{d_i}q_i + r_i$, where $y$ does not divide any term of $q_i$;
                                that is, ${\rm in}_y(g_i) = y^{d_i}q_i$.   Given this setup, we define two ideals:
                                $$C_{y,I} = \langle q_1,\ldots,q_m\rangle$$
                                and
                                $$N_{y,I} = \langle q_i ~|~ d_i = 0 \rangle.$$

                                If ${\rm in}_y(I) = C_{y,I} \cap (N_{y,I} + \langle y \rangle),$
                                then we call this decomposition a {\it geometric vertex decomposition of $I$}.

                                Furthermore, we say that a geometric vertex decomposition is {\it degenerate} if
                                $C_{y,I} = \langle 1 \rangle$ or if $\sqrt{C_{y,I}} = \sqrt{N_{y,I}}$.
                                The geometric vertex decomposition is {\it nondegenerate} otherwise.

				For a given variable $y$, the function {\tt oneStepGVD} returns a sequence, where the first element in the sequence is true or false
				depending if the given variable $y$ gives a geometric vertex decomposition of $I$, while the second element is the
				ideal $C_{y,I}$ and the third element in the sequence is the ideal $N_{y,I}$.
                                If {\tt CheckDegenerate=>true}, then there is a fourth element in the output, either "degenerate" or "nondegenerate", corresponding
                                to whether the geometric vertex decomposition is degenerate.

				{\it Note:}  The ideals $C_{y,I}$ and $N_{y,I}$ do not depend upon the choice of the Gröbner basis or
                        	a particular $y$-compatible order (see comment after Definition 2.3 of [KR]).
                        	When computing $C_{y,I}$ and $N_{y,I}$ we use a lexicographical ordering
                        	on $R$ where $y > x_j$ for all $i \neq j$ if $y = x_i$ since this gives us a $y$-compatible order.
			Example
			        R = QQ[a,b,c,d]
                		f = 3*a*b + 4*b*c+ 16*a*c+18*d
                		i = ideal f
                		oneStepGVD(i, a)

                        Text
                                In the example below, the ideal $I$ is the edge ideal of the complete graph $K_4$.  We also check
				if the decomposition is degenerate (see @TO CheckDegenerate@).
                                For more on edge ideals, see the EdgeIdeals package.

                        Example
                                R = QQ[a,b,c,d];
                                i = ideal(a*b, a*c, a*d, b*c, b*d, c*d); -- edge ideal of complete graph K_4, a chordal graph
                                oneStepGVD(i, c, CheckDegenerate=>true)
			Text
			        The example below is the toric ideal of a graph such that the quotient ring is not Cohen-Macaulay.  By [KR, Lemma 2.6], for an ideal $I$
				to have a geometric vertex decomposition with respect to the variable $y$, no term of
				the Gröbner bases can be divided by $y^2$.  In this example, the Gröbner basis of $I$ contains an element with a term
				divisible by $e_1^2$. So $I$ does not have a geometric vertex decomposition with respect to $y = e_1$.
                                For background on toric ideals of graphs, see [CDSRVT, Section 3].

			Example
                	        R = QQ[e_1..e_10];
                		i = ideal(e_1*e_4-e_2*e_3, e_2^2*e_7*e_8*e_9-e_4^2*e_5*e_6*e_10, e_1*e_2*e_7*e_8*e_9-e_3*e_4*e_5*e_6*e_10, e_1^2*e_7*e_8*e_9-e_3^2*e_5*e_6*e_10);
                		mingens gb i
				oneStepGVD(i, e_1)

		References
                        [CDSRVT] Mike Cummings, Sergio Da Silva, Jenna Rajchgot, and Adam Van Tuyl.
                        Geometric vertex decomposition and liaison for toric ideals of
                        graphs. Algebr. Comb., 6 (2023), no. 4, 965--997.

                        [KMY] Allen Knutson, Ezra Miller, and Alexander Yong. Gröbner geometry of vertex
                        decompositions and of flagged tableaux. J. Reine Angew. Math. 630 (2009), 1–-31.

                        [KR] Patricia Klein and Jenna Rajchgot. Geometric vertex decomposition and
                        liaison. Forum Math. Sigma, 9 (2021) Paper No. e70, 23pp.
		SeeAlso
                        CheckDegenerate
                        CheckUnmixed
                        findOneStepGVD
                        getGVDIdeal
                        isGVD
                        isLexCompatiblyGVD
                        isWeaklyGVD
                        oneStepGVDCyI
                        oneStepGVDNyI
                        UniversalGB
                        Verbose
///


doc///
        Node
                Key
                        oneStepGVDCyI
                        (oneStepGVDCyI, Ideal, RingElement)
                Headline
                        computes the ideal $C_{y,I}$ for a given ideal and indeterminate
                Usage
                        oneStepGVDCyI(I, y)
                Inputs
                        I:Ideal
                        y:RingElement
                                an indeterminate in the ring
                Outputs
                        :Ideal

                Caveat
                        This method is a shortcut to extract the ideal $C_{y,I}$ as computed
                        in @TO oneStepGVD@. That is, to compute $C_{y,I}$, {\tt oneStepGVD} is called in the background.
                        As a result, work is also done in the background to compute $N_{y,I}$ at
                        the same time, and as such, we encourage calling {\tt oneStepGVD}
                        directly if we want both the $C_{y,I}$ and $N_{y,I}$ ideals to avoid
                        performing the same computation twice.

	        Description
	                Text
			        Let $y$ be a variable of the polynomial ring $R = k[x_1,\ldots,x_n]$. A monomial ordering $<$ on $R$ is said to be
                                {\it $y$-compatible} if the initial term of $f$ satisfies ${\rm in}_<(f) = {\rm in}_<({\rm in}_y(f))$ for all $f \in R$.
				Here, ${\rm in}_y(f)$ is the {\it initial $y$-form} of $f$, that is, if $f = \sum_i \alpha_iy^i$ and $\alpha_d \neq 0$
				but $\alpha_t = 0$ for all $t >d$, then ${\rm in}_y(f) = \alpha_d y^d$.

                                Given an ideal $I$ and a $y$-compatible monomial ordering $<$, let $G(I) = \{ g_1,\ldots,g_m\}$ be a Gröbner basis of $I$ with respect to this
                                ordering.  For $i=1,\ldots,m$, write $g_i$ as $g_i = y^{d_i}q_i + r_i$, where $y$ does not divide any term of $q_i$;
                                that is, ${\rm in}_y(g_i) = y^{d_i}q_i$.   Given this setup, the ideal $C_{y,I}$ is given by
                                $$C_{y,I} = \langle q_1,\ldots,q_m\rangle$$
			        This functions  takes an ideal $I$ and variable $y$, and returns $C_{y,I}$.

                                The ideal $C_{y,I}$ does not depend upon the choice of the Gröbner basis or
                        	a particular $y$-compatible order (see comment after [KR, Definition 2.3]).
				When computing $C_{y,I}$ we use a lexicographical ordering
                        	on $R$ where $y > x_j$ for all $i \neq j$ if $y = x_i$ since this gives us a $y$-compatible order.

                                The ideal $I$ in the example below is the edge ideal of the complete graph $K_4$.
                                For more on edge ideals, see the EdgeIdeals package.

                        Example
                                R = QQ[a,b,c,d];
                                I = ideal(a*b, a*c, a*d, b*c, b*d, c*d); -- edge ideal of the complete graph K_4, a chordal graph
                                oneStepGVDCyI(I, b)
				L = oneStepGVD(I, b);
			        L_1 == oneStepGVDCyI(I, b) -- CyI is the second element in the list given by oneStepGVD
    	    	References
		        [KR] Patricia Klein and Jenna Rajchgot. Geometric vertex decomposition and
                        liaison. Forum Math. Sigma, 9 (2021) Paper No. e70, 23pp.
                SeeAlso
                        CheckUnmixed
                        getGVDIdeal
                        oneStepGVD
                        oneStepGVDNyI
                        UniversalGB
///


doc///
        Node
                Key
                        oneStepGVDNyI
                        (oneStepGVDNyI, Ideal, RingElement)
                Headline
                        computes the ideal $N_{y,I}$ for a given ideal and indeterminate
                Usage
                        oneStepGVDNyI(I, y)
                Inputs
                        I:Ideal
                        y:RingElement
                                an indeterminate in the ring
                Outputs
                        :Ideal

                Caveat
                        This method is a shortcut to extract the ideal $N_{y,I}$ as computed
                        in @TO oneStepGVD@. That is, to compute $N_{y,I}$, {\tt oneStepGVD} is called in the background.
                        As a result, work is also done in the background to compute $C_{y,I}$ at
                        the same time, and as such, we encourage calling {\tt oneStepGVD}
                        directly if we want both the $C_{y,I}$ and $N_{y,I}$ ideals to avoid
                        performing the same computation twice.
                Description
                        Text
                                Let $y$ be a variable of the polynomial ring $R = k[x_1,\ldots,x_n]$. A monomial ordering $<$ on $R$ is said to be
                                {\it $y$-compatible} if the initial term of $f$ satisfies ${\rm in}_<(f) = {\rm in}_<({\rm in}_y(f))$ for all $f \in R$.
				Here,
				${\rm in}_y(f)$ is the {\it initial $y$-form} of $f$, that is, if $f = \sum_i \alpha_iy^i$ and $\alpha_d \neq 0$
				but $\alpha_t = 0$ for all $t >d$, then ${\rm in}_y(f) = \alpha_d y^d$.

                                Given an ideal $I$ and a $y$-compatible monomial ordering $<$, let $G(I) = \{ g_1,\ldots,g_m\}$ be a Gröbner basis of $I$ with respect to this
                                ordering.  For $i=1,\ldots,m$, write $g_i$ as $g_i = y^{d_i}q_i + r_i$, where $y$ does not divide any term of $q_i$;
                                that is, ${\rm in}_y(g_i) = y^{d_i}q_i$.   Given this setup, the ideal $N_{y,I}$ is given by
                                $$N_{y,I} = \langle q_i ~|~ d_i = 0\rangle$$
                                This functions  takes an ideal $I$ and variable $y$, and returns $N_{y,I}$

                                The ideal $N_{y,I}$ does not depend upon the choice of the Gröbner basis or
                        	a particular $y$-compatible order (see comment after [KR, Definition 2.3]).
				When computing $N_{y,I}$ we use a lexicographical ordering
                        	on $R$ where $y > x_j$ for all $i \neq j$ if $y = x_i$ since this gives us a $y$-compatible order.

                                The ideal $I$ in the example below is the edge ideal of the complete graph $K_4$.
                                For more on edge ideals, see the EdgeIdeals package.

                        Example
                                R = QQ[a,b,c,d];
                                I = ideal(a*b, a*c, a*d, b*c, b*d, c*d); -- edge ideal of a complete graph K_4, a chordal graph
                                oneStepGVDNyI(I, b)
                                L = oneStepGVD(I, b);
                                L_2 == oneStepGVDNyI(I, b) -- NyI is the second element in the list given by oneStepGVD
		References
		        [KR] Patricia Klein and Jenna Rajchgot. Geometric vertex decomposition and
                        liaison. Forum Math. Sigma, 9 (2021) Paper No. e70, 23pp.

		SeeAlso
                        CheckUnmixed
                        getGVDIdeal
                        oneStepGVDCyI
                        oneStepGVD
                        UniversalGB
///


--******************************************************************************
-- Documentation for optional inputs
--******************************************************************************


doc///
        Node
                Key
                        CheckCM
                        [isGVD, CheckCM]
                        [isLexCompatiblyGVD, CheckCM]
                Headline
                        when to perform a Cohen-Macaulay check on the ideal
                Description
                        Text
                                Whether to check that the ideal is geometrically vertex
                                decomposable using the result of [KR, Corollary 4.5] which relates the
                                geometrically vertex decomposable and Cohen-Macaulay properties.
                                Set {\tt CheckCM=>"once"} to perform this check once, that is, only for the
                                ideal given in the input; {\tt CheckCM=>"always"} (default) for
                                all of the following $C_{y,I}$ and $N_{y,I}$ ideals as well; or
                                {\tt CheckCM=>"never"}.

                                In particular, [KR, Corollary 4.5] states that if a homogeneous ideal $I$ is 
                                geometrically vertex decomposable then $I$ must be Cohen-Macaulay.
                                Equivalently, if a homogeneous ideal $I$ is not Cohen-Macaulay, then it is 
                                not geometrically vertex decomposable.
                                By the Auslander-Buchsbaum formula, if $I$ is homogeneous, then Cohen-Macualayness
                                is equivalent to equality of projective dimension and codimension, which is 
                                in general a quicker check than using isCM from the @TO Depth@ package.

                                Since the result of [KR, Corollary 4.5] holds only for homogeneous ideals, the 
                                Cohen-Macaulayness is checked only when the given ideal is homogeneous, no matter the 
                                value of {\tt CheckCM}.

                                We set the default value for {\tt CheckCM} to {\tt "always"} for, if the ideal is 
                                homogeneous, it is quicker to check equality of projective dimension and codimension
                                than it is to check unmixedness, and Cohen-Macaulay impilies unmixed.
                                Hence if an ideal is homogeneous, we need only check unmixedness directly 
                                when it is not Cohen-Macaulay.


                References
                        [KR] Patricia Klein and Jenna Rajchgot. Geometric vertex decomposition and
                        liaison. Forum Math. Sigma, 9 (2021) Paper No. e70, 23pp.

                SeeAlso
                        isGVD
                        isLexCompatiblyGVD
///


doc///
        Node
                Key
                        CheckDegenerate
                        [oneStepGVD, CheckDegenerate]
                Headline
                        check whether the geometric vertex decomposition is degenerate
                Description
                        Text
                                A geometric vertex decomposition is degenerate if
                                $\sqrt{C_{y,I}} = \sqrt{N_{y,I}}$ or if $C_{y,I} = \langle 1 \rangle$,
                                and nondegenerate otherwise [KR, Section 2.2].

                                If {\tt CheckDegenerate=>true}, then {\tt oneStepGVD} returns
                                a sequence of length four, where the fourth entry is either
                                {\tt "degenerate"} or {\tt "nondegenerate"}.
                                Otherwise, {\tt oneStepGVD} does not check whether the geometric
                                vertex decomposition is degenerate and the sequence in the output has length three.

                                Note that the degeneracy of a geometric vertex decomposition does not matter
                                with regards to whether an ideal is geometrically vertex decomposable.
                                As a result, @TO isGVD@ does not check this. However, the definition
                                of weakly geometrically vertex decomposable depends the
                                one-step geometric vertex decomposition at each step is degenerate, so
                                @TO isWeaklyGVD@ asks for this check.

                        Example
                                R = QQ[x,y,z]
                                I = ideal(x-y, x-z)
                                oneStepGVD(I, x, CheckDegenerate=>true)

                References
                        [KR] Patricia Klein and Jenna Rajchgot. Geometric vertex decomposition and
                        liaison. Forum Math. Sigma, 9 (2021) Paper No. e70, 23pp.

                SeeAlso
                        isWeaklyGVD
                        oneStepGVD
///


doc///
        Node
                Key
                        CheckUnmixed
                        [findLexCompatiblyGVDOrders, CheckUnmixed]
                        [findOneStepGVD, CheckUnmixed]
                        [getGVDIdeal, CheckUnmixed]
                        [isGVD, CheckUnmixed]
                        [isLexCompatiblyGVD, CheckUnmixed]
                        [isWeaklyGVD, CheckUnmixed]
                        [oneStepGVD, CheckUnmixed]
                        [oneStepGVDCyI, CheckUnmixed]
                        [oneStepGVDNyI, CheckUnmixed]
                Headline
                        check whether ideals encountered are unmixed
                Description
                        Text

                                If set to {\tt false}, the program never checks whether the ideal $I$ or
                                any $C_{y,I}$ or $N_{y,I}$ ideals are unmixed. Setting {\tt CheckUnmixed=>false}
                                will speed up computations since it is not performing a check of this condition but comes
                                at the cost that not all the necessary conditions are checked.
                                Notice that if {\tt isGVD(I, CheckUnmixed=>false)} returns {\tt false}, then $I$ is
                                conclusively not geometrically vertex decomposable as there is some other condition
                                that is not met.
                                The default value is {\tt true}.

                                If you know that $I$ is unmixed but want to check unmixedness for $C_{y,I}$, $N_{y,I}$,
                                and any later ideals, use @TO IsIdealUnmixed@ instead.

                                The following is not unmixed [SM, Example 1.6] and hence not geometrically vertex
                                decomposable. However, if we disable the unmixedness check and skip the Cohen-Macaulay check,
                                {\tt isGVD} returns true.
                        Example
                                R = QQ[x_1..x_5]
                                I = ideal(x_1*x_3, x_1*x_4, x_1*x_5, x_2*x_3, x_2*x_4, x_2*x_5)
                                isUnmixed I
                                isGVD(I, CheckCM=>"never", CheckUnmixed=>false)

                Caveat
                        As in the above example, if you set {\tt CheckUnmixed=>false} and you do not already know
                        that both $I$ is unmixed and all later $C_{y,I}$ and $N_{y,I}$ ideals are unmixed, then the
                        output of @TO isGVD@ or any other GVD method cannot definitely conclude that $I$ is geometrically
                        vertex decomposable, as not all of conditions in the definition were checked.

                References
                        [SM] Hero Saremi and Amir Mafi. Unmixedness and arithmetic properties of
                        matroidal ideals. Arch. Math. 114 (2020), no. 3 299–-304.

                SeeAlso
                        findLexCompatiblyGVDOrders
                        findOneStepGVD
                        getGVDIdeal
                        isGVD
                        IsIdealUnmixed
                        isLexCompatiblyGVD
                        isUnmixed
                        isWeaklyGVD
                        oneStepGVD
                        oneStepGVDCyI
                        oneStepGVDNyI
///


doc///
        Node
                Key
                        IsIdealHomogeneous
                        [isGVD, IsIdealHomogeneous]
                        [isLexCompatiblyGVD, IsIdealHomogeneous]
                Headline
                        specify whether an ideal is homogeneous
                Description
                        Text
                                Whether the input ideal is homogeneous, if known.
                                The value of this input is only used if {\tt CheckCM=>true}.

                                If an ideal $I$ is homogeneous and has a geometric vertex decomposition with respect to 
                                an indeterminate $y$, which is to say that 
                                ${\rm in}_y(I) = C_{y, I} \cap (N_{y, I} + \langle y \rangle)$,
                                then both $C_{y, I}$ and $N_{y, I}$ are also homogeneous.
                                Also, an ideal that is both homogeneous and geometrically vertex decomposable is 
                                Cohen-Macaulay [KR, Corollary 4.5].

                References
                        [KR] Patricia Klein and Jenna Rajchgot. Geometric vertex decomposition and
                        liaison. Forum Math. Sigma, 9 (2021) Paper No. e70, 23pp.

                SeeAlso
                        CheckCM
                        isGVD
                        isLexCompatiblyGVD
///


doc///
        Node
                Key
                        IsIdealUnmixed
                        [isGVD, IsIdealUnmixed]
                        [isLexCompatiblyGVD, IsIdealUnmixed]
                        [isWeaklyGVD, IsIdealUnmixed]
                Headline
                        specify whether an ideal is unmixed
                Description
                        Text
                                Specify {\tt IsIdealUnmixed=>true} if it is known {\em a priori}
                                that an ideal is unmixed. In this case, the program will not
                                check whether the given ideal $I$ is unmixed -- it will assume
                                that it is unmixed -- but it will check whether $C_{y,I}$ and
                                $N_{y,I}$ are unmixed, as well as any ideals defined from further
                                degenerations.
                                The default value is {\tt false} and in this case, the unmixedness
                                property will be checked for $I$ and all later ideals.

                                To always skip the unmixedness check (perhaps you know that
                                every ideal you will encounter through repeated geometric vertex decompositions
                                will always be unmixed), use @TO CheckUnmixed@.

                SeeAlso
                        CheckUnmixed
                        isGVD
                        isLexCompatiblyGVD
                        isUnmixed
                        isWeaklyGVD
///


doc///
        Node
                Key
                        OnlyDegenerate
                        [findOneStepGVD, OnlyDegenerate]
                Headline
                        restrict to degenerate geometric vertex decompositions
                Description
                        Text
                                Set to {\tt true} to restrict the output of @TO findOneStepGVD@ to return only
                                the indeterminates for which their geometric vertex decomposition is degenerate.
                                Default value {\tt false}.
                                Following the notation of @TO oneStepGVD@, a geometric vertex decomposition is {\bf degenerate}
                                if either $\sqrt{C_{y, I}} = \sqrt{N_{y, I}}$ or if $C_{y, I} = \langle 1 \rangle$, and 
                                is {\bf nondegenerate} otherwise.

                SeeAlso
                        findOneStepGVD
                        oneStepGVD
                        OnlyNondegenerate
///


doc///
        Node
                Key
                        OnlyNondegenerate
                        [findOneStepGVD, OnlyNondegenerate]
                Headline
                        restrict to nondegenerate geometric vertex decompositions
                Description
                        Text
                                Set to {\tt true} to restrict the output of @TO findOneStepGVD@ to return only
                                the indeterminates for which their geometric vertex decomposition is nondegenerate.
                                Default value {\tt false}.
                                Following the notation of @TO oneStepGVD@, a geometric vertex decomposition is {\bf degenerate}
                                if either $\sqrt{C_{y, I}} = \sqrt{N_{y, I}}$ or if $C_{y, I} = \langle 1 \rangle$, and 
                                is {\bf nondegenerate} otherwise.
                SeeAlso
                        findOneStepGVD
                        OnlyDegenerate
///


doc///
        Node
                Key
                        SquarefreeOnly
                        [findOneStepGVD, SquarefreeOnly]
                Headline
                        only return the squarefree variables from the generators
                Description
                        Text
                                The algorithm for @TO findOneStepGVD@ comprises two steps.
                                First, it checks the given generators for the given ideal $I$ and creates a list 
                                of all indeterminates which appear squarefree in all of the generators.
                                For each of the remaining variables $y$, it then computes a Gröbner basis for $I$
                                with respect to a $y$-compatible monomial order.
                                If $y$ appears in the elements of the Gröbner basis with only degree zero or degree one,
                                then we have a geometric vertex decomposition, and $y$ is appended to the list of 
                                indeterminates.

                                If {\tt SquarefreeOnly=>true}, then only the first half of the algorithm runs.
                                This option is used by the @TO isGVD@ and @TO isWeaklyGVD@ functions to avoid 
                                unnecessary duplicate computations of Gröbner bases.
                SeeAlso
                        findOneStepGVD
///


doc///
        Node
                Key
                        UniversalGB
                        [findOneStepGVD, UniversalGB]
                        [getGVDIdeal, UniversalGB]
                        [isGVD, UniversalGB]
                        [isLexCompatiblyGVD, UniversalGB]
                        [isWeaklyGVD, UniversalGB]
                        [oneStepGVD, UniversalGB]
                        [oneStepGVDCyI, UniversalGB]
                        [oneStepGVDNyI, UniversalGB]
                        [initialYForms, UniversalGB]
                Headline
                        whether the generators for an ideal form a universal Gröbner basis
                Description
                        Text
                                Let $I \subseteq R = k[x_1, \ldots, x_n]$ be an ideal.
                                A set of generators $\mathcal G$ for $I$ is a universal Gröbner basis for $I$
                                if it is a Gröbner basis for $I$ with respect to any monomial order on $R$.
                                The default value is always {\tt UniversalGB=>false}.

                                Set {\tt UniversalGB} to {\tt true} if it is known that the given generators for 
                                your ideal form a universal Gröbner basis.
                                In this case, we can avoid computing Gröbner bases as geometric vertex decompositions 
                                preserve universal Gröbner basis. That is,
                                if $\{ y^{d_i}q_i + r_i \mid i = 1, \ldots, s \}$ is a universal Gröbner basis for an 
                                ideal $I$, then $\{ q_1, \ldots, q_s \}$ and $\{ q_i \mid d_i = 0 \}$ are universal 
                                Gröbner bases for $C_{y,I}$ and $N_{y,I}$ in $k[x_1, \ldots, \hat y, \ldots, x_n]$, 
                                respectively.

                Caveat
                        If a universal Gr\"obner basis is not given, the intersection condition 
                        ${\rm in}_y(I) = C_{y,I} \cap (N_{y,I} + \langle y \rangle)$ via the results of 
                        [KR, Lemmas 2.6 and 2.12], which looks at the degree of $y$ in the reduced 
                        Gr\"obner basis of $I$.
                        In general, a universal Gr\"obner basis is not reduced, so the intersection condition
                        must be checked explicitly.
                        So, although providing a universal Gr\"obner basis will speed up computing the ideals
                        $C_{y, I}$ and $N_{y, I}$, it may take longer to verify the intersection condition.

                SeeAlso
                        oneStepGVDCyI
                        findOneStepGVD
                        getGVDIdeal
                        initialYForms
                        isGVD
                        isLexCompatiblyGVD
                        isWeaklyGVD
                        oneStepGVDNyI
                        oneStepGVD
///


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--
-- TESTS
--
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Test findLexCompatiblyGVDOrders
--------------------------------------------------------------------------------


TEST///
R = QQ[x,y];
I = ideal(x^2 - y^2);
assert(findLexCompatiblyGVDOrders I == {})
///


TEST///
R = QQ[x..z];
I = ideal(x-y, x-z);
assert( findLexCompatiblyGVDOrders I == {{x, y, z}, {x, z, y}, {y, x, z}, {y, z, x}, {z, x, y}, {z, y, x}} )
///


--------------------------------------------------------------------------------
-- Test findOneStepGVD
--------------------------------------------------------------------------------


TEST///
R = QQ[x..z];
I = ideal(x-y, y-z);
assert( rsort findOneStepGVD I == {x,y,z} )
///


TEST///  -- [KR, Example 2.16]
R = QQ[x..z,w,r,s];
I = ideal( y*(z*s - x^2), y*w*r, w*r*(z^2 + z*x + w*r + s^2) );
assert( findOneStepGVD I == {y} )
///


--------------------------------------------------------------------------------
-- Test getGVDIdeal
--------------------------------------------------------------------------------

-- [KR, Example 2.16]
TEST///
R = QQ[x,y,z,w,r,s]
I = ideal(y*(z*s - x^2), y*w*r, w*r*(z^2+z*x+w*r+s^2))
assert(apply(getGVDIdeal(I, {{"C", y}, {"N", s}}), i -> sub(i, R)) == {ideal(x*z*w*r+z^2*w*r+w^2*r^2+w*r*s^2,w*r,x^2-z*s), ideal(w*r)})
///


--------------------------------------------------------------------------------
-- Test initialYForms
--------------------------------------------------------------------------------


TEST///  -- [KR, Example 2.16]
R = QQ[x..z,w,r,s];
I = ideal( y*(z*s - x^2), y*w*r, w*r*(z^2 + z*x + w*r + s^2) );
assert( initialYForms(I, y) == ideal(x*z*w*r+z^2*w*r+w^2*r^2+w*r*s^2,y*w*r,y*x^2-y*z*s) )
///


--------------------------------------------------------------------------------
-- Test isGeneratedByIndeterminates
--------------------------------------------------------------------------------


TEST///
R = QQ[x,y,z];
I = ideal(x,y);
assert(isGeneratedByIndeterminates I)
///


TEST///
R = QQ[x_1..x_5];
I = ideal(x_1*x_2-x_3*x_4);
assert(not isGeneratedByIndeterminates I)
///


TEST///
R = QQ[a..d];
I = ideal 0;
assert(isGeneratedByIndeterminates I)
///


TEST///
R = QQ[a..d];
I = ideal 1;
assert(not isGeneratedByIndeterminates I)
///


--------------------------------------------------------------------------------
-- Test isGVD
--------------------------------------------------------------------------------


TEST///  -- [KR, Example 2.16]
R = QQ[x,y,z,w,r,s];
I = ideal(y*(z*s - x^2), y*w*r, w*r*(z^2 + z*x + w*r + s^2));
assert(isGVD I)
///


TEST///  -- [KR, Example 4.10]
R = QQ[x,y,z,w,r,s];
I = ideal(y*(z*s - x^2), y*w*r, w*r*(x^2+ z^2 + s^2 + w*r));
assert(not isGVD I)
///


TEST///  -- Toric ideal of the complete bipartite graph K_{3,2}; GVD by [CDSRVT, Theorem 5.8]
needsPackage "Quasidegrees";
R = QQ[e_1..e_6];
A = matrix{
        {1,0,0,1,0,0},
        {0,1,0,0,1,0},
        {0,0,1,0,0,1},
        {1,1,1,0,0,0},
        {0,0,0,1,1,1}
        };
I = toricIdeal(A, R);
assert(isGVD I)
///


TEST///  -- Toric ideal of the graph constructed by connecting two triangles by a bridge of length 2
needsPackage "Quasidegrees";
R = QQ[e_1..e_8];
A = matrix{
        {1, 0, 1, 0, 0, 0, 0, 0},
        {0, 1, 0, 0, 0, 1, 0, 0},
        {0, 0, 0, 1, 0, 0, 1, 0},
        {1, 0, 0, 0, 1, 0, 0, 0},
        {0, 1, 0, 0, 0, 0, 0, 1},
        {0, 0, 1, 1, 1, 0, 0, 0},
        {0, 0, 0, 0, 0, 1, 1, 1}
        };
I = toricIdeal(A, R);
assert(not isGVD I)
///


--------------------------------------------------------------------------------
-- Test isLexCompatiblyGVD
--------------------------------------------------------------------------------


TEST///
R = QQ[x,y];
I = ideal(x^2 - y^2);
assert(not isLexCompatiblyGVD(I, {x,y}))
///


TEST///
R = QQ[x..z];
I = ideal(x-y,x-z);
assert(isLexCompatiblyGVD(I, {x,y,z}))
///


--------------------------------------------------------------------------------
-- Test isUnmixed
--------------------------------------------------------------------------------


TEST///  -- Not unmixed by [SM, Example 1.6]
R = QQ[x_1..x_5];
I = ideal(x_1*x_3, x_1*x_4, x_1*x_5, x_2*x_3, x_2*x_4, x_2*x_5);
assert(not isUnmixed I)
///


TEST///  -- Unmixed by [DSH, Corollary 5.13]
R = QQ[x_11..x_15, x_21..x_24, x_31..x_33, x_41, x_42, x_51];
A = matrix{
        {x_11, x_12, x_13, x_14, x_15, 1},
        {x_21, x_22, x_23, x_24, 1, 0},
        {x_31, x_32, x_33, 1, 0, 0},
        {x_41, x_42, 1, 0, 0, 0},
        {x_51, 1, 0, 0, 0, 0},
        {1, 0, 0, 0, 0, 0}
        };
N = matrix{
        {0, 1, 0, 0, 0, 0},
        {0, 0, 1, 0, 0, 0},
        {0, 0, 0, 1, 0, 0},
        {0, 0, 0, 0, 1, 0},
        {0 ,0, 0, 0, 0, 1},
        {0, 0, 0, 0, 0, 0}
        };
X = inverse(A) * N * A;
I = ideal( X_(2,0), X_(3,0), X_(3,1), X_(4,0), X_(4,1), X_(4,2), X_(5,0), X_(5,1), X_(5,2), X_(5,3) );
assert(isUnmixed I)
///


--------------------------------------------------------------------------------
-- Test isWeaklyGVD
--------------------------------------------------------------------------------


TEST///  -- [KR, Example 4.10]
R = QQ[x,y,z,w,r,s];
I = ideal(y*(z*s - x^2), y*w*r, w*r*(x^2 + s^2 + z^2 + w*r));
assert(isWeaklyGVD I)
///


--------------------------------------------------------------------------------
-- Test oneStepGVD
--------------------------------------------------------------------------------


TEST///  -- [KR, Example 2.16]
R = QQ[x..z,w,r,s];
I = ideal( y*(z*s - x^2), y*w*r, w*r*(z^2 + z*x + w*r + s^2) );
assert( toSequence apply(toList(oneStepGVD(I, y, CheckDegenerate=>true)), i -> if class i === Ideal then sub(i, R) else i ) 
== (true, ideal(x*z*w*r+z^2*w*r+w^2*r^2+w*r*s^2,w*r,x^2-z*s), ideal(x*z*w*r+z^2*w*r+w^2*r^2+w*r*s^2), "nondegenerate") )
///

--------------------------------------------------------------------------------
-- Test oneStepGVDCyI
--------------------------------------------------------------------------------


TEST///  -- [KR, Example 2.16]
R = QQ[x..z,w,r,s];
I = ideal( y*(z*s - x^2), y*w*r, w*r*(z^2 + z*x + w*r + s^2) );
C = oneStepGVDCyI(I,y);
assert( C == sub(ideal(x*z*w*r+z^2*w*r+w^2*r^2+w*r*s^2,w*r,x^2-z*s), ring C) )
///


TEST///  -- [KR, Example 4.10]
R = QQ[x..z,w,r,s];
I = ideal( y*(z*s - x^2), y*w*r, w*r*(x^2 + s^2 + z^2 + w*r) );
C = oneStepGVDCyI(I, y);
assert( C == sub(ideal(z*s-x^2, w*r), ring C) )
///


--------------------------------------------------------------------------------
-- Test oneStepGVDNyI
--------------------------------------------------------------------------------


TEST///  -- [KR, Example 2.16]
R = QQ[x..z,w,r,s];
I = ideal( y*(z*s - x^2), y*w*r, w*r*(z^2 + z*x + w*r + s^2) );
assert( sub(oneStepGVDNyI(I, y), R) == ideal(x*z*w*r+z^2*w*r+w^2*r^2+w*r*s^2) )
///


TEST///  -- [KR, Example 4.10]
R = QQ[x..z,w,r,s];
I = ideal( y*(z*s - x^2), y*w*r, w*r*(x^2 + s^2 + z^2 + w*r) );
assert( sub(oneStepGVDNyI(I, y), R) == ideal(x^2*w*r+w*r*s^2+z^2*w*r+w^2*r^2) )
///


end--
