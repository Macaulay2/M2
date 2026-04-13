
------------------------------------------------------------------------
--- Lattice-polarized K3 surfaces from doubly special cubic fourfolds --
------------------------------------------------------------------------

LatticePolarizationOnK3Surface = new Type of HashTable;
globalAssignment LatticePolarizationOnK3Surface;
LatticePolarizationOnK3Surface.synonym = "lattice-polarization";

net LatticePolarizationOnK3Surface := S -> (
    M := latticeMatrix S;
    w := if S#"isVirtual" then "Virtual lattice" else "Lattice";
    w = (w | " rank-2 polarization defined by the intersection matrix: ") | (net M);
    if even M_(0,0) then w = w || (scanPolarizations S) || "[ more lines with: polarize(i,...) ]";
    w
);
texMath LatticePolarizationOnK3Surface := texMath @@ net;

LatticePolarizationOnK3Surface#{WebApp,AfterPrint} =
LatticePolarizationOnK3Surface#{WebApp,AfterNoPrint} =
LatticePolarizationOnK3Surface#{Standard,AfterPrint} =
LatticePolarizationOnK3Surface#{Standard,AfterNoPrint} = S -> (
    virtualK3 := if S#"isVirtual" then "Virtual lattice" else "Lattice";
    << endl << concatenate(interpreterDepth:"o") << lineNumber << " : " << virtualK3 << "-polarization on K3 surface associated to " << (shortDescriptionFourfold recoverFourfold S) << endl;
);

latticeMatrix = method();
latticeMatrix LatticePolarizationOnK3Surface := S -> S#"latticeMatrix";

recoverFourfold LatticePolarizationOnK3Surface := S -> recoverFourfold S#"SurfaceAssociatedToRationalFourfold";

building LatticePolarizationOnK3Surface := S -> building S#"SurfaceAssociatedToRationalFourfold";

latticePolarizationOnK3Surface = method(TypicalValue => LatticePolarizationOnK3Surface, Options => {Verbose => true, Verify => true});
latticePolarizationOnK3Surface (SurfaceAssociatedToRationalFourfold,EmbeddedProjectiveVariety) := o -> (S,C) -> (
    if not isStandardK3surface S then error "expected a standard K3 surface";
    if dim C != 1 then error("while polarizing the K3 surface: expected a curve; found: "|(? C));
    if not isSubset(C,S) then error "expected a curve contained in the K3 surface";
    gC := sectionalGenus C;
    if gC < 0 then error "found: sectional genus of the curve < 0";
    n := 2*gC - 2;
    if gC > 1 and o.Verify then (
        if o.Verbose then << "-- verifying self-intersection of the curve..." << endl;
        f := rationalMap(S, tally {C});
        if dim target f != gC then error "self-intersection calculation failed: unexpected target dimension for the linear system";
        E1 := f^* random(1,0_(target f));
        E2 := f^* random(1,0_(target f));
        if dim E1 != 1 or dim E2 != 1 or dim(E1 * E2) != 0 then error "self-intersection calculation failed: divisors are not in general position";
        assert(degree E1 == degree C and sectionalGenus E1 == gC);
        assert(degree E2 == degree C and sectionalGenus E2 == gC);
        n' := degree(E1 * E2);
        if n != n' then error("self-intersection calculation failed: expected "|(toString n)|", obtained "|(toString n'));
    );
    d := degree C;
    gS := sectionalGenus S;
    if o.Verbose then << "-- constructing lattice polarized K3 with (g, d, C^2) = (" << gS << ", " << d << ", " << n << ")" << endl;
    M := matrix{{2*gS-2,d},{d,n}};
    if det M == 0 then error "lattice polarization failed: intersection matrix has determinant 0";
    new LatticePolarizationOnK3Surface from {
        symbol cache => new CacheTable,
        "SurfaceAssociatedToRationalFourfold" => S,
        "specialCurve" => C,
        "latticeMatrix" => M,
        "isVirtual" => false
    }
);

latticePolarizationOnK3Surface (SurfaceAssociatedToRationalFourfold,ZZ,ZZ,ZZ) := o -> (S,degS,degC,C2) -> (
    M := matrix{{degS,degC},{degC,C2}};
    if det M == 0 then error "virtual lattice polarization failed: intersection matrix has determinant 0";
    new LatticePolarizationOnK3Surface from {
        symbol cache => new CacheTable,
        "SurfaceAssociatedToRationalFourfold" => S,
        "specialCurve" => null,
        "latticeMatrix" => M,
        "isVirtual" => true
    }
);

LatticePolarizationOnK3Surface Sequence := (S,ab) -> (
    if not(#ab == 2 and instance(first ab,ZZ) and instance(last ab,ZZ)) then error "expected a sequence of two integers";
    (a,b) := ab;
    if not S#"isVirtual" then (
        f := map(S,a,b);
        if char coefficientRing f <= 65521 then return image(f,"F4");
        return image f;
    );
    if gcd(a,b) != 1 then error "expected a and b to be coprime for a primitive polarization";
    M := latticeMatrix S;
    g := lift((M_(0,0) + 2)/2, ZZ);
    d := M_(0,1);
    n := M_(1,1);
    g' := (a^2*(2*g-2) + 2*a*b*d + b^2*n + 2)/2;
    d' := a*d + b*n;
    if not (floor g' == g' and g' >= 3 and d' >= 1) then error "failed to construct virtual lattice polarized K3 surface";
    latticePolarizationOnK3Surface(S#"SurfaceAssociatedToRationalFourfold", 2*(lift(g',ZZ))-2, d', n)
);

map (LatticePolarizationOnK3Surface,ZZ,ZZ) := o -> (S,a,b) -> (
    if S.cache#?("map",a,b) then return S.cache#("map",a,b);
    T := S#"SurfaceAssociatedToRationalFourfold";
    C := S#"specialCurve";
    if not(dim T =!= -1 and C =!= null and (not S#"isVirtual")) then error "invalid or virtual lattice polarization";
    H := random(1,0_T);
    M := latticeMatrix S;
    d := M_(0,1);
    n := M_(1,1);
    g := lift((M_(0,0) + 2)/2, ZZ);
    g' := lift((a^2*(2*g-2) + 2*a*b*d + b^2*n + 2)/2,ZZ);
    if g' <= 0 then error "invalid pair of integers: target of map would be empty or a point";
    phi := if a > 0 and b < 0 then rationalMap((-b)*(C % T), a) else mapDefinedByDivisor(T,{(H,a),(C,b)});
    if dim target phi =!= g' then error("expected map to PP^"|(toString g')|", but got map to PP^"|toString(dim target phi));
    S.cache#("map",a,b) = phi
);

polarize (ZZ,LatticePolarizationOnK3Surface) := o -> (i,S) -> scanPolarizations(i,S);

scanPolarizations  = method();
scanPolarizations (ZZ,Matrix) := (i,M) -> (
    if numColumns M != 2 or numRows M != 2 or ring M =!= ZZ then error "expected a 2x2 matrix over ZZ";
    g := lift((M_(0,0) + 2)/2, ZZ);
    d := M_(0,1);
    assert(d == M_(1,0));
    n := M_(1,1);
    L := {};
    local g'; local d';
    for a from -i to i do (
        for b from -i to i do (
            g' = (a^2*(2*g-2) + 2*a*b*d + b^2*n + 2)/2;
            d' = a*d + b*n;
            if floor g' == g' and g' >= 3 and d' >= 1 and gcd(a,b) == 1 and (2*g'-2)*n - d'^2 != 0 then (
                g' = lift(g',ZZ);
                L = prepend((g', d', n, a, b, (2*g'-2)*n - d'^2), L);
            );
        );
    );
    sort L
);

integerSols = p -> (
    if instance(p,ZZ) then (
        if p == 0 then return {null} else return {};
    );
    R := ring p;
    if not (isPolynomialRing R and coefficientRing R === ZZ and numgens R == 1) then error "expected a univariate polynomial ring over ZZ";
    x := first gens R;
    L := toList factor p;
    solList := {};
    local f; local c0; local c1;
    for i in L do (
        f = first i;
        if first degree f == 1 and abs leadCoefficient f == 1 then (
            c0 = lift(sub(f, x => 0),ZZ);
            c1 = leadCoefficient f;
            solList = append(solList, -c0 // c1);
        );
    );
    solList
);

scanPolarizations (ZZ,LatticePolarizationOnK3Surface) := (i,T) -> (
    M := latticeMatrix T;
    L := scanPolarizations(i,M);
    c1 := {}; d1 := {}; c2 := {}; d2 := {}; c3 := {}; c4 := {}; c5 := {};
    for e in L do (
        c1 = append(c1, "(a,b) = " | toString(e_3,e_4));
        d1 = append(d1, " -> ");
        c2 = append(c2, "g = "| toString e_0);
        d2 = append(d2, " ");
        c3 = append(c3, "H.C = " | toString e_1);
        c4 = append(c4, "det = " | (toString(2*e_0-2) | "*" | (if e_2 < 0 then "(" | toString e_2 | ")" else toString e_2) | "-" | toString e_1 | "^2"));
        c5 = append(c5, (" = " | toString e_5));
    );
    F := stack c1 | stack d1 | stack c2 | stack d2 | stack c3 | stack d2 | stack c4 | stack c5;
    X := recoverFourfold T;
    A := latticeIntersectionMatrix3x3 X;
    D := det A;
    local s;
    ST := {};
    for e in L do (
        s = integerSols(e_5 + D);
        if #s == 0 then ST = append(ST,"") else (
            if ring A === ZZ
            then ST = append(ST, " [S.T = " | (toString A_(1,2)) | "]")
            else ST = append(ST, " [S.T = " | (toString unsequence toSequence s) |"]");
        );
    );
    F | stack ST
);

scanPolarizations LatticePolarizationOnK3Surface := T -> scanPolarizations(3,T);

------------------------------------------------------------------------
------------------------------------------------------------------------

K3SurfaceFromDoublySpecialCubicFourfold = new Type of MutableHashTable;
globalAssignment K3SurfaceFromDoublySpecialCubicFourfold;
K3SurfaceFromDoublySpecialCubicFourfold.synonym = "K3 surface";

net K3SurfaceFromDoublySpecialCubicFourfold := describe K3SurfaceFromDoublySpecialCubicFourfold := S -> (
    out := describe underlyingK3 S;
    if S#"LatticePolarization" === null then return (out|| "Lattice polarization: not yet computed; use 'polarize' or 'polarizedK3surface'");
    M := latticeMatrix S;
    if isVirtualLatticeK3 S and computationStatus S <= 3 then out = out||("Lattice intersection matrix (virtual, computed from U): "|(net M));
    out
);
texMath K3SurfaceFromDoublySpecialCubicFourfold := texMath @@ net;

K3SurfaceFromDoublySpecialCubicFourfold#{WebApp,AfterPrint} =
K3SurfaceFromDoublySpecialCubicFourfold#{WebApp,AfterNoPrint} =
K3SurfaceFromDoublySpecialCubicFourfold#{Standard,AfterPrint} =
K3SurfaceFromDoublySpecialCubicFourfold#{Standard,AfterNoPrint} = S -> (
    << endl << concatenate(interpreterDepth:"o") << lineNumber << " : " << "Lattice-polarized K3 surface associated to " << (shortDescriptionFourfold recoverFourfold S) << " — " << computationStatusLog(S) << endl;
);

underlyingK3 = method();
underlyingK3 K3SurfaceFromDoublySpecialCubicFourfold := S -> S#"UnderlyingK3";

projectiveVariety K3SurfaceFromDoublySpecialCubicFourfold := o -> S -> (
    if dim underlyingK3 S == -1 then << "-- warning: underlying K3 surface not fully computed (incomplete data)" << endl;
    underlyingK3 S
);

building K3SurfaceFromDoublySpecialCubicFourfold := S -> building underlyingK3 S;
recoverFourfold K3SurfaceFromDoublySpecialCubicFourfold := S -> recoverFourfold underlyingK3 S;
getInverseFanoMap K3SurfaceFromDoublySpecialCubicFourfold := Utilde -> getInverseFanoMap underlyingK3 Utilde;

latticePolarization = method();
latticePolarization K3SurfaceFromDoublySpecialCubicFourfold := S -> (
    if S#"LatticePolarization" === null then error "lattice polarization not yet computed; use 'polarize' or 'polarizedK3surface'";
    S#"LatticePolarization"
);

latticeMatrix K3SurfaceFromDoublySpecialCubicFourfold := S -> latticeMatrix latticePolarization S;
K3SurfaceFromDoublySpecialCubicFourfold Sequence := (S,ab) -> (latticePolarization S) ab;
map(K3SurfaceFromDoublySpecialCubicFourfold,ZZ,ZZ) := o -> (S,a,b) -> map(latticePolarization S,a,b);

isVirtualLatticeK3 = method();
isVirtualLatticeK3 K3SurfaceFromDoublySpecialCubicFourfold := S -> S#"LatticePolarization" =!= null and (S#"LatticePolarization")#"isVirtual";

polarizedK3surface = method(TypicalValue => K3SurfaceFromDoublySpecialCubicFourfold, Options => {Verbose => false, Strategy => null, FanoMapType => null});
polarizedK3surface DoublySpecialCubicFourfold := o -> X -> (
    if not instance(o.Verbose,Boolean) then error "expected a Boolean value for option 'Verbose'";
    local StrK3; local StrPol; local S;
    StrK3Set := {"Inverse","Approximate"};
    StrPolSet := {null, "SpecialCurve", "MapFromW", "MapFromU", "MapFromW-Virtual", "MapFromU-Virtual"};
    if member(o.Strategy,StrPolSet)
    then (StrK3,StrPol) = (null,o.Strategy)
    else if member(o.Strategy,StrK3Set)
    then (StrK3,StrPol) = (o.Strategy,null)
    else if instance(o.Strategy,VisibleList) and # o.Strategy == 2 and member(first o.Strategy, StrK3Set) and member(last o.Strategy, StrPolSet)
    then (StrK3,StrPol) = toSequence o.Strategy
    else error("polarizedK3surface: invalid Strategy; expected one of {\"SpecialCurve\", \"MapFromW\", \"MapFromU\", \"MapFromW-Virtual\", \"MapFromU-Virtual\"}, or {\"Inverse\", \"Approximate\"}, or a pair of these");
    if not member(o.FanoMapType,{null,"Standard","P2xP2"}) then error("polarizedK3surface: invalid FanoMapType '" | toString(o.FanoMapType) | "'; expected one of {\"Standard\", \"P2xP2\"}");
    mu := getCachedFanoMapIfCompatible(X,o.FanoMapType);
    if mu === null or (not mu.cache#?("K3SurfaceFromDoublySpecialCubicFourfold",X)) then (
        U := associatedUnderlyingK3Raw(X, Verbose=>o.Verbose, Strategy=>StrK3, FanoMapType=>o.FanoMapType);
        assert(mu === null or mu === first building U);
        mu = first building U;
        if mu.cache#?("K3SurfaceFromDoublySpecialCubicFourfold",X) then (
            S = mu.cache#("K3SurfaceFromDoublySpecialCubicFourfold",X);
        ) else (
            S = new K3SurfaceFromDoublySpecialCubicFourfold from {
                symbol cache => new CacheTable,
                "UnderlyingK3" => U,
                "LatticePolarization" => null
            };
            mu.cache#("K3SurfaceFromDoublySpecialCubicFourfold",X) = S;
        );
        S.cache#"userStrategyPolarization" = StrPol;
        return S;
    );
    S = mu.cache#("K3SurfaceFromDoublySpecialCubicFourfold",X);
    if StrPol === null then (
        if S#"LatticePolarization" =!= null then return S;
        if S.cache#?"userStrategyPolarization" then StrPol = S.cache#"userStrategyPolarization";
    );
    StrPol = setStrategyDSCFtoPolarize(S,StrPol);
    if not S.cache#?("polarization",StrPol) then S.cache#("polarization",StrPol) = associatedLatticePolarizationRaw(underlyingK3 S,Verbose=>o.Verbose,Strategy=>StrPol);
    T := S.cache#("polarization",StrPol);
    S#"UnderlyingK3" = T#"SurfaceAssociatedToRationalFourfold";
    S#"LatticePolarization" = T;
    return S;
);

polarizedK3surface K3SurfaceFromDoublySpecialCubicFourfold := o -> S -> polarizedK3surface(recoverFourfold S,Verbose=>o.Verbose,Strategy=>o.Strategy,FanoMapType=>o.FanoMapType);
polarizedK3surface SurfaceAssociatedToRationalFourfold := o -> S -> (
    X := recoverFourfold S;
    if not instance(X,DoublySpecialCubicFourfold) then error "K3 surface is expected to be associated to a doubly special cubic fourfold";
    polarizedK3surface(recoverFourfold S,Verbose=>o.Verbose,Strategy=>o.Strategy,FanoMapType=>o.FanoMapType)
);

polarize K3SurfaceFromDoublySpecialCubicFourfold := o -> S -> polarizedK3surface S;

associatedK3surface DoublySpecialCubicFourfold := o -> X -> polarizedK3surface(X,Verbose=>o.Verbose,Strategy=>o.Strategy);

------------------------------------------------------------------------
------------------ Associated polarized K3 (raw data) ------------------
------------------------------------------------------------------------

associatedLatticePolarizationRaw = method(TypicalValue => LatticePolarizationOnK3Surface, Options => {Verbose => true, Strategy => null});
associatedLatticePolarizationRaw SurfaceAssociatedToRationalFourfold := o -> Utilde -> (
    if o.Verbose then (
        tPolStart := currentTime();
        tPolStartCPU := cpuTime();
        printFinalLog := () -> (
            tPolEnd := currentTime() - tPolStart;
            tPolEndCPU := cpuTime() - tPolStartCPU;
            << " ✦ polarization successfully completed in " << humanReadableSeconds(tPolEnd) << " (cpu: " << humanReadableSeconds(floor tPolEndCPU) << ")" << endl;
            if Utilde.cache#?"computationTime" then (
                << "-- total time (K3 surface + polarization): " << humanReadableSeconds(first Utilde.cache#"computationTime" + tPolEnd) << " (cpu: " << humanReadableSeconds floor(last Utilde.cache#"computationTime" + tPolEndCPU) << ")" << endl;
            );
        );
        << "-- starting polarization computation" << endl;
        << "-- settings: Verbose => " << o.Verbose << ", Strategy => " << (if instance(o.Strategy,String) then "\"" | o.Strategy | "\"" else toString(o.Strategy)) << endl;
        << "-- available strategies: \"SpecialCurve\", \"MapFromW\", \"MapFromU\", \"MapFromW-Virtual\", \"MapFromU-Virtual\"" << endl;
    );
    if member(o.Strategy,{"MapFromW-Virtual","MapFromU-Virtual"}) then return virtualAssociatedLatticePolarizationRaw(Utilde,Verbose=>o.Verbose,Strategy=>o.Strategy);
    Utilde = buildAssociatedSurfaceFromPartialData(Utilde,Verbose=>o.Verbose);
    (mu,U,exC,f) := building Utilde;
    X := recoverFourfold Utilde;
    if f === null then error "incomplete K3 data (failed to obtain contraction map)";
    if not isStandardK3surface Utilde then error "expected a standard K3 surface (invariant mismatch)";
    StrPol := setStrategyDSCFtoPolarize(Utilde,o.Strategy);
    specialCurveK3 := null;
    if StrPol === "SpecialCurve" then specialCurveK3 = specialCurveOnK3FromCurvesOnU(Utilde,o.Verbose,isHigherDegreeCurveInExceptionalSetKnownToBeSpecial X);
    if specialCurveK3 === null then (
        if isFanoMapStandard(X) and (not U.cache#?"birational maps from X to W and from W to X") then (
            if not U.cache#?"strategy for surface U" then error "surface U does not appear as computed using the standard polarization methods";
            if U.cache#"strategy for surface U" === "Approximate" then error("the K3 surface was computed with Strategy => \"Approximate\", which is incompatible with this specific lattice polarization case. Please clear the cache and recompute using Strategy => \"Inverse\". Example: X' = clean X; polarizedK3surface(X', Strategy => \"Inverse\")");
        );
        if StrPol === "MapFromU" or StrPol === "SpecialCurve" then (
            specialCurveK3 = specialCurveOnK3viaMapFromU(Utilde,o.Verbose)
        ) else (
            if StrPol === "MapFromW" then (
                specialCurveK3 = specialCurveOnK3viaMapFromW(Utilde,o.Verbose);
            ) else (
                error "internal error: unhandled polarization strategy";
            );
        );
    );
    if o.Verbose then << "-- constructing lattice polarization..." << endl;
    T := latticePolarizationOnK3Surface(Utilde,specialCurveK3,Verbose=>o.Verbose,Verify=>not(isSelfIntersectionVerificationKnownToBeSuperfluous X));
    if o.Verbose then printFinalLog();
    T
);

specialCurveOnK3FromCurvesOnU = (Utilde,PolarizeVerbosity,includeCurveC) -> (
    (mu,U,exC,f) := building Utilde;
    (L,C) := toSequence exC;
    if PolarizeVerbosity then << "-- special curves already detected on U" << endl << flush;
    spC := if U.cache#?"special curves on U"
           then select(U.cache#"special curves on U", spC0 -> not (isSubset(spC0,L) or isSubset(spC0,C)))
           else {};
    if includeCurveC then spC = prepend(C,spC);
    if # spC == 0 then (
        -- error "curves detected on U are exceptional";
        if PolarizeVerbosity then << "-- curves detected on U are exceptional; reverting to standard strategy" << endl;
        return null;
    );
    D := null; i := 0;
    for E in spC when D === null do (
        i = i + 1;
        if PolarizeVerbosity then << "  -- pushing forward curve to K3 (" << i << "/" << #spC << ")..." << endl;
        D = f E;
        if dim D != 1 then (
            D = null;
            if PolarizeVerbosity then << "  -- image is not a curve, skipping..." << endl;
        );
    );
    if D === null then (
        -- error "surface polarization calculation failed: no image on K3 is a curve";
        if PolarizeVerbosity then << "-- no image on K3 is a curve; reverting to standard strategy" << endl;
    ) else (
        if PolarizeVerbosity then << "  -- image curve: " << ? D << endl << flush;
    );
    D
);

specialCurveOnK3viaMapFromU = (Utilde,PolarizeVerbosity) -> (
    f := last building Utilde;
    psi := mapFromUtoP2xP2(Utilde,Verbose=>PolarizeVerbosity);
    (psi1,psi2) := toSequence projectionMaps psi;
    if PolarizeVerbosity then << "-- obtained the two maps p1, p2: U --> PP^2" << endl;
    if PolarizeVerbosity then << "-- computing p1^*(H_PP^2)" << endl << flush;
    E1 := psi1^* random(1,0_(target psi1));
    if dim E1 != 1 then error "surface polarization calculation failed, expected dimension 1 for p1^*(H_PP^2)";
    sE1 := interpolateTop(E1,Verbose=>verbosityInterpolateTop(PolarizeVerbosity));
    if PolarizeVerbosity and sE1 != E1 then << "  -- unexpected non-pure dimensional components were found" << endl;
    if PolarizeVerbosity then << "  -- obtained the first curve on U: " << ? sE1 << endl << "  -- computing image on K3 surface..." << endl;
    sE1onK3 := f sE1;
    if dim sE1onK3 != 1 then error "surface polarization calculation failed: image on K3 is not a curve";
    if PolarizeVerbosity and degree sE1onK3 != degree Utilde then << "  -- unexpected degree for f(p1^*(H_PP^2)): " << degree sE1onK3 << " (expected " << degree Utilde << ")" << endl;
    if PolarizeVerbosity then << "  -- image curve: " << ? sE1onK3 << endl;
    if PolarizeVerbosity then << "-- computing p2^*(H_PP^2)" << endl << flush;
    E2 := psi2^* random(1,0_(target psi2));
    if dim E2 != 1 then error "surface polarization calculation failed, expected dimension 1 for p2^*(H_PP^2)";
    sE2 := interpolateTop(E2,Verbose=>verbosityInterpolateTop(PolarizeVerbosity));
    if PolarizeVerbosity and sE2 != E2 then << "  -- unexpected non-pure dimensional components were found" << endl;
    if PolarizeVerbosity then << "  -- obtained the second curve on U: " << ? sE2 << endl << "  -- computing image on K3 surface..." << endl;
    sE2onK3 := f sE2;
    if dim sE2onK3 != 1 then error "surface polarization calculation failed: image on K3 is not a curve";
    if PolarizeVerbosity then << "  -- image curve: " << ? sE2onK3 << endl << flush;
    sE2onK3
);

specialCurveOnK3viaMapFromW = (Utilde,PolarizeVerbosity) -> (
    (mu,U,exC,f) := building Utilde;
    psi := mapFromWtoP2xP2(Utilde,Verbose=>PolarizeVerbosity);
    (psi1,psi2) := toSequence projectionMaps psi;
    W := target mu;
    -- -- assert(source psi === W); -- bug?
    -- assert(source psi == W);
    if PolarizeVerbosity then << "-- obtained the two maps p1, p2: W --> PP^2" << endl;
    if PolarizeVerbosity then << "-- computing p1^*(H_PP^2)" << endl << flush;
    E1 := psi1^* random(1,0_(target psi1));
    if dim E1 != 3 then error "surface polarization calculation failed, expected dimension 3 for p1^*(H_PP^2)";
    if PolarizeVerbosity and degree E1 != degree W then << "  -- unexpected degree for p1^*(H_PP^2): " << degree E1 << " (expected " << degree W << ")" << endl;
    if PolarizeVerbosity then << "-- computing p2^*(H_PP^2)" << endl << flush;
    E2 := psi2^* random(1,0_(target psi2));
    if dim E2 != 3 then error "surface polarization calculation failed, expected dimension 3 for p2^*(H_PP^2)";
    E2U := E2 * U;
    if dim E2U != 1 then error "surface polarization calculation failed, expected dimension 1 for p2^*(H_PP^2) * U";
    sE2 := interpolateTop(E2U,Verbose=>verbosityInterpolateTop(PolarizeVerbosity));
    if PolarizeVerbosity and sE2 != E2U then << "  -- unexpected non-pure dimensional components were found" << endl;
    if PolarizeVerbosity then << "  -- obtained the curve on U: " << ? sE2 << endl << "  -- computing image on K3 surface..." << endl;
    sE2onK3 := f sE2;
    if dim sE2onK3 != 1 then error "surface polarization calculation failed: image on K3 is not a curve";
    if PolarizeVerbosity then << "  -- image curve: " << ? sE2onK3 << endl << flush;
    sE2onK3
);

mapFromWtoP2xP2 = method(Options => {Verbose => true});
mapFromWtoP2xP2 DoublySpecialCubicFourfold := o -> X -> (
    local mu;
    if isFanoMapToP2xP2 X then (
        mu = fanoMapDSCF(X,Verbose=>o.Verbose);
        s := last mu.cache#"FactorsViaSegreEmbedding";
        if s#"inverse" =!= null then return inverse s;
        if o.Verbose then << "-- computing inverse of Segre embedding PP^2 x PP^2 -> W ⊂ PP^8" << endl;
        return inverse s;
    );
    eta := getInverseFanoMap X;
    U := surfaceDeterminingInverseOfFanoMap(X,Verbose=>o.Verbose);
    if U.cache#?"map from W to P2xP2" then return U.cache#"map from W to P2xP2";
    mu = fanoMapDSCFtoP2xP2(X,Verbose=>o.Verbose);
    psi := first mu.cache#"FactorsViaSegreEmbedding";
    if o.Verbose then << "-- composing maps W --> X --> PP^2 x PP^2" << endl;
    U.cache#"map from W to P2xP2" = eta * psi
);
mapFromWtoP2xP2 SurfaceAssociatedToRationalFourfold := o -> Utilde -> (
    X := recoverFourfold Utilde;
    mapFromWtoP2xP2(X,Verbose=>o.Verbose)
);
mapFromWtoP2xP2 K3SurfaceFromDoublySpecialCubicFourfold := o -> S -> mapFromWtoP2xP2(underlyingK3 S,Verbose=>o.Verbose);

mapFromUtoP2xP2 = method(Options => {Verbose => true});
mapFromUtoP2xP2 DoublySpecialCubicFourfold := o -> X -> (
    fromWtoP2P2 := mapFromWtoP2xP2(X,Verbose=>o.Verbose);
    U := surfaceDeterminingInverseOfFanoMap(X,Verbose=>o.Verbose);
    if U.cache#?"map from U to P2xP2" then return U.cache#"map from U to P2xP2";
    (F,G) := toSequence projectionMaps fromWtoP2P2;
    if o.Verbose then << "-- obtained the two maps W --> PP^2, restricting to U via suitable representatives" << endl;
    local M; local B;
    if o.Verbose then << "-- working on the representatives of the first map" << endl << flush;
    f := toRationalMap F;
    F' := null;
    if not isSubset(U,projectiveVariety matrix f) then (
        F' = F;
    ) else (
        degsf := degreeSequence f; -- this computes f#"maps"
        assert(f#"maps" =!= null);
        if o.Verbose then << "  -- degree sequence: " << degsf << endl;
        for h in f#"maps" when F' === null do (
            M = matrix rationalMap h;
            B = projectiveVariety M;
            if not isSubset(U,B) then F' = (Hom(source F, target F)) {M};
        );
        if F' === null then error "not possible to restrict first map W --> PP^2 to surface U";
        -- provisional test --
        assert(F == F');
    );
    F'' := F'|U;
    if o.Verbose then << "-- working on the representatives of the second map" << endl << flush;
    g := toRationalMap G;
    G' := null;
    if not isSubset(U,projectiveVariety matrix g) then (
        G' = G;
    ) else (
        degsg := degreeSequence g; -- this computes g#"maps"
        assert(g#"maps" =!= null);
        if o.Verbose then << "  -- degree sequence: " << degsg << endl;
        for h in g#"maps" when G' === null do (
            M = matrix rationalMap h;
            B = projectiveVariety M;
            if not isSubset(U,B) then G' = (Hom(source G, target G)) {M};
        );
        if G' === null then error "not possible to restrict second map W --> PP^2 to surface U";
        -- provisional test --
        assert(G == G');
    );
    G'' := G'|U;
    phi := (Hom(U, target fromWtoP2P2)) ((entries F'')|(entries G''));
    if o.Verbose then (
        << "-- map U --> PP^2 x PP^2 stored in cache: retrieve it with importFrom(SpecialFanoFourfolds, \"mapFromUtoP2xP2\")" << endl << flush;
        -- provisional log --
        << "  -- map degree: " << degree(phi,Strategy=>"random point") << endl << flush;
    );
    return U.cache#"map from U to P2xP2" = phi;
);
mapFromUtoP2xP2 SurfaceAssociatedToRationalFourfold := o -> Utilde -> (
    X := recoverFourfold Utilde;
    mapFromUtoP2xP2(X,Verbose=>o.Verbose)
);
mapFromUtoP2xP2 K3SurfaceFromDoublySpecialCubicFourfold := o -> S -> mapFromUtoP2xP2(underlyingK3 S,Verbose=>o.Verbose);

virtualAssociatedLatticePolarizationRaw = method(Options => {Verbose => true, Strategy => "MapFromU-Virtual"});
virtualAssociatedLatticePolarizationRaw SurfaceAssociatedToRationalFourfold := o -> Utilde -> (
    if o.Verbose then (
        tPolStart := currentTime();
        tPolStartCPU := cpuTime();
        printFinalLog := withoutWarning -> (
            tPolEnd := currentTime() - tPolStart;
            tPolEndCPU := cpuTime() - tPolStartCPU;
            if withoutWarning then (
                << " ✧ virtual polarization completed in " << humanReadableSeconds(tPolEnd) << " (cpu: " << humanReadableSeconds(floor tPolEndCPU) << ")" << endl;
            ) else (
                << " ⟐ virtual polarization completed with warnings in " << humanReadableSeconds(tPolEnd) << " (cpu: " << humanReadableSeconds(floor tPolEndCPU) << ")" << endl;
            );
            if Utilde.cache#?"computationTime" then (
                << "-- total time (K3 surface + virtual polarization): " << humanReadableSeconds(first Utilde.cache#"computationTime" + tPolEnd) << " (cpu: " << humanReadableSeconds floor(last Utilde.cache#"computationTime" + tPolEndCPU) << ")" << endl;
            );
        );
    );
    U := (building Utilde)_1;
    local psi;
    if o.Strategy === "MapFromU-Virtual" then (
        psi = mapFromUtoP2xP2(Utilde,Verbose=>o.Verbose);
        if o.Verbose then << "-- obtained map p1xp2: U --> PP^2xPP^2" << endl;
    ) else if o.Strategy === "MapFromW-Virtual" then (
        psi = mapFromWtoP2xP2(Utilde,Verbose=>o.Verbose);
        if o.Verbose then << "-- obtained map p1xp2: W --> PP^2xPP^2" << endl;
    ) else error("strategy \"" | (toString o.Strategy) | "\" not available; expected: \"MapFromW-Virtual\" or \"MapFromU-Virtual\"");
    (psi1,psi2) := toSequence projectionMaps psi;
    if o.Verbose then << "-- computing p1^*(H_PP^2)" << endl << flush;
    E1 := psi1^* random(1,0_(target psi1));
    if o.Strategy === "MapFromW-Virtual" then E1 = E1 * U;
    if dim E1 != 1 then error "surface polarization calculation failed, expected dimension 1 for p1^*(H_PP^2)";
    sE1 := interpolateTop(E1,Verbose=>verbosityInterpolateTop(o.Verbose));
    if o.Verbose and sE1 != E1 then << "  -- unexpected non-pure dimensional components were found" << endl;
    if o.Verbose then << "  -- obtained p1^*(H_PP^2): " << ? sE1 << endl;
    if o.Verbose then << "-- computing another p1^*(H_PP^2)" << endl << flush;
    E2:= psi1^* random(1,0_(target psi1));
    if o.Strategy === "MapFromW-Virtual" then E2 = E2 * U;
    if dim E2 != 1 then error "surface polarization calculation failed, expected dimension 1 for p1^*(H_PP^2)";
    sE2 := interpolateTop(E2,Verbose=>verbosityInterpolateTop(o.Verbose));
    if o.Verbose and sE2 != E2 then << "  -- unexpected non-pure dimensional components were found" << endl;
    if o.Verbose then << "  -- second p1^*(H_PP^2) obtained: " << ? sE2 << endl;
    sE1sE2 := sE1*sE2;
    if dim sE1sE2 > 0 then (
        if o.Verbose then <<" -- dim((p1^*(H)) * p1^*(H')) > 0, removing fixed components" << endl;
        sE1sE2 = sE1sE2\\interpolateTop(sE1sE2,Verbose=>verbosityInterpolateTop(o.Verbose));
    );
    if dim sE1sE2 != 0 then error "(residual) intersection (p1^*(H)) * p1^*(H')) has dimension != 0";
    sE1sE2 = degree sE1sE2;
    withoutWarning := true;
    if 2*(sectionalGenus U) - 2 != sE1sE2 then (
        << "-- WARNING: [virtual polarization] degree mismatch! Expected " << (2*(sectionalGenus U) - 2) << " but got " << sE1sE2 << endl;
        withoutWarning = false;
    );
    if o.Verbose then << "-- computing p2^*(H_PP^2)" << endl << flush;
    C1 := psi2^* random(1,0_(target psi2));
    if o.Strategy === "MapFromW-Virtual" then C1 = C1 * U;
    if dim C1 != 1 then error "surface polarization calculation failed, expected dimension 1 for p2^*(H_PP^2)";
    sC1 := interpolateTop(C1,Verbose=>verbosityInterpolateTop(o.Verbose));
    if o.Verbose and sC1 != C1 then << "  -- unexpected non-pure dimensional components were found" << endl;
    if o.Verbose then << "  -- obtained p2^*(H_PP^2): " << ? sC1 << endl;
    sE1sC1 := sE1*sC1;
    if dim sE1sC1 > 0 then (
        if o.Verbose then <<" -- dim((p1^*(H)) * p2^*(H)) > 0, removing fixed components" << endl;
        sE1sC1 = sE1sC1\\interpolateTop(sE1sC1,Verbose=>verbosityInterpolateTop(o.Verbose));
    );
    if dim sE1sC1 != 0 then error "(residual) intersection (p1^*(H)) * p2^*(H)) has dimension != 0";
    sE1sC1 = degree sE1sC1;
    if o.Verbose then << "-- computing another p2^*(H_PP^2)" << endl << flush;
    C2:= psi2^* random(1,0_(target psi2));
    if o.Strategy === "MapFromW-Virtual" then C2 = C2 * U;
    if dim C2 != 1 then error "surface polarization calculation failed, expected dimension 1 for p2^*(H_PP^2)";
    sC2 := interpolateTop(C2,Verbose=>verbosityInterpolateTop(o.Verbose));
    if o.Verbose and sC2 != C2 then << "  -- unexpected non-pure dimensional components were found" << endl;
    if o.Verbose then << "  -- second p2^*(H_PP^2) obtained: " << ? sC2 << endl;
    sC1sC2 := sC1*sC2;
    if dim sC1sC2 > 0 then (
        if o.Verbose then <<" -- dim((p2^*(H)) * p2^*(H')) > 0, removing fixed components" << endl;
        sC1sC2 = sC1sC2\\interpolateTop(sC1sC2,Verbose=>verbosityInterpolateTop(o.Verbose));
    );
    if dim sC1sC2 != 0 then error "(residual) intersection (p2^*(H)) * p2^*(H')) has dimension != 0";
    sC1sC2 = degree sC1sC2;
    if o.Verbose then << flush;
    virtK3 := latticePolarizationOnK3Surface(Utilde,sE1sE2,sE1sC1,sC1sC2);
    if o.Verbose then printFinalLog withoutWarning;
    virtK3
);
