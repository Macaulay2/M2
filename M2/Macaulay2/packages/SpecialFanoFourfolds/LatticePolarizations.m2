
------------------------------------------------------------------------
--------- Lattice polarizations on K3 surfaces from D.S.C.F ------------
------------------------------------------------------------------------

LatticePolarizationOnK3Surface = new Type of HashTable;
globalAssignment LatticePolarizationOnK3Surface;
LatticePolarizationOnK3Surface.synonym = "lattice-polarization";

net LatticePolarizationOnK3Surface := L -> (
    M := latticeMatrix L;
    w := if L#"isVirtual" then "Virtual lattice" else "Lattice";
    w = (w | " rank-2 polarization with intersection matrix: ") | (net M);
    if even M_(0,0) then w = w || (scanPolarizations L) || "[ more lines with: polarize(i,oo) ]" || "[ use: map(oo,a,b) to obtain the corresponding map ]" || "[ on surface(oo) defined by |aH+bC|, where (H,C)=basis(oo) ]";
    w
);
texMath LatticePolarizationOnK3Surface := texMath @@ net;

LatticePolarizationOnK3Surface#{WebApp,AfterPrint} =
LatticePolarizationOnK3Surface#{WebApp,AfterNoPrint} =
LatticePolarizationOnK3Surface#{Standard,AfterPrint} =
LatticePolarizationOnK3Surface#{Standard,AfterNoPrint} = L -> (
    virtualK3 := if L#"isVirtual" then "Virtual lattice" else "Lattice";
    << endl << concatenate(interpreterDepth:"o") << lineNumber << " : " << virtualK3 << "-polarization on K3 surface associated to " << (shortDescriptionFourfold recoverFourfold L) << endl;
);

latticeMatrix = method();
latticeMatrix LatticePolarizationOnK3Surface := L -> L#"latticeMatrix";

coefficientRing LatticePolarizationOnK3Surface := L -> coefficientRing L#"UnderlyingSurface";
recoverFourfold LatticePolarizationOnK3Surface := L -> L#"DoublySpecialFourfold";
basis LatticePolarizationOnK3Surface := o -> L -> (
    if L.cache#?"LatticePolarizationBasis" then return L.cache#"LatticePolarizationBasis";
    if L#"isVirtual" then error "cannot determine a basis for a virtual lattice";
    S := L#"UnderlyingSurface";
    H := S * random(1,0_S);
    C := L#"specialCurve";
    if not(dim H == 1 and C =!= null and dim C == 1) then error "internal error encountered while determining a basis for the lattice";
    L.cache#"LatticePolarizationBasis" = (H%S,C%S)
);
surface LatticePolarizationOnK3Surface := L -> L#"UnderlyingSurface";

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
    if o.Verbose then << "-- constructing lattice-polarized K3 with (g, d, C^2) = (" << gS << ", " << d << ", " << n << ")" << endl;
    M := matrix{{2*gS-2,d},{d,n}};
    if det M == 0 then error "lattice polarization failed: intersection matrix has determinant 0";
    new LatticePolarizationOnK3Surface from {
        symbol cache => new CacheTable,
        "DoublySpecialFourfold" => recoverFourfold S,
        "UnderlyingSurface" => S,
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
        "DoublySpecialFourfold" => recoverFourfold S,
        "UnderlyingSurface" => S,
        "specialCurve" => null,
        "latticeMatrix" => M,
        "isVirtual" => true
    }
);

latticePolarizationOnK3Surface (SurfaceAssociatedToRationalFourfold,EmbeddedProjectiveVariety,ZZ) := o -> (Utilde,D,genK3) -> (
    (mu,U,exC,f) := building Utilde;
    (L,C) := exC;
    if not(dim D == 1 and isSubset(D,U)) then error "expected a curve on the surface U";
    errLog := "unable to determine the degree of the curve on the K3 surface";
    (degD, genD) := (degree D, sectionalGenus D);
    if dim(D * C) >= 0 then (
        if o.Verbose then << "-- detected that the curve on U intersects higher-degree exceptional curves" << endl;
        if dim(D * C) >= 1 then error errLog;
        if not isPresumedRationalNormalCurve C then error errLog;
        if degree(D * C) != degree C then error errLog;
        (degD, genD) = (degD + (degree C)^2, genD + binomial(degree C,2));
    );
    if dim(D * L) >= 0 then (
        if o.Verbose then << "-- detected that the curve on U intersects exceptional lines" << endl;
        if dim(D * L) >= 1 then error errLog;
        B := select(decompose L, l -> dim(l * D)>=0);
        B = apply(B, l -> degree(l * D));
        if any(B, l -> l>=2) then error errLog;
        degD = degD + (sum B);
    );
    M := matrix{{2*genK3-2,degD},{degD,2*genD-2}};
    if det M == 0 then error "virtual lattice polarization failed: intersection matrix has determinant 0";
    new LatticePolarizationOnK3Surface from {
        symbol cache => new CacheTable,
        "DoublySpecialFourfold" => recoverFourfold Utilde,
        "UnderlyingSurface" => Utilde,
        "specialCurve" => D,
        "latticeMatrix" => M,
        "isVirtual" => true
    }
);

mapDefinedByLatticePolarization = method(Options => {Verbose => true, Verify => true});

mapDefinedByLatticePolarization (LatticePolarizationOnK3Surface,ZZ,ZZ,ZZ) := o -> (L,g,a,b) -> (
    if L.cache#?("map",g,a,b) then return L.cache#("map",g,a,b);
    if g <= 0 then error "map target must have dimension >= 1";
    M := latticeMatrix L;
    if 2*g - 2 != a^2*M_(0,0) + 2*a*b*M_(0,1) + b^2*M_(1,1) then error "internal error: target of map between K3 surfaces would lie in an unexpected projective space";
    if L#"isVirtual" then error "cannot determine the map defined by a virtual lattice";
    T := L#"UnderlyingSurface";
    if dim T == -1 then error "underlying K3 surface not fully computed";
    C := L#"specialCurve";
    if C === null then error "invalid or virtual lattice polarization";
    H := random(1,0_T);
    phi := if a > 0 and b < 0 then rationalMap((-b)*(C % T), a) else mapDefinedByDivisor(T,{(H,a),(C,b)});
    if o.Verify and dim target phi != g then error("expected map to PP^"|(toString g)|", but got map to PP^"|toString(dim target phi));
    phi.cache#"pair of integers defining the divisor" = (a,b);
    L.cache#("map",g,a,b) = phi
);

mapDefinedByLatticePolarization (LatticePolarizationOnK3Surface,ZZ,ZZ) := o -> (L,a,b) -> (
    M := latticeMatrix L;
    g := lift((a^2*M_(0,0) + 2*a*b*M_(0,1) + b^2*M_(1,1) + 2)/2, ZZ);
    if g <= 0 then error "invalid pair of integers: target of map would be empty or a point";
    mapDefinedByLatticePolarization(L,g,a,b,Verbose=>o.Verbose,Verify=>o.Verify)
);

mapDefinedByLatticePolarization (LatticePolarizationOnK3Surface,EmbeddedProjectiveVariety,ZZ,ZZ) := o -> (L,P,a,b) -> (
    if L.cache#?("map",P,a,b) then return L.cache#("map",P,a,b);
    if codim P != 0 then error "expected a (weighted) projective space";
    g := dim P;
    if g < 2 then error "expected a projective space of dimension >= 2";
    K := coefficientRing P;
    if K =!= coefficientRing L then error "expected the projective space to be defined over the same coefficient ring";
    f := mapDefinedByLatticePolarization(L,g,a,b,Verbose=>o.Verbose,Verify=>o.Verify);
    f = (Hom(source f,P)) entries f;
    if g > 2 then (
        if o.Verbose then << "  -- computing K3 surface as image of map to PP^" << g;
        if char K <= 65521 then (
            if o.Verbose then << " (using \"F4\")" << endl << flush;
            image(f,"F4");
        ) else (
            if o.Verbose then << endl << flush;
            image f;
        );
        if o.Verify and (not isStandardK3surface image f) then error "inconsistent invariants for a K3 surface";
        f = rationalMap(f,Dominant=>true);
    );
    f.cache#"pair of integers defining the divisor" = (a,b);
    L.cache#("map",P,a,b) = f
);

mapDefinedByLatticePolarization (LatticePolarizationOnK3Surface,WeightedProjectiveVariety,ZZ,ZZ) := o -> (L,P,a,b) -> (
    if L.cache#?("map",P,a,b) then return L.cache#("map",P,a,b);
    if codim P != 0 then error "expected a (weighted) projective space";
    if degrees ring P =!= {{1},{1},{1},{3}} then error "expected a standard projective space or PP(1,1,1,3)";
    K := coefficientRing P;
    f := mapDefinedByLatticePolarization(L,PP_K^2,a,b,Verbose=>o.Verbose,Verify=>o.Verify);
    d := degreeOfDefiningForms f;
    if o.Verbose then << "  -- linear system |D=aH+bC| defines a map to PP^2 by forms of degree " << d << endl;
    f' := mapDefinedByLatticePolarization(L,3*a,3*b,Verbose=>o.Verbose,Verify=>o.Verify);
    d' := degreeOfDefiningForms f';
    if d' != 3*d then error("expected |3(aH+bC)| to be defined by forms of degree 3*"|(toString d));
    if o.Verbose then << "  -- linear system |3D| defines a map to PP^" << dim target f' << " by forms of degree " << d' << endl;
    M := (matrix f) | ((matrix f') * matrix apply(dim target f' + 1, i -> {random K}));
    h := (Hom(source f,P)) {M};
    if o.Verbose then << "  -- obtained map from K3 surface in " << net(ambient source h) << " to PP(1,1,1,3)" << endl;
    if o.Verbose then << "  -- computing the image in PP(1,1,1,3)..." << endl;
    T := projectiveVariety kernel(map toRationalMap h, SubringLimit=>1);
    if dim T != 2 then error "expected the image to be a surface";
    if o.Verify and degrees T =!= {({6}, 1)} then error("expected the image to be a surface of degree 6 in PP(1,1,1,3), but obtained: "|toString(? T));
    forceImage(h,T);
    h = rationalMap(h,Dominant=>true);
    if o.Verify then (
        if o.Verbose then << "  -- obtained map to sextic surface in PP(1,1,1,3)" << endl;
        p := point source h;
        if p != h^* h p then error "expected to obtain a birational map";
        p = point target h;
        if p != h h^* p then error "expected to obtain a birational map";
        h#"isBirational" = true;
        if o.Verbose then << "  -- birationality of map verified" << endl;
    );
    h.cache#"pair of integers defining the divisor" = (a,b);
    L.cache#("map",P,a,b) = h
);

map(LatticePolarizationOnK3Surface,ZZ,ZZ) := o -> (L,a,b) -> mapDefinedByLatticePolarization(L,a,b,Verbose=>false,Verify=>true);

------------------------------------------------------------------------
------------------------------------------------------------------------

polarize (ZZ,LatticePolarizationOnK3Surface) := o -> (i,L) -> scanPolarizations(i,L);

scanPolarizations  = method();
scanPolarizations (ZZ,Matrix) := (i,M) -> (
    if numColumns M != 2 or numRows M != 2 or ring M =!= ZZ then error "expected a 2x2 matrix over ZZ";
    if M_(0,1) != M_(1,0) then error "expected a symmetric matrix";
    L := {};
    local g1; local d1; local n1;
    for a from 0 to i do ( -- for a from -i to i do (
        for b from -i to i do (
            g1 = (a^2*M_(0,0) + 2*a*b*M_(0,1) + b^2*M_(1,1) + 2)/2;
            d1 = if g1 == 2 then a*M_(0,0) + b*M_(0,1) else a*M_(0,1) + b*M_(1,1);
            n1 = if g1 == 2 then M_(0,0) else M_(1,1);
            if floor g1 == g1 and g1 >= 2 and d1 >= 1 and gcd(a,b) == 1 and (2*g1-2)*n1 - d1^2 != 0 then (
                g1 = lift(g1,ZZ);
                L = prepend((g1, d1, n1, a, b, (2*g1-2)*n1 - d1^2), L);
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
            then ST = append(ST, " (*)")
            else ST = append(ST, " [S.T = " | (toString unsequence toSequence s) |"]");
        );
    );
    F | stack ST
);

scanPolarizations LatticePolarizationOnK3Surface := T -> scanPolarizations(3,T);

------------------------------------------------------------------------
-------------- Associated polarizations on K3 (raw data) ---------------
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
        << "-- available strategies: \"Genus2Curve\", \"SpecialCurve\", \"MapFromW\", \"MapFromU\"," << endl;
        << "--                       \"Genus2Curve-Virtual\", \"MapFromW-Virtual\", \"MapFromU-Virtual\"" << endl;
    );
    if member(o.Strategy,{"Genus2Curve-Virtual","SpecialCurve-Virtual","MapFromW-Virtual","MapFromU-Virtual"}) then return virtualAssociatedLatticePolarizationRaw(Utilde,Verbose=>o.Verbose,Strategy=>o.Strategy);
    compTimeUtilde := if Utilde.cache#?"computationTime" then Utilde.cache#"computationTime" else null;
    Utilde = buildAssociatedSurfaceFromPartialData(Utilde,Verbose=>o.Verbose);
    if (not Utilde.cache#?"computationTime") and compTimeUtilde =!= null then Utilde.cache#"computationTime" = compTimeUtilde;
    (mu,U,exC,f) := building Utilde;
    X := recoverFourfold Utilde;
    if f === null then error "K3 surface not fully determined (contraction map unavailable)";
    if not isStandardK3surface Utilde then error "invariants do not match those of a standard K3 surface";
    StrPol := setStrategyDSCFtoPolarize(Utilde,o.Strategy);
    specialCurveK3 := null;
    if StrPol === "SpecialCurve" then (
        specialCurveK3 = specialCurveOnK3FromCurvesOnU(Utilde,o.Verbose);
    );
    if specialCurveK3 === null then (
        errorIfIncompatibleK3Strategy(U,X);
        if StrPol === "Genus2Curve" or StrPol === "SpecialCurve" then (
            specialCurveK3 = specialGenus2CurveOnK3(Utilde,o.Verbose);
        ) else (
            if StrPol === "MapFromU" then (
                specialCurveK3 = specialCurveOnK3viaMapFromU(Utilde,o.Verbose);
            ) else (
                if StrPol === "MapFromW" then (
                    specialCurveK3 = specialCurveOnK3viaMapFromW(Utilde,o.Verbose);
                ) else (
                    error "internal error: unhandled polarization strategy";
                );
            );
        );
    );
    if o.Verbose then << "-- constructing lattice polarization..." << endl;
    T := latticePolarizationOnK3Surface(Utilde,specialCurveK3,Verbose=>o.Verbose,Verify=>not(isSelfIntersectionVerificationKnownToBeSuperfluous X));
    if o.Verbose then printFinalLog();
    T
);

specialGenus2CurveOnU = (Utilde,PolarizeVerbosity) -> (
    (mu,U,exC,f) := building Utilde;
    local F;
    if U.cache#?"Genus2CurveOnSurfaceU" then (
        F = U.cache#"Genus2CurveOnSurfaceU";
    ) else (
        X := recoverFourfold Utilde;
        psi := getInverseFanoMap X;
        if PolarizeVerbosity then << "-- taking curve D = (π_P)|S^(-1)(line)..." << endl;
        q := (quadricFibration X)|(surface X);
        D := q^* random(1, 0_(target q));
        if PolarizeVerbosity then << "-- computing U ∩ μ|X^(-1)(D)..." << endl;
        F = (psi^* D) * U;
        if PolarizeVerbosity then << "  -- checking/fixing equidimensionality..." << endl;
        -- F = top F;
        F = interpolateTop(F,Verbose=>verbosityInterpolateTop(PolarizeVerbosity));
        if not isSubset(F,U) then (
            if PolarizeVerbosity then << "-- warning: expected a curve on U; correcting..." << endl;
            F = F * U;
            assert isSubset(F,U);
        );
        if dim F != 1 then error "something went wrong: expected to obtain a curve";
        if sectionalGenus F != 2 then error("something went wrong: expected a curve of genus 2, but obtained a curve of genus "|(toString sectionalGenus F));
        U.cache#"Genus2CurveOnSurfaceU" = F;
    );
    if PolarizeVerbosity then << "-- obtained a curve on U of degree " << degree F << " and genus " << sectionalGenus F << endl << flush;
    return F;
);

specialGenus2CurveOnK3 = (Utilde,PolarizeVerbosity) -> (
    (mu,U,exC,f) := building Utilde;
    if f === null then error "K3 surface not fully determined (contraction map unavailable)";
    F := specialGenus2CurveOnU(Utilde,PolarizeVerbosity);
    if PolarizeVerbosity then << "-- computing image on K3 surface..." << endl;
    G := f F;
    if dim G != 1 then error "surface polarization calculation failed: image on K3 is not a curve";
    if PolarizeVerbosity then << "-- image curve: " << ? G << endl << flush;
    G
);

specialCurveOnK3FromCurvesOnU = (Utilde,PolarizeVerbosity) -> (
    (mu,U,exC,f) := building Utilde;
    if not U.cache#?"special curves on U" then (
        if PolarizeVerbosity then << "-- no special curve found on surface U; reverting to the \"Genus2Curve\" strategy" << endl;
        return;
    );
    if PolarizeVerbosity then << "-- special curves already detected on U" << endl << flush;
    (L,C) := toSequence exC;
    spC := select(U.cache#"special curves on U", spC0 -> not (isSubset(spC0,L) or isSubset(spC0,C)));
    if # spC == 0 then (
        if PolarizeVerbosity then << "-- curves detected on U are exceptional; reverting to the \"Genus2Curve\" strategy" << endl;
        return;
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
        if PolarizeVerbosity then << "-- no image on K3 is a curve; reverting to the \"Genus2Curve\" strategy" << endl;
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
    -- assert(source psi === W); -- bug?
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
    (mu,U,exC,f) := building Utilde;
    (L,C) := toSequence exC;
    X := recoverFourfold Utilde;
    (woWarn,g') := unverifiedExpectedGenusOfK3FromExceptionalCurves(X,U,L,C);
    if f =!= null and 2*g'-2 != degree Utilde then error "internal error encountered: mismatch between expected and actual degree and genus of the K3 surface";
    if o.Strategy === "SpecialCurve-Virtual" then (
        if not U.cache#?"special curves on U" then error("strategy \"SpecialCurve-Virtual\" not available: no special curve found on surface U");
        D := select(U.cache#"special curves on U", z -> not(isSubset(z,L) or isSubset(z,C)));
        if #D == 0 then error("strategy \"SpecialCurve-Virtual\" not available: curves detected on U are exceptional");
        D = first D;
        VirtPol := latticePolarizationOnK3Surface(Utilde, D, g');
        if o.Verbose then printFinalLog woWarn;
        return VirtPol;
    );
    errorIfIncompatibleK3Strategy(U,X);
    if o.Strategy === "Genus2Curve-Virtual" then (
        D' := specialGenus2CurveOnU(Utilde,o.Verbose);
        VirtPol' := latticePolarizationOnK3Surface(Utilde, D', g');
        if o.Verbose then printFinalLog woWarn;
        return VirtPol';
    );
    local psi;
    if o.Strategy === "MapFromU-Virtual" then (
        psi = mapFromUtoP2xP2(Utilde,Verbose=>o.Verbose);
        if o.Verbose then << "-- obtained map p1xp2: U --> PP^2xPP^2" << endl;
    ) else if o.Strategy === "MapFromW-Virtual" then (
        psi = mapFromWtoP2xP2(Utilde,Verbose=>o.Verbose);
        if o.Verbose then << "-- obtained map p1xp2: W --> PP^2xPP^2" << endl;
    ) else error("strategy \"" | (toString o.Strategy) | "\" not available; expected one of: \"Genus2Curve-Virtual\", \"SpecialCurve-Virtual\", \"MapFromW-Virtual\", or \"MapFromU-Virtual\"");
    (psi1,psi2) := toSequence projectionMaps psi;
    if o.Verbose then << "-- computing p1^*(H_PP^2)" << endl << flush;
    E1 := psi1^* random(1,0_(target psi1));
    if o.Strategy === "MapFromW-Virtual" then E1 = E1 * U;
    if dim E1 != 1 then error "surface polarization calculation failed, expected dimension 1 for p1^*(H_PP^2)";
    sE1 := interpolateTop(E1,Verbose=>verbosityInterpolateTop(o.Verbose));
    if o.Verbose and sE1 != E1 then << "  -- unexpected non-pure dimensional components were found" << endl;
    if o.Verbose then << "  -- obtained p1^*(H_PP^2): " << ? sE1 << endl;
    if o.Verbose then << "-- computing another p1^*(H_PP^2)" << endl << flush;
    E2 := psi1^* random(1,0_(target psi1));
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
    woWarn2 := true;
    -- if 2*(sectionalGenus U) - 2 != sE1sE2 then (
    --     << "-- WARNING: [virtual polarization] degree mismatch! Expected " << (2*(sectionalGenus U) - 2) << " but got " << sE1sE2 << endl;
    --     woWarn2 = false;
    -- );
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
    C2 := psi2^* random(1,0_(target psi2));
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
    if o.Verbose then printFinalLog(woWarn and woWarn2);
    virtK3
);

errorIfIncompatibleK3Strategy = (U,X) -> (
    if (not isFanoMapStandard X) or U.cache#?"birational maps from X to W and from W to X" then return;
    if not U.cache#?"strategy for surface U" then error "surface U does not appear as computed using the standard polarization methods";
    if U.cache#"strategy for surface U" =!= "Approximate" then return;
    error("the K3 surface was computed with Strategy => \"Approximate\", which is incompatible with the current polarization strategy. Please clear the cache and recompute using Strategy => \"Inverse\". Example: X' = clean X; polarizedK3surface(X', Strategy => \"Inverse\")");
);
