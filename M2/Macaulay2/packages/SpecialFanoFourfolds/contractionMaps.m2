
------------------------------------------------------------------------
-------------- Exceptional curves on U and contraction maps ------------
------------------------------------------------------------------------

exceptionalCurves = method(Options => {Verbose => false, Strategy => null, "NumberLines" => infinity});

exceptionalCurves HodgeSpecialFourfold := o -> X -> (
    NumLines := o#"NumberLines";
    if NumLines =!= infinity then if not(instance(NumLines,ZZ) and NumLines >= 0) then error "option NumberLines expects a nonnegative integer";
    if NumLines === infinity then (
        if member(recognize X,{"planeInPP7", "quarticScrollSurface", "oneNodalSepticDelPezzoSurfaceC26", "FarkasVerra", 3, "6NodalOcticSrollC38", 18, "gushel26''", "surf-5-6-2-nodal", "surf-4-3-1-external", "surf-7-1-9"}) then NumLines = 0;
        if member(recognize X,{"surf-5-7-0-1", 17, 6, "mukai26''", "hyperplane section of a conic bundle over PP2"}) then NumLines = 1;
        if recognize X === 1 then NumLines = 2;
        if recognize X === "october2021-34'" then NumLines = 3;
        if recognize X === "october2021-26''" then NumLines = 4;
        if member(recognize X,{"quinticDelPezzoSurface", "C42", "october2021-20"}) then NumLines = 5;
        if recognize X === "C38Coble" then NumLines = 10;
    );
    a := degreeHypersurface X;
    if o.Verbose then << "-- computing the Fano map μ from " << (if codim ambientFivefold X > 0 then "the fivefold in PP^"|toString(dim ambient X) else "PP^5") << endl;
    mu := fanoMap X;
    e := (map mu).cache#"multiplicityFanoMap";
    if o.Verbose then << "-- computed the map μ from " << (if codim ambientFivefold X > 0 then "the fivefold in PP^"|toString(dim ambient X) else "PP^5") << " to PP^" <<(numgens ambient target mu -1) << " defined by the hypersurfaces" << endl;
    if o.Verbose then << "-- of degree " << a*e-1 << " with points of multiplicity " << e << " along the surface S of degree " << degree surface X << " and genus " << sectionalGenus surface X << endl << flush;
    if o.Verbose then << endl << "-- computing the surface U corresponding to the fourfold X" << endl;
    U := surfaceDeterminingInverseOfFanoMap(X,Verbose=>o.Verbose,Strategy=>o.Strategy);
    if U.cache#?"exceptionalCurves" then return U.cache#"exceptionalCurves";
    if o.Verbose then << endl << "-- computing the surface U' corresponding to another fourfold X'" << endl;
    X' := random X;
    U' := surfaceDeterminingInverseOfFanoMap(X',Verbose=>o.Verbose,Strategy=>o.Strategy);
    if dim(U*U') <= 0 then (
        if o.Verbose then (
            << "-- U ∩ U' contains no (exceptional) curves" << endl;
        );
        return U.cache#"exceptionalCurves" = ((0_U)%U,(0_U)%U);
    );
    if dim(U*U') > 1 then (
        if o.Verbose then (
            if not isStandardK3surface U then (
                << "-- warning: dim(U ∩ U') = 2: 'associatedK3surface' may not complete as expected." << endl;
            ) else (
                << "-- obtained dim(U ∩ U') = 2" << endl;
            );
        );
        return U.cache#"exceptionalCurves" = ((0_U)%U,(0_U)%U);
    );
    if o.Verbose then << endl;
    local LL; local L;
    if instance(NumLines,ZZ) and NumLines > 0 and member(recognize X,{"quinticDelPezzoSurface", 1}) then (
        if o.Verbose then << "-- computing the " << NumLines << " exceptional lines as the top components of U ∩ U'" << endl;
        L = interpolateTop(U*U',if recognize X === 1 then {2} else {3},Verbose=>o.Verbose,"Deep"=>2);
        if degree(U*U') =!= degree(L) then error "something went wrong";
    ) else if instance(NumLines,ZZ) and NumLines == 1 and recognize X === "mukai26''" then (
        if o.Verbose then << "-- computing the " << NumLines << " exceptional line" << " as one of the top components of U ∩ U'" << endl;
        L = first select(decompose interpolateTop((U*U'),{2},Verbose=>o.Verbose,"Deep"=>2),Cu -> dim Cu == 1 and degree Cu == 1);
    ) else if instance(NumLines,ZZ) and NumLines > 0 then (
        if o.Verbose then << "-- computing the " << NumLines << " exceptional line(s) via the Fano scheme of lines" << endl;
        LL = Fano(1,U*U');
        while not(dim LL == 0 and degree LL == NumLines) do (if o.Verbose then << "-- recomputing Fano scheme of lines" << endl; LL = Fano(1,U*U'));
        L = Fano LL;
        while not(dim L == 1 and degree L == NumLines) do (if o.Verbose then << "-- recomputing variety swept out by lines" << endl; L = Fano LL);
    ) else if NumLines === infinity then (
        if o.Verbose then << "-- computing the exceptional lines via the Fano scheme of lines" << endl;
        LL = Fano(1,U*U');
        L = if dim LL >= 0 then Fano LL else 0_U;
    ) else L = 0_U;
    if not (isSubset(L,U) and isSubset(L,U')) then error "containment of exceptional lines in U and U' failed";
    if degree(U*U') == degree(L) then (
        if o.Verbose then << "-- exceptional curves computed: obtained " << NumLines << " line(s)" << endl;
        return U.cache#"exceptionalCurves" = (L%U,(0_U)%U);
    );
    if degree((U*U')\L) != degree (U*U') - degree L then error "decomposition failed: multiplicity detected in exceptional lines (not currently supported)";
    if o.Verbose then << "-- computing the top components of (U ∩ U')\\{exceptional lines} via interpolation" << endl;
    local C;
    if member(recognize X,{"quarticScrollSurface", "oneNodalSepticDelPezzoSurfaceC26", "FarkasVerra", "C38Coble", "C42", 17, "october2021-26''", "october2021-34'", "6NodalOcticSrollC38", 18, " mukai26''"})
    then C = interpolateTop((U*U')\L,{2},Verbose=>o.Verbose,"Deep"=>2)
    else C = interpolateTop(2,(U*U')\L,Verbose=>o.Verbose,"Deep"=>3);
    U.cache#"exceptionalCurves" = (L%U,C%U)
);

exceptionalCurves DoublySpecialCubicFourfold := o -> X -> (
    fanoMapDSCF(X,Verbose=>o.Verbose);
    if o.Verbose then << "-- obtaining surface U corresponding to fourfold X..." << endl;
    U := surfaceDeterminingInverseOfFanoMap(X,Verbose=>o.Verbose,Strategy=>o.Strategy);
    if U.cache#?"exceptionalCurves" then return U.cache#"exceptionalCurves";
    if o.Verbose then << endl << "-- obtaining surface U' corresponding to another fourfold X'..." << endl;
    X' := random X;
    if ideal X' == ideal X then (
        if o.Verbose then << "-- obtained X == X'" << endl;
        if U.cache#?"special curves on U" then (
            -- Used in case "DSCF-V1-27"; consider generalizing this approach for other cases.
            SpecialLines := select(U.cache#"special curves on U", D -> degree D == 1);
            SpecialLines = if #SpecialLines == 0 then (0_U)%U else (sum SpecialLines)%U;
            OtherCurves := select(U.cache#"special curves on U", D -> degree D > 1);
            OtherCurves = if #OtherCurves == 0 then (0_U)%U else (sum OtherCurves)%U;
            return U.cache#"exceptionalCurves" = (SpecialLines,OtherCurves);
        );
        return U.cache#"exceptionalCurves" = ((0_U)%U,(0_U)%U);
    );
    U' := surfaceDeterminingInverseOfFanoMap(X',Verbose=>o.Verbose,Strategy=>o.Strategy);
    if dim(U*U') <= 0 then (
        if o.Verbose then (
            << "-- U ∩ U' contains no (exceptional) curves" << endl;
        );
        return U.cache#"exceptionalCurves" = ((0_U)%U,(0_U)%U);
    );
    if dim(U*U') > 1 then (
        if o.Verbose then (
            if not isStandardK3surface U then (
                << "-- warning: dim(U ∩ U') = 2: 'polarizedK3surface' may not complete as expected." << endl;
            ) else (
                << "-- obtained dim(U ∩ U') = 2" << endl;
            );
        );
        return U.cache#"exceptionalCurves" = ((0_U)%U,(0_U)%U);
    );
    if o.Verbose then << endl << "-- extracting the 1-dimensional part of the intersection U ∩ U'..." << endl;
    E := interpolateTop(U * U',Verbose=>verbosityInterpolateTop(o.Verbose),cache=>true);
    if o.Verbose then << "-- the result has dimension 1 and degree " << degree E << endl;
    assert(degree E >= 1);
    if o.Verbose then << "-- decomposing U ∩ U' via projection to PP^2..." << endl;
    prToP2 := (rationalMap apply(3, i -> random(1, ring ambient E))) | E;
    Z := projectiveVariety kernel(map toRationalMap prToP2,SubringLimit=>1);
    if o.Verbose and degree E != degree Z then << "-- degree mismatch in projection to PP^2 (expected " << degree E << ", obtained " << degree Z << ")" << endl;
    Z1 := {}; Z2 := {};
    for D in decompose Z do if dim D == 1 and degree D == 1 then Z1 = append(Z1,D) else Z2 = append(Z2,D);
    if o.Verbose and degree E == degree Z then (
        if #Z2 == 0 and #Z1 == degree Z then (
            << "-- decomposition: only K-rational lines" << endl;
        );
        if degree Z > sum(Z1,degree) + sum(Z2,degree) then << "  -- multiplicity > 1 detected" << endl;
    );
    if #Z1 == 0 then (
        if o.Verbose then << "-- no K-rational lines found in U ∩ U'" << endl;
        if degree E == degree Z and degree Z > sum(Z2,degree) and #Z2 == 1 then E = support E;
        return U.cache#"exceptionalCurves" = ((0_U)%U,E%U);
    );
    if o.Verbose then << "-- computing inverse images of plane lines..." << endl;
    LinesInE := select(apply(Z1, l -> prToP2^* l), D -> dim D >= 1);
    LinesInE = apply(LinesInE, D -> interpolateTop(D,Verbose=>verbosityInterpolateTop(o.Verbose),cache=>true));
    if someExceptionalCurvesKnownToAppearWithMultiplicity X then LinesInE = apply(LinesInE,support);
    assert all(LinesInE, l -> dim l == 1 and degree l == 1);
    if o.Verbose then (
        << "-- number of lines found: " << #LinesInE << endl;
        if degree E == degree Z and #LinesInE < #Z1 then << "  -- some plane lines have inverse images of dimension < 1 (ignored)" << endl;
    );
    L := sum LinesInE;
    L.cache#"Decomposition" = LinesInE;
    if degree E == degree L then (
        return U.cache#"exceptionalCurves" = (L%U,(0_U)%U);
    );
    C := E \ L;
    if degree C != degree E - degree L then error "decomposition failed: multiplicity detected in exceptional lines (not currently supported)";
    if someExceptionalCurvesKnownToAppearWithMultiplicity X then C = C \\ L;
    U.cache#"exceptionalCurves" = (L%U,C%U)
);

mapFromExceptionalCurves = method(Options => {Verbose => true, Strategy => null, "Normalization" => false, "ForceExperimentalNormalization" => false, "TargetExpDim" => null, "IsK3Type" => true});

mapFromExceptionalCurves EmbeddedProjectiveVariety := o -> U -> (
    if not U.cache#?"exceptionalCurves" then error "exceptional curves not found in cache: method exceptionalCurves() must be called first";
    (L,C) := U.cache#"exceptionalCurves";
    b := null;
    if dim C == -1 then b = 0;
    if dim C == 1 and isPresumedRationalNormalCurve C then b = degree C;
    if b === null then (
        if o.Verbose then << "-- warning: multiple exceptional curves of degree > 1 detected; not yet supported" << endl;
        return;
    );
    mapFromExceptionalCurves(U,(1,1,b),Verbose=>o.Verbose,Strategy=>o.Strategy,"Normalization"=>o#"Normalization","ForceExperimentalNormalization"=>o#"ForceExperimentalNormalization","TargetExpDim"=>o#"TargetExpDim","IsK3Type"=>o#"IsK3Type")
);

mapFromExceptionalCurves (EmbeddedProjectiveVariety,Sequence) := o -> (U,dab) -> (
    if not U.cache#?"exceptionalCurves" then error "exceptional curves not found in cache: method exceptionalCurves() must be called first";
    (L,C) := U.cache#"exceptionalCurves";
    (d,a,b) := dab;
    withNorm := o#"Normalization";
    (eulerExpVal,strSurf) := if o#"IsK3Type" then (2,"K3") else (4,"Castelnuovo");
    if (not withNorm) and d == 1 and a == 0 and b == 0 then (
        if o.Verbose then << "-- U is already in the target space; defining f as the identity map" << endl;
        return multirationalMap super toRationalMap(1_U);
    );
    toNormU := null;
    if withNorm then (
        if o#"ForceExperimentalNormalization" then (
            try toNormU = inverseNormalizationMapExperimentalRaw(U,"U",eulerExpVal,o.Verbose) else return;
        ) else (
            try toNormU = inverseNormalizationMapRaw(U,"U",eulerExpVal,o.Verbose) else return;
        );
    );
    if withNorm and d == 1 and a == 0 and b == 0 then return toNormU;
    local f;
    if toNormU === null then (
        if o.Verbose then << "-- computing the map f from U to the minimal " << strSurf << " surface" << endl << flush;
        H := random(1,0_U);
        D := {(H,d)};
        if a > 0 and dim L != -1 then D = append(D,(L,a));
        if b > 0 and dim C != -1 then D = append(D,(C,b));
        f = mapDefinedByDivisor(U,D);
    ) else (
        if o.Verbose then << "-- computing the map from the normalization of U to the minimal " << strSurf << " surface" << endl << flush;
        H' := random(1,0_(target toNormU));
        D' := {(H',d)};
        if a > 0 and dim L != -1 then D' = append(D',(toNormU L,a));
        if b > 0 and dim C != -1 then D' = append(D',(toNormU C,b));
        f = toNormU * mapDefinedByDivisor(image toNormU,D');
    );
    if o#"TargetExpDim" =!= null and o#"TargetExpDim" != dim target f then (
        if o.Verbose then << "-- failed to compute map to minimal " << strSurf << "; result is a map to PP^" << dim target f << " instead of PP^" << o#"TargetExpDim" << endl;
        return null;
    );
    makeImageMapWithStrategy(f,o.Strategy,o#"TargetExpDim",o.Verbose);
    if o.Verbose and o#"IsK3Type" and (not isStandardK3surface image f) then << "-- note: invariant mismatch for standard K3 surface" << endl;
    f
);

makeImageMapWithStrategy = (f,Str,g,Verb) -> (
    if f#"image" =!= null then return image f;
    if char coefficientRing f <= 65521 then (
        if Verb then << "-- computing the image of f via 'F4' algorithm..." << endl << flush;
        image(f,"F4");
        assert(f#"image" =!= null);
        return;
    );
    if Str =!= "Inverse" and g =!= null then (
        if Verb then << "-- computing the image of f via interpolation..." << endl << flush;
        interpolateImage(f,toList(binomial(g-2,2) : 2),2,Verbose=>Verb);
        assert(f#"image" =!= null);
        return;
    );
    if Verb then << "-- computing the image of f..." << endl << flush;
    image f;
);

saveAndReturnMap = (f,U) -> (
    if f =!= null then U.cache#"MapToMinimalK3Surface" = f;
    return f;
);

contractionMap = method(Options => {Verbose => true, Strategy => null, "ForceNormalization" => false});

contractionMap (EmbeddedProjectiveVariety,HodgeSpecialFourfold) := o -> (U,X) -> (
    if U.cache#?"MapToMinimalK3Surface" then return U.cache#"MapToMinimalK3Surface";
    withNorm := o#"ForceNormalization";
    if not U.cache#?"exceptionalCurves" then error "exceptional curves not found in cache: method exceptionalCurves() must be called first";
    (L,C) := U.cache#"exceptionalCurves";
    genK3 := null;
    if instance(X,DoublySpecialCubicFourfold) and dim C == -1 then (
        genK3 = sectionalGenus U;
    ) else if instance(X,CubicFourfold) or instance(X,GushelMukaiFourfold) then (
        genK3 = lift((discriminant(X)+2)/2,ZZ);
    );
    f := null;
    -------------------------------------
    --- CubicFourfold and GMFourfold ----
    if recognize X === 17 then (
        if o.Verbose then << "-- computing desingularization of U" << endl;
        f0 := rationalMap((support singularLocus U)_U,2,Dominant=>true);
        if o.Verbose then << "-- computing the map f from U to the minimal K3 surface of degree " << discriminant X << endl;
        H := random(1,0_U);
        f1 := mapDefinedByDivisor(target f0,{(f0 (H*U),1),(f0 (3*C),1),(f0 L,1)});
        f = f0 * f1;
        if dim target f != genK3 then (
            if o.Verbose then << "-- failed to compute map to minimal K3 surface; result is a map to PP^" << dim target f << " instead of PP^" << genK3 << endl;
            return null;
        );
        makeImageMapWithStrategy(f,o.Strategy,genK3,o.Verbose);
        if o.Verbose and (not isStandardK3surface image f) then << "-- note: invariant mismatch for standard K3 surface" << endl;
        return saveAndReturnMap(f,U);
    );
    if recognize X === 1 then (
        f = mapFromExceptionalCurves(U,Verbose=>o.Verbose,"Normalization"=>false);
        f = rationalMap(f,Dominant=>true);
        if o.Verbose then << "-- computing normalization of the surface image" << endl;
        f = multirationalMap super toRationalMap(f * inverse3 normalization(target f,Verbose=>false));
        assert(dim target f == genK3 and f#"image" =!= null);
        if o.Verbose and (not isStandardK3surface image f) then << "-- note: invariant mismatch for standard K3 surface" << endl;
        return saveAndReturnMap(f,U);
    );
    if recognize X === "C42" then (
        f = mapFromExceptionalCurves(U,(1,1,2),Verbose=>o.Verbose,"Normalization"=>false,"TargetExpDim"=>genK3);
        return saveAndReturnMap(f,U);
    );
    if recognize X === "mukai26''" then (
        f = mapFromExceptionalCurves(U,(1,1,3),Verbose=>o.Verbose,"Normalization"=>false,"TargetExpDim"=>genK3);
        return saveAndReturnMap(f,U);
    );
    if recognize X === 3 then (
        if withNorm then (
            f = mapFromExceptionalCurves(U,(1,0,0),Verbose=>o.Verbose,"Normalization"=>true,"TargetExpDim"=>genK3);
            return saveAndReturnMap(f,U);
        ) else (
            if o.Verbose then << "-- skipping computation of the map f from U to the minimal K3 surface of degree " << discriminant X << endl << "-- re-run associatedK3surface to finalize computation" << endl;
            return;
        );
    );
    if member(recognize X,{"oneNodalSepticDelPezzoSurfaceC26", 18}) then (
        if withNorm then (
            -- use "ForceExperimentalNormalization"=>true to restore previous working behavior
            f = mapFromExceptionalCurves(U,Verbose=>o.Verbose,"Normalization"=>true,"ForceExperimentalNormalization"=>false,"TargetExpDim"=>genK3);
            return saveAndReturnMap(f,U);
        ) else (
            if o.Verbose then << "-- skipping computation of the map f from U to the minimal K3 surface of degree " << discriminant X << endl << "-- re-run associatedK3surface to finalize computation" << endl;
            return;
        );
    );
    if recognize X === "FarkasVerra" then (
        if withNorm then (
            f = mapFromExceptionalCurves(U,Verbose=>o.Verbose,"Normalization"=>false);
            f = rationalMap(f,Dominant=>true);
            -- use inverseNormalizationMapExperimentalRaw to restore previous working behavior
            n := inverseNormalizationMapRaw(image f,"f(U)",2,o.Verbose);
            f = super(f * n);
            assert(f#"image" =!= null);
            if dim target f != genK3 then (
                if o.Verbose then << "-- failed to compute map to minimal K3 surface; result is a map to PP^" << dim target f << " instead of PP^" << genK3 << endl;
                return null;
            );
            if o.Verbose and (not isStandardK3surface image f) then << "-- note: invariant mismatch for standard K3 surface" << endl;
            return saveAndReturnMap(f,U);
        ) else (
            if o.Verbose then << "-- skipping computation of the map f from U to the minimal K3 surface of degree " << discriminant X << endl << "-- re-run associatedK3surface to finalize computation" << endl;
            return;
        );
    );
    -------------------------------------
    -- IntersectionOfThreeQuadricsInP7 --
    if member(recognize X, {"planeInPP7", "internal-projection-K3-genus-8", "surf-5-6-2-nodal"}) then (
        f = mapFromExceptionalCurves(U,(1,0,0),Verbose=>o.Verbose,"Normalization"=>false,"IsK3Type"=>false);
        return saveAndReturnMap(f,U);
    );
    if recognize X === "surf-5-7-0-1" then (
        f = mapFromExceptionalCurves(U,(1,1,0),Verbose=>o.Verbose,"Normalization"=>false,"IsK3Type"=>false);
        return saveAndReturnMap(f,U);
    );
    if member(recognize X, {"surf-4-3-1-external", "surf-7-1-9"}) then (
        if withNorm then (
            f = mapFromExceptionalCurves(U,(1,0,0),Verbose=>o.Verbose,"Normalization"=>true,"IsK3Type"=>false);
            return saveAndReturnMap(f,U);
        ) else (
            if o.Verbose then << "-- skipping computation of the map f from U to the minimal Castelnuovo surface" << endl << "-- re-run associatedCastelnuovoSurface to finalize computation" << endl;
            return;
        );
    );
    if recognize X === "surf-5-10-1" then (
        if withNorm then (
            n1 := multirationalMap normalization(U,Verbose=>o.Verbose);
            if o.Verbose then << "-- computing the map f from U to the minimal Castelnuovo surface" << endl;
            h1 := rationalMap(source n1,tally {n1^* (random(1,0_U)),n1^* L},Dominant=>3);
            f = multirationalMap inverse rationalMap(ring target h1,ring target n1,take(gens ring target h1,5));
            if not((f * (inverse f) == 1 and (inverse f) * f == 1)) then error "something went wrong";
            return saveAndReturnMap(f,U);
        ) else (
            if o.Verbose then << "-- skipping computation of the map f from U to the minimal Castelnuovo surface" << endl << "-- re-run associatedCastelnuovoSurface to finalize computation" << endl;
            return;
        );
    );
    -------------------------------------
    ---- DoublySpecialCubicFourfold -----
    if instance(X,DoublySpecialCubicFourfold) and isStandardK3surface U then (
        f = mapFromExceptionalCurves(U,(1,0,0),Verbose=>o.Verbose,"Normalization"=>false);
        return saveAndReturnMap(f,U);
    );
    if instance(X,DoublySpecialCubicFourfold) and isHigherDegreeCurveInExceptionalSetKnownToBeSpecial X then (
        f = mapFromExceptionalCurves(U,(1,1,0),Verbose=>o.Verbose,Strategy=>o.Strategy,"Normalization"=>false);
        return saveAndReturnMap(f,U);
    );
    -------------------------------------
    (EulerExpVal,IsK3Type) := if instance(X,IntersectionOfThreeQuadricsInP7) then (4,false) else (2,true);
    if withNorm or euler hilbertPolynomial U == EulerExpVal then (
        f = mapFromExceptionalCurves(U,Verbose=>o.Verbose,Strategy=>o.Strategy,"Normalization"=>(euler hilbertPolynomial U != EulerExpVal),"TargetExpDim"=>genK3,"IsK3Type"=>IsK3Type);
    );
    if f === null and o.Verbose then (
        << "-- skipping computation of the map f : U -> Ũ" << endl;
        << "-- re-run the function to try finalizing computation" << endl;
    );
    saveAndReturnMap(f,U)
);
