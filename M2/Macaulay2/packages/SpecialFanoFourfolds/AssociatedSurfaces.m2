
------------------------------------------------------------------------
------------------------ Associated K3 surfaces ------------------------
------------------------------------------------------------------------

SurfaceAssociatedToRationalFourfold = new Type of EmbeddedProjectiveVariety;
WeightedSurfaceAssociatedToRationalFourfold = new Type of WeightedProjectiveVariety;

globalAssignment SurfaceAssociatedToRationalFourfold;
globalAssignment WeightedSurfaceAssociatedToRationalFourfold;

WeightedSurfaceAssociatedToRationalFourfold.synonym = SurfaceAssociatedToRationalFourfold.synonym = "surface associated to rational fourfold";

expression WeightedSurfaceAssociatedToRationalFourfold := expression SurfaceAssociatedToRationalFourfold := U -> (
    X := recoverFourfold U;
    (S,F) := if instance(X,CubicFourfold)
             then ("K3 surface","cubic fourfold")
             else if instance(X,GushelMukaiFourfold)
             then ("K3 surface","GM fourfold")
             else if instance(X,IntersectionOfThreeQuadricsInP7)
             then ("Castelnuovo surface","complete intersection of 3 quadrics in PP^7")
             else ("surface","fourfold");
    if dim U == -1 then S = "not-fully-calculated "|S;
    A := if hasAttribute(X,ReverseDictionary) then toString getAttribute(X,ReverseDictionary) else (F|" of discriminant "|toString(discriminant X));
    expression(S|" associated to "|A)
);

net WeightedSurfaceAssociatedToRationalFourfold := net SurfaceAssociatedToRationalFourfold := U -> (
    if hasAttribute(U,ReverseDictionary) then return toString getAttribute(U,ReverseDictionary);
    if dim U >= 0 then return ? U;
    "-* some calculations are missing *-"
);
texMath WeightedSurfaceAssociatedToRationalFourfold := texMath SurfaceAssociatedToRationalFourfold := texMath @@ net;

describe WeightedSurfaceAssociatedToRationalFourfold := describe SurfaceAssociatedToRationalFourfold := S -> (
    out := "Fourfold: ";
    if hasAttribute(recoverFourfold S,ReverseDictionary) then out = out|(toString getAttribute(recoverFourfold S,ReverseDictionary))|", ";
    (out|shortDescriptionFourfold(recoverFourfold S,false))||(describeMirrorFourfoldAndK3 recoverFourfold S)
);

makeSurfaceAssociated = (X,mu,U,C,f) -> (
    assert(instance(X,HodgeSpecialFourfold) and instance(mu,MultirationalMap) and instance(U,EmbeddedProjectiveVariety) and instance(C,List) and (instance(f,Nothing) or instance(f,MultirationalMap)));
    S := if f =!= null then image f else projectiveVariety((coefficientRing X)[],Saturate=>false);
    S = if instance(S,WeightedProjectiveVariety) then new WeightedSurfaceAssociatedToRationalFourfold from S else new SurfaceAssociatedToRationalFourfold from S;
    S.cache#"construction of SurfaceAssociatedToRationalFourfold" = (mu,U,C,f);
    S.cache#"Hodge-special fourfold" = X;
    if f =!= null then (
        if instance(X,DoublySpecialCubicFourfold) then mu.cache#("AssociatedUnderlyingK3",X) = S else X.cache#"AssociatedSurfaceCompleteData" = S;
    ) else (
        if instance(X,DoublySpecialCubicFourfold) then mu.cache#("AssociatedK3PartialData",X) = S else X.cache#"AssociatedSurfacePartialData" = S;
    );
    return S;
);

building = method();
building WeightedSurfaceAssociatedToRationalFourfold := building SurfaceAssociatedToRationalFourfold := S -> S.cache#"construction of SurfaceAssociatedToRationalFourfold";

recoverFourfold = method();
recoverFourfold WeightedSurfaceAssociatedToRationalFourfold := recoverFourfold SurfaceAssociatedToRationalFourfold := S -> S.cache#"Hodge-special fourfold";

surfaceDeterminingInverseOfFanoMap = method(Options => {Verbose => false, Strategy => null});

surfaceDeterminingInverseOfFanoMap HodgeSpecialFourfold := o -> X -> (
    if (surface X).cache#?("surfaceDeterminingInverseOfFanoMap",ideal X) then return (surface X).cache#("surfaceDeterminingInverseOfFanoMap",ideal X);
    a := degreeHypersurface X;
    Str := o.Strategy;
    if Str === null then (
        Str = "Interpolate";
        if member(recognize X,{"planeInPP7", "quinticDelPezzoSurface", "quarticScrollSurface", 1, 6}) then Str = "Inverse";
        if member(recognize X,{"C38Coble", "FarkasVerra", 3, 17, "october2021-26''", 18}) and char coefficientRing X <= 65521 then Str = "F4";
    );
    mu := fanoMap X;
    if Str === "Inverse" then (
        if o.Verbose then << "-- computing inverse of the restriction of the Fano map..." << endl << flush;
        muInv := inverse3(mu|X);
        if o.Verbose then << "-- inverse map computed: defined by forms of degree " << degreeOfDefiningForms muInv << endl << flush;
        if not X.cache#?"rationalParametrization" then X.cache#"rationalParametrization" = muInv;
        Ui := projectiveVariety(if member(recognize X,{"planeInPP7", 1, 6}) then gens saturate ideal matrix muInv else matrix muInv);
        if not isSurfaceUknownToNotNeedRefining X then Ui = refineSurfaceU(Ui, o.Verbose);
        Ui.cache#"strategy for surface U" = Str;
        if o.Verbose then << "-- result: " << ? Ui << endl << flush;
        return (surface X).cache#("surfaceDeterminingInverseOfFanoMap",ideal X) = Ui;
    );
    mu = super mu;
    if Str === "Interpolate" then (W := Var image mu; iW := lift(3 - (2*(sectionalGenus W)-2)/(degree W),ZZ));
    m := numgens ambient target map X -1;
    if o.Verbose then << "-- needed " << m << " steps" << endl << flush;
    local mu';
    U := Var trim sum apply(m,j ->
        (
            if o.Verbose then <<"-- (step "<<j+1<<" of "<<m<<")"<<endl;
            mu' = mu|(X * random(a,surface X));
            if Str === "DirectImage"
            then ideal image mu'
            else if Str === "F4"
            then ideal image(mu',"F4")
            else if Str === "Interpolate"
            then ideal interpolateImage(mu',append(flatten degrees ideal W,iW),iW,Verbose=>o.Verbose)
            else error "unrecognized Strategy; available strategies are: \"DirectImage\", \"F4\", \"Interpolate\""
        )
    );
    if recognize X === "NotRecognized" then U = interpolateTop(U,Verbose=>verbosityInterpolateTop(o.Verbose),cache=>true);
    if member(recognize X,{"FarkasVerra", 1, "surf-5-6-2-nodal"}) then U = top U;
    if recognize X === "surf-7-1-9" then (if o.Verbose then <<"-- removing cubic scroll component from surface"<<endl; U = U \ Fano Fano(1,U));
    if not isSurfaceUknownToNotNeedRefining X then U = refineSurfaceU(U, o.Verbose);
    U.cache#"strategy for surface U" = Str;
    if o.Verbose then << "-- result: " << ? U << endl << flush;
    (surface X).cache#("surfaceDeterminingInverseOfFanoMap",ideal X) = U
);

surfaceDeterminingInverseOfFanoMap DoublySpecialCubicFourfold := o -> X -> (
    mu := fanoMapDSCF(X,Verbose=>o.Verbose);
    if mu.cache#?("surfaceDeterminingInverseOfFanoMap",X) then return mu.cache#("surfaceDeterminingInverseOfFanoMap",X);
    if o.Verbose then << "-- restricting Fano map to the cubic fourfold" << endl;
    mu' := mu|X;
    local eta;
    Str := setStrategyDSCFtoK3(X,o.Strategy);
    if Str === "Inverse" then (
        if o.Verbose then << "-- computing inverse of the restricted map..." << endl << flush;
        eta = inverse3 mu';
        if o.Verbose then (
            << "-- inverse map computed: defined by forms of degree " << degreeOfDefiningForms eta << endl;
            << "-- map stored in cache: retrieve it with importFrom(SpecialFanoFourfolds, \"getInverseFanoMap\")" << endl << flush;
        );
    ) else if Str === "Approximate" then (
        if o.Verbose then << "-- computing approximate inverse of the restricted map..." << endl << flush;
        eta = multirationalMap approximateInverseMap(toRationalMap mu',Verbose=>o.Verbose,Certify=>false);
        if o.Verbose then << "-- approximate inverse computed: defined by forms of degree " << degreeOfDefiningForms eta << endl << flush;
    ) else (
        error("invalid strategy: '" | toString(Str) | "'; available options are \"Inverse\" or \"Approximate\"");
    );
    if o.Verbose then << "-- analyzing the base locus of the inverse map..." << endl;
    B := projectiveVariety(trim lift(ideal matrix eta,ring ambient source eta),Saturate=>true); -- quickBaseLocus
    if dim B < 2 then error "base locus dimension of at least 2 required";
    U := B;
    if dim B > 2 then (
        topB := interpolateTop(B,Verbose=>verbosityInterpolateTop(o.Verbose),cache=>true);
        U = B \ topB;
        if dim U != 2 then error "extraction failed: dimension of (B \\ top(B)) is not 2";
    );
    assert(dim U == 2);
    if not isSurfaceUknownToBeAlreadyEquidimensional(X,mu) then (
        if o.Verbose then << "-- surface found in base locus; verifying equidimensionality..." << endl;
        tU := interpolateTop(U,Verbose=>verbosityInterpolateTop(o.Verbose),cache=>true);
        assert(dim tU == 2);
        if tU != U then (
            U = tU;
            if o.Verbose then << "-- removed components of dimension < 2" << endl;
            mu.cache#"surface U is already equidimensional" = false;
        ) else (
            if o.Verbose then << "-- equidimensionality verified" << endl;
            mu.cache#"surface U is already equidimensional" = true;
        );
    ) else (
        if o.Verbose then << "-- surface found in base locus; equidimensionality already known, skipping..." << endl;
    );
    if not isSurfaceUknownToNotNeedRefining X then U = refineSurfaceU(U, o.Verbose);
    if not U.cache#?"top" then U.cache#"top" = U;
    if Str === "Inverse" then U.cache#"birational maps from X to W and from W to X" = (mu',eta);
    U.cache#"strategy for surface U" = Str;
    if o.Verbose then << "-- result: " << ? U << endl << flush;
    mu.cache#("surfaceDeterminingInverseOfFanoMap",X) = U
);

refineSurfaceU = (U,verb) -> (
    if verb then << "-- projecting to PP^3 for surface decomposition" << endl;
    pr := rationalMap for i to 3 list random(1,ring ambient U);
    Z := projectiveVariety kernel(map toRationalMap(pr|U),SubringLimit=>1);
    Z1 := {}; Z2 := {};
    for D in decompose Z do if dim D == 2 and degree D <= 3 then Z1 = append(Z1,D) else Z2 = append(Z2,D);
    if #Z2 != 1 then error("unsupported surface decomposition: found "| toString(#Z2) |" components of degree >= 4");
    if #Z1 == 0 then (
        if verb then << "  -- surface was already irreducible" << endl;
        return U;
    ) else (
        if verb then << "  -- removing " << #Z1 << " components of degrees " << apply(Z1, degree) << endl;
    );
    W := apply(Z1, D -> support interpolateTop(U * (pr^* D), Verbose=>verbosityInterpolateTop(verb),cache=>true));
    assert all(W, D -> dim D == 2 and degree D <= 3);
    vU := U;
    for D in W do vU = vU \\ D;
    assert(dim vU == 2);
    Curves := apply(select(apply(W, D -> vU * D), C -> dim C == 1), E -> support interpolateTop(E, Verbose=>verbosityInterpolateTop(verb),cache=>true));
    Curves = apply(sort apply(Curves, C -> (degree C,C)), last);
    if # Curves > 0 then vU.cache#"special curves on U" = Curves;
    vU
);

isSurfaceUknownToNotNeedRefining = method();
isSurfaceUknownToNotNeedRefining DoublySpecialCubicFourfold := X -> isFanoMapStandard(X) and member(recognizeDSCF X, {"DSCF-V1-1", "DSCF-V1-2", "DSCF-V1-3", "DSCF-V1-7", "DSCF-V1-9", "DSCF-V1-12", "DSCF-V1-16", "DSCF-V1-17", "DSCF-V1-18", "DSCF-V1-20", "DSCF-V1-21", "DSCF-V1-25", "DSCF-V1-26", "DSCF-V1-28", "DSCF-V1-29", "DSCF-V1-30", "DSCF-V1-32", "DSCF-V1-33", "DSCF-V1-35", "DSCF-V1-36", "DSCF-V1-37", "DSCF-V1-38", "DSCF-V1-39", "DSCF-V1-40"});
isSurfaceUknownToNotNeedRefining IntersectionOfThreeQuadricsInP7 := X -> not member(recognize X, {"surf-7-1-9", "NotRecognized"});
isSurfaceUknownToNotNeedRefining GushelMukaiFourfold := isSurfaceUknownToNotNeedRefining CubicFourfold := X -> true;
isSurfaceUknownToNotNeedRefining HodgeSpecialFourfold := X -> false;

getInverseFanoMap = method();
getInverseFanoMap SurfaceAssociatedToRationalFourfold := Utilde -> (
    (mu,U,C,f) := building Utilde;
    if not U.cache#?"birational maps from X to W and from W to X" then error "birational maps between cubic fourfold X and mirror fourfold W not found in cache; the K3 surface may have been computed using an unsuitable strategy";
    last U.cache#"birational maps from X to W and from W to X"
);
getInverseFanoMap DoublySpecialCubicFourfold := X -> (
    (S,P) := surfaces X;
    if not S.cache#?("FanoMapDSCF",P) then error "Fano map not found in cache; polarizedK3surface or mirrorFourfold must be called first";
    mu := fanoMapDSCF(X,Verbose=>false);
    if not mu.cache#?("surfaceDeterminingInverseOfFanoMap",X) then error "inverse of Fano map not found in cache; polarizedK3surface or mirrorFourfold must be called first";
    U := surfaceDeterminingInverseOfFanoMap(X,Verbose=>false);
    if not U.cache#?"birational maps from X to W and from W to X" then error "birational maps between cubic fourfold X and mirror fourfold W not found in cache; polarizedK3surface may have been computed using an unsuitable strategy";
    last U.cache#"birational maps from X to W and from W to X"
);

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
    if o.Verbose then <<"-- computing the Fano map μ from "<<(if codim ambientFivefold X > 0 then "the fivefold in PP^"|toString(dim ambient X) else "PP^5")<<endl;
    mu := fanoMap X;
    e := (map mu).cache#"multiplicityFanoMap";
    if o.Verbose then <<"-- computed the map μ from "<<(if codim ambientFivefold X > 0 then "the fivefold in PP^"|toString(dim ambient X) else "PP^5")<<" to PP^"<<(numgens ambient target mu -1)<<" defined by the hypersurfaces"<<endl;
    if o.Verbose then <<"-- of degree "<<a*e-1<<" with points of multiplicity "<<e<<" along the surface S of degree "<<degree surface X<<" and genus "<<sectionalGenus surface X<<endl<<flush;
    if o.Verbose then <<endl<<"-- computing the surface U corresponding to the fourfold X"<<endl;
    U := surfaceDeterminingInverseOfFanoMap(X,Verbose=>o.Verbose,Strategy=>o.Strategy);
    if U.cache#?"exceptionalCurves" then return U.cache#"exceptionalCurves";
    if o.Verbose then <<endl<<"-- computing the surface U' corresponding to another fourfold X'"<<endl;
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
    if o.Verbose then <<endl;
    local LL; local L;
    if instance(NumLines,ZZ) and NumLines > 0 and member(recognize X,{"quinticDelPezzoSurface", 1}) then (
        if o.Verbose then <<"-- computing the "<<NumLines<<" exceptional lines"<<" as the top components of U ∩ U'"<<endl;
        L = interpolateTop(U*U',if recognize X === 1 then {2} else {3},Verbose=>o.Verbose,"Deep"=>2);
        if degree(U*U') =!= degree(L) then error "something went wrong";
    ) else if instance(NumLines,ZZ) and NumLines == 1 and recognize X === "mukai26''" then (
        if o.Verbose then <<"-- computing the "<<NumLines<<" exceptional line"<<" as one of the top components of U ∩ U'"<<endl;
        L = first select(decompose interpolateTop((U*U'),{2},Verbose=>o.Verbose,"Deep"=>2),Cu -> dim Cu == 1 and degree Cu == 1);
    ) else if instance(NumLines,ZZ) and NumLines > 0 then (
        if o.Verbose then <<"-- computing the "<<NumLines<<" exceptional line(s) via the Fano scheme of lines"<<endl;
        LL = Fano(1,U*U');
        while not(dim LL == 0 and degree LL == NumLines) do (if o.Verbose then <<"-- recomputing Fano scheme of lines"<<endl; LL = Fano(1,U*U'));
        L = Fano LL;
        while not(dim L == 1 and degree L == NumLines) do (if o.Verbose then <<"-- recomputing variety swept out by lines"<<endl; L = Fano LL);
    ) else if NumLines === infinity then (
        if o.Verbose then <<"-- computing the exceptional lines via the Fano scheme of lines"<<endl;
        LL = Fano(1,U*U');
        L = if dim LL >= 0 then Fano LL else 0_U;
    ) else L = 0_U;
    if not (isSubset(L,U) and isSubset(L,U')) then error "containment of exceptional lines in U and U' failed";
    if degree(U*U') == degree(L) then (
        if o.Verbose then <<"-- exceptional curves computed: obtained "<<NumLines<<" line(s)"<<endl;
        return U.cache#"exceptionalCurves" = (L%U,(0_U)%U);
    );
    if degree((U*U')\L) != degree (U*U') - degree L then error "decomposition failed: multiplicity detected in exceptional lines (not currently supported)";
    if o.Verbose then <<"-- computing the top components of (U ∩ U')\\{exceptional lines} via interpolation"<<endl;
    local C;
    if member(recognize X,{"quarticScrollSurface", "oneNodalSepticDelPezzoSurfaceC26", "FarkasVerra", "C38Coble", "C42", 17, "october2021-26''", "october2021-34'", "6NodalOcticSrollC38", 18, " mukai26''"})
    then C = interpolateTop((U*U')\L,{2},Verbose=>o.Verbose,"Deep"=>2)
    else C = interpolateTop(2,(U*U')\L,Verbose=>o.Verbose,"Deep"=>3);
    U.cache#"exceptionalCurves" = (L%U,C%U)
);

exceptionalCurves DoublySpecialCubicFourfold := o -> X -> (
    fanoMapDSCF(X,Verbose=>o.Verbose);
    if o.Verbose then << "-- obtaining surface U corresponding to fourfold X..." <<endl;
    U := surfaceDeterminingInverseOfFanoMap(X,Verbose=>o.Verbose,Strategy=>o.Strategy);
    if U.cache#?"exceptionalCurves" then return U.cache#"exceptionalCurves";
    if o.Verbose then << endl << "-- obtaining surface U' corresponding to another fourfold X'..." <<endl;
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

printFinalLogSurface = strSurf -> (
    Tstart := currentTime();
    TstartCPU := cpuTime();
    U -> (
        Tend := currentTime() - Tstart;
        TendCPU := cpuTime() - TstartCPU;
        U.cache#"computationTime" = (Tend, TendCPU);
        X := recoverFourfold U;
        isStandard := if instance(X,CubicFourfold) or instance(X,GushelMukaiFourfold) then isStandardK3surface U else true;
        statusOut := if dim U == 2 and isStandard
                     then (" ✦ ", "successfully completed")
                     else if dim U == 2
                     then (" ⟐ ", "completed")
                     else (" ✧ ", "completed (partial data)");
        << first statusOut << strSurf << last statusOut << " in " << humanReadableSeconds(Tend) << " (cpu: " << humanReadableSeconds(floor TendCPU) << ")" << endl;
    )
);

associatedK3surface = method(Options => {Verbose => false, Strategy => null, Singular => null});
associatedCastelnuovoSurface = method(Options => {Verbose => false, Strategy => null, Singular => null});
associatedCastelnuovoSurface IntersectionOfThreeQuadricsInP7 := associatedK3surface GushelMukaiFourfold := associatedK3surface CubicFourfold := o -> X -> (
    if X.cache#?"AssociatedSurfaceCompleteData" then return X.cache#"AssociatedSurfaceCompleteData";
    if X.cache#?"AssociatedSurfacePartialData" then (
        UtildeP := X.cache#"AssociatedSurfacePartialData";
        return buildMinimalK3ViaNormalization(UtildeP,Verbose=>o.Verbose);
    );
    strSurf := if instance(X,IntersectionOfThreeQuadricsInP7) then "Castelnuovo " else "K3 ";
    if o.Verbose then (
        printFinalLog := printFinalLogSurface("associated "|strSurf);
        << "-- starting associated "<< strSurf << "computation" << endl;
        << "-- input: fourfold containing a " << surfaceDescription surface X << endl;
        printInfoOnPlannedSteps X;
    );
    if (not X.cache#?(surface X,"label")) and o.Verbose then <<"-- recognizing the fourfold"<<endl;
    recognize X;
    if o.Verbose then (if X.cache#(surface X,"label") === "NotRecognized" then <<"-- fourfold not recognized"<<endl else <<"-- the fourfold has been successfully recognized"<<endl);
    fanoMap(X,Singular=>o.Singular,Verbose=>o.Verbose);
    (L,C) := exceptionalCurves(X,Verbose=>o.Verbose,Strategy=>o.Strategy);
    U := ambientVariety L;
    mu := multirationalMap fanoMap X;
    f := if instance(X,IntersectionOfThreeQuadricsInP7) then buildContractionMapCastelnuovo(U,L,C,X,o.Verbose) else buildContractionMapK3(U,L,C,X,o.Strategy,o.Verbose);
    Utilde := makeSurfaceAssociated(X,mu,U,{L,C},f);
    if o.Verbose then printFinalLog Utilde;
    Utilde
);

associatedUnderlyingK3Raw = method(TypicalValue => SurfaceAssociatedToRationalFourfold, Options => {Verbose => true, Strategy => null, FanoMapType => null});
associatedUnderlyingK3Raw DoublySpecialCubicFourfold := o -> X -> (
    mu := getCachedFanoMapIfCompatible(X,o.FanoMapType);
    if mu =!= null and mu.cache#?("AssociatedUnderlyingK3",X) then return mu.cache#("AssociatedUnderlyingK3",X);
    if o.Verbose and (not any(surfaces X, isPlaneInP5)) then (
        << "-- warning: expected at least one plane surface" << endl << "-- attempting to find the K3 surface anyway..." << endl;
    );
    if o.Verbose then (
        printFinalLog := printFinalLogSurface "underlying K3 ";
        << "-- starting underlying K3 computation" << endl;
        << "-- input: cubic fourfold containing two surfaces:" << endl;
        << ("  -- " | surfaceDescription first surfaces X) << endl;
        << ("  -- " | surfaceDescription last surfaces X) << endl;
        << "-- settings: Verbose => " << o.Verbose << ", Strategy => " << (if instance(o.Strategy, String) then "\"" | o.Strategy | "\"" else toString(o.Strategy)) << ", FanoMapType => " << (if instance(o.FanoMapType, String) then "\"" | o.FanoMapType | "\"" else toString(o.FanoMapType)) << endl;
        << "-- available strategies: \"Inverse\", \"Approximate\"" << endl;
        printInfoOnPlannedSteps X;
        if not any(surfaces X, isPlaneInP5) then << "-- warning: no plane detected; trying K3 computation anyway" << endl;
    );
    if mu === null then (
        if o.FanoMapType === "Standard" then setFanoMapToBeStandard(X,Verbose=>o.Verbose);
        if o.FanoMapType === "P2xP2" then setFanoMapToBeMapFromP5toP2xP2(X,Verbose=>o.Verbose);
        mu = fanoMapDSCF(X,Verbose=>o.Verbose);
    );
    if mu.cache#?("AssociatedUnderlyingK3",X) then return mu.cache#("AssociatedUnderlyingK3",X);
    if (not isPlaneInP5 last surfaces X) and X.cache#?"fusedVersion" and (surface fuse X).cache#?("fanoMap",ambientFivefold fuse X) then (
        if ((fanoMap fuse X)#"map").cache#"multiplicityFanoMap" >= 2 then (
            if o.Verbose then (
                << "-------------------------------------------------------------------" << endl;
                << "-- info: special configuration detected.                           " << endl;
                << "--       redirecting computation to associatedK3surface(fuse X)... " << endl;
                << "-------------------------------------------------------------------" << endl;
            );
            UtildeFuse := associatedK3surface(fuse X,Verbose=>o.Verbose,Strategy=>"F4");
            if o.Verbose then printFinalLog UtildeFuse;
            return UtildeFuse;
        );
    );
    (L,C) := exceptionalCurves(X,Verbose=>o.Verbose,Strategy=>setStrategyDSCFtoK3(X,o.Strategy));
    U := ambientVariety L;
    f := buildContractionMapK3DSCF(U,L,C,X,isNormalizationKnownToTerminateQuickly(X),setStrategyDSCFtoK3(X,o.Strategy),o.Verbose);
    Utilde := makeSurfaceAssociated(X,mu,U,{L,C},f);
    if o.Verbose then printFinalLog Utilde;
    Utilde
);

printInfoOnPlannedSteps = X -> (
    << "-- planned steps:" << endl;
    << "-- 1. compute Fano map μ" << (if dim ambient X == 5 then " : ℙ⁵ ⇢ W" else "") << endl;
    << "-- 2. extract surface U from the base locus of (μ|X)⁻¹ : W ⇢ X" << endl;
    << "-- 3. take a general "<< (if instance(X,CubicFourfold) then "cubic " else "fourfold ") << "X' containing the " << (if instance(X,DoublySpecialCubicFourfold) then "two surfaces " else "surface ") << "and extract U'" << endl;
    << "-- 4. analyze the intersection U ∩ U' for exceptional curves" << endl;
    << "-- 5. compute the contraction map f : U -> Ũ" << endl;
    if instance(X,DoublySpecialCubicFourfold) then (
        << "-- 6. prepare data for lattice polarization" << endl;
        << "-- info: re-run the function for lattice computation and use building() to access" << endl;
        << "-- construction data (μ, U, exceptional curves, f)" << endl;
    ) else (
        << "-- info: use building() to access construction data (μ, U, exceptional curves, f)" << endl;
    );
    << endl;
);

buildContractionMapCastelnuovo = (U,L,C,X,Verb) -> (
    if U.cache#?"MapToMinimalK3Surface" then return U.cache#"MapToMinimalK3Surface"; -- inappropriate key name
    f := null; H := random(1,0_U);
    if member(recognize X,{"NotRecognized", "surf-7-1-9"}) then (
        if Verb then <<"-- skipping computation of the map f from U to the minimal Castelnuovo surface"<<endl;
    ) else if member(recognize X,{"planeInPP7", "internal-projection-K3-genus-8", "surf-5-6-2-nodal"}) then (
        f = multirationalMap super toRationalMap 1_U;
    ) else if recognize X === "surf-5-7-0-1" then (
        if Verb then <<"-- computing the map f from U to the minimal Castelnuovo surface"<<endl;
        f = mapDefinedByDivisor(U,{(H,1),(L,1)});
        if char coefficientRing X <= 65521 then image(f,"F4") else image f;
    ) else if recognize X === "surf-5-10-1" then (
        n := multirationalMap normalization(U,Verbose=>Verb);
        if Verb then <<"-- computing the map f from U to the minimal Castelnuovo surface"<<endl;
        h := rationalMap(source n,tally {n^* H,n^* L},Dominant=>3);
        f = multirationalMap inverse rationalMap(ring target h,ring target n,take(gens ring target h,5));
        if not((f * (inverse f) == 1 and (inverse f) * f == 1)) then error "something went wrong";
    ) else if recognize X === "surf-4-3-1-external" then (
        f = inverseNormalizationMapRaw(U,"U",2,Verb);
    );
    if f =!= null then U.cache#"MapToMinimalK3Surface" = f;
    return f;
);

buildContractionMapK3 = (U,L,C,X,Str,Verb) -> (
    if U.cache#?"MapToMinimalK3Surface" then return U.cache#"MapToMinimalK3Surface";
    genK3 := lift((discriminant(X)+2)/2,ZZ);
    f := null; H := random(1,0_U); local normU;
    if member(recognize X,{"NotRecognized", "october2021-34'", "october2021-20"}) then (
        if Verb then <<"-- skipping computation of the map f from U to the minimal K3 surface of degree "<<discriminant X<<endl;
    ) else if recognize X === 3 then (
        f = inverseNormalizationMapRaw(U,"U",2,Verb);
    ) else if recognize X === 17 then (
        if Verb then <<"-- computing desingularization of U"<<endl;
        f0 := rationalMap((support singularLocus U)_U,2,Dominant=>true);
        if Verb then <<"-- computing the map f from U to the minimal K3 surface of degree "<<discriminant X<<endl;
        f1 := mapDefinedByDivisor(target f0,{(f0 (H*U),1),(f0 (3*C),1),(f0 L,1)});
        f = f0 * f1;
    ) else if member(recognize X,{"oneNodalSepticDelPezzoSurfaceC26", 18}) then (
        if Verb then <<"-- computing desingularization of U"<<endl;
        normU = experimentalNormalizationInv(U,Verbose=>Verb);
        if Verb then <<"-- computing the map f from U to the minimal K3 surface of degree "<<discriminant X<<endl;
        f = normU * mapDefinedByDivisor(image(normU,"F4"),{(random(1,0_(target normU)),1),(normU L,1),(normU C,degree C)});
    ) else if recognize X === 1 then (
        if Verb then <<"-- computing the map f from U to the minimal K3 surface of degree "<<discriminant X<<endl;
        f = mapDefinedByDivisor(U,{(H,1),(L,1),(C,degree C)});
        if char coefficientRing X <= 65521 then image(f,"F4");
        f = rationalMap(f,Dominant=>true);
        if Verb then <<"-- computing normalization of the surface image"<<endl;
        f = multirationalMap super toRationalMap(f * inverse3 normalization(target f,Verbose=>false));
        if f#"image" === null then error "something went wrong";
    ) else (
        if Verb then <<"-- computing the map f from U to the minimal K3 surface of degree "<<discriminant X<<endl;
        m := degree C;
        if recognize X === "C42" then m = 2;
        if recognize X === "mukai26''" then m = 3;
        f = mapDefinedByDivisor(U,{(H,1),(L,1),(C,m)});
        if recognize X === "FarkasVerra" then f = f * experimentalNormalizationInv(image(f,"F4"),Verbose=>Verb);
    );
    if f =!= null then (
        if dim target f =!= genK3 then error("expected map to PP^"|(toString genK3)|", but got map to PP^"|toString(dim target f));
        if char coefficientRing X <= 65521 then (
            if Verb then <<"-- computing the image of f via F4 algorithm"<<endl<<flush;
            image(f,"F4");
        ) else if Str =!= "Inverse" then (
            if Verb then <<"-- computing the image of f via interpolation"<<endl<<flush;
            interpolateImage(f,toList(binomial(genK3-2,2) : 2),2,Verbose=>Verb);
        ) else (
            if Verb then <<"-- computing the image of f"<<endl<<flush;
            image f;
        );
        assert(f#"image" =!= null);
        if not isStandardK3surface image f then (
            << "-- WARNING: invariant mismatch for standard K3 surface; " << endl;
            << "            returning object for debugging purposes." << endl;
        );
        U.cache#"MapToMinimalK3Surface" = f;
    );
    return f;
);

buildContractionMapK3DSCF = (U,L,C,X,withNorm,Str,Verb) -> (
    if U.cache#?"MapToMinimalK3Surface" then return U.cache#"MapToMinimalK3Surface";
    f := null;
    if isStandardK3surface U then (
        if Verb then << "-- U is already in the target space; defining f as the identity map" << endl;
        f = rationalMap(U,ambient U);
        image f;
    );
    toNormU := null;
    if f === null and withNorm then (
        try toNormU = inverseNormalizationMapRaw(U,"U",2,Verb);
    );
    if f === null then (
        if toNormU =!= null then (
            if Verb then << "-- computing the map from the normalization of U to the minimal K3 surface" << endl << flush;
            H' := random(1,0_(target toNormU));
            if dim C == -1
            then f = toNormU * mapDefinedByDivisor(image toNormU,{(H',1),(toNormU L,1)})
            else if dim C == 1 and sectionalGenus C == 0
            then f = toNormU * mapDefinedByDivisor(image toNormU,{(H',1),(toNormU L,1),(toNormU C,degree C)})
            else (
                if Verb then << "-- warning: multiple exceptional curves of degree > 1 detected; not yet supported" << endl;
            );
        ) else if euler hilbertPolynomial U == 2 then (
            if Verb then << "-- computing the map f from U to the minimal K3 surface" << endl << flush;
            H := random(1,0_U);
            if dim C == -1 or isHigherDegreeCurveInExceptionalSetKnownToBeSpecial(X)
            then f = mapDefinedByDivisor(U,{(H,1),(L,1)})
            else if dim C == 1 and sectionalGenus C == 0
            then f = mapDefinedByDivisor(U,{(H,1),(L,1),(C,degree C)})
            else (
                if Verb then << "-- warning: multiple exceptional curves of degree > 1 detected; not yet supported" << endl;
            );
        );
    );
    if f =!= null and dim C == -1 and dim target f != sectionalGenus U then (
        if Verb then << "-- failed to compute map to minimal K3; result is a map to PP^" << dim target f << " instead of PP^" << sectionalGenus U << endl;
        f = null;
    );
    if f =!= null then (
        if f#"image" === null then (
            if char coefficientRing X <= 65521 then (
                if Verb then << "-- computing the image of f via 'F4' algorithm..." << endl << flush;
                image(f,"F4");
            ) else if Str =!= "Inverse" and dim C == -1 then (
                if Verb then << "-- computing the image of f via interpolation..." << endl << flush;
                interpolateImage(f,toList(binomial((sectionalGenus U)-2,2) : 2),2,Verbose=>Verb);
            ) else (
                if Verb then << "-- computing the image of f..." << endl << flush;
                image f;
            );
        );
        assert(f#"image" =!= null);
        if not isStandardK3surface image f then (
            << "-- WARNING: invariant mismatch for standard K3 surface; " << endl;
            << "            returning object for debugging purposes." << endl;
        );
    );
    if f =!= null then U.cache#"MapToMinimalK3Surface" = f;
    return f;
);

buildMinimalK3ViaNormalization = method(Options => {Verbose => true});
buildMinimalK3ViaNormalization SurfaceAssociatedToRationalFourfold := o -> Utilde -> (
    (mu,U,exC,f) := building Utilde;
    if f =!= null or Utilde.cache#?"attemptedMinimalK3ViaNormalization" or euler hilbertPolynomial U == 2 then return Utilde;
    (L,C) := toSequence exC;
    X := recoverFourfold Utilde;
    if instance(X,DoublySpecialCubicFourfold) then (
        Str := U.cache#"strategy for surface U";
        f = buildContractionMapK3DSCF(U,L,C,X,true,Str,o.Verbose);
    ) else if instance(X,CubicFourfold) or instance(X,GushelMukaiFourfold) then (
        if o.Verbose then << "-- surface normalization required: not yet automated" << endl;
    ) else if instance(X,IntersectionOfThreeQuadricsInP7) then (
        if o.Verbose then << "-- surface normalization required: not yet automated" << endl;
    ) else error "fourfold type not supported";
    Utilde.cache#"attemptedMinimalK3ViaNormalization" = true;
    if f === null then return Utilde;
    return makeSurfaceAssociated(X,mu,U,{L,C},f);
);

mirrorFourfold = method(TypicalValue => HodgeSpecialFourfold, Options => {Verbose => false, Strategy => null, Singular => null});

mirrorFourfold CubicFourfold := mirrorFourfold GushelMukaiFourfold := mirrorFourfold IntersectionOfThreeQuadricsInP7 := o -> X -> (
    if X.cache#?(surface X,"associatedFourfold") then return X.cache#(surface X,"associatedFourfold");
    if o.Verbose then <<"-- computing the Fano map"<<endl;
    mu := fanoMap(X,Singular=>o.Singular,Verbose=>o.Verbose);
    if o.Verbose then <<"-- computing the surface U corresponding to the given fourfold"<<endl;
    U := surfaceDeterminingInverseOfFanoMap(X,Verbose=>o.Verbose,Strategy=>o.Strategy);
    W := specialFourfold(U,Var target mu,InputCheck=>0);
    W.cache#(surface W,"associatedFourfold") = X;
    X.cache#(surface X,"associatedFourfold") = W
);

mirrorFourfold DoublySpecialCubicFourfold := o -> X -> (
    mu := fanoMapDSCF(X,Verbose=>o.Verbose);
    if mu.cache#?("mirrorFourfold",X) then return mu.cache#("mirrorFourfold",X);
    U := surfaceDeterminingInverseOfFanoMap(X,Verbose=>o.Verbose,Strategy=>o.Strategy);
    W := specialFourfold(U,target mu,InputCheck=>0);
    W.cache#(U,"associatedFourfold") = X;
    mu.cache#("mirrorFourfold",X) = W
);

mirrorFourfold HodgeSpecialFourfold := o -> X -> (
    if not X.cache#?(surface X,"associatedFourfold") then error "can't find associated fourfold";
    X.cache#(surface X,"associatedFourfold")
);
