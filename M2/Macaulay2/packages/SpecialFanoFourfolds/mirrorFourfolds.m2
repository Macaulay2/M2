
------------------------------------------------------------------------
---------------- Mirror fourfolds: computing surfaces U ----------------
------------------------------------------------------------------------

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
