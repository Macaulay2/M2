
eulerCharacteristic = method(Options => {Algorithm => null});
eulerCharacteristic EmbeddedProjectiveVariety := o -> X -> (
    if X.cache#?"euler" then return X.cache#"euler";
    if codim linearSpan X > 0 then return X.cache#"euler" = eulerCharacteristic((parametrize linearSpan X)^^ X,Algorithm=>o.Algorithm);
    if o.Algorithm === "Hodge" then return X.cache#"euler" = euler variety X;
    if o.Algorithm === "CremonaCertifyTrue" then return euler(X,Verify=>true);
    K := coefficientRing X;
    if K === QQ then return X.cache#"euler" = eulerCharacteristic(X ** (ZZ/65521),Algorithm=>o.Algorithm);
    if char K < 1000 and K === ZZ/(char K) then <<"--warning: base field too small to use probabilistic methods"<<endl;
    if o.Algorithm === "CremonaCertifyFalse" then return X.cache#"euler" = euler(X,Verify=>false);
    if o.Algorithm === "CharacteristicClasses" then return X.cache#"euler" = Euler(ideal X,InputIsSmooth=>true);
    if o.Algorithm === null then (
        X' := if max flatten degrees ideal X > 2 and dim X == 2 then isomorphicProjectionOfSurfaceInP5 X else X;
        return X.cache#"euler" = if codim X' > 0 then Euler(ideal X',InputIsSmooth=>true) else euler(X',Verify=>false);
    );
    error(///Algorithm option: Expected method to compute the topological Euler characteristic.
Possible methods are the following:
"Hodge" -- command: euler variety X -- package: Core;
"CremonaCertifyTrue" -- command: EulerCharacteristic(ideal X,Certify=>true) -- package: Cremona;
"CremonaCertifyFalse" -- command: EulerCharacteristic ideal X -- package: Cremona;
"CharacteristicClasses" -- command: Euler(ideal X,InputIsSmooth=>true) -- package: CharacteristicClasses///);
);

inverse3 = method();
inverse3 RationalMap := psi -> (
    if psi#"inverseRationalMap" =!= null then return psi#"inverseRationalMap";
    phi := rationalMap map inverseOfMap(map psi,CheckBirational=>false,AssumeDominant=>true,QuickRank=>false,MinorsLimit=>0,Verbosity=>0);
    forceInverseMap(phi,psi);
    phi
);
inverse3 MultirationalMap := Psi -> (
    if Psi#"inverse" =!= null then return Psi#"inverse";
    Phi := multirationalMap inverse3 toRationalMap Psi;
    if ring source Phi =!= ring target Psi then error "internal error encountered";
    Phi#"source" = target Psi;
    if ring target Phi =!= ring source Psi then error "internal error encountered";
    Phi = multirationalMap(Phi,source Psi);
    Phi#"inverse" = Psi;
    Psi#"inverse" = Phi
);
inverse3 (RationalMap,Option) := (psi,opt) -> inverse3 psi;
inverse3 (MultirationalMap,Option) := (Psi,opt) -> inverse3 Psi;

VirtualInverseWeightedRationalMap = new Type of WeightedRationalMap;
virtualInverseWeightedRationalMap = method();
virtualInverseWeightedRationalMap WeightedRationalMap := Phi -> (
    new VirtualInverseWeightedRationalMap from {
        symbol cache => new CacheTable,
        "maps" => {},
        "target" => source Phi,
        "source" => target Phi,
        "image" => source Phi,
        "isDominant" => true,
        "isBirational" => true,
        "graph" => null,
        "multidegree" => null,
        "baseLocus" => null,
        "inverse" => Phi
    }
);
inverse3 VirtualInverseWeightedRationalMap := inverse VirtualInverseWeightedRationalMap := Phi -> Phi#"inverse";
super VirtualInverseWeightedRationalMap := Phi -> (
    Psi := virtualInverseWeightedRationalMap inverse Phi;
    Psi#"target" = ambient target Psi;
    Psi#"isDominant" = null;
    Psi#"isBirational" = null;
    return Psi;
);
VirtualInverseWeightedRationalMap EmbeddedProjectiveVariety := (Phi,Z) -> (
    -- Provisional implementation: thought specifically for computing images of curves
    -- under the inverse normalization of a surface.
    if Phi#"inverse" === null and Phi.cache#?"Composition of VirtualInverseWeightedRationalMap and WeightedRationalMap" then (
        (f,g) := Phi.cache#"Composition of VirtualInverseWeightedRationalMap and WeightedRationalMap";
        return g (f Z);
    );
    h := inverse Phi;
    decZ := if Z.cache#?"Decomposition" then decompose Z else {Z};
    W := for L in decZ list h^* L;
    return sum W;
);
VirtualInverseWeightedRationalMap * MultirationalMap := (Phi,Psi) -> (
    if Psi#"image" === null then (
        << "-- virtual maps used; computing image and composition..." << endl;
        image Psi;
    );
    Eta := new VirtualInverseWeightedRationalMap from {
        symbol cache => new CacheTable,
        "maps" => {},
        "target" => target Psi,
        "source" => source Phi,
        "image" => image Psi,
        "isDominant" => null,
        "isBirational" => null,
        "graph" => null,
        "multidegree" => null,
        "baseLocus" => null,
        "inverse" => null
    };
    Eta.cache#"Composition of VirtualInverseWeightedRationalMap and WeightedRationalMap" = (Phi,Psi);
    return Eta;
);
MultirationalMap * VirtualInverseWeightedRationalMap := (Phi,Psi) -> virtualInverseWeightedRationalMap((inverse Psi) * (inverse3 Phi));

normalization = method(Options => {Verbose => true});
normalization EmbeddedProjectiveVariety := o -> X -> (
    if X.cache#?"Normalization" then return X.cache#"Normalization";
    if o.Verbose then <<"-- computing normalization of "|toString(? ideal X)<<endl;
    f := rationalMap icMap ring X;
    if o.Verbose then <<"-- got: "|toString(expression f)<<endl;
    X.cache#"Normalization" = f
);

-*
experimentalNormalizationInv = method(Options => {Verbose => true});
experimentalNormalizationInv EmbeddedProjectiveVariety := o -> X -> (
    if X.cache#?"ExperimentalNormalizationInv" then return X.cache#"ExperimentalNormalizationInv";
    if not (dim X == 2 and dim linearSpan X > 5) then error "expected a surface with linear span of dimension > 5";
    if o.Verbose then <<"-- computing experimental normalization of "|toString(? ideal X)<<endl;
    pts := apply(dim ambient X - 5,i -> point X);
    pr := rationalMap((linearSpan pts)_X,Dominant=>true);
    if degree target pr != degree X - (dim ambient X - 5) then error "something went wrong";
    n := multirationalMap normalization(target pr,Verbose=>o.Verbose);
    if o.Verbose then <<"-- inverting normalization"<<endl;
    n' := inverse3 n;
    if o.Verbose then <<"-- inverting projection"<<endl;
    pr' := inverse3 pr;
    if o.Verbose then <<"-- contracting exceptional line(s) on the normalization"<<endl;
    h := mapDefinedByDivisor(source n,{(random(1,0_(source n)),1)}|apply(pts,p -> (n' pr'^* p,1)));
    if dim ambient target h != dim ambient source h + (dim ambient X - 5) then error "something went wrong";
    N := pr * n' * h;
    X.cache#"ExperimentalNormalizationInv" = N
);
*-

MinimumCodimensionForExperimentalNormalization = 7; -- 5;
inverseNormalizationMapRaw = (Y,strY,expEulOut,verb) -> (
    if codim Y >= MinimumCodimensionForExperimentalNormalization then return inverseNormalizationMapExperimentalRaw(Y,strY,expEulOut,verb);
    -- (
    --     try return inverseNormalizationMapExperimentalRaw(Y,strY,expEulOut,verb);
    --     if verb then << "-- experimental normalization failed; falling back to standard method..." << endl;
    -- );
    if verb then << "-- computing normalization of " << strY << "..." << endl << flush;
    normU := multirationalMap normalization(Y,Verbose=>false);
    X := source normU;
    if instance(normU,WeightedRationalMap) then (
        if verb then << "-- warning: normalization of " << strY << " has unsupported type: " << class X << endl;
        if verb then << "--          trying to use a virtual map to bypass this limitation."<<endl;
        Psi := super virtualInverseWeightedRationalMap normU;
        if verb then <<"-- result: virtual map from " << strY << " to " << ? image Psi << endl << flush;
        return Psi;
    );
    if verb then << "-- inverting normalization map..." << endl << flush;
    Phi := super inverse3 normU;
    if Phi#"image" === null then ( -- forceImage --
        f := toRationalMap Phi;
        assert(multirationalMap f === Phi);
        if f#"idealImage" === null then forceImage(f,sub(ideal X,target f));
        multirationalMap f;
        if not(Phi#"image" =!= null and Phi#"image" == X) then error "internal error encountered";
        Phi#"image" = X;
    );
    if verb then (
        << "-- result: map from " << strY << " to " << ? image Phi << endl << flush;
        if not instance(image Phi,WeightedProjectiveVariety) then (
            if euler hilbertPolynomial image Phi != expEulOut then << "-- warning: normalization has unexpected invariants" << endl;
        );
    );
    return Phi;
);

inverseNormalizationMapExperimentalRaw = (Y,strY,expEulOut,verb) -> (
    -- A variant and improvement of experimentalNormalizationInv.
    if not (dim Y == 2 and dim linearSpan Y > 5) then error "experimental normalization requires a surface with linear span dimension > 5";
    if verb then << "-- computing experimental normalization of " << strY << "..." << endl;
    pts := apply(dim ambient Y - 5, i -> point Y);
    pr := rationalMap((linearSpan pts)_Y, Dominant=>true);
    if degree target pr != degree Y - (dim ambient Y - 5) then error "something went wrong";
    if verb then << "  -- computing normalization of " << (? target pr) << endl << flush;
    n := multirationalMap normalization(target pr, Verbose=>false);
    if verb then << "  -- inverting normalization map..." << endl << flush;
    local n';
    if instance(n,WeightedRationalMap) then (
        if verb then << "  -- warning: normalization of the surface has unsupported type: " << class source n << endl;
        if verb then << "  --          trying to use a virtual map to bypass this limitation."<<endl;
        n' = super virtualInverseWeightedRationalMap n;
        if verb then <<"  -- result: virtual map to " << ? image n' << endl << flush;
    ) else (
        n' = inverse3 n;
    );
    if verb then << "  -- inverting projection..." << endl;
    pr' := inverse3 pr;
    if verb then << "  -- contracting exceptional lines on the normalization" << endl;
    h := mapDefinedByDivisor(source n,{(random(1,0_(source n)),1)}|apply(pts,p -> (n' pr'^* p,1)));
    if dim ambient target h != dim ambient source h + (dim ambient Y - 5) then << "  -- unexpected behavior: expected map to PP^" << dim ambient source h + (dim ambient Y - 5) << " but obtained map to PP^" << dim ambient target h << endl;
    if verb then << "  -- computing image of map to PP^" << dim target h << endl;
    if instance(h,WeightedRationalMap) then image h else image(h,"F4");
    if verb then << "  -- composing maps" << endl;
    N := (pr * n') * h;
    if verb then (
        << "-- experimental normalization complete; result: map from " << strY << " to " << ? image N << endl << flush;
        if not instance(image N,WeightedProjectiveVariety) then (
            if euler hilbertPolynomial image N != expEulOut then << "-- warning: normalization has unexpected invariants" << endl;
        );
    );
    N
);

interpolateImage = method(Options => {Verbose => false, cache => true});
interpolateImage (MultirationalMap,List,ZZ) := o -> (Phi,D,j) -> (
    if Phi#"image" =!= null then return Phi#"image";
    if Phi#"isDominant" === true then return target Phi;
    if not all(D,d -> instance(d,ZZ)) then error "expected a list of integers";
    doPrint := (v,c) -> (
        if not v then return false;
        if c <= 5 then return true;
        if c <= 15 then return c % 3 == 0;
        if c <= 50 then return c % 10 == 0;
        if c <= 150 then return c % 30 == 0;
        c % 100 == 0
    );
    cont := 0;
    W := Phi point source Phi;
    while select(flatten degrees ideal W,d -> d <= j) =!= D do (
        if doPrint(o.Verbose,cont) then <<"  -- image "<<cont<<", ";
        W = W + Phi point source Phi;
        if doPrint(o.Verbose,cont) then (<<"degrees: "; <<toStringDegreesVar W<<endl);
        cont = cont + 1;
    );
    for i to 3 do (
        if o.Verbose then <<"  -- (verifying) image "<<cont<<", ";
        W = W + Phi point source Phi;
        if o.Verbose then (<<"degrees: "; <<toStringDegreesVar W<<endl);
        cont = cont + 1;
    );
    W = projectiveVariety(ideal select(flatten entries gens ideal W,w -> first degree w <= j),MinimalGenerators=>true,Saturate=>false);
    if flatten degrees ideal W =!= D then error "something went wrong: the degrees of the generators are wrong";
    if o.cache then (
        if Phi#"image" === null then Phi#"image" = W;
        Phi#"isDominant" = Phi#"image" == target Phi;
        if Phi#"isDominant" then Phi#"image" = target Phi;
        return Phi#"image";
    ) else return W;
);
interpolateImage (RationalMap,List,ZZ) := o -> (Phi,D,j) -> (
    if Phi#"idealImage" =!= null then return Phi#"idealImage";
    W := interpolateImage(multirationalMap Phi,D,j,Verbose=>o.Verbose,cache=>false);
    if o.cache then forceImage(Phi,ideal W);
    ideal W
);

interpolateTop = method(Options => {Verbose => false, cache => true, "Deep" => 3});
interpolateTop (EmbeddedProjectiveVariety,List) := o -> (X,j) -> (
    if X.cache#?"top" then return X.cache#"top";
    assert(#j == 1 and instance(first j,ZZ));
    j = first j;
    cont := 0; m := o#"Deep"; local f;
    W := 0_X;
    D := toList(1..m);
    while # unique take(D,-m) != 1 do (
        cont = cont + 1;
        if o.Verbose then <<"  -- top "<<cont<<", ";
        f = parametrize random({(dim X):{1}},0_X);
        W = W + f f^* X;
        if o.Verbose then (<<"degrees: "; <<toStringDegreesVar W<<endl);
        D = append(D,select(flatten degrees ideal W,d -> d <= j));
    );
    gW := select(flatten entries gens ideal W,w -> first degree w <= j);
    if # gW > 0 then (
        W = projectiveVariety(ideal gW,MinimalGenerators=>true,Saturate=>false);
        if o.cache then X.cache#"top" = W;
        return W;
    ) else return ambient W;
);
interpolateTop (ZZ,EmbeddedProjectiveVariety) := o -> (i,X) -> (
    W := 0_X;
    while not (dim W == dim X and degree W == degree X) do (
        i = i + 1;
        W = interpolateTop(X,{i},Verbose=>o.Verbose,cache=>false,"Deep"=>o#"Deep");
    );
    if o.cache then X.cache#"top" = W else W
);
interpolateTop EmbeddedProjectiveVariety := o -> X -> interpolateTop(0,X,Verbose=>o.Verbose,cache=>o.cache,"Deep"=>o#"Deep");

DefaultVerbosityInterpolateTop = false;
verbosityInterpolateTop = v -> v and DefaultVerbosityInterpolateTop;

mapDefinedByDivisor = method();
mapDefinedByDivisor (QuotientRing,VisibleList) := (R,D) -> rationalMap(R,new Tally from apply(select(D,l -> last l > 0),d -> first d => last d));
mapDefinedByDivisor (MultiprojectiveVariety,VisibleList) := (X,D) -> rationalMap(X,new Tally from apply(select(D,l -> last l > 0),d -> first d => last d));

-- sufficient conditions for smoothness ('Y' is assumed to be equidimensional)
isSmooth EmbeddedProjectiveVariety := {} >> o -> (cacheValue "isSmooth") (Y -> (
    if Y.cache#?"singularLocus" or Y.cache#?"nonSaturatedSingularLocus" then return (dim singLocus Y == -1);
    X := fitVariety Y;
    isXsm := dim singLocus X == -1;
    if isXsm then Y.cache#"singularLocus" = 0_Y;
    isXsm
));

singLocus = method();
singLocus EmbeddedProjectiveVariety := X -> singularLocus(X,Saturate=>false);

numberNodes = method(Options => {Verbose => false});
numberNodes EmbeddedProjectiveVariety := o -> Y -> (
    if Y.cache#?"FiniteNumberOfNodes" then return Y.cache#"FiniteNumberOfNodes";
    X := if not(Y.cache#?"singularLocus" or Y.cache#?"nonSaturatedSingularLocus") then fitVariety Y else Y;
    if dim singLocus X >= 1 then error "expected at most a finite number of nodes";
    n := if dim singLocus X == -1 then 0 else (
             if (singLocus X).cache#?"Support" then
                 degree support singLocus X
             else
                 degree support (rationalMap random({{1},{1}},0_X)) singLocus X
             );
    if o.Verbose then <<"-- calculated number of nodes (got "<< n <<" nodes)"<<endl;
    Y.cache#"FiniteNumberOfNodes" = n
);

fitVariety = method();
fitVariety EmbeddedProjectiveVariety := (cacheValue "fitVariety") (X -> (
    if coefficientRing X === QQ then X = X ** (ZZ/nextPrime random(1000,11000000));
    if codim linearSpan X > 0 then X = (parametrize linearSpan X)^^ X;
    n := dim ambient X; k := dim X;
    if k > 0 and k <= 2 and 2*k+1 < n then (
        pr := rationalMap linearSpan apply(n-(2*k+1),i -> point X);
        if dim target pr != 2*k+1 then error "internal error encountered";
        X = pr X;
    );
    return X;
));

isomorphicProjectionOfSurfaceInP5 = method();
isomorphicProjectionOfSurfaceInP5 EmbeddedProjectiveVariety := X -> (
    if dim X != 2 then error "expected a surface";
    if codim linearSpan X > 0 then X = (parametrize linearSpan X)^^ X;
    if dim ambient X <= 5 then return X;
    pr := rationalMap apply(6,i -> random(1,ring ambient X));
    Var pr (ideal X)
);

varietyDefinedBylinearSyzygies = method();
varietyDefinedBylinearSyzygies EmbeddedProjectiveVariety := (cacheValue "varietyDefinedBylinearSyzygies") (Y -> (
    G := transpose syz gens ideal Y;
    M := matrix select(entries G,g -> max flatten degrees ideal g == 1);
    K := mingens kernel M;
    I := unique apply(entries transpose K,g -> trim ideal g);
    Var first select(I,i -> dim i >= 1)
));

mapY5 = memoize (K -> (
    X := GG(K,1,4);
    h := (parametrize projectiveVariety ideal sum gens ring ambient X)||X;
    -- assert(dim singLocus source h == -1);
    h
));
Y5 = K -> source mapY5 K;

mapY4 = memoize (K -> (
    y := gens ring ambient source mapY5 K;
    h := (parametrize projectiveVariety ideal(y_0-y_1+y_2-y_3+y_4-y_5+y_6))||(Y5 K);
    h = h * (mapY5 K);
    -- assert(dim singLocus source h == -1);
    h
));
Y4 = K -> source mapY4 K;

Var = method(Options => {MinimalGenerators => false});
Var Ideal := o -> I -> projectiveVariety(I,MinimalGenerators=>o.MinimalGenerators,Saturate=>false);
Var Ring := o -> R -> projectiveVariety(R,MinimalGenerators=>o.MinimalGenerators,Saturate=>false);
Var Matrix := o -> M -> projectiveVariety(M,MinimalGenerators=>o.MinimalGenerators,Saturate=>false);

isStandardK3surface = method();
isStandardK3surface EmbeddedProjectiveVariety := (cacheValue "is standard K3 surface") (
    S -> (
        if dim S != 2 then return false;
        g := sectionalGenus S;
        if g != dim ambient S then return false;
        if degree S != 2*g-2 then return false;
        if euler hilbertPolynomial S != 2 then return false;
        if dim linearSpan S < dim ambient S then return false;
        if g >= 6 and degrees S =!= {({2},binomial(g-2,2))} then << "-- notice: ideal generators deviate from expected quadrics for a K3 of genus " << g << endl;
        return true;
    )
);

isPlaneInP5 = P -> degrees P === {({1}, 3)} and codim P == 3 and dim P == 2;

isPresumedRationalNormalCurve = method();
isPresumedRationalNormalCurve EmbeddedProjectiveVariety := C -> (
    degs := flatten degrees ideal C;
    if max degs > 2 then return false;
    d1 := number(degs, i -> i == 1);
    d2 := number(degs, i -> i == 2);
    n := (dim ambient C) - d1;
    if d2 != binomial(n,2) then return false;
    if dim C != 1 then return false;
    if degree C != n then return false;
    if sectionalGenus C != 0 then return false;
    if n <= 3 then return isSmooth C;
    true
);

commonNameRationalNormalCurve = method();
commonNameRationalNormalCurve EmbeddedProjectiveVariety := C -> (
    if not isPresumedRationalNormalCurve C then error "expected a rational normal curve";
    d := degree C;
    if d == 1 then return "a line";
    if d == 2 then return "an irreducible conic curve";
    if d == 3 then return "a twisted cubic curve";
    "a rational normal curve of degree " | (toString d)
);

surfaceDescription = method();
surfaceDescription (ZZ,EmbeddedProjectiveVariety,Boolean) := (m,S,withNodes) -> (
    if dim S != 2 then error "expected a surface";
    degs := flatten degrees ideal S;
    if degree S == 1 and unique degs === {1} then return "plane";
    n := if withNodes or S.cache#?"FiniteNumberOfNodes" then numberNodes(S,Verbose=>false) else -1;
    nodal := if n > 0 then (toString n)|"-nodal " else (if S.cache#?"singularLocus" and dim(singularLocus S) == -1 then "smooth " else "");
    rational := if S.cache#?"rationalParametrization" then "rational " else "";
    surfaceHeader := nodal | rational | "surface of degree " | (toString degree S) | " and sectional genus " | (toString sectionalGenus S);
    cutOutLine := "cut out by "|(if same degs then (toString(#degs)|" hypersurfaces of degree "|(toString first degs)) else (toString(#degs)|" hypersurfaces of degrees "|toStringDegreesVar(S)));
    sp := if m < 0 then " " else (newline|concatenate(m:" "));
    surfaceHeader|sp|cutOutLine
);
surfaceDescription (ZZ,EmbeddedProjectiveVariety) := (m,S) -> surfaceDescription(m,S,S.cache#?"FiniteNumberOfNodes" or S.cache#?"singularLocus" or S.cache#?"nonSaturatedSingularLocus" or (S.cache#?"fitVariety" and (S.cache#"fitVariety").cache#?"nonSaturatedSingularLocus"));
surfaceDescription EmbeddedProjectiveVariety := S -> surfaceDescription(-1,S);

degreeOfDefiningForms = method();
degreeOfDefiningForms MultirationalMap := f -> (
    u := unique degrees ideal compress matrix f;
    if #u != 1 then error "internal error encountered";
    unsequence toSequence first u
);

humanReadableSeconds = t -> (
    assert(instance(t,ZZ) and t >= 0);
    d := t // 86400;
    r := t % 86400;
    h := r // 3600;
    r = r % 3600;
    m := r // 60;
    s := r % 60;
    units := {};
    if d > 0 then units = append(units, toString d | " day" | (if d == 1 then "" else "s"));
    if h > 0 then units = append(units, toString h | " hour" | (if h == 1 then "" else "s"));
    if m > 0 then units = append(units, toString m | " minute" | (if m == 1 then "" else "s"));
    if s > 0 or #units == 0 then units = append(units, toString s | " second" | (if s == 1 then "" else "s"));
    n := #units;
    if n == 1 then units#0
    else if n == 2 then units#0 | " and " | units#1
    else (
        out := units#0;
        for i from 1 to n-2 do out = out | ", " | units#i;
        out | ", and " | units#(n-1)
    )
);

computationStatus = method();
-- -1: Fano map not found
--  0: surface U missing from cache: no computation performed
--  1: Fano map and U computed; exceptional curves missing
--  2: exceptional curves computed; associated K3/Castelnuovo surface missing
--  3: surface Utilde computed; lattice polarization missing
--  4: computation complete
computationStatus DoublySpecialCubicFourfold := X -> (
    (S,P) := surfaces X;
    if not S.cache#?("FanoMapDSCF",P) then return -1;
    mu := S.cache#("FanoMapDSCF",P);
    if not mu.cache#?("surfaceDeterminingInverseOfFanoMap",X) then return 0;
    U := mu.cache#("surfaceDeterminingInverseOfFanoMap",X);
    if not U.cache#?"exceptionalCurves" then return 1;
    if not(mu.cache#?("AssociatedUnderlyingK3",X) and mu.cache#?("K3SurfaceFromDoublySpecialCubicFourfold",X)) then return 2;
    E := mu.cache#("K3SurfaceFromDoublySpecialCubicFourfold",X);
    if E#"LatticePolarization" === null or isVirtualLatticeK3 E then return 3;
    return 4;
);
computationStatus HodgeSpecialFourfold := X -> (
    S := surface X;
    if not S.cache#?("fanoMap",ambientFivefold X) then return -1;
    if not S.cache#?("surfaceDeterminingInverseOfFanoMap",ideal X) then return 0;
    U := S.cache#("surfaceDeterminingInverseOfFanoMap",ideal X);
    if not U.cache#?"exceptionalCurves" then return 1;
    if not X.cache#?"AssociatedSurfaceCompleteData" then return 2;
    return 3;
);
computationStatus K3SurfaceFromDoublySpecialCubicFourfold := S -> computationStatus recoverFourfold S;

computationStatusLog = method();
computationStatusLog HodgeSpecialFourfold := X -> (
    s := computationStatus X;
    if s >= 0 and (not instance(X,DoublySpecialCubicFourfold)) then s = s + 1;
    st := if      s == -1 then "░░░░░"
          else if s == 0  then "▓░░░░"
          else if s == 1  then "▓▓░░░"
          else if s == 2  then "▓▓▓░░"
          else if s == 3  then "▓▓▓▓░"
          else if s == 4  then "▓▓▓▓▓"
          else error "internal error: computationStatus returned an invalid value";
    (if instance(X,IntersectionOfThreeQuadricsInP7) then "Castelnuovo" else "K3") | " status: " | "["|st|" / ▓▓▓▓▓]"
);
computationStatusLog K3SurfaceFromDoublySpecialCubicFourfold := S -> computationStatusLog recoverFourfold S;

describeMirrorFourfoldAndK3 = method();
describeMirrorFourfoldAndK3 HodgeSpecialFourfold := X -> (
    s := computationStatus X;
    if s <= 0 then return "";
    mu := if instance(X,DoublySpecialCubicFourfold) then fanoMapDSCF(X,Verbose=>true) else multirationalMap fanoMap(X,Verbose=>true);  -- already in cache
    W := target mu;
    U := surfaceDeterminingInverseOfFanoMap(X,Verbose=>true);  -- already in cache
    cutOutLine := Y -> (
        degs := flatten degrees ideal Y;
        " cut out by "|(if same degs then (toString(#degs)|" hypersurfaces of degree "|(toString first degs)) else (toString(#degs)|" hypersurfaces of degrees "|toStringDegreesVar(Y)))
    );
    strW := if codim W == 0
            then "PP^4"
            else if codim W == 1
            then ("hypersurface in PP^5 of degree " | (toString degree W))
            else if codim W == numgens ideal W and degree W == product flatten degrees ideal W
            then ("complete intersection of type " | (toString toSequence flatten degrees ideal W) | " in PP^" | (toString dim ambient W))
            else if isDeformationP2P2 W
            then "≋ ℙ² × ℙ² ⊂ ℙ⁸"
            else if isDeformationDP5 W
            then "≋ 𝔾(1,4) ∩ ℙ⁷ ⊂ ℙ⁷"
            else (("fourfold of degree " | (toString degree W) | " and sectional genus " | (toString sectionalGenus W) | " in PP^" | (toString dim ambient W)) | (cutOutLine W));
    descr := "Mirror fourfold: " | strW | newline | "Surface U of degree " | (toString degree U) | ", sectional genus " | (toString sectionalGenus U) | ", χ(O_U) = " | (toString euler hilbertPolynomial U) | "," | (cutOutLine U);
    if s <= 1 then return descr;
    (L,C) := exceptionalCurves(X,Verbose=>false);  -- already in cache
    descr = descr | newline | printInfoOnExceptionalCurves(L,C);
    if s <= 2 then return descr;
    local Utilde;
    if instance(X,DoublySpecialCubicFourfold) then (
        Utilde = mu.cache#("AssociatedUnderlyingK3",X);
    ) else if instance(X,CubicFourfold) or instance(X,GushelMukaiFourfold) or instance(X,IntersectionOfThreeQuadricsInP7) then (
        Utilde = X.cache#"AssociatedSurfaceCompleteData";
    ) else error "unsupported fourfold type";
    strSurf := if instance(X,IntersectionOfThreeQuadricsInP7) then "Castelnuovo" else "K3";
    descr = descr | newline | "Minimal " | strSurf | " surface Ũ: ";
    if instance(Utilde,EmbeddedProjectiveVariety) then (
        descr = descr | "degree " | (toString degree Utilde) | " and sectional genus " | (toString sectionalGenus Utilde) | " in " | toString(? ambient Utilde) | (cutOutLine Utilde);
    ) else (
       descr = descr | toString(? Utilde);
    );
    if s <= 3 then return descr;
    pUtilde := polarizedK3surface(X,Verbose=>true);  -- already in cache
    descr || ((net "Lattice intersection matrix on Ũ: ") | (net latticeMatrix pUtilde))
);

isDeformationP2P2 = X -> (
    if degrees X =!= {({2}, 9)} or dim X != 4 or codim X != 4 or degree X != 6 then return false;
    p := hilbertPolynomial(X,Projective=>false);
    i := first gens ring p;
    p == (1/4)*i^4+(3/2)*i^3+(13/4)*i^2+3*i+1
);

isDeformationDP5 = X -> (
    if degrees X =!= {({2}, 5)} or dim X != 4 or codim X != 3 or degree X != 5 then return false;
    p := hilbertPolynomial(X,Projective=>false);
    i := first gens ring p;
    p == (5/24)*i^4+(5/4)*i^3+(67/24)*i^2+(11/4)*i+1
);

printInfoOnExceptionalCurves = (L,C) -> (
    if dim L == -1 and dim C == -1 then return "No exceptional curves";
    if dim L == -1 and dim C != -1 then (
        if isPresumedRationalNormalCurve C then return "Exceptional curves: " | (commonNameRationalNormalCurve C);
        return "Exceptional curves: only curves of degree > 1";
    );
    if dim L != -1 and dim C == -1 then (
        if degree L == 1 then return "Exceptional curves: 1 line";
        return ("Exceptional curves: " | (toString degree L) | " lines");
    );
    assert(dim L != -1 and dim C != -1);
    if isPresumedRationalNormalCurve C then (
        if degree L == 1 then return "Exceptional curves: 1 line and " | (commonNameRationalNormalCurve C);
        return ("Exceptional curves: " | (toString degree L) | " lines and " | (commonNameRationalNormalCurve C));
    );
    if degree L == 1 then return "Exceptional curves: 1 line and other curves of degree > 1";
    "Exceptional curves: " | (toString degree L) | " lines and other curves of degree > 1"
);
