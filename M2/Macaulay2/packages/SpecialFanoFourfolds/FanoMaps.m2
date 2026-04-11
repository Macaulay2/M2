
------------------------------------------------------------------------
------------------------------ Fano maps -------------------------------
------------------------------------------------------------------------

fanoMap = method(Options => {Singular => null, RaiseError => true, Verbose => false});

fanoMap HodgeSpecialFourfold := o -> X -> (
    if (surface X).cache#?("fanoMap",ambientFivefold X) then return (surface X).cache#("fanoMap",ambientFivefold X);
    if (surface X).cache#?("fanoMap",ambient X) and dim ambient X >= 5 then (
        Mu := (surface X).cache#("fanoMap",ambient X);
        mu := toRationalMap(Mu|(ambientFivefold X));
        (mu#"map").cache#"multiplicityFanoMap" = (Mu#"map").cache#"multiplicityFanoMap";
        if mu#"idealImage" === null then forceImage(mu,ideal(0_(target mu))); --
        (surface X).cache#("fanoMap",ambientFivefold X) = mu;
        return mu
    );
    local e; sat := 1;
    if member(recognize X,{"quinticDelPezzoSurface","quarticScrollSurface",1,6,"planeInPP7"})
    then e = 1
    else if member(recognize X,{"C38Coble","FarkasVerra","oneNodalSepticDelPezzoSurfaceC26",17,18,"october2021-26''"})
    then e = 2
    else if member(recognize X,{"C42","6NodalOcticSrollC38",3,"october2021-34'","mukai26''","october2021-20","surf-5-7-0-1"})
    then e = 3
    else if member(recognize X,{"surf-5-10-1","surf-7-1-9"})
    then e = 4
    else if member(recognize X,{"gushel26''","internal-projection-K3-genus-8", "hyperplane section of a conic bundle over PP2", "surf-4-3-1-external"})
    then e = 5
    else if member(recognize X,{"surf-5-6-2-nodal"})
    then e = 6
    else e = (detectCongruence(X,Verbose=>o.Verbose))#"degree";
    if o.Verbose then <<"-- detected degree of the curves of the congruence: "<<e<<endl;
    if member(recognize X,{"october2021-34'","gushel26''","surf-7-1-9"}) then sat = 2;
    fanoMap(X,e,sat,Singular=>o.Singular,RaiseError=>o.RaiseError,Verbose=>o.Verbose)
);

fanoMap (HodgeSpecialFourfold,ZZ) := o -> (X,e) -> (
    sat := 1;
    if member(recognize X,{"october2021-34'","gushel26''","surf-7-1-9"}) then sat = 2;
    fanoMap(X,e,sat,Singular=>o.Singular,RaiseError=>o.RaiseError,Verbose=>o.Verbose)
);

imageOfFanoMap = method(Options => {RaiseError => true, Verbose => false});
imageOfFanoMap (HodgeSpecialFourfold,RationalMap) := o -> (X,mu) -> (
    M := mu#"dimAmbientSource";
    if M =!= mu#"dimSource" then error "expected target of map to be a projective space";
    if M <= 3 then error "Fano map not valid";
    if mu#"idealImage" =!= null then return image mu;
    if o.Verbose then <<"-- computing/forcing image of map to PP^"<<M<<endl;
    if member(recognize X,{"quarticScrollSurface", 6})
    then forceImage(mu,image(2,mu))
    else if recognize X === "october2021-26''"
    then interpolateImage(mu,{2},2,Verbose=>o.Verbose)
    else if member(recognize X,{3, "internal-projection-K3-genus-8", "october2021-34'"})
    then interpolateImage(mu,{3},3,Verbose=>o.Verbose)
    else if recognize X === "hyperplane section of a conic bundle over PP2"
    then interpolateImage(mu,{2,2,2},2,Verbose=>o.Verbose)
    else if M == 4
    then forceImage(mu,ideal(0_(target mu)))
    else if M == 5
    then (try interpolateImage(mu,{2},2,Verbose=>o.Verbose) else try interpolateImage(mu,{3},3,Verbose=>o.Verbose))
    else if M == 6
    then (try interpolateImage(mu,{2,2},2,Verbose=>o.Verbose))
    else if M == 7
    then (try interpolateImage(mu,{2,2,2,2,2},2,Verbose=>o.Verbose) else try interpolateImage(mu,{2,2,2},2,Verbose=>o.Verbose))
    else if member(M,{8,9,10,11,12})
    then (try interpolateImage(mu,toList(binomial(M-4,2) : 2),2,Verbose=>o.Verbose));
    if mu#"idealImage" =!= null then (
        if dim image mu != 5 then error "something went wrong; the image of the Fano map is not of dimension 4";
        return image mu;
    );
    if o.RaiseError then error "not implemented yet: image of Fano map for unrecognized fourfold";
);

fanoMap (HodgeSpecialFourfold,ZZ,ZZ) := o -> (X,e,sat) -> (
    if (surface X).cache#?("fanoMap",ambientFivefold X) then return (surface X).cache#("fanoMap",ambientFivefold X);
    P := o.Singular;
    if P === null then (
        P = findProgram("Singular","Singular --help",RaiseError=>false) =!= null;
        if o.Verbose then (
            displayVar := getenv "DISPLAY";
            hasX11 := displayVar =!= null and displayVar =!= "";
            P = P and hasX11 and findProgram("xterm","xterm -version",RaiseError=>false) =!= null;
        );
    );
    if not instance(P,Boolean) then error "option Singular expects true or false";
    if e == 1 then sat = 0;
    local mu;
    if P then (
        mu = fanoMapUsingSingular(X,e,sat,Verbose=>o.Verbose);
    ) else (
        S := idealOfSubvariety((surface X)%(ambientFivefold X));
        a := degreeHypersurface X;
        if o.Verbose then <<"-- computing power of ideal"<<endl;
        I := trim(S^e);
        for i to sat-1 do (
            if o.Verbose then <<"-- computing colon ideal"<<endl;
            I = I : ideal first gens ring I;
        );
        mu = rationalMap gens trim ideal select(flatten entries gens I,i -> degree i <= {a*e-1});
    );
    W := imageOfFanoMap(X,mu,RaiseError=>o.RaiseError,Verbose=>o.Verbose);
    if W =!= null then (
        mu = rationalMap(mu,Dominant=>true);
        (mu#"map").cache#"multiplicityFanoMap" = e;
        (surface X).cache#("fanoMap",ambientFivefold X) = mu;
    ) else (
        <<"--error: not implemented yet: image of Fano map for unrecognized fourfold"<<endl<<"--returning non-dominant map"<<endl;
    );
    return mu;
);

fanoMapUsingSingular = method(Options => {Verbose => false});
fanoMapUsingSingular (HodgeSpecialFourfold,ZZ,ZZ) := o -> (X,e,sat) -> (
    Singular := findProgram("Singular", "Singular --help");
    if o.Verbose then XTermSingular := findProgram("xterm", "xterm -version"); -- using xterm we see Singular messages instantly and not at the end of the calculation
    V := ambientFivefold X;
    S := surface X;
    x := local x;
    K := coefficientRing X;
    if not (if char K == 0 then K === QQ else K === ZZ/(char K)) then error "expected coefficient ring of the form ZZ/p or QQ";
    R := Grass(0,dim ambient X,K,Variable=>x);
    idV := sub(ideal V,vars R);
    R = R/idV;
    I := sub(idealOfSubvariety(S%V),vars R);
    dir := temporaryFileName() | "/";
    mkdir dir;
    if o.Verbose then <<"-- writing Singular code on file: "<<(dir|"input.singular")<<endl;
    F := openOut(dir|"input.singular");
    F << "// Singular code " << endl;
    F << "// Singular -qc 'execute(read(\"input.singular\"));'" << endl;
    F << "// " << endl;
    if o.Verbose then F << "print(\"Singular: starting computation, input file: "<< (dir|"input.singular")<<"\");"<<endl;
    F << "ring R = "<< char K << "," << toString unsequence toSequence gens R << ",dp;" << endl;
    F << "short = 0;" << endl;
    if idV != 0 then (
        F << "ideal idV = " << toString unsequence toSequence flatten entries gens idV << ";" << endl;
        F << "qring qR = std(idV);" << endl;
    );
    F << "ideal I = " << toString unsequence toSequence flatten entries gens I << ";" << endl;
    if o.Verbose then F << "print(\"Singular: computing power of ideal\");"<<endl;
    F << "ideal Ie = minbase(I^"<<e<< ");" << endl;
    F << "ideal J = x_0;" << endl;
    F << "ideal Q = Ie;" << endl;
    for i to sat-1 do (
        if o.Verbose then F << "print(\"Singular: computing colon ideal\");"<<endl;
        F << "Q = quotient(Q,J);" << endl;
    );
    F << "int m = size(Q);" << endl;
    F << "ideal E;" << endl;
    F << "for (int i=m; i>=1; i=i-1) {if (deg(Q[i]) <= "<<degreeHypersurface(X)*e-1<< ") {E = E,Q[i];}};" << endl;
    F << "write(\":w output.singular\",\"OUTPUTVALUE = ideal(\",E,\");\");" << endl;
    if o.Verbose then F << "print(\"Singular: computation ended\");"<<endl;
    F << "exit;" << endl;
    close F;
    if o.Verbose then <<"-- running Singular"<<endl;
    if o.Verbose
    then runProgram(XTermSingular,"-e Singular -qc 'execute(read(\"input.singular\"));'",RunDirectory=>dir,RaiseError=>true,Verbose=>o.Verbose,KeepFiles=>false)
    else runProgram(Singular,"-qc 'execute(read(\"input.singular\"));'",RunDirectory=>dir,RaiseError=>true,Verbose=>o.Verbose,KeepFiles=>false);
    Out := "x := local x; x = gens Grass(0,"|toString(dim ambient X)|","|(toExternalString K)|",Variable=>x);" | newline | get(dir|"output.singular") | newline | "OUTPUTVALUE" | newline;
    D := value Out;
    D = if ring D === ZZ then sub(D,ring V) else sub(D,vars ring V);
    rationalMap gens trim D
);

------------------------------------------------------------------------
------------------- Fano maps for D. S. C. F. --------------------------
------------------------------------------------------------------------

fanoMapDSCF = method(Options => {Verify => true, Verbose => true});
fanoMapDSCF DoublySpecialCubicFourfold := o -> X -> (
    (S,T) := surfaces X;
    if S.cache#?("FanoMapDSCF",T) then return S.cache#("FanoMapDSCF",T);
    if isFanoMapStandard X then return S.cache#("FanoMapDSCF",T) = fanoMapDSCFstandard(X,Verify=>o.Verify,Verbose=>o.Verbose);
    if isFanoMapToP2xP2 X then return S.cache#("FanoMapDSCF",T) = fanoMapDSCFtoP2xP2(X,Verify=>o.Verify,Verbose=>o.Verbose);
    -- error "no Fano map type detected";
    setFanoMapToBeStandard(X,Verbose=>o.Verbose);
    fanoMapDSCF(X,Verify=>o.Verify,Verbose=>o.Verbose)
);

fanoMapDSCFstandard = method(Options => {Verify => true, Verbose => true});
fanoMapDSCFstandard (DoublySpecialCubicFourfold,ZZ,ZZ,ZZ) := o -> (X,d,a,b) -> (
    (S,T) := surfaces X;
    if S.cache#?("FanoMapDSCFstandard",T,d,a,b) then return S.cache#("FanoMapDSCFstandard",T,d,a,b);
    mu := rationalMap(a*S + b*T, d);
    if o.Verbose then (
        << "-- computed map μ: PP^5 --> PP^" << (dim ambient target mu) << " defined by hypersurfaces of degree " << d << endl;
        << "-- with multiplicities " << a << " and " << b << " along the two surfaces" << endl;
    );
    if o.Verify then (
        if dim target mu <= 3 then (
            S.cache#("generic fiber verification fano map",T,d,a,b) = false;
            error "generic fiber verification failed: target dimension is too small for the expected map";
        );
        C := mu^* mu point source mu;
        e := degree C;
        CS := C * S;
        CT := C * T;
        if not(dim C == 1 and dim CS == 0 and dim CT == 0 and degree CS + degree CT == 3*e-1) then (
            S.cache#("generic fiber verification fano map",T,d,a,b) = false;
            error "generic fiber verification failed: expected a curve of degree e which is (3e-1)-secant the union of the two surfaces";
        );
        if o.Verbose then (
            if e == 1 and flatten degrees ideal C === toList(4:1) and isPoint CS and isPoint CT then (
                << "-- verified: generic fiber is a line connecting a point on each of the two surfaces" << endl;
            ) else (
                assert(e >= 1);
                << "-- verified: generic fiber is a curve of degree " << e << " intersecting the surfaces in " << degree CS << " + " << degree CT << " = " << (degree CS + degree CT) << " points" << endl;
            );
        );
        S.cache#("generic fiber verification fano map",T,d,a,b) = true;
    );
    S.cache#("FanoMapDSCFstandard",T,d,a,b) = mu
);

fanoMapDSCFstandard DoublySpecialCubicFourfold := o -> X -> (
    (S,T) := surfaces X;
    if S.cache#?("FanoMapDSCFstandard",T) then return S.cache#("FanoMapDSCFstandard",T);
    local mu;
    found := false;
    linSys := null;
    if isPlaneInP5 T then (
        quadricFibration X;
        assert X.cache#?"quadricFibrationCubicFourfoldInC8";
        if o.Verbose and (not X.cache#"quadricFibrationCubicFourfoldInC8"_1) then (
            <<"-- warning: fanoMap: possible infinite loop. " << X.cache#"quadricFibrationCubicFourfoldInC8"_2 << endl;
        );
        if X.cache#"quadricFibrationCubicFourfoldInC8"_1 then (
            for d from 2 do (
                if o.Verbose then << "-- fanoMap: attempting map μ with linear system of degree " << d << "..." << endl << flush;
                try mu = fanoMapDSCFstandard(X,d,1,d-1,Verify=>true,Verbose=>o.Verbose);
                if S.cache#("generic fiber verification fano map",T,d,1,d-1) then (
                    found = true;
                    linSys = (d,1,d-1);
                    if o.Verbose then (
                        << "-- success: valid map found at degree " << d << " with multiplicities (" << 1 << ", " << d-1 << ")" << endl << endl << flush;
                    );
                    break;
                );
            );
        );
    );
    if not found then (
        if o.Verbose and (isPlaneInP5 S) and (not isPlaneInP5 T) then (
            << "-- note: the first surface in the fourfold is a plane. fanoMap() performs better" << endl
            << "   if the plane is the second surface. Consider interchanging them" << endl
            << "   when constructing the fourfold." << endl;
        );
        if o.Verbose then (
            << "-----------------------------------------" << endl;
            << "-- fanoMap: brute-force search starting  " << endl;
            << "-----------------------------------------" << endl;
        );
        for d from 2 do (
            if o.Verbose then << "-- fanoMap: attempting map μ with linear system of degree " << d << "..." << endl << flush;
            for a from 1 to d-1 do (
                for b from 1 to d-1 do (
                    try mu = fanoMapDSCFstandard(X,d,a,b,Verify=>true,Verbose=>o.Verbose);
                    if S.cache#("generic fiber verification fano map",T,d,a,b) then (
                        found = true;
                        linSys = (d,a,b);
                        if o.Verbose then (
                            << "-- success: valid map found at degree " << d << " with multiplicities (" << a << ", " << b << ")" << endl << endl << flush;
                        );
                        break;
                    );
                );
                if found then break;
            );
            if found then break;
        );
    );
    targetDim := dim ambient target mu;
    if targetDim == 4 then forceImage(mu,target mu);
    local pDegs;
    if targetDim == 5 and mu#"image" === null then (
        pDegs = reverse projectiveDegrees mu;
        assert(pDegs_0 == 0 and pDegs_1 != 0);
        if o.Verbose then (
            << "-- projective degrees of μ: " << reverse pDegs << endl;
            << "  -- computing image of μ : PP^5 --> PP^" << targetDim << " using 'image(μ, " << pDegs_1 << ")'..." << endl;
        );
        forceImage(mu,image(mu,pDegs_1));
        if o.Verbose then << "-- image of μ: " << ? image mu << endl << endl << flush;
    );
    F4StrategyLimit := 7;
    if targetDim > 5 and mu#"image" === null then (
        if targetDim <= F4StrategyLimit then (
            if o.Verbose then << "-- computing image of μ : PP^5 --> PP^" << targetDim << " using 'F4' algorithm..." << endl;
            image(mu,"F4");
        ) else (
            if o.Verbose then (
                << "-- computing image of μ : PP^5 --> PP^" << targetDim << " using 'W = image(μ, 2)'..." << endl;
                << "  -- determining degree of the image via projective degrees..." << endl;
            );
            pDegs = reverse projectiveDegrees mu;
            assert(pDegs_0 == 0 and pDegs_1 != 0);
            if o.Verbose then << "  -- projective degrees: " << reverse pDegs << ", expected degree: " << pDegs_1 << endl;
            W := image(mu,2);
            if not (dim W == 4 and degree W == pDegs_1) then (
                if o.Verbose then << "-- 'image(μ, 2)' failed, recomputing with 'F4' algorithm..." << endl;
                image(mu,"F4");
            ) else (
                if o.Verbose then << "  -- degree confirmed: proceeding to force the image to W" << endl;
                forceImage(mu,W);
            );
        );
        if o.Verbose then << "-- image of μ: " << ? image mu << endl << endl << flush;
    );
    Mu := rationalMap(mu,Dominant=>true);
    Mu.cache#"FanoMapType" = "Standard";
    S.cache#("FanoMapDSCFstandard",T) = Mu;
    if linSys_1 == linSys_2 and linSys_0 == 3 * linSys_1 - 1 and (not (surface fuse X).cache#?("fanoMap",ambientFivefold fuse X)) then (
        mu2 := toRationalMap Mu;
        (mu2#"map").cache#"multiplicityFanoMap" = linSys_1;
        (surface fuse X).cache#("fanoMap",ambientFivefold fuse X) = mu2;
    );
    Mu
);

fanoMapDSCFtoP2xP2 = method(Options => {Verify => true, Verbose => true});
fanoMapDSCFtoP2xP2 DoublySpecialCubicFourfold := o -> X -> (
    (S,T) := surfaces X;
    if S.cache#?("FanoMapDSCFtoP2xP2",T) then return S.cache#("FanoMapDSCFtoP2xP2",T);
    if o.Verbose then << "-- constructing the map PP^5 --> PP^2 x PP^2 via abstract join" << endl << flush;
    (f,g) := abstractJoinOfRationalSurfaces(S,T,Verbose=>o.Verbose);
    if o.Verbose then << "-- computing the inverse of the second projection..." << endl;
    g' := inverse(g, Verify=>false);
    if o.Verify and g' * g != 1 then error "inverse verification failed";
    if o.Verbose then << "-- inverse of second projection computed" << endl;
    h := g' * f;
    if o.Verify then (
        if o.Verbose then << "-- verifying properties on composition PP^5 --> .. --> PP^2 x PP^2" << endl << flush;
        (a1,a2) := toSequence apply(projectionMaps h, degreeOfDefiningForms);
        (M1,M2) := toSequence apply(factor h,matrix);
        Z1 := projectiveVariety ideal det(M1 * matrix apply(numColumns M1, i -> {random coefficientRing h}));
        Z2 := projectiveVariety ideal det(M2 * matrix apply(numColumns M2, i -> {random coefficientRing h}));
        assert(degree Z1 == a1 and degree Z2 == a2);
        multHyp := (p,Z) -> (if not isSubset(p,Z) then return 0; degree tangentCone(p,Z));
        (z1,z2) := ((a1, multHyp(point S,Z1), multHyp(point T,Z1)), (a2, multHyp(point S,Z2), multHyp(point T,Z2)));
        p := point target h;
        Fibh := h^* p;
        if degree(h|X,Strategy=>"random point") == 1 and dim Fibh == 1 and degree Fibh == 1 and isPoint(Fibh * S) and isPoint(Fibh * T) then (
            if o.Verbose then (
                << "  -- info on the map: (1) deg " << z1_0 << ", mult. (" << z1_1 << "," << z1_2 << "); (2) deg " << z2_0 << ", mult. (" << z2_1 << "," << z2_2 << ")" << endl;
                << "  -- generic fiber is a line connecting a point on each of the two surfaces" << endl;
                << "  -- restriction to the cubic fourfold is birational" << endl << flush;
            );
            h#"isDominant" = true;
        ) else error "some verifications did not match the expected results";
    );
    s := rationalMap(segreEmbedding target h,Dominant=>true);
    mu := h * s;
    if o.Verify then assert(mu#"isDominant");
    mu.cache#"FactorsViaSegreEmbedding" = (h,s);
    mu.cache#"FanoMapType" = "FromP5toP2xP2";
    S.cache#("FanoMapDSCFtoP2xP2",T) = mu
);

abstractJoinOfRationalSurfaces = method(Options => {Verbose => true});
abstractJoinOfRationalSurfaces (EmbeddedProjectiveVariety,EmbeddedProjectiveVariety) := o -> (S,P) -> (
    -- Input: two rational surfaces in PP^n
    -- Output: the two projections from the abstract join: J --> PP^2xPP^2, J --> PP^n
    if S.cache#?("abstract join of rational surfaces",P) then return S.cache#("abstract join of rational surfaces",P);
    if o.Verbose then << "  -- computing abstract join of two rational surfaces..." << endl;
    if ring ideal P =!= ring ideal S then error "expected varieties in the same ambient projective space";
    if dim P != 2 or dim S != 2 then error "expected two surfaces";
    try (
        f := super parametrize P;
        assert(dim source f == 2 and codim source f == 0);
    ) else error("abstractJoin: failed to parametrize the surface: " | (? P));
    try (
        g := super parametrize S;
        assert(dim source g == 2 and codim source g == 0);
    ) else error("abstractJoin: failed to parametrize the surface: " | (? S));
    K := coefficientRing S;
    n := dim ambient S;
    (t,x,y,z) := (local t,local x,local y,local z);
    R := K[t_0,t_1, x_0..x_2, y_0..y_2, z_0..z_n, MonomialOrder=>Eliminate 2];
    F := (map(R,ring source f,{x_0..x_2})) matrix f;
    G := (map(R,ring source g,{y_0..y_2})) matrix g;
    J := ideal(matrix{{z_0..z_n}} - t_0*F - t_1*G);
    R' := K[x_0..x_2, y_0..y_2, z_0..z_n, Degrees=>{3:{1,0,0},3:{0,1,0},(n+1):{0,0,1}}];
    J' := sub(ideal selectInSubring(1, gens gb J), R');
    if o.Verbose then << "  -- abstract join computation almost complete" << endl;
    JJ := projectiveVariety J';
    if o.Verbose then << "  -- constructing the two projections: p1:..-->PP^2xPP^2 and p2:..-->PP^" << n << endl;
    -- psi := rationalMap(last projectionMaps JJ,target f); -- might cause bugs, forgot details
    psi := rationalMap(last projectionMaps JJ); psi = psi * rationalMap(target psi,target f);
    eta := rationalMap((projectionMaps JJ)_0 | (projectionMaps JJ)_1, PP_K^{2,2});
    S.cache#("abstract join of rational surfaces",P) = (eta,psi)
);

setFanoMapToBeMapFromP5toP2xP2 = method(Options => {Verbose => true});
setFanoMapToBeMapFromP5toP2xP2 DoublySpecialCubicFourfold := o -> X -> (
    if isFanoMapToP2xP2 X then return;
    (S,T) := surfaces X;
    if S.cache#?("FanoMapDSCF",T) then (
        if isFanoMapToP2xP2 S.cache#("FanoMapDSCF",T) then (
            X.cache#"FanoMapType" = "FromP5toP2xP2";
            return;
        );
        if o.Verbose then << "-- existing Fano map in cache: overwriting with map to PP^2 x PP^2..." << endl;
    );
    mu := fanoMapDSCFtoP2xP2(X,Verbose=>o.Verbose,Verify=>true);
    X.cache#"FanoMapType" = "FromP5toP2xP2";
    S.cache#("FanoMapDSCF",T) = mu;
);

setFanoMapToBeStandard = method(Options => {Verbose => true});
setFanoMapToBeStandard DoublySpecialCubicFourfold := o -> X -> (
    if isFanoMapStandard X then return;
    (S,T) := surfaces X;
    if S.cache#?("FanoMapDSCF",T) then (
        if isFanoMapStandard S.cache#("FanoMapDSCF",T) then (
            X.cache#"FanoMapType" = "Standard";
            return;
        );
        if o.Verbose then << "-- existing Fano map in cache: overwriting with standard map..." << endl;
    );
    mu := fanoMapDSCFstandard(X,Verbose=>o.Verbose,Verify=>true);
    X.cache#"FanoMapType" = "Standard";
    S.cache#("FanoMapDSCF",T) = mu;
);

isFanoMapStandard = E -> E.cache#?"FanoMapType" and E.cache#"FanoMapType" === "Standard";
isFanoMapToP2xP2 = E -> E.cache#?"FanoMapType" and E.cache#"FanoMapType" === "FromP5toP2xP2";

getCachedFanoMapIfCompatible = method();
getCachedFanoMapIfCompatible (DoublySpecialCubicFourfold,Thing) := (X,FanoMapTypeStr) -> (
    assert member(FanoMapTypeStr,{null,"Standard","P2xP2"});
    (S,T) := surfaces X;
    if not S.cache#?("FanoMapDSCF",T) then return;
    mu := S.cache#("FanoMapDSCF",T);
    if FanoMapTypeStr === null then (
        assert(X.cache#?"FanoMapType" and mu.cache#?"FanoMapType");
        assert(X.cache#"FanoMapType" === mu.cache#"FanoMapType");
        return mu;
    );
    if FanoMapTypeStr === "Standard" and isFanoMapStandard X then (
        assert isFanoMapStandard mu;
        return mu;
    );
    if FanoMapTypeStr === "P2xP2" and isFanoMapToP2xP2 X then (
        assert isFanoMapToP2xP2 mu;
        return mu;
    );
);

fanoMap DoublySpecialCubicFourfold := o -> X -> toRationalMap fanoMapDSCF(X,Verify=>o.RaiseError,Verbose=>o.Verbose); -- for compatibility
