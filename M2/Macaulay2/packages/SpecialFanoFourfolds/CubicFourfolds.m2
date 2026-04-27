
------------------------------------------------------------------------
--------------------------- Cubic fourfolds ----------------------------
------------------------------------------------------------------------

CubicFourfold = new Type of HodgeSpecialFourfold;

globalAssignment CubicFourfold;

CubicFourfold.synonym = "cubic fourfold";

cubicFourfold = method(TypicalValue => CubicFourfold, Options => {NumNodes => null, InputCheck => 1, Verbose => false});

cubicFourfold (EmbeddedProjectiveVariety,EmbeddedProjectiveVariety) := o -> (S,X) -> (
    if ring ideal S =!= ring ideal X then error "expected varieties in the same ambient space";
    if not (dim ambient X == 5 and degrees X == {({3},1)}) then error "expected a cubic fourfold";
    if dim S != 2 then error "expected a surface";
    i := o.InputCheck;
    if not(instance(i,ZZ) and i >= -1) then error("option InputCheck expects a nonnegative integer:"|newline|"0: no check is done about the smoothness of the fourfold and of the (normalization of the) surface"|newline|"1: just the smoothness of the fourfold is checked"|newline|"2: the smoothness of the fourfold and of a general hyperplane section of the surface are checked"|newline|"3: as above and furthermore the smoothness of the normalization of the surface is checked");
    if i >= 0 then if not isSubset(S,X) then error "the given surface is not contained in the cubic fourfold";
    if i >= 1 then if not isSmooth X then error "expected a smooth cubic fourfold";
    n := o.NumNodes;
    if n === null then n = numberNodes(S,Verbose=>o.Verbose);
    if not(instance(n,ZZ) and n >= 0) then error "option NumNodes expects a nonnegative integer or null";
    if S.cache#?"FiniteNumberOfNodes" then if n =!= S.cache#"FiniteNumberOfNodes" then error "the number of nodes is wrong";
    if i == 2 or (i >= 3 and n > 0) then (
        if not isSmooth(S * random(1,0_S)) then error "expected a surface with at most a finite number of nodes";
        if o.Verbose then <<"-- smoothness in codimension 1 of the surface verified"<<endl;
    );
    q := null;
    if i >= 3 then (
        if n > 0 then (
            q = normalization(S,Verbose=>o.Verbose);
            if not isSmooth(Var source q) then error "expected a surface with smooth normalization";
            if o.Verbose then <<"-- smoothness of the normalization of the surface verified (assuming equidimensionality)"<<endl;
            if n != numberNodes(S,Verbose=>o.Verbose) then error "the number of nodes is wrong";
            if o.Verbose then <<"-- number of nodes (partially) verified"<<endl;
        ) else (
            if not isSmooth S then error "expected a smooth surface (NumNodes=>0)";
            if o.Verbose then <<"-- smoothness of the surface verified (assuming equidimensionality)"<<endl;
        );
    );
    if i >= 4 then (
        if S != top S then error "expected an irreducible reduced surface";
        if o.Verbose then <<"-- equidimensionality of the surface verified"<<endl;
    );
    S.cache#"FiniteNumberOfNodes" = n;
    Fourfold := new CubicFourfold from X;
    if Fourfold#?"SurfaceContainedInTheFourfold" then Fourfold#"SurfaceContainedInTheFourfold" = prepend(S,Fourfold#"SurfaceContainedInTheFourfold") else Fourfold#"SurfaceContainedInTheFourfold" = {S};
    Fourfold
);

cubicFourfold (Ideal,Ideal) := o -> (idS,idX) -> cubicFourfold(projectiveVariety idS,projectiveVariety idX,NumNodes=>o.NumNodes,InputCheck=>o.InputCheck,Verbose=>o.Verbose);

cubicFourfold (Ideal,RingElement) := o -> (idS,C) -> cubicFourfold(idS,ideal C,NumNodes=>o.NumNodes,InputCheck=>o.InputCheck,Verbose=>o.Verbose);

cubicFourfold EmbeddedProjectiveVariety := o -> S -> (
    if dim ambient S == 5 and codim S == 1 and degrees S === {({3},1)} then return cubicFourfold(S * random({2:{1}},0_S),S,NumNodes=>o.NumNodes,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
    if not(dim ambient S == 5 and dim S == 2) then error "expected a surface in P^5";
    cubicFourfold(S,random({3},S),NumNodes=>o.NumNodes,InputCheck=>o.InputCheck,Verbose=>o.Verbose)
);

cubicFourfold Ideal := o -> idS -> cubicFourfold(projectiveVariety idS,NumNodes=>o.NumNodes,InputCheck=>o.InputCheck,Verbose=>o.Verbose);

cubicFourfold (String,Ring) := o -> (str,K) -> (
    if o.NumNodes =!= null then error "the option NumNodes is ignored, the number of nodes is determined automatically";
    local X;
    if str === "quintic del Pezzo surface" then (
        X = cubicFourfold(surface({3,4},K),NumNodes=>0,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        X.cache#(surface X,"label") = "quinticDelPezzoSurface";
        return X;
    );
    if str === "quartic scroll" then (
        X = cubicFourfold(surface({3,1,1},K),NumNodes=>0,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        X.cache#(surface X,"label") = "quarticScrollSurface";
        return X;
    );
    if str === "general cubic 4-fold of discriminant 38" or str === "C38" then (
        X = cubicFourfold(surface({10,0,0,10},K),NumNodes=>0,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        X.cache#(surface X,"label") = "C38Coble";
        return X;
    );
    if str === "6-nodal octic scroll" then (
        X = cubicFourfold("C38",K,InputCheck=>0,Verbose=>o.Verbose);
        X = cubicFourfold(((top baseLocus fanoMap X) * X)\surface X,X,NumNodes=>6,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        X.cache#(surface X,"label") = "6NodalOcticSrollC38";
        return X;
    );
    if str === "3-nodal septic scroll" or str === "Farkas-Verra C26" then (
        t := gens ring PP_K^2;
        f := multirationalMap rationalMap(ring PP_K^2,ring PP_K^8,{t_0^5, t_0^4*t_1, t_0^3*t_1^2, t_0^2*t_1^3, t_0^4*t_2, t_0^3*t_1*t_2, t_0^2*t_1^2*t_2, t_0*t_1^3*t_2, t_1^4*t_2});
        f = f * rationalMap linearSpan apply(3,i -> point linearSpan {f point source f,f point source f});
        X = cubicFourfold(image f,NumNodes=>3,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        X.cache#(surface X,"label") = "FarkasVerra";
        return X;
    );
   if str === "one-nodal septic del Pezzo surface" then (
       g := multirationalMap rationalMap(ring PP_K^2,{3,2});
       g = g * rationalMap(ring target g,ring PP_K^5,gens ideal linearSpan {point target g,point linearSpan {g point source g,g point source g}});
       X = cubicFourfold(image g,NumNodes=>1,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
       X.cache#(surface X,"label") = "oneNodalSepticDelPezzoSurfaceC26";
       return X;
   );
   if str === "general cubic 4-fold of discriminant 42" or str === "C42" then (
       X = cubicFourfold(last last randomS42data(K),NumNodes=>5,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
       X.cache#(surface X,"label") = "C42";
       return X;
   );
   if str === "cubic 4-fold of discriminant 48" or str === "C48" then (
       X = cubicFourfold(randomS48 K,NumNodes=>6,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
       X.cache#(surface X,"label") = "C48";
       return X;
   );
   if str === "general cubic 4-fold of discriminant 32" or str === "C32" then (
       X = cubicFourfold(surface({9,1,4,6},K),NumNodes=>0,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
       X.cache#(surface X,"label") = "C32";
       return X;
   );
   if str === "general cubic 4-fold of discriminant 44" or str === "C44" then ( -- Enriques surface (see e.g. https://arxiv.org/pdf/1210.1903.pdf, p. 7)
       J := Var ideal jacobian ideal discriminant first genericPolynomials({2,-1,-1,-1},K);
       X = cubicFourfold((parametrize random({{1},{1},{1},{1}},0_J))^* J,NumNodes=>0,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
       X.cache#(surface X,"label") = "C44";
       return X;
   );
   if str === "8-nodal nonic scroll" then (
       X = LaiFarkasVerraC42(K,NumNodes=>8,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
       X.cache#(surface X,"label") = "LaiFarkasVerraC42";
       (surface X).cache#"euler" = -44;
       return X;
   );
   error(///not valid string, permitted strings are:
"quintic del Pezzo surface",
"quartic scroll",
"3-nodal septic scroll",
"one-nodal septic del Pezzo surface",
"6-nodal octic scroll",
"general cubic 4-fold of discriminant 32",
"general cubic 4-fold of discriminant 38",
"general cubic 4-fold of discriminant 42",
"8-nodal nonic scroll",
"general cubic 4-fold of discriminant 44",
"cubic 4-fold of discriminant 48"///);
);

cubicFourfold String := o -> str -> cubicFourfold(str,ZZ/65521,NumNodes=>o.NumNodes,InputCheck=>o.InputCheck,Verbose=>o.Verbose);

expression CubicFourfold := X -> expression("cubic fourfold containing a surface of degree "|toString(degree surface X)|" and sectional genus "|toString(sectionalGenus surface X));

describe CubicFourfold := X -> (
    descr:="Special cubic fourfold of discriminant "|(toString discriminant X)|newline|"containing a "|surfaceDescription(0,surface X,true);
    if recognize X === "LaiFarkasVerraC42" then descr = descr|newline|"(the surface is the 8-nodal nonic scroll studied by K.-W. Lai, G. Farkas and A. Verra,"|newline|"this implementation is due to M. Hoff)";
    if computationStatus X >= 0 then descr = descr|newline|(computationStatusLog X)|newline|toString(describeMirrorFourfoldAndK3 X);
    net expression descr
);

shortDescriptionFourfold (CubicFourfold,Boolean) := (X,UseAttribute) -> (
    if UseAttribute and hasAttribute(X,ReverseDictionary) then return toString getAttribute(X,ReverseDictionary);
    "cubic fourfold in C_"|(toString discriminant X)
);

-- discriminant CubicFourfold := o -> X -> (
--     if X.cache#?(surface X,"discriminantFourfold") then return last X.cache#(surface X,"discriminantFourfold");
--     S := surface X;
--     degS := degree S; g := sectionalGenus S; chiOS := euler hilbertPolynomial S;
--     chiS := eulerCharacteristic(S,Algorithm=>if o.Algorithm === "Poisson" then null else o.Algorithm);
--     KS2 := 12*chiOS-chiS;
--     n := numberNodes S;
--     S2 := 3*degS+6*g-12*chiOS+2*KS2+2*n-6;
--     d := 3*S2 - degS^2;
--     X.cache#(surface X,"discriminantFourfold") = (S2,d);
--     d
-- );

discriminant CubicFourfold := discriminant HodgeSpecialFourfold := o -> X -> (
    if X.cache#?(surface X,"discriminantFourfold") then return last X.cache#(surface X,"discriminantFourfold");
    r := codim X;
    a := flatten degrees ideal X;
    if #a != r then error "expected a special fourfold which is a complete intersection";
    S := surface X;
    HS2 := degree S;
    KSHS := 2*(sectionalGenus S)-2-HS2;
    chiOS := euler hilbertPolynomial S;
    c2TS := eulerCharacteristic(S,Algorithm=>if o.Algorithm === "Poisson" then null else o.Algorithm);
    KS2 := 12*chiOS-c2TS;
    n := if instance(X,CubicFourfold) or S.cache#?"FiniteNumberOfNodes" or S.cache#?"singularLocus" or S.cache#?"nonSaturatedSingularLocus" or (S.cache#?"fitVariety" and (S.cache#"fitVariety").cache#?"nonSaturatedSingularLocus") then numberNodes S else 0;
    S2 := 2*n + (binomial(r+5,2) - (r+5)*(sum a) + (sum a)^2 - sum flatten for i to r-1 list for j from i+1 to r-1 list a_i*a_j) * HS2 + (r+5-sum a) * KSHS + KS2 - c2TS;
    d := det(X.cache#(surface X,"LatticeIntersectionMatrix") = matrix {{degree X,HS2},{HS2,S2}});
    if S.cache#?"FiniteNumberOfNodes" then X.cache#(surface X,"discriminantFourfold") = (S2,d);
    d
);

map CubicFourfold := o -> X -> associatedMapFromFivefold X;

recognizeCubicFourfold = X -> (
    S := surface X;
    d := discriminant X;
    e := eulerCharacteristic S;
    n := numberNodes surface X;
    invS := (degree S,sectionalGenus S,euler hilbertPolynomial S);
    degs := flatten degrees ideal S;
    if (d == 14 and e == 7 and n == 0 and invS === (5,1,1) and degs == toList(5:2)) then return "quinticDelPezzoSurface";
    if (d == 14 and e == 4 and n == 0 and invS === (4,0,1) and degs == toList(6:2)) then return "quarticScrollSurface";
    if (d == 32 and e == 14 and n == 0 and invS === (10,6,1) and degs == toList(10:3)) then return "C32";
    if (d == 38 and e == 13 and n == 0 and invS === (10,6,1) and degs == toList(10:3)) then return "C38Coble";
    if (d == 38 and e == -32 and n == 6 and invS === (8,0,-5) and degs == toList(10:3)) then return "6NodalOcticSrollC38";
    if (d == 44 and e == 12 and n == 0 and invS === (10,6,1) and degs == toList(10:3)) then return "C44";
    if (d == 26 and e == -14 and n == 3 and invS === (7,0,-2) and degs == toList(13:3)) then return "FarkasVerra";
    if (d == 26 and e == -1 and n == 1 and invS === (7,1,0) and degs == toList(14:3)) then return "oneNodalSepticDelPezzoSurfaceC26";
    if (d == 42 and e == -23 and n == 5 and invS === (9,2,-4) and degs == toList(9:3)) then return "C42";
    if (d == 48 and e == -29 and n == 6 and invS === (9,2,-5) and degs == {2,3,3,3,3}) then return "C48";
    if (d == 14 and e == 46 and n == 0 and invS === (13,12,4) and degs == toList(7:3)) then return "hyperplane section of a conic bundle over PP2";
    "NotRecognized"
);

random CubicFourfold := o -> X -> (
    X' := cubicFourfold(surface X,InputCheck=>-1);
    if X.cache#?(surface X,"label") and (not X'.cache#?(surface X',"label")) then X'.cache#(surface X',"label") = X.cache#(surface X,"label");
    X'
);

parameterCount CubicFourfold := o -> X -> parameterCount(surface X,X,Verbose=>o.Verbose);

isAdmissible = method();

isAdmissible ZZ := d -> (
    if d <= 6 then return false;
    if d % 2 != 0 then return false;
    if d % 4 == 0 then return false;
    if d % 9 == 0 then return false;
    for p from 3 to floor(d/2) do if (p % 3 == 2 and isPrime p and d % p == 0) then return false;
    if d % 6 != 0 and d % 6 != 2 then error toString d;
    return true;
);

isAdmissible CubicFourfold := X -> isAdmissible discriminant X;

unirationalParametrization (CubicFourfold,EmbeddedProjectiveVariety) := (X,L) -> (
    if not(isSubset(L,X) and dim L == 1 and degree L == 1) then error "expected a line in the cubic fourfold";
    ringP5 := ring ambient X;
    K := coefficientRing X;
    l := toRationalMap parametrize L;
    K' := frac(source l);
    ringP5' := K'[gens ringP5];
    p' := trim minors(2,(vars ringP5')||(matrix l));
    X' := sub(ideal X,ringP5');
    TpX' := trim ideal((vars ringP5') * sub(jacobian X',apply(gens ringP5',flatten entries coefficients parametrize p',(x,s) -> x => s)));
    U := TpX' + ideal(last gens ringP5');
    u := parametrize U;
    u = rationalMap(Grass(0,3,K',Variable=>"s"),source u) * u;
    e := lcm apply(flatten entries sub(last coefficients matrix u,K'),denominator);
    M := transpose((matrix l)||(e * matrix u));
    ringP1xP3 := (source l) ** K[gens source u];
    M = sub(M,ringP1xP3);
    r := local r;
    Kr := ringP1xP3[r];
    P := first first entries gens sub(ideal X,apply(gens ringP5,flatten entries(submatrix(sub(M,Kr),{0}) + r*submatrix(sub(M,Kr),{1})),(v,v') -> v => v'));
    psi := rationalMap(ringP1xP3,ringP5,transpose(coefficient(r^3,P) * submatrix(M,{0}) - coefficient(r^2,P) * submatrix(M,{1})));
    Psi := multirationalMap({parametrize psi},X);
    if not isSubset(Psi point source Psi,X) then error "internal error encountered";
    Psi
);

unirationalParametrization CubicFourfold := (cacheValue "unirationalParametrization") (X -> unirationalParametrization(X,line X));

randomS42data = method();
randomS42data Ring := K -> (
    p := apply(5,i -> point PP_K^2);
    f := (rationalMap(p_0 + p_2 + p_3 + p_4,2)) | (rationalMap(p_0 + p_1,2));
    s := multirationalMap segre target f;
    f = multirationalMap segre f;
    L := linearSpan {f point source f,f point source f};
    j := parametrize random(1,L);
    pr := rationalMap point (j^^ L);
    B := pr(j^^ (image s));
    C := pr(j^^ (image f));
    phi := toRationalMap rationalMap(C,Dominant=>true);
    forceInverseMap(phi,inverseMap phi);
    Bs := baseLocus inverse phi;
    Q := 0_Bs;
    while select(degrees ideal Q,d -> d <= {2}) =!= {{1},{1},{1},{2}} do (
        u := parametrize random({{1},{1}},0_Bs);
        v := (rationalMap {random(1,ring source u),random(1,ring source u)})|(u^^ Bs);
        Q = Q + u support v^*((v (u^^ Bs))\support(v (u^^ Bs)));
    );
    Q = Var trim ideal select(flatten entries gens ideal Q,d -> degree d <= {2});
    if not (dim Q == 2 and degree Q == 2 and dim singularLocus Q == -1) then error "internal error encountered";
    P1xP2 := phi^* Q;
    w := Var trim lift(phi (rationalMap flatten entries syz gens ideal P1xP2)^-1 (ideal submatrix'(vars ring ambient P1xP2,{0})),ambient target phi);
    e := super inverse(rationalMap(w%Q),Verify=>false);
    w = e point source e;
    (L1,L2) := toSequence decompose(Q * tangentSpace(w,Q));
    D := phi^* L1;
    if not(dim D == 2 and degree D == 5 and sectionalGenus D == 1) then D = phi^* L2;
    if not(dim D == 2 and degree D == 5 and sectionalGenus D == 1) then error "internal error encountered";
    psi := rationalMap(B,3);
    T := psi D;
    if not(dim T == 2 and codim T == 6 and degree T == 9 and sectionalGenus T == 2) then error "internal error encountered";
    eta := rationalMap((SchubertCycle22OnLinearSectionOfG14 image psi)%(image psi));
    S42 := eta T;
    if not(dim S42 == 2 and degree S42 == 9 and sectionalGenus S42 == 2 and genera ideal S42 == {-5,2,8} and degrees S42 == {({3},9)}) then error "internal error encountered";
    S42.cache#"euler" = -23; S42.cache#"FiniteNumberOfNodes" = 5;
    T.cache#"euler" = 7; T.cache#"FiniteNumberOfNodes" = 0;
    ((psi,D),(super inverse3 eta,S42))
);

randomS48 = method();
randomS48 Ring := K -> (
    (psi,D) := first randomS42data K;
    p := psi point source psi;
    V := coneOfLines(image psi,p);
    j := parametrize linearSpan V;
    V' := j^* V;
    p' := j^* p;
    h := rationalMap p'_V';
    e := point image h;
    P := j h^* ((coneOfLines(image h,e))\e);
    assert(dim P == 2 and degree P == 1 and isSubset(P,image psi));
    S := (psi * rationalMap P) D;
    if not(dim S == 2 and degree S == 9 and sectionalGenus S == 2 and genera ideal S == {-6,2,8} and degrees S == {({2}, 1),({3}, 4)}) then error "internal error encountered";
    return S;
);

LaiFarkasVerraC42 = method(Options => options cubicFourfold);
LaiFarkasVerraC42 Ring := o -> K -> (
--  **
--  The code of this function has been written by Michael Hoff
--  **
--  24.10.2019
--  The construction and notation follow Farkas and Verra:
--  "THE UNIRATIONALITY OF THE MODULI SPACE OF K3 SURFACES OF DEGREE 42"
    y := local y;
    P2 := K[y_0..y_2];
    p := ideal(y_0,y_1);
    z := local z;
    P4 := K[z_0..z_4];
    blowup := map(P2,P4, gens p *matrix basis(2,p)); -- linear system |2h-p|
    F1 := kernel blowup; -- Hirzebruch surface F1
    l := ideal(random(P4^1,P4^{3:-1})); -- line in P4
    t := apply(8, i-> (preimage_blowup(ideal(random(P2^1,P2^{2:-1}))))); -- 8 points on F1
    spantl := apply(#t,i->gens intersect(t_i,l)*matrix basis(1,intersect(t_i,l))); -- linear span of l and t_i
    points16 := intersect (points2 := apply(#spantl,i->(saturate((ideal(spantl_i)+F1),t_i)))); -- 16 residual points on F1
    assert((dim points16, degree points16, genus points16) == (1, 16, 15));
    -- see FV end of page 4
    RF1 := P4/F1;
    z = gens RF1;
    E := ideal(z_0,z_1,z_3);
    pointsOnF1 := sub(points16,RF1);
    I := intersect(pointsOnF1,E);
    C := ideal(gens I * random(source gens I, RF1^{1:-3}));
    C = saturate(sub(C,P4)+F1,sub(E,P4));
    assert((dim C, degree C, genus C) == (2, 8, 4));
    projl := map(P4,P2,gens l);
    octic := preimage_projl(C); -- plane octic curve with 8 + 9 singular points
    singOctic := saturate ideal singularLocus octic;
    assert((dim singOctic, degree singOctic, genus singOctic) == (1, 17, 16));
    points8 := saturate(preimage_projl(points16));
    points9 := saturate(singOctic, points8);
    -- blow up of 9 points
    x := local x;
    P5 := K[x_0..x_5];
    blowup2 := map(P2,P5,gens points9*matrix basis(4,points9));
    T := kernel blowup2;
    -- betti res T
    gamma := preimage_blowup2(octic); -- hyperelliptic curve of degree 14 and arithmetic genus 12
    -- betti res gamma
    singGamma := preimage_blowup2(points8);
    -- betti (resSingGamma = res singGamma)
    -- I take the canonical map to compute pairs of points which are involuted.
    w := local w;
    P3 := K[w_0..w_3];
    canMap := map(P2,P3,gens singOctic*matrix basis(5,singOctic));
    twistedCubic := preimage_canMap(octic);
    betti (resTwistedC := res twistedCubic);
    assert((dim twistedCubic, degree twistedCubic, genus twistedCubic) == (2, 3, 0));
    M := resTwistedC.dd_2;
    -- I take many lines and compute the ideal where the generators stabilizes
    pNip := apply(40, i-> saturate(canMap(ideal(random(K)*M_{0} + random(K)*M_{1})),singOctic));
    L := apply(#pNip,i->(ideal(gens(preimage_blowup2(pNip#i)))_{0,1,2,3}));
    betti (R := intersect(L));
    R = ideal( (gens R)_{0..5,6..8}); -- the nonic scroll with 8 singularities
    -- betti res R
    assert((dim R, degree R, genus R) == (3, 9, -8) and gamma + R == gamma);
    if o.NumNodes =!= 8 then error "the number of nodes is equal to 8";
    cubicFourfold(R,NumNodes=>8,Verbose=>o.Verbose,InputCheck=>o.InputCheck)
);
