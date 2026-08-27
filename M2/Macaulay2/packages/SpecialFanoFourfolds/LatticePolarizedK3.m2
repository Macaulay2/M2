
------------------------------------------------------------------------
--- Lattice-polarized K3 surfaces from doubly special cubic fourfolds --
------------------------------------------------------------------------

K3SurfaceFromDoublySpecialCubicFourfold = new Type of MutableHashTable;
globalAssignment K3SurfaceFromDoublySpecialCubicFourfold;
K3SurfaceFromDoublySpecialCubicFourfold.synonym = "K3 surface";

net K3SurfaceFromDoublySpecialCubicFourfold := describe K3SurfaceFromDoublySpecialCubicFourfold := S -> (
    out := describe underlyingK3 S;
    sanityCheckDSCF S;
    if S#"LatticePolarization" === null then return (out|| "Lattice polarization: not yet computed; rerun 'polarizedK3surface'");
    M := latticeMatrix S;
    isDetAsExpected := () -> (A := latticeIntersectionMatrix3x3 recoverFourfold S; if ring A =!= ZZ then return false; (det A) + (det M) == 0);
    if isVirtualLatticeK3 S and computationStatus S <= 3 then out = out||("Lattice intersection matrix (virtual, computed from U): "|(net M)|net(if isDetAsExpected() then " (det = "|(toString det M)|") ✓" else ""));
    if computationStatus S >= 4 and isDetAsExpected() then out = out || "[ use: oo(a,b) or oo(g) for polarized K3 models; call latticePolarization(oo) ]";
    out
);
texMath K3SurfaceFromDoublySpecialCubicFourfold := texMath @@ net;

K3SurfaceFromDoublySpecialCubicFourfold#{WebApp,AfterPrint} =
K3SurfaceFromDoublySpecialCubicFourfold#{WebApp,AfterNoPrint} =
K3SurfaceFromDoublySpecialCubicFourfold#{Standard,AfterPrint} =
K3SurfaceFromDoublySpecialCubicFourfold#{Standard,AfterNoPrint} = S -> (
    sanityCheckDSCF S;
    << endl << concatenate(interpreterDepth:"o") << lineNumber << " : " << "Lattice-polarized K3 surface associated to " << (shortDescriptionFourfold recoverFourfold S) << " — " << computationStatusLog(S) << endl;
);

underlyingK3 = method();
underlyingK3 K3SurfaceFromDoublySpecialCubicFourfold := S -> S#"UnderlyingK3";
projectiveVariety K3SurfaceFromDoublySpecialCubicFourfold := o -> S -> (
    if dim underlyingK3 S == -1 then << "-- warning: underlying K3 surface not fully computed" << endl;
    underlyingK3 S
);
surface K3SurfaceFromDoublySpecialCubicFourfold := S -> projectiveVariety S;

coefficientRing K3SurfaceFromDoublySpecialCubicFourfold := S -> coefficientRing underlyingK3 S;
genus K3SurfaceFromDoublySpecialCubicFourfold := S -> (
    Y := underlyingK3 S;
    if instance(Y,WeightedProjectiveVariety) and degrees ring ambient Y === {{1},{1},{1},{3}} and dim Y == 2 and codim Y == 1 and degree Y == 6 then return 2;
    if dim Y == -1 then error "unable to determine genus: underlying K3 surface not fully computed";
    sectionalGenus Y
);
degree K3SurfaceFromDoublySpecialCubicFourfold := S -> 2*(genus S)-2;
building K3SurfaceFromDoublySpecialCubicFourfold := S -> building underlyingK3 S;
recoverFourfold K3SurfaceFromDoublySpecialCubicFourfold := S -> recoverFourfold underlyingK3 S;
map(K3SurfaceFromDoublySpecialCubicFourfold,ZZ,ZZ) := o -> (S,a,b) -> map(latticePolarization S,a,b);

getInverseFanoMap K3SurfaceFromDoublySpecialCubicFourfold := S -> getInverseFanoMap underlyingK3 S;
mapFromWtoP2xP2 K3SurfaceFromDoublySpecialCubicFourfold := o -> S -> mapFromWtoP2xP2(underlyingK3 S,Verbose=>o.Verbose);
mapFromUtoP2xP2 K3SurfaceFromDoublySpecialCubicFourfold := o -> S -> mapFromUtoP2xP2(underlyingK3 S,Verbose=>o.Verbose);

latticePolarization = method();
latticePolarization K3SurfaceFromDoublySpecialCubicFourfold := S -> (
    if S#"LatticePolarization" === null then error "lattice polarization not computed; rerun polarizedK3surface";
    S#"LatticePolarization"
);

latticeMatrix K3SurfaceFromDoublySpecialCubicFourfold := S -> latticeMatrix latticePolarization S;

isVirtualLatticeK3 = method();
isVirtualLatticeK3 K3SurfaceFromDoublySpecialCubicFourfold := S -> S#"LatticePolarization" =!= null and (S#"LatticePolarization")#"isVirtual";

polarizedK3surface = method(TypicalValue => K3SurfaceFromDoublySpecialCubicFourfold, Options => {Verbose => false, Strategy => null, FanoMapType => null});
polarizedK3surface DoublySpecialCubicFourfold := o -> X -> (
    if not instance(o.Verbose,Boolean) then error "expected a Boolean value for option 'Verbose'";
    local StrK3; local StrPol; local S;
    StrK3Set := {"Inverse","Approximate"};
    StrPolSet := {null, "Genus2Curve", "SpecialCurve", "MapFromW", "MapFromU", "Genus2Curve-Virtual", "SpecialCurve-Virtual", "MapFromW-Virtual", "MapFromU-Virtual"};
    if member(o.Strategy,StrPolSet)
    then (StrK3,StrPol) = (null,o.Strategy)
    else if member(o.Strategy,StrK3Set)
    then (StrK3,StrPol) = (o.Strategy,null)
    else if instance(o.Strategy,VisibleList) and # o.Strategy == 2 and member(first o.Strategy, StrK3Set) and member(last o.Strategy, StrPolSet)
    then (StrK3,StrPol) = toSequence o.Strategy
    else error("polarizedK3surface: invalid Strategy; expected one of {\"Genus2Curve\", \"SpecialCurve\", \"MapFromW\", \"MapFromU\", \"Genus2Curve-Virtual\", \"SpecialCurve-Virtual\", \"MapFromW-Virtual\", \"MapFromU-Virtual\"}, or {\"Inverse\", \"Approximate\"}, or a pair of these");
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
    S#"UnderlyingK3" = T#"UnderlyingSurface";
    S#"LatticePolarization" = T;
    return S;
);

polarizedK3surface K3SurfaceFromDoublySpecialCubicFourfold := o -> S -> polarizedK3surface(recoverFourfold S,Verbose=>o.Verbose,Strategy=>o.Strategy,FanoMapType=>o.FanoMapType);
polarizedK3surface SurfaceAssociatedToRationalFourfold := o -> S -> (
    X := recoverFourfold S;
    if not instance(X,DoublySpecialCubicFourfold) then error "K3 surface is expected to be associated to a doubly special cubic fourfold";
    polarizedK3surface(X,Verbose=>o.Verbose,Strategy=>o.Strategy,FanoMapType=>o.FanoMapType)
);

polarize K3SurfaceFromDoublySpecialCubicFourfold := o -> S -> polarizedK3surface(S,Verbose=>false);

associatedK3surface DoublySpecialCubicFourfold := o -> X -> polarizedK3surface(X,Verbose=>o.Verbose,Strategy=>o.Strategy);

------------------------------------------------------------------------
------------------------------------------------------------------------

EmbeddedK3SurfaceFromDoublySpecialCubicFourfold = new Type of K3SurfaceFromDoublySpecialCubicFourfold;
globalAssignment EmbeddedK3SurfaceFromDoublySpecialCubicFourfold;
EmbeddedK3SurfaceFromDoublySpecialCubicFourfold.synonym = "K3 surface";

embeddedK3SurfaceFromDoublySpecialCubicFourfold = method(TypicalValue => EmbeddedK3SurfaceFromDoublySpecialCubicFourfold);
embeddedK3SurfaceFromDoublySpecialCubicFourfold (K3SurfaceFromDoublySpecialCubicFourfold,MultirationalMap,MultiprojectiveVariety,Matrix) := (E,h,D,A) -> (
    assert(dim D == 1 and dim image h == 2 and isSubset(D,image h));
    L := new LatticePolarizationOnK3Surface from {
        symbol cache => new CacheTable,
        "DoublySpecialFourfold" => recoverFourfold E,
        "UnderlyingSurface" => image h,
        "specialCurve" => D,
        "latticeMatrix" => A,
        "isVirtual" => false
    };
    new EmbeddedK3SurfaceFromDoublySpecialCubicFourfold from {
        symbol cache => new CacheTable,
        "ParentK3Surface" => if E#?"ParentK3Surface" then E#"ParentK3Surface" else E,
        "MapFromParentK3Surface" => if E#?"MapFromParentK3Surface" then append(E#"MapFromParentK3Surface",h) else {h},
        "UnderlyingK3" => image h,
        "LatticePolarization" => L
    }
);

net EmbeddedK3SurfaceFromDoublySpecialCubicFourfold := describe EmbeddedK3SurfaceFromDoublySpecialCubicFourfold := E -> (
    X := recoverFourfold E;
    (S,T) := surfaces X;
    Y := X.cache#"parentCubicFourfold";
    d1 := discriminant X;
    d2 := discriminant Y;
    A := latticeIntersectionMatrix3x3 X;
    B := latticeMatrix E;
    g := genus E;
    Cd1Cd2 := "C_"|(toString d1);
    if d1 != d2 then Cd1Cd2 = Cd1Cd2|" ∩ C_"|(toString d2);
    F := ("Lattice-polarized K3 surface of genus "|(toString g)|" in ")|(net ambient underlyingK3 E);
    F = F||(("of lattice discriminant det(")|(net B)|") = "|(toString det B));
    F = F||("associated to a cubic fourfold in "|Cd1Cd2|" of lattice discriminant");
    F = F||((net(newline|"det("))|(net A)|(net(newline|") = "|(toString det A))));
    F
);
texMath EmbeddedK3SurfaceFromDoublySpecialCubicFourfold := texMath @@ net;

EmbeddedK3SurfaceFromDoublySpecialCubicFourfold#{WebApp,AfterPrint} =
EmbeddedK3SurfaceFromDoublySpecialCubicFourfold#{WebApp,AfterNoPrint} =
EmbeddedK3SurfaceFromDoublySpecialCubicFourfold#{Standard,AfterPrint} =
EmbeddedK3SurfaceFromDoublySpecialCubicFourfold#{Standard,AfterNoPrint} = E -> (
    << endl << concatenate(interpreterDepth:"o") << lineNumber << " : " << "Lattice-polarized K3 surface associated to " << (shortDescriptionFourfold recoverFourfold E) << endl;
);

building EmbeddedK3SurfaceFromDoublySpecialCubicFourfold := E -> (
    (mu,U,LC,f) := building E#"ParentK3Surface";
    (mu,U,LC,toSequence prepend(f,E#"MapFromParentK3Surface"))
);
recoverFourfold EmbeddedK3SurfaceFromDoublySpecialCubicFourfold := E -> recoverFourfold E#"ParentK3Surface";

K3SurfaceFromDoublySpecialCubicFourfold Sequence := (E,ab) -> (
    Verb := true;
    if #ab >= 2 and #ab <= 3 and instance(last ab,Option) then (
        opt := toSequence last ab;
        if first opt =!= Verbose then error "Verbose is the only available option";
        Verb = last opt;
        if not instance(Verb,Boolean) then error "expected a Boolean value";
        ab = drop(ab,-1);
    );
    L := latticePolarization E;
    M := latticeMatrix L;
    if #ab == 1 and instance(first ab,ZZ) then (
        ab = first ab;
        if ab < 2 then error "expected genus at least 2";
        scPol := scanPolarizations(3,M);
        scPol = select(scPol, v -> v_0 == ab and v_5 == det M);
        if #scPol == 0 then error("failed to find a valid divisor D = aH+bC on K3 surface with D^2 = "|toString(2*ab-2)|"; try polarize(i,...) manually");
        ab = ((first scPol)_3,(first scPol)_4);
    );
    if not(#ab == 2 and instance(first ab,ZZ) and instance(last ab,ZZ)) then error "expected an integer or a sequence of two integers";
    (a,b) := ab;
    if E.cache#?("EmbeddedByLatticePolarization",a,b) then return E.cache#("EmbeddedByLatticePolarization",a,b);
    -- if gcd(a,b) != 1 then error "expected a and b to be coprime for a primitive polarization";
    g := lift((a^2*M_(0,0) + 2*a*b*M_(0,1) + b^2*M_(1,1) + 2)/2, ZZ);
    if g < 2 then error "invalid pair of integers: map target would have dimension < 2";
    K := coefficientRing E;
    P := if g == 2 then PP_K(1,1,1,3) else PP_K^g;
    if Verb then << "-- (▪) constructing K3 surface of genus " << g << " in " << (net P) << endl;
    if isVirtualLatticeK3 E then (
        if Verb then << "  -- using divisor D = aH+bC, with D^2=" << 2*g-2 << "=2*" << g << "-2, where (a,b) = " << toString(a,b) << endl;
        d1 := if g == 2 then a*M_(0,0) + b*M_(0,1) else a*M_(0,1) + b*M_(1,1);
        n1 := if g == 2 then M_(0,0) else M_(1,1);
        if d1 < 1 then error "failed to construct (virtual) lattice-polarized K3 surface";
        << "-- warning: lattice polarization is virtual; rerun polarizedK3surface with an appropriate option, e.g. Strategy=>\"SpecialCurve\"" << endl;
        << "-- no polarized K3 surface constructed; returning a virtual lattice polarization only" << endl;
        return latticePolarizationOnK3Surface(underlyingK3 E, 2*g-2, d1, n1);
    );
    if Verb then (
        << "  -- from K3 surface of genus " << (genus E) << " in " << (net ambient underlyingK3 E) << endl;
        << "  -- using divisor D = aH+bC, with D^2=" << 2*g-2 << "=2*" << g << "-2, where (a,b) = " << toString(a,b) << endl;
    );
    h := mapDefinedByLatticePolarization(L,P,a,b,Verbose=>Verb,Verify=>true);
    if Verb then << "-- (▪) constructing polarization on K3 surface of genus " << g << " in " << (net P) << "..." << endl;
    Y := source h;
    T := image h;
    local D; local A;
    if g == 2 then (
        D = h (Y * random(1,0_Y));
        H := T * random(1,0_T);
        if dim D != 1 or dim H != 1 then error "failed to obtain divisor curve for polarization";
        D' := h (Y * random(1,0_Y));
        H' := T * random(1,0_T);
        if dim(H*H') != 0 or dim(H*D) != 0 or dim(D*D') != 0 then error "expected the intersection of divisors on the surface to be zero-dimensional";
        v00 := degree(H*H'); -- since the dimension is zero, this equals 'degree image segreEmbedding(..)' (pkg: MultiprojectiveVarieties)
        v01 := degree(H*D);
        v11 := degree(D*D');
        A = matrix {{v00,v01},{v01,v11}};
        if v00 != 2 or v11 != M_(0,0) then error("unexpected lattice matrix on the genus 2 K3 surface: "|(toString A));
        if v01 != a*M_(0,0) + b*M_(0,1) then error("failed to obtain polarization: divisor curves have intersection number "|(toString v01)|", expected "|toString(a*M_(0,0) + b*M_(0,1)));
    ) else (
        D = h L#"specialCurve";
        if dim D == -1 then (
            if Verb then << "  -- need to move curve divisor on K3 surface" << endl;
            Q := L#"specialCurve";
            gQ := sectionalGenus Q;
            if gQ == 0 then error "cannot move rational curve divisor on K3 surface";
            mQ := mapDefinedByLatticePolarization(L,gQ,0,1,Verbose=>Verb,Verify=>true);
            Q' := mQ^* random(1,0_(target mQ));
            if dim Q' == 1 and degree Q' == degree Q and sectionalGenus Q' == gQ and dim(Q*Q') == 0 and degree(Q*Q') == 2*gQ-2 then (
                D = h Q';
            ) else (
                if Verb then << "  -- failed to move curve divisor" << endl;
            );
        );
        if dim D != 1 then error "failed to obtain divisor curve for polarization";
        if degree D != a*M_(0,1) + b*M_(1,1) then error("failed to obtain polarization: divisor curve has degree "|(toString degree D)|", expected "|toString(a*M_(0,1) + b*M_(1,1)));
        A = matrix {{2*g-2, degree D}, {degree D, M_(1,1)}};
    );
    if Verb then (
        if det A != det M then (
            << "-- unsuitable lattice discriminant on the genus " << g << " K3 surface: " << det A << " != " << det M << endl;
        ) else (
            << "-- (✓) polarized K3 surface of genus " << g << " in " << (net P) << " successfully constructed" << endl;
        );
    );
    E.cache#("EmbeddedByLatticePolarization",a,b) = embeddedK3SurfaceFromDoublySpecialCubicFourfold(E,h,D,A)
);

K3SurfaceFromDoublySpecialCubicFourfold ZZ := (E,g) -> E(g,Verbose=>true);
