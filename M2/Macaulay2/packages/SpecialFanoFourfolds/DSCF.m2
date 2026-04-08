
------------------------------------------------------------------------
------- New section (version 2.8, April 2026) replaces ver 2.7.1 -------
------------------------------------------------------------------------
--------------------- Double special cubic fourfolds -------------------
------------------------------------------------------------------------

PairOfSurfaces = new Type of List;
globalAssignment PairOfSurfaces;
EmbeddedProjectiveVariety & EmbeddedProjectiveVariety := (S,T) -> (
    if dim S != 2 or dim T != 2 then error "expected two surfaces";
    if ring ideal S =!= ring ideal T then error "expected surfaces in the same ambient projective space";
    new PairOfSurfaces from {S,T}
);
ambient PairOfSurfaces := SandT -> ambient first SandT;
random (ZZ,PairOfSurfaces) := random (List,PairOfSurfaces) := o -> (l,SandT) -> random(l,sum SandT);

DoubleSpecialCubicFourfold = new Type of SpecialCubicFourfold;
globalAssignment DoubleSpecialCubicFourfold;
DoubleSpecialCubicFourfold.synonym = "double special cubic fourfold";

specialFourfold (PairOfSurfaces,EmbeddedProjectiveVariety) := specialCubicFourfold (PairOfSurfaces,EmbeddedProjectiveVariety) := o -> (SandT,X) -> (
    (S,T) := toSequence SandT;
    (nS,nT) := if instance(o.NumNodes,VisibleList) and # o.NumNodes == 2 then toSequence o.NumNodes else (o.NumNodes,o.NumNodes);
    Y := specialCubicFourfold(T,X,NumNodes=>nT,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
    Z := new DoubleSpecialCubicFourfold from specialCubicFourfold(S,Y,NumNodes=>nS,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
    Z.cache#"parentCubicFourfold" = Y;
    assert(surface Z === S and surface Y === T and take(Z#"SurfaceContainedInTheFourfold",2) === {S,T});
    if dim (S * T) == 2 then error "intersection of the two surfaces has dimension 2 (unsupported)";
    Z
);

specialFourfold PairOfSurfaces := specialCubicFourfold PairOfSurfaces := o -> SandT -> (
    if dim ambient SandT != 5 then error "expected two surfaces in P^5";
    specialCubicFourfold(SandT,random(3,SandT),NumNodes=>o.NumNodes,InputCheck=>o.InputCheck,Verbose=>o.Verbose)
);

------------------------------------------------------------------------
RationalSurfaceWithAttachedPlaneInCubicFourfold = new Type of EmbeddedProjectiveVariety;
globalAssignment RationalSurfaceWithAttachedPlaneInCubicFourfold;
rationalSurfaceWithAttachedPlaneInCubicFourfold = method(TypicalValue => RationalSurfaceWithAttachedPlaneInCubicFourfold, Options => {"EulerCharacteristic" => true, Verbose => true});
rationalSurfaceWithAttachedPlaneInCubicFourfold (EmbeddedProjectiveVariety,VisibleList) := o -> (S,dj1j2j3) -> (
    -- Input: 1) a rational surface S = surface(a,i1,i2,...), or its defining parameters (a,i1,i2,...).
    --        2) the parameters for a curve C on S, defined as the image of a plane curve of degree d and multiplicities (j1,j2,...).
    -- Output: if no errors occur, the projection of S into PP^5 from a center contained in the span <C>.
    ai1i2i3 := S.cache#"linear system on PP^2";
    expEul := 3 + sum toList drop(ai1i2i3,1);
    if not o#"EulerCharacteristic" then (
        S.cache#"euler" = expEul;
        if o.Verbose then << "-- set the Euler characteristic of the surface in PP^" << dim ambient S << " to the expected value " << S.cache#"euler" << endl;
    );
    C := S.cache#"takeCurve" (first dj1j2j3, toList drop(dj1j2j3,1));
    if dim linearSpan S < dim ambient S then error "the surface is degenerate";
    if dim ambient S - 5 != dim linearSpan C - 2 then error("dim ambient S = "|toString(dim ambient S)|", dim linearSpan C = "|toString(dim linearSpan C)|", dim ambient S - 5 != dim linearSpan C - 2");
    if o.Verbose and dim linearSpan S > 5 then (
        << "-- computing the projection to PP^5 of" << endl;
        << "  -- " << ? S << endl;
        << "-- from points on the linear span of" << endl;
        << "  -- " << ? C << endl;
    );
    piLin := if dim linearSpan S > 5 then rationalMap linearSpan apply(dim linearSpan S - 5, i -> point linearSpan C) else rationalMap(ambient S,ambient S);
    if dim target piLin != 5 then error "linear projection failed, target dimension is not 5";
    f := (super parametrize S) * piLin;
    S' := new RationalSurfaceWithAttachedPlaneInCubicFourfold from image f;
    if min apply(degrees S',i -> first first i) > 3 then error "surface in PP^5 is not contained in any cubic hypersurfaces";
    C' := piLin C;
    planeC' := linearSpan C';
    if dim planeC' != 2 then error "dim linearSpan C' != 2";
    if not(dim C' == 1 and degree C' == degree C) then error "not: dim C' == 1 and degree C' == degree C";
    S'.cache#"rationalParametrization" = rationalMap(f,Dominant=>true);
    if sectionalGenus C' < 0 or sectionalGenus C < 0 then error "reducible curve";
    S'.cache#"FiniteNumberOfNodes" = (sectionalGenus C') - (sectionalGenus C);
    planeC'.cache#"FiniteNumberOfNodes" = 0;
    cubicS' := random(3, S' + planeC');
    if not isSmooth cubicS' then error "obtained a singular cubic fourfold";
    if o#"EulerCharacteristic" then (
        if not S.cache#?"euler" then (
            if o.Verbose then << "-- computing Euler characteristic of the surface in PP^" << dim ambient S << ", expected value: " << expEul << endl;
            eulerCharacteristic S;
            assert(S.cache#?"euler");
            if o.Verbose then << "-- Euler characteristic computation done, obtained value: " << S.cache#"euler" << endl;
        ) else (
            if o.Verbose then << "-- Euler characteristic of the surface in PP^" << dim ambient S << " found in cache, skipping computation" << endl;
        );
    );
    S'.cache#"euler" = (eulerCharacteristic S) - 6*(numberNodes S');
    if o.Verbose then << "-- constructing a cubic fourfold containing the surface and the plane" << endl;
    X := specialCubicFourfold(S' & planeC',cubicS',Verbose=>o.Verbose);
    X.cache#"Construction" = "X = specialFourfold surface("|(toString toSequence ai1i2i3)|","|(toString toSequence dj1j2j3)|");";
    X.cache#"DataConstruction" = (S,C,piLin);
    surfaceIntersectionNumber(X,Verbose=>o.Verbose,Verify=>true,"AttemptComputation"=>false);
    if o.Verbose then <<endl<<describe X<<endl;
    S'.cache#"attachedPlane" = planeC';
    S'.cache#"pickedCubicFourfold" = X;
    S'
);
rationalSurfaceWithAttachedPlaneInCubicFourfold (VisibleList,VisibleList,Ring) := o -> (ai1i2i3,dj1j2j3,K) -> (
    if # ai1i2i3 != # dj1j2j3 then error "expected two lists of the same length";
    rationalSurfaceWithAttachedPlaneInCubicFourfold(surface(toList ai1i2i3,K),dj1j2j3,"EulerCharacteristic"=>o#"EulerCharacteristic",Verbose=>o.Verbose)
);

surface (VisibleList,VisibleList,Ring,Option) := (ai1i2i3,dj1j2j3,K,opt) -> (
    o := toList opt;
    if not(#o == 2 and first o === Verbose) then error "Verbose is the only available option for surface(VisibleList,VisibleList)";
    rationalSurfaceWithAttachedPlaneInCubicFourfold(ai1i2i3,dj1j2j3,K,"EulerCharacteristic"=>true,Verbose=>last o)
);
surface (VisibleList,VisibleList,Option) := (ai1i2i3,dj1j2j3,opt) -> surface(ai1i2i3,dj1j2j3,ZZ/65521,opt);
surface (VisibleList,VisibleList,Ring) := (ai1i2i3,dj1j2j3,K) -> surface(ai1i2i3,dj1j2j3,K,Verbose=>false);
surface (VisibleList,VisibleList) := (ai1i2i3,dj1j2j3) -> surface(ai1i2i3,dj1j2j3,Verbose=>false);

specialFourfold RationalSurfaceWithAttachedPlaneInCubicFourfold := specialCubicFourfold RationalSurfaceWithAttachedPlaneInCubicFourfold := o -> S -> S.cache#"pickedCubicFourfold";
------------------------------------------------------------------------

surfaces = method();
surfaces DoubleSpecialCubicFourfold := X -> toSequence take(X#"SurfaceContainedInTheFourfold",2);

expression DoubleSpecialCubicFourfold := X -> (
    (S,T) := surfaces X;
    if isPlaneInP5 S and isPlaneInP5 T then return expression("cubic fourfold in C_8 containing two planes");
    if isPlaneInP5 S then return expression("cubic fourfold in C_8 containing a plane and a surface of degree "|toString(degree T)|" and sectional genus "|toString(sectionalGenus T));
    if isPlaneInP5 T then return expression("cubic fourfold in C_8 containing a surface of degree "|toString(degree S)|" and sectional genus "|toString(sectionalGenus S)|" and a plane");
    expression("cubic fourfold containing two surfaces of degrees " | (toString degree S) | " and " | (toString degree T) | ", sectional genera " | (toString sectionalGenus S) | " and " | (toString sectionalGenus T))
);

describe DoubleSpecialCubicFourfold := X -> (
    (S,T) := surfaces X;
    Y := X.cache#"parentCubicFourfold";
    d1 := discriminant X;
    d2 := discriminant Y;
    A := latticeIntersectionMatrix3x3 X;
    Cd1Cd2 := "C_"|(toString d1);
    if d1 != d2 then Cd1Cd2 = Cd1Cd2|" ∩ C_"|(toString d2);
    descr := "Cubic fourfold in "|Cd1Cd2|" over "|toString(coefficientRing X)|" of lattice discriminant";
    descr = descr||((net(newline|"det("))|(net A)|(net(newline|") = "|(toString det A))));
    surfaceInFourfoldPrint := S0 -> (
        if isPlaneInP5 S0 then return net(" - plane");
        n := numberNodes S0;
        nodal := if n > 0 then (toString n)|"-nodal " else (if S0.cache#?"singularLocus" and dim(singularLocus S0) == -1 then "smooth " else "");
        rational := if S0.cache#?"rationalParametrization" then "rational " else "";
        surfaceHeader := net(" - " | nodal | rational | "surface of degree " | (toString degree S0) | " and sectional genus " | (toString sectionalGenus S0));
        degs := flatten degrees ideal S0;
        cutOutLine := net(" cut out by "|(if same degs then (toString(#degs)|" hypersurfaces of degree "|(toString first degs)) else (toString(#degs)|" hypersurfaces of degrees "|toStringDegreesVar(S0))));
        surfaceHeader|cutOutLine
    );
    descr = descr||net "containing two surfaces:";
    descr = descr||(surfaceInFourfoldPrint S);
    descr = descr||(surfaceInFourfoldPrint T);
    if dim(S * T) >= 0 and top(S * T) != S * T then (
        descr = descr||("Intersection of the surfaces: non-equidimensional scheme of dimension "|(toString dim (S * T)));
    ) else (
        -- if dim(S * T) >= 2 then descr = descr||("Intersection of the surfaces: "|(? ideal (S * T)));
        if dim(S * T) == 1 then descr = descr||("Intersection of the surfaces: curve of degree "|toString degree(S * T)|" and arithmetic genus "|toString sectionalGenus(S * T));
        if dim(S * T) <= 0 then descr = descr||("Intersection of the surfaces: "|(toString degree (S * T))|" points");
        if dim (S * T) >= 1 and degree(S * T) >= 2 then (
            if dim singularLocus(S * T) <= 0 then (
                m := degree support singularLocus(S * T);
                descr = descr||(net "Singular locus of the intersection: "|(if m == 0 then "∅" else (if m == 1 then "a single point" else (toString m)|" points")));
            ) else (
                descr = descr||(net "Singular locus of the intersection: "|(? ideal support singularLocus(S * T)));
            );
        );
    );
    if isPlaneInP5 T or isPlaneInP5 S then (
        idX := if substring(0,8,recognizeDSCF X) === "DSCF-V1-" then "   {ID: "|substring(8,recognizeDSCF X)|"}" else "";
        h := quadricFibration X;
        symb := if X.cache#"quadricFibrationCubicFourfoldInC8"_1 then "★ " else "☆ ";
        descr = descr||(symb|X.cache#"quadricFibrationCubicFourfoldInC8"_2);
        if X.cache#"quadricFibrationCubicFourfoldInC8"_1 then descr = descr||((K3statusLog X)|idX);
        if statusK3 X >= 1 then descr = descr || (describeMirrorFourfoldAndK3 X);
    );
    net expression descr
);

shortDescriptionFourfold = method();
shortDescriptionFourfold (DoubleSpecialCubicFourfold,Boolean) := (X,UseAttribute) -> (
    if UseAttribute and hasAttribute(X,ReverseDictionary) then return toString getAttribute(X,ReverseDictionary);
    Y := X.cache#"parentCubicFourfold";
    d1 := discriminant X;
    d2 := discriminant Y;
    Cd1Cd2 := "C_"|(toString d1);
    if d1 != d2 then Cd1Cd2 = Cd1Cd2|" ∩ C_"|(toString d2);
    "cubic fourfold in "|Cd1Cd2
);
shortDescriptionFourfold (SpecialCubicFourfold,Boolean) := (X,UseAttribute) -> (
    if UseAttribute and hasAttribute(X,ReverseDictionary) then return toString getAttribute(X,ReverseDictionary);
    "cubic fourfold in C_"|(toString discriminant X)
);
shortDescriptionFourfold DoubleSpecialCubicFourfold := shortDescriptionFourfold SpecialCubicFourfold := X -> shortDescriptionFourfold(X,true);

describeMirrorFourfoldAndK3 = method();
describeMirrorFourfoldAndK3 DoubleSpecialCubicFourfold := X -> (
    s := statusK3 X;
    if s <= 0 then return "";
    mu := fanoMapDSCF(X,Verbose=>true);  -- already in cache
    W := target mu;
    U := surfaceDeterminingInverseOfFanoMap(X,Verbose=>true);  -- already in cache
    assert(dim W == 4 and dim U == 2);
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
    Utilde := associatedUnderlyingK3Raw(X,Verbose=>true);  -- already in cache
    descr = descr | newline | "Minimal K3 surface Ũ: degree " | (toString degree Utilde) | " and sectional genus " | (toString sectionalGenus Utilde) | " in PP^" | (toString dim ambient Utilde) | (cutOutLine Utilde);
    if s <= 3 then return descr;
    pUtilde := polarizedK3surface(X,Verbose=>true);  -- already in cache
    descr || ((net "Lattice intersection matrix on Ũ: ") | (net latticeMatrix pUtilde))
);
describeMirrorFourfoldAndK3 SpecialCubicFourfold := X -> ""; -- not implemented yet

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

K3statusLog = X -> (
    s := statusK3 X;
    st := if      s == -1 then "░░░░░"
          else if s == 0  then "▓░░░░"
          else if s == 1  then "▓▓░░░"
          else if s == 2  then "▓▓▓░░"
          else if s == 3  then "▓▓▓▓░"
          else if s == 4  then "▓▓▓▓▓"
          else error "internal error: statusK3 returned an invalid value";
    "K3 status: " | "["|st|" / ▓▓▓▓▓]"
);

statusK3 = method();
statusK3 DoubleSpecialCubicFourfold := X -> (
    -- -1: Fano map not found
    --  0: surface U missing from cache: no computation performed
    --  1: Fano map and U computed; exceptional curves missing
    --  2: exceptional curves computed; associated K3 surface missing
    --  3: K3 surface Utilde computed; lattice polarization missing
    --  4: computation complete
    (S,P) := surfaces X;
    if not S.cache#?("FanoMapDSCF",P) then return -1;
    mu := S.cache#("FanoMapDSCF",P);
    if not mu.cache#?("surfaceDeterminingInverseOfFanoMap",X) then return 0;
    U := mu.cache#("surfaceDeterminingInverseOfFanoMap",X);
    if not U.cache#?"exceptionalCurves" then return 1;
    if not(mu.cache#?("AssociatedUnderlyingK3",X) and mu.cache#?("K3SurfaceFromDoubleSpecialCubicFourfold",X)) then return 2;
    E := mu.cache#("K3SurfaceFromDoubleSpecialCubicFourfold",X);
    if E#"LatticePolarization" === null or isVirtualLatticeK3 E then return 3;
    return 4;
);
statusK3 SpecialCubicFourfold := X -> -1; -- not implemented yet

genRingIntMatr3x3 = memoize(() -> (
    □ := local □;
    K := ZZ[□];
    first gens K
));

latticeIntersectionMatrix3x3 = method();
latticeIntersectionMatrix3x3 DoubleSpecialCubicFourfold := X -> (
    (S,T) := surfaces X;
    if X.cache#?(S,T,"LatticeIntersectionMatrix3x3") then return X.cache#(S,T,"LatticeIntersectionMatrix3x3");
    d1 := discriminant X;
    MS := X.cache#(S,"LatticeIntersectionMatrix");
    Y := X.cache#"parentCubicFourfold";
    d2 := discriminant Y;
    MT := Y.cache#(T,"LatticeIntersectionMatrix");
    h2sq := MS_(0,0);   -- (H^2)^2 = deg X (= 3)
    h2S  := MS_(0,1);   -- H^2 * S = deg S
    S2   := MS_(1,1);   -- S^2
    h2T  := MT_(0,1);   -- H^2 * T = deg T
    T2   := MT_(1,1);   -- T^2
    ST := if X.cache#?(S,T,"intersection of surface cycles in cubic fourfold")
          then X.cache#(S,T,"intersection of surface cycles in cubic fourfold")
          else if dim(S * T) <= 0
          then degree(S * T)
          else genRingIntMatr3x3();
    A := matrix {
        {h2sq, h2S,  h2T},
        {h2S,  S2,   ST },
        {h2T,  ST,   T2 }
    };
    if ring A === ZZ then X.cache#(S,T,"LatticeIntersectionMatrix3x3") = A;
    A
);

deformViaDoubleLiaison = method(Options => {Verbose => true, Verify => true});
deformViaDoubleLiaison (ZZ,EmbeddedProjectiveVariety) := o -> (e,S) -> (
    if o.Verbose then << "-- initial ideal generators degrees: " << toStringDegreesVar S << endl;
    c := codim S;
    if number(flatten degrees ideal S, d -> d <= e) <= c then error("not enough freedom to deform via " | toString(c:e) | " liaison");
    S' := random({c:{e}}, S) \ S;
    if o.Verbose then << "-- first " << toString(c:e) << " liaison step: " << toStringDegreesVar S' << endl;
    if number(flatten degrees ideal S', d -> d <= e) <= c then error "secondary liaison is trivial: not enough freedom to deform";
    S'' := random({c:{e}}, S') \ S';
    if degrees S'' =!= degrees S then error "liaison failed to preserve degrees";
    if o.Verify then (
        if hilbertPolynomial S != hilbertPolynomial S'' then error "liaison failed to preserve Hilbert polynomial";
        if not isSmooth S'' then error "deformation not smooth";
    );
    S''
);

surfaceIntersectionNumber = method(Options => {Verbose => true, Verify => true, "AttemptComputation" => true});
surfaceIntersectionNumber DoubleSpecialCubicFourfold := o -> X -> (
    (S,T) := surfaces X;
    if X.cache#?(S,T,"intersection of surface cycles in cubic fourfold") then return X.cache#(S,T,"intersection of surface cycles in cubic fourfold");
    if not o#"AttemptComputation" then return genRingIntMatr3x3();
    ST := S + T;
    e := 0;
    if number(flatten degrees ideal ST, d -> d <= 2) >= 4 then e = 2
    else if number(flatten degrees ideal ST, d -> d <= 3) >= 4 then e = 3;
    if e == 0 then return genRingIntMatr3x3();
    if o.Verbose then << "-- trying to compute surface cycle intersection via " << (e,e,e) << " liaison" << endl;
    try ST'' := deformViaDoubleLiaison(e,ST,Verbose=>o.Verbose,Verify=>o.Verify) then (
        if o.Verbose then (
            if o.Verify then << "-- smooth deformation of the union of the surfaces obtained" << endl
            else << "-- deformation of the union of the surfaces obtained" << endl;
        );
        Z := specialCubicFourfold(ST'',Verbose=>false);
        discriminant Z;
        selfIntST := first Z.cache#(ST'',"discriminantFourfold");
        if o.Verbose then << "-- discriminant of the cubic fourfold containing the deformed surface: " << discriminant Z << endl;
        discriminant X;
        selfIntS := first X.cache#(S,"discriminantFourfold");
        Y := X.cache#"parentCubicFourfold";
        discriminant Y;
        selfIntT := first Y.cache#(T,"discriminantFourfold");
        a := lift((selfIntST - selfIntS - selfIntT)/2, ZZ);
        if o.Verbose then << "-- surface cycles intersection value: " << a << endl;
        return X.cache#(S,T,"intersection of surface cycles in cubic fourfold") = a;
    ) else (
        if o.Verbose then << "-- liaison " << (e,e,e) << " did not yield a suitable deformation" << endl;
        return genRingIntMatr3x3();
    );
);

random DoubleSpecialCubicFourfold := o -> X -> (
    (S,T) := surfaces X;
    Y := specialCubicFourfold(S & T,InputCheck=>-1);
    if isFanoMapStandard X then setFanoMapToBeStandard Y;
    if isFanoMapToP2xP2 X then setFanoMapToBeMapFromP5toP2xP2 Y;
    Y
);

clean DoubleSpecialCubicFourfold := X -> (
    (S,T) := surfaces X;
    K := coefficientRing X;
    x := local x;
    R := K[x_0..x_5];
    S' := Var sub(sub(ideal S,vars R),vars ring ambient X);
    if S.cache#?"euler" then S'.cache#"euler" = S.cache#"euler";
    T' := Var sub(sub(ideal T,vars R),vars ring ambient X);
    if T.cache#?"euler" then T'.cache#"euler" = T.cache#"euler";
    X' := Var sub(sub(ideal X,vars R),vars ring ambient X);
    nS := if S.cache#?"FiniteNumberOfNodes" then S.cache#"FiniteNumberOfNodes" else null;
    nT := if T.cache#?"FiniteNumberOfNodes" then T.cache#"FiniteNumberOfNodes" else null;
    specialCubicFourfold(S' & T',X',InputCheck=>0,NumNodes=>(nS,nT))
);

swap = method();
swap DoubleSpecialCubicFourfold := X -> (
    if X.cache#?"swappedSurfaces" then return X.cache#"swappedSurfaces";
    (S,T) := surfaces X;
    K := coefficientRing X;
    x := local x;
    R := K[x_0..x_5];
    idX := sub(sub(ideal X,vars R),vars ring ambient X);
    nS := if S.cache#?"FiniteNumberOfNodes" then S.cache#"FiniteNumberOfNodes" else null;
    nT := if T.cache#?"FiniteNumberOfNodes" then T.cache#"FiniteNumberOfNodes" else null;
    Y := specialCubicFourfold(T & S,Var idX,InputCheck=>0,NumNodes=>(nT,nS),Verbose=>false);
    X.cache#"swappedSurfaces" = Y;
    Y.cache#"swappedSurfaces" = X;
    Y
);

fuse = method();
fuse DoubleSpecialCubicFourfold := X -> (
    if X.cache#?"fusedVersion" then return X.cache#"fusedVersion";
    (S,T) := surfaces X;
    K := coefficientRing X;
    x := local x;
    R := K[x_0..x_5];
    idX := sub(sub(ideal X,vars R),vars ring ambient X);
    Y := specialCubicFourfold(S+T,Var idX,InputCheck=>0,NumNodes=>0,Verbose=>false);
    X.cache#"fusedVersion" = Y;
    Y.cache#"fusedOrigin" = X;
    Y
);

unfuse = method();
unfuse SpecialCubicFourfold := X -> (
    if X.cache#?"fusedOrigin" then return X.cache#"fusedOrigin";
    ST := surface X;
    decST := decompose ST;
    if # decST != 2 then error("unfuse: expected a surface with exactly 2 components, but found " | toString(#decST));
    K := coefficientRing X;
    x := local x;
    R := K[x_0..x_5];
    idX := sub(sub(ideal X,vars R),vars ring ambient X);
    Z := specialCubicFourfold(decST_0 & decST_1,Var idX,InputCheck=>1,Verbose=>false);
    Z.cache#"fusedVersion" = X;
    X.cache#"fusedOrigin" = Z;
    Z
);

quadricFibration DoubleSpecialCubicFourfold := o -> X -> (
    if X.cache#?"quadricFibrationCubicFourfoldInC8" then return first X.cache#"quadricFibrationCubicFourfoldInC8";
    (T,P) := surfaces X;
    if not isPlaneInP5 P then (P,T) = (T,P);
    if not isPlaneInP5 P then error "one of the two surfaces is required to be a plane";
    h := quadricFibration(rationalMap(P_X),Verify=>false);
    if not(codim target h == 0 and dim target h == 2) then error "something went wrong in the construction of the quadric fibration, target != PP^2";
    F := h^* point target h;
    if not(dim F == 2 and degree F == 2) then error "something went wrong in the construction of the quadric fibration, the generic fiber is not a quadric surface";
    Z := (F * T)\\P;
    assert(dim Z <= 1);
    resFib := "";
    if dim Z == 1 then resFib = "The generic quadric fiber meets the other surface residually along a curve";
    if dim Z == 0 and degree Z != 1 then resFib = "The generic quadric fiber meets the other surface residually in "|(toString degree Z)|" points";
    if dim Z == 0 and degree Z == 1 then resFib = "The generic quadric fiber meets the other surface residually in a single point";
    if dim Z == -1 then resFib = "The generic quadric fiber meets the other surface residually in the empty set";
    X.cache#"quadricFibrationCubicFourfoldInC8" = (h, dim Z == 0 and degree Z == 1, resFib);
    first X.cache#"quadricFibrationCubicFourfoldInC8"
);

------------------------------------------------------------------------
--- Lattice-polarized K3 surfaces from double special cubic fourfolds --
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

recoverFourfold = method();
recoverFourfold LatticePolarizationOnK3Surface := S -> recoverFourfold S#"SurfaceAssociatedToRationalFourfold";
recoverFourfold SurfaceAssociatedToRationalFourfold := S -> S.cache#"Hodge-special fourfold";

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
        "SurfaceAssociatedToRationalFourfold" => S,
        "specialCurve" => null,
        "latticeMatrix" => M,
        "isVirtual" => true
    }
);

LatticePolarizationOnK3Surface Sequence := (S,ab) -> (
    if not(#ab == 2 and instance(first ab,ZZ) and instance(last ab,ZZ)) then error "expected a sequence of two integers";
    (a,b) := ab;
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

K3SurfaceFromDoubleSpecialCubicFourfold = new Type of MutableHashTable;
globalAssignment K3SurfaceFromDoubleSpecialCubicFourfold;
K3SurfaceFromDoubleSpecialCubicFourfold.synonym = "K3 surface";

net K3SurfaceFromDoubleSpecialCubicFourfold := S -> (
    out := "Fourfold: ";
    if hasAttribute(recoverFourfold S,ReverseDictionary) then out = out|(toString getAttribute(recoverFourfold S,ReverseDictionary))|", ";
    out = (out|(shortDescriptionFourfold(recoverFourfold S,false)))||(describeMirrorFourfoldAndK3 recoverFourfold S);
    if S#"LatticePolarization" === null then return (out|| "Lattice polarization: not yet computed; use 'polarize' or 'polarizedK3surface'");
    M := latticeMatrix S;
    if isVirtualLatticeK3 S and statusK3 S <= 3 then out = out||("Lattice intersection matrix (virtual, computed from U): "|(net M));
    out
);
texMath K3SurfaceFromDoubleSpecialCubicFourfold := texMath @@ net;

K3SurfaceFromDoubleSpecialCubicFourfold#{WebApp,AfterPrint} =
K3SurfaceFromDoubleSpecialCubicFourfold#{WebApp,AfterNoPrint} =
K3SurfaceFromDoubleSpecialCubicFourfold#{Standard,AfterPrint} =
K3SurfaceFromDoubleSpecialCubicFourfold#{Standard,AfterNoPrint} = S -> (
    << endl << concatenate(interpreterDepth:"o") << lineNumber << " : " << "Lattice-polarized K3 surface associated to " << (shortDescriptionFourfold recoverFourfold S) << " — " << K3statusLog(S) << endl;
);

underlyingK3 = method();
underlyingK3 K3SurfaceFromDoubleSpecialCubicFourfold := S -> S#"UnderlyingK3";

projectiveVariety K3SurfaceFromDoubleSpecialCubicFourfold := o -> S -> (
    if dim underlyingK3 S == -1 then << "-- warning: underlying K3 surface not fully computed (incomplete data)" << endl;
    underlyingK3 S
);

latticePolarization = method();
latticePolarization K3SurfaceFromDoubleSpecialCubicFourfold := S -> (
    if S#"LatticePolarization" === null then error "lattice polarization not yet computed; use 'polarize' or 'polarizedK3surface'";
    S#"LatticePolarization"
);

building K3SurfaceFromDoubleSpecialCubicFourfold := S -> building underlyingK3 S;
latticeMatrix K3SurfaceFromDoubleSpecialCubicFourfold := S -> latticeMatrix latticePolarization S;
recoverFourfold K3SurfaceFromDoubleSpecialCubicFourfold := S -> recoverFourfold underlyingK3 S;
statusK3 K3SurfaceFromDoubleSpecialCubicFourfold := S -> statusK3 recoverFourfold S;

isVirtualLatticeK3 = method();
isVirtualLatticeK3 K3SurfaceFromDoubleSpecialCubicFourfold := S -> S#"LatticePolarization" =!= null and (S#"LatticePolarization")#"isVirtual";

polarizedK3surface = method(TypicalValue => K3SurfaceFromDoubleSpecialCubicFourfold, Options => {Verbose => false, Strategy => null, FanoMapType => null});
polarizedK3surface DoubleSpecialCubicFourfold := o -> X -> (
    if not instance(o.Verbose,Boolean) then error "expected a Boolean value for option 'Verbose'";
    local StrK3; local StrPol; local S;
    StrK3Set := {"Inverse","Approximate"};
    StrPolSet := {null, "SpecialCurve", "MapFromW", "MapFromU", "MapFromW-Virtual", "MapFromU-Virtual"};
    if member(o.Strategy,StrPolSet)
    then (StrK3,StrPol) = (null,o.Strategy)
    else if member(o.Strategy,StrK3Set) then (StrK3,StrPol) = (o.Strategy,null)
    else if instance(o.Strategy,VisibleList) and # o.Strategy == 2 and member(first o.Strategy, StrK3Set) and member(last o.Strategy, StrPolSet) then (StrK3,StrPol) = toSequence o.Strategy
    else error("polarizedK3surface: invalid Strategy; expected one of {\"SpecialCurve\", \"MapFromW\", \"MapFromU\", \"MapFromW-Virtual\", \"MapFromU-Virtual\"}, or {\"Inverse\", \"Approximate\"}, or a pair of these");
    if not member(o.FanoMapType,{null,"Standard","P2xP2"}) then error("polarizedK3surface: invalid FanoMapType '" | toString(o.FanoMapType) | "'; expected one of {\"Standard\", \"P2xP2\"}");
    mu := getCachedFanoMapIfCompatible(X,o.FanoMapType);
    if mu === null or (not mu.cache#?("K3SurfaceFromDoubleSpecialCubicFourfold",X)) then (
        U := associatedUnderlyingK3Raw(X, Verbose=>o.Verbose, Strategy=>StrK3, FanoMapType=>o.FanoMapType);
        assert(mu === null or mu === first building U);
        mu = first building U;
        if mu.cache#?("K3SurfaceFromDoubleSpecialCubicFourfold",X) then (
            S = mu.cache#("K3SurfaceFromDoubleSpecialCubicFourfold",X);
        ) else (
            S = new K3SurfaceFromDoubleSpecialCubicFourfold from {
                symbol cache => new CacheTable,
                "UnderlyingK3" => U,
                "LatticePolarization" => null
            };
            mu.cache#("K3SurfaceFromDoubleSpecialCubicFourfold",X) = S;
        );
        S.cache#"userStrategyPolarization" = StrPol;
        return S;
    );
    S = mu.cache#("K3SurfaceFromDoubleSpecialCubicFourfold",X);
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

polarizedK3surface K3SurfaceFromDoubleSpecialCubicFourfold := o -> S -> polarizedK3surface(recoverFourfold S,Verbose=>o.Verbose,Strategy=>o.Strategy,FanoMapType=>o.FanoMapType);
polarizedK3surface SurfaceAssociatedToRationalFourfold := o -> S -> (
    X := recoverFourfold S;
    if not instance(X,DoubleSpecialCubicFourfold) then error "K3 surface is expected to be associated to a double special cubic fourfold";
    polarizedK3surface(recoverFourfold S,Verbose=>o.Verbose,Strategy=>o.Strategy,FanoMapType=>o.FanoMapType)
);

polarize K3SurfaceFromDoubleSpecialCubicFourfold := o -> S -> polarizedK3surface S;

associatedK3surface DoubleSpecialCubicFourfold := o -> X -> polarizedK3surface(X,Verbose=>o.Verbose,Strategy=>o.Strategy);

------------------------------------------------------------------------
------------------- Fano maps for D. S. C. F. --------------------------
------------------------------------------------------------------------

fanoMapDSCF = method(Options => {Verify => true, Verbose => true});
fanoMapDSCF DoubleSpecialCubicFourfold := o -> X -> (
    (S,T) := surfaces X;
    if S.cache#?("FanoMapDSCF",T) then return S.cache#("FanoMapDSCF",T);
    if isFanoMapStandard X then return S.cache#("FanoMapDSCF",T) = fanoMapDSCFstandard(X,Verify=>o.Verify,Verbose=>o.Verbose);
    if isFanoMapToP2xP2 X then return S.cache#("FanoMapDSCF",T) = fanoMapDSCFtoP2xP2(X,Verify=>o.Verify,Verbose=>o.Verbose);
    -- error "no Fano map type detected";
    setFanoMapToBeStandard(X,Verbose=>o.Verbose);
    fanoMapDSCF(X,Verify=>o.Verify,Verbose=>o.Verbose)
);

fanoMapDSCFstandard = method(Options => {Verify => true, Verbose => true});
fanoMapDSCFstandard (DoubleSpecialCubicFourfold,ZZ,ZZ,ZZ) := o -> (X,d,a,b) -> (
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

fanoMapDSCFstandard DoubleSpecialCubicFourfold := o -> X -> (
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
fanoMapDSCFtoP2xP2 DoubleSpecialCubicFourfold := o -> X -> (
    (S,T) := surfaces X;
    if S.cache#?("FanoMapDSCFtoP2xP2",T) then return S.cache#("FanoMapDSCFtoP2xP2",T);
    if o.Verbose then << "-- constructing the map PP^5 --> PP^2 x PP^2 via abstract join" << endl << flush;
    (f,g) := abstractJoinOfRationalSurfaces(S,T,Verbose=>o.Verbose);
    if o.Verbose then << "-- computing the inverse of the second projection..." << endl;
    -- BUG: inverse(g,Verify=>true) in MultiprojectiveVarieties; at line "if image Phi != target Phi then (Phi#"isDominant" = false; Phi#"isBirational" = false; error "the multi-rational map is not dominant");" ambient spaces seem different. Not reproducible by converting g to string.
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
    -- psi := rationalMap(last projectionMaps JJ,target f); -- this causes the bug below
    psi := rationalMap(last projectionMaps JJ); psi = psi * rationalMap(target psi,target f);
    eta := rationalMap((projectionMaps JJ)_0 | (projectionMaps JJ)_1, PP_K^{2,2});
    S.cache#("abstract join of rational surfaces",P) = (eta,psi)
);

setFanoMapToBeMapFromP5toP2xP2 = method(Options => {Verbose => true});
setFanoMapToBeMapFromP5toP2xP2 DoubleSpecialCubicFourfold := o -> X -> (
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
setFanoMapToBeStandard DoubleSpecialCubicFourfold := o -> X -> (
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
getCachedFanoMapIfCompatible (DoubleSpecialCubicFourfold,Thing) := (X,FanoMapTypeStr) -> (
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

fanoMap DoubleSpecialCubicFourfold := o -> X -> toRationalMap fanoMapDSCF(X,Verify=>o.RaiseError,Verbose=>o.Verbose); -- for compatibility

------------------------------------------------------------------------
------------------------------------------------------------------------

surfaceDeterminingInverseOfFanoMap DoubleSpecialCubicFourfold := o -> X -> (
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
        topB := interpolateTop(B,Verbose=>verbosityDSCFtoInterpolateTop(o.Verbose),cache=>true);
        U = B \ topB;
        if dim U != 2 then error "extraction failed: dimension of (B \\ top(B)) is not 2";
    );
    assert(dim U == 2);
    if not isSurfaceUknownToBeAlreadyEquidimensional(X,mu) then (
        if o.Verbose then << "-- surface found in base locus; verifying equidimensionality..." << endl;
        tU := interpolateTop(U,Verbose=>verbosityDSCFtoInterpolateTop(o.Verbose),cache=>true);
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
    W := apply(Z1, D -> support interpolateTop(U * (pr^* D), Verbose=>verbosityDSCFtoInterpolateTop(verb),cache=>true));
    assert all(W, D -> dim D == 2 and degree D <= 3);
    vU := U;
    for D in W do vU = vU \\ D;
    assert(dim vU == 2);
    Curves := apply(select(apply(W, D -> vU * D), C -> dim C == 1), E -> support interpolateTop(E, Verbose=>verbosityDSCFtoInterpolateTop(verb),cache=>true));
    Curves = apply(sort apply(Curves, C -> (degree C,C)), last);
    if # Curves > 0 then vU.cache#"special curves on U" = Curves;
    vU
);

getInverseFanoMap = method();
getInverseFanoMap SurfaceAssociatedToRationalFourfold := Utilde -> (
    (mu,U,C,f) := building Utilde;
    if not U.cache#?"birational maps from X to W and from W to X" then error "birational maps between cubic fourfold X and mirror fourfold W not found in cache; polarizedK3surface may have been computed using an unsuitable strategy";
    last U.cache#"birational maps from X to W and from W to X"
);
getInverseFanoMap K3SurfaceFromDoubleSpecialCubicFourfold := Utilde -> getInverseFanoMap underlyingK3 Utilde;
getInverseFanoMap DoubleSpecialCubicFourfold := X -> (
    (S,P) := surfaces X;
    if not S.cache#?("FanoMapDSCF",P) then error "Fano map not found in cache; polarizedK3surface or mirrorFourfold must be called first";
    mu := fanoMapDSCF(X,Verbose=>false);
    if not mu.cache#?("surfaceDeterminingInverseOfFanoMap",X) then error "inverse of Fano map not found in cache; polarizedK3surface or mirrorFourfold must be called first";
    U := surfaceDeterminingInverseOfFanoMap(X,Verbose=>false);
    if not U.cache#?"birational maps from X to W and from W to X" then error "birational maps between cubic fourfold X and mirror fourfold W not found in cache; polarizedK3surface may have been computed using an unsuitable strategy";
    last U.cache#"birational maps from X to W and from W to X"
);

mirrorFourfold DoubleSpecialCubicFourfold := o -> X -> (
    mu := fanoMapDSCF(X,Verbose=>o.Verbose);
    if mu.cache#?("mirrorFourfold",X) then return mu.cache#("mirrorFourfold",X);
    U := surfaceDeterminingInverseOfFanoMap(X,Verbose=>o.Verbose,Strategy=>o.Strategy);
    W := specialFourfold(U,target mu,InputCheck=>0);
    W.cache#(U,"associatedFourfold") = X;
    mu.cache#("mirrorFourfold",X) = W
);

exceptionalCurves DoubleSpecialCubicFourfold := o -> X -> (
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
                << "-- warning: dim(U ∩ U') = 2: 'polarizedK3surface' may not complete as expected." << endl
            ) else (
                << "-- obtained dim(U ∩ U') = 2" << endl
            );
        );
        return U.cache#"exceptionalCurves" = ((0_U)%U,(0_U)%U);
    );
    if o.Verbose then << endl << "-- extracting the 1-dimensional part of the intersection U ∩ U'..." << endl;
    E := interpolateTop(U * U',Verbose=>verbosityDSCFtoInterpolateTop(o.Verbose),cache=>true);
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
    LinesInE = apply(LinesInE, D -> interpolateTop(D,Verbose=>verbosityDSCFtoInterpolateTop(o.Verbose),cache=>true));
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

------------------------------------------------------------------------
------------------ Associated polarized K3 (raw data) ------------------
------------------------------------------------------------------------

associatedUnderlyingK3Raw = method(TypicalValue => SurfaceAssociatedToRationalFourfold, Options => {Verbose => true, Strategy => null, FanoMapType => null});
associatedUnderlyingK3Raw DoubleSpecialCubicFourfold := o -> X -> (
    mu := getCachedFanoMapIfCompatible(X,o.FanoMapType);
    if mu =!= null and mu.cache#?("AssociatedUnderlyingK3",X) then return mu.cache#("AssociatedUnderlyingK3",X);
    if o.Verbose and (not any(surfaces X, isPlaneInP5)) then (
        << "------------------------------------------------------------------------" << endl
        << "-- warning: polarizedK3surface(DoubleSpecialCubicFourfold) is designed  " << endl
        << "--          for the case where one of the two surfaces is a plane.      " << endl
        << "--          Attempting to find the K3 surface anyway...                 " << endl
        << "------------------------------------------------------------------------" << endl;
    );
    if o.Verbose then (
        tK3start := currentTime();
        tK3startCPU := cpuTime();
        printFinalLogK3 := T -> (
            tK3end := currentTime() - tK3start;
            tK3endCPU := cpuTime() - tK3startCPU;
            T.cache#"computationTime" = (tK3end, tK3endCPU);
            statusOut := if dim T == 2 and isStandardK3surface T
                         then (" ✦ ", "successfully completed")
                         else if dim T == 2
                         then (" ⟐ ", "completed")
                         else (" ✧ ", "completed (partial data)");
            << first statusOut << "underlying K3 " << last statusOut << " in " << humanReadableSeconds(tK3end) << " (cpu: " << humanReadableSeconds(floor tK3endCPU) << ")" << endl;
        );
        << "-- starting underlying K3 computation" << endl;
        (S,P) := surfaces X;
        if isPlaneInP5 P then (
            nodalStr := if S.cache#?"FiniteNumberOfNodes" and S.cache#"FiniteNumberOfNodes" > 0 then (toString S.cache#"FiniteNumberOfNodes")|"-nodal " else "";
            << "-- input: cubic fourfold containing a " << nodalStr << "surface of degree " << degree S << " and sectional genus " << sectionalGenus S << " and a plane" << endl;
        ) else (
            << "-- input: cubic fourfold containing two surfaces of degrees " << degree S << " and " << degree P << ", sectional genera " << sectionalGenus S << " and " << sectionalGenus P << endl;
        );
        << "-- settings: Verbose => " << o.Verbose << ", Strategy => " << (if instance(o.Strategy, String) then "\"" | o.Strategy | "\"" else toString(o.Strategy)) << ", FanoMapType => " << (if instance(o.FanoMapType, String) then "\"" | o.FanoMapType | "\"" else toString(o.FanoMapType)) << endl;
        << "-- available strategies: \"Inverse\", \"Approximate\"" << endl;
        << "-- planned steps:" << endl;
        << "-- 1. compute Fano map μ : ℙ⁵ ⇢ W" << endl;
        << "-- 2. extract surface U from the base locus of (μ|X)⁻¹ : W ⇢ X" << endl;
        << "-- 3. take a general cubic X' containing the two surfaces and extract U'" << endl;
        << "-- 4. analyze the intersection U ∩ U' for exceptional curves" << endl;
        << "-- 5. compute the contraction map f : U -> Ũ" << endl;
        << "-- 6. prepare data for lattice polarization" << endl;
        << "-- info: re-run the function for lattice computation and use building() to access" << endl;
        << "-- construction data (μ, U, exceptional curves, f)" << endl;
        <<endl;
    );
    if mu === null then (
        if o.FanoMapType === "Standard" then setFanoMapToBeStandard(X,Verbose=>o.Verbose);
        if o.FanoMapType === "P2xP2" then setFanoMapToBeMapFromP5toP2xP2(X,Verbose=>o.Verbose);
        mu = fanoMapDSCF(X,Verbose=>o.Verbose);
    );
    if mu.cache#?("AssociatedUnderlyingK3",X) then return mu.cache#("AssociatedUnderlyingK3",X);
    if (not isPlaneInP5 P) and X.cache#?"fusedVersion" and (surface fuse X).cache#?("fanoMap",ambientFivefold fuse X) then (
        if ((fanoMap fuse X)#"map").cache#"multiplicityFanoMap" >= 2 then (
            if o.Verbose then (
                << "-------------------------------------------------------------------" << endl;
                << "-- info: special configuration detected.                           " << endl;
                << "--       redirecting computation to associatedK3surface(fuse X)... " << endl;
                << "-------------------------------------------------------------------" << endl;
            );
            UtildeFuse := associatedK3surface(fuse X,Verbose=>o.Verbose,Strategy=>"F4");
            if o.Verbose then printFinalLogK3 UtildeFuse;
            return UtildeFuse;
        );
    );
    (L,C) := exceptionalCurves(X,Verbose=>o.Verbose,Strategy=>setStrategyDSCFtoK3(X,o.Strategy));
    U := ambientVariety L;
    if U.cache#?"MapToMinimalK3Surface" then return mu.cache#("AssociatedUnderlyingK3",X) = makeSurfaceAssociated(X,mu,U,{L,C},U.cache#"MapToMinimalK3Surface");
    f := buildContractionMap(U,L,C,X,isNormalizationKnownToTerminateQuickly(X),setStrategyDSCFtoK3(X,o.Strategy),o.Verbose);
    Utilde := makeSurfaceAssociated(X,mu,U,{L,C},f);
    if f =!= null then mu.cache#("AssociatedUnderlyingK3",X) = Utilde;
    if o.Verbose then printFinalLogK3 Utilde;
    Utilde
);

buildContractionMap = (U,L,C,X,withNorm,Str,Verb) -> (
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
    Str := U.cache#"strategy for surface U";
    f = buildContractionMap(U,L,C,X,true,Str,o.Verbose);
    Utilde.cache#"attemptedMinimalK3ViaNormalization" = null;
    if f === null then return Utilde;
    return mu.cache#("AssociatedUnderlyingK3",X) = makeSurfaceAssociated(X,mu,U,{L,C},f);
);

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
    Utilde = buildMinimalK3ViaNormalization(Utilde,Verbose=>o.Verbose);
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
    T := latticePolarizationOnK3Surface(Utilde,specialCurveK3,Verbose=>o.Verbose,Verify=>true);
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
    sE1 := interpolateTop(E1,Verbose=>verbosityDSCFtoInterpolateTop(PolarizeVerbosity));
    if PolarizeVerbosity and sE1 != E1 then << "  -- unexpected non-pure dimensional components were found" << endl;
    if PolarizeVerbosity then << "  -- obtained the first curve on U: " << ? sE1 << endl << "  -- computing image on K3 surface..." << endl;
    sE1onK3 := f sE1;
    if dim sE1onK3 != 1 then error "surface polarization calculation failed: image on K3 is not a curve";
    if PolarizeVerbosity and degree sE1onK3 != degree Utilde then << "  -- unexpected degree for f(p1^*(H_PP^2)): " << degree sE1onK3 << " (expected " << degree Utilde << ")" << endl;
    if PolarizeVerbosity then << "  -- image curve: " << ? sE1onK3 << endl;
    if PolarizeVerbosity then << "-- computing p2^*(H_PP^2)" << endl << flush;
    E2 := psi2^* random(1,0_(target psi2));
    if dim E2 != 1 then error "surface polarization calculation failed, expected dimension 1 for p2^*(H_PP^2)";
    sE2 := interpolateTop(E2,Verbose=>verbosityDSCFtoInterpolateTop(PolarizeVerbosity));
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
    sE2 := interpolateTop(E2U,Verbose=>verbosityDSCFtoInterpolateTop(PolarizeVerbosity));
    if PolarizeVerbosity and sE2 != E2U then << "  -- unexpected non-pure dimensional components were found" << endl;
    if PolarizeVerbosity then << "  -- obtained the curve on U: " << ? sE2 << endl << "  -- computing image on K3 surface..." << endl;
    sE2onK3 := f sE2;
    if dim sE2onK3 != 1 then error "surface polarization calculation failed: image on K3 is not a curve";
    if PolarizeVerbosity then << "  -- image curve: " << ? sE2onK3 << endl << flush;
    sE2onK3
);

mapFromWtoP2xP2 = method(Options => {Verbose => true});
mapFromWtoP2xP2 DoubleSpecialCubicFourfold := o -> X -> (
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
mapFromWtoP2xP2 K3SurfaceFromDoubleSpecialCubicFourfold := o -> S -> mapFromWtoP2xP2(underlyingK3 S,Verbose=>o.Verbose);

mapFromUtoP2xP2 = method(Options => {Verbose => true});
mapFromUtoP2xP2 DoubleSpecialCubicFourfold := o -> X -> (
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
mapFromUtoP2xP2 K3SurfaceFromDoubleSpecialCubicFourfold := o -> S -> mapFromUtoP2xP2(underlyingK3 S,Verbose=>o.Verbose);

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
    sE1 := interpolateTop(E1,Verbose=>verbosityDSCFtoInterpolateTop(o.Verbose));
    if o.Verbose and sE1 != E1 then << "  -- unexpected non-pure dimensional components were found" << endl;
    if o.Verbose then << "  -- obtained p1^*(H_PP^2): " << ? sE1 << endl;
    if o.Verbose then << "-- computing another p1^*(H_PP^2)" << endl << flush;
    E2:= psi1^* random(1,0_(target psi1));
    if o.Strategy === "MapFromW-Virtual" then E2 = E2 * U;
    if dim E2 != 1 then error "surface polarization calculation failed, expected dimension 1 for p1^*(H_PP^2)";
    sE2 := interpolateTop(E2,Verbose=>verbosityDSCFtoInterpolateTop(o.Verbose));
    if o.Verbose and sE2 != E2 then << "  -- unexpected non-pure dimensional components were found" << endl;
    if o.Verbose then << "  -- second p1^*(H_PP^2) obtained: " << ? sE2 << endl;
    sE1sE2 := sE1*sE2;
    if dim sE1sE2 > 0 then (
        if o.Verbose then <<" -- dim((p1^*(H)) * p1^*(H')) > 0, removing fixed components" << endl;
        sE1sE2 = sE1sE2\\interpolateTop(sE1sE2,Verbose=>verbosityDSCFtoInterpolateTop(o.Verbose));
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
    sC1 := interpolateTop(C1,Verbose=>verbosityDSCFtoInterpolateTop(o.Verbose));
    if o.Verbose and sC1 != C1 then << "  -- unexpected non-pure dimensional components were found" << endl;
    if o.Verbose then << "  -- obtained p2^*(H_PP^2): " << ? sC1 << endl;
    sE1sC1 := sE1*sC1;
    if dim sE1sC1 > 0 then (
        if o.Verbose then <<" -- dim((p1^*(H)) * p2^*(H)) > 0, removing fixed components" << endl;
        sE1sC1 = sE1sC1\\interpolateTop(sE1sC1,Verbose=>verbosityDSCFtoInterpolateTop(o.Verbose));
    );
    if dim sE1sC1 != 0 then error "(residual) intersection (p1^*(H)) * p2^*(H)) has dimension != 0";
    sE1sC1 = degree sE1sC1;
    if o.Verbose then << "-- computing another p2^*(H_PP^2)" << endl << flush;
    C2:= psi2^* random(1,0_(target psi2));
    if o.Strategy === "MapFromW-Virtual" then C2 = C2 * U;
    if dim C2 != 1 then error "surface polarization calculation failed, expected dimension 1 for p2^*(H_PP^2)";
    sC2 := interpolateTop(C2,Verbose=>verbosityDSCFtoInterpolateTop(o.Verbose));
    if o.Verbose and sC2 != C2 then << "  -- unexpected non-pure dimensional components were found" << endl;
    if o.Verbose then << "  -- second p2^*(H_PP^2) obtained: " << ? sC2 << endl;
    sC1sC2 := sC1*sC2;
    if dim sC1sC2 > 0 then (
        if o.Verbose then <<" -- dim((p2^*(H)) * p2^*(H')) > 0, removing fixed components" << endl;
        sC1sC2 = sC1sC2\\interpolateTop(sC1sC2,Verbose=>verbosityDSCFtoInterpolateTop(o.Verbose));
    );
    if dim sC1sC2 != 0 then error "(residual) intersection (p2^*(H)) * p2^*(H')) has dimension != 0";
    sC1sC2 = degree sC1sC2;
    if o.Verbose then << flush;
    virtK3 := latticePolarizationOnK3Surface(Utilde,sE1sE2,sE1sC1,sC1sC2);
    if o.Verbose then printFinalLog withoutWarning;
    virtK3
);

------------------------------------------------------------------------
----------- Recognition and auxiliary utilities for D. S. C. F. --------
------------------------------------------------------------------------

-* -- recognizeDSCF --
debug SpecialFanoFourfolds;
getInv = i -> (X := exampleDSCFourfoldC8(i,ZZ/65521); (S,T) := surfaces X; C := S * T; invX := (degrees S,degree S,sectionalGenus S,euler hilbertPolynomial S,eulerCharacteristic S,numberNodes S, degrees T,degree T,sectionalGenus T,euler hilbertPolynomial T,eulerCharacteristic T,numberNodes T,degrees C,degree C,degrees(S + T)); "if invX === "|(toString invX)|" then return X.cache#(S,T,\"labelDSCF\") = \"DSCF-V1-"|toString(i)|"\"");
getInvTot = () -> (L := ""; for i from 1 to 40 do L = L|newline|getInv(i); L);
*-
recognizeDSCF = X -> (
    (S,T) := surfaces X;
    if X.cache#?(S,T,"labelDSCF") then return X.cache#(S,T,"labelDSCF");
    C := S * T;
    invX := (degrees S,degree S,sectionalGenus S,euler hilbertPolynomial S,eulerCharacteristic S,numberNodes S,
             degrees T,degree T,sectionalGenus T,euler hilbertPolynomial T,eulerCharacteristic T,numberNodes T,
             degrees C,degree C,degrees(S + T));
    if invX === ({({2},4)},6,2,1,10,0,{({1},3)},1,0,1,3,0,{({1},3), ({2},1)},2,{({2},3), ({3},1)}) then return X.cache#(S,T,"labelDSCF") = "DSCF-V1-1";
    if invX === ({({2},5)},5,1,1,7,0,{({1},3)},1,0,1,3,0,{({1},3), ({2},1)},2,{({2},4)}) then return X.cache#(S,T,"labelDSCF") = "DSCF-V1-2";
    if invX === ({({2},3), ({3},1)},7,3,1,12,0,{({1},3)},1,0,1,3,0,{({1},3), ({3},1)},3,{({2},3)}) then return X.cache#(S,T,"labelDSCF") = "DSCF-V1-3";
    if invX === ({({2},1), ({3},8)},8,3,0,6,1,{({1},3)},1,0,1,3,0,{({1},3), ({3},1)},3,{({2},1), ({3},7)}) then return X.cache#(S,T,"labelDSCF") = "DSCF-V1-4";
    if invX === ({({3},7), ({4},4)},10,4,-2,-4,3,{({1},3)},1,0,1,3,0,{({1},3), ({4},1)},4,{({3},7), ({4},3)}) then return X.cache#(S,T,"labelDSCF") = "DSCF-V1-5";
    if invX === ({({2},6)},4,0,1,3,0,{({1},3)},1,0,1,3,0,{({1},3), ({2},1)},2,{({2},5)}) then return X.cache#(S,T,"labelDSCF") = "DSCF-V1-6";
    if invX === ({({2},2), ({3},5)},7,2,0,3,1,{({1},3)},1,0,1,3,0,{({1},3), ({3},1)},3,{({2},2), ({3},4)}) then return X.cache#(S,T,"labelDSCF") = "DSCF-V1-7";
    if invX === ({({2},1), ({3},8)},8,3,0,5,1,{({1},3)},1,0,1,3,0,{({1},3), ({3},1)},3,{({2},1), ({3},7)}) then return X.cache#(S,T,"labelDSCF") = "DSCF-V1-8";
    if invX === ({({2},3), ({3},2)},6,1,0,0,1,{({1},3)},1,0,1,3,0,{({1},3), ({3},1)},3,{({2},3), ({3},1)}) then return X.cache#(S,T,"labelDSCF") = "DSCF-V1-9";
    if invX === ({({3},10), ({4},1)},9,3,-2,-7,3,{({1},3)},1,0,1,3,0,{({1},3), ({4},1)},4,{({3},10)}) then return X.cache#(S,T,"labelDSCF") = "DSCF-V1-10";
    if invX === ({({2},1), ({3},6), ({4},1)},9,4,-1,1,2,{({1},3)},1,0,1,3,0,{({1},3), ({4},1)},4,{({2},1), ({3},6)}) then return X.cache#(S,T,"labelDSCF") = "DSCF-V1-11";
    if invX === ({({2},2), ({3},3), ({4},1)},8,3,-1,-1,2,{({1},3)},1,0,1,3,0,{({1},3), ({4},1)},4,{({2},2), ({3},3)}) then return X.cache#(S,T,"labelDSCF") = "DSCF-V1-12";
    if invX === ({({3},7), ({4},2)},10,4,-2,-6,3,{({1},3)},1,0,1,3,0,{({1},3), ({4},1)},4,{({3},7), ({4},1)}) then return X.cache#(S,T,"labelDSCF") = "DSCF-V1-13";
    if invX === ({({3},4), ({4},9), ({5},1)},11,4,-5,-23,6,{({1},3)},1,0,1,3,0,{({1},3), ({5},1)},5,{({3},4), ({4},9)}) then return X.cache#(S,T,"labelDSCF") = "DSCF-V1-14";
    if invX === ({({3},10), ({4},1)},9,3,-2,-8,3,{({1},3)},1,0,1,3,0,{({1},3), ({4},1)},4,{({3},10)}) then return X.cache#(S,T,"labelDSCF") = "DSCF-V1-15";
    if invX === ({({3},1), ({4},21), ({5},1)},12,5,-5,-23,6,{({1},3)},1,0,1,3,0,{({1},3), ({5},1)},5,{({3},1), ({4},21)}) then return X.cache#(S,T,"labelDSCF") = "DSCF-V1-16";
    if invX === ({({2},1), ({3},7), ({4},1)},8,2,-2,-10,3,{({1},3)},1,0,1,3,0,{({1},3), ({4},1)},4,{({2},1), ({3},7)}) then return X.cache#(S,T,"labelDSCF") = "DSCF-V1-17";
    if invX === ({({3},7), ({4},2)},10,4,-2,-7,3,{({1},3)},1,0,1,3,0,{({1},3), ({4},1)},4,{({3},7), ({4},1)}) then return X.cache#(S,T,"labelDSCF") = "DSCF-V1-18";
    if invX === ({({3},6), ({4},2), ({5},1)},11,5,-4,-16,5,{({1},3)},1,0,1,3,0,{({1},3), ({5},1)},5,{({3},6), ({4},2)}) then return X.cache#(S,T,"labelDSCF") = "DSCF-V1-19";
    if invX === ({({3},1), ({4},21), ({5},1)},12,5,-5,-24,6,{({1},3)},1,0,1,3,0,{({1},3), ({5},1)},5,{({3},1), ({4},21)}) then return X.cache#(S,T,"labelDSCF") = "DSCF-V1-20";
    if invX === ({({2},2), ({3},4), ({4},1)},7,1,-2,-13,3,{({1},3)},1,0,1,3,0,{({1},3), ({4},1)},4,{({2},2), ({3},4)}) then return X.cache#(S,T,"labelDSCF") = "DSCF-V1-21";
    if invX === ({({3},7), ({4},1), ({5},1)},10,3,-5,-26,6,{({1},3)},1,0,1,3,0,{({1},3), ({5},1)},5,{({3},7), ({4},1)}) then return X.cache#(S,T,"labelDSCF") = "DSCF-V1-22";
    if invX === ({({3},9), ({5},1)},10,4,-4,-18,5,{({1},3)},1,0,1,3,0,{({1},3), ({5},1)},5,{({3},9)}) then return X.cache#(S,T,"labelDSCF") = "DSCF-V1-23";
    if invX === ({({3},4), ({4},9), ({5},1)},11,4,-5,-25,6,{({1},3)},1,0,1,3,0,{({1},3), ({5},1)},5,{({3},4), ({4},9)}) then return X.cache#(S,T,"labelDSCF") = "DSCF-V1-24";
    if invX === ({({3},6), ({4},2), ({5},1)},11,5,-4,-17,5,{({1},3)},1,0,1,3,0,{({1},3), ({5},1)},5,{({3},6), ({4},2)}) then return X.cache#(S,T,"labelDSCF") = "DSCF-V1-25";
    if invX === ({({2},1), ({3},6), ({5},1)},9,3,-4,-20,5,{({1},3)},1,0,1,3,0,{({1},3), ({5},1)},5,{({2},1), ({3},6)}) then return X.cache#(S,T,"labelDSCF") = "DSCF-V1-26";
    if invX === ({({3},1), ({4},21), ({5},1)},12,5,-5,-25,6,{({1},3)},1,0,1,3,0,{({1},3), ({5},1)},5,{({3},1), ({4},21)}) then return X.cache#(S,T,"labelDSCF") = "DSCF-V1-27";
    if invX === ({({3},7), ({4},1), ({5},1)},10,3,-5,-27,6,{({1},3)},1,0,1,3,0,{({1},3), ({5},1)},5,{({3},7), ({4},1)}) then return X.cache#(S,T,"labelDSCF") = "DSCF-V1-28";
    if invX === ({({3},4), ({4},9), ({5},1)},11,4,-5,-26,6,{({1},3)},1,0,1,3,0,{({1},3), ({5},1)},5,{({3},4), ({4},9)}) then return X.cache#(S,T,"labelDSCF") = "DSCF-V1-29";
    if invX === ({({3},10), ({5},1)},9,2,-5,-29,6,{({1},3)},1,0,1,3,0,{({1},3), ({5},1)},5,{({3},10)}) then return X.cache#(S,T,"labelDSCF") = "DSCF-V1-30";
    if invX === ({({3},3), ({4},12), ({6},1)},12,5,-8,-41,9,{({1},3)},1,0,1,3,0,{({1},3), ({6},1)},6,{({3},3), ({4},12)}) then return X.cache#(S,T,"labelDSCF") = "DSCF-V1-31";
    if invX === ({({3},3), ({4},12), ({6},1)},12,5,-8,-42,9,{({1},3)},1,0,1,3,0,{({1},3), ({6},1)},6,{({3},3), ({4},12)}) then return X.cache#(S,T,"labelDSCF") = "DSCF-V1-32";
    if invX === ({({2},1), ({3},7), ({5},1)},8,1,-5,-32,6,{({1},3)},1,0,1,3,0,{({1},3), ({5},1)},5,{({2},1), ({3},7)}) then return X.cache#(S,T,"labelDSCF") = "DSCF-V1-33";
    if invX === ({({3},4), ({4},9), ({6},1)},11,3,-9,-51,10,{({1},3)},1,0,1,3,0,{({1},3), ({6},1)},6,{({3},4), ({4},9)}) then return X.cache#(S,T,"labelDSCF") = "DSCF-V1-34";
    if invX === ({({3},6), ({4},2), ({6},1)},11,4,-8,-43,9,{({1},3)},1,0,1,3,0,{({1},3), ({6},1)},6,{({3},6), ({4},2)}) then return X.cache#(S,T,"labelDSCF") = "DSCF-V1-35";
    if invX === ({({3},9), ({6},1)},10,3,-8,-45,9,{({1},3)},1,0,1,3,0,{({1},3), ({6},1)},6,{({3},9)}) then return X.cache#(S,T,"labelDSCF") = "DSCF-V1-36";
    if invX === ({({3},4), ({4},9), ({6},1)},11,3,-9,-52,10,{({1},3)},1,0,1,3,0,{({1},3), ({6},1)},6,{({3},4), ({4},9)}) then return X.cache#(S,T,"labelDSCF") = "DSCF-V1-37";
    if invX === ({({3},7), ({4},2), ({6},1)},10,2,-9,-54,10,{({1},3)},1,0,1,3,0,{({1},3), ({6},1)},6,{({3},7), ({4},2)}) then return X.cache#(S,T,"labelDSCF") = "DSCF-V1-38";
    if invX === ({({3},10), ({6},1)},9,1,-9,-57,10,{({1},3)},1,0,1,3,0,{({1},3), ({6},1)},6,{({3},10)}) then return X.cache#(S,T,"labelDSCF") = "DSCF-V1-39";
    if invX === ({({3},6), ({4},3), ({7},1)},11,3,-13,-76,14,{({1},3)},1,0,1,3,0,{({1},3), ({7},1)},7,{({3},6), ({4},3)}) then return X.cache#(S,T,"labelDSCF") = "DSCF-V1-40";
    if invX === ({({2}, 6)}, 4, 0, 1, 3, 0, {({1}, 3)}, 1, 0, 1, 3, 0, {({1}, 3), ({2}, 3)}, 3, {({2}, 3), ({3}, 3)}) then return X.cache#(S,T,"labelDSCF") = "Tregub1";
    X.cache#(S,T,"labelDSCF") = "NotRecognized"
);

isSurfaceUknownToBeAlreadyEquidimensional = (X,mu) -> (
    if mu.cache#?"surface U is already equidimensional" then return mu.cache#"surface U is already equidimensional";
    if isFanoMapStandard(X) and member(recognizeDSCF X,{"DSCF-V1-13", "DSCF-V1-15", "DSCF-V1-18", "DSCF-V1-20", "DSCF-V1-25", "DSCF-V1-27", "DSCF-V1-36"}) then (
        return mu.cache#"surface U is already equidimensional" = false;
    );
    if isFanoMapStandard(X) and member(recognizeDSCF X, {"DSCF-V1-1", "DSCF-V1-2", "DSCF-V1-3", "DSCF-V1-4", "DSCF-V1-5", "DSCF-V1-6", "DSCF-V1-7", "DSCF-V1-8", "DSCF-V1-9", "DSCF-V1-10", "DSCF-V1-11", "DSCF-V1-12", "DSCF-V1-14", "DSCF-V1-16", "DSCF-V1-17", "DSCF-V1-19", "DSCF-V1-21", "DSCF-V1-22", "DSCF-V1-23", "DSCF-V1-24", "DSCF-V1-26", "DSCF-V1-28", "DSCF-V1-29", "DSCF-V1-30", "DSCF-V1-31", "DSCF-V1-32", "DSCF-V1-33", "DSCF-V1-34", "DSCF-V1-35", "DSCF-V1-37", "DSCF-V1-38", "DSCF-V1-39", "DSCF-V1-40"}) then (
        return mu.cache#"surface U is already equidimensional" = true;
    );
    return false;
);

isSurfaceUknownToNotNeedRefining = X -> isFanoMapStandard(X) and member(recognizeDSCF X, {"DSCF-V1-1", "DSCF-V1-2", "DSCF-V1-3", "DSCF-V1-7", "DSCF-V1-9", "DSCF-V1-12", "DSCF-V1-16", "DSCF-V1-17", "DSCF-V1-18", "DSCF-V1-20", "DSCF-V1-21", "DSCF-V1-25", "DSCF-V1-26", "DSCF-V1-28", "DSCF-V1-29", "DSCF-V1-30", "DSCF-V1-32", "DSCF-V1-33", "DSCF-V1-35", "DSCF-V1-36", "DSCF-V1-37", "DSCF-V1-38", "DSCF-V1-39", "DSCF-V1-40"});

isNormalizationKnownToTerminateQuickly = X -> isFanoMapStandard(X) and member(recognizeDSCF X,{"DSCF-V1-17", "DSCF-V1-27", "DSCF-V1-30"});

isHigherDegreeCurveInExceptionalSetKnownToBeSpecial = X -> isFanoMapStandard(X) and member(recognizeDSCF X,{"DSCF-V1-34","DSCF-V1-40"});

someExceptionalCurvesKnownToAppearWithMultiplicity = X -> isFanoMapStandard(X) and member(recognizeDSCF X,{"DSCF-V1-5", "DSCF-V1-14"});

setStrategyDSCFtoK3 = (X,Str) -> (
    if Str =!= null then return Str;
    if isFanoMapStandard X then (
        if member(recognizeDSCF X,{"DSCF-V1-13","DSCF-V1-18","DSCF-V1-24","DSCF-V1-25","DSCF-V1-27","DSCF-V1-31","DSCF-V1-40"}) then return "Approximate";
        return "Inverse";
    );
    if isFanoMapToP2xP2 X then return "Approximate";
    return "Inverse";
);

setStrategyDSCFtoPolarize = (Utilde,Str) -> (
    if Str =!= null then return Str;
    X := recoverFourfold Utilde;
    (mu,U,LC,f) := building Utilde;
    if U.cache#?"special curves on U" or isHigherDegreeCurveInExceptionalSetKnownToBeSpecial(X) then return "SpecialCurve";
    if isFanoMapStandard X then (
        if member(recognizeDSCF X,{"DSCF-V1-6","DSCF-V1-21","DSCF-V1-26","DSCF-V1-30","DSCF-V1-33","DSCF-V1-36","DSCF-V1-39"}) then return "MapFromU";
        if f =!= null then return "MapFromW" else return "MapFromW-Virtual";
    );
    if isFanoMapToP2xP2 X then (
        if f =!= null then return "MapFromU" else return "MapFromU-Virtual";
    );
    return "MapFromU";
);

------------------------------------------------------------------------
------------------------------------------------------------------------

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

DefaultVerbosityDSCFtoInterpolateTop = false;
verbosityDSCFtoInterpolateTop = v -> v and DefaultVerbosityDSCFtoInterpolateTop;

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
    -- Currently under development and testing.
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

------------------------------------------------------------------------
---------------------- Construction of DSCF examples  ------------------
------------------------------------------------------------------------

exampleDSCFourfoldC8 = method(TypicalValue => DoubleSpecialCubicFourfold, Options => {Verbose => true});
exampleDSCFourfoldC8 (ZZ,Ring) := o -> (i,K) -> (
    if not (1 <= i and i <= 40) then error "DSCF example out of range: expected an integer between 1 and 40";
    L := {null,
((4,6,1,0,0),(1,2,0,0,0)),
((3,4,0,0,0),(1,1,0,0,0)),
((4,9,0,0,0),(3,9,0,0,0)),
((5,8,0,1,0),(1,2,0,0,0)),
((6,10,0,0,1),(1,2,0,0,0)),
((2,0,0,0,0),(1,0,0,0,0)),
((4,5,1,0,0),(1,1,0,0,0)),
((4,8,0,0,0),(2,5,0,0,0)),
((3,3,0,0,0),(1,0,0,0,0)),
((5,7,0,1,0),(1,1,0,0,0)),
((5,8,2,0,0),(3,7,2,0,0)),
((4,8,0,0,0),(3,8,0,0,0)),
((5,7,2,0,0),(2,4,1,0,0)),
((6,9,0,0,1),(1,1,0,0,0)),
((4,7,0,0,0),(2,4,0,0,0)),
((6,7,2,1,0),(2,4,0,1,0)),
((4,4,1,0,0),(1,0,0,0,0)),
((6,2,6,0,0),(1,2,0,0,0)),
((5,10,1,0,0),(3,8,1,0,0)),
((6,4,5,0,0),(2,3,2,0,0)),
((3,2,0,0,0),(2,2,0,0,0)),
((5,6,0,1,0),(1,0,0,0,0)),
((5,7,2,0,0),(3,6,2,0,0)),
((5,6,2,0,0),(2,3,1,0,0)),
((6,5,5,0,0),(3,5,4,0,0)),
((4,7,0,0,0),(3,7,0,0,0)),
((8,0,4,4,0),(2,0,4,1,0)),
((4,6,0,0,0),(2,3,0,0,0)),
((6,1,6,0,0),(1,1,0,0,0)),
((4,3,1,0,0),(2,1,1,0,0)),
((5,9,1,0,0),(3,7,1,0,0)),
((6,4,5,0,0),(3,4,4,0,0)),
((3,1,0,0,0),(2,1,0,0,0)),
((5,5,0,1,0),(2,1,0,1,0)),
((5,6,2,0,0),(3,5,2,0,0)),
((4,6,0,0,0),(3,6,0,0,0)),
((4,5,0,0,0),(2,2,0,0,0)),
((4,2,1,0,0),(2,0,1,0,0)),
((3,0,0,0,0),(2,0,0,0,0)),
((4,5,0,0,0),(3,5,0,0,0))};
    if o.Verbose then << "-- constructing example n. " << i << ", specialFourfold surface" << toString(L_i) << endl;
    specialCubicFourfold rationalSurfaceWithAttachedPlaneInCubicFourfold(splice(L_i,K),Verbose=>o.Verbose)
);

check DoubleSpecialCubicFourfold := o -> X -> (
    if not (X.cache#?"DataConstruction" and X.cache#?"Construction" and instance(X.cache#"Construction",String) and substring(0,29,X.cache#"Construction") == "X = specialFourfold surface((") then error "expected a cubic fourfold constructed via specialFourfold surface((...),(...))";
    (S,P) := surfaces X;
    (S',C',pr) := X.cache#"DataConstruction";
    C := pr C';
    if not isSubset(C, S * P) then error "projection of curve is not contained in surface-plane intersection";
    D := (S * P) \\ C;
    if dim D != -1 then error("surface-plane intersection contains extra components: " | (? ideal D));
    SingC := support singularLocus C;
    n := S.cache#"FiniteNumberOfNodes";
    if degree SingC == n then (
        if o.Verbose then << "-- verified: surface-plane intersection has correct number of nodes (" << n << ")" << endl;
    ) else (
        error("incorrect number of nodes in surface-plane intersection: expected " | (toString n) | ", obtained " | (toString degree SingC));
    );
    SingS := support singularLocus S;
    if not isSubset(SingC, SingS) then error "singular locus of intersection curve is not contained in the singular locus of the surface";
    resSing := SingS \\ SingC;
    if dim resSing == -1 then (
        if o.Verbose then << "-- verified: surface is smooth outside the curve" << endl;
    ) else (
        errLog := if dim resSing == 0 then (toString degree resSing) | " points" else "singular locus is not 0-dimensional";
        error("surface has singularities outside the curve: " | errLog);
    );
    return X;
);

discoverCubicFourfoldsInC8 = (e,dmin,dmax,Nmin,Nmax,summaryFileName,rationalSectionOnly,maxTime,NewCollectionFourfolds) -> (
    -- usage: discoverCubicFourfoldsInC8({10,10,10,10,10},1,4,5,13,"examples_v1",true,300,{});
    startTime := currentTime();
    runTime := () -> humanReadableSeconds(currentTime() - startTime);
    for Z in NewCollectionFourfolds do (Z.cache#"instanceCount" = 0; Z.cache#"calculationTime" = humanReadableSeconds(0));
    instanceCount := 0;
    K := ZZ/65521;
    local X; local S; local T; local P; local C; local SummaryFile; local DataFile;
    for a from 1 to e_0 do
        for i1 to e_1 do
            for i2 to e_2 do
                for i3 to e_3 do
                    for i4 to e_4 do (
                        if binomial(a+2,2) - i1 - 3*i2 - 6*i3 - 10*i4 - 1 > Nmax or binomial(a+2,2) - i1 - 3*i2 - 6*i3 - 10*i4 - 1 < Nmin then continue;
                        try (
                            alarm maxTime;
                            <<"-- constructing surface S"<<(a,i1,i2,i3,i4)<<"..."<<endl;
                            S = surface({a,i1,i2,i3,i4},K);
                            alarm 0;
                        ) else (
                            alarm 0;
                            <<"-- something went wrong when calling S"<<(a,i1,i2,i3,i4)<<", possible alarm triggered: "<<humanReadableSeconds(maxTime)<<endl;
                            continue;
                        );
                        <<"-- obtained surface in PP^"<<dim ambient S<<endl;
                        -- if dim ambient S > Nmax or dim ambient S < Nmin then continue;
                        if dim ambient S < Nmin then continue;
                        for d from dmin to dmax do
                            for u1 to i1 do
                                for u2 to i2 do
                                    for u3 to i3 do
                                        for u4 to i4 do (
                                            <<"-- surface: "<<toString{a,i1,i2,i3,i4}<<", curve: "<<toString (d,{u1,u2,u3,u4})<<endl;
                                            if rationalSectionOnly and 1 != (a-d)^2 - (i1-u1+u2) - 4*(i2-u2+u3) - 9*(i3-u3+u4) - 16*(i4-u4) then (
                                                <<"-- formula not satisfied: 1 != (a-d)^2 - (i1-u1+u2) - 4*(i2-u2+u3) - 9*(i3-u3+u4) - 16*(i4-u4) = " << (a-d)^2 - (i1-u1+u2) - 4*(i2-u2+u3) - 9*(i3-u3+u4) - 16*(i4-u4) << endl;
                                                continue;
                                            );
                                            try (
                                                X = specialCubicFourfold rationalSurfaceWithAttachedPlaneInCubicFourfold(S,(d,u1,u2,u3,u4),Verbose=>true);
                                            ) else (
                                                <<"-- something went wrong when calling 'surface((...),(...))'"<<endl;
                                                continue;
                                            );
                                            quadricFibration X;
                                            if rationalSectionOnly and (not X.cache#"quadricFibrationCubicFourfoldInC8"_1) then (
                                                <<"-- fourfold not allowed: "<<X.cache#"quadricFibrationCubicFourfoldInC8"_2<<endl;
                                                continue;
                                            );
                                            if (not rationalSectionOnly) and X.cache#"quadricFibrationCubicFourfoldInC8"_1 and 1 != (a-d)^2 - (i1-u1+u2) - 4*(i2-u2+u3) - 9*(i3-u3+u4) - 16*(i4-u4) then (
                                                <<"-- exception: "<<(a,i1,i2,i3,i4)<<","<<(d,u1,u2,u3,u4)<<" -> surface is a section but formula not satisfied: 1 != (a-d)^2 - (i1-u1+u2) - 4*(i2-u2+u3) - 9*(i3-u3+u4) - 16*(i4-u4) = "<<((a-d)^2 - (i1-u1+u2) - 4*(i2-u2+u3) - 9*(i3-u3+u4) - 16*(i4-u4))<<endl;
                                            );
                                            T = surface X;
                                            P = surface X.cache#"parentCubicFourfold";
                                            assert(degree P == 1);
                                            C = T * P;
                                            if not member((degrees T,betti res ideal T,degree T,sectionalGenus T,euler hilbertPolynomial T,eulerCharacteristic T,numberNodes T,dim C,degree C,degrees(T + P)), apply(NewCollectionFourfolds, Y -> (degrees surface Y,betti res ideal surface Y,degree surface Y,sectionalGenus surface Y,euler hilbertPolynomial surface Y,eulerCharacteristic surface Y,numberNodes surface Y,dim((surface Y)*(surface Y.cache#"parentCubicFourfold")),degree((surface Y)*(surface Y.cache#"parentCubicFourfold")),degrees((surface Y)+(surface Y.cache#"parentCubicFourfold"))))) then (
                                                instanceCount = instanceCount + 1;
                                                X.cache#"instanceCount" = instanceCount;
                                                X.cache#"calculationTime" = runTime();
                                                NewCollectionFourfolds = sort append(NewCollectionFourfolds,X);
                                                <<endl;
                                                <<"--## NEW EXAMPLE FOUND ##-- "<<endl;
                                                <<"-- number of distinct fourfolds found: "<<#NewCollectionFourfolds<<endl;
                                                <<"-- elapsed time: "<<runTime()<<endl;
                                                SummaryFile = openOut (summaryFileName|".txt");
                                                DataFile = openOut (summaryFileName|"_commands.m2");
                                                SummaryFile<<"-- File automatically generated by the function 'discoverCubicFourfoldsInC8()' in SpecialFanoFourfolds.m2, date: "<<get "!date"<<endl;
                                                SummaryFile<<"Time to reach last fourfold in list: "<<runTime()<<endl;
                                                SummaryFile<<"Number of distinct fourfolds found: "<<#NewCollectionFourfolds<<endl<<endl<<"Summary:"<<endl;
                                                for Y in NewCollectionFourfolds do (
                                                    SummaryFile<<position(NewCollectionFourfolds,i->i===Y)+1<<") obtained after "<<Y.cache#"calculationTime"<<", instance: "<<Y.cache#"instanceCount"<<endl<<Y.cache#"Construction"<<endl;
                                                    DataFile<<Y.cache#"Construction"<<endl;
                                                    SummaryFile<<describe Y<<endl;
                                                    SummaryFile<<endl;
                                                );
                                                close SummaryFile;
                                                close DataFile;
                                            ) else (
                                                <<"-- fourfold not allowed: already in collection --"<<endl;
                                            );
                                        );
                    );
    <<"-- SEARCH FINISHED with "<<#NewCollectionFourfolds<<" distinct examples"<<endl;
    NewCollectionFourfolds
);

------------------------------------------------------------------------
------------------------- Tests for D. S. C. F.  -----------------------
------------------------------------------------------------------------

generateInputsForRunExampleTest = i -> (
    X := specialFourfold("DSCF-"|(toString i),Verbose=>false);
    (S,P) := surfaces X;
    E := polarizedK3surface polarizedK3surface X;
    (mu,U,LC,f) := building E;
    W := target mu;
    (i, nextPrime random(33331,65521), degree S, sectionalGenus S, discriminant X, degree U, sectionalGenus U, euler hilbertPolynomial U, dim ambient W, degree W, sectionalGenus W, degree first LC, degree last LC, dim target f, latticeMatrix latticePolarization E)
);

runExampleTest = (i,charK,degS,gS,dX,degU,gU,chiOU,ambW,degW,gW,degL,degC,gK3,M) -> (
    X := specialFourfold("DSCF-"|(toString i),ZZ/charK,Verbose=>true);
    assert(charK === char coefficientRing X);
    (S,P) := surfaces X;
    describe X;
    assert(X.cache#(S,P,"labelDSCF") === "DSCF-V1-"|toString(i));
    assert(degree P == 1 and degree S == degS and sectionalGenus S == gS);
    assert(discriminant X == dX and discriminant X.cache#"parentCubicFourfold" == 8);
    assert(statusK3 X == -1);
    E := polarizedK3surface(X,Verbose=>true);
    assert instance(E, K3SurfaceFromDoubleSpecialCubicFourfold);
    assert(statusK3 X == 3);
    (mu,U,LC,f) := building E; (L,C) := toSequence LC;
    assert(U === surface mirrorFourfold X);
    assert(degree U == degU and sectionalGenus U == gU and euler hilbertPolynomial U == chiOU);
    W := target mu;
    assert(W == mirrorFourfold X);
    assert(dim ambient W == ambW and degree W == degW and sectionalGenus W == gW);
    assert(degree L == degL and degree C == degC);
    assert(dim target f == gK3 and f#"image" =!= null and sectionalGenus image f == gK3);
    assert(ideal projectiveVariety E === ideal image f);
    assert instance(projectiveVariety E, SurfaceAssociatedToRationalFourfold);
    assert(isFanoMapStandard X and fanoMapDSCF X === mu);
    assert(E#"LatticePolarization" === null);
    polarizedK3surface(X,Verbose=>true);
    assert(E#"LatticePolarization" =!= null);
    assert(statusK3 X == 4);
    assert instance(latticePolarization E, LatticePolarizationOnK3Surface);
    assert(latticeMatrix latticePolarization E == M);
    assert(recoverFourfold E === X and recoverFourfold projectiveVariety E === X);
    assert(setStrategyDSCFtoK3(X,null) === U.cache#"strategy for surface U");
    if setStrategyDSCFtoK3(X,null) === "Approximate" then return;
    (mu',eta) := U.cache#"birational maps from X to W and from W to X";
    assert(eta === getInverseFanoMap X and eta === getInverseFanoMap E);
    assert(source eta  === target mu and target eta == X and eta#"inverse" === mu');
);

------------------------------------------------------------------------
----------------------- End of new section (v. 2.8) --------------------
------------------------------------------------------------------------
