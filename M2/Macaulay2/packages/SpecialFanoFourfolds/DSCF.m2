
------------------------------------------------------------------------
--------------------- Doubly special cubic fourfolds -------------------
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

DoublySpecialCubicFourfold = new Type of CubicFourfold;
globalAssignment DoublySpecialCubicFourfold;
DoublySpecialCubicFourfold.synonym = "doubly special cubic fourfold";

specialFourfold (PairOfSurfaces,EmbeddedProjectiveVariety) := cubicFourfold (PairOfSurfaces,EmbeddedProjectiveVariety) := o -> (SandT,X) -> (
    (S,T) := toSequence SandT;
    (nS,nT) := if instance(o.NumNodes,VisibleList) and # o.NumNodes == 2 then toSequence o.NumNodes else (o.NumNodes,o.NumNodes);
    Y := cubicFourfold(T,X,NumNodes=>nT,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
    Z := new DoublySpecialCubicFourfold from cubicFourfold(S,Y,NumNodes=>nS,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
    Z.cache#"parentCubicFourfold" = Y;
    assert(surface Z === S and surface Y === T and take(Z#"SurfaceContainedInTheFourfold",2) === {S,T});
    if dim (S * T) == 2 then error "intersection of the two surfaces has dimension 2 (unsupported)";
    Z
);

specialFourfold PairOfSurfaces := cubicFourfold PairOfSurfaces := o -> SandT -> (
    if dim ambient SandT != 5 then error "expected two surfaces in P^5";
    cubicFourfold(SandT,random(3,SandT),NumNodes=>o.NumNodes,InputCheck=>o.InputCheck,Verbose=>o.Verbose)
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
    X := cubicFourfold(S' & planeC',cubicS',Verbose=>o.Verbose);
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

specialFourfold RationalSurfaceWithAttachedPlaneInCubicFourfold := cubicFourfold RationalSurfaceWithAttachedPlaneInCubicFourfold := o -> S -> S.cache#"pickedCubicFourfold";
------------------------------------------------------------------------

surfaces = method();
surfaces DoublySpecialCubicFourfold := X -> toSequence take(X#"SurfaceContainedInTheFourfold",2);

expression DoublySpecialCubicFourfold := X -> (
    (S,T) := surfaces X;
    if isPlaneInP5 S and isPlaneInP5 T then return expression("cubic fourfold in C_8 containing two planes");
    if isPlaneInP5 S then return expression("cubic fourfold in C_8 containing a plane and a surface of degree "|toString(degree T)|" and sectional genus "|toString(sectionalGenus T));
    if isPlaneInP5 T then return expression("cubic fourfold in C_8 containing a surface of degree "|toString(degree S)|" and sectional genus "|toString(sectionalGenus S)|" and a plane");
    expression("cubic fourfold containing two surfaces of degrees " | (toString degree S) | " and " | (toString degree T) | ", sectional genera " | (toString sectionalGenus S) | " and " | (toString sectionalGenus T))
);

describe DoublySpecialCubicFourfold := X -> (
    (S,T) := surfaces X;
    Y := X.cache#"parentCubicFourfold";
    d1 := discriminant X;
    d2 := discriminant Y;
    A := latticeIntersectionMatrix3x3 X;
    Cd1Cd2 := "C_"|(toString d1);
    if d1 != d2 then Cd1Cd2 = Cd1Cd2|" ∩ C_"|(toString d2);
    descr := "Cubic fourfold in "|Cd1Cd2|" over "|toString(coefficientRing X)|" of lattice discriminant";
    descr = descr||((net(newline|"det("))|(net A)|(net(newline|") = "|(toString det A))));
    descr = descr||net "containing two surfaces:";
    descr = descr||net(" - " | surfaceDescription(3,S,true));
    descr = descr||net(" - " | surfaceDescription(3,T,true));
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
        if X.cache#"quadricFibrationCubicFourfoldInC8"_1 then descr = descr||((computationStatusLog X)|idX);
        if computationStatus X >= 1 then descr = descr || (describeMirrorFourfoldAndK3 X);
    );
    net expression descr
);

shortDescriptionFourfold (DoublySpecialCubicFourfold,Boolean) := (X,UseAttribute) -> (
    if UseAttribute and hasAttribute(X,ReverseDictionary) then return toString getAttribute(X,ReverseDictionary);
    Y := X.cache#"parentCubicFourfold";
    d1 := discriminant X;
    d2 := discriminant Y;
    Cd1Cd2 := "C_"|(toString d1);
    if d1 != d2 then Cd1Cd2 = Cd1Cd2|" ∩ C_"|(toString d2);
    "cubic fourfold in "|Cd1Cd2
);

genRingIntMatr3x3 = memoize(() -> (
    □ := local □;
    K := ZZ[□];
    first gens K
));

latticeIntersectionMatrix3x3 = method();
latticeIntersectionMatrix3x3 DoublySpecialCubicFourfold := X -> (
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
surfaceIntersectionNumber DoublySpecialCubicFourfold := o -> X -> (
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
        Z := cubicFourfold(ST'',Verbose=>false);
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

random DoublySpecialCubicFourfold := o -> X -> (
    (S,T) := surfaces X;
    Y := cubicFourfold(S & T,InputCheck=>-1);
    if X.cache#?(S,T,"labelDSCF") and (not Y.cache#?(S,T,"labelDSCF")) then Y.cache#(S,T,"labelDSCF") = X.cache#(S,T,"labelDSCF");
    if isFanoMapStandard X then setFanoMapToBeStandard Y;
    if isFanoMapToP2xP2 X then setFanoMapToBeMapFromP5toP2xP2 Y;
    Y
);

clean DoublySpecialCubicFourfold := X -> (
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
    cubicFourfold(S' & T',X',InputCheck=>0,NumNodes=>(nS,nT))
);

swap = method();
swap DoublySpecialCubicFourfold := X -> (
    if X.cache#?"swappedSurfaces" then return X.cache#"swappedSurfaces";
    (S,T) := surfaces X;
    K := coefficientRing X;
    x := local x;
    R := K[x_0..x_5];
    idX := sub(sub(ideal X,vars R),vars ring ambient X);
    nS := if S.cache#?"FiniteNumberOfNodes" then S.cache#"FiniteNumberOfNodes" else null;
    nT := if T.cache#?"FiniteNumberOfNodes" then T.cache#"FiniteNumberOfNodes" else null;
    Y := cubicFourfold(T & S,Var idX,InputCheck=>0,NumNodes=>(nT,nS),Verbose=>false);
    X.cache#"swappedSurfaces" = Y;
    Y.cache#"swappedSurfaces" = X;
    Y
);

fuse = method();
fuse DoublySpecialCubicFourfold := X -> (
    if X.cache#?"fusedVersion" then return X.cache#"fusedVersion";
    (S,T) := surfaces X;
    K := coefficientRing X;
    x := local x;
    R := K[x_0..x_5];
    idX := sub(sub(ideal X,vars R),vars ring ambient X);
    Y := cubicFourfold(S+T,Var idX,InputCheck=>0,NumNodes=>0,Verbose=>false);
    X.cache#"fusedVersion" = Y;
    Y.cache#"fusedOrigin" = X;
    Y
);

unfuse = method();
unfuse CubicFourfold := X -> (
    if X.cache#?"fusedOrigin" then return X.cache#"fusedOrigin";
    ST := surface X;
    decST := decompose ST;
    if # decST != 2 then error("unfuse: expected a surface with exactly 2 components, but found " | toString(#decST));
    K := coefficientRing X;
    x := local x;
    R := K[x_0..x_5];
    idX := sub(sub(ideal X,vars R),vars ring ambient X);
    Z := cubicFourfold(decST_0 & decST_1,Var idX,InputCheck=>1,Verbose=>false);
    Z.cache#"fusedVersion" = X;
    X.cache#"fusedOrigin" = Z;
    Z
);

quadricFibration DoublySpecialCubicFourfold := o -> X -> (
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

isNormalizationKnownToTerminateQuickly = X -> isFanoMapStandard(X) and member(recognizeDSCF X,{"DSCF-V1-17", "DSCF-V1-27", "DSCF-V1-30"});

isHigherDegreeCurveInExceptionalSetKnownToBeSpecial = X -> isFanoMapStandard(X) and member(recognizeDSCF X,{"DSCF-V1-34","DSCF-V1-40"});

isSelfIntersectionVerificationKnownToBeSuperfluous = X -> isFanoMapStandard(X) and member(recognizeDSCF X,{"DSCF-V1-40"});

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
---------------------- Construction of DSCF examples  ------------------
------------------------------------------------------------------------

exampleDSCFourfoldC8 = method(TypicalValue => DoublySpecialCubicFourfold, Options => {Verbose => true});
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
    cubicFourfold rationalSurfaceWithAttachedPlaneInCubicFourfold(splice(L_i,K),Verbose=>o.Verbose)
);

check DoublySpecialCubicFourfold := o -> X -> (
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
                                                X = cubicFourfold rationalSurfaceWithAttachedPlaneInCubicFourfold(S,(d,u1,u2,u3,u4),Verbose=>true);
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
    assert(computationStatus X == -1);
    E := polarizedK3surface(X,Verbose=>true);
    assert instance(E, K3SurfaceFromDoublySpecialCubicFourfold);
    assert(computationStatus X == 3);
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
    assert(computationStatus X == 4);
    assert instance(latticePolarization E, LatticePolarizationOnK3Surface);
    assert(latticeMatrix latticePolarization E == M);
    assert(recoverFourfold E === X and recoverFourfold projectiveVariety E === X);
    assert(setStrategyDSCFtoK3(X,null) === U.cache#"strategy for surface U");
    if setStrategyDSCFtoK3(X,null) === "Approximate" then return;
    (mu',eta) := U.cache#"birational maps from X to W and from W to X";
    assert(eta === getInverseFanoMap X and eta === getInverseFanoMap E);
    assert(source eta  === target mu and target eta == X and eta#"inverse" === mu');
);
