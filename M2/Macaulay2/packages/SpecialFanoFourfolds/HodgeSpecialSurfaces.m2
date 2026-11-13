
---------------------------------------------------------
-- Experimental: stability and utility under evaluation.
-- These functions may be deprecated in future versions.

HodgeSpecialSurface = new Type of WeightedProjectiveVariety;
globalAssignment HodgeSpecialSurface;
HodgeSpecialSurface.synonym = "Hodge-special surface";
surface (MultiprojectiveVariety,MultiprojectiveVariety) := (C,S) -> (
    if ring ideal C =!= ring ideal S then error "expected varieties in the same ambient space";
    if dim S != 2 then error "expected a surface";
    if abs dim C != 1 then error "expected a curve";
    if not isSubset(C,S) then error "the given curve is not contained in the surface";
    newS := new HodgeSpecialSurface of (class S) from S;
    if newS#?"CurveContainedInTheSurface" then newS#"CurveContainedInTheSurface" = prepend(C,newS#"CurveContainedInTheSurface") else newS#"CurveContainedInTheSurface" = {C};
    if not newS.cache#?"hyperplane" then newS.cache#"hyperplane" = random(1,0_S);
    newS
);
map (HodgeSpecialSurface,ZZ,ZZ) := o -> (S,a,b) -> (
    if S.cache#?("map",a,b) then return S.cache#("map",a,b);
    H := S.cache#"hyperplane";
    C := first S#"CurveContainedInTheSurface";
    if a < 0 then error "expected a nonnegative integer";
    S.cache#("map",a,b) = if b >= 0 then mapDefinedByDivisor(S,{(H,a),(C,b)}) else rationalMap(C_S,a,-b)
);
HodgeSpecialSurface Sequence := (S,ab) -> (
    if not(#ab == 2 and instance(first ab,ZZ) and instance(last ab,ZZ)) then error "expected a sequence of two integers";
    (a,b) := ab;
    image map(S,a,b)
);
map HodgeSpecialSurface := o -> S -> (
    if S.cache#?"doubleCover" then return S.cache#"doubleCover";
    S.cache#"doubleCover" = quadricFibration(map(S,1,0),Verify=>true)
);
curve = method();
curve HodgeSpecialSurface := U -> first U#"CurveContainedInTheSurface";
discriminant HodgeSpecialSurface := ZZ => o -> S -> (
    if S.cache#?(curve S,"discriminantSurface") then return last S.cache#(curve S,"discriminantSurface");
    if not member(o.Algorithm, {null, 1, 2}) then error "the Algorithm option accepts the values 1 and 2";
    C := curve S;
    if dim C != 1 then error "expected a Hodge-special surface";
    if o.Algorithm === 2 then return discriminant2 S;
    f := map(S,0,1);
    if dim target f <= 0 then (if o.Algorithm === 1 then error "the option Algorithm=>1 is not available for this case" else return discriminant2 S);
    C' := f^* random(1,0_(target f));
    if dim(C * C') != 0 then error "something went wrong :(";
    C2 := degree(C * C');
    H := S.cache#"hyperplane";
    H2 := S * H * random(1,0_S);
    if dim H2 != 0 then error "something went wrong :(";
    H2 = degree H2;
    HC := S * H * C;
    if dim HC != 0 then error "something went wrong :(";
    HC = degree HC;
    disc := det(S.cache#(curve S,"LatticeIntersectionMatrix") = matrix {{H2,HC},{HC,C2}});
    S.cache#(curve S,"discriminantSurface") = (C2,disc);
    disc
);
discriminant2 = method();
discriminant2 HodgeSpecialSurface := S -> (
    if not (codim S == 1 and numgens ideal S == 1) then error "the option Algorithm=>2 is not available for this case";
    if S.cache#?(curve S,"discriminantSurface") then return last S.cache#(curve S,"discriminantSurface");
    C := curve S;
    H := S.cache#"hyperplane";
    HC := H * C;
    if dim HC != 0 then error "something went wrong :(";
    HC = degree HC;
    a := degree S;
    g := sectionalGenus image segreEmbedding C;
    e := sum degrees ring ambient S;
    if #e != 1 then error "something went wrong :(";
    C2 := (e_0 - a)*HC + 2*g -2;
    H2 := S * H * random(1,0_S);
    if dim H2 != 0 then error "something went wrong :(";
    H2 = degree H2;
    disc := det(S.cache#(curve S,"LatticeIntersectionMatrix") = matrix {{H2,HC},{HC,C2}});
    S.cache#(curve S,"discriminantSurface") = (C2,disc);
    disc
);
HodgeSpecialSurface#{WebApp,AfterPrint} = HodgeSpecialSurface#{WebApp,AfterNoPrint} =
HodgeSpecialSurface#{Standard,AfterPrint} = HodgeSpecialSurface#{Standard,AfterNoPrint} = S -> (
    d := degree S;
    str := "ProjectiveVariety, "|(if d <= 9 then ({"linear", "quadratic", "cubic", "quartic", "quintic", "sextic", "septic", "octic", "nonic"}_(d-1))|" surface" else "surface of degree "|toString(d));
    str = str|" in "|(toString expression ambient S);
    try discriminant S;
    if S.cache#?(curve S,"LatticeIntersectionMatrix") then (
        M := S.cache#(curve S,"LatticeIntersectionMatrix");
        str = (str|" with rank 2 lattice")||("defined by the intersection matrix "|net(M)|" (det: "|(toString discriminant S)|")");
    );
    << endl << concatenate(interpreterDepth:"o") << lineNumber << " : " << str << endl;
);
parameterCount HodgeSpecialSurface := o -> S -> (
    if not(codim S == 1 and numgens ideal S == 1) then error "expected a surface in a three-dimensional weighted projective space";
    a := first degree (ideal S)_0;
    C := curve S;
    if dim C != 1 then error "expected a Hodge-special surface";
    if o.Verbose then <<"C: "|toString(? C)<<endl;
    if o.Verbose then <<"S: "|toString(? S)<<endl;
    if o.Verbose then <<"ambient: P = "|toString(? ambient S)<<endl;
    N := normalSheaf C;
    h1N := rankHH(1,N);
    if o.Verbose then <<"h^1(N_{C,P}) = "|toString(h1N)<<endl;
    if h1N != 0 then <<"--warning: condition h^1(N_{C,P}) == 0 not satisfied"<<endl;
    h0N := rankHH(0,N);
    if o.Verbose then <<"h^0(N_{C,P}) = "|toString(h0N)<<endl;
    m := # basisMem({a},C);
    if o.Verbose then <<"h^0(I_{C,P}("|(toString a)|")) = "|toString(m)<<endl;
    if o.Verbose then <<"h^0(N_{C,P}) + "|toString(m-1)|" = "|toString(h0N + m - 1)<<endl;
    NS := normalSheaf(C,S);
    h0NS := rankHH(0,NS);
    if o.Verbose then <<"h^0(N_{C,S}) = "|toString(h0NS)<<endl;
    if o.Verbose then <<"dim{[S] : C ⊂ S ⊂ P} >= "|toString(h0N + m-1 - h0NS)<<endl;
    Amb := # basisMem({a},0_(ambient S));
    if o.Verbose then <<"dim P(H^0(O_P("|(toString a)|"))) = "|toString(Amb-1)<<endl;
    w := Amb-1 - (h0N + m-1 - h0NS);
    if o.Verbose then <<"codim{[S] : C ⊂ S ⊂ P} <= "|toString(w)<<endl;
    return (w,(m,h0N,h0NS));
);
---------------------------------------------------------
