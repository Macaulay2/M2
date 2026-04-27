
------------------------------------------------------------------------
----------------------- Hodge-special fourfolds ------------------------
------------------------------------------------------------------------

HodgeSpecialFourfold = new Type of EmbeddedProjectiveVariety;

globalAssignment HodgeSpecialFourfold;

HodgeSpecialFourfold.synonym = "Hodge-special fourfold";

specialFourfold = method(TypicalValue => HodgeSpecialFourfold, Options => {NumNodes => null, InputCheck => 1, Verbose => false});

specialFourfold (EmbeddedProjectiveVariety,EmbeddedProjectiveVariety,EmbeddedProjectiveVariety) := o -> (S,X,V) -> (
    if not (ring ideal S === ring ideal X and ring ideal X === ring ideal V) then error "expected varieties in the same ambient space";
    if dim X != 4 then error "expected a fourfold";
    if not (dim V == 5 and isSubset(X,V)) then error "expected a fivefold containing the fourfold";
    if dim S != 2 then error "expected a surface";
    i := o.InputCheck;
    if not(instance(i,ZZ) and i >= -1) then error("option InputCheck expects a nonnegative integer:"|newline|"0: no check is done about the smoothness of the fourfold and of the surface"|newline|"1: just the smoothness of the fourfold is checked"|newline|"2: both the smoothness of the fourfold and the smoothness of the surface are checked");
    if i >= 0 then (
        if not isSubset(S,X) then error "the given surface is not contained in the fourfold";
        if not isSmooth V then error "the ambient fivefold is not smooth";
    );
    if i >= 1 then if not isSmooth X then error "expected a smooth fourfold";
    if i >= 2 then (
        if not isSmooth S then error "expected a smooth surface";
        if o.Verbose then <<"-- smoothness of the surface verified (assuming equidimensionality)"<<endl;
    );
    if i >= 4 then (
        if S != top S then error "expected an irreducible reduced surface";
        if o.Verbose then <<"-- equidimensionality of the surface verified"<<endl;
    );
    Fourfold := new HodgeSpecialFourfold from X;
    if Fourfold#?"SurfaceContainedInTheFourfold" then Fourfold#"SurfaceContainedInTheFourfold" = prepend(S,Fourfold#"SurfaceContainedInTheFourfold") else Fourfold#"SurfaceContainedInTheFourfold" = {S};
    X.cache#"AmbientFivefold" = V;
    if dim ambient X == 5 and degrees X == {({3},1)} and codim V == 0 then Fourfold = new CubicFourfold from Fourfold;
    if dim ambient X == 7 and degrees X == {({2},3)} and codim V == 2 and degrees V == {({2},2)} then Fourfold = new IntersectionOfThreeQuadricsInP7 from Fourfold;
    if dim ambient X == 8 and degrees X == {({2},6)} and codim V == 3 and degrees V == {({2},5)} and degree V == 5 then Fourfold = new GushelMukaiFourfold from Fourfold;
    Fourfold
);

specialFourfold (EmbeddedProjectiveVariety,EmbeddedProjectiveVariety) := o -> (S,X) -> (
    if ring ideal S =!= ring ideal X then error "expected varieties in the same ambient space";
    if dim S != 2 then error "expected a surface";
    if dim X == 4 and dim ambient X == 5 and degrees X == {({3},1)} and degree X == 3 then return cubicFourfold(S,X,NumNodes=>o.NumNodes,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
    if dim X == 4 and dim ambient X == 8 and degrees X == {({2},6)} and degree X == 10 then return gushelMukaiFourfold(S,X,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
    if dim X == 5 and (degrees X == {({2},2)} or degrees X == {({3},1)}) then return specialFourfold(S,X * random(2,S),X,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
    if dim X != 4 then error "expected a fourfold";
    if dim ambient X == 7 and degrees X == {({2},3)} and degree X == 8 then return specialFourfold(S,X,random({{2},{2}},X),InputCheck=>o.InputCheck,Verbose=>o.Verbose);
    if dim ambient X == 6 and degrees X == {({2},1),({3},1)} and degree X == 6 then return specialFourfold(S,X,random(3,X),InputCheck=>o.InputCheck,Verbose=>o.Verbose);
    if dim ambient X == 5 and degrees X == {({4},1)} and degree X == 4 then return specialFourfold(S,X,ambient X,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
    if o.InputCheck >= 0 then if not isSubset(S,X) then error "the given surface is not contained in fourfold";
    if o.InputCheck >= 1 then if not isSmooth X then error "expected a smooth fourfold";
    Fourfold := new HodgeSpecialFourfold from X;
    if Fourfold#?"SurfaceContainedInTheFourfold" then Fourfold#"SurfaceContainedInTheFourfold" = prepend(S,Fourfold#"SurfaceContainedInTheFourfold") else Fourfold#"SurfaceContainedInTheFourfold" = {S};
    Fourfold
);

specialFourfold (Ideal,Ideal) := o -> (idS,idX) -> specialFourfold(projectiveVariety idS,projectiveVariety idX,NumNodes=>o.NumNodes,InputCheck=>o.InputCheck,Verbose=>o.Verbose);

specialFourfold (Ideal,RingElement) := o -> (idS,C) -> specialFourfold(idS,ideal C,NumNodes=>o.NumNodes,InputCheck=>o.InputCheck,Verbose=>o.Verbose);

specialFourfold EmbeddedProjectiveVariety := o -> S -> (
    if dim S == 4 then (
        if S#?"SurfaceContainedInTheFourfold" then
            return specialFourfold(first S#"SurfaceContainedInTheFourfold",S,InputCheck=>o.InputCheck,Verbose=>o.Verbose)
        else
            return specialFourfold(S * random({2:{1}},0_S),S,InputCheck=>o.InputCheck,Verbose=>o.Verbose)
    );
    if dim S != 2 then error "expected a surface";
    if dim ambient S == 5 then return cubicFourfold(S,NumNodes=>o.NumNodes,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
    if dim ambient S == 6 then return specialFourfold(S,random({{2},{3}},S),InputCheck=>o.InputCheck,Verbose=>o.Verbose);
    if dim ambient S == 7 then return specialFourfold(S,random({{2},{2},{2}},S),InputCheck=>o.InputCheck,Verbose=>o.Verbose);
    if dim ambient S == 8 or dim ambientVariety S == 5 or dim ambientVariety S == 6 then return gushelMukaiFourfold(S,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
    error "no valid input provided";
);

specialFourfold Ideal := o -> idS -> specialFourfold(makeSubvariety idS,NumNodes=>o.NumNodes,InputCheck=>o.InputCheck,Verbose=>o.Verbose);

specialFourfold (String,Ring) := o -> (str,K) -> (
    try return gushelMukaiFourfold(str,K,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
    try return cubicFourfold(str,K,NumNodes=>o.NumNodes,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
    local X; local S;
    if str === "plane in PP^7" then (
        X = specialFourfold(surface({1},K,ambient=>7),InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        assert(recognize X === "planeInPP7");
        return X;
    );
    if str === "internal projection of K3 surface of genus 8" then (
        G := GG(K,1,5);
        G = (parametrize random({6:{1}},0_G))^^ G;
        X = specialFourfold((rationalMap point G) G,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        assert(recognize X === "internal-projection-K3-genus-8");
        return X;
    );
    if substring(0,5,str) === "DSCF-" then (
        i := value substring(5,str);
        if instance(i,ZZ) then return exampleDSCFourfoldC8(i,K,Verbose=>o.Verbose);
    );
    error ///string may not be valid, permitted strings are:
*** Cubic fourfolds ***
"quartic scroll",
"3-nodal septic scroll",
"one-nodal septic del Pezzo surface",
"6-nodal octic scroll",
"general cubic 4-fold of discriminant 32",
"general cubic 4-fold of discriminant 38",
"general cubic 4-fold of discriminant 42",
"8-nodal nonic scroll",
"general cubic 4-fold of discriminant 44",
"cubic 4-fold of discriminant 48"
*** GM fourfolds ***
"sigma-plane",
"rho-plane",
"tau-quadric",
"cubic scroll",
"quintic del Pezzo surface",
"K3 surface of genus 8 with class (9,5)",
"general GM 4-fold of discriminant 20",
"1","2",...,"21",
"nodal surface of degree 11 and genus 3 with class (7,4)",
"surface of degree 11 and genus 3 with class (7,4)",
"GM 4-fold of discriminant 26('')",
"GM 4-fold of discriminant 28",
"GM 4-fold of discriminant 34(')",
"triple projection of K3 surface of degree 26"
*** Complete intersections of 3 quadrics in PP^7 ***
"plane in PP^7",
"internal projection of K3 surface of genus 8"
*** Doubly special cubic fourfolds ***
"DSCF-1","DSCF-2",...,"DSCF-40"///;
);

specialFourfold String := o -> str -> specialFourfold(str,ZZ/65521,NumNodes=>o.NumNodes,InputCheck=>o.InputCheck,Verbose=>o.Verbose);

specialFourfold (String,ZZ) := o -> (str,i) -> (prebuiltExamplesOfRationalFourfolds()) (str,i);

expression HodgeSpecialFourfold := X -> (
    E := if dim ambient X == 4
         then "a "
         else if degrees X == {({2},1),({3},1)}
         then "complete intersection of type (2,3) in "
         else if degrees X == {({4},1)}
         then "quartic hypersurface in "
         else "fourfold in ";
    E = E|"PP^"|toString(dim ambient X)|" containing a surface of degree "|toString(degree surface X)|" and sectional genus "|toString(sectionalGenus surface X);
    expression E
);

describe HodgeSpecialFourfold := X -> (
    descr := if dim ambient X == 4
             then "Four-dimensional projective space"
             else if degrees X == {({2},3)}
             then "Complete intersection of 3 quadrics in PP^7"
             else if degrees X == {({2},1),({3},1)}
             then "Complete intersection of type (2,3) in PP^6"
             else if degrees X == {({4},1)}
             then "Quartic hypersurface in PP^5"
             else "Hodge-special fourfold in PP^"|toString(dim ambient X);
    a := flatten degrees ideal X; r := codim X;
    if r > 0 and #a == r then (
        disc := discriminant X;
        A := X.cache#(surface X,"LatticeIntersectionMatrix");
        descr = descr|newline|toString("of discriminant "|(toString disc)|" = det"|(net A));
    );
    descr = descr|newline|"containing a "|surfaceDescription(0,surface X);
    net expression descr
);

shortDescriptionFourfold = method();
shortDescriptionFourfold (HodgeSpecialFourfold,Boolean) := (X,UseAttribute) -> (
    if UseAttribute and hasAttribute(X,ReverseDictionary) then return toString getAttribute(X,ReverseDictionary);
    "Hodge-special fourfold"
);
shortDescriptionFourfold HodgeSpecialFourfold := X -> shortDescriptionFourfold(X,true);

ambientFivefold = method(TypicalValue => EmbeddedProjectiveVariety);

ambientFivefold HodgeSpecialFourfold := X -> (
    if X.cache#?"AmbientFivefold" then return X.cache#"AmbientFivefold";
    if codim X == 1 then return X.cache#"AmbientFivefold" = ambient X;
    if instance(X,GushelMukaiFourfold) then return X.cache#"AmbientFivefold" = varietyDefinedBylinearSyzygies X;
    error "no ambient fivefold found";
);

associatedMapFromFivefold = X -> (
    if (surface X).cache#?("AssociatedMapFromFivefold",ambientFivefold X) then return (surface X).cache#("AssociatedMapFromFivefold",ambientFivefold X);
    if dim ambient X == 5 then return (surface X).cache#("AssociatedMapFromFivefold",ambientFivefold X) = rationalMap(ideal surface X,degreeHypersurface X);
    if degreeHypersurface X == 2 then (
        S := ideal surface X;
        J1 := select(S_*,s -> degree s == {1});
        if #J1 > 0 then J1 = (ideal J1) * (ideal vars ring S) else J1 = ideal ring S;
        J2 := select(S_*,s -> degree s == {2});
        if #J2 > 0 then J2 = ideal J2 else J2 = ideal ring S;
        return (surface X).cache#("AssociatedMapFromFivefold",ambientFivefold X) = rationalMap trim sub(J1+J2,ring ambientFivefold X);
    );
    (surface X).cache#("AssociatedMapFromFivefold",ambientFivefold X) = rationalMap(idealOfSubvariety((surface X)%(ambientFivefold X)),degreeHypersurface X)
);

map HodgeSpecialFourfold := o -> X -> associatedMapFromFivefold X;

imageOfAssociatedMap = method();
imageOfAssociatedMap HodgeSpecialFourfold := X -> (
    f := map X;
    if f#"idealImage" =!= null then return image f;
    e := if X.cache#?(surface X,"label") then X.cache#(surface X,"label") else "not recognized yet";
    if e === "quinticDelPezzoSurface" or e === "quarticScrollSurface" or e === "FarkasVerra" or e === "planeInPP7" then forceImage(f,image(f,2));
    if e === "C42" then forceImage(f,image(f,3));
    if instance(e,ZZ) and e >= 1 and e <= 21 and e != 3 and e != 21 then forceImage(f,image(f,2));
    if instance(e,ZZ) and (e == 3 or e == 21) then forceImage(f,trim lift(kernel(map rationalMap(f,Dominant=>2),SubringLimit=>1),ambient target f));
    if member(e,{"gushel26''", "hyperplane section of a conic bundle over PP2", "surf-5-6-2-nodal"}) then forceImage(f,trim kernel(map f,SubringLimit=>1));
    ch := char coefficientRing X;
    if (coefficientRing X === ZZ/ch and ch <= 65521) then image(f,"F4") else image f
);

degreeHypersurface = method();

degreeHypersurface HodgeSpecialFourfold := X -> (
    if instance(X,CubicFourfold) then return 3;
    if instance(X,GushelMukaiFourfold) or instance(X,IntersectionOfThreeQuadricsInP7) then return 2;
    I := flatten degrees trim sub(ideal X,ring ambientFivefold X);
    if #I != 1 then error "degree is not defined";
    first I
);

recognize = method();

recognize HodgeSpecialFourfold := X -> (
    if instance(X,DoublySpecialCubicFourfold) then return recognizeDSCF X;
    if X.cache#?(surface X,"label") then return X.cache#(surface X,"label");
    if instance(X,CubicFourfold) then return X.cache#(surface X,"label") = recognizeCubicFourfold X;
    if instance(X,GushelMukaiFourfold) then return X.cache#(surface X,"label") = recognizeGMFourfold X;
    if instance(X,IntersectionOfThreeQuadricsInP7) then return X.cache#(surface X,"label") = recognize3QuadricsP7 X;
    "NotRecognized"
);

HodgeSpecialFourfold ? HodgeSpecialFourfold := (X,Y) -> (
    if not uniform {X,Y} then return incomparable;
    try (dX,dY) := (discriminant X,discriminant Y) else return incomparable;
    if dX < dY then return symbol <;
    if dX > dY then return symbol >;
    if instance(X,GushelMukaiFourfold) and instance(Y,GushelMukaiFourfold) and dX == dY and dX % 8 == 2 then (
        (a,b) := last cycleClass X; (a',b') := last cycleClass Y;
        if (even(a+b) and odd(b)) and (odd(a'+b') and even(b')) then return symbol <;
        if (odd(a+b) and even(b)) and (even(a'+b') and odd(b')) then return symbol >;
    );
    if X.cache#?(surface X,"parameterCount") and Y.cache#?(surface Y,"parameterCount") then (
        if first X.cache#(surface X,"parameterCount") < first Y.cache#(surface Y,"parameterCount") then return symbol <;
        if first X.cache#(surface X,"parameterCount") > first Y.cache#(surface Y,"parameterCount") then return symbol >;
    );
    if degree surface X < degree surface Y then return symbol <;
    if degree surface X > degree surface Y then return symbol >;
    if sectionalGenus surface X < sectionalGenus surface Y then return symbol <;
    if sectionalGenus surface X > sectionalGenus surface Y then return symbol >;
    if (surface X).cache#?"linear system on PP^2" and (surface Y).cache#?"linear system on PP^2" then return (((surface X).cache#"linear system on PP^2") ? ((surface Y).cache#"linear system on PP^2"));
    if X == Y and surface X == surface Y then return symbol ==;
    return incomparable;
);

parameterCount = method(Options => {Verbose => false})

parameterCount (EmbeddedProjectiveVariety,EmbeddedProjectiveVariety) := o -> (S,X) -> (
    isSing := true;
    if S.cache#?"isSmooth" then isSing = not isSmooth S else (
        if S.cache#?"singularLocus" or S.cache#?"nonSaturatedSingularLocus" then isSing = dim singLocus S >= 0 else (
            if S.cache#?"FiniteNumberOfNodes" then isSing = numberNodes S >= 1;
        );
    );
    if ring ambient S =!= ring ambient X then error "expected varieties in the same ambient space";
    d := first first degrees ideal X;
    c := codim X;
    if not ({{d}} === unique degrees ideal X and c == # degrees ideal X) then error "the second argument must be a complete intersection of hypersurfaces of the same degree";
    r := dim S;
    if (r <= 0) then error "the first argument must be a positive dimensional scheme";
    if not isSubset(S,X) then error "expected the first scheme to be a subscheme of the second one";
    if o.Verbose then <<"S: "|toString(? ideal S)<<endl;
    if o.Verbose then <<"X: "|toString(? ideal X)<<endl;
    n := dim ambient S;
    N := normalSheaf(S,ambient S);
    if isSing then (
        if o.Verbose then <<"(assumption: dim Ext^1(I_{S,P^"|toString(n)|"},O_S) = 0)"<<endl;
    ) else (
    --   h1N := rankHH(1,N);
    --   if o.Verbose then <<"h^1(N_{S,P^"|toString(n)|"}) = "|toString(h1N)<<endl;
    --   if h1N != 0 then <<"--warning: condition not satisfied: h^1(N_{S,P^"|toString(n)|"}) = 0"<<endl;
        if o.Verbose then <<"(assumption: h^1(N_{S,P^"|toString(n)|"}) = 0)"<<endl;
    );
    h0N := rankHH(0,N);
    if o.Verbose then <<"h^0(N_{S,P^"|toString(n)|"}) = "|toString(h0N)<<endl;
     ------------------------------
    -- If h^1(O_S(d)) == 0, h^3(O_S(d)) == 0,..., and h^0(I_S(d)) == h^0(O_(P^n)(d)) - \chi(O_S(d)) for a particular S,
    -- then we have h^0(I_S(d)) == h^0(O_(P^n)(d)) - \chi(O_S(d)) for the generic S.
    -- Indeed, let S be generic.
    -- h^0(I_S(d)) = h^0(O_(P^n)(d)) - h^0(O_S(d)) + h^1(I_S(d))
    --            >= h^0(O_(P^n)(d)) - h^0(O_S(d))
    --             = h^0(O_(P^n)(d)) - \chi(O_S(d)) - h^1(O_S(d)) + h^2(O_S(d)) - h^3(O_S(d)) + ...
    --            >= h^0(O_(P^n)(d)) - \chi(O_S(d)) - h^1(O_S(d)) - h^3(O_S(d)) - ...
    -- (by semicontinuity we have h^1(O_S(d))=0, h^3(O_S(d))=0,...)
    --             = h^0(O_(P^n)(d)) - \chi(O_S(d))
    -- (by semicontinuity we have h^0(O_(P^n)(d)) - \chi(O_S(d)) >= h^0(I_S(d)))
    --            >= h^0(I_S(d))
    ------------------------------
    OS := OO_(variety S);
    h1OSd := for j from 1 to r list if odd j then rank HH^j (OS(d)) else continue;
    if unique h1OSd =!= {0} then error("condition not satisfied: h^(2j-1)(O_S("|toString(d)|")) = 0");
    m := # basisMem({d},S);
    m' := binomial(n+d,d) - ((hilbertPolynomial S) d);
    if m != m' then error("condition not satisfied: h^0(I_{S,P^"|toString(n)|"}("|toString(d)|")) == h^0(O_(P^"|toString(n)|")("|toString(d)|")) - \\chi(O_S("|toString(d)|"))");
    if o.Verbose then (
        for j from 1 to r list if odd j then  <<"h^"|toString(j)|"(O_S("|toString(d)|")) = 0, ";
        <<"and h^0(I_{S,P^"|toString(n)|"}("|toString(d)|")) = "|toString(m)|" = h^0(O_(P^"|toString(n)|")("|toString(d)|")) - \\chi(O_S("|toString(d)|"));"|newline|"in particular, h^0(I_{S,P^"|toString(n)|"}("|toString(d)|")) is minimal"<<endl;
    );
    M := c*(m-c); -- dim GG(c-1,m-1)
    if c > 1 and o.Verbose then <<"dim GG("|toString(c-1)|","|toString(m-1)|") = "|toString(M)<<endl;
    if o.Verbose then <<"h^0(N_{S,P^"|toString(n)|"}) + "|(if c > 1 then "dim GG("|toString(c-1)|","|toString(m-1)|")" else toString(m-1))|" = "|toString(h0N + M)<<endl;
    NX := normalSheaf(S,X);
    h0NX := rankHH(0,NX);
    if o.Verbose then <<"h^0(N_{S,X}) = "|toString(h0NX)<<endl;
    if o.Verbose then <<"dim{[X] : S ⊂ X} >= "|toString(h0N + M - h0NX)<<endl;
    if o.Verbose then <<(if c > 1 then "dim GG("|toString(c-1)|",P(H^0(O_(P^"|toString(n)|")("|toString(d)|")))) = " else "dim P(H^0(O_(P^"|toString(n)|")("|toString(d)|"))) = ")|toString(c * (binomial(n+d,d) - c))<<endl;
    w := c*(binomial(n+d,d)-c) - (h0N+M-h0NX);
    if o.Verbose then <<"codim{[X] : S ⊂ X} <= "|toString(w)<<endl;
    return X.cache#(S,"parameterCount") = (w,(m,h0N,h0NX));
);

parameterCount HodgeSpecialFourfold := o -> X -> (
    Y := ambientFivefold X;
    S := surface X;
    a := degreeHypersurface X;
    if o.Verbose then <<"S: "|toString(? ideal S)<<endl;
    if o.Verbose then <<"X: "|toString(? ideal X)<<endl;
    if o.Verbose then <<"Y: "|toString(? ideal Y)<<endl;
    if o.Verbose then <<"X is a fourfold containing S which is a hypersurface of degree "<<a<<" in Y"<<endl;
    N := normalSheaf(S,Y);
    h1N := rankHH(1,N);
    if o.Verbose then <<"h^1(N_{S,Y}) = "|toString(h1N)<<endl;
    if h1N != 0 then error("condition not satisfied: h^1(N_{S,Y}) = 0");
    h0N := rankHH(0,N);
    if o.Verbose then <<"h^0(N_{S,Y}) = "|toString(h0N)<<endl;
    m := numgens ambient target map X;
    OS := OO_(variety S);
    h1OSa := rank HH^1 (OS(a));
    if h1OSa != 0 then error("condition not satisfied: h^1(O_S("|(toString a)|")) = 0");
    Amb := rank HH^0 OO_(variety Y)(a);
    m' := Amb - ((hilbertPolynomial S) a);
    if m != m' then error("condition not satisfied: h^0(I_{S,Y}("|(toString a)|")) == h^0(O_Y("|(toString a)|")) - \\chi(O_S("|(toString a)|"))");
    if o.Verbose then (
        <<"h^1(O_S("|(toString a)|")) = 0, ";
        <<"and h^0(I_{S,Y}("|(toString a)|")) = "|toString(m)|" = h^0(O_Y("|(toString a)|")) - \\chi(O_S("|(toString a)|"));"|newline|"in particular, h^0(I_{S,Y}("|(toString a)|")) is minimal"<<endl;
    );
    if o.Verbose then <<"h^0(N_{S,Y}) + "|toString(m-1)|" = "|toString(h0N + m - 1)<<endl;
    NX := normalSheaf(S,X);
    h0NX := rankHH(0,NX);
    if o.Verbose then <<"h^0(N_{S,X}) = "|toString(h0NX)<<endl;
    if o.Verbose then <<"dim{[X] : S ⊂ X ⊂ Y} >= "|toString(h0N + m-1 - h0NX)<<endl;
    if o.Verbose then <<"dim P(H^0(O_Y("|(toString a)|"))) = "|toString(Amb-1)<<endl;
    w := Amb-1 - (h0N + m-1 - h0NX);
    if o.Verbose then <<"codim{[X] : S ⊂ X ⊂ Y} <= "|toString(w)<<endl;
    if o.Verbose and instance(X,IntersectionOfThreeQuadricsInP7) then infoAboutParameterCountInAmbientP7(w,(m,h0N,h0NX));
    return X.cache#(S,"parameterCount") = (w,(m,h0N,h0NX));
);

CoherentSheafOnEmbeddedProjectiveVariety = new Type of CoherentSheaf;
projectiveVariety CoherentSheafOnEmbeddedProjectiveVariety := o -> F -> F.variety.cache#"embedded projective variety";
CoherentSheafOnEmbeddedProjectiveVariety#{WebApp,AfterPrint} = CoherentSheafOnEmbeddedProjectiveVariety#{WebApp,AfterNoPrint} =
CoherentSheafOnEmbeddedProjectiveVariety#{Standard,AfterPrint} = CoherentSheafOnEmbeddedProjectiveVariety#{Standard,AfterNoPrint} = F -> (<< endl << concatenate(interpreterDepth:"o") << lineNumber << " : Coherent sheaf on " << projectiveVariety F << endl);

normalSheaf = method(TypicalValue => CoherentSheaf);
normalSheaf MultiprojectiveVariety := normalSheaf EmbeddedProjectiveVariety := X -> (
    if X.cache#?("normalSheaf",ambientVariety X) then return X.cache#("normalSheaf",ambientVariety X);
    I := idealOfSubvariety X;
    R := (ring I)/I;
    N := sheaf Hom((module I) ** R,R);
    if N.variety.ring =!= R then error "internal error encountered";
    N.variety.cache#"embedded projective variety" = X;
    X.cache#("normalSheaf",ambientVariety X) = new CoherentSheafOnEmbeddedProjectiveVariety from N
);
normalSheaf (MultiprojectiveVariety,MultiprojectiveVariety) := normalSheaf (EmbeddedProjectiveVariety,EmbeddedProjectiveVariety) := (X,Y) -> (
    Z := ambientVariety X;
    N := normalSheaf makeSubvariety(X,Y);
    makeSubvariety(X,Z);
    N
);

rankHH = method();
rankHH (ZZ,CoherentSheafOnEmbeddedProjectiveVariety) := (i,F) -> (
    X := projectiveVariety F;
    if X.cache#?("rank HH",i,F) then return X.cache#("rank HH",i,F);
    X.cache#("rank HH",i,F) = rank HH^i F
);

unirationalParametrization = method();

unirationalParametrization HodgeSpecialFourfold := (cacheValue "unirationalParametrization") (X -> (
    error "not implemented yet";
));

parametrize HodgeSpecialFourfold := X -> (
    if X.cache#?"rationalParametrization" then return X.cache#"rationalParametrization";
    Psi := fanoMap X;
    X.cache#"rationalParametrization" = inverse3(Psi|X)
);

surface = method(TypicalValue => EmbeddedProjectiveVariety);

surface HodgeSpecialFourfold := X -> first X#"SurfaceContainedInTheFourfold";

surface (VisibleList,Ring,Option,Option) := (L,K,oN,oA) -> (
    oN = toList oN; oA = toList oA;
    if first oN === ambient and first oA === NumNodes then (oN,oA) = (oA,oN);
    if first oN =!= NumNodes and first oA =!= ambient then error "NumNodes and ambient are the only available options for surface(VisibleList)";
    if not (#L>0 and all(L,i->instance(i,ZZ) and i>=0)) then error "expected a list of nonnegative integers";
    P := for i from 1 to #L-1 list for j from 1 to L_i list (i,point PP_K^2);
    B := if #L==1 or all(take(L,-(#L-1)),i->i==0) then 0_(PP_K^2) else ⋃ apply(flatten P,p -> p_0 * p_1);
    f := rationalMap(B,L_0,Dominant=>true);
    if last oN > 0 then f = f * rationalMap((linearSpan apply(last oN,i -> point linearSpan {f point source f,f point source f}))_(target f),Dominant=>true);
    if last oA =!= null then f = rationalMap(f << PP_K^(last oA),Dominant=>true);
    S := target f;
    S.cache#"rationalParametrization" = f;
    S.cache#"linear system on PP^2" = L;
    S.cache#"points on PP^2" = P;
    if dim linearSpan S >= 5 and last oN >= 0 then S.cache#"FiniteNumberOfNodes" = last oN;
    S.cache#"takeCurve" = ((D,d) -> (
        if not(instance(D,ZZ) and D>0) then error "expected a positive integer for the degree of a plane curve";
        if not (instance(d,VisibleList) and #d==#L-1 and all(#d,i->instance(d_i,ZZ) and d_i>=0 and d_i<=L_(i+1)))
        then error("expected a list of "|toString(#L-1)|" nonnegative integers representing a curve on the surface"|toString(L));
        local C; local pt;
        if sum toList d == 0 then (
            pt = point PP_K^2;
            C = random(D,pt);
        ) else (
            pts := flatten for a to #d-1 list take(apply(P_a,last),d_a);
            C = random(D,⋃ pts);
            pt = first pts;
        );
        if D == 2 then C.cache#"rationalParametrization" = inverse(rationalMap(pt%C,1),Verify=>true);
        fC := f C;
        makeSubvariety(fC,S,Verify=>true);
        if not (dim fC == 1 and degree fC == D*L_0 - sum(#d,i->(i+1)*d_i)) then error "something went wrong when taking the curve";
        fC.cache#"plane representation" = (D,d);
        if D <= 2 then fC.cache#"rationalParametrization" = check rationalMap((parametrize C)*f,fC);
        return fC;
    ));
    return S;
);
surface (VisibleList,Ring,Option) := (L,K,opt) -> (
    o := first toList opt;
    if o === ambient
    then return surface(L,K,NumNodes=>0,opt)
    else if o === NumNodes
    then return surface(L,K,opt,ambient=>null);
    error "NumNodes and ambient are the only available options for surface(VisibleList)";
);
surface(VisibleList,Option,Option) := (L,opt1,opt2) -> surface(L,ZZ/65521,opt1,opt2);
surface(VisibleList,Option) := (L,opt) -> surface(L,ZZ/65521,opt);
surface (VisibleList,Ring) := (L,K) -> surface(L,K,NumNodes=>0,ambient=>null);
surface List := L -> surface(L,ZZ/65521,NumNodes=>0,ambient=>null);

clean HodgeSpecialFourfold := X -> (
    K := coefficientRing X;
    x := local x;
    n := dim ambient X;
    R := K[x_0..x_n];
    S := surface X;
    S' := Var sub(sub(ideal S,vars R),vars ring ambient X);
    X' := Var sub(sub(ideal X,vars R),vars ring ambient X);
    V' := Var sub(sub(ideal ambientFivefold X,vars R),vars ring ambient X);
    if S.cache#?"euler" then S'.cache#"euler" = S.cache#"euler";
    if S.cache#?"FiniteNumberOfNodes" then S'.cache#"FiniteNumberOfNodes" = S.cache#"FiniteNumberOfNodes";
    specialFourfold(S',X',V',InputCheck=>0)
);

HodgeSpecialFourfold ** Ring := (X,K) -> (
    if not isField K then error "expected a field";
    S := (surface X) ** K;
    if (surface X).cache#?"FiniteNumberOfNodes" and (not S.cache#?"FiniteNumberOfNodes") then S.cache#"FiniteNumberOfNodes" = (surface X).cache#"FiniteNumberOfNodes";
    if (surface X).cache#?"euler" and (not S.cache#?"euler") then S.cache#"euler" = (surface X).cache#"euler";
    specialFourfold(S,(projectiveVariety ring X) ** K,(ambientFivefold X) ** K,InputCheck=>0)
);

random HodgeSpecialFourfold := o -> X -> (
    X' := specialFourfold(surface X,random(degreeHypersurface X,surface X) * ambientFivefold X,ambientFivefold X,InputCheck=>-1);
    if X.cache#?(surface X,"label") and (not X'.cache#?(surface X',"label")) then X'.cache#(surface X',"label") = X.cache#(surface X,"label");
    X'
);

toExternalString HodgeSpecialFourfold := X -> (
    x := local x;
    K := coefficientRing X;
    n := dim ambient X;
    R := K[x_0..x_n];
    s := ///needsPackage "SpecialFanoFourfolds";///|newline;
    s = s|"(i -> (x := local x;"|newline;
    s = s|"R := "|toExternalString(K)|"[x_0..x_"|toString(n)|"];"|newline;
    s = s|"S := projectiveVariety "|toString sub(ideal surface X,vars R)|";"|newline;
    if codim ambientFivefold X == 0 then s = s|"V := ambient S;"|newline else s = s|"V := projectiveVariety "|toString sub(ideal ambientFivefold X,vars R)|";"|newline;
    s = s|"X := projectiveVariety "|toString sub(ideal X,vars R)|";"|newline;
    if instance(X,CubicFourfold)
    then s = s|"X = specialFourfold(S,X,NumNodes=>"|toString(numberNodes surface X)|",InputCheck=>0);"|newline else
    if instance(X,GushelMukaiFourfold)
    then s = s|"X = specialFourfold(S,X,InputCheck=>0);"|newline|///X.cache#"AmbientFivefold" = V;///|newline else
    s = s|"X = specialFourfold(S,X,V,InputCheck=>0);"|newline;
    if (surface X).cache#?"euler" then s = s|///(surface X).cache#"euler" = ///|toString(euler surface X)|";"|newline;
    if (surface X).cache#?"FiniteNumberOfNodes" then s = s|///(surface X).cache#"FiniteNumberOfNodes" = ///|toString(numberNodes surface X)|";"|newline;
    N := numgens target map X -1;
    if N >= 6 and (map X)#"idealImage" =!= null then (
        z := local z; Z := K[z_0..z_N];
        phi := sub(map X,R,Z);
        s = s|"z := local z; Z := "|toExternalString(K)|"[z_0..z_"|toString(N)|"];"|newline;
        s = s|///(surface X).cache#("AssociatedMapFromFivefold",ambientFivefold X) = rationalMap(ring V,Z,///|(toString entries phi)|");"|newline;
        s = s|"forceImage(map X,"|(toString image phi)|");"|newline;
    );
    if (surface X).cache#?("fanoMap",ambientFivefold X) then (
        mu := fanoMap X;
        m := numgens ambient target mu -1;
        y := local y;
        T := K[y_0..y_m];
        mu = sub(mu,R,T);
        s = s|"y := local y; T := "|toExternalString(K)|"[y_0..y_"|toString(m)|"];"|newline;
        if m > 4 then s = s|"T = T/"|toString ideal target mu|";"|newline;
        s = s|"mu := rationalMap map(ring V,T,"|toString entries mu|");"|newline;
        s = s|"forceImage(mu,ideal(0_(target mu)));"|newline;
        s = s|///(mu#"map").cache#"multiplicityFanoMap" = ///|toString(((fanoMap X)#"map").cache#"multiplicityFanoMap")|";"|newline;
        s = s|///(surface X).cache#("fanoMap",ambientFivefold X) = mu;///|newline;
        if (surface X).cache#?("surfaceDeterminingInverseOfFanoMap",ideal X) then (
            U := surfaceDeterminingInverseOfFanoMap X;
            s = s|"U := projectiveVariety("|toString sub(ideal U,vars T)|",Saturate=>false);"|newline;
            s = s|///(surface X).cache#("surfaceDeterminingInverseOfFanoMap",ideal X) = U;///|newline;
            if U.cache#?"exceptionalCurves" then (
                (L,C) := exceptionalCurves X;
                s = s|"L := "|(if dim L >= 0 then "projectiveVariety("|toString sub(ideal L,vars T)|",Saturate=>false)" else "0_U")|";"|newline;
                s = s|"C := "|(if dim C >= 0 then "projectiveVariety("|toString sub(ideal C,vars T)|",Saturate=>false)" else "0_U")|";"|newline;
                s = s|///U.cache#"exceptionalCurves" = (L%U,C%U);///|newline;
            );
        );
    );
    s = s|"X))()";
    return s;
);
