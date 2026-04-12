
------------------------------------------------------------------------
--------------------------- GM fourfolds -------------------------------
------------------------------------------------------------------------

GushelMukaiFourfold = new Type of HodgeSpecialFourfold;

globalAssignment GushelMukaiFourfold;

GushelMukaiFourfold.synonym = "Gushel-Mukai fourfold";

gushelMukaiFourfold = method(TypicalValue => GushelMukaiFourfold, Options => {InputCheck => 1, Verbose => false});

gushelMukaiFourfold (EmbeddedProjectiveVariety,EmbeddedProjectiveVariety) := o -> (S,X) -> (
    if ring ideal S =!= ring ideal X then error "expected varieties in the same ambient space";
    if not (dim ambient X == 8 and degrees X == {({2},6)} and codim X == 4 and degree X == 10 and sectionalGenus X == 6) then error "expected a 4-dimensional subvariety of PP^8 of degree 10 and sectional genus 6 cut out by 6 quadrics";
    if dim S != 2 then error "expected a surface";
    i := o.InputCheck;
    if not(instance(i,ZZ) and i >= -1) then error("option InputCheck expects a nonnegative integer:"|newline|"0: no check is done about the smoothness of the fourfold and of the surface"|newline|"1: just the smoothness of the fourfold is checked"|newline|"2: both the smoothness of the fourfold and the smoothness of the surface are checked");
    if i >= 0 then if not isSubset(S,X) then error "the given surface is not contained in the fourfold";
    if i >= 1 then if not isSmooth X then error "expected a smooth GM fourfold";
    if i >= 2 then (
        if not isSmooth S then error "expected a smooth surface";
        if o.Verbose then <<"-- smoothness of the surface verified (assuming equidimensionality)"<<endl;
    );
    if i >= 4 then (
        if S != top S then error "expected an irreducible reduced surface";
        if o.Verbose then <<"-- equidimensionality of the surface verified"<<endl;
    );
    Fourfold := new GushelMukaiFourfold from X;
    if Fourfold#?"SurfaceContainedInTheFourfold" then Fourfold#"SurfaceContainedInTheFourfold" = prepend(S,Fourfold#"SurfaceContainedInTheFourfold") else Fourfold#"SurfaceContainedInTheFourfold" = {S};
    Fourfold
);

gushelMukaiFourfold (Ideal,Ideal) := o -> (idS,idX) -> gushelMukaiFourfold(projectiveVariety idS,projectiveVariety idX,InputCheck=>o.InputCheck,Verbose=>o.Verbose);

gushelMukaiFourfold EmbeddedProjectiveVariety := o -> S -> (
    if dim ambient S == 8 and codim S == 4 and degrees S === {({2},6)} and degree S == 10 then return gushelMukaiFourfold(S * random({2:{1}},0_S),S,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
    Y := ambientVariety S;
    er := "expected a surface in a GM fourfold or del Pezzo fivefold or del Pezzo sixfold";
    if dim S != 2 then error er;
    if not((dim ambient Y == 9 and dim Y == 6 and degrees Y === {({2},5)}) or
           (dim ambient Y == 8 and dim Y == 5 and degrees Y === {({2},5)}) or
           (dim ambient Y == 8 and dim Y == 4 and degrees Y === {({2},6)})
          ) then error er;
    if dim Y == 4 then return gushelMukaiFourfold(S,Y,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
    local X;
    if dim Y == 5 then (
        X = gushelMukaiFourfold(S,Y * random(2,S),InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        X.cache#"AmbientFivefold" = Y;
        return X;
    );
    if dim Y == 6 then (
        if dim linearSpan S > 8 then error "expected linear span of the surface to be of dimension at most 8";
        j := parametrize random(1,linearSpan S);
        T := makeSubvariety(j^^ S,j^^ ambientVariety S);
        if S.cache#?"FiniteNumberOfNodes" and (not T.cache#?"FiniteNumberOfNodes") then T.cache#"FiniteNumberOfNodes" = S.cache#"FiniteNumberOfNodes";
        if S.cache#?"top" and (not T.cache#?"top") then (T.cache#"top" = j^^(S.cache#"top"); if top T == T then T.cache#"top" = T);
        if S.cache#?"singularLocus" and (not T.cache#?"singularLocus") then T.cache#"singularLocus" = j^^(S.cache#"singularLocus");
        if S.cache#?"euler" and (not T.cache#?"euler") then T.cache#"euler" = S.cache#"euler";
        X = gushelMukaiFourfold(T,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        if instance(Y,GrassmannianVariety) then (
            j = j||Y;
            (source j).cache#"toGrass" = j;
            try assert(ideal ambientFivefold X == ideal source j) else error "internal error encountered";
            X.cache#"AmbientFivefold" = source j;
        );
        return X;
    );
);

gushelMukaiFourfold Ideal := o -> I -> gushelMukaiFourfold(makeSubvariety I,InputCheck=>o.InputCheck,Verbose=>o.Verbose);

gushelMukaiFourfold (String,Ring) := o -> (str,K) -> (
    G := GG(K,1,4);
    local X; local S;
    if str === "sigma-plane" then (
        X = gushelMukaiFourfold(schubertCycle({3,1},G),InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        X.cache#(surface X,"label") = 6;
        return X;
    );
    if str === "rho-plane" then (
        X = gushelMukaiFourfold(schubertCycle({2,2},G),InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        X.cache#(surface X,"label") = 9;
        return X;
    );
    if str === "tau-quadric" then (
        X = gushelMukaiFourfold((schubertCycle({1,1},G) * random({{1},{1}},0_G))%G,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        X.cache#(surface X,"label") = 1;
        return X;
    );
    if str === "cubic scroll" then (
        X = gushelMukaiFourfold((schubertCycle({2,0},G) * random({{1},{1}},0_G))%G,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        X.cache#(surface X,"label") = 7;
        return X;
    );
    if str === "quintic del Pezzo surface" then (
        X = gushelMukaiFourfold((G * random({4:{1}},0_G))%G,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        X.cache#(surface X,"label") = 4;
        return X;
    );
    if str === "K3 surface of genus 8 with class (9,5)" then (
        G15 := Grass replace(1,5,Grass ring G);
        pr := rationalMap(G15,ring G,select(gens ambient G15,g -> last last baseName g != 5));
        X = gushelMukaiFourfold(pr sub(ideal for i to 5 list random(1,ambient G15),G15),InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        X.cache#(surface X,"label") = 3;
        return X;
    );
    if str === "general GM 4-fold of discriminant 20" then (
        (g,T) := first randomS42data(K);
        X = gushelMukaiFourfold((g T)%image(g),InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        X.cache#(surface X,"label") = 17;
        return X;
    );
    if #str >= 1 and #str <= 2 then (
        Vstr := value str;
        if instance(Vstr,ZZ) and Vstr >= 1 and Vstr <= 21 then return fourfoldFromTriple(Vstr,GMtables(Vstr,K),InputCheck=>o.InputCheck,Verbose=>o.Verbose);
    );
    if str === "nodal surface of degree 11 and genus 3 with class (7,4)" then (
        X = gushelMukaiFourfold([4,5,1,0],[3,5,1,0],"cubic scroll",(1,K),InputCheck=>o.InputCheck,Verbose=>false);
        (surface X).cache#"FiniteNumberOfNodes" = 1;
        X.cache#(surface X,"label") = "mukai26''";
        return X;
    );
    if str === "nodal D44" then (
        X = gushelMukaiFourfold([2,0,0,0],[1,0,0,0],"cubic scroll",K,InputCheck=>o.InputCheck,Verbose=>false);
        (surface X).cache#"FiniteNumberOfNodes" = 1;
        X.cache#(surface X,"label") = "nodal D44";
        return X;
    );
    if str === "GM 4-fold of discriminant 26('')" then (
        X = gushelMukaiFourfold([4,5,1,0],[2,3,0,0],"cubic scroll",K,InputCheck=>o.InputCheck,Verbose=>false);
        (surface X).cache#"FiniteNumberOfNodes" = 0;
        X.cache#(surface X,"label") = "october2021-26''";
        return X;
    );
    if str === "GM 4-fold of discriminant 28" then (
        X = gushelMukaiFourfold([6,4,6,0],[3,3,5,0],"cubic scroll",K,InputCheck=>o.InputCheck,Verbose=>false);
        (surface X).cache#"FiniteNumberOfNodes" = 0;
        X.cache#(surface X,"label") = "october2021-28";
        return X;
    );
    if str === "GM 4-fold of discriminant 34(')" then (
        X = gushelMukaiFourfold([6,4,6,0],[3,1,6,0],"cubic scroll",K,InputCheck=>o.InputCheck,Verbose=>false);
        (surface X).cache#"FiniteNumberOfNodes" = 0;
        X.cache#(surface X,"label") = "october2021-34'";
        return X;
    );
    if str === "triple projection of K3 surface of degree 26" then (
        X = gushelMukaiFourfold([4,5,1],[2,3,0],"cubic scroll",K,InputCheck=>0,Verbose=>false);
        B := projectiveVariety matrix fanoMap X;
        interpolateTop(B,{2},Verbose=>false,cache=>true,"Deep"=>2);
        P := (B\top B)\top B;
        f := rationalMap(P % ambientFivefold X);
        S = (X * f^* f surface X)\(surface X);
        if not(dim S == 2 and degree S == 17 and sectionalGenus S == 11) then error "something went wrong";
        X = gushelMukaiFourfold(S,X,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        X.cache#(surface X,"label") = "gushel26''";
        return X;
    );
    if str === "surface of degree 11 and genus 3 with class (7,4)" then (
        X = gushelMukaiFourfold([4,5,1],[2,3,0],"cubic scroll",K,InputCheck=>0,Verbose=>false);
        S = (X * ((fanoMap X)^* first decompose first exceptionalCurves X))\surface X;
        if not(dim S == 2 and degree S == 11 and sectionalGenus S == 3 and degrees S == {({2},16)}) then error "something went wrong";
        S.cache#"FiniteNumberOfNodes" = 1;
        X = gushelMukaiFourfold(S,X,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        X.cache#(surface X,"label") = "mukai26''";
        return X;
    );
    error(///not valid string, permitted strings are:
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
"triple projection of K3 surface of degree 26"///);
);

gushelMukaiFourfold String := o -> str -> gushelMukaiFourfold(str,ZZ/65521,InputCheck=>o.InputCheck,Verbose=>o.Verbose);

expression GushelMukaiFourfold := X -> expression("GM fourfold containing a surface of degree "|toString(degree surface X)|" and sectional genus "|toString(sectionalGenus surface X)|(if X.cache#?(surface X,"classSurfaceInG14") then (" with class "|toString(first cycleClass X)) else ""));

describe GushelMukaiFourfold := X -> (
    (cS,ab) := cycleClass X;
    (a,b) := ab;
    recognize X;
    discrX := discriminant X;
    descr:="Special Gushel-Mukai fourfold of discriminant "|toString(discrX);
    if discrX % 8 == 2 then (
        if even(a+b) and odd(b) then
            descr = descr|"(')"
        else
            if odd(a+b) and even(b) then
                descr = descr|"('')"
            else error "internal error encountered"
    );
    descr = descr|newline|"containing a "|surfaceDescription(0,surface X);
    descr = descr|newline|"and with class in G(1,4) given by "|toString(cS);
    if dim singLocus ambientFivefold X >= 0 then descr = descr|newline|"Type: Gushel (not ordinary)" else descr = descr|newline|"Type: ordinary";
    if instance(recognize X,ZZ) then descr = descr|newline|"(case "|toString(recognize X)|" of Table 1 in arXiv:2002.07026)";
    if recognize X === "gushel26''" then (
        if dim singLocus ambientFivefold X >= 0
        then descr = descr|newline|"(case considered in Section 1 of arXiv:2003.07809)"
        else descr = descr|newline|"(case discovered in November 2021; see also Section 1 of arXiv:2003.07809)";
    );
    if recognize X === "mukai26''" and dim singLocus ambientFivefold X >= 0 then descr = descr|newline|"(case considered in Section 2 of arXiv:2003.07809)";
    if recognize X === "mukai26''" and dim singLocus ambientFivefold X == -1 then descr = descr|newline|"(case considered in Section 3 of arXiv:2003.07809)";
    if recognize X === "october2021-26''" or recognize X === "october2021-28" or recognize X === "october2021-28-2" or recognize X === "october2021-34'" then descr = descr|newline|"(case discovered in October 2021)";
    if recognize X === "october2021-20" then descr = descr|newline|"(case discovered in October 2021; see also Table 1 of arXiv:2003.07809)";
    if recognize X === "april2022-GM42''" then descr = descr|newline|"(strange example discovered in October 2021)";
    if computationStatus X >= 0 then descr = descr|newline|(computationStatusLog X)|newline|toString(describeMirrorFourfoldAndK3 X);
    net expression descr
);

shortDescriptionFourfold (GushelMukaiFourfold,Boolean) := (X,UseAttribute) -> (
    if UseAttribute and hasAttribute(X,ReverseDictionary) then return toString getAttribute(X,ReverseDictionary);
    "Gushel-Mukai fourfold of discriminant "|(discriminant X)
);

discriminant GushelMukaiFourfold := o -> X -> (
    if X.cache#?(surface X,"discriminantFourfold") then return last X.cache#(surface X,"discriminantFourfold");
    S := surface X;
    degS := degree S; g := sectionalGenus S; chiOS := euler hilbertPolynomial S;
    chiS := eulerCharacteristic(S,Algorithm=>if o.Algorithm === "Poisson" then null else o.Algorithm);
    KS2 := 12*chiOS-chiS;
    KSHS := 2*g-2-degS;
    (a,b) := last cycleClass X;
    n := if S.cache#?"FiniteNumberOfNodes" or S.cache#?"singularLocus" or S.cache#?"nonSaturatedSingularLocus" or (S.cache#?"fitVariety" and (S.cache#"fitVariety").cache#?"nonSaturatedSingularLocus") then numberNodes S else 0;
    S2 := 3*a + 4*b + 2*KSHS + 2*KS2 - 12*chiOS + 2*n;
    d := 4*S2 - 2*(b^2+(a-b)^2);
    if S.cache#?"FiniteNumberOfNodes" then X.cache#(surface X,"discriminantFourfold") = (S2,d);
    d
);

cycleClass GushelMukaiFourfold := X -> (
    if X.cache#?(surface X,"classSurfaceInG14") then return X.cache#(surface X,"classSurfaceInG14");
    S := surface X;
    j := toGrass X;
    cS := cycleClass makeSubvariety(j S,target j);
    ab := toSequence flatten entries lift(transpose last coefficients(cS,Monomials=>vars ring cS),ZZ);
    X.cache#(surface X,"classSurfaceInG14") = (cS,ab)
);

map GushelMukaiFourfold := o -> X -> associatedMapFromFivefold X;

recognizeGMFourfold = X -> (
    S := surface X;
    d := discriminant X;
    e := eulerCharacteristic S;
    (a,b) := last cycleClass X;
    invS := (degree S,sectionalGenus S,euler hilbertPolynomial S);
    degs := flatten degrees ideal S;
    if (d == 10 and e == 4 and a == 1 and b == 1 and invS == (2,0,1) and degs == {1, 1, 1, 1, 1, 2}) then return 1;
    if (d == 10 and e == 4 and a == 3 and b == 1 and invS == (4,0,1) and degs == {1, 1, 1, 2, 2, 2, 2, 2, 2}) then return 2;
    if (d == 10 and e == 24 and a == 9 and b == 5 and invS == (14,8,2) and degs == {2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}) then return 3;
    if (d == 10 and e == 7 and a == 3 and b == 2 and invS == (5,1,1) and degs == {1, 1, 1, 2, 2, 2, 2, 2}) then return 4;
    if (d == 10 and e == 11 and a == 5 and b == 4 and invS == (9,3,1) and degs == {1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}) then return 5;
    if (d == 10 and e == 3 and a == 1 and b == 0 and invS == (1,0,1) and degs == {1, 1, 1, 1, 1, 1}) then return 6;
    if (d == 12 and e == 4 and a == 2 and b == 1 and invS == (3,0,1) and degs == {1, 1, 1, 1, 2, 2, 2}) then return 7;
    if (d == 12 and e == 9 and a == 4 and b == 3 and invS == (7,2,1) and degs == {1, 1, 2, 2, 2, 2, 2, 2, 2, 2}) then return 8;
    if (d == 12 and e == 3 and a == 0 and b == 1 and invS == (1,0,1) and degs == {1, 1, 1, 1, 1, 1}) then return 9;
    if (d == 16 and e == 12 and a == 6 and b == 4 and invS == (10,4,1) and degs == {1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}) then return 10;
    if (d == 16 and e == 10 and a == 6 and b == 4 and invS == (10,3,1) and degs == {2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}) then return 11;
    if (d == 16 and e == 24 and a == 8 and b == 6 and invS == (14,8,2) and degs == {2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}) then return 12;
    if (d == 18 and e == 13 and a == 7 and b == 5 and invS == (12,5,1) and degs == {2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}) then return 13;
    if (d == 18 and e == 8 and a == 5 and b == 3 and invS == (8,2,1) and degs == {1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}) then return 14;
    if (d == 18 and e == 10 and a == 5 and b == 4 and invS == (9,3,1) and degs == {1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}) then return 15;
    if (d == 18 and e == 13 and a == 7 and b == 4 and invS == (11,5,1) and degs == {1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}) then return 16;
    if (d == 20 and e == 7 and a == 6 and b == 3 and invS == (9,2,1) and degs == {2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}) then return 17;
    if (d == 20 and e == 4 and a == 4 and b == 3 and invS == (7,0,1) and degs == {2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}) then return 18;
    if (d == 24 and e == 9 and a == 6 and b == 4 and invS == (10,3,1) and degs == {2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}) then return 19;
    if (d == 24 and e == 3 and a == 2 and b == 2 and invS == (4,0,1) and degs == {1, 1, 1, 2, 2, 2, 2, 2, 2}) then return 20;
    if (d == 26 and e == 12 and a == 7 and b == 5 and invS == (12,5,1) and degs == {2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}) then return 21;
    --
    if (d == 26 and e == 25 and a == 11 and b == 6 and invS == (17,11,2) and degs == {2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}) then return "gushel26''";
    if ((d == 18 or d == 26) and e == 3 and a == 7 and b == 4 and invS == (11,3,0) and degs == {2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}) then if numberNodes(S,Verbose=>false) == 1 and discriminant X == 26 then return "mukai26''";
    --
    if (d == 20 and e == 14 and a == 8 and b == 5 and invS == (13,6,1) and degs == {2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}) then return "october2021-20";
    if (d == 26 and e == 7 and a == 5 and b == 4 and invS == (9,2,1) and degs == {2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}) then return "october2021-26''";
    if (d == 28 and e == 13 and a == 8 and b == 5 and invS == (13,6,1) and degs == {2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}) then return "october2021-28";
    if (d == 28 and e == 10 and a == 6 and b == 5 and invS == (11,4,1) and degs == {2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}) then return "october2021-28-2";
    if (d == 34 and e == 13 and a == 9 and b == 5 and invS == (14,7,1) and degs == {2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3}) then return "october2021-34'";
    if (d == 34 and e == 7 and a == 9 and b == 6 and invS == (15,7,0) and degs == {2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3}) then if numberNodes(S,Verbose=>false) == 1 and discriminant X == 42 then return "april2022-GM42''";
    "NotRecognized"
);

random GushelMukaiFourfold := o -> X -> (
    X' := gushelMukaiFourfold(surface X,random(2,surface X) * ambientFivefold X,InputCheck=>-1);
    X'.cache#"AmbientFivefold" = ambientFivefold X;
    if X.cache#?(surface X,"label") and (not X'.cache#?(surface X',"label")) then X'.cache#(surface X',"label") = X.cache#(surface X,"label");
    X'
);

toGrass = method(TypicalValue => MultirationalMap)

toGrass EmbeddedProjectiveVariety := (cacheValue "toGrass") (X -> (
    K := coefficientRing X;
    Y6 := GG(K,1,4);
    if dim X == 6 and dim ambient X == 9 and degrees X == {({2},5)} and degree X == 5
    then return (findIsomorphism(X,Y6,Verify=>true))||Y6;
    if dim X == 5 and dim ambient X == 8 and degrees X == {({2},5)} and degree X == 5
    then (
        if dim singLocus X == -1 then return ((findIsomorphism(X,Y5 K,Verify=>true))||(Y5 K)) * (mapY5 K);
        p := sum decompose singLocus X;
        if not isPoint p then error "expected a point to be the vertex of a cone";
        p' := Var ideal submatrix'(vars ring ambient X,{0});
        f := inverse findIsomorphism(p,p',Verify=>true);
        X' := f^^ X;
        j := toGrass Var sub(ideal X',K[flatten entries submatrix'(vars ring ambient X,{0})]);
        y0 := local y0;
        coneG := quotient sub(ideal target j,K[y0,gens ring ambient target j]);
        J := multirationalMap(
                 rationalMap(ring X,ring X',matrix first factor inverse f) *
                 rationalMap(ring X',coneG,matrix{{first gens ring ambient X}}|sub(lift(matrix first factor j,ring ambient source j),ring ambient X))
             );
        if not(source J == X and degree(J,Strategy=>"random point") == 1) then error "internal error encountered";
        return J;
    );
    if dim X == 4 and dim ambient X == 7 and degrees X == {({2},5)} and degree X == 5
    then return ((findIsomorphism(X,Y4 K,Verify=>true))||(Y4 K)) * (mapY4 K);
    if dim X == 4 and dim ambient X == 8 and degrees X == {({2},6)} and degree X == 10
    then (
        Y := varietyDefinedBylinearSyzygies X;
        psi := toGrass Y;
        if dim singLocus Y == -1 then return psi|X;
        return psi * rationalMap(ring target psi,ring Y6,submatrix'(vars ring ambient target psi,{0}));
    );
    error "expected a Gushel-Mukai fourfold, or a del Pezzo fourfold/fivefold/sixfold";
));

toGrass GushelMukaiFourfold := (cacheValue "toGrass") (X -> ( -- for better documentation
    Y := ambientFivefold X;
    psi := toGrass Y;
    if dim singLocus Y == -1 then return psi|X;
    K := coefficientRing X;
    check multirationalMap((psi|X) * rationalMap(ring target psi,ring GG(K,1,4),submatrix'(vars ring ambient target psi,{0})),GG(K,1,4))
));

isAdmissibleGM = method();

isAdmissibleGM ZZ := d -> (
    if d <= 8 then return false;
    if d % 8 == 0 then return false;
    if d % 8 != 2 and d % 8 != 4 then return false;
    for p from 3 to floor(d/2) do if ((p % 4 == 0 or p % 4 == 2 or p % 4 == 3) and isPrime p and d % p == 0) then return false;
    return true;
);

isAdmissibleGM GushelMukaiFourfold := X -> isAdmissibleGM discriminant X;

parameterCount GushelMukaiFourfold := o -> X -> (
    S := surface X;
    Y := ambientFivefold X;
    if o.Verbose then <<"S: "|toString(? ideal S)<<endl;
    if o.Verbose then <<"X: GM fourfold containing S"<<endl;
    if o.Verbose then <<"Y: del Pezzo fivefold containing X"<<endl;
    N := normalSheaf(S,Y);
    h1N := rankHH(1,N);
    if o.Verbose then <<"h^1(N_{S,Y}) = "|toString(h1N)<<endl;
    if h1N != 0 then error("condition not satisfied: h^1(N_{S,Y}) = 0");
    h0N := rankHH(0,N);
    if o.Verbose then <<"h^0(N_{S,Y}) = "|toString(h0N)<<endl;
    m := (# basisMem({2},S)) - 5;
    OS := OO_(variety S);
    h1OS2 := rank HH^1 (OS(2));
    if h1OS2 != 0 then error("condition not satisfied: h^1(O_S(2)) = 0");
    m' := 40 - ((hilbertPolynomial S) 2);
    if m != m' then error("condition not satisfied: h^0(I_{S,Y}(2)) == h^0(O_Y(2)) - \\chi(O_S(2))");
    if o.Verbose then (
        <<"h^1(O_S(2)) = 0, ";
        <<"and h^0(I_{S,Y}(2)) = "|toString(m)|" = h^0(O_Y(2)) - \\chi(O_S(2));"|newline|"in particular, h^0(I_{S,Y}(2)) is minimal"<<endl;
    );
    if o.Verbose then <<"h^0(N_{S,Y}) + "|toString(m-1)|" = "|toString(h0N + m - 1)<<endl;
    NX := normalSheaf(S,X);
    h0NX := rankHH(0,NX);
    if o.Verbose then <<"h^0(N_{S,X}) = "|toString(h0NX)<<endl;
    if o.Verbose then <<"dim{[X] : S ⊂ X ⊂ Y} >= "|toString(h0N + m-1 - h0NX)<<endl;
    if o.Verbose then <<"dim P(H^0(O_Y(2))) = 39"<<endl;
    w := 39 - (h0N + m-1 - h0NX);
    if o.Verbose then <<"codim{[X] : S ⊂ X ⊂ Y} <= "|toString(w)<<endl;
    return X.cache#(S,"parameterCount") = (w,(m,h0N,h0NX));
);

sigmaQuadric = method();
sigmaQuadric GushelMukaiFourfold := X -> (
    f := toRationalMap toGrass X;
    H := image(f,1);
    Q := ideal random(2,makeSubvariety image f);
    S := trim(Q + tangentialChowForm(chowEquations(H_0),0,1));
    return trim lift(f^* S,ambient source f);
);

unirationalParametrization GushelMukaiFourfold := (cacheValue "unirationalParametrization") (X -> (
    K := coefficientRing X;
    S := sigmaQuadric X;
    s := parametrize S;
    s = rationalMap(Grass(0,2,K,Variable=>"u"),source s) * s;
    K' := frac(K[gens source s]);
    ringP8' := K'[gens ring ambient X];
    p' := trim minors(2,vars ringP8' || sub(matrix s,K'));
    Y := ideal ambientFivefold X;
    V := ideal coneOfLines(Var sub(Y,ringP8'),Var p');
    j := parametrize((ideal select(V_*,v -> degree v == {1})) + (ideal first gens ring V));
    W := trim (map j) V;
    P := plucker(W,2); while dim P <= 0 do P = plucker(W,2); P = trim sub(plucker P,vars ring W);
    Q := trim quotient(W,P);
    q := trim minors(2,vars ring W || transpose submatrix(coefficients parametrize(P+Q),,{0}));
    f := (inverse(rationalMap trim sub(q,quotient Q),Certify=>true)) * j;
    ringP2xP2 := (source s) ** K[gens source f];
    K'' := frac(ringP2xP2);
    ringP8'' := K''[gens ring ambient X];
    X'' := sub(ideal X,ringP8'');
    p'' := sub(p',ringP8'');
    ringP1'' := Grass(0,1,K'',Variable=>"v");
    l := rationalMap(ringP1'',ringP8'', (vars ringP1'') * (sub(matrix s,K'') || sub(matrix f,K'')));
    e := parametrize trim quotient(trim (map l) X'',trim (map l) p'');
    el := rationalMap((map e) * (map l));
    el = rationalMap(source el,target el,(lcm apply(flatten entries sub(last coefficients matrix el,K''),denominator)) * (matrix el));
    psi := rationalMap(ringP2xP2,ring ambient X,sub(transpose coefficients el,ringP2xP2));
    Psi := multirationalMap({parametrize psi},X);
    if not isSubset(Psi point source Psi,X) then error "internal error encountered";
    Psi
));

toGushel = method();
toGushel GushelMukaiFourfold := X -> (
    if dim singLocus ambientFivefold X >= 0 then return X;
    j := toRationalMap toGrass X;
    Y := local Y;
    i := rationalMap(target j,(coefficientRing X)[Y,gens ambient target j],0|vars target j);
    i = rationalMap(i,Dominant=>sub(ideal target j,target i));
    S := trim lift((j*i) ideal surface X,ambient target i);
    Sv := intersect(S,ideal submatrix'(vars ambient target i,{0}));
    try H := ideal random({{1},{1}},Var Sv) else error "not able to specialize to Gushel type";
    h := (parametrize H)||(target i);
    gushelMukaiFourfold h^* S
);

fromOrdinaryToGushel = method();
fromOrdinaryToGushel GushelMukaiFourfold := X -> try toGushel X else error "not able to deform to Gushel type";

------------------------------------------------------------------------
---------------- arXiv:2002.07026 --------------------------------------
------------------------------------------------------------------------

GMtables = method(Options => {Verify => true});
GMtables (ZZ,Ring) := o -> (i,K) -> (
    if i < 1 or i > 21 then error "expected an integer between 1 and 21";
    t := gens ring PP_K^2;
    s := multirationalMap rationalMap(ring PP_K^2,ring PP_K^5,{t_0^2,t_0*t_1,t_1^2,t_0*t_2,t_1*t_2,0});
    S := image s;
    p0 := Var ideal(t_0,t_1); -- base point of s
    assert(p0 == baseLocus s);
    x := gens ring PP_K^5;
    L0 := Var ideal(x_0,x_1,x_2,x_5); -- directrix line of S
    u := gens ring PP_K^3;
    b := multirationalMap rationalMap(ring PP_K^3,ring PP_K^5,{u_0^3*u_1-u_0*u_1^3, u_0^2*u_1^2-u_0*u_1^3+u_0^3*u_2-u_1^3*u_3, u_0^2*u_1*u_2-u_1^3*u_3, -u_0^2*u_1^2+u_0*u_1^3+u_0*u_1^2*u_2-u_1^3*u_3, u_0^2*u_1*u_3-u_1^3*u_3, u_0^2*u_1^2-u_0*u_1^3+u_0*u_1^2*u_3-u_1^3*u_3});
    B := image b;
    E := (Var ideal(u_0,u_1), Var ideal(u_3,u_0), Var ideal(u_2,u_1), Var ideal(u_2-u_3,u_0-u_1)); -- base locus of b: E_0 triple line, E_1,E_2,E_3 simple lines
    assert(⋃ {3*E_0,E_1,E_2,E_3} == baseLocus b);
    pE := apply(E,L -> parametrize L);
    curveOnS := e -> (
        assert(instance(e,ZZ) and e >= 1 and e <= 4);
        local j;
        if e == 1 then j = parametrize random({1},p0);
        if e == 2 then j = parametrize random({1},0_(PP_K^2));
        if e == 3 then j = inverse rationalMap(p0_(random({2},p0)));
        if e == 4 then (
            p1 := point PP_K^2;
            j = inverse rationalMap(p1_(random({2},p1)));
        );
        g := rationalMap(j * s,Dominant=>true);
        (target g).cache#"rationalParametrization" = g;
        return target g;
    );
    curveOnB := e  -> (
        assert(instance(e,ZZ) and e >= 1 and e <= 5);
        local C; local p; local p'; local p''; local j;
        if e == 1 then (
            p = point source pE_0;
            C = random({{1},{1}},pE_0 p);
            j = parametrize C;
        );
        if e == 2 then (
            p = point source pE_0;
            p' = point source pE_0;
            C = random({{2},{1}},pE_0(p) + pE_0(p'));
            j = inverse rationalMap((pE_0 p)_C);
        );
        if e == 3 then (
            p = point source pE_0;
            p' = point source pE_1;
            p'' = point source pE_2;
            C = random({{2},{1}},pE_0(p) + pE_1(p') + pE_2(p''));
            j = inverse rationalMap((pE_0 p)_C);
        );
        if e == 4 then (
            p = point source pE_0;
            p' = point source pE_1;
            C = random({{2},{1}},pE_0(p) + pE_1(p'));
            j = inverse rationalMap((pE_0 p)_C);
        );
        if e == 5 then (
            p = point source pE_0;
            C = random({{2},{1}},pE_0 p);
            j = inverse rationalMap((pE_0 p)_C);
        );
        g := rationalMap(j * b,Dominant=>true);
        (target g).cache#"rationalParametrization" = g;
        return target g;
    );
    SB :=     {,S,S,S,S,B,S,S,B,S,B,B,S,S,S,S,S,S,B,B,S,S};
    degsV :=  {,2,2,8,4,3,1,3,2,1,4,4,8,7,4,6,6,5,4,4,4,7};
    sGenV :=  {,0,0,5,1,0,0,0,0,0,1,0,5,3,1,2,3,1,0,0,0,3};
    degsC :=  {,2,1,4,3,2,1,3,1,1,4,4,4,4,2,4,3,3,5,4,4,4};
    inters := {,0,0,0,0,7,0,0,5,0,4,6,0,0,0,0,0,0,3,6,0,0};
    verify := (i0,S',V',C') -> (
        if not o.Verify then return (S',V',C');
        try assert(
            S' == SB_i0 and
            dim V' == 2 and
            degree V' == degsV_i0 and
            sectionalGenus V' == sGenV_i0 and
            dim C' == 1 and
            degree C' == degsC_i0 and
            sectionalGenus C' == 0 and
            isSubset(C',S') and isSubset(C',V') and
            degree((S' * V') \ C') == inters_i0
        ) else error ("something went wrong while constructing a triple for case "|toString(i));
        return (S',V',C');
    );
    local C; local p; local j; local D; local V; local v; local phi;
    if i == 1 then (
        C = curveOnS 2;
        V = random({{1},{1},{2}},C);
        return verify(i,S,V,C);
    );
    if i == 2 then (
        C = curveOnS 1;
        V = random({{1},{1},{2}},C);
        return verify(i,S,V,C);
    );
    if i == 3 then (
        C = ⋃ {L0,curveOnS 1,curveOnS 1,curveOnS 1};
        V = random({{2},{2},{2}},C);
        return verify(i,S,V,C);
    );
    if i == 4 then (
        C = curveOnS 3;
        V = random({{1},{2},{2}},C);
        return verify(i,S,V,C);
    );
    if i == 5 then (
        C = curveOnB 2;
        D = curveOnS 2;
        phi = findIsomorphism(D,C,Verify=>o.Verify);
        V = phi S;
        return verify(i,B,V,C);
    );
    if i == 6 then (
        C = curveOnS 1;
        V = random({{1},{1},{1}},C);
        return verify(i,S,V,C);
    );
    if i == 7 then (
        C = curveOnS 3;
        D = curveOnS 3;
        phi = findIsomorphism(D,C,Verify=>o.Verify);
        V = phi S;
        return verify(i,S,V,C);
    );
    if i == 8 then (
        C = curveOnB 1;
        D = random({{1},{1},{1},{1}},0_(PP_K^5));
        V = random({{1},{1},{2}},D);
        phi = findIsomorphism(D,C,Verify=>o.Verify);
        V = phi V;
        return verify(i,B,V,C);
    );
    if i == 9 then return verify(i,S,random({{1},{1},{1}},L0),L0);
    if i == 10 then (
        C = curveOnB 4;
        j = rationalMap((parametrize PP_K^(1,4)) << PP_K^5,Dominant=>true);
        (target j).cache#"rationalParametrization" = j;
        D = image j;
        V = random({{1},{2},{2}},D);
        phi = findIsomorphism(D,C,Verify=>o.Verify);
        V = phi V;
        return verify(i,B,V,C);
    );
    if i == 11 then (
        C = curveOnB 4;
        p = for i to 1 list point PP_K^2;
        v = rationalMap(p_0 + 2*p_1,3);
        V = image v;
        j = rationalMap((inverse rationalMap((p_1)_(random({2},p_1)))) * v,Dominant=>true);
        (target j).cache#"rationalParametrization" = j;
        D = image j;
        phi = findIsomorphism(D,C,Verify=>o.Verify);
        V = phi V;
        return verify(i,B,V,C);
    );
    if i == 12 then (
        C = curveOnS 4;
        V = random({{2},{2},{2}},C);
        return verify(i,S,V,C);
    );
    if i == 13 then (
        C = curveOnS 4;
        p = for i to 9 list point PP_K^2;
        v = rationalMap((⋃ take(p,9)) + 3*p_9,5);
        V = image v;
        j = rationalMap((inverse rationalMap((p_0)_(random({2},⋃{p_0,p_1,p_2,p_9})))) * v,Dominant=>true);
        (target j).cache#"rationalParametrization" = j;
        D = image j;
        phi = findIsomorphism(D,C,Verify=>o.Verify);
        V = phi V;
        return verify(i,S,V,C);
    );
    if i == 14 then (
        C = curveOnS 2;
        V = random({{1},{2},{2}},C);
        return verify(i,S,V,C);
    );
    if i == 15 then (
        C = curveOnS 4;
        p = for i to 6 list point PP_K^2;
        v = rationalMap((⋃ take(p,6)) + 2*p_6,4);
        V = image v;
        j = rationalMap((inverse rationalMap((p_0)_(random({2},⋃{p_0,p_1,p_6})))) * v,Dominant=>true);
        (target j).cache#"rationalParametrization" = j;
        D = image j;
        phi = findIsomorphism(D,C,Verify=>o.Verify);
        V = phi V;
        return verify(i,S,V,C);
    );
    if i == 16 then (
        C = curveOnS 3;
        p = for i to 9 list point PP_K^2;
        v = rationalMap(⋃ p,4) << PP_K^5;
        V = image v;
        j = (inverse rationalMap((p_0)_(random({2},⋃ take(p,5))))) * v;
        j = rationalMap(j,image j);
        (target j).cache#"rationalParametrization" = j;
        D = image j;
        phi = findIsomorphism(D,C,Verify=>o.Verify);
        V = phi V;
        return verify(i,S,V,C);
    );
    if i == 17 then (
        C = curveOnS 3;
        p = for i to 3 list point PP_K^2;
        v = rationalMap(⋃ p,3);
        V = image v;
        j = rationalMap((parametrize random({1},0_(PP_K^2))) * v,Dominant=>true);
        (target j).cache#"rationalParametrization" = j;
        D = image j;
        phi = findIsomorphism(D,C,Verify=>o.Verify);
        V = phi V;
        return verify(i,S,V,C);
    );
    if i == 18 then (
        C = curveOnB 5;
        p = for i to 1 list point PP_K^2;
        v = rationalMap(p_0 + 2*p_1,3);
        V = image v;
        j = rationalMap((inverse rationalMap((p_0)_(random({2},p_0)))) * v,Dominant=>true);
        (target j).cache#"rationalParametrization" = j;
        D = image j;
        phi = findIsomorphism(D,C,Verify=>o.Verify);
        V = phi V;
        return verify(i,B,V,C);
    );
    if i == 19 then (
        C = curveOnB 4;
        V = PP_K^(2,2);
        v = parametrize V;
        p = point source v;
        j = rationalMap((inverse rationalMap(p_(random({2},p)))) * v,Dominant=>true);
        (target j).cache#"rationalParametrization" = j;
        D = image j;
        phi = findIsomorphism(D,C,Verify=>o.Verify);
        V = phi V;
        return verify(i,B,V,C);
    );
    if i == 20 then (
        C = curveOnS 4;
        V = PP_K^(2,2);
        v = parametrize V;
        p = point source v;
        j = rationalMap((inverse rationalMap(p_(random({2},p)))) * v,Dominant=>true);
        (target j).cache#"rationalParametrization" = j;
        D = image j;
        phi = findIsomorphism(D,C,Verify=>o.Verify);
        V = phi V;
        return verify(i,S,V,C);
    );
    if i == 21 then (
        C = curveOnS 4;
        p = for i to 8 list point PP_K^2;
        v = rationalMap(⋃ p,4);
        V = image v;
        j = rationalMap((inverse rationalMap((p_0)_(random({2},⋃ take(p,4))))) * v,Dominant=>true);
        (target j).cache#"rationalParametrization" = j;
        D = image j;
        phi = findIsomorphism(D,C,Verify=>o.Verify);
        V = phi V;
        return verify(i,S,V,C);
    );
);

GMtables (Ring,String) := o -> (K,name) -> ( -- store all data in a file
F := openOut (name|".dat");
F <<"-- file created automatically using: GMtables("|toExternalString K|",\""|name|"\",Verify=>"<<(o.Verify)<<"); date: "|(toString get "!date")<<endl;
F <<"GMtables ZZ := o -> j -> ("<<endl;
F << "    x := gens ring PP_("<<(toExternalString K)<<")^5;"<<endl;
close F;
local A;
x := gens ring PP_K^5;
for i from 1 to 21 do (
   A = GMtables(i,K,Verify=>o.Verify);
   F = openOutAppend (name|".dat");
   F << "    if j == "<<i<<" then return (projectiveVariety("<<toString ideal A_0<<",Saturate=>false,MinimalGenerators=>false),"<<endl<<"                           projectiveVariety("<<toString ideal A_1<<",Saturate=>false,MinimalGenerators=>false),"<<endl<<"                           projectiveVariety("<<toString ideal A_2<<",Saturate=>false,MinimalGenerators=>false));"<<endl;
   close F;
);
F = openOutAppend (name|".dat");
F << ");"<<endl<<endl;
close F;
);

GMtables ZZ := o -> i -> (
    if i < 1 or i > 21 then error "expected integer between 1 and 21";
    try value get "data_examples.dat" else error("file \"data_examples.dat\" not found. You can make it using GMtables(K,\"data_examples\")");
    GMtables i
);

GMtables (EmbeddedProjectiveVariety,EmbeddedProjectiveVariety,EmbeddedProjectiveVariety) := o -> (B,V,C) -> (
    if not ((dim B == 2 or dim B == 3) and degree B == dim B + 1 and dim V == 2 and dim C == 1 and ring ambient C === ring ambient V and ring ambient C === ring ambient B) then error "invalid input for GMtables";
    psi := rationalMap(ideal B,Dominant=>2);
    if o.Verify then <<"-- constructing random GM fourfold from the triple..."<<endl;
    X := gushelMukaiFourfold(psi ideal V,InputCheck=>(if o.Verify then 10 else 0),Verbose=>true);
    if o.Verify then <<"-- GM fourfold correctly constructed."<<endl;
    if not o.Verify then return X;
    <<"-- recognizing fourfold..."<<endl;
    i := recognize X;
    if not (instance(i,ZZ) and i >= 1 and i <= 21) then error "something went wrong";
    <<"-- done (got case "<<i<<" of Table 1)"<<endl;
    <<"-- calculating discriminant..."<<endl;
    describe X;
    (a,b) := last cycleClass X;
    d := discriminant X;
    D := toString d;
    if d % 8 == 2 then (
        if even(a+b) and odd(b) then D = D|"(')"
        else
            if odd(a+b) and even(b) then D = D|"('')"
            else error "internal error encountered"
    );
    <<"-- done (got "<<D<<")"<<endl;
    <<"-- running parameterCount..."<<endl;
    (c,h) := parameterCount(X,Verbose=>true);
    <<"-- parameterCount successfully terminated (got: "<<(c,h)<<")"<<endl;
    S := surface X;
    T := "Case "|toString(i)|"/21 of Table 1 in arXiv:2002.07026:"|newline|"deg(S) = "|toString(degree S)|", g(S) = "|toString(sectionalGenus S);
    T = T|", K_S^2 = "|toString(12*(euler hilbertPolynomial S) - (eulerCharacteristic S));
    T = T|", class in G(1,4) = "|toString(first cycleClass X)|","|newline|"codim in M_4 = "|toString(c)|", discriminant = "|D;
    T = T|","|newline|"h^0(I_{S,Y}(2)) = "|toString(h_0)|", h^0(N_{S,Y}) = "|toString(h_1)|", h^0(N_{S,X}) = "|toString(h_2);
    << endl << "-- *** --" << endl << T << endl << "-- *** --"<<endl;
    return X;
);

fourfoldFromTriple = method(Options => {InputCheck => 1, Verbose => true});
fourfoldFromTriple (ZZ,VisibleList) := o -> (i,E) -> (
    psi := rationalMap(ideal E_0,Dominant=>2);
    X := gushelMukaiFourfold(psi ideal E_1,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
    X.cache#(surface X,"label") = i;
    return X;
);

------------------------------------------------------------------------
----------- GM fourfolds from curves on surfaces in PP^6 ---------------
------------------------------------------------------------------------

makeGMfromCurveOnSurfaceInP6 = method(Options => {InputCheck => 1, Verbose => true, "Gluing" => "cubic scroll", Degrees => hashTable{1=>(1,infinity),2=>(0,infinity),3=>(0,1)}});
makeGMfromCurveOnSurfaceInP6 EmbeddedProjectiveVariety := o -> C -> (
    S := ambientVariety C;
    if not (dim C == 1 and dim S == 2 and dim ambient S == 6) then error "expected a curve on a surface in PP^6";
    B := if o#"Gluing" === "cubic scroll"
         then glueScroll C
         else if o#"Gluing" === "quartic scroll"
         then glueScroll' C
         else error "the option \"Gluing\" expects \"cubic scroll\" or \"quartic scroll\"";
    psi := rationalMap B;
    psi' := if S.cache#?"rationalParametrization" then (parametrize S) * psi else psi|S;
    H := image(1,psi');
    if codim H == 0 then (if o.Verbose then (<<"got surface in GG(1,4) ⊂ PP^9 with linear span of dimension 9"<<endl); error "expected linear span of dimension at most 8");
    if numgens ideal H > o.Degrees#1_1 then error "request on the degrees is not satisfied";
    -- V := if char coefficientRing C <= 65521 then image(psi',"F4") else image psi';
    V := image psi';
    if o.Verbose then <<"got surface in GG(1,4) ⊂ PP^9 with ideal generated in degrees "<<toStringDegreesVar V<<endl;
    degs := flatten degrees ideal V;
    if codim H == 1 and # select(degs,d -> d == 2) < 6 then error "the surface in G(1,4) is not contained in any GM fourfold";
    if # select(degs,d -> d > 3) > 0 or
       # select(degs,d -> d == 2) < o.Degrees#2_0 or # select(degs,d -> d == 2) > o.Degrees#2_1 or
       # select(degs,d -> d == 3) < o.Degrees#3_0 or # select(degs,d -> d == 3) > o.Degrees#3_1
    then error "request on the degrees is not satisfied";
    if o.InputCheck >= 2 then (
        if isIsomorphism rationalMap(psi|S,V) then (
            if S.cache#?"FiniteNumberOfNodes" and S.cache#"FiniteNumberOfNodes" == 0 then (V.cache#"top" = V; V.cache#"singularLocus" = 0_V);
            if S.cache#?"FiniteNumberOfNodes" and S.cache#"FiniteNumberOfNodes" > 0 then V.cache#"FiniteNumberOfNodes" = S.cache#"FiniteNumberOfNodes";
            V.cache#"euler" = eulerCharacteristic S;
            if o.Verbose then <<"isomorphism between surface in PP^6 and surface in GG(1,4): YES"<<endl;
        ) else (if o.Verbose then <<"isomorphism between surface in PP^6 and surface in GG(1,4): NO"<<endl);
    );
    X := gushelMukaiFourfold(V%image(psi,2),InputCheck=>o.InputCheck);
    X.cache#"Construction" = (B,C);
    return X;
);

glueScroll = method(); -- glue P^1xP^2 along a curve
glueScroll EmbeddedProjectiveVariety := C -> (
    if not (dim C == 1 and dim ambientVariety C == 2 and dim ambient C == 6) then error "expected a curve on a surface in PP^6";
    if not((degree C <= 5 and sectionalGenus C == 0) or (degree C == 5 and sectionalGenus C == 1)) then error "not implemented yet: expected a rational curve of degree at most 5 or an elliptic curve of degree 5";
    K := coefficientRing C;
    (p,L,s) := CubicScroll K;
    E := image s;
    local D;
    if degree C == 5 and sectionalGenus C == 0 then (
        D = image(PP_K^(1,3) << source s);
        D = ((point D) ===> (point L)) D;
        E = (s(D) ===> C) E;
    );
    if degree C == 5 and sectionalGenus C == 1 then (
        f := super parametrize ambientVariety C;
        g := f|(f^* C);
        H := random(1,linearSpan C);
        pr := inverse parametrize H;
        g = g * pr;
        j := inverse rationalMap((linearSpan {g point source g,g point source g})_(image g),Dominant=>true);
        B := image((rationalMap lift(matrix j,ring ambient source j)) * inverse pr);
        if not(dim B == 2 and degree B == 3 and sectionalGenus B == 0 and isSubset(C,B)) then error "something went wrong";
        E = (random(1,0_E) * E ===> B) E;
    );
    if degree C == 4 and sectionalGenus C == 0 then (
        D = random({{2},{1}},0_(source s));
        assert(dim(D*(baseLocus s)) == -1);
        E = (s(D) ===> C) E;
    );
    if degree C == 3 and sectionalGenus C == 0 then (
        E = (E * random({{1},{1}},0_E) ===> C) E;
    );
    if degree C == 2 and sectionalGenus C == 0 then (
        D = random({{1},{1}},0_(source s));
        assert(dim(D*(baseLocus s)) == -1);
        E = (s(D) ===> C) E;
    );
    if degree C == 1 and sectionalGenus C == 0 then (
        D = random({{1},{1}},point L);
        assert(dim(D*(baseLocus s)) == 0);
        E = (s(D) ===> C) E;
    );
    if not(dim E == 3 and degree E == 3 and isSubset(C,E)) then error "something went wrong";
    E
);

glueScroll' = method(); -- glue quartic scroll fourfold along a curve
glueScroll' EmbeddedProjectiveVariety := C -> (
    if not (dim C == 1 and dim ambientVariety C == 2 and dim ambient C == 6) then error "expected a curve on a surface in PP^6";
    if not(degree C <= 6 and sectionalGenus C == 0) then error "not implemented yet: expected rational curve of degree at most 6";
    K := coefficientRing C;
    (P,s) := QuarticScroll K;
    E := image s;
    local D;
    if degree C == 6 and sectionalGenus C == 0 then (
        D = image(PP_K^(1,3) << source s);
        D = (sum(4,i -> point D) ===> sum(P,point)) D;
        E = (s(D) ===> C) E;
    );
    if degree C == 5 and sectionalGenus C == 0 then (
        D = image(PP_K^(1,3) << source s);
        D = (sum(3,i -> point D) ===> sum {point P_0,point P_0,point P_1}) D;
        E = (s(D) ===> C) E;
    );
    if degree C == 4 and sectionalGenus C == 0 then (
        D = image(PP_K^(1,3) << source s);
        D = (sum(4,i -> point D) ===> sum {point P_0,point P_0,point P_1,point P_2}) D;
        E = (s(D) ===> C) E;
    );
    if degree C == 3 and sectionalGenus C == 0 then (
        D = random({2:{1},{2}},0_(source s));
        D = (sum(3,i -> point D) ===> sum {point P_0,point P_1,point P_2}) D;
        E = (s(D) ===> C) E;
    );
    if degree C == 2 and sectionalGenus C == 0 then (
        D = random({2:{1},{2}},0_(source s));
        D = (sum(2,i -> point D) ===> sum {point P_0,point P_0}) D;
        E = (s(D) ===> C) E;
    );
    if degree C == 1 and sectionalGenus C == 0 then (
        D = random({3:{1}},point P_0);
        E = (s(D) ===> C) E;
    );
    if not(dim E == 4 and degree E == 4 and isSubset(C,E)) then error "something went wrong";
    E
);

CubicScroll = memoize(K -> (
    s := (multirationalMap segre parametrize(PP_K^1 ** PP_K^2)) << PP_K^6;
    p := (baseLocus s)\top baseLocus s;
    L := top baseLocus s;
    assert(baseLocus s == L + p and dim(L*p) == -1 and dim L == 1 and degree L == 1 and dim p == 0 and degree p == 1);
    (p,L,s)
));

QuarticScroll = memoize(K -> (
    b := super parametrize(PP_K[1,1,1,1]);
    b = b * rationalMap point target b;
    P0 := ((baseLocus b)\(support baseLocus b))\(support baseLocus b);
    (P1,P2,P3) := toSequence decompose((baseLocus b)\\P0);
    P := {P0,P1,P2,P3};
    assert(⋃ {3*P_0,P_1,P_2,P_3} == baseLocus b);
    assert(dim(P0*P1)==1 and dim(P0*P2)==1 and dim(P0*P3)==1);
    assert(all(P,L -> degree L == 1 and dim L == 2));
    (P,b)
));

curvesOnSurface = method(); -- some curves of degree d and genus g on a rational surface
curvesOnSurface (EmbeddedProjectiveVariety,ZZ,ZZ) := (S,d,g) -> (
    L := S.cache#"linear system on PP^2";
    if #L != 4 then error "not implemented yet: the linear system on PP^2 must be of the form {a,i,j,k}";
    U := {};
    if g == 0 then (
        for a from 1 to 2 do for i to L_1 do for j to L_2 do for k to L_3 do
        if a*L_0-i-2*j-3*k == d and i+j+k <= (if a == 1 then 2 else 5) then U = append(U,(a,{i,j,k}));
    ) else if g == 1 then (
        for i to L_1 do for j to L_2 do for k to L_3 do
        if 3*L_0-i-2*j-3*k == d and i+j+k <= 9 then U = append(U,(3,{i,j,k}));
    ) else error "not implemented yet: the genus must be 0 or 1";
    apply(U,u -> S.cache#"takeCurve" u)
);

takeGMsfromSurfaceInP6 = method(Options => {InputCheck => 1, Verbose => true, "Gluing" => "cubic scroll", Degrees => (options makeGMfromCurveOnSurfaceInP6).Degrees, "OnlyNewOnes" => false, "Output" => true});
takeGMsfromSurfaceInP6 EmbeddedProjectiveVariety := o -> S -> (
    if dim S != 2 or dim ambient S != 6 then error "expected a surface in PP^6";
    if not(S.cache#?"linear system on PP^2" and S.cache#?"takeCurve") then error "expected a surface constructed with the function \"surface\"";
    a := apply(toSequence S.cache#"linear system on PP^2",toString);
    if #a != 4 then error "not implemented yet: the surface must be of the form \"surface {a,i,j,k}\"";
    S.cache#"nice description" = "S("|a_0|";"|a_1|","|a_2|","|a_3|",NumNodes=>"|(if S.cache#?"FiniteNumberOfNodes" then toString(S.cache#"FiniteNumberOfNodes") else "?")|") ⊂ PP^"|toString(dim linearSpan S)|(if codim linearSpan S > 0 then " ⊂ PP^"|toString(dim ambient S) else "")|" of degree: "|toString(degree S)|", genus: "|toString(sectionalGenus S)|", and degrees: "|(toStringDegreesVar S);
    if o.Verbose then <<"*******"<<endl<<"Surface: "<<S.cache#"nice description"<<" glued with a "<<o#"Gluing"<<endl;
    local W; local X; U := {};
    for d from 1 to 6 do for g to 1 do (
        if g == 1 and d != 5 then continue;
        W = curvesOnSurface(S,d,g);
        for i to #W-1 do (
            if o.Verbose then <<"Case "<<i+1<<" of "<<#W<<": "<<(if d >= 3 and g == 0 then "rational" else if d >= 3 and g == 1 then "elliptic" else "")<<" curve of degree "<<d<<" with plane representation "<<(W_i).cache#"plane representation"<<" on "<<"S("|a_0|";"|a_1|","|a_2|","|a_3|",NumNodes=>"|(if S.cache#?"FiniteNumberOfNodes" then toString(S.cache#"FiniteNumberOfNodes") else "?")|")"<<endl;
            try X = makeGMfromCurveOnSurfaceInP6(W_i,InputCheck=>o.InputCheck,Verbose=>o.Verbose,"Gluing"=>o#"Gluing",Degrees=>o.Degrees) else continue;
            if (not o#"OnlyNewOnes" or recognize X === "NotRecognized") and (not member(describe X,apply(U,describe))) then (
                U = append(U,X);
                if o.Verbose then <<"using "<<o#"Gluing"<<" found new GM fourfold with description:"<<endl<<describe X<<endl;
            ) else (if o.Verbose then <<"using "<<o#"Gluing"<<" found GM fourfold of discriminant "<<discriminant X<<" but that was already included"<<endl);
        );
    );
    if o.Verbose then (
        <<endl<<"Summary for the surface: "<<S.cache#"nice description"<<endl<<"glued with a "<<o#"Gluing"<<". Found "<<#U<<" new GM fourfold(s)"<<endl<<endl;
        local C;
        for i to #U-1 do (
            C = ((U_i).cache#"Construction")_1;
            assert(ambientVariety C === S);
            <<i+1<<" of "<<#U<<": gushelMukaiFourfold("|(toExternalString new Array from S.cache#"linear system on PP^2")|","|(toExternalString new Array from flatten C.cache#"plane representation")|",\""|(o#"Gluing")|"\""|(if S.cache#?"FiniteNumberOfNodes" and S.cache#"FiniteNumberOfNodes" > 0 then (","|toString(S.cache#"FiniteNumberOfNodes")|")") else ")")<<endl;
            <<"used "<<(if o#"Gluing"=="cubic scroll" then "the cubic Segre PP^1 x PP^2 ⊂ PP^5 ⊂ PP^6" else "a quartic scroll fourfold in PP^6")<<endl;
            <<"glued along "<<(if degree C >= 3 and sectionalGenus C == 0 then "a rational" else if degree C >= 3 and sectionalGenus C == 1 then "an elliptic" else "")<<" curve of degree "<<degree C<<endl;
            <<"Description of the fourfold:"<<endl<<describe(U_i)<<endl<<endl;
        );
        <<endl;
    );
    if o#"Output" then U else apply(U,describe)
);

gushelMukaiFourfold (Array,Array,String,Thing) := o -> (a,b,s,nK) -> (
    if #a =!= #b then error "expected two arrays of the same length";
    if s =!= "cubic scroll" and s =!= "quartic scroll" then error "expected string to be \"cubic scroll\" or \"quartic scroll\"";
    (n,K) := if instance(nK,ZZ) then (nK,ZZ/65521) else if instance(nK,Ring) then (0,nK) else if instance(nK,VisibleList) and #nK==2 then (nK_0,nK_1) else error "not valid input";
    C := (surface(a,K,NumNodes=>n,ambient=>6)).cache#"takeCurve" (first b,toList take(b,-(#b-1)));
    makeGMfromCurveOnSurfaceInP6(C,InputCheck=>o.InputCheck,Verbose=>o.Verbose,"Gluing"=>s)
);
gushelMukaiFourfold (Array,Array,String) := o -> (a,b,s) -> gushelMukaiFourfold(a,b,s,(0,ZZ/65521),InputCheck=>o.InputCheck,Verbose=>o.Verbose);
gushelMukaiFourfold (Array,Array,Thing) := o -> (a,b,nK) -> gushelMukaiFourfold(a,b,"cubic scroll",nK,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
gushelMukaiFourfold (Array,Array) := o -> (a,b) -> gushelMukaiFourfold(a,b,"cubic scroll",(0,ZZ/65521),InputCheck=>o.InputCheck,Verbose=>o.Verbose);
