
-*
   Copyright 2020, Giovanni Staglianò.

   You may redistribute this file under the terms of the GNU General Public
   License as published by the Free Software Foundation, either version 2 of
   the License, or any later version.
*-

if version#"VERSION" < "1.21" then error "this package requires Macaulay2 version 1.21 or newer";

newPackage(
    "SpecialFanoFourfolds",
    Version => "2.8", 
    Date => "Apr 7, 2026",
    Authors => {{Name => "Giovanni Staglianò", Email => "giovanni.stagliano@unict.it" }},
    Headline => "Hodge-special fourfolds",
    Keywords => {"Algebraic Geometry"},
    PackageImports => {"PrimaryDecomposition","TangentCone"},
    PackageExports => {"MultiprojectiveVarieties"},
    DebuggingMode => false,
    Reload => false,
    Certification => {
	"journal name" => "Journal of Software for Algebra and Geometry",
	"journal URI" => "https://msp.org/jsag/",
	"article title" => "The SpecialFanoFourfolds package in Macaulay2",
	"acceptance date" => "2024-04-14",
	"published article URI" => "https://msp.org/jsag/2024/14-1/p12.xhtml",
	"published article DOI" => "10.2140/jsag.2024.14.111",
	"published code URI" => "https://msp.org/jsag/2024/14-1/jsag-v14-n1-x12-SpecialFanoFourfolds.m2",
	"release at publication" => "67f7b2f777314d7f85c02a661d8e54f9d2c5e8d3",
	"version at publication" => "2.7.1",
	"volume number" => "14",
	"volume URI" => "https://msp.org/jsag/2024/14-1/"
	}
)

requiredMultiprojectiveVarietiesVersion := "2.7.1";
if MultiprojectiveVarieties.Options.Version < requiredMultiprojectiveVarietiesVersion then (
    <<endl<<"Your version of the MultiprojectiveVarieties package is outdated (required version "<<requiredMultiprojectiveVarietiesVersion<<" or newer);"<<endl;
    <<"you can manually download the latest version from"<<endl;
    <<"https://github.com/Macaulay2/M2/tree/development/M2/Macaulay2/packages."<<endl;
    <<"To automatically download the latest version of MultiprojectiveVarieties in your current directory,"<<endl;
    <<"you may run the following Macaulay2 code:"<<endl<<"***"<<endl<<endl;
    <<///run "curl -s -o MultiprojectiveVarieties.m2 https://raw.githubusercontent.com/Macaulay2/M2/development/M2/Macaulay2/packages/MultiprojectiveVarieties.m2";///<<endl<<endl<<"***"<<endl;
    error("required MultiprojectiveVarieties package version "|requiredMultiprojectiveVarietiesVersion|" or newer");
);

export{
   "HodgeSpecialFourfold",
   "specialFourfold",
   "SpecialCubicFourfold",
   "specialCubicFourfold",
   "SpecialGushelMukaiFourfold",
   "specialGushelMukaiFourfold",
   "CongruenceOfCurves",
   "detectCongruence",
   "ambientFivefold",
   "surface",
   "curve",
   "NumNodes",
   "InputCheck",
   "parameterCount",
   "normalSheaf",
   "unirationalParametrization",         
   "toGrass",
   "isAdmissible",
   "isAdmissibleGM",
   "mirrorFourfold",
   "Singular",
   "associatedK3surface",
   "associatedCastelnuovoSurface",
   "building",
   "trisecantFlop",
   "GMtables",
   "fanoFourfold",
   "parametrizeFanoFourfold",
   "fromOrdinaryToGushel",
   "IntersectionOfThreeQuadricsInP7",
   "beauvilleMap",
   "DoubleSpecialCubicFourfold",
   "surfaces",
   "polarizedK3surface",
   "latticePolarization",
   "FanoMapType"
}

needsPackage "IntegralClosure"; -- for method: normalization
needsPackage "CharacteristicClasses"; -- for method: eulerCharacteristic
needsPackage("RationalMaps",DebuggingMode=>false); -- for method: inverse3

debug SparseResultants
debug MultiprojectiveVarieties

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
    if dim ambient X == 5 and degrees X == {({3},1)} and codim V == 0 then Fourfold = new SpecialCubicFourfold from Fourfold;
    if dim ambient X == 7 and degrees X == {({2},3)} and codim V == 2 and degrees V == {({2},2)} then Fourfold = new IntersectionOfThreeQuadricsInP7 from Fourfold;
    if dim ambient X == 8 and degrees X == {({2},6)} and codim V == 3 and degrees V == {({2},5)} and degree V == 5 then Fourfold = new SpecialGushelMukaiFourfold from Fourfold;
    Fourfold
);

specialFourfold (EmbeddedProjectiveVariety,EmbeddedProjectiveVariety) := o -> (S,X) -> (
    if ring ideal S =!= ring ideal X then error "expected varieties in the same ambient space";
    if dim S != 2 then error "expected a surface";
    if dim X == 4 and dim ambient X == 5 and degrees X == {({3},1)} and degree X == 3 then return specialCubicFourfold(S,X,NumNodes=>o.NumNodes,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
    if dim X == 4 and dim ambient X == 8 and degrees X == {({2},6)} and degree X == 10 then return specialGushelMukaiFourfold(S,X,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
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
    if dim ambient S == 5 then return specialCubicFourfold(S,NumNodes=>o.NumNodes,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
    if dim ambient S == 6 then return specialFourfold(S,random({{2},{3}},S),InputCheck=>o.InputCheck,Verbose=>o.Verbose);
    if dim ambient S == 7 then return specialFourfold(S,random({{2},{2},{2}},S),InputCheck=>o.InputCheck,Verbose=>o.Verbose);
    if dim ambient S == 8 or dim ambientVariety S == 5 or dim ambientVariety S == 6 then return specialGushelMukaiFourfold(S,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
    error "no valid input provided";
);

specialFourfold Ideal := o -> idS -> specialFourfold(makeSubvariety idS,NumNodes=>o.NumNodes,InputCheck=>o.InputCheck,Verbose=>o.Verbose);

specialFourfold (String,Ring) := o -> (str,K) -> (
    try return specialGushelMukaiFourfold(str,K,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
    try return specialCubicFourfold(str,K,NumNodes=>o.NumNodes,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
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
*** Double special cubic fourfolds ***
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
    S := surface X;
    d := degree S; g := sectionalGenus S;
    degs := flatten degrees ideal S;
    if instance(X,IntersectionOfThreeQuadricsInP7) then recognize X;
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
        A := X.cache#(S,"LatticeIntersectionMatrix");
        descr = descr|newline|toString("of discriminant "|(toString disc)|" = det"|(net A));
    );
    descr = descr|newline|"containing a ";
    n := if S.cache#?"FiniteNumberOfNodes" or S.cache#?"singularLocus" or S.cache#?"nonSaturatedSingularLocus" or (S.cache#?"fitVariety" and (S.cache#"fitVariety").cache#?"nonSaturatedSingularLocus") then numberNodes S else -1;
    descr = descr|(if n > 0 then toString(n)|"-nodal " else (if n == 0 then "smooth " else ""));
    descr = descr|"surface of degree "|toString(d)|" and sectional genus "|toString(g)|newline;
    descr = descr|(if # unique degs == 1 then "cut out by "|toString(#degs)|" hypersurfaces of degree "|toString(first degs) else "cut out by "|toString(#degs)|" hypersurfaces of degrees "|toString(toSequence degs));
    if instance(X,IntersectionOfThreeQuadricsInP7) then (
        if member(recognize X,{"surf-5-7-0-1","surf-5-10-1","internal-projection-K3-genus-8","surf-4-3-1-external","surf-5-6-2-nodal","surf-7-1-9"}) then descr = descr|newline|"(This is a rational fourfold discovered in August 2022)";
        if recognize X === "planeInPP7" then descr = descr|newline|"(This is a classical example of rational fourfold)";
    );
    net expression descr
);

ambientFivefold = method(TypicalValue => EmbeddedProjectiveVariety);

ambientFivefold HodgeSpecialFourfold := X -> (
    if X.cache#?"AmbientFivefold" then return X.cache#"AmbientFivefold"; 
    if codim X == 1 then return X.cache#"AmbientFivefold" = ambient X;
    if instance(X,SpecialGushelMukaiFourfold) then return X.cache#"AmbientFivefold" = varietyDefinedBylinearSyzygies X;
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

degreeHypersurface = method();

degreeHypersurface HodgeSpecialFourfold := X -> (
    if instance(X,SpecialCubicFourfold) then return 3;
    if instance(X,SpecialGushelMukaiFourfold) or instance(X,IntersectionOfThreeQuadricsInP7) then return 2;
    I := flatten degrees trim sub(ideal X,ring ambientFivefold X);
    if #I != 1 then error "degree is not defined";
    first I
);

recognize = method();

recognize HodgeSpecialFourfold := X -> (
    if X.cache#?(surface X,"label") then return X.cache#(surface X,"label");
    if instance(X,SpecialCubicFourfold) then return X.cache#(surface X,"label") = recognizeCubicFourfold X;
    if instance(X,SpecialGushelMukaiFourfold) then return X.cache#(surface X,"label") = recognizeGMFourfold X;
    if instance(X,IntersectionOfThreeQuadricsInP7) then return X.cache#(surface X,"label") = recognize3QuadricsP7 X;
    "NotRecognized"
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

---------------------------------------------------------
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

clean HodgeSpecialFourfold := X -> (
    K := coefficientRing X;
    x := local x;
    n := dim ambient X;
    R := K[x_0..x_n];
    idS := sub(sub(ideal surface X,vars R),vars ring ambient X);
    idX := sub(sub(ideal X,vars R),vars ring ambient X);
    idV := sub(sub(ideal ambientFivefold X,vars R),vars ring ambient X);
    specialFourfold(projectiveVariety idS,projectiveVariety idX,projectiveVariety idV,InputCheck=>0)
);

HodgeSpecialFourfold ** Ring := (X,K) -> (
    if not isField K then error "expected a field";
    S := (surface X) ** K;
    if (surface X).cache#?"FiniteNumberOfNodes" and (not S.cache#?"FiniteNumberOfNodes") then S.cache#"FiniteNumberOfNodes" = (surface X).cache#"FiniteNumberOfNodes";
    specialFourfold(S,(projectiveVariety ring X) ** K,(ambientFivefold X) ** K,InputCheck=>0)
);

random HodgeSpecialFourfold := o -> X -> (
    if instance(X,SpecialGushelMukaiFourfold) then (
        X' := specialGushelMukaiFourfold(surface X,random(2,surface X) * ambientFivefold X,InputCheck=>-1);
        X'.cache#"AmbientFivefold" = ambientFivefold X;
        return X';
    );
    if instance(X,SpecialCubicFourfold) then return specialCubicFourfold(surface X,InputCheck=>-1);
    specialFourfold(surface X,random(degreeHypersurface X,surface X) * ambientFivefold X,ambientFivefold X,InputCheck=>-1)
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
    if instance(X,SpecialCubicFourfold) 
    then s = s|"X = specialFourfold(S,X,NumNodes=>"|toString(numberNodes surface X)|",InputCheck=>0);"|newline else
    if instance(X,SpecialGushelMukaiFourfold) 
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

------------------------------------------------------------------------
--------------------------- Cubic fourfolds ----------------------------
------------------------------------------------------------------------

SpecialCubicFourfold = new Type of HodgeSpecialFourfold;

globalAssignment SpecialCubicFourfold;

SpecialCubicFourfold.synonym = "special cubic fourfold";

specialCubicFourfold = method(TypicalValue => SpecialCubicFourfold, Options => {NumNodes => null, InputCheck => 1, Verbose => false});

specialCubicFourfold (EmbeddedProjectiveVariety,EmbeddedProjectiveVariety) := o -> (S,X) -> (
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
    Fourfold := new SpecialCubicFourfold from X;
    if Fourfold#?"SurfaceContainedInTheFourfold" then Fourfold#"SurfaceContainedInTheFourfold" = prepend(S,Fourfold#"SurfaceContainedInTheFourfold") else Fourfold#"SurfaceContainedInTheFourfold" = {S};
    Fourfold
);

specialCubicFourfold (Ideal,Ideal) := o -> (idS,idX) -> specialCubicFourfold(projectiveVariety idS,projectiveVariety idX,NumNodes=>o.NumNodes,InputCheck=>o.InputCheck,Verbose=>o.Verbose);

specialCubicFourfold (Ideal,RingElement) := o -> (idS,C) -> specialCubicFourfold(idS,ideal C,NumNodes=>o.NumNodes,InputCheck=>o.InputCheck,Verbose=>o.Verbose);

specialCubicFourfold EmbeddedProjectiveVariety := o -> S -> (
    if dim ambient S == 5 and codim S == 1 and degrees S === {({3},1)} then return specialCubicFourfold(S * random({2:{1}},0_S),S,NumNodes=>o.NumNodes,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
    if not(dim ambient S == 5 and dim S == 2) then error "expected a surface in P^5";
    specialCubicFourfold(S,random({3},S),NumNodes=>o.NumNodes,InputCheck=>o.InputCheck,Verbose=>o.Verbose)
);

specialCubicFourfold Ideal := o -> idS -> specialCubicFourfold(projectiveVariety idS,NumNodes=>o.NumNodes,InputCheck=>o.InputCheck,Verbose=>o.Verbose);

specialCubicFourfold (String,Ring) := o -> (str,K) -> (
    if o.NumNodes =!= null then error "the option NumNodes is ignored, the number of nodes is determined automatically";
    local X;
    if str === "quintic del Pezzo surface" then (
        X = specialCubicFourfold(surface({3,4},K),NumNodes=>0,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        X.cache#(surface X,"label") = "quinticDelPezzoSurface";
        return X;
    );
    if str === "quartic scroll" then (
        X = specialCubicFourfold(surface({3,1,1},K),NumNodes=>0,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        X.cache#(surface X,"label") = "quarticScrollSurface";
        return X;
    );
    if str === "general cubic 4-fold of discriminant 38" or str === "C38" then (
        X = specialCubicFourfold(surface({10,0,0,10},K),NumNodes=>0,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        X.cache#(surface X,"label") = "C38Coble";
        return X;
    );
    if str === "6-nodal octic scroll" then (
        X = specialCubicFourfold("C38",K,InputCheck=>0,Verbose=>o.Verbose);
        X = specialCubicFourfold(((top baseLocus fanoMap X) * X)\surface X,X,NumNodes=>6,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        X.cache#(surface X,"label") = "6NodalOcticSrollC38";
        return X;
    );    
    if str === "3-nodal septic scroll" or str === "Farkas-Verra C26" then (
        t := gens ring PP_K^2;
        f := multirationalMap rationalMap(ring PP_K^2,ring PP_K^8,{t_0^5, t_0^4*t_1, t_0^3*t_1^2, t_0^2*t_1^3, t_0^4*t_2, t_0^3*t_1*t_2, t_0^2*t_1^2*t_2, t_0*t_1^3*t_2, t_1^4*t_2});
        f = f * rationalMap linearSpan apply(3,i -> point linearSpan {f point source f,f point source f});
        X = specialCubicFourfold(image f,NumNodes=>3,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        X.cache#(surface X,"label") = "FarkasVerra";
        return X;
    );  
   if str === "one-nodal septic del Pezzo surface" then (
       g := multirationalMap rationalMap(ring PP_K^2,{3,2});
       g = g * rationalMap(ring target g,ring PP_K^5,gens ideal linearSpan {point target g,point linearSpan {g point source g,g point source g}});
       X = specialCubicFourfold(image g,NumNodes=>1,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
       X.cache#(surface X,"label") = "oneNodalSepticDelPezzoSurfaceC26";
       return X;
   );
   if str === "general cubic 4-fold of discriminant 42" or str === "C42" then (
       X = specialCubicFourfold(last last randomS42data(K),NumNodes=>5,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
       X.cache#(surface X,"label") = "C42";
       return X;
   );
   if str === "cubic 4-fold of discriminant 48" or str === "C48" then (
       X = specialCubicFourfold(randomS48 K,NumNodes=>6,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
       X.cache#(surface X,"label") = "C48";
       return X;
   );
   if str === "general cubic 4-fold of discriminant 32" or str === "C32" then (
       X = specialCubicFourfold(surface({9,1,4,6},K),NumNodes=>0,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
       X.cache#(surface X,"label") = "C32";
       return X;
   );
   if str === "general cubic 4-fold of discriminant 44" or str === "C44" then ( -- Enriques surface (see e.g. https://arxiv.org/pdf/1210.1903.pdf, p. 7)
       J := Var ideal jacobian ideal discriminant first genericPolynomials({2,-1,-1,-1},K);
       X = specialCubicFourfold((parametrize random({{1},{1},{1},{1}},0_J))^* J,NumNodes=>0,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
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

specialCubicFourfold String := o -> str -> specialCubicFourfold(str,ZZ/65521,NumNodes=>o.NumNodes,InputCheck=>o.InputCheck,Verbose=>o.Verbose);

expression SpecialCubicFourfold := X -> expression("cubic fourfold containing a surface of degree "|toString(degree surface X)|" and sectional genus "|toString(sectionalGenus surface X));

describe SpecialCubicFourfold := X -> (
    S := surface X;
    d := degree S; g := sectionalGenus S;
    degs := flatten degrees ideal S;
    discrX := discriminant X;
    descr:="Special cubic fourfold of discriminant "|toString(discrX)|newline|"containing a ";
    n := numberNodes surface X;
    descr = descr|(if n > 0 then toString(n)|"-nodal " else "(smooth) ");
    descr = descr|"surface of degree "|toString(d)|" and sectional genus "|toString(g)|newline;
    descr = descr|(if # unique degs == 1 then "cut out by "|toString(#degs)|" hypersurfaces of degree "|toString(first degs) else "cut out by "|toString(#degs)|" hypersurfaces of degrees "|toString(toSequence degs));
    if recognize X === "LaiFarkasVerraC42" then descr = descr|newline|"(the surface is the 8-nodal nonic scroll studied by K.-W. Lai, G. Farkas and A. Verra,"|newline|"this implementation is due to M. Hoff)";
    net expression descr
);

map SpecialCubicFourfold := o -> X -> associatedMapFromFivefold X;

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

parameterCount SpecialCubicFourfold := o -> X -> parameterCount(surface X,X,Verbose=>o.Verbose);

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

isAdmissible SpecialCubicFourfold := X -> isAdmissible discriminant X;

unirationalParametrization (SpecialCubicFourfold,EmbeddedProjectiveVariety) := (X,L) -> (
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

unirationalParametrization SpecialCubicFourfold := (cacheValue "unirationalParametrization") (X -> unirationalParametrization(X,line X));

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

LaiFarkasVerraC42 = method(Options => options specialCubicFourfold);
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
    specialCubicFourfold(R,NumNodes=>8,Verbose=>o.Verbose,InputCheck=>o.InputCheck)
);

------------------------------------------------------------------------
--------------------------- GM fourfolds -------------------------------
------------------------------------------------------------------------

SpecialGushelMukaiFourfold = new Type of HodgeSpecialFourfold;

globalAssignment SpecialGushelMukaiFourfold;

SpecialGushelMukaiFourfold.synonym = "special Gushel-Mukai fourfold";

specialGushelMukaiFourfold = method(TypicalValue => SpecialGushelMukaiFourfold, Options => {InputCheck => 1, Verbose => false});

specialGushelMukaiFourfold (EmbeddedProjectiveVariety,EmbeddedProjectiveVariety) := o -> (S,X) -> (
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
    Fourfold := new SpecialGushelMukaiFourfold from X;
    if Fourfold#?"SurfaceContainedInTheFourfold" then Fourfold#"SurfaceContainedInTheFourfold" = prepend(S,Fourfold#"SurfaceContainedInTheFourfold") else Fourfold#"SurfaceContainedInTheFourfold" = {S};
    Fourfold
);

specialGushelMukaiFourfold (Ideal,Ideal) := o -> (idS,idX) -> specialGushelMukaiFourfold(projectiveVariety idS,projectiveVariety idX,InputCheck=>o.InputCheck,Verbose=>o.Verbose);

specialGushelMukaiFourfold EmbeddedProjectiveVariety := o -> S -> (
    if dim ambient S == 8 and codim S == 4 and degrees S === {({2},6)} and degree S == 10 then return specialGushelMukaiFourfold(S * random({2:{1}},0_S),S,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
    Y := ambientVariety S;
    er := "expected a surface in a GM fourfold or del Pezzo fivefold or del Pezzo sixfold";
    if dim S != 2 then error er;
    if not((dim ambient Y == 9 and dim Y == 6 and degrees Y === {({2},5)}) or 
           (dim ambient Y == 8 and dim Y == 5 and degrees Y === {({2},5)}) or 
           (dim ambient Y == 8 and dim Y == 4 and degrees Y === {({2},6)})
          ) then error er;
    if dim Y == 4 then return specialGushelMukaiFourfold(S,Y,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
    local X;
    if dim Y == 5 then (
        X = specialGushelMukaiFourfold(S,Y * random(2,S),InputCheck=>o.InputCheck,Verbose=>o.Verbose);
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
        X = specialGushelMukaiFourfold(T,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        if instance(Y,GrassmannianVariety) then (
            j = j||Y;
            (source j).cache#"toGrass" = j;
            try assert(ideal ambientFivefold X == ideal source j) else error "internal error encountered";
            X.cache#"AmbientFivefold" = source j;
        );
        return X;
    );
);

specialGushelMukaiFourfold Ideal := o -> I -> specialGushelMukaiFourfold(makeSubvariety I,InputCheck=>o.InputCheck,Verbose=>o.Verbose);

specialGushelMukaiFourfold (String,Ring) := o -> (str,K) -> (
    G := GG(K,1,4);
    local X; local S;
    if str === "sigma-plane" then (
        X = specialGushelMukaiFourfold(schubertCycle({3,1},G),InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        X.cache#(surface X,"label") = 6;
        return X;
    );
    if str === "rho-plane" then (
        X = specialGushelMukaiFourfold(schubertCycle({2,2},G),InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        X.cache#(surface X,"label") = 9;
        return X;
    );
    if str === "tau-quadric" then (
        X = specialGushelMukaiFourfold((schubertCycle({1,1},G) * random({{1},{1}},0_G))%G,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        X.cache#(surface X,"label") = 1;
        return X;
    );
    if str === "cubic scroll" then (
        X = specialGushelMukaiFourfold((schubertCycle({2,0},G) * random({{1},{1}},0_G))%G,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        X.cache#(surface X,"label") = 7; 
        return X;
    );
    if str === "quintic del Pezzo surface" then (
        X = specialGushelMukaiFourfold((G * random({4:{1}},0_G))%G,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        X.cache#(surface X,"label") = 4;
        return X;
    );
    if str === "K3 surface of genus 8 with class (9,5)" then (
        G15 := Grass replace(1,5,Grass ring G);
        pr := rationalMap(G15,ring G,select(gens ambient G15,g -> last last baseName g != 5));
        X = specialGushelMukaiFourfold(pr sub(ideal for i to 5 list random(1,ambient G15),G15),InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        X.cache#(surface X,"label") = 3;
        return X;
    );
    if str === "general GM 4-fold of discriminant 20" then (
        (g,T) := first randomS42data(K);
        X = specialGushelMukaiFourfold((g T)%image(g),InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        X.cache#(surface X,"label") = 17;
        return X;     
    );
    if #str >= 1 and #str <= 2 then (
        Vstr := value str;
        if instance(Vstr,ZZ) and Vstr >= 1 and Vstr <= 21 then return fourfoldFromTriple(Vstr,GMtables(Vstr,K),InputCheck=>o.InputCheck,Verbose=>o.Verbose);
    );
    if str === "nodal surface of degree 11 and genus 3 with class (7,4)" then (
        X = specialGushelMukaiFourfold([4,5,1,0],[3,5,1,0],"cubic scroll",(1,K),InputCheck=>o.InputCheck,Verbose=>false);
        (surface X).cache#"FiniteNumberOfNodes" = 1;
        X.cache#(surface X,"label") = "mukai26''";
        return X;
    );
    if str === "nodal D44" then (
        X = specialGushelMukaiFourfold([2,0,0,0],[1,0,0,0],"cubic scroll",K,InputCheck=>o.InputCheck,Verbose=>false);
        (surface X).cache#"FiniteNumberOfNodes" = 1;
        X.cache#(surface X,"label") = "nodal D44";
        return X;
    );
    if str === "GM 4-fold of discriminant 26('')" then (
        X = specialGushelMukaiFourfold([4,5,1,0],[2,3,0,0],"cubic scroll",K,InputCheck=>o.InputCheck,Verbose=>false);
        (surface X).cache#"FiniteNumberOfNodes" = 0;
        X.cache#(surface X,"label") = "october2021-26''";
        return X;
    );
    if str === "GM 4-fold of discriminant 28" then (
        X = specialGushelMukaiFourfold([6,4,6,0],[3,3,5,0],"cubic scroll",K,InputCheck=>o.InputCheck,Verbose=>false);
        (surface X).cache#"FiniteNumberOfNodes" = 0;
        X.cache#(surface X,"label") = "october2021-28";
        return X;
    );
    if str === "GM 4-fold of discriminant 34(')" then (
        X = specialGushelMukaiFourfold([6,4,6,0],[3,1,6,0],"cubic scroll",K,InputCheck=>o.InputCheck,Verbose=>false);
        (surface X).cache#"FiniteNumberOfNodes" = 0;
        X.cache#(surface X,"label") = "october2021-34'";
        return X;
    );
    if str === "triple projection of K3 surface of degree 26" then (
        X = specialGushelMukaiFourfold([4,5,1],[2,3,0],"cubic scroll",K,InputCheck=>0,Verbose=>false);
        B := projectiveVariety matrix fanoMap X;
        interpolateTop(B,{2},Verbose=>false,cache=>true,"Deep"=>2);
        P := (B\top B)\top B;
        f := rationalMap(P % ambientFivefold X);
        S = (X * f^* f surface X)\(surface X);
        if not(dim S == 2 and degree S == 17 and sectionalGenus S == 11) then error "something went wrong";
        X = specialGushelMukaiFourfold(S,X,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        X.cache#(surface X,"label") = "gushel26''";
        return X;
    );
    if str === "surface of degree 11 and genus 3 with class (7,4)" then (
        X = specialGushelMukaiFourfold([4,5,1],[2,3,0],"cubic scroll",K,InputCheck=>0,Verbose=>false);
        S = (X * ((fanoMap X)^* first decompose first exceptionalCurves X))\surface X;
        if not(dim S == 2 and degree S == 11 and sectionalGenus S == 3 and degrees S == {({2},16)}) then error "something went wrong";
        S.cache#"FiniteNumberOfNodes" = 1;
        X = specialGushelMukaiFourfold(S,X,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
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

specialGushelMukaiFourfold String := o -> str -> specialGushelMukaiFourfold(str,ZZ/65521,InputCheck=>o.InputCheck,Verbose=>o.Verbose);

expression SpecialGushelMukaiFourfold := X -> expression("GM fourfold containing a surface of degree "|toString(degree surface X)|" and sectional genus "|toString(sectionalGenus surface X)|(if X.cache#?(surface X,"classSurfaceInG14") then (" with class "|toString(first cycleClass X)) else ""));

describe SpecialGushelMukaiFourfold := X -> (
    S := surface X;
    d := degree S; g := sectionalGenus S;
    degs := flatten degrees ideal S;
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
    descr = descr|newline|"containing a ";
    n := if S.cache#?"FiniteNumberOfNodes" then numberNodes S else -1;
    if n > 0 then descr = descr|toString(n)|"-nodal " else if n == 0 then descr = descr|"smooth ";
    descr = descr|"surface in PP^8 of degree "|toString(d)|" and sectional genus "|toString(g)|newline;
    descr = descr|(if # unique degs == 1 then "cut out by "|toString(#degs)|" hypersurfaces of degree "|toString(first degs) else "cut out by "|toString(#degs)|" hypersurfaces of degrees "|toString(toSequence degs));
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
    net expression descr
);

cycleClass SpecialGushelMukaiFourfold := X -> (
    if X.cache#?(surface X,"classSurfaceInG14") then return X.cache#(surface X,"classSurfaceInG14");
    S := surface X;
    j := toGrass X;
    cS := cycleClass makeSubvariety(j S,target j);
    ab := toSequence flatten entries lift(transpose last coefficients(cS,Monomials=>vars ring cS),ZZ);
    X.cache#(surface X,"classSurfaceInG14") = (cS,ab)
);

map SpecialGushelMukaiFourfold := o -> X -> associatedMapFromFivefold X;

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

toGrass SpecialGushelMukaiFourfold := (cacheValue "toGrass") (X -> ( -- for better documentation
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

isAdmissibleGM SpecialGushelMukaiFourfold := X -> isAdmissibleGM discriminant X;

parameterCount SpecialGushelMukaiFourfold := o -> X -> (
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
sigmaQuadric SpecialGushelMukaiFourfold := X -> (
    f := toRationalMap toGrass X;
    H := image(f,1);
    Q := ideal random(2,makeSubvariety image f);
    S := trim(Q + tangentialChowForm(chowEquations(H_0),0,1));
    return trim lift(f^* S,ambient source f);
);

unirationalParametrization SpecialGushelMukaiFourfold := (cacheValue "unirationalParametrization") (X -> (
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

------------------------------------------------------------------------
----------- Complete intersections of three quadrics in PP^7 -----------
------------------------------------------------------------------------

IntersectionOfThreeQuadricsInP7 = new Type of HodgeSpecialFourfold;

globalAssignment IntersectionOfThreeQuadricsInP7;

IntersectionOfThreeQuadricsInP7.synonym = "complete intersection of three quadrics in PP^7";

expression IntersectionOfThreeQuadricsInP7 := X -> expression("complete intersection of three quadrics in PP^7 containing a surface of degree "|toString(degree surface X)|" and sectional genus "|toString(sectionalGenus surface X));

recognize3QuadricsP7 = X -> ( 
    S := surface X;
    d := discriminant X;
    e := eulerCharacteristic S;
    invS := (degree S,sectionalGenus S,euler hilbertPolynomial S);
    degs := flatten degrees ideal S;
    if (d == 31 and e == 3 and invS === (1,0,1) and degs == toList(5:1)) then return "planeInPP7";
    if (d == 47 and e == 11 and invS === (9,3,1) and degs == toList(12:2)) then return "surf-5-7-0-1";
    if (d == 55 and e == 14 and invS === (11,5,1) and degs == toList(10:2)) then return "surf-5-10-1";
    if (d == 55 and e == 25 and invS === (13,8,2) and degs == toList(9:2)) then return "internal-projection-K3-genus-8";
    if (d == 79 and e == 7 and invS === (9,2,1) and degs == {2,2,2,2,2,2,2,2,2,2,3}) then return "surf-4-3-1-external";
    if ((d == 71 or d == 87) and e == 5 and invS === (11,4,0) and degs == toList(9:2)) then if numberNodes(S,Verbose=>false) == 1 and discriminant X == 87 then return "surf-5-6-2-nodal";
    if (d == 96 and e == 13 and invS === (12,6,1) and degs == {2,2,2,2,2,2,2,2,2,3}) then return "surf-7-1-9";
    "NotRecognized"
);

infoAboutParameterCountInAmbientP7 = (x,y) -> (
    --------------------------------------------------
        -- Conversion between parameter counts
        -- X : c. i. of 3 quadrics in PP^7; Y = ambientFivefold X; S = surface X; 
        -- Suppose we have computed:
        -- h^1(N_{S,Y}) = 0, h^0(N_{S,Y}) = a
        -- h^1(O_S(2)) = 0, and h^0(I_{S,Y}(2)) = b = h^0(O_Y(2)) - \chi(O_S(2));
        -- h^0(N_{S,X}) = c, dim P(H^0(O_Y(2))) = 33
        -- codim{[X] : S ⊂ X ⊂ Y} <= 33 - (a + (b-1) - c)
        -- parameterCount(X,Y) ----> (34-a-b+c, (b, a, c))
        -- Now from the exact sequence 0 -> N_(S,Y) -> N_(S,PP^7) -> N_(Y,PP^7)|_S -> 0
        -- that is 0 -> N_(S,Y) -> N_(S,PP^7) -> O_S(2)++O_S(2) -> 0
        -- we deduce h^0(N_(S,PP^7)) = h^0(N_{S,Y}) + h^0(O_S(2)++O_S(2)) = a + 2*(34 - b) = 68 + a - 2b
        -- Further, from the exact sequence 0 -> I_(Y,PP^7) -> I_(S,PP^7) -> I_(S,Y) -> 0
        -- we deduce h^0(I_{S,PP^7}(2)) = h^0(I_{S,Y}(2)) + 2 = b + 2
        -- from which we have dim GG(2, PP(h^0(I_{S,PP^7}(2)))) = 3 * (b-1)
        -- Since dim GG(2,P(H^0(O_(P^7)(2)))) = 99 we obtain 
        -- 99 - ( (68 + a - 2b) + 3*(b-1) - c ) = 31 - a + 2b - 3b + 3 + c = 34 - a - b + c
        -- Therefore parameterCount(X) ----> (34-a-b+c, (b+2, 68 + a - 2b , c))
    --------------------------------------------------
    (b,a,c) := y;
    if 34-a-b+c != x then <<"--warning: something went wrong"<<endl;
    <<"[parameterCount in the ambient PP^7: "<<(34-a-b+c, (b+2, 68 + a - 2*b , c))<<"]"<<endl;
);

beauvilleMap = method();
beauvilleMap IntersectionOfThreeQuadricsInP7 := X -> (
    R := ring ambient X;
    l := projectiveVariety ideal submatrix'(vars R,{0,1});
    j := rationalMap(((line X) ===> l)|X,Dominant=>true);
    X' := target j;
    if not isSubset(l,X') then error "something went wrong";
    K := coefficientRing R;
    x := local x; P7 := projectiveVariety(K[x_0..x_7]);
    t := local t; P2 := projectiveVariety(K[t_0..t_2]);
    u := local u; P5 := projectiveVariety(K[u_0..u_5]);
    Q := flatten entries sub(gens ideal X',vars ring P7);
    S := (K[x_2..x_7])[x_0,x_1];
    L := apply(3,i -> coefficient((x_0)_S,sub(Q_i,S)));
    M := apply(3,i -> coefficient((x_1)_S,sub(Q_i,S)));
    F := apply(3,i -> coefficient(1_S,sub(Q_i,S)));
    if F != apply(3,i -> sub(Q_i,S) - (x_0)_S * L_i - (x_1)_S * M_i) then error "something went wrong";
    P2xP5 := P2 ** P5;
    A := sub(matrix {L,M,F},for i to 5 list (gens coefficientRing S)_i => u_i);
    Z := projectiveVariety ideal det A;
    Y := projectiveVariety ideal(((map last projections P2xP5) A) * transpose matrix {first P2xP5#"multigens"});
    fromXtoZ := j * rationalMap(ring X',ring P5,submatrix'(vars R,{0,1}));
    if image fromXtoZ != Z then error "something went wrong";
    fromXtoZ = rationalMap(fromXtoZ,Z);
    fromYtoZ := (last projections P2xP5)|Y;
    if image fromYtoZ != Z then error "something went wrong";
    fromYtoZ = rationalMap(fromYtoZ,Z);
    Y.cache#"projection maps" = {quadricFibration first projectionMaps Y, fromYtoZ};
    f := fromXtoZ * inverse fromYtoZ;
    g := fromYtoZ * inverse fromXtoZ;
    if not(f * g == 1 and g * f == 1) then error "something went wrong";
    forceInverseMap(f,g);
    f
);

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
        if o.Verbose then P = P and findProgram("xterm","xterm -version",RaiseError=>false) =!= null;
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
----------------------------- Congruences ------------------------------
------------------------------------------------------------------------

CongruenceOfCurves = new Type of HashTable;

globalAssignment CongruenceOfCurves;

CongruenceOfCurves.synonym = "congruence of curves";

detectCongruence = method(TypicalValue => CongruenceOfCurves, Options => {Verbose => false});

detectCongruence (SpecialCubicFourfold,ZZ) := o -> (X,e) -> congruenceOfCurves(X,e);

detectCongruence (SpecialGushelMukaiFourfold,ZZ) := o -> (X,e) -> congruenceOfCurves(X,e);

detectCongruence (HodgeSpecialFourfold,ZZ) := o -> (X,e) -> congruenceOfCurves(X,e);

detectCongruenceInt = method(Options => {Verbose => false});
detectCongruenceInt (EmbeddedProjectiveVariety,HodgeSpecialFourfold) := o -> (p,X) -> (
    a := degreeHypersurface X;
    if not(isPoint p and ring ambient p === ring ambient X and isSubset(p,ambientFivefold X)) then error "expected a point in the ambient fivefold";
    phi := map X;
    imageOfAssociatedMap X; -- image of phi
    S := surface X; 
    secants := new MutableList from {null,0,0,0,0,0,0};
    phip := phi p;
    E := coneOfLines(Var image phi,phip);
    if dim E == 0 then (
        if o.Verbose then <<"no ("<<a<<"e"<<-1<<")-secant curves have been found"<<endl;
        error "no congruences detected"; 
    );
    if dim E != 1 then error "expected cone of lines to be one dimensional";
    if o.Verbose then <<"number lines contained in the image of the "<<(if a == 2 then "quadratic" else (if a == 3 then "cubic" else ""))<<" map and passing through a general point: "<<degree E<<endl;
    if degree E == 1 then return detectCongruence(X,degree phi^* E);
    pr := rationalMap phip;
    v := (rationalMap {random(1,ring target pr),random(1,ring target pr)});
    pr' := toRationalMap((pr * v)|E);
    E' := Var kernel(map pr',SubringLimit=>1);
    if not(dim E' == 0 and degree E' == degree E) then error "something went wrong";
    local n; local C;
    P := apply(decompose E',q -> (assert(dim q == 0); C = phi^* pr'^* q; assert(dim C == 1 and dim(C*S) == 0); (degree q,C)));
    for nC in P do (
        (n,C) = nC;
        for e from 1 to 6 do 
            if degree(C) == n*e and degree(C*S) == n*(a*e-1) and all(delete(e,toList(1..6)),e' -> not(degree(C) == n*e' and degree(C*S) == n*(a*e'-1)))
            then secants#e = secants#e + n;
    );
    secants = toList take(secants,-(#secants-1));
    if sum secants =!= degree E then <<"--warning: something went wrong: "<<sum secants<<" =!= "<<degree E<<endl;
    nC := apply({"lines","conics","cubics","quartics","quintics","sextics"},apply(toList(0..5),e -> a*(e+1)-1),(s,t) -> "number "|toString(t)|"-secant "|s);
    if o.Verbose then for e to #secants-1 do if secants_e != 0 then <<nC_e<<" = "<<secants_e<<endl;
    ee := null;
    ee = for e to #secants-1 do if secants_e == 1 then break (e+1);
    if ee === null then error "no congruences detected";    
    return congruenceOfCurves(X,ee);
);
detectCongruence SpecialCubicFourfold := o -> X -> detectCongruenceInt(point ambient X,X,Verbose=>o.Verbose);
detectCongruence SpecialGushelMukaiFourfold := o -> X ->  detectCongruenceInt(pointOnLinearSectionOfG14 ambientFivefold X,X,Verbose=>o.Verbose);
detectCongruence HodgeSpecialFourfold := o -> X -> detectCongruenceInt(point ambientFivefold X,X,Verbose=>o.Verbose);

congruenceOfCurves = method();

congruenceOfCurves (HodgeSpecialFourfold,ZZ) := (X,e) -> (
    a := degreeHypersurface X;
    Y := ambientFivefold X;
    phi := map X;
    imageOfAssociatedMap X; -- image of phi
    S := surface X;
    f := method();
    f EmbeddedProjectiveVariety := p -> (
        if not isPoint p then error "expected a point";
        if not(ring ambient p === ring ambient Y and isSubset(p,Y)) then error "expected a point on the ambient fivefold containing the surface";
        phip := phi p;
        E := coneOfLines(Var image phi,phip);
        if dim E == 0 then error "no congruences detected";
        if dim E != 1 then error "expected cone of lines to be one dimensional";
        if degree E == 1 then (
            D := phi^* E;
            if not(dim D == 1 and degree D == e and dim(D*S) == 0 and degree(D*S) == a*e-1) then error "no congruences detected";
            if sectionalGenus D != 0 then D = top D;
            return makeSubvariety(D,Y);
        );
        pr := rationalMap phip;
        v := (rationalMap {random(1,ring target pr),random(1,ring target pr)});
        pr' := toRationalMap((pr * v)|E);
        E' := Var kernel(map pr',SubringLimit=>1);
        if not(dim E' == 0 and degree E' == degree E) then error "something went wrong";
        decE' := select(decompose E',q -> (dim q == 0 and degree q == 1));
        P := select(apply(decE',q -> phi^* pr'^* q),C -> (dim C == 1 and degree C == e and dim(C*S) == 0 and degree(C*S) == a*e-1)); 
        if #P != 1 then (if #P > 1 
                         then error "got more than one curve of the congruence that passes through the point"
                         else error "got no curve of the congruence that passes through the point");
        C := first P;
        if sectionalGenus C != 0 then C = top C;
        makeSubvariety(C,Y)
    );    
    try f (if instance(X,SpecialGushelMukaiFourfold) then pointOnLinearSectionOfG14 Y else point Y) else error "no congruences detected";
    new CongruenceOfCurves from {
        "function" => f,
        "fourfold" => X,
        "degree" => e,
        "string" => "of "|toString(a*e-1)|"-secant "|(if e == 1 then "lines" else if e == 2 then "conics" else if e == 3 then "cubic curves" else if e == 4 then "quartic curves" else if e == 5 then "quintic curves" else "curves of degree "|toString(e))
    }
);

toString CongruenceOfCurves := net CongruenceOfCurves := f -> if hasAttribute(f,ReverseDictionary) then toString getAttribute(f,ReverseDictionary) else "a congruence "|f#"string";
texMath CongruenceOfCurves := texMath @@ net;

CongruenceOfCurves#{WebApp,AfterPrint} = CongruenceOfCurves#{WebApp,AfterNoPrint} =
CongruenceOfCurves#{Standard,AfterPrint} = CongruenceOfCurves#{Standard,AfterNoPrint} = f -> (
    S := surface f#"fourfold";
    Y := ambientFivefold f#"fourfold";
    e := f#"degree";
    << endl << concatenate(interpreterDepth:"o") << lineNumber << " : " << "Congruence " << f#"string" << " to ";
    << if hasAttribute(S,ReverseDictionary) then toString getAttribute(S,ReverseDictionary) else "surface";
    <<" in ";
    << if hasAttribute(Y,ReverseDictionary) then toString getAttribute(Y,ReverseDictionary) else (if codim Y > 0 then "a fivefold in PP^"|toString(dim ambient Y) else "PP^5");
    if S.cache#?("fanoMap",Y) and ((S.cache#("fanoMap",Y))#"map").cache#"multiplicityFanoMap" == e
    then << "; parameter space: " << Var target S.cache#("fanoMap",Y);
    << endl;
);

CongruenceOfCurves EmbeddedProjectiveVariety := (f,p) -> f#"function" p;
CongruenceOfCurves Ideal := (f,Ip) -> idealOfSubvariety f projectiveVariety gens Ip;

member(EmbeddedProjectiveVariety,CongruenceOfCurves) := (C,f) -> (
    L := (map f#"fourfold") C;
    dim L == 1 and unique degrees ideal L === {{1}}
);

map CongruenceOfCurves := o -> f -> (
    X := f#"fourfold";
    e := f#"degree";
    mu := fanoMap(X,e);
    e' := (mu#"map").cache#"multiplicityFanoMap";
    if e' =!= e then error("the congruence seems not valid; try the command: detectCongruence(...,"|toString(e')|")");
    multirationalMap mu
);

check (ZZ,CongruenceOfCurves) := o -> (n,f) -> (
    X := f#"fourfold";
    try for i to n-1 do f(point ambientFivefold X) else error "the congruence of curves is not valid";
    f
);
check CongruenceOfCurves := o -> f -> check(5,f);

------------------------------------------------------------------------
--------------------------- Discriminants ------------------------------
------------------------------------------------------------------------

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

isomorphicProjectionOfSurfaceInP5 = method();
isomorphicProjectionOfSurfaceInP5 EmbeddedProjectiveVariety := X -> (
    if dim X != 2 then error "expected a surface";
    if codim linearSpan X > 0 then X = (parametrize linearSpan X)^^ X;
    if dim ambient X <= 5 then return X;
    pr := rationalMap apply(6,i -> random(1,ring ambient X));
    Var pr (ideal X)
);

discriminant SpecialGushelMukaiFourfold := o -> X -> (
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

-- discriminant SpecialCubicFourfold := o -> X -> ( 
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

discriminant SpecialCubicFourfold := discriminant HodgeSpecialFourfold := o -> X -> (
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
    n := if instance(X,SpecialCubicFourfold) or S.cache#?"FiniteNumberOfNodes" or S.cache#?"singularLocus" or S.cache#?"nonSaturatedSingularLocus" or (S.cache#?"fitVariety" and (S.cache#"fitVariety").cache#?"nonSaturatedSingularLocus") then numberNodes S else 0;
    S2 := 2*n + (binomial(r+5,2) - (r+5)*(sum a) + (sum a)^2 - sum flatten for i to r-1 list for j from i+1 to r-1 list a_i*a_j) * HS2 + (r+5-sum a) * KSHS + KS2 - c2TS;
    d := det(X.cache#(surface X,"LatticeIntersectionMatrix") = matrix {{degree X,HS2},{HS2,S2}});
    if S.cache#?"FiniteNumberOfNodes" then X.cache#(surface X,"discriminantFourfold") = (S2,d);
    d
);

------------------------------------------------------------------------
------------------------ Associated K3 surfaces ------------------------
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
        muInv := inverse3(mu|X);
        if not X.cache#?"rationalParametrization" then X.cache#"rationalParametrization" = muInv;
        return (surface X).cache#("surfaceDeterminingInverseOfFanoMap",ideal X) = projectiveVariety(if member(recognize X,{"planeInPP7", 1, 6}) then gens saturate ideal matrix muInv else matrix muInv);
    );
    mu = super mu;
    if Str === "Interpolate" then (W := Var image mu; iW := lift(3 - (2*(sectionalGenus W)-2)/(degree W),ZZ));
    m := numgens ambient target map X -1;    
    if o.Verbose then <<"-- needed "<<m<<" steps"<<endl;
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
    if member(recognize X,{"NotRecognized", "FarkasVerra", 1, "surf-5-6-2-nodal"}) then U = top U;
    if recognize X === "surf-7-1-9" then (if o.Verbose then <<"-- removing cubic scroll component from surface"<<endl; U = U \ Fano Fano(1,U));
    (surface X).cache#("surfaceDeterminingInverseOfFanoMap",ideal X) = U
);

exceptionalCurves = method(Options => {Verbose => false, Strategy => null, "NumberLines" => infinity});
exceptionalCurves HodgeSpecialFourfold := o -> X -> (
    NumLines := o#"NumberLines";
    if NumLines =!= infinity then if not(instance(NumLines,ZZ) and NumLines >= 0) then error "option NumberLines expects a nonnegative integer";
    if NumLines === infinity then (
        if member(recognize X,{"planeInPP7", "quarticScrollSurface", "oneNodalSepticDelPezzoSurfaceC26", "FarkasVerra", 3, "6NodalOcticSrollC38", 18, "gushel26''", "surf-5-6-2-nodal", "surf-4-3-1-external", "surf-7-1-9"}) then NumLines = 0;
        if member(recognize X,{"surf-5-7-0-1", 17, 6, "mukai26''", "hyperplane section of a conic bundle over PP2"}) then NumLines = 1;
        if recognize X === 1 then NumLines = 2;
        if recognize X === "october2021-34'" then NumLines = 3;
        if recognize X === "october2021-26''" then NumLines = 4;
        if member(recognize X,{"quinticDelPezzoSurface", "C42", "october2021-20"}) then NumLines = 5;
        if recognize X === "C38Coble" then NumLines = 10;
    );
    a := degreeHypersurface X;
    if o.Verbose then <<"-- computing the Fano map mu from "<<(if codim ambientFivefold X > 0 then "the fivefold in PP^"|toString(dim ambient X) else "PP^5")<<endl;
    mu := fanoMap X;
    e := (map mu).cache#"multiplicityFanoMap";
    if o.Verbose then <<"-- computed the map mu from "<<(if codim ambientFivefold X > 0 then "the fivefold in PP^"|toString(dim ambient X) else "PP^5")<<" to PP^"<<(numgens ambient target mu -1)<<" defined by the hypersurfaces"<<endl;
    if o.Verbose then <<"   of degree "<<a*e-1<<" with points of multiplicity "<<e<<" along the surface S of degree "<<degree surface X<<" and genus "<<sectionalGenus surface X<<endl;
    if o.Verbose then <<"-- computing the surface U corresponding to the fourfold X"<<endl;
    U := surfaceDeterminingInverseOfFanoMap(X,Verbose=>o.Verbose,Strategy=>o.Strategy);
    if U.cache#?"exceptionalCurves" then return U.cache#"exceptionalCurves";
    if o.Verbose then <<"-- computing the surface U' corresponding to another fourfold X'"<<endl;
    X' := random X;    
    U' := surfaceDeterminingInverseOfFanoMap(X',Verbose=>o.Verbose,Strategy=>o.Strategy);
    if dim(U*U') <= 0 then return U.cache#"exceptionalCurves" = ((0_U)%U,(0_U)%U);
    if dim(U*U') > 1 then error "something went wrong: dim(U*U') > 1";
    local LL; local L;
    if instance(NumLines,ZZ) and NumLines > 0 and member(recognize X,{"quinticDelPezzoSurface", 1}) then (
        if o.Verbose then <<"-- computing the "<<NumLines<<" exceptional line(s)"<<" as the top components of U*U'"<<endl;
        L = interpolateTop(U*U',if recognize X === 1 then {2} else {3},Verbose=>o.Verbose,"Deep"=>2);
        if degree(U*U') =!= degree(L) then error "something went wrong";
    ) else if instance(NumLines,ZZ) and NumLines == 1 and recognize X === "mukai26''" then (
        if o.Verbose then <<"-- computing the "<<NumLines<<" exceptional line(s)"<<" as the top components of U*U'"<<endl;
        L = first select(decompose interpolateTop((U*U'),{2},Verbose=>o.Verbose,"Deep"=>2),Cu -> dim Cu == 1 and degree Cu == 1);
    ) else if instance(NumLines,ZZ) and NumLines > 0 then (
        if o.Verbose then <<"-- computing the "<<NumLines<<" exceptional line(s) via the Fano scheme of lines"<<endl;
        LL = Fano(1,U*U');
        while not(dim LL == 0 and degree LL == NumLines) do (if o.Verbose then <<"-- recomputing Fano scheme of lines"<<endl; LL = Fano(1,U*U'));
        L = Fano LL;
        while not(dim L == 1 and degree L == NumLines) do (if o.Verbose then <<"-- recomputing variety swept out by lines"<<endl; L = Fano LL);
    ) else if NumLines === infinity then (
        if o.Verbose then <<"-- computing the exceptional lines via the Fano scheme of lines"<<endl;
        LL = Fano(1,U*U');
        L = if dim LL >= 0 then Fano LL else 0_U;
    ) else L = 0_U;
    if not (isSubset(L,U) and isSubset(L,U')) then error "something went wrong";
    if degree(U*U') == degree(L) then return U.cache#"exceptionalCurves" = (L%U,(0_U)%U);
    if degree((U*U')\L) =!= degree(U*U') - degree(L) then error "some exceptional line has multiplicity > 1";    
    if o.Verbose then <<"-- computing the top components of (U*U')\\{exceptional lines} via interpolation"<<endl;
    local C;
    if member(recognize X,{"quarticScrollSurface", "oneNodalSepticDelPezzoSurfaceC26", "FarkasVerra", "C38Coble", "C42", 17, "october2021-26''", "october2021-34'", "6NodalOcticSrollC38", 18, " mukai26''"})
    then C = interpolateTop((U*U')\L,{2},Verbose=>o.Verbose,"Deep"=>2)
    else C = interpolateTop(2,(U*U')\L,Verbose=>o.Verbose,"Deep"=>3);
    U.cache#"exceptionalCurves" = (L%U,C%U)
);

SurfaceAssociatedToRationalFourfold = new Type of EmbeddedProjectiveVariety;
WeightedSurfaceAssociatedToRationalFourfold = new Type of WeightedProjectiveVariety;

globalAssignment SurfaceAssociatedToRationalFourfold;
globalAssignment WeightedSurfaceAssociatedToRationalFourfold;

WeightedSurfaceAssociatedToRationalFourfold.synonym = SurfaceAssociatedToRationalFourfold.synonym = "surface associated to rational fourfold";

expression WeightedSurfaceAssociatedToRationalFourfold := expression SurfaceAssociatedToRationalFourfold := U -> (
    X := U.cache#"Hodge-special fourfold";
    (S,F) := if instance(X,SpecialCubicFourfold)
             then ("K3 surface","cubic fourfold")
             else if instance(X,SpecialGushelMukaiFourfold)
             then ("K3 surface","GM fourfold")
             else if instance(X,IntersectionOfThreeQuadricsInP7)
             then ("Castelnuovo surface","complete intersection of 3 quadrics in PP^7")
             else ("surface","fourfold");
    if dim U == -1 then S = "not-fully-calculated "|S;
    A := if hasAttribute(X,ReverseDictionary) then toString getAttribute(X,ReverseDictionary) else (F|" of discriminant "|toString(discriminant X));
    expression(S|" associated to "|A)
);

net WeightedSurfaceAssociatedToRationalFourfold := net SurfaceAssociatedToRationalFourfold := U -> (
    if hasAttribute(U,ReverseDictionary) then return toString getAttribute(U,ReverseDictionary);
    if dim U >= 0 then return ? U;
    "-* some calculations are missing *-"
);
texMath WeightedSurfaceAssociatedToRationalFourfold := texMath SurfaceAssociatedToRationalFourfold := texMath @@ net;

makeSurfaceAssociated = (X,mu,U,C,f) -> (
    assert(instance(X,HodgeSpecialFourfold) and instance(mu,MultirationalMap) and instance(U,EmbeddedProjectiveVariety) and instance(C,List) and (instance(f,Nothing) or instance(f,MultirationalMap)));
    S := if f =!= null then image f else projectiveVariety((coefficientRing X)[],Saturate=>false);
    S = if instance(S,WeightedProjectiveVariety) then new WeightedSurfaceAssociatedToRationalFourfold from S else new SurfaceAssociatedToRationalFourfold from S;
    S.cache#"construction of SurfaceAssociatedToRationalFourfold" = (mu,U,C,f);
    S.cache#"Hodge-special fourfold" = X;
    return S;
);

building = method();
building WeightedSurfaceAssociatedToRationalFourfold := building SurfaceAssociatedToRationalFourfold := S -> S.cache#"construction of SurfaceAssociatedToRationalFourfold";

associatedK3surface = method(Options => {Verbose => false, Strategy => null, Singular => null});
associatedK3surface SpecialGushelMukaiFourfold := associatedK3surface SpecialCubicFourfold := o -> X -> (
    if (not X.cache#?(surface X,"label")) and o.Verbose then <<"-- trying to recognize the fourfold"<<endl;
    recognize X;
    if o.Verbose then (if X.cache#(surface X,"label") === "NotRecognized" then <<"-- fourfold not recognized"<<endl else <<"-- the fourfold has been successfully recognized"<<endl);
    fanoMap(X,Singular=>o.Singular,Verbose=>o.Verbose);
    (L,C) := exceptionalCurves(X,Verbose=>o.Verbose,Strategy=>o.Strategy);
    U := ambientVariety L;
    mu := multirationalMap fanoMap X;
    if U.cache#?"MapToMinimalK3Surface" then return makeSurfaceAssociated(X,mu,U,{L,C},U.cache#"MapToMinimalK3Surface");
    genK3 := lift((discriminant(X)+2)/2,ZZ);
    f := null; H := random(1,0_U); local normU;
    if member(recognize X,{"NotRecognized", "october2021-34'", "october2021-20"}) then (
        if o.Verbose then <<"-- skipping computation of the map f from U to the minimal K3 surface of degree "<<discriminant X<<endl;
    ) else if recognize X === 3 then (
        if o.Verbose then <<"-- computing the normalization of U"<<endl;
        normU = normalization(U,Verbose=>false);
        if o.Verbose then <<"-- inverting the normalization of U"<<endl;
        f = multirationalMap super inverse3 normU;
    ) else if recognize X === 17 then (
        if o.Verbose then <<"-- computing desingularization of U"<<endl;
        f0 := rationalMap((support singularLocus U)_U,2,Dominant=>true);
        if o.Verbose then <<"-- computing the map f from U to the minimal K3 surface of degree "<<discriminant X<<endl;
        f1 := mapDefinedByDivisor(target f0,{(f0 (H*U),1),(f0 (3*C),1),(f0 L,1)});        
        f = f0 * f1;
    ) else if member(recognize X,{"oneNodalSepticDelPezzoSurfaceC26", 18}) then (
        if o.Verbose then <<"-- computing desingularization of U"<<endl;
        normU = experimentalNormalizationInv(U,Verbose=>o.Verbose);
        if o.Verbose then <<"-- computing the map f from U to the minimal K3 surface of degree "<<discriminant X<<endl;
        f = normU * mapDefinedByDivisor(image(normU,"F4"),{(random(1,0_(target normU)),1),(normU L,1),(normU C,degree C)});
    ) else if recognize X === 1 then (
        if o.Verbose then <<"-- computing the map f from U to the minimal K3 surface of degree "<<discriminant X<<endl;
        f = mapDefinedByDivisor(U,{(H,1),(L,1),(C,degree C)});
        if char coefficientRing X <= 65521 then image(f,"F4");
        f = rationalMap(f,Dominant=>true);
        if o.Verbose then <<"-- computing normalization of the surface image"<<endl;
        f = multirationalMap super toRationalMap(f * inverse3 normalization(target f,Verbose=>false));
        if f#"image" === null then error "something went wrong";
    ) else (
        if o.Verbose then <<"-- computing the map f from U to the minimal K3 surface of degree "<<discriminant X<<endl;    
        m := degree C;
        if recognize X === "C42" then m = 2;
        if recognize X === "mukai26''" then m = 3;
        f = mapDefinedByDivisor(U,{(H,1),(L,1),(C,m)});
        if recognize X === "FarkasVerra" then f = f * experimentalNormalizationInv(image(f,"F4"),Verbose=>o.Verbose);
    );
    if f =!= null then (
        if dim target f =!= genK3 then error("expected map to PP^"|(toString genK3)|", but got map to PP^"|toString(dim target f));
        if char coefficientRing X <= 65521 then (
            if o.Verbose then <<"-- computing the image of f using the F4 algorithm"<<endl;
            image(f,"F4");
        ) else if o.Strategy =!= "Inverse" then (
            if o.Verbose then <<"-- computing the image of f via interpolation"<<endl;
            interpolateImage(f,toList(binomial(genK3-2,2) : 2),2,Verbose=>o.Verbose);
        ) else image f;
        if f#"image" === null then error "something went wrong";
        if degrees image f =!= {({2},binomial(genK3-2,2))} then error "the degrees for the generators of the ideal of the K3 surface are not as expected";
        U.cache#"MapToMinimalK3Surface" = f;
    );
    return makeSurfaceAssociated(X,mu,U,{L,C},f);
);

associatedCastelnuovoSurface = method(Options => {Verbose => false, Strategy => null, Singular => null});
associatedCastelnuovoSurface IntersectionOfThreeQuadricsInP7 := o -> X -> (
    if (not X.cache#?(surface X,"label")) and o.Verbose then <<"-- trying to recognize the fourfold"<<endl;
    recognize X;
    if o.Verbose then (if X.cache#(surface X,"label") === "NotRecognized" then <<"-- fourfold not recognized"<<endl else <<"-- the fourfold has been successfully recognized"<<endl);
    fanoMap(X,Singular=>o.Singular,Verbose=>o.Verbose);
    (L,C) := exceptionalCurves(X,Verbose=>o.Verbose,Strategy=>o.Strategy);
    U := ambientVariety L;
    mu := multirationalMap fanoMap X;
    if U.cache#?"MapToMinimalK3Surface" then return makeSurfaceAssociated(X,mu,U,{L,C},U.cache#"MapToMinimalK3Surface"); -- inappropriate key name
    f := null; H := random(1,0_U);
    if member(recognize X,{"NotRecognized", "surf-7-1-9"}) then (
        if o.Verbose then <<"-- skipping computation of the map f from U to the minimal Castelnuovo surface"<<endl;
    ) else if member(recognize X,{"planeInPP7", "internal-projection-K3-genus-8", "surf-5-6-2-nodal"}) then (
        f = multirationalMap super toRationalMap 1_U;
    ) else if recognize X === "surf-5-7-0-1" then (
        if o.Verbose then <<"-- computing the map f from U to the minimal Castelnuovo surface"<<endl;
        f = mapDefinedByDivisor(U,{(H,1),(L,1)});
        if char coefficientRing X <= 65521 then image(f,"F4") else image f;
    ) else if recognize X === "surf-5-10-1" then (
        n := multirationalMap normalization(U,Verbose=>o.Verbose);
        if o.Verbose then <<"-- computing the map f from U to the minimal Castelnuovo surface"<<endl;
        h := rationalMap(source n,tally {n^* H,n^* L},Dominant=>3);
        f = multirationalMap inverse rationalMap(ring target h,ring target n,take(gens ring target h,5));
        if not((f * (inverse f) == 1 and (inverse f) * f == 1)) then error "something went wrong";
    ) else if recognize X === "surf-4-3-1-external" then (
        if o.Verbose then <<"-- computing the normalization of U"<<endl;
        n' := normalization(U,Verbose=>false);
        if o.Verbose then <<"-- inverting the normalization of U"<<endl;
        f = multirationalMap super inverse3 n';
    );
    if f =!= null then (
         U.cache#"MapToMinimalK3Surface" = f;
    );
    return makeSurfaceAssociated(X,mu,U,{L,C},f);
);

mirrorFourfold = method(TypicalValue => HodgeSpecialFourfold, Options => {Verbose => false, Strategy => null, Singular => null});
mirrorFourfold SpecialCubicFourfold := mirrorFourfold SpecialGushelMukaiFourfold := mirrorFourfold IntersectionOfThreeQuadricsInP7 := o -> X -> (
    if X.cache#?(surface X,"associatedFourfold") then return X.cache#(surface X,"associatedFourfold");
    if o.Verbose then <<"-- computing the Fano map"<<endl;
    mu := fanoMap(X,Singular=>o.Singular,Verbose=>o.Verbose); 
    if o.Verbose then <<"-- computing the surface U corresponding to the given fourfold"<<endl;
    U := surfaceDeterminingInverseOfFanoMap(X,Verbose=>o.Verbose,Strategy=>o.Strategy);
    W := specialFourfold(U,Var target mu,InputCheck=>0);
    W.cache#(surface W,"associatedFourfold") = X;
    X.cache#(surface X,"associatedFourfold") = W
);
mirrorFourfold HodgeSpecialFourfold := o -> X -> (
    if not X.cache#?(surface X,"associatedFourfold") then error "can't find associated fourfold";
    X.cache#(surface X,"associatedFourfold")
);

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
    X := specialGushelMukaiFourfold(psi ideal V,InputCheck=>(if o.Verify then 10 else 0),Verbose=>true);
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
    X := specialGushelMukaiFourfold(psi ideal E_1,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
    X.cache#(surface X,"label") = i;
    return X;
);

------------------------------------------------------------------------
---------------------- Prime Fano fourfolds ----------------------------
------------------------------------------------------------------------

fanoFourfold = method(TypicalValue => EmbeddedProjectiveVariety, Options => {CoefficientRing => ZZ/65521});
fanoFourfold (ZZ,ZZ) := o -> (d,g) -> (
    K := o.CoefficientRing;
    if not (instance(K,Ring) and isField K) then error "CoefficientRing option expects a field";
    local Y; local j; local S; local psi;
    local X;
    dg := {(2,0),(3,1),(4,1),(5,1),(4,3),(6,4),(8,5),(10,6),(12,7),(14,8),(16,9),(18,10)};
    if not member((d,g),dg) then error("expected a pair of integers in the set "|toString(dg));
    if d == 2 and g == 0 then X = random({2},0_(PP_K^5));
    if d == 3 and g == 1 then X = random({3},0_(PP_K^5));
    if d == 4 and g == 1 then X = random({{2},{2}},0_(PP_K^6));
    if d == 5 and g == 1 then (
        Y = GG(K,1,4);
        j = parametrize random({{1},{1}},0_Y);
        X = j^* Y;
    );
    if d == 4 and g == 3 then X = random({4},0_(PP_K^5));
    if d == 6 and g == 4 then X = random({{2},{3}},0_(PP_K^6));
    if d == 8 and g == 5 then X = random({3:{2}},0_(PP_K^7));
    if d == 10 and g == 6 then (
        Y = GG(K,1,4);
        j = parametrize random({1},0_Y);
        X = (j^* Y) * random({2},0_(source j));    
    );
    if d == 12 and g == 7 then (
        S = surface({3,4},K);
        psi = rationalMap(S * random({1},0_S),2);
        j = parametrize random({1},0_(target psi));
        X = j^* image(2,psi);
    );
    if d == 14 and g == 8 then (
        Y = GG(K,1,5);
        j = parametrize random({4:{1}},0_Y);
        X = j^* Y;
    );
    if d == 16 and g == 9 then (
        S = surface({2},K,ambient=>6);
        psi = rationalMap(S,3,2);
        j = parametrize random({{1},{1}},0_(target psi));
        X = j^* image psi;
    );
    if d == 18 and g == 10 then (
        -- p. 4 of [Kapustka and Ranestad - Vector Bundles On Fano Varieties Of Genus Ten] 
        w := gens ring PP_K^13;
        M := matrix {{0,-w_5,w_4,w_6,w_7,w_8,w_0},
                     {w_5,0,-w_3,w_12,w_13,w_9,w_1},
                     {-w_4,w_3,0,w_10,w_11,-w_6-w_13,w_2},
                     {-w_6,-w_12,-w_10,0,w_2,-w_1,w_3},
                     {-w_7,-w_13,-w_11,-w_2,0,w_0,w_4},
                     {-w_8,-w_9,w_6+w_13,w_1,-w_0,0,w_5},
                     {-w_0,-w_1,-w_2,-w_3,-w_4,-w_5,0}};
        Y = Var trim pfaffians(4,M);
        j = toRationalMap parametrize random({1},0_Y);
        X = j^* Y;
    );
    if not (dim X == 4 and degree X == d and sectionalGenus X == g) then error("something went wrong while computing random fourfold of degree "|toString(d)|" and sectional genus "|toString(g));
    return X;
);

parametrizeFanoFourfold = method(TypicalValue => MultirationalMap, Options => {Strategy => 1});
parametrizeFanoFourfold EmbeddedProjectiveVariety := o -> X -> (
    X#InverseMethod = inverse3;
    if dim X != 4 then error "expected a fourfold";
    if degree X == 5 and sectionalGenus X == 1 then (
        if o.Strategy === 2 then return parametrize X;
        if o.Strategy =!= 1 then error("the available strategies are:"|newline|"-- 1: projection from the plane spanned by a conic contained in the fourfold"|newline|"-- 2: projection from the unique sigma_(2,2) plane contained in the fourfold (Todd's result)");
        p := point X;
        C := line(X,p) + line(X,p);
        if degree C != 2 then error "something went wrong while finding conic on fourfold";
        return inverse3 multirationalMap rationalMap(sub(ideal C,ring X),1);
    );  
    if o.Strategy =!= 1 then error "strategy not available";
    parametrize X
);

------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------

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
    Phi#"target" = source Psi;
    -- if Phi#"inverse" != Psi then error "internal error encountered";  
    Phi#"inverse" = Psi;
    return Phi;
);
inverse3 (RationalMap,Option) := (psi,opt) -> inverse3 psi;
inverse3 (MultirationalMap,Option) := (Psi,opt) -> inverse3 Psi;

interpolateImage = method(Options => {Verbose => false, cache => true});
interpolateImage (MultirationalMap,List,ZZ) := o -> (Phi,D,j) -> (
    if Phi#"image" =!= null then return Phi#"image";
    if Phi#"isDominant" === true then return target Phi;
    if not all(D,d -> instance(d,ZZ)) then error "expected a list of integers";
    cont := 0;
    W := Phi point source Phi;
    while select(flatten degrees ideal W,d -> d <= j) =!= D do (
        if o.Verbose then <<"image "<<cont<<", ";
        W = W + Phi point source Phi;
        if o.Verbose then (<<"degrees: "; <<toStringDegreesVar W<<endl);
        cont = cont + 1;
    );
    for i to 3 do (
        if o.Verbose then <<"(verifying) image "<<cont<<", ";
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
        if o.Verbose then <<"top "<<cont<<", ";
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

numberNodes = method(Options => {Verbose => true});
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

normalization = method(Options => {Verbose => true});
normalization EmbeddedProjectiveVariety := o -> X -> (
    if X.cache#?"Normalization" then return X.cache#"Normalization";    
    if o.Verbose then <<"-- computing normalization of "|toString(? ideal X)<<endl;
    f := rationalMap icMap ring X;
    if o.Verbose then <<"-- got: "|toString(expression f)<<endl;
    X.cache#"Normalization" = f
);

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

varietyDefinedBylinearSyzygies = method();
varietyDefinedBylinearSyzygies EmbeddedProjectiveVariety := (cacheValue "varietyDefinedBylinearSyzygies") (Y -> (
    G := transpose syz gens ideal Y;
    M := matrix select(entries G,g -> max flatten degrees ideal g == 1);
    K := mingens kernel M;
    I := unique apply(entries transpose K,g -> trim ideal g);
    Var first select(I,i -> dim i >= 1)
));

toGushel = method();
toGushel SpecialGushelMukaiFourfold := X -> (
    if dim singLocus ambientFivefold X >= 0 then return X;
    j := toRationalMap toGrass X;
    Y := local Y;
    i := rationalMap(target j,(coefficientRing X)[Y,gens ambient target j],0|vars target j);
    i = rationalMap(i,Dominant=>sub(ideal target j,target i));
    S := trim lift((j*i) ideal surface X,ambient target i);
    Sv := intersect(S,ideal submatrix'(vars ambient target i,{0}));
    try H := ideal random({{1},{1}},Var Sv) else error "not able to specialize to Gushel type";
    h := (parametrize H)||(target i);
    specialGushelMukaiFourfold h^* S
);

fromOrdinaryToGushel = method();
fromOrdinaryToGushel SpecialGushelMukaiFourfold := X -> try toGushel X else error "not able to deform to Gushel type";

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

singLocus = method(); 
singLocus EmbeddedProjectiveVariety := X -> singularLocus(X,Saturate=>false);

Var = method(Options => {MinimalGenerators => false});
Var Ideal := o -> I -> projectiveVariety(I,MinimalGenerators=>o.MinimalGenerators,Saturate=>false);
Var Ring := o -> R -> projectiveVariety(R,MinimalGenerators=>o.MinimalGenerators,Saturate=>false);
Var Matrix := o -> M -> projectiveVariety(M,MinimalGenerators=>o.MinimalGenerators,Saturate=>false);

HodgeSpecialFourfold ? HodgeSpecialFourfold := (X,Y) -> (
    if not uniform {X,Y} then return incomparable;
    try (dX,dY) := (discriminant X,discriminant Y) else return incomparable;
    if dX < dY then return symbol <;
    if dX > dY then return symbol >;
    if instance(X,SpecialGushelMukaiFourfold) and instance(Y,SpecialGushelMukaiFourfold) and dX == dY and dX % 8 == 2 then (
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

------------------------------------------------------------------------
-------------------------- Prebuilt Examples ---------------------------
------------------------------------------------------------------------

trisecantFlop = method(Options => {Verbose => false});
trisecantFlop ZZ := o -> i -> (
    try needsPackage "TrisecantFlops" else (
        git := findProgram("git", "git --help");
        dir := temporaryFileName() | "/";
        mkdir dir;
        <<"The package TrisecantFlops is not present."<<endl;
        e := "";
        while not(e == "y" or e == "yes" or e == "Y" or e == "Yes") do (
            e = read("Do you want to download the latest version of the package now? (y/n) ");
            if e == "n" or e == "no" or e == "N" or e == "No" then error "required package TrisecantFlops";
        );
        <<"-- downloading the package TrisecantFlops from https://github.com/giovannistagliano"<<endl;    
        runProgram(git,"clone --depth 1 --no-checkout https://github.com/giovannistagliano/TrisecantFlops.git", RunDirectory => dir);
        runProgram(git, "checkout master", RunDirectory => dir | "/TrisecantFlops");
        if not fileExists(dir|"TrisecantFlops/TrisecantFlops.m2") then error "something went wrong in downloading the package TrisecantFlops";
        try needsPackage("TrisecantFlops",FileName => dir|"TrisecantFlops/TrisecantFlops.m2") else error "something went wrong in loading the package TrisecantFlops";
        <<"The package TrisecantFlops has been successfully loaded."<<endl;
        f := "";
        while not(f == "y" or f == "yes" or f == "Y" or f == "Yes" or f == "n" or f == "no" or f == "N" or f == "No") 
        do f = read("Do you want to install the package for future use? (y/n) ");
        if f == "y" or f == "yes" or f == "Y" or f == "Yes" then (
            <<"-- installing the package TrisecantFlops"<<endl;    
            installPackage("TrisecantFlops",Verbose => false,FileName => dir|"TrisecantFlops/TrisecantFlops.m2");
        );
    );
    if not member(value "TrisecantFlops",loadedPackages) then error "something went wrong";
    if (value "TrisecantFlops").Options.Version < "1.7" then (
        if o.Verbose then <<"-- removing old version of TrisecantFlops"<<endl;  
        uninstallPackage "TrisecantFlops";
        error "Your version of the TrisecantFlops package was outdated and has been removed. Please restart Macaulay2 and re-execute the function 'trisecantFlop'";
    );    
    value("trisecantFlop("|toString(i)|",Verbose=>"|toString(o.Verbose)|")")    
);

prebuiltExamplesOfRationalFourfolds = memoize(() -> (
    try needsPackage "PrebuiltExamplesOfRationalFourfolds" else (
        curl := findProgram("curl", "curl -h");
        dir := temporaryFileName() | "/";
        mkdir dir;
        <<"The package PrebuiltExamplesOfRationalFourfolds is not present."<<endl;
        e := "";
        while not(e == "y" or e == "yes" or e == "Y" or e == "Yes") do (
            e = read("Do you want to download the latest version of the package now? (y/n) ");
            if e == "n" or e == "no" or e == "N" or e == "No" then error "required package PrebuiltExamplesOfRationalFourfolds";
        );
        <<"-- downloading the package PrebuiltExamplesOfRationalFourfolds from https://github.com/giovannistagliano"<<endl;    
        runProgram(curl,"-s -o PrebuiltExamplesOfRationalFourfolds.m2 https://raw.githubusercontent.com/giovannistagliano/PrebuiltExamplesOfRationalFourfolds/main/PrebuiltExamplesOfRationalFourfolds.m2",RunDirectory=>dir);
        if not fileExists(dir|"PrebuiltExamplesOfRationalFourfolds.m2") then error "something went wrong in downloading the package PrebuiltExamplesOfRationalFourfolds";
        try needsPackage("PrebuiltExamplesOfRationalFourfolds",FileName => dir|"PrebuiltExamplesOfRationalFourfolds.m2") else error "something went wrong in loading the package PrebuiltExamplesOfRationalFourfolds";
        <<"The package PrebuiltExamplesOfRationalFourfolds has been successfully loaded."<<endl;
        f := "";
        while not(f == "y" or f == "yes" or f == "Y" or f == "Yes" or f == "n" or f == "no" or f == "N" or f == "No") 
        do f = read("Do you want to install the package for future use? (y/n) ");
        if f == "y" or f == "yes" or f == "Y" or f == "Yes" then (
            <<"-- installing the package PrebuiltExamplesOfRationalFourfolds"<<endl;    
            installPackage("PrebuiltExamplesOfRationalFourfolds",Verbose => false,FileName => dir|"PrebuiltExamplesOfRationalFourfolds.m2");
        );
    );
    if not member(value "PrebuiltExamplesOfRationalFourfolds",loadedPackages) then error "something went wrong";
    if (value "PrebuiltExamplesOfRationalFourfolds").Options.Version < "1.1" then (
        <<"-- removing old version of PrebuiltExamplesOfRationalFourfolds"<<endl;  
        uninstallPackage "PrebuiltExamplesOfRationalFourfolds";
        error "Your version of the PrebuiltExamplesOfRationalFourfolds package was outdated and has been removed. Macaulay2 should be restarted as soon as possible.";
    );    
    value ///importFrom(PrebuiltExamplesOfRationalFourfolds,{"prebuiltExampleOfRationalFourfold"});///;
    value ///prebuiltExampleOfRationalFourfold///
));

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
    X := specialGushelMukaiFourfold(V%image(psi,2),InputCheck=>o.InputCheck);
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
            <<i+1<<" of "<<#U<<": specialGushelMukaiFourfold("|(toExternalString new Array from S.cache#"linear system on PP^2")|","|(toExternalString new Array from flatten C.cache#"plane representation")|",\""|(o#"Gluing")|"\""|(if S.cache#?"FiniteNumberOfNodes" and S.cache#"FiniteNumberOfNodes" > 0 then (","|toString(S.cache#"FiniteNumberOfNodes")|")") else ")")<<endl;
            <<"used "<<(if o#"Gluing"=="cubic scroll" then "the cubic Segre PP^1 x PP^2 ⊂ PP^5 ⊂ PP^6" else "a quartic scroll fourfold in PP^6")<<endl;
            <<"glued along "<<(if degree C >= 3 and sectionalGenus C == 0 then "a rational" else if degree C >= 3 and sectionalGenus C == 1 then "an elliptic" else "")<<" curve of degree "<<degree C<<endl;
            <<"Description of the fourfold:"<<endl<<describe(U_i)<<endl<<endl;
        );
        <<endl;
    );
    if o#"Output" then U else apply(U,describe)
);

specialGushelMukaiFourfold (Array,Array,String,Thing) := o -> (a,b,s,nK) -> (
    if #a =!= #b then error "expected two arrays of the same length";
    if s =!= "cubic scroll" and s =!= "quartic scroll" then error "expected string to be \"cubic scroll\" or \"quartic scroll\""; 
    (n,K) := if instance(nK,ZZ) then (nK,ZZ/65521) else if instance(nK,Ring) then (0,nK) else if instance(nK,VisibleList) and #nK==2 then (nK_0,nK_1) else error "not valid input";
    C := (surface(a,K,NumNodes=>n,ambient=>6)).cache#"takeCurve" (first b,toList take(b,-(#b-1)));
    makeGMfromCurveOnSurfaceInP6(C,InputCheck=>o.InputCheck,Verbose=>o.Verbose,"Gluing"=>s)
);
specialGushelMukaiFourfold (Array,Array,String) := o -> (a,b,s) -> specialGushelMukaiFourfold(a,b,s,(0,ZZ/65521),InputCheck=>o.InputCheck,Verbose=>o.Verbose);
specialGushelMukaiFourfold (Array,Array,Thing) := o -> (a,b,nK) -> specialGushelMukaiFourfold(a,b,"cubic scroll",nK,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
specialGushelMukaiFourfold (Array,Array) := o -> (a,b) -> specialGushelMukaiFourfold(a,b,"cubic scroll",(0,ZZ/65521),InputCheck=>o.InputCheck,Verbose=>o.Verbose);

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
    -- inverse(g,Verify=>true) -- bug in MultiprojectiveVarieties, funzione inverse: alla riga "if image Phi != target Phi then (Phi#"isDominant" = false; Phi#"isBirational" = false; error "the multi-rational map is not dominant");" sembra che image Phi e target Phi non abbiano lo stesso ambiente. Non sono riuscito a riprodurre il bug convertendo g in stringa...
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

------------------------------------------------------------------------
---------------------------- Documentation -----------------------------
------------------------------------------------------------------------

beginDocumentation() 

document {Key => SpecialFanoFourfolds, 
Headline => "A package for working with Hodge-special fourfolds",
PARA {"This package contains several tools related to the rationality problem for cubic fourfolds, Gushel-Mukai fourfolds, and some other special Fano fourfolds. See ",HREF{"https://arxiv.org/abs/2204.11518","arXiv:2204.11518"}," for some applications."},
PARA {"The following are some papers that have benefited from this package."},
References => UL{
{"M. Hoff and G. Staglianò, ",EM"Explicit constructions of K3 surfaces and unirational Noether-Lefschetz divisors",", available at ",HREF{"https://arxiv.org/abs/2110.15819","arXiv:2110.15819"}," (2021)."},
{"G. Staglianò, ",EM"Some new rational Gushel fourfolds",", available at ",HREF{"https://arxiv.org/abs/2003.07809","arXiv:2003.07809"}," (2020)."},
{"G. Staglianò, ",EM"On some families of Gushel-Mukai fourfolds",", available at ",HREF{"https://arxiv.org/abs/2002.07026","arXiv:2002.07026"}," (2020)."},
{"M. Hoff and G. Staglianò, ",EM"New examples of rational Gushel-Mukai fourfolds",", available at ",HREF{"https://arxiv.org/abs/1910.12838","arXiv:1910.12838"}," (2020)."},
{"F. Russo and G. Staglianò, ",EM"Trisecant Flops, their associated K3 surfaces and the rationality of some Fano fourfolds",", available at ",HREF{"https://arxiv.org/abs/1909.01263","arXiv:1909.01263"}," (2020)."},
{"F. Russo and G. Staglianò, ",EM"Explicit rationality of some cubic fourfolds",", available at ",HREF{"https://arxiv.org/abs/1811.03502","arXiv:1811.03502"}," (2019)."},
{"F. Russo and G. Staglianò, ",EM"Congruences of 5-secant conics and the rationality of some admissible cubic fourfolds",", available at ",HREF{"https://arxiv.org/abs/1707.00999","arXiv:1707.00999"}," (2018)."}}}

document {Key => {SpecialGushelMukaiFourfold}, 
Headline => "the class of all special Gushel-Mukai fourfolds", 
PARA{"The general type of Gushel-Mukai fourfold (called ",EM "ordinary",") can be realized as the intersection of a smooth del Pezzo fivefold ", TEX///$\mathbb{G}(1,4)\cap\mathbb{P}^8\subset \mathbb{P}^8$///, " with a quadric hypersurface in ", TEX///$\mathbb{P}^8$///, ". A Gushel-Mukai fourfold is said to be ", EM"special", " if it contains a surface whose cohomology class ", EM "does not come", " from the Grassmannian ", TEX///$\mathbb{G}(1,4)$///, ". The special Gushel-Mukai fourfolds are parametrized by a countable union of (not necessarily irreducible) hypersurfaces in the corresponding moduli space, labelled by the integers ", TEX///$d \geq 10$///, " with ", TEX///$d = 0, 2, 4\ ({mod}\ 8)$///, "; the number ",TEX///$d$///," is called the discriminant of the fourfold. For precise definition and results, we refer mainly to the paper ", HREF{"https://arxiv.org/abs/1302.1398", "Special prime Fano fourfolds of degree 10 and index 2"}, ", by O. Debarre, A. Iliev, and L. Manivel."}, 
PARA{"An object of the class ", TO SpecialGushelMukaiFourfold, " is basically represented by a couple ", TEX///(S,X)///, ", where ", TEX///$X$///, " is a Gushel-Mukai fourfold and ", TEX///$S$///, " is a surface contained in ", TEX///$X$///, ".  The main constructor for the objects of the class is the function ", TO specialGushelMukaiFourfold,"."},
SeeAlso => {(discriminant,SpecialGushelMukaiFourfold)}}

document {Key => {(discriminant, SpecialCubicFourfold), (discriminant, HodgeSpecialFourfold)}, 
Headline => "discriminant of a special cubic fourfold", 
Usage => "discriminant X", 
Inputs => {"X" => SpecialCubicFourfold}, 
Outputs => {ZZ => {"the discriminant of ", TEX///$X$///}}, 
PARA{"This calculation passes through the determination of the topological Euler characteristic of the surface contained in the fourfold, which is obtained thanks to the functions ", TO EulerCharacteristic, " and ", TO Euler, " (the option ", TT "Algorithm", " allows you to select the method)."}, 
EXAMPLE {"X = specialCubicFourfold \"quintic del Pezzo surface\";", "time discriminant X"}, 
SeeAlso => {(discriminant, SpecialGushelMukaiFourfold)}} 

document {Key => {(discriminant, SpecialGushelMukaiFourfold)}, 
Headline => "discriminant of a special Gushel-Mukai fourfold", 
Usage => "discriminant X", 
Inputs => {"X" => SpecialGushelMukaiFourfold}, 
Outputs => {ZZ => {"the discriminant of ", TEX///$X$///}}, 
PARA{"This function applies a formula given in Section 7 of the paper ", HREF{"https://arxiv.org/abs/1302.1398", "Special prime Fano fourfolds of degree 10 and index 2"}, ", obtaining the data required through the functions ", TO cycleClass, ", ", TO EulerCharacteristic, " and ", TO Euler, " (the option ", TT "Algorithm", " allows you to select the method)."}, 
EXAMPLE {"X = specialGushelMukaiFourfold \"tau-quadric\";", "time discriminant X"}, 
SeeAlso => {(discriminant, SpecialCubicFourfold)}} 

undocumented{(expression, SpecialGushelMukaiFourfold), (describe, SpecialGushelMukaiFourfold)} 

document {Key => {Verbose, [specialCubicFourfold, Verbose], [specialGushelMukaiFourfold, Verbose], [mirrorFourfold, Verbose], [specialFourfold, Verbose], [parameterCount, Verbose],  [associatedK3surface, Verbose], [associatedCastelnuovoSurface, Verbose], [detectCongruence, Verbose], [trisecantFlop, Verbose]}, 
Headline => "request verbose feedback"}

document {Key => {specialGushelMukaiFourfold, (specialGushelMukaiFourfold, EmbeddedProjectiveVariety, EmbeddedProjectiveVariety), (specialGushelMukaiFourfold, Ideal, Ideal), [specialGushelMukaiFourfold, InputCheck]}, 
Headline => "make a special Gushel-Mukai fourfold", 
Usage => "specialGushelMukaiFourfold(S,X)", 
Inputs => {"S" => EmbeddedProjectiveVariety => {"a smooth irreducible surface ", TEX///$S\subset\mathbb{P}^8$///}, "X" => EmbeddedProjectiveVariety => {"a smooth prime Fano fourfold ", TEX///$X\subset \mathbb{P}^8$///, " of degree 10 and sectional genus 6, which contains the surface ", TEX///$S$///}}, 
Outputs => {SpecialGushelMukaiFourfold => {"the special Gushel-Mukai fourfold corresponding to the pair ", TEX///$(S,X)$///}}, 
PARA{"In the following example, we define a Gushel-Mukai fourfold containing a so-called ", TEX///$\tau$///, "-quadric."}, 
EXAMPLE {"K = ZZ/33331; x = gens ring PP_K^8;", "S = projectiveVariety ideal(x_6-x_7, x_5, x_3-x_4, x_1, x_0-x_4, x_2*x_7-x_4*x_8);", "X = projectiveVariety ideal(x_4*x_6-x_3*x_7+x_1*x_8, x_4*x_5-x_2*x_7+x_0*x_8, x_3*x_5-x_2*x_6+x_0*x_8+x_1*x_8-x_5*x_8, x_1*x_5-x_0*x_6+x_0*x_7+x_1*x_7-x_5*x_7, x_1*x_2-x_0*x_3+x_0*x_4+x_1*x_4-x_2*x_7+x_0*x_8, x_0^2+x_0*x_1+x_1^2+x_0*x_2+2*x_0*x_3+x_1*x_3+x_2*x_3+x_3^2-x_0*x_4-x_1*x_4-2*x_2*x_4-x_3*x_4-2*x_4^2+x_0*x_5+x_2*x_5+x_5^2+2*x_0*x_6+x_1*x_6+2*x_2*x_6+x_3*x_6+x_5*x_6+x_6^2-3*x_4*x_7+2*x_5*x_7-x_7^2+x_1*x_8+x_3*x_8-3*x_4*x_8+2*x_5*x_8+x_6*x_8-x_7*x_8);", "time F = specialGushelMukaiFourfold(S,X);", "time describe F", "assert(F == X)"},
SeeAlso => {(specialGushelMukaiFourfold, EmbeddedProjectiveVariety), (specialGushelMukaiFourfold, String, Ring), specialFourfold}} 

document {Key => {(specialGushelMukaiFourfold, EmbeddedProjectiveVariety),(specialGushelMukaiFourfold, Ideal)}, 
Headline => "random special Gushel-Mukai fourfold", 
Usage => "specialGushelMukaiFourfold S
specialGushelMukaiFourfold (S%Y)", 
Inputs => {"S" => EmbeddedProjectiveVariety => {"a smooth irreducible surface ",TEX///$S$///," which is a ",TO2{(symbol %,MultiprojectiveVariety,MultiprojectiveVariety),"subvariety"}," of a del Pezzo fivefold/sixfold ",TEX///$Y$///,"; alternatively, you can pass the ideal of ",TEX///$S$///," in ",TEX///$Y$///," (e.g., an ideal in the ring ", TO Grass, TEX///$(1,4)$///, ")"}}, 
Outputs => {SpecialGushelMukaiFourfold => {"a random special Gushel-Mukai fourfold containing the given surface"}}, 
EXAMPLE {"Y = GG(ZZ/33331,1,4);", "-- cubic scroll in G(1,4)"|newline|"S = schubertCycle({2,0},Y) * schubertCycle({1,0},Y) * schubertCycle({1,0},Y);", "X = specialGushelMukaiFourfold S;", "discriminant X"}, 
SeeAlso => {(specialGushelMukaiFourfold, String, Ring),(symbol %,MultiprojectiveVariety,MultiprojectiveVariety)}} 

document {Key => {(specialGushelMukaiFourfold, String, Ring), (specialGushelMukaiFourfold, String)}, 
Headline => "random special Gushel-Mukai fourfold of a given type", 
Usage => "specialGushelMukaiFourfold(n,K)
specialGushelMukaiFourfold n", 
Inputs => {"n" => String => {"the name of some known type of Gushel-Mukai fourfolds"}, "K" => {"the coefficient ring"}}, 
Outputs => {SpecialGushelMukaiFourfold => {"a random special Gushel-Mukai fourfold of the indicated type over ",TT"K"}},  
EXAMPLE {"X = specialGushelMukaiFourfold(\"cubic scroll\",ZZ/65521);", "describe X"},
References => UL{
{"O. Debarre, A. Iliev, and L. Manivel, ",EM"Special prime Fano fourfolds of degree 10 and index 2",", available at ",HREF{"https://arxiv.org/abs/1302.1398","arXiv:1302.1398"}," (2014)."},
{"G. Staglianò, ",EM"On some families of Gushel-Mukai fourfolds",", available at ",HREF{"https://arxiv.org/abs/2002.07026","arXiv:2002.07026"}," (2020)."}},
SeeAlso => {(specialGushelMukaiFourfold, EmbeddedProjectiveVariety), GMtables}}

document {Key => {toGrass, (toGrass, SpecialGushelMukaiFourfold)}, 
Headline => "Gushel morphism from a GM fourfold to GG(1,4)", 
Usage => "toGrass X", 
Inputs => {"X" => SpecialGushelMukaiFourfold}, 
Outputs => {MultirationalMap => {"a linear morphism from ", TEX///$X$///, " into the ",TO2{GrassmannianVariety,"Grassmannian"}," ", TEX///$\mathbb{G}(1,4)\subset\mathbb{P}^9$///, ", Plücker embedded, which is an embedding when ",TEX///$X$///," is of ordinary type"}},
EXAMPLE {"x = gens ring PP_(ZZ/33331)^8;", "X = specialGushelMukaiFourfold(ideal(x_6-x_7, x_5, x_3-x_4, x_1, x_0-x_4, x_2*x_7-x_4*x_8), ideal(x_4*x_6-x_3*x_7+x_1*x_8, x_4*x_5-x_2*x_7+x_0*x_8, x_3*x_5-x_2*x_6+x_0*x_8+x_1*x_8-x_5*x_8, x_1*x_5-x_0*x_6+x_0*x_7+x_1*x_7-x_5*x_7, x_1*x_2-x_0*x_3+x_0*x_4+x_1*x_4-x_2*x_7+x_0*x_8, x_0^2+x_0*x_1+x_1^2+x_0*x_2+2*x_0*x_3+x_1*x_3+x_2*x_3+x_3^2-x_0*x_4-x_1*x_4-2*x_2*x_4-x_3*x_4-2*x_4^2+x_0*x_5+x_2*x_5+x_5^2+2*x_0*x_6+x_1*x_6+2*x_2*x_6+x_3*x_6+x_5*x_6+x_6^2-3*x_4*x_7+2*x_5*x_7-x_7^2+x_1*x_8+x_3*x_8-3*x_4*x_8+2*x_5*x_8+x_6*x_8-x_7*x_8));", "time toGrass X", "show oo"}, 
SeeAlso => {(toGrass, EmbeddedProjectiveVariety), (symbol ===>, EmbeddedProjectiveVariety, EmbeddedProjectiveVariety)}} 

document {Key => {(toGrass, EmbeddedProjectiveVariety)}, 
Headline => "embedding of an ordinary Gushel-Mukai fourfold or a del Pezzo variety into GG(1,4)", 
Usage => "toGrass X", 
Inputs => {"X" => EmbeddedProjectiveVariety => {"an ordinary Gushel-Mukai fourfold, or a del Pezzo variety of dimension at least 4 (e.g., a sixfold projectively equivalent to ", TEX///$\mathbb{G}(1,4)\subset\mathbb{P}^9$///,")"}}, 
Outputs => {MultirationalMap => {"an embedding of ", TEX///$X$///, " into the ",TO2{GrassmannianVariety,"Grassmannian"}," ", TEX///$\mathbb{G}(1,4)\subset\mathbb{P}^9$///, ", Plücker embedded"}},
EXAMPLE {"x = gens ring PP_(ZZ/33331)^8;", "X = projectiveVariety ideal(x_4*x_6-x_3*x_7+x_1*x_8, x_4*x_5-x_2*x_7+x_0*x_8, x_3*x_5-x_2*x_6+x_0*x_8+x_1*x_8-x_5*x_8, x_1*x_5-x_0*x_6+x_0*x_7+x_1*x_7-x_5*x_7, x_1*x_2-x_0*x_3+x_0*x_4+x_1*x_4-x_2*x_7+x_0*x_8);", "time toGrass X", "show oo"}, 
SeeAlso => {(toGrass,SpecialGushelMukaiFourfold), (symbol ===>, EmbeddedProjectiveVariety, EmbeddedProjectiveVariety)}}

undocumented{(cycleClass, SpecialGushelMukaiFourfold)}

document {Key => {GMtables, (GMtables, ZZ, Ring), (GMtables, ZZ), [GMtables, Verify]}, 
Headline => "make examples of reducible subschemes of P^5", 
Usage => "GMtables(i,K)", 
Inputs => {"i" => ZZ => {"an integer between 1 and 21"}, "K" => Ring => {"the coefficient ring"}}, 
Outputs => {{"a triple of ",TO2{EmbeddedProjectiveVariety,"varieties"},", ",TEX///$(B,V,C)$///, ", which represents a reducible subscheme of ", TEX///$\mathbb{P}^5$///, ", in accordance with the 21 examples listed in Table 2 of the paper ", HREF{"https://arxiv.org/abs/2002.07026", "On some families of Gushel-Mukai fourfolds"}, "."}}, 
EXAMPLE {"(B,V,C) := GMtables(1,ZZ/33331)", "B * V == C"}, 
PARA{"The corresponding example of fourfold reported in Table 1 of the aforementioned paper can be obtained as follows."}, 
EXAMPLE {"psi = rationalMap(ideal B,Dominant=>2);", "X = specialGushelMukaiFourfold psi ideal V;"}, 
PARA{"This is basically the same as doing this:"}, 
EXAMPLE {"specialGushelMukaiFourfold(\"1\",ZZ/33331);"},
SeeAlso => {(specialGushelMukaiFourfold,String,Ring),(specialGushelMukaiFourfold,Array,Array)}} 

undocumented {(GMtables, Ring, String), (GMtables,EmbeddedProjectiveVariety,EmbeddedProjectiveVariety,EmbeddedProjectiveVariety)}; 

document {Key => {parameterCount, (parameterCount, EmbeddedProjectiveVariety, EmbeddedProjectiveVariety), (parameterCount, HodgeSpecialFourfold)}, 
Headline => "count of parameters",
Usage => "parameterCount(S,X)", 
Inputs => {"S" => EmbeddedProjectiveVariety, "X" => EmbeddedProjectiveVariety => {"such that ", TEX///$S\subseteq X$///}}, 
Outputs => {{"a count of parameters to estimate the dimensions of the corresponding Hilbert schemes"}},
PARA{"See ",TO (parameterCount, SpecialCubicFourfold)," and ", TO (parameterCount, SpecialGushelMukaiFourfold)," for more precise applications of this function."},
PARA{"The following calculation shows that the family of complete intersections of 3 quadrics in ",TEX///$\mathbb{P}^5$///," containing a rational normal quintic curve has codimension 1 in the space of all such complete intersections."},
EXAMPLE {"K = ZZ/33331; S = PP_K^(1,5);", "X = random({{2},{2},{2}},S);", "time parameterCount(S,X,Verbose=>true)"}, 
SeeAlso => {(parameterCount, SpecialCubicFourfold), (parameterCount, SpecialGushelMukaiFourfold), normalSheaf}} 

document {Key => {(parameterCount, SpecialCubicFourfold)}, 
Headline => "count of parameters in the moduli space of GM fourfolds", 
Usage => "parameterCount X", 
Inputs => {"X" => SpecialCubicFourfold => {"a special cubic fourfold containing a surface ", TEX///$S$///}}, 
Outputs => {ZZ => {"an upper bound for the codimension in the moduli space of cubic fourfolds of the locus of cubic fourfolds that contain a surface belonging to the same irreducible component of the Hilbert scheme containing ", TEX///$[S]$///}, Sequence => {"the triple of integers: ", TEX///$(h^0(I_{S/P^5}(3)), h^0(N_{S/P^5}), h^0(N_{S/X}))$///}}, 
PARA{"This function implements a parameter count explained in the paper ", HREF{"https://arxiv.org/abs/1503.05256", "Unirationality of moduli spaces of special cubic fourfolds and K3 surfaces"}, ", by H. Nuer."}, 
PARA{"Below, we show that the closure of the locus of cubic fourfolds containing a Veronese surface has codimension at most one (hence exactly one) in the moduli space of cubic fourfolds. Then, by the computation of the discriminant, we deduce that the cubic fourfolds containing a Veronese surface describe the Hassett's divisor ", TEX///$\mathcal{C}_{20}$///}, 
EXAMPLE {"K = ZZ/33331; V = PP_K^(2,2);", "X = specialCubicFourfold V;", "time parameterCount(X,Verbose=>true)", "discriminant X"}, 
SeeAlso => {(parameterCount, SpecialGushelMukaiFourfold), normalSheaf}} 

document {Key => {(parameterCount, SpecialGushelMukaiFourfold)}, 
Headline => "count of parameters in the moduli space of GM fourfolds", 
Usage => "parameterCount X", 
Inputs => {"X" => SpecialGushelMukaiFourfold => {"a special GM fourfold containing a surface ", TEX///$S$///, " and contained in a del Pezzo fivefold ", TEX///$Y$///}}, 
Outputs => {ZZ => {"an upper bound for the codimension in the moduli space of GM fourfolds of the locus of GM fourfolds that contain a surface belonging to the same irreducible component of the Hilbert scheme of ", TEX///$Y$///, " that contains ", TEX///$[S]$///}, Sequence => {"the triple of integers: ", TEX///$(h^0(I_{S/Y}(2)), h^0(N_{S/Y}), h^0(N_{S/X}))$///}}, 
PARA{"This function implements a parameter count explained in the paper ", HREF{"https://arxiv.org/abs/2002.07026", "On some families of Gushel-Mukai fourfolds"}, "."}, 
PARA{"Below, we show that the closure of the locus of GM fourfolds containing a cubic scroll has codimension at most one (hence exactly one) in the moduli space of GM fourfolds."}, 
EXAMPLE {"G = GG(ZZ/33331,1,4);", "S = (schubertCycle({2,0},G) * random({{1},{1}},0_G))%G", "X = specialGushelMukaiFourfold S;", "time parameterCount(X,Verbose=>true)", "discriminant X"}, 
SeeAlso => {(parameterCount, SpecialCubicFourfold), normalSheaf}} 

document {Key => {normalSheaf, (normalSheaf, EmbeddedProjectiveVariety), (normalSheaf, EmbeddedProjectiveVariety, EmbeddedProjectiveVariety)}, 
Headline => "normal sheaf", 
Usage => "normalSheaf X"|newline|"normalSheaf(X % Y)"|newline|"normalSheaf(X,Y)", 
Inputs => {"X" => EmbeddedProjectiveVariety, "Y" => EmbeddedProjectiveVariety => {" such that ",TEX///$X\subset Y$///," (if not given, it is taken to be the ",TO2{ambientVariety,"ambient variety"}," of ",TEX///$X$///,")"}}, 
Outputs => {CoherentSheaf => {"the normal sheaf ", TEX///$\mathcal{N}_{X, Y}$///, " of ", TEX///$X$///, " in ", TEX///$Y$///}},
EXAMPLE {"X = PP_(ZZ/65521)^(2,2);", "Y = random(2,X);", "N = normalSheaf X;", "N' = normalSheaf(X,Y);", "rank HH^0 N", "rank HH^0 N'"}}

undocumented {(normalSheaf, MultiprojectiveVariety), (normalSheaf, MultiprojectiveVariety, MultiprojectiveVariety)}

document {Key => {isAdmissible, (isAdmissible, ZZ), (isAdmissible, SpecialCubicFourfold)}, 
Headline => "whether an integer is admissible (in the sense of the theory of cubic fourfolds)", 
Usage => "isAdmissible d", 
Inputs => {"d" => ZZ}, 
Outputs => {Boolean => {"whether ", TT"d", " is admissible, i.e., it is an even integer ", TT"d>6", " which is not divisible by 4, 9 or any odd prime congruent to 2 modulo 3"}}, 
EXAMPLE{"select(150,isAdmissible)"},
SeeAlso => {isAdmissibleGM}} 

document {Key => {isAdmissibleGM, (isAdmissibleGM, ZZ), (isAdmissibleGM, SpecialGushelMukaiFourfold)}, 
Headline => "whether an integer is admissible (in the sense of the theory of GM fourfolds)", 
Usage => "isAdmissibleGM d", 
Inputs => {"d" => ZZ}, 
Outputs => {Boolean => {"whether ",TEX///$d$///," is an integer ",TEX///$>$///," 8 and ",TEX///$\equiv$///," 2 or 4 (mod 8) such that the only odd primes that divide ",TEX///$d$///," are ",TEX///$\equiv$///," 1 (mod 4). In other words, whether a GM fourfold of discriminant ", TT"d", " has an associated K3 surface."}},
EXAMPLE{"select(140,isAdmissibleGM)"},
SeeAlso => {isAdmissible}} 

document {Key => {CongruenceOfCurves}, 
Headline => "the class of all congruences of secant curves to surfaces", 
PARA{"Objects of this type are created by ",TO detectCongruence,"."}}

document {Key => {(symbol SPACE, CongruenceOfCurves, EmbeddedProjectiveVariety), (symbol SPACE, CongruenceOfCurves, Ideal)}, 
Headline => "get the curve of a congruence passing through a point", 
Usage => "f(p)", 
Inputs => {"f" => CongruenceOfCurves => {"a congruence of curves to a surface inside a variety ", TEX///$Y$///}, "p" => EmbeddedProjectiveVariety => {"a general point on ",TEX///$Y$///," (that is, a point on ",TEX///$Y$///," outside a certain proper Zariski closed subset)"}}, 
Outputs => {EmbeddedProjectiveVariety => {"the unique curve of the congruence ", TEX///$f$///, " that passes through ", TEX///$p$///}}, 
EXAMPLE {"X = specialCubicFourfold surface {3,4};", "f = detectCongruence(X,1);","C = f point ambient X;","member(C,f)","assert oo"},
SeeAlso => {detectCongruence, (member, EmbeddedProjectiveVariety, CongruenceOfCurves)}}

document {Key => {(map, CongruenceOfCurves)}, 
Headline => "compute the parameter space of a congruence", 
Usage => "map f", 
Inputs => {"f" => CongruenceOfCurves => {"a congruence of curves to a surface inside a variety ", TEX///$Y$///}}, 
Outputs => {MultirationalMap => {"a dominant map from ",TEX///$Y$///," to the parameter space of ",TEX///$f$///," whose general fibers are the curves of the congruence"}},
EXAMPLE {"S = PP_(ZZ/65521)[2,2];","Y = ambient S;","X = specialCubicFourfold S;","f = detectCongruence(X,1);","F = map f;","Q = target F","f;","p = point Y;","assert(f p == F^* F p)"},
SeeAlso => {detectCongruence}}

document {Key => {(member, EmbeddedProjectiveVariety, CongruenceOfCurves)}, 
Headline => "test membership in a congruence of curves", 
Usage => "member(C,f)", 
Inputs => {"C" => EmbeddedProjectiveVariety => {"a curve"}, "f" => CongruenceOfCurves}, 
Outputs => {Boolean => {"whether the curve ",TEX///$C$///," belongs to the congruence ", TEX///$f$///}}, 
SeeAlso => {(symbol SPACE, CongruenceOfCurves, EmbeddedProjectiveVariety)}}

undocumented{(toString, CongruenceOfCurves), (net, CongruenceOfCurves), (texMath, CongruenceOfCurves)} 

document {Key => {detectCongruence, (detectCongruence, HodgeSpecialFourfold, ZZ), (detectCongruence, HodgeSpecialFourfold)}, 
Headline => "detect and return a congruence of secant curves to a surface", 
PARA{"See ",TO (detectCongruence, SpecialCubicFourfold)," and ",TO (detectCongruence, SpecialGushelMukaiFourfold),"."}} 

document {Key => {(detectCongruence, SpecialCubicFourfold, ZZ), (detectCongruence, SpecialCubicFourfold)}, 
Headline => "detect and return a congruence of (3e-1)-secant curves of degree e", 
Usage => "detectCongruence X"|newline|"detectCongruence(X,e)", 
Inputs => {"X" => SpecialCubicFourfold => {"containing a surface ", TEX///$S\subset\mathbb{P}^5$///}, "e" => ZZ => {"a positive integer (optional but recommended)"}}, 
Outputs => {CongruenceOfCurves => {"that is a function which takes a general point ", TEX///$p\in\mathbb{P}^5$///, " (that is, outside a certain proper Zariski closed subset of ",TEX///$\mathbb{P}^5$///,") and returns the unique rational curve of degree ", TEX///$e$///, ", ", TEX///$(3e-1)$///, "-secant to ", TEX///$S$///, ", and passing through ", TEX///$p$///, " (an error is thrown if such a curve does not exist or is not unique)"}}, 
EXAMPLE {"-- A general cubic fourfold of discriminant 26"|newline|"X = specialCubicFourfold(\"3-nodal septic scroll\",ZZ/33331);", "describe X", "time f = detectCongruence(X,Verbose=>true);", "p := point ambient X -- random point on P^5", "time C = f p; -- 5-secant conic to the surface", "assert(dim C == 1 and degree C == 2 and dim(C * surface X) == 0 and degree(C * surface X) == 5 and isSubset(p, C))"}, 
SeeAlso => {(detectCongruence, SpecialGushelMukaiFourfold, ZZ), coneOfLines}} 

document {Key => {(detectCongruence, SpecialGushelMukaiFourfold, ZZ), (detectCongruence, SpecialGushelMukaiFourfold)}, 
Headline => "detect and return a congruence of (2e-1)-secant curves of degree e inside a del Pezzo fivefold", 
Usage => "detectCongruence X"|newline|"detectCongruence(X,e)", 
Inputs => {"X" => SpecialGushelMukaiFourfold => {"containing a surface ", TEX///$S\subset Y$///,", where ",TEX///$Y$///," denotes the unique del Pezzo fivefold containing the fourfold ",TEX///$X$///}, "e" => ZZ => {"a positive integer (optional but recommended)"}}, 
Outputs => {CongruenceOfCurves => {"that is a function which takes a general point ", TEX///$p\in Y$///, "(that is, a point on ",TEX///$Y$///," outside a certain proper Zariski closed subset) and returns the unique rational curve of degree ", TEX///$e$///, ", ", TEX///$(2e-1)$///, "-secant to ", TEX///$S$///, ", contained in ",TEX///$Y$///," and passing through ", TEX///$p$///, " (an error is thrown if such a curve does not exist or is not unique)"}}, 
EXAMPLE{"-- A GM fourfold of discriminant 20"|newline|"X = specialGushelMukaiFourfold(\"17\",ZZ/33331);", "describe X", "time f = detectCongruence(X,Verbose=>true);", "Y = ambientFivefold X; -- del Pezzo fivefold containing X", "p := point Y -- random point on Y", "time C = f p; -- 3-secant conic to the surface", "S = surface X;", "assert(dim C == 1 and degree C == 2 and dim(C*S) == 0 and degree(C*S) == 3 and isSubset(p,C) and isSubset(C,Y))"}, 
SeeAlso => {(detectCongruence, SpecialCubicFourfold, ZZ), coneOfLines}} 

document {Key => {SpecialCubicFourfold}, 
Headline => "the class of all special cubic fourfolds", 
PARA{"A cubic fourfold is a smooth cubic hypersurface in ", TEX///$\mathbb{P}^5$///, ". A cubic fourfold ", TEX///$X\subset \mathbb{P}^5$///, " is ", EM "special", " of discriminant ", TEX///$d>6$///, " if it contains an algebraic surface ", TEX///$S$///, ", and the discriminant of the saturated lattice spanned by ", TEX///$h^2$///, " and ", TEX///$[S]$///, " in ", TEX///$H^{2,2}(X,\mathbb{Z}):=H^4(X,\mathbb{Z})\cap H^2(\Omega_X^2)$///, " is ", TEX///$d$///, ", where ", TEX///$h$///, " denotes the class of a hyperplane section of ", TEX///$X$///, ". The set ", TEX///$\mathcal{C}_d$///, " of special cubic fourfolds of discriminant ", TEX///$d$///, " is either empty or an irreducible divisor inside the moduli space of cubic fourfolds ", TEX///$\mathcal{C}$///, ". Moreover, ", TEX///$\mathcal{C}_d\neq \emptyset$///, " if and only if ", TEX///$d>6$///, " and ", TEX///$d=$///, "0 or 2 (mod 6). For the general theory, see the papers ", HREF{"https://link.springer.com/article/10.1023/A:1001706324425", "Special cubic fourfolds"}, " and ", HREF{"http://imperium.lenin.ru/~kaledin/math/hasset.pdf", "Some rational cubic fourfolds"}, ", by B. Hassett."}, 
PARA{"An object of the class ", TO SpecialCubicFourfold, " is basically represented by a couple ", TEX///(S,X)///, ", where ", TEX///$X$///, " is a cubic fourfold and ", TEX///$S$///, " is a surface contained in ", TEX///$X$///, ". The surface ", TEX///$S$///, " is required to be smooth or with at most a finite number ", TEX///$n$///, " of non-normal nodes. This number ", TEX///$n$///, " (if known) can be specified manually using the option ", TT "NumNodes", ". The main constructor for the objects of the class is the function ", TO specialCubicFourfold,"."},
SeeAlso => {(discriminant,SpecialCubicFourfold)}} 

undocumented{(expression, SpecialCubicFourfold), (describe, SpecialCubicFourfold)} 

undocumented{InputCheck, NumNodes}

document {Key => {specialCubicFourfold, (specialCubicFourfold, EmbeddedProjectiveVariety, EmbeddedProjectiveVariety), (specialCubicFourfold, Ideal, Ideal), (specialCubicFourfold, Ideal, RingElement), [specialCubicFourfold, NumNodes], [specialCubicFourfold, InputCheck]}, 
Headline => "make a special cubic fourfold", 
Usage => "specialCubicFourfold(S,X)"|newline|"specialCubicFourfold(S,X,NumNodes=>n)", 
Inputs => {"S" => EmbeddedProjectiveVariety => {"an irreducible surface ", TEX///$S\subset\mathbb{P}^5$///, ", which has as singularities only a finite number ",TEX///$n\geq 0$///," of non-normal nodes (this number ",TEX///$n$///," should be passed with the option ", TT "NumNodes",", otherwise it is obtained using a probabilistic method)"}, "X" => EmbeddedProjectiveVariety => {"a smooth cubic fourfold ", TEX///$X\subset \mathbb{P}^5$///, " containing the surface ", TEX///$S$///}}, 
Outputs => {SpecialCubicFourfold => {"the special cubic fourfold corresponding to the pair ", TEX///$(S,X)$///}}, 
PARA{"In the example below, we define a cubic fourfold containing a rational scroll of degree 7 with 3 nodes."}, 
EXAMPLE {"K = ZZ/33331; x = gens ring PP_K^5;", "S = projectiveVariety ideal(x_0*x_2*x_3-2*x_1*x_2*x_3-x_1*x_3^2-x_2*x_3^2-x_0*x_1*x_4+2*x_1^2*x_4-x_1*x_2*x_4+x_2^2*x_4+2*x_0*x_3*x_4-x_1*x_3*x_4-x_1*x_4^2+x_1*x_3*x_5, x_1^2*x_3-4*x_1*x_2*x_3-x_0*x_3^2-3*x_1*x_3^2-2*x_2*x_3^2+2*x_0^2*x_4-9*x_0*x_1*x_4+11*x_1^2*x_4-x_0*x_2*x_4-2*x_1*x_2*x_4+2*x_2^2*x_4+12*x_0*x_3*x_4-7*x_1*x_3*x_4-4*x_3^2*x_4+x_0*x_4^2-6*x_1*x_4^2+4*x_2*x_4^2-2*x_3*x_4^2-2*x_4^3-x_0*x_1*x_5+x_1^2*x_5+2*x_1*x_2*x_5+3*x_0*x_3*x_5+2*x_1*x_3*x_5-x_3^2*x_5-x_0*x_4*x_5-4*x_1*x_4*x_5+3*x_2*x_4*x_5+2*x_3*x_4*x_5-x_1*x_5^2, x_0*x_1*x_3-7*x_1*x_2*x_3-3*x_0*x_3^2-4*x_1*x_3^2-3*x_2*x_3^2+x_3^3+3*x_0^2*x_4-14*x_0*x_1*x_4+17*x_1^2*x_4-x_0*x_2*x_4-3*x_1*x_2*x_4+3*x_2^2*x_4+19*x_0*x_3*x_4-9*x_1*x_3*x_4-x_2*x_3*x_4-6*x_3^2*x_4+x_0*x_4^2-9*x_1*x_4^2+6*x_2*x_4^2-3*x_3*x_4^2-3*x_4^3-2*x_0*x_1*x_5+2*x_1^2*x_5+4*x_1*x_2*x_5+5*x_0*x_3*x_5+4*x_1*x_3*x_5-2*x_3^2*x_5-2*x_0*x_4*x_5-7*x_1*x_4*x_5+5*x_2*x_4*x_5+3*x_3*x_4*x_5-2*x_1*x_5^2, x_0^2*x_3-12*x_1*x_2*x_3-6*x_0*x_3^2-6*x_1*x_3^2-5*x_2*x_3^2+2*x_3^3+5*x_0^2*x_4-24*x_0*x_1*x_4+29*x_1^2*x_4-x_0*x_2*x_4-5*x_1*x_2*x_4+5*x_2^2*x_4+32*x_0*x_3*x_4-14*x_1*x_3*x_4-2*x_2*x_3*x_4-10*x_3^2*x_4+x_0*x_4^2-15*x_1*x_4^2+10*x_2*x_4^2-5*x_3*x_4^2-5*x_4^3-3*x_0*x_1*x_5+3*x_1^2*x_5+6*x_1*x_2*x_5+8*x_0*x_3*x_5+7*x_1*x_3*x_5-3*x_3^2*x_5-3*x_0*x_4*x_5-11*x_1*x_4*x_5+8*x_2*x_4*x_5+5*x_3*x_4*x_5-3*x_1*x_5^2, x_1*x_2^2+6*x_1*x_2*x_3+2*x_0*x_3^2+3*x_1*x_3^2+2*x_2*x_3^2-x_3^3-3*x_0^2*x_4+12*x_0*x_1*x_4-14*x_1^2*x_4-2*x_2^2*x_4-15*x_0*x_3*x_4+6*x_1*x_3*x_4+x_2*x_3*x_4+5*x_3^2*x_4+x_0*x_4^2+8*x_1*x_4^2-5*x_2*x_4^2+2*x_3*x_4^2+2*x_4^3+x_0*x_1*x_5-2*x_1^2*x_5-4*x_1*x_2*x_5-4*x_0*x_3*x_5-3*x_1*x_3*x_5+2*x_3^2*x_5+2*x_0*x_4*x_5+7*x_1*x_4*x_5-4*x_2*x_4*x_5-2*x_3*x_4*x_5+2*x_1*x_5^2, x_0*x_2^2+10*x_1*x_2*x_3+3*x_0*x_3^2+5*x_1*x_3^2+4*x_2*x_3^2-x_3^3-5*x_0^2*x_4+19*x_0*x_1*x_4-22*x_1^2*x_4-x_0*x_2*x_4+3*x_1*x_2*x_4-4*x_2^2*x_4-24*x_0*x_3*x_4+9*x_1*x_3*x_4+x_2*x_3*x_4+8*x_3^2*x_4+2*x_0*x_4^2+11*x_1*x_4^2-7*x_2*x_4^2+4*x_3*x_4^2+3*x_4^3+2*x_0*x_1*x_5-4*x_1^2*x_5-7*x_1*x_2*x_5-7*x_0*x_3*x_5-5*x_1*x_3*x_5-x_2*x_3*x_5+3*x_3^2*x_5+4*x_0*x_4*x_5+12*x_1*x_4*x_5-7*x_2*x_4*x_5-3*x_3*x_4*x_5+4*x_1*x_5^2, x_1^2*x_2+17*x_1*x_2*x_3+6*x_0*x_3^2+9*x_1*x_3^2+7*x_2*x_3^2-2*x_3^3-9*x_0^2*x_4+36*x_0*x_1*x_4-44*x_1^2*x_4+3*x_0*x_2*x_4+5*x_1*x_2*x_4-7*x_2^2*x_4-47*x_0*x_3*x_4+21*x_1*x_3*x_4+2*x_2*x_3*x_4+16*x_3^2*x_4+24*x_1*x_4^2-16*x_2*x_4^2+7*x_3*x_4^2+7*x_4^3+3*x_0*x_1*x_5-6*x_1^2*x_5-9*x_1*x_2*x_5-12*x_0*x_3*x_5-8*x_1*x_3*x_5+5*x_3^2*x_5+5*x_0*x_4*x_5+19*x_1*x_4*x_5-12*x_2*x_4*x_5-7*x_3*x_4*x_5+5*x_1*x_5^2, x_0*x_1*x_2+29*x_1*x_2*x_3+11*x_0*x_3^2+15*x_1*x_3^2+12*x_2*x_3^2-4*x_3^3-16*x_0^2*x_4+62*x_0*x_1*x_4-74*x_1^2*x_4+5*x_0*x_2*x_4+9*x_1*x_2*x_4-12*x_2^2*x_4-80*x_0*x_3*x_4+35*x_1*x_3*x_4+4*x_2*x_3*x_4+27*x_3^2*x_4+40*x_1*x_4^2-27*x_2*x_4^2+12*x_3*x_4^2+12*x_4^3+5*x_0*x_1*x_5-10*x_1^2*x_5-16*x_1*x_2*x_5-21*x_0*x_3*x_5-14*x_1*x_3*x_5+9*x_3^2*x_5+9*x_0*x_4*x_5+33*x_1*x_4*x_5-21*x_2*x_4*x_5-12*x_3*x_4*x_5+9*x_1*x_5^2, x_0^2*x_2+49*x_1*x_2*x_3+19*x_0*x_3^2+25*x_1*x_3^2+20*x_2*x_3^2-7*x_3^3-28*x_0^2*x_4+106*x_0*x_1*x_4-124*x_1^2*x_4+8*x_0*x_2*x_4+16*x_1*x_2*x_4-20*x_2^2*x_4-134*x_0*x_3*x_4+58*x_1*x_3*x_4+7*x_2*x_3*x_4+45*x_3^2*x_4+66*x_1*x_4^2-45*x_2*x_4^2+20*x_3*x_4^2+20*x_4^3+9*x_0*x_1*x_5-18*x_1^2*x_5-28*x_1*x_2*x_5-37*x_0*x_3*x_5-23*x_1*x_3*x_5+16*x_3^2*x_5+16*x_0*x_4*x_5+57*x_1*x_4*x_5-36*x_2*x_4*x_5-20*x_3*x_4*x_5+16*x_1*x_5^2, x_1^3+47*x_1*x_2*x_3+18*x_0*x_3^2+23*x_1*x_3^2+19*x_2*x_3^2-7*x_3^3-24*x_0^2*x_4+97*x_0*x_1*x_4-117*x_1^2*x_4+8*x_0*x_2*x_4+16*x_1*x_2*x_4-19*x_2^2*x_4-127*x_0*x_3*x_4+54*x_1*x_3*x_4+7*x_2*x_3*x_4+42*x_3^2*x_4-x_0*x_4^2+62*x_1*x_4^2-42*x_2*x_4^2+19*x_3*x_4^2+19*x_4^3+9*x_0*x_1*x_5-16*x_1^2*x_5-25*x_1*x_2*x_5-33*x_0*x_3*x_5-23*x_1*x_3*x_5+14*x_3^2*x_5+14*x_0*x_4*x_5+51*x_1*x_4*x_5-33*x_2*x_4*x_5-19*x_3*x_4*x_5+14*x_1*x_5^2, x_0*x_1^2+79*x_1*x_2*x_3+29*x_0*x_3^2+40*x_1*x_3^2+32*x_2*x_3^2-11*x_3^3-41*x_0^2*x_4+164*x_0*x_1*x_4-196*x_1^2*x_4+14*x_0*x_2*x_4+26*x_1*x_2*x_4-32*x_2^2*x_4-214*x_0*x_3*x_4+92*x_1*x_3*x_4+11*x_2*x_3*x_4+71*x_3^2*x_4-2*x_0*x_4^2+105*x_1*x_4^2-71*x_2*x_4^2+32*x_3*x_4^2+32*x_4^3+14*x_0*x_1*x_5-26*x_1^2*x_5-41*x_1*x_2*x_5-55*x_0*x_3*x_5-38*x_1*x_3*x_5+23*x_3^2*x_5+23*x_0*x_4*x_5+85*x_1*x_4*x_5-55*x_2*x_4*x_5-32*x_3*x_4*x_5+23*x_1*x_5^2, x_0^2*x_1+133*x_1*x_2*x_3+48*x_0*x_3^2+68*x_1*x_3^2+54*x_2*x_3^2-18*x_3^3-70*x_0^2*x_4+278*x_0*x_1*x_4-330*x_1^2*x_4+24*x_0*x_2*x_4+44*x_1*x_2*x_4-54*x_2^2*x_4-361*x_0*x_3*x_4+156*x_1*x_3*x_4+18*x_2*x_3*x_4+120*x_3^2*x_4-4*x_0*x_4^2+177*x_1*x_4^2-120*x_2*x_4^2+54*x_3*x_4^2+54*x_4^3+23*x_0*x_1*x_5-44*x_1^2*x_5-69*x_1*x_2*x_5-93*x_0*x_3*x_5-63*x_1*x_3*x_5+39*x_3^2*x_5+39*x_0*x_4*x_5+144*x_1*x_4*x_5-93*x_2*x_4*x_5-54*x_3*x_4*x_5+39*x_1*x_5^2, x_0^3+224*x_1*x_2*x_3+80*x_0*x_3^2+115*x_1*x_3^2+91*x_2*x_3^2-30*x_3^3-119*x_0^2*x_4+470*x_0*x_1*x_4-555*x_1^2*x_4+41*x_0*x_2*x_4+75*x_1*x_2*x_4-91*x_2^2*x_4-608*x_0*x_3*x_4+263*x_1*x_3*x_4+30*x_2*x_3*x_4+202*x_3^2*x_4-8*x_0*x_4^2+297*x_1*x_4^2-202*x_2*x_4^2+91*x_3*x_4^2+91*x_4^3+39*x_0*x_1*x_5-76*x_1^2*x_5-118*x_1*x_2*x_5-158*x_0*x_3*x_5-105*x_1*x_3*x_5+67*x_3^2*x_5+68*x_0*x_4*x_5+245*x_1*x_4*x_5-158*x_2*x_4*x_5-91*x_3*x_4*x_5+67*x_1*x_5^2);", "X = projectiveVariety ideal(x_1^2*x_3+x_0*x_2*x_3-6*x_1*x_2*x_3-x_0*x_3^2-4*x_1*x_3^2-3*x_2*x_3^2+2*x_0^2*x_4-10*x_0*x_1*x_4+13*x_1^2*x_4-x_0*x_2*x_4-3*x_1*x_2*x_4+3*x_2^2*x_4+14*x_0*x_3*x_4-8*x_1*x_3*x_4-4*x_3^2*x_4+x_0*x_4^2-7*x_1*x_4^2+4*x_2*x_4^2-2*x_3*x_4^2-2*x_4^3-x_0*x_1*x_5+x_1^2*x_5+2*x_1*x_2*x_5+3*x_0*x_3*x_5+3*x_1*x_3*x_5-x_3^2*x_5-x_0*x_4*x_5-4*x_1*x_4*x_5+3*x_2*x_4*x_5+2*x_3*x_4*x_5-x_1*x_5^2);", "time F = specialCubicFourfold(S,X,NumNodes=>3);", "time describe F", "assert(F == X)"},
SeeAlso => {(specialCubicFourfold, EmbeddedProjectiveVariety), (specialCubicFourfold, String, Ring), specialFourfold}} 

document {Key => {(specialCubicFourfold, EmbeddedProjectiveVariety), (specialCubicFourfold, Ideal)}, 
Headline => "random special cubic fourfold", 
Usage => "specialCubicFourfold S"|newline|"specialCubicFourfold(S,NumNodes=>n)", 
Inputs => {"S" => EmbeddedProjectiveVariety => {"an irreducible surface in ", TEX///$\mathbb{P}^5$///}}, 
Outputs => {SpecialCubicFourfold => {"a random cubic fourfold containing the given surface"}}, 
EXAMPLE {"-- quintic del Pezzo surface"|newline|"S = surface({3,4},ZZ/33331);", "X = specialCubicFourfold S;", "discriminant X"}, 
SeeAlso => {(specialCubicFourfold, String, Ring)}} 

document {Key => {(specialCubicFourfold, String, Ring), (specialCubicFourfold, String)}, 
Headline => "random special cubic fourfold of a given type", 
Usage => "specialCubicFourfold(n,K)
specialCubicFourfold n", 
Inputs => {"n" => String => {"the name of some known type of cubic fourfolds"}, "K" => {"the coefficient ring"}}, 
Outputs => {SpecialCubicFourfold => {"a random special cubic fourfold of the indicated type over ",TT"K"}},  
EXAMPLE {"X = specialCubicFourfold(\"3-nodal septic scroll\",ZZ/65521);", "describe X"},
SeeAlso => (specialCubicFourfold, EmbeddedProjectiveVariety)}

document {Key => {ambientFivefold, (ambientFivefold, HodgeSpecialFourfold)}, 
Headline => "get the ambient fivefold of the Hodge-special fourfold", 
Usage => "ambientFivefold X", 
Inputs => {"X" => HodgeSpecialFourfold}, 
Outputs => {EmbeddedProjectiveVariety => {"the ambient fivefold of ",TT"X"}},
EXAMPLE {"S = surface {4,5,1};", "V = random(3,S);", "X = V * random(2,S);", "F = specialFourfold(S,X,V);", "ambientFivefold F"},
PARA {"When ",TEX///$X$///," is a ",TO2{SpecialGushelMukaiFourfold,"GM fourfold"},", the ambient fivefold of ",TEX///$X$///," is a fivefold ",TEX///$Y\subset\mathbb{P}^8$///," of degree 5 such that ",TEX///$X\subset Y$///," is a quadric hypersurface. We have that the fourfold ",TEX///$X$///," is of ordinary type if and only if ",TEX///$Y$///," is smooth."},
EXAMPLE {
"X = specialFourfold(\"21\",ZZ/33331);",
"describe X",
"Y = ambientFivefold X;",
"isSubset(X,Y)",
"Y!"}}

document {Key => {(map, SpecialCubicFourfold)}, 
Headline => "associated cubic map", 
Usage => "map X", 
Inputs => {"X" => SpecialCubicFourfold => {"containing a surface ", TEX///$S\subset\mathbb{P}^5$///}}, 
Outputs => {RationalMap => {"the rational map from ", TEX///$\mathbb{P}^5$///, " defined by the linear system of cubics through ", TEX///$S$///}}} 

document {Key => {(map, SpecialGushelMukaiFourfold)}, 
Headline => "associated quadratic map", 
Usage => "map X", 
Inputs => {"X" => SpecialGushelMukaiFourfold => {"containing a surface ", TEX///$S\subset Y$///, ", where ", TEX///$Y\subset\mathbb{P}^8$///, " is the unique del Pezzo fivefold containing ", TEX///$X$///}}, 
Outputs => {RationalMap => {"the rational map from ", TEX///$Y$///, " defined by the linear system of quadrics through ", TEX///$S$///}}} 

document {Key => {surface, (surface, HodgeSpecialFourfold)}, 
Headline => "get the special surface contained in the fourfold", 
Usage => "surface X", 
Inputs => {"X" => HodgeSpecialFourfold}, 
Outputs => {EmbeddedProjectiveVariety => {"the special surface contained in the fourfold ",TT"X"}}, 
EXAMPLE {"X = specialFourfold \"quintic del Pezzo surface\";", "V = ambientFivefold X;", "S = surface X;", "assert isSubset(S,X)"},
SeeAlso => {(surface,List)}} 

document { 
Key => {(surface, List), (surface, VisibleList, Ring), (surface, VisibleList, Option), (surface, VisibleList, Option), (surface, VisibleList, Ring, Option), (surface, VisibleList, Option, Option), (surface, VisibleList, Ring, Option, Option)},
Headline => "get a rational surface", 
Usage => "surface {a,i,j,k,...}
surface({a,i,j,k,...),K)
surface({a,i,j,k,...},K,NumNodes=>n,ambient=>m)",
Inputs => {List => {"a list ",TEX///$\{a,i,j,k,\ldots\}$///," of nonnegative integers"}}, 
Outputs => {EmbeddedProjectiveVariety => {"the image of the rational map defined by the linear system of curves of degree ",TEX///$a$///," in ",TEX///$\mathbb{P}_{K}^2$///," having ",TEX///$i$///," random base points of multiplicity 1, ",TEX///$j$///," random base points of multiplicity 2, ",TEX///$k$///," random base points of multiplicity 3, and so on until the last integer in the given list."}},
PARA{"In the example below, we take the image of the rational map defined by the linear system of septic plane curves with 3 random simple base points and 9 random double points."}, 
EXAMPLE { 
"S = surface {7,3,9};",
"coefficientRing S",
"T = surface({7,3,9},ZZ/33331);",
"X = specialCubicFourfold T;",
"coefficientRing X",
"describe X"},
SeeAlso => {(rationalMap,PolynomialRing,List),(specialGushelMukaiFourfold,Array,Array)}}

typValSurf := typicalValues#surface;
typicalValues#surface = Nothing;
document {Key => {(surface, MultiprojectiveVariety, MultiprojectiveVariety)}, 
Headline => "make a Hodge-special surface", 
Usage => "surface(C,S)", 
Inputs => {"C" => MultiprojectiveVariety => {"an irreducible curve"}, "S" => MultiprojectiveVariety => {"a smooth surface ", TEX///$S$///, " containing the curve ", TEX///$C$///}}, 
Outputs => {{"the Hodge special surface corresponding to the pair ", TEX///$(C,S)$///}}, 
PARA{"The curve ",TEX///$C$///," can be recovered using the function ",TT "curve","."},
EXAMPLE lines ///K = ZZ/65521;
C = random PP_K^(1,3); -- random twisted cubic in P^3
j = parametrize PP_K(1,1,1,4); 
C = (rationalMap(ambient C,source j) * j) C;
describe C
S = random(8,C);
describe S
S = surface(C,S);
discriminant S
parameterCount(S,Verbose=>true)
f := map(S,1,0)
f = quadricFibration f
discriminant f///,
Caveat => {"This feature is currently under development."}}
typicalValues#surface = typValSurf;

undocumented {"curve"}

document {Key => {unirationalParametrization, (unirationalParametrization, SpecialCubicFourfold), (unirationalParametrization, SpecialCubicFourfold, EmbeddedProjectiveVariety), (unirationalParametrization, SpecialGushelMukaiFourfold), (unirationalParametrization, HodgeSpecialFourfold)}, 
Headline => "unirational parametrization", 
Usage => "unirationalParametrization X", 
Inputs => {"X" => SpecialCubicFourfold => {"or ", ofClass SpecialGushelMukaiFourfold}}, 
Outputs => {MultirationalMap => {"a rational map of degree 2 from ",TEX///$\mathbb{P}^4$///," to ",TEX///$X$///}}, 
PARA{"The degree of the forms defining the returned map is 10 in the case of cubic fourfolds, and 26 in the case of GM fourfolds."}, 
EXAMPLE {"K = ZZ/10000019; S = PP_K^(2,2); -- Veronese surface;", "X = specialCubicFourfold S;", "time f = unirationalParametrization X;", "degreeSequence f", "degree(f,Strategy=>\"random point\")"}, 
SeeAlso => {(parametrize, HodgeSpecialFourfold), (parametrize, MultiprojectiveVariety)}} 

document {Key => {(parametrize, HodgeSpecialFourfold)}, 
Headline => "rational parametrization", 
Usage => "parametrize X", 
Inputs => {"X" => HodgeSpecialFourfold}, 
Outputs => {MultirationalMap => {"a birational map from a rational fourfold to ", TT "X"}}, 
PARA{"Some Hodge-special fourfolds are known to be rational. In this case, the function tries to obtain a birational map from ", TEX///$\mathbb{P}^4$///, " (or, e.g., from a quadric hypersurface in ", TEX///$\mathbb{P}^5$///, ") to the fourfold."}, 
EXAMPLE {"X = specialFourfold surface {3,4};", "phi = parametrize X;", "describe phi"}, 
EXAMPLE {"Y = specialFourfold \"tau-quadric\";", "psi = parametrize Y;", "describe psi"}, 
EXAMPLE {"Z = specialFourfold \"plane in PP^7\";", "eta = parametrize Z;", "describe eta"}, 
SeeAlso => {unirationalParametrization, (parametrize,MultiprojectiveVariety)}} 

document {Key => {fromOrdinaryToGushel, (fromOrdinaryToGushel, SpecialGushelMukaiFourfold)},
Headline => "try to deform to a fourfold of Gushel type",
Usage => "fromOrdinaryToGushel X", 
Inputs => {"X" => SpecialGushelMukaiFourfold => {"a fourfold of ordinary type"}}, 
Outputs => {SpecialGushelMukaiFourfold => {"a fourfold of Gushel type, a deformation of ",TT"X"}}, 
EXAMPLE {"X = specialGushelMukaiFourfold \"quintic del Pezzo surface\";", "singularLocus ambientFivefold X", "X' = fromOrdinaryToGushel X;", "support singularLocus ambientFivefold X'"}} 

document {Key => {associatedK3surface, [associatedK3surface, Strategy], building}, 
Headline => "K3 surface associated to a rational fourfold", 
PARA{"See ",TO (associatedK3surface, SpecialCubicFourfold)," and ",TO (associatedK3surface, SpecialGushelMukaiFourfold),"."}} 

document {Key => {(associatedK3surface, SpecialCubicFourfold)}, 
Headline => "K3 surface associated to a rational cubic fourfold", 
Usage => "U' = associatedK3surface X", 
Inputs => {"X" => SpecialCubicFourfold => {"containing a surface ", TEX///$S\subset\mathbb{P}^5$///," that admits a ",TO2{CongruenceOfCurves,"congruence"}," of ",TEX///$(3e-1)$///,"-secant curves of degree ",TEX///$e$///}}, 
Outputs => {"U'" => {"the (minimal) K3 surface associated to ",TEX///$X$///}},
Consequences => {{{TT"building U'"," will return the following four objects:"}, UL{{"the dominant ",TO2{MultirationalMap,"rational map"}," ",TEX///$\mu:\mathbb{P}^5 \dashrightarrow W$///," defined by the linear system of hypersurfaces of degree ",TEX///$3e-1$///," having points of multiplicity ",TEX///$e$///," along ",TEX///$S$///,";"}, {"the ",TO2{EmbeddedProjectiveVariety,"surface"}," ",TEX///$U\subset W$///," determining the inverse map of the restriction of ",TEX///$\mu$///," to ",TEX///$X$///,";"}, {"the ",TO2{List,"list"}," of the exceptional curves on the surface ",TEX///$U$///,";"}, {"a ",TO2{MultirationalMap,"rational map"}," of degree 1 from the surface ",TEX///$U$///," to the minimal K3 surface ",TEX///$U'$///,"."}}}},
PARA {"For more details and notation, see the papers ",HREF{"https://arxiv.org/abs/1909.01263","Trisecant Flops, their associated K3 surfaces and the rationality of some Fano fourfolds"}," and ",HREF{"https://arxiv.org/abs/2204.11518","Explicit computations with cubic fourfolds, Gushel-Mukai fourfolds, and their associated K3 surfaces"},"."},
EXAMPLE {"X = specialCubicFourfold \"quartic scroll\";", "describe X", "time U' = associatedK3surface X;", "(mu,U,C,f) = building U';", "? mu", "? U", "last C", "assert(image f == U')"},
SeeAlso => {(associatedK3surface, SpecialGushelMukaiFourfold), detectCongruence, mirrorFourfold}} 

document {Key => {(associatedK3surface, SpecialGushelMukaiFourfold)}, 
Headline => "K3 surface associated to a rational Gushel-Mukai fourfold", 
Usage => "U' = associatedK3surface X", 
Inputs => {"X" => SpecialGushelMukaiFourfold => {"containing a surface ", TEX///$S\subset Y$///," that admits a ",TO2{CongruenceOfCurves,"congruence"}," of ",TEX///$(2e-1)$///,"-secant curves of degree ",TEX///$e$///," inside the ",TO2{ambientFivefold,"ambient fivefold"}," ",TEX///$Y$///," of the fourfold ",TEX///$X$///}}, 
Outputs => {"U'" => {"the (minimal) K3 surface associated to ",TEX///$X$///}},
Consequences => {{{TT"building U'"," will return the following four objects:"}, UL{{"the dominant ",TO2{MultirationalMap,"rational map"}," ",TEX///$\mu:Y\dashrightarrow W$///," defined by the linear system of hypersurfaces of degree ",TEX///$2e-1$///," having points of multiplicity ",TEX///$e$///," along ",TEX///$S$///,";"}, {"the ",TO2{EmbeddedProjectiveVariety,"surface"}," ",TEX///$U\subset W$///," determining the inverse map of the restriction of ",TEX///$\mu$///," to ",TEX///$X$///,";"}, {"the ",TO2{List,"list"}," of the exceptional curves on the surface ",TEX///$U$///,";"}, {"a ",TO2{MultirationalMap,"rational map"}," of degree 1 from the surface ",TEX///$U$///," to the minimal K3 surface ",TEX///$U'$///,"."}}}},
PARA {"For more details and notation, see the paper ",HREF{"https://arxiv.org/abs/2204.11518","Explicit computations with cubic fourfolds, Gushel-Mukai fourfolds, and their associated K3 surfaces"},"."},
EXAMPLE {"X = specialGushelMukaiFourfold \"tau-quadric\";", "describe X", "time U' = associatedK3surface X;", "(mu,U,C,f) = building U';", "? mu", "? U", "first C -- two disjoint lines", "assert(image f == U')"},
SeeAlso => {(associatedK3surface, SpecialCubicFourfold), detectCongruence, mirrorFourfold}} 

document {Key => {parametrizeFanoFourfold, (parametrizeFanoFourfold, EmbeddedProjectiveVariety), [parametrizeFanoFourfold,Strategy]}, 
Headline => "rational parametrization of a prime Fano fourfold of coindex at most 3", 
Usage => "parametrize X
parametrizeFanoFourfold(X,Strategy=>...)", 
Inputs => {"X" => EmbeddedProjectiveVariety => {"a prime Fano fourfold ",TEX///$X$///," of coindex at most 3 having degree ",TEX///$d$///," and genus ",TEX///$g$///," with ",TEX///$(d,g)\in\{(2,0),(4,1),(5,1),(12,7),(14,8),(16,9),(18,10)\}$///}}, 
Outputs => {MultirationalMap => {"a birational map from ",TEX///$\mathbb{P}^4$///," to ", TEX///$X$///}}, 
PARA{"This function is mainly based on results contained in the classical paper ",HREF{"https://link.springer.com/article/10.1007/BF02413916","Algebraic varieties with canonical curve sections"},", by L. Roth. In some examples, more strategies are available. For instance, if ",TEX///$X\subset\mathbb{P}^7$///," is a 4-dimensional linear section of ",TEX///$\mathbb{G}(1,4)\subset\mathbb{P}^9$///,", then by passing ",TT"Strategy=>1"," (which is the default choice) we get the inverse of the projection from the plane spanned by a conic contained in ",TEX///$X$///,"; while with ",TT"Strategy=>2"," we get the projection from the unique ",TEX///$\sigma_{2,2}$///,"-plane contained in ",TEX///$X$///," (Todd's result)."},
EXAMPLE {"K = ZZ/65521; X = GG_K(1,4) * random({{1},{1}},0_(GG_K(1,4)));","? X", "time parametrizeFanoFourfold X"}, 
SeeAlso => {fanoFourfold,(parametrize,HodgeSpecialFourfold),unirationalParametrization,(parametrize,MultiprojectiveVariety)}} 

document {Key => {fanoFourfold, (fanoFourfold,ZZ,ZZ), [fanoFourfold,CoefficientRing]}, 
Headline => "random prime Fano fourfold of coindex at most 3", 
Usage => "fanoFourfold(d,g)
fanoFourfold(d,g,CoefficientRing=>K)", 
Inputs => {{TT"(d,g)"," a pair of integers belonging to the set ",TEX///$\{(2,0),(3,1),(4,1),(5,1),(4,3),(6,4),(8,5),(10,6),(12,7),(14,8),(16,9),(18,10)\}$///}},
Outputs => {EmbeddedProjectiveVariety => {"a random prime Fano fourfold of coindex at most 3 having degree ",TEX///$d$///," and genus ",TEX///$g$///}},
EXAMPLE {"X = fanoFourfold(4,1);", "describe X", "parametrize X"}, 
SeeAlso => {parametrizeFanoFourfold}}

document { 
Key => {(clean,HodgeSpecialFourfold)}, 
Headline => "clean the internal information of a fourfold", 
Usage => "clean X", 
Inputs => {"X" => HodgeSpecialFourfold}, 
Outputs => {HodgeSpecialFourfold => {"which is mathematically identical to ",TT"X",", but new to the system"}},
PARA{"This function is only useful for testing."},
EXAMPLE {"X = specialFourfold \"quartic scroll\"", "X' = clean X", "X === X'"}}

document {Key => {trisecantFlop}, 
Headline => "examples of trisecant flops", 
Usage => "trisecantFlop i", 
Inputs => {"i" => ZZ => {"an integer between 0 and 17"}},
Outputs => {{"the i-th example of birational map ",TEX///$X\dashrightarrow W$///," in accordance to the Table 1 in the paper ",HREF{"https://arxiv.org/abs/1909.01263","Trisecant Flops, their associated K3 surfaces and the rationality of some Fano fourfolds"},"."}}, 
PARA{"This function requires the package ",HREF{"https://github.com/giovannistagliano/TrisecantFlops","TrisecantFlops"},". If not present the user will be asked to automatically install the package."},
SeeAlso => {(specialFourfold, String, ZZ)}}
undocumented {(trisecantFlop,ZZ)}

document {Key => {(specialFourfold, String, ZZ)}, 
Headline => "load a prebuilt example of fourfold", 
Usage => "specialFourfold(str,i)", 
Inputs => {"str" => String => {"such as \"",TT"prebuilt-example-in-P5","\" or \"",TT"prebuilt-example-in-P7","\"."}, "i" => ZZ},
Outputs => {HodgeSpecialFourfold => {"the i-th example of fourfold in accordance with some classification (e.g., ",TT"specialFourfold(\"prebuilt-example-in-P5\",i)"," is the same as ",TO2{(source,MultirationalMap),"source"}," ",TO trisecantFlop,TT"(i)","."}}, 
PARA{"This function requires the package ",HREF{"https://github.com/giovannistagliano/TrisecantFlops","TrisecantFlops"},". If not present the user will be asked to automatically install the package."},
SeeAlso => trisecantFlop}

undocumented {(random, HodgeSpecialFourfold), (symbol **, HodgeSpecialFourfold,Ring), (map, HodgeSpecialFourfold), (describe, HodgeSpecialFourfold)}

document {Key => {(specialGushelMukaiFourfold, Array, Array, String, Thing), (specialGushelMukaiFourfold, Array, Array), (specialGushelMukaiFourfold, Array, Array, String), (specialGushelMukaiFourfold, Array, Array, Thing)}, 
Headline => "construct GM fourfolds by gluing cubic or quartic scrolls to surfaces in PP^6", 
Usage => "specialGushelMukaiFourfold(surface,curve)
specialGushelMukaiFourfold(surface,curve,scroll)
specialGushelMukaiFourfold(surface,curve,K)
specialGushelMukaiFourfold(surface,curve,scroll,K)", 
Inputs => {"surface" => Array => {"an array of integers ",TT"[a,i,j,k,...]"," to indicate the rational surface ",TEX///$S\subset\mathbb{P}^6$///," constructed by ",TO2{(surface,List),"surface"},TT"({a,i,j,k,...},K,ambient=>6)"},
           "curve" => Array => {"an array of integers ",TT"[d,l,m,n,...]"," to indicate the plane representation of a curve ",TEX///$C$///," on the surface ",TEX///$S$///," (the command that constructs ",TEX///$C$///," is ",TT///S.cache#"takeCurve"(d,{l,m,n,...})///,")"},
           "scroll" => String => {"which can be either \"cubic scroll\" (the default value) or \"quartic scroll\", to indicate the type of scroll ",TEX///$B\subset\mathbb{P}^6$///," to be used; in the former case ",TEX///$B\simeq\mathbb{P}^1\times\mathbb{P}^2\subset\mathbb{P}^5\subset\mathbb{P}^6$///," while in the latter case ",TEX///$B\subset\mathbb{P}^6$///," is a generic projection of a rational normal quartic scroll of dimension 4 in ",TEX///$\mathbb{P}^7$///},
           "K" => {{"the coefficient ring (",TT"ZZ/65521"," is used by default)"}}},
Outputs => {SpecialGushelMukaiFourfold => {"a GM fourfold ",TEX///$X$///," containing the surface ",TEX///$\overline{\psi_{B}(S)}\subset\mathbb{G}(1,4)\subset\mathbb{P}^9$///,", where ",TEX///$B$///," is a scroll of the indicated type such that ",TEX///$C\subseteq S\cap B$///," and ",TEX///$\psi_{B}:\mathbb{P}^6\dashrightarrow\mathbb{G}(1,4)$///," is the birational map defined by ",TEX///$B$///}},
PARA {"From the returned fourfold ",TEX///$X$///,", with the following commands we obtain the surface ",TEX///$S$///,", the curve ",TEX///$C$///,", and the scroll ",TEX///$B$///," used in the construction: "},PARA{TT///(B,C) = X.cache#"Construction"; S = ambientVariety C;///},PARA{"Then the surface ",TEX///$\overline{\psi_{B}(S)}\subset\mathbb{G}(1,4)$///," can be constructed with "},PARA{TT///psi = rationalMap B; (psi S)%(image psi);///},
PARA {"In the following example we construct a GM fourfold containing the image via ",TEX///$\psi_B:\mathbb{P}^6\dashrightarrow\mathbb{G}(1,4)$///," of a quintic del Pezzo surface ",TEX///$S\subset\mathbb{P}^5\subset\mathbb{P}^6$///,", obtained as the image of the plane via the linear system of quartic curves with three general simple base points and two general double points, which cuts ",TEX///$B\simeq\mathbb{P}^1\times\mathbb{P}^2\subset\mathbb{P}^5\subset\mathbb{P}^6$///," along a rational normal quartic curve obtained as the image of a general conic passing through the two double points."},
EXAMPLE lines ///X = specialGushelMukaiFourfold([4, 3, 2],[2, 0, 2]);
describe X
(B,C) = X.cache#"Construction";
S = ambientVariety C;
C;
B;
assert(C == S * B)///,
References => UL{{"G. Staglianò, ",EM"Explicit computations with cubic fourfolds, Gushel-Mukai fourfolds, and their associated K3 surfaces",", available at ",HREF{"https://arxiv.org/abs/2204.11518","arXiv:2204.11518"}," (2022)."}},
SeeAlso => {(surface,VisibleList,Ring), GMtables}}

document {Key => {specialFourfold, (specialFourfold, EmbeddedProjectiveVariety), (specialFourfold, EmbeddedProjectiveVariety, EmbeddedProjectiveVariety), (specialFourfold, EmbeddedProjectiveVariety, EmbeddedProjectiveVariety, EmbeddedProjectiveVariety), (specialFourfold, Ideal), (specialFourfold, Ideal, Ideal), (specialFourfold, Ideal, RingElement), (specialFourfold, String), (specialFourfold, String, Ring), [specialFourfold, NumNodes], [specialFourfold, InputCheck]}, 
Headline => "make a Hodge-special fourfold", 
Usage => "specialFourfold(S,X,V)"|newline|"specialFourfold(S,X)"|newline|"specialFourfold S", 
Inputs => {"S" => EmbeddedProjectiveVariety => {"an irreducible ",TO2{(surface,HodgeSpecialFourfold),"surface"}}, "X" => EmbeddedProjectiveVariety => {"a smooth fourfold ", TEX///$X$///, " containing the surface ", TEX///$S$///}, "V" => EmbeddedProjectiveVariety => {"optional, a ",TO2{ambientFivefold,"fivefold"}," where ",TT"X"," is a hypersurface"}}, 
Outputs => {HodgeSpecialFourfold => {"which corresponds to the pair ", TEX///$(S,X)$///}}, 
PARA{"This can also be used as a shortcut for both ",TO specialCubicFourfold," and ",TO specialGushelMukaiFourfold,"."},
EXAMPLE {"S = surface {3,4};", "X = specialFourfold S -- a random cubic fourfold through S", "describe X", "Y = specialFourfold \"tau-quadric\" -- a random GM fourfold through a tau-quadric", "describe Y", "T = surface {3,2};", "Z = specialFourfold T -- a random c. i. of 3 quadrics through T", "describe Z"},
SeeAlso => {specialCubicFourfold, specialGushelMukaiFourfold}}

document {
Key => {Singular, [associatedK3surface, Singular], [associatedCastelnuovoSurface, Singular], [mirrorFourfold, Singular]},
Headline => "whether to transfer computation to Singular",
Usage => "Singular => true or false",
PARA{"This option allows you to transfer part of the computation to Singular."},
SeeAlso => {associatedK3surface}} 

document {Key => {mirrorFourfold, (mirrorFourfold, SpecialCubicFourfold), (mirrorFourfold, SpecialGushelMukaiFourfold), (mirrorFourfold, IntersectionOfThreeQuadricsInP7), (mirrorFourfold, HodgeSpecialFourfold), [mirrorFourfold, Strategy]}, 
Headline => "associated fourfold to a rational cubic or GM fourfold", 
Usage => "mirrorFourfold X", 
Inputs => {"X" => SpecialCubicFourfold => {"or ", ofClass SpecialGushelMukaiFourfold}}, 
Outputs => {HodgeSpecialFourfold => {"the fourfold ",TT"W"," containing the surface ",TT"U",", with the same notation as in the function ",TO associatedK3surface,"."}}, 
EXAMPLE {"X = specialFourfold(PP_(ZZ/65521)[2,2]);", "W = mirrorFourfold X;", "U = surface W;", "mirrorFourfold W", "(building associatedK3surface X)_1", "assert(oo === U)"},
EXAMPLE {"X' = specialFourfold \"tau-quadric\";", "W' = mirrorFourfold X';", "U' = surface W';", "mirrorFourfold W'", "(building associatedK3surface X')_1", "assert(oo === U')"},
SeeAlso => {associatedK3surface}} 

document { 
Key => {(toExternalString, HodgeSpecialFourfold)}, 
Headline => "convert to a readable string", 
Usage => "toExternalString X", 
Inputs => {"X" => HodgeSpecialFourfold}, 
Outputs => {String => {"a string representation of ",TT "X",", which can be used, in conjunction with ",TO "value",", to read the object back into the program later"}},
PARA{"Some of the internal data of the input ",TT"X"," are included in the returned string."},
EXAMPLE {"describe (X = specialFourfold \"tau-quadric\")", "str = toExternalString X;", "describe (value str)"}}

undocumented {(expression, HodgeSpecialFourfold)}

document {Key => {HodgeSpecialFourfold}, 
Headline => "the class of all Hodge-special fourfolds", 
PARA{"An object of the class ", TO HodgeSpecialFourfold, " is basically represented by a couple ", TEX///(S,X)///, ", where ", TEX///$X$///, " is a fourfold and ", TEX///$S$///, " is a surface contained in ", TEX///$X$///,". Such objects are created by the function ",TO specialFourfold,"."},
SeeAlso => {specialFourfold, SpecialCubicFourfold, SpecialGushelMukaiFourfold, IntersectionOfThreeQuadricsInP7}}

document {Key => {(check, ZZ, CongruenceOfCurves), (check, CongruenceOfCurves)}, 
Headline => "check that a congruence of curves is well-defined", 
Usage => "check f"|newline|"check(n,f)",
Inputs => {"n" => ZZ => {"(optional with default value 5)"}, "f" => CongruenceOfCurves},
Outputs => {CongruenceOfCurves => {"the same object passed as input, but an error is thrown if the congruence fails when ",TO2{(symbol SPACE, CongruenceOfCurves, EmbeddedProjectiveVariety),"evaluated"}," on ",TT"n"," random points."}}}

undocumented {(symbol ?, HodgeSpecialFourfold, HodgeSpecialFourfold)}

document {Key => {IntersectionOfThreeQuadricsInP7}, 
Headline => "the class of all special intersection of three quadrics in P^7",
PARA {"This is a subtype of the ",TO HodgeSpecialFourfold," type."},
SeeAlso => {specialFourfold}}

undocumented {(expression, IntersectionOfThreeQuadricsInP7)}

document {Key => {associatedCastelnuovoSurface, (associatedCastelnuovoSurface, IntersectionOfThreeQuadricsInP7), [associatedCastelnuovoSurface, Strategy]},
Headline => "Castelnuovo surface associated to a rational complete intersection of three quadrics in P^7", 
Usage => "U' = associatedCastelnuovoSurface X", 
Inputs => {"X" => IntersectionOfThreeQuadricsInP7 => {"containing a surface ", TEX///$S\subset Y$///," that admits a ",TO2{CongruenceOfCurves,"congruence"}," of ",TEX///$(2e-1)$///,"-secant curves of degree ",TEX///$e$///," inside the ",TO2{ambientFivefold,"ambient fivefold"}," ",TEX///$Y$///," of the fourfold ",TEX///$X$///}}, 
Outputs => {"U'" => {"the (minimal) Castelnuovo surface associated to ",TEX///$X$///}},
Consequences => {{{TT"building U'"," will return the following four objects:"}, UL{{"the dominant ",TO2{MultirationalMap,"rational map"}," ",TEX///$\mu:Y\dashrightarrow W$///," defined by the linear system of hypersurfaces of degree ",TEX///$2e-1$///," having points of multiplicity ",TEX///$e$///," along ",TEX///$S$///,";"}, {"the ",TO2{EmbeddedProjectiveVariety,"surface"}," ",TEX///$U\subset W$///," determining the inverse map of the restriction of ",TEX///$\mu$///," to ",TEX///$X$///,";"}, {"the ",TO2{List,"list"}," of the exceptional curves on the surface ",TEX///$U$///,";"}, {"a ",TO2{MultirationalMap,"rational map"}," of degree 1 from the surface ",TEX///$U$///," to the minimal Castelnuovo surface ",TEX///$U'$///,"."}}}},
PARA {"More details will be provided in a forthcoming paper by F. Russo and G. Staglianò."},
EXAMPLE {"X = specialFourfold random({5:{1}},0_(PP_(ZZ/33331)^7));", "describe X", "time U' = associatedCastelnuovoSurface X;", "(mu,U,C,f) = building U';", "? mu"},
SeeAlso => {associatedK3surface, detectCongruence}}

document {Key => {beauvilleMap, (beauvilleMap, IntersectionOfThreeQuadricsInP7)},
Headline => "construction of Beauville for complete intersections of three quadrics in P^7", 
Usage => "f = beauvilleMap X", 
Inputs => {"X" => IntersectionOfThreeQuadricsInP7}, 
Outputs => {"f" => MultirationalMap => {"a birational map from ",TEX///$X$///," to a fourfold ",TEX///$Y\subset\mathbb{P}^2\times\mathbb{P}^5$///," whose first projection ",TEX///$Y\to\mathbb{P}^2$///," is a quadric surface bundle"}},
PARA{"Smooth intersections of three quadrics in ",TEX///$\mathbb{P}^7$///," are birational to quadric surface bundles over ",TEX///$\mathbb{P}^2$///," with discriminant curve of degree 8. This is a construction of Beauville; see e.g. Proposition 6 in the paper ",HREF{"https://www.intlpress.com/site/pub/files/_fulltext/journals/sdg/2017/0022/0001/SDG-2017-0022-0001-a009.pdf", "Intersections of three quadrics in P7"}, ", by B. Hassett, A. Pirutka, and Y. Tschinkel."},
EXAMPLE {"X = specialFourfold random({5:{1}},0_(PP_(ZZ/33331)^7));", "f = beauvilleMap X;", "Y = target f;", "inverse f;", "first projectionMaps target f;"}}

document {Key => {DoubleSpecialCubicFourfold, (symbol &, EmbeddedProjectiveVariety, EmbeddedProjectiveVariety), surfaces, (surfaces, DoubleSpecialCubicFourfold)},
Headline => "the class of all cubic fourfolds belonging to the intersection of two Hassett divisors",
PARA{"A cubic fourfold ", TEX///$X\subset \mathbb{P}^5$///, " is called ", EM "double special", " if it belongs to the intersection of two Hassett divisors ", TEX///$\mathcal{C}_{d_1} \cap \mathcal{C}_{d_2}$///, ". This means that the algebraic part of its middle cohomology, ", TEX///$H^{2,2}(X, \mathbb{Z})$///, ", has rank at least 3, containing the classes of two surfaces ", TEX///$S$///, " and ", TEX///$T$///, " which, together with the square of the hyperplane class ", TEX///$h^2$///, ", span a saturated lattice of rank 3."},
PARA{"An object of the class ", TO DoubleSpecialCubicFourfold, " can be thought of as a triple ", TEX///$(S,T,X)$///, ", where ", TEX///$X$///, " is a cubic fourfold and ", TEX///$S, T$///, " are two surfaces contained in ", TEX///$X$///, ". Internally, it is represented as a nested structure, where the ", TO2{SpecialCubicFourfold, "cubic fourfold"}, " containing ", TEX///$S$///, " is built upon the ", TO2{SpecialCubicFourfold, "cubic fourfold"}, " containing ", TEX///$T$///, ". This nesting allows the object to inherit the functions and properties of the cubic fourfold containing ", TEX///$S$///, "."},
PARA{"Such an object is constructed using the operator ", TT "&", " to combine the two surfaces. Specifically, if ", TEX///$S$///, " and ", TEX///$T$///, " are two surfaces and ", TEX///$X$///, " is a cubic fourfold containing both, an object is created via the function ", TO specialFourfold, " (or equivalently ", TO specialCubicFourfold, ") as follows:"},
EXAMPLE {"K = ZZ/33331;", "S = PP_K^(2,2); -- the Veronese surface", "T = random({{2},{1},{1}}, sum(3, i -> point S)); -- a 3-secant quadric to S", "X = random(3, S + T);", "X = specialFourfold(S & T, X)", "surface X", "discriminant X"},
PARA{"If the second argument (the cubic fourfold) is omitted, the constructor will automatically provide a random cubic fourfold containing the union ", TEX///$S \cup T$///, ". A summary of the created fourfold ",TEX///$X$///," is provided by the function ", TT "describe", ", while the pair of surfaces ", TEX///$(S,T)$///, " defining the double special structure can be retrieved with the function ", TT "surfaces", "."},
EXAMPLE {"X = specialFourfold(S & T);", "describe X", "surfaces X"},
SeeAlso => {SpecialCubicFourfold, (polarizedK3surface, DoubleSpecialCubicFourfold)}}

document {Key => {polarizedK3surface, (polarizedK3surface, DoubleSpecialCubicFourfold), FanoMapType, [polarizedK3surface, FanoMapType], [polarizedK3surface, Strategy], [polarizedK3surface, Verbose], latticePolarization},
Headline => "polarized K3 surface associated to a double special cubic fourfold",
Usage => "E = polarizedK3surface X",
Inputs => {"X" => DoubleSpecialCubicFourfold => {"whose defining union of surfaces admits a ", TO2{CongruenceOfCurves, "congruence"}, " of curves"}},
Outputs => {"E" => {"the polarized K3 surface associated to ", TEX///$X$///}},
PARA {"At the first call, this function initializes a ", EM "container", " object: it computes the underlying ", TO2{EmbeddedProjectiveVariety, "projective surface"}, " but leaves the lattice polarization data empty to save time."},
PARA {"The full lattice data is computed and stored within the object only upon a second call. Once initialized, ", TO polarize, " can be used as a synonym (using default options)."},
PARA {"The following methods can be used to access the construction data of ", TEX///$E$///, ":"},
UL {{TT "building E", " -- returns the four construction objects as in ", TO (associatedK3surface, SpecialCubicFourfold), " (the Fano rational map ", TEX///$\mu$///, ", the non-minimal K3 surface ", TEX///$U$///, ", the exceptional curves on ", TEX///$U$///, " and the morphism ", TEX///$f: U \to E$///, " contracting them);"}, {TT "projectiveVariety E", " -- returns the underlying ", TO2{EmbeddedProjectiveVariety, "projective surface"}, ";"}, {TT "latticePolarization E", " -- returns the lattice data induced by the double special structure of ", TEX///$X$///, "."}},
PARA {"Optional inputs:"},
UL {{TO "Strategy", " -- provides instructions for each step. The first stage handles the construction of the non-minimal K3 surface ", TEX///$U$///, " by inverting the Fano map restricted to the cubic (via ", TT "\"Inverse\"", " or ", TT "\"Approximate\"", "), while the second computes the lattice data (common options include ", TT "\"MapFromW\"", ", ", TT "\"MapFromU\"", ", or ", TT "\"SpecialCurve\"", "). Both can be passed at once as a sequence, e.g., ", TT "Strategy => (\"Inverse\", \"MapFromW\")", ", or provided individually through nested calls."}, {TO "FanoMapType", " -- sets the Fano map to ", TT "\"Standard\"", " (default) or ", TT "\"P2xP2\"", ". Note that switching this type updates the global behavior of ", TEX///$X$///, " (e.g., affecting methods like ", TO mirrorFourfold, "), though previously computed data is preserved;"}, {TO "Verbose", " -- if set to ", TO true, ", provides feedback during the construction."}},
EXAMPLE {"X = specialFourfold surface((2,0,0),(1,0,0));", "describe X", "polarizedK3surface(X,Verbose=>true)", "polarizedK3surface(X,Verbose=>true)", "describe X"},
SeeAlso => {DoubleSpecialCubicFourfold, (associatedK3surface, SpecialCubicFourfold)}}

undocumented {(expression, DoubleSpecialCubicFourfold), (describe, DoubleSpecialCubicFourfold), (check, DoubleSpecialCubicFourfold), (clean, DoubleSpecialCubicFourfold), (random, DoubleSpecialCubicFourfold), (mirrorFourfold, DoubleSpecialCubicFourfold), (quadricFibration, DoubleSpecialCubicFourfold), (associatedK3surface, DoubleSpecialCubicFourfold)}

------------------------------------------------------------------------
------------------------------- Tests ----------------------------------
------------------------------------------------------------------------

TEST /// -- Test 0 -- cubic fourfolds from strings: describe, discriminant, parameterCount
strIn := {"quintic del Pezzo surface", "quartic scroll", "3-nodal septic scroll", "one-nodal septic del Pezzo surface", "general cubic 4-fold of discriminant 38", "general cubic 4-fold of discriminant 42", "cubic 4-fold of discriminant 48"};
strOut := "Special cubic fourfold of discriminant 14
containing a (smooth) surface of degree 5 and sectional genus 1
cut out by 5 hypersurfaces of degree 2
Special cubic fourfold of discriminant 14
containing a (smooth) surface of degree 4 and sectional genus 0
cut out by 6 hypersurfaces of degree 2
Special cubic fourfold of discriminant 26
containing a 3-nodal surface of degree 7 and sectional genus 0
cut out by 13 hypersurfaces of degree 3
Special cubic fourfold of discriminant 26
containing a 1-nodal surface of degree 7 and sectional genus 1
cut out by 14 hypersurfaces of degree 3
Special cubic fourfold of discriminant 38
containing a (smooth) surface of degree 10 and sectional genus 6
cut out by 10 hypersurfaces of degree 3
Special cubic fourfold of discriminant 42
containing a 5-nodal surface of degree 9 and sectional genus 2
cut out by 9 hypersurfaces of degree 3
Special cubic fourfold of discriminant 48
containing a 6-nodal surface of degree 9 and sectional genus 2
cut out by 5 hypersurfaces of degrees (2,3,3,3,3)
";
X = apply(strIn,specialCubicFourfold);
-- X = apply(strIn,x -> specialCubicFourfold(x,InputCheck=>10,Verbose=>true));
assert all(X,x -> x.cache#?(surface x,"label"));
assert(concatenate apply(X,x -> toString describe x | newline) == strOut);
Y = apply(X,x -> specialCubicFourfold surface x);
assert all(Y,y -> not y.cache#?(surface y,"label"));
assert(apply(Y,discriminant) == {14,14,26,26,38,42,48});
assert(concatenate apply(Y,y -> toString describe y | newline) == strOut);
assert(parameterCount(Y_0,Verbose=>true) == (1, (25, 35, 5)) and parameterCount(Y_1,Verbose=>true) == (1, (28, 29, 2)));
///

TEST /// -- Test 1 (1/2) -- GM fourfolds from strings: describe, discriminant, parameterCount, toGrass
strIn := {"sigma-plane", "rho-plane", "tau-quadric"};
strOut := "Special Gushel-Mukai fourfold of discriminant 10('')
containing a surface in PP^8 of degree 1 and sectional genus 0
cut out by 6 hypersurfaces of degree 1
and with class in G(1,4) given by s_(3,1)
Type: ordinary
(case 6 of Table 1 in arXiv:2002.07026)
Special Gushel-Mukai fourfold of discriminant 12
containing a surface in PP^8 of degree 1 and sectional genus 0
cut out by 6 hypersurfaces of degree 1
and with class in G(1,4) given by s_(2,2)
Type: ordinary
(case 9 of Table 1 in arXiv:2002.07026)
Special Gushel-Mukai fourfold of discriminant 10(')
containing a surface in PP^8 of degree 2 and sectional genus 0
cut out by 6 hypersurfaces of degrees (1,1,1,1,1,2)
and with class in G(1,4) given by s_(3,1)+s_(2,2)
Type: ordinary
(case 1 of Table 1 in arXiv:2002.07026)
";
X = apply(strIn,specialGushelMukaiFourfold);
assert(apply(X,x -> x.cache#(surface x,"label")) == {6, 9, 1});
assert(concatenate apply(X,x -> toString describe x | newline) == strOut);
Y = apply(X,x -> specialGushelMukaiFourfold(sub(ideal (toGrass x) surface x,ring target toGrass x),InputCheck=>0))
assert all(Y,y -> not y.cache#?(surface y,"label"));
assert(apply(Y,discriminant) == {10, 12, 10});
assert(concatenate apply(Y,y -> toString describe y | newline) == strOut);
assert(parameterCount(Y_0,Verbose=>true) == (2, (34, 4, 0)) and parameterCount(Y_1,Verbose=>true) == (3, (34, 3, 0)));
///

TEST /// -- Test 2 (2/2) -- GM fourfolds from strings: describe, discriminant, parameterCount, toGrass
strIn := {"cubic scroll", "quintic del Pezzo surface", "general GM 4-fold of discriminant 20"};
strOut := "Special Gushel-Mukai fourfold of discriminant 12
containing a surface in PP^8 of degree 3 and sectional genus 0
cut out by 7 hypersurfaces of degrees (1,1,1,1,2,2,2)
and with class in G(1,4) given by 2*s_(3,1)+s_(2,2)
Type: ordinary
(case 7 of Table 1 in arXiv:2002.07026)
Special Gushel-Mukai fourfold of discriminant 10('')
containing a surface in PP^8 of degree 5 and sectional genus 1
cut out by 8 hypersurfaces of degrees (1,1,1,2,2,2,2,2)
and with class in G(1,4) given by 3*s_(3,1)+2*s_(2,2)
Type: ordinary
(case 4 of Table 1 in arXiv:2002.07026)
Special Gushel-Mukai fourfold of discriminant 20
containing a surface in PP^8 of degree 9 and sectional genus 2
cut out by 19 hypersurfaces of degree 2
and with class in G(1,4) given by 6*s_(3,1)+3*s_(2,2)
Type: ordinary
(case 17 of Table 1 in arXiv:2002.07026)
";
X = apply(strIn,x -> clean specialGushelMukaiFourfold x);
debug SpecialFanoFourfolds;
assert(apply(X,recognize) == {7, 4, 17});
assert(concatenate apply(X,x -> toString describe x | newline) == strOut);
Y = apply(X,x -> specialGushelMukaiFourfold(sub(ideal (toGrass x) surface x,ring target toGrass x),InputCheck=>0))
assert all(Y,y -> not y.cache#?(surface y,"label"));
assert(apply(Y,discriminant) == {12, 10, 20});
assert(concatenate apply(Y,y -> toString describe y | newline) == strOut);
assert(parameterCount(Y_1,Verbose=>true) == (1, (24, 18, 3)));
///

TEST /// -- Test 3 -- 21 examples from GMtables
X = for i from 1 to 21 list (
   A = GMtables(i,ZZ/65521);
   time specialGushelMukaiFourfold((rationalMap(ideal A_0,Dominant=>2)) ideal A_1,InputCheck=>0)
); 
S = apply(X,x -> surface x);
assert(apply(X,x -> degree surface x) === {2, 4, 14, 5, 9, 1, 3, 7, 1, 10, 10, 14, 12, 8, 9, 11, 9, 7, 10, 4, 12});
assert(apply(X,x-> sectionalGenus surface x) == {0, 0, 8, 1, 3, 0, 0, 2, 0, 4, 3, 8, 5, 2, 3, 5, 2, 0, 3, 0, 5});
assert(last cycleClass X_18 == (6,4) and discriminant X_18 == 24);
assert(last cycleClass X_7 == (4,3) and discriminant X_7 == 12);
///

TEST /// -- Test 4 -- parametrizations of Fano fourfolds
setRandomSeed 0;
for dg in {(2,0),(3,1),(4,1),(5,1),(4,3),(6,4),(8,5),(10,6),(12,7),(14,8),(16,9),(18,10)} do (
    <<"(d,g) = "<<dg<<endl;
    X = fanoFourfold dg;
    assert(dim X == 4 and degree X == dg_0 and (genera ideal X)_3 == dg_1);
    if member(dg,{(2,0),(4,1),(5,1),(16,9)}) then (        
        time f = parametrizeFanoFourfold X;        
        assert(source f == ambient source f and dim source f == 4);
        assert(target f === X);
        g = f#"inverse";        
        assert(g =!= null);
        p = point source f;
        assert(g f p == p);
    );   
);
///

TEST /// -- Test 5 -- rational and unirational parametrizations
X = specialCubicFourfold surface({3,4},ZZ/333331);
time h = parametrize X;
assert(degree(h,Strategy=>"random point") == 1 and target h === X and ambient source h == source h and h#"inverse" =!= null);
time f = unirationalParametrization X;
assert(# factor f == 1 and target f === X and unique degrees ideal matrix first factor f == {{10}});
assert isSubset(f point source f,X);
S = schubertCycle({3,1},GG(ZZ/33331,1,4),Standard=>true);
Y = specialGushelMukaiFourfold S;
time g = parametrize Y;
assert(degree(g,Strategy=>"random point") == 1 and target g === Y and dim ambient source g == 5 and dim source g == 4 and g#"inverse" =!= null);
-- time g = unirationalParametrization Y;
-- assert(# factor g == 1 and target g === Y and unique degrees ideal matrix first factor g == {{26}})
-- assert isSubset(g point source g,Y)
///

TEST /// -- Test 6 (1/3) -- associated K3 surfaces
f = last building associatedK3surface(specialCubicFourfold "quartic scroll",Verbose=>false);
assert(f#"image" =!= null and dim image f == 2 and degree image f == 14 and dim target f == 8)
///

TEST /// -- Test 7 (2/3) -- associated K3 surfaces
g = last building associatedK3surface(specialCubicFourfold "quintic del Pezzo surface",Verbose=>true,Singular=>false);
assert(g#"image" =!= null and dim image g == 2 and degree image g == 14 and dim target g == 8)
///

TEST /// -- Test 8 (3/3) -- associated K3 surfaces
building associatedK3surface(specialGushelMukaiFourfold "tau-quadric",Verbose=>false);
///

TEST /// -- Test 9 -- simple tests on schubertCycle
debug MultiprojectiveVarieties;
S = schubertCycle({2,2},GG(ZZ/33331,1,4),Standard=>true)
assert(idealOfSubvariety S == idealOfSubvariety tangentialChowForm(projectiveVariety ideal((Grass(0,4,ZZ/33331,Variable=>"x"))_0,(Grass(0,4,ZZ/33331,Variable=>"x"))_1),1,1))
S = schubertCycle({3,2,1},GG(ZZ/33331,2,5),Standard=>true)
use ring ambientVariety S;
assert(idealOfSubvariety S == ideal(x_(0,4,5),x_(0,3,5),x_(1,2,5),x_(0,2,5),x_(0,1,5),x_(2,3,4),x_(1,3,4),x_(0,3,4),x_(1,2,4),x_(0,2,4),x_(0,1,4),x_(1,2,3),x_(0,2,3),x_(0,1,3),x_(0,1,2)))
///

TEST /// -- Test 10 (1/2) -- detectCongruence
X = specialCubicFourfold("quintic del Pezzo surface",ZZ/33331);
detectCongruence(X,Verbose=>true);
///

TEST /// -- Test 11 (2/2) -- detectCongruence
use Grass(1,4,ZZ/33331);
S31 = ideal(p_(3,4),p_(2,4),p_(1,4),p_(0,4),p_(2,3),p_(1,3),p_(1,2));
Y = specialGushelMukaiFourfold(S31,InputCheck=>0);
assert(not Y.cache#?(surface Y,"label")); Y.cache#(surface Y,"label") = 6;
detectCongruence(Y,Verbose=>true);
-- Y = specialGushelMukaiFourfold("18",ZZ/3331);
-- detectCongruence Y;
///

TEST /// -- Test 12 (1/2) -- GM fourfolds containing nodal surfaces
debug SpecialFanoFourfolds;
K = ZZ/65521;
X = makeGMfromCurveOnSurfaceInP6((surface({2,0,0,0},K,ambient=>6)).cache#"takeCurve" (1,(0,0,0)),InputCheck=>0);
assert(discriminant X == 36);
assert(numberNodes surface X == 1);
X' = random X;
assert(surface X === surface X' and ambientFivefold X === ambientFivefold X' and isSubset(surface X',X') and dim(X*X') == 3);
assert(discriminant X' == 44 and discriminant X == 44);
///

TEST /// -- Test 13 (2/2) -- GM fourfolds containing nodal surfaces
X = specialGushelMukaiFourfold("nodal surface of degree 11 and genus 3 with class (7,4)",ZZ/33331,InputCheck=>0);
assert(discriminant X == 26 and last cycleClass X == (7,4) and degree surface X == 11 and sectionalGenus surface X == 3);
Y = specialGushelMukaiFourfold("nodal D44",ZZ/33331,InputCheck=>0);
assert(discriminant Y == 44 and last cycleClass Y == (6,3) and degree surface Y == 9 and sectionalGenus surface Y == 1);
///

TEST /// -- Test 14 -- gluing scrolls along curves
debug SpecialFanoFourfolds
S = surface({3,4,0,0},ambient=>6);
for a in {(1,0),(2,0),(3,0),(4,0),(5,0),(5,1)} do (
    (d,g) := a;
    E := curvesOnSurface(S,d,g);
    assert(#E>0);
    for C in E do (
        <<"(d,g) = "<<(d,g)<<", curve: "<<? ideal C<<endl;
        assert(degree C == d and sectionalGenus C == g);
        B := glueScroll C;
        assert(dim B == 3 and degree B == 3 and degrees B == {({1},1),({2},3)} and isSubset(C,S*B));
        if g == 0 then (
            B' := glueScroll' C;
            assert(dim B' == 4 and degree B' == 4 and degrees B' == {({2},1),({3},3)} and isSubset(C,B'*S));
        );    
    );
);
C = first curvesOnSurface(surface({3,3,0,0},ZZ/333331),6,0);
assert(dim C == 1 and degree C == 6 and sectionalGenus C == 0)
B = glueScroll' C;
assert(dim B == 4 and degree B == 4 and degrees B == {({2},1), ({3},3)} and isSubset(C,B) and isSubset(C,(ambientVariety C)*B))
///

TEST /// -- Test 15
debug SpecialFanoFourfolds
L = takeGMsfromSurfaceInP6(surface({3,1,1,0},ambient=>6),InputCheck=>0,"Gluing"=>"cubic scroll",Degrees=>hashTable{1=>(1,1),2=>(19,infinity),3=>(0,0)});
X = first L;
assert(#L == 1 and discriminant X == 18 and last cycleClass X == (5,3))
-- L = takeGMsfromSurfaceInP6(surface({3,1,1,0},ambient=>6),InputCheck=>0,"Gluing"=>"quartic scroll",Degrees=>hashTable{1=>(1,1),2=>(19,infinity),3=>(0,0)});
-- X = first L;
-- assert(#L == 1 and discriminant X == 20 and last cycleClass X == (4,3))
///

TEST /// -- Test 16
debug SpecialFanoFourfolds;
S = surface({3,1},NumNodes=>2);
assert(dim S == 2 and degree S == 8 and dim ambient S == 6 and degrees S == {({2},5),({3},4)});
T = image experimentalNormalizationInv S;
assert(dim T == 2 and degree T == 8 and dim ambient T == 8 and degrees T == {({2},20)})
///

TEST /// -- Test 17
X = specialFourfold "plane in PP^7";
assert(discriminant X == 31);
S = associatedCastelnuovoSurface X;
assert((dim S,dim ambient S,degree S,sectionalGenus S,degrees S) == (2, 4, 9, 9, {({3}, 1), ({4}, 3)}))
///

TEST /// -- Test 18
debug MultiprojectiveVarieties;
K := ZZ/65521;
P := PP_K(1,1,1,4);
C = random({{2},{3}},0_P)
assert(discriminant(surface(C,random(8,C)),Algorithm=>2) == -7)
D = internalProjection_2 (surface({3, 1, 1, 0},K)).cache#"takeCurve"(3,{0, 0, 0});
x := gens ring P;
f := (rationalMap {{x_0^4,x_0^3*x_1,x_0^3*x_2,x_3}}) << (ambient D);
E = f^* D;
assert(discriminant(surface(E,random(8,E)),Algorithm=>1) == -43)
E' = f^* random D;
assert(discriminant(surface(E',random(8,E')),Algorithm=>2) == -43)
///

TEST /// -- test 19 DSCF
-- generateInputsForRunExampleTest 6
debug SpecialFanoFourfolds;
runExampleTest(6,64997,4,0,20,6,4,2,4,1,0,0,0,4,matrix {{6, 5}, {5, -2}})
///

TEST /// -- test 20 DSCF
debug SpecialFanoFourfolds;
(B,V,C) = GMtables(1,ZZ/65521);
assert(B * V == C and dim C == 1)
X = specialFourfold(B & V);
assert(surfaceIntersectionNumber X == 1)
assert(latticeIntersectionMatrix3x3 X == matrix {{3, 3, 2}, {3, 7, 1}, {2, 1, 4}})
///

TEST /// -- test 21 DSCF
debug SpecialFanoFourfolds;
debug MultiprojectiveVarieties;
S = nodalProjection surface {3,3};
n = inverseNormalizationMapRaw(S,"S",1,true);
assert(dim target n == 6 and dim image n == 2 and degree image n == 6 and euler hilbertPolynomial image n == 1)
S' = nodalProjection surface {3,1};
n' = inverseNormalizationMapExperimentalRaw(S',"S'",1,true)
assert(dim target n' == 8 and dim image n' == 2 and degree image n' == 8 and euler hilbertPolynomial image n' == 1)
///

TEST /// -- test 22 DSCF
debug SpecialFanoFourfolds;
L = discoverCubicFourfoldsInC8({4,4,3,2,1},1,4,5,7,"test_file",true,300,{});
assert(apply(L,discriminant) == {14, 20, 24, 32, 38})
assert(fileExists "test_file.txt" and fileExists "test_file_commands.m2")
removeFile "test_file.txt"
removeFile "test_file_commands.m2"
///

TEST /// -- test 23 DSCF -- switch between Fano map types
debug SpecialFanoFourfolds;
X = specialFourfold("DSCF-"|(toString 6))
E = polarizedK3surface(X,Verbose=>true,Strategy=>("Approximate","MapFromU-Virtual"),FanoMapType=>"P2xP2")
assert(statusK3 X == 2);
(mu,U,LC,f) = building E;
assert(U === surface mirrorFourfold X);
assert(mirrorFourfold mirrorFourfold X === X);
assert(degree U == 19 and sectionalGenus U == 15 and euler hilbertPolynomial U == 4);
W = target mu;
assert(W == mirrorFourfold X);
assert isDeformationP2P2 mirrorFourfold X
assert(isFanoMapToP2xP2 X and fanoMapDSCF X === mu);
assert(E#"LatticePolarization" === null);
polarizedK3surface(X,Verbose=>true);
assert(E#"LatticePolarization" =!= null);
assert(statusK3 X == 2);
assert(isVirtualLatticeK3 E);
assert instance(latticePolarization E, LatticePolarizationOnK3Surface);
assert(latticeMatrix latticePolarization E == matrix {{3, 7}, {7, 2}});
assert(recoverFourfold E === X and recoverFourfold projectiveVariety E === X);
assert("Approximate" === U.cache#"strategy for surface U");
assert(not U.cache#?"birational maps from X to W and from W to X");
E' = polarizedK3surface(X,Verbose=>true,Strategy=>("Inverse","MapFromW-Virtual"),FanoMapType=>"Standard")
W' = mirrorFourfold X;
U' = (building E')_1;
assert(not isDeformationP2P2 W');
assert(isFanoMapStandard X and fanoMapDSCF X === first building E');
assert(E'#"LatticePolarization" === null);
polarizedK3surface(X,Verbose=>true);
assert(E'#"LatticePolarization" =!= null);
assert(statusK3 X == 3);
assert(isVirtualLatticeK3 E');
assert instance(latticePolarization E', LatticePolarizationOnK3Surface);
assert(latticeMatrix latticePolarization E' ==  matrix {{6, 12}, {12, 2}});
assert(recoverFourfold E' === X and recoverFourfold projectiveVariety E' === X);
assert("Inverse" === U'.cache#"strategy for surface U");
assert(U'.cache#?"birational maps from X to W and from W to X");
E'' = polarizedK3surface polarizedK3surface(X,Verbose=>true,Strategy=>"Approximate",FanoMapType=>"P2xP2")
assert(isDeformationP2P2 mirrorFourfold X);
assert(latticeMatrix latticePolarization E'' == matrix {{3, 7}, {7, 2}});
///

TEST /// -- test 24 DSCF
debug SpecialFanoFourfolds;
for i from 1 to 40 do assert(recognizeDSCF specialFourfold("DSCF-"|(toString i)) === "DSCF-V1-"|toString(i))
///

TEST /// -- test 25 DSCF -- Tregub
debug SpecialFanoFourfolds;
K = ZZ/65521;
S = PP_K^(2,2);
P = linearSpan {point S,point S,point S};
X = specialFourfold(S & P);
describe X
assert(latticeIntersectionMatrix3x3 X == matrix {{3, 4, 1}, {4, 12, 3}, {1, 3, 3}})
Utilde = polarizedK3surface(X,Strategy=>"Approximate",Verbose=>true)
assert(X.cache#(first surfaces X,last surfaces X,"labelDSCF") === "Tregub1")
assert(recognizeDSCF X === "Tregub1")
(mu,U,C,f) = building Utilde;
assert(degree U == 18 and sectionalGenus U == 10 and euler hilbertPolynomial U == -1)
///

TEST /// -- test 26 DSCF
-- generateInputsForRunExampleTest 33
debug SpecialFanoFourfolds;
runExampleTest(33,42169,8,1,56,13,8,2,7,7,3,1,0,8,matrix {{14, 9}, {9, 2}})
///

TEST /// -- test 27 DSCF
-- generateInputsForRunExampleTest 21
debug SpecialFanoFourfolds;
runExampleTest(21,38113,7,1,38,16,10,2,8,8,3,2,0,10,matrix {{18, 9}, {9, 2}})
///
