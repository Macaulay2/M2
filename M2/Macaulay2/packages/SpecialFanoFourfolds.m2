
-*
   Copyright 2020, Giovanni Staglianò.

   You may redistribute this file under the terms of the GNU General Public
   License as published by the Free Software Foundation, either version 2 of
   the License, or any later version.
*-

newPackage(
    "SpecialFanoFourfolds",
    Version => "2.2", 
    Date => "June 9, 2021",
    Authors => {{Name => "Giovanni Staglianò", Email => "giovannistagliano@gmail.com" }},
    Headline => "special cubic fourfolds and special Gushel-Mukai fourfolds",
    Keywords => {"Algebraic Geometry"},
    PackageImports => {"PrimaryDecomposition"},
    PackageExports => {"MultiprojectiveVarieties"},
    DebuggingMode => false,
    Reload => false
)

if MultiprojectiveVarieties.Options.Version < "2.2" then (
    <<endl<<"Your version of the MultiprojectiveVarieties package is outdated (required version 2.2 or newer);"<<endl;
    <<"you can manually download the latest version from"<<endl;
    <<"https://github.com/Macaulay2/M2/tree/master/M2/Macaulay2/packages."<<endl;
    <<"To automatically download the latest version of MultiprojectiveVarieties in your current directory,"<<endl;
    <<"you may run the following Macaulay2 code:"<<endl<<"***"<<endl<<endl;
    <<///run "curl -s -o MultiprojectiveVarieties.m2 https://raw.githubusercontent.com/Macaulay2/M2/development/M2/Macaulay2/packages/MultiprojectiveVarieties.m2";///<<endl<<endl<<"***"<<endl;
    error "required MultiprojectiveVarieties package version 2.2 or newer";
);

export{
   "SpecialGushelMukaiFourfold",
   "specialGushelMukaiFourfold",
   "schubertCycle",
   "cycleClass",
   "toGrass",
   "SpecialCubicFourfold",
   "specialCubicFourfold",
   "NumNodes",
   "parameterCount",
   "normalSheaf",
   "isAdmissible",
   "isAdmissibleGM",
   "detectCongruence",
   "surface",
   "GMtables",
   "unirationalParametrization",
   "grassmannianHull",
   "InputCheck",
   "associatedK3surface",
   "fanoFourfold",
   "parametrizeFanoFourfold"
}

needsPackage "IntegralClosure"; -- for method: normalization
needsPackage "CharacteristicClasses"; -- for method: eulerCharacteristic
needsPackage("RationalMaps",DebuggingMode=>false); -- for method: inverse3

importFrom("Cremona",{"secantCone"})
debug SparseResultants
debug MultiprojectiveVarieties
exportFrom(MultiprojectiveVarieties,{"coneOfLines"})

------------------------------------------------------------------------
--------------------------- Cubic fourfolds ----------------------------
------------------------------------------------------------------------

SpecialCubicFourfold = new Type of EmbeddedProjectiveVariety;

globalAssignment SpecialCubicFourfold;

SpecialCubicFourfold.synonym = "special cubic fourfold";

specialCubicFourfold = method(TypicalValue => SpecialCubicFourfold, Options => {NumNodes => null, InputCheck => 1, Verbose => true});

specialCubicFourfold (EmbeddedProjectiveVariety,EmbeddedProjectiveVariety) := o -> (S,X) -> (
    if ring ideal S =!= ring ideal X then error "expected varieties in the same ambient space";
    if not (dim ambient X == 5 and degrees X == {({3},1)}) then error "expected a cubic fourfold";
    if dim S != 2 then error "expected a surface";
    i := o.InputCheck;
    if not (instance(i,ZZ) and i >= -1) then error("option InputCheck expects a nonnegative integer:"|newline|"0: no check is done about the smoothness of the fourfold and of the (normalization of the) surface"|newline|"1: just the smoothness of the fourfold is checked"|newline|"2: the smoothness of the fourfold and of a general hyperplane section of the surface are checked"|newline|"3: as above and furthermore the smoothness of the normalization of the surface is checked");
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
    Fourfold#"SurfaceContainedInTheFourfold" = S;
    Fourfold
);

specialCubicFourfold (Ideal,Ideal) := o -> (idS,idX) -> specialCubicFourfold(projectiveVariety idS,projectiveVariety idX,NumNodes=>o.NumNodes,InputCheck=>o.InputCheck,Verbose=>o.Verbose);

specialCubicFourfold (Ideal,RingElement) := o -> (idS,C) -> specialCubicFourfold(idS,ideal C,NumNodes=>o.NumNodes,InputCheck=>o.InputCheck,Verbose=>o.Verbose);

specialCubicFourfold EmbeddedProjectiveVariety := o -> S -> (
    if not(dim ambient S == 5 and dim S == 2) then error "expected a surface in P^5";
    specialCubicFourfold(S,random({3},S),NumNodes=>o.NumNodes,InputCheck=>o.InputCheck,Verbose=>o.Verbose)
);

specialCubicFourfold Ideal := o -> idS -> specialCubicFourfold(projectiveVariety idS,NumNodes=>o.NumNodes,InputCheck=>o.InputCheck,Verbose=>o.Verbose);

specialCubicFourfold (String,Ring) := o -> (str,K) -> (
    local X;
    if str === "very general" then (
        X = specialCubicFourfold(random({{1},{1},{3}},0_(PP_K^5)),NumNodes=>0,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        X.cache#"label" = "very general";
        return X;
    );
    if str === "quintic del Pezzo surface" then (
        X = specialCubicFourfold(Var image rationalMap(ring PP_K^2,{3,4}),NumNodes=>0,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        X.cache#"label" = "quinticDelPezzoSurface";
        return X;
    );
    if str === "quartic scroll" then (
        X = specialCubicFourfold(Var image rationalMap(ring PP_K^2,{3,1,1}),NumNodes=>0,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        X.cache#"label" = "quarticScrollSurface";
        return X;
    );
    if str === "C38" then (
        X = specialCubicFourfold(Var image rationalMap(ring PP_K^2,{10,0,0,10}),NumNodes=>0,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        X.cache#"label" = "C38Coble";
        return X;
    );
    if str === "Farkas-Verra C26" then (
        t := gens ring PP_K^2;
        f := multirationalMap rationalMap(ring PP_K^2,ring PP_K^8,{t_0^5, t_0^4*t_1, t_0^3*t_1^2, t_0^2*t_1^3, t_0^4*t_2, t_0^3*t_1*t_2, t_0^2*t_1^2*t_2, t_0*t_1^3*t_2, t_1^4*t_2});
        f = f * rationalMap linearSpan apply(3,i -> point linearSpan {f point source f,f point source f});
        X = specialCubicFourfold(image f,NumNodes=>3,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        X.cache#"label" = "FarkasVerra";
        return X;
    );  
   if str === "one-nodal septic del Pezzo surface" then (
       g := multirationalMap rationalMap(ring PP_K^2,{3,2});
       g = g * rationalMap(ring target g,ring PP_K^5,gens ideal linearSpan {point target g,point linearSpan {g point source g,g point source g}});
       X = specialCubicFourfold(image g,NumNodes=>1,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
       X.cache#"label" = "oneNodalSepticDelPezzoSurfaceC26";
       return X;
   );
   if str === "C42" then (
       X = specialCubicFourfold(last last randomS42data(K),NumNodes=>5,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
       X.cache#"label" = "C42";
       return X;
   );
   if str === "C48" then (
       X = specialCubicFourfold(randomS48 K,NumNodes=>6,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
       X.cache#"label" = "C48";
       return X;
   );
   if str === "C32" then (
        X = specialCubicFourfold(Var image rationalMap(ring PP_K^2,{9,1,4,6}),NumNodes=>0,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        X.cache#"label" = "C32";
        return X;
   );
   if str === "C44" then ( -- Enriques surface (see e.g. https://arxiv.org/pdf/1210.1903.pdf, p. 7)
        J := Var ideal jacobian ideal discriminant first genericPolynomials({2,-1,-1,-1},K);
        X = specialCubicFourfold((parametrize random({{1},{1},{1},{1}},0_J))^* J,NumNodes=>0,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        X.cache#"label" = "C44";
        return X;
   );
   error "not valid string, permitted strings are: \"quintic del Pezzo surface\", \"quartic scroll\", \"Farkas-Verra C26\", \"one-nodal septic del Pezzo surface\", \"C32\", \"C38\", \"C42\", \"C44\", \"C48\"";
);

specialCubicFourfold String := o -> str -> specialCubicFourfold(str,ZZ/65521,NumNodes=>o.NumNodes,InputCheck=>o.InputCheck,Verbose=>o.Verbose);

surface = method(TypicalValue => EmbeddedProjectiveVariety);
surface SpecialCubicFourfold := X -> X#"SurfaceContainedInTheFourfold";

expression SpecialCubicFourfold := X -> expression("cubic fourfold containing a surface of degree "|toString(degree surface X)|" and sectional genus "|toString(sectionalGenus surface X));

describe SpecialCubicFourfold := X -> (
    S := surface X;
    d := degree S; g := sectionalGenus S; chiOS := eulerHilbertPol S;
    degs := flatten degrees ideal S;
    discrX := discriminant X;
    descr:="Special cubic fourfold of discriminant "|toString(discrX)|newline|"containing a ";
    n := numberNodes surface X;
    descr = descr|(if n > 0 then toString(n)|"-nodal " else "(smooth) ");
    descr = descr|"surface of degree "|toString(d)|" and sectional genus "|toString(g)|newline;
    descr = descr|(if # unique degs == 1 then "cut out by "|toString(#degs)|" hypersurfaces of degree "|toString(first degs) else "cut out by "|toString(#degs)|" hypersurfaces of degrees "|toString(toSequence degs));
    recognize X;
    net expression descr
);

map SpecialCubicFourfold := o -> X -> (
    if X.cache#?"AssociatedMap" then return X.cache#"AssociatedMap";
    X.cache#"AssociatedMap" = rationalMap(ideal surface X,3)
);

recognize = method(); -- try to recognize

recognize SpecialCubicFourfold := (cacheValue "label") (X -> (
    S := surface X;
    d := discriminant X;
    e := eulerCharacteristic S;
    n := numberNodes surface X;
    invS := (degree S,sectionalGenus S,eulerHilbertPol S);
    degs := flatten degrees ideal S;
    if (d == 14 and e == 7 and n == 0 and invS === (5,1,1) and degs == toList(5:2)) then return "quinticDelPezzoSurface";
    if (d == 14 and e == 4 and n == 0 and invS === (4,0,1) and degs == toList(6:2)) then return "quarticScrollSurface";
    if (d == 32 and e == 14 and n == 0 and invS === (10,6,1) and degs == toList(10:3)) then return "C32";
    if (d == 38 and e == 13 and n == 0 and invS === (10,6,1) and degs == toList(10:3)) then return "C38Coble";
    if (d == 44 and e == 12 and n == 0 and invS === (10,6,1) and degs == toList(10:3)) then return "C44";
    if (d == 26 and e == -14 and n == 3 and invS === (7,0,-2) and degs == toList(13:3)) then return "FarkasVerra";
    if (d == 26 and e == -1 and n == 1 and invS === (7,1,0) and degs == toList(14:3)) then return "oneNodalSepticDelPezzoSurfaceC26";
    if (d == 42 and e == -23 and n == 5 and invS === (9,2,-4) and degs == toList(9:3)) then return "C42";
    if (d == 48 and e == -29 and n == 6 and invS === (9,2,-5) and degs == {2,3,3,3,3}) then return "C48";
    "NotRecognized"
));

fanoMap = method();

fanoMap SpecialCubicFourfold := (cacheValue "fanoMap") (X -> (
    recognize X;
    S := ideal surface X;
    local mu;
    if X.cache#"label" === "quinticDelPezzoSurface" then (
        mu = rationalMap S;
        forceImage(mu,ideal(0_(target mu)));
        return mu;
    );
    if X.cache#"label" === "quarticScrollSurface" then (
        mu = rationalMap(S,Dominant=>2);
        forceImage(mu,ideal(0_(target mu)));
        return mu;
    );
    if X.cache#"label" === "C38Coble" or X.cache#"label" === "FarkasVerra" then (
        mu = rationalMap(S,5,2);
        forceImage(mu,ideal(0_(target mu)));
        return mu;
    );
    if X.cache#"label" === "oneNodalSepticDelPezzoSurfaceC26" then (
        mu = rationalMap(S,5,2);
        interpoleImage(mu,{2,2,2,2,2},2);
        mu = rationalMap(mu,Dominant=>true);
        return mu;
    );
    if X.cache#"label" === "C42" then (
        mu = rationalMap(S^3 : ideal first gens ring S,8);
        interpoleImage(mu,{2,2,2,2,2},2);
        mu = rationalMap(mu,Dominant=>true);
        return mu;
    );
    error "not implemented yet: fourfold not recognized yet or not rational";
));

parametrize SpecialCubicFourfold := X -> (
    if X.cache#?"rationalParametrization" then return X.cache#"rationalParametrization";
    Psi := fanoMap X;
    X.cache#"rationalParametrization" = inverse3(Psi|X)
);

associatedK3surface = method(Options => {Verbose => false});

associatedK3surface SpecialCubicFourfold := o -> X -> (
    recognize X;
    if not isAdmissible X then error "expected an admissible cubic fourfold";
    S := ideal surface X; I := ideal X;
    ch := char coefficientRing X;
    local mu; local I2; local U; local U2; local P; local exceptionalLines; local exceptionalConics; local exceptionalQuarticCurve; local f;
    if X.cache#"label" === "quinticDelPezzoSurface" then (
        if o.Verbose then <<"-- computing the map mu from P^5 to P^4 defined by the quadrics through the surface S_14"<<endl;
        mu = fanoMap X;
        if o.Verbose then <<"-- computing the surface U corresponding to the fourfold X"<<endl;
        U = ideal matrix toRationalMap parametrize X;
        I2 = arandom({3},S);
        if o.Verbose then <<"-- computing the surface U' corresponding to another fourfold X'"<<endl;
        U2 = ideal matrix inverse3(mu|I2);
        if o.Verbose then <<"-- computing the 5 exceptional lines on U and U'"<<endl;
        exceptionalLines = decompose top trim(U+U2);
        if o.Verbose then <<"-- computing the map f from U to the minimal K3 surface of degree 14"<<endl;
        f = mapDefinedByDivisor(quotient U,{(arandom({1},ring U),1)}|apply(exceptionalLines,l->(l,1)));
        if numgens target f != 8+1 then error "something went wrong on the target of the map defined by the divisor";
        if o.Verbose then <<"-- computing the image of f"<<endl;
        image f;
        return (multirationalMap mu,Var U,Var exceptionalLines,multirationalMap f);
    );
    if X.cache#"label" === "quarticScrollSurface" then (
        if o.Verbose then <<"-- computing the map mu from P^5 to P^5 defined by the quadrics through the surface S_14"<<endl;
        mu = fanoMap X;
        if o.Verbose then <<"-- computing the surface U corresponding to the fourfold X"<<endl;
        U = trim lift(ideal matrix toRationalMap parametrize X,ambient target mu);
        I2 = arandom({3},S);
        if o.Verbose then <<"-- computing the surface U' corresponding to another fourfold X'"<<endl;
        U2 = trim lift(ideal matrix inverse3(mu|I2),ambient target mu);
        if o.Verbose then <<"-- computing the exceptional conic on U and U'"<<endl;
        exceptionalConics = {top trim(U+U2)};
        if o.Verbose then <<"-- computing the map f from U to the minimal K3 surface of degree 14"<<endl;
        f = mapDefinedByDivisor(quotient U,{(ideal first gens ring U,1),(first exceptionalConics,2)});
        if numgens target f != 8+1 then error "something went wrong on the target of the map defined by the divisor";
        if o.Verbose then <<"-- computing the image of f"<<endl;
        image f;
        return (multirationalMap rationalMap mu,Var U,Var exceptionalConics,multirationalMap f);
    );
    if X.cache#"label" === "C38Coble" then (
        if o.Verbose then <<"-- computing the map mu from P^5 to P^4 defined by the quintic hypersurfaces"<<endl;
        if o.Verbose then <<"   with points of multiplicity 2 along the surface S_38"<<endl;
        mu = fanoMap X;
        if o.Verbose then  <<"-- computing the surface U corresponding to the fourfold X"<<endl;
        U = trim ideal apply(9,j -> if ch <= 65521 then image(mu|(I + ideal arandom S),"F4") else interpoleImage(mu,I + ideal arandom S,{5},5));
        I2 = ideal arandom S;
        if o.Verbose then <<"-- computing the surface U' corresponding to another fourfold X'"<<endl;
        U2 = trim ideal apply(9,j -> if ch <= 65521 then image(mu|(I2 + ideal arandom S),"F4") else interpoleImage(mu,I2 + ideal arandom S,{5},5));
        if o.Verbose then <<"-- computing the 10 exceptional lines on U and U'"<<endl;
        P = ideal 1; while dim P != 1 or degree P != 10 do P = plucker(trim(U+U2),1); 
        exceptionalLines = apply(decompose trim lift(P,ambient ring P),l -> sub(plucker sub(l,ring P),vars ring U));
        if o.Verbose then <<"-- computing the exceptional quartic curve on U and U'"<<endl;
        exceptionalQuarticCurve = U+U2; 
        for L in exceptionalLines do exceptionalQuarticCurve = quotient(exceptionalQuarticCurve,L); 
        exceptionalQuarticCurve = top exceptionalQuarticCurve;
        if o.Verbose then <<"-- computing the map f from U to the minimal K3 surface of degree 38"<<endl;
        f = mapDefinedByDivisor(quotient U,{(ideal first gens ring U,1)}|apply(exceptionalLines,l->(l,1))|{(exceptionalQuarticCurve,4)});
        if numgens target f != 20+1 then error "something went wrong on the target of the map defined by the divisor";
        if o.Verbose then <<"-- computing the image of f"<<endl;
        if ch <= 65521 then image(f,"F4") else interpoleImage(f,toList(153:2),2);
        return (multirationalMap mu,Var U,Var append(exceptionalLines,exceptionalQuarticCurve),multirationalMap f);
    );
    if X.cache#"label" === "C42" then (
        if o.Verbose then <<"-- computing the map mu from P^5 to P^7 defined by the octic hypersurfaces"<<endl;
        if o.Verbose then <<"   with points of multiplicity 3 along the surface S_42"<<endl;
        mu = rationalMap fanoMap X;
        if o.Verbose then <<"-- computing the surface U corresponding to the fourfold X"<<endl;
        U = trim ideal apply(8,j -> interpoleImage(mu,I + ideal arandom S,{2,2,2,2,2,3},3));
        I2 = ideal arandom S;
        if o.Verbose then <<"-- computing the surface U' corresponding to another fourfold X'"<<endl;
        U2 = trim ideal apply(8,j -> interpoleImage(mu,I2 + ideal arandom S,{2,2,2,2,2,3},3));
        if o.Verbose then <<"-- computing the 5 exceptional lines and the 4 exceptional conics on U and U'"<<endl;
        E := trim(U+U2); 
        topE := trim ideal select((intersect for i to 3 list (j := parametrize arandom({1},ring U); j top j^* E))_*,l -> degree l <= {3}); 
        pr := (rationalMap for i to 3 list random(1,ring U))|topE; 
        P = plucker(image pr,1); 
        while dim P <= 0 do P = plucker(image pr,1); 
        exceptionalLines = sub(plucker P,vars target pr); 
        exceptionalConics = saturate(image pr,exceptionalLines); 
        exceptionalLines = trim lift(pr^* exceptionalLines,ring U); 
        exceptionalConics = trim lift(pr^* exceptionalConics,ring U); 
        if o.Verbose then <<"-- computing the map f from U to the minimal K3 surface of degree 42"<<endl;
        f = mapDefinedByDivisor(quotient U,{(ideal first gens ring U,1),(exceptionalLines,1),(exceptionalConics,2)});
        if numgens target f != 22+1 then error "something went wrong on the target of the map defined by the divisor";
        if o.Verbose then <<"-- computing the image of f"<<endl;
        if ch <= 65521 then image(f,"F4") else interpoleImage(f,toList(190:2),2);
        return (multirationalMap mu,Var U,Var {exceptionalLines,exceptionalConics},multirationalMap f);
    );
    if X.cache#"label" === "FarkasVerra" then (
        if o.Verbose then <<"-- computing the map mu from P^5 to P^4 defined by the quintic hypersurfaces"<<endl;
        if o.Verbose then <<"   with points of multiplicity 2 along the surface S_26"<<endl;
        mu = fanoMap X;
        if o.Verbose then <<"-- computing the surface U corresponding to the fourfold X"<<endl;
        U = top trim ideal apply(12,j -> interpoleImage(mu,I + ideal arandom S,{5},5));
        I2 = ideal arandom S;
        if o.Verbose then <<"-- computing the surface U' corresponding to another fourfold X'"<<endl;
        U2 = top trim ideal apply(12,j -> interpoleImage(mu,I2 + ideal arandom S,{5},5));
        if o.Verbose then <<"-- computing the exceptional quartic curve on U and U'"<<endl;
        exceptionalQuarticCurve = ideal (1_(ring U));
        for i to 5 do (
            j := parametrize ideal random(1,ring U); 
            exceptionalQuarticCurve = intersect(exceptionalQuarticCurve,j top j^*(U+U2))
        ); 
        exceptionalQuarticCurve = trim ideal select(exceptionalQuarticCurve_*,y -> degree y <= {2});
        if not(dim exceptionalQuarticCurve == 2 and degree exceptionalQuarticCurve == 4 and flatten degrees exceptionalQuarticCurve == {2,2,2,2,2,2}) then error "something went wrong";
        if o.Verbose then <<"-- skipping computation of the map f from U to the minimal K3 surface of degree 26"<<endl;
        return (multirationalMap mu,Var U,Var {exceptionalQuarticCurve},null);
    );
    if X.cache#"label" === "oneNodalSepticDelPezzoSurfaceC26" then (
        if o.Verbose then <<"-- computing the map mu from P^5 to P^7 defined by the quintic hypersurfaces"<<endl;
        if o.Verbose then <<"   with points of multiplicity 2 along the surface S_26"<<endl;
        mu = rationalMap fanoMap X;
        if o.Verbose then <<"-- computing the surface U corresponding to the fourfold X"<<endl;
        U = trim ideal apply(13,j -> interpoleImage(mu,I + ideal arandom S,{2,2,2,2,2,3},3));
        I2 = ideal arandom S;
        if o.Verbose then <<"-- computing the surface U' corresponding to another fourfold X'"<<endl;
        U2 = trim ideal apply(13,j -> interpoleImage(mu,I2 + ideal arandom S,{2,2,2,2,2,3},3));
        if o.Verbose then <<"-- computing the exceptional twisted cubic on U and U'"<<endl;
        exceptionalCubic := trim(U+U2);
        exceptionalCubic = trim ideal select((intersect for i to 3 list (j := parametrize arandom({1},ring U); j top j^* exceptionalCubic))_*,l -> degree l <= {2}); 
        if not ? exceptionalCubic == "cubic curve of arithmetic genus 0 in PP^7 cut out by 7 hypersurfaces of degrees (1,1,1,1,2,2,2)" then error "something went wrong";
        if o.Verbose then <<"-- skipping computation of the normalization of U and"<<endl;
        if o.Verbose then <<"   of the map f from U to the minimal K3 surface of degree 26"<<endl;
        return (multirationalMap mu,Var U,Var {exceptionalCubic},null);
    );
    error "not implemented yet: fourfold not recognized yet or not rational";
);

parameterCount = method(Options => {Verbose => true})

parameterCount (Ideal,Ideal,Boolean) := o -> (S,X,isSing) -> (
    if ring S =!= ring X then error "expected same ring";
    if not (isField coefficientRing ring S and isPolynomialRing ring S and isHomogeneous S and isHomogeneous X) then error "expected homogeneous ideals in a polynomial ring over a field";
    d := first first degrees X;
    c := codim X;
    if not ({{d}} === unique degrees X and c == # degrees X) then error "the second argument must be the ideal of a complete intersection of hypersurfaces of the same degree";
    r := max(dim S -1,-1);
    if (r <= 0) then error "the first argument must be the ideal of a positive dimensional scheme";
    if not isSubset(X,S) then error "expected the first scheme to be a subscheme of the second one";
    if o.Verbose then <<"S: "|toString(? S)<<endl;
    if o.Verbose then <<"X: "|toString(? X)<<endl;
    n := numgens ring S -1;
    N := normalSheaf S;
    if isSing then (
    --   R := (ring S)/S;
    --   XX := Proj R;
    --   IXX := sheaf ((module S) ** R);
    --   y := rank Ext^1(IXX,OO_XX);
    --   if o.Verbose then <<"dim Ext^1(I_{S,P^"|toString(n)|"},O_S) = "|toString(y)<<endl; 
    --   if y != 0 then <<"--warning: condition not satisfied: dim Ext^1(I_{S,P^"|toString(n)|"},O_S) = 0"<<endl;
       if o.Verbose then <<"(assumption: dim Ext^1(I_{S,P^"|toString(n)|"},O_S) = 0)"<<endl; 
    ) else (
    --   h1N := rank HH^1 N;
    --   if o.Verbose then <<"h^1(N_{S,P^"|toString(n)|"}) = "|toString(h1N)<<endl; 
    --   if h1N != 0 then <<"--warning: condition not satisfied: h^1(N_{S,P^"|toString(n)|"}) = 0"<<endl;
       if o.Verbose then <<"(assumption: h^1(N_{S,P^"|toString(n)|"}) = 0)"<<endl; 
    );
    h0N := rank HH^0 N;
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
    h1OSd := for j from 1 to r list if odd j then rank HH^j(OS(d)) else continue;
    if unique h1OSd =!= {0} then error("condition not satisfied: h^(2j-1)(O_S("|toString(d)|")) = 0");
    m := numgens ideal image basis(d,S);
    pS := hilbertPolynomial(S,Projective=>false);
    m' := binomial(n+d,d) - sub(pS,first gens ring pS => d);
    if m != m' then error("condition not satisfied: h^0(I_{S,P^"|toString(n)|"}("|toString(d)|")) == h^0(O_(P^"|toString(n)|")("|toString(d)|")) - \\chi(O_S("|toString(d)|"))");
    if o.Verbose then (
       for j from 1 to r list if odd j then  <<"h^"|toString(j)|"(O_S("|toString(d)|")) = 0, ";
       <<"and h^0(I_{S,P^"|toString(n)|"}("|toString(d)|")) = "|toString(m)|" = h^0(O_(P^"|toString(n)|")("|toString(d)|")) - \\chi(O_S("|toString(d)|"));"|newline|"in particular, h^0(I_{S,P^"|toString(n)|"}("|toString(d)|")) is minimal"<<endl;
    );
    M := c*(m-c); -- dim GG(c-1,m-1)
    if c > 1 and o.Verbose then <<"dim GG("|toString(c-1)|","|toString(m-1)|") = "|toString(M)<<endl;
    if o.Verbose then <<"h^0(N_{S,P^"|toString(n)|"}) + "|(if c > 1 then "dim GG("|toString(c-1)|","|toString(m-1)|")" else toString(m-1))|" = "|toString(h0N + M)<<endl;
    NX := normalSheaf(S,X);
    -- if o.NumNodes == 0 then (
    --    h1NX := rank HH^1 NX;
    --    if o.Verbose then <<"h^1(N_{S,X}) = "|toString(h1NX)<<endl;
    -- );
    h0NX := rank HH^0 NX;
    if o.Verbose then <<"h^0(N_{S,X}) = "|toString(h0NX)<<endl;
    if o.Verbose then <<"dim{[X] : S\\subset X} >= "|toString(h0N + M - h0NX)<<endl;
    if o.Verbose then <<(if c > 1 then "dim GG("|toString(c-1)|",P(H^0(O_(P^"|toString(n)|")("|toString(d)|")))) = " else "dim P(H^0(O_(P^"|toString(n)|")("|toString(d)|"))) = ")|toString(c * (binomial(n+d,d) - c))<<endl;
    w := c*(binomial(n+d,d)-c) - (h0N+M-h0NX);
    if o.Verbose then <<"codim{[X] : S\\subset X} <= "|toString(w)<<endl;
    return (w,(m,h0N,h0NX));
);

parameterCount (Ideal,Ideal) := o -> (S,X) -> parameterCount(S,X,true,Verbose=>o.Verbose); 

parameterCount (EmbeddedProjectiveVariety,EmbeddedProjectiveVariety) := o -> (S,X) -> parameterCount(ideal S,ideal X,true,Verbose=>o.Verbose); 

parameterCount SpecialCubicFourfold := o -> X -> parameterCount(ideal surface X,ideal X,numberNodes surface X > 0,Verbose=>o.Verbose);

normalSheaf = method(TypicalValue=>CoherentSheaf);

normalSheaf Ideal := I -> (
    if not isHomogeneous I then error "expected a homogeneous ideal";
    R := (ring I)/I;
    sheaf Hom((module I) ** R,R)
);

normalSheaf (Ideal,Ideal) := (I,J) -> (
    if ring I =!= ring J then error "expected same ring";
    if not isSubset(J,I) then error "inclusion not satisfied";
    normalSheaf sub(I,(ring J)/J)
);

normalSheaf EmbeddedProjectiveVariety := X -> normalSheaf ideal X;

normalSheaf (EmbeddedProjectiveVariety,EmbeddedProjectiveVariety) := (X,Y) -> normalSheaf(ideal X,ideal Y);

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

find3Eminus1secantCurveOfDegreeE = method(Options => {Verbose => true})
find3Eminus1secantCurveOfDegreeE (EmbeddedProjectiveVariety,SpecialCubicFourfold) := o -> (p,X) -> (
    phi := map X;
    if not(isPoint p and ring ambient p === ring ambient X) then error "expected a point in the ambient projective space of the cubic fourfold";
    p = ideal p;
    S := ideal surface X; 
    if o.Verbose then <<"S: "<<?S<<endl;
    imageOfAssociatedMap X; -- image of phi
    if o.Verbose then <<"phi: "<<toString expression phi<<endl;
    if o.Verbose then <<"Z=phi(P^"|toString(numgens source phi -1)|")"<<endl;
    -- if o.Verbose then <<"multidegre(phi): "<<projectiveDegrees phi<<endl;
    lines2secant := 0;
    conics5secant := 0;
    cubics8secant := 0;
    quartics11secant := 0;
    quintics14secant := 0;
    sectics17secant := 0;
    Lines2secant := {};
    Conics5secant := {};
    Cubics8secant := {};
    Quartics11secant := {};
    Quintics14secant := {};
    Sectics17secant := {};
    T := secantCone(flatten entries coefficients parametrize p,S);
    try assert(dim T -1 == 1) else error "expected secant cone to be one dimensional";
    degT := degree T;
    V := coneOfLines(image phi,phi p);
    try assert (dim V -1 == 1) else error "expected cone of lines to be one dimensional";
    degV := degree V;
    if o.Verbose then <<"number lines contained in Z and passing through the point phi(p): "<<degV<<endl;
    if o.Verbose then <<"number 2-secant lines to S passing through p: "<<degree T<<endl;
    E := saturate(V,phi T);   
    degE := degree E;
    try assert(degE + degT == degV) else error "internal error encountered";
    if dim E -1 <= 0 then return ({lines2secant+degT,conics5secant,cubics8secant,quartics11secant,quintics14secant,sectics17secant},{Lines2secant|{T},Conics5secant,Cubics8secant,Quartics11secant,Quintics14secant,Sectics17secant});
    try assert(dim E -1 == 1) else error "internal error encountered";
    g := rationalMap(phi p);
    E' := g E;
    g' := rationalMap(target g,Grass(0,1,coefficientRing g),{random(1,target g),random(1,target g)});
    decE' := apply(decompose g' E',y -> radical trim (g'^*y + E'));
    decE := apply(decE',D -> g^* D);
    P := apply(decE,D -> (D' := phi^* D; (degree D',dim D' -1, dim (D'+S) -1,degree (D'+S),D')));
    local e;
    local cond;
    for p in P do (
        try assert (p_1 == 1 and p_2 == 0) else error "internal error encountered";
        e=1;
        cond = (3*e-1)*p_0 == e*p_3;
        for j in delete(e,toList(1..6)) do cond = cond and ((3*j-1)*p_0 != j*p_3);
        if cond then (lines2secant = lines2secant + lift(p_0/e,ZZ); Lines2secant = append(Lines2secant,p_4); continue);
        e=2;
        cond = (3*e-1)*p_0 == e*p_3;
        for j in delete(e,toList(1..6)) do cond = cond and ((3*j-1)*p_0 != j*p_3);
        if cond then (conics5secant = conics5secant + lift(p_0/e,ZZ); Conics5secant = append(Conics5secant,p_4); continue);
        e=3;
        cond = (3*e-1)*p_0 == e*p_3;
        for j in delete(e,toList(1..6)) do cond = cond and ((3*j-1)*p_0 != j*p_3);
        if cond then (cubics8secant = cubics8secant + lift(p_0/e,ZZ); Cubics8secant = append(Cubics8secant,p_4); continue);
        e=4;
        cond = (3*e-1)*p_0 == e*p_3;
        for j in delete(e,toList(1..6)) do cond = cond and ((3*j-1)*p_0 != j*p_3);
        if cond then (quartics11secant = quartics11secant + lift(p_0/e,ZZ); Quartics11secant = append(Quartics11secant,p_4); continue);
        e=5;
        cond = (3*e-1)*p_0 == e*p_3;
        for j in delete(e,toList(1..6)) do cond = cond and ((3*j-1)*p_0 != j*p_3);
        if cond then (quintics14secant = quintics14secant + lift(p_0/e,ZZ); Quintics14secant = append(Quintics14secant,p_4); continue);
        e=6;
        cond = (3*e-1)*p_0 == e*p_3;
        for j in delete(e,toList(1..6)) do cond = cond and ((3*j-1)*p_0 != j*p_3);
        if cond then (sectics17secant = sectics17secant + lift(p_0/e,ZZ); Sectics17secant = append(Sectics17secant,p_4); continue);
    );
    try assert (lines2secant == 0) else error "internal error encountered";
    try assert(conics5secant + cubics8secant + quartics11secant + quintics14secant + sectics17secant == degE) else error "internal error encountered";
    if o.Verbose then <<"number 5-secant conics to S passing through p: "<<conics5secant<<endl;
    Out := {Lines2secant|{T},Conics5secant,Cubics8secant,Quartics11secant,Quintics14secant,Sectics17secant};
    if conics5secant == degE then return ({lines2secant+degT,conics5secant,cubics8secant,quartics11secant,quintics14secant,sectics17secant},Out);
    if o.Verbose then<<"number 8-secant cubics to S passing through p: "<<cubics8secant<<endl;
    if conics5secant + cubics8secant == degE then return ({lines2secant+degT,conics5secant,cubics8secant,quartics11secant,quintics14secant,sectics17secant},Out);
    if o.Verbose then <<"number 11-secant quartics to S passing through p: "<<quartics11secant<<endl;
    if conics5secant + cubics8secant + quartics11secant == degE then return ({lines2secant+degT,conics5secant,cubics8secant,quartics11secant,quintics14secant,sectics17secant},Out);
    if o.Verbose then <<"number 14-secant quintics to S passing through p: "<<quintics14secant<<endl;
    if conics5secant + cubics8secant + quartics11secant + quintics14secant == degE then return ({lines2secant+degT,conics5secant,cubics8secant,quartics11secant,quintics14secant,sectics17secant},Out);
    if o.Verbose then <<"number 17-secant sectics to S passing through p: "<<sectics17secant<<endl;
    return ({lines2secant+degT,conics5secant,cubics8secant,quartics11secant,quintics14secant,sectics17secant},Out);
);

detectCongruence = method();

hintCongruence = method();
hintCongruence MutableHashTable := X -> (
    if X.cache#?"label" and X.cache#"label" =!= "NotRecognized" then return; 
    if (map X)#"idealImage" =!= null then return;
    <<///Hint: the current version of the method 'detectCongruence' needs 
of the often long calculation 'image map X', where 'X' is the fourfold.
If you know, for instance, that the image of 'map X' is cut out by quadrics, then 
you can use a command like 'forceImage(map X,image(2,map X))' to inform the system.
This should considerably reduce calculation times.///<<endl<<endl;
);

detectCongruence SpecialCubicFourfold := X -> (
    hintCongruence X;
    (l,L) := find3Eminus1secantCurveOfDegreeE(point ambient X,X,Verbose=>true);
    e := for i to 5 do if l_i == 1 then break (i+1);
    if e === null then error "no congruences detected";
    return detectCongruence(X,e);
);

detectCongruence (SpecialCubicFourfold,ZZ) := (X,e) -> (
    hintCongruence X;
    f := method();
    f Ideal := p -> (
       phi := map X;
       S := ideal surface X; 
       q := phi p;
       E := coneOfLines(imageOfAssociatedMap X,q);
       g := rationalMap(q);
       E' := g E;
       g' := rationalMap(target g,Grass(0,1,coefficientRing g),{random(1,target g),random(1,target g)});
       decE' := apply(select(decompose g' E',s -> (dim s,degree s) == (1,1)),y -> radical trim (g'^*y + E'));
       decE := apply(decE',D -> g^* D);
       P := apply(decE,D -> (D' := phi^* D; (degree D',dim D' -1, dim (D'+S) -1,degree (D'+S),D')));
       P = select(P,s -> s_0 == e and s_1 == 1 and s_2 == 0 and s_3 == 3*e-1);
       if #P != 1 then error "internal error encountered";
       C := last first P;
       if genus C != 0 then C = top C;
       C
    );
    f EmbeddedProjectiveVariety := p -> Var f (ideal p);
    try f point ambient X else error "no congruences detected";
    f
);

unirationalParametrization = method();

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

unirationalParametrization SpecialCubicFourfold := (cacheValue "unirationalParametrization") (X -> (
    p := point X;
    L := linearSpan {p,point coneOfLines(X,p)};
    unirationalParametrization(X,L)
));

randomS42data = method();
randomS42data Ring := K -> (
    P2 := Grass(0,2,K,Variable=>"t");
    p := for i to 4 list point P2;
    f1 := rationalMap(intersect(p_0,p_2,p_3,p_4),2);
    f2 := rationalMap(intersect(p_0,p_1),2);
    f := rationalMap gens((ideal matrix f1) * (ideal matrix f2));
    P1xP3 := (target f1) ** (target f2);
    s := rationalMap(P1xP3,target f,gens((ideal sub(vars target f1,P1xP3)) * (ideal sub(vars target f2,P1xP3))));
    Sigma := image s;
    S := image f;
    q1 := f (point P2);
    q2 := f (point P2);
    j := parametrize arandom({1},intersect(q1,q2));
    C' := j^* S;
    Scr := j^* Sigma;
    q := trim(ideal image basis(1,intersect(j^* q1,j^* q2)) + arandom({1},source j));
    pr := rationalMap q ;
    B := pr Scr;
    C := pr C';
    phi := rationalMap(ring C,Grass(0,6,K,Variable=>"x"),gens C,Dominant=>true);
    forceInverseMap(phi,inverseMap phi);
    Bs := trim lift(ideal inverse phi,ambient target phi);
    --
    if char K > 0 then (try assert(genera Bs == {20, 48, 33} and degree Bs == 34) else error "internal error encountered");
    --
    findPtsOnQ := () -> (
       i := parametrize arandom({1,1},ambient target phi);
       Bs' := i^* Bs;
       i' := rationalMap {arandom(1,source i),arandom(1,source i)};
       i' = i'|Bs';
       Bs'' := i' Bs';
       i radical trim lift(i'^* quotient(Bs'',radical Bs''),ambient source i')
    );
    Q := findPtsOnQ();
    while select(degrees Q,d -> d <= {2}) != {{1},{1},{1},{2}} do Q = intersect(Q,findPtsOnQ());
    Q = trim ideal select(Q_*,d -> degree d <= {2});
    --
    try assert(? Q == "smooth quadric surface in PP^6") else error "internal error encountered";
    --
    P1xP2 := phi^* Q;
    w := trim lift(phi (rationalMap flatten entries syz gens P1xP2)^-1 (ideal submatrix'(vars ring P1xP2,{0})),ambient target phi);
    e := rationalMap inverse(rationalMap trim sub(w,quotient Q),MathMode=>true);
    w = e point source e;
    TwQ := ideal((vars ring Q) * sub(jacobian Q,apply(gens ring Q,flatten entries coefficients parametrize w,(x0,s0) -> x0 => s0)));
    (L1,L2) := toSequence decompose trim(TwQ + Q);
    --
    try assert(?L1 == "line in PP^6" and ?L2 == "line in PP^6" and isSubset(Q,L1) and isSubset(Q,L2)) else error "internal error encountered";
    --
    D := first select({phi^* L1,phi^* L2},w-> dim w -1 == 2 and degree w == 5 and (genera w)_1 == 1);
    psi := rationalMap(B,3);
    T := psi D;
    --
    try assert(dim T -1 == 2 and codim T == 6 and degree T == 9 and genera T == {0,2,8}) else error "internal error encountered";
    -- 
    eta := rationalMap(quotient image psi,source psi,gens ideal SchubertCycle22OnLinearSectionOfG14(Var image psi));
    S42 := eta T;
    --
    try assert(dim S42 -1 == 2 and degree S42 == 9 and genera S42 == {-5, 2, 8} and degrees S42 == toList(9:{3})) else error "internal error encountered";
    -- 
    ((psi,D),(rationalMap inverse(eta,MathMode=>true),S42))
);

randomS48 = method();
randomS48 Ring := K -> (
    (psi,D) := first randomS42data(K);
    DP5 := image psi;
    p := psi point source psi;
    V := coneOfLines(DP5,p);
    j := parametrize ideal image basis(1,V);
    V' := j^* V;
    p' := j^* p;
    h := (rationalMap p')|V';
    V'' := image h;
    e := point V'';
    P := j trim lift(h^* quotient(coneOfLines(V'',e),e),ambient source h);
    assert(? P == "plane in PP^8" and isSubset(DP5,P));
    g := rationalMap(target psi,source psi,gens P);
    (psi * g) D
);

clean SpecialCubicFourfold := X -> (
    K := coefficientRing X;
    x := local x; 
    R := K[x_0..x_5];
    idS := sub(ideal surface X,vars R);
    idX := sub(ideal X,vars R);
    specialCubicFourfold(idS,idX,InputCheck=>0,NumNodes=>numberNodes surface X)
);

random SpecialCubicFourfold := o -> X -> specialCubicFourfold(surface X,InputCheck=>-1);

------------------------------------------------------------------------
--------------------------- GM fourfolds -------------------------------
------------------------------------------------------------------------

SpecialGushelMukaiFourfold = new Type of EmbeddedProjectiveVariety;

globalAssignment SpecialGushelMukaiFourfold;

SpecialGushelMukaiFourfold.synonym = "special Gushel-Mukai fourfold";

specialGushelMukaiFourfold = method(TypicalValue => SpecialGushelMukaiFourfold, Options => {InputCheck => 1, Verbose => true});

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
    Fourfold#"SurfaceContainedInTheFourfold" = S;
    Fourfold
);

specialGushelMukaiFourfold (Ideal,Ideal) := o -> (idS,idX) -> specialGushelMukaiFourfold(projectiveVariety idS,projectiveVariety idX,InputCheck=>o.InputCheck,Verbose=>o.Verbose);

specialGushelMukaiFourfold Ideal := o -> S -> (
    if isPolynomialRing ring S and numgens ring S === 10 then try(
         G14 := Grass(1,4,coefficientRing ring S,Variable=>ring S);
         assert isSubset(ideal G14,S);
         return specialGushelMukaiFourfold(sub(S,G14),InputCheck=>o.InputCheck,Verbose=>o.Verbose);
    );
    er := "expected the ideal of a surface in the coordinate ring of a GM fourfold or del Pezzo fivefold or del Pezzo sixfold";
    R := ring S;
    if not(isPolynomialRing ambient R and isHomogeneous ideal R) then error er;
    if not((numgens ambient R -1 == 9 and degrees ideal R === toList(5:{2})) or (numgens ambient R -1 == 8 and (degrees ideal R === toList(5:{2}) or degrees ideal R === toList(6:{2})))) then error er;
    r := dim ideal R -1;
    if r != 4 and r != 5 and r != 6 then error er;
    I := trim lift(S,ambient R);
    if not(isHomogeneous I and dim I -1 == 2) then error er;
    if r == 4 then (
        return specialGushelMukaiFourfold(I,ideal R,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
    );
    if r == 5 then (
        return specialGushelMukaiFourfold(I,(ideal R) + arandom({2},I),InputCheck=>o.InputCheck,Verbose=>o.Verbose);
    );
    if r == 6 then (
        j := toRationalMap toGrass Var R;
        I' := trim lift(j S,ambient target j);
        L := ideal image basis(1,I');
        if codim L <= 0 then error "expected linear span of the surface to be of dimension at most 8";
        if codim L >= 2 then L = arandom({1},L);
        l := parametrize L;
        I' = l^* I';
        return specialGushelMukaiFourfold(I',((map l) ideal target j) + arandom({2},I'),InputCheck=>o.InputCheck,Verbose=>o.Verbose);
    );
);

specialGushelMukaiFourfold (String,Ring) := o -> (str,K) -> (
    G14 := Grass(1,4,K,Variable=>"p");
    local X;
    if str === "very general" then (
        X = specialGushelMukaiFourfold(trim sub(ideal(G14) + arandom({1,1,1,2},ambient G14),G14),InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        X.cache#"label" = "very general";
        return X;
    );
    if str === "sigma-plane" then (
        X = specialGushelMukaiFourfold(schubertCycle({3,1},G14),InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        X.cache#"label" = 6;
        return X;
    );
    if str === "rho-plane" then (
        X = specialGushelMukaiFourfold(schubertCycle({2,2},G14),InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        X.cache#"label" = 9;
        return X;
    );
    if str === "tau-quadric" then (
        X = specialGushelMukaiFourfold(schubertCycle({1,1},G14) + arandom({1,1},G14),InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        X.cache#"label" = 1;
        return X;
    );
    if str === "cubic scroll" then (
        X = specialGushelMukaiFourfold(schubertCycle({2,0},G14) + arandom({1,1},G14),InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        X.cache#"label" = 7; 
        return X;
    );
    if str === "quintic del Pezzo surface" then (
        X = specialGushelMukaiFourfold(trim sub(ideal(G14) + arandom({1,1,1,1},ambient G14),G14),InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        X.cache#"label" = 4;
        return X;
    );
    if str === "quintic" then return specialGushelMukaiFourfold("quintic del Pezzo surface",K,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
    if str === "K3 surface of degree 14" then (
        G15 := Grass replace(1,5,Grass G14);
        pr := rationalMap(G15,G14,select(gens ambient G15,g -> last last baseName g != 5));
        X = specialGushelMukaiFourfold(pr sub(ideal for i to 5 list random(1,ambient G15),G15),InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        X.cache#"label" = 3;
        return X;
    );
    if str === "K3 surface of genus 8" then return specialGushelMukaiFourfold("K3 surface of degree 14",K,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
    if str === "surface of degree 9 and genus 2" then (
        (g,T) := first randomS42data(K);
        X = specialGushelMukaiFourfold((toRationalMap toGrass Var image g) g T,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        X.cache#"label" = 17;
        return X;     
    );
    if #str >= 1 and #str <= 2 then (
        Vstr := value str;
        if instance(Vstr,ZZ) and Vstr >= 1 and Vstr <= 21 then return fourfoldFromTriple(Vstr,GMtables(Vstr,K),InputCheck=>o.InputCheck,Verbose=>o.Verbose);
    );
    if str === "nodal D26''" then (
        X = specialGushelMukaiFourfold(last exampleD26''data K,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        (surface X).cache#"FiniteNumberOfNodes" = 1;
        X.cache#"label" = "mukai26''";
        return X;
    );
    if str === "nodal D44" then (
        X = specialGushelMukaiFourfold(last exampleD44data K,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        (surface X).cache#"FiniteNumberOfNodes" = 1;
        X.cache#"label" = "nodal D44";
        return X;
    );
    error "not valid string, permitted strings are: \"sigma-plane\", \"rho-plane\", \"tau-quadric\", \"cubic scroll\", \"quintic del Pezzo surface\", \"K3 surface of degree 14\", \"surface of degree 9 and genus 2\", \"1\",...,\"21\", \"nodal D26''\", \"nodal D44\"";
);

specialGushelMukaiFourfold String := o -> str -> specialGushelMukaiFourfold(str,ZZ/65521,InputCheck=>o.InputCheck,Verbose=>o.Verbose);

surface SpecialGushelMukaiFourfold := X -> X#"SurfaceContainedInTheFourfold";

expression SpecialGushelMukaiFourfold := X -> expression("GM fourfold containing a surface of degree "|toString(degree surface X)|" and sectional genus "|toString(sectionalGenus surface X)|(if X.cache#?"classSurfaceInG14" then (" with class "|toString(first cycleClass X)) else ""));

describe SpecialGushelMukaiFourfold := X -> (
    S := surface X;
    d := degree S; g := sectionalGenus S; chiOS := eulerHilbertPol S;
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
    if dim singLocus grassmannianHull X >= 0 then descr = descr|newline|"Type: Gushel (not ordinary)" else descr = descr|newline|"Type: ordinary";
    if instance(recognize X,ZZ) then descr = descr|newline|"(case "|toString(recognize X)|" of Table 1 in arXiv:2002.07026)";
    if recognize X === "gushel26''" and dim singLocus grassmannianHull X >= 0 then descr = descr|newline|"(case considered in Section 1 of arXiv:2003.07809)";
    if recognize X === "mukai26''" and dim singLocus grassmannianHull X >= 0 then descr = descr|newline|"(case considered in Section 2 of arXiv:2003.07809)";
    if recognize X === "mukai26''" and dim singLocus grassmannianHull X == -1 then descr = descr|newline|"(case considered in Section 3 of arXiv:2003.07809)";
    net expression descr
);

grassmannianHull = method();
grassmannianHull SpecialGushelMukaiFourfold := X -> varietyDefinedBylinearSyzygies X;
grassmannianHull EmbeddedProjectiveVariety := X -> varietyDefinedBylinearSyzygies X; -- undocumented

map SpecialGushelMukaiFourfold := o -> X -> (
    if X.cache#?"AssociatedMap" then return X.cache#"AssociatedMap";
    S := ideal surface X;
    J1 := select(S_*,s -> degree s == {1});
    if #J1 > 0 then J1 =  (ideal J1) * (ideal vars ring S) else J1 = ideal ring S;
    J2 := select(S_*,s -> degree s == {2});
    if #J2 > 0 then J2 =  ideal J2 else J2 = ideal ring S;
    X.cache#"AssociatedMap" = rationalMap trim sub(J1+J2,ring grassmannianHull X)    
);

recognize SpecialGushelMukaiFourfold := (cacheValue "label") (X -> ( 
    S := surface X;
    d := discriminant X;
    e := eulerCharacteristic S;
    (a,b) := last cycleClass X;
    invS := (degree S,sectionalGenus S,eulerHilbertPol S);
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
    "NotRecognized"
));

fanoMap SpecialGushelMukaiFourfold := (cacheValue "fanoMap") (X -> (
    recognize X;
    S := trim sub(ideal surface X,ring grassmannianHull X);
    local mu;
    if X.cache#"label" === 1 then (
        mu = rationalMap(S,1);
        forceImage(mu,ideal(0_(target mu)));
        return mu;
    );
    if X.cache#"label" === 3 then (
        mu = rationalMap(S^3 : ideal first gens ring S,5);
        interpoleImage(mu,{3},3);
        mu = rationalMap(mu,Dominant=>true);
        return mu;
    );
    if X.cache#"label" === 6 then (
        mu = rationalMap(S,1,Dominant=>true);
        return mu;
    );
    if X.cache#"label" === 17 then (
        mu = rationalMap(S^2 : ideal first gens ring S,3);
        forceImage(mu,ideal(0_(target mu)));
        return mu;
    );
    error "not implemented yet: fourfold not recognized yet or not rational";
));

parametrize SpecialGushelMukaiFourfold := X -> (
    if X.cache#?"rationalParametrization" then return X.cache#"rationalParametrization";
    Psi := fanoMap X;
    X.cache#"rationalParametrization" = inverse3(Psi|X)
);

associatedK3surface SpecialGushelMukaiFourfold := o -> X -> (
    recognize X;
    if not isAdmissibleGM X then error "expected an admissible GM fourfold";
    S := ideal surface X; I := ideal X;
    ch := char coefficientRing X;
    local mu; local I2; local U; local U2; local f;
    if X.cache#"label" === 1 then (
        if o.Verbose then <<"-- computing the map mu from the fivefold in P^8 to P^4 defined by the hypersurfaces"<<endl;
        if o.Verbose then <<"   of degree 1 through the tau quadric surface"<<endl;
        mu = fanoMap X;
        if o.Verbose then <<"-- computing the surface U corresponding to the fourfold X"<<endl;
        U = ideal matrix toRationalMap parametrize X;
        I2 = trim((ideal source mu) + (ideal arandom(2,S)));
        if o.Verbose then <<"-- computing the surface U' corresponding to another fourfold X'"<<endl;
        U2 = ideal matrix inverse3(mu|I2);
        if o.Verbose then <<"-- computing the two exceptional lines"<<endl;
        P := ideal 1; while dim P != 1 or degree P != 2 do P = plucker(trim(U+U2),1); 
        exceptionalLines := apply(decompose trim lift(P,ambient ring P),l -> sub(plucker sub(l,ring P),vars ring U));
        if not((# exceptionalLines == 1 and dim first exceptionalLines == 2 and degree first exceptionalLines == 2 and flatten degrees first exceptionalLines == {1,2,2,2,2}) or (# exceptionalLines == 2 and all(exceptionalLines,l->dim l == 2 and degree l == 1 and flatten degrees l == {1,1,1}))) then error "something went wrong";
        if o.Verbose then <<"-- skipping computation of the map f from U to the minimal K3 surface of degree 10"<<endl;
        return (multirationalMap mu,Var U,Var exceptionalLines,null);
    );  
    if X.cache#"label" === 3 then (
        if o.Verbose then <<"-- computing the map mu from the fivefold in P^8 to P^5 defined by the hypersurfaces"<<endl;
        if o.Verbose then <<"   of degree 5 with points of multiplicity 3 along the surface S of degree 14 and genus 8"<<endl;
        mu = rationalMap fanoMap X;
        if o.Verbose then <<"-- computing the surface U corresponding to the fourfold X"<<endl;
        U = trim ideal apply(9,j -> if ch <= 65521 then image(mu|(I + ideal arandom S),"F4") else interpoleImage(mu,I + ideal arandom S,{3,3},3));
        if o.Verbose then <<"-- computing the normalization of U"<<endl;
        normU := normalization(Var U,Verbose=>false);
        if o.Verbose then <<"-- inverting the normalization of U"<<endl;
        f = inverse3 normU;
        return (multirationalMap mu,Var U,{},multirationalMap f);
    );
    if X.cache#"label" === 17 then (
        if o.Verbose then <<"-- computing the map mu from the fivefold in P^8 to P^4 defined by the hypersurfaces"<<endl;
        if o.Verbose then <<"   of degree 3 with points of multiplicity 2 along the surface S of degree 9 and genus 2"<<endl;
        mu = fanoMap X;
        if o.Verbose then <<"-- computing the surface U corresponding to the fourfold X"<<endl;
        U = trim ideal apply(13,j -> (if o.Verbose then <<"-- (step "<<j+1<<" of 13)"<<endl; if ch <= 65521 then image(mu|(I + ideal arandom S),"F4") else interpoleImage(mu,I + ideal arandom S,{5},5)));
        I2 = trim((ideal source mu) + (ideal arandom S));
        if o.Verbose then <<"-- computing the surface U' corresponding to another fourfold X'"<<endl;
        U2 = trim ideal apply(13,j -> (if o.Verbose then <<"-- (step "<<j+1<<" of 13)"<<endl; if ch <= 65521 then image(mu|(I2 + ideal arandom S),"F4") else interpoleImage(mu,I2 + ideal arandom S,{5},5)));
        if o.Verbose then <<"-- computing the exceptional line and the exceptional cubic curve"<<endl;
        L := trim sub(plucker plucker(trim(U+U2),1),vars ring U);
        if not(dim L == 2 and degree L == 1) then error "something went wrong";
        C := saturate quotient(U+U2,L); 
        topC := ideal(1_(ring C)); 
        for i to 6 do (
            j := parametrize arandom({1},ring C); 
            topC = intersect(topC,j top j^* C)
        ); 
        C = trim ideal select(topC_*,t -> degree t <= {2});
        if not(dim C == 2 and degree C == 3) then error "something went wrong";
        if o.Verbose then <<"-- computing the map f from U to the minimal K3 surface of degree 20"<<endl;
        f = rationalMap(sub(radical saturate ideal singLocus Var U,quotient U),2,Dominant=>true);
        f0 := mapDefinedByDivisor(target f,{(f ideal arandom(1,source f),1),(f C^3,1),(f L,1)});        
        if numgens target f0 != 11+1 then error "something went wrong on the target of the map defined by the divisor";
        f = f * f0;
        if o.Verbose then <<"-- computing the image of f"<<endl;
        if ch <= 65521 then image(f,"F4") else interpoleImage(f,toList(36:2),2);
        return (multirationalMap mu,Var U,Var {L,C},multirationalMap f);
    );
    error "not implemented yet: fourfold not recognized yet or not rational";
);

toGrass = method(TypicalValue => MultirationalMap)

toGrass EmbeddedProjectiveVariety := (cacheValue "toGrass") (X -> (
    K := coefficientRing X;
    Y6 := Var Grass(1,4,K);
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
    Y := grassmannianHull X;
    psi := toGrass Y;
    if dim singLocus Y == -1 then return psi|X;
    (psi|X) * rationalMap(ring target psi,Grass(1,4,coefficientRing X),submatrix'(vars ring ambient target psi,{0}))
));

find2Eminus1secantCurveOfDegreeE = method(Options => {Verbose => true})
find2Eminus1secantCurveOfDegreeE (EmbeddedProjectiveVariety,SpecialGushelMukaiFourfold) := o -> (p,X) -> (
    phi := map X;
    if not(isPoint p and isSubset(p,grassmannianHull X)) then error "expected a point in P^8 contained in the del Pezzo fivefold";
    p = ideal p;
    S := ideal surface X; 
    if o.Verbose then <<"S: "<<?S<<endl;
    imageOfAssociatedMap X; -- image of phi
    if o.Verbose then <<"phi: "<<toString expression phi<<endl;
    if o.Verbose then <<"Z=phi(del Pezzo fivefold)"<<endl;
    -- if o.Verbose then <<"multidegre(phi): "<<projectiveDegrees phi<<endl;
    lines1secant := 0;
    conics3secant := 0;
    cubics5secant := 0;
    quartics7secant := 0;
    quintics9secant := 0;
    sectics11secant := 0;
    Lines1secant := {};
    Conics3secant := {};
    Cubics5secant := {};
    Quartics7secant := {};
    Quintics9secant := {};
    Sectics11secant := {};
    E := coneOfLines(image phi,phi p);
    if dim E -1 == 0 then (
        if o.Verbose then <<"number lines contained in Z and passing through the point phi(p): "<<0<<endl;
        return ({lines1secant,conics3secant,cubics5secant,quartics7secant,quintics9secant,sectics11secant},{Lines1secant,Conics3secant,Cubics5secant,Quartics7secant,Quintics9secant,Sectics11secant});
    );
    try assert (dim E -1 == 1) else error "expected cone of lines to be one dimensional";
    degE := degree E;
    if o.Verbose then <<"number lines contained in Z and passing through the point phi(p): "<<degE<<endl;
    g := rationalMap(phi p);
    E' := g E;
    g' := rationalMap(target g,Grass(0,1,coefficientRing g),{random(1,target g),random(1,target g)});
    decE' := apply(decompose g' E',y -> radical trim (g'^*y + E'));
    decE := apply(decE',D -> g^* D);
    P := apply(decE,D -> (D' := phi^* D; (degree D',dim D' -1, dim (D'+S) -1,degree (D'+S),D')));
    local e;
    local cond;
    for p in P do (
        try assert (p_1 == 1 and p_2 == 0) else error "internal error encountered";
        e=1;
        cond = (2*e-1)*p_0 == e*p_3;
        for j in delete(e,toList(1..6)) do cond = cond and ((2*j-1)*p_0 != j*p_3);
        if cond then (lines1secant = lines1secant + lift(p_0/e,ZZ); Lines1secant = append(Lines1secant,p_4); continue);
        e=2;
        cond = (2*e-1)*p_0 == e*p_3;
        for j in delete(e,toList(1..6)) do cond = cond and ((2*j-1)*p_0 != j*p_3);
        if cond then (conics3secant = conics3secant + lift(p_0/e,ZZ); Conics3secant = append(Conics3secant,p_4); continue);
        e=3;
        cond = (2*e-1)*p_0 == e*p_3;
        for j in delete(e,toList(1..6)) do cond = cond and ((2*j-1)*p_0 != j*p_3);
        if cond then (cubics5secant = cubics5secant + lift(p_0/e,ZZ); Cubics5secant = append(Cubics5secant,p_4); continue);
        e=4;
        cond = (2*e-1)*p_0 == e*p_3;
        for j in delete(e,toList(1..6)) do cond = cond and ((2*j-1)*p_0 != j*p_3);
        if cond then (quartics7secant = quartics7secant + lift(p_0/e,ZZ); Quartics7secant = append(Quartics7secant,p_4); continue);
        e=5;
        cond = (2*e-1)*p_0 == e*p_3;
        for j in delete(e,toList(1..6)) do cond = cond and ((2*j-1)*p_0 != j*p_3);
        if cond then (quintics9secant = quintics9secant + lift(p_0/e,ZZ); Quintics9secant = append(Quintics9secant,p_4); continue);
        e=6;
        cond = (2*e-1)*p_0 == e*p_3;
        for j in delete(e,toList(1..6)) do cond = cond and ((2*j-1)*p_0 != j*p_3);
        if cond then (sectics11secant = sectics11secant + lift(p_0/e,ZZ); Sectics11secant = append(Sectics11secant,p_4); continue);
    );
    try assert(lines1secant + conics3secant + cubics5secant + quartics7secant + quintics9secant + sectics11secant == degE) else error "internal error encountered";
    Out := {Lines1secant,Conics3secant,Cubics5secant,Quartics7secant,Quintics9secant,Sectics11secant};
    if o.Verbose then <<"number 1-secant lines to S passing through p: "<<lines1secant<<endl;
    if lines1secant == degE then return ({lines1secant,conics3secant,cubics5secant,quartics7secant,quintics9secant,sectics11secant},Out);
    if o.Verbose then <<"number 3-secant conics to S passing through p: "<<conics3secant<<endl;
    if lines1secant + conics3secant == degE then return ({lines1secant,conics3secant,cubics5secant,quartics7secant,quintics9secant,sectics11secant},Out);
    if o.Verbose then <<"number 5-secant cubics to S passing through p: "<<cubics5secant<<endl;
    if lines1secant + conics3secant + cubics5secant == degE then return ({lines1secant,conics3secant,cubics5secant,quartics7secant,quintics9secant,sectics11secant},Out);
    if o.Verbose then <<"number 7-secant quartics to S passing through p: "<<quartics7secant<<endl;
    if lines1secant + conics3secant + cubics5secant + quartics7secant == degE then return ({lines1secant,conics3secant,cubics5secant,quartics7secant,quintics9secant,sectics11secant},Out);
    if o.Verbose then <<"number 9-secant quintics to S passing through p: "<<quintics9secant<<endl;
    if lines1secant + conics3secant + cubics5secant + quartics7secant + quintics9secant == degE then return ({lines1secant,conics3secant,cubics5secant,quartics7secant,quintics9secant,sectics11secant},Out);
    if o.Verbose then <<"number 11-secant sectics to S passing through p: "<<sectics11secant<<endl;
    return ({lines1secant,conics3secant,cubics5secant,quartics7secant,quintics9secant,sectics11secant},Out);
);

detectCongruence SpecialGushelMukaiFourfold := X -> (
    hintCongruence X;
    (l,L) := find2Eminus1secantCurveOfDegreeE(pointOnLinearSectionOfG14 grassmannianHull X,X,Verbose=>true);
    e := for i to 5 do if l_i == 1 then break (i+1);
    if e === null then error "no congruences detected";
    return detectCongruence(X,e);
);

detectCongruence (SpecialGushelMukaiFourfold,ZZ) := (X,e) -> (
    hintCongruence X;
    f := method();
    f Ideal := p -> (
       phi := map X;
       S := ideal surface X; 
       q := phi p;
       E := coneOfLines(imageOfAssociatedMap X,q);
       g := rationalMap(q);
       E' := g E;
       g' := rationalMap(target g,Grass(0,1,coefficientRing g),{random(1,target g),random(1,target g)});
       decE' := apply(select(decompose g' E',s -> (dim s,degree s) == (1,1)),y -> radical trim (g'^*y + E'));
       decE := apply(decE',D -> g^* D);
       P := apply(decE,D -> (D' := phi^* D; (degree D',dim D' -1, dim (D'+S) -1,degree (D'+S),D')));
       P = select(P,s -> s_0 == e and s_1 == 1 and s_2 == 0 and s_3 == 2*e-1);
       if #P != 1 then error "internal error encountered";
       C := last first P;
       if genus C != 0 then C = trim sub(top trim lift(C,ambient ring C),ring C);
       C
    );
    f EmbeddedProjectiveVariety := p -> Var trim lift(f sub(ideal p,ring grassmannianHull X),ring ambient X);
    try f (pointOnLinearSectionOfG14 grassmannianHull X) else error "no congruences detected";
    f
);

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
    S := ideal surface X; G := ideal X;
    Y := ideal grassmannianHull X;
    if o.Verbose then <<"S: "|toString(? S)<<endl;
    if o.Verbose then <<"X: GM fourfold containing S"<<endl;
    if o.Verbose then <<"Y: del Pezzo fivefold containing X"<<endl;
    N := normalSheaf(S,Y);
    --
    h1N := rank HH^1 N;
    if o.Verbose then <<"h^1(N_{S,Y}) = "|toString(h1N)<<endl; 
    if h1N != 0 then error("condition not satisfied: h^1(N_{S,Y}) = 0");
    -- if h1N != 0 then <<"--warning: condition not satisfied: h^1(N_{S,Y}) = 0"<<endl;
    -- if o.Verbose then <<"(assumption: h^1(N_{S,Y}) = 0)"<<endl; 
    -- 
    h0N := rank HH^0 N;
    if o.Verbose then <<"h^0(N_{S,Y}) = "|toString(h0N)<<endl; 
    -- m := numgens ideal image basis(2,trim sub(S,source map X));
    m := numgens ideal image basis(2,S) - 5;
    OS := OO_(variety S);
    h1OS2 := rank HH^1(OS(2));
    if h1OS2 != 0 then error("condition not satisfied: h^1(O_S(2)) = 0");
    pS := hilbertPolynomial(S,Projective=>false);
    m' := 40 - sub(pS,first gens ring pS => 2);
    if m != m' then error("condition not satisfied: h^0(I_{S,Y}(2)) == h^0(O_Y(2)) - \\chi(O_S(2))");
    if o.Verbose then (
       <<"h^1(O_S(2)) = 0, ";
       <<"and h^0(I_{S,Y}(2)) = "|toString(m)|" = h^0(O_Y(2)) - \\chi(O_S(2));"|newline|"in particular, h^0(I_{S,Y}(2)) is minimal"<<endl;
    );
    if o.Verbose then <<"h^0(N_{S,Y}) + "|toString(m-1)|" = "|toString(h0N + m - 1)<<endl;
    NX := normalSheaf(S,G);
    h0NX := rank HH^0 NX;
    if o.Verbose then <<"h^0(N_{S,X}) = "|toString(h0NX)<<endl;
    if o.Verbose then <<"dim{[X] : S\\subset X \\subset Y} >= "|toString(h0N + m-1 - h0NX)<<endl;
    if o.Verbose then <<"dim P(H^0(O_Y(2))) = 39"<<endl;
    w := 39 - (h0N + m-1 - h0NX);
    if o.Verbose then <<"codim{[X] : S\\subset X \\subset Y} <= "|toString(w)<<endl;
    return (w,(m,h0N,h0NX));
);

sigmaQuadric = method(); 
sigmaQuadric SpecialGushelMukaiFourfold := X -> (
    f := toRationalMap toGrass X;
    H := image(f,1);
    Q := arandom({2},image f);
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
    Y := ideal grassmannianHull X;
    V := coneOfLines(sub(Y,ringP8'),p'); 
    j := parametrize((ideal select(V_*,v -> degree v == {1})) + (ideal first gens ring V));
    W := trim (map j) V;
    P := plucker(W,2); while dim P <= 0 do P = plucker(W,2); P = trim sub(plucker P,vars ring W);
    Q := trim quotient(W,P);
    q := trim minors(2,vars ring W || transpose submatrix(coefficients parametrize(P+Q),,{0}));
    f := (inverse(rationalMap trim sub(q,quotient Q),MathMode=>true)) * j;
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

clean SpecialGushelMukaiFourfold := X -> (
    K := coefficientRing X;
    x := local x; 
    R := K[x_0..x_8];
    idS := sub(ideal surface X,vars R);
    idX := sub(ideal X,vars R);
    specialGushelMukaiFourfold(idS,idX,InputCheck=>0)
);

random SpecialGushelMukaiFourfold := o -> X -> (
    X' := specialGushelMukaiFourfold(surface X,random(2,surface X) * grassmannianHull X,InputCheck=>-1);
    X'.cache#"varietyDefinedBylinearSyzygies" = grassmannianHull X;
    return X';
);

------------------------------------------------------------------------
--------------------------- Discriminants ------------------------------
------------------------------------------------------------------------

eulerCharacteristic = method(Options => {Algorithm => null});

eulerCharacteristic EmbeddedProjectiveVariety := o -> X -> (
    if X.cache#?"euler" then return X.cache#"euler";
    if codim linearSpan X > 0 then return X.cache#"euler" = eulerCharacteristic((parametrize linearSpan X)^^ X,Algorithm=>o.Algorithm);
    if o.Algorithm === "Hodge" then return X.cache#"euler" = euler variety X;
    if o.Algorithm === "CremonaMathModeTrue" then return euler(X,Verify=>true);
    K := coefficientRing X;
    if K === QQ then return X.cache#"euler" = eulerCharacteristic(X ** (ZZ/65521),Algorithm=>o.Algorithm);
    if char K < 1000 and K === ZZ/(char K) then error "base field too small to use probabilistic methods";
    if o.Algorithm === "CremonaMathModeFalse" then return X.cache#"euler" = euler(X,Verify=>false);
    if o.Algorithm === "CharacteristicClasses" then return X.cache#"euler" = Euler(ideal X,InputIsSmooth => true);
    if o.Algorithm === null then (
        if dim X == 2 then X' := isomorphicProjectionOfSurfaceInP5 X; 
        if char K <= 65521 and K === ZZ/(char K) and codim X' > 0 then return X.cache#"euler" = Euler(ideal X',InputIsSmooth => true) else return X.cache#"euler" = euler(X',Verify=>false);
   );
   error(///Algorithm option: Expected method to compute the topological Euler characteristic.
Possible methods are the following:
"Hodge" -- command: euler variety I -- package: Core;
"CremonaMathModeTrue" -- command: EulerCharacteristic(I,MathMode=>true) -- package: Cremona;
"CremonaMathModeFalse" -- command: EulerCharacteristic I -- package: Cremona;
"CharacteristicClasses" -- command: Euler(I,InputIsSmooth=>true) -- package: CharacteristicClasses
///);  
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
    if X.cache#?"discriminantFourfold" then return last X.cache#"discriminantFourfold";
    S := surface X;
    degS := degree S; g := sectionalGenus S; chiOS := eulerHilbertPol S;
    chiS := eulerCharacteristic(S,Algorithm=>if o.Algorithm === "Poisson" then null else o.Algorithm); 
    KS2 := 12*chiOS-chiS; 
    KSHS := 2*g-2-degS; 
    (a,b) := last cycleClass X;
    n := if S.cache#?"FiniteNumberOfNodes" or S.cache#?"singularLocus" or S.cache#?"nonSaturatedSingularLocus" or (S.cache#?"fitVariety" and (S.cache#"fitVariety").cache#?"nonSaturatedSingularLocus") then numberNodes S else 0;
    S2 := 3*a + 4*b + 2*KSHS + 2*KS2 - 12*chiOS + 2*n;
    d := 4*S2 - 2*(b^2+(a-b)^2);
    if S.cache#?"FiniteNumberOfNodes" then X.cache#"discriminantFourfold" = (S2,d);
    d
);

discriminant SpecialCubicFourfold := o -> X -> ( 
    if X.cache#?"discriminantFourfold" then return last X.cache#"discriminantFourfold";
    S := surface X;
    degS := degree S; g := sectionalGenus S; chiOS := eulerHilbertPol S;
    chiS := eulerCharacteristic(S,Algorithm=>if o.Algorithm === "Poisson" then null else o.Algorithm); 
    KS2 := 12*chiOS-chiS; 
    n := numberNodes S;
    S2 := 3*degS+6*g-12*chiOS+2*KS2+2*n-6;
    d := 3*S2 - degS^2;
    X.cache#"discriminantFourfold" = (S2,d);
    d
); 

------------------------------------------------------------------------
---------------------- Schubert Varieties ------------------------------
------------------------------------------------------------------------

AA := local AA;
s := local s;
chowRing = method();
chowRing (ZZ,ZZ,ZZ) := (k,n,m) -> (
    if (class AA_(k,n,m) === PolynomialRing) then return AA_(k,n,m); 
    L := rsort select(apply(toList (set toList(0..(n-k)))^**(k+1),l -> toList deepSplice l),l -> l == rsort l and sum l == m);
    AA_(k,n,m) = ZZ[apply(L,l -> s_(unsequence toSequence l))]
);

completeFlag = method();
completeFlag PolynomialRing := R -> (
    V := {ideal R};
    for i to numgens R - 1 do V = prepend(trim(ideal(random(1,R)) + first(V)),V);
    if apply(V,dim) != toList(0 .. numgens R) then completeFlag(R) else V
);

schubertCycleInt = method(Options => {Variable => "p"});
schubertCycleInt (VisibleList,ZZ,ZZ,Ring) := o -> (a,k',n',K) -> (
    a = toList a;
    n := n' + 1;
    k := #a;
    if not (all(a,j -> instance(j,ZZ)) and rsort a == a and first a <= n-k and k == k'+1) then error("expected a nonincreasing sequence of "|toString(k'+1)|" nonnegative integers bounded by "|toString(n'-k'));
    a = prepend(null,a);
    V := completeFlag Grass(0,n',K,Variable=>o.Variable);
    S := for i from 1 to k list tangentialChowForm(V_(n-k+i-a_i),i-1,k-1,Variable=>o.Variable,SingularLocus=>first V);
    (V,trim sum apply(S,s -> if isIdeal s then s else ideal s))
);

schubertCycle = method();

schubertCycle (VisibleList,Ring) := (L,R) -> last schubertCycleInt prepend(L,Grass R);

schubertCycle (VisibleList,Ring,String) := (L,R,nu) -> (
    if nu =!= "standard" then error "invalid input string";
    (V,S) := schubertCycleInt prepend(L,Grass R);
    V' := apply(reverse V,gens);
    K := coefficientRing ambient R;
    f := rationalMap reverse for i from 1 to #V -1 list (V'_i * random(K^(numColumns V'_i),K^1))_(0,0);
    (S,rationalMap(f,R))
);

schubertCycle (VisibleList,EmbeddedProjectiveVariety) := (L,X) -> (
    if isGrass X === false then error "expected a Grassmannian variety";
    makeSubvariety schubertCycle(L,ring X)
);

schubertCycle (VisibleList,EmbeddedProjectiveVariety,String) := (L,X,nu) -> (
    (V,f) := schubertCycle(L,ring X,nu);
    (makeSubvariety V,multirationalMap f)
);

rationalMap (RationalMap,Ring) := o -> (Phi,Gr) -> (
    (k,n,K,Vp) := Grass Gr;
    if not (isPolynomialRing source Phi and isPolynomialRing target Phi and numgens source Phi == numgens target Phi and n == numgens source Phi -1 and K === coefficientRing Phi) then error "expected a pair: (projectivity of PP^n, coordinate ring of Grass(k,PP^n))";
    A := coefficients Phi;
    x := local x;
    R := K[x_(0,0)..x_(n,k)];
    M := genericMatrix(R,k+1,n+1);
    N := M*(transpose A);
    mM := matrix{apply(subsets(n+1,k+1),m -> det submatrix(M,m))};
    B := matrix apply(subsets(n+1,k+1),m -> linearCombination(det submatrix(N,m),mM));
    Psi := rationalMap(Gr,Gr,transpose(B * transpose vars Gr));
    if o.Dominant === true or o.Dominant === infinity then forceInverseMap(Psi,rationalMap inverse map Psi);
    Psi
);

dimdegree = X -> if dim X <= 0 then 0 else if dim X == 1 then degree X else error "expected a zero-dimensional scheme"; 

cycleClass = method();
cycleClass Ideal := X -> (
    (k,n,KK,Vp) := Grass ring X;
    m := codim X;
    A := chowRing(k,n,m);
    sum(gens A,g -> g * dimdegree(X + last schubertCycleInt(toList(k+1:n-k) - toList reverse last baseName g,k,n,KK,Vp)))
);

cycleClass EmbeddedProjectiveSubvariety := X -> cycleClass trim sub(ideal X,ring ambientVariety X);

cycleClass SpecialGushelMukaiFourfold := X -> (
    if X.cache#?"classSurfaceInG14" then return X.cache#"classSurfaceInG14";
    S := ideal surface X;
    j := toRationalMap toGrass X;
    cS := cycleClass j S;
    ab := toSequence flatten entries lift(transpose last coefficients(cS,Monomials=>vars ring cS),ZZ);
    X.cache#"classSurfaceInG14" = (cS,ab)
);

------------------------------------------------------------------------
---------------- arXiv:2002.07026 --------------------------------------
------------------------------------------------------------------------

constrTriple = method();
constrTriple (String,Ideal,Ideal) := (name,V,C) -> (
    if not(isPolynomialRing ring V and numgens ring V == 6 and ring V === ring C and isHomogeneous V and isHomogeneous C and dim V == 3 and dim C == 2 and isSubset(V,C)) then error "expected a pair (V,C) where C is a curve contained in a surface of PP^5";
    ringP5 := ring V;
    x := gens ringP5;
    K := coefficientRing ringP5;
    ringP2 := Grass(0,2,K,Variable=>"t");
    t := gens ringP2;
    ringP3 := Grass(0,3,K,Variable=>"u");
    u := gens ringP3;
    s := rationalMap(ringP2,ringP5,{t_0^2,t_0*t_1,t_1^2,t_0*t_2,t_1*t_2,0});
    S := image s; -- cubic scroll surface
    L0 := ideal(x_0,x_1,x_2,x_5); -- directrix line of S
    curveOnS := (e) -> (
        p0 := ideal(t_0,t_1); -- base point of s
        if even e then s arandom({lift(e/2,ZZ)},source s) else s arandom({lift((e+1)/2,ZZ)},p0)
    );    
    b := rationalMap(ringP3,ringP5,{u_0^3*u_1-u_0*u_1^3, u_0^2*u_1^2-u_0*u_1^3+u_0^3*u_2-u_1^3*u_3, u_0^2*u_1*u_2-u_1^3*u_3, -u_0^2*u_1^2+u_0*u_1^3+u_0*u_1^2*u_2-u_1^3*u_3, u_0^2*u_1*u_3-u_1^3*u_3, u_0^2*u_1^2-u_0*u_1^3+u_0*u_1^2*u_3-u_1^3*u_3});
    B := image b; -- quartic scroll threefold
    curveOnB := (e)  -> (
       E := (ideal(u_0,u_1), ideal(u_3,u_0), ideal(u_2,u_1), ideal(u_2-u_3,u_0-u_1)); -- base locus of b: E_0 triple line, E_1,E_2,E_3 simple lines
       -- assert(intersect(E_0^3,E_1,E_2,E_3) == saturate ideal b);
       pE := apply(E,L -> parametrize L);
       po := method(); po(RationalMap) := (psi) -> psi(point source psi);
       if e == 1 then return b arandom({1,1},po pE_0); --dim:3
       if e == 2 then return b arandom({2,1},intersect(po pE_0,po pE_0)); --dim:6
       -- if e == 2 then return b arandom({1,1},intersect(po pE_1,po pE_2)); --dim:4
       if e == 3 then return b arandom({2,1},intersect(po pE_0,po pE_1,po pE_2)); --dim:7
       -- if e == 3 then b arandom({1,1},po pE_1); --dim:7
       if e == 4 then return b arandom({1,2},intersect(po pE_0,po pE_1)); --dim:10
       -- if e == 4 then return b arandom({1,1},source b); --dim:10
       if e == 5 then return b arandom({1,2},po pE_0); --dim:13
       -- if e == 5 then return b arandom({1,2},intersect(po pE_1,po pE_2,po pE_3)); --dim:13
    );
    w := if name === "semple" or name === "Semple" or name === "s" or name === "S" then true else (if name === "verra" or name === "Verra" or name === "v" or name === "V" then false else error "expected name method to be Semple or Verra");
    C' := if w then curveOnS(degree C) else curveOnB(degree C);
    phi := toRationalMap findIsomorphism(Var C,Var C',Verify=>true);
    V' := phi V;
    (if w then S else B,V',C')
);

GMtables = method();
GMtables (ZZ,Ring) := (i,K) -> (
    if K =!= ZZ/(char K) then error "not implemented yet: coefficient ring different from ZZ/p";
    if i < 1 or i > 21 then error "expected integer between 1 and 21";
    ringP5 := Grass(0,5,K,Variable=>"x");
    x := gens ringP5;
    ringP2 := Grass(0,2,K,Variable=>"t");
    t := gens ringP2;
    s := rationalMap(ringP2,ringP5,{t_0^2,t_0*t_1,t_1^2,t_0*t_2,t_1*t_2,0});
    S := image s; -- cubic scroll surface
    L0 := ideal(x_0,x_1,x_2,x_5); -- directrix line of S
    curveOnS := (e) -> (
        p0 := ideal(t_0,t_1); -- base point of s
        if even e then s arandom({lift(e/2,ZZ)},source s) else s arandom({lift((e+1)/2,ZZ)},p0)
    );    
    local C; local V; local p; local v;
    if i == 1 then (
        C = curveOnS 2;
        V = arandom({1,1,2},C);
        return (S,V,C);
    );
    if i == 2 then (
        C = curveOnS 1;
        V = arandom({1,1,2},C);
        return (S,V,C);
    );
    if i == 3 then (
        C = intersect(L0,curveOnS 1,curveOnS 1,curveOnS 1);
        V = arandom({2,2,2},C);
        return (S,V,C);
    ); 
    if i == 4 then (
        C = curveOnS 3;
        V = arandom({1,2,2},C);
        return (S,V,C);
    ); 
    if i == 5 then return constrTriple("v",S,curveOnS 2);
    if i == 6 then (
        C = curveOnS 1;
        V = arandom({1,1,1},C);
        return (S,V,C);
    ); 
    if i == 7 then return constrTriple("s",S,curveOnS 3);
    if i == 8 then (
        C = arandom({1,1,1,1},ringP5);
        V = arandom({1,1,2},C);
        return constrTriple("v",V,C);
    ); 
    if i == 9 then return (S,arandom({1,1,1},L0),L0); 
    if i == 10 then (
        C = ideal(x_5,x_3^2-x_2*x_4,x_2*x_3-x_1*x_4,x_1*x_3-x_0*x_4,x_2^2-x_0*x_4,x_1*x_2-x_0*x_3,x_1^2-x_0*x_2); -- r.n.c. of degree 4
        V = arandom({1,2,2},C);
        return constrTriple("v",V,C);
    ); 
    if i == 11 then (
        p = for i to 1 list point ringP2;
        v = rationalMap(ringP2,ringP5,gens image basis(3,intersect(p_0,p_1^2)));
        V = image v;
        C = v arandom({2},p_1);
        return constrTriple("v",V,C);
    );
    if i == 12 then (
        C = curveOnS 4;
        V = arandom({2,2,2},C);
        return (S,V,C);
    ); 
    if i == 13 then (
        p = for i to 9 list point ringP2;
        v = rationalMap(ringP2,ringP5,gens intersect(intersect(p_{0..8}),p_9^3));
        V = image v;
        C = v arandom({2},intersect(p_0,p_1,p_2,p_9));
        return constrTriple("s",V,C);
    ); 
    if i == 14 then (
        C = curveOnS 2;
        V = arandom({1,2,2},C);
        return (S,V,C);
    ); 
    if i == 15 then (
        p = for i to 6 list point ringP2;
        v = rationalMap(ringP2,ringP5,gens image basis(4,intersect(intersect(p_{0..5}),p_6^2)));
        V = image v;
        C = v arandom({2},intersect(p_0,p_1,p_6));
        return constrTriple("s",V,C);
    );
    if i == 16 then (
        p = for i to 9 list point ringP2;
        v = rationalMap(ringP2,ringP5,(gens image basis(4,intersect p))|matrix{{0}});
        V = image v;
        C = v arandom({2},intersect(p_0,p_1,p_2,p_3,p_4));
        return constrTriple("s",V,C);
    );
    if i == 17 then (
        p = for i to 3 list point ringP2;
        v = rationalMap(ringP2,ringP5,gens image basis(3,intersect p));
        V = image v;
        C = v arandom({1},source v);
        return constrTriple("s",V,C);
    ); 
    if i == 18 then (
        p = for i to 1 list point ringP2;
        v = rationalMap(ringP2,ringP5,gens image basis(3,intersect(p_0,p_1^2)));
        V = image v;
        C = v arandom({2},p_0);
        return constrTriple("v",V,C);
    );
    if i == 19 then (
        V = ideal(x_4^2-x_3*x_5,x_2*x_4-x_1*x_5,x_2*x_3-x_1*x_4,x_2^2-x_0*x_5,x_1*x_2-x_0*x_4,x_1^2-x_0*x_3); -- Veronese surface
        C = trim(V + arandom({1},ringP5));
        return constrTriple("v",V,C);
    ); 
    if i == 20 then (
        V = ideal(x_4^2-x_3*x_5,x_2*x_4-x_1*x_5,x_2*x_3-x_1*x_4,x_2^2-x_0*x_5,x_1*x_2-x_0*x_4,x_1^2-x_0*x_3); -- Veronese surface
        C = trim(V + arandom({1},ringP5));
        return constrTriple("s",V,C);
    ); 
    if i == 21 then (
        p = for i to 8 list point ringP2;
        v = rationalMap(ringP2,ringP5,gens image basis(4,intersect p));
        V = image v;
        C = v arandom({2},intersect(p_0,p_1,p_2,p_3));
        return constrTriple("s",V,C);
    );
);

GMtables (Ring,String) := (K,Name) -> ( -- store all data in a file
ch := char K; if K =!= ZZ/ch then error "expected a finite field";
F := openOut (Name|".dat"); 
F <<"-- file created automatically using: GMtables("|toExternalString K|",\""|Name|"\"); date: "|(toString get "!date")<<endl;
F <<"GMtables (ZZ) := (j) -> ("<<endl;
F << "   x := gens Grass(0,5,ZZ/"|toString(ch)|///,Variable=>"x");///<<endl;
F << ///   cubicScrollSurface := ideal(x_5,x_2*x_3-x_1*x_4,x_1*x_3-x_0*x_4,x_1^2-x_0*x_2);///<<endl;
F << ///   quarticScrollThreefold := ideal(x_0*x_2+x_2*x_4-x_1*x_5-x_2*x_5+x_5^2,x_0*x_1*x_3+x_1*x_3*x_4+x_0*x_1*x_5-x_1*x_2*x_5-x_2^2*x_5-x_0*x_3*x_5+x_2*x_4*x_5-x_3*x_4*x_5-x_0*x_5^2+x_2*x_5^2,x_0^2*x_3+2*x_0*x_3*x_4+x_3*x_4^2+x_0^2*x_5-x_0*x_3*x_5+x_0*x_4*x_5-x_3*x_4*x_5-x_0*x_5^2-x_1*x_5^2-x_2*x_5^2+x_4*x_5^2+x_5^3,x_1*x_2^2+x_2^3-x_1^2*x_3-x_1*x_2*x_3+x_1*x_2*x_4-x_2^2*x_4-x_1^2*x_5-x_1*x_2*x_5-x_2^2*x_5+2*x_1*x_3*x_5+x_2*x_3*x_5-x_2*x_4*x_5+2*x_1*x_5^2+x_2*x_5^2-x_3*x_5^2-x_5^3);///<<endl;
close F;
T := {,"cubicScrollSurface","cubicScrollSurface","cubicScrollSurface","cubicScrollSurface","quarticScrollThreefold","cubicScrollSurface","cubicScrollSurface","quarticScrollThreefold","cubicScrollSurface","quarticScrollThreefold","quarticScrollThreefold","cubicScrollSurface","cubicScrollSurface","cubicScrollSurface","cubicScrollSurface","cubicScrollSurface","cubicScrollSurface","quarticScrollThreefold","quarticScrollThreefold","cubicScrollSurface","cubicScrollSurface"};
inters := {,0,0,0,0,7,0,0,5,0,4,6,0,0,0,0,0,0,3,6,0,0};
degsV :=  {,2,2,8,4,3,1,3,2,1,4,4,8,7,4,6,6,5,4,4,4,7};
degsC :=  {,2,1,4,3,2,1,3,1,1,4,4,4,4,2,4,3,3,5,4,4,4};
local A;
x := gens Grass(0,5,ZZ/ch,Variable=>"x");
for i from 1 to 21 do (
   A = GMtables(i,ZZ/ch);
   if degree A_0 == 3 then (
       assert(A_0 === ideal(x_5,x_2*x_3-x_1*x_4,x_1*x_3-x_0*x_4,x_1^2-x_0*x_2));
       assert(saturate(A_0 + A_1) == A_2);
       assert(degree A_1 == degsV_i and dim A_1 == 3);
       assert(degree A_2 == degsC_i and dim A_2 == 2);
   ) else (
       assert(A_0 === ideal(x_0*x_2+x_2*x_4-x_1*x_5-x_2*x_5+x_5^2,x_0*x_1*x_3+x_1*x_3*x_4+x_0*x_1*x_5-x_1*x_2*x_5-x_2^2*x_5-x_0*x_3*x_5+x_2*x_4*x_5-x_3*x_4*x_5-x_0*x_5^2+x_2*x_5^2,x_0^2*x_3+2*x_0*x_3*x_4+x_3*x_4^2+x_0^2*x_5-x_0*x_3*x_5+x_0*x_4*x_5-x_3*x_4*x_5-x_0*x_5^2-x_1*x_5^2-x_2*x_5^2+x_4*x_5^2+x_5^3,x_1*x_2^2+x_2^3-x_1^2*x_3-x_1*x_2*x_3+x_1*x_2*x_4-x_2^2*x_4-x_1^2*x_5-x_1*x_2*x_5-x_2^2*x_5+2*x_1*x_3*x_5+x_2*x_3*x_5-x_2*x_4*x_5+2*x_1*x_5^2+x_2*x_5^2-x_3*x_5^2-x_5^3));
       assert(degree quotient(A_0+A_1,A_2) == inters_i);
       assert(degree A_1 == degsV_i and dim A_1 == 3);
       assert(degree A_2 == degsC_i and dim A_2 == 2);
   );
   F = openOutAppend (Name|".dat");
   F << "   if j == "<<i<<" then return ("<<T_i<<","<< toString A_1 <<");"<<endl;  
   close F; 
);
F = openOutAppend (Name|".dat"); 
F << ");"<<endl<<endl;
close F;
);

GMtables ZZ := (i) -> (
    if i < 1 or i > 21 then error "expected integer between 1 and 21";
    try value get "data_examples.dat" else error("file \"data_examples.dat\" not found. You can make it using GMtables(K,\"data_examples\")");
    GMtables i
); 

fourfoldFromTriple = method(Options => {InputCheck => 1, Verbose => true});
fourfoldFromTriple (ZZ,VisibleList) := o -> (i,E) -> (
    psi := rationalMap(E_0,Dominant=>2);
    X := specialGushelMukaiFourfold(psi E_1,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
    X.cache#"label" = i;
    return X;
);

GMtables (ZZ,Ring,Nothing) := (i,K,nu) -> (
    E := GMtables(i,K);
    fourfoldFromTriple(i,E)
);

GMtables (ZZ,Nothing) := (i,nu) -> (
    E := GMtables i;
    fourfoldFromTriple(i,E)
);

------------------------------------------------------------------------
---------------- arXiv:2003.07809 --------------------------------------
------------------------------------------------------------------------

exampleD26''data = K -> (
    q := apply(8,i -> point PP_K^2);
    f := rationalMap(2*q_0+q_1+q_2+q_3+q_4+q_5,4);
    f = f * rationalMap point linearSpan {f point source f,f point source f};
    C := random(3,⋃ q);
    g := f|C;
    D := image g;
    pr := inverse parametrize linearSpan D;
    g = g * pr;
    j := toRationalMap inverse rationalMap((linearSpan {g q_6,g q_7})_(image g),Dominant=>true);
    B := image((rationalMap lift(matrix j,ambient source j)) * inverse pr);
    V := image f;
    if not(D == B * V and dim B == 2 and dim V == 2 and dim D == 1 and degree B == 3 and degree V == 7 and degree D == 5 and sectionalGenus B == 0 and sectionalGenus V == 2 and sectionalGenus D == 1) then error "something went wrong";
    psi := rationalMap(B,Dominant=>true);
    (B,V,D,psi,(toRationalMap psi) ideal V)
);

exampleD44data = K -> (
    b := rationalMap(point PP_K^2,2) << PP_K^5;
    v := rationalMap{veronese(2,2,K)};
    V := image v;
    B := ((b random(1,0_(source b))) ===> (v random(1,0_(source v)))) image b;
    C := B * V;
    if not(dim B == 2 and dim V == 2 and dim C == 1 and degree B == 3 and degree V == 4 and degree C == 2 and sectionalGenus B == 0 and sectionalGenus V == 0 and sectionalGenus C == 0) then error "something went wrong";
    psi := rationalMap(B,Dominant=>true);
    (B,V,C,psi,(toRationalMap psi) ideal V)
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
    if d == 2 and g == 0 then X = arandom({2},Grass(0,5,K,Variable=>"x"));
    if d == 3 and g == 1 then X = arandom({3},Grass(0,5,K,Variable=>"x"));
    if d == 4 and g == 1 then X = arandom({2,2},Grass(0,6,K,Variable=>"x"));
    if d == 5 and g == 1 then (
        Y = ideal Grass(1,4,K,Variable=>"x");
        j = parametrize arandom({1,1},ring Y);
        X = j^* Y;
    );
    if d == 4 and g == 3 then X = arandom({4},Grass(0,5,K,Variable=>"x"));
    if d == 6 and g == 4 then X = arandom({2,3},Grass(0,6,K,Variable=>"x"));
    if d == 8 and g == 5 then X = arandom({2,2,2},Grass(0,7,K,Variable=>"x"));
    if d == 10 and g == 6 then (
        Y = ideal Grass(1,4,K,Variable=>"x");
        j = parametrize arandom({1},ring Y);
        X = trim (j^* Y + arandom({2},source j));    
    );
    if d == 12 and g == 7 then (
        S = image rationalMap(Grass(0,2,K),{3,4});
        psi = rationalMap(S + arandom({1},ring S),2);
        j = parametrize arandom({1},target psi);
        X = j^* image(2,psi);
    );
    if d == 14 and g == 8 then (
        Y = ideal Grass(1,5,K,Variable=>"x");
        j = parametrize arandom({1,1,1,1},ring Y);
        X = j^* Y;
    );
    if d == 16 and g == 9 then (
        S = trim(sub(kernel veronese(2,2,K,Variable=>("t","x")),Grass(0,6,K,Variable=>"x")) + ideal last gens Grass(0,6,K,Variable=>"x"));
        psi = rationalMap(S,3,2);
        j = parametrize arandom({1,1},target psi);
        X = j^* image psi;
    );
    if d == 18 and g == 10 then (
        -- p. 4 of [Kapustka and Ranestad - Vector Bundles On Fano Varieties Of Genus Ten] 
        w := gens Grass(0,13,K,Variable=>"x");
        M := matrix {{0,-w_5,w_4,w_6,w_7,w_8,w_0},
                     {w_5,0,-w_3,w_12,w_13,w_9,w_1},
                     {-w_4,w_3,0,w_10,w_11,-w_6-w_13,w_2},
                     {-w_6,-w_12,-w_10,0,w_2,-w_1,w_3},
                     {-w_7,-w_13,-w_11,-w_2,0,w_0,w_4},
                     {-w_8,-w_9,w_6+w_13,w_1,-w_0,0,w_5},
                     {-w_0,-w_1,-w_2,-w_3,-w_4,-w_5,0}};
        Y = pfaffians(4,M);
        j = parametrize arandom({1},ring Y);
        X = j^* Y;
    );
    X = Var sub(X,vars Grass(0,numgens ring X -1,K,Variable=>"x"));
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

arandom = method();
arandom Ideal := I -> (
    if # unique degrees I > 1 then error "expected generators of the same degree";
    K := coefficientRing ring I;
    sum(I_*,i -> (random K) * i)
);
arandom (ZZ,Ideal) := (d,I) -> (
    J := ideal select(I_*,g -> degree g <= {d});
    if numgens J == 0 then J = sub(J,ring I);
    arandom ideal image basis(d,J)
);
arandom (VisibleList,Ideal) := (l,I) -> (
    J := trim ideal for i in (toList l) list arandom(i,I);
    if numgens J == 0 then J = sub(J,ring I);
    if codim J != #l then error "unable to find random elements";
    J
);
arandom (ZZ,PolynomialRing) := (d,R) -> random(d,R);
arandom (VisibleList,PolynomialRing) := (l,R) -> (
    J := trim ideal for i in (toList l) list arandom(i,R);
    if numgens J == 0 then J = sub(J,R);
    if codim J != #l then error "unable to find random elements";
    J
);
arandom (ZZ,QuotientRing) := (d,S) -> (
    R := ambient S;
    if not isPolynomialRing R then error "expected ambient ring to be polynomial";
    sub(random(d,R),S)
);
arandom (VisibleList,QuotientRing) := (l,S) -> (
    R := ambient S;
    if not isPolynomialRing R then error "expected ambient ring to be polynomial";
    sub(arandom(l,R),S)
);

inverse3 = method();
inverse3 RationalMap := psi -> (
    if psi#"inverseRationalMap" =!= null then return psi#"inverseRationalMap";
    phi := rationalMap inverseOfMap(map psi,CheckBirational=>false,AssumeDominant=>true,MinorsCount=>0,Verbose=>false); 
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

interpoleImage = method(Options => {Verbose => false});
interpoleImage (RationalMap,Ideal,List,ZZ) := o -> (g,X,D,j) -> (
    -- Try to return the ideal W generated up to degree j of the image of X via the map g, assuming that "degrees I == D"
    if not all(D,d -> instance(d,ZZ)) then error "expected a list of integers";
    cont := 0;
    W := g point X;
    while select(flatten degrees W,d -> d <= j) =!= D do (
        if o.Verbose then <<cont<<", ";
        W = intersect(W,g point X);
        if o.Verbose then (<<"degrees: ";for l in pairs tally degrees W do (<<(first first l)<<"^"<<last l<<" ");<<endl);
        cont = cont + 1;
    );
    for i to 4 do (
        if o.Verbose then <<"extra "<<i<<", ";
        W = intersect(W,g point X);
        if o.Verbose then (<<"degrees: ";for l in pairs tally degrees W do (<<(first first l)<<"^"<<last l<<" ");<<endl);
    );
    W = ideal select(W_*,w -> first degree w <= j);
    if flatten degrees W =!= D then error "something went wrong";
    W
);
interpoleImage (RationalMap,List,ZZ) := o -> (g,D,j) -> (
    W := interpoleImage(g,ideal source g,D,j,Verbose=>o.Verbose);
    forceImage(g,W);
    W
);

mapDefinedByDivisor = method();
mapDefinedByDivisor (QuotientRing,VisibleList) := (R,D) -> rationalMap(R,new Tally from apply(D,d -> first d => last d));

linearCombination = method();
linearCombination (RingElement,Matrix) := (F,I) -> (
    if not(ring F === ring I and isPolynomialRing ring I and numRows I === 1) then error "internal error encountered";
    K := coefficientRing ring I;
    n := numgens ring I -1;
    m := numColumns I;
    a := local a;
    Ka := K[a_1..a_m];
    x := local x;
    Ra := Ka[x_0..x_n];
    M := (matrix {{sub(F,vars Ra)}}) - ((vars Ka) * transpose sub(I,vars Ra));
    E := trim ideal sub(last coefficients M,Ka);
    H := sub(transpose last coefficients(gens E,Monomials=>((vars Ka)|matrix{{1_Ka}})),K);
    l := flatten entries solve(submatrix'(H,{m}),-submatrix(H,{m}));
    -- Test:
    if F != sum(m,i -> l_i * I_(0,i)) then error "internal error encountered";
    --
    l
);

isSmooth = method(TypicalValue => Boolean); -- sufficient conditions for smoothness ('Y' is assumed to be equidimensional)
isSmooth EmbeddedProjectiveVariety := (cacheValue "isSmooth") (Y -> (
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
    if o.Verbose then <<"-- computing normalization... "<<endl;
    f := rationalMap icMap ring X;
    if o.Verbose then <<"-- got: "|toString(expression f)<<endl;
    X.cache#"Normalization" = f
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
    if dim singLocus grassmannianHull X >= 0 then return X;
    j := toRationalMap toGrass X;
    Y := local Y;
    i := rationalMap(target j,(coefficientRing X)[Y,gens ambient target j],0|vars target j);
    i = rationalMap(i,Dominant=>sub(ideal target j,target i));
    S := trim lift((j*i) ideal surface X,ambient target i);
    Sv := intersect(S,ideal submatrix'(vars ambient target i,{0}));
    try H := arandom({1,1},Sv) else error "not able to specialize to Gushel type";
    h := (parametrize H)||(target i);
    specialGushelMukaiFourfold h^* S
);

< SpecialGushelMukaiFourfold := X -> try toGushel X else error "not able to deform to Gushel type";

imageOfAssociatedMap = method();
imageOfAssociatedMap MutableHashTable := X -> (
    f := map X;
    if f#"idealImage" =!= null then return image f;
    e := if X.cache#?"label" then X.cache#"label" else "not recognized yet";
    if e === "quinticDelPezzoSurface" or e === "quarticScrollSurface" or e === "FarkasVerra" then forceImage(f,image(f,2));
    if e === "C38Coble" or e === "C42" then forceImage(f,image(f,3));
    if instance(e,ZZ) and e >= 1 and e <= 21 and e != 3 and e != 21 then forceImage(f,image(f,2));
    if instance(e,ZZ) and (e == 3 or e == 21) then forceImage(f,trim lift(kernel(map rationalMap(f,Dominant=>2),SubringLimit=>1),ambient target f));
    if e === "gushel26''" then forceImage(f,trim kernel(map f,SubringLimit=>1));
    image f
);

coneOfLines (Ideal,Ideal) := (I,p) -> ideal coneOfLines(Var I,Var p);

mapY5 = memoize (K -> (
    X := Var Grass(1,4,K); 
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
Var List := o -> L -> apply(L,X -> Var(X,MinimalGenerators=>o.MinimalGenerators));

eulerHilbertPol = method();
eulerHilbertPol EmbeddedProjectiveVariety := (cacheValue "eulerHilbertPol") (X -> euler(hilbertPolynomial ideal X));

------------------------------------------------------------------------
---------------------------- Documentation -----------------------------
------------------------------------------------------------------------

beginDocumentation() 

document {Key => SpecialFanoFourfolds, 
Headline => "A package for working with special cubic fourfolds and special Gushel-Mukai fourfolds",
PARA {"This package is still under development, but it already contains several tools related to the rationality problem for cubic fourfolds and Gushel-Mukai fourfolds."},
PARA {"The following are some references that have benefited from this package."},
References => UL{
{"G. S., ",EM"Some new rational Gushel fourfolds",", available at ",HREF{"https://arxiv.org/abs/2003.07809","arXiv:2003.07809"}," (2020)."},
{"G. S., ",EM"On some families of Gushel-Mukai fourfolds",", available at ",HREF{"https://arxiv.org/abs/2002.07026","arXiv:2002.07026"}," (2020)."},
{"M. Hoff and G. S., ",EM"New examples of rational Gushel-Mukai fourfolds",", available at ",HREF{"https://arxiv.org/abs/1910.12838","arXiv:1910.12838"}," (2020)."},
{"F. Russo and G. S., ",EM"Trisecant Flops, their associated K3 surfaces and the rationality of some Fano fourfolds",", available at ",HREF{"https://arxiv.org/abs/1909.01263","arXiv:1909.01263"}," (2020)."},
{"F. Russo and G. S., ",EM"Explicit rationality of some cubic fourfolds",", available at ",HREF{"https://arxiv.org/abs/1811.03502","arXiv:1811.03502"}," (2019)."},
{"F. Russo and G. S., ",EM"Congruences of 5-secant conics and the rationality of some admissible cubic fourfolds",", available at ",HREF{"https://arxiv.org/abs/1707.00999","arXiv:1707.00999"}," (2018)."}}}

document {Key => {SpecialGushelMukaiFourfold}, 
Headline => "the class of all special Gushel-Mukai fourfolds", 
PARA{"The general type of Gushel-Mukai fourfold (called ",EM "ordinary",") can be realized as the intersection of a smooth del Pezzo fivefold ", TEX///$\mathbb{G}(1,4)\cap\mathbb{P}^8\subset \mathbb{P}^8$///, " with a quadric hypersurface in ", TEX///$\mathbb{P}^8$///, ". A Gushel-Mukai fourfold is said to be ", EM"special", " if it contains a surface whose cohomology class ", EM "does not come", " from the Grassmannian ", TEX///$\mathbb{G}(1,4)$///, ". The special Gushel-Mukai fourfolds are parametrized by a countable union of (not necessarily irreducible) hypersurfaces in the corresponding moduli space, labelled by the integers ", TEX///$d \geq 10$///, " with ", TEX///$d = 0, 2, 4\ ({mod}\ 8)$///, "; the number ",TEX///$d$///," is called the discriminant of the fourfold. For precise definition and results, we refer mainly to the paper ", HREF{"https://arxiv.org/abs/1302.1398", "Special prime Fano fourfolds of degree 10 and index 2"}, ", by O. Debarre, A. Iliev, and L. Manivel."}, 
PARA{"An object of the class ", TO SpecialGushelMukaiFourfold, " is basically a couple ", TEX///(S,X)///, ", where ", TEX///$X$///, " is (the ideal of) a Gushel-Mukai fourfold and ", TEX///$S$///, " is (the ideal of) a surface contained in ", TEX///$X$///, ".  The main constructor for the objects of the class is the function ", TO specialGushelMukaiFourfold,", and the discriminant ", TEX///$d$///, " can be calculated by the function ", TO (discriminant,SpecialGushelMukaiFourfold),"."}}  

typValDisc := typicalValues#discriminant;
typicalValues#discriminant = ZZ;

document {Key => {(discriminant, SpecialCubicFourfold)}, 
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

typicalValues#discriminant = typValDisc;

undocumented{(expression, SpecialGushelMukaiFourfold), (describe, SpecialGushelMukaiFourfold)} 

document {Key => {specialGushelMukaiFourfold, (specialGushelMukaiFourfold, EmbeddedProjectiveVariety, EmbeddedProjectiveVariety), (specialGushelMukaiFourfold, Ideal, Ideal), [specialGushelMukaiFourfold, InputCheck], [specialGushelMukaiFourfold, Verbose]}, 
Headline => "make a special Gushel-Mukai fourfold", 
Usage => "specialGushelMukaiFourfold(S,X)", 
Inputs => {"S" => EmbeddedProjectiveVariety => {"a smooth irreducible surface ", TEX///$S\subset\mathbb{P}^8$///}, "X" => EmbeddedProjectiveVariety => {"a smooth prime Fano fourfold ", TEX///$X\subset \mathbb{P}^8$///, " of degree 10 and sectional genus 6, which contains the surface ", TEX///$S$///}}, 
Outputs => {SpecialGushelMukaiFourfold => {"the special Gushel-Mukai fourfold corresponding to the pair ", TEX///$(S,X)$///}}, 
PARA{"In the following example, we define a Gushel-Mukai fourfold containing a so-called ", TEX///$\tau$///, "-quadric."}, 
EXAMPLE {"K = ZZ/33331; x = gens ring PP_K^8;", "S = projectiveVariety ideal(x_6-x_7, x_5, x_3-x_4, x_1, x_0-x_4, x_2*x_7-x_4*x_8);", "X = projectiveVariety ideal(x_4*x_6-x_3*x_7+x_1*x_8, x_4*x_5-x_2*x_7+x_0*x_8, x_3*x_5-x_2*x_6+x_0*x_8+x_1*x_8-x_5*x_8, x_1*x_5-x_0*x_6+x_0*x_7+x_1*x_7-x_5*x_7, x_1*x_2-x_0*x_3+x_0*x_4+x_1*x_4-x_2*x_7+x_0*x_8, x_0^2+x_0*x_1+x_1^2+x_0*x_2+2*x_0*x_3+x_1*x_3+x_2*x_3+x_3^2-x_0*x_4-x_1*x_4-2*x_2*x_4-x_3*x_4-2*x_4^2+x_0*x_5+x_2*x_5+x_5^2+2*x_0*x_6+x_1*x_6+2*x_2*x_6+x_3*x_6+x_5*x_6+x_6^2-3*x_4*x_7+2*x_5*x_7-x_7^2+x_1*x_8+x_3*x_8-3*x_4*x_8+2*x_5*x_8+x_6*x_8-x_7*x_8);", "time F = specialGushelMukaiFourfold(S,X);", "time describe F", "assert(F == X)"}} 

document {Key => {(specialGushelMukaiFourfold, Ideal)}, 
Headline => "random special Gushel-Mukai fourfold", 
Usage => "specialGushelMukaiFourfold I", 
Inputs => {"I" => Ideal => {"the ideal of a smooth irreducible surface in the coordinate ring of a del Pezzo fivefold or del Pezzo sixfold (e.g., an ideal in the ring ", TO Grass, TEX///$(1,4)$///, ")"}}, 
Outputs => {SpecialGushelMukaiFourfold => {"a random special Gushel-Mukai fourfold containing the given surface"}}, 
EXAMPLE {"G = Grass(1,4,ZZ/33331);", "-- cubic scroll in G(1,4)"|newline|"I = schubertCycle({2,0},G) + schubertCycle({1,0},G) + schubertCycle({1,0},G)", "X = specialGushelMukaiFourfold I;", "discriminant X"}, 
SeeAlso => (specialGushelMukaiFourfold, String, Ring)} 

document {Key => {(specialGushelMukaiFourfold, String, Ring), (specialGushelMukaiFourfold, String)}, 
Headline => "random special Gushel-Mukai fourfold of a given type", 
Usage => "specialGushelMukaiFourfold(n,K)
specialGushelMukaiFourfold n", 
Inputs => {"n" => String => {"the name of some known type of Gushel-Mukai fourfolds"}, "K" => {"the coefficient ring"}}, 
Outputs => {SpecialGushelMukaiFourfold => {"a random special Gushel-Mukai fourfold of the indicated type over ",TT"K"}},  
EXAMPLE {"X = specialGushelMukaiFourfold(\"cubic scroll\",ZZ/65521);", "describe X"},
References => UL{
{"O. Debarre, A. Iliev, and L. Manivel, ",EM"Special prime Fano fourfolds of degree 10 and index 2",", available at ",HREF{"https://arxiv.org/abs/1302.1398","arXiv:1302.1398"}," (2014)."},
{"G. S., ",EM"On some families of Gushel-Mukai fourfolds",", available at ",HREF{"https://arxiv.org/abs/2002.07026","arXiv:2002.07026"}," (2020)."}},
SeeAlso => {(specialGushelMukaiFourfold, Ideal), GMtables}}

document {Key => {toGrass, (toGrass, SpecialGushelMukaiFourfold)}, 
Headline => "Gushel morphism from a GM fourfold to Grass(1,4)", 
Usage => "toGrass X", 
Inputs => {"X" => SpecialGushelMukaiFourfold}, 
Outputs => {MultirationalMap => {"a linear morphism from ", TEX///$X$///, " into the Grassmannian ", TEX///$\mathbb{G}(1,4)\subset\mathbb{P}^9$///, ", Plücker embedded, which is an embedding when ",TEX///$X$///," is of ordinary type"}},
EXAMPLE {"x = gens ring PP_(ZZ/33331)^8;", "X = specialGushelMukaiFourfold(ideal(x_6-x_7, x_5, x_3-x_4, x_1, x_0-x_4, x_2*x_7-x_4*x_8), ideal(x_4*x_6-x_3*x_7+x_1*x_8, x_4*x_5-x_2*x_7+x_0*x_8, x_3*x_5-x_2*x_6+x_0*x_8+x_1*x_8-x_5*x_8, x_1*x_5-x_0*x_6+x_0*x_7+x_1*x_7-x_5*x_7, x_1*x_2-x_0*x_3+x_0*x_4+x_1*x_4-x_2*x_7+x_0*x_8, x_0^2+x_0*x_1+x_1^2+x_0*x_2+2*x_0*x_3+x_1*x_3+x_2*x_3+x_3^2-x_0*x_4-x_1*x_4-2*x_2*x_4-x_3*x_4-2*x_4^2+x_0*x_5+x_2*x_5+x_5^2+2*x_0*x_6+x_1*x_6+2*x_2*x_6+x_3*x_6+x_5*x_6+x_6^2-3*x_4*x_7+2*x_5*x_7-x_7^2+x_1*x_8+x_3*x_8-3*x_4*x_8+2*x_5*x_8+x_6*x_8-x_7*x_8));", "time toGrass X", "show oo"}, 
SeeAlso => {(toGrass, EmbeddedProjectiveVariety), (symbol ===>, EmbeddedProjectiveVariety, EmbeddedProjectiveVariety)}} 

document {Key => {(toGrass, EmbeddedProjectiveVariety)}, 
Headline => "embedding of an ordinary Gushel-Mukai fourfold or a del Pezzo variety into Grass(1,4)", 
Usage => "toGrass X", 
Inputs => {"X" => EmbeddedProjectiveVariety => {"an ordinary Gushel-Mukai fourfold, or a del Pezzo variety of dimension at least 4 (e.g., a sixfold projectively equivalent to ", TEX///$\mathbb{G}(1,4)\subset\mathbb{P}^9$///,")"}}, 
Outputs => {MultirationalMap => {"an embedding of ", TEX///$X$///, " into the Grassmannian ", TEX///$\mathbb{G}(1,4)\subset\mathbb{P}^9$///, ", Plücker embedded"}},
EXAMPLE {"x = gens ring PP_(ZZ/33331)^8;", "X = projectiveVariety ideal(x_4*x_6-x_3*x_7+x_1*x_8, x_4*x_5-x_2*x_7+x_0*x_8, x_3*x_5-x_2*x_6+x_0*x_8+x_1*x_8-x_5*x_8, x_1*x_5-x_0*x_6+x_0*x_7+x_1*x_7-x_5*x_7, x_1*x_2-x_0*x_3+x_0*x_4+x_1*x_4-x_2*x_7+x_0*x_8);", "time toGrass X", "show oo"}, 
SeeAlso => {(toGrass,SpecialGushelMukaiFourfold), (symbol ===>, EmbeddedProjectiveVariety, EmbeddedProjectiveVariety)}}

document {Key => {cycleClass, (cycleClass, Ideal)}, 
Headline => "determine the expression of the class of a cycle as a linear combination of Schubert classes", 
Usage => "cycleClass C", 
Inputs => {"C" => Ideal => {"an ideal in ", TO Grass, TEX///$(k, n)$///, " representing a cycle of pure codimension ", TEX///$m$///, " in the Grassmannian of ", TEX///$k$///, "-dimensional subspaces of ", TEX///$\mathbb{P}^n$///}}, 
Outputs => {RingElement => {"the expression of the class of the cycle as a linear combination of Schubert classes"}}, 
PARA{"For the general theory on Chow rings of Grassmannians, see e.g. the book ", HREF{"https://scholar.harvard.edu/files/joeharris/files/000-final-3264.pdf", "3264 & All That - Intersection Theory in Algebraic Geometry"}, ", by D. Eisenbud and J. Harris."}, 
EXAMPLE {"G = Grass(2,5,ZZ/33331);", "C = schubertCycle({3,2,1},G);", "time cycleClass C", "C' = intersect(C,schubertCycle({2,2,2},G));", "time cycleClass C'"}, 
SeeAlso => {schubertCycle}} 

undocumented{(cycleClass, SpecialGushelMukaiFourfold)} 

document {Key => {schubertCycle, (schubertCycle, VisibleList, Ring), (schubertCycle, VisibleList, Ring, String)}, 
Headline => "take a random Schubert cycle", 
Usage => "schubertCycle(a,G)", 
Inputs => {"a" => VisibleList => {"a list of integers ", TEX///$a = (a_0,\ldots,a_k)$///, " with ", TEX///$n-k\geq a_0 \geq \cdots \geq a_k \geq 0$///}, "G" => Ring => {"the coordinate ring ", TO Grass, TEX///$(k,n)$///, " of the Grassmannian of ", TEX///$k$///, "-dimensional subspaces of ", TEX///$\mathbb{P}^n$///}}, 
Outputs => {Ideal => {"the Schubert cycle ", TEX///$\Sigma_a(\mathcal P)\subset\mathbb{G}(k,n)$///, " associated to a random complete flag ", TEX///$\mathcal P$///, " of nested projective subspace ", TEX///$\emptyset\subset P_0\subset \cdots \subset P_{n-1} \subset P_{n} = \mathbb{P}^n$///, " with ", TEX///$dim(P_i)=i$///}}, 
PARA{"For the general theory, see e.g. the book ", HREF{"https://scholar.harvard.edu/files/joeharris/files/000-final-3264.pdf", "3264 & All That - Intersection Theory in Algebraic Geometry"}, ", by D. Eisenbud and J. Harris."}, 
EXAMPLE {"G = Grass(1,5,ZZ/33331,Variable=>\"x\");", "S = schubertCycle({2,1},G)", "cycleClass S"}, 
PARA{"By calling the function as below, it returns as second output an automorphism of the Grassmannian which sends the random Schubert cycle to a standard Schubert cycle."}, 
EXAMPLE {"(S,f) = schubertCycle({2,1},G,\"standard\");", "f;", "S", "f S"}, 
SeeAlso => {cycleClass, (rationalMap, RationalMap, Ring), (schubertCycle, VisibleList, EmbeddedProjectiveVariety)}} 

document {Key => {(schubertCycle, VisibleList, EmbeddedProjectiveVariety), (schubertCycle, VisibleList, EmbeddedProjectiveVariety, String)}, 
Headline => "take a random Schubert cycle", 
Usage => "schubertCycle(a,G)", 
Inputs => {"a" => VisibleList => {"a list of integers ", TEX///$a = (a_0,\ldots,a_k)$///, " with ", TEX///$n-k\geq a_0 \geq \cdots \geq a_k \geq 0$///}, "G" => EmbeddedProjectiveVariety => {"the Grassmannian of ", TEX///$k$///, "-dimensional subspaces of ", TEX///$\mathbb{P}^n$///}}, 
Outputs => {EmbeddedProjectiveVariety => {"the Schubert cycle ", TEX///$\Sigma_a(\mathcal P)\subset\mathbb{G}(k,n)$///, " associated to a random complete flag ", TEX///$\mathcal P$///, " of nested projective subspace ", TEX///$\emptyset\subset P_0\subset \cdots \subset P_{n-1} \subset P_{n} = \mathbb{P}^n$///, " with ", TEX///$dim(P_i)=i$///}}, 
EXAMPLE {"G = projectiveVariety Grass(1,5,ZZ/33331);", "S = schubertCycle({2,1},G);", "cycleClass S", "ideal S"}, 
PARA{"By calling the function as below, it returns as second output an automorphism of the Grassmannian which sends the random Schubert cycle to a standard Schubert cycle."}, 
EXAMPLE {"(S,f) = schubertCycle({2,1},G,\"standard\");", "f;", "ideal S", "ideal f S"}, 
SeeAlso => {(schubertCycle, VisibleList, Ring)}} 

document {Key => {(rationalMap, RationalMap, Ring)}, 
Headline => "induced automorphism of the Grassmannian", 
Usage => "rationalMap(phi,G)", 
Inputs => {"phi" => RationalMap => {"an automorphism of ", TEX///$\mathbb{P}^n$///}, "G" => Ring => {"the coordinate ring ", TO Grass, TEX///$(k,n)$///, " of the Grassmannian of ", TEX///$k$///, "-dimensional subspaces of ", TEX///$\mathbb{P}^n$///}}, 
Outputs => {RationalMap => {"the induced automorphism of ", TO Grass, TEX///$(k,n)$///}}, 
EXAMPLE {"K = ZZ/33331;", "phi = rationalMap apply(5, i -> random(1,ring PP_K^4))", "rationalMap(phi,Grass(1,4,K))"}} 

document {Key => {GMtables, (GMtables, ZZ, Ring), (GMtables, ZZ), (GMtables, ZZ, Ring, Nothing), (GMtables, ZZ, Nothing)}, 
Headline => "make examples of reducible subschemes of P^5", 
Usage => "GMtables(i,K)", 
Inputs => {"i" => ZZ => {"an integer between 1 and 21"}, "K" => Ring => {"the coefficient ring"}}, 
Outputs => {{"a triple of ideals ", TEX///$(B,V,C)$///, ", which represents a reducible subscheme of ", TEX///$\mathbb{P}^5$///, " as indicated in the paper ", HREF{"https://arxiv.org/abs/2002.07026", "On some families of Gushel-Mukai fourfolds"}, "."}}, 
EXAMPLE {"(B,V,C) = GMtables(1,ZZ/33331)", "(?B,?V,?C)", "B + V == C"}, 
PARA{"The corresponding example of fourfold can be obtained as follows."}, 
EXAMPLE {"psi = rationalMap(B,Dominant=>2);", "X = specialGushelMukaiFourfold psi V;"}, 
PARA{"This is basically the same as doing this:"}, 
EXAMPLE {"specialGushelMukaiFourfold(\"1\",ZZ/33331);"},
SeeAlso => (specialGushelMukaiFourfold,String,Ring)} 

undocumented {(GMtables, Ring, String)}; 

document {Key => {parameterCount, (parameterCount, EmbeddedProjectiveVariety, EmbeddedProjectiveVariety), (parameterCount, Ideal, Ideal), [parameterCount, Verbose]}, 
Headline => "count of parameters",
Usage => "parameterCount(S,X)", 
Inputs => {"S" => EmbeddedProjectiveVariety, "X" => EmbeddedProjectiveVariety => {"such that ", TEX///$S\subseteq X$///}}, 
Outputs => {{"a count of parameters to estimate the dimensions of the corresponding Hilbert schemes"}},
PARA{"See ",TO (parameterCount, SpecialCubicFourfold)," and ", TO (parameterCount, SpecialGushelMukaiFourfold)," for more precise applications of this function."},
PARA{"The following calculation shows that the family of complete intersections of 3 quadrics in ",TEX///$\mathbb{P}^5$///," containing a rational normal quintic curve has codimension 1 in the space of all such complete intersections."},
EXAMPLE {"K = ZZ/33331; S = PP_K^(1,5);", "X = random({{2},{2},{2}},S);", "time parameterCount(S,X)"}, 
SeeAlso => {(parameterCount, SpecialCubicFourfold), (parameterCount, SpecialGushelMukaiFourfold), normalSheaf}} 

undocumented {(parameterCount, Ideal, Ideal, Boolean)} 

document {Key => {(parameterCount, SpecialCubicFourfold)}, 
Headline => "count of parameters in the moduli space of GM fourfolds", 
Usage => "parameterCount X", 
Inputs => {"X" => SpecialCubicFourfold => {"a special cubic fourfold containing a surface ", TEX///$S$///}}, 
Outputs => {ZZ => {"an upper bound for the codimension in the moduli space of cubic fourfolds of the locus of cubic fourfolds that contain a surface belonging to the same irreducible component of the Hilbert scheme containing ", TEX///$[S]$///}, Sequence => {"the triple of integers: ", TEX///$(h^0(I_{S/P^5}(3)), h^0(N_{S/P^5}), h^0(N_{S/X}))$///}}, 
PARA{"This function implements a parameter count explained in the paper ", HREF{"https://arxiv.org/abs/1503.05256", "Unirationality of moduli spaces of special cubic fourfolds and K3 surfaces"}, ", by H. Nuer."}, 
PARA{"Below, we show that the closure of the locus of cubic fourfolds containing a Veronese surface has codimension at most one (hence exactly one) in the moduli space of cubic fourfolds. Then, by the computation of the discriminant, we deduce that the cubic fourfolds containing a Veronese surface describe the Hassett's divisor ", TEX///$\mathcal{C}_{20}$///}, 
EXAMPLE {"K = ZZ/33331; V = PP_K^(2,2);", "X = specialCubicFourfold V;", "time parameterCount X", "time discriminant X"}, 
SeeAlso => {(parameterCount, SpecialGushelMukaiFourfold), normalSheaf}} 

document {Key => {(parameterCount, SpecialGushelMukaiFourfold)}, 
Headline => "count of parameters in the moduli space of GM fourfolds", 
Usage => "parameterCount X", 
Inputs => {"X" => SpecialGushelMukaiFourfold => {"a special GM fourfold containing a surface ", TEX///$S$///, " and contained in a del Pezzo fivefold ", TEX///$Y$///}}, 
Outputs => {ZZ => {"an upper bound for the codimension in the moduli space of GM fourfolds of the locus of GM fourfolds that contain a surface belonging to the same irreducible component of the Hilbert scheme of ", TEX///$Y$///, " that contains ", TEX///$[S]$///}, Sequence => {"the triple of integers: ", TEX///$(h^0(I_{S/Y}(2)), h^0(N_{S/Y}), h^0(N_{S/X}))$///}}, 
PARA{"This function implements a parameter count explained in the paper ", HREF{"https://arxiv.org/abs/2002.07026", "On some families of Gushel-Mukai fourfolds"}, "."}, 
PARA{"Below, we show that the closure of the locus of GM fourfolds containing a cubic scroll has codimension at most one (hence exactly one) in the moduli space of GM fourfolds."}, 
EXAMPLE {"G = Grass(1,4,ZZ/33331);", "S = schubertCycle({2,0},G) + ideal(random(1,G), random(1,G))", "X = specialGushelMukaiFourfold S;", "time parameterCount X", "time discriminant X"}, 
SeeAlso => {(parameterCount, SpecialCubicFourfold), normalSheaf}} 

document {Key => {normalSheaf, (normalSheaf, EmbeddedProjectiveVariety), (normalSheaf, EmbeddedProjectiveVariety, EmbeddedProjectiveVariety), (normalSheaf, Ideal), (normalSheaf, Ideal, Ideal)}, 
Headline => "normal sheaf", 
Usage => "normalSheaf X"|newline|"normalSheaf(X,Y)", 
Inputs => {"X" => EmbeddedProjectiveVariety => {"a subvariety ", TEX///$X\subset \mathbb{P}^n$///}, "Y" => EmbeddedProjectiveVariety => {"a subvariety ", TEX///$Y\subset \mathbb{P}^n$///, " such that ", TEX///$X\subset Y$///, " (if not given, it is assumed to be ", TEX///$Y = \mathbb{P}^n$///, ")"}}, 
Outputs => {CoherentSheaf => {"the normal sheaf ", TEX///$\mathcal{N}_{X, Y}$///, " of ", TEX///$X$///, " in ", TEX///$Y$///}}} 

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

document {Key => {detectCongruence}, 
Headline => "detect and return a congruence of secant curves to a surface", 
PARA{"See ",TO (detectCongruence, SpecialCubicFourfold)," and ",TO (detectCongruence, SpecialGushelMukaiFourfold),"."}} 

document {Key => {(detectCongruence, SpecialCubicFourfold, ZZ), (detectCongruence, SpecialCubicFourfold)}, 
Headline => "detect and return a congruence of (3e-1)-secant curves of degree e", 
Usage => "detectCongruence X"|newline|"detectCongruence(X,e)", 
Inputs => {"X" => SpecialCubicFourfold => {"containing a surface ", TEX///$S\subset\mathbb{P}^5$///}, "e" => ZZ => {"a positive integer (optional but recommended)"}}, 
Outputs => {FunctionClosure => {"which takes a (general) point ", TEX///$p\in\mathbb{P}^5$///, " and returns the unique rational curve of degree ", TEX///$e$///, ", ", TEX///$(3e-1)$///, "-secant to ", TEX///$S$///, ", and passing through ", TEX///$p$///, " (an error is thrown if such a curve does not exist or is not unique)"}}, 
EXAMPLE {"-- A general cubic fourfold of discriminant 26"|newline|"X = specialCubicFourfold(\"Farkas-Verra C26\",ZZ/33331);", "describe X", "time f = detectCongruence X;", "p := point ambient X -- random point on P^5", "time C = f p; -- 5-secant conic to the surface", "assert(dim C == 1 and degree C == 2 and dim(C * surface X) == 0 and degree(C * surface X) == 5 and isSubset(p, C))"}, 
SeeAlso => {(detectCongruence, SpecialGushelMukaiFourfold, ZZ), coneOfLines}} 

document {Key => {(detectCongruence, SpecialGushelMukaiFourfold, ZZ), (detectCongruence, SpecialGushelMukaiFourfold)}, 
Headline => "detect and return a congruence of (2e-1)-secant curves of degree e inside a del Pezzo fivefold", 
Usage => "detectCongruence X"|newline|"detectCongruence(X,e)", 
Inputs => {"X" => SpecialGushelMukaiFourfold => {"containing a surface ", TEX///$S\subset Y$///,", where ",TEX///$Y$///," denotes the unique del Pezzo fivefold containing the fourfold ",TEX///$X$///}, "e" => ZZ => {"a positive integer (optional but recommended)"}}, 
Outputs => {FunctionClosure => {"which takes a (general) point ", TEX///$p\in Y$///, " and returns the unique rational curve of degree ", TEX///$e$///, ", ", TEX///$(2e-1)$///, "-secant to ", TEX///$S$///, ", contained in ",TEX///$Y$///," and passing through ", TEX///$p$///, " (an error is thrown if such a curve does not exist or is not unique)"}}, 
EXAMPLE{"-- A GM fourfold of discriminant 20"|newline|"X = specialGushelMukaiFourfold(\"17\",ZZ/33331);", "describe X", "time f = detectCongruence X;", "Y = grassmannianHull X; -- del Pezzo fivefold containing X", "p := point Y -- random point on Y", "time C = f p; -- 3-secant conic to the surface", "S = surface X;", "assert(dim C == 1 and degree C == 2 and dim(C*S) == 0 and degree(C*S) == 3 and isSubset(p,C) and isSubset(C,Y))"}, 
SeeAlso => {(detectCongruence, SpecialCubicFourfold, ZZ), coneOfLines}} 

document {Key => {SpecialCubicFourfold}, 
Headline => "the class of all special cubic fourfolds", 
PARA{"A cubic fourfold is a smooth cubic hypersurface in ", TEX///$\mathbb{P}^5$///, ". A cubic fourfold ", TEX///$X\subset \mathbb{P}^5$///, " is ", EM "special", " of discriminant ", TEX///$d>6$///, " if it contains an algebraic surface ", TEX///$S$///, ", and the discriminant of the saturated lattice spanned by ", TEX///$h^2$///, " and ", TEX///$[S]$///, " in ", TEX///$H^{2,2}(X,\mathbb{Z}):=H^4(X,\mathbb{Z})\cap H^2(\Omega_X^2)$///, " is ", TEX///$d$///, ", where ", TEX///$h$///, " denotes the class of a hyperplane section of ", TEX///$X$///, ". The set ", TEX///$\mathcal{C}_d$///, " of special cubic fourfolds of discriminant ", TEX///$d$///, " is either empty or an irreducible divisor inside the moduli space of cubic fourfolds ", TEX///$\mathcal{C}$///, ". Moreover, ", TEX///$\mathcal{C}_d\neq \emptyset$///, " if and only if ", TEX///$d>6$///, " and ", TEX///$d=$///, "0 or 2 (mod 6). For the general theory, see the papers ", HREF{"https://link.springer.com/article/10.1023/A:1001706324425", "Special cubic fourfolds"}, " and ", HREF{"http://imperium.lenin.ru/~kaledin/math/hasset.pdf", "Some rational cubic fourfolds"}, ", by B. Hassett."}, 
PARA{"An object of the class ", TO SpecialCubicFourfold, " is basically a couple ", TEX///(S,X)///, ", where ", TEX///$X$///, " is (the principal ideal of) a cubic fourfold and ", TEX///$S$///, " is (the ideal of) a surface contained in ", TEX///$X$///, ". The surface ", TEX///$S$///, " is required to be smooth or with at most a finite number ", TEX///$n$///, " of non-normal nodes. This number ", TEX///$n$///, " (if known) can be specified manually using the option ", TT "NumNodes", ". The main constructor for the objects of the class is the function ", TO specialCubicFourfold,", and the discriminant ", TEX///$d$///, " can be calculated by the function ", TO (discriminant,SpecialCubicFourfold),"."}} 

undocumented{(expression, SpecialCubicFourfold), (describe, SpecialCubicFourfold)} 

undocumented{InputCheck, NumNodes}

document {Key => {specialCubicFourfold, (specialCubicFourfold, EmbeddedProjectiveVariety, EmbeddedProjectiveVariety), (specialCubicFourfold, Ideal, Ideal), (specialCubicFourfold, Ideal, RingElement), [specialCubicFourfold, NumNodes], [specialCubicFourfold, InputCheck], [specialCubicFourfold, Verbose]}, 
Headline => "make a special cubic fourfold", 
Usage => "specialCubicFourfold(S,X)"|newline|"specialCubicFourfold(S,X,NumNodes=>n)", 
Inputs => {"S" => EmbeddedProjectiveVariety => {"an irreducible surface ", TEX///$S\subset\mathbb{P}^5$///, ", which has as singularities only a finite number ",TEX///$n\geq 0$///," of non-normal nodes (this number ",TEX///$n$///," should be passed with the option ", TT "NumNodes",", otherwise it is obtained using a probabilistic method)"}, "X" => EmbeddedProjectiveVariety => {"a smooth cubic fourfold ", TEX///$X\subset \mathbb{P}^5$///, " containing the surface ", TEX///$S$///}}, 
Outputs => {SpecialCubicFourfold => {"the special cubic fourfold corresponding to the pair ", TEX///$(S,X)$///}}, 
PARA{"In the example below, we define a cubic fourfold containing a rational scroll of degree 7 with 3 nodes."}, 
EXAMPLE {"K = ZZ/33331; x = gens ring PP_K^5;", "S = projectiveVariety ideal(x_0*x_2*x_3-2*x_1*x_2*x_3-x_1*x_3^2-x_2*x_3^2-x_0*x_1*x_4+2*x_1^2*x_4-x_1*x_2*x_4+x_2^2*x_4+2*x_0*x_3*x_4-x_1*x_3*x_4-x_1*x_4^2+x_1*x_3*x_5, x_1^2*x_3-4*x_1*x_2*x_3-x_0*x_3^2-3*x_1*x_3^2-2*x_2*x_3^2+2*x_0^2*x_4-9*x_0*x_1*x_4+11*x_1^2*x_4-x_0*x_2*x_4-2*x_1*x_2*x_4+2*x_2^2*x_4+12*x_0*x_3*x_4-7*x_1*x_3*x_4-4*x_3^2*x_4+x_0*x_4^2-6*x_1*x_4^2+4*x_2*x_4^2-2*x_3*x_4^2-2*x_4^3-x_0*x_1*x_5+x_1^2*x_5+2*x_1*x_2*x_5+3*x_0*x_3*x_5+2*x_1*x_3*x_5-x_3^2*x_5-x_0*x_4*x_5-4*x_1*x_4*x_5+3*x_2*x_4*x_5+2*x_3*x_4*x_5-x_1*x_5^2, x_0*x_1*x_3-7*x_1*x_2*x_3-3*x_0*x_3^2-4*x_1*x_3^2-3*x_2*x_3^2+x_3^3+3*x_0^2*x_4-14*x_0*x_1*x_4+17*x_1^2*x_4-x_0*x_2*x_4-3*x_1*x_2*x_4+3*x_2^2*x_4+19*x_0*x_3*x_4-9*x_1*x_3*x_4-x_2*x_3*x_4-6*x_3^2*x_4+x_0*x_4^2-9*x_1*x_4^2+6*x_2*x_4^2-3*x_3*x_4^2-3*x_4^3-2*x_0*x_1*x_5+2*x_1^2*x_5+4*x_1*x_2*x_5+5*x_0*x_3*x_5+4*x_1*x_3*x_5-2*x_3^2*x_5-2*x_0*x_4*x_5-7*x_1*x_4*x_5+5*x_2*x_4*x_5+3*x_3*x_4*x_5-2*x_1*x_5^2, x_0^2*x_3-12*x_1*x_2*x_3-6*x_0*x_3^2-6*x_1*x_3^2-5*x_2*x_3^2+2*x_3^3+5*x_0^2*x_4-24*x_0*x_1*x_4+29*x_1^2*x_4-x_0*x_2*x_4-5*x_1*x_2*x_4+5*x_2^2*x_4+32*x_0*x_3*x_4-14*x_1*x_3*x_4-2*x_2*x_3*x_4-10*x_3^2*x_4+x_0*x_4^2-15*x_1*x_4^2+10*x_2*x_4^2-5*x_3*x_4^2-5*x_4^3-3*x_0*x_1*x_5+3*x_1^2*x_5+6*x_1*x_2*x_5+8*x_0*x_3*x_5+7*x_1*x_3*x_5-3*x_3^2*x_5-3*x_0*x_4*x_5-11*x_1*x_4*x_5+8*x_2*x_4*x_5+5*x_3*x_4*x_5-3*x_1*x_5^2, x_1*x_2^2+6*x_1*x_2*x_3+2*x_0*x_3^2+3*x_1*x_3^2+2*x_2*x_3^2-x_3^3-3*x_0^2*x_4+12*x_0*x_1*x_4-14*x_1^2*x_4-2*x_2^2*x_4-15*x_0*x_3*x_4+6*x_1*x_3*x_4+x_2*x_3*x_4+5*x_3^2*x_4+x_0*x_4^2+8*x_1*x_4^2-5*x_2*x_4^2+2*x_3*x_4^2+2*x_4^3+x_0*x_1*x_5-2*x_1^2*x_5-4*x_1*x_2*x_5-4*x_0*x_3*x_5-3*x_1*x_3*x_5+2*x_3^2*x_5+2*x_0*x_4*x_5+7*x_1*x_4*x_5-4*x_2*x_4*x_5-2*x_3*x_4*x_5+2*x_1*x_5^2, x_0*x_2^2+10*x_1*x_2*x_3+3*x_0*x_3^2+5*x_1*x_3^2+4*x_2*x_3^2-x_3^3-5*x_0^2*x_4+19*x_0*x_1*x_4-22*x_1^2*x_4-x_0*x_2*x_4+3*x_1*x_2*x_4-4*x_2^2*x_4-24*x_0*x_3*x_4+9*x_1*x_3*x_4+x_2*x_3*x_4+8*x_3^2*x_4+2*x_0*x_4^2+11*x_1*x_4^2-7*x_2*x_4^2+4*x_3*x_4^2+3*x_4^3+2*x_0*x_1*x_5-4*x_1^2*x_5-7*x_1*x_2*x_5-7*x_0*x_3*x_5-5*x_1*x_3*x_5-x_2*x_3*x_5+3*x_3^2*x_5+4*x_0*x_4*x_5+12*x_1*x_4*x_5-7*x_2*x_4*x_5-3*x_3*x_4*x_5+4*x_1*x_5^2, x_1^2*x_2+17*x_1*x_2*x_3+6*x_0*x_3^2+9*x_1*x_3^2+7*x_2*x_3^2-2*x_3^3-9*x_0^2*x_4+36*x_0*x_1*x_4-44*x_1^2*x_4+3*x_0*x_2*x_4+5*x_1*x_2*x_4-7*x_2^2*x_4-47*x_0*x_3*x_4+21*x_1*x_3*x_4+2*x_2*x_3*x_4+16*x_3^2*x_4+24*x_1*x_4^2-16*x_2*x_4^2+7*x_3*x_4^2+7*x_4^3+3*x_0*x_1*x_5-6*x_1^2*x_5-9*x_1*x_2*x_5-12*x_0*x_3*x_5-8*x_1*x_3*x_5+5*x_3^2*x_5+5*x_0*x_4*x_5+19*x_1*x_4*x_5-12*x_2*x_4*x_5-7*x_3*x_4*x_5+5*x_1*x_5^2, x_0*x_1*x_2+29*x_1*x_2*x_3+11*x_0*x_3^2+15*x_1*x_3^2+12*x_2*x_3^2-4*x_3^3-16*x_0^2*x_4+62*x_0*x_1*x_4-74*x_1^2*x_4+5*x_0*x_2*x_4+9*x_1*x_2*x_4-12*x_2^2*x_4-80*x_0*x_3*x_4+35*x_1*x_3*x_4+4*x_2*x_3*x_4+27*x_3^2*x_4+40*x_1*x_4^2-27*x_2*x_4^2+12*x_3*x_4^2+12*x_4^3+5*x_0*x_1*x_5-10*x_1^2*x_5-16*x_1*x_2*x_5-21*x_0*x_3*x_5-14*x_1*x_3*x_5+9*x_3^2*x_5+9*x_0*x_4*x_5+33*x_1*x_4*x_5-21*x_2*x_4*x_5-12*x_3*x_4*x_5+9*x_1*x_5^2, x_0^2*x_2+49*x_1*x_2*x_3+19*x_0*x_3^2+25*x_1*x_3^2+20*x_2*x_3^2-7*x_3^3-28*x_0^2*x_4+106*x_0*x_1*x_4-124*x_1^2*x_4+8*x_0*x_2*x_4+16*x_1*x_2*x_4-20*x_2^2*x_4-134*x_0*x_3*x_4+58*x_1*x_3*x_4+7*x_2*x_3*x_4+45*x_3^2*x_4+66*x_1*x_4^2-45*x_2*x_4^2+20*x_3*x_4^2+20*x_4^3+9*x_0*x_1*x_5-18*x_1^2*x_5-28*x_1*x_2*x_5-37*x_0*x_3*x_5-23*x_1*x_3*x_5+16*x_3^2*x_5+16*x_0*x_4*x_5+57*x_1*x_4*x_5-36*x_2*x_4*x_5-20*x_3*x_4*x_5+16*x_1*x_5^2, x_1^3+47*x_1*x_2*x_3+18*x_0*x_3^2+23*x_1*x_3^2+19*x_2*x_3^2-7*x_3^3-24*x_0^2*x_4+97*x_0*x_1*x_4-117*x_1^2*x_4+8*x_0*x_2*x_4+16*x_1*x_2*x_4-19*x_2^2*x_4-127*x_0*x_3*x_4+54*x_1*x_3*x_4+7*x_2*x_3*x_4+42*x_3^2*x_4-x_0*x_4^2+62*x_1*x_4^2-42*x_2*x_4^2+19*x_3*x_4^2+19*x_4^3+9*x_0*x_1*x_5-16*x_1^2*x_5-25*x_1*x_2*x_5-33*x_0*x_3*x_5-23*x_1*x_3*x_5+14*x_3^2*x_5+14*x_0*x_4*x_5+51*x_1*x_4*x_5-33*x_2*x_4*x_5-19*x_3*x_4*x_5+14*x_1*x_5^2, x_0*x_1^2+79*x_1*x_2*x_3+29*x_0*x_3^2+40*x_1*x_3^2+32*x_2*x_3^2-11*x_3^3-41*x_0^2*x_4+164*x_0*x_1*x_4-196*x_1^2*x_4+14*x_0*x_2*x_4+26*x_1*x_2*x_4-32*x_2^2*x_4-214*x_0*x_3*x_4+92*x_1*x_3*x_4+11*x_2*x_3*x_4+71*x_3^2*x_4-2*x_0*x_4^2+105*x_1*x_4^2-71*x_2*x_4^2+32*x_3*x_4^2+32*x_4^3+14*x_0*x_1*x_5-26*x_1^2*x_5-41*x_1*x_2*x_5-55*x_0*x_3*x_5-38*x_1*x_3*x_5+23*x_3^2*x_5+23*x_0*x_4*x_5+85*x_1*x_4*x_5-55*x_2*x_4*x_5-32*x_3*x_4*x_5+23*x_1*x_5^2, x_0^2*x_1+133*x_1*x_2*x_3+48*x_0*x_3^2+68*x_1*x_3^2+54*x_2*x_3^2-18*x_3^3-70*x_0^2*x_4+278*x_0*x_1*x_4-330*x_1^2*x_4+24*x_0*x_2*x_4+44*x_1*x_2*x_4-54*x_2^2*x_4-361*x_0*x_3*x_4+156*x_1*x_3*x_4+18*x_2*x_3*x_4+120*x_3^2*x_4-4*x_0*x_4^2+177*x_1*x_4^2-120*x_2*x_4^2+54*x_3*x_4^2+54*x_4^3+23*x_0*x_1*x_5-44*x_1^2*x_5-69*x_1*x_2*x_5-93*x_0*x_3*x_5-63*x_1*x_3*x_5+39*x_3^2*x_5+39*x_0*x_4*x_5+144*x_1*x_4*x_5-93*x_2*x_4*x_5-54*x_3*x_4*x_5+39*x_1*x_5^2, x_0^3+224*x_1*x_2*x_3+80*x_0*x_3^2+115*x_1*x_3^2+91*x_2*x_3^2-30*x_3^3-119*x_0^2*x_4+470*x_0*x_1*x_4-555*x_1^2*x_4+41*x_0*x_2*x_4+75*x_1*x_2*x_4-91*x_2^2*x_4-608*x_0*x_3*x_4+263*x_1*x_3*x_4+30*x_2*x_3*x_4+202*x_3^2*x_4-8*x_0*x_4^2+297*x_1*x_4^2-202*x_2*x_4^2+91*x_3*x_4^2+91*x_4^3+39*x_0*x_1*x_5-76*x_1^2*x_5-118*x_1*x_2*x_5-158*x_0*x_3*x_5-105*x_1*x_3*x_5+67*x_3^2*x_5+68*x_0*x_4*x_5+245*x_1*x_4*x_5-158*x_2*x_4*x_5-91*x_3*x_4*x_5+67*x_1*x_5^2);", "X = projectiveVariety ideal(x_1^2*x_3+x_0*x_2*x_3-6*x_1*x_2*x_3-x_0*x_3^2-4*x_1*x_3^2-3*x_2*x_3^2+2*x_0^2*x_4-10*x_0*x_1*x_4+13*x_1^2*x_4-x_0*x_2*x_4-3*x_1*x_2*x_4+3*x_2^2*x_4+14*x_0*x_3*x_4-8*x_1*x_3*x_4-4*x_3^2*x_4+x_0*x_4^2-7*x_1*x_4^2+4*x_2*x_4^2-2*x_3*x_4^2-2*x_4^3-x_0*x_1*x_5+x_1^2*x_5+2*x_1*x_2*x_5+3*x_0*x_3*x_5+3*x_1*x_3*x_5-x_3^2*x_5-x_0*x_4*x_5-4*x_1*x_4*x_5+3*x_2*x_4*x_5+2*x_3*x_4*x_5-x_1*x_5^2);", "time F = specialCubicFourfold(S,X,NumNodes=>3);", "time describe F", "assert(F == X)"}} 

document {Key => {(specialCubicFourfold, EmbeddedProjectiveVariety), (specialCubicFourfold, Ideal)}, 
Headline => "random special cubic fourfold", 
Usage => "specialCubicFourfold S"|newline|"specialCubicFourfold(S,NumNodes=>n)", 
Inputs => {"S" => EmbeddedProjectiveVariety => {"an irreducible surface in ", TEX///$\mathbb{P}^5$///}}, 
Outputs => {SpecialCubicFourfold => {"a random cubic fourfold containing the given surface"}}, 
EXAMPLE {"-- quintic del Pezzo surface"|newline|"S = projectiveVariety image rationalMap(ring PP_(ZZ/33331)^2,{3,4});", "X = specialCubicFourfold(S,NumNodes=>0);", "discriminant X"}, 
SeeAlso => {(specialCubicFourfold, String, Ring)}} 

document {Key => {(specialCubicFourfold, String, Ring), (specialCubicFourfold, String)}, 
Headline => "random special cubic fourfold of a given type", 
Usage => "specialCubicFourfold(n,K)
specialCubicFourfold n", 
Inputs => {"n" => String => {"the name of some known type of cubic fourfolds"}, "K" => {"the coefficient ring"}}, 
Outputs => {SpecialCubicFourfold => {"a random special cubic fourfold of the indicated type over ",TT"K"}},  
EXAMPLE {"X = specialCubicFourfold(\"Farkas-Verra C26\",ZZ/65521);", "describe X"},
SeeAlso => (specialCubicFourfold, EmbeddedProjectiveVariety)}

document {Key => {coneOfLines, (coneOfLines, EmbeddedProjectiveVariety, EmbeddedProjectiveVariety), (coneOfLines, Ideal, Ideal)}, 
Headline => "cone of lines on a subvariety passing through a point", 
Usage => "coneOfLines(X,p)", 
Inputs => {"X" => EmbeddedProjectiveVariety => {"a subvariety of ", TEX///$\mathbb{P}^n$///}, "p" => EmbeddedProjectiveVariety => {"a point on ", TEX///$X$///}}, 
Outputs => {EmbeddedProjectiveVariety => {"the subscheme of ",TEX///$\mathbb{P}^n$///, " consisting of the union of all lines contained in ",TEX///$X$///, " and passing through ",TEX///$p$///}}, 
PARA{"In the example below we compute the cone of lines passing through the generic point of a smooth del Pezzo fourfold in ",TEX///$\mathbb{P}^7$///, "."}, 
EXAMPLE {"K := frac(QQ[a,b,c,d,e]); t = gens ring PP_K^4; phi = rationalMap {minors(2,matrix{{t_0,t_1,t_2},{t_1,t_2,t_3}}) + t_4};", "X = image phi;", "ideal X", "p := projectiveVariety minors(2,(vars K)||(vars ring PP_K^4))", "time coneOfLines(X,phi p)"}} 

document {Key => {grassmannianHull, (grassmannianHull, SpecialGushelMukaiFourfold)}, 
Headline => "grassmannian hull of a Gushel-Mukai fourfold", 
Usage => "grassmannianHull X", 
Inputs => {"X" => SpecialGushelMukaiFourfold}, 
Outputs => {EmbeddedProjectiveVariety => {"a fivefold ",TEX///$Y\subset\mathbb{P}^8$///," of degree 5 such that ",TEX///$X\subset Y$///," is a quadric section (the fourfold ",TEX///$X$///," is of ordinary type if and only if ",TEX///$Y$///," is smooth)"}},
EXAMPLE {
"X = specialGushelMukaiFourfold(\"21\",ZZ/33331);",
"describe X",
"Y = grassmannianHull X;",
"isSubset(X,Y)",
"Y!"}}

undocumented {(grassmannianHull, EmbeddedProjectiveVariety)}

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

document {Key => {surface, (surface, SpecialCubicFourfold), (surface, SpecialGushelMukaiFourfold)}, 
Headline => "get the special surface contained in the fourfold", 
Usage => "surface X", 
Inputs => {"X" => SpecialCubicFourfold => {"or ", ofClass SpecialGushelMukaiFourfold}}, 
Outputs => {EmbeddedProjectiveVariety => {"the special surface contained in the fourfold ",TT"X"}}, 
EXAMPLE {"X = specialCubicFourfold \"quintic del Pezzo surface\";", "S = surface X;", "assert isSubset(S,X)"}} 

document {Key => {unirationalParametrization, (unirationalParametrization, SpecialCubicFourfold), (unirationalParametrization, SpecialCubicFourfold, EmbeddedProjectiveVariety), (unirationalParametrization, SpecialGushelMukaiFourfold)}, 
Headline => "unirational parametrization", 
Usage => "unirationalParametrization X", 
Inputs => {"X" => SpecialCubicFourfold => {"or ", ofClass SpecialGushelMukaiFourfold}}, 
Outputs => {MultirationalMap => {"a rational map of degree 2 from ",TEX///$\mathbb{P}^4$///," to ",TEX///$X$///}}, 
PARA{"The degree of the forms defining the returned map is 10 in the case of cubic fourfolds, and 26 in the case of GM fourfolds."}, 
EXAMPLE {"K = ZZ/10000019; S = PP_K^(2,2); -- Veronese surface;", "X = specialCubicFourfold S;", "time f = unirationalParametrization X;", "degreeSequence f", "degree(f,Strategy=>\"random point\")"}, 
SeeAlso => {(parametrize, SpecialCubicFourfold), (parametrize, SpecialGushelMukaiFourfold), (parametrize, MultiprojectiveVariety)}} 

document {Key => {(parametrize, SpecialCubicFourfold)}, 
Headline => "rational parametrization", 
Usage => "parametrize X", 
Inputs => {"X" => SpecialCubicFourfold}, 
Outputs => {MultirationalMap => {"a birational map from a rational fourfold to ", TT "X"}}, 
PARA{"Some special cubic fourfolds are known to be rational. In this case, the function tries to obtain a birational map from ", TEX///$\mathbb{P}^4$///, " (or, e.g., from a quadric hypersurface in ", TEX///$\mathbb{P}^5$///, ") to the fourfold."}, 
EXAMPLE {"X = specialCubicFourfold \"quintic del Pezzo surface\";", "time phi = parametrize X;", "describe phi", "describe phi^-1"}, 
SeeAlso => {unirationalParametrization, (parametrize, SpecialGushelMukaiFourfold), (parametrize,MultiprojectiveVariety)}} 

document {Key => {(parametrize, SpecialGushelMukaiFourfold)}, 
Headline => "rational parametrization", 
Usage => "parametrize X", 
Inputs => {"X" => SpecialGushelMukaiFourfold}, 
Outputs => {MultirationalMap => {"a birational map from a rational fourfold to ", TT "X"}}, 
PARA{"Some special GM fourfolds are known to be rational. In this case, the function tries to obtain a birational map from ", TEX///$\mathbb{P}^4$///, " (or, e.g., from a quadric hypersurface in ", TEX///$\mathbb{P}^5$///, ") to the fourfold."}, 
EXAMPLE {"X = specialGushelMukaiFourfold \"tau-quadric\";", "time phi = parametrize X;", "time describe phi"}, 
SeeAlso => {unirationalParametrization, (parametrize, SpecialCubicFourfold), (parametrize,MultiprojectiveVariety)}} 

document {Key => {(symbol <, SpecialGushelMukaiFourfold)},
Headline => "try to deform to a fourfold of Gushel type",
Usage => "< X", 
Inputs => {"X" => SpecialGushelMukaiFourfold => {"a fourfold of ordinary type"}}, 
Outputs => {SpecialGushelMukaiFourfold => {"a fourfold of Gushel type, a deformation of ",TT"X"}}, 
EXAMPLE {"X = specialGushelMukaiFourfold \"quintic del Pezzo surface\";", "singularLocus grassmannianHull X", "time X' = < X;", "decompose singularLocus grassmannianHull X'"}} 

document {Key => {associatedK3surface, [associatedK3surface, Verbose]}, 
Headline => "associated K3 surface to a rational fourfold", 
PARA{"See ",TO (associatedK3surface, SpecialCubicFourfold)," and ",TO (associatedK3surface, SpecialGushelMukaiFourfold),"."}} 

document {Key => {(associatedK3surface, SpecialCubicFourfold)}, 
Headline => "associated K3 surface to a rational cubic fourfold", 
Usage => "associatedK3surface X", 
Inputs => {"X" => SpecialCubicFourfold => {"containing a surface ", TEX///$S\subset\mathbb{P}^5$///," that admits a congruence of ",TEX///$(3e-1)$///,"-secant curves of degree ",TEX///$e$///}}, 
Outputs => {{"the dominant ",TO2{MultirationalMap,"rational map"}," ",TEX///$\mu:\mathbb{P}^5 \dashrightarrow W$///," defined by the linear system of hypersurfaces of degree ",TEX///$3e-1$///," having points of multiplicity ",TEX///$e$///," along ",TEX///$S$///,";"}, {"the ",TO2{EmbeddedProjectiveVariety,"surface"}," ",TEX///$U\subset W$///," determining the inverse map of the restriction of ",TEX///$\mu$///," to ",TEX///$X$///,";"}, {"the ",TO2{List,"list"}," of the exceptional curves on the surface ",TEX///$U$///,";"}, {"a ",TO2{MultirationalMap,"rational map"}," of degree 1 from the surface ",TEX///$U$///," to a minimal K3 surface, the associated K3 surface to ",TEX///$X$///,"."}},
PARA {"Thus, the code ",TT "image last associatedK3surface X"," gives the (minimal) associated K3 surface to ",TT"X",". For more details and notation, see the paper ",HREF{"https://arxiv.org/abs/1909.01263","Trisecant Flops, their associated K3 surfaces and the rationality of some Fano fourfolds"},"."},
EXAMPLE {"X = specialCubicFourfold \"quartic scroll\";", "describe X", "time (mu,U,C,f) = associatedK3surface(X,Verbose=>true);", "? mu", "? U", "first C", "image f"},
SeeAlso => {(associatedK3surface, SpecialGushelMukaiFourfold), detectCongruence}} 

document {Key => {(associatedK3surface, SpecialGushelMukaiFourfold)}, 
Headline => "associated K3 surface to a rational Gushel-Mukai fourfold", 
Usage => "associatedK3surface X", 
Inputs => {"X" => SpecialGushelMukaiFourfold => {"containing a surface ", TEX///$S\subset Y$///," that admits a congruence of ",TEX///$(2e-1)$///,"-secant curves of degree ",TEX///$e$///," inside the unique del Pezzo fivefold ",TEX///$Y$///," containing the fourfold ",TEX///$X$///}}, 
Outputs => {{"the dominant ",TO2{MultirationalMap,"rational map"}," ",TEX///$\mu:Y\dashrightarrow W$///," defined by the linear system of hypersurfaces of degree ",TEX///$2e-1$///," having points of multiplicity ",TEX///$e$///," along ",TEX///$S$///,";"}, {"the ",TO2{EmbeddedProjectiveVariety,"surface"}," ",TEX///$U\subset W$///," determining the inverse map of the restriction of ",TEX///$\mu$///," to ",TEX///$X$///,";"}, {"the ",TO2{List,"list"}," of the exceptional curves on the surface ",TEX///$U$///,";"}, {"a ",TO2{MultirationalMap,"rational map"}," of degree 1 from the surface ",TEX///$U$///," to a minimal K3 surface, the associated K3 surface to ",TEX///$X$///,"."}},
PARA {"Thus, the code ",TT "image last associatedK3surface X"," gives the (minimal) associated K3 surface to ",TT"X",". For more details and notation, see the paper ",HREF{"https://arxiv.org/abs/1909.01263","Trisecant Flops, their associated K3 surfaces and the rationality of some Fano fourfolds"},"."},
EXAMPLE {"X = specialGushelMukaiFourfold \"tau-quadric\";", "describe X", "time (mu,U,C,f) = associatedK3surface X;", "? mu", "? U", "C -- two disjoint lines"},
SeeAlso => {(associatedK3surface, SpecialCubicFourfold), detectCongruence}} 

document {Key => {parametrizeFanoFourfold, (parametrizeFanoFourfold, EmbeddedProjectiveVariety), [parametrizeFanoFourfold,Strategy]}, 
Headline => "rational parametrization of a prime Fano fourfold of coindex at most 3", 
Usage => "parametrize X
parametrizeFanoFourfold(X,Strategy=>...)", 
Inputs => {"X" => EmbeddedProjectiveVariety => {"a prime Fano fourfold ",TEX///$X$///," of coindex at most 3 having degree ",TEX///$d$///," and genus ",TEX///$g$///," with ",TEX///$(d,g)\in\{(2,0),(4,1),(5,1),(12,7),(14,8),(16,9),(18,10)\}$///}}, 
Outputs => {MultirationalMap => {"a birational map from ",TEX///$\mathbb{P}^4$///," to ", TEX///$X$///}}, 
PARA{"This function is mainly based on results contained in the classical paper ",HREF{"https://link.springer.com/article/10.1007/BF02413916","Algebraic varieties with canonical curve sections"},", by L. Roth. In some examples, more strategies are available. For instance, if ",TEX///$X\subset\mathbb{P}^7$///," is a 4-dimensional linear section of ",TEX///$\mathbb{G}(1,4)\subset\mathbb{P}^9$///,", then by passing ",TT"Strategy=>1"," (which is the default choice) we get the inverse of the projection from the plane spanned by a conic contained in ",TEX///$X$///,"; while with ",TT"Strategy=>2"," we get the projection from the unique ",TEX///$\sigma_{2,2}$///,"-plane contained in ",TEX///$X$///," (Todd's result)."},
EXAMPLE {"G'1'4 = projectiveVariety Grass(1,4,ZZ/65521 ); X = G'1'4 * random({{1},{1}},0_G'1'4);","? X", "time parametrizeFanoFourfold X"}, 
SeeAlso => {fanoFourfold,(parametrize,SpecialCubicFourfold),(parametrize,SpecialGushelMukaiFourfold),unirationalParametrization,(parametrize,MultiprojectiveVariety)}} 

document {Key => {fanoFourfold, (fanoFourfold,ZZ,ZZ), [fanoFourfold,CoefficientRing]}, 
Headline => "random prime Fano fourfold of coindex at most 3", 
Usage => "fanoFourfold(d,g)
fanoFourfold(d,g,CoefficientRing=>K)", 
Inputs => {{TT"(d,g)"," a pair of integers belonging to the set ",TEX///$\{(2,0),(3,1),(4,1),(5,1),(4,3),(6,4),(8,5),(10,6),(12,7),(14,8),(16,9),(18,10)\}$///}},
Outputs => {EmbeddedProjectiveVariety => {"a random prime Fano fourfold of coindex at most 3 having degree ",TEX///$d$///," and genus ",TEX///$g$///}},
EXAMPLE {"X = fanoFourfold(4,1);", "describe X", "parametrize X"}, 
SeeAlso => {parametrizeFanoFourfold}}

document { 
Key => {(clean,SpecialCubicFourfold)}, 
Headline => "clean the internal information of a cubic fourfold", 
Usage => "clean X", 
Inputs => {"X" => SpecialCubicFourfold}, 
Outputs => {SpecialCubicFourfold => {"which is mathematically identical to ",TT"X",", but new to the system"}},
PARA{"This function is only useful for testing."},
EXAMPLE {"X = specialCubicFourfold \"quartic scroll\"", "X' = clean X", "X === X'"},
SeeAlso => {(clean,SpecialGushelMukaiFourfold)}}

document { 
Key => {(clean,SpecialGushelMukaiFourfold)}, 
Headline => "clean the internal information of a GM fourfold", 
Usage => "clean X", 
Inputs => {"X" => SpecialGushelMukaiFourfold}, 
Outputs => {SpecialGushelMukaiFourfold => {"which is mathematically identical to ",TT"X",", but new to the system"}},
PARA{"This function is only useful for testing."},
EXAMPLE {"X = specialGushelMukaiFourfold \"cubic scroll\"", "X' = clean X", "X === X'"},
SeeAlso => {(clean,SpecialCubicFourfold)}}

undocumented {(random,SpecialCubicFourfold),(random,SpecialGushelMukaiFourfold)}

------------------------------------------------------------------------
------------------------------- Tests ----------------------------------
------------------------------------------------------------------------

TEST /// -- Test 0 -- cubic fourfolds from strings: describe, discriminant, parameterCount
strIn := {"quintic del Pezzo surface", "quartic scroll", "Farkas-Verra C26", "one-nodal septic del Pezzo surface", "C38", "C42", "C48"};
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
assert all(X,x -> x.cache#?"label");
assert(concatenate apply(X,x -> toString describe x | newline) == strOut);
Y = apply(X,x -> specialCubicFourfold surface x);
assert all(Y,y -> not y.cache#?"label");
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
assert(apply(X,x -> x.cache#"label") == {6, 9, 1});
assert(concatenate apply(X,x -> toString describe x | newline) == strOut);
Y = apply(X,x -> specialGushelMukaiFourfold(sub(ideal (toGrass x) surface x,ring target toGrass x),InputCheck=>0))
assert all(Y,y -> not y.cache#?"label");
assert(apply(Y,discriminant) == {10, 12, 10});
assert(concatenate apply(Y,y -> toString describe y | newline) == strOut);
assert(parameterCount(Y_0,Verbose=>true) == (2, (34, 4, 0)) and parameterCount(Y_1,Verbose=>true) == (3, (34, 3, 0)));
///

TEST /// -- Test 2 (2/2) -- GM fourfolds from strings: describe, discriminant, parameterCount, toGrass
strIn := {"cubic scroll", "quintic del Pezzo surface", "surface of degree 9 and genus 2"};
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
X = apply(strIn,specialGushelMukaiFourfold);
assert(apply(X,x -> x.cache#"label") == {7, 4, 17});
assert(concatenate apply(X,x -> toString describe x | newline) == strOut);
Y = apply(X,x -> specialGushelMukaiFourfold(sub(ideal (toGrass x) surface x,ring target toGrass x),InputCheck=>0))
assert all(Y,y -> not y.cache#?"label");
assert(apply(Y,discriminant) == {12, 10, 20});
assert(concatenate apply(Y,y -> toString describe y | newline) == strOut);
assert(parameterCount(Y_1,Verbose=>true) == (1, (24, 18, 3)));
///

TEST /// -- Test 3 -- 21 examples from GMtables
X = for i from 1 to 21 list (
   A = GMtables(i,ZZ/65521);
   time specialGushelMukaiFourfold((rationalMap(A_0,Dominant=>2)) A_1,InputCheck=>0)
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
X = specialCubicFourfold image multirationalMap rationalMap(ring PP_(ZZ/333331)^2,{3,4});
time h = parametrize X;
assert(degree(h,Strategy=>"random point") == 1 and target h === X and ambient source h == source h and h#"inverse" =!= null);
time f = unirationalParametrization X;
assert(# factor f == 1 and target f === X and unique degrees ideal matrix first factor f == {{10}});
assert isSubset(f point source f,X);
(S,j) = schubertCycle({3,1},Grass(1,4,ZZ/33331),"standard"); S = j S;
Y = specialGushelMukaiFourfold S;
time g = parametrize Y;
assert(degree(g,Strategy=>"random point") == 1 and target g === Y and dim ambient source g == 5 and dim source g == 4 and g#"inverse" =!= null);
-- time g = unirationalParametrization Y;
-- assert(# factor g == 1 and target g === Y and unique degrees ideal matrix first factor g == {{26}})
-- assert isSubset(g point source g,Y)
///

TEST /// -- Test 6 (1/3) -- associated K3 surfaces
f = last associatedK3surface(specialCubicFourfold "quartic scroll",Verbose=>true);
assert(f#"image" =!= null and dim image f == 2 and degree image f == 14 and dim target f == 8)
///

TEST /// -- Test 7 (2/3) -- associated K3 surfaces
g = last associatedK3surface(specialCubicFourfold "quintic del Pezzo surface",Verbose=>true);
assert(g#"image" =!= null and dim image g == 2 and degree image g == 14 and dim target g == 8)
///

TEST /// -- Test 8 (3/3) -- associated K3 surfaces
associatedK3surface(specialGushelMukaiFourfold "tau-quadric",Verbose=>true);
///

TEST /// -- Test 9 -- simple tests on schubertCycle
(S,f) = schubertCycle({2,2},Grass(1,4,ZZ/33331,Variable=>"x"),"standard");
assert(f S == tangentialChowForm(ideal((Grass(0,4,ZZ/33331,Variable=>"x"))_3,(Grass(0,4,ZZ/33331,Variable=>"x"))_4),1,1));
(S,f) = schubertCycle({3,2,1},Grass(2,5,ZZ/33331,Variable=>"x"),"standard");
use ring S;
assert(f S == ideal(x_(3,4,5),x_(2,4,5),x_(1,4,5),x_(0,4,5),x_(2,3,5),x_(1,3,5),x_(0,3,5),x_(1,2,5),x_(0,2,5),x_(0,1,5),x_(2,3,4),x_(1,3,4),x_(0,3,4),x_(1,2,4),x_(1,2,3)));
///

TEST /// -- Test 10 (1/2) -- detectCongruence
X = specialCubicFourfold("quintic del Pezzo surface",ZZ/33331);
detectCongruence X;
///

TEST /// -- Test 11 (2/2) -- detectCongruence
use Grass(1,4,ZZ/33331);
S31 = ideal(p_(3,4),p_(2,4),p_(1,4),p_(0,4),p_(2,3),p_(1,3),p_(1,2));
Y = specialGushelMukaiFourfold(S31,InputCheck=>0);
assert(not Y.cache#?"label"); Y.cache#"label" = 6;
detectCongruence Y;
-- Y = specialGushelMukaiFourfold("18",ZZ/3331);
-- detectCongruence Y;
///

TEST /// -- Test 12 (1/2) -- GM fourfolds containing nodal surfaces
debug SpecialFanoFourfolds;
K = ZZ/65521;
(B,V,C,psi,idS) = exampleD44data K;
X = specialGushelMukaiFourfold(idS,InputCheck=>0);
assert(discriminant X == 36);
assert(numberNodes surface X == 1);
X' = random X;
assert(surface X === surface X' and grassmannianHull X === grassmannianHull X' and isSubset(surface X',X') and dim(X*X') == 3);
assert(discriminant X' == 44 and discriminant X == 44);
///

TEST /// -- Test 13 (2/2) -- GM fourfolds containing nodal surfaces
X = specialGushelMukaiFourfold("nodal D26''",ZZ/33331,InputCheck=>0);
assert(discriminant X == 26 and last cycleClass X == (7,4) and degree surface X == 11 and sectionalGenus surface X == 3);
Y = specialGushelMukaiFourfold("nodal D44",ZZ/33331,InputCheck=>0);
assert(discriminant Y == 44 and last cycleClass Y == (6,3) and degree surface Y == 9 and sectionalGenus surface Y == 1);
///

