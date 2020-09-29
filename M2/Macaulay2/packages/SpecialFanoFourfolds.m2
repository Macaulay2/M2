-- These are the commands to install and view the documentation:
--   installPackage "SpecialFanoFourfolds";
--   viewHelp SpecialFanoFourfolds
----------------------------------------------------------------

if version#"VERSION" < "1.16" then error "this package requires Macaulay2 version 1.16 or newer";

newPackage(
       "SpecialFanoFourfolds",
    	Version => "0.9.3", 
        Date => "September 25, 2020",
    	Authors => {{Name => "Giovanni Staglianò", Email => "giovannistagliano@gmail.com" }},
    	Headline => "special cubic fourfolds and special Gushel-Mukai fourfolds",
        PackageExports => {"Resultants","Cremona"},
    	DebuggingMode => false,
    	Reload => false
	)

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
   "detectCongruence",
   "coneOfLines",
   "ideals",
   "tables",
   "unirationalParametrization",
   "grassmannianHull",
   "InputCheck",
   "associatedK3surface"
}

needsPackage "IntegralClosure"; -- for method: normalization
needsPackage "CharacteristicClasses"; -- for method: eulerCharacteristic
needsPackage("RationalMaps",DebuggingMode=>false); -- for method: inverse2
needsPackage "Resultants";
needsPackage "Cremona";

------------------------------------------------------------------------
--------------------------- Cubic fourfolds ----------------------------
------------------------------------------------------------------------

SpecialCubicFourfold = new Type of MutableHashTable;

globalAssignment SpecialCubicFourfold;

specialCubicFourfold = method(TypicalValue => SpecialCubicFourfold, Options => {NumNodes => null, InputCheck => 1, Verbose => true});

specialCubicFourfold (Ideal,Ideal) := o -> (S,X) -> (
   if ring S =!= ring X then error "expected same ring";
   -- if not isField coefficientRing ring X then error "the coefficient ring needs to be a field";
   ch := char coefficientRing ring X;
   if coefficientRing ring X =!= (if ch == 0 then QQ else ZZ/ch) then error "expected base field to be QQ or ZZ/p";
   S = trim S; X = trim X;
   if not (isPolynomialRing ring X and isHomogeneous X and numgens ring X == 6 and numgens X == 1 and degree X == 3) then error "expected the principal ideal of a cubic fourfold";
   if not(isHomogeneous S and dim S -1 == 2) then error "expected the ideal of a surface";
   i := o.InputCheck;
   try assert(instance(i,ZZ) and i >= -1) else error("option InputCheck expects a nonnegative integer:"|newline|"0: no check is done about the smoothness of the fourfold and of the (normalization of the) surface"|newline|"1: just the smoothness of the fourfold is checked"|newline|"2: the smoothness of the fourfold and of a general hyperplane section of the surface are checked"|newline|"3: as above and furthermore the smoothness of the normalization of the surface is checked");
   if i >= 0 then if not isSubset(X,S) then error "the given surface is not contained in the cubic fourfold";
   if i >= 1 then if not assertSmoothness(X,1) then error "expected a smooth cubic fourfold";
   n := o.NumNodes;
   if n === null then n = numberNodesOfSurface(S,Verbose=>o.Verbose);
   try assert(instance(n,ZZ) and n >= 0) else error "option NumNodes expects a nonnegative integer or null";
   if i == 2 or (i >= 3 and n > 0) then (
       if not assertSmoothness(S+arandom({1},ring S),2) then error "expected a surface with at most a finite number of nodes";
       if o.Verbose then <<"-- smoothness in codimension 1 of the surface verified"<<endl;
   );
   q := null;
   if i >= 3 then (
       if n > 0 then (
           q = normalization(S,Verbose=>o.Verbose);
           if not assertSmoothness(ideal source q) then error "expected a surface with smooth normalization";
           if o.Verbose then <<"-- smoothness of the normalization of the surface verified (assuming equidimensionality)"<<endl;
       ) else (
           if not assertSmoothness S then error "expected a smooth surface (NumNodes=>0)";
           if o.Verbose then <<"-- smoothness of the surface verified (assuming equidimensionality)"<<endl;
       );
   );
   if i >= 4 then (
       if not isEquidimensional S then error "expected an irreducible reduced surface";
       if o.Verbose then <<"-- equidimensionality of the surface verified"<<endl;
   );
   new SpecialCubicFourfold from {
        "idealFourfold" => X,
        "coordinateRing" => quotient X,
        "idealSurface" => S,
        "normalizationSurface" => q,
        "surfaceInvariants" => (degree S,(genera S)_1,euler(hilbertPolynomial S)),
        "eulerCharacteristicSurface" => null,
        "discriminant" => null,
        "NumNodesSurface" => n,
        "map" => null,
        "fanoMap" => null,
        "parameterization" => null,
        "label" => null
   }
);

specialCubicFourfold (Ideal,RingElement) := o -> (S,C) -> specialCubicFourfold(S,ideal C,NumNodes=>o.NumNodes,InputCheck=>o.InputCheck,Verbose=>o.Verbose);

specialCubicFourfold (Ideal) := o -> (S) -> (
    R := ring S;
    if isPolynomialRing R then return specialCubicFourfold(S,arandom({3},S),NumNodes=>o.NumNodes,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
    if not(isPolynomialRing ambient R and isHomogeneous ideal R and numgens ambient R == 6 and degrees ideal R === {{3}}) then error "expected the ideal of a surface in P^5 or in the coordinate ring of a cubic fourfold";
    X := specialCubicFourfold(lift(S,ambient R),ideal R,NumNodes=>o.NumNodes,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
    X#"coordinateRing" = R;
    return X;
);

specialCubicFourfold (String,Ring) := o -> (str,K) -> (
   local X;
   if str === "very general" then (
       X = specialCubicFourfold(arandom({1,1,3},Grass(0,5,K,Variable=>"x")),NumNodes=>0,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
       X#"label" = "very general";
       return X;
   );
   if str === "quintic del Pezzo surface" then (
       X = specialCubicFourfold(image rationalMap(Grass(0,2,K),{3,4}),NumNodes=>0,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
       X#"label" = "quinticDelPezzoSurface";
       return X;
   );
   if str === "quartic scroll" then (
       X = specialCubicFourfold(image rationalMap(Grass(0,2,K),{3,1,1}),NumNodes=>0,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
       X#"label" = "quarticScrollSurface";
       return X;
   );
   if str === "C38" then (
       X = specialCubicFourfold(image rationalMap(Grass(0,2,K),{10,0,0,10}),NumNodes=>0,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
       X#"label" = "C38Coble";
       return X;
   );
   if str === "Farkas-Verra C26" then (
       t := gens Grass(0,2,K);
       f := rationalMap(Grass(0,2,K),Grass(0,8,K),{t_0^5, t_0^4*t_1, t_0^3*t_1^2, t_0^2*t_1^3, t_0^4*t_2, t_0^3*t_1*t_2, t_0^2*t_1^2*t_2, t_0*t_1^3*t_2, t_1^4*t_2});
       f = f * rationalMap(target f,Grass(0,5,K,Variable=>"x"),gens image basis(1,intersect apply(3,i -> (ideal random(1,target f)) + (ideal image basis(1,intersect(f point source f,f point source f))))));
       X = specialCubicFourfold(image f,NumNodes=>3,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
       X#"label" = "FarkasVerra";
       return X;
   );  
   if str === "one-nodal septic del Pezzo surface" then (
       g := rationalMap(Grass(0,2,K),{3,2});
       g = g * rationalMap(target g,Grass(0,5,K,Variable=>"x"),gens image basis(1,intersect(point target g,point ideal image basis(1,intersect(g point source g,g point source g)))));
       X = specialCubicFourfold(image g,NumNodes=>1,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
       X#"label" = "oneNodalSepticDelPezzoSurfaceC26";
       return X;
   );
   if str === "C42" then (
       X = specialCubicFourfold(last last randomS42data(K),NumNodes=>5,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
       X#"label" = "C42";
       return X;
   );
   if str === "C48" then (
       X = specialCubicFourfold(randomS48 K,NumNodes=>6,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
       X#"label" = "C48";
       return X;
   );
   error "not valid string, permitted strings are: \"quintic del Pezzo surface\", \"quartic scroll\", \"Farkas-Verra C26\", \"one-nodal septic del Pezzo surface\", \"C38\", \"C42\", \"C48\"";
);

specialCubicFourfold (String) := o -> (str) -> specialCubicFourfold(str,ZZ/65521,NumNodes=>o.NumNodes,InputCheck=>o.InputCheck,Verbose=>o.Verbose);

expression SpecialCubicFourfold := (X) -> expression("Cubic fourfold containing a surface of degree "|toString(X#"surfaceInvariants"_0)|" and sectional genus "|toString(X#"surfaceInvariants"_1));

hasAttribute = value Core#"private dictionary"#"hasAttribute"
getAttribute = value Core#"private dictionary"#"getAttribute"
ReverseDictionary = value Core#"private dictionary"#"ReverseDictionary"

net SpecialCubicFourfold := (X) -> (
   if hasAttribute(X,ReverseDictionary) then toString getAttribute(X,ReverseDictionary) else "SpecialCubicFourfold"
);

describe SpecialCubicFourfold := (X) -> (
   (d,g,chiOS) := X#"surfaceInvariants";
   degs := flatten degrees first ideals X;
   discrX := discriminant X;
   descr:="Special cubic fourfold of discriminant "|toString(discrX)|newline|"containing a ";
   descr = descr|(if X#"NumNodesSurface" > 0 then toString(X#"NumNodesSurface")|"-nodal " else "(smooth) ");
   descr = descr|"surface of degree "|toString(d)|" and sectional genus "|toString(g)|newline;
   descr = descr|(if # unique degs == 1 then "cut out by "|toString(#degs)|" hypersurfaces of degree "|toString(first degs) else "cut out by "|toString(#degs)|" hypersurfaces of degrees "|toString(toSequence degs));
   net expression descr
);

SpecialCubicFourfold#{Standard,AfterPrint} = SpecialCubicFourfold#{Standard,AfterNoPrint} = (X) -> (
  << endl << concatenate(interpreterDepth:"o") << lineNumber << " : " << class X << " (" << expression X << ")" << endl;
);

ideal SpecialCubicFourfold := (X) -> X#"idealFourfold";

ideals = method();

ideals (SpecialCubicFourfold) := (X) -> (X#"idealSurface",X#"idealFourfold");

ring SpecialCubicFourfold := (X) -> ring ideal X;

coefficientRing SpecialCubicFourfold := (X) -> coefficientRing ring X;

map (SpecialCubicFourfold) := o -> (X) -> (
    if X#"map" =!= null then return X#"map";
    X#"map" = rationalMap(first ideals X,3)
);

recognize = method(); -- try to recognize

recognize (SpecialCubicFourfold) := (X) -> ( 
    if X#"label" =!= null then return X#"label";
    d := discriminant X;
    e := eulerCharacteristic X;
    n := X#"NumNodesSurface";
    invS := X#"surfaceInvariants";
    degs := flatten degrees first ideals X;
    if (d == 14 and e == 7 and n == 0 and invS === (5,1,1) and degs == toList(5:2)) then return X#"label" = "quinticDelPezzoSurface";
    if (d == 14 and e == 4 and n == 0 and invS === (4,0,1) and degs == toList(6:2)) then return X#"label" = "quarticScrollSurface";
    if (d == 38 and e == 13 and n == 0 and invS === (10,6,1) and degs == toList(10:3)) then return X#"label" = "C38Coble";
    if (d == 26 and e == -14 and n == 3 and invS === (7,0,-2) and degs == toList(13:3)) then return X#"label" = "FarkasVerra";
    if (d == 26 and e == -1 and n == 1 and invS === (7,1,0) and degs == toList(14:3)) then return X#"label" = "oneNodalSepticDelPezzoSurfaceC26";
    if (d == 42 and e == -23 and n == 5 and invS === (9,2,-4) and degs == toList(9:3)) then return X#"label" = "C42";
);

fanoMap = method();

fanoMap (SpecialCubicFourfold) := (X) -> (
    if X#"fanoMap" =!= null then return X#"fanoMap";
    recognize X;
    S := first ideals X;
    local mu;
    if X#"label" === "quinticDelPezzoSurface" then (
        mu = rationalMap S;
        forceImage(mu,ideal(0_(target mu)));
        return X#"fanoMap" = mu;
    );
    if X#"label" === "quarticScrollSurface" then (
        mu = rationalMap(S,Dominant=>2);
        forceImage(mu,ideal(0_(target mu)));
        return X#"fanoMap" = mu;
    );
    if X#"label" === "C38Coble" or X#"label" === "FarkasVerra" then (
        mu = rationalMap(S,5,2);
        forceImage(mu,ideal(0_(target mu)));
        return X#"fanoMap" = mu;
    );
    if X#"label" === "oneNodalSepticDelPezzoSurfaceC26" then (
        mu = rationalMap(S,5,2);
        interpoleImage(mu,{2,2,2,2,2},2);
        mu = rationalMap(mu,Dominant=>true);
        return X#"fanoMap" = mu;
    );
    if X#"label" === "C42" then (
        mu = rationalMap(S^3 : ideal first gens ring S,8);
        interpoleImage(mu,{2,2,2,2,2},2);
        mu = rationalMap(mu,Dominant=>true);
        return X#"fanoMap" = mu;
    );
    error "not implemented yet: fourfold not recognized yet or not rational";
);

parametrize (SpecialCubicFourfold) := (X) -> (
    if X#"parameterization" =!= null then return X#"parameterization";
    Psi := fanoMap X;
    X#"parameterization" = inverse2(Psi|X#"coordinateRing")
);

associatedK3surface = method(Options => {Verbose => false});

associatedK3surface (SpecialCubicFourfold) := o -> (X) -> (
    recognize X;
    (S,I) := ideals X;
    local mu; local I2; local U; local U2; local P; local exceptionalLines; local exceptionalConics; local exceptionalQuarticCurve; local f;
    if X#"label" === "quinticDelPezzoSurface" then (
        if o.Verbose then <<"-- computing the map mu from P^5 to P^4 defined by the quadrics through the surface S_14"<<endl;
        mu = fanoMap X;
        if o.Verbose then <<"-- computing the surface U corresponding to the fourfold X"<<endl;
        U = ideal matrix parametrize X;
        I2 = arandom({3},S);
        if o.Verbose then <<"-- computing the surface U' corresponding to another fourfold X'"<<endl;
        U2 = ideal inverse2(mu|I2);
        if o.Verbose then <<"-- computing the 5 exceptional lines on U and U'"<<endl;
        exceptionalLines = decompose top trim(U+U2);
        if o.Verbose then <<"-- computing the map f from U to the minimial K3 surface of degree 14"<<endl;
        f = mapDefinedByDivisor(quotient U,{(arandom({1},ring U),1)}|apply(exceptionalLines,l->(l,1)));
        if numgens target f != 8+1 then error "something went wrong";
        if o.Verbose then <<"-- computing the image of f"<<endl;
        image f;
        return (mu,U,exceptionalLines,f);
    );
    if X#"label" === "quarticScrollSurface" then (
        if o.Verbose then <<"-- computing the map mu from P^5 to P^5 defined by the quadrics through the surface S_14"<<endl;
        mu = fanoMap X;
        if o.Verbose then <<"-- computing the surface U corresponding to the fourfold X"<<endl;
        U = trim lift(ideal matrix parametrize X,ambient target mu);
        I2 = arandom({3},S);
        if o.Verbose then <<"-- computing the surface U' corresponding to another fourfold X'"<<endl;
        U2 = trim lift(ideal matrix inverse2(mu|I2),ambient target mu);
        if o.Verbose then <<"-- computing the exceptional conic on U and U'"<<endl;
        exceptionalConics = {top trim(U+U2)};
        if o.Verbose then <<"-- computing the map f from U to the minimial K3 surface of degree 14"<<endl;
        f = mapDefinedByDivisor(quotient U,{(ideal first gens ring U,1),(first exceptionalConics,2)});
        if numgens target f != 8+1 then error "something went wrong";
        if o.Verbose then <<"-- computing the image of f"<<endl;
        image f;
        return (rationalMap mu,U,exceptionalConics,f);
    );
    if X#"label" === "C38Coble" then (
        if o.Verbose then <<"-- computing the map mu from P^5 to P^4 defined by the quintic hypersurfaces"<<endl;
        if o.Verbose then <<"   with points of multiplicity 2 along the surface S_38"<<endl;
        mu = fanoMap X;
        if o.Verbose then  <<"-- computing the surface U corresponding to the fourfold X"<<endl;
        U = trim ideal apply(9,j -> interpoleImage(mu,I + ideal arandom S,{5},5));
        I2 = ideal arandom S;
        if o.Verbose then <<"-- computing the surface U' corresponding to another fourfold X'"<<endl;
        U2 = trim ideal apply(9,j -> interpoleImage(mu,I2 + ideal arandom S,{5},5));
        if o.Verbose then <<"-- computing the 10 exceptional lines on U and U'"<<endl;
        P = ideal 1; while dim P != 1 or degree P != 10 do P = plucker(trim(U+U2),1); 
        exceptionalLines = apply(decompose trim lift(P,ambient ring P),l -> sub(plucker sub(l,ring P),vars ring U));
        if o.Verbose then <<"-- computing the exceptional quartic curve on U and U'"<<endl;
        exceptionalQuarticCurve = U+U2; 
        for L in exceptionalLines do exceptionalQuarticCurve = quotient(exceptionalQuarticCurve,L); 
        exceptionalQuarticCurve = top exceptionalQuarticCurve;
        if o.Verbose then <<"-- computing the map f from U to the minimial K3 surface of degree 38"<<endl;
        f = mapDefinedByDivisor(quotient U,{(ideal first gens ring U,1)}|apply(exceptionalLines,l->(l,1))|{(exceptionalQuarticCurve,4)});
        if numgens target f != 20+1 then error "something went wrong";
        if o.Verbose then <<"-- computing the image of f"<<endl;
        interpoleImage(f,toList(153:2),2);
        return (mu,U,append(exceptionalLines,exceptionalQuarticCurve),f);
    );
    if X#"label" === "C42" then (
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
        if o.Verbose then <<"-- computing the map f from U to the minimial K3 surface of degree 42"<<endl;
        f = mapDefinedByDivisor(quotient U,{(ideal first gens ring U,1),(exceptionalLines,1),(exceptionalConics,2)});
        if numgens target f != 22+1 then error "something went wrong";
        if o.Verbose then <<"-- computing the image of f"<<endl;
        interpoleImage(f,toList(190:2),2);
        return (mu,U,{exceptionalLines,exceptionalConics},f);
    );
    if X#"label" === "FarkasVerra" then (
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
        if o.Verbose then <<"-- skipping computation of the map f from U to the minimial K3 surface of degree 26"<<endl;
        return (mu,U,{exceptionalQuarticCurve},null);
    );
    if X#"label" === "oneNodalSepticDelPezzoSurfaceC26" then (
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
        if o.Verbose then <<"   of the map f from U to the minimial K3 surface of degree 26"<<endl;
        return (mu,U,{exceptionalCubic},null);
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

parameterCount (SpecialCubicFourfold) := o -> (X) -> parameterCount(X#"idealSurface",X#"idealFourfold",X#"NumNodesSurface" > 0,Verbose=>o.Verbose);

normalSheaf = method(TypicalValue=>CoherentSheaf);

normalSheaf (Ideal) := (I) -> (
     if not isHomogeneous I then error "expected a homogeneous ideal";
     R := (ring I)/I;
     sheaf Hom((module I) ** R,R)
);

normalSheaf (Ideal,Ideal) := (I,J) -> (
      if ring I =!= ring J then error "expected same ring";
      if not isSubset(J,I) then error "inclusion not satisfied";
      normalSheaf sub(I,(ring J)/J)
);

isAdmissible = method();

isAdmissible (ZZ) := (d) -> (
   if d <= 6 then return false;
   if d % 2 != 0 then return false;
   if d % 4 == 0 then return false;
   if d % 9 == 0 then return false;
   for p from 3 to floor(d/2) do if (p % 3 == 2 and isPrime p and d % p == 0) then return false;
   if d % 6 != 0 and d % 6 != 2 then error toString d;
   return true;
);

isAdmissible (SpecialCubicFourfold) := (X) -> isAdmissible discriminant X;

find3Eminus1secantCurveOfDegreeE = method(Options => {Verbose => true})
find3Eminus1secantCurveOfDegreeE (Ideal,SpecialCubicFourfold) := o -> (p,X) -> (
   phi := map X;
   if not(unique degrees p == {{1}} and dim p == 1 and degree p == 1 and ring p === source phi) then error "expected the ideal of a point in the ambient projective space of the cubic fourfold";
   S := X#"idealSurface"; 
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
   T := secantCone(S,p);
   try assert(dim T -1 == 1) else error "expected secant cone to be one dimensional";
   degT := degree T;
   V := coneOfLines(image phi,phi p);
   try assert (dim V -1 == 1) else error "expected cone of lines to be one dimensional";
   degV := degree V;
   if o.Verbose then <<"number lines containing in Z and passing through the point phi(p): "<<degV<<endl;
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

secantCone = method();
secantCone (Ideal,Ideal) := (I,p) -> (
   if ring p =!= ring I then error "expected same ring";
   x := flatten entries coefficients parametrize p;
   try assert(#x == numgens ring I and matrix{x} != 0) else error("expected a point of PP^"|toString(numgens ring I -1));
   PN := ring I;
   N := numgens PN -1;
   K := coefficientRing PN;
   s := local s; t := local t; u := local u; v := local v; 
   R := K[u,v,s_0..s_N,t_0..t_N,MonomialOrder=>Eliminate (N+3)];
   S := (map(R,PN,{s_0..s_N})) I;
   T := (map(R,PN,{t_0..t_N})) I;
   Inc := ideal(sub(matrix{x},R) - u *  matrix{{s_0..s_N}} - v *  matrix{{t_0..t_N}}) + S + T;
   z := local z;
   R = K[u,v,t_0..t_N,z_0..z_N,MonomialOrder=>Eliminate (N+3)];
   Inc = sub(ideal selectInSubring(1,gens gb Inc),R) + ideal(u * sub(matrix{x},R) + v * matrix{{t_0..t_N}} - matrix{{z_0..z_N}});
   R = K[z_0..z_N];
   J := sub(ideal selectInSubring(1,gens gb Inc),R);
   trim (map(PN,R,vars PN)) J
);

detectCongruence = method();

hintCongruence = method();
hintCongruence (MutableHashTable) := (X) -> (
   if X#"label" =!= null then return; 
   if X#"map" =!= null then if (X#"map")#"idealImage" =!= null then return;
   <<///Hint: the current version of the method 'detectCongruence' needs 
of the often long calculation 'image map X', where 'X' is the fourfold.
If you know, for instance, that the image of 'map X' is cut out by quadrics, then 
you can use a command like 'forceImage(map X,image(2,map X))' to inform the system.
This should considerably reduce calculation times.///<<endl<<endl;
);

detectCongruence (SpecialCubicFourfold) := (X) -> (
    hintCongruence X;
    (l,L) := find3Eminus1secantCurveOfDegreeE(point ring X,X,Verbose=>true);
    e := for i to 5 do if l_i == 1 then break (i+1);
    if e === null then error "no congruences detected";
    return detectCongruence(X,e);
);

detectCongruence (SpecialCubicFourfold,ZZ) := (X,e) -> (
   hintCongruence X;
   f := p -> (
      phi := map X;
      S := X#"idealSurface"; 
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
    try f point ring X else error "no congruences detected";
    f
);

unirationalParametrization = method();

unirationalParametrization (SpecialCubicFourfold,Ideal) := (X,L) -> (
   if not(ring L === ring X and degrees L == toList(4:{1}) and isSubset(ideal X,L)) then error "expected a line in the cubic fourfold";
   ringP5 := ring X;
   K := coefficientRing ringP5;
   l := parametrize L;
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
   Psi := parametrize psi;
   --
   try assert isSubset(ideal X,Psi point source Psi) else error "internal error encountered";
   forceImage(Psi,ideal X);
   --
   Psi
);

unirationalParametrization (SpecialCubicFourfold) := (X) -> (
   p := point ideal X;
   L := ideal image basis(1,intersect(p,point coneOfLines(ideal X,p)));
   unirationalParametrization(X,L)
);

randomS42data = method();
randomS42data (Ring) := (K) -> (
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
   e := rationalMap inverse rationalMap trim sub(w,quotient Q);
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
   eta := rationalMap(quotient image psi,source psi,gens randomSigma22OnDelPezzoFivefold(image psi,psi point source psi));
   S42 := eta T;
   --
   try assert(dim S42 -1 == 2 and degree S42 == 9 and genera S42 == {-5, 2, 8} and degrees S42 == toList(9:{3})) else error "internal error encountered";
   -- 
   ((psi,D),(rationalMap inverse eta,S42))
);

randomS48 = method();
randomS48 (Ring) := (K) -> (
   (psi,D) := first randomS42data(K);
   g := rationalMap(target psi,source psi,gens randomSigma31OnDelPezzoFivefold(image psi,psi point source psi));
   (psi * g) D
);

------------------------------------------------------------------------
--------------------------- GM fourfolds -------------------------------
------------------------------------------------------------------------

SpecialGushelMukaiFourfold = new Type of MutableHashTable;

globalAssignment SpecialGushelMukaiFourfold;

specialGushelMukaiFourfold = method(TypicalValue => SpecialGushelMukaiFourfold, Options => {InputCheck => 1, Verbose => true});

specialGushelMukaiFourfold (Ideal,Ideal) := o -> (S,X) -> (
   if ring S =!= ring X then error "expected same ring";
   -- if not isField coefficientRing ring X then error "the coefficient ring needs to be a field";
   ch := char coefficientRing ring X;
   if coefficientRing ring X =!= (if ch == 0 then QQ else ZZ/ch) then error "expected base field to be QQ or ZZ/p";
   S = trim S; X = trim X;
   if not (isPolynomialRing ring X and isHomogeneous X and degrees X === toList(6:{2}) and numgens ring X == 9 and codim X == 4 and degree X == 10 and (genera X)_3 == 6) then error "expected (the ideal of) a 4-dimensional subvariety of PP^8 of degree 10 and sectional genus 6 cut out by 6 quadrics"; 
   if not(isHomogeneous S and dim S -1 == 2) then error "expected the ideal of a surface";
   i := o.InputCheck;
   try assert(instance(i,ZZ) and i >= -1) else error("option InputCheck expects a nonnegative integer:"|newline|"0: no check is done about the smoothness of the fourfold and of the surface"|newline|"1: just the smoothness of the fourfold is checked"|newline|"2: both the smoothness of the fourfold and the smoothness of the surface are checked");
   if i >= 0 then if not isSubset(X,S) then error "the given surface is not contained in the fourfold";
   if i >= 1 then if not assertSmoothness(X,4) then error "expected a smooth GM fourfold";
   if i >= 2 then (
       if not assertSmoothness(S,6) then error "expected a smooth surface";
       if o.Verbose then <<"-- smoothness of the surface verified (assuming equidimensionality)"<<endl;
   );
   if i >= 4 then (
       if not isEquidimensional S then error "expected an irreducible reduced surface";
       if o.Verbose then <<"-- equidimensionality of the surface verified"<<endl;
   );
   Y := varietyDefinedBylinearSyzygies X;
   new SpecialGushelMukaiFourfold from {
        "idealFourfold" => X,
        "coordinateRing" => quotient X,
        "idealSurface" => S,
        "surfaceInvariants" => (degree S,(genera S)_1,euler(hilbertPolynomial S)),
        "eulerCharacteristicSurface" => null,
        "classSurfaceInG14" => null,
        "idealDelPezzoFivefold" => Y,
        "ringDelPezzoFivefold" => null,
        "fromDelPezzoFivefoldToG14" => null,
        "mapToGrass" => null,
        "isGushelType" => null,
        "discriminant" => null,
        "map" => null,
        "fanoMap" => null,
        "parameterization" => null,
        "label" => null
   }
);

specialGushelMukaiFourfold (Ideal) := o -> (S) -> (
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
    local X;
    if r == 4 then (
        X = specialGushelMukaiFourfold(I,ideal R,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        X#"coordinateRing" = R;
        return X;
    );
    if r == 5 then (
        X = specialGushelMukaiFourfold(I,(ideal R) + arandom({2},I),InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        X#"ringDelPezzoFivefold" = R;
        return X;
    );
    if r == 6 then (
        j := embedDelPezzoSixfoldInG14 R;
        I' := trim lift(j S,ambient target j);
        L := ideal image basis(1,I');
        if codim L <= 0 then error "expected linear span of the surface to be of dimension at most 8";
        if codim L >= 2 then L = arandom({1},L);
        l := parametrize L;
        I' = l^* I';
        X = specialGushelMukaiFourfold(I',((map l) ideal target j) + arandom({2},I'),InputCheck=>o.InputCheck,Verbose=>o.Verbose);
        return X;
    );
);

specialGushelMukaiFourfold (String,Ring) := o -> (str,K) -> (
   if char K == 0 then error "the coefficient ring needs to be a finite field";
   G14 := Grass(1,4,K,Variable=>"p");
   local X;
   if str === "very general" then (
       X = specialGushelMukaiFourfold(trim sub(ideal(G14) + arandom({1,1,1,2},ambient G14),G14),InputCheck=>o.InputCheck,Verbose=>o.Verbose);
       X#"label" = "very general";
       return X;
   );
   if str === "sigma-plane" then (
       X = specialGushelMukaiFourfold(schubertCycle({3,1},G14),InputCheck=>o.InputCheck,Verbose=>o.Verbose);
       X#"label" = 6;
       return X;
   );
   if str === "rho-plane" then (
       X = specialGushelMukaiFourfold(schubertCycle({2,2},G14),InputCheck=>o.InputCheck,Verbose=>o.Verbose);
       X#"label" = 9;
       return X;
   );
   if str === "tau-quadric" then (
       X = specialGushelMukaiFourfold(schubertCycle({1,1},G14) + arandom({1,1},G14),InputCheck=>o.InputCheck,Verbose=>o.Verbose);
       X#"label" = 1;
       return X;
   );
   if str === "cubic scroll" then (
       X = specialGushelMukaiFourfold(schubertCycle({2,0},G14) + arandom({1,1},G14),InputCheck=>o.InputCheck,Verbose=>o.Verbose);
       X#"label" = 7; 
       return X;
   );
   if str === "quintic del Pezzo surface" then (
       X = specialGushelMukaiFourfold(trim sub(ideal(G14) + arandom({1,1,1,1},ambient G14),G14),InputCheck=>o.InputCheck,Verbose=>o.Verbose);
       X#"label" = 4;
       return X;
   );
   if str === "quintic" then return specialGushelMukaiFourfold("quintic del Pezzo surface",K,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
   if str === "K3 surface of degree 14" then (
       G15 := Grass replace(1,5,Grass G14);
       pr := rationalMap(G15,G14,select(gens ambient G15,g -> last last baseName g != 5));
       X = specialGushelMukaiFourfold(pr sub(ideal for i to 5 list random(1,ambient G15),G15),InputCheck=>o.InputCheck,Verbose=>o.Verbose);
       X#"label" = 3;
       return X;
   );
   if str === "K3 surface of genus 8" then return specialGushelMukaiFourfold("K3 surface of degree 14",K,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
   if str === "surface of degree 9 and genus 2" then (
       (g,T) := first randomS42data(K);
       X = specialGushelMukaiFourfold((toGrass image g) g T,InputCheck=>o.InputCheck,Verbose=>o.Verbose);
       X#"label" = 17;
       return X;     
   );
   if #str >= 1 and #str <= 2 then try return tables(value str,K,);
   error "not valid string, permitted strings are: \"sigma-plane\", \"rho-plane\", \"tau-quadric\", \"cubic scroll\", \"quintic del Pezzo surface\", \"K3 surface of degree 14\", \"surface of degree 9 and genus 2\", \"1\",...,\"21\"";
);

specialGushelMukaiFourfold (String) := o -> (str) -> specialGushelMukaiFourfold(str,ZZ/65521,InputCheck=>o.InputCheck,Verbose=>o.Verbose);

expression SpecialGushelMukaiFourfold := (X) -> expression("Gushel-Mukai fourfold containing a surface of degree "|toString(X#"surfaceInvariants"_0)|" and sectional genus "|toString(X#"surfaceInvariants"_1)|(if X#"classSurfaceInG14" =!= null then (", with class "|toString(X#"classSurfaceInG14")|" in G(1,4)") else ""));

net SpecialGushelMukaiFourfold := (X) -> (
   if hasAttribute(X,ReverseDictionary) then toString getAttribute(X,ReverseDictionary) else "SpecialGushelMukaiFourfold"
);

describe SpecialGushelMukaiFourfold := (X) -> (
   (d,g,chiOS) := X#"surfaceInvariants";
   degs := flatten degrees first ideals X;
   cS := cycleClass X;
   (a,b) := toSequence flatten entries lift(transpose last coefficients(cS,Monomials=>vars ring cS),ZZ);
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
   descr = descr|newline|"containing a surface in PP^8 of degree "|toString(d)|" and sectional genus "|toString(g)|newline;
   descr = descr|(if # unique degs == 1 then "cut out by "|toString(#degs)|" hypersurfaces of degree "|toString(first degs) else "cut out by "|toString(#degs)|" hypersurfaces of degrees "|toString(toSequence degs));
   descr = descr|newline|"and with class in G(1,4) given by "|toString(cS);
   if X#"isGushelType" =!= null then (if X#"isGushelType" then descr = descr|newline|"Type: Gushel (not ordinary)" else descr = descr|newline|"Type: ordinary");
   if instance(recognize(X,discrX,eulerCharacteristic X,(a,b)),ZZ) then descr = descr|newline|"(case "|toString(recognize X)|" of Table 1 in "|"arXiv:2002.07026"|")";
   net expression descr
);

SpecialGushelMukaiFourfold#{Standard,AfterPrint} = SpecialGushelMukaiFourfold#{Standard,AfterNoPrint} = (X) -> (
  << endl << concatenate(interpreterDepth:"o") << lineNumber << " : " << class X << " (" << expression X << ")" << endl;
);

ideal SpecialGushelMukaiFourfold := (X) -> X#"idealFourfold";

ideals (SpecialGushelMukaiFourfold) := (X) -> (X#"idealSurface",X#"idealFourfold");

ring SpecialGushelMukaiFourfold := (X) -> ring ideal X;

coefficientRing SpecialGushelMukaiFourfold := (X) -> coefficientRing ring X;

grassmannianHull = method();
grassmannianHull SpecialGushelMukaiFourfold := (X) -> X#"idealDelPezzoFivefold";

map (SpecialGushelMukaiFourfold) := o -> (X) -> (
    if X#"map" =!= null then return X#"map";
    -- X#"map" = rationalMap(trim sub(first ideals X,ringDP5 X),2);
    S := first ideals X;
    J1 := select(S_*,s -> degree s == {1});
    if #J1 > 0 then J1 =  (ideal J1) * (ideal vars ring S) else J1 = ideal ring S;
    J2 := select(S_*,s -> degree s == {2});
    if #J2 > 0 then J2 =  ideal J2 else J2 = ideal ring S;
    X#"map" = rationalMap trim sub(J1+J2,ringDP5 X)
);

recognize (SpecialGushelMukaiFourfold) := (X) -> ( 
    if X#"label" =!= null then return X#"label";
    d := discriminant X;
    e := eulerCharacteristic X;
    cS := cycleClass X;
    (a,b) := toSequence flatten entries lift(transpose last coefficients(cS,Monomials=>vars ring cS),ZZ);
    recognize(X,d,e,(a,b))
);

recognize (SpecialGushelMukaiFourfold,ZZ,ZZ,Sequence) := (X,d,e,ab) -> ( 
    if X#"label" =!= null then return X#"label";
    (a,b) := ab;
    invS := X#"surfaceInvariants";
    degs := flatten degrees first ideals X;
    if (d == 10 and e == 4 and a == 1 and b == 1 and invS == (2,0,1) and degs == {1, 1, 1, 1, 1, 2}) then return X#"label" = 1;
    if (d == 10 and e == 4 and a == 3 and b == 1 and invS == (4,0,1) and degs == {1, 1, 1, 2, 2, 2, 2, 2, 2}) then return X#"label" = 2;
    if (d == 10 and e == 24 and a == 9 and b == 5 and invS == (14,8,2) and degs == {2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}) then return X#"label" = 3;
    if (d == 10 and e == 7 and a == 3 and b == 2 and invS == (5,1,1) and degs == {1, 1, 1, 2, 2, 2, 2, 2}) then return X#"label" = 4;
    if (d == 10 and e == 11 and a == 5 and b == 4 and invS == (9,3,1) and degs == {1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}) then return X#"label" = 5;
    if (d == 10 and e == 3 and a == 1 and b == 0 and invS == (1,0,1) and degs == {1, 1, 1, 1, 1, 1}) then return X#"label" = 6;
    if (d == 12 and e == 4 and a == 2 and b == 1 and invS == (3,0,1) and degs == {1, 1, 1, 1, 2, 2, 2}) then return X#"label" = 7;
    if (d == 12 and e == 9 and a == 4 and b == 3 and invS == (7,2,1) and degs == {1, 1, 2, 2, 2, 2, 2, 2, 2, 2}) then return X#"label" = 8;
    if (d == 12 and e == 3 and a == 0 and b == 1 and invS == (1,0,1) and degs == {1, 1, 1, 1, 1, 1}) then return X#"label" = 9;
    if (d == 16 and e == 12 and a == 6 and b == 4 and invS == (10,4,1) and degs == {1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}) then return X#"label" = 10;
    if (d == 16 and e == 10 and a == 6 and b == 4 and invS == (10,3,1) and degs == {2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}) then return X#"label" = 11;
    if (d == 16 and e == 24 and a == 8 and b == 6 and invS == (14,8,2) and degs == {2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}) then return X#"label" = 12;
    if (d == 18 and e == 13 and a == 7 and b == 5 and invS == (12,5,1) and degs == {2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}) then return X#"label" = 13;
    if (d == 18 and e == 8 and a == 5 and b == 3 and invS == (8,2,1) and degs == {1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}) then return X#"label" = 14;
    if (d == 18 and e == 10 and a == 5 and b == 4 and invS == (9,3,1) and degs == {1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}) then return X#"label" = 15;
    if (d == 18 and e == 13 and a == 7 and b == 4 and invS == (11,5,1) and degs == {1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}) then return X#"label" = 16;
    if (d == 20 and e == 7 and a == 6 and b == 3 and invS == (9,2,1) and degs == {2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}) then return X#"label" = 17;
    if (d == 20 and e == 4 and a == 4 and b == 3 and invS == (7,0,1) and degs == {2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}) then return X#"label" = 18;
    if (d == 24 and e == 9 and a == 6 and b == 4 and invS == (10,3,1) and degs == {2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}) then return X#"label" = 19;
    if (d == 24 and e == 3 and a == 2 and b == 2 and invS == (4,0,1) and degs == {1, 1, 1, 2, 2, 2, 2, 2, 2}) then return X#"label" = 20;
    if (d == 26 and e == 12 and a == 7 and b == 5 and invS == (12,5,1) and degs == {2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}) then return X#"label" = 21;
    --
    if (d == 26 and e == 25 and a == 11 and b == 6 and invS == (17,11,2) and degs == {2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}) then return X#"label" = "gushel26''";
);

fanoMap (SpecialGushelMukaiFourfold) := (X) -> (
    if X#"fanoMap" =!= null then return X#"fanoMap";
    recognize X;
    S := trim sub(first ideals X,ringDP5 X);
    local mu;
    if X#"label" === 1 then (
        mu = rationalMap(S,1);
        forceImage(mu,ideal(0_(target mu)));
        return X#"fanoMap" = mu;
    );
    if X#"label" === 3 then (
        mu = rationalMap(S^3 : ideal first gens ring S,5);
        interpoleImage(mu,{3},3);
        mu = rationalMap(mu,Dominant=>true);
        return X#"fanoMap" = mu;
    );
    if X#"label" === 6 then (
        mu = rationalMap(S,1,Dominant=>true);
        return X#"fanoMap" = mu;
    );
    if X#"label" === 17 then (
        mu = rationalMap(S^2 : ideal first gens ring S,3);
        forceImage(mu,ideal(0_(target mu)));
        return X#"fanoMap" = mu;
    );
    error "not implemented yet: fourfold not recognized yet or not rational";
);

parametrize (SpecialGushelMukaiFourfold) := (X) -> (
    if X#"parameterization" =!= null then return X#"parameterization";
    Psi := fanoMap X;
    X#"parameterization" = inverse2(Psi|X#"coordinateRing")
);

associatedK3surface (SpecialGushelMukaiFourfold) := o -> (X) -> (
    recognize X;
    (S,I) := ideals X;
    local mu; local I2; local U; local U2; local f;
    if X#"label" === 1 then (
        if o.Verbose then <<"-- computing the map mu from the fivefold in P^8 to P^4 defined by the hypersurfaces"<<endl;
        if o.Verbose then <<"   of degree 1 through the tau quadric surface"<<endl;
        mu = fanoMap X;
        if o.Verbose then <<"-- computing the surface U corresponding to the fourfold X"<<endl;
        U = ideal matrix parametrize X;
        I2 = trim((ideal source mu) + (ideal arandom(2,S)));
        if o.Verbose then <<"-- computing the surface U' corresponding to another fourfold X'"<<endl;
        U2 = ideal matrix inverse2(mu|I2);
        if o.Verbose then <<"-- computing the two exceptional lines"<<endl;
        P := ideal 1; while dim P != 1 or degree P != 2 do P = plucker(trim(U+U2),1); 
        exceptionalLines := apply(decompose trim lift(P,ambient ring P),l -> sub(plucker sub(l,ring P),vars ring U));
        if not((# exceptionalLines == 1 and dim first exceptionalLines == 2 and degree first exceptionalLines == 2 and flatten degrees first exceptionalLines == {1,2,2,2,2}) or (# exceptionalLines == 2 and all(exceptionalLines,l->dim l == 2 and degree l == 1 and flatten degrees l == {1,1,1}))) then error "something went wrong";
        if o.Verbose then <<"-- skipping computation of the map f from U to the minimial K3 surface of degree 10"<<endl;
        return (mu,U,exceptionalLines,null);
    );  
    if X#"label" === 3 then (
        if o.Verbose then <<"-- computing the map mu from the fivefold in P^8 to P^5 defined by the hypersurfaces"<<endl;
        if o.Verbose then <<"   of degree 5 with points of multiplicity 3 along the surface S of degree 14 and genus 8"<<endl;
        mu = rationalMap fanoMap X;
        if o.Verbose then <<"-- computing the surface U corresponding to the fourfold X"<<endl;
        U = trim ideal apply(9,j -> interpoleImage(mu,I + ideal arandom S,{3,3},3));
        if o.Verbose then <<"-- computing the normalization of U"<<endl;
        normU := normalization(quotient U,Verbose=>false);
        if o.Verbose then <<"-- inverting the normalization of U"<<endl;
        f = inverse2 normU;
        return (mu,U,{},f);
    );
    if X#"label" === 17 then (
        if o.Verbose then <<"-- computing the map mu from the fivefold in P^8 to P^4 defined by the hypersurfaces"<<endl;
        if o.Verbose then <<"   of degree 3 with points of multiplicity 2 along the surface S of degree 9 and genus 2"<<endl;
        mu = fanoMap X;
        if o.Verbose then <<"-- computing the surface U corresponding to the fourfold X"<<endl;
        U = trim ideal apply(13,j -> interpoleImage(mu,I + ideal arandom S,{5},5));
        I2 = trim((ideal source mu) + (ideal arandom S));
        if o.Verbose then <<"-- computing the surface U' corresponding to another fourfold X'"<<endl;
        U2 = trim ideal apply(13,j -> interpoleImage(mu,I2 + ideal arandom S,{5},5));
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
        if o.Verbose then <<"-- skipping computation of the map f from U to the minimial K3 surface of degree 20"<<endl;
        return (mu,U,{L,C},null);
    );
    error "not implemented yet: fourfold not recognized yet or not rational";
);

fromDP5toG14 = method();
fromDP5toG14 (SpecialGushelMukaiFourfold) := (X) -> (
   if X#"fromDelPezzoFivefoldToG14" === null then (
       Y := grassmannianHull X;
       local f;
       try (
          f = embedDelPezzoFivefoldInG14 Y;
          X#"isGushelType" = false;
       ) else try(
            q := trim(Y + minors(3,jacobian Y,Strategy=>Cofactor));
            assert(dim q == 1);
            f = embedConeOverDelPezzoFourfoldInConeOverG14(Y,radical q);
            f = f * rationalMap(target f,Grass(1,4,coefficientRing X,Variable=>ring X),submatrix'(vars ambient target f,{0}));
            X#"isGushelType" = true;
       ) else error "error occurred trying to embed into G(1,4)";
       if X#"ringDelPezzoFivefold" =!= null then f = rationalMap(X#"ringDelPezzoFivefold",source f) * f else X#"ringDelPezzoFivefold" = source f;
       X#"fromDelPezzoFivefoldToG14" = f;
   );
   return X#"fromDelPezzoFivefoldToG14";
);

ringDP5 = method();
ringDP5 (SpecialGushelMukaiFourfold) := (X) -> (
   if X#"ringDelPezzoFivefold" === null then X#"ringDelPezzoFivefold" = quotient grassmannianHull X;
   X#"ringDelPezzoFivefold"
);

pointOnDP5 = method();
pointOnDP5 (Ideal) := (Y) -> (
   -- if char ring Y > 0 then return point Y;
   j := parametrize arandom({1,1,1},ring Y);
   S := trim (map j) Y;
   T := quotient(arandom({2,2,2},S),S);
   i := parametrize ideal image basis(1,T);
   T = trim (map i) T;
   L := i dualVariety top ideal jacobian dualVariety T;
   p := saturate(L + S);
   j p
);

toGrass = method(TypicalValue => RationalMap)

toGrass (SpecialGushelMukaiFourfold) := (X) -> (
   if X#"mapToGrass" =!= null then return X#"mapToGrass";
   X#"mapToGrass" = (fromDP5toG14 X)|(X#"coordinateRing")
);

toGrass (Ideal) := (X) -> (
   r := dim X -1;
   if r == 6 and numgens ring X -1 == 9 and degrees X == toList(5:{2}) then return embedDelPezzoSixfoldInG14 X;
   if r == 5 and numgens ring X -1 == 8 and degrees X == toList(5:{2}) then return embedDelPezzoFivefoldInG14 X;
   if r == 4 and numgens ring X -1 == 7 and degrees X == toList(5:{2}) then return embedDelPezzoFourfoldInG14 X;
   if r == 4 and numgens ring X -1 == 8 and degrees X == toList(6:{2}) then return (embedDelPezzoFivefoldInG14(varietyDefinedBylinearSyzygies X))|X;
   error "expected the ideal of a Gushel-Mukai fourfold, or of a del Pezzo fourfold/fivefold/sixfold";
);

find2Eminus1secantCurveOfDegreeE = method(Options => {Verbose => true})
find2Eminus1secantCurveOfDegreeE (Ideal,SpecialGushelMukaiFourfold) := o -> (p,X) -> (
   phi := map X;
   Y := ideal source phi;
   if not(unique degrees p == {{1}} and dim p == 1 and degree p == 1 and ring p === ambient source phi and isSubset(Y,p)) then error "expected the ideal of a point in P^8 contained in the del Pezzo fivefold";
   S := X#"idealSurface"; 
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
       if o.Verbose then <<"number lines containing in Z and passing through the point phi(p): "<<0<<endl;
       return ({lines1secant,conics3secant,cubics5secant,quartics7secant,quintics9secant,sectics11secant},{Lines1secant,Conics3secant,Cubics5secant,Quartics7secant,Quintics9secant,Sectics11secant});
   );
   try assert (dim E -1 == 1) else error "expected cone of lines to be one dimensional";
   degE := degree E;
   if o.Verbose then <<"number lines containing in Z and passing through the point phi(p): "<<degE<<endl;
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

detectCongruence (SpecialGushelMukaiFourfold) := (X) -> (
    hintCongruence X;
    (l,L) := find2Eminus1secantCurveOfDegreeE(pointOnDP5 ideal source map X,X,Verbose=>true);
    e := for i to 5 do if l_i == 1 then break (i+1);
    if e === null then error "no congruences detected";
    return detectCongruence(X,e);
);

detectCongruence (SpecialGushelMukaiFourfold,ZZ) := (X,e) -> (
   hintCongruence X;
   f := p -> (
      phi := map X;
      S := X#"idealSurface"; 
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
    try f (pointOnDP5 ideal source map X) else error "no congruences detected";
    f
);

parameterCount (SpecialGushelMukaiFourfold) := o -> (X) -> (
   (S,G) := ideals X;
   Y := grassmannianHull X;
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
sigmaQuadric (SpecialGushelMukaiFourfold) := (X) -> (
   f := toGrass X;
   H := image(f,1);
   Q := arandom({2},image f);
   S := trim(Q + tangentialChowForm(chowEquations(H_0),0,1));
   return trim lift(f^* S,ambient source f);
);

unirationalParametrization (SpecialGushelMukaiFourfold) := (X) -> (
   K := coefficientRing X;
   if K =!= ZZ/(char K) then error "not implemented yet: unirational parametrization of GM fourfolds over a non-finite field";
   S := sigmaQuadric X;
   s := parametrize S;
   s = rationalMap(Grass(0,2,K,Variable=>"u"),source s) * s;
   K' := frac(K[gens source s]);
   ringP8' := K'[gens ring X];
   p' := trim minors(2,vars ringP8' || sub(matrix s,K'));
   Y := grassmannianHull X;
   V := trim coneOfLines(sub(Y,ringP8'),p');
   j := parametrize((ideal select(V_*,v -> degree v == {1})) + (ideal first gens ring V));
   W := j^* V;
   P := plucker(W,2); while dim P <= 0 do P = plucker(W,2); P = trim sub(plucker P,vars ring W);
   Q := trim quotient(W,P);
   q := trim minors(2,vars ring W || transpose submatrix(coefficients parametrize(P+Q),,{0}));
   f := (inverse rationalMap trim sub(q,quotient Q)) * j;
   ringP2xP2 := (source s) ** K[gens source f];
   K'' := frac(ringP2xP2);
   ringP8'' := K''[gens ring X];
   X'' := sub(ideal X,ringP8'');
   p'' := sub(p',ringP8'');
   ringP1'' := Grass(0,1,K'',Variable=>"v");
   l := rationalMap(ringP1'',ringP8'', (vars ringP1'') * (sub(matrix s,K'') || sub(matrix f,K'')));
   e := parametrize trim quotient(trim (map l) X'',trim (map l) p'');
   el := rationalMap((map e) * (map l));
   el = rationalMap(source el,target el,(lcm apply(flatten entries sub(last coefficients matrix el,K''),denominator)) * (matrix el));
   psi := rationalMap(ringP2xP2,ring X,sub(transpose coefficients el,ringP2xP2));
   Psi := parametrize psi;
   --
   try assert isSubset(ideal X,Psi point source Psi) else error "internal error encountered";
   forceImage(Psi,ideal X);
   --
   Psi
);

------------------------------------------------------------------------
---------------------- Parameterizations of G(1,4) ---------------------
------------------------------------------------------------------------

coneOfLines = method(TypicalValue => Ideal)

coneOfLines (Ideal,Ideal) := (I,p) -> (
   if not isPolynomialRing ring I then error "expected ideal in a polynomial ring";
   if not isHomogeneous I then error "expected a homogeneous ideal";
   if not isField coefficientRing ring I then error "the coefficient ring needs to be a field";
   if ring p =!= ring I then error "expected same ring";
   if not(unique degrees p == {{1}} and dim p -1 == 0 and degree p == 1) then error "expected the ideal of a point"; 
   a := flatten entries coefficients parametrize p;
   (f,g) := ChangeCoordinates(a,ring I);
   I = f I;
   x := gens ring I;
   if not isSubset(I,ideal submatrix'(matrix{x},{0})) then error "expected a point on the variety"; 
   S := (coefficientRing ring I)[delete(x_0,x)][x_0];
   return trim g sub(ideal flatten entries sub(last coefficients(gens sub(I,S)),coefficientRing S),ring I);
);

coneOfLines (Ideal,Ideal,ZZ) := (I,p,m) -> ( --undocumented
   if not isPolynomialRing ring I then error "expected ideal in a polynomial ring";
   if not isHomogeneous I then error "expected a homogeneous ideal";
   if not numgens I == 1 then error "the first ideal must be principal";
   d := degree I;
   if not isField coefficientRing ring I then error "the coefficient ring needs to be a field";
   if ring p =!= ring I then error "expected same ring";
   if not(unique degrees p == {{1}} and dim p -1 == 0 and degree p == 1) then error "expected the ideal of a point"; 
   a := flatten entries coefficients parametrize p;
   (f,g) := ChangeCoordinates(a,ring I);
   I = f I;
   x := gens ring I;
   if not isSubset(I,ideal submatrix'(matrix{x},{0})) then error "expected a point on the variety"; 
   S := (coefficientRing ring I)[delete(x_0,x)][x_0];
   F := (gens sub(I,S))_(0,0);
   return g sub(ideal apply(apply(toList(d-m+1 .. d),i -> (first gens S)^i),y -> coefficient(y,F)),ring I);
);

randomSigma22OnDelPezzoFivefold = method();
randomSigma22OnDelPezzoFivefold (Ideal,Ideal) := (DP5,p) -> (
   V := coneOfLines(DP5,p);
   j := parametrize ideal image basis(1,V);
   V' := j^* V;
   p' := j^* p;
   h := (rationalMap p')|V';
   V'' := image h;
   L := dualVariety top ideal jacobian dualVariety V'';
   -- try assert(L == sub(plucker first select(decompose plucker(V'',1,AffineChartGrass=>false),o -> (dim o -1, degree o) == (0,1)),vars ring V'')) else error "internal error encountered";
   j trim lift(h^* L,ambient source h)
);
randomSigma22OnDelPezzoFivefold (Ideal) := (DP5) -> randomSigma22OnDelPezzoFivefold(DP5,pointOnDP5 DP5);

randomSigma31OnDelPezzoFivefold = method(); -- to be implemented if the base field is not finite
randomSigma31OnDelPezzoFivefold (Ideal,Ideal) := (DP5,p) -> (
   V := coneOfLines(DP5,p);
   j := parametrize ideal image basis(1,V);
   V' := j^* V;
   p' := j^* p;
   h := (rationalMap p')|V';
   V'' := image h;
   e := point V'';
   P := j trim lift(h^* quotient(coneOfLines(V'',e),e),ambient source h);
   assert(? P == "plane in PP^8" and isSubset(DP5,P));
   P
);
randomSigma31OnDelPezzoFivefold (Ideal) := (DP5) -> randomSigma31OnDelPezzoFivefold(DP5,pointOnDP5 DP5);

parametrizeDelPezzoFivefold = method();

parametrizeDelPezzoFivefold (Ideal) := (DP5) -> (
   if not (isPolynomialRing ring DP5 and isHomogeneous DP5 and degrees DP5 === toList(5:{2}) and numgens ring DP5 == 9 and codim DP5 == 3 and degree DP5 == 5) then error "expected (the ideal of) a del Pezzo fivefold in PP^8"; 
   P := randomSigma22OnDelPezzoFivefold DP5;   
   f := inverseMap((rationalMap P)|DP5);
   (toCoordinateHyperplane ideal image basis(1,saturate ideal f)) * f
);

parametrizeDelPezzoSixfold = method();

parametrizeDelPezzoSixfold (Ideal) := (G) -> (
   if not (isPolynomialRing ring G and isHomogeneous G and degrees G === toList(5:{2}) and numgens ring G == 10 and codim G == 3 and degree G == 5) then error "expected (the ideal of) a 6-dimensional subvariety in PP^9, which is projectively equivalent to Grass(1,4)";
   J := parametrize arandom({1},ring G);
   P := J randomSigma22OnDelPezzoFivefold(J^* G);   
   f := inverse2((rationalMap P)|G);
   f = (toCoordinateHyperplane ideal image basis(1,saturate ideal f)) * f;
   P5 := (coefficientRing f)[flatten entries submatrix'(vars source f,{6})];
   j := rationalMap(P5,source f,(vars P5)|matrix{{0}});
   B := j^* ideal matrix f;
   M := syz gens B;
   h := rationalMap {M_(0,0),M_(0,1),M_(1,0),M_(1,1),M_(2,0),M_(2,1)}; 
   H := rationalMap(source f,source f,transpose((((coefficients h)||matrix{{0,0,0,0,0,0}})|matrix{{0},{0},{0},{0},{0},{0},{1}}) * (transpose vars source f)));
   f = (inverseMap H) * f;
   f = rationalMap(Grass(0,6,coefficientRing f,Variable=>source f),source f) * f;
   -- test:
   x := gens source f;
   try assert(saturate ideal matrix f == ideal(x_6,x_3*x_4-x_2*x_5,x_1*x_4-x_0*x_5,x_1*x_2-x_0*x_3)) else error "internal error encountered";
   --
   f
);

parametrizeDelPezzoFourfold = method();
-- Todd's result: a quintic del Pezzo fourfold contains exactly one rho-plane 
-- (Roth, "algebraic varieties with canonical curve section", p. 95)
parametrizeDelPezzoFourfold (Ideal) := (X) -> (
   if not (isPolynomialRing ring X and isHomogeneous X and numgens ring X == 8 and dim X == 5 and degree X == 5 and flatten degrees X == {2,2,2,2,2}) then error "expected a del Pezzo fourfold of degree 5";
   V := plucker(X,2); X' := null;
   if not(dim V -1 == 1 and degree V == 5 and genera V == {-1,4}) then (
      <<"-- re-executing plucker(..,2,AffineChartGrass=>true)"<<endl;
      phi := map(ring X,ring X,for i to 7 list arandom(1,ring X));
      psi := inverse phi;
      X' = trim phi X;
      V = plucker(X',2);
      if not(dim V -1 == 1 and degree V == 5 and genera V == {-1,4}) then error "error occurred trying to pick rho-plane in del Pezzo fourfold";
   );
   lV := trim lift(V,ambient ring V);
   h := parametrize ideal select(lV_*,g -> degree g == {1});
   lV' := h^* lV;
   P := sub(plucker sub(h quotient(lV',top lV'),ring V),vars ring X);
   if X' =!= null then P = trim psi P;
   try assert(? P == "plane in PP^7" and isSubset(X,P)) else "error occurred trying to pick rho-plane in del Pezzo fourfold";
   f := inverseMap rationalMap sub(P,quotient X);
   (toCoordinateHyperplane ideal image basis(1,saturate ideal matrix f)) * f
);

extendCubicScroll = method();

extendCubicScroll (Ideal) := (S) -> (
    try assert(isPolynomialRing ring S and isHomogeneous S and degree S == 3) else error "expected the ideal of a rational normal scroll of degree 3";
    K := coefficientRing ring S;
    N := syz gens S;
    y := local y;
    R := K[gens ring S,y];
    M := sub(N,R);
    M' := matrix for i to 2 list for j to 1 list (M_(i,j) + random(K) * y);
    Y := trim minors(2,M');
    j := parametrize ideal y;
    j = rationalMap(ring S,source j) * j;
    try assert(j^* Y == S) else error "internal error encountered";
    (j,Y)
);

embedDelPezzoSixfoldInG14 = method();

embedDelPezzoSixfoldInG14 (QuotientRing) := (G) -> (
   G'1'4 := Grass(1,4,coefficientRing ambient G,Variable=>ambient G);
   if G === G'1'4 then return rationalMap(G,G'1'4,vars ambient G);
   if G === dualize G'1'4 then return rationalMap(G,G'1'4,dualize vars ambient G'1'4);
   f := parametrizeDelPezzoSixfold(ideal G);
   y := gens source f;
   V := matrix {{-108*y_3*y_4+108*y_2*y_5+24*y_2*y_6-108*y_3*y_6+243*y_4*y_6-36*y_5*y_6+235*y_6^2, 108*y_1*y_4-108*y_0*y_5-24*y_0*y_6+108*y_1*y_6+486*y_4*y_6-108*y_5*y_6+462*y_6^2, -108*y_1*y_2+108*y_0*y_3-243*y_0*y_6+36*y_1*y_6-486*y_2*y_6+108*y_3*y_6-81*y_6^2, 216*y_4*y_6+216*y_6^2, -216*y_2*y_6+72*y_6^2, 216*y_0*y_6+216*y_6^2, 216*y_5*y_6+48*y_6^2, -216*y_3*y_6+486*y_6^2, 216*y_1*y_6+972*y_6^2, 432*y_6^2}};
   M := matrix apply(entries f,u -> linearCombination(u,V));
   phi := rationalMap(G'1'4,G,transpose(M * (transpose vars ambient G'1'4)));
   psi := rationalMap inverse map phi;
   try assert(isInverseMap(phi,psi)) else error "internal error encountered";
   psi
);

embedDelPezzoSixfoldInG14 (Ideal) := (G) -> embedDelPezzoSixfoldInG14(quotient G);

embedDelPezzoFivefoldInG14 = method();

embedDelPezzoFivefoldInG14 (Ideal) := (DP5) -> (
    f := parametrizeDelPezzoFivefold DP5;
    S := (parametrize ideal last gens source f)^* ideal matrix f;
    (j,Y) := extendCubicScroll S;
    K := coefficientRing f;
    R := K[gens ring S,last gens source f,last gens ring Y];
    W := sub(Y,R) + ideal(R_5);
    W = trim sub(W,vars(K[gens source f,last gens R]));
    h := rationalMap(source f,ring W,(vars source f)|0);
    try assert((saturate ideal matrix f) == h^* W) else error "internal error encountered";
    F := rationalMap(W,2);
    g := (inverse2 f) * h * F;
    g = rationalMap inverse map inverse2 rationalMap(g,Dominant=>true);
    try assert(unique flatten degrees ideal matrix g === {1}) else error "internal error encountered";
    g = rationalMap(source g,quotient image F,matrix g);
    g * embedDelPezzoSixfoldInG14(target g)
);

embedDelPezzoFourfoldInG14 = method();

embedDelPezzoFourfoldInG14 (Ideal) := (DP4) -> (
    f := parametrizeDelPezzoFourfold(DP4);
    S := (parametrize ideal last gens source f)^* ideal matrix f;
    (j,Y) := extendCubicScroll S;
    K := coefficientRing f;
    R := K[gens ring S,last gens source f,last gens ring Y];
    W := sub(Y,R) + ideal(R_4);
    W = trim sub(W,vars(K[gens source f,last gens R]));
    h := rationalMap(source f,ring W,(vars source f)|0);
    try assert((saturate ideal matrix f) == h^* W) else error "internal error encountered";
    F := rationalMap(W,2);
    g := (inverse2 f) * h * F;
    g = rationalMap inverse map inverse2 rationalMap(g,Dominant=>true);
    try assert(unique flatten degrees ideal matrix g === {1}) else error "internal error encountered";
    g = rationalMap(source g,quotient image F,matrix g);
    g = g * embedDelPezzoFivefoldInG14(ideal target g);
    try assert((toList Grass target g)_{0,1} == {1,4}) else error "internal error encountered";
    g
);

embedConeOverDelPezzoFourfoldInConeOverG14 = method();

embedConeOverDelPezzoFourfoldInConeOverG14 (Ideal,Ideal) := (X,p) -> (
   if not (isPolynomialRing ring X and isHomogeneous X and numgens ring X == 9 and dim X == 6 and degree X == 5 and flatten degrees X == {2,2,2,2,2}) then error "expected a cone over a del Pezzo fourfold of degree 5";
   if not (ring X === ring p and isHomogeneous p and dim p == 1 and degree p == 1 and sub(jacobian X,apply(gens ring X,flatten entries coefficients parametrize p,(x,x') -> x=>x')) == 0) then error "expected a point to be the vertex of a cone";
   ringP8 := ring X;
   K := coefficientRing ringP8;
   (f1,f2) := ChangeCoordinates(flatten entries coefficients parametrize p,ringP8);
   ringP7 := K[flatten entries submatrix'(vars ringP8,{0})];
   j := embedDelPezzoFourfoldInG14(sub(trim f1 X,ringP7));
   R := ringP8/sub(ideal source j,ringP8);
   y0 := last last Grass target j; y0 = getSymbol(toString(y0)|toString(y0));
   R' := K[y0,gens ambient target j];
   R' = R'/sub(ideal target j,R');
   J := rationalMap(quotient X,R,matrix f2) * rationalMap(R,R',matrix{{first gens R}}|sub(lift(matrix j,ambient source j),R));
   assert(degreeOfRationalMap J == 1);
   J
);

embedConeOverDelPezzoFourfoldInConeOverG14 (Ideal) := (X) -> (
   if not (isPolynomialRing ring X and isHomogeneous X and numgens ring X == 9 and dim X == 6 and degree X == 5 and flatten degrees X == {2,2,2,2,2}) then error "expected a cone over a del Pezzo fourfold of degree 5";
   p := radical trim(X + minors(3,jacobian X,Strategy=>Cofactor));
   embedConeOverDelPezzoFourfoldInConeOverG14(X,p)
);

------------------------------------------------------------------------
--------------------------- Discriminants ------------------------------
------------------------------------------------------------------------

eulerCharacteristic = method(Options => {Algorithm => null});

eulerCharacteristic (Ideal) := o -> (I) -> (
   if (not isPolynomialRing ring I and isHomogeneous I) then error "expected a homogeneous ideal in a polynomial ring";
   if # select(degrees I,i -> i == {1}) > 0 and # select(degrees I,i -> i > {1}) > 0 then (
       j := parametrize ideal select(I_*,i -> degree i == {1});
       return eulerCharacteristic(trim (map j) I,Algorithm=>o.Algorithm);
   );
   if o.Algorithm === "Hodge" then return euler variety I;
   if o.Algorithm === "CremonaMathModeTrue" then return EulerCharacteristic(I,MathMode=>true,Verbose=>false);
   K := coefficientRing ring I;
   ch := char K;
   if K =!= (if ch == 0 then QQ else ZZ/ch) then error "expected base field to be QQ or ZZ/p";
   if ch == 0 then return eulerCharacteristic(reduceToPrimeCharacteristic(I,65521),Algorithm=>o.Algorithm);
   if ch < 1000 then error "base field too small to use probabilistic methods";
   if o.Algorithm === "CremonaMathModeFalse" then return EulerCharacteristic(I,MathMode=>false);
   if o.Algorithm === "CharacteristicClasses" then return Euler(I,InputIsSmooth => true);
   if o.Algorithm === null then (
        if numgens ring I - 1 > 5 and # select(flatten degrees I,i -> i > 2) > 0 and dim I - 1 == 2 then I = isomorphicProjectionOfSurfaceInP5 I; 
        if ch <= 65521 then return Euler(I,InputIsSmooth => true) else return EulerCharacteristic(I,MathMode=>false);
   );
   error(///Algorithm option: Expected method to compute the topological Euler characteristic.
Possible methods are the following:
"Hodge" -- command: euler variety I -- package: Core;
"CremonaMathModeTrue" -- command: EulerCharacteristic(I,MathMode=>true) -- package: Cremona;
"CremonaMathModeFalse" -- command: EulerCharacteristic I -- package: Cremona;
"CharacteristicClasses" -- command: Euler(I,InputIsSmooth=>true) -- package: CharacteristicClasses
///);  
);

eulerCharacteristic (SpecialGushelMukaiFourfold) := o -> (X) -> (
    if X#"eulerCharacteristicSurface" =!= null then return X#"eulerCharacteristicSurface";
    e := eulerCharacteristic(X#"idealSurface",Algorithm=>o.Algorithm);
    -- if o.Algorithm === "Hodge" or o.Algorithm === "CremonaMathModeTrue" then X#"eulerCharacteristicSurface" = e;
    X#"eulerCharacteristicSurface" = e;
    e
);

eulerCharacteristic (SpecialCubicFourfold) := o -> (X) -> (
    if X#"eulerCharacteristicSurface" =!= null then return X#"eulerCharacteristicSurface";
    e := eulerCharacteristic(X#"idealSurface",Algorithm=>o.Algorithm);
    -- if o.Algorithm === "Hodge" or o.Algorithm === "CremonaMathModeTrue" then X#"eulerCharacteristicSurface" = e;
    X#"eulerCharacteristicSurface" = e;
    e
);

isomorphicProjectionOfSurfaceInP5 = method();
isomorphicProjectionOfSurfaceInP5 (Ideal) := (S) -> (
   -- if not(isPolynomialRing ring S and isHomogeneous S) then error "expected a homogeneous ideal in a polynomial ring";
   n := numgens ring S -1;
   if n <= 5 then return S;
   d := 2; -- d := max(dim S -1,-1);
   if d != 2 then error "expected the ideal of a surface";
   L := select(S_*,s -> degree s == {1});
   if # L > 0 then (
       j := parametrize ideal L;
       S = trim((map j) S);
       n = numgens ring S -1;
       if n <= 5 then return S;
   );
   pr := rationalMap trim ideal apply(6,i -> random(1,ring S));
   pr S
);

discriminant (SpecialGushelMukaiFourfold) := o -> (X) -> (
   if X#"discriminant" =!= null then return last X#"discriminant";
   (degS,g,chiOS) := X#"surfaceInvariants";
   chiS := eulerCharacteristic(X,Algorithm=>if o.Algorithm === "Poisson" then null else o.Algorithm); 
   KS2 := 12*chiOS-chiS; 
   KSHS := 2*g-2-degS; 
   c := cycleClass X;
   (a,b) := toSequence flatten entries lift(transpose last coefficients(c,Monomials=>vars ring c),ZZ);
   S2 := 3*a + 4*b + 2*KSHS + 2*KS2 - 12*chiOS;
   d := 4*S2 - 2*(b^2+(a-b)^2);
   -- if o.Algorithm === "Hodge" or o.Algorithm === "CremonaMathModeTrue" then X#"discriminant" = (S2,d);
   X#"discriminant" = (S2,d);
   d
);

discriminant (SpecialCubicFourfold) := o -> (X) -> ( 
   if X#"discriminant" =!= null then return last X#"discriminant";
   (degS,g,chiOS) := X#"surfaceInvariants";
   chiS := eulerCharacteristic(X,Algorithm=>if o.Algorithm === "Poisson" then null else o.Algorithm); 
   KS2 := 12*chiOS-chiS; 
   S2 := 3*degS+6*g-12*chiOS+2*KS2+2*(X#"NumNodesSurface")-6;
   d := 3*S2 - degS^2;
   -- if o.Algorithm === "Hodge" or o.Algorithm === "CremonaMathModeTrue" then X#"discriminant" = (S2,d);
   X#"discriminant" = (S2,d);
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
completeFlag (PolynomialRing) := (R) -> (
   V := {ideal R};
   for i to numgens R - 1 do V = prepend(trim(ideal(random(1,R)) + first(V)),V);
   if apply(V,dim) != toList(0 .. numgens R) then completeFlag(R) else V
);

schubertCycleInt = method(Options => {Variable => "p"});
schubertCycleInt (VisibleList,ZZ,ZZ,Ring) := o -> (a,k',n',K) -> (
   a = toList a;
   n := n' + 1;
   k := #a;
   try assert(ring matrix {a} === ZZ and rsort a == a and first a <= n-k and k == k'+1) else error("expected a decreasing sequence of "|toString(k'+1)|" nonnegative integers bounded by "|toString(n'-k'));
   a = prepend(null,a);
   V := completeFlag Grass(0,n',K,Variable=>o.Variable);
   S := for i from 1 to k list tangentialChowForm(V_(n-k+i-a_i),i-1,k-1,Variable=>o.Variable,SingularLocus=>first V);
   (V,trim sum apply(S,s -> if isIdeal s then s else ideal s))
);

schubertCycle = method(TypicalValue => Ideal);

schubertCycle (VisibleList,Ring) := (L,R) -> last schubertCycleInt prepend(L,Grass R);

schubertCycle (VisibleList,Ring,String) := (L,R,nu) -> (
   if nu =!= "standard" then error "invalid input string";
   (V,S) := schubertCycleInt prepend(L,Grass R);
   V' := apply(reverse V,gens);
   K := coefficientRing ambient R;
   f := rationalMap reverse for i from 1 to #V -1 list (V'_i * random(K^(numColumns V'_i),K^1))_(0,0);
   (S,rationalMap(f,R))
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

dimdegree = (X) -> if dim X <= 0 then 0 else if dim X == 1 then degree X else error "expected a zero-dimensional scheme"; 

cycleClass = method();
cycleClass (Ideal) := (X) -> (
   (k,n,KK,Vp) := Grass ring X;
   m := codim X;
   A := chowRing(k,n,m);
   sum(gens A,g -> g * dimdegree(X + last schubertCycleInt(toList(k+1:n-k) - toList reverse last baseName g,k,n,KK,Vp)))
);

cycleClass (SpecialGushelMukaiFourfold) := (X) -> (
   if X#"classSurfaceInG14" =!= null then return X#"classSurfaceInG14";
   S := X#"idealSurface";
   j := fromDP5toG14 X;
   X#"classSurfaceInG14" = cycleClass j S
);

------------------------------------------------------------------------
---------------- arXiv:2002.07026 --------------------------------------
------------------------------------------------------------------------

constrTriple = method();
constrTriple (String,Ideal,Ideal) := (name,V,C) -> (
    try assert(isPolynomialRing ring V and numgens ring V == 6 and ring V === ring C and isHomogeneous V and isHomogeneous C and dim V == 3 and dim C == 2 and isSubset(V,C)) else error "expected a pair (V,C) where C is a curve contained in a surface of PP^5";
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
    phi := projectivityBetweenRationalNormalCurves(C,C');
    V' := phi V;
    (if w then S else B,V',C')
);

tables = method();
tables (ZZ,Ring) := (i,K) -> (
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

tables (Ring,String) := (K,Name) -> ( -- store all data in a file
ch := char K; if K =!= ZZ/ch then error "expected a finite field";
F := openOut (Name|".dat"); 
F <<"-- file created automatically using: tables("|toExternalString K|",\""|Name|"\"); date: "|(toString get "!date")<<endl;
F <<"tables (ZZ) := (j) -> ("<<endl;
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
   A = tables(i,ZZ/ch);
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

tables (ZZ) := (i) -> (
    if i < 1 or i > 21 then error "expected integer between 1 and 21";
    try value get "data_examples.dat" else error("file \"data_examples.dat\" not found. You can make it using tables(K,\"data_examples\")");
    tables i
); 

fourfoldFromTriple = (i,E) -> (
    psi := rationalMap(E_0,max flatten degrees E_0,Dominant=>2);
    X := specialGushelMukaiFourfold(psi E_1);
    X#"label" = i;
    return X;
);

tables (ZZ,Ring,Nothing) := (i,K,nu) -> (
    E := tables(i,K);
    fourfoldFromTriple(i,E)
);

tables (ZZ,Nothing) := (i,nu) -> (
    E := tables i;
    fourfoldFromTriple(i,E)
);

------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------

arandom = method();

arandom (Ideal) := (I) -> (
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
   try assert(codim J == #l) else error "unable to find random elements";
   J
);

arandom (ZZ,PolynomialRing) := (d,R) -> random(d,R);

arandom (VisibleList,PolynomialRing) := (l,R) -> (
   J := trim ideal for i in (toList l) list arandom(i,R);
   if numgens J == 0 then J = sub(J,R);
   try assert(codim J == #l) else error "unable to find random elements";
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

inverse2 = method();
inverse2 (RationalMap,ZZ,Boolean) := (psi,minsCount,verbosity) -> (
    phi := rationalMap inverseOfMap(map psi,CheckBirational=>false,AssumeDominant=>true,MinorsCount=>minsCount,Verbose=>verbosity); 
    forceInverseMap(phi,psi); 
    phi
);
inverse2 (RationalMap) := (psi) -> inverse2(psi,0,false);

interpoleImage = method(Options => {Verbose => false});
interpoleImage (RationalMap,Ideal,List,ZZ) := o -> (g,X,D,j) -> (
    -- Try to return the ideal W generated up to degree j of the image of X via the map g, assuming that "degrees I == D"
    if not all(D,d -> instance(d,ZZ)) then error "expected a list of integers";
    cont := 0;
    W := g point X;
    while select(flatten degrees W,d -> d <= j) =!= D do (
        if o.Verbose then <<cont<<endl;
        W = intersect(W,g point X);
        if o.Verbose then <<tally degrees W<<endl;
        cont = cont + 1;
    );
    for i to 4 do (
        if o.Verbose then <<"extra "<<i<<endl;
        W = intersect(W,g point X);
        if o.Verbose then <<tally degrees W<<endl;
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
mapDefinedByDivisor (QuotientRing,VisibleList) := (R,D) -> ( 
    for d in D do if not (instance(first d,Ideal) and instance(last d,ZZ) and #d == 2 and (ring first d === R or ring first d === ambient R) and last d > 0) then error "expected a list of pairs (I,n), where I is an ideal in the given quotient ring and n is a positive integer";
    D = apply(D,d -> if ring first d === ambient R then (trim sub(first d,R),last d) else d);
    I := intersect for d in D list (first d)^(last d); 
    J := quotient(ideal I_0,I);
    rationalMap(J,max flatten degrees J)
);

linearCombination = method();
linearCombination (RingElement,Matrix) := (F,I) -> (
   try assert(ring F === ring I and isPolynomialRing ring I and numRows I === 1) else error "internal error encountered";
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
   try assert(F == sum(m,i -> l_i * I_(0,i))) else error "internal error encountered";
   --
   l
);

toCoordinateHyperplane = method();
toCoordinateHyperplane (Ideal) := (H) -> (
    if not(isPolynomialRing ring H and isHomogeneous H and numgens H == 1 and degree H == 1) then error "internal error encountered";
    K := coefficientRing ring H;
    f := inverse rationalMap transpose(((coefficients parametrize H) | (random(K^(numgens ring H),K^1))) * (transpose vars ring H));
    try assert(f H == ideal last gens ring H) else error "internal error encountered";
    inverse f
);

ChangeCoordinates = method();
ChangeCoordinates (List,PolynomialRing) := (a,R) -> (
   -- returns a change of coordinates s.t. a=(a_0,...,a_n)-->[1,0,...,0] 
   n := numgens R -1;
   try assert(#a == n+1 and matrix{a} != 0) else error("expected coordinate list of a point of PP^"|toString(n));
   j := 0; while a_j == 0 do j = j+1;
   A := (transpose matrix{a}) | submatrix'(diagonalMatrix(R,toList((n+1):1)),{j});
   f := map(R,R,transpose(A*transpose(vars R)));
   (f,f^-1)
);

reduceToPrimeCharacteristic = method();
reduceToPrimeCharacteristic (Ideal,ZZ) := (I,p) -> (
   assert(isPolynomialRing ring I and coefficientRing ring I === QQ);
   -- <<"*** Reduction to char "<< p <<" ***"<<endl;
   K := ZZ/p;
   x := local x;
   R := K[x_1 .. x_(numgens ring I)];
   sub(I,vars R)
);
reduceToPrimeCharacteristic (Ideal) := (I) -> reduceToPrimeCharacteristic(I,nextPrime random(300,11000000));

assertSmoothness = method(); -- sufficient conditions for smoothness ('I' is assumed to be equidimensional)
assertSmoothness (Ideal,ZZ) := (I,c) -> (
   if not (isPolynomialRing ring I and isHomogeneous I) then error "expected a homogeneous ideal in a polynomial ring";
   ch := char coefficientRing ring I;
   if coefficientRing ring I =!= (if ch == 0 then QQ else ZZ/ch) then error "expected base field to be QQ or ZZ/p";
   if coefficientRing ring I === QQ then I = reduceToPrimeCharacteristic I;
   n := numgens ring I -1;
   k := n - c;
   L := select(I_*,x -> degree x == {1});
   if # L > 0 then (
       j := parametrize ideal L;
       I = trim((map j) I);
       n = numgens ring I -1;
       c = n - k;
   );
   if k > 0 and 2*k+1 < n then (
       pr := rationalMap(intersect apply(n-(2*k+1),x -> point I),1);
       if numgens target pr -1 != 2*k+1 then error "internal error encountered";
       I = pr I;
       n = 2*k+1;
       c = n - k;
   );
   if c == 0 then return true;
   if c == 1 then return dim ideal jacobian I <= 0;
   dim(I + minors(c,jacobian I,Strategy=>Cofactor)) <= 0
);
assertSmoothness (Ideal) := (I) -> assertSmoothness(I,codim I);

isEquidimensional = method(); -- assume I is saturated
isEquidimensional (Ideal) := (I) -> (
   if not (isPolynomialRing ring I and isHomogeneous I) then error "expected a homogeneous ideal in a polynomial ring";
   if coefficientRing ring I === QQ then I = reduceToPrimeCharacteristic I;
   L := select(I_*,x -> degree x == {1});
   if # L > 0 then I = trim((map parametrize ideal L) I);
   I == top I
);

numberNodesOfSurface = method(Options => {Verbose => true});
numberNodesOfSurface (Ideal) := o -> (I) -> (
   try assert(isPolynomialRing ring I and numgens ring I <= 6 and isHomogeneous I) else error "internal error encountered";
   if o.Verbose then <<"-- computing number of nodes using a probabilistic method... "<<endl;
   if coefficientRing ring I === QQ then I = reduceToPrimeCharacteristic I;
   L := select(I_*,x -> degree x == {1});
   if # L > 0 then I = trim((map parametrize ideal L) I);
   c := codim I;
   if c == 0 then return 0;
   SingI := trim(I + minors(c,jacobian I,Strategy=>Cofactor));
   n := dimdegree radical (rationalMap arandom({1,1},ring I)) SingI;
   if o.Verbose then <<"-- got "|toString(n)|" nodes"|newline;
   n
);

normalization = method(Options => {Verbose => true});
normalization (Ring) := o -> (R) -> (
   if o.Verbose then <<"-- computing normalization... "<<endl;
   f := rationalMap icMap R;
   if o.Verbose then <<"-- got: "|toString(expression f)<<endl;
   f
);
normalization (Ideal) := o -> (I) -> normalization((ring I)/I,Verbose=>o.Verbose);
normalization (SpecialCubicFourfold) := o -> (X) -> (
   if X#"normalizationSurface" === null then X#"normalizationSurface" = normalization(first ideals X,Verbose=>o.Verbose);
   return X#"normalizationSurface";
);

varietyDefinedBylinearSyzygies = method();
varietyDefinedBylinearSyzygies (Ideal) := (Y) -> (
   assert(isPolynomialRing ring Y and isHomogeneous Y);
   G := transpose syz gens Y;
   M := matrix select(entries G,g -> max flatten degrees ideal g == 1);
   K := mingens kernel M;
   I := unique apply(entries transpose K,g -> trim ideal g);
   first select(I,i -> dim i >= 1)
);

toGushel = method();
toGushel (SpecialGushelMukaiFourfold) := (X) -> (
   j := fromDP5toG14 X;
   if X#"isGushelType" === true then return X;
   Y := local Y;
   i := rationalMap(target j,(coefficientRing X)[Y,gens ambient target j],0|vars target j);
   i = rationalMap(i,Dominant=>sub(ideal target j,target i));
   S := trim lift((j*i) first ideals X,ambient target i);
   Sv := intersect(S,ideal submatrix'(vars ambient target i,{0}));
   try H := arandom({1,1},Sv) else error "not able to specialize to Gushel type";
   h := (parametrize H)||(target i);
   specialGushelMukaiFourfold h^* S
);

< SpecialGushelMukaiFourfold := (X) -> try toGushel X else error "not able to deform to Gushel type";

imageOfAssociatedMap = method();
imageOfAssociatedMap (MutableHashTable) := (X) -> (
   f := X#"map";
   if f#"idealImage" =!= null then return image f;
   e := X#"label";
   if e === "quinticDelPezzoSurface" or e === "quarticScrollSurface" or e === "FarkasVerra" then forceImage(f,image(f,2));
   if e === "C38Coble" or e === "C42" then forceImage(f,image(f,3));
   if instance(e,ZZ) and e >= 1 and e <= 21 and e != 3 and e != 21 then forceImage(f,image(f,2));
   if instance(e,ZZ) and (e == 3 or e == 21) then forceImage(f,trim lift(kernel(map rationalMap(f,Dominant=>2),SubringLimit=>1),ambient target f));
   if e === "gushel26''" then forceImage(f,trim kernel(map f,SubringLimit=>1));
   image f
);

projectivitySendingRationalNormalCurveToStandardRationalNormalCurve = method();
projectivitySendingRationalNormalCurveToStandardRationalNormalCurve (Ideal) := (C) -> (
   if not(isPolynomialRing ring C and isHomogeneous C and dim C == 2) then error "expected the ideal of a rational normal curve";
   K := coefficientRing ring C;
   n := numgens ring C -1;
   H := ideal image basis(1,C);
   c := codim H;
   if c > 0 then (
       x := local x;
       ringPn := K[x_0..x_n];
       h := inverse rationalMap(ringPn,ring H,transpose(((coefficients parametrize H) | (random(K^(n+1),K^c))) * (transpose vars ringPn)));
       C' := h C;
       ringPm := K[x_0..x_(n-c)];
       C' = trim sub(C',ringPm);
       phi' := projectivitySendingRationalNormalCurveToStandardRationalNormalCurve C';
       phi' = h * rationalMap(ringPn,ring H,sub(matrix phi',ringPn)|submatrix'(vars ringPn,{0.. n-c}));
       return phi';
   );
   d := degree C;
   f := if d > 1 
        then rationalMap inverseMap rationalMap trim sub(ideal image basis(1,intersect(for i to d-2 list point C)),quotient C)
        else parametrize C;
   g := rationalMap(source f,target f,sub(matrix veronese(1,d,K),vars source f));
   V := matrix g;
   M := matrix apply(entries f,u -> linearCombination(u,V));
   phi := inverse rationalMap(ring C,ring C,transpose(M * (transpose vars ring C)));
   -- try assert(f * phi == g) else error "internal error encountered";
   phi
);

projectivityBetweenRationalNormalCurves = method();
projectivityBetweenRationalNormalCurves (Ideal,Ideal) := (C1,C2) -> (
   j1 := projectivitySendingRationalNormalCurveToStandardRationalNormalCurve C1;
   j2 := projectivitySendingRationalNormalCurveToStandardRationalNormalCurve C2;
   phi := j1 * (inverse j2);
   try assert(phi C1 == C2) else error "failed to construct projectivity";
   phi
);

point (SpecialCubicFourfold) := (X) -> point ideal X;  

point (SpecialGushelMukaiFourfold) := (X) -> trim sub(point ideal X,ringDP5 X);

------------------------------------------------------------------------
---------------------------- Documentation -----------------------------
------------------------------------------------------------------------

beginDocumentation() 

document {Key => SpecialFanoFourfolds, 
Headline => "A package for working with special cubic fourfolds and special Gushel-Mukai fourfolds"} 

document {Key => {SpecialGushelMukaiFourfold}, 
Headline => "the class of all special Gushel-Mukai fourfolds", 
PARA{"An (ordinary) Gushel-Mukai fourfold is the intersection of a smooth del Pezzo fivefold ", TEX///$\mathbb{G}(1,4)\cap\mathbb{P}^8\subset \mathbb{P}^8$///, " with a quadric hypersurface in ", TEX///$\mathbb{P}^8$///, ". A Gushel-Mukai fourfold is said to be ", EM"special", " if it contains a surface whose cohomology class ", EM "does not come", " from the Grassmannian ", TEX///$\mathbb{G}(1,4)$///, ". The special Gushel-Mukai fourfolds are parametrized by a countable union of (not necessarily irreducible) hypersurfaces in the corresponding moduli space, labelled by the integers ", TEX///$d \geq 10$///, " with ", TEX///$d = 0, 2, 4\ ({mod}\ 8)$///, "; the number ",TEX///$d$///," is called the discriminant of the fourfold. For precise definition and results, we refer mainly to the paper ", HREF{"https://arxiv.org/abs/1302.1398", "Special prime Fano fourfolds of degree 10 and index 2"}, ", by O. Debarre, A. Iliev, and L. Manivel."}, 
PARA{"An object of the class ", TO SpecialGushelMukaiFourfold, " is basically a couple ", TEX///(S,X)///, ", where ", TEX///$X$///, " is (the ideal of) a Gushel-Mukai fourfold and ", TEX///$S$///, " is (the ideal of) a surface contained in ", TEX///$X$///, ".  The main constructor for the objects of the class is the method ", TO specialGushelMukaiFourfold,", and the discriminant ", TEX///$d$///, " can be calculated by the method ", TO (discriminant,SpecialGushelMukaiFourfold),"."}}  

document {Key => {(discriminant, SpecialCubicFourfold)}, 
Headline => "discriminant of a special cubic fourfold", 
Usage => "ideals X", 
Inputs => {"X" => SpecialCubicFourfold}, 
Outputs => {RingElement => {"an integer, the discriminant of ", TEX///$X$///}}, 
PARA{"This calculation passes through the determination of the topological Euler characteristic of the surface contained in the fourfold, which is obtained thanks to the methods ", TO EulerCharacteristic, " and ", TO Euler, " (the option ", TT "Algorithm", " allows you to select the method)."}, 
EXAMPLE {"X = specialCubicFourfold \"quintic del Pezzo surface\";", "time discriminant X"}, 
SeeAlso => {(discriminant, SpecialGushelMukaiFourfold)}} 

document {Key => {(discriminant, SpecialGushelMukaiFourfold)}, 
Headline => "discriminant of a special Gushel-Mukai fourfold", 
Usage => "ideals X", 
Inputs => {"X" => SpecialGushelMukaiFourfold}, 
Outputs => {RingElement => {"an integer, the discriminant of ", TEX///$X$///}}, 
PARA{"This method applies a formula given in Section 7 of the paper ", HREF{"https://arxiv.org/abs/1302.1398", "Special prime Fano fourfolds of degree 10 and index 2"}, ", obtaining the data required through the methods ", TO cycleClass, ", ", TO EulerCharacteristic, " and ", TO Euler, " (the option ", TT "Algorithm", " allows you to select the method)."}, 
EXAMPLE {"X = specialGushelMukaiFourfold \"tau-quadric\";", "time discriminant X"}, 
SeeAlso => {(discriminant, SpecialCubicFourfold)}} 

undocumented{(expression, SpecialGushelMukaiFourfold), (net, SpecialGushelMukaiFourfold), (coefficientRing, SpecialGushelMukaiFourfold), (describe, SpecialGushelMukaiFourfold)} 

document {Key => {specialGushelMukaiFourfold, (specialGushelMukaiFourfold, Ideal, Ideal), [specialGushelMukaiFourfold, InputCheck], [specialGushelMukaiFourfold, Verbose]}, 
Headline => "make a special Gushel-Mukai fourfold", 
Usage => "specialGushelMukaiFourfold(S,X)", 
Inputs => {"S" => Ideal => {"the ideal of a smooth irreducible surface ", TEX///$S\subset\mathbb{P}^8$///}, "X" => Ideal => {"the ideal of a smooth prime Fano fourfold ", TEX///$X\subset \mathbb{P}^8$///, " of degree 10 and sectional genus 6, which contains the surface ", TEX///$S$///}}, 
Outputs => {SpecialGushelMukaiFourfold => {"the special Gushel-Mukai fourfold corresponding to the pair ", TEX///$(S,X)$///}}, 
PARA{"In the following example, we define a Gushel-Mukai fourfold containing a so-called ", TEX///$\tau$///, "-quadric."}, 
EXAMPLE {"K = ZZ/33331; ringP8 = K[x_0..x_8];", "idealS = ideal(x_6-x_7, x_5, x_3-x_4, x_1, x_0-x_4, x_2*x_7-x_4*x_8);", "idealX = ideal(x_4*x_6-x_3*x_7+x_1*x_8, x_4*x_5-x_2*x_7+x_0*x_8, x_3*x_5-x_2*x_6+x_0*x_8+x_1*x_8-x_5*x_8, x_1*x_5-x_0*x_6+x_0*x_7+x_1*x_7-x_5*x_7, x_1*x_2-x_0*x_3+x_0*x_4+x_1*x_4-x_2*x_7+x_0*x_8, x_0^2+x_0*x_1+x_1^2+x_0*x_2+2*x_0*x_3+x_1*x_3+x_2*x_3+x_3^2-x_0*x_4-x_1*x_4-2*x_2*x_4-x_3*x_4-2*x_4^2+x_0*x_5+x_2*x_5+x_5^2+2*x_0*x_6+x_1*x_6+2*x_2*x_6+x_3*x_6+x_5*x_6+x_6^2-3*x_4*x_7+2*x_5*x_7-x_7^2+x_1*x_8+x_3*x_8-3*x_4*x_8+2*x_5*x_8+x_6*x_8-x_7*x_8);", "time X = specialGushelMukaiFourfold(idealS,idealX);", "time describe X"}} 

document {Key => {(specialGushelMukaiFourfold, Ideal), (specialGushelMukaiFourfold, String, Ring), (specialGushelMukaiFourfold, String)}, 
Headline => "random special Gushel-Mukai fourfold", 
Usage => "specialGushelMukaiFourfold S", 
Inputs => {"S" => Ideal => {"the ideal of a smooth irreducible surface in the coordinate ring of a del Pezzo fivefold or del Pezzo sixfold (e.g., an ideal in the ring ", TO Grass, TEX///$(1,4)$///, ")"}}, 
Outputs => {SpecialGushelMukaiFourfold => {"a random special Gushel-Mukai fourfold containing the given surface"}}, 
EXAMPLE {"G = Grass(1,4,ZZ/33331);", "-- cubic scroll in G(1,4)"|newline|"S = schubertCycle({2,0},G) + schubertCycle({1,0},G) + schubertCycle({1,0},G)", "X = specialGushelMukaiFourfold S;", "discriminant X"}, 
PARA{"Some random Gushel-Mukai fourfolds can also be obtained by passing strings. For instance, an object as above is also given as follows."}, 
EXAMPLE {"specialGushelMukaiFourfold(\"cubic scroll\");"}} 

document {Key => {toGrass, (toGrass, SpecialGushelMukaiFourfold)}, 
Headline => "Gushel morphism from a GM fourfold to Grass(1,4)", 
Usage => "toGrass X", 
Inputs => {"X" => SpecialGushelMukaiFourfold}, 
Outputs => {RationalMap => {"a linear morphism from ", TEX///$X$///, " into the Grassmannian ", TEX///$\mathbb{G}(1,4)\subset\mathbb{P}^9$///, ", Plucker embedded, which is an embedding when ",TEX///$X$///," is of ordinary type"}}, 
EXAMPLE {"P8 = ZZ/33331[x_0..x_8];", "time X = specialGushelMukaiFourfold(ideal(x_6-x_7, x_5, x_3-x_4, x_1, x_0-x_4, x_2*x_7-x_4*x_8), ideal(x_4*x_6-x_3*x_7+x_1*x_8, x_4*x_5-x_2*x_7+x_0*x_8, x_3*x_5-x_2*x_6+x_0*x_8+x_1*x_8-x_5*x_8, x_1*x_5-x_0*x_6+x_0*x_7+x_1*x_7-x_5*x_7, x_1*x_2-x_0*x_3+x_0*x_4+x_1*x_4-x_2*x_7+x_0*x_8, x_0^2+x_0*x_1+x_1^2+x_0*x_2+2*x_0*x_3+x_1*x_3+x_2*x_3+x_3^2-x_0*x_4-x_1*x_4-2*x_2*x_4-x_3*x_4-2*x_4^2+x_0*x_5+x_2*x_5+x_5^2+2*x_0*x_6+x_1*x_6+2*x_2*x_6+x_3*x_6+x_5*x_6+x_6^2-3*x_4*x_7+2*x_5*x_7-x_7^2+x_1*x_8+x_3*x_8-3*x_4*x_8+2*x_5*x_8+x_6*x_8-x_7*x_8));", "time toGrass X"}, 
SeeAlso => {(toGrass, Ideal)}} 

document {Key => {(toGrass, Ideal)}, 
Headline => "embedding of an ordinary Gushel-Mukai fourfold or a del Pezzo variety into Grass(1,4)", 
Usage => "toGrass X", 
Inputs => {"X" => Ideal => {"the ideal of an ordinary Gushel-Mukai fourfold, or of a del Pezzo variety of dimension at least 4 (e.g., a sixfold projectively equivalent to ", TEX///$\mathbb{G}(1,4)\subset\mathbb{P}^9$///,")"}}, 
Outputs => {RationalMap => {"an embedding of ", TEX///$X$///, " into the Grassmannian ", TEX///$\mathbb{G}(1,4)\subset\mathbb{P}^9$///, ", Plucker embedded"}}, 
EXAMPLE {"P8 = ZZ/33331[x_0..x_8];", "X = ideal(x_4*x_6-x_3*x_7+x_1*x_8, x_4*x_5-x_2*x_7+x_0*x_8, x_3*x_5-x_2*x_6+x_0*x_8+x_1*x_8-x_5*x_8, x_1*x_5-x_0*x_6+x_0*x_7+x_1*x_7-x_5*x_7, x_1*x_2-x_0*x_3+x_0*x_4+x_1*x_4-x_2*x_7+x_0*x_8);", "time toGrass X"}, 
SeeAlso => {(toGrass, SpecialGushelMukaiFourfold)}} 

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
PARA{"By calling the method as below, it returns as second output an automorphism of the Grassmannian which sends the random Schubert cycle to a standard Schubert cycle."}, 
EXAMPLE {"(S,f) = schubertCycle({2,1},G,\"standard\");", "f;", "S", "f S"}, 
SeeAlso => {cycleClass, (rationalMap, RationalMap, Ring)}} 

document {Key => {(rationalMap, RationalMap, Ring)}, 
Headline => "induced automorphism of the Grassmannian", 
Usage => "rationalMap(phi,G)", 
Inputs => {"phi" => RationalMap => {"an automorphism of ", TEX///$\mathbb{P}^n$///}, "G" => Ring => {"the coordinate ring ", TO Grass, TEX///$(k,n)$///, " of the Grassmannian of ", TEX///$k$///, "-dimensional subspaces of ", TEX///$\mathbb{P}^n$///}}, 
Outputs => {RationalMap => {"the induced automorphism of ", TO Grass, TEX///$(k,n)$///}}, 
EXAMPLE {"P4 = Grass(0,4,ZZ/33331);", "G'1'4 = Grass(1,4,ZZ/33331);", "phi = rationalMap apply(5, i -> random(1,P4))", "Phi = rationalMap(phi,G'1'4)"}} 

document {Key => {tables, (tables, ZZ, Ring), (tables, ZZ), (tables, ZZ, Ring, Nothing), (tables, ZZ, Nothing)}, 
Headline => "make examples of reducible subschemes of P^5", 
Usage => "tables(i,K)", 
Inputs => {"i" => ZZ => {"an integer between 1 and 21"}, "K" => Ring => {"the coefficient ring"}}, 
Outputs => {{"a triple of ideals ", TEX///$(B,V,C)$///, ", which represents a reducible subscheme of ", TEX///$\mathbb{P}^5$///, " as indicated in the paper ", HREF{"https://arxiv.org/abs/2002.07026", "On some families of Gushel-Mukai fourfolds"}, "."}}, 
EXAMPLE {"(B,V,C) = tables(1,ZZ/33331)", "(?B,?V,?C)", "B + V == C"}, 
PARA{"The corresponding example of fourfold can be obtained as follows."}, 
EXAMPLE {"psi = rationalMap(B,max flatten degrees B,Dominant=>2);", "X = specialGushelMukaiFourfold psi V;"}, 
PARA{"This is basically the same as doing this:"}, 
EXAMPLE {"specialGushelMukaiFourfold(\"1\",ZZ/33331);"}} 

undocumented {(tables, Ring, String)}; 

document {Key => {parameterCount, (parameterCount, SpecialCubicFourfold), (parameterCount, Ideal, Ideal), [parameterCount, Verbose]}, 
Headline => "count of parameters", 
Usage => "parameterCount X", 
Inputs => {"X" => SpecialCubicFourfold => {"a special cubic fourfold containing a surface ", TEX///$S$///}}, 
Outputs => {ZZ => {"an upper bound for the codimension in the moduli space of cubic fourfolds of the locus of cubic fourfolds that contain a surface belonging to the same irreducible component of the Hilbert scheme containing ", TEX///$[S]$///}, Sequence => {"the triple of integers: ", TEX///$(h^0(I_{S/P^5}(3)), h^0(N_{S/P^5}), h^0(N_{S/X}))$///}}, 
PARA{"This method implements a parameter count explained in the paper ", HREF{"https://arxiv.org/abs/1503.05256", "Unirationality of moduli spaces of special cubic fourfolds and K3 surfaces"}, ", by H. Nuer."}, 
PARA{"Below, we show that the closure of the locus of cubic fourfolds containing a Veronese surface has codimension at most one (hence exactly one) in the moduli space of cubic fourfolds. Then, by the computation of the discriminant, we deduce that the cubic fourfolds containing a Veronese surface describe the Hassett's divisor ", TEX///$\mathcal{C}_{20}$///}, 
EXAMPLE {"P5 = ZZ/33331[x_0..x_5];", "V = trim minors(2,genericSymmetricMatrix(P5,3))", "X = specialCubicFourfold V", "time parameterCount X", "time discriminant X"}, 
SeeAlso => {normalSheaf}} 

undocumented {(parameterCount, Ideal, Ideal, Boolean)} 

document {Key => {(parameterCount, SpecialGushelMukaiFourfold)}, 
Headline => "count of parameters in the moduli space of GM fourfolds", 
Usage => "parameterCount X", 
Inputs => {"X" => SpecialGushelMukaiFourfold => {"a special GM fourfold containing a surface ", TEX///$S$///, " and contained in a del Pezzo fivefold ", TEX///$Y$///}}, 
Outputs => {ZZ => {"an upper bound for the codimension in the moduli space of GM fourfolds of the locus of GM fourfolds that contain a surface belonging to the same irreducible component of the Hilbert scheme of ", TEX///$Y$///, " that contains ", TEX///$[S]$///}, Sequence => {"the triple of integers: ", TEX///$(h^0(I_{S/Y}(2)), h^0(N_{S/Y}), h^0(N_{S/X}))$///}}, 
PARA{"This method implements a parameter count explained in the paper ", HREF{"https://arxiv.org/abs/2002.07026", "On some families of Gushel-Mukai fourfolds"}, "."}, 
PARA{"Below, we show that the closure of the locus of GM fourfolds containing a cubic scroll has codimension at most one (hence exactly one) in the moduli space of GM fourfolds."}, 
EXAMPLE {"G = Grass(1,4,ZZ/33331);", "S = schubertCycle({2,0},G) + ideal(random(1,G), random(1,G))", "X = specialGushelMukaiFourfold S;", "time parameterCount X", "time discriminant X"}, 
SeeAlso => {normalSheaf, (parameterCount, SpecialCubicFourfold)}} 

document {Key => {normalSheaf, (normalSheaf, Ideal), (normalSheaf, Ideal, Ideal)}, 
Headline => "normal sheaf", 
Usage => "normalSheaf I"|newline|"normalSheaf(I,J)", 
Inputs => {"I" => Ideal => {"the ideal of a subvariety ", TEX///$X\subset \mathbb{P}^n$///}, "J" => Ideal => {"the ideal of a subvariety ", TEX///$Y\subset \mathbb{P}^n$///, " such that ", TEX///$X\subset Y$///, " (if not given, it is assumed to be ", TEX///$Y = \mathbb{P}^n$///, ")"}}, 
Outputs => {CoherentSheaf => {"the normal sheaf ", TEX///$\mathcal{N}_{X, Y}$///, " of ", TEX///$X$///, " in ", TEX///$Y$///}}} 

document {Key => {isAdmissible, (isAdmissible, ZZ), (isAdmissible, SpecialCubicFourfold)}, 
Headline => "whether an integer is admissible", 
Usage => "isAdmissible d", 
Inputs => {"d" => ZZ}, 
Outputs => {Boolean => {"wheter ", TT"d", " is admissible, i.e., it is an even integer ", TT"d>6", " which is not divisible by 4, 9 or any odd prime congruent to 2 modulo 3"}}, 
EXAMPLE{"select(100,isAdmissible)"}} 

document {Key => {detectCongruence, (detectCongruence, SpecialCubicFourfold), (detectCongruence, SpecialCubicFourfold, ZZ), (detectCongruence, SpecialGushelMukaiFourfold), (detectCongruence, SpecialGushelMukaiFourfold, ZZ)}, 
Headline => "detect and return a congruence of (3e-1)-secant curves of degree e", 
Usage => "detectCongruence X"|newline|"detectCongruence(X,e)", 
Inputs => {"X" => SpecialCubicFourfold => {"containing a surface ", TEX///$S\subset\mathbb{P}^5$///}, "e" => ZZ => {"a positive integer (optional but recommended)"}}, 
Outputs => {FunctionClosure => {"which takes the ideal of a (general) point ", TEX///$p\in\mathbb{P}^5$///, " and returns the unique rational curve of degree ", TEX///$e$///, ", ", TEX///$(3e-1)$///, "-secant to ", TEX///$S$///, ", and passing through ", TEX///$p$///, " (an error is thrown if such a curve does not exist or is not unique)"}}, 
EXAMPLE {"-- A general cubic fourfold of discriminant 26"|newline|"X = specialCubicFourfold(\"Farkas-Verra C26\",ZZ/33331);", "describe X", "time f = detectCongruence X;", "p = point ring X -- random point on P^5", "time C = f p -- 5-secant conic to the surface", "assert(codim C == 4 and degree C == 2 and codim(C+(first ideals X)) == 5 and degree(C+(first ideals X)) == 5 and isSubset(C, p))"}, 
PARA{"The same method can be also applied to a ", TO SpecialGushelMukaiFourfold, ". In this case it will detect and return a congruence of (2e-1)-secant curves of degree e inside the unique del Pezzo fivefold containing the GM fourfold."}, 
EXAMPLE{"-- A general GM fourfold of discriminant 20"|newline|"X = specialGushelMukaiFourfold(\"surface of degree 9 and genus 2\",ZZ/33331);", "describe X", "time f = detectCongruence X;", "Y = source map X; -- del Pezzo fivefold containing X", "p = point Y -- random point on Y", "time C = f p -- 3-secant conic to the surface", "S = sub(first ideals X,Y);", "assert(dim C -1 == 1 and degree C == 2 and dim(C+S)-1 == 0 and degree(C+S) == 3 and isSubset(C, p))"}, 
SeeAlso => {coneOfLines}} 

document {Key => {SpecialCubicFourfold}, 
Headline => "the class of all special cubic fourfolds", 
PARA{"A cubic fourfold is a smooth cubic hypersurface in ", TEX///$\mathbb{P}^5$///, ". A cubic fourfold ", TEX///$X\subset \mathbb{P}^5$///, " is ", EM "special", " of discriminant ", TEX///$d>6$///, " if it contains an algebraic surface ", TEX///$S$///, ", and the discriminant of the saturated lattice spanned by ", TEX///$h^2$///, " and ", TEX///$[S]$///, " in ", TEX///$H^{2,2}(X,\mathbb{Z}):=H^4(X,\mathbb{Z})\cap H^2(\Omega_X^2)$///, " is ", TEX///$d$///, ", where ", TEX///$h$///, " denotes the class of a hyperplane section of ", TEX///$X$///, ". The set ", TEX///$\mathcal{C}_d$///, " of special cubic fourfolds of discriminant ", TEX///$d$///, " is either empty or an irreducible divisor inside the moduli space of cubic fourfolds ", TEX///$\mathcal{C}$///, ". Moreover, ", TEX///$\mathcal{C}_d\neq \emptyset$///, " if and only if ", TEX///$d>6$///, " and ", TEX///$d=$///, "0 or 2 (mod 6). For the general theory, see the papers ", HREF{"https://link.springer.com/article/10.1023/A:1001706324425", "Special cubic fourfolds"}, " and ", HREF{"http://imperium.lenin.ru/~kaledin/math/hasset.pdf", "Some rational cubic fourfolds"}, ", by B. Hassett."}, 
PARA{"An object of the class ", TO SpecialCubicFourfold, " is basically a couple ", TEX///(S,X)///, ", where ", TEX///$X$///, " is (the principal ideal of) a cubic fourfold and ", TEX///$S$///, " is (the ideal of) a surface contained in ", TEX///$X$///, ". The surface ", TEX///$S$///, " is required to be smooth or with at most a finite number ", TEX///$n$///, " of non-normal nodes. This number ", TEX///$n$///, " (if known) can be specified manually using the option ", TT "NumNodes", ". The main constructor for the objects of the class is the method ", TO specialCubicFourfold,", and the discriminant ", TEX///$d$///, " can be calculated by the method ", TO (discriminant,SpecialCubicFourfold),"."}} 

undocumented{(expression, SpecialCubicFourfold), (net, SpecialCubicFourfold), (coefficientRing, SpecialCubicFourfold), (describe, SpecialCubicFourfold)} 

undocumented{InputCheck,NumNodes}

document {Key => {specialCubicFourfold, (specialCubicFourfold, Ideal, Ideal), (specialCubicFourfold, Ideal, RingElement), [specialCubicFourfold, NumNodes], [specialCubicFourfold, InputCheck], [specialCubicFourfold, Verbose]}, 
Headline => "make a special cubic fourfold", 
Usage => "specialCubicFourfold(S,X)"|newline|"specialCubicFourfold(S,X,NumNodes=>n)", 
Inputs => {"S" => Ideal => {"the ideal of an irreducible surface ", TEX///$S\subset\mathbb{P}^5$///, ", which has as singularities only a finite number ",TEX///$n\geq 0$///," of non-normal nodes (this number ",TEX///$n$///," should be passed with the option ", TT "NumNodes",", otherwise it is obtained using a probabilistic method)"}, "X" => Ideal => {"the ideal of a smooth cubic fourfold ", TEX///$X\subset \mathbb{P}^5$///, " containing the surface ", TEX///$S$///}}, 
Outputs => {SpecialCubicFourfold => {"the special cubic fourfold corresponding to the pair ", TEX///$(S,X)$///}}, 
PARA{"In the example below, we define a cubic fourfold containg a rational scroll of degree 7 with 3 nodes."}, 
EXAMPLE {"K = ZZ/33331; ringP5 = K[x_0..x_5];", "idS = ideal(x_0*x_2*x_3-2*x_1*x_2*x_3-x_1*x_3^2-x_2*x_3^2-x_0*x_1*x_4+2*x_1^2*x_4-x_1*x_2*x_4+x_2^2*x_4+2*x_0*x_3*x_4-x_1*x_3*x_4-x_1*x_4^2+x_1*x_3*x_5, x_1^2*x_3-4*x_1*x_2*x_3-x_0*x_3^2-3*x_1*x_3^2-2*x_2*x_3^2+2*x_0^2*x_4-9*x_0*x_1*x_4+11*x_1^2*x_4-x_0*x_2*x_4-2*x_1*x_2*x_4+2*x_2^2*x_4+12*x_0*x_3*x_4-7*x_1*x_3*x_4-4*x_3^2*x_4+x_0*x_4^2-6*x_1*x_4^2+4*x_2*x_4^2-2*x_3*x_4^2-2*x_4^3-x_0*x_1*x_5+x_1^2*x_5+2*x_1*x_2*x_5+3*x_0*x_3*x_5+2*x_1*x_3*x_5-x_3^2*x_5-x_0*x_4*x_5-4*x_1*x_4*x_5+3*x_2*x_4*x_5+2*x_3*x_4*x_5-x_1*x_5^2, x_0*x_1*x_3-7*x_1*x_2*x_3-3*x_0*x_3^2-4*x_1*x_3^2-3*x_2*x_3^2+x_3^3+3*x_0^2*x_4-14*x_0*x_1*x_4+17*x_1^2*x_4-x_0*x_2*x_4-3*x_1*x_2*x_4+3*x_2^2*x_4+19*x_0*x_3*x_4-9*x_1*x_3*x_4-x_2*x_3*x_4-6*x_3^2*x_4+x_0*x_4^2-9*x_1*x_4^2+6*x_2*x_4^2-3*x_3*x_4^2-3*x_4^3-2*x_0*x_1*x_5+2*x_1^2*x_5+4*x_1*x_2*x_5+5*x_0*x_3*x_5+4*x_1*x_3*x_5-2*x_3^2*x_5-2*x_0*x_4*x_5-7*x_1*x_4*x_5+5*x_2*x_4*x_5+3*x_3*x_4*x_5-2*x_1*x_5^2, x_0^2*x_3-12*x_1*x_2*x_3-6*x_0*x_3^2-6*x_1*x_3^2-5*x_2*x_3^2+2*x_3^3+5*x_0^2*x_4-24*x_0*x_1*x_4+29*x_1^2*x_4-x_0*x_2*x_4-5*x_1*x_2*x_4+5*x_2^2*x_4+32*x_0*x_3*x_4-14*x_1*x_3*x_4-2*x_2*x_3*x_4-10*x_3^2*x_4+x_0*x_4^2-15*x_1*x_4^2+10*x_2*x_4^2-5*x_3*x_4^2-5*x_4^3-3*x_0*x_1*x_5+3*x_1^2*x_5+6*x_1*x_2*x_5+8*x_0*x_3*x_5+7*x_1*x_3*x_5-3*x_3^2*x_5-3*x_0*x_4*x_5-11*x_1*x_4*x_5+8*x_2*x_4*x_5+5*x_3*x_4*x_5-3*x_1*x_5^2, x_1*x_2^2+6*x_1*x_2*x_3+2*x_0*x_3^2+3*x_1*x_3^2+2*x_2*x_3^2-x_3^3-3*x_0^2*x_4+12*x_0*x_1*x_4-14*x_1^2*x_4-2*x_2^2*x_4-15*x_0*x_3*x_4+6*x_1*x_3*x_4+x_2*x_3*x_4+5*x_3^2*x_4+x_0*x_4^2+8*x_1*x_4^2-5*x_2*x_4^2+2*x_3*x_4^2+2*x_4^3+x_0*x_1*x_5-2*x_1^2*x_5-4*x_1*x_2*x_5-4*x_0*x_3*x_5-3*x_1*x_3*x_5+2*x_3^2*x_5+2*x_0*x_4*x_5+7*x_1*x_4*x_5-4*x_2*x_4*x_5-2*x_3*x_4*x_5+2*x_1*x_5^2, x_0*x_2^2+10*x_1*x_2*x_3+3*x_0*x_3^2+5*x_1*x_3^2+4*x_2*x_3^2-x_3^3-5*x_0^2*x_4+19*x_0*x_1*x_4-22*x_1^2*x_4-x_0*x_2*x_4+3*x_1*x_2*x_4-4*x_2^2*x_4-24*x_0*x_3*x_4+9*x_1*x_3*x_4+x_2*x_3*x_4+8*x_3^2*x_4+2*x_0*x_4^2+11*x_1*x_4^2-7*x_2*x_4^2+4*x_3*x_4^2+3*x_4^3+2*x_0*x_1*x_5-4*x_1^2*x_5-7*x_1*x_2*x_5-7*x_0*x_3*x_5-5*x_1*x_3*x_5-x_2*x_3*x_5+3*x_3^2*x_5+4*x_0*x_4*x_5+12*x_1*x_4*x_5-7*x_2*x_4*x_5-3*x_3*x_4*x_5+4*x_1*x_5^2, x_1^2*x_2+17*x_1*x_2*x_3+6*x_0*x_3^2+9*x_1*x_3^2+7*x_2*x_3^2-2*x_3^3-9*x_0^2*x_4+36*x_0*x_1*x_4-44*x_1^2*x_4+3*x_0*x_2*x_4+5*x_1*x_2*x_4-7*x_2^2*x_4-47*x_0*x_3*x_4+21*x_1*x_3*x_4+2*x_2*x_3*x_4+16*x_3^2*x_4+24*x_1*x_4^2-16*x_2*x_4^2+7*x_3*x_4^2+7*x_4^3+3*x_0*x_1*x_5-6*x_1^2*x_5-9*x_1*x_2*x_5-12*x_0*x_3*x_5-8*x_1*x_3*x_5+5*x_3^2*x_5+5*x_0*x_4*x_5+19*x_1*x_4*x_5-12*x_2*x_4*x_5-7*x_3*x_4*x_5+5*x_1*x_5^2, x_0*x_1*x_2+29*x_1*x_2*x_3+11*x_0*x_3^2+15*x_1*x_3^2+12*x_2*x_3^2-4*x_3^3-16*x_0^2*x_4+62*x_0*x_1*x_4-74*x_1^2*x_4+5*x_0*x_2*x_4+9*x_1*x_2*x_4-12*x_2^2*x_4-80*x_0*x_3*x_4+35*x_1*x_3*x_4+4*x_2*x_3*x_4+27*x_3^2*x_4+40*x_1*x_4^2-27*x_2*x_4^2+12*x_3*x_4^2+12*x_4^3+5*x_0*x_1*x_5-10*x_1^2*x_5-16*x_1*x_2*x_5-21*x_0*x_3*x_5-14*x_1*x_3*x_5+9*x_3^2*x_5+9*x_0*x_4*x_5+33*x_1*x_4*x_5-21*x_2*x_4*x_5-12*x_3*x_4*x_5+9*x_1*x_5^2, x_0^2*x_2+49*x_1*x_2*x_3+19*x_0*x_3^2+25*x_1*x_3^2+20*x_2*x_3^2-7*x_3^3-28*x_0^2*x_4+106*x_0*x_1*x_4-124*x_1^2*x_4+8*x_0*x_2*x_4+16*x_1*x_2*x_4-20*x_2^2*x_4-134*x_0*x_3*x_4+58*x_1*x_3*x_4+7*x_2*x_3*x_4+45*x_3^2*x_4+66*x_1*x_4^2-45*x_2*x_4^2+20*x_3*x_4^2+20*x_4^3+9*x_0*x_1*x_5-18*x_1^2*x_5-28*x_1*x_2*x_5-37*x_0*x_3*x_5-23*x_1*x_3*x_5+16*x_3^2*x_5+16*x_0*x_4*x_5+57*x_1*x_4*x_5-36*x_2*x_4*x_5-20*x_3*x_4*x_5+16*x_1*x_5^2, x_1^3+47*x_1*x_2*x_3+18*x_0*x_3^2+23*x_1*x_3^2+19*x_2*x_3^2-7*x_3^3-24*x_0^2*x_4+97*x_0*x_1*x_4-117*x_1^2*x_4+8*x_0*x_2*x_4+16*x_1*x_2*x_4-19*x_2^2*x_4-127*x_0*x_3*x_4+54*x_1*x_3*x_4+7*x_2*x_3*x_4+42*x_3^2*x_4-x_0*x_4^2+62*x_1*x_4^2-42*x_2*x_4^2+19*x_3*x_4^2+19*x_4^3+9*x_0*x_1*x_5-16*x_1^2*x_5-25*x_1*x_2*x_5-33*x_0*x_3*x_5-23*x_1*x_3*x_5+14*x_3^2*x_5+14*x_0*x_4*x_5+51*x_1*x_4*x_5-33*x_2*x_4*x_5-19*x_3*x_4*x_5+14*x_1*x_5^2, x_0*x_1^2+79*x_1*x_2*x_3+29*x_0*x_3^2+40*x_1*x_3^2+32*x_2*x_3^2-11*x_3^3-41*x_0^2*x_4+164*x_0*x_1*x_4-196*x_1^2*x_4+14*x_0*x_2*x_4+26*x_1*x_2*x_4-32*x_2^2*x_4-214*x_0*x_3*x_4+92*x_1*x_3*x_4+11*x_2*x_3*x_4+71*x_3^2*x_4-2*x_0*x_4^2+105*x_1*x_4^2-71*x_2*x_4^2+32*x_3*x_4^2+32*x_4^3+14*x_0*x_1*x_5-26*x_1^2*x_5-41*x_1*x_2*x_5-55*x_0*x_3*x_5-38*x_1*x_3*x_5+23*x_3^2*x_5+23*x_0*x_4*x_5+85*x_1*x_4*x_5-55*x_2*x_4*x_5-32*x_3*x_4*x_5+23*x_1*x_5^2, x_0^2*x_1+133*x_1*x_2*x_3+48*x_0*x_3^2+68*x_1*x_3^2+54*x_2*x_3^2-18*x_3^3-70*x_0^2*x_4+278*x_0*x_1*x_4-330*x_1^2*x_4+24*x_0*x_2*x_4+44*x_1*x_2*x_4-54*x_2^2*x_4-361*x_0*x_3*x_4+156*x_1*x_3*x_4+18*x_2*x_3*x_4+120*x_3^2*x_4-4*x_0*x_4^2+177*x_1*x_4^2-120*x_2*x_4^2+54*x_3*x_4^2+54*x_4^3+23*x_0*x_1*x_5-44*x_1^2*x_5-69*x_1*x_2*x_5-93*x_0*x_3*x_5-63*x_1*x_3*x_5+39*x_3^2*x_5+39*x_0*x_4*x_5+144*x_1*x_4*x_5-93*x_2*x_4*x_5-54*x_3*x_4*x_5+39*x_1*x_5^2, x_0^3+224*x_1*x_2*x_3+80*x_0*x_3^2+115*x_1*x_3^2+91*x_2*x_3^2-30*x_3^3-119*x_0^2*x_4+470*x_0*x_1*x_4-555*x_1^2*x_4+41*x_0*x_2*x_4+75*x_1*x_2*x_4-91*x_2^2*x_4-608*x_0*x_3*x_4+263*x_1*x_3*x_4+30*x_2*x_3*x_4+202*x_3^2*x_4-8*x_0*x_4^2+297*x_1*x_4^2-202*x_2*x_4^2+91*x_3*x_4^2+91*x_4^3+39*x_0*x_1*x_5-76*x_1^2*x_5-118*x_1*x_2*x_5-158*x_0*x_3*x_5-105*x_1*x_3*x_5+67*x_3^2*x_5+68*x_0*x_4*x_5+245*x_1*x_4*x_5-158*x_2*x_4*x_5-91*x_3*x_4*x_5+67*x_1*x_5^2);", "idX = ideal(x_1^2*x_3+x_0*x_2*x_3-6*x_1*x_2*x_3-x_0*x_3^2-4*x_1*x_3^2-3*x_2*x_3^2+2*x_0^2*x_4-10*x_0*x_1*x_4+13*x_1^2*x_4-x_0*x_2*x_4-3*x_1*x_2*x_4+3*x_2^2*x_4+14*x_0*x_3*x_4-8*x_1*x_3*x_4-4*x_3^2*x_4+x_0*x_4^2-7*x_1*x_4^2+4*x_2*x_4^2-2*x_3*x_4^2-2*x_4^3-x_0*x_1*x_5+x_1^2*x_5+2*x_1*x_2*x_5+3*x_0*x_3*x_5+3*x_1*x_3*x_5-x_3^2*x_5-x_0*x_4*x_5-4*x_1*x_4*x_5+3*x_2*x_4*x_5+2*x_3*x_4*x_5-x_1*x_5^2);", "time X = specialCubicFourfold(idS,idX,NumNodes=>3);", "time describe X"}} 

document {Key => {(specialCubicFourfold, Ideal), (specialCubicFourfold, String, Ring), (specialCubicFourfold, String)}, 
Headline => "random special cubic fourfold", 
Usage => "specialCubicFourfold S"|newline|"specialCubicFourfold(S,NumNodes=>n)", 
Inputs => {"S" => Ideal => {"the ideal of an irreducible surface in ", TEX///$\mathbb{P}^5$///}}, 
Outputs => {SpecialCubicFourfold => {"a random cubic fourfold containing the given surface"}}, 
EXAMPLE {"use Grass(0,5,ZZ/33331);", "-- quintic del Pezzo surface"|newline|"S = ideal(p_2*p_4-p_1*p_5, p_0*p_4-p_1*p_5-p_3*p_5+p_4*p_5, p_2*p_3-p_0*p_5, p_1*p_3-p_1*p_5-p_3*p_5+p_4*p_5, p_0*p_1-p_1*p_2-p_0*p_5+p_1*p_5);", "X = specialCubicFourfold(S,NumNodes=>0);", "discriminant X"}, 
PARA{"Some random cubic fourfolds can also be obtained by passing strings. For instance, an object as above is also given as follows."}, 
EXAMPLE {"specialCubicFourfold(\"quintic del Pezzo surface\");"}} 

document {Key => {coneOfLines, (coneOfLines, Ideal, Ideal)}, 
Headline => "cone of lines on a subvariety passing through a point", 
Usage => "coneOfLines(X,p)", 
Inputs => {"X" => Ideal => {"the ideal of a subvariety of ", TEX///$\mathbb{P}^n$///}, "p" => Ideal => {"the ideal of a point on ", TEX///$X$///}}, 
Outputs => {Ideal => {"the ideal of the subscheme of ",TEX///$\mathbb{P}^n$///, " consisting of the union of all lines contained in ",TEX///$X$///, " and passing through ",TEX///$p$///}}, 
PARA{"In the example below we compute the cone of lines passing through the generic point of a smooth del Pezzo fourfold in ",TEX///$\mathbb{P}^7$///, "."}, 
EXAMPLE {"K := frac(QQ[a,b,c,d,e]); P4 = K[t_0..t_4]; phi = rationalMap(minors(2,matrix{{t_0,t_1,t_2},{t_1,t_2,t_3}}) + t_4,2);", "X = image phi", "p = phi minors(2,(vars K)||(vars P4))", "time V = coneOfLines(X,p)", "? V"}} 

undocumented {(coneOfLines, Ideal, Ideal, ZZ)} 

document {Key => {grassmannianHull, (grassmannianHull, SpecialGushelMukaiFourfold)}, 
Headline => "grassmannian hull of a Gushel-Mukai fourfold", 
Usage => "grassmannianHull X", 
Inputs => {"X" => SpecialGushelMukaiFourfold}, 
Outputs => {Ideal => {"the ideal of a fivefold of degree 5 ",TEX///$Y\subset\mathbb{P}^8$///," such that ",TEX///$X\subset Y$///," is a quadric section (the fourfold ",TEX///$X$///," is of ordinary type if and only if ",TEX///$Y$///," is smooth)"}}}, 

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

document {Key => {ideals, (ideals, SpecialCubicFourfold), (ideals, SpecialGushelMukaiFourfold)}, 
Headline => "corresponding ideals", 
Usage => "ideals X", 
Inputs => {"X" => SpecialCubicFourfold => {"or ", TO SpecialGushelMukaiFourfold}}, 
Outputs => {Ideal => {"the ideal of the special surface contained in the fourfold"}, Ideal => {"the ideal of the fourfold ", TT "X"}}, 
EXAMPLE {"X = specialCubicFourfold \"quintic del Pezzo surface\";", "? first ideals X", "? last ideals X"}} 

document {Key => {(ideal, SpecialCubicFourfold), (ideal, SpecialGushelMukaiFourfold)}, 
Headline => "ideal of the fourfold", 
Usage => "ideal X", 
Inputs => {"X" => SpecialCubicFourfold => {"or ", TO SpecialGushelMukaiFourfold}}, 
Outputs => {Ideal => {"the ideal of the fourfold in the ambient polynomial ring"}}} 

document {Key => {(ring, SpecialCubicFourfold), (ring, SpecialGushelMukaiFourfold)}, 
Headline => "ambient polynomial ring of the fourfold", 
Usage => "ring X", 
Inputs => {"X" => SpecialCubicFourfold => {"or ", TO SpecialGushelMukaiFourfold}}, 
Outputs => {Ring => {"the common ambient polynomial ring of the fourfold and of the surface contained in it."}}} 

-- document {Key => {secantCone, (secantCone, Ideal, Ideal)}, 
-- Headline => "secant cone of a subvariety with respect a point", 
-- Usage => "secantCone(X,p)", 
-- Inputs => {"X" => Ideal => {"the ideal of an irreducible subvariety of ", TT"PP^n"}, "p" => Ideal => {"the ideal of a point in ", TT"PP^n"}}, 
-- Outputs => {Ideal => {"the ideal of the subscheme of ", TT"PP^n", " consisting of the union of all secant lines to ", TT"X", " passing through ", TT"p"}}, 
-- EXAMPLE {"-- a quintic del Pezzo surface"|newline|"X = image rationalMap((ZZ/33331)[vars(0..2)],{3,4});", "p = point ring X;", "V = secantCone(X,p);", "(codim V,degree V)"}} 

document {Key => {unirationalParametrization, (unirationalParametrization, SpecialCubicFourfold), (unirationalParametrization, SpecialCubicFourfold, Ideal), (unirationalParametrization, SpecialGushelMukaiFourfold)}, 
Headline => "unirational parametrization", 
Usage => "unirationalParametrization X", 
Inputs => {"X" => SpecialCubicFourfold => {"or ", TO SpecialGushelMukaiFourfold}}, 
Outputs => {RationalMap => {"a rational map of degree 2 whose image is ", TT "X"}}, 
PARA{"The degree of the forms defining the returned map is 10 in the case of cubic fourfolds, and 26 in the case of GM fourfolds."}, 
EXAMPLE {"K = ZZ/10000019; S = ideal(random(3,Grass(0,5,K)), random(1,Grass(0,5,K)), random(1,Grass(0,5,K)));", "X = specialCubicFourfold S;", "time f = unirationalParametrization X;", "describe f", "image f == ideal X", "degreeOfRationalMap f"}, 
SeeAlso => {(parametrize, SpecialCubicFourfold)}} 

document {Key => {(parametrize, SpecialCubicFourfold), (parametrize, SpecialGushelMukaiFourfold)}, 
Headline => "rational parametrization", 
Usage => "parametrize X", 
Inputs => {"X" => SpecialCubicFourfold => {"or ", TO SpecialGushelMukaiFourfold}}, 
Outputs => {RationalMap => {"a birational map from a rational fourfold to ", TT "X"}}, 
PARA{"Some special cubic fourfolds and GM fourfolds are known to be rational. In this case, the method tries to obtain a birational map from ", TEX///$\mathbb{P}^4$///, " (or, e.g., from a quadric hypersurface in ", TEX///$\mathbb{P}^5$///, ") to the fourfold."}, 
EXAMPLE {"X = specialCubicFourfold \"quintic del Pezzo surface\";", "time phi = parametrize X;", "describe phi", "X' = specialGushelMukaiFourfold \"tau-quadric\";", "time phi' = parametrize X';", "describe phi'"}, 
SeeAlso => {unirationalParametrization}} 

document {Key => {(symbol <,SpecialGushelMukaiFourfold)},
Headline => "try to deform to a fourfold of Gushel type",
Usage => "< X", 
Inputs => {"X" => SpecialGushelMukaiFourfold => {"a fourfold of ordinary type"}}, 
Outputs => {SpecialGushelMukaiFourfold => {"a fourfold of Gushel type, a deformation of ",TT"X"}}, 
EXAMPLE {"time X = specialGushelMukaiFourfold \"quintic del Pezzo surface\";", "? ideal singularLocus grassmannianHull X", "time X' = < X;", "time ? radical saturate ideal singularLocus grassmannianHull X'"}} 

undocumented{(point,SpecialCubicFourfold), (point,SpecialGushelMukaiFourfold)}

document {Key => {associatedK3surface, (associatedK3surface, SpecialCubicFourfold), (associatedK3surface, SpecialGushelMukaiFourfold), [associatedK3surface, Verbose]}, 
Headline => "associated K3 surface to a rational cubic fourfold", 
Usage => "associatedK3surface X", 
Inputs => {"X" => SpecialCubicFourfold => {"containing a surface ", TEX///$S\subset\mathbb{P}^5$///," that admits a congruence of ",TEX///$(3e-1)$///,"-secant curves of degree ",TEX///$e$///}}, 
Outputs => {{"the dominant ",TO2{RationalMap,"rational map"}," ",TEX///\psi:\mathbb{P}^5 ---> W///," defined by the linear system of hypersurfaces of degree ",TEX///$3e-1$///," having points of multiplicity ",TEX///$e$///," along ",TEX///$S$///,";"}, {"the ",TO2{Ideal,"ideal"}," of the surface ",TEX///$U\subset W$///," determining the inverse map of the restriction of ",TEX///$\psi$///," to ",TEX///$X$///,";"}, {"the ",TO2{List,"list"}," of the ideals of the exceptional curves on the surface ",TEX///$U$///,";"}, {"a ",TO2{RationalMap,"rational map"}," of degree 1 from the surface ",TEX///$U$///," to a minimal K3 surface, the associated K3 surface to ",TEX///$X$///,"."}},
PARA {"Thus, the code ",TT "image last associatedK3surface X"," gives the ideal of the (minimal) associated K3 surface to ",TT"X",". For more details and notation, see the paper ",HREF{"https://arxiv.org/abs/1909.01263","Trisecant Flops, their associated K3 surfaces and the rationality of some Fano fourfolds"},"."},
EXAMPLE {"X = specialCubicFourfold \"quartic scroll\";", "describe X", "time (psi,U,C,f) = associatedK3surface(X,Verbose=>true);", "describe psi", "? U", "? first C", "? image f"},
PARA {"The same method can be also applied to a ",TO2{SpecialGushelMukaiFourfold, "Gushel-Mukai fourfold"},". In this case the surface is required to admit a congruence of ",TEX///$(2e-1)$///,"-secant curves of degree ",TEX///$e$///," inside the unique del Pezzo fivefold containing the fourfold."},
EXAMPLE {"X = specialGushelMukaiFourfold \"tau-quadric\";", "describe X", "time (psi,U,C,f) = associatedK3surface X;", "describe psi", "? U", "? first C -- two disjoint lines"},
SeeAlso => {detectCongruence}} 

------------------------------------------------------------------------
------------------------------- Tests ----------------------------------
------------------------------------------------------------------------

TEST///
(S,f) = schubertCycle({2,2},Grass(1,4,ZZ/33331,Variable=>"x"),"standard");
assert(f S == tangentialChowForm(ideal((Grass(0,4,ZZ/33331,Variable=>"x"))_3,(Grass(0,4,ZZ/33331,Variable=>"x"))_4),1,1));
--
(S,f) = schubertCycle({3,2,1},Grass(2,5,ZZ/33331,Variable=>"x"),"standard");
use ring S;
assert(f S == ideal(x_(3,4,5),x_(2,4,5),x_(1,4,5),x_(0,4,5),x_(2,3,5),x_(1,3,5),x_(0,3,5),x_(1,2,5),x_(0,2,5),x_(0,1,5),x_(2,3,4),x_(1,3,4),x_(0,3,4),x_(1,2,4),x_(1,2,3)));
///

-- end

TEST ///
K := ZZ/3331;
--
time X1 = specialGushelMukaiFourfold("sigma-plane",K,InputCheck=>0);
time assert(discriminant X1 == 10)
--
time X2 = specialGushelMukaiFourfold("rho-plane",K,InputCheck=>0);
time assert(discriminant X2 == 12)
--
time X3 = specialGushelMukaiFourfold("tau-quadric",K,InputCheck=>0);
time assert(discriminant X3 == 10)
--
time X4 = specialGushelMukaiFourfold("cubic scroll",K,InputCheck=>0);
time assert(discriminant X4 == 12)
--
time X5 = specialGushelMukaiFourfold("quintic",K,InputCheck=>0);
time assert(discriminant X5 == 10)
--
time X6 = specialGushelMukaiFourfold("K3 surface of genus 8",K,InputCheck=>0);
time assert(discriminant X6 == 10)
--
time X7 = specialGushelMukaiFourfold("surface of degree 9 and genus 2",InputCheck=>0);
time assert(discriminant X7 == 20)
///

TEST///
K = ZZ/65521;
X = for i from 1 to 21 list (
   A = tables(i,K);
   time specialGushelMukaiFourfold((rationalMap(A_0,max flatten degrees A_0,Dominant=>2)) A_1,InputCheck=>0)
); 
assert(apply(X,w -> time discriminant w) === {10,10,10,10,10,10,12,12,12,16,16,16,18,18,18,18,20,20,24,24,26})
S = apply(X,w -> first ideals w);
assert(apply(S,s -> degree s) === {2, 4, 14, 5, 9, 1, 3, 7, 1, 10, 10, 14, 12, 8, 9, 11, 9, 7, 10, 4, 12})
debug SpecialFanoFourfolds
assert(apply(X,x -> x#"label") === toList(21:null));
assert(apply(X,x -> recognize x) === toList(1..21));
assert(apply(X,x -> x#"label") === toList(1..21));
///


