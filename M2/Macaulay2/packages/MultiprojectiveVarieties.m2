
-*
   Copyright 2020, Giovanni Staglianò.

   You may redistribute this file under the terms of the GNU General Public
   License as published by the Free Software Foundation, either version 2 of
   the License, or any later version.
*-

if version#"VERSION" < "1.17" then error "this package requires Macaulay2 version 1.17 or newer";

newPackage(
    "MultiprojectiveVarieties",
    Version => "1.0", 
    Date => "January 7, 2021",
    Authors => {{Name => "Giovanni Staglianò", Email => "giovannistagliano@gmail.com"}},
    Headline => "multi-projective varieties and multi-rational maps",
    Keywords => {"Projective Algebraic Geometry"},
    PackageImports => {"PrimaryDecomposition", "Cremona","SparseResultants"},
    PackageExports => {"Cremona","SparseResultants"},
    DebuggingMode => false,
    Reload => false
)

export{"MultiprojectiveVariety", "projectiveVariety", "Saturate", "projections", "fiberProduct", "Probabilistic",
       "MultirationalMap", "multirationalMap", "baseLocus"}

debug Cremona;

MultiprojectiveVariety = new Type of MutableHashTable;

globalAssignment MultiprojectiveVariety;

MultiprojectiveVariety.synonym = "projective variety";

projectiveVariety = method(TypicalValue => MultiprojectiveVariety, Options => {MinimalGenerators => true, Saturate => true});

projectiveVariety Ideal := o -> I -> (
    R := ring I;
    if not isPolynomialRing R then error "expected an ideal in a polynomial ring";
    if not isField coefficientRing R then error "the coefficient ring needs to be a field";
    m := multigens R;
    if flatten m != gens R then error "the given grading on the polynomial ring is not allowed: the degree of each variable must be a standard basis vector of ZZ^r in the commonly used order";
    if not isHomogeneous I then error "expected a (multi)-homogeneous ideal";
    if o.Saturate 
    then for x in m do I = saturate(I,ideal x,MinimalGenerators=>o.MinimalGenerators)
    else if o.MinimalGenerators then I = trim I;
    new MultiprojectiveVariety from {
        "idealVariety" => I,
        "ringVariety" => null,
        "dimVariety" => null,        
        "dimAmbientSpaces" => apply(m, n -> (#n)-1),
        "multigens" => m,
        "multidegree" => null,
        "ambient" => null,
        "top" => null,
        "singularLocus" => null,
        "segreMap" => null,
        "flattenMap" => null,
        "projections" => null,
        "euler" => null
    }
);

projectiveVariety Ring := o -> R -> projVarFromRing(R,o.MinimalGenerators,o.Saturate);

projVarFromRing = memoize ((R,optMinGen,optSat) -> (
    if not isPolynomialRing ambient R then error "expected the ambient ring to be polynomial";
    X := projectiveVariety(ideal R,MinimalGenerators=>false,Saturate=>false);
    I := X#"idealVariety";
    m := X#"multigens";
    if optSat then (
        for x in m do I = saturate(I,ideal x,MinimalGenerators=>optMinGen);
        if I != X#"idealVariety" then error "the ideal is not multisaturated";
    );
    X#"ringVariety" = R;
    X
));

projectiveVariety MultidimensionalMatrix := o -> A -> projectiveVariety(ideal(A!),MinimalGenerators=>true,Saturate=>false);

expression MultiprojectiveVariety := X -> expression expressionVar(dim X,X#"dimAmbientSpaces");

hasAttribute = value Core#"private dictionary"#"hasAttribute"
getAttribute = value Core#"private dictionary"#"getAttribute"
ReverseDictionary = value Core#"private dictionary"#"ReverseDictionary"

net MultiprojectiveVariety := X -> (
   if hasAttribute(X,ReverseDictionary) then toString getAttribute(X,ReverseDictionary) else "ProjectiveVariety"
);

MultiprojectiveVariety#{Standard,AfterPrint} = MultiprojectiveVariety#{Standard,AfterNoPrint} = X -> (
  << endl << concatenate(interpreterDepth:"o") << lineNumber << " : " << "ProjectiveVariety, " << expression X << endl;
);

ideal MultiprojectiveVariety := X -> X#"idealVariety";

ring MultiprojectiveVariety := X -> (
    if X#"ringVariety" =!= null then return X#"ringVariety";
    X#"ringVariety" = (ring ideal X)/(ideal X)
);

coefficientRing MultiprojectiveVariety := X -> coefficientRing ring ideal X;

ambient MultiprojectiveVariety := X -> (
    if X#"ambient" =!= null then return X#"ambient";
    ambX := if ideal X == 0 then X else projectiveVariety ideal ring ideal X;
    X#"ambient" = ambX
);

dim MultiprojectiveVariety := X -> (
    if X#"dimVariety" =!= null then return X#"dimVariety";
    R := ring ideal X;
    I := ideal X;
    X#"dimVariety" = max(dim I - (# heft R),-1)
);

codim MultiprojectiveVariety := {} >> o -> X -> sum(X#"dimAmbientSpaces") - (dim X);

multidegree MultiprojectiveVariety := X -> (
    if X#"multidegree" =!= null then return X#"multidegree";
    X#"multidegree" = multidegree ideal X
);

degree MultiprojectiveVariety := X -> getMultidegree(multidegree X, X#"dimAmbientSpaces");

projections = method();
projections MultiprojectiveVariety := X -> (
    if X#"projections" =!= null then return X#"projections";
    X#"projections" = apply(X#"multigens",x -> rationalMap sub(matrix{x},ring X))
);

segre MultiprojectiveVariety := X -> (
    if X#"segreMap" =!= null then return X#"segreMap";
    X#"segreMap" = segre ring X
);

describe MultiprojectiveVariety := X -> (
    n := X#"dimAmbientSpaces";
    amb := "empty space";
    if # n >= 1 and min n >= 0 then (
        amb = "PP^"|toString(n_0);
        for i from 1 to #n-1 do amb = amb | " x PP^" | toString(n_i);
    );
    s := "ambient:.............. "|toString(amb)|newline;
    s = s|"dim:.................. "|toString(dim X);
    if dim X == -1 then return s;
    s = s|newline|"codim:................ "|toString(codim X)|newline;
    s = s|"degree:............... "|toString(degree X);
    if codim X == 0 then return s; 
    s = s|newline;
    if # n > 1 then s = s|"multidegree:.......... "|toString(multidegree X)|newline;        
    s = s|"generators:........... "|toString((concatenate for l in degrees X list (toString unsequence toSequence first l)|"^"|(toString(last l)|" ")))|newline;
    purity := X == top X;
    s = s|"purity:............... "|toString(purity); 
    if purity then (
        s = s|newline|"dim sing. l.:......... "|toString(dim singularLocus X); 
        if dim singularLocus X >= 0 then s = s|newline|"gens sing. l.:........ "|toString((concatenate for l in degrees singularLocus X list (toString unsequence toSequence first l)|"^"|(toString(last l)|" ")));
    );
    if # n > 1 then (
        s = s|newline|"Segre embedding:...... "|"map to PP^"|toString(numgens target segre X -1); 
        N := product apply(n, i -> i+1) -1;
        if numgens target segre X -1 < N then s = s|" ⊂ PP^"|toString(N);
    );
    return s;
);

? MultiprojectiveVariety := X -> ? ideal X;

degrees MultiprojectiveVariety := X -> pairs tally degrees ideal X;

singularLocus MultiprojectiveVariety := X -> ( -- assume equidimensionality: X == top X
    if X#"singularLocus" =!= null then return X#"singularLocus";
    if X#"top" =!= null then if X =!= top X then error "expected an equidimensional projective variety";
    I := ideal X;
    X#"singularLocus" = projectiveVariety(I + minors(codim X,jacobian I,Strategy=>Cofactor),MinimalGenerators=>true,Saturate=>true)
);

top MultiprojectiveVariety := X -> (
    if X#"top" =!= null then return X#"top";
    T := top ideal X;
    X#"top" = if T == ideal X then X else projectiveVariety(T,MinimalGenerators=>true,Saturate=>false)
);

decompose MultiprojectiveVariety := {} >> o -> X -> apply(decompose ideal X,D -> projectiveVariety(D,MinimalGenerators=>true,Saturate=>false));

MultiprojectiveVariety == MultiprojectiveVariety := (X,Y) -> (
    if ring ideal X =!= ring ideal Y then error "expected varieties in the same ambient";
    if X === Y or ideal X === ideal Y then return true;
    if dim X != dim Y then return false;
    ideal X == ideal Y
);

flattenMap = method(); 
flattenMap MultiprojectiveVariety := X -> (
    if X#"flattenMap" =!= null then return X#"flattenMap";
    ttt := local ttt;
    X#"flattenMap" = parametrizeProductOfProjectiveSpaces(ring ideal X,ttt)
);

point (MultiprojectiveVariety,Boolean) := (X,b) -> (
    if # X#"dimAmbientSpaces" == 1 then return projectiveVariety(point(ideal X,b),MinimalGenerators=>false,Saturate=>false);
    f := flattenMap X;
    j := (map parametrize point f ideal X) * f;
    p := projectiveVariety sum(projections ambient X,g -> g^* kernel(j * (map g)));
    if b then if not (dim p == 0 and degree p == 1 and isSubset(ideal X,ideal p)) then error("something went wrong in trying to pick a random "|toString(coefficientRing X)|"-rational point on the variety");
    return p;
);
point MultiprojectiveVariety := X -> point(X,true);

MultiprojectiveVariety ** MultiprojectiveVariety := (X,Y) -> (
    K := coefficientRing X;
    if K =!= coefficientRing Y then error "different coefficient rings encountered";
    d := matrix degrees ring ideal X;
    e := matrix degrees ring ideal Y;
    de := (d | matrix pack(numColumns e,apply(numRows d * numColumns e,i->0))) || (matrix pack(numColumns d,apply(numRows e * numColumns d,j->0)) | e);
    x := local x; y := local y;
    n := X#"dimAmbientSpaces"; m := Y#"dimAmbientSpaces";
    R := K[x_0..x_(#n + sum n - 1),y_0..y_(#m + sum m - 1),Degrees=>entries de]; 
    sX := map(R,ring ideal X,submatrix(vars R,0 .. #n + sum n - 1));
    sY := map(R,ring ideal Y,submatrix'(vars R,0 .. #n + sum n - 1));
    XxY := projectiveVariety((sX ideal X) + (sY ideal Y),MinimalGenerators=>true,Saturate=>false);
    XxY#"projections" = apply(projections XxY,apply((projections X)|(projections Y),target),(f,T) -> rationalMap((map f) * (map rationalMap(target f,T)),Dominant=>"notSimplify"));
    XxY
);

MultiprojectiveVariety + MultiprojectiveVariety := (X,Y) -> (
    if ring ideal X =!= ring ideal Y then error "expected varieties in the same ambient";
    projectiveVariety(intersect(ideal X,ideal Y),MinimalGenerators=>true,Saturate=>false)
);

MultiprojectiveVariety - MultiprojectiveVariety := (X,Y) -> (
    if ring ideal X =!= ring ideal Y then error "expected varieties in the same ambient";
    projectiveVariety(quotient(ideal X,ideal Y,MinimalGenerators=>true),MinimalGenerators=>false,Saturate=>false)
);

MultiprojectiveVariety * MultiprojectiveVariety := (X,Y) -> (
    if ring ideal X =!= ring ideal Y then error "expected varieties in the same ambient";
    projectiveVariety(ideal X + ideal Y,MinimalGenerators=>true,Saturate=>true)
);

isSubset (MultiprojectiveVariety,MultiprojectiveVariety) := (X,Y) -> (
    if ring ideal X =!= ring ideal Y then error "expected varieties in the same ambient";
    if X === Y then return true;
    isSubset(ideal Y,ideal X)
);

fiberProductInt = method();
fiberProductInt (MutableHashTable,MutableHashTable) := (phi,psi) -> (
    if target phi =!= target psi then error "expected two morphisms with the same target";
    if not isMorphism phi then <<"--warning: the first map is not a morphism"<<endl;
    if not isMorphism psi then <<"--warning: the second map is not a morphism"<<endl;
    ambX := projectiveVariety ambient source phi;
    ambY := projectiveVariety ambient source psi;
    ambXxY := ambX ** ambY;
    R := ring ambXxY;
    n := numgens ring ambX -1;    
    sx := map(R,ring ambX,submatrix(vars R,{0..n}));
    sy := map(R,ring ambY,submatrix'(vars R,{0..n}));
    I := sx ideal source phi;
    J := sy ideal source psi;
    F := apply(maps phi,f -> sx lift(toMatrix f,ring ambX));
    G := apply(maps psi,g -> sy lift(toMatrix g,ring ambY));
    Z := projectiveVariety(I + J + intersect flatten for f in F list for g in G list saturate(saturate(minors(2,f||g),ideal f),ideal g),MinimalGenerators=>true,Saturate=>true); 
    Z#"projections" = apply(projections Z,projections ambXxY,(f,g) -> rationalMap((map f) * (map rationalMap(target f,target g)),Dominant=>"notSimplify"));
    Z
);

fiberProduct = method(TypicalValue => MultiprojectiveVariety);
fiberProduct (MultihomogeneousRationalMap,MultihomogeneousRationalMap) := (phi,psi) -> fiberProductInt(phi,psi);
fiberProduct (MultihomogeneousRationalMap,RationalMap) := (phi,psi) -> fiberProductInt(phi,psi);
fiberProduct (RationalMap,MultihomogeneousRationalMap) := (phi,psi) -> fiberProductInt(phi,psi);
fiberProduct (RationalMap,RationalMap) := (phi,psi) -> fiberProductInt(phi,psi);

euler (MultiprojectiveVariety,Option) := (X,opt) -> (
    o := toList opt;
    if not(#o == 2 and first o === Probabilistic) then error "Probabilistic is the only available option for euler(MultiprojectiveVariety)";
    if not instance(last o,Boolean) then error "option Probabilistic accepts true or false";
    if X#"euler" =!= null then return X#"euler";
    local e;
    if # X#"dimAmbientSpaces" == 1 then (
        if codim X == 0 then return X#"euler" = numgens ring X;
        e = EulerCharacteristic(ideal X,MathMode=>not last o,Verbose=>false);
     ) else (
        -- <<"--warning: code to be improved"<<endl;
        e = EulerCharacteristic(image segre X,MathMode=>not last o,Verbose=>false);
    );
    if not last o then X#"euler" = e;
    return e;
);

euler MultiprojectiveVariety := X -> euler(X,Probabilistic=>true);


MultirationalMap = new Type of MutableHashTable;

globalAssignment MultirationalMap;

MultirationalMap.synonym = "multi-rational map";

expression MultirationalMap := Phi -> (
    if dim source Phi == -1 or dim target Phi == -1 then return expression("map from " | (toString expression source Phi) | " to " | (toString expression target Phi));
    return expression((if Phi#"isBirational" === true then "birational " else (if Phi#"isDominant" === true then "dominant rational " else "rational "))| "map from " | (toString expression source Phi) | " to " | (toString expression target Phi));
);

net MultirationalMap := Phi -> (
   if hasAttribute(Phi,ReverseDictionary) then toString getAttribute(Phi,ReverseDictionary) else "a multi-rational map"
);

MultirationalMap#{Standard,AfterPrint} = MultirationalMap#{Standard,AfterNoPrint} = Phi -> (
  << endl << concatenate(interpreterDepth:"o") << lineNumber << " : " << class Phi << " (" << expression Phi << ")" << endl;
);

multirationalMap = method(TypicalValue => MultirationalMap);

multirationalMap (List,MultiprojectiveVariety) := (L,Y) -> (
    if not (# L > 0 and all(L,f -> instance(f,RationalMap) or instance(f,MultihomogeneousRationalMap))) then error "expected a list of rational maps";
    R := unique apply(L,source);
    if #R != 1 then error "expected a list of rational maps from the same source variety";
    R = first R;
    K := coefficientRing ambient R;
    if K =!= coefficientRing Y then error("expected a multi-projective variety defined over "|toString(K));
    m := apply(L,f -> f#"dimAmbientSource");
    if m =!= Y#"dimAmbientSpaces"
    then if # m == 1 
         then error("expected a subvariety of PP^"|toString(first m))
         else error("expected a subvariety of a product of "|toString(# m)|" projective spaces of dimensions "|(toString toSequence m));
    new MultirationalMap from {
        "maps" => L,
        "target" => Y,
        "source" => projectiveVariety(R,Saturate=>false),
        "image" => null,
        "isDominant" => null,
        "isBirational" => null,
        "compositionWithSegreEmbedding" => null,
        "graph" => null,
        "baseLocus" => null,
        "inverse" => null
    }
);

multirationalMap List := L -> (
    if not (# L > 0 and all(L,f -> instance(f,RationalMap) or instance(f,MultihomogeneousRationalMap))) then error "expected a list of rational maps";
    Y := projectiveVariety(target first L,Saturate=>false);
    for i from 1 to #L-1 do Y = Y ** (projectiveVariety(target L_i,Saturate=>false));
    Phi := multirationalMap(L,Y);
    if #L == 1 then Phi#"isDominant" = (first L)#"isDominant";
    if #L == 1 then Phi#"isBirational" = (first L)#"isBirational";
    Phi
);

multirationalMap (MultirationalMap,MultiprojectiveVariety) := (Phi,Y) -> (
    if Y === target Phi then return Phi;
    Psi := multirationalMap(factor Phi,Y);
    if Phi#"image" === Y then Psi#"isDominant" = true;
    return Psi;
);

check MultirationalMap := o -> Phi -> (
    Y := target Phi;
    L := factor Phi;
    P := apply(projections Y,L,(p,f) -> if target p === target f then p else rationalMap(source p,ambient target f,matrix p));
    for i to #L -1 do if not isSubset(image P_i,image L_i) then error "the target variety is not compatible with the maps";
    Phi
);

source MultirationalMap := Phi -> Phi#"source";

target MultirationalMap := Phi -> Phi#"target";

coefficientRing MultirationalMap := Phi -> coefficientRing target Phi;

factor MultirationalMap := o -> Phi -> Phi#"maps";

toRingMap = method();
toRingMap (MultirationalMap,Ring) := (Phi,R) -> (
    F := factor Phi;
    if #F == 1 then if R === target first F then return map first F;
    M := matrix first F;
    for i from 1 to #F-1 do M = M | (matrix F_i);
    map(ring source Phi,R,M)
);

segre MultirationalMap := Phi -> (
    if Phi#"compositionWithSegreEmbedding" =!= null then return Phi#"compositionWithSegreEmbedding";
    s := segre target Phi;
    f := toRingMap(Phi,source s);
    Phi#"compositionWithSegreEmbedding" = rationalMap(f * (map s),Dominant=>"notSimplify")
);

compose (MultirationalMap,MultirationalMap) := (Phi,Psi) -> (
    if ring target Phi =!= ring source Psi then error "multi-rational maps not composable: incompatible target and source";
    f := toRingMap(Phi,ring source Psi);
    Eta := multirationalMap(apply(factor Psi,g -> rationalMap(compose(f,map g),Dominant=>"notSimplify")),target Psi);
    try assert(ring source Eta === ring source Phi) else error "internal error encountered: bad source found";
    Eta#"source" = source Phi;
    if Phi#"isDominant" === true and Psi#"isDominant" === true then Eta#"isDominant" = true;
    if Phi#"isBirational" === true and Psi#"isBirational" === true then Eta#"isBirational" = true;
    return Eta;
);

MultirationalMap * MultirationalMap := (Phi,Psi) -> compose(Phi,Psi);

MultirationalMap * MultihomogeneousRationalMap := (Phi,Psi) -> compose(Phi,multirationalMap {Psi});
MultirationalMap * RationalMap := (Phi,Psi) -> compose(Phi,multirationalMap {Psi});
MultihomogeneousRationalMap * MultirationalMap := (Phi,Psi) -> compose(multirationalMap {Phi},Psi);
RationalMap * MultirationalMap := (Phi,Psi) -> compose(multirationalMap {Phi},Psi);

MultirationalMap == MultirationalMap := (Phi,Psi) -> (
    if ring ideal source Phi =!= ring ideal source Psi or source Phi != source Psi then error "expected multi-rational maps with the same source";
    if ring ideal target Phi =!= ring ideal target Psi or target Phi != target Psi then error "expected multi-rational maps with the same target";
    F := factor Phi;
    G := factor Psi;
    assert(#F == #G);
    for i to #F-1 do if minors(2,(matrix F_i)||(matrix G_i)) != 0 then return false;
    return true;
);

MultirationalMap == MultihomogeneousRationalMap := (Phi,Psi) -> Phi == multirationalMap {Psi};
MultirationalMap == RationalMap := (Phi,Psi) -> Phi == multirationalMap {Psi};
MultihomogeneousRationalMap == MultirationalMap := (Phi,Psi) -> multirationalMap {Phi} == Psi;
RationalMap == MultirationalMap := (Phi,Psi) -> multirationalMap {Phi} == Psi;

multirationalMap MultiprojectiveVariety := X -> (
    I := multirationalMap(apply(multigens ring X,rationalMap),X);
    try assert(ring source I === ring X) else error "internal error encountered: bad source found";
    I#"source" = X;
    I#"isDominant" = true;
    I#"isBirational" = true;
    I
);

multirationalMap (MultiprojectiveVariety,MultiprojectiveVariety) := (X,Y) -> (
    if X === Y then return multirationalMap X;
    I := multirationalMap(super multirationalMap X,Y);
    try return check I else error "not able to define a natural map between the two varieties";
);

MultirationalMap == ZZ := (Phi,n) -> (
    if n =!= 1 then error "encountered integer other than 1 in comparison with a multi-rational map";
    if source Phi =!= target Phi then error "source and target are different";
    Phi == multirationalMap source Phi
);
ZZ == MultirationalMap := (n,Phi) -> Phi == n;

MultirationalMap ^ ZZ := (Phi,j) -> (
   if j == 0 then if source Phi === target Phi then return multirationalMap(source Phi) else error "expected non-zero integer";
   if j < 0 then (Psi := inverse Phi; return (Psi^(-j)));
   Psi2 := Phi; for i from 1 to j-1 do Psi2 = Psi2 * Phi; 
   return Psi2;
);

MultirationalMap MultiprojectiveVariety := (Phi,Z) -> (
    if ring ideal source Phi =!= ring ideal Z then error "expected a multi-projective variety in the same ambient of the source of the map";
    if not isSubset(Z,source Phi) then error "expected a subvariety of the source of the map";
    F := apply(factor Phi,f -> lift(matrix f,ring ambient source Phi));
    m := # F;
    t := local t;
    R := (coefficientRing Phi)[t_0 .. t_(m-1),gens ring ambient source Phi,gens ring ambient target Phi,MonomialOrder => Eliminate (m + numgens ring ambient source Phi)];
    y := (target Phi)#"multigens";
    I := sub(ideal Z,R) + sum(m,i -> ideal(sub(matrix{y_i},R) - t_i * sub(F_i,R)));
    projectiveVariety(sub(ideal selectInSubring(1,gens gb I),ring ambient target Phi),MinimalGenerators=>true,Saturate=>true)
);

image MultirationalMap := Phi -> (
    if Phi#"image" =!= null then return Phi#"image";
    if Phi#"isDominant" === true then return target Phi;
    Phi#"image" = Phi (source Phi);
    Phi#"isDominant" = Phi#"image" == target Phi;
    return Phi#"image";
);

inverseImageViaMultirationalMap = (Phi,Z,b) -> (
    if ring ideal target Phi =!= ring ideal Z then error "expected a multi-projective variety in the same ambient of the target of the map";
    if not isSubset(Z,target Phi) then error "expected a subvariety of the target of the map";
    if b then (
        return projectiveVariety trim lift((segre Phi)^** ((segre target Phi) ideal Z),ring ambient source Phi);
    ) else (
        return projectiveVariety trim lift((segre Phi)^* ((segre target Phi) ideal Z),ring ambient source Phi);
    );
);

MultirationalMap ^* := (Phi) -> MultiprojectiveVariety := (Z) -> inverseImageViaMultirationalMap(Phi,Z,false);

MultirationalMap ^** MultiprojectiveVariety := (Phi,Z) -> inverseImageViaMultirationalMap(Phi,Z,true);

graph MultirationalMap := o -> Phi -> (
    if Phi#"graph" =!= null then return Phi#"graph";
    if o.BlowUpStrategy =!= "Eliminate" then error "given strategy is not available";
    F := apply(factor Phi,f -> lift(matrix f,ring ambient source Phi));
    m := # F;
    K := coefficientRing Phi;
    t := local t; x := local x; y := local y;
    R := K[t_0 .. t_(m-1), x_0 .. x_(numgens ring ambient source Phi -1), y_0 .. y_(numgens ring ambient target Phi -1), MonomialOrder => Eliminate m];
    subx := map(R,ring ambient source Phi,toList(x_0 .. x_(numgens ring ambient source Phi -1)));
    suby := map(R,ring ambient target Phi,toList(y_0 .. y_(numgens ring ambient target Phi -1)));
    yy := (target Phi)#"multigens";
    I := subx(ideal source Phi) + sum(m,i -> ideal(suby(matrix{yy_i}) - t_i * subx(F_i)));
    d := matrix degrees ring ambient source Phi;
    e := matrix degrees ring ambient target Phi;
    de := (d | matrix pack(numColumns e,apply(numRows d * numColumns e,i->0))) || (matrix pack(numColumns d,apply(numRows e * numColumns d,j->0)) | e);
    R' := K[take(gens R,-(numgens R - m)),Degrees=>entries de];
    G := projectiveVariety(sub(ideal selectInSubring(1,gens gb I),R'),MinimalGenerators=>true,Saturate=>false);
    pr := projections G;
    psi1 := multirationalMap(take(pr,# (source Phi)#"dimAmbientSpaces"),source Phi);
    psi2 := multirationalMap(take(pr,-(# (target Phi)#"dimAmbientSpaces")),target Phi);
    psi1#"isDominant" = true;
    psi1#"isBirational" = true;
    psi2#"isDominant" = Phi#"isDominant";
    psi2#"isBirational" = Phi#"isBirational";
    Phi#"graph" = (psi1,psi2)
);

projectiveDegrees MultirationalMap := o -> Phi -> projectiveDegrees(segre Phi,MathMode=>o.MathMode,NumDegrees=>o.NumDegrees,BlowUpStrategy=>o.BlowUpStrategy,Verbose=>o.Verbose);

multidegree MultirationalMap := Phi -> multidegree segre Phi;

degreeMap MultirationalMap := o -> Phi -> degreeMap(segre Phi,MathMode=>o.MathMode,BlowUpStrategy=>o.BlowUpStrategy,Verbose=>o.Verbose);

degree MultirationalMap := Phi -> degree segre Phi;

baseLocus = method(TypicalValue => MultiprojectiveVariety);
baseLocus MultirationalMap := Phi -> (
    if Phi#"baseLocus" =!= null then return Phi#"baseLocus";
    Phi#"baseLocus" = projectiveVariety trim lift(intersect apply(factor Phi,ideal),ring ambient source Phi)
);

baseLocus RationalMap := Phi -> baseLocus multirationalMap {Phi};
baseLocus MultihomogeneousRationalMap := Phi -> baseLocus multirationalMap {Phi};

isMorphism MultirationalMap := Phi -> dim baseLocus Phi == -1;

inverse (MultirationalMap,Option) := (Phi,opt) -> (
    o := toList opt;
    if not(#o == 2 and first o === MathMode) then error "MathMode is the only available option for inverse(MultirationalMap)";
    if not instance(last o,Boolean) then error "option MathMode accepts true or false";
    if Phi#"inverse" =!= null then return Phi#"inverse";
    if Phi#"isBirational" === false or Phi#"isDominant" === false then error "expected a birational map";
    if Phi#"isBirational" === null then if dim source Phi != dim target Phi then (Phi#"isBirational" = false; error "expected a birational map"); 
    Gr := source first graph Phi;
    Sub := map(ring target Phi,ring ambient Gr,matrix{toList((numgens ring ambient source Phi):0_(ring ambient target Phi))}|(vars ring ambient target Phi));
    r := # (source Phi)#"dimAmbientSpaces";
    x := apply(take(Gr#"multigens",r),g -> matrix{g});
    d := entries diagonalMatrix toList(r:1);
    gensGr := flatten entries gens ideal Gr;
    local I; local J; local F; local phi;
    L := for i to r-1 list (
        I = select(gensGr,g -> take(degree g,r) == d_i);
        J = matrix apply(I,g -> flatten entries diff(x_i,g));
        F = entries transpose mingens kernel Sub J;
        if #F == 0 then error "not able to obtain an inverse multi-rational map; the map may not be birational";
        phi = rationalMap first F;
        phi#"maps" = apply(F,f -> map(source phi,target phi,f));
        phi#"map" = first phi#"maps";
        if last o then (for f in phi#"maps" do try assert(rationalMap f == phi) else error "something went wrong in calculating the inverse map");
        phi
    );
    Psi := multirationalMap(L,source Phi);
    try assert(ring source Psi === ring target Phi) else error "internal error encountered";  
    Psi#"source" = target Phi;
    Psi#"isBirational" = true;
    Psi#"isDominant" = true;
    Psi#"inverse" = Phi;
    Psi#"graph" = reverse graph Phi;
    if last o then (try assert(Phi * Psi == 1 and Psi * Phi == 1) else error "something went wrong in calculating the inverse map");
    Phi#"isBirational" = true;
    Phi#"isDominant" = true;
    Phi#"inverse" = Psi;
    (last graph Phi)#"isBirational" = true;
    (last graph Phi)#"isDominant" = true;
    return Psi;
);

inverse MultirationalMap := Phi -> inverse(Phi,MathMode=>false);

inverse (MultihomogeneousRationalMap,Option) := (Phi,opt) -> inverse(multirationalMap {Phi},opt);
inverse MultihomogeneousRationalMap := Phi -> inverse(Phi,MathMode=>false);

isIsomorphism MultirationalMap := Phi -> (
    if dim source Phi != dim target Phi or Phi#"isBirational" === false or Phi#"isDominant" === false then return false;
    if not isMorphism Phi then return false;
    isMorphism inverse(Phi,MathMode=>true)
);

MultirationalMap | MultiprojectiveVariety := (Phi,X) -> (
    if X === source Phi then return Phi;
    if ring ideal source Phi =!= ring ideal X then error "expected a subvariety in the ambient space of the source";
    if not isSubset(X,source Phi) then error "expected a subvariety of the source";
    I := multirationalMap(apply(multigens ring X,rationalMap),source Phi);
    try assert(ring source I === ring X) else error "internal error encountered: bad source found";
    I#"source" = X;
    I * Phi
);

MultirationalMap | List := (Phi,d) -> ( -- undocumented
    if not(# d == # (source Phi)#"dimAmbientSpaces" and all(d,i->instance(i,ZZ) and i>=0)) then error("expected a list of "|toString(# (source Phi)#"dimAmbientSpaces")|" non-negative integer(s) to indicate the degree of a hypersurface in the source"); 
    Phi|((source Phi) * projectiveVariety ideal random(d,ring ambient source Phi))
);

MultirationalMap || MultiprojectiveVariety := (Phi,Y) -> (
    if Y === target Phi then return Phi;
    X := Phi^* Y;
    I := multirationalMap(apply(multigens ring X,rationalMap),source Phi);
    try assert(ring source I === ring X) else error "internal error encountered: bad source found";
    I#"source" = X;
    multirationalMap(I * Phi,Y)
);

MultirationalMap || List := (Phi,d) -> ( -- undocumented
    if not(# d == # (target Phi)#"dimAmbientSpaces" and all(d,i->instance(i,ZZ) and i>=0)) then error("expected a list of "|toString(# (target Phi)#"dimAmbientSpaces")|" non-negative integer(s) to indicate the degree of a hypersurface in the target"); 
    Phi||((target Phi) * projectiveVariety ideal random(d,ring ambient target Phi))
);

super MultirationalMap := Phi -> (
    if target Phi == ambient target Phi then return Phi; 
    multirationalMap(apply(factor Phi,super),ambient target Phi)
);

MultirationalMap | MultirationalMap := (Phi,Psi) -> (
    if source Phi != source Psi then error "expected multi-rational maps with the same source";
    multirationalMap((factor Phi)|(factor Psi),(target Phi) ** (target Psi))
);

RationalMap | MultirationalMap := (Phi,Psi) -> (multirationalMap {Phi})|Psi;
MultirationalMap | RationalMap := (Phi,Psi) -> Phi|(multirationalMap {Psi});
MultihomogeneousRationalMap | MultirationalMap := (Phi,Psi) -> (multirationalMap {Phi})|Psi;
MultirationalMap | MultihomogeneousRationalMap := (Phi,Psi) -> Phi|(multirationalMap {Psi});

RationalMap | RationalMap := (Phi,Psi) -> (multirationalMap {Phi})|(multirationalMap {Psi});
MultihomogeneousRationalMap | RationalMap := (Phi,Psi) -> (multirationalMap {Phi})|(multirationalMap {Psi});
RationalMap | MultihomogeneousRationalMap := (Phi,Psi) -> (multirationalMap {Phi})|(multirationalMap {Psi});
MultihomogeneousRationalMap | MultihomogeneousRationalMap := (Phi,Psi) -> (multirationalMap {Phi})|(multirationalMap {Psi});

describeInt (MutableHashTable,ZZ,ZZ) := (Phi,I,N) -> (
    d := max degrees ideal compress matrix Phi; 
    isStandardMap := false; if class Phi#"dimAmbientTarget" === ZZ then (d = first d; isStandardMap = true);
    descr:="rational map ("|toString(I)|"/"|toString(N)|") defined by "|(if not isStandardMap then "multiforms" else "forms")|" of degree "|toString(d)|newline;
    descr=descr|"target variety: "|expressionVar(ideal source Phi#"map",Phi#"dimSource",Phi#"dimAmbientSource")|newline;
    if Phi#"isDominant" =!= true and Phi#"idealImage" =!= null then descr=descr|"image: "|expressionVar(if Phi#"dimAmbientSource" == Phi#"dimSource" then Phi#"idealImage" else (lift(Phi#"idealImage",ambient source Phi#"map") + ideal source Phi#"map"))|newline;
    if Phi#"isDominant" =!= null then descr=descr|"dominance: "|toString(Phi#"isDominant")|newline;
    if Phi#"isBirational" =!= null then descr=descr|"birationality: "|toString(Phi#"isBirational")|newline;
    if Phi#"isBirational" =!= true and Phi#"degree" =!= null then descr=descr|"degree of map: "|toString(Phi#"degree")|newline;
    if Phi#"projectiveDegrees" =!= {} then descr=descr|"projective degrees: "|toString(Phi#"projectiveDegrees")|newline;
    if Phi#"maps" =!= null then (
                 descr=descr|"number of minimal representatives: "|toString(# Phi#"maps");
                 if # Phi#"maps" >1 then descr=descr|", with degrees "|toString(toSequence apply(Phi#"maps",F-> max degrees ideal compress toMatrix F));
                 descr=descr|newline;
                 B:=ideal Phi; dimB:=max(dim B - (# heft ambient source Phi),-1);
                 descr=descr|"dimension base locus: "|toString(dimB)|newline;    
    );
    descr
);

describe MultirationalMap := Phi -> (
    F := factor Phi;
    descr:="multi-rational map consisting of "|(if #F == 1 then "one single rational map" else (toString(#F))|" rational maps")|newline;
    descr=descr|"source variety: "|(? source Phi)|newline;
    descr=descr|"target variety: "|(? target Phi)|newline;
    if Phi#"baseLocus" =!= null then descr=descr|"base locus: "|(? baseLocus Phi)|newline;
    if Phi#"isDominant" =!= true and Phi#"image" =!= null then descr=descr|"image: "|(? image Phi)|newline;
    if Phi#"isDominant" =!= null then descr=descr|"dominance: "|toString(Phi#"isDominant")|newline;
    if Phi#"isBirational" =!= null then descr=descr|"birationality: "|toString(Phi#"isBirational")|newline;
    for i to #F-1 do (
        descr=descr|"--"|newline;
        descr=descr|describeInt(F_i,i+1,#F)
    );
    descr = descr|"--"|newline;
    descr=descr|"coefficient ring: "|toString(coefficientRing Phi);
    net expression descr
);

MultirationalMap ! := Phi -> (
    apply(factor Phi,f -> f!);
    image Phi;
    isMorphism Phi;
    Phi
);

beginDocumentation() 

document {Key => {MultiprojectiveVarieties}, 
Headline => "Multi-projective varieties and multi-rational maps",
PARA{"This is a work in progress package to handling multi-projective varieties, that is, closed subvarieties of products of projective spaces, and rational maps between them. This extends the package ",TO Cremona,", which treats ",TO2{RationalMap,"rational maps"}," from multi-projective varieties to ",EM"standard"," projective varieties, ",TEX///$X\subseteq \mathbb{P}^{k_1}\times\mathbb{P}^{k_2}\times\cdots\times\mathbb{P}^{k_n}\dashrightarrow Y\subseteq\mathbb{P}^N$///,"."}}

document {Key => {MultiprojectiveVariety}, 
Headline => "the class of all multi-projective varieties", 
PARA {"A ",EM"multi-projective variety"," is a closed subvariety of a product of projective spaces ",TEX///$\mathbb{P}^{k_1}\times\mathbb{P}^{k_2}\times\cdots\times\mathbb{P}^{k_n}$///,"."}}

document { 
Key => {projectiveVariety, (projectiveVariety,Ideal), (projectiveVariety,Ring), [projectiveVariety,Saturate], [projectiveVariety,MinimalGenerators]}, 
Headline => "the closed multi-projective subvariety defined by a multi-homogeneous ideal", 
Usage => "projectiveVariety I", 
Inputs => { "I" => Ideal => {"a homogeneous ideal in a polynomial ring ",TEX///$R$///," with the ",TEX///$\mathbb{Z}^n$///,"-grading where the degree of each variable is a standard basis vector, that is, ",TEX///$R$///," is the homogeneous coordinate ring of a product of ",TEX///$n$///," projective spaces ",TEX///$\mathbb{P}^{k_1}\times\mathbb{P}^{k_2}\times\cdots\times\mathbb{P}^{k_n}$///}},
Outputs => {MultiprojectiveVariety => {"the projective subvariety of ",TEX///$\mathbb{P}^{k_1}\times\mathbb{P}^{k_2}\times\cdots\times\mathbb{P}^{k_n}$///," defined by ",TEX///$I$///}},
PARA{"Equivalently, one can give as input the coordinate ring of the projective variety, that is, the quotient of ",TEX///$R$///," by (the multisaturation of) ",TEX///$I$///,"."}, 
PARA{"In the example, we take a complete intersection ",TEX///$X\subset\mathbb{P}^{2}\times\mathbb{P}^{3}\times\mathbb{P}^{1}$///," of two hypersurfaces of multidegrees ",TEX///$(2,1,0)$///," and ",TEX///$(1,0,1)$///,"."},
EXAMPLE {
"K = ZZ/333331;", 
"R = K[x_0..x_2,y_0..y_3,z_0,z_1,Degrees=>{3:{1,0,0},4:{0,1,0},2:{0,0,1}}];",
"I = ideal(random({2,1,0},R),random({1,0,1},R))",
"X = projectiveVariety I",
"describe X"
},
PARA{"Below, we calculate the image of ",TEX///$X$///," via the Segre embedding of ",TEX///$\mathbb{P}^{2}\times\mathbb{P}^{3}\times\mathbb{P}^{1}$///," in ",TEX///$\mathbb{P}^{23}$///,"; thus we get a projective variety isomorphic to ",TEX///$X$///," and embedded in a single projective space ",TEX///$\mathbb{P}^{19}=<X>\subset\mathbb{P}^{23}$///,"."},
EXAMPLE {
"s = segre X;",
"X' = projectiveVariety image s",
"(dim X', codim X', degree X')",
"degrees X'"
},
SeeAlso => {(segre,MultiprojectiveVariety),(dim,MultiprojectiveVariety),(codim,MultiprojectiveVariety),(degree,MultiprojectiveVariety),(singularLocus,MultiprojectiveVariety)}} 

undocumented {Saturate};

document {Key => {(dim,MultiprojectiveVariety)}, 
Headline => "the dimension of the variety", 
Usage => "dim X", 
Inputs => {"X" => MultiprojectiveVariety}, 
Outputs => {ZZ => {"the dimension of ", TEX///$X$///}}, 
EXAMPLE {"X = projectiveVariety ideal random({2,1},ZZ/101[x_0,x_1,x_2,y_0,y_1,Degrees=>{3:{1,0},2:{0,1}}]);","dim X"}, 
SeeAlso => {(codim,MultiprojectiveVariety)}} 

document {Key => {(codim,MultiprojectiveVariety)}, 
Headline => "the codimension of the variety", 
Usage => "codim X", 
Inputs => {"X" => MultiprojectiveVariety}, 
Outputs => { ZZ => {"the codimension of ", TEX///$X$///}}, 
EXAMPLE {"X = projectiveVariety ideal random({2,1},ZZ/101[x_0,x_1,x_2,y_0,y_1,Degrees=>{3:{1,0},2:{0,1}}]);","codim X"}, 
SeeAlso => {(dim,MultiprojectiveVariety)}} 

document {Key => {(ideal,MultiprojectiveVariety)}, 
Headline => "the defining ideal of the variety", 
Usage => "ideal X", 
Inputs => {"X" => MultiprojectiveVariety}, 
Outputs => {Ideal => {"the defining ideal of ", TEX///$X$///}}, 
EXAMPLE {"X = projectiveVariety ideal random({2,1},ZZ/101[x_0,x_1,x_2,y_0,y_1,Degrees=>{3:{1,0},2:{0,1}}]);","ideal X"}, 
SeeAlso => {(ring,MultiprojectiveVariety)}} 

document {Key => {(ring,MultiprojectiveVariety)}, 
Headline => "the coordinate ring of the variety", 
Usage => "ring X", 
Inputs => {"X" => MultiprojectiveVariety}, 
Outputs => {Ring => {"the coordinate ring of ", TEX///$X$///}}, 
EXAMPLE {"X = projectiveVariety ideal random({2,1},ZZ/101[x_0,x_1,x_2,y_0,y_1,Degrees=>{3:{1,0},2:{0,1}}]);","ring X"}, 
SeeAlso => {(ideal,MultiprojectiveVariety),(coefficientRing,MultiprojectiveVariety)}} 

document {Key => {(coefficientRing,MultiprojectiveVariety)}, 
Headline => "the coefficient ring of the variety", 
Usage => "coefficientRing X", 
Inputs => {"X" => MultiprojectiveVariety}, 
Outputs => {Ring => {"the coefficient ring of ", TEX///$X$///}}, 
EXAMPLE {"X = projectiveVariety ideal random({2,1},ZZ/101[x_0,x_1,x_2,y_0,y_1,Degrees=>{3:{1,0},2:{0,1}}]);","coefficientRing X"}, 
SeeAlso => {(ring,MultiprojectiveVariety)}} 

document {Key => {(degree,MultiprojectiveVariety)}, 
Headline => "the degree of the variety", 
Usage => "degree X", 
Inputs => {"X" => MultiprojectiveVariety}, 
Outputs => { ZZ => {"the degree of the image of ", TEX///$X$///," via the Segre embedding of the ",TO2{(ambient,MultiprojectiveVariety),"ambient"}," of ",TEX///$X$///}}, 
EXAMPLE {"X = projectiveVariety ideal random({2,1},ZZ/101[x_0,x_1,x_2,y_0,y_1,Degrees=>{3:{1,0},2:{0,1}}]);","degree X"}, 
SeeAlso => {(multidegree,MultiprojectiveVariety),(segre,MultiprojectiveVariety)}} 

document {Key => {projections,(projections,MultiprojectiveVariety)}, 
Headline => "projections of a multi-projective variety", 
Usage => "projections X", 
Inputs => {"X" => MultiprojectiveVariety => {"a subvariety of ",TEX///$\mathbb{P}^{k_1}\times\mathbb{P}^{k_2}\times\cdots\times\mathbb{P}^{k_n}$///}}, 
Outputs => {{"the list of the projections ", TEX///$X\to \mathbb{P}^{k_i}$///,", for ",TEX///$i=1,\ldots,n$///}}, 
EXAMPLE {"X = projectiveVariety(ZZ/101[x_0..x_3]) ** projectiveVariety(ZZ/101[y_0..y_2]);","projections X"}} 

document {Key => {(ambient,MultiprojectiveVariety)}, 
Headline => "the ambient of the variety", 
Usage => "ambient X", 
Inputs => {"X" => MultiprojectiveVariety}, 
Outputs => { MultiprojectiveVariety => {"the product of the projective spaces where ", TEX///$X$///," is embedded"}}, 
EXAMPLE {"X = projectiveVariety ideal random({2,1},ZZ/101[x_0,x_1,x_2,y_0,y_1,Degrees=>{3:{1,0},2:{0,1}}]);","ambient X;"}} 

document {Key => {(multidegree,MultiprojectiveVariety)}, 
Headline => "the multidegree of the variety", 
Usage => "multidegree X", 
Inputs => {"X" => MultiprojectiveVariety}, 
Outputs => {{"the multi-degree of the defining ideal of ", TEX///$X$///}}, 
EXAMPLE {"X = projectiveVariety ideal random({2,1},ZZ/101[x_0,x_1,x_2,y_0,y_1,Degrees=>{3:{1,0},2:{0,1}}]);","multidegree X"}, 
SeeAlso => {(degree,MultiprojectiveVariety),(multidegree,Ideal)}} 

document {Key => {(segre,MultiprojectiveVariety)}, 
Headline => "the Segre embedding of the variety", 
Usage => "segre X", 
Inputs => {"X" => MultiprojectiveVariety}, 
Outputs => {{"the map returned by ",TO segre," ",TO2{(ring,MultiprojectiveVariety),"ring"}," ", TEX///$X$///}}, 
EXAMPLE {"X = projectiveVariety ideal random({2,1},ZZ/101[x_0,x_1,x_2,y_0,y_1,Degrees=>{3:{1,0},2:{0,1}}]);","segre X"}, 
SeeAlso => {segre,(segre,MultirationalMap)}}

document {Key => {(point,MultiprojectiveVariety)}, 
Headline => "pick a random rational point on a projective variety", 
Usage => "point X", 
Inputs => {"X" => MultiprojectiveVariety => {"defined over a finite field"}}, 
Outputs => {MultiprojectiveVariety => {"a random rational point on ", TEX///$X$///}}, 
EXAMPLE {"X = projectiveVariety ideal random({2,1},ZZ/101[x_0,x_1,x_2,y_0,y_1,Degrees=>{3:{1,0},2:{0,1}}]);","p = point X;", "describe p"},
SeeAlso => {point,randomKRationalPoint}} 

document {Key => {(singularLocus,MultiprojectiveVariety)}, 
Headline => "the singular locus of the variety", 
Usage => "singularLocus X", 
Inputs => {"X" => MultiprojectiveVariety => {"which is assumed to be equidimensional"}}, 
Outputs => { MultiprojectiveVariety => {"the singular locus of ", TEX///$X$///}}, 
EXAMPLE {"X = projectiveVariety ideal random({2,1},ZZ/101[x_0,x_1,x_2,y_0,y_1,Degrees=>{3:{1,0},2:{0,1}}]);","singularLocus X;","Y = X + projectiveVariety (ideal random({1,1},ring ambient X));","singularLocus Y;"}} 

document {Key => {(symbol ==,MultiprojectiveVariety,MultiprojectiveVariety)}, 
Headline => "equality of projective varieties", 
Usage => "X == Y", 
Inputs => { 
MultiprojectiveVariety => "X",
MultiprojectiveVariety => "Y"}, 
Outputs => { 
Boolean => {"whether ",TT"X"," and ",TT"Y", " are the same variety"}},
EXAMPLE {
"R = ZZ/101[x_0,x_1,x_2,y_0,y_1,Degrees=>{3:{1,0},2:{0,1}}];",
"(I,J) = (ideal(y_0-26*y_1,x_0*y_1+36*x_1*y_1-40*x_2*y_1),ideal(x_0*y_1+36*x_1*y_1-40*x_2*y_1,x_2*y_0-26*x_2*y_1,x_1*y_0-26*x_1*y_1,x_0*y_0+27*x_1*y_1-30*x_2*y_1));",
"I == J",
"X = projectiveVariety I",
"Y = projectiveVariety J",
"X == Y"}}

document {Key => {(isSubset,MultiprojectiveVariety,MultiprojectiveVariety)}, 
Headline => "whether one variety is a subvariety of another", 
Usage => "isSubset(X,Y)", 
Inputs => { 
MultiprojectiveVariety => "X",
MultiprojectiveVariety => "Y"}, 
Outputs => { 
Boolean => {"whether ",TT"X"," is contained in ",TT"Y"}}}

document {Key => {(symbol **,MultiprojectiveVariety,MultiprojectiveVariety)}, 
Headline => "product of projective varieties", 
Usage => "X ** Y", 
Inputs => { 
MultiprojectiveVariety => "X",
MultiprojectiveVariety => "Y"}, 
Outputs => { 
MultiprojectiveVariety => {"the product of ",TT"X"," and ",TT"Y"}},
EXAMPLE {"R = ZZ/101[x_0..x_2,y_0,y_1,Degrees=>{3:{1,0},2:{0,1}}];",
"S = ZZ/101[x_0,x_1,y_0..y_2,z_0,z_1,Degrees=>{2:{1,0,0},3:{0,1,0},2:{0,0,1}}];",
"X = projectiveVariety ideal(random({2,1},R),random({1,1},R));", 
"Y = projectiveVariety ideal random({1,1,1},S);",
"XxY = X ** Y;",
"describe X",
"describe Y",
"describe XxY"},
SeeAlso => {fiberProduct}}

document {Key => {(symbol *,MultiprojectiveVariety,MultiprojectiveVariety)}, 
Headline => "intersection of projective varieties", 
Usage => "X * Y", 
Inputs => { 
MultiprojectiveVariety => "X",
MultiprojectiveVariety => "Y"}, 
Outputs => { 
MultiprojectiveVariety => {"the intersection of ",TT"X"," and ",TT"Y",", that is, the projective variety defined by the sum of the corresponding ideals"}},
EXAMPLE {"R = ZZ/101[x_0,x_1,x_2,y_0,y_1,Degrees=>{3:{1,0},2:{0,1}}];",
"X = projectiveVariety ideal random({2,1},R);",
"Y = projectiveVariety ideal random({1,1},R);", 
"Z = X * Y;"},
SeeAlso => {(symbol +,MultiprojectiveVariety,MultiprojectiveVariety),(symbol +,Ideal,Ideal)}}

document {Key => {(symbol +,MultiprojectiveVariety,MultiprojectiveVariety)}, 
Headline => "union of projective varieties", 
Usage => "X + Y", 
Inputs => { 
MultiprojectiveVariety => "X",
MultiprojectiveVariety => "Y"}, 
Outputs => { 
MultiprojectiveVariety => {"the union of ",TT"X"," and ",TT"Y",", that is, the projective variety defined by the intersection of the corresponding ideals"}},
EXAMPLE {"R = ZZ/101[x_0,x_1,x_2,y_0,y_1,Degrees=>{3:{1,0},2:{0,1}}];",
"X = projectiveVariety ideal random({2,1},R);",
"Y = projectiveVariety ideal random({1,1},R);", 
"Z = X + Y;",
"assert(Z - X == Y and Z - Y == X)"},
SeeAlso => {(symbol -,MultiprojectiveVariety,MultiprojectiveVariety),(symbol *,MultiprojectiveVariety,MultiprojectiveVariety),(intersect,List)}}

document {Key => {(symbol -,MultiprojectiveVariety,MultiprojectiveVariety)}, 
Headline => "difference of projective varieties", 
Usage => "X - Y", 
Inputs => { 
MultiprojectiveVariety => "X",
MultiprojectiveVariety => "Y"}, 
Outputs => { 
MultiprojectiveVariety => {"the difference of ",TT"X"," and ",TT"Y",", that is, the projective variety defined by the colon ideal ",TT"ideal X : ideal Y"}},
EXAMPLE {"R = ZZ/101[x_0,x_1,x_2,y_0,y_1,Degrees=>{3:{1,0},2:{0,1}}];",
"X = projectiveVariety ideal(x_0^3*y_0+2*x_0^2*x_1*y_0+2*x_0*x_1^2*y_0+x_1^3*y_0+2*x_0^2*x_2*y_0+3*x_0*x_1*x_2*y_0+2*x_1^2*x_2*y_0+2*x_0*x_2^2*y_0+2*x_1*x_2^2*y_0+x_2^3*y_0+x_0^3*y_1+2*x_0^2*x_1*y_1+2*x_0*x_1^2*y_1+x_1^3*y_1+2*x_0^2*x_2*y_1+3*x_0*x_1*x_2*y_1+2*x_1^2*x_2*y_1+2*x_0*x_2^2*y_1+2*x_1*x_2^2*y_1+x_2^3*y_1);",
"Y = projectiveVariety ideal(x_0*y_0+x_1*y_0+x_2*y_0+x_0*y_1+x_1*y_1+x_2*y_1);", 
"Z = X - Y;",
"assert(Z + Y == X and X - Z == Y)"},
SeeAlso => {(symbol +,MultiprojectiveVariety,MultiprojectiveVariety),(quotient,Ideal,Ideal)}}

undocumented {(expression,MultiprojectiveVariety), (net,MultiprojectiveVariety), (point,MultiprojectiveVariety,Boolean), (top,MultiprojectiveVariety), (decompose,MultiprojectiveVariety),(describe,MultiprojectiveVariety),(symbol ?,MultiprojectiveVariety),(degrees,MultiprojectiveVariety)}

document {Key => {fiberProduct,(fiberProduct,RationalMap,RationalMap)}, 
Headline => "fiber product of projective varieties", 
Usage => "fiberProduct(phi,psi)", 
Inputs => { 
"phi" => {"a ",TO2{RationalMap,"morphism"}," ",TEX///$X\to Z$///," (that is, ",ofClass RationalMap," that is everywhere defined)"},
"psi" => {"another ",TO2{RationalMap,"morphism"}," ",TEX///$Y\to Z$///,", with the same target ",TEX///$Z$///}}, 
Outputs => { 
MultiprojectiveVariety => {"the fiber product ",TEX///$X\times_{Z} Y$///}},
PARA {"The natural morphisms ",TEX///$X\times_{Z} Y\to X$///," and ",TEX///$X\times_{Z} Y\to Y$///," can be easily obtained using ",TO projections," and ",TO multirationalMap,"."},
PARA {"As an example, we calculate the fiber product of the blowing up ",TEX///$\phi:Bl_{C}(\mathbb{P}^3)\to\mathbb{P}^3$///," of ",TEX///$\mathbb{P}^3$///," along a twisted cubic curve ",TEX///$C\subset\mathbb{P}^3$///," and the inclusion ",TEX///$\psi:L\to \PP^3$///," of a secant line ",TEX///$L\subset\mathbb{P}^3$///," to ",TEX///$C$///,"."},
EXAMPLE {
"ringP3 = ZZ/33331[a..d]; C = ideal(c^2-b*d,b*c-a*d,b^2-a*c), L = ideal(b+c+d,a-d)", 
"phi = first graph rationalMap C;",
"psi = parametrize L;",
"F = fiberProduct(phi,psi);",
"describe F",
"p = projections F;",
"-- first natural morphism
phi' = check multirationalMap({p_0,p_1},projectiveVariety source phi);",
"-- second natural morphism
psi' = check multirationalMap({p_2},projectiveVariety source psi);",
"assert(phi' * phi == psi' * psi)"},
SeeAlso => {(symbol **,MultiprojectiveVariety,MultiprojectiveVariety),(symbol ^**,MultirationalMap,MultiprojectiveVariety)}}

document { 
Key => {(euler,MultiprojectiveVariety)}, 
Headline => "topological Euler characteristic of a (smooth) projective variety", 
Usage => "euler X
euler(X,Probabilistic=>b)", 
Inputs => { 
MultiprojectiveVariety => "X" => {"which has to be smooth, and ",TT"b"," is a ",TO2{Boolean,"boolean value"},", that is, ",TT"true"," or ",TT"false"," (the default value is ",TT"true",")"}}, 
Outputs => { 
ZZ => {"the topological Euler characteristics of the variety ",TT"X",", calculated as ",TO EulerCharacteristic,TT"(ideal X,MathMode=>(not b))"}},
EXAMPLE {
"X = projectiveVariety minors(2,genericSymmetricMatrix(ZZ/33331[vars(0..5)],3));",
"euler X"},
SeeAlso => {EulerCharacteristic,(euler,ProjectiveVariety)}}

undocumented {(euler,MultiprojectiveVariety,Option),Probabilistic}

document {Key => {MultirationalMap}, 
Headline => "the class of all multi-rational maps", 
PARA {"A ",EM"multi-rational map"," is a rational map between ",TO2{MultiprojectiveVariety,"multi-projective varieties"},", ",TEX///$$\Phi:X\subseteq \mathbb{P}^{r_1}\times\mathbb{P}^{r_2}\times\cdots\times\mathbb{P}^{r_n}\dashrightarrow Y \subseteq \mathbb{P}^{s_1}\times\mathbb{P}^{s_2}\times\cdots\times\mathbb{P}^{s_m} .$$///,"Thus, it can be represented by an ",TO2{List,"ordered list"}," of ",TO2{RationalMap,"rational maps"},TEX///$$\Phi_i = (\Phi:X\dashrightarrow Y)\circ(pr_i:Y\to Y_i\subseteq\mathbb{P}^{s_i}) ,$$///,"for ",TEX///$i=1,\ldots,m$///,". The maps ",TEX///$\Phi_i:X\dashrightarrow Y_i\subseteq\mathbb{P}^{s_i}$///,", since the target ",TEX///$Y_i$///," is a standard projective variety, are implemented with the class ",TO RationalMap," (more properly, when ",TEX///$n>1$///," the class of such maps is called ",TT "MultihomogeneousRationalMap","). Recall that the main constructor for the class ",TO RationalMap," (as well as for the class ", TT"MultihomogeneousRationalMap",") is the method ",TO rationalMap,"."},
PARA {"The constructor for the class of multi-rational maps is ",TO multirationalMap,", which takes as input the list of maps ",
TEX///$\{\Phi_1:X\dashrightarrow Y_1,\ldots,\Phi_m:X\dashrightarrow Y_m\}$///,", together with the variety ",TEX///$Y$///,", and returns the map ",TEX///$\Phi:X\dashrightarrow Y$///,"."},
Caveat => {"At the moment there are just a few functions implemented."}}

document { 
Key => {multirationalMap, (multirationalMap,List,MultiprojectiveVariety), (multirationalMap,List)}, 
Headline => "the multi-rational map defined by a list of rational maps", 
Usage => "multirationalMap Phi
multirationalMap(Phi,Y)", 
Inputs => { "Phi" => {ofClass List," of ",TO2{RationalMap,"rational maps"},", ",TEX///$\{\Phi_1:X\dashrightarrow Y_1\subseteq\mathbb{P}^{s_1},\ldots,\Phi_m:X\dashrightarrow Y_m\subseteq\mathbb{P}^{s_m}\}$///,", all having the same ",TO2{(source,RationalMap),"source"}," ",TEX///$X\subseteq \mathbb{P}^{r_1}\times\mathbb{P}^{r_2}\times\cdots\times\mathbb{P}^{r_n}$///},
"Y" => {ofClass MultiprojectiveVariety," ",TEX///$Y \subseteq \mathbb{P}^{s_1}\times\mathbb{P}^{s_2}\times\cdots\times\mathbb{P}^{s_m}$///," (if omitted, then the ",TO2{(symbol **,MultiprojectiveVariety,MultiprojectiveVariety),"product"}," ",TEX///$Y_1\times\cdots \times Y_m$///," is taken)"}},
Outputs => {MultirationalMap => {"the unique rational map ",TEX///$\Phi:X\subseteq \mathbb{P}^{r_1}\times\mathbb{P}^{r_2}\times\cdots\times\mathbb{P}^{r_n}\dashrightarrow Y \subseteq \mathbb{P}^{s_1}\times\mathbb{P}^{s_2}\times\cdots\times\mathbb{P}^{s_m}$///," such that ",TEX///$pr_i\circ\Phi = \Phi_i$///,", where ",TEX///$pr_i:Y\subseteq \mathbb{P}^{s_1}\times\mathbb{P}^{s_2}\times\cdots\times\mathbb{P}^{s_m} \to Y_i\subseteq \mathbb{P}^{s_i}$///," denotes the i-th projection"}},
EXAMPLE {
"ZZ/65521[x_0..x_4];",
"f = rationalMap {x_3^2-x_2*x_4,x_2*x_3-x_1*x_4,x_1*x_3-x_0*x_4,x_2^2-x_0*x_4,x_1*x_2-x_0*x_3,x_1^2-x_0*x_2};",
"g = rationalMap(f,Dominant=>true);",
"Y = (projectiveVariety target g) ** (projectiveVariety target g);",
"multirationalMap {f,g};",
"multirationalMap({f,g},Y);",
"assert(factor oo === {f,g} and target oo === Y)",
"multirationalMap {f,f,g};",
"h = last graph f;",
"multirationalMap {h};",
"multirationalMap {h,h};",
"multirationalMap({h,h,h},Y ** projectiveVariety(target h));",
"describe oo!"},
SeeAlso => {rationalMap,(graph,MultirationalMap),(image,MultirationalMap),(baseLocus,MultirationalMap),(inverse,MultirationalMap)},
Caveat => {"Be careful when you pass the target ",TT"Y"," as input, because it must be compatible with the maps but for efficiency reasons a full check is not done automatically. See ",TO (check,MultirationalMap),"."}}

undocumented {(expression,MultirationalMap),(net,MultirationalMap),(describe,MultirationalMap),(symbol !,MultirationalMap)};

document { 
Key => {(check,MultirationalMap)}, 
Headline => "check that a multi-rational map is well-defined", 
Usage => "check Phi", 
Inputs => {MultirationalMap}, 
Outputs => {MultirationalMap => {"the same object passed as input, but an error is thrown if the target of the map is not compatible."}},
EXAMPLE {
"ZZ/65521[x_0..x_4], f = rationalMap {x_3^2-x_2*x_4,x_2*x_3-x_1*x_4,x_1*x_3-x_0*x_4,x_2^2-x_0*x_4,x_1*x_2-x_0*x_3,x_1^2-x_0*x_2};",
"Phi = multirationalMap {f}",
"check Phi",
"Y = image Phi",
"Psi = multirationalMap({f},Y)",
"check Psi",
"p = point Y;",
"Eta = multirationalMap({f},p);",
"try check Eta else <<\"meaningless object!\";"}}

document { 
Key => {(target,MultirationalMap)}, 
Headline => "the target for a multi-rational map", 
Usage => "target Phi", 
Inputs => { 
MultirationalMap => "Phi"}, 
Outputs => { 
MultiprojectiveVariety => {"the target of ",TT"Phi"}},
PARA{"Note that, instead, the ",TO2{(target,RationalMap),"target"}," of a standard ",TO2{RationalMap,"rational map"}," is the coordinate ring of the target variety (this is done mainly for efficiency reasons)."},
SeeAlso => {(source,MultirationalMap),(factor,MultirationalMap)}}

document { 
Key => {(source,MultirationalMap)}, 
Headline => "the source for a multi-rational map", 
Usage => "source Phi", 
Inputs => { 
MultirationalMap => "Phi"}, 
Outputs => { 
MultiprojectiveVariety => {"the source of ",TT"Phi"}},
PARA{"Note that, instead, the ",TO2{(source,RationalMap),"source"}," of a standard ",TO2{RationalMap,"rational map"}," is the coordinate ring of the source variety (this is done mainly for efficiency reasons)."},
SeeAlso => {(target,MultirationalMap),(factor,MultirationalMap)}}

document { 
Key => {(factor,MultirationalMap)}, 
Headline => "the list of rational maps defining a multi-rational map", 
Usage => "factor Phi", 
Inputs => {MultirationalMap => "Phi"}, 
Outputs => {{"the ",TO2{List,"list"}," of ",TO2{RationalMap,"rational maps"}," defining ",TT"Phi"}},
SeeAlso => {(target,MultirationalMap),(source,MultirationalMap)}}

document { 
Key => {(coefficientRing,MultirationalMap)}, 
Headline => "the coefficient ring of a multi-rational map", 
Usage => "coefficientRing Phi", 
Inputs => {MultirationalMap => "Phi"}, 
Outputs => {Ring => {"the coefficient ring of ",TT"Phi"}},
SeeAlso => {(coefficientRing,MultiprojectiveVariety)}}

document { 
Key => {(image,MultirationalMap)}, 
Headline => "image of a multi-rational map", 
Usage => "image Phi", 
Inputs => {MultirationalMap => "Phi"}, 
Outputs => {MultiprojectiveVariety => {"the image of ",TT"Phi"}},
PARA{"Note that, instead, the ",TO2{(image,RationalMap),"image"}," of a standard ",TO2{RationalMap,"rational map"}," is the defining ideal of the image (this is done mainly for efficiency reasons)."},
EXAMPLE {
"ZZ/65521[x_0..x_4];",
"f = rationalMap {x_2^2-x_1*x_3, x_1*x_2-x_0*x_3, x_1^2-x_0*x_2, x_0*x_4, x_1*x_4, x_2*x_4, x_3*x_4, x_4^2};",
"g = rationalMap {-x_3^2+x_2*x_4, 2*x_2*x_3-2*x_1*x_4, -3*x_2^2+2*x_1*x_3+x_0*x_4, 2*x_1*x_2-2*x_0*x_3, -x_1^2+x_0*x_2};",
"Phi = multirationalMap {f,g};",
"time Z = image Phi;",
"dim Z, degree Z, degrees Z"},
PARA {"Alternatively, the calculation can be performed using the Segre embedding as follows:"},
EXAMPLE {
"time Z' = projectiveVariety (map segre target Phi) image(segre Phi,\"F4\");",
"assert(Z == Z')"},
SeeAlso => {(symbol SPACE,MultirationalMap,MultiprojectiveVariety),(image,RationalMap),(segre,MultirationalMap)}}

document { 
Key => {(symbol SPACE,MultirationalMap,MultiprojectiveVariety)}, 
Headline => "direct image via a multi-rational map", 
Usage => "Phi X", 
Inputs => {MultirationalMap => "Phi", MultiprojectiveVariety => "X" => {"a subvariety of the ",TO2{(source,MultirationalMap),"source"}," of ",TT "Phi"}}, 
Outputs => {MultiprojectiveVariety => {"the (closure of the) direct image of ", TT"X", " via ",TT"Phi"}},
EXAMPLE {
"ZZ/65521[x_0..x_4];",
"f = last graph rationalMap {x_2^2-x_1*x_3, x_1*x_2-x_0*x_3, x_1^2-x_0*x_2, x_0*x_4, x_1*x_4, x_2*x_4, x_3*x_4, x_4^2};",
"Phi = multirationalMap {f,f};",
"Z = source Phi;",
"time Phi Z;",
"dim oo, degree oo, degrees oo",
"time Phi (point Z + point Z + point Z)",
"dim oo, degree oo, degrees oo"},
SeeAlso => {(image,MultirationalMap), (symbol ^*, MultirationalMap), (symbol SPACE,RationalMap,Ideal)}}

document { 
Key => {(symbol ^**,MultirationalMap,MultiprojectiveVariety), (symbol ^*,MultirationalMap)}, 
Headline => "inverse image via a multi-rational map", 
Usage => "Phi^** Y
Phi^* Y", 
Inputs => { 
MultirationalMap => "Phi",
MultiprojectiveVariety => "Y" => {"a subvariety of the ",TO2{(target,MultirationalMap),"target"}," of ",TT "Phi"}}, 
Outputs => { 
MultiprojectiveVariety => {"the (closure of the) inverse image of ", TT"Y", " via ",TT"Phi"}},
EXAMPLE {
"ZZ/300007[x_0..x_3], f = rationalMap {x_2^2-x_1*x_3, x_1*x_2-x_0*x_3, x_1^2-x_0*x_2}, g = rationalMap {x_1^2-x_0*x_2, x_0*x_3, x_1*x_3, x_2*x_3, x_3^2};",
"Phi = last graph multirationalMap {f,g};",
"Y = projectiveVariety ideal(random({1,1},ring target Phi), random({1,1},ring target Phi));",
"time X = Phi^* Y;",
"dim X, degree X, degrees X"},
SeeAlso => {(symbol SPACE,MultirationalMap,MultiprojectiveVariety),(symbol ^*,RationalMap),(symbol ||,MultirationalMap,MultiprojectiveVariety)}}

document {Key => {(segre,MultirationalMap)}, 
Headline => "the composition of a multi-rational map with the Segre embedding of the target", 
Usage => "segre Phi", 
Inputs => {"Phi" => MultirationalMap}, 
Outputs => {RationalMap => {"the composition of the multi-rational map ",TT"Phi"," with the ",TO2{(segre,MultiprojectiveVariety),"Segre embedding"}," of the ",TO2{(target,MultirationalMap),"target"}," of ",TT"Phi"}}, 
EXAMPLE {"ZZ/65521[x_0..x_4];",
"f = rationalMap({x_3^2-x_2*x_4,x_2*x_3-x_1*x_4,x_1*x_3-x_0*x_4,x_2^2-x_0*x_4,x_1*x_2-x_0*x_3,x_1^2-x_0*x_2},Dominant=>true);",
"g = rationalMap {x_3^2-x_2*x_4,x_2*x_3-x_1*x_4,x_1*x_3-x_0*x_4,x_2^2-x_0*x_4,x_1*x_2-x_0*x_3};",
"h = rationalMap {-x_3^2+x_2*x_4,2*x_2*x_3-2*x_1*x_4,-3*x_2^2+2*x_1*x_3+x_0*x_4, 2*x_1*x_2-2*x_0*x_3,-x_1^2+x_0*x_2};",
"Phi = multirationalMap {f,g,h};",
"time segre Phi",
"describe segre Phi",
"degreeMap segre Phi",
"Psi = multirationalMap {h,h};",
"degreeMap segre Psi",
"projectiveDegrees segre Psi"}, 
SeeAlso => {(segre,MultiprojectiveVariety)}}

document { 
Key => {(graph,MultirationalMap)}, 
Headline => "the graph of a multi-rational map", 
Usage => "graph Phi", 
Inputs => { 
MultirationalMap => "Phi"}, 
Outputs => { 
MultirationalMap => {"the first projection from the graph of ",TT"Phi"},
MultirationalMap => {"the second projection from the graph of ",TT"Phi"}}, 
PARA{"The equalities ",TT"(first graph Phi) * Phi == last graph Phi"," and ",TT"(first graph Phi)^-1 * (last graph Phi) == Phi"," are always satisfied."},
EXAMPLE { 
"ZZ/333331[x_0..x_4];",
"Phi = multirationalMap {rationalMap(minors(2,matrix{{x_0..x_3},{x_1..x_4}}),Dominant=>true)}",
"time (Phi1,Phi2) = graph Phi",
"Phi1;",
"Phi2;",
"time (Phi21,Phi22) = graph Phi2", 
"Phi21;",
"Phi22;",
"time (Phi211,Phi212) = graph Phi21",
"Phi211;",
"Phi212;",
"assert(
source Phi1 == source Phi2 and target Phi1 == source Phi and target Phi2 == target Phi and
source Phi21 == source Phi22 and target Phi21 == source Phi2 and target Phi22 == target Phi2 and 
source Phi211 == source Phi212 and target Phi211 == source Phi21 and target Phi212 == target Phi21)",
"assert(Phi1 * Phi == Phi2 and Phi21 * Phi2 == Phi22 and Phi211 * Phi21 == Phi212)"},
SeeAlso => {(graph,RationalMap),(symbol *,MultirationalMap,MultirationalMap),(symbol ==,MultirationalMap,MultirationalMap),(inverse,MultirationalMap)}}

document { 
Key => {(symbol *,MultirationalMap,MultirationalMap),(compose,MultirationalMap,MultirationalMap)}, 
Headline => "composition of multi-rational maps", 
Usage => "Phi * Psi 
compose(Phi,Psi)", 
Inputs => { 
MultirationalMap => "Phi" => { TEX///$X \dashrightarrow Y$///},
MultirationalMap => "Psi" => { TEX///$Y \dashrightarrow Z$///}}, 
Outputs => { 
MultirationalMap => { TEX///$X \dashrightarrow Z$///, ", the composition of ",TT"Phi"," and ",TT"Psi"}}, 
EXAMPLE { 
"ZZ/65521[x_0..x_4];",
"Psi = multirationalMap {last graph rationalMap({x_2^2-x_1*x_3, x_1*x_2-x_0*x_3, x_1^2-x_0*x_2, x_0*x_4, x_1*x_4, x_2*x_4, x_3*x_4, x_4^2},Dominant=>true)};",
"Phi = first graph Psi;",
"Eta = Phi * Psi;",
"assert(Eta == last graph Psi);"},
SeeAlso => {(symbol *,RationalMap,RationalMap)}}

undocumented {(symbol *,MultihomogeneousRationalMap,MultirationalMap),(symbol *,MultirationalMap,MultihomogeneousRationalMap),(symbol *,RationalMap,MultirationalMap),(symbol *,MultirationalMap,RationalMap),(symbol ^,MultirationalMap,ZZ)}

document { 
Key => {(symbol ==,MultirationalMap,MultirationalMap)}, 
Headline => "equality of multi-rational maps", 
Usage => "Phi == Psi", 
Inputs => { 
MultirationalMap => "Phi",
MultirationalMap => "Psi"}, 
Outputs => { 
Boolean => {"whether ",TT"Phi"," and ",TT"Psi", " are the same multi-rational map"}},
SeeAlso => {(symbol ==,RationalMap,RationalMap)}}

undocumented {(symbol ==,MultihomogeneousRationalMap,MultirationalMap),(symbol ==,MultirationalMap,MultihomogeneousRationalMap),(symbol ==,RationalMap,MultirationalMap),(symbol ==,MultirationalMap,RationalMap),(symbol ==,MultirationalMap,ZZ),(symbol ==,ZZ,MultirationalMap)}

document { 
Key => {(projectiveDegrees,MultirationalMap),(multidegree,MultirationalMap)}, 
Headline => "projective degrees of a multi-rational map", 
Usage => "projectiveDegrees Phi
multidegree Phi", 
Inputs => { 
MultirationalMap => "Phi"}, 
Outputs => { 
List => {"the list of projective degrees of ",TT"Phi"}},
PARA{"This computation is performed by converting the ",TO2{MultirationalMap,"multi-rational map"}," to a ",EM"standard"," ",TO2{RationalMap,"rational map"}," (using ",TO2{(segre,MultirationalMap),"segre"},") and then applying the corresponding method for rational maps."},
EXAMPLE {
"ZZ/300007[x_0..x_3], f = rationalMap {x_2^2-x_1*x_3, x_1*x_2-x_0*x_3, x_1^2-x_0*x_2}, g = rationalMap {x_1^2-x_0*x_2, x_0*x_3, x_1*x_3, x_2*x_3, x_3^2};",
"Phi = last graph multirationalMap {f,g}",
"time projectiveDegrees Phi",
"(degree source Phi,degree image Phi)"},
SeeAlso => {(projectiveDegrees,RationalMap),(multidegree,RationalMap),(degree,MultirationalMap)}}

document { 
Key => {(degreeMap,MultirationalMap),(degree,MultirationalMap)}, 
Headline => "degree of a multi-rational map", 
Usage => "degreeMap Phi
degree Phi", 
Inputs => { 
MultirationalMap => "Phi"}, 
Outputs => { 
ZZ => {"the degree of ",TT"Phi",". So this value is 1 if and only if the map is birational onto its image."}},
PARA{"This computation is performed by converting the ",TO2{MultirationalMap,"multi-rational map"}," to a ",EM"standard"," ",TO2{RationalMap,"rational map"}," (using ",TO2{(segre,MultirationalMap),"segre"},") and then applying the corresponding method for rational maps."},
EXAMPLE {
"ZZ/300007[x_0..x_3], f = rationalMap {x_2^2-x_1*x_3, x_1*x_2-x_0*x_3, x_1^2-x_0*x_2}, g = rationalMap {x_1^2-x_0*x_2, x_0*x_3, x_1*x_3, x_2*x_3, x_3^2};",
"Phi = last graph multirationalMap {f,g}",
"time degree Phi"},
SeeAlso => {(degreeMap,RationalMap),(degree,RationalMap),(multidegree,MultirationalMap)}}

document { 
Key => {(isMorphism,MultirationalMap)}, 
Headline => "whether a multi-rational map is a morphism", 
Usage => "isMorphism Phi", 
Inputs => { 
"Phi" => MultirationalMap}, 
Outputs => { 
Boolean => {"whether ",TT"Phi"," is a morphism (i.e., everywhere defined)"}},
EXAMPLE { 
"ZZ/300007[a..e], f = first graph rationalMap ideal(c^2-b*d,b*c-a*d,b^2-a*c,e), g = rationalMap submatrix(matrix f,{0..2});",
"Phi = multirationalMap {f,g};",
"time isMorphism Phi",
"time Psi = first graph Phi;",
"time isMorphism Psi",
"assert((not o3) and o5)"},
SeeAlso => {(isIsomorphism,MultirationalMap),(isMorphism,RationalMap)}}

document { 
Key => {(multirationalMap,MultiprojectiveVariety)}, 
Headline => "identity map", 
Usage => "multirationalMap X", 
Inputs => {MultiprojectiveVariety => "X"}, 
Outputs => {MultirationalMap => {"the identity map on ",TT"X"}},
SeeAlso => {(multirationalMap,MultiprojectiveVariety,MultiprojectiveVariety)}}

document { 
Key => {(multirationalMap,MultiprojectiveVariety,MultiprojectiveVariety)}, 
Headline => "get the natural inclusion", 
Usage => "multirationalMap(X,Y)", 
Inputs => {MultiprojectiveVariety => "X",MultiprojectiveVariety => "Y" => {"with ",TEX///$X\subseteq Y$///," (after identifying the ambient spaces)"}}, 
Outputs => {MultirationalMap => {"the natural inclusion of ",TEX///$X$///," into ",TEX///$Y$///}},
EXAMPLE {
"R = ZZ/101[a_0,a_1,b_0..b_2,Degrees=>{2:{1,0},3:{0,1}}], S = ZZ/101[c_0,c_1,d_0..d_2,Degrees=>{2:{1,0},3:{0,1}}]",
"I = ideal (random({0,1},R),random({1,1},R)), J = sub(I,vars S)",
"X = projectiveVariety I, Y = projectiveVariety J",
"multirationalMap(X,ambient X);",
"multirationalMap(X,Y);",
"try multirationalMap(ambient X,X) else <<\"not able to construct it!\";"},
SeeAlso => {(multirationalMap,MultiprojectiveVariety)}}

document { 
Key => {(multirationalMap,MultirationalMap,MultiprojectiveVariety)}, 
Headline => "change the target of a rational map", 
Usage => "multirationalMap(Phi,Y)
check multirationalMap(Phi,Y)", 
Inputs => {MultirationalMap => "Phi",MultiprojectiveVariety => "Y" => {"which must be compatible with ",TT"Phi"}}, 
Outputs => {MultirationalMap => {"defined in the same way as ",TT"Phi"," but with ",TT"Y"," as target"}},
EXAMPLE {
"Phi = multirationalMap {super specialQuadraticTransformation 1}",
"Y = image Phi",
"Psi = multirationalMap(Phi,Y)",
"target Psi"},
SeeAlso => {(check,MultirationalMap)}}

document { 
Key => {(inverse,MultirationalMap)}, 
Headline => "inverse of a birational map", 
Usage => "inverse Phi
Phi^-1", 
Inputs => {MultirationalMap => "Phi" => {"a birational map"}}, 
Outputs => {MultirationalMap => {"the inverse map of ",TT"Phi"}},
PARA{"This function applies a general algorithm to calculate the inverse map passing through the computation of the ",TO2{(graph,MultirationalMap),"graph"},"."},
EXAMPLE {
"-- map defined by the quadrics through a rational normal quartic curve
ZZ/65521[a..e], f = rationalMap minors(2,matrix {{a,b,c,d},{b,c,d,e}});",
"Phi = multirationalMap {f};",
"-- we see Phi as a dominant map
Phi = multirationalMap(Phi,image Phi);",
"time inverse Phi;",
"Psi = last graph Phi;",
"time inverse Psi;",
"Eta = first graph Psi;",
"time inverse Eta;", 
"describe oo!",
"assert(Phi * Phi^-1 == 1 and Phi^-1 * Phi == 1)",
"assert(Psi * Psi^-1 == 1 and Psi^-1 * Psi == 1)",
"assert(Eta * Eta^-1 == 1 and Eta^-1 * Eta == 1)"},
SeeAlso => {(graph,MultirationalMap),(symbol *,MultirationalMap,MultirationalMap),(symbol ==,MultirationalMap,MultirationalMap),(degree,MultirationalMap),(image,MultirationalMap),(inverse,RationalMap)},
Caveat => {"No test is done to check that the map is birational, and if not then often the error is not thrown at all and a nonsense answer is returned. You can for instance do ",
TO2{(degree,MultirationalMap),"degree"},TT" Phi == 1",
" to check that the map is birational onto its image, and ",TO2{(image,MultirationalMap),"image"},TT" Phi == ",TO2{(target,MultirationalMap),"target"},TT" Phi"," to check the dominance."}}

undocumented {(inverse,MultirationalMap,Option)} -- for tests only

document { 
Key => {(isIsomorphism,MultirationalMap)}, 
Headline => "whether a birational map is an isomorphism", 
Usage => "isIsomorphism Phi", 
Inputs => {"phi" => MultirationalMap}, 
Outputs => {Boolean => {"whether ",TT"Phi"," is an isomorphism"}},
EXAMPLE { 
"-- map defined by the quadrics through a twisted cubic curve
ZZ/33331[a..d]; f = rationalMap {c^2-b*d,b*c-a*d,b^2-a*c};",
"Phi = multirationalMap {f,f};",
"time isIsomorphism Phi",
"Psi = first graph Phi;",
"time isIsomorphism Psi",
"Eta = first graph Psi;",
"time isIsomorphism Eta",
"describe Eta!",
"describe (inverse Eta)!",
"assert(o8 and (not o6) and (not o4))"},
SeeAlso => {(inverse,MultirationalMap),(isMorphism,MultirationalMap)}}

document { 
Key => {baseLocus,(baseLocus,MultirationalMap)}, 
Headline => "the base locus of a multi-rational map", 
Usage => "baseLocus Phi", 
Inputs => {MultirationalMap => "Phi"}, 
Outputs => {MultiprojectiveVariety => {"the base locus of ",TT"Phi",", that is, the locus where it is not defined"}},
SeeAlso => {(isMorphism,MultirationalMap),(ideal,RationalMap)}}

undocumented {(baseLocus,RationalMap)}

document { 
Key => {(projectiveVariety,MultidimensionalMatrix)}, 
Headline => "the multi-projective variety defined by a multi-dimensional matrix", 
Usage => "projectiveVariety A", 
Inputs => {MultidimensionalMatrix => "A" => {"an ",TEX///$n$///,"-dimensional matrix of shape ",TEX///$(k_1+1)\times\cdots\times (k_n+1)$///}}, 
Outputs => {MultiprojectiveVariety => {"the corresponding hypersurface of multi-degree ",TEX///$(1,\ldots,1)$///," on the product of projective spaces ",TEX///$\mathbb{P}^{k_1}\times\cdots\times\mathbb{P}^{k_n}$///}},
PARA {"In particular, we have ",TO2{(determinant,MultidimensionalMatrix),"det"},TT" A != 0"," if and only if ",TO2{(dim,MultiprojectiveVariety),"dim"}," ",TO2{(singularLocus,MultiprojectiveVariety),"singularLocus"},TT"(projectiveVariety A) == -1","."},
EXAMPLE {
"K = ZZ/33331;",
"A = randomMultidimensionalMatrix({2,2,3},CoefficientRing=>K)",
"det A",
"X = projectiveVariety A;",
"dim singularLocus X",
"B = multidimensionalMatrix {{{9492_K, 13628, -9292}, {9311, -5201, -16439}}, {{11828, -16301, 8162}, {15287, 8345, -2094}}}",
"det B",
"Y = projectiveVariety B;",
"dim singularLocus Y"},
SeeAlso => {(det,MultidimensionalMatrix),(singularLocus,MultiprojectiveVariety)}}

document { 
Key => {(symbol |,MultirationalMap,MultiprojectiveVariety)}, 
Headline => "restriction of a multi-rational map", 
Usage => "Phi | Z", 
Inputs => {MultirationalMap => "Phi" => { TEX///$\Phi:X \dashrightarrow Y$///},
MultiprojectiveVariety => "Z" => {"a subvariety of ",TEX///$X$///}}, 
Outputs => {MultirationalMap => {"the restriction of ",TEX///$\Phi$///," to ",TEX///$Z$///,", ",TEX///$\phi|_{Z}: Z \dashrightarrow Y$///}}, 
EXAMPLE {
"ZZ/33331[x_0..x_3], f = rationalMap {x_2^2-x_1*x_3,x_1*x_2-x_0*x_3,x_1^2-x_0*x_2}, g = rationalMap {x_2^2-x_1*x_3,x_1*x_2-x_0*x_3};",
"Phi = last graph multirationalMap {f,g};",
"Z = (source Phi) * projectiveVariety ideal random({1,1,2},ring ambient source Phi);",
"Phi' = Phi|Z;",
"source Phi'",
"assert(image Phi' == Phi Z)"},
PARA{"The following is a shortcut to take restrictions on random hypersurfaces as above."},
EXAMPLE {"Phi|{1,1,2};"},
SeeAlso => {(symbol ||,MultirationalMap,MultiprojectiveVariety),(symbol |,RationalMap,Ideal),(symbol *,MultiprojectiveVariety,MultiprojectiveVariety)}}

document { 
Key => {(symbol ||,MultirationalMap,MultiprojectiveVariety)}, 
Headline => "restriction of a multi-rational map", 
Usage => "Phi || Z", 
Inputs => {MultirationalMap => "Phi" => { TEX///$\Phi:X \dashrightarrow Y$///},
MultiprojectiveVariety => "Z" => {"a subvariety of ",TEX///$Y$///}}, 
Outputs => { 
MultirationalMap => {"the restriction of ",TEX///$\Phi$///," to ",TEX///${\Phi}^{(-1)} Z$///,", ",TEX///${{\Phi}|}_{{\Phi}^{(-1)} Z}: {\Phi}^{(-1)} Z \dashrightarrow Z$///}}, 
EXAMPLE {
"ZZ/33331[x_0..x_3], f = rationalMap {x_2^2-x_1*x_3,x_1*x_2-x_0*x_3,x_1^2-x_0*x_2}, g = rationalMap {x_2^2-x_1*x_3,x_1*x_2-x_0*x_3};",
"Phi = last graph multirationalMap {f,g};",
"Z = projectiveVariety ideal random({1,2},ring target Phi);",
"Phi' = Phi||Z;",
"target Phi'",
"assert(source Phi' == Phi^* Z)"},
PARA{"The following is a shortcut to take restrictions on random hypersurfaces as above."},
EXAMPLE {"Phi||{1,2};"},
SeeAlso => {(symbol |,MultirationalMap,MultiprojectiveVariety),(symbol ||,RationalMap,Ideal),(symbol ^*,MultirationalMap)}}

undocumented {(symbol |,MultirationalMap,List),(symbol ||,MultirationalMap,List)}

document { 
Key => {(symbol |,MultirationalMap,MultirationalMap)}, 
Headline => "product of multi-rational maps", 
Usage => "Phi | Psi", 
Inputs => {MultirationalMap => "Phi" => { TEX///$X \dashrightarrow Y$///},
MultirationalMap => "Psi" => { TEX///$X \dashrightarrow Z$///}}, 
Outputs => {MultirationalMap => {TEX///$X \dashrightarrow Y\times Z$///,", defined by the ",TO2{(symbol |,List,List),"join"}," of ",TO2{(factor,MultirationalMap),"factor"},TT" Phi"," with ",TO2{(factor,MultirationalMap),"factor"},TT" Psi"}}, 
EXAMPLE {
"Phi = rationalMap(veronese(1,2,ZZ/33331),Dominant=>true);",
"Psi = rationalMap veronese(1,3,ZZ/33331);",
"Eta = Phi | Psi;",
"Eta | Phi;",
"Phi | Psi | Eta;",
"super oo;",
"multirationalMap(oo,image oo);"},
SeeAlso => {(symbol |,List,List),(factor,MultirationalMap),(symbol *,MultiprojectiveVariety,MultiprojectiveVariety),(super,MultirationalMap)}}

undocumented {(symbol |,RationalMap,MultirationalMap),(symbol |,MultirationalMap,RationalMap),(symbol |,RationalMap,RationalMap)}

document { 
Key => {(super,MultirationalMap)}, 
Headline => "get the multi-rational map whose target is a product of projective spaces", 
Usage => "super Phi", 
Inputs => {MultirationalMap => "Phi" => {"whose target is a subvariety ",TEX///$Y\subseteq\mathbb{P}^{k_1}\times\cdots\times\mathbb{P}^{k_n}$///}}, 
Outputs => {MultirationalMap => {"the composition of ",TT"Phi"," with the inclusion of ",TEX///$Y$///," into ",TEX///$\mathbb{P}^{k_1}\times\cdots\times\mathbb{P}^{k_n}$///}},
EXAMPLE {
"Phi = multirationalMap{rationalMap(veronese(1,2,ZZ/33331),Dominant=>true),rationalMap(veronese(1,3,ZZ/33331),Dominant=>true)};",
"super Phi;",
"Psi = multirationalMap(Phi,image Phi);",
"super Psi == super Phi"},
SeeAlso => {(target,MultirationalMap),(ambient,MultiprojectiveVariety),(super,RationalMap)}}

---------------
---- Tests ----
---------------

TEST ///
ZZ/300007[x_0..x_3], f = rationalMap {x_2^2-x_1*x_3, x_1*x_2-x_0*x_3, x_1^2-x_0*x_2}, g = rationalMap {x_1^2-x_0*x_2, x_0*x_3, x_1*x_3, x_2*x_3, x_3^2};
Phi = last graph multirationalMap {f,g}
assert(projectiveDegrees Phi == {66, 46, 31, 20} and multidegree Phi == {66, 46, 31, 20})
assert(degreeMap Phi == 1 and degree Phi == 1)
assert(degree source Phi == 66 and degree image Phi == 20 and degree target Phi == 15)
Z = {target Phi,
     projectiveVariety ideal(random({1,1},ring target Phi)),
     projectiveVariety ideal(random({1,1},ring target Phi),random({1,1},ring target Phi)),
     projectiveVariety ideal(random({1,1},ring target Phi),random({1,1},ring target Phi),random({1,1},ring target Phi))}
assert(apply(Z,z -> (W = Phi^* z; (dim W,degree W))) == {(3, 66), (2, 46), (1, 31), (0, 20)})  
assert(Phi^* (Z_1) == Phi^** (Z_1))
///

TEST ///
ringP4 := ZZ/300007[a..e];
f = rationalMap minors(2,matrix {{a,b,c,d},{b,c,d,e}});
g = rationalMap(minors(2,matrix{{a,b,c},{b,c,d}}) + ideal e);
Phi = multirationalMap {f,g};
Phi = multirationalMap(Phi,image Phi);
Psi = inverse(Phi,MathMode=>true);
assert(Phi * Psi == 1 and Psi * Phi == 1)
(F,G) = graph Phi;
F' = inverse(F,MathMode=>true);
assert(F * F' == 1 and F' * F == 1 and F * Phi == G and G * Phi^-1 == F)
G' = inverse(G,MathMode=>true);
assert(G * G' == 1 and G' * G == 1 and G' * F == Phi^-1)
///

