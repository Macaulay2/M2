
-*
   Copyright 2020, Giovanni Staglianò.

   You may redistribute this file under the terms of the GNU General Public
   License as published by the Free Software Foundation, either version 2 of
   the License, or any later version.
*-

if version#"VERSION" < "1.17" then error "this package requires Macaulay2 version 1.17 or newer";

newPackage(
    "MultiprojectiveVarieties",
    Version => "1.1", 
    Date => "January 22, 2021",
    Authors => {{Name => "Giovanni Staglianò", Email => "giovannistagliano@gmail.com"}},
    Headline => "multi-projective varieties and multi-rational maps",
    Keywords => {"Projective Algebraic Geometry"},
    PackageImports => {"PrimaryDecomposition", "Cremona","SparseResultants"},
    PackageExports => {"Cremona","SparseResultants"},
    DebuggingMode => false,
    Reload => false
)

if Cremona.Options.Version < "5.1" then error "your version of the Cremona package is outdated (required version 5.1 or newer); you can download the latest version from https://github.com/Macaulay2/M2/tree/development/M2/Macaulay2/packages";
if SparseResultants.Options.Version < "1.1" then error "your version of the SparseResultants package is outdated (required version 1.1 or newer); you can download the latest version from https://github.com/Macaulay2/M2/tree/development/M2/Macaulay2/packages";

export{"MultiprojectiveVariety", "projectiveVariety", "Saturate", "projections", "fiberProduct",
       "MultirationalMap", "multirationalMap", "baseLocus", "degreeSequence", "inverse2"}

debug Cremona;
debug SparseResultants;

MultiprojectiveVariety = new Type of MutableHashTable;

globalAssignment MultiprojectiveVariety;

MultiprojectiveVariety.synonym = "multi-projective variety";

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
        "parametrization" => null,
        "projections" => null,
        "euler" => null,
        "expression" => null,
        "IsSaturationCalculated" => o.Saturate
    }
);

projectiveVariety Ring := o -> R -> (
    if not isPolynomialRing ambient R then error "expected the ambient ring to be polynomial";
    X := projVarFromRing R;
    if o.Saturate then if not isSatIdeal X then error "the ideal is not multi-saturated";
    X
);

projVarFromRing = memoize (R -> (
    X := projectiveVariety(ideal R,MinimalGenerators=>false,Saturate=>false);
    X#"ringVariety" = R;
    X
));

isSatIdeal = memoize (X -> (
    if X#"IsSaturationCalculated" then return true;
    I := ideal X;
    J := multisaturate I;
    if I == J then (X#"IsSaturationCalculated" = true; return true) else return false;
));

projectiveVariety MultidimensionalMatrix := o -> A -> projectiveVariety(ideal(A!),MinimalGenerators=>true,Saturate=>false);

projectiveVariety (List,Ring) := o -> (l,K) -> (
    if not all(l,i -> instance(i,ZZ) and i >= 0) then error "expected a list of non-negative integers"; 
    if not isField K then error "expected a field";
    if #l == 0 then return projectiveVariety(K[],MinimalGenerators=>false,Saturate=>false);
    projectiveVariety(ring first first gensRing(K,apply(l,i -> i+1)),MinimalGenerators=>false,Saturate=>false)
);

expression MultiprojectiveVariety := X -> (
    if X#"expression" =!= null then return X#"expression";
    n := X#"dimAmbientSpaces";
    if dim X == 0 and codim X > 0 then if sort degrees X == sort pairs tally deepSplice apply(n,entries diagonalMatrix toList(#n:1),(i,d) -> i:d) then return expression("a point in "|expressionVar(sum n,n));
    X#"expression" = expression expressionVar(dim X,n)
);

net MultiprojectiveVariety := X -> (
   if hasAttribute(X,ReverseDictionary) then toString getAttribute(X,ReverseDictionary) else "a projective variety"
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
    ambX := if ideal X == 0 then X else projectiveVariety ring ideal X;
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
    X#"projections" = apply(X#"multigens",x -> rationalMap(sub(matrix{x},ring X),Dominant=>"notSimplify"))
);

segre MultiprojectiveVariety := X -> (
    if X#"segreMap" =!= null then return X#"segreMap";
    X#"segreMap" = segre ring X
);

toStringDegreesVar = X -> toString(concatenate for l in degrees X list (toString unsequence toSequence first l)|"^"|(toString(last l)|" "));

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
    s = s|"generators:........... "|toStringDegreesVar(X)|newline;
    purity := X == top X;
    s = s|"purity:............... "|toString(purity); 
    if purity then (
        s = s|newline|"dim sing. l.:......... "|toString(dim singularLocus X); 
        if dim singularLocus X >= 0 then s = s|newline|"gens sing. l.:........ "|toStringDegreesVar(singularLocus X);
    );
    if # n > 1 then (
        s = s|newline|"Segre embedding:...... "|"map to PP^"|toString(numgens target segre X -1); 
        N := product apply(n, i -> i+1) -1;
        if numgens target segre X -1 < N then s = s|" ⊂ PP^"|toString(N);
    );
    return s;
);

? MultiprojectiveVariety := X -> (
    if dim X == -1 or codim X <= 0 then return toString expression X;
    degs := degrees ideal X; 
    m := "multi-";
    if # X#"dimAmbientSpaces" == 1 then m = "";
    if # degs == 1 then return(toString expression X|" defined by a "|m|"form of "|m|"degree "|toString(unsequence toSequence first degs));
    cutOut:=""; if #degs>1 then cutOut = if # unique degs == 1 then " cut out by "|toString(#degs)|" hypersurfaces of "|m|"degree "|toString(unsequence toSequence first degs) else " cut out by "|toString(#degs)|" hypersurfaces of "|m|"degrees "|toStringDegreesVar(X); 
    (toString expression X)|cutOut
);

degrees MultiprojectiveVariety := X -> pairs tally degrees ideal X;

shape MultiprojectiveVariety := X -> X#"dimAmbientSpaces";

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

parametrize MultiprojectiveVariety := X -> (
    f := parametrization X;
    try return (parametrize ring source f) * f else error "not able to get a parametrization";
);

parametrization = method();
parametrization MultiprojectiveVariety := X -> (
    if X#"parametrization" =!= null then return X#"parametrization";
    t := local t;
    g := parametrizeProductOfProjectiveSpaces(ring ambient X,t);
    X#"parametrization" = (multirationalMap(apply(projections ambient X,p -> rationalMap(g * (map p),Dominant=>"notSimplify")),ambient X))||X
);

point (MultiprojectiveVariety,Boolean) := (X,b) -> (
    if # X#"dimAmbientSpaces" == 1 then return projectiveVariety(point(ideal X,b),MinimalGenerators=>false,Saturate=>false);
    f := parametrization X;
    p := f point(source f,false);
    if b then if not (dim p == 0 and degree p == 1 and isSubset(ideal X,ideal p)) then error("something went wrong in trying to pick a random "|toString(coefficientRing X)|"-rational point on the variety");
    return p;
);
point MultiprojectiveVariety := X -> point(X,true);

MultiprojectiveVariety ** MultiprojectiveVariety := (X,Y) -> productMem(X,Y);

productMem = memoize(L -> (
    if not (#L > 0 and all(L,X -> instance(X,MultiprojectiveVariety))) then error "expected a list of multi-projective varieties";
    if #L == 1 then return first L;
    K := coefficientRing first L;
    for i from 1 to #L-1 do if K =!= coefficientRing(L_i) then error "different coefficient rings encountered";
    n := toSequence apply(L,X -> apply(X#"dimAmbientSpaces",i->i+1));
    R := ring first first gensRing(K,join n);
    j := for i to #L list sum toList join take(n,i);
    s := for i to #L-1 list map(R,ring ideal L_i,submatrix(vars R,j_i .. j_(i+1)-1));
    W := projectiveVariety(sum(#L,i -> s_i ideal L_i),MinimalGenerators=>true,Saturate=>false);
    W#"projections" = apply(projections W,apply(join toSequence apply(L,projections),target),(f,T) -> rationalMap((map f) * (map rationalMap(target f,T)),Dominant=>"notSimplify"));
    W
));

MultiprojectiveVariety ^ ZZ := (X,n) -> (
    if n < 0 then error "expected a nonnegative integer";
    if n == 0 then return projectiveVariety ideal(1_(ring ambient X));
    productMem toList(n : X)
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
    if not(#o == 2 and first o === Verify) then error "Verify is the only available option for euler(MultiprojectiveVariety)";
    if not instance(last o,Boolean) then error "option Verify accepts true or false";
    if X#"euler" =!= null then return X#"euler";
    local e;
    if # X#"dimAmbientSpaces" == 1 then (
        if codim X == 0 then return X#"euler" = numgens ring X;
        e = EulerCharacteristic(ideal X,MathMode=>last o,Verbose=>false);
     ) else (
        -- <<"--warning: code to be improved"<<endl;
        e = EulerCharacteristic(image segre X,MathMode=>last o,Verbose=>false);
    );
    if last o then X#"euler" = e;
    return e;
);

euler MultiprojectiveVariety := X -> euler(X,Verify=>false);

random (List,MultiprojectiveVariety) := o -> (l,X) -> (
    K := coefficientRing X;
    n := # X#"dimAmbientSpaces";
    if #l == n and all(l,j -> instance(j,ZZ)) then return random({l},X);
    L := pairs tally l;
    if not all(L,i -> instance(first i,List) and # first i == n and all(first i,j -> instance(j,ZZ))) then error("expected lists of integers of length "|toString(n)); 
    local B;
    Y := projectiveVariety ideal flatten for d in L list (
        B := flatten entries gens image basis(first d,ideal X);
        if #B == 0 then error("unable to find random elements of degree "|(toString first d));
        for i from 1 to last d list sum(B,b -> (random K) * b)
    );
    if codim Y != #l then error "unable to find random elements, too many multi-degrees are given";
    return Y;
);

random (ZZ,MultiprojectiveVariety) := o -> (i,X) -> random({i},X);

MultiprojectiveVariety ** Ring := (X,K) -> (
    if not isField K then error "expected a field";
    if (char coefficientRing X =!= char K and char coefficientRing X =!= 0) then error "characteristic not valid";
    projectiveVariety(sub(ideal X,vars ring projectiveVariety(shape X,K)),Saturate=>false,MinimalGenerators=>true)
);


MultirationalMap = new Type of MutableHashTable;

globalAssignment MultirationalMap;

MultirationalMap.synonym = "multi-rational map";

expression MultirationalMap := Phi -> (
    X := if hasAttribute(source Phi,ReverseDictionary) then toString getAttribute(source Phi,ReverseDictionary) else toString expression source Phi;
    Y := if hasAttribute(target Phi,ReverseDictionary) then toString getAttribute(target Phi,ReverseDictionary) else toString expression target Phi;
    if dim source Phi == -1 or dim target Phi == -1 then return expression("map from " | X | " to " | Y);
    return expression((if Phi#"isBirational" === true then "birational " else (if Phi#"isDominant" === true then "dominant rational " else "rational "))| "map from " | X | " to " | Y);
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
        "multidegree" => null,
        "baseLocus" => null,
        "inverse" => null
    }
);

multirationalMap List := L -> (
    if not (# L > 0 and all(L,f -> instance(f,RationalMap) or instance(f,MultihomogeneousRationalMap))) then error "expected a list of rational maps";
    Y := productMem apply(L,f -> projectiveVariety(target f,Saturate=>false));
    Phi := multirationalMap(L,Y);
    if #L == 1 then Phi#"isDominant" = (first L)#"isDominant";
    if #L == 1 then Phi#"isBirational" = (first L)#"isBirational";
    Phi
);

multirationalMap RationalMap := phi -> multirationalMap {phi};
multirationalMap(RationalMap,RationalMap) := (phi1,phi2) -> multirationalMap {phi1,phi2};
multirationalMap(RationalMap,RationalMap,RationalMap) := (phi1,phi2,phi3) -> multirationalMap {phi1,phi2,phi3};
multirationalMap MultihomogeneousRationalMap := phi -> multirationalMap {phi};
multirationalMap(MultihomogeneousRationalMap,MultihomogeneousRationalMap) := (phi1,phi2) -> multirationalMap {phi1,phi2};
multirationalMap(MultihomogeneousRationalMap,MultihomogeneousRationalMap,MultihomogeneousRationalMap) := (phi1,phi2,phi3) -> multirationalMap {phi1,phi2,phi3};
multirationalMap MultirationalMap := Phi -> multirationalMap factor Phi;
multirationalMap (MultirationalMap,MultirationalMap) := (Phi1,Phi2) -> multirationalMap((factor Phi1)|(factor Phi2));
multirationalMap (MultirationalMap,MultirationalMap,MultirationalMap) := (Phi1,Phi2,Phi3) -> multirationalMap((factor Phi1)|(factor Phi2)|(factor Phi3));

multirationalMap (MultirationalMap,MultiprojectiveVariety) := (Phi,Y) -> (
    if Y === target Phi then return Phi;
    Psi := multirationalMap(factor Phi,Y);
    if Phi#"image" === Y then Psi#"isDominant" = true;
    return Psi;
);

check MultirationalMap := o -> Phi -> (
    X := source Phi; Y := target Phi;
    if not isSatIdeal X then error "the ideal of the source is not multi-saturated";
    if not isSatIdeal Y then error "the ideal of the target is not multi-saturated";
    L := apply(factor Phi,super);
    P := apply(projections Y,L,(p,f) -> if target p === target f then p else rationalMap(source p,target f,matrix p,Dominant=>"notSimplify"));
    for i to #L -1 do if not isSubset(image P_i,image L_i) then error "the target variety is not compatible with the maps";
    Phi
);

checkRepresentatives = method();
checkRepresentatives MultirationalMap := Phi -> (
   local F;
   for phi in factor Phi do (
       F := phi#"maps";
       if F =!= null then for i from 1 to #F -1 do assert(minors(2,(toMatrix F_i)||(toMatrix F_0)) == 0);
   );
);

checkAndCompare = method();
checkAndCompare (MultirationalMap,Boolean) := (Phi,recursive) -> (
    try (checkRepresentatives Phi; check Phi) else error "found a wrong map";
    if Phi#"image" =!= null then (if not isSatIdeal image Phi then error "found a wrong ideal for an image");
    if Phi#"inverse" =!= null and recursive then (
        if Phi * Phi^-1 != 1 then error "found a wrong inverse map";
        if Phi#"isDominant" === false or Phi#"isBirational" === false or (inverse Phi)#"isDominant" === false or (inverse Phi)#"isBirational" === false then error "found wrong value for 'isDominant' or 'isBirational'";
        if Phi#"multidegree" =!= null and (inverse Phi)#"multidegree" =!= null then if multidegree inverse Phi != reverse multidegree Phi then error "found a wrong multidegree";
        checkAndCompare(inverse Phi,false));
    if Phi#"graph" =!= null then (
        checkAndCompare(first graph Phi,true); 
        checkAndCompare(last graph Phi,true);
        if (first graph Phi) * Phi != last graph Phi then error "found a wrong graph"); 
);
checkAndCompare (MultirationalMap,MultirationalMap) := (Phi,Psi) -> (
    checkAndCompare(Phi,true);
    checkAndCompare(Psi,true);
    if Phi != Psi then return false;
    if Phi#"image" =!= null and Psi#"image" =!= null then (if image Phi != image Psi then error "found wrong value for 'image'");
    if Phi#"baseLocus" =!= null and Psi#"baseLocus" =!= null then (if baseLocus Phi != baseLocus Psi then error "found wrong value for 'baseLocus'");
    if Phi#"multidegree" =!= null and Psi#"multidegree" =!= null then (if multidegree Phi != multidegree Psi then error "found wrong value for 'multidegree'");
    if Phi#"isDominant" =!= null and Psi#"isDominant" =!= null then (if Phi#"isDominant" != Psi#"isDominant" then error "found wrong value for 'isDominant'");
    if Phi#"isBirational" =!= null and Psi#"isBirational" =!= null then (if Phi#"isBirational" != Psi#"isBirational" then error "found wrong value for 'isBirational'");
    if Phi#"graph" =!= null and Psi#"graph" =!= null then (if source graph Phi != source graph Psi then error "found wrong value for 'graph'");
    return true;   
);
MultirationalMap <==> MultirationalMap := (Phi,Psi) -> checkAndCompare(Phi,Psi);

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

compose0 = method(); -- this is the same as compose(RingMap,RingMap) but it needs the updated version of Cremona.m2 
compose0 (RingMap,RingMap) := (f,g) -> (
    if source f =!= target g then error "rational maps not composable: incompatible target and source";
    L := toMatrix (f * g);
    if L == 0 then error "rational maps may not be composable: got the empty map by composing chosen representatives";
    D := try gcd flatten entries compress L else 1_(target f);
    local Q;
    M := if D != 0 and D != 1 then apply(flatten entries L,l -> (Q = quotientRemainder(l,D); assert(last Q == 0); first Q)) else flatten entries L;
    return map(target f,source g,M);
);

compose (MultirationalMap,MultirationalMap) := (Phi,Psi) -> (
    if ring ambient target Phi =!= ring ambient source Psi or target Phi != source Psi then error "multi-rational maps not composable: the target of the first one is different from the source of the second one";
    f := toRingMap(Phi,ring source Psi);
    Eta := multirationalMap(apply(factor Psi,g -> rationalMap(compose0(f,map g),Dominant=>"notSimplify")),target Psi);
    if ring source Eta =!= ring source Phi then error "internal error encountered: bad source found";
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
    I := multirationalMap(apply(multigens ring X,o -> rationalMap(o,Dominant=>"notSimplify")),X);
    if ring source I =!= ring X then error "internal error encountered: bad source found";
    I#"source" = X;
    I#"isDominant" = true;
    I#"isBirational" = true;
    I
);

ZZ _ MultiprojectiveVariety := (n,X) -> (if n =!= 1 then error "expected integer to be 1"; multirationalMap X);

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
    if ring ambient source Phi =!= ring ambient Z then error "expected a multi-projective variety in the same ambient of the source of the map";
    if not isSubset(Z,source Phi) then error "expected a subvariety of the source of the map";
    F := apply(factor Phi,f -> lift(matrix f,ring ambient source Phi));
    s := # F;
    n := apply((source Phi)#"dimAmbientSpaces",i->i+1);
    m := apply((target Phi)#"dimAmbientSpaces",i->i+1);
    K := coefficientRing Phi;
    t := local t;
    R := K[t_0 .. t_(s-1), flatten gensRing(K,n|m), MonomialOrder => Eliminate (s + sum n)];
    subx := map(R,ring ambient source Phi,submatrix(vars R,{s .. s + sum n - 1}));
    suby := map(R,ring ambient target Phi,submatrix(vars R,{s + sum n .. s + sum n + sum m - 1}));
    suby' := map(ring ambient target Phi,R,matrix{toList(s + sum n : 0)} | vars ring ambient target Phi);       
    yy := (target Phi)#"multigens";
    I := subx(ideal Z) + sum(s,i -> ideal(suby(matrix{yy_i}) - t_i * subx(F_i)));
    projectiveVariety(suby' ideal selectInSubring(1,gens gb I),MinimalGenerators=>true,Saturate=>false)
);

image MultirationalMap := Phi -> (
    if Phi#"image" =!= null then return Phi#"image";
    if Phi#"isDominant" === true then return target Phi;
    Phi#"image" = Phi (source Phi);
    Phi#"isDominant" = Phi#"image" == target Phi;
    if Phi#"isDominant" then Phi#"image" = target Phi;
    return Phi#"image";
);

inverseImageViaMultirationalMapWeak = (Phi,Z) -> (
    if ring ambient target Phi =!= ring ambient Z then error "expected a multi-projective variety in the same ambient of the target of the map";
    -- if not isSubset(Z,target Phi) then error "expected a subvariety of the target of the map";
    F := apply(factor Phi,f -> ideal matrix f);
    g := toRingMap(Phi,ring target Phi);
    I := g sub(ideal Z,ring target Phi);
    for f in F do I = saturate(I,f);
    projectiveVariety trim lift(I,ring ambient source Phi)
);

MultirationalMap ^* := (Phi) -> MultiprojectiveVariety := (Z) -> inverseImageViaMultirationalMapWeak(Phi,Z);

MultirationalMap ^** MultiprojectiveVariety := (Phi,Z) -> (
    if ring ambient target Phi =!= ring ambient Z then error "expected a multi-projective variety in the same ambient of the target of the map";
    -- if not isSubset(Z,target Phi) then error "expected a subvariety of the target of the map";
    <<"--warning: the code for ^** must be improved, use instead the method ^*"<<endl;
    projectiveVariety trim lift((segre Phi)^** ((segre target Phi) ideal Z),ring ambient source Phi)
);

graphViaElim = Phi -> (
    n := apply((source Phi)#"dimAmbientSpaces",i->i+1);
    m := apply((target Phi)#"dimAmbientSpaces",i->i+1);
    s := #m;
    K := coefficientRing Phi;
    R' := ring first first gensRing(K,n|m);
    t := local t;
    R := K[t_0 .. t_(s-1), gens R', MonomialOrder => Eliminate s];
    subx := map(R,ring ambient source Phi,submatrix(vars R,{s .. s + sum n - 1}));
    suby := map(R,ring ambient target Phi,submatrix(vars R,{s + sum n .. s + sum n + sum m - 1}));
    yy := apply((target Phi)#"multigens",y -> suby matrix{y});
    F := apply(factor Phi,f -> subx lift(matrix f,ring ambient source Phi));
    I := subx(ideal source Phi) + sum(s,i -> ideal(yy_i - t_i * F_i));
    projectiveVariety(sub(ideal selectInSubring(1,gens gb I),R'),MinimalGenerators=>true,Saturate=>false)
);

SymmIdeal = Phi -> (
    n := apply((source Phi)#"dimAmbientSpaces",i->i+1);
    r := #n;
    m := apply((target Phi)#"dimAmbientSpaces",i->i+1);
    s := #m;
    K := coefficientRing Phi;
    g := gensRing(K,n|m);
    R := ring first first g;
    subx := map(R,ring ambient source Phi,submatrix(vars R,{0 .. sum n - 1}));
    suby := map(R,ring ambient target Phi,submatrix(vars R,{sum n .. sum n + sum m - 1}));
    yy := apply((target Phi)#"multigens",y -> suby matrix{y});
    F := apply(factor Phi,f -> subx lift(syz matrix f,ring ambient source Phi));
    I := subx(ideal source Phi) + sum(s,i -> ideal(yy_i * F_i));
    -- for i to r-1 do I = saturate(I,ideal g_i);
    trim I -- this may not be multi-saturated
);

graphViaSyzygies = Phi -> (
    I := SymmIdeal Phi;
    n := apply((source Phi)#"dimAmbientSpaces",i->i+1);
    subx := map(ring I,ring ambient source Phi,submatrix(vars ring I,{0 .. sum n - 1}));
    F := apply(factor Phi,f -> subx lift(first flatten entries compress matrix f,ring ambient source Phi));
    for i to #F-1 do I = saturate(I,F_i);
    projectiveVariety(I,MinimalGenerators=>true,Saturate=>false)
);

graphViaKoszul = Phi -> (
    n := apply((source Phi)#"dimAmbientSpaces",i->i+1);
    m := apply((target Phi)#"dimAmbientSpaces",i->i+1);
    s := #m;
    K := coefficientRing Phi;
    R := ring first first gensRing(K,n|m);
    subx := map(R,ring ambient source Phi,submatrix(vars R,{0 .. sum n - 1}));
    suby := map(R,ring ambient target Phi,submatrix(vars R,{sum n .. sum n + sum m - 1}));
    yy := apply((target Phi)#"multigens",y -> suby matrix{y});
    F := apply(factor Phi,f -> subx lift(matrix f,ring ambient source Phi));
    I := subx(ideal source Phi) + sum(s,i -> minors(2,yy_i || F_i));
    for i to s-1 do I = saturate(I,ideal F_i);
    projectiveVariety(I,MinimalGenerators=>true,Saturate=>false)
);

graph MultirationalMap := o -> Phi -> (
    if Phi#"graph" =!= null then return Phi#"graph";
    local G;
    if o.BlowUpStrategy === "Eliminate" then G = graphViaElim(Phi) else (
        if o.BlowUpStrategy === "Syzygies" or o.BlowUpStrategy === "Saturate" then G = graphViaSyzygies(Phi) else (
            if o.BlowUpStrategy === "Koszul" then G = graphViaKoszul(Phi) else (
                error "possible values for the option BlowUpStrategy are: \"Eliminate\", \"Syzygies\", \"Koszul\"";
            );
        );
    );
    r := # (source Phi)#"dimAmbientSpaces";
    s := # (target Phi)#"dimAmbientSpaces";
    pr := projections G;
    psi1 := multirationalMap(take(pr,r),source Phi);
    psi2 := multirationalMap(take(pr,-s),target Phi);
    psi1#"isDominant" = true;
    psi1#"isBirational" = true;
    psi2#"isDominant" = Phi#"isDominant";
    psi2#"isBirational" = Phi#"isBirational";
    Phi#"graph" = (psi1,psi2)
);

reverseGraph = method();
reverseGraph (MultirationalMap,MultirationalMap) := (p1,p2) -> (
    G := source(p1,p2);
    n := apply((target p1)#"dimAmbientSpaces",i -> i+1);
    m := apply((target p2)#"dimAmbientSpaces",i -> i+1);
    R := ring first first gensRing(coefficientRing G,m|n);
    s := map(R,ring ideal G,submatrix(vars R,{sum m .. sum m + sum n - 1}) | submatrix(vars R,{0 .. sum m - 1}));
    G' := projectiveVariety(s ideal G,MinimalGenerators=>true,Saturate=>false);
    p1' := multirationalMap(take(projections G',# m),target p2);
    p2' := multirationalMap(take(projections G',-(# n)),target p1);
    p1'#"isDominant" = p2#"isDominant";
    p1'#"isBirational" = p2#"isBirational";
    p2'#"isDominant" = p1#"isDominant";
    p2'#"isBirational" = p1#"isBirational";
    (p1',p2')
);

multidegree MultirationalMap := Phi -> (
    if Phi#"multidegree" =!= null then return Phi#"multidegree";
    Phi#"multidegree" = multidegree graph Phi
);

multidegree (MultirationalMap,MultirationalMap) := (p1,p2) -> (
    P := multidegree source(p1,p2);
    n := (target p1)#"dimAmbientSpaces";
    m := (target p2)#"dimAmbientSpaces";
    k := (sum n) + (sum m) - first degree P;
    -- assert(k == dim source(p1,p2));
    N := product apply(#n,i -> n_i+1) -1; 
    M := product apply(#m,i -> m_i+1) -1; 
    T1 := take(gens ring P,#n); 
    T2 := take(gens ring P,-(#m));
    mon := (product apply(#n,i -> T1_i^(n_i))) * (product apply(#m,i -> T2_i^(m_i)));
    d := local d;
    for i from 0 to max(0,k-M) -1 do d_i = 0;
    for i from max(0,k-M) to min(k,N) do d_i = coefficient(mon,P * (sum T1)^i * (sum T2)^(k-i));
    for i from min(k,N) + 1 to k do d_i = 0;
    reverse for i to k list d_i
);

source (MultirationalMap,MultirationalMap) := (p1,p2) -> (
    if source p1 =!= source p2 then error "expected the graph of a multi-rational map";
    n := (target p1)#"dimAmbientSpaces";
    m := (target p2)#"dimAmbientSpaces";
    if n|m != (source p1)#"dimAmbientSpaces" then error "expected the graph of a multi-rational map";
    source p1
);

degree MultirationalMap := Phi -> (
    d := lift((last multidegree Phi)/(degree image Phi),ZZ);
    if (Phi#"isBirational" === true and d != 1) or (Phi#"isDominant" === true and Phi#"isBirational" === false and d == 1) then error "internal error encountered: obtained an incoherent value for the degree";
    if d != 1 then Phi#"isBirational" = false;
    if Phi#"isDominant" === true and d == 1 then Phi#"isBirational" = true;
    d
);

multidegree (ZZ,MultirationalMap) := (i,Phi) -> (
    s := # factor Phi;
    d := toList(s:1); 
    Y := ambient target Phi;
    X := source Phi;
    if i < 0 or i > dim X then error("expected an integer between 0 and "|toString(dim X));
    if i == dim X then return degree X;
    F := Phi^* projectiveVariety(ideal apply(dim X - i,l -> random(d,ring Y)),MinimalGenerators=>true,Saturate=>false);
    if dim F == i then return degree F else return 0;
);

multidegree (Nothing,MultirationalMap) := (nu,Phi) -> reverse for i to dim source Phi list multidegree(i,Phi);

degree (MultirationalMap,Option) := (Phi,opt) -> (
    o := toList opt;
    if not(#o == 2 and first o === Strategy) then error "Strategy is the only available option for degree(MultirationalMap)";
    if last o =!= "random point" and last o =!= "0-th projective degree" then error "available strategies are: \"random point\" and \"0-th projective degree\"";
    if last o == "random point" then (
        F := Phi^* Phi (point source Phi);
        if dim F > 0 then return 0 else return degree F;
    ) else (
        d := multidegree(0,Phi);
        if d == 0 or d == 1 then return d;
        return lift(d/(degree image Phi),ZZ);
    );
);

baseLocus = method(TypicalValue => MultiprojectiveVariety);
baseLocus MultirationalMap := Phi -> (
    if Phi#"baseLocus" =!= null then return Phi#"baseLocus";
    if isPolynomialRing ring source Phi then ( -- this is not needed with the updated version of Cremona.m2
        local w; local q;
        B := for f in factor Phi list (
            if ideal matrix f == 0 or codim ideal matrix f > 1 then ideal matrix f else (
                w = gcd entries f;
                ideal apply(entries f,e -> (q = quotientRemainder(e,w); assert(last q == 0); first q))));
        return Phi#"baseLocus" = projectiveVariety intersect B;
    );
    Phi#"baseLocus" = projectiveVariety trim lift(intersect apply(factor Phi,ideal),ring ambient source Phi)
);

baseLocus RationalMap := Phi -> baseLocus multirationalMap {Phi};
baseLocus MultihomogeneousRationalMap := Phi -> baseLocus multirationalMap {Phi};

isMorphism MultirationalMap := Phi -> dim baseLocus Phi == -1;

inverse (MultirationalMap,Option) := (Phi,opt) -> (
    if Phi#"inverse" =!= null then return Phi#"inverse";
    if Phi#"isBirational" === false or Phi#"isDominant" === false then error "expected a birational map";
    if Phi#"isBirational" === null then if dim source Phi != dim target Phi then (Phi#"isBirational" = false; error "expected a birational map"); 
    o := toList opt;
    if not(#o == 2 and first o === Verify) then error "Verify is the only available option for inverse(MultirationalMap)";
    b := last o;
    if b === false then b = 0 else if b === true then b = 1;
    if not (instance(b,ZZ) and b >= 0) then error "option Verify accepts true or false";
    Gr := source graph Phi;
    Sub := map(ring target Phi,ring ambient Gr,matrix{toList((numgens ring ambient source Phi):0_(ring ambient target Phi))}|(vars ring ambient target Phi));
    r := # (source Phi)#"dimAmbientSpaces";
    x := apply(take(Gr#"multigens",r),g -> matrix{g});
    d := entries diagonalMatrix toList(r:1);
    gensGr := flatten entries gens ideal Gr;
    local I; local J; local F; local psi;
    L := for i to r-1 list (
        I = select(gensGr,g -> take(degree g,r) == d_i);
        J = matrix apply(I,g -> flatten entries diff(x_i,g));
        F = entries transpose mingens kernel Sub J;
        if #F == 0 then (Phi#"isBirational" = false; error "the multi-rational map is not birational");
        psi = rationalMap(first F,Dominant=>"notSimplify");
        psi#"maps" = apply(F,f -> map(source psi,target psi,f));
        psi#"map" = first psi#"maps";
        psi
    );
    Psi := multirationalMap(L,source Phi);
    if ring source Psi =!= ring target Phi then error "internal error encountered";  
    Psi#"source" = target Phi;
    if b >= 2 then (check Psi; try checkRepresentatives Psi else error "something went wrong in calculating the inverse map: wrong representatives"; <<"-- representatives of "<<toString expression Psi<<" have been successfully checked!"<<endl);
    Psi#"isBirational" = true;
    Psi#"isDominant" = true;
    if b >= 1 and Phi#"isBirational" === null and Phi#"isDominant" === null then (
        if image Phi != target Phi then (Phi#"isDominant" = false; Phi#"isBirational" = false; error "the multi-rational map is not dominant");
        Phi#"isDominant" = true;
    );
    if b >= 1 and Phi#"isBirational" === null then (
        if degree Phi != 1 then (Phi#"isBirational" = false; error("the multi-rational map is not birational, its degree is "|toString(degree Phi)));
        Phi#"isBirational" = true;
    );
    if b >= 3 then (if Phi * Psi != 1 then error "something went wrong in calculating the inverse map: the composition of the maps is not identity"; <<"-- composition of "<<toString expression Phi<<" with "<<toString expression Psi<<" has been successfully verified!"<<endl);
    if b >= 4 then (if Psi * Phi != 1 then error "something went wrong in calculating the inverse map: the composition of the maps is not identity"; <<"-- composition of "<<toString expression Psi<<" with "<<toString expression Phi<<" has been successfully verified!"<<endl);
    if Phi#"isBirational" === true then (
        (last graph Phi)#"isBirational" = true;
        (last graph Phi)#"isDominant" = true;
        Phi#"inverse" = Psi;
        Psi#"inverse" = Phi;
        Psi#"graph" = reverseGraph graph Phi;
    );
    return Psi;
);

inverse MultirationalMap := Phi -> inverse(Phi,Verify=>true);
inverse (MultihomogeneousRationalMap,Option) := (Phi,opt) -> inverse(multirationalMap {Phi},opt);
inverse MultihomogeneousRationalMap := Phi -> inverse(Phi,Verify=>true);

inverse2 = method(TypicalValue => MultirationalMap);
inverse2 (MultirationalMap,Option) := (Phi,opt) -> (
    if Phi#"inverse" =!= null or Phi#"graph" =!= null then return inverse(Phi,opt);
    G := projectiveVariety(SymmIdeal Phi,MinimalGenerators=>false,Saturate=>false); -- warning: this may not be multi-saturated
    (r,s) := (# (source Phi)#"dimAmbientSpaces", # (target Phi)#"dimAmbientSpaces");
    Phi#"graph" = (multirationalMap(take(projections G,r),source Phi), multirationalMap(take(projections G,-s),target Phi));
    isBir := Phi#"isBirational";
    Phi#"isBirational" = null;
    Psi := inverse(Phi,Verify=>false);
    Phi#"graph" = null;
    Phi#"isBirational" = isBir;
    err := "not able to get an inverse map by using dedicated algorithm for the multi-linear type case; try using the general function inverse";
    if last toList opt then (
        try checkRepresentatives Psi else error(err|"(*)");
        if not(Phi * Psi == 1 and Psi * Phi == 1) then error(err|"()"); 
        Phi#"isBirational" = true;
        Phi#"isDominant" = true;
        Psi#"isBirational" = true;
        Psi#"isDominant" = true;
        Phi#"inverse" = Psi;
        Psi#"inverse" = Phi;
    );
    return Psi;
);

inverse2 MultirationalMap := Phi -> inverse2(Phi,Verify=>true);
inverse2 (MultihomogeneousRationalMap,Option) := (Phi,opt) -> inverse2(multirationalMap {Phi},opt);
inverse2 MultihomogeneousRationalMap := Phi -> inverse2(Phi,Verify=>true);

isIsomorphism MultirationalMap := Phi -> (
    if dim source Phi != dim target Phi or Phi#"isBirational" === false or Phi#"isDominant" === false then return false;
    if not isMorphism Phi then return false;
    isMorphism inverse(Phi,Verify=>true)
);

MultirationalMap | MultiprojectiveVariety := (Phi,X) -> (
    if X === source Phi then return Phi;
    if ring ideal source Phi =!= ring ideal X then error "expected a subvariety in the ambient space of the source";
    if not isSubset(X,source Phi) then error "expected a subvariety of the source";
    I := multirationalMap(apply(multigens ring X,o -> rationalMap(o,Dominant=>"notSimplify")),source Phi);
    if ring source I =!= ring X then error "internal error encountered: bad source found";
    I#"source" = X;
    I * Phi
);

MultirationalMap | List := (Phi,d) -> (
    if not(# d == # (source Phi)#"dimAmbientSpaces" and all(d,i->instance(i,ZZ) and i>=0)) then error("expected a list of "|toString(# (source Phi)#"dimAmbientSpaces")|" non-negative integer(s) to indicate the degree of a hypersurface in the source"); 
    Phi|((source Phi) * projectiveVariety ideal random(d,ring ambient source Phi))
);

MultirationalMap || MultiprojectiveVariety := (Phi,Y) -> (
    if Y === target Phi then return Phi;
    X := Phi^* Y;
    I := multirationalMap(apply(multigens ring X,o -> rationalMap(o,Dominant=>"notSimplify")),source Phi);
    if ring source I =!= ring X then error "internal error encountered: bad source found";
    I#"source" = X;
    multirationalMap(I * Phi,Y)
);

MultirationalMap || List := (Phi,d) -> (
    if not(# d == # (target Phi)#"dimAmbientSpaces" and all(d,i->instance(i,ZZ) and i>=0)) then error("expected a list of "|toString(# (target Phi)#"dimAmbientSpaces")|" non-negative integer(s) to indicate the degree of a hypersurface in the target"); 
    Phi||((target Phi) * projectiveVariety ideal random(d,ring ambient target Phi))
);

super MultirationalMap := Phi -> (
    if target Phi == ambient target Phi then return Phi; 
    multirationalMap(apply(factor Phi,super),ambient target Phi)
);

MultirationalMap | MultirationalMap := (Phi,Psi) -> (
    if source Phi =!= source Psi then error "expected multi-rational maps with the same source";
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

describe MultirationalMap := Phi -> (
    n := # factor Phi;
    descr:="multi-rational map consisting of "|(if n == 1 then "one single rational map" else (toString(n))|" rational maps")|newline;
    descr=descr|"source variety: "|(? source Phi)|newline;
    descr=descr|"target variety: "|(? target Phi)|newline;
    descr=descr|"base locus: "|(? baseLocus Phi)|newline;
    if image Phi == target Phi then descr=descr|"dominance: "|toString(Phi#"isDominant")|newline else descr=descr|"dominance: "|toString(Phi#"isDominant")|newline|"image: "|(? image Phi)|newline;
    descr = descr|"multidegree: "|toString(multidegree Phi)|newline;
    descr=descr|"degree: "|toString(degree Phi)|newline;
    for i to n-1 do descr=descr|"degree sequence (map "|toString(i+1)|"/"|toString(n)|"): "|toString(degreeSequence (factor Phi)_i)|newline;
    descr=descr|"coefficient ring: "|toString(coefficientRing Phi);
    net expression descr
);

? MultirationalMap := Phi -> (
    n := # factor Phi;
    descr:="multi-rational map consisting of "|(if n == 1 then "one single rational map" else (toString(n))|" rational maps")|newline;
    descr=descr|"source variety: "|(? source Phi)|newline;
    descr=descr|"target variety: "|(? target Phi)|newline;
    if Phi#"baseLocus" =!= null then descr=descr|"base locus: "|(? baseLocus Phi)|newline;
    if Phi#"image" =!= null then (if image Phi == target Phi then descr=descr|"dominance: "|toString(Phi#"isDominant")|newline else descr=descr|"dominance: "|toString(Phi#"isDominant")|newline|"image: "|(? image Phi)|newline);
    if Phi#"multidegree" =!= null then descr = descr|"multidegree: "|toString(multidegree Phi)|newline;
    if Phi#"multidegree" =!= null and Phi#"image" =!= null then descr=descr|"degree: "|toString(degree Phi)|newline;
    net expression descr
);

degreeSequence = method();
degreeSequence RationalMap := phi -> new Array from apply(maps phi,F -> (u := unique degrees ideal compress toMatrix F; if #u != 1 then error "internal error encountered"; unsequence toSequence first u));
degreeSequence MultihomogeneousRationalMap := phi -> new Array from apply(maps phi,F -> (u := unique degrees ideal compress toMatrix F; if #u != 1 then error "internal error encountered"; unsequence toSequence first u));
degreeSequence MultirationalMap := Phi -> apply(factor Phi,degreeSequence);

permute (MultiprojectiveVariety,List) := (X,l) -> (
    if not all(l,i -> instance(i,ZZ)) then error "expected a list of integers";
    d := # X#"dimAmbientSpaces";
    if sort l != toList(0 .. d-1) then error("expected a permutation of the set "|toString toList(0 .. d-1));
    m := X#"multigens"; 
    m' := for i to d-1 list m_(l_i);
    K := coefficientRing X;
    D := entries diagonalMatrix toList(d : 1);
    R := K[flatten m',Degrees=>apply(d,i -> #(m'_i) : D_i)];
    X' := projectiveVariety(sub(ideal X,R),MinimalGenerators=>true,Saturate=>false);
    check multirationalMap(apply(m',x -> rationalMap sub(matrix{x},ring X)),X')
);

show MultirationalMap := Phi -> (
    F := factor Phi;
    n := # F;
    S := "-- multi-rational map --"||("source: "|nicePrint(ring source Phi))||("target: "|nicePrint(ring target Phi));
    for i to n-1 do (
        S = S||"-- rational map "|toString(i+1)|"/"|toString(n)|" -- ";
        if (F_i)#"maps" === null 
        then S = S||"map "|toString(i+1)|"/"|toString(n)|", one of its representatives:"||nicePrint(entries F_i) 
        else if #((F_i)#"maps") == 1 
        then S = S||"map "|toString(i+1)|"/"|toString(n)|", unique representative:"||nicePrint(entries F_i) 
        else for j to #((F_i)#"maps")-1 do S = S||"map "|toString(i+1)|"/"|toString(n)|", representative "|toString(j+1)|"/"|toString(#((F_i)#"maps"))|":"||nicePrint(flatten entries toMatrix ((F_i)#"maps")_j);
    );
    return S;
);
show RationalMap := Phi -> show multirationalMap {Phi};
show MultihomogeneousRationalMap := Phi -> show multirationalMap {Phi};

rationalMap MultiprojectiveVariety := o -> X -> multirationalMap rationalMap(ideal X,Dominant=>o.Dominant);
rationalMap (MultiprojectiveVariety,List) := o -> (X,l) -> multirationalMap rationalMap(ideal X,l,Dominant=>o.Dominant);
rationalMap (MultiprojectiveVariety,ZZ) := o -> (X,a) -> multirationalMap rationalMap(ideal X,a,Dominant=>o.Dominant);
rationalMap (MultiprojectiveVariety,ZZ,ZZ) := o -> (X,a,b) -> multirationalMap rationalMap(ideal X,a,b,Dominant=>o.Dominant);

clean MultirationalMap := Phi -> multirationalMap(apply(factor Phi,clean),target Phi);
clean RationalMap := phi -> rationalMap(map phi,Dominant=>"notSimplify");
clean MultihomogeneousRationalMap := phi -> rationalMap(map phi,Dominant=>"notSimplify");

MultirationalMap ** Ring := (Phi,K) -> (
   if not isField K then error "expected a field";
   if (char coefficientRing Phi =!= char K and char coefficientRing Phi =!= 0) then error "characteristic not valid";
   X := (source Phi) ** K;
   Y := (target Phi) ** K;
   F := apply(factor Phi,projections Y,(f,p) -> rationalMap(map(ring X,target p,sub(lift(matrix f,ring ambient source Phi),vars ring ambient X)),Dominant=>"notSimplify"));
   Psi := multirationalMap(F,Y);
   if ring source Psi =!= ring X then error "internal error encountered";
   Psi#"source" = X;
   Psi#"isDominant" = Phi#"isDominant";
   Psi#"isBirational" = Phi#"isBirational";
   return Psi;
);

beginDocumentation() 

document {Key => {MultiprojectiveVarieties}, 
Headline => "Multi-projective varieties and multi-rational maps",
PARA{"This is package for handling multi-projective varieties, that is, closed subvarieties of products of projective spaces, and rational maps between them. This extends the package ",TO Cremona,", which treats ",TO2{RationalMap,"rational maps"}," from multi-projective varieties to ",EM"standard"," projective varieties, ",TEX///$X\subseteq \mathbb{P}^{k_1}\times\mathbb{P}^{k_2}\times\cdots\times\mathbb{P}^{k_n}\dashrightarrow Y\subseteq\mathbb{P}^N$///,"."},References => {"ArXiv preprint: ",HREF{"https://arxiv.org/abs/2101.04503","Computations with rational maps between multi-projective varieties"},"."}}

document {Key => {MultiprojectiveVariety}, 
Headline => "the class of all multi-projective varieties", 
PARA {"A ",EM"multi-projective variety"," is a closed subvariety of a product of projective spaces ",TEX///$\mathbb{P}^{k_1}\times\mathbb{P}^{k_2}\times\cdots\times\mathbb{P}^{k_n}$///,"."}}

document {Key => {Saturate, [projectiveVariety,Saturate]},
Headline => "whether to compute the multi-saturation of the ideal",
Usage => "projectiveVariety(I,Saturate=>false)", 
PARA{"Use this option only in the case you know that the ideal ",TT"I"," is already multi-saturated, otherwise nonsensical answers may result."},
SeeAlso => {projectiveVariety}}

document { 
Key => {projectiveVariety, (projectiveVariety,Ideal), (projectiveVariety,Ring), [projectiveVariety,MinimalGenerators]}, 
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
"? X -- short description",
"describe X -- long description"},
PARA{"Below, we calculate the image of ",TEX///$X$///," via the Segre embedding of ",TEX///$\mathbb{P}^{2}\times\mathbb{P}^{3}\times\mathbb{P}^{1}$///," in ",TEX///$\mathbb{P}^{23}$///,"; thus we get a projective variety isomorphic to ",TEX///$X$///," and embedded in a single projective space ",TEX///$\mathbb{P}^{19}=<X>\subset\mathbb{P}^{23}$///,"."},
EXAMPLE {
"s = segre X;",
"X' = projectiveVariety image s",
"(dim X', codim X', degree X')",
"? X'"},
SeeAlso => {(segre,MultiprojectiveVariety),(dim,MultiprojectiveVariety),(codim,MultiprojectiveVariety),(degree,MultiprojectiveVariety),(singularLocus,MultiprojectiveVariety),(point,MultiprojectiveVariety)}} 

document {Key => {(projectiveVariety,List,Ring)}, 
Headline => "product of projective spaces", 
Usage => "projectiveVariety(l,K)", 
Inputs => {"l" => List => {"a list of non-negative integers ",TEX///$l=\{l_1,l_2,\ldots,l_n\}$///},"K" => Ring => {"a field"}}, 
Outputs => {MultiprojectiveVariety => {"the product of projective spaces ", TEX///$\mathbb{P}^{l_1}\times\mathbb{P}^{l_2}\times\cdots\times\mathbb{P}^{l_n}$///," over ",TEX///$K$///}}, 
EXAMPLE {"projectiveVariety({2,1,3},ZZ/33331);","projectiveVariety({1,1,1,1},QQ);","projectiveVariety({},QQ);"}} 

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
SeeAlso => {(ring,MultiprojectiveVariety),(symbol **,MultiprojectiveVariety,Ring)}} 

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
Headline => "pick a random rational point on a multi-projective variety", 
Usage => "point X", 
Inputs => {"X" => MultiprojectiveVariety => {"defined over a finite field"}}, 
Outputs => {MultiprojectiveVariety => {"a random rational point on ", TEX///$X$///}}, 
EXAMPLE {"X = projectiveVariety ideal random({2,1},ZZ/101[x_0,x_1,x_2,y_0,y_1,Degrees=>{3:{1,0},2:{0,1}}]);","p = point X;"},
SeeAlso => {point,randomKRationalPoint}} 

document {Key => {(singularLocus,MultiprojectiveVariety)}, 
Headline => "the singular locus of the variety", 
Usage => "singularLocus X", 
Inputs => {"X" => MultiprojectiveVariety => {"which is assumed to be equidimensional"}}, 
Outputs => { MultiprojectiveVariety => {"the singular locus of ", TEX///$X$///}}, 
EXAMPLE {"X = projectiveVariety ideal random({2,1},ZZ/101[x_0,x_1,x_2,y_0,y_1,Degrees=>{3:{1,0},2:{0,1}}]);","singularLocus X;","Y = X + projectiveVariety (ideal random({1,1},ring ambient X));","singularLocus Y;"}} 

document {Key => {(symbol ==,MultiprojectiveVariety,MultiprojectiveVariety)}, 
Headline => "equality of multi-projective varieties", 
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
Headline => "product of multi-projective varieties", 
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
SeeAlso => {fiberProduct,(symbol ^,MultiprojectiveVariety,ZZ)}}

document {Key => {(symbol ^,MultiprojectiveVariety,ZZ)}, 
Headline => "power of a multi-projective variety", 
Usage => "X^n", 
Inputs => {"X" => MultiprojectiveVariety,"n" => ZZ}, 
Outputs => {MultiprojectiveVariety => {"the product of ",TEX///$n$///," copies of ", TEX///$X$///}}, 
EXAMPLE {"X = projectiveVariety kernel veronese(1,3,ZZ/33331);",
"X^2",
"X^3",
"X^5",
"assert(X^3 == X^2 ** X)",
"assert(X^5 == X^3 ** X^2)"},
SeeAlso => {(symbol **,MultiprojectiveVariety,MultiprojectiveVariety)}} 

document {Key => {(symbol *,MultiprojectiveVariety,MultiprojectiveVariety)}, 
Headline => "intersection of multi-projective varieties", 
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
Headline => "union of multi-projective varieties", 
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
Headline => "difference of multi-projective varieties", 
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

document {Key => {fiberProduct,(fiberProduct,RationalMap,RationalMap)}, 
Headline => "fiber product of multi-projective varieties", 
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
Headline => "topological Euler characteristic of a (smooth) multi-projective variety", 
Usage => "euler X
euler(X,Verify=>b)", 
Inputs => { 
MultiprojectiveVariety => "X" => {"which has to be smooth, and ",TT"b"," is a ",TO2{Boolean,"boolean value"},", that is, ",TT"true"," or ",TT"false"," (the default value is ",TT"false",")"}}, 
Outputs => { 
ZZ => {"the topological Euler characteristics of the variety ",TT"X",", calculated as ",TO EulerCharacteristic,TT"(ideal X,MathMode=>b)"}},
EXAMPLE {
"X = projectiveVariety minors(2,genericSymmetricMatrix(ZZ/33331[vars(0..5)],3));",
"euler X"},
SeeAlso => {EulerCharacteristic,(euler,ProjectiveVariety)}}

document {Key => {MultirationalMap}, 
Headline => "the class of all multi-rational maps", 
PARA {"A ",EM"multi-rational map"," is a rational map between ",TO2{MultiprojectiveVariety,"multi-projective varieties"},", ",TEX///$$\Phi:X\subseteq \mathbb{P}^{r_1}\times\mathbb{P}^{r_2}\times\cdots\times\mathbb{P}^{r_n}\dashrightarrow Y \subseteq \mathbb{P}^{s_1}\times\mathbb{P}^{s_2}\times\cdots\times\mathbb{P}^{s_m} .$$///,"Thus, it can be represented by an ",TO2{List,"ordered list"}," of ",TO2{RationalMap,"rational maps"},TEX///$$\Phi_i = (\Phi:X\dashrightarrow Y)\circ(pr_i:Y\to Y_i\subseteq\mathbb{P}^{s_i}) ,$$///,"for ",TEX///$i=1,\ldots,m$///,". The maps ",TEX///$\Phi_i:X\dashrightarrow Y_i\subseteq\mathbb{P}^{s_i}$///,", since the target ",TEX///$Y_i$///," is a standard projective variety, are implemented with the class ",TO RationalMap," (more properly, when ",TEX///$n>1$///," the class of such maps is called ",TT "MultihomogeneousRationalMap","). Recall that the main constructor for the class ",TO RationalMap," (as well as for the class ", TT"MultihomogeneousRationalMap",") is the method ",TO rationalMap,"."},
PARA {"The constructor for the class of multi-rational maps is ",TO multirationalMap,", which takes as input the list of maps ",
TEX///$\{\Phi_1:X\dashrightarrow Y_1,\ldots,\Phi_m:X\dashrightarrow Y_m\}$///,", together with the variety ",TEX///$Y$///,", and returns the map ",TEX///$\Phi:X\dashrightarrow Y$///,"."}}

document { 
Key => {multirationalMap, (multirationalMap,List,MultiprojectiveVariety), (multirationalMap,List)}, 
Headline => "the multi-rational map defined by a list of rational maps", 
Usage => "multirationalMap Phi
multirationalMap(Phi,Y)", 
Inputs => { "Phi" => {ofClass List," of ",TO2{RationalMap,"rational maps"},", ",TEX///$\{\Phi_1:X\dashrightarrow Y_1\subseteq\mathbb{P}^{s_1},\ldots,\Phi_m:X\dashrightarrow Y_m\subseteq\mathbb{P}^{s_m}\}$///,", all having the same ",TO2{(source,RationalMap),"source"}," ",TEX///$X\subseteq \mathbb{P}^{r_1}\times\mathbb{P}^{r_2}\times\cdots\times\mathbb{P}^{r_n}$///},
"Y" => {ofClass MultiprojectiveVariety," ",TEX///$Y \subseteq \mathbb{P}^{s_1}\times\mathbb{P}^{s_2}\times\cdots\times\mathbb{P}^{s_m}$///," (if omitted, then the ",TO2{(symbol **,MultiprojectiveVariety,MultiprojectiveVariety),"product"}," ",TEX///$Y_1\times\cdots \times Y_m$///," is taken)"}},
Outputs => {MultirationalMap => {"the unique rational map ",TEX///$\Phi:X\subseteq \mathbb{P}^{r_1}\times\mathbb{P}^{r_2}\times\cdots\times\mathbb{P}^{r_n}\dashrightarrow Y \subseteq \mathbb{P}^{s_1}\times\mathbb{P}^{s_2}\times\cdots\times\mathbb{P}^{s_m}$///," such that ",TEX///$pr_i\circ\Phi = \Phi_i$///,", where ",TEX///$pr_i:Y\subseteq \mathbb{P}^{s_1}\times\mathbb{P}^{s_2}\times\cdots\times\mathbb{P}^{s_m} \to Y_i\subseteq \mathbb{P}^{s_i}$///," denotes the i-th projection"}},
EXAMPLE {
"R = ZZ/65521[x_0..x_2,y_0,y_1,Degrees=>{3:{1,0},2:{0,1}}];", 
"f = rationalMap for i to 3 list random({1,1},R);",
"g = rationalMap(for i to 4 list random({0,1},R),Dominant=>true);",
"h = rationalMap for i to 2 list random({1,0},R);",
"Phi = multirationalMap {f,g,h}",
"describe Phi -- long description",
"? Phi -- short description",
"X = projectiveVariety R;",
"Phi;",
"Y = target Phi;",
"Phi;",
"Z = (image multirationalMap {f,g}) ** projectiveVariety target h;",
"Psi = multirationalMap({f,g,h},Z)",
"assert(image Psi == image Phi)"},
SeeAlso => {rationalMap,(graph,MultirationalMap),(image,MultirationalMap),(baseLocus,MultirationalMap),(inverse,MultirationalMap)},
Caveat => {"Be careful when you pass the target ",TT"Y"," as input, because it must be compatible with the maps but for efficiency reasons a full check is not done automatically. See ",TO (check,MultirationalMap),"."}}

document { 
Key => {(check,MultirationalMap)}, 
Headline => "check that a multi-rational map is well-defined", 
Usage => "check Phi", 
Inputs => {MultirationalMap}, 
Outputs => {MultirationalMap => {"the same object passed as input, but an error is thrown if the target of the map is not compatible."}},
EXAMPLE {
"f = rationalMap kernel veronese(1,4,ZZ/65521);",
"Phi = multirationalMap {f}",
"check Phi",
"Y = image Phi",
"Psi = multirationalMap({f},Y)",
"check Psi",
"p = point Y;",
"Eta = multirationalMap({f},p);",
"try check Eta else <<endl<<\"meaningless object!\";"}}

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
SeeAlso => {(coefficientRing,MultiprojectiveVariety),(symbol **,MultirationalMap,Ring)}}

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
MultiprojectiveVariety => "Y" => {"a subvariety of the ambient of the ",TO2{(target,MultirationalMap),"target"}," of ",TT "Phi"}}, 
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

document {Key => {(parametrize,MultiprojectiveVariety)}, 
Headline => "try to get a parametrization of a multi-projective variety", 
Usage => "parametrize X", 
Inputs => {"X" => MultiprojectiveVariety => {"a rational ",TEX///$k$///,"-dimensional subvariety of ",TEX///$\mathbb{P}^{r_1}\times\cdots\times\mathbb{P}^{r_n}$///," (of some simple type)"}}, 
Outputs => {MultirationalMap => {"a birational map from ",TEX///$\mathbb{P}^k$///," to ",TEX///$X$///}}, 
EXAMPLE {"K = ZZ/65521;",
"X = projectiveVariety({2,4,1,3},K);",
"f = parametrize X;",
"R = ring X;",
"Y = projectiveVariety ideal(random({1,0,0,0},R),random({0,1,0,0},R),random({0,1,0,0},R),random({0,0,0,1},R));",
"g = parametrize Y;",
"Z = projectiveVariety ideal(random({1,1,0,0},R),random({0,1,0,0},R),random({0,0,1,0},R),random({0,0,0,1},R),random({0,0,0,1},R));",
"h = parametrize Z;",
"describe h",
"describe inverse h"}, 
SeeAlso => {(inverse,MultirationalMap)}}

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

document { 
Key => {(symbol ==,MultirationalMap,MultirationalMap)}, 
Headline => "equality of multi-rational maps", 
Usage => "Phi == Psi", 
Inputs => { 
MultirationalMap => "Phi",
MultirationalMap => "Psi"}, 
Outputs => { 
Boolean => {"whether ",TT"Phi"," and ",TT"Psi", " are the same multi-rational map"}},
SeeAlso => {(symbol ==,RationalMap,RationalMap),(symbol <==>,MultirationalMap,MultirationalMap)}}

document { 
Key => {(symbol <==>,MultirationalMap,MultirationalMap)}, 
Headline => "equality of multi-rational maps with checks on internal data", 
Usage => "Phi <==> Psi", 
Inputs => { 
MultirationalMap => "Phi",
MultirationalMap => "Psi"}, 
Outputs => { 
Boolean => {"whether ",TT"Phi"," and ",TT"Psi", " are the same multi-rational map, by throwing an error if an inconsistency of the internal data is detected."}},
PARA{"For example, if you have calculated the degree of the two maps before, it will be checked that it is the same for both."},
SeeAlso => {(symbol ==,MultirationalMap,MultirationalMap)}}

document { 
Key => {(multidegree,ZZ,MultirationalMap)}, 
Headline => "i-th projective degree of a multi-rational map using a probabilistic approach", 
Usage => "multidegree(i,Phi)", 
Inputs => {ZZ => "i", MultirationalMap => "Phi"}, 
Outputs => { 
ZZ => {"the ",TEX///$i$///,"-th projective degree of ",TT"Phi"}},
PARA{"This is calculated by means of the inverse image of an appropriate random subvariety of the target."},
EXAMPLE {
"Phi = last graph multirationalMap {rationalMap kernel veronese(1,4,ZZ/300007)};",
"time multidegree(4,Phi)",
"time multidegree(3,Phi)",
"time multidegree(2,Phi)",
"time multidegree(1,Phi)",
"time multidegree(0,Phi)",
"time multidegree Phi"},
SeeAlso => {(multidegree,MultirationalMap),(projectiveDegrees,RationalMap),(degree,MultirationalMap,Option),(symbol ^*,MultirationalMap)},
References => {"ArXiv preprint: ",HREF{"https://arxiv.org/abs/2101.04503","Computations with rational maps between multi-projective varieties"},"."}}

document { 
Key => {(multidegree,MultirationalMap)}, 
Headline => "projective degrees of a multi-rational map", 
Usage => "multidegree Phi", 
Inputs => { 
MultirationalMap => "Phi"}, 
Outputs => { 
List => {"the list of projective degrees of ",TT"Phi"}},
PARA{"This calculates the ",TO2{(multidegree,MultiprojectiveVariety),"multidegree"}," of the ",TO2{(graph,MultirationalMap),"graph"}," and converts it to the list of projective degrees."},
EXAMPLE {
"ZZ/300007[x_0..x_3], f = rationalMap {x_2^2-x_1*x_3, x_1*x_2-x_0*x_3, x_1^2-x_0*x_2}, g = rationalMap {x_1^2-x_0*x_2, x_0*x_3, x_1*x_3, x_2*x_3, x_3^2};",
"Phi = last graph multirationalMap {f,g}",
"time multidegree Phi",
"(degree source Phi,degree image Phi)"},
SeeAlso => {(multidegree,ZZ,MultirationalMap),(multidegree,RationalMap),(degree,MultirationalMap)},
References => {"ArXiv preprint: ",HREF{"https://arxiv.org/abs/2101.04503","Computations with rational maps between multi-projective varieties"},"."}}

document { 
Key => {(degree,MultirationalMap,Option)}, 
Headline => "degree of a multi-rational map using a probabilistic approach", 
Usage => "degree(Phi,Strategy=>\"random point\")
degree(Phi,Strategy=>\"0-th projective degree\")", 
Inputs => {MultirationalMap => "Phi", Option => "Strategy"}, 
Outputs => { 
ZZ => {"the degree of ",TT"Phi",". So this value is 1 if and only if (with high probability) the map is birational onto its image."}},
EXAMPLE {
"R = ZZ/33331[x_0..x_4];",
"Phi = (last graph multirationalMap {rationalMap {transpose jacobian(-x_2^3+2*x_1*x_2*x_3-x_0*x_3^2-x_1^2*x_4+x_0*x_2*x_4)}})||projectiveVariety ideal(random(2,R));",
"? source Phi, ? target Phi",
"time degree(Phi,Strategy=>\"random point\")",
"time degree(Phi,Strategy=>\"0-th projective degree\")",
"time degree Phi"},
SeeAlso => {(degree,MultirationalMap),(degreeMap,RationalMap),(multidegree,ZZ,MultirationalMap),(point,MultiprojectiveVariety),(symbol ^*,MultirationalMap)}}

document { 
Key => {(degree,MultirationalMap)}, 
Headline => "degree of a multi-rational map", 
Usage => "degree Phi", 
Inputs => { 
MultirationalMap => "Phi"}, 
Outputs => { 
ZZ => {"the degree of ",TT"Phi",". So this value is 1 if and only if the map is birational onto its image."}},
PARA{"This is just a shortcut for ",TT"(last multidegree Phi)/(degree image Phi)","."},
EXAMPLE {
"ZZ/300007[x_0..x_3], f = rationalMap {x_2^2-x_1*x_3, x_1*x_2-x_0*x_3, x_1^2-x_0*x_2}, g = rationalMap {x_1^2-x_0*x_2, x_0*x_3, x_1*x_3, x_2*x_3, x_3^2};",
"Phi = last graph multirationalMap {f,g}",
"time degree Phi"},
SeeAlso => {(degree,MultirationalMap,Option),(degree,RationalMap),(multidegree,MultirationalMap)}}

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
Key => {(multirationalMap,MultiprojectiveVariety),(symbol _,ZZ,MultiprojectiveVariety)}, 
Headline => "identity map", 
Usage => "multirationalMap X
1_X", 
Inputs => {MultiprojectiveVariety => "X"}, 
Outputs => {MultirationalMap => {"the identity map on ",TT"X"}},
EXAMPLE {"X = projectiveVariety({2,3,1},QQ);", "1_X;"},
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
"try multirationalMap(ambient X,X) else <<endl<<\"not able to construct it!\";"},
SeeAlso => {(multirationalMap,MultiprojectiveVariety)}}

document { 
Key => {(multirationalMap,MultirationalMap,MultiprojectiveVariety)}, 
Headline => "change the target of a multi-rational map", 
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
Phi^-1
inverse(Phi,Verify=>true)
inverse(Phi,Verify=>false)", 
Inputs => {MultirationalMap => "Phi" => {"a birational map"}}, 
Outputs => {MultirationalMap => {"the inverse map of ",TT"Phi"}},
PARA{"This function applies a general algorithm to calculate the inverse map passing through the computation of the ",TO2{(graph,MultirationalMap),"graph"},". Note that by default the option ",TT"Verify"," is set to ",TT"true",", which means that the birationality of the map is verified using ",TO2{(degree,MultirationalMap),"degree"},TT" Phi == 1"," and ",TO2{(image,MultirationalMap),"image"},TT" Phi == ",TO2{(target,MultirationalMap),"target"},TT" Phi","."},
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
"assert(Phi * Phi^-1 == 1 and Phi^-1 * Phi == 1)",
"assert(Psi * Psi^-1 == 1 and Psi^-1 * Psi == 1)",
"assert(Eta * Eta^-1 == 1 and Eta^-1 * Eta == 1)"},
SeeAlso => {(graph,MultirationalMap),(symbol *,MultirationalMap,MultirationalMap),(symbol ==,MultirationalMap,MultirationalMap),(degree,MultirationalMap),(image,MultirationalMap),(inverse,RationalMap),inverse2},
Caveat => {"If the option ",TT"Verify"," is set to ",TT"false"," (which is preferable for efficiency), then no test is done to check that the map is birational, and if not then often the error is not thrown at all and a nonsense answer is returned."},
References => {"ArXiv preprint: ",HREF{"https://arxiv.org/abs/2101.04503","Computations with rational maps between multi-projective varieties"},"."}}

document { 
Key => {inverse2,(inverse2,MultirationalMap)}, 
Headline => "inverse of a birational map using a faster algorithm for a special class of maps", 
Usage => "inverse2 Phi
inverse2(Phi,Verify=>true)
inverse2(Phi,Verify=>false)", 
Inputs => {MultirationalMap => "Phi" => {"a birational map of so-called ",EM"multi-linear type"}}, 
Outputs => {MultirationalMap => {"the inverse map of ",TT"Phi"}},
PARA{"This assumes that the ",TO2{(graph,MultirationalMap),"graph"}," of the input map ",TT"Phi"," is defined by a ",EM "simplified"," system of equations, which may not be true. If the option ",TT"Verify"," is set to ",TT"true",", which is the default choice, then it is verified that the left and right composition of ",TT"Phi"," with the returned map is the identity, throwing an error if this is not the case."},
EXAMPLE {
"Phi = last graph multirationalMap quadroQuadricCremonaTransformation(11,1,ZZ/65521);",
"time Psi = inverse2 Phi;",
"Phi' = clean Phi;",
"time Psi' = inverse Phi';",
"assert(Psi == Psi' and describe Psi == describe Psi')",
"describe Psi"},
SeeAlso => {(inverse,MultirationalMap)},
Caveat => {"This is an experimental function."}}

document { 
Key => {(isIsomorphism,MultirationalMap)}, 
Headline => "whether a birational map is an isomorphism", 
Usage => "isIsomorphism Phi", 
Inputs => {"Phi" => MultirationalMap}, 
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
"assert(o8 and (not o6) and (not o4))"},
SeeAlso => {(inverse,MultirationalMap),(isMorphism,MultirationalMap)}}

document { 
Key => {baseLocus,(baseLocus,MultirationalMap)}, 
Headline => "the base locus of a multi-rational map", 
Usage => "baseLocus Phi", 
Inputs => {MultirationalMap => "Phi"}, 
Outputs => {MultiprojectiveVariety => {"the base locus of ",TT"Phi",", that is, the locus where it is not defined"}},
SeeAlso => {(isMorphism,MultirationalMap),(ideal,RationalMap)}}

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

document { 
Key => {(random,List,MultiprojectiveVariety)}, 
Headline => "get a random hypersurface of given multi-degree containing a multi-projective variety", 
Usage => "random(d,X)", 
Inputs => {List => "d" => {"a list of ",TEX///$n$///," nonnegative integers"},
MultiprojectiveVariety => "X" => {"a subvariety of ",TEX///$\mathbb{P}^{k_1}\times\cdots\times\mathbb{P}^{k_n}$///}}, 
Outputs => {MultiprojectiveVariety => {"a random hypersurface in ",TEX///$\mathbb{P}^{k_1}\times\cdots\times\mathbb{P}^{k_n}$///," of multi-degree ",TEX///$d$///," containing ",TEX///$X$///}},
PARA{"More generally, if ",TT"d"," is a list of multi-degrees, then the output is the intersection of the hypersurfaces ",TT "random(d_i,X)","."},
EXAMPLE {
"X = projectiveVariety kernel veronese(1,3,ZZ/65521);",
"random({2},X);",
"ideal oo",
"random({{2},{2}},X);",
"ideal oo",
"X = X^2;",
"random({1,2},X);",
"ideal oo",
"random({{1,2},{1,2},{2,0}},X);",
"degrees oo"}}

document { 
Key => {(permute,MultiprojectiveVariety,List)}, 
Headline => "permute the dimensions of the ambient space", 
Usage => "permute(X,s)", 
Inputs => {"X" => MultiprojectiveVariety => {"a subvariety of ",TEX///$\mathbb{P}^{k_0}\times\mathbb{P}^{k_1}\times\cdots\times\mathbb{P}^{k_{n}}$///},
"s" => List => {"a permutation of the set ",TEX///$\{0,1,\ldots,n\}$///}},
Outputs => {MultirationalMap => {"an isomorphism from ",TEX///$X$///," to a subvariety of ",TEX///$\mathbb{P}^{k_{s(0)}}\times\mathbb{P}^{k_{s(1)}}\times\cdots\times\mathbb{P}^{k_{s(n)}}$///}},
EXAMPLE {
"R = ZZ/33331[x_0..x_2,y_0..y_3,z_0..z_1,Degrees=>{3:{1,0,0},4:{0,1,0},2:{0,0,1}}];",
"X = projectiveVariety R;",
"f = permute(X,{1,0,2});",
"assert isIsomorphism f",
"X = projectiveVariety(ideal random({0,1,1},R));",
"f = permute(X,{2,0,1});",
"assert isIsomorphism f"},
SeeAlso => {(permute,MultidimensionalMatrix,List)}}

document { 
Key => {(shape,MultiprojectiveVariety)}, 
Headline => "shape of the ambient of a multi-projective variety", 
Usage => "shape X", 
Inputs => {"M" => MultiprojectiveVariety => {"a subvariety of ",TEX///$\mathbb{P}^{k_1}\times\cdots\times\mathbb{P}^{k_{n}}$///}},
Outputs => {List => {"the list of integers ",TEX///$\{k_1, \ldots, k_n\}$///}},
EXAMPLE {
"X = projectiveVariety(ZZ/65521[x_0..x_2,y_0..y_3,z_0..z_1,Degrees=>{3:{1,0,0},4:{0,1,0},2:{0,0,1}}]);",
"shape X",
"p = point X;",
"shape p"},
SeeAlso => {(shape,MultidimensionalMatrix)}}

document { 
Key => {(show,MultirationalMap)}, 
Headline => "display a multi-rational map", 
Usage => "show Phi", 
Inputs => {"Phi" => MultirationalMap}, 
Outputs => {Net => {"a net of ",TT"Phi"}},
EXAMPLE { 
"Phi = inverse first graph last graph multirationalMap {rationalMap kernel veronese(1,3,ZZ/33331)}",
"time describe Phi",
"show Phi"},
SeeAlso => {(describe,MultirationalMap)}}

document { 
Key => {degreeSequence,(degreeSequence,MultirationalMap)}, 
Headline => "the (multi)-degree sequence of a (multi)-rational map", 
Usage => "degreeSequence Phi", 
Inputs => {"Phi" => MultirationalMap}, 
Outputs => {List => {"the list of the degree sequences for the rational maps returned by ",TO2{(factor,MultirationalMap),"factor"},TT" Phi","."}},
EXAMPLE { 
"Phi = inverse first graph last graph multirationalMap {rationalMap kernel veronese(1,3,ZZ/33331)};",
"degreeSequence Phi"},
SeeAlso => {(factor,MultirationalMap)},
References => {HREF{"https://www.sciencedirect.com/science/article/pii/S0021869304001930","Cremona transformations and some related algebras"},", by A. Simis."}}

document { 
Key => {(describe,MultirationalMap),(symbol ?,MultirationalMap)}, 
Headline => "describe a multi-rational map", 
Usage => "describe Phi
? Phi", 
Inputs => {"Phi" => MultirationalMap}, 
Outputs => {{"a description of ",TT"Phi"}},
PARA{TT"? Phi"," is a lite version of ",TT"describe Phi",". The latter has a different behavior than ",TO (describe,RationalMap),", since it performs computations."},
EXAMPLE {
"Phi = multirationalMap graph rationalMap kernel veronese(1,4,ZZ/65521);",
"time ? Phi",
"image Phi;",
"time ? Phi",
"time describe Phi",
"time ? Phi"},
SeeAlso => {(describe,MultiprojectiveVariety),(show,MultirationalMap)}}

document { 
Key => {(describe,MultiprojectiveVariety),(symbol ?,MultiprojectiveVariety)}, 
Headline => "describe a multi-projective variety", 
Usage => "describe X
? X", 
Inputs => {"X" => MultiprojectiveVariety}, 
Outputs => {{"a description of ",TT"X"}},
PARA{TT"? X"," is a lite version of ",TT"describe X","."},
EXAMPLE {
"X = source graph multirationalMap {rationalMap kernel veronese(1,3,ZZ/65521)};",
"? X",
"describe X",
"? image segre X"},
SeeAlso => {(describe,MultirationalMap)}}

document { 
Key => {(clean,MultirationalMap)}, 
Headline => "clean the internal information of a multi-rational map", 
Usage => "clean Phi", 
Inputs => {"Phi" => MultirationalMap}, 
Outputs => {MultirationalMap => {"which is identical to ",TT"Phi",", but new to the system"}},
PARA{"This is only useful for comparing computation times for various algorithms."},
EXAMPLE {"Phi = 1_(projectiveVariety({2},QQ));", "Psi = clean Phi;", "Phi == Psi", "Phi === Psi"}}

document { 
Key => {(symbol **,MultirationalMap,Ring)},
Headline => "change the coefficient ring of a multi-rational map", 
Usage => "Phi ** K", 
Inputs => {MultirationalMap => "Phi" => {"defined over a coefficient ring ",TT"F"},
Ring => "K" => {"the new coefficient ring (which must be a field)"}}, 
Outputs => {MultirationalMap => {"a multi-rational map defined over ",TT"K",", obtained by coercing the coefficients of the multi-forms defining ",TT"Phi", " into ",TT"K"}}, 
PARA {"It is necessary that all multi-forms in the old coefficient ring ",TT"F"," can be automatically coerced into the new coefficient ring ",TT"K","."},
EXAMPLE {
"Phi = inverse first graph rationalMap kernel veronese(2,2);",
"describe Phi",
"K = ZZ/65521;",
"Phi' = Phi ** K;",
"describe Phi'",
"Phi'' = Phi ** frac(K[t]);",
"describe Phi''"},
SeeAlso => {(coefficientRing,MultirationalMap),(symbol **,RationalMap,Ring),(symbol **,MultiprojectiveVariety,Ring)}}

document { 
Key => {(symbol **,MultiprojectiveVariety,Ring)},
Headline => "change the coefficient ring of a multi-projective variety", 
Usage => "X ** K", 
Inputs => {MultiprojectiveVariety => "X" => {"defined over a coefficient ring ",TT"F"},
Ring => "K" => {"the new coefficient ring (which must be a field)"}}, 
Outputs => {MultiprojectiveVariety => {"a multi-projective variety defined over ",TT"K",", obtained by coercing the coefficients of the multi-forms defining ",TT"X", " into ",TT"K"}}, 
PARA {"It is necessary that all multi-forms in the old coefficient ring ",TT"F"," can be automatically coerced into the new coefficient ring ",TT"K","."},
EXAMPLE {
"use ring projectiveVariety({2,3},QQ);",
"X = projectiveVariety ideal(x1_2^2-x1_1*x1_3,x1_1*x1_2-x1_0*x1_3,x1_1^2-x1_0*x1_2,x0_1^2-x0_0*x0_2);",
"ideal X",
"K = ZZ/65521;",
"X' = X ** K;",
"ideal X'"},
SeeAlso => {(coefficientRing,MultiprojectiveVariety),(symbol **,MultirationalMap,Ring)}}

undocumented {
(expression,MultiprojectiveVariety),
(net,MultiprojectiveVariety),
(point,MultiprojectiveVariety,Boolean),
(top,MultiprojectiveVariety),
(decompose,MultiprojectiveVariety),
(degrees,MultiprojectiveVariety),
(euler,MultiprojectiveVariety,Option),
(expression,MultirationalMap),
(net,MultirationalMap),
(multirationalMap,RationalMap),
(multirationalMap,RationalMap,RationalMap),
(multirationalMap,RationalMap,RationalMap,RationalMap),
(multirationalMap,MultirationalMap),
(multirationalMap,MultirationalMap,MultirationalMap),
(multirationalMap,MultirationalMap,MultirationalMap,MultirationalMap),
(rationalMap,MultiprojectiveVariety),
(rationalMap,MultiprojectiveVariety,List),
(rationalMap,MultiprojectiveVariety,ZZ),
(rationalMap,MultiprojectiveVariety,ZZ,ZZ),
(symbol *,RationalMap,MultirationalMap),
(symbol *,MultirationalMap,RationalMap),
(symbol ^,MultirationalMap,ZZ),
(symbol ==,RationalMap,MultirationalMap),
(symbol ==,MultirationalMap,RationalMap),
(symbol ==,MultirationalMap,ZZ),
(symbol ==,ZZ,MultirationalMap),
(multidegree,MultirationalMap,MultirationalMap),
(multidegree,Nothing,MultirationalMap),
(source,MultirationalMap,MultirationalMap),
(clean,RationalMap),
(inverse,MultirationalMap,Option),
(inverse2,MultirationalMap,Option),
(baseLocus,RationalMap),
(symbol |,MultirationalMap,List),
(symbol ||,MultirationalMap,List),
(symbol |,RationalMap,MultirationalMap),
(symbol |,MultirationalMap,RationalMap),
(symbol |,RationalMap,RationalMap),
(random,ZZ,MultiprojectiveVariety),
(show,RationalMap),
(degreeSequence,RationalMap)}

---------------
---- Tests ----
---------------

TEST ///
K = ZZ/333331, R = K[x_0..x_5];
f = rationalMap for i to 2 list random(1,R);
g = rationalMap for i to 2 list random(1,R);
Phi = multirationalMap {f,g};
assert(degree Phi == 0 and multidegree Phi == {1, 2, 4, 6, 6, 0});
assert (apply(decompose baseLocus Phi,o -> ?ideal o) == {"plane in PP^5","plane in PP^5"});
X = random(3,baseLocus Phi);
assert(? X == "hypersurface in PP^5 defined by a form of degree 3" and ? ideal X == "smooth cubic hypersurface in PP^5" and isSubset(baseLocus Phi,X));
Phi = Phi|X;
assert(image Phi == target Phi and degree Phi == 1 and multidegree Phi == {3, 6, 10, 12, 6});
inverse(Phi,Verify=>true);
assert(Phi * (inverse Phi) == 1 and (inverse Phi) * Phi == 1);
B = baseLocus inverse Phi;
assert(dim B == 2 and degree B == 14 and dim singularLocus B == -1 and degrees B == {({2,1},1),({1,2},1)});
(p1,p2) = graph Phi;
assert((multidegree p1, multidegree p2) == ({141, 63, 25, 9, 3}, {141, 78, 40, 18, 6}));
inverse(p1,Verify=>true);
assert(p1 * (inverse p1) == 1 and (inverse p1) * p1 == 1);
inverse(p2,Verify=>true);
assert(p2 * (inverse p2) == 1 and (inverse p2) * p2 == 1);
assert((multidegree inverse p1, multidegree inverse p2) == (reverse {141, 63, 25, 9, 3}, reverse {141, 78, 40, 18, 6}));
assert((inverse p2) * p1 == inverse Phi and isMorphism p2 and (not isIsomorphism p2));
assert(baseLocus Phi == baseLocus inverse p1);
E = p1^* (baseLocus Phi);
assert((dim E, degree E) == (3,48));
h = first graph p2
assert((degree source h, degree target h) == (771, 141));
///

TEST ///
strForTest := "multi-rational map consisting of 2 rational maps
source variety: threefold in PP^3 x PP^2 x PP^4 cut out by 12 hypersurfaces of multi-degrees (1,1,0)^2 (1,0,1)^7 (0,0,2)^1 (0,1,1)^2 
target variety: threefold in PP^2 x PP^4 cut out by 3 hypersurfaces of multi-degrees (1,1)^2 (0,2)^1 
base locus: empty subscheme of PP^3 x PP^2 x PP^4
dominance: true
multidegree: {66, 46, 31, 20}
degree: 1
degree sequence (map 1/2): [(0,1,0), (0,0,2), (1,0,1), (2,0,0)]
degree sequence (map 2/2): [(0,0,1), (2,0,0)]
coefficient ring: ZZ/300007";

R = ZZ/300007[x_0..x_3];
C3 = ideal(x_2^2-x_1*x_3,x_1*x_2-x_0*x_3,x_1^2-x_0*x_2);
C2 = ideal(x_1^2-x_0*x_2,x_3);
Phi = last graph multirationalMap(rationalMap C3,rationalMap C2);
Phi = multirationalMap(Phi,image Phi);
assert(multidegree(,Phi) == {66, 46, 31, 20} and multidegree Phi == {66, 46, 31, 20})
assert(degree(Phi,Strategy=>"random point") == 1 and degree Phi == 1)
assert(Phi * inverse Phi == 1 and Phi^-1 * Phi == 1)
assert(toString describe Phi == toString strForTest);
Z = {target Phi,
     projectiveVariety ideal(random({1,1},ring ambient target Phi)),
     projectiveVariety ideal(random({1,1},ring ambient target Phi),random({1,1},ring ambient target Phi)),
     projectiveVariety ideal(random({1,1},ring ambient target Phi),random({1,1},ring ambient target Phi),random({1,1},ring ambient target Phi)),
     point target Phi,
     (point target Phi) + (point target Phi) + (point target Phi)}
W = apply(Z,z -> Phi^* z);
assert(apply(W,w -> (dim w,degree w)) == {(3, 66), (2, 46), (1, 31), (0, 20), (0,1), (0,3)})  
assert(W_5 == Phi^** (last Z))
Psi = check multirationalMap({rationalMap matrix(2,first factor Phi),rationalMap matrix(1,last factor Phi)},target Phi);
assert(apply(factor Phi,f -> first degrees ideal matrix f) == {{0, 1, 0}, {0, 0, 1}})
assert(apply(factor Psi,f -> first degrees ideal matrix f) == {{1, 0, 1}, {2, 0, 0}})
assert(Phi == Psi and inverse Phi == inverse Psi and source graph Phi == source graph Psi)
W' = apply(Z,z -> Psi^* z);
assert(W == W')
assert(apply(factor Psi,f -> first degrees ideal matrix f) == {{1, 0, 1}, {2, 0, 0}})
///

TEST ///
ringP4 := ZZ/300007[a..e];
f = rationalMap minors(2,matrix {{a,b,c,d},{b,c,d,e}});
g = rationalMap(minors(2,matrix{{a,b,c},{b,c,d}}) + ideal e);
Phi = multirationalMap {f,g};
Phi = multirationalMap(Phi,image Phi);
Psi = inverse(Phi,Verify=>true);
assert(Phi * Psi == 1 and Psi * Phi == 1)
(F,G) = graph Phi;
F' = inverse(F,Verify=>true);
assert(F * F' == 1 and F' * F == 1 and F * Phi == G and G * Phi^-1 == F and F' * G == Phi)
G' = inverse(G,Verify=>true);
assert(G * G' == 1 and G' * G == 1 and G' * F == Phi^-1)
///

TEST///
Phi = last graph multirationalMap {rationalMap kernel veronese(1,4,ZZ/300007)};
assert(multidegree(,Phi) == multidegree Phi)
degree(Phi,Strategy=>"random point")
R = ZZ/33331[x_0..x_4];
Phi = (last graph multirationalMap {rationalMap {transpose jacobian(-x_2^3+2*x_1*x_2*x_3-x_0*x_3^2-x_1^2*x_4+x_0*x_2*x_4)}})||projectiveVariety ideal(random(2,R));
assert(? source Phi == "threefold in PP^4 x PP^4 cut out by 13 hypersurfaces of multi-degrees (1,1)^3 (0,2)^1 (2,1)^8 (4,0)^1 ")
assert(? target Phi == "hypersurface in PP^4 defined by a form of degree 2")
assert(degree(Phi,Strategy=>"random point") == 2)
assert(degree(Phi,Strategy=>"0-th projective degree") == 2)
assert(degree Phi == 2)
///

TEST ///
K = ZZ/65521;
X = projectiveVariety({2,4,1,3},K);
time f = parametrize X;
assert(f * f^-1 == 1 and f^-1 * f == 1)
R = ring X;
Y = projectiveVariety ideal(random({1,0,0,0},R),random({0,1,0,0},R),random({0,1,0,0},R),random({0,0,0,1},R));
time g = parametrize Y;
assert(g * g^-1 == 1 and g^-1 * g == 1)
Z = projectiveVariety ideal(random({1,1,0,0},R),random({0,1,0,0},R),random({0,0,1,0},R),random({0,0,0,1},R),random({0,0,0,1},R));
time h = parametrize Z
assert(h * h^-1 == 1 and h^-1 * h == 1)
///

TEST ///
z = gens ring projectiveVariety({3},ZZ/41)
phi = multirationalMap rationalMap toMap minors(3,matrix{{-z_1,z_0,-z_1^2+z_0*z_3},{z_0,z_1,z_0^2-z_1*z_2},{0,z_2,z_0*z_1-z_1*z_3},{0,z_3,-z_0*z_1+z_0*z_2}})
time G := source graph phi;
time Gs := source graph(clean phi,BlowUpStrategy=>"Syzygies");
time Gk := source graph(clean phi,BlowUpStrategy=>"Koszul");
assert(G == Gs and Gs == Gk);
psi = inverse phi;
assert(phi * psi == 1 and psi * phi == 1);

phi' = phi|multirationalMap(source phi);
phi' = multirationalMap(phi',image phi');
time G' := source graph phi';
time Gs' := source graph(clean phi',BlowUpStrategy=>"Syzygies");
time Gk' := source graph(clean phi',BlowUpStrategy=>"Koszul");
assert(G' == Gs' and Gs' == Gk');
psi' = inverse phi';
assert(phi' * psi' == 1 and psi' * phi' == 1);

phi'' = last graph phi;
time G'' := source graph phi'';
time Gs'' := source graph(clean phi'',BlowUpStrategy=>"Syzygies");
time Gk'' := source graph(clean phi'',BlowUpStrategy=>"Koszul");
assert(G'' == Gs'' and Gs'' == Gk'');
psi'' = inverse phi'';
assert(phi'' * psi'' == 1 and psi'' * phi'' == 1);
///

