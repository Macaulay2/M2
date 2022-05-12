
-*
   Copyright 2020, Giovanni Staglianò.

   You may redistribute this file under the terms of the GNU General Public
   License as published by the Free Software Foundation, either version 2 of
   the License, or any later version.
*-

if version#"VERSION" < "1.18" then error "this package requires Macaulay2 version 1.18 or newer";

newPackage(
    "MultiprojectiveVarieties",
    Version => "2.5", 
    Date => "November 6, 2021",
    Authors => {{Name => "Giovanni Staglianò", Email => "giovannistagliano@gmail.com"}},
    Headline => "multi-projective varieties and multi-rational maps",
    Keywords => {"Projective Algebraic Geometry"},
    PackageImports => {"PrimaryDecomposition","TangentCone"},
    PackageExports => {"Cremona","SparseResultants"},
    DebuggingMode => false,
    Reload => false,
    Certification => {
	 "journal name" => "The Journal of Software for Algebra and Geometry",
	 "journal URI" => "http://j-sag.org/",
	 "article title" => "Computations with rational maps between multi-projective varieties",
	 "acceptance date" => "31 August 2021",
	 "published article URI" => "https://msp.org/jsag/2021/11-1/p14.xhtml",
	 "published article DOI" => "10.2140/jsag.2021.11.143",
	 "published code URI" => "https://msp.org/jsag/2021/11-1/jsag-v11-n1-x14-MultiprojectiveVarieties.m2",
	 "repository code URI" => "http://github.com/Macaulay2/M2/blob/master/M2/Macaulay2/packages/MultiprojectiveVarieties.m2",
	 "release at publication" => "5831dc6b020fae7365f257256b92539d5d496954",	    -- git commit number in hex
	 "version at publication" => "2.3",
	 "volume number" => "11",
	 "volume URI" => "https://msp.org/jsag/2021/11-1/"
	 }
    )

if Cremona.Options.Version < "5.1" then (
    <<endl<<"Your version of the Cremona package is outdated (required version 5.1 or newer);"<<endl;
    <<"you can manually download the latest version from"<<endl;
    <<"https://github.com/Macaulay2/M2/tree/master/M2/Macaulay2/packages."<<endl;
    <<"To automatically download the latest version of Cremona in your current directory,"<<endl;
    <<"you may run the following Macaulay2 code:"<<endl<<"***"<<endl<<endl;
    <<///(makeDirectory("Cremona"), for f in {"Cremona.m2","Cremona/documentation.m2","Cremona/examples.m2","Cremona/tests.m2"} do run("curl -s -o "|f|" https://raw.githubusercontent.com/Macaulay2/M2/master/M2/Macaulay2/packages/"|f));///<<endl<<endl<<"***"<<endl;
    error "required Cremona package version 5.1 or newer";
);

if SparseResultants.Options.Version < "1.1" then error "your version of the SparseResultants package is outdated (required version 1.1 or newer); you can download the latest version from https://github.com/Macaulay2/M2/tree/master/M2/Macaulay2/packages";

export{"MultiprojectiveVariety", "projectiveVariety", "Saturate", "projections", "fiberProduct", 
       "EmbeddedProjectiveVariety", "linearlyNormalEmbedding", "linearSpan", "tangentSpace", "coneOfLines", "sectionalGenus",
       "MultirationalMap", "multirationalMap", "baseLocus", "degreeSequence", "inverse2", "toRationalMap",
       "∏","⋂","⋃","PP",
       "ambientVariety",
       "GrassmannianVariety", "GG", "schubertCycle", "cycleClass"}

debug Cremona;
debug SparseResultants;

importFrom("Resultants",{"fanoVariety","varietySweptOutByLinearSpaces"}); 

MultiprojectiveVariety = new Type of MutableHashTable;

globalAssignment MultiprojectiveVariety;

MultiprojectiveVariety.synonym = "multi-projective variety";

EmbeddedProjectiveVariety = new Type of MultiprojectiveVariety;

globalAssignment EmbeddedProjectiveVariety;

EmbeddedProjectiveVariety.synonym = "embedded projective variety";

projectiveVariety = method(TypicalValue => MultiprojectiveVariety, Options => {MinimalGenerators => true, Saturate => true});

projectiveVariety Ideal := o -> I -> (
    if I.cache#?GrassmannianVariety then return I.cache#GrassmannianVariety;
    if I.cache#?"multiprojectiveVariety" then return I.cache#"multiprojectiveVariety";
    R := ring I;
    if not isPolynomialRing R then error "expected an ideal in a polynomial ring";
    if not isField coefficientRing R then error "the coefficient ring needs to be a field";
    m := multigens R;
    if flatten m != gens R then error "the given grading on the polynomial ring is not allowed: the degree of each variable must be a standard basis vector of ZZ^r in the commonly used order";
    if not isHomogeneous I then error "expected a (multi)-homogeneous ideal";
    J := I;
    if o.Saturate then (
        if not(J.cache#?"isMultisaturated" and J.cache#"isMultisaturated") then (
            for x in m do J = saturate(J,ideal x,MinimalGenerators=>o.MinimalGenerators); 
            J.cache#"isMultisaturated" = true;
        );
    ) else if o.MinimalGenerators then J = trim J;
    if J === I then J = I; 
    if o.Saturate and (not I.cache#?"isMultisaturated") then I.cache#"isMultisaturated" = if I === J then true else I == J;
    X := new MultiprojectiveVariety from {
        symbol cache => new CacheTable,
        "idealVariety" => J,
        "ringVariety" => null,
        "dimVariety" => null,        
        "dimAmbientSpaces" => apply(m, n -> (#n)-1),
        "multigens" => m,
        "multidegree" => null,
        "projections" => null,
        "expression" => null
    };
    if # X#"dimAmbientSpaces" == 1 then X = new EmbeddedProjectiveVariety from X;
    J.cache#"multiprojectiveVariety" = X;
    I.cache#"multiprojectiveVariety" = J.cache#"multiprojectiveVariety"
);

projectiveVariety Ring := o -> R -> (
    if R#?GrassmannianVariety then return R#GrassmannianVariety;
    if R#?"multiprojectiveVariety" then return R#"multiprojectiveVariety";
    I := ideal R;
    if not isPolynomialRing ambient R then error "expected the ambient ring to be polynomial";
    if o.Saturate or I.cache#?"isMultisaturated" then if not isMultisaturated I then error "the ideal is not multi-saturated";
    if I.cache#?"multiprojectiveVariety" then (
        X := I.cache#"multiprojectiveVariety";
        if X#"ringVariety" === null then X#"ringVariety" = R;
        if X#"ringVariety" === R then return (R#"multiprojectiveVariety" = X);
    );
    Y := projectiveVariety(I,MinimalGenerators=>false,Saturate=>false);
    if Y#"ringVariety" =!= null then error "internal error encountered: double assignment for ring of projective variety";
    Y#"ringVariety" = R;
    R#"multiprojectiveVariety" = Y
);

isMultisaturated = (cacheValue "isMultisaturated") (I -> I == multisaturate I);

projectiveVariety MultidimensionalMatrix := o -> A -> projectiveVariety(ideal(A!),MinimalGenerators=>true,Saturate=>false);

projectiveVariety (List,Ring) := o -> (l,K) -> (
    if not all(l,i -> instance(i,ZZ) and i >= 0) then error "expected a list of non-negative integers"; 
    if not isField K then error "expected a field";
    if #l == 0 then return projectiveVariety(K[],MinimalGenerators=>false,Saturate=>false);
    X := projectiveVariety(ring first first gensRing(K,apply(l,i -> i+1)),MinimalGenerators=>false,Saturate=>false);
    X.cache#"euler" = product apply(l,i -> i+1);
    X.cache#"top" = X;
    X.cache#"singularLocus" = 0_X;
    X#"expression" = expression expressionVar(sum l,l);
    return X;
);
projectiveVariety (ZZ,Ring) := o -> (l,K) -> projectiveVariety({l},K);

projectiveVariety (List,List,Ring) := o -> (n,d,K) -> (
    if #n != #d then error "expected two lists of the same length";
    if not all(n|d,i->instance(i,ZZ) and i >= 0) then error "expected two lists of nonnegative integers";
    if K#?(n,d,"SegreVeroneseVariety") then return K#(n,d,"SegreVeroneseVariety");
    P := projectiveVariety(n,K);
    f := multirationalMap apply(#d, i -> rationalMap gens image basis(toList(i : 0) | {d_i} | toList(#d - i - 1 : 0),ring P));
    f = multirationalMap(f,image f);
    if f#"isDominant" =!= true then error "internal error encountered";    
    f#"isBirational" = true;
    X := image f;
    X.cache#"euler" = product apply(n,i -> i+1);
    X.cache#"top" = X;
    X.cache#"singularLocus" = 0_X;
    X.cache#"rationalParametrization" = (parametrize source f) * f;
    K#(n,d,"SegreVeroneseVariety") = X
);
projectiveVariety (ZZ,ZZ,Ring) := o -> (n,d,K) -> projectiveVariety({n},{d},K);

----------------------------------
higherSecantVarietyToRationalNormalScroll = method(); -- see p. 167 de [Hubert Flenner, Liam O’Carroll, Wolfgang Vogel]
higherSecantVarietyToRationalNormalScroll (Array,ZZ,Ring) := (d,k,K) -> (
    if not (#d > 0 and all(d,i -> instance(i,ZZ))) then error "expected an array of integers";
    d = toList d;
    N := sum d + #d -1;
    if k <= 0 or min d < 0 then return 0_(projectiveVariety(N,K));
    if k+1 > sum(#d,s -> d_s-k+1) then return projectiveVariety(N,K);
    t := gensRing(K,apply(d,i -> i+1));
    M := sub(fold((x,y)->x|y,apply(#d,s -> matrix for i to k list for j to d_s-k list t_s_(i+j))),vars ring projectiveVariety(N,K));
    X := projectiveVariety(ideal apply(subsets(numColumns M,k+1),m -> det submatrix(M,m)),MinimalGenerators=>false,Saturate=>false);
    X#(symbol matrix) = M;
    X
);
PP = new ScriptedFunctor from {
    symbol ring => null,
    argument => (
        A -> (
            if instance(A,Ring) then (if not isField A then error "expected a field" else (PP.ring = A; <<"-- the ring "<<toString(PP.ring)<<" is set to be the default coefficient ring; now PP may be used as an abbreviation for PP_"<<toString(PP.ring)<<endl; return PP));
            if PP.ring === null then error "coefficient ring required: you may set a ring K as default coefficient ring using PP(K)";
            return PP_(PP.ring) A;  
        )
    ),
    superscript => (
        B -> (
            if PP.ring === null then error "coefficient ring required: you may set a ring K as default coefficient ring using PP(K)";
            return PP_(PP.ring)^B;
        )
    ),
    subscript => (
        K -> (
            if not (instance(K,Ring) and isField K) then error "expected a field";
            if PP.ring === null then PP.ring = K;
            errStr := toString(///These are some ways of using PP:///||///PP^n -> n-dimensional projective space///||///PP^{n1,n2,...} -> product of projective spaces: PP^n1 x PP^n2 x ...///||///PP^(n,d) -> d-uple embedding of PP^n: v_d(PP^n)///||///PP^({n1,n2,...},{d1,d2,...}) -> Segre-Veronese variety: v_d1(PP^n1) x v_d2(PP^n2) x ...///||///PP[a1,a2,...] -> rational normal scroll: P(O(a1))+P(O(a2))+...///||///PP([a1,a2,...],k) -> k-th secant variety of PP[a1,a2,...]///||///(PP([a1,a2,...],k)).matrix -> the matrix from which PP([a1,a2,...],k) is constructed///);
            new ScriptedFunctor from {
                superscript => (
                    l -> (
                        if instance(l,List) or instance(l,ZZ) then projectiveVariety(l,K)
                        else if instance(l,Sequence) and #l==2 then projectiveVariety(l_0,l_1,K)
                        else error errStr
                    )
                ),
                argument => (
                    d -> (
                        if instance(d,Array) then higherSecantVarietyToRationalNormalScroll(d,1,K)
                        else if instance(d,Sequence) and #d==2 then higherSecantVarietyToRationalNormalScroll(d_0,d_1,K)
                        else error errStr
                    )
                )
            }
        )
    )
};
----------------------------------

isPoint = (cacheValue "isPoint") (X -> (
    n := X#"dimAmbientSpaces";
    dim X == 0 and sort degrees X == sort pairs tally deepSplice apply(n,entries diagonalMatrix toList(#n:1),(i,d) -> i:d)
));

isGrass = (cacheValue "isGrass") (X -> (
    if instance(X,GrassmannianVariety) then return true;
    try (k,n,K,Vp) := Grass ring X then (
        if isPolynomialRing ring X and k != 0 and k != n-1 then return false;
        X.cache#"top" = X; 
        X.cache#"singularLocus" = 0_X;
        X.cache#"GrassInfo" = (k,projectiveVariety(Grass(0,n,K,Vp),Saturate=>false));
        return true;
    ) else return false;
));

expression MultiprojectiveVariety := X -> (
    if X#"expression" =!= null then return X#"expression";
    n := X#"dimAmbientSpaces";
    if dim X == 0 and codim X > 0 then if isPoint X then return expression("a point in "|expressionVar(sum n,n));
    expression expressionVar(dim X,n)
);

net MultiprojectiveVariety := X -> if hasAttribute(X,ReverseDictionary) then toString getAttribute(X,ReverseDictionary) else ?X;

MultiprojectiveVariety#{Standard,AfterPrint} = MultiprojectiveVariety#{Standard,AfterNoPrint} = X -> (
    << endl << concatenate(interpreterDepth:"o") << lineNumber << " : " << "ProjectiveVariety, " << expression X;
    if isSubvariety X then << " (subvariety of codimension " << dim ambientVariety X - dim X << " in " << ambientVariety X << ")";
    << endl;
);

toString MultiprojectiveVariety := X -> if codim X == 0 then "PP_("|(toString coefficientRing X)|")^"|(toString shape X) else "projectiveVariety("|(toString ring X)|")"; -- this doesn't work well

ideal MultiprojectiveVariety := X -> X#"idealVariety";

ring MultiprojectiveVariety := X -> (
    if X#"ringVariety" =!= null then return X#"ringVariety";
    I := ideal X;
    R := (ring I)/I;
    try assert(isPolynomialRing R or I.cache.QuotientRing === R) else error "internal error encountered";
    X#"ringVariety" = R
);

coefficientRing MultiprojectiveVariety := X -> coefficientRing ring ideal X;

ambient MultiprojectiveVariety := (cacheValue "ambient") (X -> if ideal X == 0 then X else projectiveVariety ring ideal X);

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

segre MultiprojectiveVariety := (cacheValue "SegreMap") (X -> segre ring X);

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
    if isPoint X then return ("point of coordinates "|toString coordinates X); 
    n := X#"dimAmbientSpaces";
    degs := degrees ideal X; 
    m := "multi-";
    if #n == 1 then m = "";
    if # degs == 1 then return(toString expressionVar(dim X,n)|" defined by a "|m|"form of "|m|"degree "|toString(unsequence toSequence first degs));
    cutOut:=""; if #degs>1 then cutOut = if # unique degs == 1 then " cut out by "|toString(#degs)|" hypersurfaces of "|m|"degree "|toString(unsequence toSequence first degs) else " cut out by "|toString(#degs)|" hypersurfaces of "|m|"degrees "|toStringDegreesVar(X); 
    (expressionVar(dim X,n))|cutOut
);

degrees MultiprojectiveVariety := (cacheValue "degreesGensIdeal") (X -> sort pairs tally degrees ideal X);

shape MultiprojectiveVariety := X -> X#"dimAmbientSpaces";

singularLocus MultiprojectiveVariety := (cacheValue "singularLocus") (X -> (
    if X.cache#?"top" then if X != top X then error "expected an equidimensional projective variety";
    if instance(X,EmbeddedProjectiveVariety) and X.cache#?"nonSaturatedSingularLocus" then return projectiveVariety(saturate ideal singularLocus(X,Saturate=>false),MinimalGenerators=>false,Saturate=>false);
    I := ideal X;
    projectiveVariety(I + minors(codim X,jacobian I,Strategy=>Cofactor),MinimalGenerators=>true,Saturate=>true)
));

singularLocus (EmbeddedProjectiveVariety,Option) := (X,opt) -> (
    if first toList opt =!= Saturate then error "Saturate is the only available option for singularLocus(EmbeddedProjectiveVariety)";
    if (last opt) or X.cache#?"singularLocus" then return singularLocus X;        
    if X.cache#?"nonSaturatedSingularLocus" then return X.cache#"nonSaturatedSingularLocus";
    if X.cache#?"top" then if X != top X then error "expected an equidimensional projective variety";
    I := ideal X;
    X.cache#"nonSaturatedSingularLocus" = projectiveVariety(I + minors(codim X,jacobian I,Strategy=>Cofactor),MinimalGenerators=>true,Saturate=>false)
);

top MultiprojectiveVariety := (cacheValue "top") (X -> (
    T := top ideal X;
    if T == ideal X then X else projectiveVariety(T,MinimalGenerators=>true,Saturate=>false)
));

decompose MultiprojectiveVariety := {} >> o -> X -> (
    if X.cache#?"Decomposition" then return X.cache#"Decomposition";
    X.cache#"Decomposition" = apply(decompose ideal X,D -> projectiveVariety(D,MinimalGenerators=>true,Saturate=>false))
);

support MultiprojectiveVariety := (cacheValue "Support") (X -> (
    I := radical ideal X;
    if I === ideal X then return X;
    projectiveVariety(I,Saturate=>false)
));

MultiprojectiveVariety == MultiprojectiveVariety := (X,Y) -> (
    if ring ideal X =!= ring ideal Y then error "expected varieties in the same ambient space";
    if X === Y or ideal X === ideal Y then return true;
    if dim X != dim Y then return false;
    ideal X == ideal Y
);

SchubertCycle22 = X -> (
    if instance(X,GrassmannianVariety) then (
        k := X#"dimLinearSpaces";
        a := {2,2}|toList(k-1:0);
        return schubertCycle(a,X,Standard=>true);
    );
    makeSubvariety(SchubertCycle22(GG X),X,Verify=>true)
);

SchubertCycle22OnLinearSectionOfG14 = X -> (
    if dim X == 6 then (
        J := parametrize random(1,0_X);
        return J SchubertCycle22OnLinearSectionOfG14(J^^ X);
    );
    if dim X == 5 then (
        p := pointOnLinearSectionOfG14 X;
        V := coneOfLines(X,p);
        j := parametrize linearSpan V;
        h := (rationalMap(j^^ p))|(j^^ V);
        return j h^* dual top singularLocus(projectiveVariety(dualVariety ideal image h,MinimalGenerators=>false,Saturate=>false),Saturate=>false);
    );
    if dim X == 4 then ( -- Todd's result: a quintic del Pezzo fourfold contains exactly one rho-plane (Roth, "Algebraic varieties with canonical curve section", p. 95)
        planes := Fano(2,X); Y := null;
        if not(dim planes == 1 and degree planes == 5 and genera ideal planes == {-1,4}) then (
            <<"-- re-executing Fano(2,...,AffineChartGrass=>true)"<<endl;
            f := rationalMap {for i to 7 list random(1,ring ambient X)};
            Y = f^^ X;
            planes = Fano(2,Y);
            if not(dim planes == 1 and degree planes == 5 and genera ideal planes == {-1,4}) then error "error occurred trying to pick rho-plane in del Pezzo fourfold";
        );
        l := parametrize linearSpan planes;
        P := Fano(l((l^^planes)\top(l^^ planes)) % ambientVariety planes);
        if Y =!= null then P = (inverse f)^^ P;
        if not (dim P == 2 and degree P == 1 and isSubset(P,X)) then "error occurred trying to pick rho-plane in del Pezzo fourfold";
        return P;
    );
    error "expected dimension of quintic del Pezzo variety to be 4, 5, or 6";
);

parametrize MultiprojectiveVariety := (cacheValue "rationalParametrization") (X -> (
    inv := if X#?InverseMethod then X#InverseMethod else inverse;
    if dim X == -1 then error "expected a non-empty variety";
    if X.cache#?"top" then if X != top X then error "expected an equidimensional variety";
    if # X#"dimAmbientSpaces" != 1 then (
        f := parametrizeWithAnEmbeddedProjectiveVariety X;
        return (parametrize source f) * f;
    );
    -- linear varieties
    if codim X == 0 then return 1_X;
    if degree X == 1 then (
        N := mingens kernel transpose sub(last coefficients(gens ideal X,Monomials=>gens ring ambient X),coefficientRing X);
        R := ring projectiveVariety(dim X,coefficientRing X);
        return multirationalMap rationalMap map(R,ring X,(vars R) * (transpose N));
    );
    -- zero-dimensional varieties (hidden to the user)
    if dim X == 0 then return inv multirationalMap rationalMap(sub(matrix{{random(1,ring ambient X),random(1,ring ambient X)}},ring X),Dominant=>true);   
    -- Grassmannians
    if instance(X,GrassmannianVariety) then return inv(rationalMap SchubertCycle22 X,Verify=>-1);
    if isGrass X then return inv((rationalMap SchubertCycle22 X)|X,Verify=>-1);
    -- quadrics
    if degree X == 2 then return inv(multirationalMap rationalMap(trim sub(ideal point X,ring X),1),Verify=>-1);
    -- linear span
    if codim linearSpan X > 0 then (g := (parametrize linearSpan X)||X; return (parametrize source g) * g);
    -- Severi varieties (in particular, varieties projectively equivalent to G(1,5))
    if ((dim X == 2 and dim ambient X == 5 and degree X == 4) or 
        (dim X == 4 and dim ambient X == 8 and degree X == 6) or 
        (dim X == 8 and dim ambient X == 14 and degree X == 14) or 
        (dim X == 16 and dim ambient X == 26 and degree X == 78)) and
       degrees X == {({2},dim ambient X +1)}
    then return inv(multirationalMap rationalMap(trim sub((ideal X) + secantCone(toList coordinates point linearSpan {point X,point X},ideal X),ring X),1),Verify=>-1);
    -- cubic scrolls (this makes the function "===>" work with del Pezzo fivefolds and del Pezzo sixfolds in every characteristic)
    if codim X == 2 and degree X == 3 and sectionalGenus X == 0 then (
        if dim X == 2 then (
            dirLine := dual top singularLocus(projectiveVariety(dualVariety ideal X,MinimalGenerators=>false,Saturate=>false),Saturate=>false);
            rulLine := (X * tangentSpace(X,point dirLine))\dirLine;
            hX2 := inv(multirationalMap rationalMap sub(ideal rulLine,ring X),Verify=>-1);
            return sendFewPoints(projectiveVariety ideal submatrix(vars ring source hX2,{0,1}),baseLocus hX2) * hX2;
        );
        if dim X == 3 then (
            hX3 := multirationalMap({segre projectiveVariety({2,1},coefficientRing X)},ambient X) * (inv rationalMap flatten entries syz gens ideal X);
            return check multirationalMap((parametrize projectiveVariety({2,1},coefficientRing X)) * hX3,X);
        );
    );
    -- minimal degree varieties
    if degree X == codim X + 1 and sectionalGenus X == 0 
    then return inv(multirationalMap rationalMap trim sub(ideal linearSpan apply(degree X -1,i -> point X),ring X),Verify=>-1);
    -- del Pezzo fourfolds, fivefolds, and sixfolds
    if (dim X == 4 or dim X == 5 or dim X == 6) and codim X == 3 and degree X == 5 and sectionalGenus X == 1
    then return inv(multirationalMap rationalMap sub(ideal SchubertCycle22OnLinearSectionOfG14 X,ring X),Verify=>-1);
    -- complete intersections of two quadrics
    if codim X == 2 and degree X == 4 and sectionalGenus X == 1
    then return inv(multirationalMap rationalMap(sub(ideal line X,ring X),1),Verify=>-1);
    -- some special Fano fourfolds
    if dim X == 4 and codim X == 5 and degree X == 12 and sectionalGenus X == 7
    then return inv(multirationalMap rationalMap(sub(ideal tangentSpace(X,point X),ring X),1),Verify=>-1);
    if dim X == 4 and codim X == 6 and degree X == 14 and sectionalGenus X == 8
    then return inv(multirationalMap rationalMap(sub(ideal (point X + tangentSpace(X,point X)),ring X),1),Verify=>-1);
    if dim X == 4 and codim X == 7 and degree X == 16 and sectionalGenus X == 9
    then return inv(multirationalMap rationalMap(sub(ideal (line X + tangentSpace(X,point X)),ring X),1),Verify=>-1);
    if dim X == 4 and codim X == 8 and degree X == 18 and sectionalGenus X == 10 then (
        Conic := {}; t := 0;
        while #Conic <= 1 and t <= 14 do (
            Conic = select(decompose coneOfLines(X,point X),l -> dim l == 1 and degree l == 1);
            t = t+1;        
        );
        if #Conic <= 1 then error "failed to find reducible conic on fourfold of genus 10 (15 attempts performed); try executing again";
        Conic = Conic_0 + Conic_1;
        return inv(multirationalMap rationalMap(sub(ideal (Conic + tangentSpace(X,point X)),ring X),1),Verify=>-1);
    );
    error("not (yet) able to parametrize "|toString(? X)|" defined over "|toString(coefficientRing X));
));

parametrizeWithAnEmbeddedProjectiveVariety = (cacheValue "parameterizedWithAnEmbeddedProjectiveVariety") (X -> (  
    local G;
    if # X#"dimAmbientSpaces" == 1 
    then G = 1_X
    else (
        t := local t;
        g := parametrizeProductOfProjectiveSpaces(ring ambient X,t);
        G = (multirationalMap(apply(projections ambient X,p -> rationalMap(g * (map p),Dominant=>"notSimplify")),ambient X))||X;
    );
    degs := degrees ideal source G;
    if (#degs>0 and all(degs,d -> d == {1})) then G = (parametrize ring source G) * G;
    G
));

point (MultiprojectiveVariety,Boolean) := (X,b) -> (
    if # X#"dimAmbientSpaces" == 1 and 
       (not X.cache#?"rationalParametrization") and 
       (not X.cache#?"parameterizedWithAnEmbeddedProjectiveVariety") and 
       (codim X == 0 or any(degrees ideal X,d -> d != {1})) 
    then return projectiveVariety(point(ideal X,b),MinimalGenerators=>false,Saturate=>false);
    f := if X.cache#?"rationalParametrization" 
         then X.cache#"rationalParametrization"
         else parametrizeWithAnEmbeddedProjectiveVariety X;
    p := f projectiveVariety(point(ideal source f,false),MinimalGenerators=>false,Saturate=>false);
    if b then if not (isPoint p and isSubset(p,X)) then error("something went wrong in trying to pick a random "|toString(coefficientRing X)|"-rational point on the variety");
    return p;
);
point MultiprojectiveVariety := X -> (
    p := point(X,true);
    if isSubvariety X then p = makeSubvariety(p,ambientVariety X,Verify=>false);
    return p;
);

pointOnLinearSectionOfG14 = X -> (
   j := parametrize projectiveVariety(ideal apply(dim X -2,i -> random(1,ring ambient X)),MinimalGenerators=>true,Saturate=>false);
   S := j^^ X;
   T := random({{2},{2},{2}},S) \ S;
   i := parametrize linearSpan T;
   L := i dual top singularLocus(projectiveVariety(dualVariety ideal(i^^ T),MinimalGenerators=>false,Saturate=>false),Saturate=>false);
   j(L * S)
);

coordinates = (cacheValue "coordinates") (p -> (
    if not isPoint p then error "expected a point";
    unsequence toSequence apply(projections p,h -> new Array from flatten entries coefficients parametrize image h)
));

|- MultiprojectiveVariety := X -> coordinates X;

MultiprojectiveVariety ** MultiprojectiveVariety := (X,Y) -> productVars(X,Y);

∏ = method();
∏ List := L -> productVars L;

quotientRingMem = memoize(I -> (ring I)/I); -- this makes the product strict associative

productMem = memoize(L -> (
    if not (#L > 0 and all(L,X -> instance(X,MultiprojectiveVariety))) then error "expected a list of multi-projective varieties";
    if #L == 1 then return first L;
    K := coefficientRing first L;
    for i from 1 to #L-1 do if K =!= coefficientRing(L_i) then error "different coefficient rings encountered";
    n := toSequence apply(L,X -> apply(X#"dimAmbientSpaces",i->i+1));
    R := ring first first gensRing(K,join n);
    j := for i to #L list sum toList join take(n,i);
    s := for i to #L-1 list map(R,ring ideal L_i,submatrix(vars R,j_i .. j_(i+1)-1));
    W := projectiveVariety(quotientRingMem trim sum(#L,i -> s_i ideal L_i),MinimalGenerators=>false,Saturate=>false);
    W#"projections" = apply(projections W,apply(join toSequence apply(L,projections),target),(f,T) -> rationalMap((map f) * (map rationalMap(target f,T)),Dominant=>"notSimplify"));
    if all(L,X -> X.cache#?"euler") then W.cache#"euler" = product(L,euler);
    W
));

productVars = L -> (
    W := productMem L;
    if not (#L > 1 and all(L,X -> hasAttribute(X,ReverseDictionary) or X#"expression" =!= null)) then return W;
    e := apply(L,X -> if hasAttribute(X,ReverseDictionary) then toString getAttribute(X,ReverseDictionary) else toString X#"expression");
    W#"expression" = e_0;
    for i from 1 to #L-1 do W#"expression" = W#"expression" | " x " | e_i;
    W#"expression" = expression W#"expression";
    W
);

MultiprojectiveVariety ^ ZZ := (X,n) -> (
    if n < 0 then error "expected a nonnegative integer";
    if n == 0 then return projectiveVariety((coefficientRing X)[],Saturate=>false);
    productVars toList(n : X)
);

ZZ * MultiprojectiveVariety := (n,X) -> (
    if n < 0 then error "expected a nonnegative integer";
    makeSubvariety(projectiveVariety gens saturate (idealOfSubvariety X)^n,ambientVariety X)
);

MultiprojectiveVariety + MultiprojectiveVariety := (X,Y) -> ⋃ {X,Y};

⋃ = method();
⋃ List := L -> (
    if not(#L>0 and all(L,X -> instance(X,MultiprojectiveVariety))) then error "expected a list of multi-projective varieties"; 
    if #L == 1 then return first L;
    if not (first L).cache#?("union",L) then (
        if not same apply(L,ambient) then error "expected varieties in the same ambient multi-projective space";
        (first L).cache#("union",L) = projectiveVariety(intersect apply(L,ideal),MinimalGenerators=>true,Saturate=>false);
    );
    makeSubvariety((first L).cache#("union",L),L)
);

MultiprojectiveVariety \ MultiprojectiveVariety := (X,Y) -> (
    if X.cache#?("difference1",Y) then return X.cache#("difference1",Y);
    if ring ideal X =!= ring ideal Y then error "expected varieties in the same ambient multi-projective space";
    X.cache#("difference1",Y) = projectiveVariety(quotient(ideal X,ideal Y,MinimalGenerators=>true),MinimalGenerators=>false,Saturate=>false)
);

MultiprojectiveVariety \\ MultiprojectiveVariety := (X,Y) -> (
    if X.cache#?("difference2",Y) then return X.cache#("difference2",Y);
    if ring ideal X =!= ring ideal Y then error "expected varieties in the same ambient multi-projective space";
    X.cache#("difference2",Y) = projectiveVariety(saturate(ideal X,ideal Y,MinimalGenerators=>true),MinimalGenerators=>false,Saturate=>false)
);

MultiprojectiveVariety * MultiprojectiveVariety := (X,Y) -> ⋂ {X,Y};

⋂ = method();
⋂ List := L -> (
    if not(#L>0 and all(L,X -> instance(X,MultiprojectiveVariety))) then error "expected a list of multi-projective varieties"; 
    if #L == 1 then return first L;
    if not (first L).cache#?("intersection",L) then (
        if not same apply(L,ambient) then error "expected varieties in the same ambient multi-projective space";
        (first L).cache#("intersection",L) = projectiveVariety(sum apply(L,ideal),MinimalGenerators=>true,Saturate=>true);
    );
    makeSubvariety((first L).cache#("intersection",L),L)
);

isSubset (MultiprojectiveVariety,MultiprojectiveVariety) := (X,Y) -> (
    if (ideal X).cache#?("isSubsetAsVarietyOf",ideal Y) then return (ideal X).cache#("isSubsetAsVarietyOf",ideal Y);
    if ring ideal X =!= ring ideal Y then error "expected varieties in the same ambient multi-projective space";
    if X === Y then return (ideal X).cache#("isSubsetAsVarietyOf",ideal Y) = true;
    (ideal X).cache#("isSubsetAsVarietyOf",ideal Y) = isSubset(ideal Y,ideal X)
);

fiberProductInt = (phi,psi) -> (
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
    if X.cache#?"euler" then return X.cache#"euler";
    local e;
    if # X#"dimAmbientSpaces" == 1 then (
        if codim X == 0 then return X.cache#"euler" = numgens ring ideal X;
        e = EulerCharacteristic(ideal X,MathMode=>last o,Verbose=>false);
     ) else (
        -- <<"--warning: code to be improved"<<endl;
        e = EulerCharacteristic(image segre X,MathMode=>last o,Verbose=>false);
    );
    if last o then X.cache#"euler" = e;
    return e;
);

euler MultiprojectiveVariety := X -> euler(X,Verify=>true);

basisMem = (d,X) -> (
    if X.cache#?(d,"basis") then return X.cache#(d,"basis");
    J := ideal select(flatten entries gens ideal X,g -> all(degree g,d,(i,j) -> i<=j));
    if numgens J == 0 then J = sub(J,ring ideal X);
    X.cache#(d,"basis") = flatten entries gens image basis(d,J)
);

random (List,MultiprojectiveVariety) := o -> (l,X) -> (
    l = deepSplice l;
    K := coefficientRing X;
    n := # X#"dimAmbientSpaces";
    if #l == n and all(l,j -> instance(j,ZZ)) then return random({l},X);
    L := pairs tally l;
    if not all(L,i -> instance(first i,List) and # first i == n and all(first i,j -> instance(j,ZZ))) then error("expected lists of integers of length "|toString(n)); 
    local B;
    Y := projectiveVariety ideal flatten for d in L list (
        B = basisMem(first d,X);
        if #B == 0 then error("unable to find random elements of degree "|(toString first d));
        for i from 1 to last d list sum(B,b -> (random K) * b)
    );
    if codim Y != #l then error "unable to find random elements, too many multi-degrees are given";
    return Y;
);

random (ZZ,MultiprojectiveVariety) := o -> (i,X) -> random({i},X);

random MultiprojectiveVariety := o -> X -> (
    P := ambient X;
    if P == X then return X;
    Phi := rationalMap(apply(entries diagonalMatrix toList(# shape P : 1),shape P,(d,i) -> apply(i+1,j -> random(d,ring P))),P);
    -- assert(isIsomorphism Phi);
    if # shape P == 1 then Phi^^ X else Phi X
);

MultiprojectiveVariety ** Ring := (X,K) -> (
    if not isField K then (
        if instance(K,QuotientRing) or instance(K,PolynomialRing) then if coefficientRing K === coefficientRing X then return X ** (projectiveVariety K);
        error "expected a field";
    );
    if (char coefficientRing X =!= char K and char coefficientRing X =!= 0) then error "characteristic not valid";
    projectiveVariety(sub(ideal X,vars ring projectiveVariety(shape X,K)),Saturate=>false,MinimalGenerators=>true)
);

MultiprojectiveVariety ? MultiprojectiveVariety := (X,Y) -> (
    if ring ideal X =!= ring ideal Y then return incomparable;
    if X == Y then return symbol ==;
    if isSubset(X,Y) then return symbol <;
    if isSubset(Y,X) then return symbol >;
    return incomparable;
);

variety EmbeddedProjectiveVariety := (cacheValue "ProjOfRing") (X -> Proj ring X);

linearSpan = method();
linearSpan EmbeddedProjectiveVariety := (cacheValue "linearSpan") (X -> (
    L := select(flatten entries gens ideal X,i -> degree i == {1});
    if #L == 0 then return ambient X;
    Y := projectiveVariety(ideal L,MinimalGenerators=>true,Saturate=>false);
    if Y == X then return X else return Y;
));
linearSpan List := L -> (
    if #L == 0 then error "expected a nonempty list";
    if not all(L,X -> instance(X,EmbeddedProjectiveVariety)) then error "expected a list of embedded projective varieties";
    linearSpan ⋃ L
);

sectionalGenus = method();
sectionalGenus EmbeddedProjectiveVariety := (cacheValue "sectionalGenus") (X -> (
    if dim X <= 0 then error "expected a positive dimensional variety";
    if X.cache#?(true,"HilbertPolynomial") then return 1 - euler diff(hilbertPolynomial X,dim X -1);
    (reverse genera ideal X)_1
));

hilbertPolynomial EmbeddedProjectiveVariety := o -> ((cacheValue (o.Projective,"HilbertPolynomial")) (X -> hilbertPolynomial(ideal X,Projective=>o.Projective)));

EmbeddedProjectiveVariety ! := X -> (
    if coefficientRing X === QQ then (
        p := nextPrime random(300,10000000);
        -- <<"*** reduction to char "<< p <<" ***"<<endl;
        return (X ** (ZZ/p))!;
    );
    <<"dim:.................. "<<dim X;<<endl;
    <<"codim:................ "<<codim X<<endl;
    if dim X == -1 then return;
    <<"degree:............... "<<degree X<<endl;
    if codim X == 0 then return;
    if dim X >= 2 then <<"sectional genus:...... "<<sectionalGenus X<<endl;
    if dim X == 1 then <<"genus:................ "<<sectionalGenus X<<endl;
    <<"generators:........... "<<toStringDegreesVar X<<endl;
    d := null;
    if # degrees X == 1 and first first degrees X >= {2} and last first degrees X >= dim ambient X +1 then <<"degree associated map: "<<toString(d = degreeMap rationalMap ideal X)<<endl;
    ln := null;
    if linearSpan X == ambient X then <<"linear normality:..... "<<toString(ln = rank HH^0(OO_(variety X)(1)) == dim ambient X + 1)<<endl;
    nc := null;
    <<"connected components:. "<<toString(nc = rank HH^0(OO_(variety X)))<<endl;
    <<"purity:............... "<<X == top X<<endl; 
    if X == top X then (
        <<"dim sing. l.:......... "<<dim singularLocus X<<endl;
        if dim singularLocus X >= 0 then <<"degree sing. l.:...... "<<degree singularLocus X<<endl;
        if dim singularLocus X >= 0 then <<"gens. sing. l.:....... "<<toStringDegreesVar singularLocus X<<endl;
    ) else return;
    if ln === true and linearSpan X == ambient X and nc === 1 and X == top X and dim singularLocus X == -1 and dim X >= 1 and codim X > 1 and codim X == degree X - 1 then (if codim X == 3 and dim X == 2 and d === 1 then (<<"*** This is the Veronese surface in P^5 ***"<<endl) else (if dim X > 1 then <<"*** This is a rational normal scroll of dimension "<<dim X<<" and degree "<<degree X<<" in PP^"<<first shape X<<" ***"<<endl else <<"*** This is a rational normal curve of degree "<<degree X<<" in PP^"<<first shape X<<" ***"<<endl));
    if # degrees X == 1 and d === 1 then (
        <<"*** This is the base locus of a ";
        if X == top X and dim singularLocus X == -1 and nc === 1 then <<"special ";
        if last first degrees X == first shape X + 1 then <<"Cremona " else <<"birational ";
        <<"transformation of PP^"<<first shape X<<" ***"<<endl;
    );
    if codim X == 1 and X == top X then (
        <<"*** This is a"; 
        if dim singularLocus X == -1 then <<" smooth" else if dim singularLocus X < dim X - 3 then <<" factorial";
        <<" hypersurface of degree "<<degree X<<" in PP^"<<first shape X<<" ***"<<endl;
    );
    if codim X > 1 and X == top X and numgens ideal X == codim X then (
        <<"*** This is a"; 
        if dim singularLocus X == -1 then <<" smooth" else if dim singularLocus X < dim X - 3 then <<" factorial";
        <<" complete intersection of type "<<toString(toSequence flatten degrees ideal X)<<" in PP^"<<first shape X<< " ***"<<endl;
    );
    if ln === true and linearSpan X == ambient X and X == top X and dim singularLocus X == -1 and nc === 1 and codim X == 4 and dim X == 3 and degree X == 6 and d === 0 then (<<"*** This is P^1xP^1xP^1 in P^7 ***"<<endl);
);

dual EmbeddedProjectiveVariety := {} >> o -> X -> (
    if codim linearSpan X > 0 then return projectiveVariety(dualVariety ideal X,MinimalGenerators=>false,Saturate=>false); -- from Resultants
    return projectiveVariety(dualvariety ideal X,MinimalGenerators=>false,Saturate=>false); -- from SparseResultants
);

conormalVariety EmbeddedProjectiveVariety := o -> X -> ( 
    S := o.SingularLocus;
    if instance(S,EmbeddedProjectiveVariety) then S = ideal S;
    if S === null and X.cache#?"singularLocus" then S = ideal singularLocus X;
    idW := conormalVariety(ideal X,Variable=>o.Variable,Strategy=>o.Strategy,SingularLocus=>S);
    W := projectiveVariety(idW,MinimalGenerators=>true,Saturate=>false);
    W#"projections" = apply(projections W,f -> rationalMap((map f) * (map rationalMap(target f,ring ambient X)),Dominant=>"notSimplify"));
    return W;
);

EmbeddedProjectiveVariety ++ EmbeddedProjectiveVariety := (X,Y) -> (
    if ring ideal X =!= ring ideal Y then error "expected varieties in the same ambient projective space";
    K := coefficientRing X;
    n := dim ambient X;
    (t,x,y,z) := (local t,local x,local y,local z);
    R := K[t_0,t_1,x_0..x_n,y_0..y_n,z_0..z_n,MonomialOrder=>Eliminate (2*n+4)];
    sx := map(R,ring ambient X,{x_0..x_n});
    sy := map(R,ring ambient Y,{y_0..y_n});
    W := (sx ideal X) + (sy ideal Y) + ideal(matrix{{z_0..z_n}} - t_0*matrix{{x_0..x_n}} - t_1*matrix{{y_0..y_n}});
    I := sub(sub(ideal selectInSubring(1,gens gb W),K[z_0..z_n]),vars ring ambient X);
    projectiveVariety(I,MinimalGenerators=>true,Saturate=>false)
);

tangentSpace = method();
tangentSpace (EmbeddedProjectiveVariety,EmbeddedProjectiveVariety) := (X,p) -> (
    if not isPoint p then if isPoint X then return tangentSpace(p,X);
    if not (isPoint p and isSubset(p,X)) then error "expected a point on the variety";
    I := ideal X;
    subs := apply(gens ring I,toList coordinates p,(x,s) -> x => s);
    projectiveVariety(ideal((vars ring I) * sub(jacobian I,subs)),MinimalGenerators=>true,Saturate=>false)
);

tangentCone (EmbeddedProjectiveVariety,EmbeddedProjectiveVariety) := o -> (X,p) -> (
    if not isPoint p then if isPoint X then return tangentCone(p,X);
    if not (isPoint p and isSubset(p,X)) then error "expected a point on the variety";
    K := coefficientRing X;
    n := dim ambient X;
    a := toList coordinates p;
    j := 0; while a_j == 0 do j = j+1;
    A := transpose matrix{a} | submatrix'(diagonalMatrix toList(n+1:1),{j});
    t := local t;
    T := K[t_0..t_n];
    f := map(T,ring ambient X,(vars T) * (transpose A));
    I := trim f ideal X;
    -- assert(f ideal p == ideal submatrix'(vars T,{0}));
    J := (inverse f) sub(tangentCone(sub(sub(I,t_0=>1),K[t_1..t_n]),Strategy=>o.Strategy),T);
    projectiveVariety(J,MinimalGenerators=>true,Saturate=>false)
);

coneOfLines = method(TypicalValue => EmbeddedProjectiveVariety);
coneOfLines (EmbeddedProjectiveVariety,EmbeddedProjectiveVariety) := (X,p) -> (
    if not isPoint p then if isPoint X then return coneOfLines(p,X);
    if not (isPoint p and isSubset(p,X)) then error "expected a point on the variety";
    K := coefficientRing X;
    n := dim ambient X;
    a := toList coordinates p;
    j := 0; while a_j == 0 do j = j+1;
    A := transpose matrix{a} | submatrix'(diagonalMatrix toList(n+1:1),{j});
    t := local t;
    T := K[t_0..t_n];
    f := map(T,ring ambient X,(vars T) * (transpose A));
    I := trim f ideal X;
    -- assert(f ideal p == ideal submatrix'(vars T,{0}));
    R := K[t_1..t_n]; Rt0 := R[t_0]; 
    V := (inverse f) sub(ideal flatten entries sub(last coefficients(gens sub(I,Rt0)),R),T);
    projectiveVariety(V,MinimalGenerators=>true,Saturate=>false)
);

line = method();
line (EmbeddedProjectiveVariety,EmbeddedProjectiveVariety) := (X,p) -> (
    V := coneOfLines(X,p);
    if dim V <= 0 then error("failed to find line in "|toString(? X));
    if dim V >= 2 then return linearSpan {p,point V};
    L := select(decompose V,l -> dim l == 1 and degree l == 1);
    if # L == 0 then error("failed to find line in "|toString(? X));
    first random L    
);
line EmbeddedProjectiveVariety := X -> line(X,point X);

linearlyNormalEmbedding = method();
linearlyNormalEmbedding EmbeddedProjectiveVariety := X -> (
    Phi := multirationalMap X;
    f := first factor Phi;
    d := degreeSequence f;
    if not(#factor Phi == 1 and #d == #(maps f)) then error "internal error encountered";
    if #d == 1 then return Phi;
    local I;
    for i to #d-1 do (
        I = rationalMap(saturate ideal matrix(i,f),d_i);
        if numgens ambient target I -1 > dim linearSpan X then (
            I = multirationalMap rationalMap(I,Dominant=>true);
            if ring source I =!= ring X then error "internal error encountered";
            I#"source" = X;
            if dim target I == dim X then (I#"isBirational" = true; return I);
        );
    );
    error "failed to construct the embedding";
);

sendFewPoints = (X,Y) -> (
    n := X#"dimAmbientSpaces";
    K := coefficientRing X;
    assert(dim X == 0 and dim Y == 0 and degree X == degree Y and #n == 1 and n == Y#"dimAmbientSpaces" and K === coefficientRing Y);
    dX := decompose X,
    dY := decompose Y;
    if not (all(dX|dY,p -> isPoint p) and #dX == degree X and #dY == degree Y) then error("cannot decompose zero-dimensional subscheme of PP^"|toString(n_0)|" into the union of rational points");
    if degree X == n_0+2 then (
        MX' := transpose matrix apply(take(dX,#dX-1),p -> toList coordinates p);
        MX' = transpose matrix apply(flatten entries solve(MX',transpose matrix{toList coordinates last dX}),entries transpose MX',(i,j) -> i*j);
        MY' := transpose matrix apply(take(dY,#dY-1),p -> toList coordinates p);
        MY' = transpose matrix apply(flatten entries solve(MY',transpose matrix{toList coordinates last dY}),entries transpose MY',(i,j) -> i*j);
        return multirationalMap rationalMap(ring ambient X,ring ambient Y,(vars ring ambient X) * transpose(MY' * MX'^-1));
    );
    if degree X > n_0+2 then error("too many pairs of points of PP^"|toString(n_0)|" to be identified"); 
    MX := (transpose matrix apply(dX,p -> toList coordinates p)) | random(K^(n_0+1),K^(n_0-#dX+1));
    MY := (transpose matrix apply(dY,p -> toList coordinates p)) | random(K^(n_0+1),K^(n_0-#dY+1));
    multirationalMap rationalMap(ring ambient X,ring ambient Y,(vars ring ambient X) * transpose(MY * MX^-1))
);

findIsomorphism = method(Options => {Verify => true});
findIsomorphism (EmbeddedProjectiveVariety,EmbeddedProjectiveVariety) := o -> (X,Y) -> (
    verify := f -> (
        if o.Verify then (
            if not(source f === ambient X and target f === ambient Y and f X == Y and isIsomorphism f) then (
                error("failed attempt to find an isomorphism from "|toString(?X)|" to "|(if ?X ==?Y then "another of the same type" else toString(?Y))); 
            );
        );
        f.cache#("directImage",X) = Y; 
        f.cache#("inverseImage",Y) = X; 
        return f;
    );
    K := coefficientRing X;
    if K =!= coefficientRing Y then error "expected varieties over the same coefficient ring";
    if dim X != dim Y then error "expected varieties of the same dimension";
    if dim ambient X != dim ambient Y then error "the ambient projective spaces must have the same dimension";
    if dim X == -1 or codim X == 0 or X === Y then return verify rationalMap(ambient X,ambient Y);
    if degree X != degree Y then error "expected varieties of the same degree";
    if degrees X != degrees Y then error "the two varieties are not projectively equivalent";
    natMap := rationalMap(ambient X,ambient Y); if natMap X == Y then return verify natMap;
    if dim X == 0 then return verify sendFewPoints(X,Y);
    if linearSpan X != ambient X then (
        pLX := parametrize linearSpan X; X' := pLX^^ X;
        if X.cache#?"rationalParametrization" and (not X'.cache#?"rationalParametrization") 
        then X'.cache#"rationalParametrization" = check rationalMap((parametrize X) * inverse(pLX,Verify=>true),X');
        pLY := parametrize linearSpan Y; Y' := pLY^^ Y;
        if Y.cache#?"rationalParametrization" and (not Y'.cache#?"rationalParametrization") 
        then Y'.cache#"rationalParametrization" = check rationalMap((parametrize Y) * inverse(pLY,Verify=>true),Y');
        phi := inverse(pLX,Verify=>false) * findIsomorphism(X',Y',Verify=>false) * pLY;
        L := flatten entries gens ideal linearSpan X;
        Phi := rationalMap(ring ambient X,ring ambient Y,
               apply(flatten entries lift(matrix first factor phi,ring ambient X),
               e -> e + sum(L,w -> (random K)*w))); 
        return verify multirationalMap Phi;
    );
    pX := parametrize X;
    pY := parametrize Y;
    I := findIsomorphism(baseLocus pX,baseLocus pY,Verify=>false);
    M := solve(transpose coefficients first factor pX,transpose coefficients first factor (I*pY));
    return verify multirationalMap rationalMap(ring ambient X,ring ambient Y,(vars ring ambient X) * M);
);

EmbeddedProjectiveVariety ===> EmbeddedProjectiveVariety := (X,Y) -> findIsomorphism(X,Y,Verify=>true);

EmbeddedProjectiveVariety <=== EmbeddedProjectiveVariety := (X,Y) -> findIsomorphism(Y,X,Verify=>true);

ambientVariety = method(TypicalValue => MultiprojectiveVariety);
ambientVariety MultiprojectiveVariety := X -> if X#?"ambientVariety" then X#"ambientVariety" else ambient X;

isSubvariety = method(TypicalValue => Boolean);
isSubvariety MultiprojectiveVariety := X -> codim ambientVariety X > 0;

idealOfSubvariety = X -> (
    if not isSubvariety X then return ideal X;
    Y := ambientVariety X;
    if X.cache#?("idealOfSubvariety",Y) then return X.cache#("idealOfSubvariety",Y);
    X.cache#("idealOfSubvariety",Y) = trim sub(ideal X,ring Y)
);

makeSubvariety = method(TypicalValue => MultiprojectiveVariety, Options => {Verify => false});
makeSubvariety (MultiprojectiveVariety,MultiprojectiveVariety) := o -> (X,Y) -> (
    if ring ideal X =!= ring ideal Y then error "expected varieties in the same ambient multi-projective space";
    if o.Verify then if not isSubset(X,Y) then error "the first variety must be a subvariety of the second one";
    X#"ambientVariety" = Y;
    return X;
);
makeSubvariety Ideal := o -> I -> (
    Y := projectiveVariety(ring I,MinimalGenerators=>false,Saturate=>false);
    if # shape Y == 1 and dim Y >= 4 and codim Y > 0 and isGrass Y then Y = GG Y;    
    X := projectiveVariety(lift(I,ambient ring I),MinimalGenerators=>true,Saturate=>false);
    (ideal X).cache#("isSubsetAsVarietyOf",ideal Y) = true;
    makeSubvariety(X,Y,Verify=>o.Verify)
);
makeSubvariety RingElement := o -> F -> makeSubvariety(ideal F,Verify=>o.Verify);
makeSubvariety (MultiprojectiveVariety,List) := o -> (X,L) -> makeSubvariety(X,if same apply(L,ambientVariety) then ambientVariety first L else ambient X,Verify=>o.Verify);
MultiprojectiveVariety % MultiprojectiveVariety := (X,Y) -> makeSubvariety(X,Y,Verify=>true);
projectiveVariety Matrix := o -> M -> makeSubvariety(ideal M,Verify=>false);
projectiveVariety RingElement := o -> f -> makeSubvariety(f,Verify=>false);

tangentialChowForm (EmbeddedProjectiveVariety,ZZ,ZZ) := o -> (X,s,l) -> (
    S := o.SingularLocus;
    if instance(S,EmbeddedProjectiveVariety) then S = ideal S;
    if S === null and X.cache#?"singularLocus" then S = ideal singularLocus X;
    W := makeSubvariety tangentialChowForm(ideal X,s,l,Variable=>(ring ambient X),Duality=>o.Duality,AffineChartGrass=>o.AffineChartGrass,AssumeOrdinary=>o.AssumeOrdinary,AffineChartProj=>o.AffineChartProj,SingularLocus=>S);
    try return makeSubvariety(W,GG(l,ambient X),Verify=>true) else error "something went wrong with the ambient Grassmannian of the tangential Chow Form"; 
);
tangentialChowForm (EmbeddedProjectiveVariety,ZZ) := o -> (X,s) -> tangentialChowForm(X,s,codim X -1 + s,Variable=>o.Variable,Duality=>o.Duality,AffineChartGrass=>o.AffineChartGrass,AssumeOrdinary=>o.AssumeOrdinary,AffineChartProj=>o.AffineChartProj,SingularLocus=>o.SingularLocus);
chowForm EmbeddedProjectiveVariety := o -> X -> tangentialChowForm(X,0,Variable=>o.Variable,Duality=>o.Duality,AffineChartGrass=>o.AffineChartGrass,AffineChartProj=>o.AffineChartProj);

Fano (ZZ,EmbeddedProjectiveVariety,Option) := (k,X,opt) -> (
    o := toList opt;
    if not(#o == 2 and first o === AffineChartGrass) then error "AffineChartGrass is the only available option for Fano(ZZ,EmbeddedProjectiveVariety)";
    if k <= -1 then error "expected a nonnegative integer";
    F := makeSubvariety fanoVariety(ideal X,k,AffineChartGrass=>last o);
    try return makeSubvariety(F,GG(k,ambient X),Verify=>true) else error "something went wrong with the ambient Grassmannian of the Fano variety";
);
Fano (ZZ,EmbeddedProjectiveVariety) := (k,X) -> Fano(k,X,AffineChartGrass=>true);
Fano (EmbeddedProjectiveVariety,Option) := (X,opt) -> (
    o := toList opt;
    if not(#o == 2 and first o === AffineChartGrass) then error "AffineChartGrass is the only available option for Fano(EmbeddedProjectiveVariety)";
    if instance(ambientVariety X,GrassmannianVariety) then (
        I := varietySweptOutByLinearSpaces(idealOfSubvariety X,AffineChartGrass=>last o);
        return projectiveVariety(sub(I,vars ring (ambientVariety X)#"ProjectiveSpace"),MinimalGenerators=>true,Saturate=>false);
    );
    if unique flatten apply(degrees X,first) == {1} then (
        F := makeSubvariety plucker(ideal X,AffineChartGrass=>last o);
        k := first Grass ring ambient F;
        try return makeSubvariety(F,GG(k,ambient X),Verify=>true) else error "something went wrong with the ambient Grassmannian";
    ) else return Fano(dim X,X,AffineChartGrass=>last o);
);
Fano EmbeddedProjectiveVariety := X -> Fano(X,AffineChartGrass=>true);


MultirationalMap = new Type of MutableHashTable;

globalAssignment MultirationalMap;

MultirationalMap.synonym = "multi-rational map";

expression MultirationalMap := Phi -> (
    X := if hasAttribute(source Phi,ReverseDictionary) then toString getAttribute(source Phi,ReverseDictionary) else toString expression source Phi;
    Y := if hasAttribute(target Phi,ReverseDictionary) then toString getAttribute(target Phi,ReverseDictionary) else toString expression target Phi;
    if dim source Phi == -1 or dim target Phi == -1 then return expression("map from " | X | " to " | Y);
    if Phi#"baseLocus" =!= null and (Phi#"baseLocus")#"dimVariety" === -1 then (
        if Phi#"inverse" =!= null and (Phi#"inverse")#"baseLocus" =!= null and ((Phi#"inverse")#"baseLocus")#"dimVariety" === -1 then (
            if source Phi === target Phi and X == Y then return expression("automorphism of " | X) else return expression("isomorphism from " | X | " to " | Y);
        ) else (
            return expression((if Phi#"isBirational" === true then "birational morphism " else (if Phi#"isDominant" === true then "dominant morphism " else "morphism ")) | "from " | X | " to " | Y);  
        );
    );
    return expression((if Phi#"isBirational" === true then "birational " else (if Phi#"isDominant" === true then "dominant rational " else "rational "))| "map from " | X | " to " | Y);
);

net MultirationalMap := Phi -> if hasAttribute(Phi,ReverseDictionary) then toString getAttribute(Phi,ReverseDictionary) else ?Phi;

MultirationalMap#{Standard,AfterPrint} = MultirationalMap#{Standard,AfterNoPrint} = Phi -> (
    << endl << concatenate(interpreterDepth:"o") << lineNumber << " : " << class Phi << " (" << expression Phi << ")" << endl;
);

toString MultirationalMap := Phi -> "rationalMap("|(toString apply(factor Phi,f -> toString super f))|","|(toString target Phi)|")"; -- this doesn't work well

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
        symbol cache => new CacheTable,
        "maps" => L,
        "target" => Y,
        "source" => projectiveVariety(R,Saturate=>false),
        "image" => null,
        "isDominant" => null,
        "isBirational" => null,
        "graph" => null,
        "multidegree" => null,
        "baseLocus" => null,
        "inverse" => null
    }
);

multirationalMap List := L -> (
    if not (# L > 0 and all(L,f -> instance(f,RationalMap) or instance(f,MultihomogeneousRationalMap))) then error "expected a list of rational maps";
    Y := productVars apply(L,f -> projectiveVariety(target f,Saturate=>false));
    Phi := multirationalMap(L,Y);
    if #L == 1 then (
        Phi#"isDominant" = (first L)#"isDominant";
        Phi#"isBirational" = (first L)#"isBirational";
        if # (first L)#"projectiveDegrees" > 0 then Phi#"multidegree" = (first L)#"projectiveDegrees";
        if instance(first L,RationalMap) and (first L)#"inverseRationalMap" =!= null then (
            Phi#"inverse" = multirationalMap({(first L)#"inverseRationalMap"},source Phi);
            if ring source Phi#"inverse" =!= ring Y then error "internal error encountered";
            (Phi#"inverse")#"source" = Y;
            (Phi#"inverse")#"isDominant" = true;
            (Phi#"inverse")#"isBirational" = true;
            Phi#"isDominant" = true;
            Phi#"isBirational" = true;
            if # ((first L)#"inverseRationalMap")#"projectiveDegrees" > 0 then (Phi#"inverse")#"multidegree" = ((first L)#"inverseRationalMap")#"projectiveDegrees";
            (Phi#"inverse")#"inverse" = Phi;
        );
        if (first L)#"idealImage" =!= null and numgens (first L)#"idealImage" > 0 then (
            Phi#"image" = projectiveVariety(lift((first L)#"idealImage",ring ambient Y),MinimalGenerators=>true,Saturate=>false);
        );
    );
    Phi
);

rationalMap (List,MultiprojectiveVariety) := o -> (L,Y) -> (
    if o.Dominant =!= null then error "option Dominant is not allowed when you specify the target";
    if all(L,l -> instance(l,RingMap) or instance(l,Matrix) or instance(l,Ideal) or (instance(l,List) and #l>0 and all(l,i -> instance(i,RingElement)))) then L = apply(L,l -> rationalMap(l,Dominant=>"notSimplify"));
    return multirationalMap(L,Y);
);

rationalMap List := o -> L -> ( -- this redefines a method in Cremona.m2
    if #L == 0 then error "expected a nonempty list";
    if all(L,l -> instance(l,RingElement)) then return rationalMap(toMap(L,Dominant=>null),Dominant=>o.Dominant);  -- this is the original definition
    if all(L,l -> instance(l,RingMap) or instance(l,Matrix) or instance(l,Ideal) or (instance(l,List) and #l>0 and all(l,i -> instance(i,RingElement)))) then L = apply(L,l -> rationalMap(l,Dominant=>"notSimplify"));
    Phi := multirationalMap L;
    if o.Dominant === null then return Phi;
    if o.Dominant === true or o.Dominant === infinity then return multirationalMap(Phi,image Phi);
    if instance(o.Dominant,ZZ) and #L == 1 then return multirationalMap(Phi,projectiveVariety(image(super first factor Phi,o.Dominant),Saturate=>true));
    if instance(o.Dominant,Ideal) then return check multirationalMap(Phi,projectiveVariety(o.Dominant));
    if instance(o.Dominant,MultiprojectiveVariety) then return check multirationalMap(Phi,o.Dominant);
    error "invalid value for option Dominant";
);

rationalMap MultirationalMap := o -> Phi -> rationalMap(factor super Phi,Dominant=>o.Dominant);

multirationalMap RationalMap := phi -> multirationalMap {phi};
multirationalMap(RationalMap,RationalMap) := (phi1,phi2) -> multirationalMap {phi1,phi2};
multirationalMap MultihomogeneousRationalMap := phi -> multirationalMap {phi};
multirationalMap(MultihomogeneousRationalMap,MultihomogeneousRationalMap) := (phi1,phi2) -> multirationalMap {phi1,phi2};
multirationalMap (MultirationalMap,MultirationalMap) := (Phi1,Phi2) -> multirationalMap((factor Phi1)|(factor Phi2));

toRationalMap = method(TypicalValue => RationalMap);
toRationalMap (MultirationalMap,Boolean) := (Phi,withInverse) -> (
    if # factor Phi > 1 then error "expected a multi-rational map whose target is embedded in a single projective space"; 
    f := rationalMap(toRingMap(Phi,ring target Phi),Dominant=>"notSimplify");
    if Phi#"isDominant" =!= null then setKeyValue(f,"isDominant",Phi#"isDominant");
    if Phi#"isBirational" =!= null then setKeyValue(f,"isBirational",Phi#"isBirational");
    if Phi#"multidegree" =!= null then setKeyValue(f,"projectiveDegrees",Phi#"multidegree");
    if (first factor Phi)#"maps" =!= null and f#"maps" === null then setKeyValue(f,"maps",apply(maps first factor Phi,m -> if source m === ring target Phi then m else map(target m,ring target Phi,toMatrix m)));
    if Phi#"image" =!= null and f#"idealImage" === null then forceImage(f,sub(ideal image Phi,ring target Phi));
    if withInverse and Phi#"inverse" =!= null and instance(f,RationalMap) and f#"inverseRationalMap" === null then (
        g := toRationalMap(inverse Phi,false);
        if g#"inverseRationalMap" === null then forceInverseMap(f,g);
    );
    return f;
);
toRationalMap MultirationalMap := Phi -> toRationalMap(Phi,true);

matrix MultirationalMap := o -> Phi -> (
    if # factor Phi > 1 then error "expected a multi-rational map whose target is embedded in a single projective space"; 
    matrix first factor Phi
);

multirationalMap (MultirationalMap,MultiprojectiveVariety) := (Phi,Y) -> (
    if Y === target Phi then return Phi;
    L := factor Phi;
    if Y === ambient target Phi then L = apply(L,super);
    Psi := multirationalMap(L,Y);
    if ring source Psi =!= ring source Phi then error "internal error encountered";
    Psi#"source" = source Phi;
    Psi#"image" = Phi#"image";
    if Psi#"image" === Y then Psi#"isDominant" = true;
    if Phi.cache#?"compositionWithSegreEmbedding" then Psi.cache#"compositionWithSegreEmbedding" = Phi.cache#"compositionWithSegreEmbedding";
    if Phi#"graph" =!= null then Psi#"graph" = (first graph Phi, multirationalMap(last graph Phi,Y));
    Psi#"multidegree" = Phi#"multidegree";
    Psi#"baseLocus" = Phi#"baseLocus";
    return Psi;
);
rationalMap (MultirationalMap,MultiprojectiveVariety) := o -> (Phi,Y) -> multirationalMap(Phi,Y);

strongCheck = method();
strongCheck MultirationalMap := Phi -> (
    if not isMultisaturated ideal source Phi then error "the ideal of the source is not multi-saturated";
    if not isMultisaturated ideal target Phi then error "the ideal of the target is not multi-saturated";
    check Phi
);

isWellDefined MultirationalMap := (cacheValue "isWellDefined") (Phi -> (
    L := apply(factor Phi,super);
    P := apply(projections target Phi,L,(p,f) -> if target p === target f then p else rationalMap(source p,target f,matrix p,Dominant=>"notSimplify"));
    for i to #L -1 do if not isSubset(image P_i,image L_i) then return false;
    return true;
));

check MultirationalMap := o -> Phi -> if isWellDefined Phi then return Phi else error "the target variety is not compatible with the maps";

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
    try (checkRepresentatives Phi; strongCheck Phi) else error "found a wrong map";
    if Phi#"image" =!= null then (if not isMultisaturated ideal image Phi then error "found a wrong ideal for an image");
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

segre MultirationalMap := (cacheValue "compositionWithSegreEmbedding") (Phi -> (
    s := segre target Phi;
    f := toRingMap(Phi,source s);
    rationalMap(f * (map s),Dominant=>"notSimplify")
));

compose (MultirationalMap,MultirationalMap) := (Phi,Psi) -> (
    if ring ambient target Phi === ring ambient source Psi and target Phi == source Psi then (
        f := toRingMap(Phi,ring source Psi);
        Eta := multirationalMap(apply(factor Psi,g -> rationalMap(compose(f,map g),Dominant=>"notSimplify")),target Psi);
        if ring source Eta =!= ring source Phi then error "internal error encountered: bad source found";
        Eta#"source" = source Phi;
        if Phi#"isDominant" === true and Psi#"isDominant" === true then Eta#"isDominant" = true;
        if Phi#"isBirational" === true and Psi#"isBirational" === true then Eta#"isBirational" = true;
        return Eta;
    );
    try Phi' := check multirationalMap(super Phi,source Psi) else error "multi-rational maps not composable: not able to define a natural map from the target of the first one to the source of the second one";
    compose(Phi',Psi)
);

MultirationalMap * MultirationalMap := (Phi,Psi) -> compose(Phi,Psi);

MultirationalMap * MultihomogeneousRationalMap := (Phi,Psi) -> compose(Phi,multirationalMap {Psi});
MultirationalMap * RationalMap := (Phi,Psi) -> compose(Phi,multirationalMap {Psi});
MultihomogeneousRationalMap * MultirationalMap := (Phi,Psi) -> compose(multirationalMap {Phi},Psi);
RationalMap * MultirationalMap := (Phi,Psi) -> compose(multirationalMap {Phi},Psi);

MultirationalMap == MultirationalMap := (Phi,Psi) -> (
    if Phi === Psi then return true;
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
    I#"baseLocus" = 0_X;
    I#"inverse" = I;
    I
);

ZZ _ MultiprojectiveVariety := (n,X) -> (
    if n == 0 then (
        if not (ambient X).cache#?"emptySubscheme" then (
            O := projectiveVariety(ideal(1_(ring ambient X)),MinimalGenerators=>true,Saturate=>false);
            O#"dimVariety" = -1;
            (ambient X).cache#"emptySubscheme" = O;
        );
        return (ambient X).cache#"emptySubscheme";
    );
    if n =!= 1 then error "expected integer to be 0 or 1"; 
    multirationalMap X
);

random MultirationalMap := o -> Phi -> (
    S := ambient source Phi;
    f := rationalMap(apply(entries diagonalMatrix toList(# shape S : 1),shape S,(d,i) -> apply(i+1,j -> random(d,ring S))),S);
    -- assert(isIsomorphism f);
    T := ambient target Phi;
    g := rationalMap(apply(entries diagonalMatrix toList(# shape T : 1),shape T,(d,i) -> apply(i+1,j -> random(d,ring T))),T);
    -- assert(isIsomorphism g);
    (f||(source Phi)) * Phi * rationalMap(g|target Phi,Dominant=>true)
);

multirationalMap (MultiprojectiveVariety,MultiprojectiveVariety,Boolean) := (X,Y,b) -> ( --undocumented
    if X === Y then return multirationalMap X;
    I := multirationalMap(multirationalMap X,Y);
    if b then (try return check I else error "not able to define a natural map between the two varieties") else return I;
);
multirationalMap (MultiprojectiveVariety,MultiprojectiveVariety) := (X,Y) -> multirationalMap(X,Y,true);
rationalMap (MultiprojectiveVariety,MultiprojectiveVariety) := o -> (X,Y) -> multirationalMap(X,Y);

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
    if Phi.cache#?("directImage",Z) then return Phi.cache#("directImage",Z);
    if ring ambient source Phi =!= ring ambient Z then error "expected a multi-projective variety in the same ambient multi-projective space of the source of the map";
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
    Phi.cache#("directImage",Z) = projectiveVariety(suby' ideal selectInSubring(1,gens gb I),MinimalGenerators=>true,Saturate=>false)
);

image MultirationalMap := Phi -> (
    if Phi#"image" =!= null then return Phi#"image";
    if Phi#"isDominant" === true then return target Phi;
    Phi#"image" = Phi (source Phi);
    Phi#"isDominant" = Phi#"image" == target Phi;
    if Phi#"isDominant" then Phi#"image" = target Phi;
    return Phi#"image";
);

RationalMap MultiprojectiveVariety := (Phi,X) -> (multirationalMap Phi) X;
MultihomogeneousRationalMap MultiprojectiveVariety := (Phi,X) -> (multirationalMap Phi) X;

directImageStrongInt (MutableHashTable,MultiprojectiveVariety) := (Phi,X) -> (
    assert(instance(Phi,RationalMap) or instance(Phi,MultihomogeneousRationalMap));
    if ambient source Phi =!= ring ideal X then error "expected a multi-projective variety in the same ambient multi-projective space of the source of the map";
    makeSubvariety directImageStrongInt(Phi,ideal X)
);

image (MultirationalMap,ZZ) := (Phi,d) -> projectiveVariety(image(toRationalMap super Phi,d),MinimalGenerators=>false,Saturate=>false);
image (ZZ,MultirationalMap) := (d,Phi) -> projectiveVariety(image(d,toRationalMap super Phi),MinimalGenerators=>false,Saturate=>false);

image (MultirationalMap,String) := (Phi,alg) -> (
    if Phi#"image" =!= null then return Phi#"image";
    if Phi#"isDominant" === true then return target Phi;
    Y := projectiveVariety(image(toRationalMap super Phi,alg),MinimalGenerators=>false,Saturate=>false);
    if Phi#"image" === null then Phi#"image" = Y;
    Phi#"isDominant" = Phi#"image" == target Phi;
    if Phi#"isDominant" then Phi#"image" = target Phi;
    return Phi#"image";
);

inverseImageViaMultirationalMapWeak = (Phi,Z) -> (
    if Phi.cache#?("inverseImage",Z) then return Phi.cache#("inverseImage",Z);
    if ring ambient target Phi =!= ring ambient Z then error "expected a multi-projective variety in the same ambient multi-projective space of the target of the map";
    -- if not isSubset(Z,target Phi) then error "expected a subvariety of the target of the map";
    F := apply(factor Phi,f -> ideal matrix f);
    g := toRingMap(Phi,ring target Phi);
    I := g sub(ideal Z,ring target Phi);
    for f in F do I = saturate(I,f);
    Phi.cache#("inverseImage",Z) = projectiveVariety trim lift(I,ring ambient source Phi)
);

MultirationalMap ^* := (Phi) -> MultiprojectiveVariety := (Z) -> inverseImageViaMultirationalMapWeak(Phi,Z);

MultirationalMap ^^ MultiprojectiveVariety := (Phi,Z) -> (
    -- A fast inverse image but to be used only when Phi is a linear embedding (intended for internal use only)
    if Phi.cache#?("inverseImage",Z) then return Phi.cache#("inverseImage",Z);
    if ring ambient target Phi =!= ring ambient Z then error "expected a projective variety in the same ambient projective space of the target of the map";
    g := toRingMap(Phi,ring target Phi);
    if not(# (source Phi)#"dimAmbientSpaces" == 1 and # (target Phi)#"dimAmbientSpaces" == 1 and 
           ambient source Phi == source Phi and first max degrees ideal toMatrix g == 1) 
    then error "expected a linear morphism between projective spaces";
    Phi.cache#("inverseImage",Z) = projectiveVariety(g sub(ideal Z,ring target Phi),MinimalGenerators=>true,Saturate=>false)
);

MultirationalMap ^** MultiprojectiveVariety := (Phi,Z) -> (
    if ring ambient target Phi =!= ring ambient Z then error "expected a multi-projective variety in the same ambient of the target of the map";
    -- if not isSubset(Z,target Phi) then error "expected a subvariety of the target of the map";
    <<"--warning: the code for ^** must be improved, use instead the method ^*"<<endl;
    projectiveVariety trim lift((segre Phi)^** ((segre target Phi) ideal Z),ring ambient source Phi)
);

inverseImageWeakInt (MutableHashTable,EmbeddedProjectiveVariety) := (Phi,X) -> (
    assert(instance(Phi,RationalMap) or instance(Phi,MultihomogeneousRationalMap));
    if ambient target Phi =!= ring ideal X then error "expected a variety in the same ambient space of the target of the map";
    makeSubvariety inverseImageWeakInt(Phi,ideal X)
);

inverseImageStrongInt (MutableHashTable,EmbeddedProjectiveVariety) := (Phi,X) -> (
    assert(instance(Phi,RationalMap) or instance(Phi,MultihomogeneousRationalMap));
    if ambient target Phi =!= ring ideal X then error "expected a variety in the same ambient space of the target of the map";
    makeSubvariety inverseImageStrongInt(Phi,ideal X)
);
RationalMap ^** EmbeddedProjectiveVariety := (Phi,X) -> inverseImageStrongInt(Phi,X);
MultihomogeneousRationalMap ^** EmbeddedProjectiveVariety := (Phi,X) -> inverseImageStrongInt(Phi,X);

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
    projectiveVariety(quotient trim sub(ideal selectInSubring(1,gens gb I),R'),MinimalGenerators=>false,Saturate=>false)
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
    projectiveVariety(quotient trim I,MinimalGenerators=>false,Saturate=>false)
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
    projectiveVariety(quotient trim I,MinimalGenerators=>false,Saturate=>false)
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
    G' := projectiveVariety(quotient trim s ideal G,MinimalGenerators=>false,Saturate=>false);
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
    if Phi#"isBirational" === true then return 1;
    d := lift((last multidegree Phi)/(degree image Phi),ZZ);
    if (Phi#"isDominant" === true and Phi#"isBirational" === false and d == 1) then error "internal error encountered: obtained an incoherent value for the degree";
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

projectiveDegrees MultirationalMap := o -> Phi -> (
    if o.NumDegrees < 0 then return {};
    r := dim source Phi;
    ll := {(r - min(r,o.NumDegrees))..r};
    certificate := "MathMode: output certified!"|newline;
    if Phi#"multidegree" =!= null then (if o.MathMode and o.Verbose then <<certificate; return (Phi#"multidegree")_ll);
    if o.MathMode or # shape source Phi > 1 or # shape target Phi > 1 then (
        graph(Phi,BlowUpStrategy=>o.BlowUpStrategy);
        d := multidegree Phi;
        if o.MathMode and o.Verbose then <<certificate;
        return d_ll;
    ) else return projectiveDegrees(toRationalMap Phi,MathMode=>o.MathMode,NumDegrees=>o.NumDegrees,BlowUpStrategy=>o.BlowUpStrategy,Verbose=>o.Verbose);
);

degreeMap MultirationalMap := o -> Phi -> (
    certificate := "MathMode: output certified!"|newline;
    if o.MathMode or Phi#"isBirational" === true or (Phi#"multidegree" =!= null and Phi#"image" =!= null) then (
        -- this ignores the option BlowUpStrategy
        d := degree Phi;
        if o.MathMode and o.Verbose then <<certificate; 
        return d;
    );
    if # shape target Phi == 1 then return degreeMap(toRationalMap Phi,MathMode=>o.MathMode,BlowUpStrategy=>o.BlowUpStrategy,Verbose=>o.Verbose);
    return degree(Phi,Strategy=>"random point");
);

baseLocus = method(TypicalValue => MultiprojectiveVariety);
baseLocus MultirationalMap := Phi -> (
    if Phi#"baseLocus" =!= null then return Phi#"baseLocus";    
    I := lift(intersect apply(factor Phi,ideal),ring ambient source Phi);
    B := if # (source Phi)#"dimAmbientSpaces" > 1 then projectiveVariety(I,MinimalGenerators=>true,Saturate=>false) else projectiveVariety(I,MinimalGenerators=>true,Saturate=>true);
    Phi#"baseLocus" = B
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
    if not (instance(b,ZZ) and b >= -1) then error "option Verify accepts true or false";
    Gr := source graph Phi;
    Sub := map(ring target Phi,ring ambient Gr,matrix{toList((numgens ring ambient source Phi):0_(ring ambient target Phi))}|(vars ring ambient target Phi));
    r := # (source Phi)#"dimAmbientSpaces";
    x := apply(take(Gr#"multigens",r),g -> matrix{g});
    d := entries diagonalMatrix toList(r:1);
    gensGr := flatten entries gens ideal Gr;
    local I; local J; local F; local psi;
    L := for i to r-1 list if ((source Phi)#"dimAmbientSpaces")_i != 0 then (
        I = select(gensGr,g -> take(degree g,r) == d_i);
        J = matrix apply(I,g -> flatten entries diff(x_i,g));
        F = entries transpose mingens kernel Sub J;
        if #F == 0 then (Phi#"isBirational" = false; error "the multi-rational map is not birational");
        psi = rationalMap(first F,Dominant=>"notSimplify");
        psi#"maps" = apply(F,f -> map(source psi,target psi,f));
        psi#"map" = first psi#"maps";
        psi
    ) else (
        psi = rationalMap(matrix{{1_(ring target Phi)}},Dominant=>"notSimplify");
        psi#"maps" = {psi};
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
    if Phi#"isBirational" === true or b == -1 then (
        if b == -1 then (Phi#"isDominant" = true; Phi#"isBirational" = true);
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
    G := projectiveVariety(quotient SymmIdeal Phi,MinimalGenerators=>false,Saturate=>false); -- warning: this may not be multi-saturated
    (r,s) := (# (source Phi)#"dimAmbientSpaces", # (target Phi)#"dimAmbientSpaces");
    Phi' := multirationalMap(factor Phi,target Phi); Phi'#"source" = source Phi;
    Phi'#"graph" = (multirationalMap(take(projections G,r),source Phi'), multirationalMap(take(projections G,-s),target Phi'));
    Phi'#"isBirational" = null;
    Psi := inverse(Phi',Verify=>false);
    err := "not able to get an inverse map by using dedicated algorithm for the multi-linear type case; try using the general function inverse";
    b := last toList opt;
    if b === true or b === -1 then (
        if b === true then (
            try checkRepresentatives Psi else error(err|"(*)");
            if not(Phi * Psi == 1 and Psi * Phi == 1) then error(err|"()");
        );
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
    I := multirationalMap(X,source Phi,false);
    I * Phi
);

MultirationalMap | List := (Phi,d) -> (
    if not(# d == # (source Phi)#"dimAmbientSpaces" and all(d,i->instance(i,ZZ) and i>=0)) then error("expected a list of "|toString(# (source Phi)#"dimAmbientSpaces")|" non-negative integer(s) to indicate the degree of a hypersurface in the source"); 
    Phi|((source Phi) * projectiveVariety ideal random(d,ring ambient source Phi))
);

RationalMap | MultiprojectiveVariety := (Phi,X) -> (multirationalMap Phi)|X;
MultihomogeneousRationalMap | MultiprojectiveVariety := (Phi,X) -> (multirationalMap Phi)|X;

MultirationalMap || MultiprojectiveVariety := (Phi,Y) -> (
    if Y === target Phi then return Phi;
    X := Phi^* Y;
    I := multirationalMap(X,source Phi,false);
    multirationalMap(I * Phi,Y)
);

MultirationalMap || List := (Phi,d) -> (
    if not(# d == # (target Phi)#"dimAmbientSpaces" and all(d,i->instance(i,ZZ) and i>=0)) then error("expected a list of "|toString(# (target Phi)#"dimAmbientSpaces")|" non-negative integer(s) to indicate the degree of a hypersurface in the target"); 
    Phi||((target Phi) * projectiveVariety ideal random(d,ring ambient target Phi))
);

RationalMap || MultiprojectiveVariety := (Phi,Y) -> (multirationalMap Phi)||Y;
MultihomogeneousRationalMap || MultiprojectiveVariety := (Phi,Y) -> (multirationalMap Phi)||Y;

super MultirationalMap := Phi -> multirationalMap(Phi,ambient target Phi);

trim RationalMap := o -> Phi -> rationalMap(gens trim image matrix Phi,Dominant=>"notSimplify");
trim MultihomogeneousRationalMap := o -> Phi -> rationalMap(gens trim image matrix Phi,Dominant=>"notSimplify");
trim MultirationalMap := o -> Phi -> multirationalMap apply(factor Phi,trim);

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

MultirationalMap || MultirationalMap := (Phi,Psi) -> (
    X := source Phi; Y := source Psi;
    XxY := X ** Y;
    r := # X#"dimAmbientSpaces";
    s := # Y#"dimAmbientSpaces";
    pX := multirationalMap(take(projections XxY,r),X);
    pY := multirationalMap(take(projections XxY,-s),Y);
    Eta := (pX * Phi) | (pY * Psi);
    if ring source Eta =!= ring XxY then error "internal error encountered";
    Eta#"source" = XxY;
    return Eta;
);

RationalMap || MultirationalMap := (Phi,Psi) -> (multirationalMap {Phi})||Psi;
MultirationalMap || RationalMap := (Phi,Psi) -> Phi||(multirationalMap {Psi});
MultihomogeneousRationalMap || MultirationalMap := (Phi,Psi) -> (multirationalMap {Phi})||Psi;
MultirationalMap || MultihomogeneousRationalMap := (Phi,Psi) -> Phi||(multirationalMap {Psi});

RationalMap || RationalMap := (Phi,Psi) -> (multirationalMap {Phi})||(multirationalMap {Psi});
MultihomogeneousRationalMap || RationalMap := (Phi,Psi) -> (multirationalMap {Phi})||(multirationalMap {Psi});
RationalMap || MultihomogeneousRationalMap := (Phi,Psi) -> (multirationalMap {Phi})||(multirationalMap {Psi});
MultihomogeneousRationalMap || MultihomogeneousRationalMap := (Phi,Psi) -> (multirationalMap {Phi})||(multirationalMap {Psi});

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
    descr=descr|"target variety: "|(? target Phi);
    if Phi#"baseLocus" =!= null then descr=descr|newline|"base locus: "|(? baseLocus Phi);
    if Phi#"isDominant" =!= null then descr=descr|newline|"dominance: "|toString(Phi#"isDominant");
    if Phi#"isDominant" =!= true and Phi#"image" =!= null then descr=descr|newline|"image: "|(? image Phi);
    if Phi#"multidegree" =!= null then descr = descr|newline|"multidegree: "|toString(multidegree Phi);
    if (Phi#"multidegree" =!= null and Phi#"image" =!= null) or Phi#"isBirational" === true then descr=descr|newline|"degree: "|toString(degree Phi);
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

rationalMap MultiprojectiveVariety := o -> X -> multirationalMap rationalMap(idealOfSubvariety X,Dominant=>o.Dominant);
rationalMap (MultiprojectiveVariety,List) := o -> (X,l) -> multirationalMap rationalMap(idealOfSubvariety X,l,Dominant=>o.Dominant);
rationalMap (MultiprojectiveVariety,ZZ) := o -> (X,a) -> multirationalMap rationalMap(idealOfSubvariety X,a,Dominant=>o.Dominant);
rationalMap (MultiprojectiveVariety,ZZ,ZZ) := o -> (X,a,b) -> multirationalMap rationalMap(idealOfSubvariety X,a,b,Dominant=>o.Dominant);

PairOfVarieties = new Type of List;
MultiprojectiveVariety _ MultiprojectiveVariety := (X,Y) -> (
    if ring ideal X =!= ring ideal Y then error "expected varieties in the same ambient multi-projective space";
    new PairOfVarieties from {X,Y}
);
ideal PairOfVarieties := Z -> trim sub(ideal Z#0,ring Z#1);
rationalMap PairOfVarieties := o -> X -> multirationalMap rationalMap(ideal X,Dominant=>o.Dominant);
rationalMap (PairOfVarieties,List) := o -> (X,l) -> multirationalMap rationalMap(ideal X,l,Dominant=>o.Dominant);
rationalMap (PairOfVarieties,ZZ) := o -> (X,a) -> multirationalMap rationalMap(ideal X,a,Dominant=>o.Dominant);
rationalMap (PairOfVarieties,ZZ,ZZ) := o -> (X,a,b) -> multirationalMap rationalMap(saturate (ideal X)^b,a,Dominant=>o.Dominant);

clean MultirationalMap := Phi -> multirationalMap(apply(factor Phi,clean),target Phi);
clean RationalMap := phi -> rationalMap(map(source phi,target phi,matrix phi),Dominant=>"notSimplify");
clean MultihomogeneousRationalMap := phi -> rationalMap(map(source phi,target phi,matrix phi),Dominant=>"notSimplify");

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

MultirationalMap << MultiprojectiveVariety := (Phi,Y) -> (
    if coefficientRing Phi =!= coefficientRing Y then error "different coefficient rings encountered";
    if not (# shape target Phi == # shape Y and all(shape target Phi,shape Y,(i,j) -> i <= j)) then error "shapes not compatible";
    L := apply(apply(factor Phi,matrix),shape Y,(M,d) -> M|matrix{toList(d+1-(numColumns M) : 0_(ring M))});
    check rationalMap(L,Y)
);
MultiprojectiveVariety << MultiprojectiveVariety := (X,Y) -> (1_X) << Y;

rationalMap (MultiprojectiveVariety,Tally) := o -> (X,E) -> (
    D := applyPairs(E,(k,v) -> if instance(k,MultiprojectiveVariety) then (idealOfSubvariety k,v) else (k,v));
    f := multirationalMap rationalMap(ring X,D,Dominant=>o.Dominant);
    if ring source f =!= ring X then error "internal error encountered: bad source found";
    f#"source" = X;
    return f;
);


GrassmannianVariety = new Type of EmbeddedProjectiveVariety;

globalAssignment GrassmannianVariety;

GrassmannianVariety.synonym = "Grassmannian variety";

? GrassmannianVariety := G -> (toString expression G) | (if codim G > 0 then " ⊂ PP^" else " = PP^") | toString(dim ambient G);

GG = method();

GG (ZZ,EmbeddedProjectiveVariety) := (k,P) -> (
    if P.cache#?(k,GrassmannianVariety) then return P.cache#(k,GrassmannianVariety);
    if codim P > 0 then error "expected a projective space";
    if k < 0 then error "expected a non-negative integer";
    if k > dim P then error("expected an integer not exceeding "|(toString dim P));
    G := new GrassmannianVariety from projectiveVariety(Grass(k,dim P,coefficientRing P,Variable=>ring P),Saturate=>false);
    (ring G)#GrassmannianVariety = G;
    (ideal G).cache#GrassmannianVariety = G;
    G.cache#"top" = G; 
    G.cache#"singularLocus" = 0_G;
    G#"expression" = expression("GG("|toString(k)|","|toString(dim P)|")");  
    G#"ProjectiveSpace" = P;
    G#"dimLinearSpaces" = k;
    P.cache#(k,GrassmannianVariety) = G
);
GG (Ring,ZZ,ZZ) := (K,k,n) -> GG(k,projectiveVariety Grass(0,n,K,Variable=>"x"));
GG (ZZ,ZZ) := (k,n) -> GG(QQ,k,n);
GG EmbeddedProjectiveVariety := X -> (
    if instance(X,GrassmannianVariety) then return X;
    if not isGrass X then error "expected a Grassmannian variety";
    Y := GG X.cache#"GrassInfo";
    if ring Y =!= ring X then error "internal error encountered";
    return Y;
);
GG Ring := R -> GG projectiveVariety(R,Saturate=>false);

GG (ZZ,MultirationalMap) := (k,Phi) -> (
    if not(# shape source Phi == 1 and # shape target Phi == 1 and codim source Phi == 0 and codim target Phi == 0 and dim source Phi == dim target Phi and first max degrees ideal matrix Phi == 1) 
    then error "expected an automorphism of a projective space";
    A := coefficients toRationalMap Phi;
    K := coefficientRing Phi;
    n := dim source Phi;
    x := local x;
    R := K[x_(0,0)..x_(n,k)];
    M := genericMatrix(R,k+1,n+1);
    N := M * transpose A;
    mM := matrix{apply(subsets(n+1,k+1),m -> det submatrix(M,m))};
    B := matrix apply(subsets(n+1,k+1),m -> linearCombination(det submatrix(N,m),mM));
    G := GG(k,source Phi);
    Psi := multirationalMap({rationalMap(ring G,ring G,(vars ring ambient G) * transpose B)},G);
    if source Psi =!= G then error "internal error encountered";
    return Psi;  
);
GG (ZZ,RationalMap) := (k,Phi) -> GG(k,multirationalMap Phi);

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
    flatten entries solve(submatrix'(H,{m}),-submatrix(H,{m}))
);

chowRing = method();
chowRing (ZZ,GrassmannianVariety) := (m,G) -> (
    if not G.cache#?(m,"ChowRing") then (
        k := G#"dimLinearSpaces";
        n := dim G#"ProjectiveSpace";
        L := rsort select(apply(toList (set toList(0..(n-k)))^**(k+1),l -> toList deepSplice l),l -> l == rsort l and sum l == m);
        s := local s;
        G.cache#(m,"ChowRing") = ZZ[apply(L,l -> s_(unsequence toSequence l))];
    );
    G.cache#(m,"ChowRing")
);

dimdegree = X -> if dim X == -1 then 0 else if dim X == 0 then degree X else error "expected a zero-dimensional scheme"; 
cycleClass = method();
cycleClass EmbeddedProjectiveVariety := X -> (
    G := ambientVariety X;
    if not instance(G,GrassmannianVariety) then (
        if not isGrass G then error "expected a subvariety of some Grassmannian";
        <<"--warning: ambient variety of "<<X<<" has been changed to be a Grassmannian"<<endl;
        return cycleClass (X % GG ambientVariety X);
    );
    k := G#"dimLinearSpaces";
    n := dim G#"ProjectiveSpace";
    m := (dim G) - (dim X);
    sum(gens chowRing(m,G),g -> g * dimdegree(X * schubertCycle(toList(k+1:n-k) - toList reverse last baseName g,G)))
);

schubertCycle = method(Options => {Standard => false});
schubertCycle (VisibleList,GrassmannianVariety) := o -> (a,G) -> (
    k' := G#"dimLinearSpaces";
    n' := dim G#"ProjectiveSpace";
    a = toList a;
    n := n'+1;
    k := #a;
    if not (all(a,j -> instance(j,ZZ)) and rsort a == a and first a <= n-k and k == k'+1) then error("expected a nonincreasing sequence of "|toString(k'+1)|" nonnegative integers bounded by "|toString(n'-k'));
    a = prepend(null,a);
    V := completeFlag(G,Standard=>o.Standard);
    S := makeSubvariety trim sum for i from 1 to k list idealOfSubvariety tangentialChowForm(V_(n-k+i-a_i),i-1,k-1,SingularLocus=>0_(first V));
    try return makeSubvariety(S,G,Verify=>true) else error "something went wrong with the ambient Grassmannian of the Schubert cycle";
);

completeFlag = method(Options => {Standard => false});
completeFlag GrassmannianVariety := o -> G -> (
    P := G#"ProjectiveSpace";
    L := if o.Standard then gens ring P else apply(1+dim P,i -> random(1,ring P));
    V := append(reverse for i to dim P list projectiveVariety(ideal take(L,i+1),Saturate=>false),P);
    if apply(V,dim) =!= toList(-1 .. dim P) then (
        <<"--warning: re-running completeFlag"<<endl;
        return completeFlag(G,Standard=>o.Standard);
    );
    return V;
);


beginDocumentation() 

document {Key => {MultiprojectiveVarieties}, 
Headline => "Multi-projective varieties and multi-rational maps",
PARA{"This is a package for handling multi-projective varieties, that is, closed subvarieties of products of projective spaces, and rational maps between them. This extends the package ",TO Cremona,", which treats ",TO2{RationalMap,"rational maps"}," from multi-projective varieties to ",EM"standard"," projective varieties, ",TEX///$X\subseteq \mathbb{P}^{k_1}\times\mathbb{P}^{k_2}\times\cdots\times\mathbb{P}^{k_n}\dashrightarrow Y\subseteq\mathbb{P}^N$///,"."},References => {"ArXiv preprint: ",HREF{"https://arxiv.org/abs/2101.04503","Computations with rational maps between multi-projective varieties"},"."},
Subnodes => {TO MultiprojectiveVariety,TO MultirationalMap}}

document {Key => {MultiprojectiveVariety}, 
Headline => "the class of all multi-projective varieties", 
PARA {"A ",EM"multi-projective variety"," is a closed subvariety of a product of projective spaces ",TEX///$\mathbb{P}^{k_1}\times\mathbb{P}^{k_2}\times\cdots\times\mathbb{P}^{k_n}$///,". This is actually the class of all closed subschemes of products of projective spaces."},
Subnodes => {TO "projectiveVariety",-- TO (projectiveVariety,Ideal),TO (projectiveVariety,Ring),
TO (projectiveVariety,List,Ring), 
TO (projectiveVariety,List,List,Ring),
TO (projectiveVariety,MultidimensionalMatrix)}}

document {Key => {EmbeddedProjectiveVariety}, 
Headline => "the class of all embedded projective varieties", 
PARA {"The ",EM"embedded projective varieties"," are exactly the ",TO2{MultiprojectiveVariety,"multi-projective varieties"}," embedded in a single projective space; so that ",TEX///$X$///," is an embedded projective variety if and only if ",TT"#"," ",TO2{(shape,MultiprojectiveVariety),"shape"},TT" X == 1","."}, 
EXAMPLE {
"X = PP_QQ^(2,2);",
"class X",
"Y = X ** X;",
"class Y"},
SeeAlso => {MultiprojectiveVariety,(ambient,MultiprojectiveVariety),(shape,MultiprojectiveVariety)}}

document {Key => {Saturate, [projectiveVariety,Saturate]},
Headline => "whether to compute the multi-saturation of the ideal (intended for internal use only)",
Usage => "projectiveVariety(I,Saturate=>false)", 
PARA{"Use this option only in the case you know that the ideal ",TT"I"," is already multi-saturated, otherwise nonsensical answers may result."},
SeeAlso => {projectiveVariety,[projectiveVariety,MinimalGenerators]}}

document {Key => {[projectiveVariety,MinimalGenerators]},
Headline => "whether to trim the ideal (intended for internal use only)",
Usage => "projectiveVariety(I,MinimalGenerators=>false)", 
PARA{"Use this option only in the case you know that the ideal ",TT"I"," is already trimmed."},
SeeAlso => {projectiveVariety,[projectiveVariety,Saturate]}}

document { 
Key => {projectiveVariety, (projectiveVariety,Ideal), (projectiveVariety,Ring), (projectiveVariety,Matrix), (projectiveVariety,RingElement)}, 
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

document {Key => {(projectiveVariety,List,Ring),(projectiveVariety,ZZ,Ring),symbol PP}, 
Headline => "product of projective spaces", 
Usage => "projectiveVariety(n,K)
PP_K^n", 
Inputs => {"n" => List => {"a list of non-negative integers ",TEX///$n=\{n_1,n_2,\ldots,n_r\}$///},"K" => Ring => {"a field"}}, 
Outputs => {MultiprojectiveVariety => {"the product of projective spaces ", TEX///$\mathbb{P}^{n_1}\times\mathbb{P}^{n_2}\times\cdots\times\mathbb{P}^{n_r}$///," over ",TEX///$K$///}}, 
EXAMPLE {"projectiveVariety({2,1,3},ZZ/33331);","PP_(ZZ/33331)^{2,1,3};","PP_QQ^{1,1,1,1};","PP_QQ^{};"},
SeeAlso => {((projectiveVariety,List,List,Ring))}} 

document {Key => {(projectiveVariety,List,List,Ring),(projectiveVariety,ZZ,ZZ,Ring)}, 
Headline => "the Segre-Veronese variety", 
Usage => "projectiveVariety(n,d,K)
PP_K^(n,d)", 
Inputs => {
"n" => List => {"a list of ",TEX///$r$///," non-negative integers ",TEX///$n=\{n_1,n_2,\ldots,n_r\}$///},
"d" => List => {"a list of ",TEX///$r$///," degrees ",TEX///$d=\{d_1,d_2,\ldots,d_r\}$///},
"K" => Ring => {"a field"}}, 
Outputs => {MultiprojectiveVariety => {"the Segre-Veronese variety ", TEX///$\nu_{d_1}(\mathbb{P}^{n_1})\times\nu_{d_2}(\mathbb{P}^{n_2})\times\cdots\times\nu_{d_r}(\mathbb{P}^{n_r})$///," over ",TEX///$K$///}}, 
EXAMPLE {"X = projectiveVariety({2,1,3},{3,4,2},ZZ/33331);","X = PP_(ZZ/33331)^({2,1,3},{3,4,2});","parametrize X;"},
SeeAlso => {((projectiveVariety,List,Ring))}} 

document {Key => {(dim,MultiprojectiveVariety)}, 
Headline => "the dimension of the variety", 
Usage => "dim X", 
Inputs => {"X" => MultiprojectiveVariety}, 
Outputs => {ZZ => {"the dimension of ", TEX///$X$///}}, 
EXAMPLE {"X = PP_QQ^({2,1},{1,3});","dim X"}, 
SeeAlso => {(codim,MultiprojectiveVariety)}} 

document {Key => {(codim,MultiprojectiveVariety)}, 
Headline => "the codimension of the variety", 
Usage => "codim X", 
Inputs => {"X" => MultiprojectiveVariety}, 
Outputs => { ZZ => {"the codimension of ", TEX///$X$///}}, 
EXAMPLE {"X = PP_QQ^({2,1},{1,3});","codim X"}, 
SeeAlso => {(dim,MultiprojectiveVariety)}} 

document {Key => {(ideal,MultiprojectiveVariety)}, 
Headline => "the defining ideal of the variety", 
Usage => "ideal X", 
Inputs => {"X" => MultiprojectiveVariety}, 
Outputs => {Ideal => {"the defining ideal of ", TEX///$X$///}}, 
EXAMPLE {"X = PP_QQ^({2,1},{1,3});","ideal X"}, 
SeeAlso => {(ring,MultiprojectiveVariety)}} 

document {Key => {(ring,MultiprojectiveVariety)}, 
Headline => "the coordinate ring of the variety", 
Usage => "ring X", 
Inputs => {"X" => MultiprojectiveVariety}, 
Outputs => {Ring => {"the coordinate ring of ", TEX///$X$///}}, 
EXAMPLE {"X = PP_QQ^({2,1},{1,3});","ring X"}, 
SeeAlso => {(ideal,MultiprojectiveVariety),(coefficientRing,MultiprojectiveVariety)}} 

document {Key => {(coefficientRing,MultiprojectiveVariety)}, 
Headline => "the coefficient ring of the variety", 
Usage => "coefficientRing X", 
Inputs => {"X" => MultiprojectiveVariety}, 
Outputs => {Ring => {"the coefficient ring of ", TEX///$X$///}}, 
EXAMPLE {"X = PP_QQ^({2,1},{1,3});","coefficientRing X"}, 
SeeAlso => {(ring,MultiprojectiveVariety),(symbol **,MultiprojectiveVariety,Ring)}} 

document {Key => {(degree,MultiprojectiveVariety)}, 
Headline => "the degree of the variety", 
Usage => "degree X", 
Inputs => {"X" => MultiprojectiveVariety}, 
Outputs => { ZZ => {"the degree of the image of ", TEX///$X$///," via the Segre embedding of the ",TO2{(ambient,MultiprojectiveVariety),"ambient"}," of ",TEX///$X$///}}, 
EXAMPLE {"X = PP_QQ^({2,1},{1,3});","degree X"}, 
SeeAlso => {(multidegree,MultiprojectiveVariety),(segre,MultiprojectiveVariety)}} 

document {Key => {projections,(projections,MultiprojectiveVariety)}, 
Headline => "projections of a multi-projective variety", 
Usage => "projections X", 
Inputs => {"X" => MultiprojectiveVariety => {"a subvariety of ",TEX///$\mathbb{P}^{k_1}\times\mathbb{P}^{k_2}\times\cdots\times\mathbb{P}^{k_n}$///}}, 
Outputs => {{"the list of the projections ", TEX///$X\to \mathbb{P}^{k_i}$///,", for ",TEX///$i=1,\ldots,n$///}}, 
EXAMPLE {"X = projectiveVariety(ZZ/101[x_0..x_3]) ** projectiveVariety(ZZ/101[y_0..y_2]);","projections X"}} 

document {Key => {(ambient,MultiprojectiveVariety)}, 
Headline => "the ambient multi-projective space of the variety", 
Usage => "ambient X", 
Inputs => {"X" => MultiprojectiveVariety}, 
Outputs => { MultiprojectiveVariety => {"the product of the projective spaces where ", TEX///$X$///," is embedded"}}, 
EXAMPLE {"X = PP_QQ^({1,1,1},{2,1,3});","ambient X;"}} 

document {Key => {(multidegree,MultiprojectiveVariety)}, 
Headline => "the multidegree of the variety", 
Usage => "multidegree X", 
Inputs => {"X" => MultiprojectiveVariety}, 
Outputs => {{"the multi-degree of the defining ideal of ", TEX///$X$///}}, 
EXAMPLE {"X = random({{1,1},{2,1}},point PP_(ZZ/33331)^{2,2});","multidegree X"}, 
SeeAlso => {(degree,MultiprojectiveVariety),(multidegree,Ideal)}} 

document {Key => {(segre,MultiprojectiveVariety)}, 
Headline => "the Segre embedding of the variety", 
Usage => "segre X", 
Inputs => {"X" => MultiprojectiveVariety}, 
Outputs => {{"the map returned by ",TO segre," ",TO2{(ring,MultiprojectiveVariety),"ring"}," ", TEX///$X$///}}, 
EXAMPLE {"X = PP_(ZZ/3331)^({2,1,1},{2,1,3});","segre X"}, 
SeeAlso => {segre,(segre,MultirationalMap)}}

document {Key => {(point,MultiprojectiveVariety),(symbol |-, MultiprojectiveVariety)}, 
Headline => "pick a random rational point on a multi-projective variety", 
Usage => "point X", 
Inputs => {"X" => MultiprojectiveVariety => {"defined over a finite field"}}, 
Outputs => {MultiprojectiveVariety => {"a random rational point on ", TEX///$X$///}}, 
EXAMPLE {"K = ZZ/1000003;","X = PP_K^({1,1,2},{3,2,3});","time p := point X","Y = random({2,1,2},X);","time q = point Y", "assert(isSubset(p,X) and isSubset(q,Y))"},
PARA {"The list of homogeneous coordinates can be obtained with the operator ",TT"|-","."},
EXAMPLE {"|- p", "|- q"},
SeeAlso => {point,randomKRationalPoint}} 

document {Key => {(singularLocus,MultiprojectiveVariety)}, 
Headline => "the singular locus of the variety", 
Usage => "singularLocus X", 
Inputs => {"X" => MultiprojectiveVariety => {"which is assumed to be equidimensional"}}, 
Outputs => { MultiprojectiveVariety => {"the singular locus of ", TEX///$X$///}}, 
EXAMPLE {"X = random({2,1},point PP_(ZZ/101)^{2,1});","singularLocus X","Y = X + random({1,1},0_X);","singularLocus Y"}} 

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
Boolean => {"whether ",TT"X"," is contained in ",TT"Y"}},
EXAMPLE lines ///Y = PP_(ZZ/33331)^(2,2);
X = point Y;
isSubset(X,Y)
isSubset(Y,X)///}

document {Key => {(symbol **,MultiprojectiveVariety,MultiprojectiveVariety)}, 
Headline => "product of two multi-projective varieties", 
Usage => "X ** Y", 
Inputs => { 
MultiprojectiveVariety => "X",
MultiprojectiveVariety => "Y"}, 
Outputs => { 
MultiprojectiveVariety => {"the product of ",TT"X"," and ",TT"Y"}},
EXAMPLE {"X = projectiveVariety ideal(random({2,1},ring PP_(ZZ/101)^{2,1}),random({1,1},ring PP^{2,1}));", 
"Y = projectiveVariety ideal random({1,1,1},ring PP^{1,2,1});",
"X ** Y"},
SeeAlso => {fiberProduct,(symbol ^,MultiprojectiveVariety,ZZ),(∏,List)}}

document {Key => {∏,(∏,List)}, 
Headline => "product of multi-projective varieties", 
Usage => "∏ {X,Y,Z,...}", 
Inputs => {{ofClass List," ",TT"{X,Y,Z,...}"," of ",TO2{MultiprojectiveVariety,"multi-projective varieties"}}}, 
Outputs => {MultiprojectiveVariety => {"the product ",TEX///$X\times Y \times Z\times \cdots$///}},
EXAMPLE {"K = ZZ/33331;",
"X = PP_K^(2,2);",
"Y = PP_K^({1,1,1},{2,3,1});",
"Z = PP_K^(1,4);",
"∏ {X,Y,Z};",
"assert(oo == ∏ {X ** Y,Z} and ∏ {X ** Y,Z} == ∏ {X, Y ** Z})"},
SeeAlso => {(symbol **,MultiprojectiveVariety,MultiprojectiveVariety)}}

document {Key => {(symbol ^,MultiprojectiveVariety,ZZ)}, 
Headline => "power of a multi-projective variety", 
Usage => "X^n", 
Inputs => {"X" => MultiprojectiveVariety,"n" => ZZ}, 
Outputs => {MultiprojectiveVariety => {"the product of ",TEX///$n$///," copies of ", TEX///$X$///}}, 
EXAMPLE {"X = PP_(ZZ/33331)^(1,3);",
"X^2;",
"X^3;",
"X^5;",
"assert(X^3 == X^2 ** X)",
"assert(X^5 == X^3 ** X^2)"},
SeeAlso => {(symbol **,MultiprojectiveVariety,MultiprojectiveVariety)}} 

document {Key => {(symbol *,MultiprojectiveVariety,MultiprojectiveVariety)}, 
Headline => "intersection of two multi-projective varieties", 
Usage => "X * Y", 
Inputs => { 
MultiprojectiveVariety => "X",
MultiprojectiveVariety => "Y"}, 
Outputs => { 
MultiprojectiveVariety => {"the intersection of ",TT"X"," and ",TT"Y",", that is, the projective variety defined by the sum of the corresponding ideals"}},
EXAMPLE {"O = 0_(PP_(ZZ/101)^{2,1});",
"X = random({2,1},O);",
"Y = random({1,1},O);",
"X * Y"},
SeeAlso => {(symbol +,MultiprojectiveVariety,MultiprojectiveVariety),(symbol +,Ideal,Ideal),(⋂,List)}}

document {Key => {⋂,(⋂,List)}, 
Headline => "intersection of multi-projective varieties", 
Usage => "⋂ {X,Y,Z,...}", 
Inputs => {{ofClass List," ",TT"{X,Y,Z,...}"," of ",TO2{MultiprojectiveVariety,"multi-projective varieties"}}}, 
Outputs => {MultiprojectiveVariety => {"the intersection ",TEX///$X\cap Y \cap Z\cap \cdots$///}},
EXAMPLE {"K = ZZ/33331;",
"p = point PP_K^({1,2},{1,1});",
"X = random({1,1},p);",
"Y = random({2,1},p);",
"Z = random({2,2},p);",
"⋂ {X,Y,Z}"},
SeeAlso => {(symbol *,MultiprojectiveVariety,MultiprojectiveVariety)}}

document {Key => {(symbol +,MultiprojectiveVariety,MultiprojectiveVariety)}, 
Headline => "union of two multi-projective varieties", 
Usage => "X + Y", 
Inputs => { 
MultiprojectiveVariety => "X",
MultiprojectiveVariety => "Y"}, 
Outputs => { 
MultiprojectiveVariety => {"the union of ",TT"X"," and ",TT"Y",", that is, the projective variety defined by the intersection of the corresponding ideals"}},
EXAMPLE {"O = 0_(PP_(ZZ/101)^{2,1});",
"X = random({2,1},O);",
"Y = random({1,1},O);",
"Z = X + Y;",
///assert(Z \ X == Y and Z \ Y == X)///},
SeeAlso => {(symbol \,MultiprojectiveVariety,MultiprojectiveVariety),(symbol *,MultiprojectiveVariety,MultiprojectiveVariety),(intersect,List),(⋃,List)}}

document {Key => {⋃,(⋃,List)}, 
Headline => "union of multi-projective varieties", 
Usage => "⋃ {X,Y,Z,...}", 
Inputs => {{ofClass List," ",TT"{X,Y,Z,...}"," of ",TO2{MultiprojectiveVariety,"multi-projective varieties"}}}, 
Outputs => {MultiprojectiveVariety => {"the union ",TEX///$X\cup Y \cup Z\cup \cdots$///}},
EXAMPLE {"K = ZZ/33331;",
"L = for i to 9 list point PP_K^({1,2,2},{1,1,3});",
"⋃ L",
"degree oo"},
SeeAlso => {(symbol +,MultiprojectiveVariety,MultiprojectiveVariety)}}

document {Key => {(symbol \,MultiprojectiveVariety,MultiprojectiveVariety)}, 
Headline => "difference of multi-projective varieties", 
Usage => ///X \ Y///, 
Inputs => { 
MultiprojectiveVariety => "X",
MultiprojectiveVariety => "Y"}, 
Outputs => { 
MultiprojectiveVariety => {"the variety defined by the colon ideal ",TT"ideal X : ideal Y"}},
EXAMPLE {"R = ZZ/101[x_0,x_1,x_2,y_0,y_1,Degrees=>{3:{1,0},2:{0,1}}];",
"X = projectiveVariety ideal(x_0^3*y_0+2*x_0^2*x_1*y_0+2*x_0*x_1^2*y_0+x_1^3*y_0+2*x_0^2*x_2*y_0+3*x_0*x_1*x_2*y_0+2*x_1^2*x_2*y_0+2*x_0*x_2^2*y_0+2*x_1*x_2^2*y_0+x_2^3*y_0+x_0^3*y_1+2*x_0^2*x_1*y_1+2*x_0*x_1^2*y_1+x_1^3*y_1+2*x_0^2*x_2*y_1+3*x_0*x_1*x_2*y_1+2*x_1^2*x_2*y_1+2*x_0*x_2^2*y_1+2*x_1*x_2^2*y_1+x_2^3*y_1);",
"Y = projectiveVariety ideal(x_0*y_0+x_1*y_0+x_2*y_0+x_0*y_1+x_1*y_1+x_2*y_1);", 
///Z = X \ Y;///,
///assert(Z + Y == X and X \ Z == Y)///},
SeeAlso => {(symbol \\,MultiprojectiveVariety,MultiprojectiveVariety),(symbol +,MultiprojectiveVariety,MultiprojectiveVariety),(quotient,Ideal,Ideal)}}

document {Key => {(symbol \\,MultiprojectiveVariety,MultiprojectiveVariety)}, 
Headline => "difference of multi-projective varieties", 
Usage => ///X \\ Y///, 
Inputs => { 
MultiprojectiveVariety => "X",
MultiprojectiveVariety => "Y"}, 
Outputs => { 
MultiprojectiveVariety => {"the variety defined by the saturation ideal ",TT"saturate(ideal X,ideal Y)"}},
EXAMPLE {"R = ZZ/101[x_0,x_1,x_2,y_0,y_1,Degrees=>{3:{1,0},2:{0,1}}];",
"X = projectiveVariety ideal(x_0^3*y_0^2+2*x_0^2*x_1*y_0^2+2*x_0*x_1^2*y_0^2+x_1^3*y_0^2+2*x_0^2*x_2*y_0^2+3*x_0*x_1*x_2*y_0^2+2*x_1^2*x_2*y_0^2+2*x_0*x_2^2*y_0^2+2*x_1*x_2^2*y_0^2+x_2^3*y_0^2+2*x_0^3*y_0*y_1+4*x_0^2*x_1*y_0*y_1+4*x_0*x_1^2*y_0*y_1+2*x_1^3*y_0*y_1+4*x_0^2*x_2*y_0*y_1+6*x_0*x_1*x_2*y_0*y_1+4*x_1^2*x_2*y_0*y_1+4*x_0*x_2^2*y_0*y_1+4*x_1*x_2^2*y_0*y_1+2*x_2^3*y_0*y_1+x_0^3*y_1^2+2*x_0^2*x_1*y_1^2+2*x_0*x_1^2*y_1^2+x_1^3*y_1^2+2*x_0^2*x_2*y_1^2+3*x_0*x_1*x_2*y_1^2+2*x_1^2*x_2*y_1^2+2*x_0*x_2^2*y_1^2+2*x_1*x_2^2*y_1^2+x_2^3*y_1^2);",
"Y = projectiveVariety ideal(x_0*y_0+x_1*y_0+x_2*y_0+x_0*y_1+x_1*y_1+x_2*y_1);", 
///Z = X \\ Y;///,
///assert(Z == (X \ Y) \ Y)///},
SeeAlso => {(symbol \,MultiprojectiveVariety,MultiprojectiveVariety),(symbol +,MultiprojectiveVariety,MultiprojectiveVariety),(quotient,Ideal,Ideal)}}

document {Key => {(support,MultiprojectiveVariety)}, 
Headline => "support of a multi-projective variety", 
Usage => "support X", 
Inputs => {MultiprojectiveVariety => "X"}, 
Outputs => {MultiprojectiveVariety => {"the support of ",TT"X",", that is, the projective variety defined by the ",TO2{(radical,Ideal),"radical"}," of the defining ",TO2{(ideal,MultiprojectiveVariety),"ideal"}," of ",TT"X"}},
EXAMPLE {"K = ZZ/65521;",
"X = 2 * PP_K^(1,3);",
"degree X, sectionalGenus X",
"X' = support X;",
"degree X', sectionalGenus X'",
///assert(X \ X' == X')///},
SeeAlso => {(decompose,MultiprojectiveVariety),(radical,Ideal)}}

document {Key => {(top,MultiprojectiveVariety)}, 
Headline => "union of the top dimensional components of a multi-projective variety", 
Usage => "top X", 
Inputs => {MultiprojectiveVariety => "X"}, 
Outputs => {MultiprojectiveVariety => {"the union of the top dimensional components of ",TT"X"}},
EXAMPLE {"K = ZZ/65521;",
"X = (linearSpan {point PP_K^4,point PP_K^4}) + (point PP_K^4);",
"top X",
///assert(top top X === top X)///},
SeeAlso => {(decompose,MultiprojectiveVariety),(top,Ideal)}}

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
phi' = check rationalMap({p_0,p_1},projectiveVariety source phi);",
"-- second natural morphism
psi' = check rationalMap({p_2},projectiveVariety source psi);",
"assert(phi' * phi == psi' * psi)"},
SeeAlso => {(symbol **,MultiprojectiveVariety,MultiprojectiveVariety),(symbol ^**,MultirationalMap,MultiprojectiveVariety)}}

document { 
Key => {(euler,MultiprojectiveVariety)}, 
Headline => "topological Euler characteristic of a (smooth) multi-projective variety", 
Usage => "euler X
euler(X,Verify=>b)", 
Inputs => { 
MultiprojectiveVariety => "X" => {"which is assumed to be smooth, and ",TT"b"," is a ",TO2{Boolean,"boolean value"},", that is, ",TT"true"," or ",TT"false"," (the default value is ",TT"true",")"}}, 
Outputs => { 
ZZ => {"the topological Euler characteristics of the variety ",TT"X",", generally calculated as ",TO EulerCharacteristic,TT"(ideal X,MathMode=>b)"}},
EXAMPLE {
"X = PP_QQ^(2,2); -- Veronese surface",
"euler X",
"X4 = X^4;",
"euler X4"},
SeeAlso => {EulerCharacteristic,(euler,ProjectiveVariety)}}

document {Key => {MultirationalMap}, 
Headline => "the class of all multi-rational maps", 
PARA {"A ",EM"multi-rational map"," is a rational map between ",TO2{MultiprojectiveVariety,"multi-projective varieties"},", ",TEX///$$\Phi:X\subseteq \mathbb{P}^{r_1}\times\mathbb{P}^{r_2}\times\cdots\times\mathbb{P}^{r_n}\dashrightarrow Y \subseteq \mathbb{P}^{s_1}\times\mathbb{P}^{s_2}\times\cdots\times\mathbb{P}^{s_m} .$$///,"Thus, it can be represented by an ",TO2{List,"ordered list"}," of ",TO2{RationalMap,"rational maps"},TEX///$$\Phi_i = (\Phi:X\dashrightarrow Y)\circ(pr_i:Y\to Y_i\subseteq\mathbb{P}^{s_i}) ,$$///,"for ",TEX///$i=1,\ldots,m$///,". The maps ",TEX///$\Phi_i:X\dashrightarrow Y_i\subseteq\mathbb{P}^{s_i}$///,", since the target ",TEX///$Y_i$///," is a standard projective variety, are implemented with the class ",TO RationalMap," (more properly, when ",TEX///$n>1$///," the class of such maps is called ",TT "MultihomogeneousRationalMap","). Recall that the main constructor for the class ",TO RationalMap," (as well as for the class ", TT"MultihomogeneousRationalMap",") is the method ",TO rationalMap,"."},
PARA {"The constructor for the class of multi-rational maps is ",TO multirationalMap,", which can often be abbreviated to ",TO2{(rationalMap,List,MultiprojectiveVariety),"rationalMap"}," (see also ",TO "shortcuts","). It takes as input the list of maps ",TEX///$\{\Phi_1:X\dashrightarrow Y_1,\ldots,\Phi_m:X\dashrightarrow Y_m\}$///,", together with the variety ",TEX///$Y$///,", and returns the map ",TEX///$\Phi:X\dashrightarrow Y$///,"."},
Subnodes => {TO multirationalMap,TO (rationalMap,List,MultiprojectiveVariety)}}

document { 
Key => {multirationalMap, (multirationalMap,List,MultiprojectiveVariety), (multirationalMap,List)}, 
Headline => "the multi-rational map defined by a list of rational maps", 
Usage => "multirationalMap Phi
multirationalMap(Phi,Y)", 
Inputs => { "Phi" => {ofClass List," of ",TO2{RationalMap,"rational maps"},", ",TEX///$\{\Phi_1:X\dashrightarrow Y_1\subseteq\mathbb{P}^{s_1},\ldots,\Phi_m:X\dashrightarrow Y_m\subseteq\mathbb{P}^{s_m}\}$///,", all having the same ",TO2{(source,RationalMap),"source"}," ",TEX///$X\subseteq \mathbb{P}^{r_1}\times\mathbb{P}^{r_2}\times\cdots\times\mathbb{P}^{r_n}$///},
"Y" => {ofClass MultiprojectiveVariety," ",TEX///$Y \subseteq \mathbb{P}^{s_1}\times\mathbb{P}^{s_2}\times\cdots\times\mathbb{P}^{s_m}$///," (if omitted, then the ",TO2{(symbol **,MultiprojectiveVariety,MultiprojectiveVariety),"product"}," ",TEX///$Y_1\times\cdots \times Y_m$///," is taken)"}},
Outputs => {MultirationalMap => {"the unique rational map ",TEX///$\Phi:X\subseteq \mathbb{P}^{r_1}\times\mathbb{P}^{r_2}\times\cdots\times\mathbb{P}^{r_n}\dashrightarrow Y \subseteq \mathbb{P}^{s_1}\times\mathbb{P}^{s_2}\times\cdots\times\mathbb{P}^{s_m}$///," such that ",TEX///$pr_i\circ\Phi = \Phi_i$///,", where ",TEX///$pr_i:Y\subseteq \mathbb{P}^{s_1}\times\mathbb{P}^{s_2}\times\cdots\times\mathbb{P}^{s_m} \to Y_i\subseteq \mathbb{P}^{s_i}$///," denotes the i-th projection"}},
EXAMPLE {
"R = ring PP_(ZZ/65521)^{2,1};", 
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
"Z = (image multirationalMap {f,g}) ** target h;",
"Psi = multirationalMap({f,g,h},Z)",
"assert(image Psi == image Phi)"},
SeeAlso => {(rationalMap,List,MultiprojectiveVariety),(graph,MultirationalMap),(image,MultirationalMap),(baseLocus,MultirationalMap),(inverse,MultirationalMap),"shortcuts",rationalMap},
Caveat => {"Be careful when you pass the target ",TT"Y"," as input, because it must be compatible with the maps but for efficiency reasons a full check is not done automatically. See ",TO (check,MultirationalMap),"."}}

document { 
Key => {(rationalMap,List,MultiprojectiveVariety),(rationalMap,MultirationalMap)}, 
Headline => "the multi-rational map defined by a list of rational maps", 
Usage => "rationalMap Phi
rationalMap(Phi,Y)", 
Inputs => { "Phi" => {"whose elements are either ",TO2{RationalMap,"rational maps"}," or representatives of them (e.g., ",TO2{Matrix,"row matrices"}," or ",TO2{RingMap,"ring maps"},")"},
"Y" => MultiprojectiveVariety => {"optional"}},
Outputs => {MultirationalMap => {"the same as ",TO "multirationalMap",TT"(Phi,Y)",", or ",TO "multirationalMap",TT" Phi"," (if ",TT"Y"," is not specified)"}},
SeeAlso => {multirationalMap,rationalMap}}

document { 
Key => {(check,MultirationalMap)}, 
Headline => "check that a multi-rational map is well-defined", 
Usage => "check Phi", 
Inputs => {MultirationalMap}, 
Outputs => {MultirationalMap => {"the same object passed as input, but an error is thrown if the target of the map is not compatible."}},
EXAMPLE {
"f = rationalMap ideal PP_(ZZ/65521)^(1,4);",
"Phi = rationalMap {f}",
"check Phi",
"Y = image Phi",
"Psi = rationalMap({f},Y)",
"check Psi",
"p = point Y;",
"Eta = rationalMap({f},p);",
"stopIfError = false;",
"check Eta"},
SeeAlso => (isWellDefined,MultirationalMap)}

document { 
Key => {(isWellDefined,MultirationalMap)}, 
Headline => "whether a multi-rational map is well-defined", 
Usage => "isWellDefined Phi", 
Inputs => {MultirationalMap}, 
Outputs => {Boolean => {"whether ",TT"Phi"," is a well-defined map"}},
EXAMPLE {
"f = rationalMap ideal PP_(ZZ/65521)^(1,4);",
"Phi = rationalMap {f}",
"isWellDefined Phi",
"Y = image Phi",
"Psi = rationalMap({f},Y)",
"isWellDefined Psi",
"p = point Y;",
"Eta = rationalMap({f},p);",
"isWellDefined Eta"},
SeeAlso => (check,MultirationalMap)}

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
EXAMPLE lines ///ZZ/33331[t_0..t_2,u_0..u_1,Degrees=>{3:{1,0},2:{0,1}}];
f0 = rationalMap {t_0,t_1,t_2}
f1 = rationalMap {u_0,u_1}
f2 = rationalMap {t_0*u_1,t_1*u_0}
Phi = rationalMap {f0,f1,f2};
assert(factor Phi === {f0,f1,f2})///,
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
Outputs => {MultiprojectiveVariety => {"the (closure of the) image of ",TT"Phi"}},
PARA{"Note that, instead, the ",TO2{(image,RationalMap),"image"}," of a standard ",TO2{RationalMap,"rational map"}," is the defining ideal of the image (this is done mainly for efficiency reasons)."},
EXAMPLE {
"ZZ/65521[x_0..x_4];",
"f = rationalMap {x_2^2-x_1*x_3, x_1*x_2-x_0*x_3, x_1^2-x_0*x_2, x_0*x_4, x_1*x_4, x_2*x_4, x_3*x_4, x_4^2};",
"g = rationalMap {-x_3^2+x_2*x_4, 2*x_2*x_3-2*x_1*x_4, -3*x_2^2+2*x_1*x_3+x_0*x_4, 2*x_1*x_2-2*x_0*x_3, -x_1^2+x_0*x_2};",
"Phi = rationalMap {f,g};",
"time Z = image Phi;",
"dim Z, degree Z, degrees Z"},
PARA {"Alternatively, the calculation can be performed using the Segre embedding as follows:"},
EXAMPLE {
"time Z' = projectiveVariety (map segre target Phi) image(segre Phi,\"F4\");",
"assert(Z == Z')"},
SeeAlso => {(symbol SPACE,MultirationalMap,MultiprojectiveVariety),(image,RationalMap),(segre,MultirationalMap)}}

document { 
Key => {(symbol SPACE,MultirationalMap,MultiprojectiveVariety),(symbol SPACE,RationalMap,MultiprojectiveVariety)}, 
Headline => "direct image via a multi-rational map", 
Usage => "Phi X", 
Inputs => {MultirationalMap => "Phi", MultiprojectiveVariety => "X" => {"a subvariety of the ",TO2{(source,MultirationalMap),"source"}," of ",TT "Phi"}}, 
Outputs => {MultiprojectiveVariety => {"the (closure of the) direct image of ", TT"X", " via ",TT"Phi"}},
EXAMPLE {
"ZZ/65521[x_0..x_4];",
"f = last graph rationalMap {x_2^2-x_1*x_3, x_1*x_2-x_0*x_3, x_1^2-x_0*x_2, x_0*x_4, x_1*x_4, x_2*x_4, x_3*x_4, x_4^2};",
"Phi = rationalMap {f,f};",
"Z = source Phi;",
"time Phi Z;",
"dim oo, degree oo, degrees oo",
"time Phi (point Z + point Z + point Z)",
"dim oo, degree oo, degrees oo"},
SeeAlso => {(image,MultirationalMap), (symbol ^*, MultirationalMap), (symbol SPACE,RationalMap,Ideal)}}

document { 
Key => {(symbol ^**,MultirationalMap,MultiprojectiveVariety), (symbol ^**,RationalMap,EmbeddedProjectiveVariety), (symbol ^*,MultirationalMap)}, 
Headline => "inverse image via a multi-rational map", 
Usage => "Phi^** Y
Phi^* Y", 
Inputs => { 
MultirationalMap => "Phi",
MultiprojectiveVariety => "Y" => {"a subvariety of the ambient multi-projective space of the ",TO2{(target,MultirationalMap),"target"}," of ",TT "Phi"}}, 
Outputs => { 
MultiprojectiveVariety => {"the (closure of the) inverse image of ", TT"Y", " via ",TT"Phi"}},
EXAMPLE {
"ZZ/300007[x_0..x_3], f = rationalMap {x_2^2-x_1*x_3, x_1*x_2-x_0*x_3, x_1^2-x_0*x_2}, g = rationalMap {x_1^2-x_0*x_2, x_0*x_3, x_1*x_3, x_2*x_3, x_3^2};",
"Phi = last graph rationalMap {f,g};",
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
"Phi = rationalMap {f,g,h};",
"time segre Phi;",
"describe segre Phi"}, 
SeeAlso => {(segre,MultiprojectiveVariety)}}

document {Key => {(parametrize,MultiprojectiveVariety)}, 
Headline => "try to get a parametrization of a multi-projective variety", 
Usage => "parametrize X", 
Inputs => {"X" => MultiprojectiveVariety => {"a rational ",TEX///$k$///,"-dimensional subvariety of ",TEX///$\mathbb{P}^{r_1}\times\cdots\times\mathbb{P}^{r_n}$///}}, 
Outputs => {MultirationalMap => {"a birational map from ",TEX///$\mathbb{P}^k$///," to ",TEX///$X$///," (or an error if it fails)"}}, 
PARA{"Currently, this function works in particular for linear varieties, quadrics, varieties of minimal degree, Grassmannians, Severi varieties, del Pezzo fivefolds, and some types of Fano fourfolds."},
EXAMPLE {"K = ZZ/65521;",
"X = PP_K^{2,4,1,3};",
"f = parametrize X;",
"Y = random({{1,0,0,0},{0,1,0,0},{0,1,0,0},{0,0,0,1}},0_X);",
"g = parametrize Y;",
"Z = random({{1,1,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1},{0,0,0,1}},0_X);",
"h = parametrize Z;",
"describe h",
"describe inverse h",
"A = matrix pack(5,for i to 24 list random(1,ring PP_K^8)), A = A - transpose A;",
"W = projectiveVariety pfaffians(4,A);",
"parametrize W",
"parametrize (W ** (point W))"}, 
SeeAlso => {(inverse,MultirationalMap),(symbol ===>,EmbeddedProjectiveVariety,EmbeddedProjectiveVariety)}}

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
"Phi = rationalMap(PP_(ZZ/333331)^(1,4),Dominant=>true)",
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
Key => {(symbol *,MultirationalMap,MultirationalMap),(compose,MultirationalMap,MultirationalMap),(symbol *,RationalMap,MultirationalMap),(symbol *,MultirationalMap,RationalMap),(symbol ^,MultirationalMap,ZZ)}, 
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
"Psi = last graph rationalMap(projectiveVariety ideal(x_4,x_2^2-x_1*x_3,x_1*x_2-x_0*x_3,x_1^2-x_0*x_2),Dominant=>true);",
"Phi = first graph Psi;",
"Eta = Phi * Psi;",
"assert(Eta == last graph Psi);"},
SeeAlso => {(symbol *,RationalMap,RationalMap)}}

document { 
Key => {(symbol ==,MultirationalMap,MultirationalMap),(symbol ==,RationalMap,MultirationalMap),(symbol ==,MultirationalMap,RationalMap),(symbol ==,MultirationalMap,ZZ),(symbol ==,ZZ,MultirationalMap)}, 
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
"Phi = last graph rationalMap PP_(ZZ/300007)^(1,4);",
"for i in {4,3,2,1,0} list time multidegree(i,Phi)",
"time assert(oo == multidegree Phi)"},
SeeAlso => {(multidegree,MultirationalMap),(projectiveDegrees,RationalMap),(degree,MultirationalMap,Option),(symbol ^*,MultirationalMap)},
References => {"ArXiv preprint: ",HREF{"https://arxiv.org/abs/2101.04503","Computations with rational maps between multi-projective varieties"},"."}}

document { 
Key => {(multidegree,MultirationalMap),(projectiveDegrees,MultirationalMap)}, 
Headline => "projective degrees of a multi-rational map", 
Usage => "multidegree Phi", 
Inputs => { 
MultirationalMap => "Phi"}, 
Outputs => { 
List => {"the list of projective degrees of ",TT"Phi"}},
PARA{"This calculates the ",TO2{(multidegree,MultiprojectiveVariety),"multidegree"}," of the ",TO2{(graph,MultirationalMap),"graph"}," and converts it to the list of projective degrees."},
EXAMPLE {
"ZZ/300007[x_0..x_3], f = rationalMap {x_2^2-x_1*x_3, x_1*x_2-x_0*x_3, x_1^2-x_0*x_2}, g = rationalMap {x_1^2-x_0*x_2, x_0*x_3, x_1*x_3, x_2*x_3, x_3^2};",
"Phi = last graph rationalMap {f,g};",
"time multidegree Phi",
"(degree source Phi,degree image Phi)"},
SeeAlso => {(multidegree,ZZ,MultirationalMap),(multidegree,RationalMap),(degree,MultirationalMap),(projectiveDegrees,RationalMap)},
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
"Phi = (last graph multirationalMap rationalMap transpose jacobian(-x_2^3+2*x_1*x_2*x_3-x_0*x_3^2-x_1^2*x_4+x_0*x_2*x_4))||projectiveVariety ideal(random(2,R));",
"? Phi",
"time degree(Phi,Strategy=>\"random point\")",
"time degree(Phi,Strategy=>\"0-th projective degree\")",
"time degree Phi"},
PARA{"Note, as in the example above, that calculation times may vary depending on the strategy used."},
SeeAlso => {(degree,MultirationalMap),(degreeMap,RationalMap),(multidegree,ZZ,MultirationalMap),(point,MultiprojectiveVariety),(symbol ^*,MultirationalMap)}}

document { 
Key => {(degree,MultirationalMap),(degreeMap,MultirationalMap)}, 
Headline => "degree of a multi-rational map", 
Usage => "degree Phi", 
Inputs => { 
MultirationalMap => "Phi"}, 
Outputs => { 
ZZ => {"the degree of ",TT"Phi",". So this value is 1 if and only if the map is birational onto its image."}},
PARA{"This is just a shortcut for ",TT"(last multidegree Phi)/(degree image Phi)","."},
EXAMPLE {
"ZZ/300007[x_0..x_3], f = rationalMap {x_2^2-x_1*x_3, x_1*x_2-x_0*x_3, x_1^2-x_0*x_2}, g = rationalMap {x_1^2-x_0*x_2, x_0*x_3, x_1*x_3, x_2*x_3, x_3^2};",
"Phi = last graph rationalMap {f,g};",
"time degree Phi"},
SeeAlso => {(degree,MultirationalMap,Option),(degree,RationalMap),(multidegree,MultirationalMap),(degreeMap,RationalMap)}}

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
"Phi = rationalMap {f,g};",
"time isMorphism Phi",
"time Psi = first graph Phi;",
"time isMorphism Psi",
"assert((not o3) and o5)"},
SeeAlso => {(isIsomorphism,MultirationalMap),(isMorphism,RationalMap)}}

document { 
Key => {(multirationalMap,MultiprojectiveVariety),(symbol _,ZZ,MultiprojectiveVariety)}, 
Headline => "identity map", 
Usage => "1_X
multirationalMap X", 
Inputs => {MultiprojectiveVariety => "X"}, 
Outputs => {MultirationalMap => {"the identity map on ",TT"X"}},
EXAMPLE {"X = PP_QQ^{2,3,1};", "1_X;"},
PARA {"The command ",TT "0_X"," returns instead the empty subscheme of ",TT "X","."},
EXAMPLE {"0_X;"},
SeeAlso => {(multirationalMap,MultiprojectiveVariety,MultiprojectiveVariety)}}

document { 
Key => {(multirationalMap,MultiprojectiveVariety,MultiprojectiveVariety),(rationalMap,MultiprojectiveVariety,MultiprojectiveVariety)}, 
Headline => "get the natural inclusion", 
Usage => "rationalMap(X,Y)
multirationalMap(X,Y)", 
Inputs => {MultiprojectiveVariety => "X",MultiprojectiveVariety => "Y" => {"with ",TEX///$X\subseteq Y$///," (after identifying the ambient spaces)"}}, 
Outputs => {MultirationalMap => {"the natural inclusion of ",TEX///$X$///," into ",TEX///$Y$///}},
EXAMPLE {
"R = ZZ/101[a_0,a_1,b_0..b_2,Degrees=>{2:{1,0},3:{0,1}}], S = ZZ/101[c_0,c_1,d_0..d_2,Degrees=>{2:{1,0},3:{0,1}}]",
"I = ideal (random({0,1},R),random({1,1},R)), J = sub(I,vars S)",
"X = projectiveVariety I, Y = projectiveVariety J",
"rationalMap(X,ambient X);",
"rationalMap(X,Y);",
"stopIfError = false;",
"rationalMap(ambient X,X)"},
SeeAlso => {(symbol _,ZZ,MultiprojectiveVariety)}}

document { 
Key => {(multirationalMap,MultirationalMap,MultiprojectiveVariety),(rationalMap,MultirationalMap,MultiprojectiveVariety)}, 
Headline => "change the target of a multi-rational map", 
Usage => "rationalMap(Phi,Y)
multirationalMap(Phi,Y)
check rationalMap(Phi,Y)", 
Inputs => {MultirationalMap => "Phi",MultiprojectiveVariety => "Y" => {"which must be compatible with ",TT"Phi"}}, 
Outputs => {MultirationalMap => {"defined in the same way as ",TT"Phi"," but with ",TT"Y"," as target"}},
EXAMPLE {
"Phi = rationalMap {super specialQuadraticTransformation 1}",
"Y = image Phi",
"Psi = rationalMap(Phi,Y)"},
SeeAlso => {(check,MultirationalMap),(symbol <<,MultirationalMap,MultiprojectiveVariety)}}

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
Phi = rationalMap PP_(ZZ/65521)^(1,4);",
"-- we see Phi as a dominant map
Phi = rationalMap(Phi,image Phi);",
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
"K = ZZ/10000019;",
"-- map defined by the cubics through the secant variety to the rational normal curve of degree 6
Phi = multirationalMap rationalMap(ring PP_K^6,ring GG_K(2,4),gens ideal PP_K([6],2));",
"time Psi = inverse2 Phi;",
"assert(Phi * Psi == 1)", 
"Phi' = Phi || Phi;",
"time Psi' = inverse2 Phi';",
"assert(Phi' * Psi' == 1)"},
SeeAlso => {(inverse,MultirationalMap),(symbol <==>,MultirationalMap,MultirationalMap)}}

document { 
Key => {(isIsomorphism,MultirationalMap)}, 
Headline => "whether a birational map is an isomorphism", 
Usage => "isIsomorphism Phi", 
Inputs => {"Phi" => MultirationalMap}, 
Outputs => {Boolean => {"whether ",TT"Phi"," is an isomorphism"}},
EXAMPLE { 
"-- map defined by the quadrics through a twisted cubic curve
ZZ/33331[a..d]; f = rationalMap {c^2-b*d,b*c-a*d,b^2-a*c};",
"Phi = rationalMap {f,f};",
"time isIsomorphism Phi",
"Psi = first graph Phi;",
"time isIsomorphism Psi",
"Eta = first graph Psi;",
"time isIsomorphism Eta",
"assert(o8 and (not o6) and (not o4))"},
SeeAlso => {(inverse,MultirationalMap),(isMorphism,MultirationalMap)}}

document { 
Key => {baseLocus,(baseLocus,MultirationalMap),(baseLocus,RationalMap)}, 
Headline => "the base locus of a multi-rational map", 
Usage => "baseLocus Phi", 
Inputs => {MultirationalMap => "Phi"}, 
Outputs => {MultiprojectiveVariety => {"the base locus of ",TT"Phi",", that is, the locus where it is not defined"}},
EXAMPLE lines ///t = gens ring PP_(ZZ/33331)^5;
Phi = rationalMap {rationalMap {t_0,t_1,t_2},rationalMap {t_3,t_4,t_5}};
X = baseLocus Phi;
describe X
Psi = inverse(Phi|random(3,baseLocus Phi));
Y = baseLocus Psi;
describe Y///,
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
Key => {(symbol |,MultirationalMap,MultiprojectiveVariety),(symbol |,RationalMap,MultiprojectiveVariety),(symbol |,MultirationalMap,List)}, 
Headline => "restriction of a multi-rational map", 
Usage => "Phi | Z", 
Inputs => {MultirationalMap => "Phi" => { TEX///$\Phi:X \dashrightarrow Y$///},
MultiprojectiveVariety => "Z" => {"a subvariety of ",TEX///$X$///}}, 
Outputs => {MultirationalMap => {"the restriction of ",TEX///$\Phi$///," to ",TEX///$Z$///,", ",TEX///$\phi|_{Z}: Z \dashrightarrow Y$///}}, 
EXAMPLE {
"ZZ/33331[x_0..x_3], f = rationalMap {x_2^2-x_1*x_3,x_1*x_2-x_0*x_3,x_1^2-x_0*x_2}, g = rationalMap {x_2^2-x_1*x_3,x_1*x_2-x_0*x_3};",
"Phi = last graph rationalMap {f,g};",
"Z = (source Phi) * projectiveVariety ideal random({1,1,2},ring ambient source Phi);",
"Phi' = Phi|Z;",
"source Phi'",
"assert(image Phi' == Phi Z)"},
PARA{"The following is a shortcut to take restrictions on random hypersurfaces as above."},
EXAMPLE {"Phi|{1,1,2};"},
SeeAlso => {(symbol ||,MultirationalMap,MultiprojectiveVariety),(symbol |,RationalMap,Ideal),(symbol *,MultiprojectiveVariety,MultiprojectiveVariety)}}

document { 
Key => {(symbol ||,MultirationalMap,MultiprojectiveVariety),(symbol ||,RationalMap,MultiprojectiveVariety),(symbol ||,MultirationalMap,List)}, 
Headline => "restriction of a multi-rational map", 
Usage => "Phi || Z", 
Inputs => {MultirationalMap => "Phi" => { TEX///$\Phi:X \dashrightarrow Y$///},
MultiprojectiveVariety => "Z" => {"a subvariety of ",TEX///$Y$///}}, 
Outputs => { 
MultirationalMap => {"the restriction of ",TEX///$\Phi$///," to ",TEX///${\Phi}^{(-1)} Z$///,", ",TEX///${{\Phi}|}_{{\Phi}^{(-1)} Z}: {\Phi}^{(-1)} Z \dashrightarrow Z$///}}, 
EXAMPLE {
"ZZ/33331[x_0..x_3], f = rationalMap {x_2^2-x_1*x_3,x_1*x_2-x_0*x_3,x_1^2-x_0*x_2}, g = rationalMap {x_2^2-x_1*x_3,x_1*x_2-x_0*x_3};",
"Phi = last graph rationalMap {f,g};",
"Z = projectiveVariety ideal random({1,2},ring target Phi);",
"Phi' = Phi||Z;",
"target Phi'",
"assert(source Phi' == Phi^* Z)"},
PARA{"The following is a shortcut to take restrictions on random hypersurfaces as above."},
EXAMPLE {"Phi||{1,2};"},
SeeAlso => {(symbol |,MultirationalMap,MultiprojectiveVariety),(symbol ||,RationalMap,Ideal),(symbol ^*,MultirationalMap)}}

document { 
Key => {(symbol |,MultirationalMap,MultirationalMap),(symbol |,RationalMap,MultirationalMap),(symbol |,MultirationalMap,RationalMap),(symbol |,RationalMap,RationalMap)}, 
Headline => "product of multi-rational maps", 
Usage => "Phi | Psi", 
Inputs => {MultirationalMap => "Phi" => { TEX///$\Phi:X \dashrightarrow Y$///},
MultirationalMap => "Psi" => { TEX///$\Psi:X \dashrightarrow Z$///}}, 
Outputs => {MultirationalMap => {"the rational map ",TEX///$X \dashrightarrow Y\times Z$///," defined by ",TEX///$p\mapsto (\Phi(p),\Psi(p))$///,"; in other words, it is the map defined by the ",TO2{(symbol |,List,List),"join"}," of ",TO2{(factor,MultirationalMap),"factor"},TT" Phi"," with ",TO2{(factor,MultirationalMap),"factor"},TT" Psi"}}, 
EXAMPLE {
"Phi = rationalMap({veronese(1,2,ZZ/33331)},Dominant=>true);",
"Psi = rationalMap {veronese(1,3,ZZ/33331)};",
"(X,Y,Z) = (source Phi,target Phi,target Psi);",
"Eta = Phi | Psi;",
"Eta | Phi;",
"Phi | Psi | Eta;",
"super oo;",
"rationalMap(oo,image oo);"},
SeeAlso => {(symbol ||,MultirationalMap,MultirationalMap),(symbol |,List,List),(factor,MultirationalMap),(symbol *,MultiprojectiveVariety,MultiprojectiveVariety),(super,MultirationalMap)}}

document { 
Key => {(symbol ||,MultirationalMap,MultirationalMap),(symbol ||,MultirationalMap,RationalMap),(symbol ||,RationalMap,MultirationalMap),(symbol ||,RationalMap,RationalMap)}, 
Headline => "product of multi-rational maps", 
Usage => "Phi || Psi", 
Inputs => {MultirationalMap => "Phi" => { TEX///$\Phi:X \dashrightarrow Y$///},
MultirationalMap => "Psi" => { TEX///$\Psi:Z \dashrightarrow W$///}}, 
Outputs => {MultirationalMap => {"the rational map ",TEX///$\Phi\times\Psi:X\times Z \dashrightarrow Y\times W$///," defined by ",TEX///$\Phi\times\Psi(p,q) = (\Phi(p),\Psi(q))$///}}, 
EXAMPLE {
"Phi = rationalMap({veronese(1,4,ZZ/33331)},Dominant=>true);",
"Psi = last graph rationalMap PP_(ZZ/33331)^(1,3);",
"(X,Y,Z,W) = (source Phi,target Phi,source Psi,target Psi);",
"Eta = Phi || Psi;",
"Psi || Eta;",
"Psi || Eta || Phi;",
"assert(oo == (Psi || Eta) || Phi and (Psi || Eta) || Phi == Psi || (Eta || Phi))"},
SeeAlso => {(symbol |,MultirationalMap,MultirationalMap)}}

document { 
Key => {(super,MultirationalMap)}, 
Headline => "get the multi-rational map whose target is a product of projective spaces", 
Usage => "super Phi", 
Inputs => {MultirationalMap => "Phi" => {"whose target is a subvariety ",TEX///$Y\subseteq\mathbb{P}^{k_1}\times\cdots\times\mathbb{P}^{k_n}$///}}, 
Outputs => {MultirationalMap => {"the composition of ",TT"Phi"," with the inclusion of ",TEX///$Y$///," into ",TEX///$\mathbb{P}^{k_1}\times\cdots\times\mathbb{P}^{k_n}$///}},
EXAMPLE {
"Phi = rationalMap{rationalMap(veronese(1,2,ZZ/33331),Dominant=>true),rationalMap(veronese(1,3,ZZ/33331),Dominant=>true)};",
"super Phi;",
"Psi = rationalMap(Phi,image Phi);",
"super Psi == super Phi"},
SeeAlso => {(target,MultirationalMap),(ambient,MultiprojectiveVariety),(super,RationalMap)}}

document { 
Key => {(trim,MultirationalMap),(trim,RationalMap)}, 
Headline => "trim the target of a multi-rational map", 
Usage => "trim Phi", 
Inputs => {MultirationalMap => "Phi" => {"from ",ofClass MultiprojectiveVariety," ",TEX///$X$///," to ",TEX///$\mathbb{P}^{k_1}\times\cdots\times\mathbb{P}^{k_n}$///}}, 
Outputs => {MultirationalMap => {"from ",TEX///$X$///," to ",TEX///$\mathbb{P}^{s_1}\times\cdots\times\mathbb{P}^{s_n}$///,", with ",TEX///$s_i\leq k_i$///,", which is isomorphic to the original map, but whose image is not contained in any hypersurface of multidegree ",TEX///$(d_1,\ldots,d_n)$///," with ",TEX///$\sum_{i=1}^n d_i = 1$///}},
EXAMPLE {
"K = ZZ/33331; C = PP_K^(1,4); -- rational normal quartic curve",
"Phi = rationalMap C; -- map defined by the quadrics through C",
"Q = random(2,C); -- random quadric hypersurface through C",
"Phi = Phi|Q;",
"image Phi",
"Psi = trim Phi;",
"image Psi",
"Phi || Phi || Psi;",
"image oo",
"trim (Phi || Phi || Psi);",
"image oo"}}

document { 
Key => {(random,List,MultiprojectiveVariety),(random,ZZ,MultiprojectiveVariety)}, 
Headline => "get a random hypersurface of given multi-degree containing a multi-projective variety", 
Usage => "random(d,X)", 
Inputs => {List => "d" => {"a list of ",TEX///$n$///," nonnegative integers"},
MultiprojectiveVariety => "X" => {"a subvariety of ",TEX///$\mathbb{P}^{k_1}\times\cdots\times\mathbb{P}^{k_n}$///}}, 
Outputs => {MultiprojectiveVariety => {"a random hypersurface in ",TEX///$\mathbb{P}^{k_1}\times\cdots\times\mathbb{P}^{k_n}$///," of multi-degree ",TEX///$d$///," containing ",TEX///$X$///}},
PARA{"More generally, if ",TT"d"," is a list of multi-degrees, then the output is the intersection of the hypersurfaces ",TT "random(d_i,X)","."},
EXAMPLE {
"X = PP_(ZZ/65521)^(1,3); -- twisted cubic curve",
"random({2},X);",
"ideal oo",
"random({{2},{2}},X);",
"ideal oo",
"X = X^2;",
"random({1,2},X);",
"ideal oo",
"random({{1,2},{1,2},{2,0}},X);",
"degrees oo"},
SeeAlso => {(random,MultiprojectiveVariety)}}

document { 
Key => {(random,MultiprojectiveVariety)}, 
Headline => "apply a random automorphism of the ambient multi-projective space", 
Usage => "random X", 
Inputs => {MultiprojectiveVariety => "X" => {"a subvariety of ",TEX///$\mathbb{P}^{k_1}\times\cdots\times\mathbb{P}^{k_n}$///}}, 
Outputs => {MultiprojectiveVariety => {"the image of ",TEX///$X$///," under the action of a random element of ",TEX///$\mathrm{Aut}(\mathbb{P}^{k_1})\times\cdots\times\mathrm{Aut}(\mathbb{P}^{k_n})$///}},
EXAMPLE {
"K = ZZ/65521;",
"X = PP_K^({1,1},{2,3});",
"ideal X",
"Y = random X;",
"ideal Y"},
SeeAlso => {(random,List,MultiprojectiveVariety),(permute,MultiprojectiveVariety,List),(symbol ===>,EmbeddedProjectiveVariety,EmbeddedProjectiveVariety)}}

document { 
Key => {(permute,MultiprojectiveVariety,List)}, 
Headline => "permute the factors of the ambient multi-projective space", 
Usage => "permute(X,s)", 
Inputs => {"X" => MultiprojectiveVariety => {"a subvariety of ",TEX///$\mathbb{P}^{k_0}\times\mathbb{P}^{k_1}\times\cdots\times\mathbb{P}^{k_{n}}$///},
"s" => List => {"a permutation of the set ",TEX///$\{0,1,\ldots,n\}$///}},
Outputs => {MultirationalMap => {"an isomorphism from ",TEX///$X$///," to a subvariety of ",TEX///$\mathbb{P}^{k_{s(0)}}\times\mathbb{P}^{k_{s(1)}}\times\cdots\times\mathbb{P}^{k_{s(n)}}$///}},
EXAMPLE {
"X = PP_(ZZ/33331)^{2,3,1};",
"f = permute(X,{1,0,2});",
"assert isIsomorphism f",
"Y = random({0,1,1},0_X);",
"g = permute(Y,{2,0,1});",
"assert isIsomorphism g"},
SeeAlso => {(permute,MultidimensionalMatrix,List)}}

document { 
Key => {(shape,MultiprojectiveVariety)}, 
Headline => "shape of the ambient multi-projective space of a multi-projective variety", 
Usage => "shape X", 
Inputs => {"M" => MultiprojectiveVariety => {"a subvariety of ",TEX///$\mathbb{P}^{k_1}\times\cdots\times\mathbb{P}^{k_{n}}$///}},
Outputs => {List => {"the list of integers ",TEX///$\{k_1, \ldots, k_n\}$///}},
EXAMPLE {
"X = PP_(ZZ/65521)^{2,3,1};",
"shape X",
"p = point X;",
"shape p"},
SeeAlso => {(shape,MultidimensionalMatrix)}}

document { 
Key => {(show,MultirationalMap),(show,RationalMap)}, 
Headline => "display a multi-rational map", 
Usage => "show Phi", 
Inputs => {"Phi" => MultirationalMap}, 
Outputs => {Net => {"a net of ",TT"Phi"}},
EXAMPLE { 
"Phi = inverse first graph last graph rationalMap PP_(ZZ/33331)^(1,3)",
"time describe Phi",
"show Phi"},
SeeAlso => {(describe,MultirationalMap)}}

document { 
Key => {degreeSequence,(degreeSequence,MultirationalMap),(degreeSequence,RationalMap)}, 
Headline => "the (multi)-degree sequence of a (multi)-rational map", 
Usage => "degreeSequence Phi", 
Inputs => {"Phi" => MultirationalMap}, 
Outputs => {List => {"the list of the degree sequences for the rational maps returned by ",TO2{(factor,MultirationalMap),"factor"},TT" Phi","."}},
EXAMPLE { 
"Phi = inverse first graph last graph rationalMap PP_(ZZ/33331)^(1,3);",
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
"Phi = multirationalMap graph rationalMap PP_(ZZ/65521)^(1,4);",
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
"X = source graph rationalMap PP_(ZZ/65521)^(1,3);",
"? X",
"describe X",
"? image segre X"},
SeeAlso => {(describe,MultirationalMap)}}

document { 
Key => {(symbol !,EmbeddedProjectiveVariety)}, 
Headline => "print a more detailed description of an embedded projective variety", 
Usage => "X!", 
Inputs => {"X" => EmbeddedProjectiveVariety},
EXAMPLE lines ///
K = ZZ/333331; K[t_0..t_5];
X = projectiveVariety ideal(t_4^2-t_3*t_5,t_2*t_4-t_1*t_5,t_2*t_3-t_1*t_4,t_2^2-t_0*t_5,t_1*t_2-t_0*t_4,t_1^2-t_0*t_3);
X!
K[x_0..x_7];
X = projectiveVariety ideal(x_5*x_6-x_4*x_7,x_4*x_6-x_3*x_7,x_2*x_6-x_1*x_7,x_1*x_6-x_0*x_7,x_4^2-x_3*x_5,x_2*x_4-x_1*x_5,x_1*x_4-x_0*x_5,x_2*x_3-x_0*x_5,x_1*x_3-x_0*x_4,x_1^2-x_0*x_2);
X!
(random({2},X))!
(random({{2},{2}},X))!///,
SeeAlso => {(describe,MultiprojectiveVariety),(symbol ?,MultiprojectiveVariety)}}

document { 
Key => {(clean,MultirationalMap),(clean,RationalMap)}, 
Headline => "clean the internal information of a multi-rational map", 
Usage => "clean Phi", 
Inputs => {"Phi" => MultirationalMap}, 
Outputs => {MultirationalMap => {"which is identical to ",TT"Phi",", but new to the system"}},
PARA{"This is only useful for comparing computation times for various algorithms."},
EXAMPLE {"Phi = 1_(PP_QQ^2);", "Psi = clean Phi;", "Phi == Psi", "Phi === Psi"}}

document { 
Key => {(symbol **,MultirationalMap,Ring)},
Headline => "change the coefficient ring of a multi-rational map", 
Usage => "Phi ** K", 
Inputs => {MultirationalMap => "Phi" => {"defined over a coefficient ring ",TT"F"},
Ring => "K" => {"the new coefficient ring (which must be a field)"}}, 
Outputs => {MultirationalMap => {"a multi-rational map defined over ",TT"K",", obtained by coercing the coefficients of the multi-forms defining ",TT"Phi", " into ",TT"K"}}, 
PARA {"It is necessary that all multi-forms in the old coefficient ring ",TT"F"," can be automatically coerced into the new coefficient ring ",TT"K","."},
EXAMPLE {
"Phi = inverse first graph rationalMap PP_QQ^(2,2);",
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
"use ring PP_QQ^{2,3};",
"X = projectiveVariety ideal(x1_2^2-x1_1*x1_3,x1_1*x1_2-x1_0*x1_3,x1_1^2-x1_0*x1_2,x0_1^2-x0_0*x0_2);",
"ideal X",
"K = ZZ/65521;",
"X' = X ** K;",
"ideal X'"},
SeeAlso => {(coefficientRing,MultiprojectiveVariety),(symbol **,MultirationalMap,Ring)}}

document { 
Key => {"shortcuts",(rationalMap,MultiprojectiveVariety),(rationalMap,MultiprojectiveVariety,List),(rationalMap,MultiprojectiveVariety,ZZ),(rationalMap,MultiprojectiveVariety,ZZ,ZZ),(multirationalMap,RationalMap)},
Headline => "Some convenient shortcuts for multi-rational maps consisting of a single rational map",
Usage => "rationalMap X <==> multirationalMap {rationalMap ideal X}
rationalMap(X,a) <==> multirationalMap {rationalMap(ideal X,a)}
rationalMap(X,a,b) <==> multirationalMap {rationalMap(ideal X,a,b)}
multirationalMap f <==> multirationalMap {f}", 
Inputs => {
"X" => MultiprojectiveVariety, 
"a" => ZZ => {"or ",ofClass List," of integers"},
"b" => ZZ,
"f" => RationalMap},
EXAMPLE {
"X = PP_QQ^(1,3);",
"a = 4, b = 2;",
"phi = rationalMap X;",
"assert(phi <==> multirationalMap {rationalMap ideal X})",
"phi = rationalMap(X,a);",
"assert(phi <==> multirationalMap {rationalMap(ideal X,a)})",
"phi = rationalMap(X,a,b);",
"assert(phi <==> multirationalMap {rationalMap(ideal X,a,b)})"},
PARA{"If you want to consider ",TEX///$X$///," as a subvariety of another multi-projective variety ",TEX///$Y$///,", you may use the command ",TT///X_Y///,". For instance, ",TT///rationalMap(X_Y,a)///," returns the rational map from ",TEX///$Y$///," defined by a basis of the linear system ",TEX///$|H^0(Y,\mathcal{I}_{X\subseteq Y}(a))|$///," (basically, this is equivalent to ",TT"trim((rationalMap(X,a))|Y)",")."},
EXAMPLE {
"Y = random(3,X);",
"rationalMap(X_Y,a);",
"rationalMap X_Y;"},
SeeAlso => {(rationalMap,Ideal),(rationalMap,Ideal,ZZ),(rationalMap,Ideal,ZZ,ZZ),(symbol <==>,MultirationalMap,MultirationalMap),toRationalMap}}

document { 
Key => {toRationalMap,(toRationalMap,MultirationalMap)},
Headline => "convert a multi-rational map consisting of a single rational map to a standard rational map",
Usage => "toRationalMap Phi",
Inputs => {"Phi" => MultirationalMap => {"whose target is ",ofClass EmbeddedProjectiveVariety}},
Outputs => {RationalMap => {"which is mathematically equal to ",TT"Phi"," but represented as an object of the class ",TO RationalMap}},
PARA{"This is useful when you need to use tools from the package ",TO Cremona,"."},
EXAMPLE {
"Phi = rationalMap(PP_QQ^(1,4),Dominant=>true);",
"class Phi",
"f = toRationalMap Phi;",
"class f",
"assert(Phi == f and Phi =!= f)"},
SeeAlso => {(multirationalMap,RationalMap)}}

document {
Key => {(variety,EmbeddedProjectiveVariety)},
Headline => "convert an embedded projective variety into a built-in projective variety",
Usage => "variety X",
Inputs => {"X" => EmbeddedProjectiveVariety},
Outputs => {ProjectiveVariety => {"which is mathematically equal to ",TT"X"}},
EXAMPLE {
"X = PP_QQ^(2,2);",
"class X",
"X' = variety X;",
"class X'",
"assert(ring X === ring X')"}}

document { 
Key => {(dual,EmbeddedProjectiveVariety)},
Headline => "the variety projectively dual to an embedded projective variety", 
Usage => "dual X", 
Inputs => {"X" => EmbeddedProjectiveVariety},
Outputs => {EmbeddedProjectiveVariety => {"which is projectively dual to ",TEX///$X$///}},
EXAMPLE {"X = PP_QQ^(2,2);","X' = dual X;", "describe X'","assert(dual X' == X)"},
SeeAlso => {(conormalVariety,EmbeddedProjectiveVariety),dualVariety,tangentSpace}}

document { 
Key => {linearlyNormalEmbedding,(linearlyNormalEmbedding,EmbeddedProjectiveVariety)},
Headline => "get the linearly normal embedding", 
Usage => "linearlyNormalEmbedding X", 
Inputs => {"X" => EmbeddedProjectiveVariety},
Outputs => {{"an ",TO2{MultirationalMap,"isomorphism"}," from ",TT"X"," to a linearly normal variety, whose inverse is a linear projection"}},
EXAMPLE {"K = ZZ/333331;", 
"X = PP_K^(1,7); -- rational normal curve of degree 7",
"time f = linearlyNormalEmbedding X;",
"Y = (rationalMap {for i to 3 list random(1,ring ambient X)}) X; -- an isomorphic projection of X in PP^3",
"time g = linearlyNormalEmbedding Y;",
"assert(isIsomorphism g)",
"describe g"},
Caveat => {"This is an experimental function."}}

document { 
Key => {linearSpan,(linearSpan,EmbeddedProjectiveVariety),(linearSpan,List)},
Headline => "the linear span of an embedded projective variety", 
Usage => "linearSpan X", 
Inputs => {"X" => EmbeddedProjectiveVariety => {" (resp., a list of ",TO2{EmbeddedProjectiveVariety,"embedded projective varieties"},")"}},
Outputs => {EmbeddedProjectiveVariety => {"the linear span of ",TT"X"," (resp., of the ",TO2{(⋃,List),"union"}," of the members of ",TT"X",")"}},
EXAMPLE {"P = PP_(ZZ/333331)^7;",
"S = apply(3,i -> point P)",
"L = linearSpan ⋃ S;",
"assert(L == linearSpan S)",
"assert(dim L == 2 and degree L == 1)"}}

document { 
Key => {tangentSpace,(tangentSpace,EmbeddedProjectiveVariety,EmbeddedProjectiveVariety)},
Headline => "tangent space to a projective variety at a point", 
Usage => "tangentSpace(X,p)
tangentSpace(p,X)", 
Inputs => {"X" => EmbeddedProjectiveVariety,"p" => EmbeddedProjectiveVariety => {"a point on ",TEX///$X$///}},
Outputs => {EmbeddedProjectiveVariety => {"the embedded tangent space ",TEX///$T_p(X)$///," to ",TEX///$X$///," at the point ",TEX///$p$///}},
EXAMPLE {"X = PP_(ZZ/333331)^(3,2);",
"p := point X",
"tangentSpace(X,p)"},
SeeAlso => {(tangentCone,EmbeddedProjectiveVariety,EmbeddedProjectiveVariety),(singularLocus,MultiprojectiveVariety),(dual,EmbeddedProjectiveVariety),(point,MultiprojectiveVariety)}}

typValTanCone := typicalValues#tangentCone;
typicalValues#tangentCone = EmbeddedProjectiveVariety;
document { 
Key => {(tangentCone,EmbeddedProjectiveVariety,EmbeddedProjectiveVariety)},
Headline => "tangent cone to a projective variety at a point", 
Usage => "tangentCone(X,p)
tangentCone(p,X)", 
Inputs => {"X" => EmbeddedProjectiveVariety,"p" => EmbeddedProjectiveVariety => {"a point on ",TEX///$X$///}},
Outputs => {EmbeddedProjectiveVariety => {"the embedded tangent cone ",TEX///$C_p(X)$///," to ",TEX///$X$///," at the point ",TEX///$p$///}},
EXAMPLE {"Y = random(3,0_(PP_(ZZ/333331)^6)), q = point Y, j = parametrize tangentSpace(Y,q);",
"(X, p) = (j^* Y, j^* q);",
"C = tangentCone(X,p);",
"describe C",
"assert(isSubset(C,tangentSpace(X,p)) and coneOfLines(C,p) == C)"},
SeeAlso => {(tangentSpace,EmbeddedProjectiveVariety,EmbeddedProjectiveVariety),(tangentCone,Ideal),coneOfLines}}
typicalValues#tangentCone = typValTanCone;

document {Key => {coneOfLines,(coneOfLines,EmbeddedProjectiveVariety,EmbeddedProjectiveVariety)}, 
Headline => "cone of lines on a subvariety passing through a point", 
Usage => "coneOfLines(X,p)
coneOfLines(p,X)", 
Inputs => {"X" => EmbeddedProjectiveVariety => {"a subvariety of ", TEX///$\mathbb{P}^n$///}, "p" => EmbeddedProjectiveVariety => {"a point on ", TEX///$X$///}}, 
Outputs => {EmbeddedProjectiveVariety => {"the subscheme of ",TEX///$\mathbb{P}^n$///, " consisting of the union of all lines contained in ",TEX///$X$///, " and passing through ",TEX///$p$///}}, 
PARA{"In the example below we compute the cone of lines passing through the generic point of a smooth del Pezzo fourfold in ",TEX///$\mathbb{P}^7$///, "."}, 
EXAMPLE {"K := frac(QQ[a,b,c,d,e]); t = gens ring PP_K^4; phi = rationalMap {minors(2,matrix{{t_0,t_1,t_2},{t_1,t_2,t_3}}) + t_4};", "X = image phi;", "ideal X", "p := projectiveVariety minors(2,(vars K)||(vars ring PP_K^4))", "coneOfLines(X,phi p)", "ideal oo"},
SeeAlso => (tangentCone,EmbeddedProjectiveVariety,EmbeddedProjectiveVariety)}

document {Key => {sectionalGenus,(sectionalGenus,EmbeddedProjectiveVariety)},
Headline => "the sectional genus of an embedded projective variety", 
Usage => "sectionalGenus X", 
Inputs => {"X" => EmbeddedProjectiveVariety => {"a positive dimensional variety"}},
Outputs => {ZZ => {"the sectional arithmetic genus of ",TT"X"}},
EXAMPLE {"X = PP_QQ^(3,2);",
"sectionalGenus X"},
SeeAlso => (genera,ProjectiveVariety)}

document {Key => {(decompose,MultiprojectiveVariety)}, 
Headline => "irreducible components of a variety", 
Usage => "decompose X", 
Inputs => {"X" => MultiprojectiveVariety}, 
Outputs => {List => {"the list of ",TO2{MultiprojectiveVariety,"multi-projective varieties"}," defined by the minimal associated primes of the ",TO2{(ideal,MultiprojectiveVariety),"ideal"}," of ",TT"X"}},
PARA {"This calculation is performed using the function ",TO (decompose,Ideal),"."},
EXAMPLE {"C = PP_(ZZ/100003)^(1,4);", 
"L = linearSpan sum{point C,point C}, L' = linearSpan sum{point C,point ambient C};",
"X = ⋃ {C,L,L'};",
"D = decompose X",
"assert(X == ⋃ D)"}, 
SeeAlso => {(support,MultiprojectiveVariety),(top,MultiprojectiveVariety),(decompose,Ideal)}} 

document {Key => {(degrees,MultiprojectiveVariety)}, 
Headline => "degrees for the minimal generators", 
Usage => "degrees X", 
Inputs => {"X" => MultiprojectiveVariety}, 
Outputs => {{"the list of multi-degrees for the minimal generators of the ",TO2{(ideal,MultiprojectiveVariety),"ideal"}," of ",TT"X"}},
EXAMPLE {"X = ⋃ for i to 10 list point PP_(ZZ/33331)^{2,3};", "? X", "degrees X"}} 

document { 
Key => {(symbol ===>,EmbeddedProjectiveVariety,EmbeddedProjectiveVariety), (symbol <===,EmbeddedProjectiveVariety,EmbeddedProjectiveVariety)},
Headline => "try to find an isomorphism between two projective varieties", 
Usage => "X ===> Y
Y <=== X", 
Inputs => {"X" => EmbeddedProjectiveVariety,"Y" => EmbeddedProjectiveVariety => {"projectively equivalent to ",TT "X"}},
Outputs => {MultirationalMap => {"an isomorphism of the ambient spaces that sends ",TT"X"," to ",TT"Y"," (or an error if it fails)"}},
PARA{"This recursively tries to find an isomorphism between the base loci of the ",TO2{(parametrize,MultiprojectiveVariety),"parameterizations"},"."},
PARA{"In the following example, ",TEX///$X$///," and ",TEX///$Y$///," are two random rational normal curves of degree 6 in ",TEX///$\mathbb{P}^6\subset\mathbb{P}^8$///,", and ",TEX///$V$///," (resp., ",TEX///$W$///,") is a random complete intersection of type (2,1) containing ",TEX///$X$///," (resp., ",TEX///$Y$///,")."},
EXAMPLE lines ///K = ZZ/10000019;
(M,N) = (apply(9,i -> random(1,ring PP_K^8)), apply(9,i -> random(1,ring PP_K^8)));
X = projectiveVariety(minors(2,matrix{take(M,6),take(M,{1,6})}) + ideal take(M,-2));
Y = projectiveVariety(minors(2,matrix{take(N,6),take(N,{1,6})}) + ideal take(N,-2));
? X
time f = X ===> Y;
f X
f^* Y
V = random({{2},{1}},X);
W = random({{2},{1}},Y);
time g = V ===> W;
g||W///,
PARA{"In the next example, ",TEX///$Z\subset\mathbb{P}^9$///," is a random (smooth) del Pezzo sixfold, hence projectively equivalent to ",TEX///$\mathbb{G}(1,4)$///,"."},
EXAMPLE lines ///A = matrix pack(5,for i to 24 list random(1,ring PP_K^9)); A = A - transpose A
Z = projectiveVariety pfaffians(4,A);
? Z
time h = Z ===> GG_K(1,4)
h || GG_K(1,4)
show oo///,
SeeAlso => {(parametrize,MultiprojectiveVariety)}}

document { 
Key => {(symbol <<,MultirationalMap,MultiprojectiveVariety),(symbol <<,MultiprojectiveVariety,MultiprojectiveVariety)},
Headline => "force the change of the target in a multi-rational map",
Usage => "Phi << Y", 
Inputs => {MultirationalMap => "Phi" => {"whose image is a subvariety ",TEX///$X\subseteq\mathbb{P}^{k_1}\times\cdots\times\mathbb{P}^{k_n}$///},MultiprojectiveVariety => "Y" => {"a subvariety of ",TEX///$\mathbb{P}^{l_1}\times\cdots\times\mathbb{P}^{l_n}$///," with ",TEX///$k_i\leq l_i$///," for ",TEX///$i=1,\ldots,n$///}}, 
Outputs => {MultirationalMap => {"the composition of ",TT"Phi"," with an inclusion of ",TEX///$X$///," into ",TEX///$Y$///," (if this is possible and easy, otherwise an error is generated)"}},
EXAMPLE {
"Phi = parametrize PP_(ZZ/65521)^({1,3},{2,1});",
"X = image Phi;",
"describe X",
"Y = PP^{3,5};",
"Psi = Phi << Y;",
"describe image Psi"},
PARA{"The inclusion ",TEX///$j:X\to Y$///," such that ",TT"Phi * j == Psi"," can be obtained as follows:"},
EXAMPLE {
"j = X << Y;",
"assert(Phi * j == Psi and j == (1_X << Y))"},
SeeAlso => {(multirationalMap,MultirationalMap,MultiprojectiveVariety)}}

document { 
Key => {(symbol ++,EmbeddedProjectiveVariety,EmbeddedProjectiveVariety)},
Headline => "join of projective varieties", 
Usage => "X ++ Y", 
Inputs => {"X" => EmbeddedProjectiveVariety,"Y" => EmbeddedProjectiveVariety => {"in the same ambient projective space of ",TEX///$X$///}},
Outputs => {EmbeddedProjectiveVariety => {"the join of ",TEX///$X$///," and ",TEX///$Y$///,", that is, the closure of the union of lines of the form ",TEX///$\langle p,q\rangle$///,", with ",TEX///$p\in X$///,", ",TEX///$q\in Y$///,", and ",TEX///$p\neq q$///}},
EXAMPLE {"K = ZZ/333331;", 
"C = PP_K^(1,5); -- rational normal quintic curve",
"L = linearSpan {point ambient C,point ambient C}; -- random line",
"C ++ L","C ++ C","(point C) ++ (point C) ++ (point C)"}}

typValTanForm := typicalValues#tangentialChowForm;
typicalValues#tangentialChowForm = EmbeddedProjectiveVariety;
document { 
Key => {(tangentialChowForm,EmbeddedProjectiveVariety,ZZ)},
Headline => "higher Chow forms of a projective variety", 
Usage => "tangentialChowForm(X,s)", 
Inputs => {"X" => EmbeddedProjectiveVariety, "s" => ZZ},
Outputs => {EmbeddedProjectiveVariety => {"the subvariety of the appropriate ",TO2{GrassmannianVariety,"Grassmannian"}," defined by ",TO tangentialChowForm,TT"(",TO2{(ideal,MultiprojectiveVariety),"ideal"}," ",TT"X,s)"}},
EXAMPLE {"X = PP_(ZZ/65521)[2,1];", "tangentialChowForm(X,1)", "ambientVariety oo"},
SeeAlso => {(chowForm,EmbeddedProjectiveVariety)}}
typicalValues#tangentialChowForm = typValTanForm;

typValChowForm := typicalValues#chowForm;
typicalValues#chowForm = EmbeddedProjectiveVariety;
document { 
Key => {(chowForm,EmbeddedProjectiveVariety)},
Headline => "chow forms of a projective variety", 
Usage => "chowForm X", 
Inputs => {"X" => EmbeddedProjectiveVariety},
Outputs => {EmbeddedProjectiveVariety => {"the subvariety of the appropriate ",TO2{GrassmannianVariety,"Grassmannian"}," defined by ",TO chowForm,TT" ",TO2{(ideal,MultiprojectiveVariety),"ideal"}," ",TT"X"}},
EXAMPLE {"X = PP_(ZZ/65521)[2,1];", "chowForm X", "ambientVariety oo"},
SeeAlso => {(tangentialChowForm,EmbeddedProjectiveVariety,ZZ)}}
typicalValues#chowForm = typValChowForm;

typValConVar := typicalValues#conormalVariety;
typicalValues#conormalVariety = MultiprojectiveVariety;
document { 
Key => {(conormalVariety,EmbeddedProjectiveVariety)},
Headline => "the conormal variety of a projective variety", 
Usage => "conormalVariety X", 
Inputs => {"X" => EmbeddedProjectiveVariety},
Outputs => {MultiprojectiveVariety => {"the conormal variety of ",TEX///$X$///}},
EXAMPLE {"X = PP_QQ^(2,2);", "C = conormalVariety X;", "p2 = multirationalMap last projections C;", "image p2 == dual X"},
SeeAlso => {(dual,EmbeddedProjectiveVariety)}}
typicalValues#conormalVariety = typValConVar;

document {
Key => {(symbol %,MultiprojectiveVariety,MultiprojectiveVariety)},
Headline => "subvariety of a projective variety", 
Usage => "X % Y", 
Inputs => {"X" => MultiprojectiveVariety,"Y" => MultiprojectiveVariety => {"which contains ",TEX///$X$///}},
Outputs => {MultiprojectiveVariety => {"the same variety ",TEX///$X$///," thought of as a subvariety of ",TEX///$Y$///}},
EXAMPLE {"Y = GG(ZZ/33331,1,4);","p = point Y","p % Y", "Fano p"},
SeeAlso => {ambientVariety,(Fano,EmbeddedProjectiveVariety)}}

document { 
Key => {(Fano,ZZ,EmbeddedProjectiveVariety),(Fano,EmbeddedProjectiveVariety)},
Headline => "Fano scheme of a projective variety", 
Usage => "Fano(k,X)
Fano(k,X,AffineChartGrass=>...)
Fano X
Fano(X,AffineChartGrass=>...)", 
Inputs => {"k" => ZZ => {"optional with default value equal to ",TEX///$\mathrm{dim}(X)$///}, "X" => EmbeddedProjectiveVariety},
Outputs => {EmbeddedProjectiveVariety => {"the subvariety of the ",TO2{GrassmannianVariety,"Grassmannian"}," ",TEX///$\mathbb{G}(k,\mathrm{ambient}(X))$///," that parametrizes the ",TEX///$k$///,"-planes lying on ",TEX///$X$///}},
PARA{"This function is based internally on the function ",TO plucker,", provided by the package ",TO Resultants,". In particular, note that by default the computation is done on a randomly chosen affine chart on the Grassmannian. To change this behavior, you can use the ",TO AffineChartGrass," option."},
EXAMPLE {"K = ZZ/33331;","L = linearSpan {point PP_K^4,point PP_K^4}; -- a line in P^4","p := Fano L","Fano p","assert(Fano p == L)"},
PARA{"If the input is a ",TO2{(symbol %,MultiprojectiveVariety,MultiprojectiveVariety),"subvariety"}," ",TEX///$Y\subset\mathbb{G}(k,\mathbb{P}^n)$///,", then the output is the variety ",TEX///$W\subset\mathbb{P}^n$///," swept out by the linear spaces corresponding to points of ",TEX///$Y$///,". As an example, we now compute a surface scroll ",TEX///$W\subset\mathbb{P}^4$///," over an elliptic curve ",TEX///$Y\subset\mathbb{G}(1,\mathbb{P}^4)$///,"."},
EXAMPLE {"G = GG_K(1,4);","Y := (G * random({{1},{1},{1},{1},{1}},0_G)) % G -- an elliptic curve","W = Fano Y; -- surface swept out by the lines of Y"},
PARA{"We can recover the subvariety ",TEX///$Y\subset\mathbb{G}(k,\mathbb{P}^n)$///," by computing the Fano variety of ",TEX///$k$///,"-planes contained in ",TEX///$W$///,"."},
EXAMPLE {"Fano(1,W) -- variety of lines contained in W","assert(oo == Y)"},
SeeAlso => {(plucker,Ideal),(Fano,ZZ,Ideal),(symbol %,MultiprojectiveVariety,MultiprojectiveVariety)}}

document { 
Key => {ambientVariety,(ambientVariety,MultiprojectiveVariety)},
Headline => "the ambient variety of a projective subvariety", 
Usage => "ambientVariety X", 
Inputs => {"X" => MultiprojectiveVariety => {"a ",TO2{(symbol %,MultiprojectiveVariety,MultiprojectiveVariety),"subvariety"}," of another ",TO2{MultiprojectiveVariety,"variety"}," ",TT "Y"}},
Outputs => {MultiprojectiveVariety => {"the ambient variety ",TT "Y"}},
EXAMPLE {"X = point PP_(ZZ/65521)^3;", "Y = random({1},X);", "X % Y", "ambientVariety X", "ambient X"},
SeeAlso => {(symbol %,MultiprojectiveVariety,MultiprojectiveVariety),(ambient,MultiprojectiveVariety)}}

document {Key => {GrassmannianVariety}, 
Headline => "the class of all Grassmannians of linear subspaces of projective spaces", 
PARA{"Objects of this type are created by ",TO GG,"."},
SeeAlso => {GG,(GG,ZZ,MultirationalMap),schubertCycle,cycleClass,(Fano,ZZ,EmbeddedProjectiveVariety),(tangentialChowForm,EmbeddedProjectiveVariety,ZZ)}}

document { 
Key => {GG,(GG,ZZ,EmbeddedProjectiveVariety),(GG,ZZ,ZZ),(GG,Ring,ZZ,ZZ),(GG,Ring),(GG,EmbeddedProjectiveVariety)}, 
Headline => "the Grassmannian of k-dimensional linear subspaces of an n-dimensional projective space", 
Usage => "GG(k,PP_K^n)
GG_K(k,n)
GG Grass(k,n,K,Variable=>\"x\")", 
Inputs => {"k" => ZZ, "P" => EmbeddedProjectiveVariety => {"a projective space of dimension ",TEX///$n$///}},
Outputs => {GrassmannianVariety => {"which parameterizes the ", TEX///$k$///, "-dimensional subspaces of ", TEX///$\mathbb{P}^n$///}},
EXAMPLE {"GG(2,PP_QQ^5)","describe oo"},
SeeAlso => {(GG,ZZ,MultirationalMap)}}

document {Key => {(GG,ZZ,MultirationalMap),(GG,ZZ,RationalMap)}, 
Headline => "induced automorphism of the Grassmannian", 
Usage => "GG(k,f)", 
Inputs => {"k" => ZZ => {"to indicate the Grassmannian ", TO GG, TEX///$(k,n)$///, " of ", TEX///$k$///, "-dimensional subspaces of ", TEX///$\mathbb{P}^n$///},
           "f" => MultirationalMap => {"an automorphism of ", TEX///$\mathbb{P}^n$///}},  
Outputs => {MultirationalMap => {"the induced automorphism of ", TO GG, TEX///$(k,n)$///}}, 
EXAMPLE {"K = ZZ/33331;", "f = random 1_(PP_K^4);", "show f", "F = GG(1,f);", "show F", "assert(F^-1 == GG(1,f^-1))"},
SeeAlso => {GG}}

document {Key => {cycleClass,(cycleClass,EmbeddedProjectiveVariety)}, 
Headline => "determine the expression of the class of a cycle as a linear combination of Schubert classes", 
Usage => "cycleClass C", 
Inputs => {"C" => EmbeddedProjectiveVariety => {"a ",TO2{(symbol %,MultiprojectiveVariety,MultiprojectiveVariety),"subvariety"}," of ", TO GG, TEX///$(k, n)$///, " representing a cycle of pure codimension ", TEX///$m$///, " in the Grassmannian of ", TEX///$k$///, "-dimensional subspaces of ", TEX///$\mathbb{P}^n$///}}, 
Outputs => {RingElement => {"the expression of the class of the cycle as a linear combination of Schubert classes"}}, 
PARA{"For the general theory on Chow rings of Grassmannians, see e.g. the book ", HREF{"https://scholar.harvard.edu/files/joeharris/files/000-final-3264.pdf", "3264 & All That - Intersection Theory in Algebraic Geometry"}, ", by D. Eisenbud and J. Harris."}, 
EXAMPLE {"G = GG(ZZ/33331,2,5);", "C = schubertCycle({3,2,1},G);", "cycleClass C", "C' = C + schubertCycle({2,2,2},G);", "cycleClass C'"}, 
SeeAlso => {schubertCycle}}

document {Key => {schubertCycle,(schubertCycle,VisibleList,GrassmannianVariety),[schubertCycle,Standard]}, 
Headline => "take a random Schubert cycle", 
Usage => "schubertCycle(a,G)", 
Inputs => {"a" => VisibleList => {"a list of integers ", TEX///$a = (a_0,\ldots,a_k)$///, " with ", TEX///$n-k\geq a_0 \geq \cdots \geq a_k \geq 0$///}, "G" => GrassmannianVariety => {"which parameterizes the ", TEX///$k$///, "-dimensional subspaces of ", TEX///$\mathbb{P}^n$///}}, 
Outputs => {EmbeddedProjectiveVariety => {"the Schubert cycle ", TEX///$\Sigma_a(\mathcal P)\subset\mathbb{G}(k,n)$///, " associated to a random complete flag ", TEX///$\mathcal P$///, " of nested projective subspace ", TEX///$\emptyset\subset P_0\subset \cdots \subset P_{n-1} \subset P_{n} = \mathbb{P}^n$///, " with ", TEX///$dim(P_i)=i$///}}, 
PARA{"For the general theory, see e.g. the book ", HREF{"https://scholar.harvard.edu/files/joeharris/files/000-final-3264.pdf", "3264 & All That - Intersection Theory in Algebraic Geometry"}, ", by D. Eisenbud and J. Harris."}, 
EXAMPLE {"G = GG(ZZ/33331,1,5);", "S = schubertCycle({2,1},G)", "cycleClass S"}, 
SeeAlso => {cycleClass}}

undocumented {
(expression,MultiprojectiveVariety),
(net,MultiprojectiveVariety),
(toString,MultiprojectiveVariety),
(point,MultiprojectiveVariety,Boolean), -- Intended for internal use only
(euler,MultiprojectiveVariety,Option),
(singularLocus,EmbeddedProjectiveVariety,Option),
(symbol *,ZZ,MultiprojectiveVariety), -- hidden to the user, since it returns non-reduced varieties
(symbol _,MultiprojectiveVariety,MultiprojectiveVariety), -- this returns a new type which is too rudimentary yet
(tangentialChowForm,EmbeddedProjectiveVariety,ZZ,ZZ),
(expression,MultirationalMap),
(net,MultirationalMap),
(toString,MultirationalMap),
(multirationalMap,RationalMap,RationalMap), -- Intended for internal use only
(multirationalMap,MultirationalMap,MultirationalMap), -- Intended for internal use only
(multidegree,MultirationalMap,MultirationalMap), --  Intended for internal use only
(multidegree,Nothing,MultirationalMap),
(toRationalMap,MultirationalMap,Boolean), -- Intended for internal use only
(source,MultirationalMap,MultirationalMap), -- Intended for internal use only
(symbol ^^,MultirationalMap,MultiprojectiveVariety), -- Intended for internal use only
(inverse,MultirationalMap,Option),
(inverse2,MultirationalMap,Option),
(multirationalMap,MultiprojectiveVariety,MultiprojectiveVariety,Boolean), -- Intended for internal use only
(symbol ?,MultiprojectiveVariety,MultiprojectiveVariety),
(symbol ?,GrassmannianVariety),
(Fano,EmbeddedProjectiveVariety,Option),
(Fano,ZZ,EmbeddedProjectiveVariety,Option),
(image,MultirationalMap,ZZ),(image,ZZ,MultirationalMap), -- This is dangerous because the defining ideal may not be saturated
(image,MultirationalMap,String),
(rationalMap,MultiprojectiveVariety,Tally), -- It is already documented in Cremona.m2
(matrix,MultirationalMap),
(random,MultirationalMap),
(hilbertPolynomial,EmbeddedProjectiveVariety) -- To be documented
}

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
assert(dim B == 2 and degree B == 14 and dim singularLocus B == -1 and degrees B == {({1,2},1),({2,1},1)});
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
source variety: threefold in PP^3 x PP^2 x PP^4 cut out by 12 hypersurfaces of multi-degrees (0,0,2)^1 (0,1,1)^2 (1,0,1)^7 (1,1,0)^2 
target variety: threefold in PP^2 x PP^4 cut out by 3 hypersurfaces of multi-degrees (0,2)^1 (1,1)^2 
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
-- assert(multidegree(,Phi) == {66, 46, 31, 20}) -- too long time
assert(multidegree(3,Phi) == 66 and multidegree(2,Phi) == 46);
assert(multidegree Phi == {66, 46, 31, 20})
assert(degree(Phi,Strategy=>"random point") == 1 and degree Phi == 1)
assert(Phi * inverse Phi == 1 and Phi^-1 * Phi == 1)
assert(toString describe Phi == toString strForTest);
///

TEST ///
R = ZZ/300007[x_0..x_3];
C3 = ideal(x_2^2-x_1*x_3,x_1*x_2-x_0*x_3,x_1^2-x_0*x_2);
C2 = ideal(x_1^2-x_0*x_2,x_3);
Phi = last graph rationalMap({C3,C2},Dominant=>true);
Y = target Phi;
Z = {target Phi, random({1,1},0_Y), random({{1,1},{1,1}},0_Y), random({{1,1},{1,1},{1,1}},0_Y),
     point Y,(point target Phi) + (point target Phi) + (point target Phi)}
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

TEST /// -- inverses of constant maps 
PP (ZZ/3333331)
Phi = parametrize point PP^(1,4);
inverse(Phi,Verify=>4)
Psi = Phi||Phi
inverse(Psi,Verify=>4)
Psi' = Phi||(inverse Phi)
inverse(Psi',Verify=>4)
Eta = rationalMap(PP^(1,4),Dominant=>true)
Eta' = Eta||(parametrize point PP^3)
inverse(Eta',Verify=>4)
Eta'' = Eta||(Eta||(point target Eta));
inverse(Eta'',Verify=>4)
///

TEST///
Phi = last graph rationalMap projectiveVariety({1},{4},ZZ/300007);
assert(multidegree(,Phi) == multidegree Phi)
assert(projectiveDegrees Phi == multidegree Phi)
degree(Phi,Strategy=>"random point")
R = ZZ/33331[x_0..x_4];
Phi = (last graph multirationalMap rationalMap transpose jacobian(-x_2^3+2*x_1*x_2*x_3-x_0*x_3^2-x_1^2*x_4+x_0*x_2*x_4))||projectiveVariety ideal(random(2,R));
assert(? source Phi == "threefold in PP^4 x PP^4 cut out by 13 hypersurfaces of multi-degrees (0,2)^1 (1,1)^3 (2,1)^8 (4,0)^1 ")
assert(? target Phi == "hypersurface in PP^4 defined by a form of degree 2")
assert(degree(Phi,Strategy=>"random point") == 2)
assert(degree(Phi,Strategy=>"0-th projective degree") == 2)
assert(degree Phi == 2)
assert(degreeMap Phi == 2)
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

TEST /// -- product must be strict associative
K = ZZ/333331;
X = projectiveVariety({1,1},{2,3},K);
Y = random({3},projectiveVariety(2,2,K));
Z = ⋃ {point X,point X,point X};
W = projectiveVariety(1,3,K);
assert((X ** Y) ** Z === X ** (Y ** Z))
assert((∏ {X,Y,Z}) ** W === X ** ∏ {Y,Z,W} and X ** ∏ {Y,Z,W} === (X ** Y) ** (Z ** W))
///

TEST /// -- some cache tests
K = ZZ/333331;
I = trim kernel veronese(1,4,K)
X = projectiveVariety I
assert(X === projectiveVariety I)
assert(I.cache#"multiprojectiveVariety" === X)
assert(I.cache#"isMultisaturated" === true)
R = (ring I)/I;
assert(X === projectiveVariety R)

R = quotient trim kernel veronese(2,2,K)
X = projectiveVariety R
assert(X === projectiveVariety ideal R)
assert(R#"multiprojectiveVariety" === X)
assert((ideal X).cache#"isMultisaturated" === true)

I' = trim ideal image basis(3,I);
X' = projectiveVariety(I',Saturate=>false);
assert(ideal X' === I')
assert(not I'.cache#?"isMultisaturated")
R' = (ring I)/I';
assert(try projectiveVariety R' then false else true)
assert(try projectiveVariety(R',Saturate=>false) then false else true)
R'' = (ring I)/trim ideal image basis(3,I);
assert(try projectiveVariety(R'',Saturate=>false) then true else false)
assert(try projectiveVariety R'' then true else false)

I = trim kernel veronese(1,5,K);
J = trim kernel veronese(1,5,K);
assert(I === J);
R = quotient I; S = quotient J;
assert(R =!= S);
X = projectiveVariety I;
assert(X === projectiveVariety R)
Y = projectiveVariety J;
assert(Y === projectiveVariety S)
assert(X == Y and ideal X === ideal Y and ring X === R and ring Y === S and X =!= Y)
assert(X^2 === X ** Y and X ** Y === Y^2)

I = trim kernel veronese(1,5,K);
J = trim kernel veronese(1,5,K);
R = quotient I; S = quotient J;
assert(R =!= S);
X = projectiveVariety I;
X#"ringVariety" = S;
assert(try projectiveVariety R then false else true)
///

TEST /// -- random points in char 0 and sorts
p = for i to 5 list point projectiveVariety({2,3},QQ);
P = reverse for i from 1 to 5 list sum take(p,i);
assert(apply(P,degree) == {5,4,3,2,1} and apply(sort P,degree) == {1,2,3,4,5})
p = for i to 6 list point projectiveVariety({2},{3},QQ);
P = reverse for i from 1 to 6 list sum take(p,i);
assert(apply(P,degree) == {6,5,4,3,2,1} and apply(sort P,degree) == {1,2,3,4,5,6})
T = tangentSpace(projectiveVariety({3},{2},QQ),point projectiveVariety({3},{2},QQ));
p = for i to 7 list point T;
P = reverse for i from 1 to 7 list sum take(p,i);
assert(apply(P,degree) == {7,6,5,4,3,2,1} and apply(sort P,degree) == {1,2,3,4,5,6,7})
///

TEST /// -- projective join
X = PP_(ZZ/333331)^(2,2);
Y = X ++ X;
assert(codim Y == 1 and degree Y == 3 and singularLocus Y == X)
U = for i to 3 list point PP^7;
assert(linearSpan U == fold(U,(x,y)->x++y))
///

TEST /// -- inverse2
Phi = last graph multirationalMap quadroQuadricCremonaTransformation(11,1,ZZ/65521);
Psi = inverse2 Phi;
Phi' = clean Phi;
Psi' = inverse Phi';
assert(Psi == Psi' and Phi * Psi == 1)
///

TEST ///
checkIso = (X,Y) -> (
    time phi := X ===> Y;
    assert(source phi === ambient X and target phi === ambient Y and degree phi == 1 and image phi === target phi and phi X == Y and phi^* Y == X);
    assert isSubset(phi point X,Y)
);
K = ZZ/3333331;
X = random({{1},{1},{1},{1}},0_(PP_K^7));
Y = random({{1},{1},{1},{1}},0_(PP_K^7));
checkIso(X,Y)
setRandomSeed 10
X = random(2,0_(PP_K^3));
Y = random(2,0_(PP_K^3));
checkIso(X,Y)
X = random({{2},{1},{1},{1},{1},{1}},0_(PP_K^9));
Y = random({{2},{1},{1},{1},{1},{1}},0_(PP_K^9));
checkIso(X,Y)
setRandomSeed 123
X = random({{2},{1},{1},{1},{1},{1}},0_(PP_K^10));
Y = random({{2},{1},{1},{1},{1},{1}},0_(PP_K^10));
checkIso(X,Y)
///

TEST /// -- parametrizations 1
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

TEST /// -- parametrizations 2
checkInverseParametrization = X -> (
    f := parametrize X;
    <<f<<endl;
    assert(instance(source f,EmbeddedProjectiveVariety) and source f === ambient source f and dim source f == dim X and target f === X);
    assert(f === parametrize X);
    assert(f#"inverse" =!= null);
    p := point source f;
    assert((f#"inverse") f p == p);
);
checkDegreeParametrization = X -> (
    f := parametrize X;
    <<f<<endl;
    assert(instance(source f,EmbeddedProjectiveVariety) and source f === ambient source f and dim source f == dim X and target f === X);
    assert(f === parametrize X);
    p := point source f;
    assert(f^* f p == p);
);
K = ZZ/333331;
for n from 3 to 5 do for k to n-1 do checkInverseParametrization GG_K(k,n);
checkInverseParametrization GG(QQ,2,4);
GG(QQ,2,4) ===> GG(QQ,1,4)
-- for i in {5,8,14} do checkInverseParametrization baseLocus quadroQuadricCremonaTransformation(i,1,K);
X = random projectiveVariety Grass(1,4,K);
-- checkInverseParametrization X
-- checkDegreeParametrization (X * random(1,0_X))
-- checkDegreeParametrization ((X * random(1,0_X) ** (point PP_K^2)))
-- checkDegreeParametrization (X * random({{1},{1}},0_X))
checkDegreeParametrization ((X * random(1,0_X))**(point PP_K^{2,1}))
Y = random (PP_QQ[2,1])
X = (PP_QQ[2,1])
checkDegreeParametrization X
checkDegreeParametrization Y
X ===> Y
Y = random (PP_QQ[1,1,1])
X = projectiveVariety image segre PP_QQ^{2,1}
checkDegreeParametrization X
checkDegreeParametrization Y
X ===> Y
X = random({{1},{1},{2},{2}},0_(PP_K^9))
checkDegreeParametrization X
X = ⋃ for i to 4 list point PP_K^({2,3,1},{1,2,2})
assert((parametrize X)*(inverse parametrize X) == 1 and (inverse parametrize X)*(parametrize X) == 1) 
///

TEST /// -- parametrizations 3
needsPackage "SpecialFanoFourfolds";
debug SpecialFanoFourfolds;
checkInverseParametrization = X -> (
    f := parametrize X;
    <<f<<endl;
    assert(instance(source f,EmbeddedProjectiveVariety) and source f === ambient source f and dim source f == dim X and target f === X);
    assert(f === parametrize X);
    assert(f#"inverse" =!= null);
    p := point source f;
    assert((f#"inverse") f p == p);
);
X = fanoFourfold (12,7);
X#InverseMethod = inverse3;
checkInverseParametrization X
-- X = fanoFourfold (14,8);
-- X#InverseMethod = inverse3;
-- time checkInverseParametrization X
setRandomSeed 0;
X = fanoFourfold (16,9);
X#InverseMethod = inverse3;
time checkInverseParametrization X
-- setRandomSeed 11111;
-- X = fanoFourfold (18,10);
-- X#InverseMethod = inverse3;
-- time checkInverseParametrization X
///

TEST /// -- conormalVariety
K = ZZ/333331;
V = PP_K^(2,2);
W = conormalVariety V;
V' = dual V;
W' = conormalVariety V';
assert(V == image multirationalMap first projections W);
assert(V' == image multirationalMap last projections W);
assert(V' == image multirationalMap first projections W');
assert(V == image multirationalMap last projections W');
j := check multirationalMap(permute(W,{1,0}),W');
assert(isIsomorphism j);
///


