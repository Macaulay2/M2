
if version#"VERSION" < "1.18" then error "this package requires Macaulay2 version 1.18 or newer";

newPackage(
    "K3Surfaces",
    Version => "1.1", 
    Date => "August 13, 2022",
    Authors => {{Name => "Michael Hoff", 
                 Email => "hahn@math.uni-sb.de"},
                {Name => "Giovanni Staglianò", 
                 Email => "giovanni.stagliano@unict.it"}},
    PackageExports => {"SpecialFanoFourfolds"},
    Keywords => {"Algebraic Geometry"},
    Headline => "Explicit constructions of K3 surfaces",
    DebuggingMode => false
)

if SpecialFanoFourfolds.Options.Version < "2.6" then (
    <<endl<<"Your version of the SpecialFanoFourfolds package is outdated (required version 2.6 or newer);"<<endl;
    <<"you can manually download the latest version from"<<endl;
    <<"https://github.com/Macaulay2/M2/tree/development/M2/Macaulay2/packages."<<endl;
    <<"To automatically download the latest version of SpecialFanoFourfolds in your current directory,"<<endl;
    <<"you may run the following Macaulay2 code:"<<endl<<"***"<<endl<<endl;
    <<///run "curl -s -o SpecialFanoFourfolds.m2 https://raw.githubusercontent.com/Macaulay2/M2/development/M2/Macaulay2/packages/SpecialFanoFourfolds.m2";///<<endl<<endl<<"***"<<endl;
    error "required SpecialFanoFourfolds package version 2.6 or newer";
);

export{"K3","LatticePolarizedK3surface","EmbeddedK3surface","project","mukaiModel",
       "trigonalK3","tetragonalK3","pentagonalK3"}

debug SpecialFanoFourfolds;
needsPackage "Truncations";
needsPackage "MinimalPrimes";

LatticePolarizedK3surface = new Type of HashTable;

LatticePolarizedK3surface.synonym = "K3 surface";

EmbeddedK3surface = new Type of EmbeddedProjectiveVariety;

EmbeddedK3surface.synonym = "embedded K3 surface";

net LatticePolarizedK3surface := S -> (
    M := S#"lattice";
    d := M_(0,1);
    n := M_(1,1);
    g := lift((M_(0,0) + 2)/2,ZZ);
    w := "K3 surface with rank 2 lattice defined by the intersection matrix: "|net(M);
    local g'; local c; local a; local b; local g0; local d0; local n0;
    w = w || concatenate for s in select(fewPossibilities,l -> take(l,{4,6}) == (g,d,n)) list ( 
    (g',c,a,b,g0,d0,n0) = s;
    "-- "|toString(a,b)|": K3 surface of genus "|toString(g')|" and degree "|toString(2*g'-2)|" containing "|(if n == 0 then "elliptic" else "rational")|" curve of degree "|toString(c)|" "|(if isAdmissible(2*g'-2) then "(cubic fourfold) " else "")|(if isAdmissibleGM(2*g'-2) then "(GM fourfold) " else "")|newline
    )
);
texMath LatticePolarizedK3surface := texMath @@ net;

? EmbeddedK3surface := S -> "K3 surface of genus "|toString(genus S)|" and degree "|toString(degree S)|" in PP^"|toString(dim ambient S);

LatticePolarizedK3surface#{WebApp,AfterPrint} = LatticePolarizedK3surface#{WebApp,AfterNoPrint} = 
LatticePolarizedK3surface#{Standard,AfterPrint} = LatticePolarizedK3surface#{Standard,AfterNoPrint} = S -> (
    << endl << concatenate(interpreterDepth:"o") << lineNumber << " : " << "Lattice-polarized K3 surface" << endl;
);

EmbeddedK3surface#{WebApp,AfterPrint} = EmbeddedK3surface#{WebApp,AfterNoPrint} = 
EmbeddedK3surface#{Standard,AfterPrint} = EmbeddedK3surface#{Standard,AfterNoPrint} = S -> (
    << endl << concatenate(interpreterDepth:"o") << lineNumber << " : " << "Embedded K3 surface" << endl;
);

map (LatticePolarizedK3surface,ZZ,ZZ) := o -> (S,a,b) -> (
    if S.cache#?("map",a,b) then return S.cache#("map",a,b);
    M := S#"lattice";
    d := M_(0,1);
    n := M_(1,1);
    g := lift((M_(0,0) + 2)/2,ZZ);
    if d == 0 and n == -2 then if b != 0 then error "the K3 surface is nodal";
    H := hyperplane S;
    C := S#"curve";
    phi := mapDefinedByDivisor(Var S,{(H,a),(C,b)});
    if dim target phi =!= genus(S,a,b) then error("expected map to PP^"|(toString genus(S,a,b))|", but got map to PP^"|toString(dim target phi));
    S.cache#("map",a,b) = phi
);

map (EmbeddedK3surface,ZZ,ZZ) := o -> (S,a,b) -> map(K3 S,a,b);

map EmbeddedK3surface := o -> S -> (
    if not(S.cache#?"GeneralK3" and S.cache#"GeneralK3") then error "expected a general K3 surface of some genus";
    if not S.cache#?"mapK3" then error "construction map not found";
    S.cache#"mapK3"
);

coefficientRing LatticePolarizedK3surface := S -> coefficientRing Var S;

genus (LatticePolarizedK3surface,ZZ,ZZ) := (S,a,b) -> (
    M := S#"lattice";
    d := M_(0,1);
    n := M_(1,1);
    g := lift((M_(0,0) + 2)/2,ZZ);
    lift((a^2*(2*g-2) + 2*a*b*d + b^2*n + 2)/2,ZZ)
);

genus (EmbeddedK3surface,ZZ,ZZ) := (S,a,b) -> genus(K3 S,a,b);

genus EmbeddedK3surface := S -> sectionalGenus S;

degree (LatticePolarizedK3surface,ZZ,ZZ) := (S,a,b) -> 2 * genus(S,a,b) - 2;

degree (EmbeddedK3surface,ZZ,ZZ) := (S,a,b) -> degree(K3 S,a,b);

degree EmbeddedK3surface := S -> 2 * genus(S) - 2;

construction = method();
construction EmbeddedK3surface := (cacheValue "construction-L.P.K3") (T -> (
    if T.cache#?"GeneralK3" and T.cache#"GeneralK3" then error "expected K3 surface to be not general";
    error "unable to recover the construction for the embedded K3 surface";
));

LatticePolarizedK3surface Sequence := (S,ab) -> (
    if not(#ab == 2 and instance(first ab,ZZ) and instance(last ab,ZZ)) then error "expected a sequence of two integers";
    (a,b) := ab;
    if S.cache#?("var",a,b) then return S.cache#("var",a,b);    
    f := map(S,a,b);
    if f#"image" === null and char coefficientRing S <= 65521 and genus(S,1,0) > 3 then image(f,"F4");
    T := new EmbeddedK3surface from image f;
    if dim ambient T <= 2 then error "the linear system is not very ample";
    -- (???) this fixes a bug in conversion of output to net, but it is a bit dangerous.
    T#"dimVariety" = 2;
    T.cache#"sectionalGenus" = genus(S,a,b);
    -- if degrees T =!= {({2},binomial(genus(T)-2,2))} then <<"--warning: the degrees for the generators are not as expected"<<endl;
    f#"image" = T;
    T.cache#"construction-L.P.K3" = (S,ab);
    S.cache#("var",a,b) = T
);

EmbeddedK3surface Sequence := (S,ab) -> (K3 S) ab;

Var LatticePolarizedK3surface := o -> S -> S#"surface";

hyperplane = method();
hyperplane LatticePolarizedK3surface := (cacheValue "hyperplane") (T -> random(1,0_(Var T)));

latticePolarizedK3surface = method();
latticePolarizedK3surface (EmbeddedK3surface,EmbeddedProjectiveVariety,List) := (T,C,gdn) -> (
    (g,d,n) := toSequence gdn;
    new LatticePolarizedK3surface from {
        symbol cache => new CacheTable,
        "surface" => T,
        "curve" => C,
        "lattice" => matrix{{2*g-2,d},{d,n}}
    }
);
latticePolarizedK3surface (EmbeddedK3surface,EmbeddedProjectiveVariety,EmbeddedProjectiveVariety,List) := (T,C,H,gdn) -> (
    S := latticePolarizedK3surface(T,C,gdn);
    S.cache#"hyperplane" = H;
    return S;
);
latticePolarizedK3surface (EmbeddedK3surface,EmbeddedProjectiveVariety,Nothing,List) := (T,C,H,gdn) -> latticePolarizedK3surface(T,C,gdn);
latticePolarizedK3surface (EmbeddedProjectiveVariety,EmbeddedProjectiveVariety,List) := (T,C,gdn) -> latticePolarizedK3surface(new EmbeddedK3surface from T,C,gdn);
latticePolarizedK3surface (EmbeddedProjectiveVariety,EmbeddedProjectiveVariety,EmbeddedProjectiveVariety,List) := (T,C,H,gdn) -> latticePolarizedK3surface(new EmbeddedK3surface from T,C,H,gdn);
latticePolarizedK3surface (EmbeddedProjectiveVariety,EmbeddedProjectiveVariety,Nothing,List) := (T,C,H,gdn) -> latticePolarizedK3surface(new EmbeddedK3surface from T,C,H,gdn);

K3 = method(Options => {CoefficientRing => ZZ/65521, Verbose => false, Singular => null});

makegeneralK3 = (f,p,g) -> (
    K3surf := new EmbeddedK3surface from image f;
    assert(sectionalGenus K3surf == g and degree K3surf == 2*g-2 and dim ambient K3surf == g and dim p <= 0 and isSubset(p,K3surf));
    -- if g <= 12 then assert(degree p == 1);
    f#"image" = K3surf;
    K3surf.cache#"mapK3" = f;
    K3surf.cache#"pointK3" = p;
    K3surf.cache#"GeneralK3" = true;
    K3surf
);

K3 ZZ := o -> g -> (
    K := o.CoefficientRing;
    local X; local p; local Ass;
    if member(g,{3,4,5,6,7,8,9,10,12}) then (
        (X,p) = randomPointedMukaiThreefold(g,CoefficientRing=>K);
        j := parametrize random({1},p);
        X = j^* X; p = j^* p;
        return makegeneralK3(super 1_X,p,g);
    );
    if g == 11 then (
        if o.Verbose then <<"-- constructing general K3 surface of genus "<<g<<" and degree "<<2*g-2<<" in PP^"<<g<<endl;
        if o.Verbose then <<"-- (taking a random GM fourfold X of discriminant 20, hence containing a surface S of degree 9 and genus 2)"<<endl;
        X = specialGushelMukaiFourfold("general GM 4-fold of discriminant 20",K);
        if o.Verbose then <<"-- (running procedure 'associatedK3surface' for the GM fourfold X of discriminant 20)"<<endl<<"-- *** --"<<endl;
        Ass = building associatedK3surface(X,Verbose=>o.Verbose,Singular=>o.Singular);
        if o.Verbose then <<"-- *** --"<<endl;
        return makegeneralK3(last Ass,(last Ass) first Ass_2,g);
    );
    if g == 14 then (
        if o.Verbose then <<"-- constructing general K3 surface of genus "<<g<<" and degree "<<2*g-2<<" in PP^"<<g<<endl;
        if o.Verbose then <<"-- (taking a random cubic fourfold X of discriminant 26, hence containing a surface S of degree 7 and genus 1)"<<endl;
        X = specialCubicFourfold("one-nodal septic del Pezzo surface",K);
        if o.Verbose then <<"-- (running procedure 'associatedK3surface' for the cubic fourfold X of discriminant 26)"<<endl<<"-- *** --"<<endl;
        Ass = building associatedK3surface(X,Verbose=>o.Verbose,Singular=>o.Singular);
        if o.Verbose then <<"-- *** --"<<endl;
        return makegeneralK3(last Ass,(last Ass) first Ass_2,g);
    );    
    if g == 20 then (
        if o.Verbose then <<"-- constructing general K3 surface of genus "<<g<<" and degree "<<2*g-2<<" in PP^"<<g<<endl;
        if o.Verbose then <<"-- (taking a random cubic fourfold X of discriminant 38, hence containing a surface S of degree 10 and genus 6)"<<endl;
        X = specialCubicFourfold("C38",K);
        if o.Verbose then <<"-- (running procedure 'associatedK3surface' for the cubic fourfold X of discriminant 38)"<<endl<<"-- *** --"<<endl;
        Ass = building associatedK3surface(X,Verbose=>o.Verbose,Singular=>o.Singular);
        if o.Verbose then <<"-- *** --"<<endl;
        return makegeneralK3(last Ass,(last Ass) first Ass_2,g);
    );
    if g == 22 then (
        if o.Verbose then <<"-- constructing general K3 surface of genus "<<g<<" and degree "<<2*g-2<<" in PP^"<<g<<endl;
        if o.Verbose then <<"-- (taking a random cubic fourfold X of discriminant 42, hence containing a surface S of degree 9 and genus 2)"<<endl;
        X = specialCubicFourfold("C42",K);
        if o.Verbose then <<"-- (running procedure 'associatedK3surface' for the cubic fourfold X of discriminant 42)"<<endl<<"-- *** --"<<endl;
        Ass = building associatedK3surface(X,Verbose=>o.Verbose,Singular=>o.Singular);
        if o.Verbose then <<"-- *** --"<<endl;
        return makegeneralK3(last Ass,(last Ass) first Ass_2,g);
    );
    error ("no procedure found to construct random K3 surface of genus "|(toString g));
);

K3 SpecialGushelMukaiFourfold := K3 SpecialCubicFourfold := o -> X -> (
    d := discriminant X;
    if (not isAdmissible d) and (not isAdmissibleGM d) then <<"--warning: expected an admissible integer for the discriminant"<<endl;
    g := lift((d+2)/2,ZZ);
    if o.Verbose then <<"-- (running procedure 'associatedK3surface' for the fourfold of discriminant "<<d<<")"<<endl<<"-- *** --"<<endl;
    Ass := building associatedK3surface(X,Verbose=>o.Verbose,Singular=>o.Singular);
    if o.Verbose then <<"-- *** --"<<endl;
    return makegeneralK3(last Ass,(last Ass) first Ass_2,g);
);

K3 (ZZ,ZZ,ZZ) := o -> (g,d,n) -> (
    -- if o.Verbose then <<"-- constructing K3 surface with rank 2 lattice defined by the intersection matrix "<<matrix{{2*g-2,d},{d,n}}<<endl;   
    K := o.CoefficientRing;
    local X; local T; local C; local p; local j;
    if g >= 3 and g <= 12 and d == 2 and n == -2 then (
        (T,C) = randomK3surfaceContainingConic(g,CoefficientRing=>K);
        H := if g != 10 and g != 12 then Var ideal first gens ring ambient T else null;
        return latticePolarizedK3surface(T,C,H,{g,d,n});
    );
    if member(g,{3,4,5,6,7,8,9,10,12}) and d == 1 and n == -2 then ( 
        (X,C) = randomMukaiThreefoldContainingLine(g,CoefficientRing=>K);
        j = parametrize random({1},C);
        (T,C) = (j^* X,j^* C);
        return latticePolarizedK3surface(T,C,{g,d,n});
    );
    if (member(g,{3,4,5}) and d >= 3 and n == -2) and (g != 5 or d <= 8) and (g != 4 or d <= 6) and (g != 3 or d <= 8) then (
        C = randomRationalCurve(d,g,CoefficientRing=>K); 
        T = random(if g == 3 then {4} else if g == 4 then {{2},{3}} else {{2},{2},{2}},C);
        return latticePolarizedK3surface(T,C,{g,d,n});
    );
    if (member(g,{3,4,5}) and member(d,{3,4,5,6,7,8,9}) and n == 0) and (g != 5 or d <= 9) and (g != 4 or d <= 7) and (g != 3 or d <= 8) then (
        C = randomEllipticCurve(d,g,CoefficientRing=>K);
        T = random(if g == 3 then {4} else if g == 4 then {{2},{3}} else {{2},{2},{2}},C);
        return latticePolarizedK3surface(T,C,{g,d,n});
    );
    if member(g,{3,4,5,6,7,8,9,10,12}) and d == 0 and n == -2 then (
        (X,p) = randomPointedMukaiThreefold(g,CoefficientRing=>K);
        j = parametrize random({1},tangentSpace(X,p));
        T = j^* X; 
        C = j^* p; -- the node
        return latticePolarizedK3surface(T,C,{g,d,n});
    );
    if g >= 5 and d == 3 and n == 0 then return trigonalK3(g,CoefficientRing=>o.CoefficientRing);
    if g >= 3 and d == 4 and n == 0 then return tetragonalK3(g,CoefficientRing=>o.CoefficientRing);
    if g >= 3 and d == 5 and n == 0 then return pentagonalK3(g,CoefficientRing=>o.CoefficientRing);    
    error ("no procedure found to construct K3 surface with rank 2 lattice defined by the "|toString(matrix{{2*g-2,d},{d,n}}));
);

K3 EmbeddedK3surface := o -> T -> (
    if T.cache#?"AssociatedLatticePolarizedK3surface" then return T.cache#"AssociatedLatticePolarizedK3surface";
    (S,ab) := construction T;
    (a,b) := ab;
    f := map(S,a,b);    
    C := f(S#"curve");
    if not(dim C == 1 and isSubset(C,T)) then error "something went wrong on the curve image";
    if degree C != a * (S#"lattice")_(0,1) + b * (S#"lattice")_(1,1) then error "something went wrong on the degree of the curve image";
    T.cache#"AssociatedLatticePolarizedK3surface" = latticePolarizedK3surface(T,C,{genus T,degree C,(S#"lattice")_(1,1)})
);

trigonalK3 = method(TypicalValue => LatticePolarizedK3surface, Options => {CoefficientRing => ZZ/65521});
trigonalK3 ZZ := o -> g -> (
    -- See also [Beauville - A remark on the generalized Franchetta conjecture for K3 surfaces]
    if g < 5 then error "expected genus to be at least 5"; 
    n := (g-1)%3 + 1; if n == 1 then n = 4;
    a := lift((g-n)/3,ZZ);
    K := o.CoefficientRing;
    P := PP_K^{1,n};
    S := if n == 2 then random({2,3},0_P) else 
         if n == 3 then random({{1,1},{1,3}},0_P) else 
         if n == 4 then random({{0,3},{1,1},{1,1}},0_P);
    f := rationalMap((0_P)%S,{a,1});
    T := image f;  
    assert(dim ambient T == g and dim T == 2 and degree T == 2*g-2 and sectionalGenus T == g);
    pr1 := multirationalMap first projections S;
    C := f(pr1^* point target pr1);
    assert(dim C == 1 and degree C == 3);
    latticePolarizedK3surface(T,C,{g,3,0})
);

tetragCase1 = (K,a,g) -> (
    x := local x;
    R := K[x_0..x_5,Degrees=>{3:{1,0},1:{1,1},2:{0,1}}];
    U := ideal(random({2,1},R),random({2,2},R));
    M := basis({1,a},R);
    y := local y;
    T := K[y_0..y_(numColumns M -1)];
    j := map(quotient U,T,M);
    S := Var trim kernel j;
    assert(dim ambient S == g and dim S == 2 and degree S == 2*g-2 and sectionalGenus S == g);
    C := Var trim preimage(j,sub(ideal(x_4),target j));
    assert(dim C == 1 and degree C == 4);
    latticePolarizedK3surface(S,C,{g,4,0})
);

tetragonalK3 = method(TypicalValue => LatticePolarizedK3surface, Options => {CoefficientRing => ZZ/65521});
tetragonalK3 ZZ := o -> g -> (
    if g < 3 then error "expected genus to be at least 3";
    n := (g-1)%4;
    a := lift((g-1-n)/4,ZZ);
    K := o.CoefficientRing;
    if n == 0 then return K3 (K3(5,4,0))(1,a-1);
    if n == 1 then return tetragCase1(K,a,g);
    if n == 2 then return K3 (K3(3,4,0))(1,a);  
    if n == 3 then return K3 (K3(4,4,0))(1,a);
);

pentagCase0 = (K,a,g) -> (
    x := local x;
    y := local y;
    R := K[x_0..x_4,y_0,y_1,Degrees=>{2:{1,0},3:{1,1},2:{0,1}}];
    M := basis({1,a},R);
    z := local z;
    T := K[z_0..z_(numColumns M -1)];
    A := matrix pack(5,for i to 24 list random({1,1},R));
    A = A - transpose A;
    j := map(quotient pfaffians(4,A),T,M);
    S := Var kernel(j,2); -- trim kernel j; -- Warning: assume that S is cut out by quadrics 
    assert(dim ambient S == g and dim S == 2 and degree S == 2*g-2 and sectionalGenus S == g);
    C := Var trim preimage(j,sub(ideal(y_0),target j));
    assert(dim C == 1 and degree C == 5);
    latticePolarizedK3surface(S,C,{g,5,0})
)

pentagCase1 = (K,a,g) -> (
    x := local x;
    y := local y;
    R := K[x_0..x_4,y_0,y_1,Degrees=>{3:{1,0},2:{1,1},2:{0,1}}];
    M := basis({1,a},R);
    z := local z;
    T := K[z_0..z_(numColumns M -1)];
    A := matrix {{0,0,random({1,1},R),random({1,1},R),random({1,1},R)},
                 {0,0,random({1,1},R),random({1,1},R),random({1,1},R)},
                 {0,0,0              ,random({1,0},R),random({1,0},R)},
                 {0,0,0              ,0              ,random({1,0},R)},
                 {0,0,0              ,0              ,0              }};
    A = A - transpose(A);             
    j := map(quotient pfaffians(4,A),T,M);
    S := Var trim(kernel(j,2)); -- trim kernel j; -- Warning: assume that S is cut out by quadrics
    assert(dim ambient S == g and dim S == 2 and degree S == 2*g-2 and sectionalGenus S == g);
    C := Var trim preimage(j,sub(ideal(y_0),target j));
    assert(dim C == 1 and degree C == 5);
    latticePolarizedK3surface(S,C,{g,5,0})
);

pentagonalK3 = method(TypicalValue => LatticePolarizedK3surface, Options => {CoefficientRing => ZZ/65521});
pentagonalK3 ZZ := o -> g -> (
    if g < 3 then error "expected genus to be at least 3";
    n := (g-1)%5;
    a := lift((g-1-n)/5,ZZ);
    K := o.CoefficientRing;
    if n == 0 then return pentagCase0(K,a,g);
    if n == 1 then return pentagCase1(K,a,g);
    if n == 2 then return K3 (K3(3,5,0))(1,a);
    if n == 3 then return K3 (K3(4,5,0))(1,a);
    if n == 4 then return K3 (K3(5,5,0))(1,a);
);

--#######################################
-*
MAXg = 100;
MAXab = 300;
possible'a'b := (g,d,n) -> (
    local g'; local c;
    L := {};
    for a from 1 to MAXab do
        for b from 0 to MAXab do (
            g' = (a^2*(2*g-2) + 2*a*b*d + b^2*n + 2)/2;
            c = a*d + b*n;
            if floor g' == g' and g' >= g and c > 0 and g' <= MAXg then L = append(L,(floor g',c,a,b));
        );
    L      
);
possibilities = {};
for g from 3 to MAXg do for d from 1 to 15 do for n in {-2,0} do 
    if (g >= 3 and g <= 12 and d == 2 and n == -2) or
       (member(g,{3,4,5,6,7,8,9,10,12}) and d == 1 and n == -2) or 
       ((member(g,{3,4,5}) and d >= 3 and n == -2) and (g != 5 or d <= 8) and (g != 4 or d <= 6) and (g != 3 or d <= 8)) or
       ((member(g,{3,4,5}) and member(d,{3,4,5,6,7,8,9}) and n == 0) and (g != 5 or d <= 9) and (g != 4 or d <= 7) and (g != 3 or d <= 8)) or
       (member(g,{3,4,5,6,7,8,9,10,12}) and d == 0 and n == -2) or
       (g >= 5 and d == 3 and n == 0) or
       (g >= 3 and d == 4 and n == 0) or
       (g >= 3 and d == 5 and n == 0) 
then possibilities = append(possibilities,(g,d,n));
possibilities = sort flatten for l in possibilities list apply(possible'a'b l,u -> append(append(append(u,l_0),l_1),l_2));
-- possibilities = {...,(g',c,a,b,g,d,n),...}
*-
--#######################################
possibilities = {(3,1,1,0,3,1,-2), (3,2,1,0,3,2,-2), (3,3,1,0,3,3,-2), (3,3,1,0,3,3,0), (3,4,1,0,3,4,-2), (3,4,1,0,3,4,0), (3,5,1,0,3,5,-2), (3,5,1,0,3,5,0), (3,6,1,0,3,6,-2), (3,6,1,0,3,6,0), (3,7,1,0,3,7,-2), (3,7,1,0,3,7,0), (3,8,1,0,3,8,-2), (3,8,1,0,3,8,0), (4,1,1,0,4,1,-2), (4,2,1,0,4,2,-2), (4,3,1,0,4,3,-2), (4,3,1,0,4,3,0), (4,4,1,0,4,4,-2), (4,4,1,0,4,4,0), (4,5,1,0,4,5,-2), (4,5,1,0,4,5,0), (4,6,1,0,4,6,-2), (4,6,1,0,4,6,0), (4,7,1,0,4,7,0), (5,1,1,0,5,1,-2), (5,1,1,1,3,3,-2), (5,2,1,0,5,2,-2), (5,3,1,0,5,3,-2), (5,3,1,0,5,3,0), (5,4,1,0,5,4,-2), (5,4,1,0,5,4,0), (5,5,1,0,5,5,-2), (5,5,1,0,5,5,0), (5,6,1,0,5,6,-2), (5,6,1,0,5,6,0), (5,7,1,0,5,7,-2), (5,7,1,0,5,7,0), (5,8,1,0,5,8,-2), (5,8,1,0,5,8,0), (5,9,1,0,5,9,0), (6,1,1,0,6,1,-2), (6,1,1,1,4,3,-2), (6,2,1,0,6,2,-2), (6,2,1,1,3,4,-2), (6,3,1,0,6,3,0), (6,3,1,1,3,3,0), (6,4,1,0,6,4,0), (6,5,1,0,6,5,0), (7,1,1,0,7,1,-2), (7,1,1,1,5,3,-2), (7,2,1,0,7,2,-2), (7,2,1,1,4,4,-2), (7,3,1,0,7,3,0), (7,3,1,1,3,5,-2), (7,3,1,1,4,3,0), (7,4,1,0,7,4,0), (7,4,1,1,3,4,0), (7,5,1,0,7,5,0), (8,1,1,0,8,1,-2), (8,2,1,0,8,2,-2), (8,2,1,1,5,4,-2), (8,3,1,0,8,3,0), (8,3,1,1,4,5,-2), (8,3,1,1,5,3,0), (8,4,1,0,8,4,0), (8,4,1,1,3,6,-2), (8,4,1,1,4,4,0), (8,5,1,0,8,5,0), (8,5,1,1,3,5,0), (9,1,1,0,9,1,-2), (9,1,1,2,3,5,-2), (9,2,1,0,9,2,-2), (9,2,2,0,3,1,-2), (9,3,1,0,9,3,0), (9,3,1,1,5,5,-2), (9,3,1,1,6,3,0), (9,3,1,2,3,3,0), (9,4,1,0,9,4,0), (9,4,1,1,4,6,-2), (9,4,1,1,5,4,0), (9,4,2,0,3,2,-2), (9,5,1,0,9,5,0), (9,5,1,1,3,7,-2), (9,5,1,1,4,5,0), (9,6,1,1,3,6,0), (9,6,2,0,3,3,-2), (9,6,2,0,3,3,0), (9,8,2,0,3,4,-2), (9,8,2,0,3,4,0), (9,10,2,0,3,5,-2), (9,10,2,0,3,5,0), (9,12,2,0,3,6,-2), (9,12,2,0,3,6,0), (9,14,2,0,3,7,-2), (9,14,2,0,3,7,0), (9,16,2,0,3,8,-2), (9,16,2,0,3,8,0), (10,1,1,0,10,1,-2), (10,1,1,2,4,5,-2), (10,2,1,0,10,2,-2), (10,3,1,0,10,3,0), (10,3,1,1,7,3,0), (10,3,1,2,4,3,0), (10,4,1,0,10,4,0), (10,4,1,1,5,6,-2), (10,4,1,1,6,4,0), (10,5,1,0,10,5,0), (10,5,1,1,5,5,0), (10,6,1,1,3,8,-2), (10,6,1,1,4,6,0), (10,7,1,1,3,7,0), (11,1,1,2,5,5,-2), (11,2,1,0,11,2,-2), (11,2,1,2,3,6,-2), (11,3,1,0,11,3,0), (11,3,1,1,8,3,0), (11,3,1,2,5,3,0), (11,4,1,0,11,4,0), (11,4,1,1,7,4,0), (11,4,1,2,3,4,0), (11,5,1,0,11,5,0), (11,5,1,1,5,7,-2), (11,5,1,1,6,5,0), (11,6,1,1,5,6,0), (11,7,1,1,4,7,0), (11,8,1,1,3,8,0), (12,1,1,0,12,1,-2), (12,2,1,0,12,2,-2), (12,2,1,2,4,6,-2), (12,2,2,1,3,2,-2), (12,3,1,0,12,3,0), (12,3,1,1,9,3,0), (12,3,1,2,6,3,0), (12,3,1,3,3,3,0), (12,4,1,0,12,4,0), (12,4,1,1,8,4,0), (12,4,1,2,4,4,0), (12,5,1,0,12,5,0), (12,5,1,1,7,5,0), (12,6,1,1,5,8,-2), (12,7,1,1,5,7,0), (13,2,1,2,5,6,-2), (13,2,2,0,4,1,-2), (13,3,1,0,13,3,0), (13,3,1,1,10,3,0), (13,3,1,2,3,7,-2), (13,3,1,2,7,3,0), (13,3,1,3,4,3,0), (13,4,1,0,13,4,0), (13,4,1,1,9,4,0), (13,4,1,2,5,4,0), (13,4,2,0,4,2,-2), (13,5,1,0,13,5,0), (13,5,1,1,8,5,0), (13,5,1,2,3,5,0), (13,6,2,0,4,3,-2), (13,6,2,0,4,3,0), (13,8,1,1,5,8,0), (13,8,2,0,4,4,-2), (13,8,2,0,4,4,0), (13,10,2,0,4,5,-2), (13,10,2,0,4,5,0), (13,12,2,0,4,6,-2), (13,12,2,0,4,6,0), (13,14,2,0,4,7,0), (14,3,1,0,14,3,0), (14,3,1,1,11,3,0), (14,3,1,2,8,3,0), (14,3,1,3,5,3,0), (14,4,1,0,14,4,0), (14,4,1,1,10,4,0), (14,4,1,2,6,4,0), (14,4,2,1,3,3,-2), (14,5,1,0,14,5,0), (14,5,1,1,9,5,0), (14,5,1,2,4,5,0), (14,9,1,1,5,9,0), (15,1,1,3,3,7,-2), (15,3,1,0,15,3,0), (15,3,1,1,12,3,0), (15,3,1,2,5,7,-2), (15,3,1,2,9,3,0), (15,3,1,3,6,3,0), (15,3,1,4,3,3,0), (15,4,1,0,15,4,0), (15,4,1,1,11,4,0), (15,4,1,2,3,8,-2), (15,4,1,2,7,4,0), (15,4,1,3,3,4,0), (15,5,1,0,15,5,0), (15,5,1,1,10,5,0), (15,5,1,2,5,5,0), (15,6,1,2,3,6,0), (15,6,2,1,3,3,0), (16,2,2,1,4,2,-2), (16,3,1,0,16,3,0), (16,3,1,1,13,3,0), (16,3,1,2,10,3,0), (16,3,1,3,7,3,0), (16,3,1,4,4,3,0), (16,4,1,0,16,4,0), (16,4,1,1,12,4,0), (16,4,1,2,8,4,0), (16,4,1,3,4,4,0), (16,5,1,0,16,5,0), (16,5,1,1,11,5,0), (16,5,1,2,6,5,0), (16,6,1,2,4,6,0), (16,6,2,1,3,4,-2), (17,1,1,3,5,7,-2), (17,2,2,0,5,1,-2), (17,2,2,2,3,3,-2), (17,3,1,0,17,3,0), (17,3,1,1,14,3,0), (17,3,1,2,11,3,0), (17,3,1,3,8,3,0), (17,3,1,4,5,3,0), (17,4,1,0,17,4,0), (17,4,1,1,13,4,0), (17,4,1,2,5,8,-2), (17,4,1,2,9,4,0), (17,4,1,3,5,4,0), (17,4,2,0,5,2,-2), (17,5,1,0,17,5,0), (17,5,1,1,12,5,0), (17,5,1,2,7,5,0), (17,6,1,2,5,6,0), (17,6,2,0,5,3,-2), (17,6,2,0,5,3,0), (17,7,1,2,3,7,0), (17,8,2,0,5,4,-2), (17,8,2,0,5,4,0), (17,8,2,1,3,4,0), (17,10,2,0,5,5,-2), (17,10,2,0,5,5,0), (17,12,2,0,5,6,-2), (17,12,2,0,5,6,0), (17,14,2,0,5,7,-2), (17,14,2,0,5,7,0), (17,16,2,0,5,8,-2), (17,16,2,0,5,8,0), (17,18,2,0,5,9,0), (18,2,1,3,3,8,-2), (18,3,1,0,18,3,0), (18,3,1,1,15,3,0), (18,3,1,2,12,3,0), (18,3,1,3,9,3,0), (18,3,1,4,6,3,0), (18,3,1,5,3,3,0), (18,4,1,0,18,4,0), (18,4,1,1,14,4,0), (18,4,1,2,10,4,0), (18,4,1,3,6,4,0), (18,4,2,1,4,3,-2), (18,5,1,0,18,5,0), (18,5,1,1,13,5,0), (18,5,1,2,8,5,0), (18,5,1,3,3,5,0), (18,7,1,2,4,7,0), (18,8,2,1,3,5,-2), (19,3,1,0,19,3,0), (19,3,1,1,16,3,0), (19,3,1,2,13,3,0), (19,3,1,3,10,3,0), (19,3,1,4,7,3,0), (19,3,1,5,4,3,0), (19,3,3,0,3,1,-2), (19,4,1,0,19,4,0), (19,4,1,1,15,4,0), (19,4,1,2,11,4,0), (19,4,1,3,7,4,0), (19,4,1,4,3,4,0), (19,5,1,0,19,5,0), (19,5,1,1,14,5,0), (19,5,1,2,9,5,0), (19,5,1,3,4,5,0), (19,6,2,1,4,3,0), (19,6,3,0,3,2,-2), (19,7,1,2,5,7,0), (19,8,1,2,3,8,0), (19,9,3,0,3,3,-2), (19,9,3,0,3,3,0), (19,10,2,1,3,5,0), (19,12,3,0,3,4,-2), (19,12,3,0,3,4,0), (19,15,3,0,3,5,-2), (19,15,3,0,3,5,0), (19,18,3,0,3,6,-2), (19,18,3,0,3,6,0), (19,21,3,0,3,7,-2), (19,21,3,0,3,7,0), (19,24,3,0,3,8,-2), (19,24,3,0,3,8,0), (20,2,1,3,5,8,-2), (20,2,2,1,5,2,-2), (20,3,1,0,20,3,0), (20,3,1,1,17,3,0), (20,3,1,2,14,3,0), (20,3,1,3,11,3,0), (20,3,1,4,8,3,0), (20,3,1,5,5,3,0), (20,4,1,0,20,4,0), (20,4,1,1,16,4,0), (20,4,1,2,12,4,0), (20,4,1,3,8,4,0), (20,4,1,4,4,4,0), (20,5,1,0,20,5,0), (20,5,1,1,15,5,0), (20,5,1,2,10,5,0), (20,5,1,3,5,5,0), (20,6,2,1,4,4,-2), (20,10,2,1,3,6,-2), (21,1,3,1,3,1,-2), (21,2,2,0,6,1,-2), (21,2,2,2,4,3,-2), (21,3,1,0,21,3,0), (21,3,1,1,18,3,0), (21,3,1,2,15,3,0), (21,3,1,3,12,3,0), (21,3,1,4,9,3,0), (21,3,1,5,6,3,0), (21,3,1,6,3,3,0), (21,4,1,0,21,4,0), (21,4,1,1,17,4,0), (21,4,1,2,13,4,0), (21,4,1,3,9,4,0), (21,4,1,4,5,4,0), (21,4,2,0,6,2,-2), (21,4,2,2,3,4,-2), (21,5,1,0,21,5,0), (21,5,1,1,16,5,0), (21,5,1,2,11,5,0), (21,5,1,3,6,5,0), (21,6,1,3,3,6,0), (21,6,2,0,6,3,0), (21,6,2,2,3,3,0), (21,8,1,2,5,8,0), (21,8,2,0,6,4,0), (21,8,2,1,4,4,0), (21,10,2,0,6,5,0), (21,12,2,1,3,6,0), (22,3,1,0,22,3,0), (22,3,1,1,19,3,0), (22,3,1,2,16,3,0), (22,3,1,3,13,3,0), (22,3,1,4,10,3,0), (22,3,1,5,7,3,0), (22,3,1,6,4,3,0), (22,4,1,0,22,4,0), (22,4,1,1,18,4,0), (22,4,1,2,14,4,0), (22,4,1,3,10,4,0), (22,4,1,4,6,4,0), (22,4,2,1,5,3,-2), (22,5,1,0,22,5,0), (22,5,1,1,17,5,0), (22,5,1,2,12,5,0), (22,5,1,3,7,5,0), (22,6,1,3,4,6,0), (22,8,2,1,4,5,-2), (22,12,2,1,3,7,-2), (23,3,1,0,23,3,0), (23,3,1,1,20,3,0), (23,3,1,2,17,3,0), (23,3,1,3,14,3,0), (23,3,1,4,11,3,0), (23,3,1,5,8,3,0), (23,3,1,6,5,3,0), (23,4,1,0,23,4,0), (23,4,1,1,19,4,0), (23,4,1,2,15,4,0), (23,4,1,3,11,4,0), (23,4,1,4,7,4,0), (23,4,1,5,3,4,0), (23,5,1,0,23,5,0), (23,5,1,1,18,5,0), (23,5,1,2,13,5,0), (23,5,1,3,8,5,0), (23,5,1,4,3,5,0), (23,6,1,3,5,6,0), (23,6,2,1,5,3,0), (23,9,1,2,5,9,0), (23,10,2,1,4,5,0), (23,14,2,1,3,7,0), (24,2,2,1,6,2,-2), (24,2,2,3,3,4,-2), (24,3,1,0,24,3,0), (24,3,1,1,21,3,0), (24,3,1,2,18,3,0), (24,3,1,3,15,3,0), (24,3,1,4,12,3,0), (24,3,1,5,9,3,0), (24,3,1,6,6,3,0), (24,3,1,7,3,3,0), (24,4,1,0,24,4,0), (24,4,1,1,20,4,0), (24,4,1,2,16,4,0), (24,4,1,3,12,4,0), (24,4,1,4,8,4,0), (24,4,1,5,4,4,0), (24,4,3,1,3,2,-2), (24,5,1,0,24,5,0), (24,5,1,1,19,5,0), (24,5,1,2,14,5,0), (24,5,1,3,9,5,0), (24,5,1,4,4,5,0), (24,6,2,1,5,4,-2), (24,7,1,3,3,7,0), (24,10,2,1,4,6,-2), (24,14,2,1,3,8,-2), (25,2,2,0,7,1,-2), (25,2,2,2,5,3,-2), (25,3,1,0,25,3,0), (25,3,1,1,22,3,0), (25,3,1,2,19,3,0), (25,3,1,3,16,3,0), (25,3,1,4,13,3,0), (25,3,1,5,10,3,0), (25,3,1,6,7,3,0), (25,3,1,7,4,3,0), (25,4,1,0,25,4,0), (25,4,1,1,21,4,0), (25,4,1,2,17,4,0), (25,4,1,3,13,4,0), (25,4,1,4,9,4,0), (25,4,1,5,5,4,0), (25,4,2,0,7,2,-2), (25,4,2,2,4,4,-2), (25,5,1,0,25,5,0), (25,5,1,1,20,5,0), (25,5,1,2,15,5,0), (25,5,1,3,10,5,0), (25,5,1,4,5,5,0), (25,6,2,0,7,3,0), (25,6,2,2,3,5,-2), (25,6,2,2,4,3,0), (25,7,1,3,4,7,0), (25,8,2,0,7,4,0), (25,8,2,1,5,4,0), (25,8,2,2,3,4,0), (25,10,2,0,7,5,0), (25,12,2,1,4,6,0), (25,16,2,1,3,8,0), (26,3,1,0,26,3,0), (26,3,1,1,23,3,0), (26,3,1,2,20,3,0), (26,3,1,3,17,3,0), (26,3,1,4,14,3,0), (26,3,1,5,11,3,0), (26,3,1,6,8,3,0), (26,3,1,7,5,3,0), (26,4,1,0,26,4,0), (26,4,1,1,22,4,0), (26,4,1,2,18,4,0), (26,4,1,3,14,4,0), (26,4,1,4,10,4,0), (26,4,1,5,6,4,0), (26,5,1,0,26,5,0), (26,5,1,1,21,5,0), (26,5,1,2,16,5,0), (26,5,1,3,11,5,0), (26,5,1,4,6,5,0), (26,7,1,3,5,7,0), (26,8,2,1,5,5,-2), (27,2,3,2,3,2,-2), (27,3,1,0,27,3,0), (27,3,1,1,24,3,0), (27,3,1,2,21,3,0), (27,3,1,3,18,3,0), (27,3,1,4,15,3,0), (27,3,1,5,12,3,0), (27,3,1,6,9,3,0), (27,3,1,7,6,3,0), (27,3,1,8,3,3,0), (27,4,1,0,27,4,0), (27,4,1,1,23,4,0), (27,4,1,2,19,4,0), (27,4,1,3,15,4,0), (27,4,1,4,11,4,0), (27,4,1,5,7,4,0), (27,4,1,6,3,4,0), (27,5,1,0,27,5,0), (27,5,1,1,22,5,0), (27,5,1,2,17,5,0), (27,5,1,3,12,5,0), (27,5,1,4,7,5,0), (27,6,1,4,3,6,0), (27,6,2,1,6,3,0), (27,6,2,3,3,3,0), (27,7,3,1,3,3,-2), (27,8,1,3,3,8,0), (27,10,2,1,5,5,0), (27,14,2,1,4,7,0), (28,2,2,1,7,2,-2), (28,2,2,3,4,4,-2), (28,3,1,0,28,3,0), (28,3,1,1,25,3,0), (28,3,1,2,22,3,0), (28,3,1,3,19,3,0), (28,3,1,4,16,3,0), (28,3,1,5,13,3,0), (28,3,1,6,10,3,0), (28,3,1,7,7,3,0), (28,3,1,8,4,3,0), (28,3,3,0,4,1,-2), (28,4,1,0,28,4,0), (28,4,1,1,24,4,0), (28,4,1,2,20,4,0), (28,4,1,3,16,4,0), (28,4,1,4,12,4,0), (28,4,1,5,8,4,0), (28,4,1,6,4,4,0), (28,5,1,0,28,5,0), (28,5,1,1,23,5,0), (28,5,1,2,18,5,0), (28,5,1,3,13,5,0), (28,5,1,4,8,5,0), (28,5,1,5,3,5,0), (28,6,1,4,4,6,0), (28,6,3,0,4,2,-2), (28,9,3,0,4,3,-2), (28,9,3,0,4,3,0), (28,9,3,1,3,3,0), (28,10,2,1,5,6,-2), (28,12,3,0,4,4,-2), (28,12,3,0,4,4,0), (28,15,3,0,4,5,-2), (28,15,3,0,4,5,0), (28,18,3,0,4,6,-2), (28,18,3,0,4,6,0), (28,21,3,0,4,7,0), (29,2,2,0,8,1,-2), (29,3,1,0,29,3,0), (29,3,1,1,26,3,0), (29,3,1,2,23,3,0), (29,3,1,3,20,3,0), (29,3,1,4,17,3,0), (29,3,1,5,14,3,0), (29,3,1,6,11,3,0), (29,3,1,7,8,3,0), (29,3,1,8,5,3,0), (29,4,1,0,29,4,0), (29,4,1,1,25,4,0), (29,4,1,2,21,4,0), (29,4,1,3,17,4,0), (29,4,1,4,13,4,0), (29,4,1,5,9,4,0), (29,4,1,6,5,4,0), (29,4,2,0,8,2,-2), (29,4,2,2,5,4,-2), (29,5,1,0,29,5,0), (29,5,1,1,24,5,0), (29,5,1,2,19,5,0), (29,5,1,3,14,5,0), (29,5,1,4,9,5,0), (29,5,1,5,4,5,0), (29,6,1,4,5,6,0), (29,6,2,0,8,3,0), (29,6,2,2,4,5,-2), (29,6,2,2,5,3,0), (29,8,1,3,5,8,0), (29,8,2,0,8,4,0), (29,8,2,1,6,4,0), (29,8,2,2,3,6,-2), (29,8,2,2,4,4,0), (29,10,2,0,8,5,0), (29,10,2,2,3,5,0), (29,12,2,1,5,6,0), (30,1,3,1,4,1,-2), (30,3,1,0,30,3,0), (30,3,1,1,27,3,0), (30,3,1,2,24,3,0), (30,3,1,3,21,3,0), (30,3,1,4,18,3,0), (30,3,1,5,15,3,0), (30,3,1,6,12,3,0), (30,3,1,7,9,3,0), (30,3,1,8,6,3,0), (30,3,1,9,3,3,0), (30,4,1,0,30,4,0), (30,4,1,1,26,4,0), (30,4,1,2,22,4,0), (30,4,1,3,18,4,0), (30,4,1,4,14,4,0), (30,4,1,5,10,4,0), (30,4,1,6,6,4,0), (30,4,2,3,3,5,-2), (30,5,1,0,30,5,0), (30,5,1,1,25,5,0), (30,5,1,2,20,5,0), (30,5,1,3,15,5,0), (30,5,1,4,10,5,0), (30,5,1,5,5,5,0), (30,10,3,1,3,4,-2), (30,12,2,1,5,7,-2), (31,3,1,0,31,3,0), (31,3,1,1,28,3,0), (31,3,1,2,25,3,0), (31,3,1,3,22,3,0), (31,3,1,4,19,3,0), (31,3,1,5,16,3,0), (31,3,1,6,13,3,0), (31,3,1,7,10,3,0), (31,3,1,8,7,3,0), (31,3,1,9,4,3,0), (31,4,1,0,31,4,0), (31,4,1,1,27,4,0), (31,4,1,2,23,4,0), (31,4,1,3,19,4,0), (31,4,1,4,15,4,0), (31,4,1,5,11,4,0), (31,4,1,6,7,4,0), (31,4,1,7,3,4,0), (31,5,1,0,31,5,0), (31,5,1,1,26,5,0), (31,5,1,2,21,5,0), (31,5,1,3,16,5,0), (31,5,1,4,11,5,0), (31,5,1,5,6,5,0), (31,6,2,1,7,3,0), (31,6,2,3,4,3,0), (31,7,1,4,3,7,0), (31,10,2,1,6,5,0), (31,12,3,1,3,4,0), (31,14,2,1,5,7,0), (32,2,2,1,8,2,-2), (32,2,2,3,5,4,-2), (32,3,1,0,32,3,0), (32,3,1,1,29,3,0), (32,3,1,2,26,3,0), (32,3,1,3,23,3,0), (32,3,1,4,20,3,0), (32,3,1,5,17,3,0), (32,3,1,6,14,3,0), (32,3,1,7,11,3,0), (32,3,1,8,8,3,0), (32,3,1,9,5,3,0), (32,4,1,0,32,4,0), (32,4,1,1,28,4,0), (32,4,1,2,24,4,0), (32,4,1,3,20,4,0), (32,4,1,4,16,4,0), (32,4,1,5,12,4,0), (32,4,1,6,8,4,0), (32,4,1,7,4,4,0), (32,5,1,0,32,5,0), (32,5,1,1,27,5,0), (32,5,1,2,22,5,0), (32,5,1,3,17,5,0), (32,5,1,4,12,5,0), (32,5,1,5,7,5,0), (32,7,1,4,4,7,0), (32,9,1,3,5,9,0), (32,14,2,1,5,8,-2), (33,2,2,0,9,1,-2), (33,2,2,4,3,5,-2), (33,3,1,0,33,3,0), (33,3,1,1,30,3,0), (33,3,1,2,27,3,0), (33,3,1,3,24,3,0), (33,3,1,4,21,3,0), (33,3,1,5,18,3,0), (33,3,1,6,15,3,0), (33,3,1,7,12,3,0), (33,3,1,8,9,3,0), (33,3,1,9,6,3,0), (33,3,1,10,3,3,0), (33,4,1,0,33,4,0), (33,4,1,1,29,4,0), (33,4,1,2,25,4,0), (33,4,1,3,21,4,0), (33,4,1,4,17,4,0), (33,4,1,5,13,4,0), (33,4,1,6,9,4,0), (33,4,1,7,5,4,0), (33,4,2,0,9,2,-2), (33,4,3,1,4,2,-2), (33,4,4,0,3,1,-2), (33,5,1,0,33,5,0), (33,5,1,1,28,5,0), (33,5,1,2,23,5,0), (33,5,1,3,18,5,0), (33,5,1,4,13,5,0), (33,5,1,5,8,5,0), (33,5,1,6,3,5,0), (33,5,3,2,3,3,-2), (33,6,1,5,3,6,0), (33,6,2,0,9,3,0), (33,6,2,2,5,5,-2), (33,6,2,2,6,3,0), (33,6,2,4,3,3,0), (33,7,1,4,5,7,0), (33,8,2,0,9,4,0), (33,8,2,1,7,4,0), (33,8,2,2,4,6,-2), (33,8,2,2,5,4,0), (33,8,2,3,3,4,0), (33,8,4,0,3,2,-2), (33,10,2,0,9,5,0), (33,10,2,2,3,7,-2), (33,10,2,2,4,5,0), (33,12,2,2,3,6,0), (33,12,4,0,3,3,-2), (33,12,4,0,3,3,0), (33,13,3,1,3,5,-2), (33,16,2,1,5,8,0), (33,16,4,0,3,4,-2), (33,16,4,0,3,4,0), (33,20,4,0,3,5,-2), (33,20,4,0,3,5,0), (33,24,4,0,3,6,-2), (33,24,4,0,3,6,0), (33,28,4,0,3,7,-2), (33,28,4,0,3,7,0), (33,32,4,0,3,8,-2), (33,32,4,0,3,8,0), (34,3,1,0,34,3,0), (34,3,1,1,31,3,0), (34,3,1,2,28,3,0), (34,3,1,3,25,3,0), (34,3,1,4,22,3,0), (34,3,1,5,19,3,0), (34,3,1,6,16,3,0), (34,3,1,7,13,3,0), (34,3,1,8,10,3,0), (34,3,1,9,7,3,0), (34,3,1,10,4,3,0), (34,4,1,0,34,4,0), (34,4,1,1,30,4,0), (34,4,1,2,26,4,0), (34,4,1,3,22,4,0), (34,4,1,4,18,4,0), (34,4,1,5,14,4,0), (34,4,1,6,10,4,0), (34,4,1,7,6,4,0), (34,4,2,3,4,5,-2), (34,5,1,0,34,5,0), (34,5,1,1,29,5,0), (34,5,1,2,24,5,0), (34,5,1,3,19,5,0), (34,5,1,4,14,5,0), (34,5,1,5,9,5,0), (34,5,1,6,4,5,0), (34,6,1,5,4,6,0), (34,15,3,1,3,5,0), (35,3,1,0,35,3,0), (35,3,1,1,32,3,0), (35,3,1,2,29,3,0), (35,3,1,3,26,3,0), (35,3,1,4,23,3,0), (35,3,1,5,20,3,0), (35,3,1,6,17,3,0), (35,3,1,7,14,3,0), (35,3,1,8,11,3,0), (35,3,1,9,8,3,0), (35,3,1,10,5,3,0), (35,4,1,0,35,4,0), (35,4,1,1,31,4,0), (35,4,1,2,27,4,0), (35,4,1,3,23,4,0), (35,4,1,4,19,4,0), (35,4,1,5,15,4,0), (35,4,1,6,11,4,0), (35,4,1,7,7,4,0), (35,4,1,8,3,4,0), (35,5,1,0,35,5,0), (35,5,1,1,30,5,0), (35,5,1,2,25,5,0), (35,5,1,3,20,5,0), (35,5,1,4,15,5,0), (35,5,1,5,10,5,0), (35,5,1,6,5,5,0), (35,6,1,5,5,6,0), (35,6,2,1,8,3,0), (35,6,2,3,5,3,0), (35,8,1,4,3,8,0), (35,10,2,1,7,5,0), (35,18,2,1,5,9,0), (36,2,2,1,9,2,-2), (36,2,3,2,4,2,-2), (36,2,4,1,3,1,-2), (36,3,1,0,36,3,0), (36,3,1,1,33,3,0), (36,3,1,2,30,3,0), (36,3,1,3,27,3,0), (36,3,1,4,24,3,0), (36,3,1,5,21,3,0), (36,3,1,6,18,3,0), (36,3,1,7,15,3,0), (36,3,1,8,12,3,0), (36,3,1,9,9,3,0), (36,3,1,10,6,3,0), (36,3,1,11,3,3,0), (36,4,1,0,36,4,0), (36,4,1,1,32,4,0), (36,4,1,2,28,4,0), (36,4,1,3,24,4,0), (36,4,1,4,20,4,0), (36,4,1,5,16,4,0), (36,4,1,6,12,4,0), (36,4,1,7,8,4,0), (36,4,1,8,4,4,0), (36,5,1,0,36,5,0), (36,5,1,1,31,5,0), (36,5,1,2,26,5,0), (36,5,1,3,21,5,0), (36,5,1,4,16,5,0), (36,5,1,5,11,5,0), (36,5,1,6,6,5,0), (36,6,2,3,3,6,-2), (36,7,3,1,4,3,-2), (36,16,3,1,3,6,-2), (37,2,2,0,10,1,-2), (37,2,2,4,4,5,-2), (37,3,1,0,37,3,0), (37,3,1,1,34,3,0), (37,3,1,2,31,3,0), (37,3,1,3,28,3,0), (37,3,1,4,25,3,0), (37,3,1,5,22,3,0), (37,3,1,6,19,3,0), (37,3,1,7,16,3,0), (37,3,1,8,13,3,0), (37,3,1,9,10,3,0), (37,3,1,10,7,3,0), (37,3,1,11,4,3,0), (37,3,3,0,5,1,-2), (37,3,3,3,3,3,-2), (37,4,1,0,37,4,0), (37,4,1,1,33,4,0), (37,4,1,2,29,4,0), (37,4,1,3,25,4,0), (37,4,1,4,21,4,0), (37,4,1,5,17,4,0), (37,4,1,6,13,4,0), (37,4,1,7,9,4,0), (37,4,1,8,5,4,0), (37,4,2,0,10,2,-2), (37,5,1,0,37,5,0), (37,5,1,1,32,5,0), (37,5,1,2,27,5,0), (37,5,1,3,22,5,0), (37,5,1,4,17,5,0), (37,5,1,5,12,5,0), (37,5,1,6,7,5,0), (37,6,2,0,10,3,0), (37,6,2,2,7,3,0), (37,6,2,4,4,3,0), (37,6,3,0,5,2,-2), (37,8,1,4,5,8,0), (37,8,2,0,10,4,0), (37,8,2,1,8,4,0), (37,8,2,2,5,6,-2), (37,8,2,2,6,4,0), (37,8,2,3,4,4,0), (37,9,3,0,5,3,-2), (37,9,3,0,5,3,0), (37,9,3,1,4,3,0), (37,9,3,2,3,3,0), (37,10,2,0,10,5,0), (37,10,2,2,5,5,0), (37,12,2,2,3,8,-2), (37,12,2,2,4,6,0), (37,12,3,0,5,4,-2), (37,12,3,0,5,4,0), (37,14,2,2,3,7,0), (37,15,3,0,5,5,-2), (37,15,3,0,5,5,0), (37,18,3,0,5,6,-2), (37,18,3,0,5,6,0), (37,18,3,1,3,6,0), (37,21,3,0,5,7,-2), (37,21,3,0,5,7,0), (37,24,3,0,5,8,-2), (37,24,3,0,5,8,0), (37,27,3,0,5,9,0), (38,3,1,0,38,3,0), (38,3,1,1,35,3,0), (38,3,1,2,32,3,0), (38,3,1,3,29,3,0), (38,3,1,4,26,3,0), (38,3,1,5,23,3,0), (38,3,1,6,20,3,0), (38,3,1,7,17,3,0), (38,3,1,8,14,3,0), (38,3,1,9,11,3,0), (38,3,1,10,8,3,0), (38,3,1,11,5,3,0), (38,4,1,0,38,4,0), (38,4,1,1,34,4,0), (38,4,1,2,30,4,0), (38,4,1,3,26,4,0), (38,4,1,4,22,4,0), (38,4,1,5,18,4,0), (38,4,1,6,14,4,0), (38,4,1,7,10,4,0), (38,4,1,8,6,4,0), (38,4,2,3,5,5,-2), (38,5,1,0,38,5,0), (38,5,1,1,33,5,0), (38,5,1,2,28,5,0), (38,5,1,3,23,5,0), (38,5,1,4,18,5,0), (38,5,1,5,13,5,0), (38,5,1,6,8,5,0), (38,5,1,7,3,5,0), (38,7,1,5,3,7,0), (39,1,3,1,5,1,-2), (39,1,3,4,3,3,-2), (39,3,1,0,39,3,0), (39,3,1,1,36,3,0), (39,3,1,2,33,3,0), (39,3,1,3,30,3,0), (39,3,1,4,27,3,0), (39,3,1,5,24,3,0), (39,3,1,6,21,3,0), (39,3,1,7,18,3,0), (39,3,1,8,15,3,0), (39,3,1,9,12,3,0), (39,3,1,10,9,3,0), (39,3,1,11,6,3,0), (39,3,1,12,3,3,0), (39,4,1,0,39,4,0), (39,4,1,1,35,4,0), (39,4,1,2,31,4,0), (39,4,1,3,27,4,0), (39,4,1,4,23,4,0), (39,4,1,5,19,4,0), (39,4,1,6,15,4,0), (39,4,1,7,11,4,0), (39,4,1,8,7,4,0), (39,4,1,9,3,4,0), (39,5,1,0,39,5,0), (39,5,1,1,34,5,0), (39,5,1,2,29,5,0), (39,5,1,3,24,5,0), (39,5,1,4,19,5,0), (39,5,1,5,14,5,0), (39,5,1,6,9,5,0), (39,5,1,7,4,5,0), (39,6,1,6,3,6,0), (39,6,2,1,9,3,0), (39,6,2,3,6,3,0), (39,6,2,5,3,3,0), (39,7,1,5,4,7,0), (39,8,3,2,3,4,-2), (39,10,2,1,8,5,0), (39,10,2,3,3,5,0), (39,10,3,1,4,4,-2), (39,19,3,1,3,7,-2), (40,2,2,1,10,2,-2), (40,3,1,0,40,3,0), (40,3,1,1,37,3,0), (40,3,1,2,34,3,0), (40,3,1,3,31,3,0), (40,3,1,4,28,3,0), (40,3,1,5,25,3,0), (40,3,1,6,22,3,0), (40,3,1,7,19,3,0), (40,3,1,8,16,3,0), (40,3,1,9,13,3,0), (40,3,1,10,10,3,0), (40,3,1,11,7,3,0), (40,3,1,12,4,3,0), (40,4,1,0,40,4,0), (40,4,1,1,36,4,0), (40,4,1,2,32,4,0), (40,4,1,3,28,4,0), (40,4,1,4,24,4,0), (40,4,1,5,20,4,0), (40,4,1,6,16,4,0), (40,4,1,7,12,4,0), (40,4,1,8,8,4,0), (40,4,1,9,4,4,0), (40,5,1,0,40,5,0), (40,5,1,1,35,5,0), (40,5,1,2,30,5,0), (40,5,1,3,25,5,0), (40,5,1,4,20,5,0), (40,5,1,5,15,5,0), (40,5,1,6,10,5,0), (40,5,1,7,5,5,0), (40,6,1,6,4,6,0), (40,6,2,3,4,6,-2), (40,6,4,1,3,2,-2), (40,7,1,5,5,7,0), (40,12,3,1,4,4,0), (40,21,3,1,3,7,0), (41,2,2,4,5,5,-2), (41,3,1,0,41,3,0), (41,3,1,1,38,3,0), (41,3,1,2,35,3,0), (41,3,1,3,32,3,0), (41,3,1,4,29,3,0), (41,3,1,5,26,3,0), (41,3,1,6,23,3,0), (41,3,1,7,20,3,0), (41,3,1,8,17,3,0), (41,3,1,9,14,3,0), (41,3,1,10,11,3,0), (41,3,1,11,8,3,0), (41,3,1,12,5,3,0), (41,4,1,0,41,4,0), (41,4,1,1,37,4,0), (41,4,1,2,33,4,0), (41,4,1,3,29,4,0), (41,4,1,4,25,4,0), (41,4,1,5,21,4,0), (41,4,1,6,17,4,0), (41,4,1,7,13,4,0), (41,4,1,8,9,4,0), (41,4,1,9,5,4,0), (41,4,2,0,11,2,-2), (41,4,2,4,3,6,-2), (41,5,1,0,41,5,0), (41,5,1,1,36,5,0), (41,5,1,2,31,5,0), (41,5,1,3,26,5,0), (41,5,1,4,21,5,0), (41,5,1,5,16,5,0), (41,5,1,6,11,5,0), (41,5,1,7,6,5,0), (41,6,1,6,5,6,0), (41,6,2,0,11,3,0), (41,6,2,2,8,3,0), (41,6,2,4,5,3,0), (41,8,2,0,11,4,0), (41,8,2,1,9,4,0), (41,8,2,2,7,4,0), (41,8,2,3,5,4,0), (41,8,2,4,3,4,0), (41,9,1,4,5,9,0), (41,10,2,0,11,5,0), (41,10,2,2,5,7,-2), (41,10,2,2,6,5,0), (41,12,2,2,5,6,0), (41,14,2,2,4,7,0), (41,16,2,2,3,8,0), (42,3,1,0,42,3,0), (42,3,1,1,39,3,0), (42,3,1,2,36,3,0), (42,3,1,3,33,3,0), (42,3,1,4,30,3,0), (42,3,1,5,27,3,0), (42,3,1,6,24,3,0), (42,3,1,7,21,3,0), (42,3,1,8,18,3,0), (42,3,1,9,15,3,0), (42,3,1,10,12,3,0), (42,3,1,11,9,3,0), (42,3,1,12,6,3,0), (42,3,1,13,3,3,0), (42,4,1,0,42,4,0), (42,4,1,1,38,4,0), (42,4,1,2,34,4,0), (42,4,1,3,30,4,0), (42,4,1,4,26,4,0), (42,4,1,5,22,4,0), (42,4,1,6,18,4,0), (42,4,1,7,14,4,0), (42,4,1,8,10,4,0), (42,4,1,9,6,4,0), (42,4,3,1,5,2,-2), (42,5,1,0,42,5,0), (42,5,1,1,37,5,0), (42,5,1,2,32,5,0), (42,5,1,3,27,5,0), (42,5,1,4,22,5,0), (42,5,1,5,17,5,0), (42,5,1,6,12,5,0), (42,5,1,7,7,5,0), (42,5,3,2,4,3,-2), (42,8,2,3,3,7,-2), (42,13,3,1,4,5,-2), (42,22,3,1,3,8,-2), (43,3,1,0,43,3,0), (43,3,1,1,40,3,0), (43,3,1,2,37,3,0), (43,3,1,3,34,3,0), (43,3,1,4,31,3,0), (43,3,1,5,28,3,0), (43,3,1,6,25,3,0), (43,3,1,7,22,3,0), (43,3,1,8,19,3,0), (43,3,1,9,16,3,0), (43,3,1,10,13,3,0), (43,3,1,11,10,3,0), (43,3,1,12,7,3,0), (43,3,1,13,4,3,0), (43,4,1,0,43,4,0), (43,4,1,1,39,4,0), (43,4,1,2,35,4,0), (43,4,1,3,31,4,0), (43,4,1,4,27,4,0), (43,4,1,5,23,4,0), (43,4,1,6,19,4,0), (43,4,1,7,15,4,0), (43,4,1,8,11,4,0), (43,4,1,9,7,4,0), (43,4,1,10,3,4,0), (43,5,1,0,43,5,0), (43,5,1,1,38,5,0), (43,5,1,2,33,5,0), (43,5,1,3,28,5,0), (43,5,1,4,23,5,0), (43,5,1,5,18,5,0), (43,5,1,6,13,5,0), (43,5,1,7,8,5,0), (43,5,1,8,3,5,0), (43,6,2,1,10,3,0), (43,6,2,3,7,3,0), (43,6,2,5,4,3,0), (43,8,1,5,3,8,0), (43,10,2,1,9,5,0), (43,10,2,3,4,5,0), (43,12,3,2,3,4,0), (43,15,3,1,4,5,0), (43,24,3,1,3,8,0), (44,2,2,1,11,2,-2), (44,2,2,5,3,6,-2), (44,3,1,0,44,3,0), (44,3,1,1,41,3,0), (44,3,1,2,38,3,0), (44,3,1,3,35,3,0), (44,3,1,4,32,3,0), (44,3,1,5,29,3,0), (44,3,1,6,26,3,0), (44,3,1,7,23,3,0), (44,3,1,8,20,3,0), (44,3,1,9,17,3,0), (44,3,1,10,14,3,0), (44,3,1,11,11,3,0), (44,3,1,12,8,3,0), (44,3,1,13,5,3,0), (44,4,1,0,44,4,0), (44,4,1,1,40,4,0), (44,4,1,2,36,4,0), (44,4,1,3,32,4,0), (44,4,1,4,28,4,0), (44,4,1,5,24,4,0), (44,4,1,6,20,4,0), (44,4,1,7,16,4,0), (44,4,1,8,12,4,0), (44,4,1,9,8,4,0), (44,4,1,10,4,4,0), (44,5,1,0,44,5,0), (44,5,1,1,39,5,0), (44,5,1,2,34,5,0), (44,5,1,3,29,5,0), (44,5,1,4,24,5,0), (44,5,1,5,19,5,0), (44,5,1,6,14,5,0), (44,5,1,7,9,5,0), (44,5,1,8,4,5,0), (44,6,2,3,5,6,-2), (44,10,4,1,3,3,-2), (45,2,2,0,12,1,-2), (45,2,3,2,5,2,-2), (45,3,1,0,45,3,0), (45,3,1,1,42,3,0), (45,3,1,2,39,3,0), (45,3,1,3,36,3,0), (45,3,1,4,33,3,0), (45,3,1,5,30,3,0), (45,3,1,6,27,3,0), (45,3,1,7,24,3,0), (45,3,1,8,21,3,0), (45,3,1,9,18,3,0), (45,3,1,10,15,3,0), (45,3,1,11,12,3,0), (45,3,1,12,9,3,0), (45,3,1,13,6,3,0), (45,3,1,14,3,3,0), (45,4,1,0,45,4,0), (45,4,1,1,41,4,0), (45,4,1,2,37,4,0), (45,4,1,3,33,4,0), (45,4,1,4,29,4,0), (45,4,1,5,25,4,0), (45,4,1,6,21,4,0), (45,4,1,7,17,4,0), (45,4,1,8,13,4,0), (45,4,1,9,9,4,0), (45,4,1,10,5,4,0), (45,4,2,0,12,2,-2), (45,4,2,4,4,6,-2), (45,4,4,2,3,2,-2), (45,5,1,0,45,5,0), (45,5,1,1,40,5,0), (45,5,1,2,35,5,0), (45,5,1,3,30,5,0), (45,5,1,4,25,5,0), (45,5,1,5,20,5,0), (45,5,1,6,15,5,0), (45,5,1,7,10,5,0), (45,5,1,8,5,5,0), (45,6,1,7,3,6,0), (45,6,2,0,12,3,0), (45,6,2,2,9,3,0), (45,6,2,4,6,3,0), (45,6,2,6,3,3,0), (45,7,1,6,3,7,0), (45,7,3,1,5,3,-2), (45,8,1,5,5,8,0), (45,8,2,0,12,4,0), (45,8,2,1,10,4,0), (45,8,2,2,8,4,0), (45,8,2,3,6,4,0), (45,8,2,4,4,4,0), (45,10,2,0,12,5,0), (45,10,2,2,7,5,0), (45,11,3,2,3,5,-2), (45,12,2,2,5,8,-2), (45,12,2,3,3,6,0), (45,12,4,1,3,3,0), (45,14,2,2,5,7,0), (45,16,3,1,4,6,-2), (46,3,1,0,46,3,0), (46,3,1,1,43,3,0), (46,3,1,2,40,3,0), (46,3,1,3,37,3,0), (46,3,1,4,34,3,0), (46,3,1,5,31,3,0), (46,3,1,6,28,3,0), (46,3,1,7,25,3,0), (46,3,1,8,22,3,0), (46,3,1,9,19,3,0), (46,3,1,10,16,3,0), (46,3,1,11,13,3,0), (46,3,1,12,10,3,0), (46,3,1,13,7,3,0), (46,3,1,14,4,3,0), (46,3,3,0,6,1,-2), (46,3,3,3,4,3,-2), (46,4,1,0,46,4,0), (46,4,1,1,42,4,0), (46,4,1,2,38,4,0), (46,4,1,3,34,4,0), (46,4,1,4,30,4,0), (46,4,1,5,26,4,0), (46,4,1,6,22,4,0), (46,4,1,7,18,4,0), (46,4,1,8,14,4,0), (46,4,1,9,10,4,0), (46,4,1,10,6,4,0), (46,5,1,0,46,5,0), (46,5,1,1,41,5,0), (46,5,1,2,36,5,0), (46,5,1,3,31,5,0), (46,5,1,4,26,5,0), (46,5,1,5,21,5,0), (46,5,1,6,16,5,0), (46,5,1,7,11,5,0), (46,5,1,8,6,5,0), (46,6,1,7,4,6,0), (46,6,3,0,6,2,-2), (46,6,3,3,3,4,-2), (46,7,1,6,4,7,0), (46,9,3,0,6,3,0), (46,9,3,1,5,3,0), (46,9,3,2,4,3,0), (46,9,3,3,3,3,0), (46,12,3,0,6,4,0), (46,15,3,0,6,5,0), (46,18,3,1,4,6,0), (47,3,1,0,47,3,0), (47,3,1,1,44,3,0), (47,3,1,2,41,3,0), (47,3,1,3,38,3,0), (47,3,1,4,35,3,0), (47,3,1,5,32,3,0), (47,3,1,6,29,3,0), (47,3,1,7,26,3,0), (47,3,1,8,23,3,0), (47,3,1,9,20,3,0), (47,3,1,10,17,3,0), (47,3,1,11,14,3,0), (47,3,1,12,11,3,0), (47,3,1,13,8,3,0), (47,3,1,14,5,3,0), (47,4,1,0,47,4,0), (47,4,1,1,43,4,0), (47,4,1,2,39,4,0), (47,4,1,3,35,4,0), (47,4,1,4,31,4,0), (47,4,1,5,27,4,0), (47,4,1,6,23,4,0), (47,4,1,7,19,4,0), (47,4,1,8,15,4,0), (47,4,1,9,11,4,0), (47,4,1,10,7,4,0), (47,4,1,11,3,4,0), (47,5,1,0,47,5,0), (47,5,1,1,42,5,0), (47,5,1,2,37,5,0), (47,5,1,3,32,5,0), (47,5,1,4,27,5,0), (47,5,1,5,22,5,0), (47,5,1,6,17,5,0), (47,5,1,7,12,5,0), (47,5,1,8,7,5,0), (47,6,1,7,5,6,0), (47,6,2,1,11,3,0), (47,6,2,3,8,3,0), (47,6,2,5,5,3,0), (47,7,1,6,5,7,0), (47,10,2,1,10,5,0), (47,10,2,3,5,5,0), (48,1,3,1,6,1,-2), (48,1,3,4,4,3,-2), (48,2,2,1,12,2,-2), (48,2,2,5,4,6,-2), (48,2,4,3,3,2,-2), (48,3,1,0,48,3,0), (48,3,1,1,45,3,0), (48,3,1,2,42,3,0), (48,3,1,3,39,3,0), (48,3,1,4,36,3,0), (48,3,1,5,33,3,0), (48,3,1,6,30,3,0), (48,3,1,7,27,3,0), (48,3,1,8,24,3,0), (48,3,1,9,21,3,0), (48,3,1,10,18,3,0), (48,3,1,11,15,3,0), (48,3,1,12,12,3,0), (48,3,1,13,9,3,0), (48,3,1,14,6,3,0), (48,3,1,15,3,3,0), (48,4,1,0,48,4,0), (48,4,1,1,44,4,0), (48,4,1,2,40,4,0), (48,4,1,3,36,4,0), (48,4,1,4,32,4,0), (48,4,1,5,28,4,0), (48,4,1,6,24,4,0), (48,4,1,7,20,4,0), (48,4,1,8,16,4,0), (48,4,1,9,12,4,0), (48,4,1,10,8,4,0), (48,4,1,11,4,4,0), (48,5,1,0,48,5,0), (48,5,1,1,43,5,0), (48,5,1,2,38,5,0), (48,5,1,3,33,5,0), (48,5,1,4,28,5,0), (48,5,1,5,23,5,0), (48,5,1,6,18,5,0), (48,5,1,7,13,5,0), (48,5,1,8,8,5,0), (48,5,1,9,3,5,0), (48,8,3,2,4,4,-2), (48,10,2,3,3,8,-2), (48,10,3,1,5,4,-2), (48,14,4,1,3,4,-2), (49,3,1,0,49,3,0), (49,3,1,1,46,3,0), (49,3,1,2,43,3,0), (49,3,1,3,40,3,0), (49,3,1,4,37,3,0), (49,3,1,5,34,3,0), (49,3,1,6,31,3,0), (49,3,1,7,28,3,0), (49,3,1,8,25,3,0), (49,3,1,9,22,3,0), (49,3,1,10,19,3,0), (49,3,1,11,16,3,0), (49,3,1,12,13,3,0), (49,3,1,13,10,3,0), (49,3,1,14,7,3,0), (49,3,1,15,4,3,0), (49,4,1,0,49,4,0), (49,4,1,1,45,4,0), (49,4,1,2,41,4,0), (49,4,1,3,37,4,0), (49,4,1,4,33,4,0), (49,4,1,5,29,4,0), (49,4,1,6,25,4,0), (49,4,1,7,21,4,0), (49,4,1,8,17,4,0), (49,4,1,9,13,4,0), (49,4,1,10,9,4,0), (49,4,1,11,5,4,0), (49,4,2,4,5,6,-2), (49,4,4,0,4,1,-2), (49,5,1,0,49,5,0), (49,5,1,1,44,5,0), (49,5,1,2,39,5,0), (49,5,1,3,34,5,0), (49,5,1,4,29,5,0), (49,5,1,5,24,5,0), (49,5,1,6,19,5,0), (49,5,1,7,14,5,0), (49,5,1,8,9,5,0), (49,5,1,9,4,5,0), (49,6,2,0,13,3,0), (49,6,2,2,10,3,0), (49,6,2,4,3,7,-2), (49,6,2,4,7,3,0), (49,6,2,6,4,3,0), (49,8,2,0,13,4,0), (49,8,2,1,11,4,0), (49,8,2,2,9,4,0), (49,8,2,3,7,4,0), (49,8,2,4,5,4,0), (49,8,2,5,3,4,0), (49,8,4,0,4,2,-2), (49,10,2,0,13,5,0), (49,10,2,2,8,5,0), (49,10,2,4,3,5,0), (49,12,2,3,4,6,0), (49,12,3,1,5,4,0), (49,12,4,0,4,3,-2), (49,12,4,0,4,3,0), (49,15,3,2,3,5,0), (49,16,2,2,5,8,0), (49,16,4,0,4,4,-2), (49,16,4,0,4,4,0), (49,16,4,1,3,4,0), (49,20,4,0,4,5,-2), (49,20,4,0,4,5,0), (49,21,3,1,4,7,0), (49,24,4,0,4,6,-2), (49,24,4,0,4,6,0), (49,28,4,0,4,7,0), (50,3,1,0,50,3,0), (50,3,1,1,47,3,0), (50,3,1,2,44,3,0), (50,3,1,3,41,3,0), (50,3,1,4,38,3,0), (50,3,1,5,35,3,0), (50,3,1,6,32,3,0), (50,3,1,7,29,3,0), (50,3,1,8,26,3,0), (50,3,1,9,23,3,0), (50,3,1,10,20,3,0), (50,3,1,11,17,3,0), (50,3,1,12,14,3,0), (50,3,1,13,11,3,0), (50,3,1,14,8,3,0), (50,3,1,15,5,3,0), (50,4,1,0,50,4,0), (50,4,1,1,46,4,0), (50,4,1,2,42,4,0), (50,4,1,3,38,4,0), (50,4,1,4,34,4,0), (50,4,1,5,30,4,0), (50,4,1,6,26,4,0), (50,4,1,7,22,4,0), (50,4,1,8,18,4,0), (50,4,1,9,14,4,0), (50,4,1,10,10,4,0), (50,4,1,11,6,4,0), (50,5,1,0,50,5,0), (50,5,1,1,45,5,0), (50,5,1,2,40,5,0), (50,5,1,3,35,5,0), (50,5,1,4,30,5,0), (50,5,1,5,25,5,0), (50,5,1,6,20,5,0), (50,5,1,7,15,5,0), (50,5,1,8,10,5,0), (50,5,1,9,5,5,0), (50,8,2,3,5,7,-2), (50,9,1,5,5,9,0), (51,3,1,0,51,3,0), (51,3,1,1,48,3,0), (51,3,1,2,45,3,0), (51,3,1,3,42,3,0), (51,3,1,4,39,3,0), (51,3,1,5,36,3,0), (51,3,1,6,33,3,0), (51,3,1,7,30,3,0), (51,3,1,8,27,3,0), (51,3,1,9,24,3,0), (51,3,1,10,21,3,0), (51,3,1,11,18,3,0), (51,3,1,12,15,3,0), (51,3,1,13,12,3,0), (51,3,1,14,9,3,0), (51,3,1,15,6,3,0), (51,3,1,16,3,3,0), (51,4,1,0,51,4,0), (51,4,1,1,47,4,0), (51,4,1,2,43,4,0), (51,4,1,3,39,4,0), (51,4,1,4,35,4,0), (51,4,1,5,31,4,0), (51,4,1,6,27,4,0), (51,4,1,7,23,4,0), (51,4,1,8,19,4,0), (51,4,1,9,15,4,0), (51,4,1,10,11,4,0), (51,4,1,11,7,4,0), (51,4,1,12,3,4,0), (51,4,3,1,6,2,-2), (51,4,3,4,3,4,-2), (51,5,1,0,51,5,0), (51,5,1,1,46,5,0), (51,5,1,2,41,5,0), (51,5,1,3,36,5,0), (51,5,1,4,31,5,0), (51,5,1,5,26,5,0), (51,5,1,6,21,5,0), (51,5,1,7,16,5,0), (51,5,1,8,11,5,0), (51,5,1,9,6,5,0), (51,5,3,2,5,3,-2), (51,5,5,0,3,1,-2), (51,6,1,8,3,6,0), (51,6,2,1,12,3,0), (51,6,2,3,9,3,0), (51,6,2,5,6,3,0), (51,6,2,7,3,3,0), (51,8,1,6,3,8,0), (51,10,2,1,11,5,0), (51,10,2,3,6,5,0), (51,10,5,0,3,2,-2), (51,13,3,1,5,5,-2), (51,14,2,3,3,7,0), (51,14,3,2,3,6,-2), (51,15,5,0,3,3,-2), (51,15,5,0,3,3,0), (51,20,5,0,3,4,-2), (51,20,5,0,3,4,0), (51,25,5,0,3,5,-2), (51,25,5,0,3,5,0), (51,30,5,0,3,6,-2), (51,30,5,0,3,6,0), (51,35,5,0,3,7,-2), (51,35,5,0,3,7,0), (51,40,5,0,3,8,-2), (51,40,5,0,3,8,0), (52,2,2,5,5,6,-2), (52,2,4,1,4,1,-2), (52,3,1,0,52,3,0), (52,3,1,1,49,3,0), (52,3,1,2,46,3,0), (52,3,1,3,43,3,0), (52,3,1,4,40,3,0), (52,3,1,5,37,3,0), (52,3,1,6,34,3,0), (52,3,1,7,31,3,0), (52,3,1,8,28,3,0), (52,3,1,9,25,3,0), (52,3,1,10,22,3,0), (52,3,1,11,19,3,0), (52,3,1,12,16,3,0), (52,3,1,13,13,3,0), (52,3,1,14,10,3,0), (52,3,1,15,7,3,0), (52,3,1,16,4,3,0), (52,4,1,0,52,4,0), (52,4,1,1,48,4,0), (52,4,1,2,44,4,0), (52,4,1,3,40,4,0), (52,4,1,4,36,4,0), (52,4,1,5,32,4,0), (52,4,1,6,28,4,0), (52,4,1,7,24,4,0), (52,4,1,8,20,4,0), (52,4,1,9,16,4,0), (52,4,1,10,12,4,0), (52,4,1,11,8,4,0), (52,4,1,12,4,4,0), (52,5,1,0,52,5,0), (52,5,1,1,47,5,0), (52,5,1,2,42,5,0), (52,5,1,3,37,5,0), (52,5,1,4,32,5,0), (52,5,1,5,27,5,0), (52,5,1,6,22,5,0), (52,5,1,7,17,5,0), (52,5,1,8,12,5,0), (52,5,1,9,7,5,0), (52,6,1,8,4,6,0), (52,7,1,7,3,7,0), (52,12,3,2,4,4,0), (52,15,3,1,5,5,0), (52,18,4,1,3,5,-2), (53,3,1,0,53,3,0), (53,3,1,1,50,3,0), (53,3,1,2,47,3,0), (53,3,1,3,44,3,0), (53,3,1,4,41,3,0), (53,3,1,5,38,3,0), (53,3,1,6,35,3,0), (53,3,1,7,32,3,0), (53,3,1,8,29,3,0), (53,3,1,9,26,3,0), (53,3,1,10,23,3,0), (53,3,1,11,20,3,0), (53,3,1,12,17,3,0), (53,3,1,13,14,3,0), (53,3,1,14,11,3,0), (53,3,1,15,8,3,0), (53,3,1,16,5,3,0), (53,4,1,0,53,4,0), (53,4,1,1,49,4,0), (53,4,1,2,45,4,0), (53,4,1,3,41,4,0), (53,4,1,4,37,4,0), (53,4,1,5,33,4,0), (53,4,1,6,29,4,0), (53,4,1,7,25,4,0), (53,4,1,8,21,4,0), (53,4,1,9,17,4,0), (53,4,1,10,13,4,0), (53,4,1,11,9,4,0), (53,4,1,12,5,4,0), (53,5,1,0,53,5,0), (53,5,1,1,48,5,0), (53,5,1,2,43,5,0), (53,5,1,3,38,5,0), (53,5,1,4,33,5,0), (53,5,1,5,28,5,0), (53,5,1,6,23,5,0), (53,5,1,7,18,5,0), (53,5,1,8,13,5,0), (53,5,1,9,8,5,0), (53,5,1,10,3,5,0), (53,6,1,8,5,6,0), (53,6,2,0,14,3,0), (53,6,2,2,11,3,0), (53,6,2,4,8,3,0), (53,6,2,6,5,3,0), (53,7,1,7,4,7,0), (53,8,1,6,5,8,0), (53,8,2,0,14,4,0), (53,8,2,1,12,4,0), (53,8,2,2,10,4,0), (53,8,2,3,8,4,0), (53,8,2,4,6,4,0), (53,8,2,5,4,4,0), (53,8,4,2,3,3,-2), (53,10,2,0,14,5,0), (53,10,2,2,9,5,0), (53,10,2,4,4,5,0), (53,12,2,3,5,6,0), (53,18,2,2,5,9,0), (53,20,4,1,3,5,0), (54,2,3,2,6,2,-2), (54,2,3,5,3,4,-2), (54,3,1,0,54,3,0), (54,3,1,1,51,3,0), (54,3,1,2,48,3,0), (54,3,1,3,45,3,0), (54,3,1,4,42,3,0), (54,3,1,5,39,3,0), (54,3,1,6,36,3,0), (54,3,1,7,33,3,0), (54,3,1,8,30,3,0), (54,3,1,9,27,3,0), (54,3,1,10,24,3,0), (54,3,1,11,21,3,0), (54,3,1,12,18,3,0), (54,3,1,13,15,3,0), (54,3,1,14,12,3,0), (54,3,1,15,9,3,0), (54,3,1,16,6,3,0), (54,3,1,17,3,3,0), (54,4,1,0,54,4,0), (54,4,1,1,50,4,0), (54,4,1,2,46,4,0), (54,4,1,3,42,4,0), (54,4,1,4,38,4,0), (54,4,1,5,34,4,0), (54,4,1,6,30,4,0), (54,4,1,7,26,4,0), (54,4,1,8,22,4,0), (54,4,1,9,18,4,0), (54,4,1,10,14,4,0), (54,4,1,11,10,4,0), (54,4,1,12,6,4,0), (54,4,2,5,3,7,-2), (54,5,1,0,54,5,0), (54,5,1,1,49,5,0), (54,5,1,2,44,5,0), (54,5,1,3,39,5,0), (54,5,1,4,34,5,0), (54,5,1,5,29,5,0), (54,5,1,6,24,5,0), (54,5,1,7,19,5,0), (54,5,1,8,14,5,0), (54,5,1,9,9,5,0), (54,5,1,10,4,5,0), (54,7,1,7,5,7,0), (54,11,3,2,4,5,-2), (54,16,3,1,5,6,-2), (55,3,1,0,55,3,0), (55,3,1,1,52,3,0), (55,3,1,2,49,3,0), (55,3,1,3,46,3,0), (55,3,1,4,43,3,0), (55,3,1,5,40,3,0), (55,3,1,6,37,3,0), (55,3,1,7,34,3,0), (55,3,1,8,31,3,0), (55,3,1,9,28,3,0), (55,3,1,10,25,3,0), (55,3,1,11,22,3,0), (55,3,1,12,19,3,0), (55,3,1,13,16,3,0), (55,3,1,14,13,3,0), (55,3,1,15,10,3,0), (55,3,1,16,7,3,0), (55,3,1,17,4,3,0), (55,3,3,0,7,1,-2), (55,3,3,3,5,3,-2), (55,3,5,1,3,1,-2), (55,4,1,0,55,4,0), (55,4,1,1,51,4,0), (55,4,1,2,47,4,0), (55,4,1,3,43,4,0), (55,4,1,4,39,4,0), (55,4,1,5,35,4,0), (55,4,1,6,31,4,0), (55,4,1,7,27,4,0), (55,4,1,8,23,4,0), (55,4,1,9,19,4,0), (55,4,1,10,15,4,0), (55,4,1,11,11,4,0), (55,4,1,12,7,4,0), (55,4,1,13,3,4,0), (55,5,1,0,55,5,0), (55,5,1,1,50,5,0), (55,5,1,2,45,5,0), (55,5,1,3,40,5,0), (55,5,1,4,35,5,0), (55,5,1,5,30,5,0), (55,5,1,6,25,5,0), (55,5,1,7,20,5,0), (55,5,1,8,15,5,0), (55,5,1,9,10,5,0), (55,5,1,10,5,5,0), (55,6,2,1,13,3,0), (55,6,2,3,10,3,0), (55,6,2,5,7,3,0), (55,6,2,7,4,3,0), (55,6,3,0,7,2,-2), (55,6,3,3,4,4,-2), (55,9,3,0,7,3,0), (55,9,3,1,6,3,0), (55,9,3,2,5,3,0), (55,9,3,3,3,5,-2), (55,9,3,3,4,3,0), (55,9,3,4,3,3,0), (55,10,2,1,12,5,0), (55,10,2,3,7,5,0), (55,12,3,0,7,4,0), (55,12,3,3,3,4,0), (55,14,2,3,4,7,0), (55,15,3,0,7,5,0), (55,18,3,1,5,6,0), (55,18,3,2,3,6,0), (56,3,1,0,56,3,0), (56,3,1,1,53,3,0), (56,3,1,2,50,3,0), (56,3,1,3,47,3,0), (56,3,1,4,44,3,0), (56,3,1,5,41,3,0), (56,3,1,6,38,3,0), (56,3,1,7,35,3,0), (56,3,1,8,32,3,0), (56,3,1,9,29,3,0), (56,3,1,10,26,3,0), (56,3,1,11,23,3,0), (56,3,1,12,20,3,0), (56,3,1,13,17,3,0), (56,3,1,14,14,3,0), (56,3,1,15,11,3,0), (56,3,1,16,8,3,0), (56,3,1,17,5,3,0), (56,4,1,0,56,4,0), (56,4,1,1,52,4,0), (56,4,1,2,48,4,0), (56,4,1,3,44,4,0), (56,4,1,4,40,4,0), (56,4,1,5,36,4,0), (56,4,1,6,32,4,0), (56,4,1,7,28,4,0), (56,4,1,8,24,4,0), (56,4,1,9,20,4,0), (56,4,1,10,16,4,0), (56,4,1,11,12,4,0), (56,4,1,12,8,4,0), (56,4,1,13,4,4,0), (56,5,1,0,56,5,0), (56,5,1,1,51,5,0), (56,5,1,2,46,5,0), (56,5,1,3,41,5,0), (56,5,1,4,36,5,0), (56,5,1,5,31,5,0), (56,5,1,6,26,5,0), (56,5,1,7,21,5,0), (56,5,1,8,16,5,0), (56,5,1,9,11,5,0), (56,5,1,10,6,5,0), (56,6,4,1,4,2,-2), (56,10,2,3,5,8,-2), (56,22,4,1,3,6,-2), (57,1,3,1,7,1,-2), (57,1,3,4,5,3,-2), (57,1,5,2,3,1,-2), (57,2,2,6,3,7,-2), (57,3,1,0,57,3,0), (57,3,1,1,54,3,0), (57,3,1,2,51,3,0), (57,3,1,3,48,3,0), (57,3,1,4,45,3,0), (57,3,1,5,42,3,0), (57,3,1,6,39,3,0), (57,3,1,7,36,3,0), (57,3,1,8,33,3,0), (57,3,1,9,30,3,0), (57,3,1,10,27,3,0), (57,3,1,11,24,3,0), (57,3,1,12,21,3,0), (57,3,1,13,18,3,0), (57,3,1,14,15,3,0), (57,3,1,15,12,3,0), (57,3,1,16,9,3,0), (57,3,1,17,6,3,0), (57,3,1,18,3,3,0), (57,4,1,0,57,4,0), (57,4,1,1,53,4,0), (57,4,1,2,49,4,0), (57,4,1,3,45,4,0), (57,4,1,4,41,4,0), (57,4,1,5,37,4,0), (57,4,1,6,33,4,0), (57,4,1,7,29,4,0), (57,4,1,8,25,4,0), (57,4,1,9,21,4,0), (57,4,1,10,17,4,0), (57,4,1,11,13,4,0), (57,4,1,12,9,4,0), (57,4,1,13,5,4,0), (57,5,1,0,57,5,0), (57,5,1,1,52,5,0), (57,5,1,2,47,5,0), (57,5,1,3,42,5,0), (57,5,1,4,37,5,0), (57,5,1,5,32,5,0), (57,5,1,6,27,5,0), (57,5,1,7,22,5,0), (57,5,1,8,17,5,0), (57,5,1,9,12,5,0), (57,5,1,10,7,5,0), (57,6,1,9,3,6,0), (57,6,2,0,15,3,0), (57,6,2,2,12,3,0), (57,6,2,4,5,7,-2), (57,6,2,4,9,3,0), (57,6,2,6,6,3,0), (57,6,2,8,3,3,0), (57,8,2,0,15,4,0), (57,8,2,1,13,4,0), (57,8,2,2,11,4,0), (57,8,2,3,9,4,0), (57,8,2,4,3,8,-2), (57,8,2,4,7,4,0), (57,8,2,5,5,4,0), (57,8,2,6,3,4,0), (57,8,3,2,5,4,-2), (57,10,2,0,15,5,0), (57,10,2,2,10,5,0), (57,10,2,4,5,5,0), (57,12,2,4,3,6,0), (57,12,4,2,3,3,0), (57,16,2,3,3,8,0), (57,17,3,2,3,7,-2), (57,19,3,1,5,7,-2), (57,24,4,1,3,6,0), (58,3,1,0,58,3,0), (58,3,1,1,55,3,0), (58,3,1,2,52,3,0), (58,3,1,3,49,3,0), (58,3,1,4,46,3,0), (58,3,1,5,43,3,0), (58,3,1,6,40,3,0), (58,3,1,7,37,3,0), (58,3,1,8,34,3,0), (58,3,1,9,31,3,0), (58,3,1,10,28,3,0), (58,3,1,11,25,3,0), (58,3,1,12,22,3,0), (58,3,1,13,19,3,0), (58,3,1,14,16,3,0), (58,3,1,15,13,3,0), (58,3,1,16,10,3,0), (58,3,1,17,7,3,0), (58,3,1,18,4,3,0), (58,4,1,0,58,4,0), (58,4,1,1,54,4,0), (58,4,1,2,50,4,0), (58,4,1,3,46,4,0), (58,4,1,4,42,4,0), (58,4,1,5,38,4,0), (58,4,1,6,34,4,0), (58,4,1,7,30,4,0), (58,4,1,8,26,4,0), (58,4,1,9,22,4,0), (58,4,1,10,18,4,0), (58,4,1,11,14,4,0), (58,4,1,12,10,4,0), (58,4,1,13,6,4,0), (58,5,1,0,58,5,0), (58,5,1,1,53,5,0), (58,5,1,2,48,5,0), (58,5,1,3,43,5,0), (58,5,1,4,38,5,0), (58,5,1,5,33,5,0), (58,5,1,6,28,5,0), (58,5,1,7,23,5,0), (58,5,1,8,18,5,0), (58,5,1,9,13,5,0), (58,5,1,10,8,5,0), (58,5,1,11,3,5,0), (58,6,1,9,4,6,0), (58,12,3,1,6,4,0), (58,15,3,2,4,5,0), (58,21,3,1,5,7,0), (59,3,1,0,59,3,0), (59,3,1,1,56,3,0), (59,3,1,2,53,3,0), (59,3,1,3,50,3,0), (59,3,1,4,47,3,0), (59,3,1,5,44,3,0), (59,3,1,6,41,3,0), (59,3,1,7,38,3,0), (59,3,1,8,35,3,0), (59,3,1,9,32,3,0), (59,3,1,10,29,3,0), (59,3,1,11,26,3,0), (59,3,1,12,23,3,0), (59,3,1,13,20,3,0), (59,3,1,14,17,3,0), (59,3,1,15,14,3,0), (59,3,1,16,11,3,0), (59,3,1,17,8,3,0), (59,3,1,18,5,3,0), (59,4,1,0,59,4,0), (59,4,1,1,55,4,0), (59,4,1,2,51,4,0), (59,4,1,3,47,4,0), (59,4,1,4,43,4,0), (59,4,1,5,39,4,0), (59,4,1,6,35,4,0), (59,4,1,7,31,4,0), (59,4,1,8,27,4,0), (59,4,1,9,23,4,0), (59,4,1,10,19,4,0), (59,4,1,11,15,4,0), (59,4,1,12,11,4,0), (59,4,1,13,7,4,0), (59,4,1,14,3,4,0), (59,5,1,0,59,5,0), (59,5,1,1,54,5,0), (59,5,1,2,49,5,0), (59,5,1,3,44,5,0), (59,5,1,4,39,5,0), (59,5,1,5,34,5,0), (59,5,1,6,29,5,0), (59,5,1,7,24,5,0), (59,5,1,8,19,5,0), (59,5,1,9,14,5,0), (59,5,1,10,9,5,0), (59,5,1,11,4,5,0), (59,6,1,9,5,6,0), (59,6,2,1,14,3,0), (59,6,2,3,11,3,0), (59,6,2,5,8,3,0), (59,6,2,7,5,3,0), (59,7,1,8,3,7,0), (59,8,1,7,3,8,0), (59,9,1,6,5,9,0), (59,10,2,1,13,5,0), (59,10,2,3,8,5,0), (59,10,2,5,3,5,0), (59,14,2,3,5,7,0), (60,3,1,0,60,3,0), (60,3,1,1,57,3,0), (60,3,1,2,54,3,0), (60,3,1,3,51,3,0), (60,3,1,4,48,3,0), (60,3,1,5,45,3,0), (60,3,1,6,42,3,0), (60,3,1,7,39,3,0), (60,3,1,8,36,3,0), (60,3,1,9,33,3,0), (60,3,1,10,30,3,0), (60,3,1,11,27,3,0), (60,3,1,12,24,3,0), (60,3,1,13,21,3,0), (60,3,1,14,18,3,0), (60,3,1,15,15,3,0), (60,3,1,16,12,3,0), (60,3,1,17,9,3,0), (60,3,1,18,6,3,0), (60,3,1,19,3,3,0), (60,4,1,0,60,4,0), (60,4,1,1,56,4,0), (60,4,1,2,52,4,0), (60,4,1,3,48,4,0), (60,4,1,4,44,4,0), (60,4,1,5,40,4,0), (60,4,1,6,36,4,0), (60,4,1,7,32,4,0), (60,4,1,8,28,4,0), (60,4,1,9,24,4,0), (60,4,1,10,20,4,0), (60,4,1,11,16,4,0), (60,4,1,12,12,4,0), (60,4,1,13,8,4,0), (60,4,1,14,4,4,0), (60,4,3,1,7,2,-2), (60,4,3,4,4,4,-2), (60,5,1,0,60,5,0), (60,5,1,1,55,5,0), (60,5,1,2,50,5,0), (60,5,1,3,45,5,0), (60,5,1,4,40,5,0), (60,5,1,5,35,5,0), (60,5,1,6,30,5,0), (60,5,1,7,25,5,0), (60,5,1,8,20,5,0), (60,5,1,9,15,5,0), (60,5,1,10,10,5,0), (60,5,1,11,5,5,0), (60,6,4,3,3,3,-2), (60,7,1,8,4,7,0), (60,8,5,1,3,2,-2), (60,10,4,1,4,3,-2), (60,14,3,2,4,6,-2), (60,22,3,1,5,8,-2), (60,26,4,1,3,7,-2), (61,3,1,0,61,3,0), (61,3,1,1,58,3,0), (61,3,1,2,55,3,0), (61,3,1,3,52,3,0), (61,3,1,4,49,3,0), (61,3,1,5,46,3,0), (61,3,1,6,43,3,0), (61,3,1,7,40,3,0), (61,3,1,8,37,3,0), (61,3,1,9,34,3,0), (61,3,1,10,31,3,0), (61,3,1,11,28,3,0), (61,3,1,12,25,3,0), (61,3,1,13,22,3,0), (61,3,1,14,19,3,0), (61,3,1,15,16,3,0), (61,3,1,16,13,3,0), (61,3,1,17,10,3,0), (61,3,1,18,7,3,0), (61,3,1,19,4,3,0), (61,4,1,0,61,4,0), (61,4,1,1,57,4,0), (61,4,1,2,53,4,0), (61,4,1,3,49,4,0), (61,4,1,4,45,4,0), (61,4,1,5,41,4,0), (61,4,1,6,37,4,0), (61,4,1,7,33,4,0), (61,4,1,8,29,4,0), (61,4,1,9,25,4,0), (61,4,1,10,21,4,0), (61,4,1,11,17,4,0), (61,4,1,12,13,4,0), (61,4,1,13,9,4,0), (61,4,1,14,5,4,0), (61,4,4,2,4,2,-2), (61,5,1,0,61,5,0), (61,5,1,1,56,5,0), (61,5,1,2,51,5,0), (61,5,1,3,46,5,0), (61,5,1,4,41,5,0), (61,5,1,5,36,5,0), (61,5,1,6,31,5,0), (61,5,1,7,26,5,0), (61,5,1,8,21,5,0), (61,5,1,9,16,5,0), (61,5,1,10,11,5,0), (61,5,1,11,6,5,0), (61,6,2,0,16,3,0), (61,6,2,2,13,3,0), (61,6,2,4,10,3,0), (61,6,2,6,7,3,0), (61,6,2,8,4,3,0), (61,7,1,8,5,7,0), (61,8,1,7,5,8,0), (61,8,2,0,16,4,0), (61,8,2,1,14,4,0), (61,8,2,2,12,4,0), (61,8,2,3,10,4,0), (61,8,2,4,8,4,0), (61,8,2,5,6,4,0), (61,8,2,6,4,4,0), (61,10,2,0,16,5,0), (61,10,2,2,11,5,0), (61,10,2,4,6,5,0), (61,12,2,4,4,6,0), (61,12,3,2,5,4,0), (61,12,4,1,4,3,0), (61,12,4,2,3,4,-2), (61,15,3,1,6,5,0), (61,21,3,2,3,7,0), (61,24,3,1,5,8,0), (61,28,4,1,3,7,0), (62,3,1,0,62,3,0), (62,3,1,1,59,3,0), (62,3,1,2,56,3,0), (62,3,1,3,53,3,0), (62,3,1,4,50,3,0), (62,3,1,5,47,3,0), (62,3,1,6,44,3,0), (62,3,1,7,41,3,0), (62,3,1,8,38,3,0), (62,3,1,9,35,3,0), (62,3,1,10,32,3,0), (62,3,1,11,29,3,0), (62,3,1,12,26,3,0), (62,3,1,13,23,3,0), (62,3,1,14,20,3,0), (62,3,1,15,17,3,0), (62,3,1,16,14,3,0), (62,3,1,17,11,3,0), (62,3,1,18,8,3,0), (62,3,1,19,5,3,0), (62,4,1,0,62,4,0), (62,4,1,1,58,4,0), (62,4,1,2,54,4,0), (62,4,1,3,50,4,0), (62,4,1,4,46,4,0), (62,4,1,5,42,4,0), (62,4,1,6,38,4,0), (62,4,1,7,34,4,0), (62,4,1,8,30,4,0), (62,4,1,9,26,4,0), (62,4,1,10,22,4,0), (62,4,1,11,18,4,0), (62,4,1,12,14,4,0), (62,4,1,13,10,4,0), (62,4,1,14,6,4,0), (62,4,2,5,5,7,-2), (62,5,1,0,62,5,0), (62,5,1,1,57,5,0), (62,5,1,2,52,5,0), (62,5,1,3,47,5,0), (62,5,1,4,42,5,0), (62,5,1,5,37,5,0), (62,5,1,6,32,5,0), (62,5,1,7,27,5,0), (62,5,1,8,22,5,0), (62,5,1,9,17,5,0), (62,5,1,10,12,5,0), (62,5,1,11,7,5,0), (63,2,3,2,7,2,-2), (63,2,3,5,4,4,-2), (63,3,1,0,63,3,0), (63,3,1,1,60,3,0), (63,3,1,2,57,3,0), (63,3,1,3,54,3,0), (63,3,1,4,51,3,0), (63,3,1,5,48,3,0), (63,3,1,6,45,3,0), (63,3,1,7,42,3,0), (63,3,1,8,39,3,0), (63,3,1,9,36,3,0), (63,3,1,10,33,3,0), (63,3,1,11,30,3,0), (63,3,1,12,27,3,0), (63,3,1,13,24,3,0), (63,3,1,14,21,3,0), (63,3,1,15,18,3,0), (63,3,1,16,15,3,0), (63,3,1,17,12,3,0), (63,3,1,18,9,3,0), (63,3,1,19,6,3,0), (63,3,1,20,3,3,0), (63,4,1,0,63,4,0), (63,4,1,1,59,4,0), (63,4,1,2,55,4,0), (63,4,1,3,51,4,0), (63,4,1,4,47,4,0), (63,4,1,5,43,4,0), (63,4,1,6,39,4,0), (63,4,1,7,35,4,0), (63,4,1,8,31,4,0), (63,4,1,9,27,4,0), (63,4,1,10,23,4,0), (63,4,1,11,19,4,0), (63,4,1,12,15,4,0), (63,4,1,13,11,4,0), (63,4,1,14,7,4,0), (63,4,1,15,3,4,0), (63,5,1,0,63,5,0), (63,5,1,1,58,5,0), (63,5,1,2,53,5,0), (63,5,1,3,48,5,0), (63,5,1,4,43,5,0), (63,5,1,5,38,5,0), (63,5,1,6,33,5,0), (63,5,1,7,28,5,0), (63,5,1,8,23,5,0), (63,5,1,9,18,5,0), (63,5,1,10,13,5,0), (63,5,1,11,8,5,0), (63,5,1,12,3,5,0), (63,6,1,10,3,6,0), (63,6,2,1,15,3,0), (63,6,2,3,12,3,0), (63,6,2,5,9,3,0), (63,6,2,7,6,3,0), (63,6,2,9,3,3,0), (63,7,3,4,3,5,-2), (63,10,2,1,14,5,0), (63,10,2,3,9,5,0), (63,10,2,5,4,5,0), (63,11,3,2,5,5,-2), (63,20,3,2,3,8,-2), (64,2,4,3,4,2,-2), (64,3,1,0,64,3,0), (64,3,1,1,61,3,0), (64,3,1,2,58,3,0), (64,3,1,3,55,3,0), (64,3,1,4,52,3,0), (64,3,1,5,49,3,0), (64,3,1,6,46,3,0), (64,3,1,7,43,3,0), (64,3,1,8,40,3,0), (64,3,1,9,37,3,0), (64,3,1,10,34,3,0), (64,3,1,11,31,3,0), (64,3,1,12,28,3,0), (64,3,1,13,25,3,0), (64,3,1,14,22,3,0), (64,3,1,15,19,3,0), (64,3,1,16,16,3,0), (64,3,1,17,13,3,0), (64,3,1,18,10,3,0), (64,3,1,19,7,3,0), (64,3,1,20,4,3,0), (64,3,3,0,8,1,-2), (64,4,1,0,64,4,0), (64,4,1,1,60,4,0), (64,4,1,2,56,4,0), (64,4,1,3,52,4,0), (64,4,1,4,48,4,0), (64,4,1,5,44,4,0), (64,4,1,6,40,4,0), (64,4,1,7,36,4,0), (64,4,1,8,32,4,0), (64,4,1,9,28,4,0), (64,4,1,10,24,4,0), (64,4,1,11,20,4,0), (64,4,1,12,16,4,0), (64,4,1,13,12,4,0), (64,4,1,14,8,4,0), (64,4,1,15,4,4,0), (64,5,1,0,64,5,0), (64,5,1,1,59,5,0), (64,5,1,2,54,5,0), (64,5,1,3,49,5,0), (64,5,1,4,44,5,0), (64,5,1,5,39,5,0), (64,5,1,6,34,5,0), (64,5,1,7,29,5,0), (64,5,1,8,24,5,0), (64,5,1,9,19,5,0), (64,5,1,10,14,5,0), (64,5,1,11,9,5,0), (64,5,1,12,4,5,0), (64,6,1,10,4,6,0), (64,6,2,5,3,8,-2), (64,6,3,0,8,2,-2), (64,6,3,3,5,4,-2), (64,9,3,0,8,3,0), (64,9,3,1,7,3,0), (64,9,3,2,6,3,0), (64,9,3,3,4,5,-2), (64,9,3,3,5,3,0), (64,9,3,4,4,3,0), (64,9,3,5,3,3,0), (64,12,3,0,8,4,0), (64,12,3,3,3,6,-2), (64,12,3,3,4,4,0), (64,14,4,1,4,4,-2), (64,15,3,0,8,5,0), (64,15,3,3,3,5,0), (64,18,3,2,4,6,0), (64,27,3,1,5,9,0), (64,30,4,1,3,8,-2), (65,2,2,6,5,7,-2), (65,3,1,0,65,3,0), (65,3,1,1,62,3,0), (65,3,1,2,59,3,0), (65,3,1,3,56,3,0), (65,3,1,4,53,3,0), (65,3,1,5,50,3,0), (65,3,1,6,47,3,0), (65,3,1,7,44,3,0), (65,3,1,8,41,3,0), (65,3,1,9,38,3,0), (65,3,1,10,35,3,0), (65,3,1,11,32,3,0), (65,3,1,12,29,3,0), (65,3,1,13,26,3,0), (65,3,1,14,23,3,0), (65,3,1,15,20,3,0), (65,3,1,16,17,3,0), (65,3,1,17,14,3,0), (65,3,1,18,11,3,0), (65,3,1,19,8,3,0), (65,3,1,20,5,3,0), (65,4,1,0,65,4,0), (65,4,1,1,61,4,0), (65,4,1,2,57,4,0), (65,4,1,3,53,4,0), (65,4,1,4,49,4,0), (65,4,1,5,45,4,0), (65,4,1,6,41,4,0), (65,4,1,7,37,4,0), (65,4,1,8,33,4,0), (65,4,1,9,29,4,0), (65,4,1,10,25,4,0), (65,4,1,11,21,4,0), (65,4,1,12,17,4,0), (65,4,1,13,13,4,0), (65,4,1,14,9,4,0), (65,4,1,15,5,4,0), (65,4,4,0,5,1,-2), (65,4,4,4,3,3,-2), (65,5,1,0,65,5,0), (65,5,1,1,60,5,0), (65,5,1,2,55,5,0), (65,5,1,3,50,5,0), (65,5,1,4,45,5,0), (65,5,1,5,40,5,0), (65,5,1,6,35,5,0), (65,5,1,7,30,5,0), (65,5,1,8,25,5,0), (65,5,1,9,20,5,0), (65,5,1,10,15,5,0), (65,5,1,11,10,5,0), (65,5,1,12,5,5,0), (65,6,1,10,5,6,0), (65,6,2,0,17,3,0), (65,6,2,2,14,3,0), (65,6,2,4,11,3,0), (65,6,2,6,8,3,0), (65,6,2,8,5,3,0), (65,8,2,0,17,4,0), (65,8,2,1,15,4,0), (65,8,2,2,13,4,0), (65,8,2,3,11,4,0), (65,8,2,4,5,8,-2), (65,8,2,4,9,4,0), (65,8,2,5,7,4,0), (65,8,2,6,5,4,0), (65,8,2,7,3,4,0), (65,8,4,0,5,2,-2), (65,10,2,0,17,5,0), (65,10,2,2,12,5,0), (65,10,2,4,7,5,0), (65,12,2,4,5,6,0), (65,12,4,0,5,3,-2), (65,12,4,0,5,3,0), (65,13,5,1,3,3,-2), (65,14,2,4,3,7,0), (65,16,2,3,5,8,0), (65,16,4,0,5,4,-2), (65,16,4,0,5,4,0), (65,16,4,1,4,4,0), (65,16,4,2,3,4,0), (65,20,4,0,5,5,-2), (65,20,4,0,5,5,0), (65,24,4,0,5,6,-2), (65,24,4,0,5,6,0), (65,28,4,0,5,7,-2), (65,28,4,0,5,7,0), (65,32,4,0,5,8,-2), (65,32,4,0,5,8,0), (65,32,4,1,3,8,0), (65,36,4,0,5,9,0), (66,1,3,1,8,1,-2), (66,3,1,0,66,3,0), (66,3,1,1,63,3,0), (66,3,1,2,60,3,0), (66,3,1,3,57,3,0), (66,3,1,4,54,3,0), (66,3,1,5,51,3,0), (66,3,1,6,48,3,0), (66,3,1,7,45,3,0), (66,3,1,8,42,3,0), (66,3,1,9,39,3,0), (66,3,1,10,36,3,0), (66,3,1,11,33,3,0), (66,3,1,12,30,3,0), (66,3,1,13,27,3,0), (66,3,1,14,24,3,0), (66,3,1,15,21,3,0), (66,3,1,16,18,3,0), (66,3,1,17,15,3,0), (66,3,1,18,12,3,0), (66,3,1,19,9,3,0), (66,3,1,20,6,3,0), (66,3,1,21,3,3,0), (66,4,1,0,66,4,0), (66,4,1,1,62,4,0), (66,4,1,2,58,4,0), (66,4,1,3,54,4,0), (66,4,1,4,50,4,0), (66,4,1,5,46,4,0), (66,4,1,6,42,4,0), (66,4,1,7,38,4,0), (66,4,1,8,34,4,0), (66,4,1,9,30,4,0), (66,4,1,10,26,4,0), (66,4,1,11,22,4,0), (66,4,1,12,18,4,0), (66,4,1,13,14,4,0), (66,4,1,14,10,4,0), (66,4,1,15,6,4,0), (66,5,1,0,66,5,0), (66,5,1,1,61,5,0), (66,5,1,2,56,5,0), (66,5,1,3,51,5,0), (66,5,1,4,46,5,0), (66,5,1,5,41,5,0), (66,5,1,6,36,5,0), (66,5,1,7,31,5,0), (66,5,1,8,26,5,0), (66,5,1,9,21,5,0), (66,5,1,10,16,5,0), (66,5,1,11,11,5,0), (66,5,1,12,6,5,0), (66,7,1,9,3,7,0), (66,15,5,1,3,3,0), (67,3,1,0,67,3,0), (67,3,1,1,64,3,0), (67,3,1,2,61,3,0), (67,3,1,3,58,3,0), (67,3,1,4,55,3,0), (67,3,1,5,52,3,0), (67,3,1,6,49,3,0), (67,3,1,7,46,3,0), (67,3,1,8,43,3,0), (67,3,1,9,40,3,0), (67,3,1,10,37,3,0), (67,3,1,11,34,3,0), (67,3,1,12,31,3,0), (67,3,1,13,28,3,0), (67,3,1,14,25,3,0), (67,3,1,15,22,3,0), (67,3,1,16,19,3,0), (67,3,1,17,16,3,0), (67,3,1,18,13,3,0), (67,3,1,19,10,3,0), (67,3,1,20,7,3,0), (67,3,1,21,4,3,0), (67,4,1,0,67,4,0), (67,4,1,1,63,4,0), (67,4,1,2,59,4,0), (67,4,1,3,55,4,0), (67,4,1,4,51,4,0), (67,4,1,5,47,4,0), (67,4,1,6,43,4,0), (67,4,1,7,39,4,0), (67,4,1,8,35,4,0), (67,4,1,9,31,4,0), (67,4,1,10,27,4,0), (67,4,1,11,23,4,0), (67,4,1,12,19,4,0), (67,4,1,13,15,4,0), (67,4,1,14,11,4,0), (67,4,1,15,7,4,0), (67,4,1,16,3,4,0), (67,5,1,0,67,5,0), (67,5,1,1,62,5,0), (67,5,1,2,57,5,0), (67,5,1,3,52,5,0), (67,5,1,4,47,5,0), (67,5,1,5,42,5,0), (67,5,1,6,37,5,0), (67,5,1,7,32,5,0), (67,5,1,8,27,5,0), (67,5,1,9,22,5,0), (67,5,1,10,17,5,0), (67,5,1,11,12,5,0), (67,5,1,12,7,5,0), (67,6,2,1,16,3,0), (67,6,2,3,13,3,0), (67,6,2,5,10,3,0), (67,6,2,7,7,3,0), (67,6,2,9,4,3,0), (67,6,5,2,3,2,-2), (67,7,1,9,4,7,0), (67,8,1,8,3,8,0), (67,10,2,1,15,5,0), (67,10,2,3,10,5,0), (67,10,2,5,5,5,0), (67,12,3,1,7,4,0), (67,12,3,4,3,4,0), (67,15,3,2,5,5,0), (67,24,3,2,3,8,0), (68,2,4,1,5,1,-2), (68,2,4,5,3,3,-2), (68,3,1,0,68,3,0), (68,3,1,1,65,3,0), (68,3,1,2,62,3,0), (68,3,1,3,59,3,0), (68,3,1,4,56,3,0), (68,3,1,5,53,3,0), (68,3,1,6,50,3,0), (68,3,1,7,47,3,0), (68,3,1,8,44,3,0), (68,3,1,9,41,3,0), (68,3,1,10,38,3,0), (68,3,1,11,35,3,0), (68,3,1,12,32,3,0), (68,3,1,13,29,3,0), (68,3,1,14,26,3,0), (68,3,1,15,23,3,0), (68,3,1,16,20,3,0), (68,3,1,17,17,3,0), (68,3,1,18,14,3,0), (68,3,1,19,11,3,0), (68,3,1,20,8,3,0), (68,3,1,21,5,3,0), (68,4,1,0,68,4,0), (68,4,1,1,64,4,0), (68,4,1,2,60,4,0), (68,4,1,3,56,4,0), (68,4,1,4,52,4,0), (68,4,1,5,48,4,0), (68,4,1,6,44,4,0), (68,4,1,7,40,4,0), (68,4,1,8,36,4,0), (68,4,1,9,32,4,0), (68,4,1,10,28,4,0), (68,4,1,11,24,4,0), (68,4,1,12,20,4,0), (68,4,1,13,16,4,0), (68,4,1,14,12,4,0), (68,4,1,15,8,4,0), (68,4,1,16,4,4,0), (68,5,1,0,68,5,0), (68,5,1,1,63,5,0), (68,5,1,2,58,5,0), (68,5,1,3,53,5,0), (68,5,1,4,48,5,0), (68,5,1,5,43,5,0), (68,5,1,6,38,5,0), (68,5,1,7,33,5,0), (68,5,1,8,28,5,0), (68,5,1,9,23,5,0), (68,5,1,10,18,5,0), (68,5,1,11,13,5,0), (68,5,1,12,8,5,0), (68,5,1,13,3,5,0), (68,7,1,9,5,7,0), (68,9,1,7,5,9,0), (68,18,4,1,4,5,-2), (69,3,1,0,69,3,0), (69,3,1,1,66,3,0), (69,3,1,2,63,3,0), (69,3,1,3,60,3,0), (69,3,1,4,57,3,0), (69,3,1,5,54,3,0), (69,3,1,6,51,3,0), (69,3,1,7,48,3,0), (69,3,1,8,45,3,0), (69,3,1,9,42,3,0), (69,3,1,10,39,3,0), (69,3,1,11,36,3,0), (69,3,1,12,33,3,0), (69,3,1,13,30,3,0), (69,3,1,14,27,3,0), (69,3,1,15,24,3,0), (69,3,1,16,21,3,0), (69,3,1,17,18,3,0), (69,3,1,18,15,3,0), (69,3,1,19,12,3,0), (69,3,1,20,9,3,0), (69,3,1,21,6,3,0), (69,3,1,22,3,3,0), (69,4,1,0,69,4,0), (69,4,1,1,65,4,0), (69,4,1,2,61,4,0), (69,4,1,3,57,4,0), (69,4,1,4,53,4,0), (69,4,1,5,49,4,0), (69,4,1,6,45,4,0), (69,4,1,7,41,4,0), (69,4,1,8,37,4,0), (69,4,1,9,33,4,0), (69,4,1,10,29,4,0), (69,4,1,11,25,4,0), (69,4,1,12,21,4,0), (69,4,1,13,17,4,0), (69,4,1,14,13,4,0), (69,4,1,15,9,4,0), (69,4,1,16,5,4,0), (69,4,2,6,3,8,-2), (69,4,3,1,8,2,-2), (69,4,3,4,5,4,-2), (69,5,1,0,69,5,0), (69,5,1,1,64,5,0), (69,5,1,2,59,5,0), (69,5,1,3,54,5,0), (69,5,1,4,49,5,0), (69,5,1,5,44,5,0), (69,5,1,6,39,5,0), (69,5,1,7,34,5,0), (69,5,1,8,29,5,0), (69,5,1,9,24,5,0), (69,5,1,10,19,5,0), (69,5,1,11,14,5,0), (69,5,1,12,9,5,0), (69,5,1,13,4,5,0), (69,5,3,5,3,5,-2), (69,6,1,11,3,6,0), (69,6,2,0,18,3,0), (69,6,2,2,15,3,0), (69,6,2,4,12,3,0), (69,6,2,6,9,3,0), (69,6,2,8,6,3,0), (69,6,2,10,3,3,0), (69,8,1,8,5,8,0), (69,8,2,0,18,4,0), (69,8,2,1,16,4,0), (69,8,2,2,14,4,0), (69,8,2,3,12,4,0), (69,8,2,4,10,4,0), (69,8,2,5,8,4,0), (69,8,2,6,6,4,0), (69,8,2,7,4,4,0), (69,8,4,2,4,3,-2), (69,10,2,0,18,5,0), (69,10,2,2,13,5,0), (69,10,2,4,8,5,0), (69,10,2,6,3,5,0), (69,12,2,5,3,6,0), (69,12,4,3,3,3,0), (69,14,2,4,4,7,0), (69,14,3,2,5,6,-2), (69,16,4,2,3,5,-2), (69,20,4,1,4,5,0), (70,3,1,0,70,3,0), (70,3,1,1,67,3,0), (70,3,1,2,64,3,0), (70,3,1,3,61,3,0), (70,3,1,4,58,3,0), (70,3,1,5,55,3,0), (70,3,1,6,52,3,0), (70,3,1,7,49,3,0), (70,3,1,8,46,3,0), (70,3,1,9,43,3,0), (70,3,1,10,40,3,0), (70,3,1,11,37,3,0), (70,3,1,12,34,3,0), (70,3,1,13,31,3,0), (70,3,1,14,28,3,0), (70,3,1,15,25,3,0), (70,3,1,16,22,3,0), (70,3,1,17,19,3,0), (70,3,1,18,16,3,0), (70,3,1,19,13,3,0), (70,3,1,20,10,3,0), (70,3,1,21,7,3,0), (70,3,1,22,4,3,0), (70,4,1,0,70,4,0), (70,4,1,1,66,4,0), (70,4,1,2,62,4,0), (70,4,1,3,58,4,0), (70,4,1,4,54,4,0), (70,4,1,5,50,4,0), (70,4,1,6,46,4,0), (70,4,1,7,42,4,0), (70,4,1,8,38,4,0), (70,4,1,9,34,4,0), (70,4,1,10,30,4,0), (70,4,1,11,26,4,0), (70,4,1,12,22,4,0), (70,4,1,13,18,4,0), (70,4,1,14,14,4,0), (70,4,1,15,10,4,0), (70,4,1,16,6,4,0), (70,5,1,0,70,5,0), (70,5,1,1,65,5,0), (70,5,1,2,60,5,0), (70,5,1,3,55,5,0), (70,5,1,4,50,5,0), (70,5,1,5,45,5,0), (70,5,1,6,40,5,0), (70,5,1,7,35,5,0), (70,5,1,8,30,5,0), (70,5,1,9,25,5,0), (70,5,1,10,20,5,0), (70,5,1,11,15,5,0), (70,5,1,12,10,5,0), (70,5,1,13,5,5,0), (70,6,1,11,4,6,0), (70,12,3,2,6,4,0), (70,15,3,1,7,5,0), (70,18,5,1,3,4,-2), (70,21,3,2,4,7,0), (71,3,1,0,71,3,0), (71,3,1,1,68,3,0), (71,3,1,2,65,3,0), (71,3,1,3,62,3,0), (71,3,1,4,59,3,0), (71,3,1,5,56,3,0), (71,3,1,6,53,3,0), (71,3,1,7,50,3,0), (71,3,1,8,47,3,0), (71,3,1,9,44,3,0), (71,3,1,10,41,3,0), (71,3,1,11,38,3,0), (71,3,1,12,35,3,0), (71,3,1,13,32,3,0), (71,3,1,14,29,3,0), (71,3,1,15,26,3,0), (71,3,1,16,23,3,0), (71,3,1,17,20,3,0), (71,3,1,18,17,3,0), (71,3,1,19,14,3,0), (71,3,1,20,11,3,0), (71,3,1,21,8,3,0), (71,3,1,22,5,3,0), (71,4,1,0,71,4,0), (71,4,1,1,67,4,0), (71,4,1,2,63,4,0), (71,4,1,3,59,4,0), (71,4,1,4,55,4,0), (71,4,1,5,51,4,0), (71,4,1,6,47,4,0), (71,4,1,7,43,4,0), (71,4,1,8,39,4,0), (71,4,1,9,35,4,0), (71,4,1,10,31,4,0), (71,4,1,11,27,4,0), (71,4,1,12,23,4,0), (71,4,1,13,19,4,0), (71,4,1,14,15,4,0), (71,4,1,15,11,4,0), (71,4,1,16,7,4,0), (71,4,1,17,3,4,0), (71,5,1,0,71,5,0), (71,5,1,1,66,5,0), (71,5,1,2,61,5,0), (71,5,1,3,56,5,0), (71,5,1,4,51,5,0), (71,5,1,5,46,5,0), (71,5,1,6,41,5,0), (71,5,1,7,36,5,0), (71,5,1,8,31,5,0), (71,5,1,9,26,5,0), (71,5,1,10,21,5,0), (71,5,1,11,16,5,0), (71,5,1,12,11,5,0), (71,5,1,13,6,5,0), (71,6,1,11,5,6,0), (71,6,2,1,17,3,0), (71,6,2,3,14,3,0), (71,6,2,5,11,3,0), (71,6,2,7,8,3,0), (71,6,2,9,5,3,0), (71,10,2,1,16,5,0), (71,10,2,3,11,5,0), (71,10,2,5,6,5,0), (71,18,2,3,5,9,0), (71,20,5,1,3,4,0), (72,2,2,7,3,8,-2), (72,2,3,2,8,2,-2), (72,2,3,5,5,4,-2), (72,3,1,0,72,3,0), (72,3,1,1,69,3,0), (72,3,1,2,66,3,0), (72,3,1,3,63,3,0), (72,3,1,4,60,3,0), (72,3,1,5,57,3,0), (72,3,1,6,54,3,0), (72,3,1,7,51,3,0), (72,3,1,8,48,3,0), (72,3,1,9,45,3,0), (72,3,1,10,42,3,0), (72,3,1,11,39,3,0), (72,3,1,12,36,3,0), (72,3,1,13,33,3,0), (72,3,1,14,30,3,0), (72,3,1,15,27,3,0), (72,3,1,16,24,3,0), (72,3,1,17,21,3,0), (72,3,1,18,18,3,0), (72,3,1,19,15,3,0), (72,3,1,20,12,3,0), (72,3,1,21,9,3,0), (72,3,1,22,6,3,0), (72,3,1,23,3,3,0), (72,4,1,0,72,4,0), (72,4,1,1,68,4,0), (72,4,1,2,64,4,0), (72,4,1,3,60,4,0), (72,4,1,4,56,4,0), (72,4,1,5,52,4,0), (72,4,1,6,48,4,0), (72,4,1,7,44,4,0), (72,4,1,8,40,4,0), (72,4,1,9,36,4,0), (72,4,1,10,32,4,0), (72,4,1,11,28,4,0), (72,4,1,12,24,4,0), (72,4,1,13,20,4,0), (72,4,1,14,16,4,0), (72,4,1,15,12,4,0), (72,4,1,16,8,4,0), (72,4,1,17,4,4,0), (72,4,5,3,3,2,-2), (72,5,1,0,72,5,0), (72,5,1,1,67,5,0), (72,5,1,2,62,5,0), (72,5,1,3,57,5,0), (72,5,1,4,52,5,0), (72,5,1,5,47,5,0), (72,5,1,6,42,5,0), (72,5,1,7,37,5,0), (72,5,1,8,32,5,0), (72,5,1,9,27,5,0), (72,5,1,10,22,5,0), (72,5,1,11,17,5,0), (72,5,1,12,12,5,0), (72,5,1,13,7,5,0), (72,6,2,5,5,8,-2), (72,6,4,1,5,2,-2), (72,7,3,4,4,5,-2), (72,10,4,3,3,4,-2), (72,22,4,1,4,6,-2), (73,3,1,0,73,3,0), (73,3,1,1,70,3,0), (73,3,1,2,67,3,0), (73,3,1,3,64,3,0), (73,3,1,4,61,3,0), (73,3,1,5,58,3,0), (73,3,1,6,55,3,0), (73,3,1,7,52,3,0), (73,3,1,8,49,3,0), (73,3,1,9,46,3,0), (73,3,1,10,43,3,0), (73,3,1,11,40,3,0), (73,3,1,12,37,3,0), (73,3,1,13,34,3,0), (73,3,1,14,31,3,0), (73,3,1,15,28,3,0), (73,3,1,16,25,3,0), (73,3,1,17,22,3,0), (73,3,1,18,19,3,0), (73,3,1,19,16,3,0), (73,3,1,20,13,3,0), (73,3,1,21,10,3,0), (73,3,1,22,7,3,0), (73,3,1,23,4,3,0), (73,3,3,0,9,1,-2), (73,3,3,6,3,5,-2), (73,4,1,0,73,4,0), (73,4,1,1,69,4,0), (73,4,1,2,65,4,0), (73,4,1,3,61,4,0), (73,4,1,4,57,4,0), (73,4,1,5,53,4,0), (73,4,1,6,49,4,0), (73,4,1,7,45,4,0), (73,4,1,8,41,4,0), (73,4,1,9,37,4,0), (73,4,1,10,33,4,0), (73,4,1,11,29,4,0), (73,4,1,12,25,4,0), (73,4,1,13,21,4,0), (73,4,1,14,17,4,0), (73,4,1,15,13,4,0), (73,4,1,16,9,4,0), (73,4,1,17,5,4,0), (73,5,1,0,73,5,0), (73,5,1,1,68,5,0), (73,5,1,2,63,5,0), (73,5,1,3,58,5,0), (73,5,1,4,53,5,0), (73,5,1,5,48,5,0), (73,5,1,6,43,5,0), (73,5,1,7,38,5,0), (73,5,1,8,33,5,0), (73,5,1,9,28,5,0), (73,5,1,10,23,5,0), (73,5,1,11,18,5,0), (73,5,1,12,13,5,0), (73,5,1,13,8,5,0), (73,5,1,14,3,5,0), (73,6,2,0,19,3,0), (73,6,2,2,16,3,0), (73,6,2,4,13,3,0), (73,6,2,6,10,3,0), (73,6,2,8,7,3,0), (73,6,2,10,4,3,0), (73,6,3,0,9,2,-2), (73,6,6,0,3,1,-2), (73,7,1,10,3,7,0), (73,8,2,0,19,4,0), (73,8,2,1,17,4,0), (73,8,2,2,15,4,0), (73,8,2,3,13,4,0), (73,8,2,4,11,4,0), (73,8,2,5,9,4,0), (73,8,2,6,7,4,0), (73,8,2,7,5,4,0), (73,8,2,8,3,4,0), (73,9,3,0,9,3,0), (73,9,3,1,8,3,0), (73,9,3,2,7,3,0), (73,9,3,3,5,5,-2), (73,9,3,3,6,3,0), (73,9,3,4,5,3,0), (73,9,3,5,4,3,0), (73,9,3,6,3,3,0), (73,10,2,0,19,5,0), (73,10,2,2,14,5,0), (73,10,2,4,9,5,0), (73,10,2,6,4,5,0), (73,12,2,5,4,6,0), (73,12,3,0,9,4,0), (73,12,3,3,4,6,-2), (73,12,3,3,5,4,0), (73,12,4,2,4,3,0), (73,12,6,0,3,2,-2), (73,14,2,4,5,7,0), (73,15,3,0,9,5,0), (73,15,3,3,3,7,-2), (73,15,3,3,4,5,0), (73,16,2,4,3,8,0), (73,18,3,2,5,6,0), (73,18,3,3,3,6,0), (73,18,6,0,3,3,-2), (73,18,6,0,3,3,0), (73,20,4,2,3,5,0), (73,24,4,1,4,6,0), (73,24,6,0,3,4,-2), (73,24,6,0,3,4,0), (73,30,6,0,3,5,-2), (73,30,6,0,3,5,0), (73,36,6,0,3,6,-2), (73,36,6,0,3,6,0), (73,42,6,0,3,7,-2), (73,42,6,0,3,7,0), (73,48,6,0,3,8,-2), (73,48,6,0,3,8,0), (74,3,1,0,74,3,0), (74,3,1,1,71,3,0), (74,3,1,2,68,3,0), (74,3,1,3,65,3,0), (74,3,1,4,62,3,0), (74,3,1,5,59,3,0), (74,3,1,6,56,3,0), (74,3,1,7,53,3,0), (74,3,1,8,50,3,0), (74,3,1,9,47,3,0), (74,3,1,10,44,3,0), (74,3,1,11,41,3,0), (74,3,1,12,38,3,0), (74,3,1,13,35,3,0), (74,3,1,14,32,3,0), (74,3,1,15,29,3,0), (74,3,1,16,26,3,0), (74,3,1,17,23,3,0), (74,3,1,18,20,3,0), (74,3,1,19,17,3,0), (74,3,1,20,14,3,0), (74,3,1,21,11,3,0), (74,3,1,22,8,3,0), (74,3,1,23,5,3,0), (74,4,1,0,74,4,0), (74,4,1,1,70,4,0), (74,4,1,2,66,4,0), (74,4,1,3,62,4,0), (74,4,1,4,58,4,0), (74,4,1,5,54,4,0), (74,4,1,6,50,4,0), (74,4,1,7,46,4,0), (74,4,1,8,42,4,0), (74,4,1,9,38,4,0), (74,4,1,10,34,4,0), (74,4,1,11,30,4,0), (74,4,1,12,26,4,0), (74,4,1,13,22,4,0), (74,4,1,14,18,4,0), (74,4,1,15,14,4,0), (74,4,1,16,10,4,0), (74,4,1,17,6,4,0), (74,5,1,0,74,5,0), (74,5,1,1,69,5,0), (74,5,1,2,64,5,0), (74,5,1,3,59,5,0), (74,5,1,4,54,5,0), (74,5,1,5,49,5,0), (74,5,1,6,44,5,0), (74,5,1,7,39,5,0), (74,5,1,8,34,5,0), (74,5,1,9,29,5,0), (74,5,1,10,24,5,0), (74,5,1,11,19,5,0), (74,5,1,12,14,5,0), (74,5,1,13,9,5,0), (74,5,1,14,4,5,0), (74,7,1,10,4,7,0), (75,1,3,1,9,1,-2), (75,1,3,7,3,5,-2), (75,2,5,4,3,2,-2), (75,3,1,0,75,3,0), (75,3,1,1,72,3,0), (75,3,1,2,69,3,0), (75,3,1,3,66,3,0), (75,3,1,4,63,3,0), (75,3,1,5,60,3,0), (75,3,1,6,57,3,0), (75,3,1,7,54,3,0), (75,3,1,8,51,3,0), (75,3,1,9,48,3,0), (75,3,1,10,45,3,0), (75,3,1,11,42,3,0), (75,3,1,12,39,3,0), (75,3,1,13,36,3,0), (75,3,1,14,33,3,0), (75,3,1,15,30,3,0), (75,3,1,16,27,3,0), (75,3,1,17,24,3,0), (75,3,1,18,21,3,0), (75,3,1,19,18,3,0), (75,3,1,20,15,3,0), (75,3,1,21,12,3,0), (75,3,1,22,9,3,0), (75,3,1,23,6,3,0), (75,3,1,24,3,3,0), (75,4,1,0,75,4,0), (75,4,1,1,71,4,0), (75,4,1,2,67,4,0), (75,4,1,3,63,4,0), (75,4,1,4,59,4,0), (75,4,1,5,55,4,0), (75,4,1,6,51,4,0), (75,4,1,7,47,4,0), (75,4,1,8,43,4,0), (75,4,1,9,39,4,0), (75,4,1,10,35,4,0), (75,4,1,11,31,4,0), (75,4,1,12,27,4,0), (75,4,1,13,23,4,0), (75,4,1,14,19,4,0), (75,4,1,15,15,4,0), (75,4,1,16,11,4,0), (75,4,1,17,7,4,0), (75,4,1,18,3,4,0), (75,5,1,0,75,5,0), (75,5,1,1,70,5,0), (75,5,1,2,65,5,0), (75,5,1,3,60,5,0), (75,5,1,4,55,5,0), (75,5,1,5,50,5,0), (75,5,1,6,45,5,0), (75,5,1,7,40,5,0), (75,5,1,8,35,5,0), (75,5,1,9,30,5,0), (75,5,1,10,25,5,0), (75,5,1,11,20,5,0), (75,5,1,12,15,5,0), (75,5,1,13,10,5,0), (75,5,1,14,5,5,0), (75,6,1,12,3,6,0), (75,6,2,1,18,3,0), (75,6,2,3,15,3,0), (75,6,2,5,12,3,0), (75,6,2,7,9,3,0), (75,6,2,9,6,3,0), (75,6,2,11,3,3,0), (75,7,1,10,5,7,0), (75,8,1,9,3,8,0), (75,10,2,1,17,5,0), (75,10,2,3,12,5,0), (75,10,2,5,7,5,0), (75,10,3,4,3,6,-2), (75,17,3,2,5,7,-2), (75,23,5,1,3,5,-2), (76,3,1,0,76,3,0), (76,3,1,1,73,3,0), (76,3,1,2,70,3,0), (76,3,1,3,67,3,0), (76,3,1,4,64,3,0), (76,3,1,5,61,3,0), (76,3,1,6,58,3,0), (76,3,1,7,55,3,0), (76,3,1,8,52,3,0), (76,3,1,9,49,3,0), (76,3,1,10,46,3,0), (76,3,1,11,43,3,0), (76,3,1,12,40,3,0), (76,3,1,13,37,3,0), (76,3,1,14,34,3,0), (76,3,1,15,31,3,0), (76,3,1,16,28,3,0), (76,3,1,17,25,3,0), (76,3,1,18,22,3,0), (76,3,1,19,19,3,0), (76,3,1,20,16,3,0), (76,3,1,21,13,3,0), (76,3,1,22,10,3,0), (76,3,1,23,7,3,0), (76,3,1,24,4,3,0), (76,4,1,0,76,4,0), (76,4,1,1,72,4,0), (76,4,1,2,68,4,0), (76,4,1,3,64,4,0), (76,4,1,4,60,4,0), (76,4,1,5,56,4,0), (76,4,1,6,52,4,0), (76,4,1,7,48,4,0), (76,4,1,8,44,4,0), (76,4,1,9,40,4,0), (76,4,1,10,36,4,0), (76,4,1,11,32,4,0), (76,4,1,12,28,4,0), (76,4,1,13,24,4,0), (76,4,1,14,20,4,0), (76,4,1,15,16,4,0), (76,4,1,16,12,4,0), (76,4,1,17,8,4,0), (76,4,1,18,4,4,0), (76,5,1,0,76,5,0), (76,5,1,1,71,5,0), (76,5,1,2,66,5,0), (76,5,1,3,61,5,0), (76,5,1,4,56,5,0), (76,5,1,5,51,5,0), (76,5,1,6,46,5,0), (76,5,1,7,41,5,0), (76,5,1,8,36,5,0), (76,5,1,9,31,5,0), (76,5,1,10,26,5,0), (76,5,1,11,21,5,0), (76,5,1,12,16,5,0), (76,5,1,13,11,5,0), (76,5,1,14,6,5,0), (76,5,5,0,4,1,-2), (76,6,1,12,4,6,0), (76,6,4,3,4,3,-2), (76,10,4,1,5,3,-2), (76,10,5,0,4,2,-2), (76,12,3,1,8,4,0), (76,12,3,4,4,4,0), (76,15,3,2,6,5,0), (76,15,5,0,4,3,-2), (76,15,5,0,4,3,0), (76,20,5,0,4,4,-2), (76,20,5,0,4,4,0), (76,25,5,0,4,5,-2), (76,25,5,0,4,5,0), (76,25,5,1,3,5,0), (76,30,5,0,4,6,-2), (76,30,5,0,4,6,0), (76,35,5,0,4,7,0), (77,3,1,0,77,3,0), (77,3,1,1,74,3,0), (77,3,1,2,71,3,0), (77,3,1,3,68,3,0), (77,3,1,4,65,3,0), (77,3,1,5,62,3,0), (77,3,1,6,59,3,0), (77,3,1,7,56,3,0), (77,3,1,8,53,3,0), (77,3,1,9,50,3,0), (77,3,1,10,47,3,0), (77,3,1,11,44,3,0), (77,3,1,12,41,3,0), (77,3,1,13,38,3,0), (77,3,1,14,35,3,0), (77,3,1,15,32,3,0), (77,3,1,16,29,3,0), (77,3,1,17,26,3,0), (77,3,1,18,23,3,0), (77,3,1,19,20,3,0), (77,3,1,20,17,3,0), (77,3,1,21,14,3,0), (77,3,1,22,11,3,0), (77,3,1,23,8,3,0), (77,3,1,24,5,3,0), (77,4,1,0,77,4,0), (77,4,1,1,73,4,0), (77,4,1,2,69,4,0), (77,4,1,3,65,4,0), (77,4,1,4,61,4,0), (77,4,1,5,57,4,0), (77,4,1,6,53,4,0), (77,4,1,7,49,4,0), (77,4,1,8,45,4,0), (77,4,1,9,41,4,0), (77,4,1,10,37,4,0), (77,4,1,11,33,4,0), (77,4,1,12,29,4,0), (77,4,1,13,25,4,0), (77,4,1,14,21,4,0), (77,4,1,15,17,4,0), (77,4,1,16,13,4,0), (77,4,1,17,9,4,0), (77,4,1,18,5,4,0), (77,4,2,6,5,8,-2), (77,4,4,2,5,2,-2), (77,5,1,0,77,5,0), (77,5,1,1,72,5,0), (77,5,1,2,67,5,0), (77,5,1,3,62,5,0), (77,5,1,4,57,5,0), (77,5,1,5,52,5,0), (77,5,1,6,47,5,0), (77,5,1,7,42,5,0), (77,5,1,8,37,5,0), (77,5,1,9,32,5,0), (77,5,1,10,27,5,0), (77,5,1,11,22,5,0), (77,5,1,12,17,5,0), (77,5,1,13,12,5,0), (77,5,1,14,7,5,0), (77,6,1,12,5,6,0), (77,6,2,0,20,3,0), (77,6,2,2,17,3,0), (77,6,2,4,14,3,0), (77,6,2,6,11,3,0), (77,6,2,8,8,3,0), (77,6,2,10,5,3,0), (77,8,1,9,5,8,0), (77,8,2,0,20,4,0), (77,8,2,1,18,4,0), (77,8,2,2,16,4,0), (77,8,2,3,14,4,0), (77,8,2,4,12,4,0), (77,8,2,5,10,4,0), (77,8,2,6,8,4,0), (77,8,2,7,6,4,0), (77,8,2,8,4,4,0), (77,9,1,8,5,9,0), (77,10,2,0,20,5,0), (77,10,2,2,15,5,0), (77,10,2,4,10,5,0), (77,10,2,6,5,5,0), (77,11,5,2,3,3,-2), (77,12,2,5,5,6,0), (77,12,4,1,5,3,0), (77,12,4,2,4,4,-2), (77,20,4,2,3,6,-2), (77,28,4,1,4,7,0), (78,3,1,0,78,3,0), (78,3,1,1,75,3,0), (78,3,1,2,72,3,0), (78,3,1,3,69,3,0), (78,3,1,4,66,3,0), (78,3,1,5,63,3,0), (78,3,1,6,60,3,0), (78,3,1,7,57,3,0), (78,3,1,8,54,3,0), (78,3,1,9,51,3,0), (78,3,1,10,48,3,0), (78,3,1,11,45,3,0), (78,3,1,12,42,3,0), (78,3,1,13,39,3,0), (78,3,1,14,36,3,0), (78,3,1,15,33,3,0), (78,3,1,16,30,3,0), (78,3,1,17,27,3,0), (78,3,1,18,24,3,0), (78,3,1,19,21,3,0), (78,3,1,20,18,3,0), (78,3,1,21,15,3,0), (78,3,1,22,12,3,0), (78,3,1,23,9,3,0), (78,3,1,24,6,3,0), (78,3,1,25,3,3,0), (78,4,1,0,78,4,0), (78,4,1,1,74,4,0), (78,4,1,2,70,4,0), (78,4,1,3,66,4,0), (78,4,1,4,62,4,0), (78,4,1,5,58,4,0), (78,4,1,6,54,4,0), (78,4,1,7,50,4,0), (78,4,1,8,46,4,0), (78,4,1,9,42,4,0), (78,4,1,10,38,4,0), (78,4,1,11,34,4,0), (78,4,1,12,30,4,0), (78,4,1,13,26,4,0), (78,4,1,14,22,4,0), (78,4,1,15,18,4,0), (78,4,1,16,14,4,0), (78,4,1,17,10,4,0), (78,4,1,18,6,4,0), (78,4,3,1,9,2,-2), (78,4,6,1,3,1,-2), (78,5,1,0,78,5,0), (78,5,1,1,73,5,0), (78,5,1,2,68,5,0), (78,5,1,3,63,5,0), (78,5,1,4,58,5,0), (78,5,1,5,53,5,0), (78,5,1,6,48,5,0), (78,5,1,7,43,5,0), (78,5,1,8,38,5,0), (78,5,1,9,33,5,0), (78,5,1,10,28,5,0), (78,5,1,11,23,5,0), (78,5,1,12,18,5,0), (78,5,1,13,13,5,0), (78,5,1,14,8,5,0), (78,5,1,15,3,5,0), (78,5,3,5,4,5,-2), (79,3,1,0,79,3,0), (79,3,1,1,76,3,0), (79,3,1,2,73,3,0), (79,3,1,3,70,3,0), (79,3,1,4,67,3,0), (79,3,1,5,64,3,0), (79,3,1,6,61,3,0), (79,3,1,7,58,3,0), (79,3,1,8,55,3,0), (79,3,1,9,52,3,0), (79,3,1,10,49,3,0), (79,3,1,11,46,3,0), (79,3,1,12,43,3,0), (79,3,1,13,40,3,0), (79,3,1,14,37,3,0), (79,3,1,15,34,3,0), (79,3,1,16,31,3,0), (79,3,1,17,28,3,0), (79,3,1,18,25,3,0), (79,3,1,19,22,3,0), (79,3,1,20,19,3,0), (79,3,1,21,16,3,0), (79,3,1,22,13,3,0), (79,3,1,23,10,3,0), (79,3,1,24,7,3,0), (79,3,1,25,4,3,0), (79,4,1,0,79,4,0), (79,4,1,1,75,4,0), (79,4,1,2,71,4,0), (79,4,1,3,67,4,0), (79,4,1,4,63,4,0), (79,4,1,5,59,4,0), (79,4,1,6,55,4,0), (79,4,1,7,51,4,0), (79,4,1,8,47,4,0), (79,4,1,9,43,4,0), (79,4,1,10,39,4,0), (79,4,1,11,35,4,0), (79,4,1,12,31,4,0), (79,4,1,13,27,4,0), (79,4,1,14,23,4,0), (79,4,1,15,19,4,0), (79,4,1,16,15,4,0), (79,4,1,17,11,4,0), (79,4,1,18,7,4,0), (79,4,1,19,3,4,0), (79,5,1,0,79,5,0), (79,5,1,1,74,5,0), (79,5,1,2,69,5,0), (79,5,1,3,64,5,0), (79,5,1,4,59,5,0), (79,5,1,5,54,5,0), (79,5,1,6,49,5,0), (79,5,1,7,44,5,0), (79,5,1,8,39,5,0), (79,5,1,9,34,5,0), (79,5,1,10,29,5,0), (79,5,1,11,24,5,0), (79,5,1,12,19,5,0), (79,5,1,13,14,5,0), (79,5,1,14,9,5,0), (79,5,1,15,4,5,0), (79,6,2,1,19,3,0), (79,6,2,3,16,3,0), (79,6,2,5,13,3,0), (79,6,2,7,10,3,0), (79,6,2,9,7,3,0), (79,6,2,11,4,3,0), (79,10,2,1,18,5,0), (79,10,2,3,13,5,0), (79,10,2,5,8,5,0), (79,10,2,7,3,5,0), (79,12,3,2,7,4,0), (79,12,3,5,3,4,0), (79,14,2,5,3,7,0), (79,15,3,1,8,5,0), (79,15,3,4,3,5,0), (79,21,3,2,5,7,0), (80,2,2,7,5,8,-2), (80,2,4,3,5,2,-2), (80,3,1,0,80,3,0), (80,3,1,1,77,3,0), (80,3,1,2,74,3,0), (80,3,1,3,71,3,0), (80,3,1,4,68,3,0), (80,3,1,5,65,3,0), (80,3,1,6,62,3,0), (80,3,1,7,59,3,0), (80,3,1,8,56,3,0), (80,3,1,9,53,3,0), (80,3,1,10,50,3,0), (80,3,1,11,47,3,0), (80,3,1,12,44,3,0), (80,3,1,13,41,3,0), (80,3,1,14,38,3,0), (80,3,1,15,35,3,0), (80,3,1,16,32,3,0), (80,3,1,17,29,3,0), (80,3,1,18,26,3,0), (80,3,1,19,23,3,0), (80,3,1,20,20,3,0), (80,3,1,21,17,3,0), (80,3,1,22,14,3,0), (80,3,1,23,11,3,0), (80,3,1,24,8,3,0), (80,3,1,25,5,3,0), (80,3,5,1,4,1,-2), (80,4,1,0,80,4,0), (80,4,1,1,76,4,0), (80,4,1,2,72,4,0), (80,4,1,3,68,4,0), (80,4,1,4,64,4,0), (80,4,1,5,60,4,0), (80,4,1,6,56,4,0), (80,4,1,7,52,4,0), (80,4,1,8,48,4,0), (80,4,1,9,44,4,0), (80,4,1,10,40,4,0), (80,4,1,11,36,4,0), (80,4,1,12,32,4,0), (80,4,1,13,28,4,0), (80,4,1,14,24,4,0), (80,4,1,15,20,4,0), (80,4,1,16,16,4,0), (80,4,1,17,12,4,0), (80,4,1,18,8,4,0), (80,4,1,19,4,4,0), (80,5,1,0,80,5,0), (80,5,1,1,75,5,0), (80,5,1,2,70,5,0), (80,5,1,3,65,5,0), (80,5,1,4,60,5,0), (80,5,1,5,55,5,0), (80,5,1,6,50,5,0), (80,5,1,7,45,5,0), (80,5,1,8,40,5,0), (80,5,1,9,35,5,0), (80,5,1,10,30,5,0), (80,5,1,11,25,5,0), (80,5,1,12,20,5,0), (80,5,1,13,15,5,0), (80,5,1,14,10,5,0), (80,5,1,15,5,5,0), (80,7,1,11,3,7,0), (80,14,4,1,5,4,-2), (80,28,5,1,3,6,-2), (81,2,3,2,9,2,-2), (81,2,6,2,3,1,-2), (81,3,1,0,81,3,0), (81,3,1,1,78,3,0), (81,3,1,2,75,3,0), (81,3,1,3,72,3,0), (81,3,1,4,69,3,0), (81,3,1,5,66,3,0), (81,3,1,6,63,3,0), (81,3,1,7,60,3,0), (81,3,1,8,57,3,0), (81,3,1,9,54,3,0), (81,3,1,10,51,3,0), (81,3,1,11,48,3,0), (81,3,1,12,45,3,0), (81,3,1,13,42,3,0), (81,3,1,14,39,3,0), (81,3,1,15,36,3,0), (81,3,1,16,33,3,0), (81,3,1,17,30,3,0), (81,3,1,18,27,3,0), (81,3,1,19,24,3,0), (81,3,1,20,21,3,0), (81,3,1,21,18,3,0), (81,3,1,22,15,3,0), (81,3,1,23,12,3,0), (81,3,1,24,9,3,0), (81,3,1,25,6,3,0), (81,3,1,26,3,3,0), (81,4,1,0,81,4,0), (81,4,1,1,77,4,0), (81,4,1,2,73,4,0), (81,4,1,3,69,4,0), (81,4,1,4,65,4,0), (81,4,1,5,61,4,0), (81,4,1,6,57,4,0), (81,4,1,7,53,4,0), (81,4,1,8,49,4,0), (81,4,1,9,45,4,0), (81,4,1,10,41,4,0), (81,4,1,11,37,4,0), (81,4,1,12,33,4,0), (81,4,1,13,29,4,0), (81,4,1,14,25,4,0), (81,4,1,15,21,4,0), (81,4,1,16,17,4,0), (81,4,1,17,13,4,0), (81,4,1,18,9,4,0), (81,4,1,19,5,4,0), (81,4,4,0,6,1,-2), (81,4,4,4,4,3,-2), (81,5,1,0,81,5,0), (81,5,1,1,76,5,0), (81,5,1,2,71,5,0), (81,5,1,3,66,5,0), (81,5,1,4,61,5,0), (81,5,1,5,56,5,0), (81,5,1,6,51,5,0), (81,5,1,7,46,5,0), (81,5,1,8,41,5,0), (81,5,1,9,36,5,0), (81,5,1,10,31,5,0), (81,5,1,11,26,5,0), (81,5,1,12,21,5,0), (81,5,1,13,16,5,0), (81,5,1,14,11,5,0), (81,5,1,15,6,5,0), (81,6,1,13,3,6,0), (81,6,2,0,21,3,0), (81,6,2,2,18,3,0), (81,6,2,4,15,3,0), (81,6,2,6,12,3,0), (81,6,2,8,9,3,0), (81,6,2,10,6,3,0), (81,6,2,12,3,3,0), (81,7,1,11,4,7,0), (81,7,3,4,5,5,-2), (81,8,2,0,21,4,0), (81,8,2,1,19,4,0), (81,8,2,2,17,4,0), (81,8,2,3,15,4,0), (81,8,2,4,13,4,0), (81,8,2,5,11,4,0), (81,8,2,6,9,4,0), (81,8,2,7,7,4,0), (81,8,2,8,5,4,0), (81,8,2,9,3,4,0), (81,8,4,0,6,2,-2), (81,8,4,4,3,4,-2), (81,10,2,0,21,5,0), (81,10,2,2,16,5,0), (81,10,2,4,11,5,0), (81,10,2,6,6,5,0), (81,12,2,6,3,6,0), (81,12,4,0,6,3,0), (81,12,4,4,3,3,0), (81,15,5,2,3,3,0), (81,16,2,4,5,8,0), (81,16,4,0,6,4,0), (81,16,4,1,5,4,0), (81,16,4,2,4,4,0), (81,16,4,3,3,4,0), (81,20,3,2,5,8,-2), (81,20,4,0,6,5,0), (81,24,4,2,3,6,0), (81,30,5,1,3,6,0), (82,1,5,2,4,1,-2), (82,3,1,0,82,3,0), (82,3,1,1,79,3,0), (82,3,1,2,76,3,0), (82,3,1,3,73,3,0), (82,3,1,4,70,3,0), (82,3,1,5,67,3,0), (82,3,1,6,64,3,0), (82,3,1,7,61,3,0), (82,3,1,8,58,3,0), (82,3,1,9,55,3,0), (82,3,1,10,52,3,0), (82,3,1,11,49,3,0), (82,3,1,12,46,3,0), (82,3,1,13,43,3,0), (82,3,1,14,40,3,0), (82,3,1,15,37,3,0), (82,3,1,16,34,3,0), (82,3,1,17,31,3,0), (82,3,1,18,28,3,0), (82,3,1,19,25,3,0), (82,3,1,20,22,3,0), (82,3,1,21,19,3,0), (82,3,1,22,16,3,0), (82,3,1,23,13,3,0), (82,3,1,24,10,3,0), (82,3,1,25,7,3,0), (82,3,1,26,4,3,0), (82,3,3,0,10,1,-2), (82,3,3,6,4,5,-2), (82,4,1,0,82,4,0), (82,4,1,1,78,4,0), (82,4,1,2,74,4,0), (82,4,1,3,70,4,0), (82,4,1,4,66,4,0), (82,4,1,5,62,4,0), (82,4,1,6,58,4,0), (82,4,1,7,54,4,0), (82,4,1,8,50,4,0), (82,4,1,9,46,4,0), (82,4,1,10,42,4,0), (82,4,1,11,38,4,0), (82,4,1,12,34,4,0), (82,4,1,13,30,4,0), (82,4,1,14,26,4,0), (82,4,1,15,22,4,0), (82,4,1,16,18,4,0), (82,4,1,17,14,4,0), (82,4,1,18,10,4,0), (82,4,1,19,6,4,0), (82,5,1,0,82,5,0), (82,5,1,1,77,5,0), (82,5,1,2,72,5,0), (82,5,1,3,67,5,0), (82,5,1,4,62,5,0), (82,5,1,5,57,5,0), (82,5,1,6,52,5,0), (82,5,1,7,47,5,0), (82,5,1,8,42,5,0), (82,5,1,9,37,5,0), (82,5,1,10,32,5,0), (82,5,1,11,27,5,0), (82,5,1,12,22,5,0), (82,5,1,13,17,5,0), (82,5,1,14,12,5,0), (82,5,1,15,7,5,0), (82,6,1,13,4,6,0), (82,6,3,0,10,2,-2), (82,7,1,11,5,7,0), (82,9,3,0,10,3,0), (82,9,3,1,9,3,0), (82,9,3,2,8,3,0), (82,9,3,3,7,3,0), (82,9,3,4,6,3,0), (82,9,3,5,5,3,0), (82,9,3,6,4,3,0), (82,9,3,7,3,3,0), (82,12,3,0,10,4,0), (82,12,3,3,5,6,-2), (82,12,3,3,6,4,0), (82,15,3,0,10,5,0), (82,15,3,3,5,5,0), (82,18,3,3,3,8,-2), (82,18,3,3,4,6,0), (82,21,3,3,3,7,0), (83,3,1,0,83,3,0), (83,3,1,1,80,3,0), (83,3,1,2,77,3,0), (83,3,1,3,74,3,0), (83,3,1,4,71,3,0), (83,3,1,5,68,3,0), (83,3,1,6,65,3,0), (83,3,1,7,62,3,0), (83,3,1,8,59,3,0), (83,3,1,9,56,3,0), (83,3,1,10,53,3,0), (83,3,1,11,50,3,0), (83,3,1,12,47,3,0), (83,3,1,13,44,3,0), (83,3,1,14,41,3,0), (83,3,1,15,38,3,0), (83,3,1,16,35,3,0), (83,3,1,17,32,3,0), (83,3,1,18,29,3,0), (83,3,1,19,26,3,0), (83,3,1,20,23,3,0), (83,3,1,21,20,3,0), (83,3,1,22,17,3,0), (83,3,1,23,14,3,0), (83,3,1,24,11,3,0), (83,3,1,25,8,3,0), (83,3,1,26,5,3,0), (83,4,1,0,83,4,0), (83,4,1,1,79,4,0), (83,4,1,2,75,4,0), (83,4,1,3,71,4,0), (83,4,1,4,67,4,0), (83,4,1,5,63,4,0), (83,4,1,6,59,4,0), (83,4,1,7,55,4,0), (83,4,1,8,51,4,0), (83,4,1,9,47,4,0), (83,4,1,10,43,4,0), (83,4,1,11,39,4,0), (83,4,1,12,35,4,0), (83,4,1,13,31,4,0), (83,4,1,14,27,4,0), (83,4,1,15,23,4,0), (83,4,1,16,19,4,0), (83,4,1,17,15,4,0), (83,4,1,18,11,4,0), (83,4,1,19,7,4,0), (83,4,1,20,3,4,0), (83,5,1,0,83,5,0), (83,5,1,1,78,5,0), (83,5,1,2,73,5,0), (83,5,1,3,68,5,0), (83,5,1,4,63,5,0), (83,5,1,5,58,5,0), (83,5,1,6,53,5,0), (83,5,1,7,48,5,0), (83,5,1,8,43,5,0), (83,5,1,9,38,5,0), (83,5,1,10,33,5,0), (83,5,1,11,28,5,0), (83,5,1,12,23,5,0), (83,5,1,13,18,5,0), (83,5,1,14,13,5,0), (83,5,1,15,8,5,0), (83,5,1,16,3,5,0), (83,6,1,13,5,6,0), (83,6,2,1,20,3,0), (83,6,2,3,17,3,0), (83,6,2,5,14,3,0), (83,6,2,7,11,3,0), (83,6,2,9,8,3,0), (83,6,2,11,5,3,0), (83,8,1,10,3,8,0), (83,10,2,1,19,5,0), (83,10,2,3,14,5,0), (83,10,2,5,9,5,0), (83,10,2,7,4,5,0), (83,14,2,5,4,7,0), (84,1,3,1,10,1,-2), (84,1,3,7,4,5,-2), (84,2,4,1,6,1,-2), (84,2,4,5,4,3,-2), (84,3,1,0,84,3,0), (84,3,1,1,81,3,0), (84,3,1,2,78,3,0), (84,3,1,3,75,3,0), (84,3,1,4,72,3,0), (84,3,1,5,69,3,0), (84,3,1,6,66,3,0), (84,3,1,7,63,3,0), (84,3,1,8,60,3,0), (84,3,1,9,57,3,0), (84,3,1,10,54,3,0), (84,3,1,11,51,3,0), (84,3,1,12,48,3,0), (84,3,1,13,45,3,0), (84,3,1,14,42,3,0), (84,3,1,15,39,3,0), (84,3,1,16,36,3,0), (84,3,1,17,33,3,0), (84,3,1,18,30,3,0), (84,3,1,19,27,3,0), (84,3,1,20,24,3,0), (84,3,1,21,21,3,0), (84,3,1,22,18,3,0), (84,3,1,23,15,3,0), (84,3,1,24,12,3,0), (84,3,1,25,9,3,0), (84,3,1,26,6,3,0), (84,3,1,27,3,3,0), (84,4,1,0,84,4,0), (84,4,1,1,80,4,0), (84,4,1,2,76,4,0), (84,4,1,3,72,4,0), (84,4,1,4,68,4,0), (84,4,1,5,64,4,0), (84,4,1,6,60,4,0), (84,4,1,7,56,4,0), (84,4,1,8,52,4,0), (84,4,1,9,48,4,0), (84,4,1,10,44,4,0), (84,4,1,11,40,4,0), (84,4,1,12,36,4,0), (84,4,1,13,32,4,0), (84,4,1,14,28,4,0), (84,4,1,15,24,4,0), (84,4,1,16,20,4,0), (84,4,1,17,16,4,0), (84,4,1,18,12,4,0), (84,4,1,19,8,4,0), (84,4,1,20,4,4,0), (84,5,1,0,84,5,0), (84,5,1,1,79,5,0), (84,5,1,2,74,5,0), (84,5,1,3,69,5,0), (84,5,1,4,64,5,0), (84,5,1,5,59,5,0), (84,5,1,6,54,5,0), (84,5,1,7,49,5,0), (84,5,1,8,44,5,0), (84,5,1,9,39,5,0), (84,5,1,10,34,5,0), (84,5,1,11,29,5,0), (84,5,1,12,24,5,0), (84,5,1,13,19,5,0), (84,5,1,14,14,5,0), (84,5,1,15,9,5,0), (84,5,1,16,4,5,0), (84,8,3,5,3,6,-2), (84,10,3,4,4,6,-2), (84,10,6,1,3,2,-2), (84,14,4,3,3,5,-2), (84,18,4,1,5,5,-2), (85,3,1,0,85,3,0), (85,3,1,1,82,3,0), (85,3,1,2,79,3,0), (85,3,1,3,76,3,0), (85,3,1,4,73,3,0), (85,3,1,5,70,3,0), (85,3,1,6,67,3,0), (85,3,1,7,64,3,0), (85,3,1,8,61,3,0), (85,3,1,9,58,3,0), (85,3,1,10,55,3,0), (85,3,1,11,52,3,0), (85,3,1,12,49,3,0), (85,3,1,13,46,3,0), (85,3,1,14,43,3,0), (85,3,1,15,40,3,0), (85,3,1,16,37,3,0), (85,3,1,17,34,3,0), (85,3,1,18,31,3,0), (85,3,1,19,28,3,0), (85,3,1,20,25,3,0), (85,3,1,21,22,3,0), (85,3,1,22,19,3,0), (85,3,1,23,16,3,0), (85,3,1,24,13,3,0), (85,3,1,25,10,3,0), (85,3,1,26,7,3,0), (85,3,1,27,4,3,0), (85,4,1,0,85,4,0), (85,4,1,1,81,4,0), (85,4,1,2,77,4,0), (85,4,1,3,73,4,0), (85,4,1,4,69,4,0), (85,4,1,5,65,4,0), (85,4,1,6,61,4,0), (85,4,1,7,57,4,0), (85,4,1,8,53,4,0), (85,4,1,9,49,4,0), (85,4,1,10,45,4,0), (85,4,1,11,41,4,0), (85,4,1,12,37,4,0), (85,4,1,13,33,4,0), (85,4,1,14,29,4,0), (85,4,1,15,25,4,0), (85,4,1,16,21,4,0), (85,4,1,17,17,4,0), (85,4,1,18,13,4,0), (85,4,1,19,9,4,0), (85,4,1,20,5,4,0), (85,5,1,0,85,5,0), (85,5,1,1,80,5,0), (85,5,1,2,75,5,0), (85,5,1,3,70,5,0), (85,5,1,4,65,5,0), (85,5,1,5,60,5,0), (85,5,1,6,55,5,0), (85,5,1,7,50,5,0), (85,5,1,8,45,5,0), (85,5,1,9,40,5,0), (85,5,1,10,35,5,0), (85,5,1,11,30,5,0), (85,5,1,12,25,5,0), (85,5,1,13,20,5,0), (85,5,1,14,15,5,0), (85,5,1,15,10,5,0), (85,5,1,16,5,5,0), (85,6,2,0,22,3,0), (85,6,2,2,19,3,0), (85,6,2,4,16,3,0), (85,6,2,6,13,3,0), (85,6,2,8,10,3,0), (85,6,2,10,7,3,0), (85,6,2,12,4,3,0), (85,8,1,10,5,8,0), (85,8,2,0,22,4,0), (85,8,2,1,20,4,0), (85,8,2,2,18,4,0), (85,8,2,3,16,4,0), (85,8,2,4,14,4,0), (85,8,2,5,12,4,0), (85,8,2,6,10,4,0), (85,8,2,7,8,4,0), (85,8,2,8,6,4,0), (85,8,2,9,4,4,0), (85,8,4,2,5,3,-2), (85,8,5,1,4,2,-2), (85,10,2,0,22,5,0), (85,10,2,2,17,5,0), (85,10,2,4,12,5,0), (85,10,2,6,7,5,0), (85,12,2,6,4,6,0), (85,12,3,1,9,4,0), (85,12,3,4,5,4,0), (85,12,4,3,4,3,0), (85,15,3,2,7,5,0), (85,16,4,2,4,5,-2), (85,20,4,1,5,5,0), (85,24,3,2,5,8,0), (85,24,4,2,3,7,-2), (85,33,5,1,3,7,-2), (86,3,1,0,86,3,0), (86,3,1,1,83,3,0), (86,3,1,2,80,3,0), (86,3,1,3,77,3,0), (86,3,1,4,74,3,0), (86,3,1,5,71,3,0), (86,3,1,6,68,3,0), (86,3,1,7,65,3,0), (86,3,1,8,62,3,0), (86,3,1,9,59,3,0), (86,3,1,10,56,3,0), (86,3,1,11,53,3,0), (86,3,1,12,50,3,0), (86,3,1,13,47,3,0), (86,3,1,14,44,3,0), (86,3,1,15,41,3,0), (86,3,1,16,38,3,0), (86,3,1,17,35,3,0), (86,3,1,18,32,3,0), (86,3,1,19,29,3,0), (86,3,1,20,26,3,0), (86,3,1,21,23,3,0), (86,3,1,22,20,3,0), (86,3,1,23,17,3,0), (86,3,1,24,14,3,0), (86,3,1,25,11,3,0), (86,3,1,26,8,3,0), (86,3,1,27,5,3,0), (86,4,1,0,86,4,0), (86,4,1,1,82,4,0), (86,4,1,2,78,4,0), (86,4,1,3,74,4,0), (86,4,1,4,70,4,0), (86,4,1,5,66,4,0), (86,4,1,6,62,4,0), (86,4,1,7,58,4,0), (86,4,1,8,54,4,0), (86,4,1,9,50,4,0), (86,4,1,10,46,4,0), (86,4,1,11,42,4,0), (86,4,1,12,38,4,0), (86,4,1,13,34,4,0), (86,4,1,14,30,4,0), (86,4,1,15,26,4,0), (86,4,1,16,22,4,0), (86,4,1,17,18,4,0), (86,4,1,18,14,4,0), (86,4,1,19,10,4,0), (86,4,1,20,6,4,0), (86,5,1,0,86,5,0), (86,5,1,1,81,5,0), (86,5,1,2,76,5,0), (86,5,1,3,71,5,0), (86,5,1,4,66,5,0), (86,5,1,5,61,5,0), (86,5,1,6,56,5,0), (86,5,1,7,51,5,0), (86,5,1,8,46,5,0), (86,5,1,9,41,5,0), (86,5,1,10,36,5,0), (86,5,1,11,31,5,0), (86,5,1,12,26,5,0), (86,5,1,13,21,5,0), (86,5,1,14,16,5,0), (86,5,1,15,11,5,0), (86,5,1,16,6,5,0), (86,9,1,9,5,9,0), (86,35,5,1,3,7,0), (87,3,1,0,87,3,0), (87,3,1,1,84,3,0), (87,3,1,2,81,3,0), (87,3,1,3,78,3,0), (87,3,1,4,75,3,0), (87,3,1,5,72,3,0), (87,3,1,6,69,3,0), (87,3,1,7,66,3,0), (87,3,1,8,63,3,0), (87,3,1,9,60,3,0), (87,3,1,10,57,3,0), (87,3,1,11,54,3,0), (87,3,1,12,51,3,0), (87,3,1,13,48,3,0), (87,3,1,14,45,3,0), (87,3,1,15,42,3,0), (87,3,1,16,39,3,0), (87,3,1,17,36,3,0), (87,3,1,18,33,3,0), (87,3,1,19,30,3,0), (87,3,1,20,27,3,0), (87,3,1,21,24,3,0), (87,3,1,22,21,3,0), (87,3,1,23,18,3,0), (87,3,1,24,15,3,0), (87,3,1,25,12,3,0), (87,3,1,26,9,3,0), (87,3,1,27,6,3,0), (87,3,1,28,3,3,0), (87,4,1,0,87,4,0), (87,4,1,1,83,4,0), (87,4,1,2,79,4,0), (87,4,1,3,75,4,0), (87,4,1,4,71,4,0), (87,4,1,5,67,4,0), (87,4,1,6,63,4,0), (87,4,1,7,59,4,0), (87,4,1,8,55,4,0), (87,4,1,9,51,4,0), (87,4,1,10,47,4,0), (87,4,1,11,43,4,0), (87,4,1,12,39,4,0), (87,4,1,13,35,4,0), (87,4,1,14,31,4,0), (87,4,1,15,27,4,0), (87,4,1,16,23,4,0), (87,4,1,17,19,4,0), (87,4,1,18,15,4,0), (87,4,1,19,11,4,0), (87,4,1,20,7,4,0), (87,4,1,21,3,4,0), (87,4,3,1,10,2,-2), (87,5,1,0,87,5,0), (87,5,1,1,82,5,0), (87,5,1,2,77,5,0), (87,5,1,3,72,5,0), (87,5,1,4,67,5,0), (87,5,1,5,62,5,0), (87,5,1,6,57,5,0), (87,5,1,7,52,5,0), (87,5,1,8,47,5,0), (87,5,1,9,42,5,0), (87,5,1,10,37,5,0), (87,5,1,11,32,5,0), (87,5,1,12,27,5,0), (87,5,1,13,22,5,0), (87,5,1,14,17,5,0), (87,5,1,15,12,5,0), (87,5,1,16,7,5,0), (87,5,3,5,5,5,-2), (87,6,1,14,3,6,0), (87,6,2,1,21,3,0), (87,6,2,3,18,3,0), (87,6,2,5,15,3,0), (87,6,2,7,12,3,0), (87,6,2,9,9,3,0), (87,6,2,11,6,3,0), (87,6,2,13,3,3,0), (87,7,1,12,3,7,0), (87,9,5,3,3,3,-2), (87,10,2,1,20,5,0), (87,10,2,3,15,5,0), (87,10,2,5,10,5,0), (87,10,2,7,5,5,0), (87,13,3,4,3,7,-2), (87,14,2,5,5,7,0), (87,16,5,2,3,4,-2), (88,3,1,0,88,3,0), (88,3,1,1,85,3,0), (88,3,1,2,82,3,0), (88,3,1,3,79,3,0), (88,3,1,4,76,3,0), (88,3,1,5,73,3,0), (88,3,1,6,70,3,0), (88,3,1,7,67,3,0), (88,3,1,8,64,3,0), (88,3,1,9,61,3,0), (88,3,1,10,58,3,0), (88,3,1,11,55,3,0), (88,3,1,12,52,3,0), (88,3,1,13,49,3,0), (88,3,1,14,46,3,0), (88,3,1,15,43,3,0), (88,3,1,16,40,3,0), (88,3,1,17,37,3,0), (88,3,1,18,34,3,0), (88,3,1,19,31,3,0), (88,3,1,20,28,3,0), (88,3,1,21,25,3,0), (88,3,1,22,22,3,0), (88,3,1,23,19,3,0), (88,3,1,24,16,3,0), (88,3,1,25,13,3,0), (88,3,1,26,10,3,0), (88,3,1,27,7,3,0), (88,3,1,28,4,3,0), (88,4,1,0,88,4,0), (88,4,1,1,84,4,0), (88,4,1,2,80,4,0), (88,4,1,3,76,4,0), (88,4,1,4,72,4,0), (88,4,1,5,68,4,0), (88,4,1,6,64,4,0), (88,4,1,7,60,4,0), (88,4,1,8,56,4,0), (88,4,1,9,52,4,0), (88,4,1,10,48,4,0), (88,4,1,11,44,4,0), (88,4,1,12,40,4,0), (88,4,1,13,36,4,0), (88,4,1,14,32,4,0), (88,4,1,15,28,4,0), (88,4,1,16,24,4,0), (88,4,1,17,20,4,0), (88,4,1,18,16,4,0), (88,4,1,19,12,4,0), (88,4,1,20,8,4,0), (88,4,1,21,4,4,0), (88,5,1,0,88,5,0), (88,5,1,1,83,5,0), (88,5,1,2,78,5,0), (88,5,1,3,73,5,0), (88,5,1,4,68,5,0), (88,5,1,5,63,5,0), (88,5,1,6,58,5,0), (88,5,1,7,53,5,0), (88,5,1,8,48,5,0), (88,5,1,9,43,5,0), (88,5,1,10,38,5,0), (88,5,1,11,33,5,0), (88,5,1,12,28,5,0), (88,5,1,13,23,5,0), (88,5,1,14,18,5,0), (88,5,1,15,13,5,0), (88,5,1,16,8,5,0), (88,5,1,17,3,5,0), (88,6,1,14,4,6,0), (88,6,4,1,6,2,-2), (88,6,4,5,3,4,-2), (88,7,1,12,4,7,0), (88,10,4,3,4,4,-2), (88,12,3,2,8,4,0), (88,12,3,5,4,4,0), (88,15,3,1,9,5,0), (88,15,3,4,4,5,0), (88,22,4,1,5,6,-2), (89,3,1,0,89,3,0), (89,3,1,1,86,3,0), (89,3,1,2,83,3,0), (89,3,1,3,80,3,0), (89,3,1,4,77,3,0), (89,3,1,5,74,3,0), (89,3,1,6,71,3,0), (89,3,1,7,68,3,0), (89,3,1,8,65,3,0), (89,3,1,9,62,3,0), (89,3,1,10,59,3,0), (89,3,1,11,56,3,0), (89,3,1,12,53,3,0), (89,3,1,13,50,3,0), (89,3,1,14,47,3,0), (89,3,1,15,44,3,0), (89,3,1,16,41,3,0), (89,3,1,17,38,3,0), (89,3,1,18,35,3,0), (89,3,1,19,32,3,0), (89,3,1,20,29,3,0), (89,3,1,21,26,3,0), (89,3,1,22,23,3,0), (89,3,1,23,20,3,0), (89,3,1,24,17,3,0), (89,3,1,25,14,3,0), (89,3,1,26,11,3,0), (89,3,1,27,8,3,0), (89,3,1,28,5,3,0), (89,4,1,0,89,4,0), (89,4,1,1,85,4,0), (89,4,1,2,81,4,0), (89,4,1,3,77,4,0), (89,4,1,4,73,4,0), (89,4,1,5,69,4,0), (89,4,1,6,65,4,0), (89,4,1,7,61,4,0), (89,4,1,8,57,4,0), (89,4,1,9,53,4,0), (89,4,1,10,49,4,0), (89,4,1,11,45,4,0), (89,4,1,12,41,4,0), (89,4,1,13,37,4,0), (89,4,1,14,33,4,0), (89,4,1,15,29,4,0), (89,4,1,16,25,4,0), (89,4,1,17,21,4,0), (89,4,1,18,17,4,0), (89,4,1,19,13,4,0), (89,4,1,20,9,4,0), (89,4,1,21,5,4,0), (89,5,1,0,89,5,0), (89,5,1,1,84,5,0), (89,5,1,2,79,5,0), (89,5,1,3,74,5,0), (89,5,1,4,69,5,0), (89,5,1,5,64,5,0), (89,5,1,6,59,5,0), (89,5,1,7,54,5,0), (89,5,1,8,49,5,0), (89,5,1,9,44,5,0), (89,5,1,10,39,5,0), (89,5,1,11,34,5,0), (89,5,1,12,29,5,0), (89,5,1,13,24,5,0), (89,5,1,14,19,5,0), (89,5,1,15,14,5,0), (89,5,1,16,9,5,0), (89,5,1,17,4,5,0), (89,6,1,14,5,6,0), (89,6,2,0,23,3,0), (89,6,2,2,20,3,0), (89,6,2,4,17,3,0), (89,6,2,6,14,3,0), (89,6,2,8,11,3,0), (89,6,2,10,8,3,0), (89,6,2,12,5,3,0), (89,7,1,12,5,7,0), (89,8,2,0,23,4,0), (89,8,2,1,21,4,0), (89,8,2,2,19,4,0), (89,8,2,3,17,4,0), (89,8,2,4,15,4,0), (89,8,2,5,13,4,0), (89,8,2,6,11,4,0), (89,8,2,7,9,4,0), (89,8,2,8,7,4,0), (89,8,2,9,5,4,0), (89,8,2,10,3,4,0), (89,10,2,0,23,5,0), (89,10,2,2,18,5,0), (89,10,2,4,13,5,0), (89,10,2,6,8,5,0), (89,10,2,8,3,5,0), (89,12,2,6,5,6,0), (89,12,4,2,5,3,0), (89,16,2,5,3,8,0), (89,18,2,4,5,9,0), (89,20,4,2,4,5,0), (89,24,4,1,5,6,0), (89,28,4,2,3,7,0), (90,2,3,2,10,2,-2), (90,3,1,0,90,3,0), (90,3,1,1,87,3,0), (90,3,1,2,84,3,0), (90,3,1,3,81,3,0), (90,3,1,4,78,3,0), (90,3,1,5,75,3,0), (90,3,1,6,72,3,0), (90,3,1,7,69,3,0), (90,3,1,8,66,3,0), (90,3,1,9,63,3,0), (90,3,1,10,60,3,0), (90,3,1,11,57,3,0), (90,3,1,12,54,3,0), (90,3,1,13,51,3,0), (90,3,1,14,48,3,0), (90,3,1,15,45,3,0), (90,3,1,16,42,3,0), (90,3,1,17,39,3,0), (90,3,1,18,36,3,0), (90,3,1,19,33,3,0), (90,3,1,20,30,3,0), (90,3,1,21,27,3,0), (90,3,1,22,24,3,0), (90,3,1,23,21,3,0), (90,3,1,24,18,3,0), (90,3,1,25,15,3,0), (90,3,1,26,12,3,0), (90,3,1,27,9,3,0), (90,3,1,28,6,3,0), (90,3,1,29,3,3,0), (90,4,1,0,90,4,0), (90,4,1,1,86,4,0), (90,4,1,2,82,4,0), (90,4,1,3,78,4,0), (90,4,1,4,74,4,0), (90,4,1,5,70,4,0), (90,4,1,6,66,4,0), (90,4,1,7,62,4,0), (90,4,1,8,58,4,0), (90,4,1,9,54,4,0), (90,4,1,10,50,4,0), (90,4,1,11,46,4,0), (90,4,1,12,42,4,0), (90,4,1,13,38,4,0), (90,4,1,14,34,4,0), (90,4,1,15,30,4,0), (90,4,1,16,26,4,0), (90,4,1,17,22,4,0), (90,4,1,18,18,4,0), (90,4,1,19,14,4,0), (90,4,1,20,10,4,0), (90,4,1,21,6,4,0), (90,5,1,0,90,5,0), (90,5,1,1,85,5,0), (90,5,1,2,80,5,0), (90,5,1,3,75,5,0), (90,5,1,4,70,5,0), (90,5,1,5,65,5,0), (90,5,1,6,60,5,0), (90,5,1,7,55,5,0), (90,5,1,8,50,5,0), (90,5,1,9,45,5,0), (90,5,1,10,40,5,0), (90,5,1,11,35,5,0), (90,5,1,12,30,5,0), (90,5,1,13,25,5,0), (90,5,1,14,20,5,0), (90,5,1,15,15,5,0), (90,5,1,16,10,5,0), (90,5,1,17,5,5,0), (90,13,5,1,4,3,-2), (90,16,6,1,3,3,-2), (90,38,5,1,3,8,-2), (91,3,1,0,91,3,0), (91,3,1,1,88,3,0), (91,3,1,2,85,3,0), (91,3,1,3,82,3,0), (91,3,1,4,79,3,0), (91,3,1,5,76,3,0), (91,3,1,6,73,3,0), (91,3,1,7,70,3,0), (91,3,1,8,67,3,0), (91,3,1,9,64,3,0), (91,3,1,10,61,3,0), (91,3,1,11,58,3,0), (91,3,1,12,55,3,0), (91,3,1,13,52,3,0), (91,3,1,14,49,3,0), (91,3,1,15,46,3,0), (91,3,1,16,43,3,0), (91,3,1,17,40,3,0), (91,3,1,18,37,3,0), (91,3,1,19,34,3,0), (91,3,1,20,31,3,0), (91,3,1,21,28,3,0), (91,3,1,22,25,3,0), (91,3,1,23,22,3,0), (91,3,1,24,19,3,0), (91,3,1,25,16,3,0), (91,3,1,26,13,3,0), (91,3,1,27,10,3,0), (91,3,1,28,7,3,0), (91,3,1,29,4,3,0), (91,3,3,6,5,5,-2), (91,4,1,0,91,4,0), (91,4,1,1,87,4,0), (91,4,1,2,83,4,0), (91,4,1,3,79,4,0), (91,4,1,4,75,4,0), (91,4,1,5,71,4,0), (91,4,1,6,67,4,0), (91,4,1,7,63,4,0), (91,4,1,8,59,4,0), (91,4,1,9,55,4,0), (91,4,1,10,51,4,0), (91,4,1,11,47,4,0), (91,4,1,12,43,4,0), (91,4,1,13,39,4,0), (91,4,1,14,35,4,0), (91,4,1,15,31,4,0), (91,4,1,16,27,4,0), (91,4,1,17,23,4,0), (91,4,1,18,19,4,0), (91,4,1,19,15,4,0), (91,4,1,20,11,4,0), (91,4,1,21,7,4,0), (91,4,1,22,3,4,0), (91,5,1,0,91,5,0), (91,5,1,1,86,5,0), (91,5,1,2,81,5,0), (91,5,1,3,76,5,0), (91,5,1,4,71,5,0), (91,5,1,5,66,5,0), (91,5,1,6,61,5,0), (91,5,1,7,56,5,0), (91,5,1,8,51,5,0), (91,5,1,9,46,5,0), (91,5,1,10,41,5,0), (91,5,1,11,36,5,0), (91,5,1,12,31,5,0), (91,5,1,13,26,5,0), (91,5,1,14,21,5,0), (91,5,1,15,16,5,0), (91,5,1,16,11,5,0), (91,5,1,17,6,5,0), (91,6,2,1,22,3,0), (91,6,2,3,19,3,0), (91,6,2,5,16,3,0), (91,6,2,7,13,3,0), (91,6,2,9,10,3,0), (91,6,2,11,7,3,0), (91,6,2,13,4,3,0), (91,6,3,0,11,2,-2), (91,6,3,6,3,6,-2), (91,8,1,11,3,8,0), (91,9,3,0,11,3,0), (91,9,3,1,10,3,0), (91,9,3,2,9,3,0), (91,9,3,3,8,3,0), (91,9,3,4,7,3,0), (91,9,3,5,6,3,0), (91,9,3,6,5,3,0), (91,9,3,7,4,3,0), (91,9,3,8,3,3,0), (91,10,2,1,21,5,0), (91,10,2,3,16,5,0), (91,10,2,5,11,5,0), (91,10,2,7,6,5,0), (91,12,3,0,11,4,0), (91,12,3,3,7,4,0), (91,12,3,6,3,4,0), (91,15,3,0,11,5,0), (91,15,3,3,5,7,-2), (91,15,3,3,6,5,0), (91,15,5,1,4,3,0), (91,18,3,3,5,6,0), (91,18,3,4,3,6,0), (91,18,6,1,3,3,0), (91,20,5,2,3,4,0), (91,21,3,3,4,7,0), (91,24,3,3,3,8,0), (91,27,3,2,5,9,0), (91,40,5,1,3,8,0), (92,3,1,0,92,3,0), (92,3,1,1,89,3,0), (92,3,1,2,86,3,0), (92,3,1,3,83,3,0), (92,3,1,4,80,3,0), (92,3,1,5,77,3,0), (92,3,1,6,74,3,0), (92,3,1,7,71,3,0), (92,3,1,8,68,3,0), (92,3,1,9,65,3,0), (92,3,1,10,62,3,0), (92,3,1,11,59,3,0), (92,3,1,12,56,3,0), (92,3,1,13,53,3,0), (92,3,1,14,50,3,0), (92,3,1,15,47,3,0), (92,3,1,16,44,3,0), (92,3,1,17,41,3,0), (92,3,1,18,38,3,0), (92,3,1,19,35,3,0), (92,3,1,20,32,3,0), (92,3,1,21,29,3,0), (92,3,1,22,26,3,0), (92,3,1,23,23,3,0), (92,3,1,24,20,3,0), (92,3,1,25,17,3,0), (92,3,1,26,14,3,0), (92,3,1,27,11,3,0), (92,3,1,28,8,3,0), (92,3,1,29,5,3,0), (92,4,1,0,92,4,0), (92,4,1,1,88,4,0), (92,4,1,2,84,4,0), (92,4,1,3,80,4,0), (92,4,1,4,76,4,0), (92,4,1,5,72,4,0), (92,4,1,6,68,4,0), (92,4,1,7,64,4,0), (92,4,1,8,60,4,0), (92,4,1,9,56,4,0), (92,4,1,10,52,4,0), (92,4,1,11,48,4,0), (92,4,1,12,44,4,0), (92,4,1,13,40,4,0), (92,4,1,14,36,4,0), (92,4,1,15,32,4,0), (92,4,1,16,28,4,0), (92,4,1,17,24,4,0), (92,4,1,18,20,4,0), (92,4,1,19,16,4,0), (92,4,1,20,12,4,0), (92,4,1,21,8,4,0), (92,4,1,22,4,4,0), (92,5,1,0,92,5,0), (92,5,1,1,87,5,0), (92,5,1,2,82,5,0), (92,5,1,3,77,5,0), (92,5,1,4,72,5,0), (92,5,1,5,67,5,0), (92,5,1,6,62,5,0), (92,5,1,7,57,5,0), (92,5,1,8,52,5,0), (92,5,1,9,47,5,0), (92,5,1,10,42,5,0), (92,5,1,11,37,5,0), (92,5,1,12,32,5,0), (92,5,1,13,27,5,0), (92,5,1,14,22,5,0), (92,5,1,15,17,5,0), (92,5,1,16,12,5,0), (92,5,1,17,7,5,0), (92,6,4,3,5,3,-2), (92,6,5,2,4,2,-2), (92,26,4,1,5,7,-2), (93,1,3,7,5,5,-2), (93,3,1,0,93,3,0), (93,3,1,1,90,3,0), (93,3,1,2,87,3,0), (93,3,1,3,84,3,0), (93,3,1,4,81,3,0), (93,3,1,5,78,3,0), (93,3,1,6,75,3,0), (93,3,1,7,72,3,0), (93,3,1,8,69,3,0), (93,3,1,9,66,3,0), (93,3,1,10,63,3,0), (93,3,1,11,60,3,0), (93,3,1,12,57,3,0), (93,3,1,13,54,3,0), (93,3,1,14,51,3,0), (93,3,1,15,48,3,0), (93,3,1,16,45,3,0), (93,3,1,17,42,3,0), (93,3,1,18,39,3,0), (93,3,1,19,36,3,0), (93,3,1,20,33,3,0), (93,3,1,21,30,3,0), (93,3,1,22,27,3,0), (93,3,1,23,24,3,0), (93,3,1,24,21,3,0), (93,3,1,25,18,3,0), (93,3,1,26,15,3,0), (93,3,1,27,12,3,0), (93,3,1,28,9,3,0), (93,3,1,29,6,3,0), (93,3,1,30,3,3,0), (93,4,1,0,93,4,0), (93,4,1,1,89,4,0), (93,4,1,2,85,4,0), (93,4,1,3,81,4,0), (93,4,1,4,77,4,0), (93,4,1,5,73,4,0), (93,4,1,6,69,4,0), (93,4,1,7,65,4,0), (93,4,1,8,61,4,0), (93,4,1,9,57,4,0), (93,4,1,10,53,4,0), (93,4,1,11,49,4,0), (93,4,1,12,45,4,0), (93,4,1,13,41,4,0), (93,4,1,14,37,4,0), (93,4,1,15,33,4,0), (93,4,1,16,29,4,0), (93,4,1,17,25,4,0), (93,4,1,18,21,4,0), (93,4,1,19,17,4,0), (93,4,1,20,13,4,0), (93,4,1,21,9,4,0), (93,4,1,22,5,4,0), (93,4,4,2,6,2,-2), (93,4,4,6,3,4,-2), (93,5,1,0,93,5,0), (93,5,1,1,88,5,0), (93,5,1,2,83,5,0), (93,5,1,3,78,5,0), (93,5,1,4,73,5,0), (93,5,1,5,68,5,0), (93,5,1,6,63,5,0), (93,5,1,7,58,5,0), (93,5,1,8,53,5,0), (93,5,1,9,48,5,0), (93,5,1,10,43,5,0), (93,5,1,11,38,5,0), (93,5,1,12,33,5,0), (93,5,1,13,28,5,0), (93,5,1,14,23,5,0), (93,5,1,15,18,5,0), (93,5,1,16,13,5,0), (93,5,1,17,8,5,0), (93,5,1,18,3,5,0), (93,6,1,15,3,6,0), (93,6,2,0,24,3,0), (93,6,2,2,21,3,0), (93,6,2,4,18,3,0), (93,6,2,6,15,3,0), (93,6,2,8,12,3,0), (93,6,2,10,9,3,0), (93,6,2,12,6,3,0), (93,6,2,14,3,3,0), (93,8,1,11,5,8,0), (93,8,2,0,24,4,0), (93,8,2,1,22,4,0), (93,8,2,2,20,4,0), (93,8,2,3,18,4,0), (93,8,2,4,16,4,0), (93,8,2,5,14,4,0), (93,8,2,6,12,4,0), (93,8,2,7,10,4,0), (93,8,2,8,8,4,0), (93,8,2,9,6,4,0), (93,8,2,10,4,4,0), (93,8,3,5,4,6,-2), (93,8,6,2,3,2,-2), (93,10,2,0,24,5,0), (93,10,2,2,19,5,0), (93,10,2,4,14,5,0), (93,10,2,6,9,5,0), (93,10,2,8,4,5,0), (93,10,3,4,5,6,-2), (93,12,2,7,3,6,0), (93,12,4,1,6,3,0), (93,12,4,2,5,4,-2), (93,12,4,5,3,3,0), (93,14,2,6,3,7,0), (93,20,4,2,4,6,-2), (93,20,4,3,3,5,0), (93,28,4,1,5,7,0), (93,28,4,2,3,8,-2), (94,3,1,0,94,3,0), (94,3,1,1,91,3,0), (94,3,1,2,88,3,0), (94,3,1,3,85,3,0), (94,3,1,4,82,3,0), (94,3,1,5,79,3,0), (94,3,1,6,76,3,0), (94,3,1,7,73,3,0), (94,3,1,8,70,3,0), (94,3,1,9,67,3,0), (94,3,1,10,64,3,0), (94,3,1,11,61,3,0), (94,3,1,12,58,3,0), (94,3,1,13,55,3,0), (94,3,1,14,52,3,0), (94,3,1,15,49,3,0), (94,3,1,16,46,3,0), (94,3,1,17,43,3,0), (94,3,1,18,40,3,0), (94,3,1,19,37,3,0), (94,3,1,20,34,3,0), (94,3,1,21,31,3,0), (94,3,1,22,28,3,0), (94,3,1,23,25,3,0), (94,3,1,24,22,3,0), (94,3,1,25,19,3,0), (94,3,1,26,16,3,0), (94,3,1,27,13,3,0), (94,3,1,28,10,3,0), (94,3,1,29,7,3,0), (94,3,1,30,4,3,0), (94,4,1,0,94,4,0), (94,4,1,1,90,4,0), (94,4,1,2,86,4,0), (94,4,1,3,82,4,0), (94,4,1,4,78,4,0), (94,4,1,5,74,4,0), (94,4,1,6,70,4,0), (94,4,1,7,66,4,0), (94,4,1,8,62,4,0), (94,4,1,9,58,4,0), (94,4,1,10,54,4,0), (94,4,1,11,50,4,0), (94,4,1,12,46,4,0), (94,4,1,13,42,4,0), (94,4,1,14,38,4,0), (94,4,1,15,34,4,0), (94,4,1,16,30,4,0), (94,4,1,17,26,4,0), (94,4,1,18,22,4,0), (94,4,1,19,18,4,0), (94,4,1,20,14,4,0), (94,4,1,21,10,4,0), (94,4,1,22,6,4,0), (94,5,1,0,94,5,0), (94,5,1,1,89,5,0), (94,5,1,2,84,5,0), (94,5,1,3,79,5,0), (94,5,1,4,74,5,0), (94,5,1,5,69,5,0), (94,5,1,6,64,5,0), (94,5,1,7,59,5,0), (94,5,1,8,54,5,0), (94,5,1,9,49,5,0), (94,5,1,10,44,5,0), (94,5,1,11,39,5,0), (94,5,1,12,34,5,0), (94,5,1,13,29,5,0), (94,5,1,14,24,5,0), (94,5,1,15,19,5,0), (94,5,1,16,14,5,0), (94,5,1,17,9,5,0), (94,5,1,18,4,5,0), (94,6,1,15,4,6,0), (94,7,1,13,3,7,0), (94,12,3,1,10,4,0), (94,12,3,4,6,4,0), (94,15,3,2,8,5,0), (94,15,3,5,3,5,0), (95,3,1,0,95,3,0), (95,3,1,1,92,3,0), (95,3,1,2,89,3,0), (95,3,1,3,86,3,0), (95,3,1,4,83,3,0), (95,3,1,5,80,3,0), (95,3,1,6,77,3,0), (95,3,1,7,74,3,0), (95,3,1,8,71,3,0), (95,3,1,9,68,3,0), (95,3,1,10,65,3,0), (95,3,1,11,62,3,0), (95,3,1,12,59,3,0), (95,3,1,13,56,3,0), (95,3,1,14,53,3,0), (95,3,1,15,50,3,0), (95,3,1,16,47,3,0), (95,3,1,17,44,3,0), (95,3,1,18,41,3,0), (95,3,1,19,38,3,0), (95,3,1,20,35,3,0), (95,3,1,21,32,3,0), (95,3,1,22,29,3,0), (95,3,1,23,26,3,0), (95,3,1,24,23,3,0), (95,3,1,25,20,3,0), (95,3,1,26,17,3,0), (95,3,1,27,14,3,0), (95,3,1,28,11,3,0), (95,3,1,29,8,3,0), (95,3,1,30,5,3,0), (95,4,1,0,95,4,0), (95,4,1,1,91,4,0), (95,4,1,2,87,4,0), (95,4,1,3,83,4,0), (95,4,1,4,79,4,0), (95,4,1,5,75,4,0), (95,4,1,6,71,4,0), (95,4,1,7,67,4,0), (95,4,1,8,63,4,0), (95,4,1,9,59,4,0), (95,4,1,10,55,4,0), (95,4,1,11,51,4,0), (95,4,1,12,47,4,0), (95,4,1,13,43,4,0), (95,4,1,14,39,4,0), (95,4,1,15,35,4,0), (95,4,1,16,31,4,0), (95,4,1,17,27,4,0), (95,4,1,18,23,4,0), (95,4,1,19,19,4,0), (95,4,1,20,15,4,0), (95,4,1,21,11,4,0), (95,4,1,22,7,4,0), (95,4,1,23,3,4,0), (95,5,1,0,95,5,0), (95,5,1,1,90,5,0), (95,5,1,2,85,5,0), (95,5,1,3,80,5,0), (95,5,1,4,75,5,0), (95,5,1,5,70,5,0), (95,5,1,6,65,5,0), (95,5,1,7,60,5,0), (95,5,1,8,55,5,0), (95,5,1,9,50,5,0), (95,5,1,10,45,5,0), (95,5,1,11,40,5,0), (95,5,1,12,35,5,0), (95,5,1,13,30,5,0), (95,5,1,14,25,5,0), (95,5,1,15,20,5,0), (95,5,1,16,15,5,0), (95,5,1,17,10,5,0), (95,5,1,18,5,5,0), (95,6,1,15,5,6,0), (95,6,2,1,23,3,0), (95,6,2,3,20,3,0), (95,6,2,5,17,3,0), (95,6,2,7,14,3,0), (95,6,2,9,11,3,0), (95,6,2,11,8,3,0), (95,6,2,13,5,3,0), (95,7,1,13,4,7,0), (95,7,5,4,3,3,-2), (95,9,1,10,5,9,0), (95,10,2,1,22,5,0), (95,10,2,3,17,5,0), (95,10,2,5,12,5,0), (95,10,2,7,7,5,0), (95,18,5,1,4,4,-2), (96,2,4,3,6,2,-2), (96,2,4,7,3,4,-2), (96,3,1,0,96,3,0), (96,3,1,1,93,3,0), (96,3,1,2,90,3,0), (96,3,1,3,87,3,0), (96,3,1,4,84,3,0), (96,3,1,5,81,3,0), (96,3,1,6,78,3,0), (96,3,1,7,75,3,0), (96,3,1,8,72,3,0), (96,3,1,9,69,3,0), (96,3,1,10,66,3,0), (96,3,1,11,63,3,0), (96,3,1,12,60,3,0), (96,3,1,13,57,3,0), (96,3,1,14,54,3,0), (96,3,1,15,51,3,0), (96,3,1,16,48,3,0), (96,3,1,17,45,3,0), (96,3,1,18,42,3,0), (96,3,1,19,39,3,0), (96,3,1,20,36,3,0), (96,3,1,21,33,3,0), (96,3,1,22,30,3,0), (96,3,1,23,27,3,0), (96,3,1,24,24,3,0), (96,3,1,25,21,3,0), (96,3,1,26,18,3,0), (96,3,1,27,15,3,0), (96,3,1,28,12,3,0), (96,3,1,29,9,3,0), (96,3,1,30,6,3,0), (96,3,1,31,3,3,0), (96,4,1,0,96,4,0), (96,4,1,1,92,4,0), (96,4,1,2,88,4,0), (96,4,1,3,84,4,0), (96,4,1,4,80,4,0), (96,4,1,5,76,4,0), (96,4,1,6,72,4,0), (96,4,1,7,68,4,0), (96,4,1,8,64,4,0), (96,4,1,9,60,4,0), (96,4,1,10,56,4,0), (96,4,1,11,52,4,0), (96,4,1,12,48,4,0), (96,4,1,13,44,4,0), (96,4,1,14,40,4,0), (96,4,1,15,36,4,0), (96,4,1,16,32,4,0), (96,4,1,17,28,4,0), (96,4,1,18,24,4,0), (96,4,1,19,20,4,0), (96,4,1,20,16,4,0), (96,4,1,21,12,4,0), (96,4,1,22,8,4,0), (96,4,1,23,4,4,0), (96,4,3,1,11,2,-2), (96,4,3,7,3,6,-2), (96,5,1,0,96,5,0), (96,5,1,1,91,5,0), (96,5,1,2,86,5,0), (96,5,1,3,81,5,0), (96,5,1,4,76,5,0), (96,5,1,5,71,5,0), (96,5,1,6,66,5,0), (96,5,1,7,61,5,0), (96,5,1,8,56,5,0), (96,5,1,9,51,5,0), (96,5,1,10,46,5,0), (96,5,1,11,41,5,0), (96,5,1,12,36,5,0), (96,5,1,13,31,5,0), (96,5,1,14,26,5,0), (96,5,1,15,21,5,0), (96,5,1,16,16,5,0), (96,5,1,17,11,5,0), (96,5,1,18,6,5,0), (96,7,1,13,5,7,0), (96,15,5,3,3,3,0), (96,18,4,3,3,6,-2), (96,20,5,1,4,4,0), (96,22,6,1,3,4,-2), (96,30,4,1,5,8,-2), (97,3,1,0,97,3,0), (97,3,1,1,94,3,0), (97,3,1,2,91,3,0), (97,3,1,3,88,3,0), (97,3,1,4,85,3,0), (97,3,1,5,82,3,0), (97,3,1,6,79,3,0), (97,3,1,7,76,3,0), (97,3,1,8,73,3,0), (97,3,1,9,70,3,0), (97,3,1,10,67,3,0), (97,3,1,11,64,3,0), (97,3,1,12,61,3,0), (97,3,1,13,58,3,0), (97,3,1,14,55,3,0), (97,3,1,15,52,3,0), (97,3,1,16,49,3,0), (97,3,1,17,46,3,0), (97,3,1,18,43,3,0), (97,3,1,19,40,3,0), (97,3,1,20,37,3,0), (97,3,1,21,34,3,0), (97,3,1,22,31,3,0), (97,3,1,23,28,3,0), (97,3,1,24,25,3,0), (97,3,1,25,22,3,0), (97,3,1,26,19,3,0), (97,3,1,27,16,3,0), (97,3,1,28,13,3,0), (97,3,1,29,10,3,0), (97,3,1,30,7,3,0), (97,3,1,31,4,3,0), (97,4,1,0,97,4,0), (97,4,1,1,93,4,0), (97,4,1,2,89,4,0), (97,4,1,3,85,4,0), (97,4,1,4,81,4,0), (97,4,1,5,77,4,0), (97,4,1,6,73,4,0), (97,4,1,7,69,4,0), (97,4,1,8,65,4,0), (97,4,1,9,61,4,0), (97,4,1,10,57,4,0), (97,4,1,11,53,4,0), (97,4,1,12,49,4,0), (97,4,1,13,45,4,0), (97,4,1,14,41,4,0), (97,4,1,15,37,4,0), (97,4,1,16,33,4,0), (97,4,1,17,29,4,0), (97,4,1,18,25,4,0), (97,4,1,19,21,4,0), (97,4,1,20,17,4,0), (97,4,1,21,13,4,0), (97,4,1,22,9,4,0), (97,4,1,23,5,4,0), (97,4,4,0,7,1,-2), (97,4,4,4,5,3,-2), (97,4,5,3,4,2,-2), (97,5,1,0,97,5,0), (97,5,1,1,92,5,0), (97,5,1,2,87,5,0), (97,5,1,3,82,5,0), (97,5,1,4,77,5,0), (97,5,1,5,72,5,0), (97,5,1,6,67,5,0), (97,5,1,7,62,5,0), (97,5,1,8,57,5,0), (97,5,1,9,52,5,0), (97,5,1,10,47,5,0), (97,5,1,11,42,5,0), (97,5,1,12,37,5,0), (97,5,1,13,32,5,0), (97,5,1,14,27,5,0), (97,5,1,15,22,5,0), (97,5,1,16,17,5,0), (97,5,1,17,12,5,0), (97,5,1,18,7,5,0), (97,6,2,0,25,3,0), (97,6,2,2,22,3,0), (97,6,2,4,19,3,0), (97,6,2,6,16,3,0), (97,6,2,8,13,3,0), (97,6,2,10,10,3,0), (97,6,2,12,7,3,0), (97,6,2,14,4,3,0), (97,8,2,0,25,4,0), (97,8,2,1,23,4,0), (97,8,2,2,21,4,0), (97,8,2,3,19,4,0), (97,8,2,4,17,4,0), (97,8,2,5,15,4,0), (97,8,2,6,13,4,0), (97,8,2,7,11,4,0), (97,8,2,8,9,4,0), (97,8,2,9,7,4,0), (97,8,2,10,5,4,0), (97,8,2,11,3,4,0), (97,8,4,0,7,2,-2), (97,8,4,4,4,4,-2), (97,10,2,0,25,5,0), (97,10,2,2,20,5,0), (97,10,2,4,15,5,0), (97,10,2,6,10,5,0), (97,10,2,8,5,5,0), (97,12,2,7,4,6,0), (97,12,3,2,9,4,0), (97,12,3,5,5,4,0), (97,12,4,0,7,3,0), (97,12,4,4,3,5,-2), (97,12,4,4,4,3,0), (97,14,2,6,4,7,0), (97,15,3,1,10,5,0), (97,15,3,4,5,5,0), (97,16,2,5,5,8,0), (97,16,4,0,7,4,0), (97,16,4,1,6,4,0), (97,16,4,2,5,4,0), (97,16,4,3,4,4,0), (97,16,4,4,3,4,0), (97,20,4,0,7,5,0), (97,21,5,2,3,5,-2), (97,24,4,2,4,6,0), (97,24,6,1,3,4,0), (97,32,4,1,5,8,0), (97,32,4,2,3,8,0), (98,3,1,0,98,3,0), (98,3,1,1,95,3,0), (98,3,1,2,92,3,0), (98,3,1,3,89,3,0), (98,3,1,4,86,3,0), (98,3,1,5,83,3,0), (98,3,1,6,80,3,0), (98,3,1,7,77,3,0), (98,3,1,8,74,3,0), (98,3,1,9,71,3,0), (98,3,1,10,68,3,0), (98,3,1,11,65,3,0), (98,3,1,12,62,3,0), (98,3,1,13,59,3,0), (98,3,1,14,56,3,0), (98,3,1,15,53,3,0), (98,3,1,16,50,3,0), (98,3,1,17,47,3,0), (98,3,1,18,44,3,0), (98,3,1,19,41,3,0), (98,3,1,20,38,3,0), (98,3,1,21,35,3,0), (98,3,1,22,32,3,0), (98,3,1,23,29,3,0), (98,3,1,24,26,3,0), (98,3,1,25,23,3,0), (98,3,1,26,20,3,0), (98,3,1,27,17,3,0), (98,3,1,28,14,3,0), (98,3,1,29,11,3,0), (98,3,1,30,8,3,0), (98,3,1,31,5,3,0), (98,4,1,0,98,4,0), (98,4,1,1,94,4,0), (98,4,1,2,90,4,0), (98,4,1,3,86,4,0), (98,4,1,4,82,4,0), (98,4,1,5,78,4,0), (98,4,1,6,74,4,0), (98,4,1,7,70,4,0), (98,4,1,8,66,4,0), (98,4,1,9,62,4,0), (98,4,1,10,58,4,0), (98,4,1,11,54,4,0), (98,4,1,12,50,4,0), (98,4,1,13,46,4,0), (98,4,1,14,42,4,0), (98,4,1,15,38,4,0), (98,4,1,16,34,4,0), (98,4,1,17,30,4,0), (98,4,1,18,26,4,0), (98,4,1,19,22,4,0), (98,4,1,20,18,4,0), (98,4,1,21,14,4,0), (98,4,1,22,10,4,0), (98,4,1,23,6,4,0), (98,5,1,0,98,5,0), (98,5,1,1,93,5,0), (98,5,1,2,88,5,0), (98,5,1,3,83,5,0), (98,5,1,4,78,5,0), (98,5,1,5,73,5,0), (98,5,1,6,68,5,0), (98,5,1,7,63,5,0), (98,5,1,8,58,5,0), (98,5,1,9,53,5,0), (98,5,1,10,48,5,0), (98,5,1,11,43,5,0), (98,5,1,12,38,5,0), (98,5,1,13,33,5,0), (98,5,1,14,28,5,0), (98,5,1,15,23,5,0), (98,5,1,16,18,5,0), (98,5,1,17,13,5,0), (98,5,1,18,8,5,0), (98,5,1,19,3,5,0), (99,2,3,2,11,2,-2), (99,2,3,8,3,6,-2), (99,3,1,0,99,3,0), (99,3,1,1,96,3,0), (99,3,1,2,93,3,0), (99,3,1,3,90,3,0), (99,3,1,4,87,3,0), (99,3,1,5,84,3,0), (99,3,1,6,81,3,0), (99,3,1,7,78,3,0), (99,3,1,8,75,3,0), (99,3,1,9,72,3,0), (99,3,1,10,69,3,0), (99,3,1,11,66,3,0), (99,3,1,12,63,3,0), (99,3,1,13,60,3,0), (99,3,1,14,57,3,0), (99,3,1,15,54,3,0), (99,3,1,16,51,3,0), (99,3,1,17,48,3,0), (99,3,1,18,45,3,0), (99,3,1,19,42,3,0), (99,3,1,20,39,3,0), (99,3,1,21,36,3,0), (99,3,1,22,33,3,0), (99,3,1,23,30,3,0), (99,3,1,24,27,3,0), (99,3,1,25,24,3,0), (99,3,1,26,21,3,0), (99,3,1,27,18,3,0), (99,3,1,28,15,3,0), (99,3,1,29,12,3,0), (99,3,1,30,9,3,0), (99,3,1,31,6,3,0), (99,3,1,32,3,3,0), (99,4,1,0,99,4,0), (99,4,1,1,95,4,0), (99,4,1,2,91,4,0), (99,4,1,3,87,4,0), (99,4,1,4,83,4,0), (99,4,1,5,79,4,0), (99,4,1,6,75,4,0), (99,4,1,7,71,4,0), (99,4,1,8,67,4,0), (99,4,1,9,63,4,0), (99,4,1,10,59,4,0), (99,4,1,11,55,4,0), (99,4,1,12,51,4,0), (99,4,1,13,47,4,0), (99,4,1,14,43,4,0), (99,4,1,15,39,4,0), (99,4,1,16,35,4,0), (99,4,1,17,31,4,0), (99,4,1,18,27,4,0), (99,4,1,19,23,4,0), (99,4,1,20,19,4,0), (99,4,1,21,15,4,0), (99,4,1,22,11,4,0), (99,4,1,23,7,4,0), (99,4,1,24,3,4,0), (99,5,1,0,99,5,0), (99,5,1,1,94,5,0), (99,5,1,2,89,5,0), (99,5,1,3,84,5,0), (99,5,1,4,79,5,0), (99,5,1,5,74,5,0), (99,5,1,6,69,5,0), (99,5,1,7,64,5,0), (99,5,1,8,59,5,0), (99,5,1,9,54,5,0), (99,5,1,10,49,5,0), (99,5,1,11,44,5,0), (99,5,1,12,39,5,0), (99,5,1,13,34,5,0), (99,5,1,14,29,5,0), (99,5,1,15,24,5,0), (99,5,1,16,19,5,0), (99,5,1,17,14,5,0), (99,5,1,18,9,5,0), (99,5,1,19,4,5,0), (99,6,1,16,3,6,0), (99,6,2,1,24,3,0), (99,6,2,3,21,3,0), (99,6,2,5,18,3,0), (99,6,2,7,15,3,0), (99,6,2,9,12,3,0), (99,6,2,11,9,3,0), (99,6,2,13,6,3,0), (99,6,2,15,3,3,0), (99,7,7,0,3,1,-2), (99,8,1,12,3,8,0), (99,10,2,1,23,5,0), (99,10,2,3,18,5,0), (99,10,2,5,13,5,0), (99,10,2,7,8,5,0), (99,10,2,9,3,5,0), (99,11,3,5,3,7,-2), (99,14,7,0,3,2,-2), (99,16,3,4,3,8,-2), (99,21,7,0,3,3,-2), (99,21,7,0,3,3,0), (99,28,7,0,3,4,-2), (99,28,7,0,3,4,0), (99,35,7,0,3,5,-2), (99,35,7,0,3,5,0), (99,42,7,0,3,6,-2), (99,42,7,0,3,6,0), (99,49,7,0,3,7,-2), (99,49,7,0,3,7,0), (99,56,7,0,3,8,-2), (99,56,7,0,3,8,0), (100,2,4,1,7,1,-2), (100,2,4,5,5,3,-2), (100,2,5,4,4,2,-2), (100,3,1,0,100,3,0), (100,3,1,1,97,3,0), (100,3,1,2,94,3,0), (100,3,1,3,91,3,0), (100,3,1,4,88,3,0), (100,3,1,5,85,3,0), (100,3,1,6,82,3,0), (100,3,1,7,79,3,0), (100,3,1,8,76,3,0), (100,3,1,9,73,3,0), (100,3,1,10,70,3,0), (100,3,1,11,67,3,0), (100,3,1,12,64,3,0), (100,3,1,13,61,3,0), (100,3,1,14,58,3,0), (100,3,1,15,55,3,0), (100,3,1,16,52,3,0), (100,3,1,17,49,3,0), (100,3,1,18,46,3,0), (100,3,1,19,43,3,0), (100,3,1,20,40,3,0), (100,3,1,21,37,3,0), (100,3,1,22,34,3,0), (100,3,1,23,31,3,0), (100,3,1,24,28,3,0), (100,3,1,25,25,3,0), (100,3,1,26,22,3,0), (100,3,1,27,19,3,0), (100,3,1,28,16,3,0), (100,3,1,29,13,3,0), (100,3,1,30,10,3,0), (100,3,1,31,7,3,0), (100,3,1,32,4,3,0), (100,3,3,0,12,1,-2), (100,4,1,0,100,4,0), (100,4,1,1,96,4,0), (100,4,1,2,92,4,0), (100,4,1,3,88,4,0), (100,4,1,4,84,4,0), (100,4,1,5,80,4,0), (100,4,1,6,76,4,0), (100,4,1,7,72,4,0), (100,4,1,8,68,4,0), (100,4,1,9,64,4,0), (100,4,1,10,60,4,0), (100,4,1,11,56,4,0), (100,4,1,12,52,4,0), (100,4,1,13,48,4,0), (100,4,1,14,44,4,0), (100,4,1,15,40,4,0), (100,4,1,16,36,4,0), (100,4,1,17,32,4,0), (100,4,1,18,28,4,0), (100,4,1,19,24,4,0), (100,4,1,20,20,4,0), (100,4,1,21,16,4,0), (100,4,1,22,12,4,0), (100,4,1,23,8,4,0), (100,4,1,24,4,4,0), (100,5,1,0,100,5,0), (100,5,1,1,95,5,0), (100,5,1,2,90,5,0), (100,5,1,3,85,5,0), (100,5,1,4,80,5,0), (100,5,1,5,75,5,0), (100,5,1,6,70,5,0), (100,5,1,7,65,5,0), (100,5,1,8,60,5,0), (100,5,1,9,55,5,0), (100,5,1,10,50,5,0), (100,5,1,11,45,5,0), (100,5,1,12,40,5,0), (100,5,1,13,35,5,0), (100,5,1,14,30,5,0), (100,5,1,15,25,5,0), (100,5,1,16,20,5,0), (100,5,1,17,15,5,0), (100,5,1,18,10,5,0), (100,5,1,19,5,5,0), (100,6,1,16,4,6,0), (100,6,3,0,12,2,-2), (100,6,3,6,4,6,-2), (100,6,6,3,3,2,-2), (100,9,3,0,12,3,0), (100,9,3,1,11,3,0), (100,9,3,2,10,3,0), (100,9,3,3,9,3,0), (100,9,3,4,8,3,0), (100,9,3,5,7,3,0), (100,9,3,6,6,3,0), (100,9,3,7,5,3,0), (100,9,3,8,4,3,0), (100,9,3,9,3,3,0), (100,12,3,0,12,4,0), (100,12,3,3,8,4,0), (100,12,3,6,4,4,0), (100,14,4,3,4,5,-2), (100,15,3,0,12,5,0), (100,15,3,3,7,5,0), (100,18,3,3,5,8,-2), (100,18,3,4,4,6,0), (100,21,3,3,5,7,0), (100,23,5,1,4,5,-2)};
assert(# possibilities == 5292);
fewPossibilities = select(possibilities,l -> first l <= 44);
assert(# fewPossibilities == 1157);

isprimitive = method();
isprimitive (Matrix,ZZ,ZZ) := memoize((M,a,b) -> (
    if numRows M =!= 2 or numColumns M =!= 2 or ring M =!= ZZ then error "expected a 2 x 2 matrix over ZZ"; 
    sage := findProgram("sage", "sage --help");
    dir := temporaryFileName() | "/";
    mkdir dir;
    S := "from sage.all import *"|newline;
    S = S|"M = Matrix(ZZ,[["|toString(M_(0,0))|","|toString(M_(0,1))|"],["|toString(M_(1,0))|","|toString(M_(1,1))|"]]);"|newline;
    S = S|"U = IntegralLattice(M);"|newline;
    S = S|"x = U.is_primitive(U.span([vector(["|toString(a)|","|toString(b)|"])]));"|newline;
    S = S|"(open('"|dir|"output.sage','w')).write(str(x));"|newline;
    dir|"input.sage" << S << close;
    runProgram(sage,"input.sage",RunDirectory=>dir);
    v := get(dir|"output.sage");
    if v === "False" then false else if v === "True" then true else error "something went wrong"
));

K3 String := o -> strG -> (
    G := value strG;
    if not instance(G,ZZ) then error "expected the string of an integer";
    if G > 100 then error "not implemented yet: show available functions to construct K3 surfaces of genus > 100";
    P := select(possibilities,l -> first l == G);
    if #P == 0 then error "no procedure found";
    if o.Verbose then (
        local g'; local c; local a; local b; local g; local d; local n;
        for s in P do (
            (g',c,a,b,g,d,n) = s;
            <<"(K3("<<g<<","<<d<<","<<n<<"))("<<a<<","<<b<<") -- K3 surface of genus "<<g'<<" and degree "<<2*g'-2<<" containing "<<(if n == -2 then "rational" else if n == 0 then "elliptic" else "")<<" curve of degree "<<c<<endl;
        );
    );
    return apply(P,l -> take(l,{4,6}));
);

K3 (String,VisibleList) := o -> (strG,opt) -> (
    if not #opt == 1 then error "option not available";
    opt = first opt;
    if first toList opt =!= Unique then error "Unique is the only available option for K3(String)";
    if not last opt then return K3 strG;    
    G := value strG;
    if not instance(G,ZZ) then error "expected the string of an integer";
    if G > 100 then error "not implemented yet: show available functions to construct K3 surfaces of genus > 100";
    P := select(possibilities,l -> first l == G);
    local g'; local c; local a; local b; local g; local d; local n;
    E := {};
    for s in P do (
        (g',c,a,b,g,d,n) = s;
        if not member((g',c,n),E) then (
            if not isprimitive(matrix{{2*g-2,d},{d,n}},a,b) then (
                -- <<"case (g',c,a,b,g,d,n) = "<<(g',c,a,b,g,d,n)<<" excluded as it does not give a primitive lattice embedding"<<endl;
                continue;
            );
            E = append(E,(g',c,n));
            if o.Verbose then <<"(K3("<<g<<","<<d<<","<<n<<"))("<<a<<","<<b<<") -- K3 surface of genus "<<g'<<" and degree "<<2*g'-2<<" containing "<<(if n == -2 then "rational" else if n == 0 then "elliptic" else "")<<" curve of degree "<<c<<endl;
        );
    );
    return E
);

randomPointedMukaiThreefold = method(Options => {CoefficientRing => ZZ/65521});
randomPointedMukaiThreefold ZZ := o -> g -> (
    local p; local X; local j; local psi;
    K := o.CoefficientRing;
    if g == 3 then (
        p = point PP_K^4;
        X = random({4},p);
        return (X,p);
    );
    if g == 4 then (
        p = point PP_K^5;
        X = random({{2},{3}},p);
        return (X,p);
    );
    if g == 5 then (
        p = point PP_K^6;
        X = random({{2},{2},{2}},p);
        return (X,p);
    );
    if g == 6 then (
        G14 := GG(K,1,4);
        p = schubertCycle((3,3),G14);
        j = parametrize random({{1},{1}},p);
        p = j^* p;
        X = (j^* G14) * random(2,p);
        return (X,p);
    );
    if g == 7 then (
        quinticDelPezzoSurface := Var image rationalMap(ring PP_K^2,{3,4});
        psi = rationalMap(quinticDelPezzoSurface * random(1,0_quinticDelPezzoSurface),2);
        p = psi point source psi;
        j = parametrize random({{1},{1}},p);
        X = j^* image(2,psi);
        p = j^* p;
        return (X,p);
    );
    if g == 8 then (
        G15 := GG(K,1,5);
        p = schubertCycle((4,4),G15);
        j = parametrize random({{1},{1},{1},{1},{1}},p);
        X = j^* G15;
        p = j^* p;
        return (X,p);
    );
    if g == 9 then (
        psi = rationalMap(image(PP_K^(2,2) << PP_K^6),3,2);
        p = psi point source psi;
        j = parametrize random({{1},{1},{1}},p);
        X = j^* image psi;
        p = j^* p;
        return (X,p);
    );
    if g == 10 then (
        XpLC10 := pointLineAndConicOnMukaiThreefoldOfGenus10(K,false,false);
        return (XpLC10_0,XpLC10_1);
    );
    if g == 12 then (
        XpLC12 := pointLineAndConicOnMukaiThreefoldOfGenus12(K,false,false);
        return (XpLC12_0,XpLC12_1);
    );
    error "the genus has to belong to the set {3,...,10,12}";
);

randomMukaiThreefoldContainingLine = method(Options => {CoefficientRing => ZZ/65521});
randomMukaiThreefoldContainingLine ZZ := o -> g -> (
    local L; local X; local j; local psi;
    K := o.CoefficientRing;
    if g == 3 then (
        L = random({{1},{1},{1}},0_(PP_K^4));
        X = random({4},L);
        return (X,L);
    );
    if g == 4 then (
        L = random({{1},{1},{1},{1}},0_(PP_K^5));
        X = random({{2},{3}},L);
        return (X,L);
    );
    if g == 5 then (
        L = random({{1},{1},{1},{1},{1}},0_(PP_K^6));
        X = random({{2},{2},{2}},L);
        return (X,L);
    );
    if g == 6 then (
        G14 := GG(K,1,4);
        L = schubertCycle((3,2),G14);
        j = parametrize random({{1},{1}},L);
        L = j^* L;
        X = (j^* G14) * random(2,L);
        return (X,L);
    );
    if g == 7 then (
        mapDP5 := multirationalMap rationalMap(ring PP_K^2,{3,4});
        pt := mapDP5 point source mapDP5;
        psi = rationalMap((image mapDP5) * random(1,pt),2);
        L = psi linearSpan {pt,point source psi};
        j = parametrize random({{1},{1}},L);
        X = j^* image(2,psi);
        L = j^* L;
        return (X,L);
    );
    if g == 8 then (
        G15 := GG(K,1,5);
        L = schubertCycle((4,3),G15);
        j = parametrize random({{1},{1},{1},{1},{1}},L);
        X = j^* G15;
        L = j^* L;
        return (X,L);
    );
    if g == 9 then (
        mapVerInP6 := (parametrize PP_K^(2,2)) << PP_K^6;
        psi = rationalMap(image mapVerInP6,3,2);
        L = psi linearSpan {mapVerInP6 point source mapVerInP6,point source psi};
        j = parametrize random({{1},{1},{1}},L);
        X = j^* image psi;
        L = j^* L;
        return (X,L);
    );
    if g == 10 then (
        XpLC10 := pointLineAndConicOnMukaiThreefoldOfGenus10(K,true,false);
        return (XpLC10_0,XpLC10_2);
    );
    if g == 12 then (
        XpLC12 := pointLineAndConicOnMukaiThreefoldOfGenus12(K,true,false);
        return (XpLC12_0,XpLC12_2);
    );
    error "the genus has to belong to the set {3,...,10,12}";
);

randomK3surfaceContainingConic = method(Options => {CoefficientRing => ZZ/65521});
randomK3surfaceContainingConic ZZ := o -> g -> (
    if not (g >= 3 and g <= 12) then error "expected integer between 3 and 12";
    local X; local T; local C; local p; local j;
    K := o.CoefficientRing;    
    if member(g,{3,4,5,6,7,8,9,11}) then (
        (X,p) = randomPointedMukaiThreefold(g+1,CoefficientRing=>K);
        j = parametrize random({1},tangentSpace(X,p));
        T = j^* X;
        p = (j^* p) % T;
        pr := rationalMap p; 
        (pr1,pr2) := graph pr;
        C = pr2 pr1^* p;
        T = image pr;
    );
    if g == 10 then (
        XpLC10 := pointLineAndConicOnMukaiThreefoldOfGenus10(K,false,true);
        (X,C) = (XpLC10_0,XpLC10_3);
        j = parametrize random({1},C);
        (T,C) = (j^* X,j^* C);
    );
    if g == 12 then (
        XpLC12 := pointLineAndConicOnMukaiThreefoldOfGenus12(K,false,true);
        (X,C) = (XpLC12_0,XpLC12_3);
        j = parametrize random({1},C);
        (T,C) = (j^* X,j^* C);
    );
    if not (dim C == 1 and degree C == 2 and sectionalGenus C == 0 and dim T == 2 and degree T == 2*g-2 and sectionalGenus T == g and isSubset(C,T)) then error "something went wrong";
    (T,C)
);

pointLineAndConicOnMukaiThreefoldOfGenus10 = (K,withLine,withConic) -> (
    line := null; conic := null;
    pts := apply(4,i -> point PP_K^2); 
    p := point PP_K^2;
    f := rationalMap((⋃ pts) % random(5,p + ⋃ apply(pts,i -> 2*i)),3);
    f = f * rationalMap point target f;
    -- C is a curve of degree 7 and genus 2 in P^4 and p is one of its points 
    C := image f;
    p = f p;
    if not(dim C == 1 and degree C == 7 and sectionalGenus C == 2 and dim p == 0 and degree p == 1 and isSubset(p,C)) then error "something went wrong";
    -- Q is a quadric in P^4 containing C and a point q
    q := point target f;
    Q := random(2,C + q);
    -- psi is the map Q --> PP^11 defined by the quintic hypersurfaces with double points along C
    psi := rationalMap(C_Q,5,2);
    if dim target psi != 11 then error "something went wrong";
    psi = toRationalMap psi;
    if K === ZZ/(char K) then interpolateImage(psi,toList(28:2),2) else forceImage(psi,image(2,psi));
    psi = multirationalMap psi;
    X := psi#"image";
    assert(X =!= null);
    q = psi q;
    if ? ideal q != "one-point scheme in PP^11" then error "something went wrong";
    if withConic then (
        -- the base locus of psi is supported on C and a reducible curve D, the union of 4 lines
        D := (Var trim lift(ideal matrix psi,ring ambient source psi)) \\ C;
        -- L is a line passing through p\in C which is contained in Q and intersects D in one point (thus psi L is a conic)
        -- (this needs to find a K-rational point on a certain 0-dimensional set defined over K)
        p' := select(decompose(D * coneOfLines(Q,p)),i -> dim i == 0 and degree i == 1);
        if # p' == 0 then error ("failed to find conic on Mukai threefold of genus 10 defined over "|toString(K));
        L := linearSpan {p,first p'};
        F := psi L;
        if not(dim F == 1 and degree F == 2 and flatten degrees ideal F == append(toList(9:1),2) and isSubset(F,X)) then error "something went wrong when trying to find conic on Mukai threefold of genus 10";
        conic = F;
    );
    if withLine then (
        psi = toRationalMap rationalMap(psi,Dominant=>true);
        -- B is the base locus of psi^-1, B-Supp(B) is a line.
        B := trim lift(ideal matrix approximateInverseMap(psi,Verbose=>false),ambient target psi);
        j := parametrize ideal matrix rationalMap(B,1);
        pr := rationalMap for i to 3 list random(1,source j);
        B' := pr j^* B;
        E' := B' : radical B';
        E := j radical trim (j^* ideal X + pr^* E');
        if not(dim E -1 == 1 and degree E == 1 and flatten degrees E == toList(10:1) and isSubset(ideal X,E)) then error "something went wrong when trying to find line on Mukai threefold of genus 10";
        line = Var E;
    );
    (X,q,line,conic)
);

pointLineAndConicOnMukaiThreefoldOfGenus12 = (K,withLine,withConic) -> (
    line := null; conic := null;
    f := rationalMap veronese(1,6,K);
    f = f * rationalMap(target f,ring PP_K^4,for i to 4 list random(1,target f));
    p := ideal point PP_K^4;    
    C := trim sub(image f,quotient ideal random(2,Var intersect(p,image f)));
    psi := rationalMap(saturate(C^2),5);
    if numgens target psi != 14 then error "something went wrong";
    if K === ZZ/(char K) then interpolateImage(psi,toList(45:2),2) else forceImage(psi,image(2,psi));
    X := psi#"idealImage";
    assert(X =!= null);
    p = psi p;
    if ? p != "one-point scheme in PP^13" then error "something went wrong";
    if withConic then (
        psi = rationalMap(psi,Dominant=>true);
        -- B is the base locus of psi^-1, B-Supp(B) is a conic.
        B := trim lift(ideal matrix approximateInverseMap(psi,Verbose=>false),ambient target psi);
        j := parametrize ideal matrix rationalMap(B,1);
        pr := rationalMap for i to 3 list random(1,source j);
        B' := pr j^* B;
        E' := B' : radical B';
        E := j radical trim (j^* X + pr^* E');
        if not(dim E -1 == 1 and degree E == 2 and flatten degrees E == {1,1,1,1,1,1,1,1,1,1,1,2} and isSubset(X,E)) then error "something went wrong when trying to find conic on Mukai threefold of genus 12";
        conic = E;
    );
    if withLine then (
        C = trim lift(C,ring PP_K^4);
        q := f point source f;
        -- L is a secant line to the sextic curve C contained in the quadric, source of psi
        q' := select(decompose saturate(C + ideal coneOfLines(Var ideal source psi,Var q),q),l -> dim l == 1 and degree l == 1);
        if # q' == 0 then error ("failed to find line on random Mukai threefold of genus 12 defined over "|toString(K));
        L := ideal image basis(1,intersect(q,first q'));
        F := psi L;
        if not(dim F -1 == 1 and degree F == 1 and flatten degrees F == toList(12:1) and isSubset(X,F)) then error "something went wrong when trying to find line on Mukai threefold of genus 12";
        line = F;
    );
    (Var X,Var p,if line === null then line else Var line,if conic === null then conic else Var conic)
);

power0 := method();
power0 (Ideal,ZZ) := (p,d) -> (
   assert(isPolynomialRing ambient ring p and isHomogeneous ideal ring p and isHomogeneous p and unique degrees p == {{1}});
   if d <= 0 then error "expected a positive integer";
   L := trim sub(ideal random(1,ambient ring p),ring p);
   if d == 1 or d == 2 or d == 3 then return saturate(p^d,L);
   if d == 4 then return saturate((power0(p,2))^2,L);
   if d == 5 then return saturate(power0(p,2) * power0(p,3),L);
   if d == 6 then return saturate(power0(p,3) * power0(p,3),L);
   if d == 7 then return saturate(saturate(power0(p,3) * power0(p,2),L) * power0(p,2),L);
   if d == 8 then return saturate(power0(p,4) * power0(p,4),L);
   if d > 8 then error "not implemented yet";
);

projectionInt = method();
projectionInt (VisibleList,RationalMap,ZZ,ZZ) := (L,phi,D,g) -> (
   try assert(ring matrix{L} === ZZ and #L>0) else error "expected a list of integers";
   E := invProjection(L,D,g,g);
   d := max flatten degrees ideal matrix phi;
   J := rationalMap(intersect apply(L,i -> power0(point source phi,i)),d);   
   f := rationalMap(intersect(ideal matrix phi,ideal matrix J),d);
   X := if char coefficientRing phi <= 65521 then Var image(f,"F4") else Var image f;
   <<endl;
   if E != (degree X,sectionalGenus X,dim ambient X) then <<"--warning: output is not as expected"<<endl else <<"-- (degree and genus are as expected)"<<endl;
   X
);

project = method();
project (VisibleList,LatticePolarizedK3surface,ZZ,ZZ) := (L,S,a,b) -> projectionInt(L,toRationalMap map(S,a,b),degree(S,a,b),genus(S,a,b));
project (VisibleList,EmbeddedK3surface) := (L,S) -> (
    if S.cache#?"GeneralK3" and S.cache#"GeneralK3" then return projectionInt(L,toRationalMap map S,degree S,genus S);
    (T,ab) := construction S;
    project(L,T,ab_0,ab_1)
);

invProjection = method();
invProjection (VisibleList,ZZ,ZZ,ZZ) := (mm,d,g,N) -> (
   expNumHyp := (d0,g0,r0,J) -> (
       t := local t;
       R := QQ[t];
       hP := (d,g) -> (1/2)*d*t^2 + ((1/2)*d+1-g)*t+2;
       max(binomial(r0+J,J) - sub(hP(d0,g0),t=>J),0)
   );
   mm = deepSplice mm; 
   <<"-- *** simulation ***"<<endl;
   <<"-- surface of degree "<<d<<" and sectional genus "<<g<<" in PP^"<<N<<" (quadrics: "<<expNumHyp(d,g,N,2)<<", cubics: "<<expNumHyp(d,g,N,3)<<")"<<endl;
   for m in mm do (
       (d,g,N) = (d - m^2,g - binomial(m,2),N - binomial(m+1,2)); 
       <<"-- surface of degree "<<d<<" and sectional genus "<<g<<" in PP^"<<N<<" (quadrics: "<<expNumHyp(d,g,N,2)<<", cubics: "<<expNumHyp(d,g,N,3)<<")"<<endl;
   );
   (d,g,N)
);

randomEllipticNormalCurve = method(Options => {CoefficientRing => ZZ/65521});
randomEllipticNormalCurve ZZ := o -> deg -> (
    K := o.CoefficientRing;
    S := ring PP_K^2;
    y := gens S;
    E := y_2^2*y_0-y_1^3+random(0,S)*y_1*y_0^2+random(0,S)*y_0^3;
    n := deg -1;
    nO := saturate((ideal(y_0,y_1))^(n+1)+ideal E);
    d := max flatten degrees nO;
    Hs := gens nO;    
    H := Hs*random(source Hs,S^{-d});
    linsys1 := gens truncate(d,(ideal H +ideal E):nO);
    linsys := mingens ideal (linsys1 % ideal E);
    SE := S/E;          
    phi := map(SE,ring PP_K^n,sub(linsys,SE));
    Var trim kernel phi
);

randomEllipticCurve = method(Options => {CoefficientRing => ZZ/65521});
randomEllipticCurve (ZZ,ZZ) := o -> (d,n) -> (
    K := o.CoefficientRing;
    C := randomEllipticNormalCurve(d,CoefficientRing=>K);
    f := rationalMap(ring PP_K^(d-1),ring PP_K^n,for i to n list random(1,ring PP_K^(d-1)));
    f C
);

randomRationalCurve = method(Options => {CoefficientRing => ZZ/65521});
randomRationalCurve (ZZ,ZZ) := o -> (d,n) -> (
    K := o.CoefficientRing;
    C := PP_K^(1,d);
    f := rationalMap(ring ambient C,ring PP_K^n,for i to n list random(1,ring ambient C));
    f C
);

mukaiModel = method(Options => {CoefficientRing => ZZ/65521});
mukaiModel ZZ := o -> g -> (
    K := o.CoefficientRing;
    if not member(g,{6,7,8,9,10,12}) then error "expected the genus to be in the set {6,7,8,9,10,12}";
    local psi; local X;
    if g == 6 then (
        psi = rationalMap(image(Var sub(ideal(PP_K[1,1,1]),ring PP_K^6) << PP_K^7),2,Dominant=>true);
        X = image psi;
        X.cache#"rationalParametrization" = psi;
        assert(dim X == 7 and codim X == 3 and degree X == 5 and sectionalGenus X == 1);
        return X;
    );
    if g == 7 then ( -- See [Zak - Tangents and secants of algebraic varieties - Thm. 3.8 (case 5), p. 67.]
        psi = rationalMap(image(GG(K,1,4) << PP_K^10),2,Dominant=>true);
        X = image psi;
        X.cache#"rationalParametrization" = psi;
        assert(dim X == 10 and codim X == 5 and degree X == 12 and sectionalGenus X == 7);
        return X;
    );
    if g == 8 then (-- See [Zak - Tangents and secants of algebraic varieties - Thm. 3.8 (case 3), p. 67.]
        psi = rationalMap(image(PP_K[1,1,1,1] << PP_K^8),2,Dominant=>true);
        X = image psi;
        X.cache#"rationalParametrization" = psi;
        assert(dim X == 8 and codim X == 6 and degree X == 14 and sectionalGenus X == 8);
        return X;
    );
    if g == 9 then (
        psi = rationalMap(image(PP_K^(2,2) << PP_K^6),3,2,Dominant=>true);
        X = image psi;
        X.cache#"rationalParametrization" = psi;
        assert(dim X == 6 and codim X == 7 and degree X == 16 and sectionalGenus X == 9);
        return X;
    );
    if g == 10 then ( -- p. 4 of [Kapustka and Ranestad - Vector Bundles On Fano Varieties Of Genus Ten] 
        w := gens ring PP_K^13;
        M := matrix {{0,-w_5,w_4,w_6,w_7,w_8,w_0},
                     {w_5,0,-w_3,w_12,w_13,w_9,w_1},
                     {-w_4,w_3,0,w_10,w_11,-w_6-w_13,w_2},
                     {-w_6,-w_12,-w_10,0,w_2,-w_1,w_3},
                     {-w_7,-w_13,-w_11,-w_2,0,w_0,w_4},
                     {-w_8,-w_9,w_6+w_13,w_1,-w_0,0,w_5},
                     {-w_0,-w_1,-w_2,-w_3,-w_4,-w_5,0}};
        X = projectiveVariety pfaffians(4,M);
        assert(dim X == 5 and codim X == 8 and degree X == 18 and sectionalGenus X == 10);
        psi = inverse((rationalMap {w_1,w_5,w_8,w_9,w_13,w_12})|X);
        X.cache#"rationalParametrization" = psi;        
        return X;
    );
    if g == 12 then ( -- see also pointLineAndConicOnMukaiThreefoldOfGenus12
        C := PP_K^(1,6);
        C = (rationalMap linearSpan{point ambient C,point ambient C}) C;
        psi = rationalMap(C_(random(2,C)),5,2,Dominant=>2);
        psi#"isDominant" = true;
        X = target psi;
        X.cache#"rationalParametrization" = psi;
        assert(dim X == 3 and codim X == 10 and degree X == 22 and sectionalGenus X == 12);
        return X;
    );
);


beginDocumentation() 

document {Key => K3Surfaces, 
Headline => "A package for constructing explicit examples of K3 surfaces",
References => UL{{"M. H. and G. S., ",EM"Explicit constructions of K3 surfaces and unirational Noether-Lefschetz divisors",", available at ",HREF{"https://arxiv.org/abs/2110.15819","arXiv:2110.15819"}," (2021)."}}}

document {Key => {LatticePolarizedK3surface}, 
Headline => "the class of all lattice-polarized K3 surfaces",
SeeAlso => {K3,EmbeddedK3surface}}

document {Key => {EmbeddedK3surface}, 
Headline => "the class of all embedded K3 surfaces",
SeeAlso => {LatticePolarizedK3surface,(symbol SPACE,LatticePolarizedK3surface,Sequence)}}

document {Key => {K3,(K3,ZZ,ZZ,ZZ),[K3,Verbose],[K3,CoefficientRing],[K3,Singular]}, 
Headline => "make a lattice-polarized K3 surface",
Usage => "K3(g,d,n)
K3(g,d,n,CoefficientRing=>K)", 
Inputs => {"g" => ZZ,"d" => ZZ,"n" => ZZ}, 
Outputs => {{"a ",TO2{LatticePolarizedK3surface,"K3 surface"}," defined over ",TEX///$K$///," with rank 2 lattice defined by the intersection matrix ",TEX///$\begin{pmatrix} 2g-2 & d \\ d & n \end{pmatrix}$///}}, 
EXAMPLE {"K3(6,1,-2)"},
SeeAlso => {(K3,ZZ),(K3,String),(symbol SPACE,LatticePolarizedK3surface,Sequence)}}

document {Key => {(K3,EmbeddedK3surface)}, 
Headline => "make a lattice-polarized K3 surface from an embedded K3 surface", 
Usage => "K3 S", 
Inputs => {"S" => EmbeddedK3surface =>{"a special K3 surface that contains a curve ",TEX///$C$///}}, 
Outputs => {LatticePolarizedK3surface => {"the K3 surface ",TEX///$S$///," with rank 2 lattice spanned by ",TEX///$H,C$///,", where ",TEX///$H$///," is the hyperplane section class"}}, 
EXAMPLE {"S = K3(3,5,-2);", "S(1,1)", "T = K3 S(1,1)", "T(1,0)"}} 

document {Key => {(K3,String)}, 
Headline => "show available functions to construct K3 surfaces of given genus", 
Usage => "K3 \"G\"
K3(\"G\",[Unique=>true])", 
Inputs => { String => "G" => {"the string of an integer"}}, 
Outputs => {List => {"a list of terns ",TT"(g,d,n)"," such that (",TO2{(K3,ZZ,ZZ,ZZ),TT"(K3(g,d,n)"},")",TO2{(symbol SPACE,LatticePolarizedK3surface,Sequence),"(a,b)"}," is a K3 surface of genus ",TT"G",", for some integers ",TT"a,b"}}, 
EXAMPLE {"K3(\"11\",Verbose=>true)", "S = K3(5,5,-2)", "S(1,2)", "K3 S(1,2)"}, 
SeeAlso => {(K3,ZZ,ZZ,ZZ),(symbol SPACE,LatticePolarizedK3surface,Sequence)}}

undocumented {(K3,String,VisibleList)};

document {Key => {(K3,ZZ)}, 
Headline => "make a general K3 surface",
Usage => "K3 g
K3(g,CoefficientRing=>K)",
Inputs => {"g" => ZZ}, 
Outputs => {EmbeddedK3surface => {"a general K3 surface defined over ",TEX///$K$///," of genus ",TEX///$g$///," in ",TEX///$\mathbb{P}^g$///}}, 
EXAMPLE {"K3 9"},
SeeAlso => {(K3,ZZ,ZZ,ZZ)}}

document {Key => {(K3,SpecialCubicFourfold),(K3,SpecialGushelMukaiFourfold)}, 
Headline => "K3 surface associated to a cubic or GM fourfold",
Usage => "K3 X",
Inputs => {"X" => SpecialCubicFourfold => {"or ",ofClass SpecialGushelMukaiFourfold}}, 
Outputs => {EmbeddedK3surface => {"a K3 surface associated to ",TEX///$X$///}}, 
PARA {"This function calls the function ",TO associatedK3surface,"."},
EXAMPLE {"X = specialFourfold \"tau-quadric\";", "K3 X", "associatedK3surface X"},
SeeAlso => {(associatedK3surface),(K3,ZZ)}}

document {Key => {(genus,LatticePolarizedK3surface,ZZ,ZZ),(genus,EmbeddedK3surface,ZZ,ZZ)}, 
Headline => "genus of a K3 surface", 
Usage => "genus(S,a,b)", 
Inputs => {"S" => LatticePolarizedK3surface,
           "a" => ZZ,
           "b" => ZZ}, 
Outputs => {ZZ => {"the genus of ", TEX///$S$///," embedded by the complete linear system ",TEX///$|a H + b C|$///,", where ",TEX///$H,C$///," is the basis of the lattice associated to ",TEX///$S$///}}, 
EXAMPLE {"S = K3(5,2,-2)", "genus(S,2,1)"}, 
SeeAlso => {(degree,LatticePolarizedK3surface,ZZ,ZZ)}} 

document {Key => {(degree,LatticePolarizedK3surface,ZZ,ZZ),(degree,EmbeddedK3surface,ZZ,ZZ)}, 
Headline => "degree of a K3 surface", 
Usage => "degree(S,a,b)", 
Inputs => {"S" => LatticePolarizedK3surface,
           "a" => ZZ,
           "b" => ZZ}, 
Outputs => {ZZ => {"the degree of ", TEX///$S$///," embedded by the complete linear system ",TEX///$|a H + b C|$///,", where ",TEX///$H,C$///," is the basis of the lattice associated to ",TEX///$S$///}}, 
EXAMPLE {"S = K3(5,2,-2)", "degree(S,2,1)"}, 
SeeAlso => {(genus,LatticePolarizedK3surface,ZZ,ZZ)}} 

document {Key => {project,(project,VisibleList,LatticePolarizedK3surface,ZZ,ZZ),(project,VisibleList,EmbeddedK3surface)}, 
Headline => "project a K3 surface", 
Usage => "project({i,j,k,...},S,a,b)
project({i,j,k,...},S(a,b))", 
Inputs => {VisibleList => {"a list ",TEX///$\{i,j,k,\ldots\}$///," of nonnegative integers"},
           LatticePolarizedK3surface => "S" => {"a lattice-polarized K3 surface with rank 2 lattice spanned by ",TEX///$H,C$///},
           ZZ => "a",
           ZZ => "b"}, 
Outputs => {EmbeddedProjectiveVariety => {"the projection of ", TEX///$S$///," embedded by the complete linear system ",TEX///$|a H + b C|$///," from ",TEX///$i$///," random points of multiplicity 1, ",TEX///$j$///," random points of multiplicity 2, ",TEX///$k$///," random points of multiplicity 3, and so on until the last integer in the given list."}}, 
EXAMPLE {"S = K3(8,2,-2)", "project({5,3,1},S,2,1); -- (5th + 3rd + simple)-projection of S(2,1)"}, 
SeeAlso => {(symbol SPACE,LatticePolarizedK3surface,Sequence)}} 

document {Key => {(coefficientRing,LatticePolarizedK3surface)}, 
Headline => "coefficient ring of a K3 surface", 
Usage => "coefficientRing S", 
Inputs => {"S" => LatticePolarizedK3surface}, 
Outputs => {Ring => {"the coefficient ring of ", TEX///$S$///}}, 
EXAMPLE {"K = ZZ/3331","S = K3(5,2,-2,CoefficientRing=>K)", "coefficientRing S"}} 

document {Key => {(symbol SPACE,LatticePolarizedK3surface,Sequence),(symbol SPACE,EmbeddedK3surface,Sequence)}, 
Headline => "image of the embedding of a K3 surface", 
Usage => "S(a,b)", 
Inputs => {"S" => LatticePolarizedK3surface => {"a lattice-polarized K3 surface with rank 2 lattice spanned by ",TEX///$H,C$///},
          {{"a sequence of two integers ",TT"(a,b)"}}}, 
Outputs => {EmbeddedK3surface => {"the image of the embedding of ", TEX///$S$///," by the complete linear system ",TEX///$|a H + b C|$///}}, 
EXAMPLE {"S = K3(5,2,-2)", "S(1,0)", "S(2,1)"}, 
SeeAlso => {(map,LatticePolarizedK3surface,ZZ,ZZ)}} 

document {Key => {(map,LatticePolarizedK3surface,ZZ,ZZ),(map,EmbeddedK3surface,ZZ,ZZ)}, 
Headline => "embedding of a K3 surface", 
Usage => "map(S,a,b)", 
Inputs => {"S" => LatticePolarizedK3surface,
           "a" => ZZ,
           "b" => ZZ}, 
Outputs => {{"the ",TO2{RationalMap,"birational morphism"}," defined by the complete linear system ",TEX///$|a H + b C|$///,", where ",TEX///$H,C$///," is the basis of the lattice associated to ",TEX///$S$///}}, 
EXAMPLE {"S = K3(3,1,-2)", "f = map(S,2,1);", "isMorphism f", "degree f", "assert(image f == S(2,1))"}, 
SeeAlso => {(symbol SPACE,LatticePolarizedK3surface,Sequence)}} 

undocumented {(texMath,LatticePolarizedK3surface),(net,LatticePolarizedK3surface),(symbol ?,EmbeddedK3surface),(map,EmbeddedK3surface),(genus,EmbeddedK3surface),(degree,EmbeddedK3surface)}

document {Key => {trigonalK3,(trigonalK3,ZZ),[trigonalK3,CoefficientRing]}, 
Headline => "trigonal K3 surface", 
Usage => "trigonalK3 g", 
Inputs => {"g" => ZZ =>{"the genus"}}, 
Outputs => {LatticePolarizedK3surface => {"a random trigonal K3 surface of genus ", TEX///$g$///}}, 
PARA{"See also the paper ",EM "A remark on the generalized franchetta conjecture for K3 surfaces",", by Beauville."},
EXAMPLE {"S = trigonalK3 11", "S' = S(1,0);", "map(S',0,1)"},
SeeAlso => {(K3,ZZ,ZZ,ZZ),tetragonalK3,pentagonalK3}}

document {Key => {tetragonalK3,(tetragonalK3,ZZ),[tetragonalK3,CoefficientRing]}, 
Headline => "tetragonal K3 surface", 
Usage => "tetragonalK3 g", 
Inputs => {"g" => ZZ =>{"the genus"}}, 
Outputs => {LatticePolarizedK3surface => {"a random tetragonal K3 surface of genus ", TEX///$g$///}}, 
EXAMPLE {"S = tetragonalK3 11", "S' = S(1,0);", "map(S',0,1)"},
SeeAlso => {(K3,ZZ,ZZ,ZZ),trigonalK3,pentagonalK3}}

document {Key => {pentagonalK3,(pentagonalK3,ZZ),[pentagonalK3,CoefficientRing]}, 
Headline => "pentagonal K3 surface", 
Usage => "pentagonalK3 g", 
Inputs => {"g" => ZZ =>{"the genus"}}, 
Outputs => {LatticePolarizedK3surface => {"a random pentagonal K3 surface of genus ", TEX///$g$///}}, 
EXAMPLE {"S = pentagonalK3 11", "S' = S(1,0);", "map(S',0,1)"},
SeeAlso => {(K3,ZZ,ZZ,ZZ),trigonalK3,tetragonalK3}}

document {Key => {mukaiModel,(mukaiModel,ZZ),[mukaiModel,CoefficientRing]}, 
Headline => "Mukai models", 
Usage => "mukaiModel g", 
Inputs => {"g" => ZZ =>{"the genus"}}, 
Outputs => {EmbeddedProjectiveVariety => {"the Mukai model of genus ",TEX///$g$///," and degree ",TEX///$2g-2$///}},
EXAMPLE {"X = mukaiModel 9;", "(degree X, sectionalGenus X)", "parametrize X"}} 

-- Tests --

TEST ///
for g in {3,4,5,6,7,8,9} do (
    <<"(g,d,n) = "<<(g,1,-2)<<endl;
    time S = K3(g,1,-2);
    T = S#"surface";
    C = S#"curve";
    L = S#"lattice";
    assert(dim T == 2 and degree T == 2*g-2 and sectionalGenus T == g and dim ambient T == g);
    assert(dim C == 1 and degree C == 1 and sectionalGenus C == 0 and isSubset(C,T));
    assert(L == matrix{{2*g-2,1},{1,-2}}); 
);
///

TEST ///
for e in 
select({3,4,5} ** toList(3..9),e -> ((g,d) = toSequence e; (member(g,{3,4,5}) and member(d,{3,4,5,6,7,8,9})) and (g != 5 or d <= 9) and (g != 4 or d <= 7) and (g != 3 or d <= 8)))
do (
    (g,d) := toSequence e;
    <<"(g,d,n) = "<<(g,d,0)<<endl;
    time S = K3(g,d,0);
    T = S#"surface";
    C = S#"curve";
    L = S#"lattice";
    assert(dim T == 2 and degree T == 2*g-2 and sectionalGenus T == g and dim ambient T == g);
    assert(dim C == 1 and degree C == d and sectionalGenus C == 1 and isSubset(C,T));
    assert(L == matrix{{2*g-2,d},{d,0}});    
);
///

TEST ///
for g in {3,4,5,6,7,8,9} do (
    <<"(g,d,n) = "<<(g,0,-2)<<endl;
    time S = K3(g,0,-2);
    T = S#"surface";
    C = S#"curve";
    L = S#"lattice";
    assert(dim T == 2 and degree T == 2*g-2 and sectionalGenus T == g and dim ambient T == g);
    assert(dim C == 0 and degree C == 1 and isSubset(C,T) and dim tangentSpace(T,C) > 2);
    assert(L == matrix{{2*g-2,0},{0,-2}});       
);
///

TEST ///
for g from 3 to 12 do (
    for d from 3 to 5 do (
        <<"(g,d,n) = "<<(g,d,0)<<endl;
        time S = K3(g,d,0);
        T = S#"surface";
        C = S#"curve";
        L = S#"lattice";
        assert(dim T == 2 and degree T == 2*g-2 and sectionalGenus T == g and dim ambient T == g);
        assert(dim C == 1 and degree C == d and isSubset(C,T) and sectionalGenus C == 1);
        assert(L == matrix{{2*g-2,d},{d,0}});       
    );
);
///

TEST ///
for e in 
select({3,4,5} ** toList(3..8),e -> ((g,d) = toSequence e; (member(g,{3,4,5}) and d >= 3) and (g != 5 or d <= 8) and (g != 4 or d <= 6) and (g != 3 or d <= 8)))
do (
    (g,d) := toSequence e;
    <<"(g,d,n) = "<<(g,d,-2)<<endl;
    time S = K3(g,d,-2);
    T = S#"surface";
    C = S#"curve";
    L = S#"lattice";
    assert(dim T == 2 and degree T == 2*g-2 and sectionalGenus T == g and dim ambient T == g);
    assert(dim C == 1 and degree C == d and sectionalGenus C == 0 and isSubset(C,T));
    assert(L == matrix{{2*g-2,d},{d,-2}});   
);
///

TEST /// -- randomPointedMukaiThreefold
debug K3Surfaces;
K = ZZ/333331;
for g in {3,4,5,6,7,8,9} do (
    <<"g = "<<g<<endl;
    time (X,p) = randomPointedMukaiThreefold(g,CoefficientRing=>K);
    assert(coefficientRing ring X === K and dim ambient X == g+1);
    assert isSubset(p,X);
    assert (? ideal p == "one-point scheme in PP^"|toString(g+1));
    assert(dim X == 3);
    assert(sectionalGenus X == g);
    assert(degree X == 2*g-2);
);
///

TEST /// -- randomMukaiThreefoldContainingLine
debug K3Surfaces;
K = ZZ/333331;
setRandomSeed 123456789
for g in {3,4,5,6,7,8,9} do (
    <<"g = "<<g<<endl;
    time (X,L) = randomMukaiThreefoldContainingLine(g,CoefficientRing=>K);
    assert(coefficientRing ring X === K and dim ambient X == g+1);
    assert isSubset(L,X);
    assert (? ideal L == "line in PP^"|toString(g+1));
    assert(dim X == 3);
    assert(sectionalGenus X == g);
    assert(degree X == 2*g-2);
);
///

TEST ///
for g from 3 to 7 do (
    setRandomSeed 123456789;
    <<"(g,d,n) = "<<(g,2,-2)<<endl;
    time S = K3(g,2,-2);
    T = S#"surface";
    C = S#"curve";
    L = S#"lattice";
    assert(dim T == 2 and degree T == 2*g-2 and sectionalGenus T == g and dim ambient T == g);
    assert(dim C == 1 and degree C == 2 and sectionalGenus C == 0 and isSubset(C,T));
    assert(L == matrix{{2*g-2,2},{2,-2}});
);
///

TEST ///
for g in {3,4,5,6,7,8,9} do (<<"g = "<<g<<endl; time K3 g); 
///;

end;

-- Hard tests

TEST ///
K3 10;
K3 12;
///

TEST ///
K3 11 
///

TEST ///
for g from 8 to 12 do (
    setRandomSeed 123456789;
    <<"(g,d,n) = "<<(g,2,-2)<<endl;
    time S = K3(g,2,-2);
    T = S#"surface";
    C = S#"curve";
    L = S#"lattice";
    assert(dim T == 2 and degree T == 2*g-2 and sectionalGenus T == g and dim ambient T == g);
    assert(dim C == 1 and degree C == 2 and sectionalGenus C == 0 and isSubset(C,T));
    assert(L == matrix{{2*g-2,2},{2,-2}});
);
///

TEST /// -- randomMukaiThreefoldContainingLine
debug K3Surfaces;
K = ZZ/333331;
for g in {10,12} do (
    setRandomSeed 12345;
    <<"g = "<<g<<endl;
    time (X,L) = randomMukaiThreefoldContainingLine(g,CoefficientRing=>K);
    assert(coefficientRing ring X === K and dim ambient X == g+1);
    assert isSubset(L,X);
    assert (? ideal L == "line in PP^"|toString(g+1));
    assert(dim X == 3);
    assert(sectionalGenus X == g);
    assert(degree X == 2*g-2);
);
///

TEST /// -- randomPointedMukaiThreefold
debug K3Surfaces;
K = ZZ/333331;
for g in {10,12} do (
    <<"g = "<<g<<endl;
    time (X,p) = randomPointedMukaiThreefold(g,CoefficientRing=>K);
    assert(coefficientRing ring X === K and dim ambient X == g+1);
    assert isSubset(p,X);
    assert (? ideal p == "one-point scheme in PP^"|toString(g+1));
    assert(dim X == 3);
    assert(sectionalGenus X == g);
    assert(degree X == 2*g-2);
);
///

TEST ///
for g from 13 to 15 do (
    for d from 3 to 5 do (
        <<"(g,d,n) = "<<(g,d,0)<<endl;
        time S = K3(g,d,0);
        T = S#"surface";
        C = S#"curve";
        L = S#"lattice";
        assert(dim T == 2 and degree T == 2*g-2 and sectionalGenus T == g and dim ambient T == g);
        assert(dim C == 1 and degree C == d and isSubset(C,T) and sectionalGenus C == 1);
        assert(L == matrix{{2*g-2,d},{d,0}});       
    );
);
///

TEST ///
setRandomSeed 123456789;
for g in {10,12} do (
    <<"(g,d,n) = "<<(g,1,-2)<<endl;
    time S = K3(g,1,-2);
    T = S#"surface";
    C = S#"curve";
    L = S#"lattice";
    assert(dim T == 2 and degree T == 2*g-2 and sectionalGenus T == g and dim ambient T == g);
    assert(dim C == 1 and degree C == 1 and sectionalGenus C == 0 and isSubset(C,T));
    assert(L == matrix{{2*g-2,1},{1,-2}}); 
);
///

TEST ///
for g in {10,12} do (
    <<"(g,d,n) = "<<(g,0,-2)<<endl;
    time S = K3(g,0,-2);
    T = S#"surface";
    C = S#"curve";
    L = S#"lattice";
    assert(dim T == 2 and degree T == 2*g-2 and sectionalGenus T == g and dim ambient T == g);
    assert(dim C == 0 and degree C == 1 and isSubset(C,T) and dim tangentSpace(T,C) > 2);
    assert(L == matrix{{2*g-2,0},{0,-2}});       
);
///

TEST ///
K3 20
///

TEST ///
K3 22
///

