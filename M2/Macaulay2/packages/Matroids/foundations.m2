needsPackage "Matroids"

------------------------------------------
-- Pasture definitions
------------------------------------------

Pasture = new Type of HashTable
Pasture.synonym = "pasture"
globalAssignment Pasture
net Pasture := P -> (
    (n, h) := (numgens P.multiplicativeGroup, #P.hexagons);
    (s1, s2) := (if n == 1 then "" else "s", if h == 1 then "" else "s");
    net ofClass class P | " on " | net n | " generator" | s1 | " with " | net h | " hexagon" | s2
)

Foundation = new Type of Pasture
Foundation.synonym = "foundation"
globalAssignment Foundation

Pasture == Pasture := (P1, P2) -> P1.multiplicativeGroup == P2.multiplicativeGroup and P1.epsilon == P2.epsilon and set(P1.hexagons/(h -> set(h/set))) === set(P2.hexagons/(h -> set(h/set)))

PastureMorphism = new Type of Matrix
PastureMorphism.synonym = "pasture morphism"
globalAssignment PastureMorphism
net PastureMorphism := phi -> net phi.map
det PastureMorphism := ZZ => opts -> phi -> det matrix phi.map
matrix PastureMorphism := Matrix => opts -> phi -> phi.map

pastureMorphism = method()
pastureMorphism (Pasture, Pasture, Matrix) := PastureMorphism => (P1, P2, m) -> (
    new PastureMorphism from {
        symbol source => P1,
        symbol target => P2,
        symbol map => map(P2.multiplicativeGroup, P1.multiplicativeGroup, m)
    }
)
pastureMorphism (Pasture, Pasture, List) := List => (P1, P2, L) -> apply(L, m -> pastureMorphism(P1, P2, m))

PastureMorphism * PastureMorphism := (phi1, phi2) -> if phi2.target == phi1.source then pastureMorphism(phi2.source, phi1.target, phi1.map * phi2.map) else error "pastureMorphism: Expected phi1.source to equal phi2.target"

------------------------------------------
-- Pasture constructions
------------------------------------------

pasture = method()
pasture (Matrix, Matrix, List) := Pasture => (A0, eps0, L) -> (
    (A, ch) := myMinPres A0;
    eps := (ch * eps0) % A;
    hexes := if all(L, l -> #l == 3) then L/(h -> h/(p -> p/(e -> ch*e)))
    else if all(L, l -> #l == 2) then hexesFromPairs(A, eps, L/(p -> p/(e -> ch*e)))
    else error "pasture: Expected a list of hexagons or fundamental pairs";
    new Pasture from {
        symbol multiplicativeGroup => coker A,
        symbol epsilon => eps,
        symbol hexagons => hexes,
        cache => new CacheTable
    }
)
pasture (Module, Matrix, List) := Pasture => (M, eps, L) -> pasture(presentation M, eps, L)
pasture GaloisField := Pasture => k -> (
    if k.?pasture then k.pasture else k.pasture = (
        q := k.order;
        if q == 2 then return specificPasture "F2";
        n := q-1;
        g := matrix{{n}};
        x := k.PrimitiveElement;
        eps := if even q then 0 else n//2;
        exps := toList(1..<n);
        multToAdd := hashTable apply(exps, e -> x^e => e);
        hexes := while #exps > 0 list (
            a := exps#0;
            b := multToAdd#(1 - x^a);
            h := {{a,b}, {n-a,(eps-a+b)%n}, {n-b,(eps-b+a)%n}};
            exps = exps - set flatten h;
            h/(p -> p/(e -> matrix{{e}} % g))
        );
        pasture(g, matrix{{eps}}, hexes)
    )
)
pasture (Array, String) := Pasture => (varArray, s) -> ( -- s should be a comma-separated list of (Laurent) monomials or binomials in varArray, which all equal 1 in the pasture
    varList := delete(null, unique toList varArray);
    qPos := positions(varList, v -> match(toString v, "qq"));
    if #qPos > 1 then error "pasture: `qq` cannot be a variable name. Please rewrite variables and relations.";
    if #qPos > 0 then varList = varList_(qPos | delete(qPos#0, toList(0..<#varList)));
    n := #varList;
    eps := ZZ^(n+1)_{0};
    qq := "(ZZ^" | toString(n+1) | ")";
    for i to n-1 do s = replace(toString varList#i, "(qq_{" | toString(i+1) | "})", s);
    G := 2*eps;
    rels := if s == "" then {} else for r in separate(",", replace("[ ]+", "", s)) list (
	sepPos := position(toList(1..<#r), i -> (not r#(i-1) == "(") and (r#i == "-" or r#i == "+"));
	t := if sepPos === null then {r} else {substring(0, sepPos+1, r), (if r#(sepPos+1) == "-" then "-" else "") | substring(sepPos+2, r)};
	t = apply(t, m -> replace("(?<!\\^\\()-", "qq_{0}*", m));
	t = apply(t, m -> if m == "1" then "(qq_{0})^0" else if m == "qq_{0}*1" then "qq_{0}*0" else m);
        v := apply(t, m -> value replace("qq", qq, replace("[\\^]", "*", replace("[\\*]", "+", m))));
        if #v == 2 then v else ( if #v == 1 then G = G | v#0; continue; )
    );
    pasture(G, eps, rels)
)

specificPasture = method()
specificPasture String := Pasture => name -> (
    id0 := id_(ZZ^0);
    id1 := id_(ZZ^1);
    z01 := map(ZZ^0, ZZ^1, 0);
    n0 := toUpper first characters name;
    if name === "F1pm" then pasture(2*id1, id1, {})
    else if name === "F2" then pasture(id0, z01, {})
    else if name === "F3" then pasture(2*id1, id1, {{id1, id1}})
    else if n0 === "D" then pasture([getSymbol "x"], "x-1")
    else if n0 === "G" then pasture([getSymbol "x"], "x+x^2")
    else if n0 === "H" then pasture([getSymbol "x"], "-x^3, x+x^(-1)")
    else if n0 === "I" then pasture(["z","t"]/getSymbol, "z + z^2, -z^2 + t, z^4 + z*t")
    else if n0 === "K" then pasture(id0, z01, {{z01, z01}})
    else if n0 === "S" then pasture(2*id1, id1, {{0*id1, 0*id1}})
    else if n0 === "U" then pasture(["x","y"]/getSymbol, "x+y")
    else if n0 === "V" then pasture(["x","y","z","s","t"]/getSymbol, "x*y*s*t+y*z*s,s*t+z*s,t+y^(-1),s^(-1)+x,y*z+x*y")
    else error "specificPasture: Expected name to be one of: D, F1pm, F2, F3, G, H, I, K, S, U, V"
)
specificPasture Symbol := Pasture => s -> specificPasture toString s

TEST ///
assert areIsomorphic(pasture([], ""), specificPasture "F1pm")
assert areIsomorphic(pasture([], "-1,-1-1"), specificPasture "krasner")
assert areIsomorphic(pasture([], "-1"), pasture GF 2)
assert areIsomorphic(pasture([], "0+0"), specificPasture "sign")
assert areIsomorphic(foundation specificMatroid nonfano, specificPasture D)
assert areIsomorphic(foundation specificMatroid fano, specificPasture F2)
assert areIsomorphic(foundation specificMatroid T8, specificPasture F3)
assert areIsomorphic(foundation specificMatroid betsyRoss, specificPasture G)
assert areIsomorphic(foundation affineGeometry(2,3), specificPasture H)
assert areIsomorphic(foundation uniformMatroid_2 4, specificPasture U)
assert areIsomorphic(foundation uniformMatroid_2 5, specificPasture V)
V5 = pasture(matrix{{4},{0}}, matrix{{2},{0}}, {{matrix{{1},{0}}, matrix{{0},{1}}}, {matrix{{3},{0}}, matrix{{1},{1}}},{matrix{{2},{0}}, matrix{{1},{2}}}})
assert areIsomorphic(V5, pasture([x,i], "-i^2,x-i,-1-i*x^2"))
///

------------------------------------------
-- General pasture helper functions
------------------------------------------

hexesFromPairs = method()
hexesFromPairs (Matrix, Matrix, List) := List => (A, eps, L) -> (
    hexTable := new MutableHashTable;
    for p in L list (
        fp := {p#0 % A, p#1 % A};
        if hexTable#?(set fp) then continue;
        h := {fp, {(-fp#1) % A, (eps + fp#0 - fp#1) % A}, {(-fp#0) % A, (eps - fp#0 + fp#1) % A}};
        scan(h, pair -> hexTable#(set pair) = 1);
        h
    )
)

fundEltPartners = method()
fundEltPartners (List, Thing) := List => (L, A) -> (
    unique for p in L list if p#0 === A then p#1 else if p#1 === A then p#0 else continue
)
fundEltPartners (Pasture, Thing) := List => (P, e) -> (
    if not P.cache#?"partnerTable" then P.cache#"partnerTable" = (
        FP := unique flatten P.hexagons;
        FE := unique flatten FP;
        hashTable apply(FE, e -> e => fundEltPartners(FP, e))
    );
    if P.cache#"partnerTable"#?e then P.cache#"partnerTable"#e else {}
)

isSlim = method()
isSlim Pasture := Boolean => P -> (
    if not P.cache#?"partnerTable" then fundEltPartners(P, null);
    unique apply(values P.cache#"partnerTable", v -> #v) === {1}
)

freePartPasture = method()
freePartPasture Pasture := List => P -> (
    A := presentation P.multiplicativeGroup;
    positions(toList(0..<numrows A), i -> A^{i} == 0)
)

hexTypes = method()
hexTypes (Pasture, Boolean) := List => (P, doTally) -> (
    -- A := presentation P.multiplicativeGroup;
    F3 := select(P.hexagons, h -> #unique flatten h == 1);
    D := select(P.hexagons - set F3, h -> any(h, p -> #unique p == 1));
    -- H := select(P.hexagons - set F3 - set D, h -> (p := unique flatten h; #p == 2 and all(p, e -> 3*e % A == P.epsilon)));
    H := select(P.hexagons - set D, h -> #unique flatten h == 2);
    U := P.hexagons - set F3 - set D - set H;
    if doTally then hashTable {("U", #U), ("D", #D), ("H", #H), ("F3", #F3)} else {U, D, H, F3}
)
hexTypes Pasture := HashTable => P -> hexTypes(P, true)

pairTypes = method()
pairTypes Pasture := HashTable => P -> (
    L := fullRankSublattice P;
    (n1, n2, n3) := (0, 0, 0);
    for p in L do (
        r := p#0;
        if r#2 > r#1 then n3 = n3 + 1 else if r#1 > r#0 then n1 = n1 + 1 else n2 = n2 + 1
    );
    hashTable {("type 1", n1), ("type 2", n2), ("type 3", n3), ("type 4", #flatten P.cache#"type4Data")}
)

savePasture = method()
savePasture (Pasture, String) := String => (P, fileName) -> (openOut fileName) << writeToString P << close
savePasture Pasture := String => P -> savePasture(P, temporaryFileName())

saveFoundation = method()
saveFoundation (Matroid, String) := String => (M, fileName) -> savePasture(foundation M, fileName)
saveFoundation Matroid := String => M -> saveFoundation(M, temporaryFileName())

readFoundation = method()
readFoundation (Matroid, String) := Foundation => (M, file) -> (
    M.cache#"foundation" = value get file;
    M.cache#"foundation".cache#"matroid" = M;
    M.cache#"foundation"
)

------------------------------------------
-- General abelian group functions
------------------------------------------

myMinPres = method()
myMinPres Matrix := Sequence => A -> (
    recursionLimitStore := recursionLimit;
    recursionLimit = max(2*numcols A, recursionLimit);
    -- customized code from minimalPresentation(Module, Strategy => 2)
    (g,ch) := smithNormalForm(A, ChangeMatrix => {true, false}, KeepZeroes => true);
    recursionLimit = recursionLimitStore;
    (D,S,T) := snf g;
    (g,ch) = (D, S*ch);
    piv := select(pivots g, ij -> abs g_ij === 1);
    (rows, cols) := (piv/first, piv/last);
    (submatrix'(g,rows,cols),submatrix'(ch,rows,))
)
myMinPres Module := Sequence => M -> myMinPres presentation M

snf = method()
snf Matrix := Matrix => X -> ( -- X should be diagonal over ZZ
    X0 := mutableMatrix X;
    (rows, cols) := (numrows X, numcols X);
    n := min(rows, cols);
    SNFIterate := (A,S,T) -> ( -- A is mutable
        for i to n - 2 do (
            (a,b) := (A_(i,i), A_(i+1,i+1));
            c := gcd(a,b);
            if a*b === 0 or a === c then continue;
            A_(i,i) = c;
            A_(i+1,i+1) = a*(b//c);
            (sigma, tau) := toSequence flatten entries(c // matrix{{a,b}});
            rho := (b//c)*tau;
            P := diagonalMatrix toList(i:1);
            S = S*(P ++ matrix{{1-rho, -1},{rho, 1}} ++ diagonalMatrix toList(rows-i-2:1));
            T = (P ++ matrix{{a//c, b//c},{-tau, sigma}} ++ diagonalMatrix toList(cols-i-2:1))*T;
        );
        (A, S, T)
    );
    output := ultimate(SNFIterate, (X0, id_(ZZ^rows), id_(ZZ^cols)));
    (matrix output#0, inverse output#1, inverse output#2)
)

TEST ///
A = diagonalMatrix {4,6,15} ++ map(ZZ^3,ZZ^5,0) -- -> {2,12,15} -> {2,3,60} -> {1,6,60}
(D, S, T) = snf A
assert(D == S*A*T)
///

liftTorsion = method()
liftTorsion (Matrix, Matrix, Module, List) := Matrix => (A, B, G, K) -> ( 
    -- finds all solutions of X*A = B over a finite abelian group G, given all solutions K to X*A = 0
    -- assumes G is given by a minimal presentation (i.e. coker of diagonal matrix with nonzero diagonal entries)
    -- numrows B should == numgens G
    torsCand := matrix for i to (numcols mingens G) - 1 list (
        n := (presentation G)_(i,i);
        R := ZZ[]/n;
        (q, r) := quotientRemainder'(sub(B^{i}, R), sub(A, R));
        if r != 0 then return false;
        {sub(q, ZZ)}
    );
    apply(K, psi -> psi + torsCand)
)

abelianGroupHom = method()
abelianGroupHom (Module, Module) := List => (G1, G2) -> (
    -- assumes G1, G2 are finite abelian groups
    -- H := minPres Hom(G1, G2);
    homSet := Hom(G1, G2);
    (H, proj) := myMinPres homSet;
    (H, proj) = (coker H, map(homSet, coker H, proj));
    if H == 0 then return {map(ZZ^(numgens G2), ZZ^(numgens G1), 0)};
    ords := apply(numgens H, i -> (presentation H)_(i,i));
    homElts := apply(toList(fold(apply(ords, a -> set(0..<a)), (a,b) -> a**b)), s -> transpose matrix {{deepSplice s}});
    -- homElts/(f -> matrix homomorphism(H.cache.pruningMap * map(H, ZZ^1, f)))
    homElts/(f -> matrix homomorphism(proj * map(H, ZZ^1, f)))
)

TEST ///
G1 = coker diagonalMatrix{2,6}
G2 = coker diagonalMatrix{2}
G3 = coker diagonalMatrix{5}
assert(#abelianGroupHom(G1,G1) == 48)
assert(#abelianGroupHom(G1,G2 ++ G3) == 4)
assert(#abelianGroupHom(G1,G3) == 1)

G1 = cokernel matrix {{306, 162}, {432, 216}}
G2 = cokernel matrix {{24, 72}, {12, 140}}
assert(#abelianGroupHom(G1, G2) == 1152)
///

------------------------------------------
-- (Fiber) products/coproducts
------------------------------------------

fiberProduct = method()
fiberProduct (Matrix, Matrix) := Module => (f1, f2) -> (
    if f1.target =!= f2.target then error "fiberProduct: Expected same target module";
    I := mingens intersect(image f1, image f2);
    G := map(target f1, source I, I);
    (K1, K2) := (f1, f2)/ker/mingens;
    subquotient(matrix(((G // f1) || (G // f2)) | (K1 ++ K2)), relations(source f1 ++ source f2))
)
fiberProduct (PastureMorphism, PastureMorphism) := Pasture => (f1, f2) -> (
    if f1.target =!= f2.target then error "fiberProduct: Expected same target pasture";
    (P1, P2) := (f1.source, f2.source);
    (A1, A2) := (f1.map, f2.map)/matrix;
    (G1, G2) := (P1.multiplicativeGroup, P2.multiplicativeGroup);
    (hex1, hex2) := (P1.hexagons, P2.hexagons);
    (i1, i2) := (id_G1 || map(ZZ^(numgens G2), ZZ^(numgens G1), 0), map(ZZ^(numgens G1), ZZ^(numgens G2), 0) || id_G2)/matrix;
    G := fiberProduct(f1.map, f2.map);
    T := hashTable(splice, for p in P1.hexagons/first list (set{A1*p#0, A1*p#1}, p));
    validPairs := apply(flatten for p in flatten P2.hexagons list (
    	im := set{A2*p#0, A2*p#1};
	if not T#?im then continue;
	P1preimages := if instance(T#im, Sequence) then toList T#im else {T#im};
	flatten apply(P1preimages, p1 -> delete(null, {
	    if A1*p1#0 == A2*p#0 then {i1 * p1#0 + i2 * p#0, i1 * p1#1 + i2 * p#1},
	    if A1*p1#1 == A2*p#0 then {i1 * p1#1 + i2 * p#0, i1 * p1#0 + i2 * p#1}
	}))
    ), pair -> {pair#0 // gens G, pair#1 // gens G});
    eps := (i1 * P1.epsilon + i2 * P2.epsilon) // gens G;
    pasture(G, eps, validPairs)
)

product (Pasture, Pasture) := Pasture => (P1, P2) -> (
    K := specificPasture "krasner";
    fiberProduct(first morphisms(P1, K), first morphisms(P2, K))
)
Pasture * Pasture := (P1, P2) -> product(P1, P2)

TEST /// -- Fiber product of abelian groups
Z6 = coker matrix{{6}}
f1 = map(Z6, ZZ^1, matrix{{2}})
assert Equation(fiberProduct(f1, f1), image matrix {{1,3,0},{1,0,3}})
f2 = 3*id_(Z6)
assert Equation(fiberProduct(f1, f2), subquotient(matrix {{3,0},{0,2}},matrix {{0},{6}}))
///

TEST /// -- Fiber product of pastures
assert areIsomorphic((pasture GF 2) * (pasture GF 3), specificPasture "F1pm")
G = pasture([x], "x + x^2")
f1 = first morphisms(G, G)
FP = fiberProduct(f1, f1)
assert areIsomorphic(G, FP)
f2 = first morphisms(G, specificPasture "krasner")
FP2 = fiberProduct(f2, f2)
assert(#freePartPasture FP2 == 2 and #FP2.hexagons == 6)
///

fiberCoproduct = method()
fiberCoproduct (Matrix, Matrix) := Module => (f1, f2) -> (
    if f1.source =!= f2.source then error "fiberCoproduct: Expected same source module";
    (A1, A2) := (f1, f2)/matrix;
    sourceGens := mingens f1.source;
    if numcols sourceGens == 0 then return (coker f1) ++ (coker f2);
    rels := matrix{apply(numcols sourceGens, i -> A1 * sourceGens_{i} || -A2 * sourceGens_{i})};
    coker(rels | (relations f1.target ++ relations f2.target))
)
fiberCoproduct (PastureMorphism, PastureMorphism) := Pasture => (f1, f2) -> (
    if f1.source =!= f2.source then error "fiberCoproduct: Expected same source pasture";
    (A1, A2) := (f1.map, f2.map)/matrix;
    (P1, P2) := (f1.target, f2.target);
    (G1, G2) := (P1.multiplicativeGroup, P2.multiplicativeGroup);
    (eps1, eps2) := (P1.epsilon, P2.epsilon);
    (hex1, hex2) := (P1.hexagons, P2.hexagons);
    (i1, i2) := (id_G1 || map(ZZ^(numgens G2), ZZ^(numgens G1), 0), map(ZZ^(numgens G1), ZZ^(numgens G2), 0) || id_G2)/matrix;
    G := fiberCoproduct(f1.map, f2.map);
    validPairs := apply(hex1/first, p -> {i1*p#0, i1*p#1}) | apply(hex2/first, p -> {i2*p#0, i2*p#1});
    eps := (i1 * P1.epsilon);
    pasture(G, eps, validPairs)
)

coproduct = method()
coproduct (Pasture, Pasture) := Pasture => (P1, P2) -> (
    F1pm := specificPasture "F1pm";
    fiberCoproduct(first morphisms(F1pm, P1), first morphisms(F1pm, P2))
)
Pasture ** Pasture := (P1, P2) -> coproduct(P1, P2)

TEST /// -- Coproduct of pastures
assert areIsomorphic((pasture GF 2) ** (pasture GF 3), specificPasture "krasner")
U24 = uniformMatroid(2, 4)
U = foundation U24
assert areIsomorphic(U ** U, foundation(U24 ++ U24))
///

------------------------------------------
-- Foundations
------------------------------------------

minTopLeft := A -> A_({{0,1,2,3},{1,0,3,2},{2,3,0,1},{3,2,1,0}}#(minPosition A))
-- the output of getPerm is {a, b}, which means sigma^a * rho^b where sigma = (13) and rho = (123)
getPerm := (A, f) -> (
    B := minTopLeft apply(A, f);
    p1 := if B_1 < B_2 then (-1, 1) else (1, -1);
    p2 := if B_2 < B_3 then (-1, 1) else (1, -1);
    p3 := if B_3 < B_1 then (-1, 1) else (1, -1);
    b := {{p1#0, p3#1}, {p2#0, p1#1}, {p3#0, p2#1}};
    i := position(b/product, x -> x == -1);
    if b#i#0 == -1 then {0, ((i+2) % 3)} else {1, (2*(i+2) % 3)}
)
H4acoeffs := (p1, p2)-> ( -- the order is (eps,xh4,yh4,xh5,yp are permutation of xh4,xh5 
    (u,v) := ((p1#0+p2#0) % 2, (2*(p1#0+1)*p1#1*(p2#0+1)+p2#1) % 3);
    transpose matrix if u == 0 then(
        if v == 0 then {{0,1,0,-1,0},{0,0,1,0,-1}}
        else if v == 1 then {{0,1,0,0,1},{1,0,-1,1,-1}}
        else {{1,-1,0,-1,1},{0,0,1,1,0}}
    ) else (
        if v == 0 then {{0,1,0,0,-1},{0,0,1,-1,0}}
        else if v == 1 then {{1,-1,0,1,-1},{0,0,1,0,1}}
        else {{0,1,0,1,0},{1,0,-1,-1,1}}
    )                  
)
H4bcoeffs := (p1,p2,p3)->( -- permutation of y_i,x_{i-1},x_{i+1}
    w3 := if p1#0 == 0 then (
        if p1#1 == 0 then (0,1,0,0,0,0,0)
        else if p1#1 == 1 then (0,0,-1,0,0,0,0)
        else (1,-1,1,0,0,0,0)
    ) else (
        if p1#1 == 0 then (0,0,1,0,0,0,0)
        else if p1#1 == 1 then (1,1,-1,0,0,0,0)
        else (0,-1,0,0,0,0,0)
    );
    w1 := if p2#0 == 0 then (
        if p2#1 == 0 then (0,0,0,-1,0,0,0)
        else if p2#1 == 1 then (0,0,0,0,1,0,0)
        else (1,0,0,1,-1,0,0)
    ) else (
        if p2#1 == 0 then (0,0,0,0,-1,0,0)
        else if p2#1 == 1 then (1,0,0,-1,1,0,0)
        else (0,0,0,1,0,0,0)
    );
    w2 := if p3#0 == 0 then (
        if p3#1 == 0 then (0,0,0,0,0,-1,0)
        else if p3#1 == 1 then (0,0,0,0,0,0,1) 
        else (1,0,0,0,0,1,-1)
    ) else (
        if p3#1 == 0 then (0,0,0,0,0,0,-1)
        else if p3#1 == 1 then (1,0,0,0,0,-1,1)
        else (0,0,0,0,0,1,0)
    );
    {(w3#0+w1#0+w2#0) % 2} | drop(toList w1 + toList w2 + toList w3,1)
)
h3 := (i, m) -> m_(delete(i-1,#m))
chooseHyp := (L,f,g) -> L#(position(L,h -> isSubset (f+g,h)))
containmentTable := (LF,LH) -> hashTable apply(LH, h -> set select(LF, f -> isSubset (f,h)) => h)

foundation = method(Options => {Strategy => null, HasF7Minor => null, HasF7dualMinor => null})
foundation Matroid := Foundation => opts -> M -> (
    if M.cache#?"foundation" and (opts.Strategy === null or M.cache#"foundation".cache#"strategy" === opts.Strategy) then M.cache#"foundation" else M.cache#"foundation" = (
    dbg := debugLevel;
    debugLevel = 0;
    r := rank M;
    cacheList := {};
    strat := toLower toString if opts.Strategy === null then "bases" else opts.Strategy;
    if dbg > 0 then << "foundation: Using strategy " << strat << endl;
    if strat === "hyperplanes" then (
        hypMap := hashTable apply(#hyperplanes M, i -> (hyperplanes M)#i => i);
        if dbg > 5 then print("foundation: hypMap: " | net hypMap);
        indexOfHyp := h -> hypMap#h;
        corank2flats := select(flats(M, 2, "corank"), F -> rank_M F == r - 2);
        corank3flats := select(flats(M, 3, "corank"), F -> rank_M F == r - 3);
	-- IS := independentSets(M, r-3);
	-- corank23flats := (if #IS == 0 then flats(M, 3, "corank") else select(flats(M, 3, "corank"), F -> any(IS, s -> isSubset(s, F)))) - set hyperplanes M;
	-- H23 := new MutableHashTable from apply(corank23flats, F -> (F,0));
	-- H3 := new MutableHashTable from {};
	-- scan(keys H23, F -> (
	--     pos := position(keys H23, k -> isSubset(F,k) and F =!= k);
	--     if pos =!= null then ( H3#F = 1; remove(H23, F); );
	-- ));
	-- corank3flats := keys H3;
	-- corank2flats := corank23flats - set corank3flats;
        if dbg > 0 then << "foundation: Finding upper U(2,4) minors..." << flush;
        U24minors := flatten apply(corank2flats, F -> subsets(select(hyperplanes M, H -> isSubset(F, H)), 4));
        if dbg > 0 then << #U24minors << " found." << endl;
        if dbg > 0 then << "foundation: Finding upper U(2,5) minors..." << flush;
        U25minors := flatten apply(corank2flats, F -> subsets(select(hyperplanes M, H -> isSubset(F, H)), 5));
        if dbg > 0 then << #U25minors << " found." << endl;
        if dbg > 0 then << "foundation: Finding rank 3 minors (C5, U(3,5), Fano)..." << flush;
        rk3minors := 0;
        F7 := specificMatroid "fano";
        (F7Known, F7dualKnown) := (opts.HasF7Minor =!= null, opts.HasF7dualMinor =!= null);
        (hasF7, hasF7dual) := (if F7Known then opts.HasF7Minor else false, if F7dualKnown then opts.HasF7dualMinor else false);
        H4hyps := flatten for F in corank3flats list (
            cork2F := select(corank2flats, F2 -> isSubset(F, F2));
            if #cork2F < 5 then continue;
            hypsF := select(hyperplanes M, H -> isSubset(F, H));
            if not F7Known and not hasF7 and min(#hypsF, #cork2F) >= 7 then (
                rk3minors = rk3minors + 1;
                hasF7 = hasMinor(M/F, F7);
            );
            if #hypsF < 8 then continue;
            for S in subsets(cork2F, 5) list (
                T := unique apply(subsets(S, 2)/toSequence, chooseHyp_hypsF);
                if member(#T, {8,10}) then ( rk3minors = rk3minors + 1; (S, T) ) else continue
            )
        );
        if dbg > 0 then << rk3minors << " found." << endl;
        if not F7dualKnown and not hasF7 then (
            if dbg > 0 then print "foundation: Detecting dual Fano minor...";
            hasF7dual = hasMinor(M, dual F7);
        );
        if dbg > 0 then print "foundation: All minors found. Finding relations...";
        H4aminors := select(H4hyps, p -> #last p == 8);
        H4bminors := select(H4hyps, p -> #last p == 10);
        u := #U24minors;
        G := ZZ^(2*u+1);
        eps := matrix G_{0};
        Hminus := (if hasF7 or hasF7dual then 1 else 2)*eps;
        twistedRatios := apply(U24minors, S -> {S#0, S#3, S#2, S#1});
        genRatios := U24minors | twistedRatios;
        genTable := hashTable apply(#U24minors, i -> set genRatios#i => i);
        if dbg > 5 then print("foundation: genTable: " | net genTable);
        hashMap := (D,i) -> matrix G_{1 + genTable#(set D) + (if i == 0 then 0 else u)};
        H3rels := flatten apply(U25minors/(m -> sort(m, indexOfHyp)), m ->
            {hashMap(h3_5 m,0) + hashMap(h3_3 m,0) - hashMap(h3_4 m,0),
            hashMap(h3_5 m,0) + hashMap(h3_2 m,1) - hashMap(h3_1 m,1),
            hashMap(h3_1 m,0) + hashMap(h3_3 m,0) - hashMap(h3_2 m,0),
            hashMap(h3_2 m,1) + hashMap(h3_4 m,1) - hashMap(h3_3 m,1),
            hashMap(h3_4 m,1) + hashMap(h3_1 m,0) - hashMap(h3_5 m,1)});
        if dbg > 0 then print "foundation: Finding H4a relations...";
        H4arels := if #H4aminors == 0 then map(ZZ^(2*u+1),ZZ^0,0) else fold(apply(H4aminors, m -> (
            h := (last m)#(position(last m, x -> #select(first m, f -> isSubset(f,x)) == 3));
            (f4,f5) := toSequence select(first m, f -> not isSubset (f,h));
            (f1,f2,f3) := toSequence (first m - set (f4,f5));
            d1 := {chooseHyp(last m,f4, f1),chooseHyp(last m,f4, f2),chooseHyp(last m, f4, f3), chooseHyp(last m, f4, f5)};
            d2 := {chooseHyp(last m,f5, f1),chooseHyp(last m,f5, f2),chooseHyp(last m, f5, f3), chooseHyp(last m, f4, f5)};
            fold({eps,hashMap(d1,0),hashMap(d1,1),hashMap(d2,0),hashMap(d2,1)},(a,b) -> a|b)*H4acoeffs(getPerm(d1, indexOfHyp), getPerm(d2, indexOfHyp))
            )), (a,b) -> a|b);
        if dbg > 5 then print("foundation: H4arels: " | toString H4arels);
        if dbg > 0 then print "foundation: Finding H4b relations...";
        H4brels := if #H4bminors == 0 then map(ZZ^(2*u+1),ZZ^0,0) else fold(flatten apply(H4bminors, p -> (
            T := containmentTable p;
            Hats := apply(5, i -> (apply(4, j -> T#(set{p#0#i,p#0#((i+1+j) %5)})), apply({0,3,2,1}, j-> T#(set{p#0#i,p#0#((i+1+j) %5)}))));
            coeffs := apply(5, i -> H4bcoeffs(getPerm(Hats#i#1, indexOfHyp), getPerm(Hats#((i-1)%5)#0, indexOfHyp), getPerm(Hats#((i+1)%5)#0, indexOfHyp)));
            if dbg > 5 then print("foundation: coeffs: " | net coeffs | ", hats:" | net Hats);
            apply(5, i -> (
                cols := {eps,hashMap(Hats#i#1,0), hashMap(Hats#i#1,1), hashMap(Hats#(i-1)#0,0), hashMap(Hats#(i-1)#1,1), hashMap(Hats#((i+1)%5)#0,0), hashMap(Hats#((i+1)%5)#1,1)};
                sum(#cols, j -> coeffs#i#j*cols#j)
            )))), (a,b) -> a|b); 
        H := matrix {{Hminus} | H3rels} | H4arels | H4brels;
        if dbg > 2 then print("foundation: Presentation matrix H: " | toString H);
        if dbg > 0 then print("foundation: All relations found. Computing Smith normal form for " | toString numrows H | " x " | toString numcols H | " matrix...");
        (g, ch) := myMinPres H;
        eps = ch_{0} % g;
        hexes := hexesFromPairs(g, eps, apply(u, i -> {ch_{i+1}, ch_{i+u+1}}));
        cacheList = {("pruningMapH", ch), ("genTableH", genTable)};
    ) else (
        G = ZZ^(#bases M + 1);
        eps = matrix G_{0};
        signPerm := s -> if det ((id_(ZZ^(#M_*)))_s)^(sort s) == 1 then 0 else 1;
        basesMap := hashTable apply(#bases M, i -> (bases M)#i => i+1);
        if dbg > 0 then << "foundation: #bases: " << #basesMap << ". Finding trivial cross ratios..." << endl;
        maxRankNonbases := select(nonbases M, N -> rank_M N == rank M - 1)/toList;
        if dbg > 0 then << "foundation: #trivial cross ratios: " << #maxRankNonbases << ". Computing relations..." << endl;
        trivialCrossRatios := matrix{flatten for N in maxRankNonbases list (
            C := toList fundamentalCircuit(M, set N, N#0);
            D := toList fundamentalCircuit(dual M, M.groundSet - N, last toList(M.groundSet - N));
            flatten table(#C-1, #D-1, (i,j) -> (
                I := N - set{C#-1, C#i};
                crossRatioIndices := {I|{C#-1,D#-1}, I|{C#i,D#j}, I|{C#-1,D#j}, I|{C#i,D#-1}};
                sum(crossRatioIndices/signPerm)*eps + matrix sum apply(crossRatioIndices_{0,1}, b -> G_(basesMap#(set b))) - matrix sum apply(crossRatioIndices_{2,3}, b -> G_(basesMap#(set b)))
            ))
        )};
        if trivialCrossRatios == 0 then trivialCrossRatios = map(ZZ^(#bases M+1),ZZ^0,0);
        if dbg > 0 then << "foundation: " << numcols trivialCrossRatios << " relations found." << endl;
        B := sort toList first bases M;
        onePos := coordinatingPath(M, Outputs => "noBasis");
        if dbg > 0 then << "foundation: Spanning forest: " << onePos << endl;
        imDegMap := matrix({G_1} | apply(onePos, p -> G_(basesMap#(set B + set p - (set B*set p))))); -- corank of image of degree map = #connectedComponents BG - 1
        (g, ch) = myMinPres (2*eps | trivialCrossRatios | imDegMap);
        eps = ch_{0} % g;
        if dbg > 0 then << "foundation: Finding upper U24 minors..." << flush;
        IS := independentSets(M, r - 2);
        corank2Table := hashTable delete(null, apply(flats(M, 2, "corank") - set hyperplanes M, F -> (
            p := position(IS, I -> isSubset(I, F));
            if p =!= null then (F, IS#p)
        )));
        U24minors = flatten apply((sort((keys corank2Table)/keys/sort))/set, F -> (
            apply(subsets(apply(select(hyperplanes M, H -> isSubset(F, H)), H -> first toList(H - F)), 4), s -> {corank2Table#F, sort s})
        ));
        u = #U24minors;
        if dbg > 0 then << u << " found. " << endl << "foundation: Generating hexagons..." << endl;
        hexes = hexesFromPairs(g, eps, apply(U24minors, p -> (
            l := {set{p#1#0, p#1#1}, set{p#1#2, p#1#3}, set{p#1#0, p#1#2}, set{p#1#1, p#1#3}, set{p#1#0, p#1#3}, set{p#1#1, p#1#2}};
            l = apply(l, q -> ch_(basesMap#(p#0 + q)));
            {l#0 + l#1 - l#2 - l#3, l#4 + l#5 - l#2 - l#3}/matrix
        )));
        cacheList = {("pruningMapB", ch), ("genTableB", basesMap), ("imDegMap", imDegMap)};
    );
    debugLevel = dbg;
    new Foundation from {
        symbol multiplicativeGroup => coker g,
        symbol epsilon => eps,
        symbol hexagons => hexes,
        cache => new CacheTable from (cacheList | {"numU24minors" => u, "strategy" => strat, "matroid" => M})
    }
    )
)

TEST /// -- rank 2 uniform matroids (k-regular partial fields)
U = foundation uniformMatroid(2,4)
assert Equation((#freePartPasture U, #U.hexagons), (2, 1))
V = foundation uniformMatroid(2,5)
assert Equation((#freePartPasture V, #V.hexagons), (5, 5))
U26 = foundation uniformMatroid(2,6)
assert Equation((#freePartPasture U26, #U26.hexagons), (9, 15))
///

------------------------------------------
-- Old morphisms code
------------------------------------------

changeToBase = (b, n) -> (
    if n < b then return {(0, n)};
    k := floor(log_b n);
    a := floor(n/b^k);
    {(k, a)} | changeToBase(b, n - a*b^k)
)

compareHex = method()
compareHex (List, List) := Boolean => (fp, hex) -> any(hex, p -> fp == p or fp == reverse p) -- fp and hex should be in normal form

subTorsion = method()
subTorsion (Matrix, Module) := Matrix => (X, G) -> ( 
    -- attempts to substitute a matrix over QQ into a finite abelian group G (column by column)
    -- assumes G is given by a minimal presentation
    -- number of rows of X should equal numgens G
    matrix for i to (numcols mingens G) - 1 list (
        n := (presentation G)_(i,i);
        for j to (numcols X) - 1 list (
            x := X_(i,j);
            (a, b) := (numerator x, denominator x);
            if gcd(b, n) != 1 then return false;
            for k to b-1 do if (a+n*k)%b == 0 then break ((a+n*k)//b) % n
        )
    )
)

TEST ///
X1 = matrix {{1/3},{1/5}}
X2 = matrix {{1/3},{3/2}}
X3 = X1 | X1
G = coker diagonalMatrix{2,6}
assert(subTorsion(X1,G) == matrix{{1},{5}})
assert(subTorsion(X2,G) == false)
assert(subTorsion(X3,G) == matrix{{1,1},{5,5}})
assert(subTorsion(matrix{{-1/5}}, coker matrix{{2}}) == matrix{{1}})
///

fullRankSublattice1 = method(Options => {Order => 2, Shuffle => false})
fullRankSublattice1 Pasture := List => opts -> P -> (
    if P.cache#?"sublattice1" then P.cache#"sublattice1" else P.cache#"sublattice1" = (
        rowIndices := freePartPasture P;
        perm := if opts.Shuffle then random else reverse;
        projections := perm apply(P.hexagons/first, pair -> {pair#0^rowIndices, pair#1^rowIndices}); -- first two pairs seem not in general position..
        r := #rowIndices;
        s := rank (matrix{flatten flatten P.hexagons})^rowIndices;
        L := ZZ^r;
        goodPairs := {};
        for pair in projections do (
            Q := L/image matrix{pair};
            if rank Q == rank L - 2 then ( L = Q; goodPairs = append(goodPairs, pair); );
            if rank L <= r - s + 1 then break;
        );
        if debugLevel > 5 then print("fullRankSublattice1: " | net(rank L, goodPairs)); 
        if rank L == r - s then return {goodPairs, {}};
        if rank L > r - s + 2 then error "fullRankSublattice1: Could not find enough good fundamental pairs. Try again with `Shuffle => true'"; -- need rank L <= 2.
        goodMatrix := matrix {flatten goodPairs};
        allExtraCols := flatten for S in subsets(projections - set goodPairs, rank L) list (
            E := matrix {flatten S};
            Q := L/image E;
            extraCols := {};
            if rank Q == r - s then (
                extraCols = for t in subsets(numcols E, rank L) list (
                    D := det(goodMatrix | E_t);
                    if abs D == 1 then break {(apply(t, i -> E_{i}), D)};
                    (apply(t, i -> E_{i}), D)
                );
            );
            extraCols
        );
        minPair := (-1, infinity);
        for p in allExtraCols do (
            if gcd(p#1, opts.Order) == 1 then (minPair = p; break);
            if p#1 < minPair#1 then minPair = p;
        );
        {goodPairs, minPair#0}
    )
)

morphisms1 = method(Options => {FindOne => false})
morphisms1 (Pasture, Pasture) := List => opts -> (P, P') -> (
    Pstar := presentation P.multiplicativeGroup;
    P'star := presentation P'.multiplicativeGroup;
    freePart := freePartPasture P;
    freePart' := freePartPasture P';
    torsPart := toList(0..<numrows Pstar) - set freePart;
    torsPart' := toList(0..<numrows P'star) - set freePart';
    fundPairsP := flatten P.hexagons;
    fundEltsP := unique flatten fundPairsP;
    fundPairsP' := flatten P'.hexagons;
    fundEltsP' := unique flatten fundPairsP';
    T := coker(Pstar^torsPart);
    T' := coker(P'star^torsPart');
    eta := map(ZZ^(numgens T'), ZZ^(#freePart), 0);
    eta' := map(ZZ^(#freePart'), ZZ^1, 0);
    H := select(abelianGroupHom(T, T'), f -> ((((f | eta) * P.epsilon) || eta') - P'.epsilon) % P'star == 0);
    if debugLevel > 0 then print("morphisms1: number of possible phi is: " | net(#H));
    G := fullRankSublattice1(P);
    latticeGens1 := apply(G#0, p -> fundPairsP_(position(fundPairsP, q -> {q#0^freePart, q#1^freePart} == p)));
    latticeGens2 := apply(G#1, g -> fundEltsP_(position(fundEltsP, q -> q^freePart == g)));
    otherHexes := select(P.hexagons, hex -> (all(latticeGens1, p -> not compareHex(p/(e -> e % Pstar), hex))));
    otherPairs := otherHexes/first;
    if debugLevel > 0 then print("morphisms1: number of other pairs is: " | net (#otherPairs));
    A := matrix {(flatten latticeGens1) | latticeGens2};
    B := inverse sub(A^freePart, QQ); -- inverse sub(matrix {(flatten G#0) | G#1}, QQ);
    FmodG := minPres coker(A^freePart);
    if debugLevel > 0 then print("morphisms1: Quotient lattice is: "| net FmodG);
    K := apply(abelianGroupHom(FmodG, T'), f -> f * transpose matrix(FmodG.cache.pruningMap));
    g := #G#0 + #G#1;
    if debugLevel > 0 then print("morphisms1: Trying " | net(#H * (#fundEltsP')^g) | " candidate morphisms...");
    unique flatten for phi in H list (
        phi' := phi || map(ZZ^(#freePart'), ZZ^(#torsPart), 0);
        D := phi' * A^torsPart;
        N := (#fundEltsP')^g - 1;
        flatten while N >= 0 list (		        
            s := hashTable changeToBase(#fundEltsP', N);
            s = apply(g, i -> if s#?(g - 1 - i) then s#(g - 1 - i) else 0);
            N = N - 1;
            candidates := apply(#G#0, i -> (
                e := fundEltsP'#(s#i);
                apply(select(fundPairsP', p -> member(e,p)), p -> matrix{if e == p#1 then reverse p else p})
            )) | apply(#G#1, i -> {fundEltsP'#(s#(#G#0+i))});
            candidates = unique if #candidates == 0 then {map(ZZ^(numrows P'star), ZZ^0, 0)} else fold(candidates, (a, b) -> flatten table(a, b, (i, j) -> i|j));
            if debugLevel > 1 and #candidates > 1 then print("morphisms1: Testing " | net(#K*#candidates) | " sub-candidates...");
            flatten for C in candidates list (
                E := (C - D) * B;
                torsE := subTorsion(E^torsPart', T');
                if torsE === false then continue;
                freeE := try sub(E^freePart', ZZ);
                if freeE === null then continue;
                for psi in K list ( 
                    M := phi' | ((torsE + psi) || freeE);
                    if not all(otherPairs, p -> any(P'.hexagons, h -> compareHex({M*p#0 % P'star, M*p#1 % P'star}, h))) then continue;
                    if opts.FindOne then return M else M 
                )
            )
        )
    )
)

------------------------------------------
-- Morphisms
------------------------------------------

fullRankSublattice = method()
fullRankSublattice Pasture := List => P -> (
    if P.cache#?"sublattice" then P.cache#"sublattice" else P.cache#"sublattice" = (
        dbg := debugLevel;
        debugLevel = 0;
        n := numrows presentation P.multiplicativeGroup;
        freePart := freePartPasture P;
        torsPart := toList(0..<n) - set freePart;
        S := {}; -- list of lists of type 4 hexagons
        s := rank (matrix{flatten(P.hexagons/first)})^freePart;
        if dbg > 0 then << "fullRankSublattice: Finding rank " << s << " sublattice..." << endl;
        currentPairs := {};
        r := 0;
        T := new MutableList from {0, 0, 0};
        while true do (
            A := if #currentPairs == 0 then map(ZZ^n,ZZ^0,0) else A | ( p := first last currentPairs; if p#0#0 == r+1 then matrix{p#1} else p#1#1 );
            r = rank A^freePart;
            if r == s then break;
            (type1Pair, type2Pairs, type3Pairs, type4Hexes) := (null, {}, {}, {});
            for h in P.hexagons - set(currentPairs/last) - set flatten S do (
                t := {rank((A | h#0#0)^freePart), rank((A | h#0#1)^freePart), rank((A | h#0#0 | h#0#1)^freePart)};
                if set t === set{r} then type4Hexes = append(type4Hexes, h);
                if type1Pair =!= null then continue;
                if set t === set{r, r+1} then type1Pair = {{sort t, if t === {r,r+1,r+1} then h#0 else reverse h#0}, h}
                else if set t === set{r+1} then ( -- hexagon with type 2 pair may also have type 1 pair
                    d := {rank((A | h#1#0)^freePart), rank((A | h#1#1)^freePart)};
                    if set d === set{r,r+1} then type1Pair = {{{r, r+1, r+1}, if d === {r,r+1} then h#1 else reverse h#1}, h}
                    else type2Pairs = append(type2Pairs, {{t, h#0}, h});
                ) else if set t === set{r+1, r+2} then type3Pairs = append(type3Pairs, {{t, h#0}, h});
            );
            S = append(S, type4Hexes);
            newPair := if type1Pair =!= null then ( T#0 = T#0 + 1; type1Pair )
            else if #type2Pairs > 0 then ( T#1 = T#1 + 1; type2Pairs#0 )
            else if #type3Pairs > 0 then ( S = append(S, {}); T#2 = T#2 + 1; type3Pairs#0 )
            else break;
            currentPairs = append(currentPairs, newPair);
            if dbg > 0 then << "\rfullRankSublattice: Types of pairs found: " << toList T << flush;
        );
        S = append(S, P.hexagons - set(currentPairs/last) - set flatten S);
        if dbg > 0 then << endl << "fullRankSublattice: Sublattice found. Creating generating rules..." << endl;
        G := currentPairs/first;
        g := #freePart;
        L := if g == 0 then map(ZZ^n, ZZ^0, 0) else matrix{flatten for i to #G-1 list if G#i#0#2 == 1 + G#i#0#1 then G#i#1 else G#i#1#1};
        k := 0;
        z := map(ZZ^(#torsPart), ZZ^1, 0);
        otherPairs := {};
        generatingRules := flatten for i to #G-1 list (
            if G#i#0#2 == 1 + G#i#0#1 then ( -- type 3 pair
                k = k+1;
                {{z,0},{z,1}}
            ) else ( -- type 1/2 pair
                B := G#i#1#0;
                isType2Pair := G#i#0#0 == G#i#0#1;
                if isType2Pair then B = B | G#i#1#1;
                coeff := (gens ker((L_(toList(0..<i+k)) | B)^freePart))_{0};
                if isType2Pair and abs((flatten entries coeff)#-2) != 1 then (
                    L = submatrix(L, , toList(0..<i+k)) | G#i#1#0 | submatrix(L, , toList(i+k+1..<g));
                    (B, coeff) = (B_{1,0}, coeff^{0..<i+k,i+k+1,i+k});
                );
                if abs((flatten entries coeff)#(if isType2Pair then -2 else -1)) != 1 then otherPairs = append(otherPairs, G#i#1);
                {{(L_(toList(0..<i+k)) | B)^torsPart*coeff, coeff}}
            )
        );
        type4Data := apply(#S, i -> apply(S#i, h -> apply(h#0, e -> (
            B := submatrix(L, , toList(0..<i));
            (BQQ, eQQ) := (sub(B^freePart, QQ), sub(e^freePart, QQ));
            C := flatten entries(eQQ // BQQ);
            c := if #C == 0 then -1 else lcm(C/denominator);
            coeffs := lift(c*(transpose matrix{C}), ZZ);
            (B*coeffs - c*e, coeffs, c)
        ))));
        otherPairs = otherPairs | delete(null, flatten apply(#S, i -> apply(#(S#i), j -> if any(type4Data#i#j, t -> abs last t != 1) then S#i#j#0)));
        (Q, proj) := myMinPres coker(L^freePart);
        P.cache#"latticeGensMatrix" = L;
        P.cache#"generatingRules" = generatingRules;
        P.cache#"otherPairs" = otherPairs; -- pairs not contained in lattice L (i.e. abs(coeff) > 1)
        P.cache#"type4Data" = type4Data;
        P.cache#"quotientLattice" = coker Q;
        P.cache#"quotientPruningMap" = proj;
        P.cache#"latticeGensMatrixInverse" = inverse sub(L^freePart, QQ);
        debugLevel = dbg;
        G
    )
)

morphisms = method(Options => {FindOne => false, FindIso => false}) -- Assumes fundamental elements of P1 generate P1.multiplicativeGroup
morphisms (Pasture, Pasture) := List => opts -> (P1, P2) -> (
    if opts.FindOne and P1 == P2 then return {pastureMorphism(P1, P2, id_(P1.multiplicativeGroup))};
    P1star := presentation P1.multiplicativeGroup;
    P2star := presentation P2.multiplicativeGroup;
    fundPairsP2 := unique flatten P2.hexagons;
    fundEltsP2 := unique flatten fundPairsP2;
    if opts.FindIso and not(#P1.hexagons == #P2.hexagons and #unique flatten flatten P1.hexagons == #fundEltsP2 and P1star == P2star) then (
        if debugLevel > 0 then print "morphisms: Pastures have different numerical data!";
        return {}
    );
    fundPairsP2unordered := unique(fundPairsP2 | fundPairsP2/reverse);
    fundPairsP2set := set(fundPairsP2/set);
    freePart1 := freePartPasture P1;
    freePart2 := freePartPasture P2;
    (r1, r2) := (#freePart1, #freePart2);
    (n1, n2) := (numrows P1star, numrows P2star);
    torsPart1 := toList(0..<n1) - set freePart1;
    torsPart2 := toList(0..<n2) - set freePart2;
    
    -- Fetch sublattice data
    G := fullRankSublattice P1;
    A := P1.cache#"latticeGensMatrix";
    generatingRules := P1.cache#"generatingRules";
    torsLatticeGens := generatingRules/first;
    otherPairs := P1.cache#"otherPairs";
    type4Data := P1.cache#"type4Data";
    B := P1.cache#"latticeGensMatrixInverse";
    
    -- Prepare torsion maps
    T1 := coker(P1star^torsPart1);
    T2 := coker(P2star^torsPart2);
    eta1 := map(ZZ^(numgens T2), ZZ^r1, 0);
    eta2 := map(ZZ^r2, ZZ^1, 0);
    H := select(abelianGroupHom(T1, T2), f -> ((((f | eta1) * P1.epsilon) || eta2) - P2.epsilon) % P2star == 0);
    H = apply(H, phi -> phi || map(ZZ^r2, ZZ^(#torsPart1), 0));
    Q := P1.cache#"quotientLattice";
    rho := P1.cache#"quotientPruningMap";
    K := apply(abelianGroupHom(Q, T2), f -> f * rho);
    T0P2 := if T2 == 0 then K else null;
    if debugLevel > 0 then print("morphisms: (#phi, psi): " | net(#H, K));
    if debugLevel > 0 then print("morphisms: Quotient lattice is: "| net Q);
    z0 := map(ZZ^n2, ZZ^0, 0);
    
    -- Main loop
    pastureMorphism(P1, P2, unique flatten for phi in H list (
        D := phi * A^torsPart1;
        torsType4 := apply(#type4Data, i -> apply(type4Data#i, t -> {phi*t#0#0^torsPart1, phi*t#1#0^torsPart1}));
        if not all(torsType4#0, p -> member(set{p#0 % P2star, p#1 % P2star}, fundPairsP2set)) then continue;
        delta := apply(r1, i -> phi * torsLatticeGens#i);
        C0 := z0;
        level := 0; -- level should always be the level of current node
        candidates := new MutableList from {{z0}};
        flatten while #(candidates#0) > 0 list (
            if #(candidates#level) == 0 then (
                C0 = C0_{0..<numcols C0-1};
                level = level - 1;
                candidates = drop(candidates, -1);
                candidates#level = drop(candidates#level, 1);
                continue;
            ) else if level < r1 then (
                C0 = C0 | candidates#level#0;
                rule := last generatingRules#level;
                newCandidates := if rule === 0 then fundEltsP2 -- first member of type 3
                else if rule === 1 then fundEltPartners(P2, candidates#level#0) -- second member of type 3
                else ( -- type 1/2 pairs
                    coeff := flatten entries rule;
                    isType2Pair := #coeff == level+2;
                    (alpha, beta) := if isType2Pair then (coeff#-2, coeff#-1) else (coeff#-1, 0);
                    w := (delta#level - C0*transpose matrix{drop(coeff, if isType2Pair then -2 else -1)}) % P2star;
                    if (abs(alpha), beta) == (1,0) then fundEltPartners(P2, alpha*w % P2star)
                    else unique flatten apply(fundPairsP2unordered, pair -> if (alpha*pair#0 + beta*pair#1) % P2star === w then {pair#1} else {})
                );
                if opts.FindIso then newCandidates = select(newCandidates, c -> rank(C0 | c) == 1 + numcols C0);
                if debugLevel > 1 then << endl << "morphisms: New candidates @ level " << level << ": " << newCandidates << endl;
                newCandidates = select(newCandidates, c -> (
                    all(#type4Data#(level+1), i -> (
                        data := type4Data#(level+1)#i;
                        torsPair := torsType4#(level+1)#i;
                        v := set apply(2, j -> ((C0 | c)*data#j#1 - torsPair#j) % P2star);
                        if data#0#2 == 1 and data#1#2 == 1 then member(v, fundPairsP2set) else (
                        if debugLevel > 0 then << endl << "morphisms: Type 4 data: " << data << endl;
                        any(fundPairsP2unordered, p -> set{p#0*data#0#2 % P2star, p#1*data#1#2 % P2star} === v)
                        )
                    ))
                ));
                level = level + 1;
                candidates#level = newCandidates;
                if debugLevel > 0 then << "\rmorphisms: Search tree: " << toString apply(#candidates, i -> #candidates#i) << flush;
                continue;
            ) else flatten while #(candidates#r1) > 0 list ( -- level == r1 (bottom of search tree)
                C := C0 | candidates#r1#0;
                candidates#r1 = drop(candidates#r1, 1);
                freeE := try sub(C^freePart2 * B, ZZ);
                if freeE === null then continue;
                torsECands := if T0P2 =!= null then T0P2 else liftTorsion(A^freePart1, (C - D)^torsPart2, T2, K);
                if torsECands === false then continue;
                for psi in torsECands list (
                    M := phi | (psi || freeE);
                    if opts.FindIso and abs det M != 1 then continue;
                    if not all(otherPairs, p -> member(set{M*p#0 % P2star, M*p#1 % P2star}, fundPairsP2set)) then continue;
                    if opts.FindOne then return {pastureMorphism(P1, P2, M)} else M
                )
            )
        )
    ))
)

areIsomorphic (Pasture, Pasture) := Boolean => (P, P') -> #morphisms(P, P', FindOne => true, FindIso => true) > 0

TEST /// -- P6, Q6
P6 = matroid(toList(0..5), {{0,1,2}}, EntryMode => "nonbases")
U26 = uniformMatroid_2 6
assert(areIsomorphic(foundation P6, foundation U26))
Q6 = matroid(toList(0..5), {{0,1,2},{0,3,4}}, EntryMode => "nonbases")
U35 = uniformMatroid_3 5
V = foundation U35
assert(hasMinor(Q6, U35))
assert(areIsomorphic(foundation Q6, V))
///

TEST /// -- Non-Fano
NF = specificMatroid "nonfano"
D = foundation NF
assert(hexTypes D === hashTable {"H"=>0,"D"=>1,"F3"=>0,"U"=>0})
D2 = foundation(NF ++ NF, Strategy => "hyperplanes")
assert(hexTypes D2 === hashTable {"H"=>0,"D"=>2,"F3"=>0,"U"=>0})
S = specificPasture "sign"
assert Equation(1, #morphisms(D, S))
assert Equation(1, #morphisms(D2, S))
///

TEST /// -- MacLane matroid
AG23 = affineGeometry (2,3)
M = AG23 \ set{0}
F = foundation M
H = pasture([x], "-x^3, x+x^5")
assert(F == foundation AG23)
assert areIsomorphic(F, H)
assert Equation(0, #morphisms(F, specificPasture "sign"))
///

TEST /// -- orientations of Pappus, non-Pappus
P = foundation specificMatroid "pappus"
Q = foundation specificMatroid "nonpappus"
S = specificPasture "sign"
assert Equation(18, #morphisms(P, S))
assert Equation(36, #morphisms(Q, S))
///

TEST /// -- Betsy-Ross
(E, H) = (toList(0..10), {{0,1},{0,2,5,6},{0,3,8,9},{0,4},{0,7,10},{1,2},{1,3,6,7},{1,4,5,9},{1,8,10},{2,3},{2,4,7,8},{2,9,10},{3,4},{3,5,10},{4,6,10},{5,7},{5,8},{6,8},{6,9},{7,9}})
BR = dual matroid(E, H/(h -> E - set h), EntryMode => "circuits")
G = pasture([t], "t + t^2")
assert areIsomorphic(G, foundation BR)
homSet = morphisms(G, G)
assert Equation(2, #homSet)
assert (set pairs tally(homSet/det) === set {(1,1),(-1,1)})
assert Equation(2, #morphisms(G, specificPasture "sign"))
assert Equation(2, #morphisms(G, pasture GF 4))
assert Equation(1, #morphisms(G, pasture GF 5))
///

TEST /// -- Betsy-Ross+
k = GF 4
BR = matrix {{1,0,0,1,1,1,1,1,1,1,1},{0,1,0,a,1,a+1,1,0,a+1,a+1,1},{0,0,1,a,a+1,1,0,a+1,a+1,a,a}} -- representation of Betsy-Ross over GF 4
BRplus = matroid(BR | matrix{{1},{0},{1}})
F = foundation(BRplus, Strategy => "hyperplanes", HasF7Minor => false, HasF7dualMinor => false)
assert(F == pasture k)
///

TEST /// -- Matroid with foundation K
M = specificMatroid fano ++ specificMatroid T8
elapsedTime foundation(M, Strategy => "hyperplanes")
areIsomorphic(foundation M, specificPasture krasner)
///

TEST /// -- Pastures which are not foundations
P = pasture GF 4 * pasture GF 5
G = pasture([t], "t + t^2")
assert Equation(0, #morphisms(P, G))
assert Equation(2, #morphisms(G, P))
-- It is known that any matroid which is representable over GF(4) and GF(5) is also representable over G, so the above shows that GF(4) * GF(5) is not a foundation
P = pasture GF 3 * specificPasture S
assert Equation(1, #morphisms(P, pasture GF 3))
assert (hexTypes P === hashTable {("D",1),("H",0),("F3",0),("U",0)} and not areIsomorphic(P, specificPasture D))
-- It is known that the foundation of any ternary matroid is a tensor product of copies of F2, F3, D, H, U, so the above shows that GF(3) * S is not a foundation
///

TEST /// -- Isomorphism check
M = specificMatroid "nonpappus"
nP = foundation M
phi = random M_*
N = matroid(M_*, (nonbases M)/(n -> n/(e -> phi#e)), EntryMode => "nonbases")
assert areIsomorphic(M, N)
F = foundation N
assert areIsomorphic(nP, F)
///

TEST /// -- R9B matroid, cf. https://doc.sagemath.org/html/en/reference/matroids/sage/matroids/catalog.html#sage.matroids.catalog.R9B
R9B = matroid(toList(0..8),{{0,1,2,7},{0,1,3,4},{0,1,6,8},{0,2,4,6},{0,3,5,8},{0,4,7,8},{1,2,3,5},{1,2,4,8},{1,3,7,8},{1,4,5,7},{2,3,6,7},{3,4,6,8},{5,6,7,8}}, EntryMode => "nonbases")
F = foundation R9B
assert (hexTypes F === hashTable {"H"=>1,"D"=>1,"F3"=>1,"U"=>12})
assert all(flatten flatten F.hexagons, e -> e != 0)
assert all({5,7,8,9,11}, q -> #morphisms(F, pasture GF q) == 0)
///

TEST ///-- Brettel partial field I
I = pasture([z,t], "z + z^2, -z^2 + t, z^4 + z*t")
P = foundation specificMatroid "pappus"
assert Equation(18, #morphisms(P, I))
///

TEST /// -- Fix for sign of torsion parts of type 4 pairs: cf. https://github.com/jchen419/Matroids-M2/commit/5565f171778ea229b9129db9bdfa7ca8ea3a50bd
k = GF 4
M = matroid matrix{{1,1,0,0,a+1,0,0,1,1},{0,0,1,0,a,0,a+1,a,1},{1,0,0,1,1,0,a,a+1,1},{a,0,0,0,0,1,1,1,1}}
assert hasMinor(M, specificMatroid "V8+")
assert Equation(2, #morphisms(foundation M, pasture k))
///

TEST /// -- all matroids on <= 8 elements with a type 2 pair in foundation
r3n8 = allMatroids(8, 3);
L3 = {21,50,51,57,86}
type2r3 = apply(L3, i -> r3n8#i);
assert all(type2r3, N -> (pairTypes foundation N)#"type 2" > 0)
assert areIsomorphic(foundation type2r3#2, foundation type2r3#4)
assert(Equation(#type2r3-1, #isoTypes(type2r3/foundation)))
r4n8 = allMatroids(8, 4);
L4 = {17,49,71,91,119,120,121,124,127,128,130,131,134,141,143,154,155,158,161,162,165,167,173,174,175,194,196,197,206,222,223,228,285,286,288,648}
type2r4 = apply(L4, i -> r4n8#i);
assert all(type2r4, N -> (pairTypes foundation N)#"type 2" > 0)
assert areIsomorphic(foundation type2r4#9, foundation type2r4#22)
assert(Equation(#type2r4-1, #isoTypes(type2r4/foundation)))
assert areIsomorphic(foundation type2r3#0, foundation type2r4#9)
assert areIsomorphic(foundation type2r3#2, foundation type2r4#35)
assert not any(type2r4, N -> areIsomorphic(foundation type2r3#3, foundation N))
-- Note: areIsomorphic(foundation type2r3#1, foundation type2r4#33) is true, but takes ~4 min.
///

TEST /// -- lifting torsion in morphisms
P1 = pasture([x,y], "x+y, x^3*y^5 + x^2*y^6")
P2 = pasture([w,z], "w^3*z^5 + w^2*z^6, w+z")
P3 = pasture([x], "-x^3,x+x^(-1),x^2+x^4")
assert areIsomorphic(P1, P2)
assert Equation(8, #morphisms(P1, P3))
assert Equation(8, #morphisms(P2, P3))
///

TEST /// -- foundations with same additive group and numbers of hexagons, but different numbers of fundamental elements
M1 = (allMatroids(8,4))#204
M2 = (allMatroids(8,4))#223
(F1, F2) = (M1, M2)/foundation
assert(F1.multiplicativeGroup == F2.multiplicativeGroup)
assert(#F1.hexagons == #F2.hexagons)
assert(#unique flatten flatten F1.hexagons == 54)
assert(#unique flatten flatten F2.hexagons == 60)
assert not areIsomorphic(F1, F2)
mor12 = morphisms(F1, F2)
assert(#mor12 == 4 and all(mor12, phi -> det phi == 0))
assert(#morphisms(F2, F1) == 0)
///

TEST /// -- number of type 3 pairs changes when reordering fundamental pairs
M = (allMatroids(8,4))#109
perm = {27,25,31,17,30,24,4,11,8,29,18,19,9,2,0,16,13,26,28,5,10,21,20,3,15,7,6,14,12,1,22,23}
F = foundation M
P = pasture(F.multiplicativeGroup, F.epsilon, (F.hexagons)_perm)
assert Equation(P, F)
assert(pairTypes F === hashTable {"type 1" => 7, "type 2" => 0, "type 3" => 3, "type 4" => 22})
assert(pairTypes P === hashTable {"type 1" => 5, "type 2" => 0, "type 3" => 4, "type 4" => 23})
-- Number of type 3 pairs can even change by more than 1
M = (allMatroids(8,4))#831
M1 = matroid(8, subsets(M.groundSet, rank M) - set bases M)
N = (allMatroids(8,4))#322
assert(isWellDefined M1 and areIsomorphic(M1, N))
assert(pairTypes foundation M1 === hashTable {"type 1" => 21, "type 2" => 0, "type 3" => 9, "type 4" => 180})
assert(pairTypes foundation N === hashTable {"type 1" => 25, "type 2" => 0, "type 3" => 7, "type 4" => 178})
///

TEST /// -- |Aut(M)| > |Aut(F_M)|, |End(F_M)| > |Aut(F_M)|
U24 = uniformMatroid_2 4
M = U24 ++ U24
assert Equation(1152, #getIsos(M, M))
U = foundation U24
F = foundation M
assert areIsomorphic(F, U ** U)
assert Equation(144, #morphisms(F, F))
assert Equation(72, #morphisms(F, F, FindIso => true))
///

TEST /// -- |Aut(V8)| < |Aut(F_V8)|
V8 = specificMatroid "vamos"
assert Equation(64, #getIsos(V8, V8))
-- F = foundation V8
-- elapsedTime mor = morphisms(F, F, FindIso => true); -- ~ 270 seconds
-- assert(tally(mor/det) === new Tally from {-1 => 384, 1 => 384})
///

------------------------------------------
-- Representations via pasture morphisms
------------------------------------------

getRepresentation (Matroid, PastureMorphism) := Matrix => (M, phi) -> (
    F := foundation(M, Strategy => "bases");
    if source phi =!= F then error "getRepresentation: Expected source of phi to equal the foundation of M";
    B := sort toList first bases M;
    (ch, basesMap) := (F.cache#"pruningMapB", F.cache#"genTableB");
    allBases := set bases M;
    table(rank M, #M_*, (i,j) -> (
        p := position(B, b -> b === j);
        if p === null then (
            B1 := set B - set{B#i} + set{j};
            if member(B1, allBases) then (
                s := (i+position(sort toList B1, b -> b === j))*phi.target.epsilon;
                s + (matrix(phi.map))*ch_{basesMap#B1}
            ) else 0
        ) else ( if i === p then 1 else 0 )
    ))
)

representations = method(Options => options morphisms)
representations (Matroid, GaloisField) := List => opts -> (M, k) -> (
    -- reps := apply(morphisms(foundation M, pasture k, opts), representation_M);
    -- apply(reps, A -> matrix table(rank M, #M_*, (i,j) -> (
    maps := morphisms(foundation(M, Strategy => "bases"), pasture k, opts);
    O := coordinatingPath M;
    apply(maps/getRepresentation_M, A -> rescalingRepresentative(matrix table(rank M, #M_*, (i,j) -> (
        if A#i#j === 0 then 0_k
        else if A#i#j === 1 then 1_k
        else (k.PrimitiveElement)^(if A#i#j == 0 then 0 else (A#i#j)_(0,0))
    )), O))
)

isOrientable = method()
isOrientable Matroid := Boolean => M -> #morphisms(foundation M, specificPasture "sign", FindOne => true) > 0

TEST ///
N = matroid(toList(0..7), {{0,1,2,3},{0,1,4,5},{2,3,4,5},{0,2,4,6},{1,3,5,7},{1,2,6,7},{3,4,6,7},{0,5,6,7}}, EntryMode => "nonbases")
N0 = (allMatroids(8,4))#128
assert(N == N0)
assert(pairTypes foundation N === hashTable apply({(1,0),(2,1),(3,1),(4,2)}, p -> ("type " | toString p#0, p#1)))
N1 = matroid(toList(0..8), {{0,1,2,3},{0,1,4,5},{0,2,4,6},{1,3,5,6},{1,2,4,7},{2,3,5,7},{3,4,6,7},{0,5,6,7},{0,1,4,8},{0,2,4,8},{0,3,4,8},{0,1,5,8},{2,3,5,8},{0,4,5,8},{1,4,5,8},{0,2,6,8},{0,4,6,8},{2,4,6,8},{2,3,7,8},{0,4,7,8},{2,5,7,8},{3,5,7,8},{1,6,7,8}}, EntryMode => "nonbases")
assert areIsomorphic(foundation N1, specificPasture G)
N2 = matroid(toList(0..8), {{0,1,2,3},{0,1,2,4},{0,1,3,4},{0,2,3,4},{1,2,3,4},{0,1,5,6},{2,3,5,6},{0,2,5,7},{1,4,5,7},{1,2,6,7},{3,4,6,7},{0,1,2,8},{0,1,3,8},{0,2,3,8},{1,2,3,8},{0,1,4,8},{0,2,4,8},{1,2,4,8},{0,3,4,8},{1,3,4,8},{2,3,4,8},{2,3,5,8},{0,4,5,8},{2,3,6,8},{0,4,6,8},{2,5,6,8},{3,5,6,8},{2,3,7,8},{0,4,7,8}}, EntryMode => "nonbases")
assert(pairTypes foundation N2 === hashTable apply({(1,1),(2,0),(3,1),(4,1)}, p -> ("type " | toString p#0, p#1)))
assert Equation(2, #morphisms(foundation N2, pasture GF 4))
assert all(representations(N2, GF 4), A -> N2 == matroid A)
///

TEST ///
B = specificMatroid betsyRoss
k = GF 4
setRandomSeed 5
elapsedTime searchRepresentation(B, k)
///

------------------------------------------
-- Natural map from foundation of minor
------------------------------------------

hyperplaneCorrespondenceTable := (M, N, e) -> (
    (HM, HN) := (hyperplanes M, hyperplanes N);
    hashTable apply(HN, h -> ( H := h/(i -> if i < e then i else i+1); (h, if member(H, HM) then H else H + set{e})))
)

inducedMapFromMinor = method()
inducedMapFromMinor (Matroid, ZZ, String) := PastureMorphism => (M, e, mode) -> (
    F := foundation M;
    N := if mode === "delete" then M \ set{e} else M / set{e};
    strat := F.cache#"strategy";
    G := foundation(N, Strategy => strat);
    (Fstar, Gstar) := (F.multiplicativeGroup, G.multiplicativeGroup);
    if (numgens Fstar == 0 or numgens Gstar == 0) then return pastureMorphism(G, F, map(Fstar, Gstar, 0));
    B := if strat === "hyperplanes" then (
        A := id_(ZZ^(numgens Gstar)) // (G.cache#"pruningMapH");
        H := hyperplaneCorrespondenceTable(M, N, e);
        inducedMinors := apply(sort(pairs G.cache#"genTableH" /toList, last)/first/toList, U -> 1 + (F.cache#"genTableH")#(set apply(U, h -> H#h)));
        id_(ZZ^(numgens Fstar))_{0} | (F.cache#"pruningMapH")_(inducedMinors | apply(inducedMinors, i -> i + F.cache#"numU24minors"))
    ) else (
        A = id_(ZZ^(numgens Gstar)) // (G.cache#"pruningMapB");
        addEltToMinor := (mode == "contract" and not member(e, loops M)) or (mode == "delete" and member(e, coloops M));
        negTable := hashTable((pairs G.cache#"genTableB")/reverse);
        I := id_(ZZ^(#bases M));
        b0 := first bases N;
        E := id_(ZZ^(#N_*));
        sf := N.cache#"coordinatingPath"; -- G.cache#"spanningForest"
        D := matrix{apply(sf, e -> E_{e#1} - E_{e#0})};
        J := matrix{apply(sf, e -> 1)} || map(ZZ^(#bases N-1), ZZ^(#sf), 0);
        K := matrix{apply(#bases N, e -> 1)} || map(ZZ^(#bases N-1), ZZ^(#bases N), 0);
        delta := id_(ZZ^1) ++ (id_(ZZ^(#bases N)) - K - matrix{apply(bases N, b -> (
            v := transpose matrix{apply(#N_*, i -> if (member(i, b) and not member(i, b0)) then 1 else if (not member(i, b) and member(i, b0)) then -1 else 0)};
            (submatrix'(G.cache#"imDegMap",{0},{0}) - J) * (v // D)
        ))});
        gamma := matrix{apply(#bases N, j -> (
            b := negTable#(j+1)/(i -> if i < e then i else i+1);
            if addEltToMinor then b = b + set{e};
            I_{F.cache#"genTableB"#b - 1}
        ))};
        F.cache#"pruningMapB" * (id_(ZZ^1) ++ gamma) * delta
    );
    pastureMorphism(G, F, B*A)
)

TEST ///
Q6 = specificMatroid "Q6"
strat = "hyperplanes"
foundation(Q6, Strategy => strat)
N1 = Q6 / set{5}
assert areIsomorphic(N1, uniformMatroid_2 5)
phi1 = inducedMapFromMinor(Q6, 5, "contract")
assert(0 != det phi1 and member(phi1, morphisms(foundation(N1, Strategy => strat), foundation Q6)))
N2 = Q6 \ set{0}
assert areIsomorphic(N2, uniformMatroid_3 5)
phi2 = inducedMapFromMinor(Q6, 0, "delete")
assert(0 != det phi2 and member(phi2, morphisms(foundation(N2, Strategy => strat), foundation Q6)))
///

------------------------------------------
-- (Quasi-)Fixed/cofixed elements
------------------------------------------

isQuasiFixed = method()
isQuasiFixed (Matroid, ZZ) := Boolean => (M, e) -> coker (inducedMapFromMinor(M, e, "delete")).map == 0

isQuasiCofixed = method()
isQuasiCofixed (Matroid, ZZ) := Boolean => (M, e) -> isQuasiFixed(dual M, e)

cosimpleMatroid = method()
cosimpleMatroid Matroid := Matroid => M -> dual simpleMatroid dual M

isQuasiFree = method()
isQuasiFree Matroid := Boolean => M -> (
    if not is3Connected M then return false;
    all(select(toList(0..<#M_*), e -> isQuasiFixed_M e), e -> not is3Connected cosimpleMatroid (M\{e})) and  all(select(toList(0..<#M_*), e -> isQuasiCofixed_M e), e -> not is3Connected simpleMatroid (M/{e}))
)

------------------------------------------
-- Fundamental diagram
------------------------------------------

fundamentalDiagram = method()
fundamentalDiagram Matroid := Sequence => M -> (
    minorList := {uniformMatroid_2 4, uniformMatroid_2 5, specificMatroid "C5"};
    minorList = minorList | (minorList_{1,2}/dual) | {uniformMatroid_2 4 ++ uniformMatroid_0 1, uniformMatroid_2 4 ++ uniformMatroid_1 1, uniformMatroid_2 4 ++ uniformMatroid_1 2};
    U24minors := allMinors(M, minorList#0);
    otherMinors := apply(toList(1..7), i -> allMinors(M, minorList#i));
    numMinors := prepend(#U24minors, apply(otherMinors, s -> #s));
    (U, N1, N2) := (numMinors#0, numMinors#-3 + numMinors#-2, numMinors#-1);
    N := sum numMinors - N1 - N2 - U;
    otherMinors = flatten otherMinors;
    V := toList(0..<(#U24minors + #otherMinors));
    E := delete(null, flatten table(U, N+N1, (i, j) -> if isSubset(otherMinors#j#0, U24minors#i#0) and isSubset(otherMinors#j#1, U24minors#i#1) then {i, U+j}));
    E = E | delete(null, flatten table(N1, N2, (i, j) -> if isSubset(otherMinors#(N+N1+j)#0, otherMinors#(N+i)#0) and isSubset(otherMinors#(N+N1+j)#1, otherMinors#(N+i)#1) then {U+N+i, U+N+N1+j}));
    (numMinors, V, E)
)

fundamentalDiagram3Connected = method()
fundamentalDiagram3Connected Matroid := Sequence => M -> (
    if not is3Connected M then error "fundamentalDiagram3Connected: expected 3-connected matroid";
    minorList := {uniformMatroid_2 4, uniformMatroid_2 5, uniformMatroid_3 5, whirl 3};
    U24minors := allMinors(M, minorList#0);
    otherMinors := apply(toList(1..3), i -> allMinors(M, minorList#i));
    numMinors := prepend(#U24minors, apply(otherMinors, s -> #s));
    -- otherMinors = flatten otherMinors;
    N := sum numMinors;
    E := flatten apply(numMinors#1, i -> (p := otherMinors#0#i; apply(toList(M.groundSet - (p#0+p#1)), e -> {position(U24minors, u -> u === {p#0, p#1+set{e}}), i + numMinors#0})));
    E = E | flatten apply(numMinors#2, i -> (p := otherMinors#1#i; apply(toList(M.groundSet - (p#0+p#1)), e -> {position(U24minors, u -> u === {p#0+set{e}, p#1}), i + numMinors#0 + numMinors#1})));
    E = E | flatten flatten apply(numMinors#3, i -> (
    	p := otherMinors#2#i;
	U24pos := positions(U24minors, u -> #(u#0-p#0) == 1 and #(u#1-p#1) == 1);
	apply(6, j -> {{N - last numMinors + i, N + 6*i + j}, {U24pos#j, N + 6*i + j}})
    ));
    (numMinors, toList(0..(N + 6*(last numMinors) - 1)), E)
)

HashTable _ List := (H, L) -> flatten apply(L, v -> H#v)

maxVradius = method()
maxVradius Matroid := ZZ => M -> (
    (S,V,E) := fundamentalDiagram3Connected M;
    H := hashTable((a,b) -> flatten {a,b}, E | E/reverse);
    L0 := apply(S#1, i -> i + S#0) | apply(S#2, i -> i + S#0+S#1);
    (n, r) := (#L0,0);
    L := unique(L0 | H_L0);
    print (L,L0);
    if #L =!= #L0 then r = 1;
    while n < #L and not isSubset(V, L) do ( (n, r) = (#L, r+1); L = unique(L | H_L); );
    (r, isSubset(V, L))
)
    
coveringNumber = method()
coveringNumber (Matroid, ZZ) := Sequence => (M, opt) -> (
    (S, V, E) := fundamentalDiagram M;
    H := hashTable((a,b) -> flatten {a,b}, E | E/reverse);
    L0 := apply(S#1, i -> i + S#0) | apply(S#3, i -> i + S#0+S#1+S#2);
    all5EltMinors := toList(S#0..<(#V-S#-1));
    additional4Elts := toList((#V-S#-1-S#-2-S#-3)..<(#V-S#-1));
    U24Minors := toList(0..<S#0);
    (n, r) := (#L0, 0);
    L := unique(L0 | H_L0);
    if #L =!= #L0 then r = 1;
    if opt == 0 then (
    	while n < #L and not isSubset(all5EltMinors, L) do ( (n, r) = (#L, r+1); L = unique(L | H_L); );
    	(r, isSubset(all5EltMinors, L))
    ) else if opt == 1 then (
        while n < #L and not isSubset(U24Minors, L) do ( (n, r) = (#L, r+1); L = unique(L | H_L); );
    	(r, isSubset(U24Minors, L))
    ) else if opt == 2 then (
        if #additional4Elts == 0 then r = 0;
        while n < #L and not isSubset(additional4Elts, L) do ( (n, r) = (#L, r+1); L = unique(L | H_L); );
    	(r, isSubset(additional4Elts, L))
    ) else if opt == 3 then (
        while n < #L and not isSubset(V, L) do ( (n, r) = (#L, r+1); L = unique(L | H_L); );
        (r, isSubset(V, L))
    )
)

fundDiagram2 = method()
fundDiagram2 Matroid := Sequence => M -> (
    minorList := {uniformMatroid_2 4, uniformMatroid_2 5, specificMatroid "C5"};
    minorList = minorList | (minorList_{1,2}/dual);
    U24minors := allMinors(M, minorList#0);
    otherMinors := apply(toList(1..4), i -> allMinors(M, minorList#i));
    numMinors := prepend(#U24minors, apply(otherMinors, s -> #s));
    U := numMinors#0;
    N := sum numMinors - U;
    -- (U, N1, N2) := (numMinors#0, numMinors#-3 + numMinors#-2, numMinors#-1);
    -- N := sum numMinors - N1 - N2 - U;
    otherMinors = flatten otherMinors;
    V := toList(0..<(#U24minors + #otherMinors));
    E := delete(null, flatten table(U, N, (i, j) -> if isSubset(otherMinors#j#0, U24minors#i#0) and isSubset(otherMinors#j#1, U24minors#i#1) then {i, U+j}));
    -- E = E | delete(null, flatten table(N1, N2, (i, j) -> if isSubset(otherMinors#(N+N1+j)#0, otherMinors#(N+i)#0) and isSubset(otherMinors#(N+N1+j)#1, otherMinors#(N+i)#1) then {U+N+i, U+N+N1+j}));
    (numMinors, V, E)
)

MVradius = method()
MVradius Matroid := Sequence => M -> (
    (S, V, E) := fundDiagram2 M;
    H := hashTable((a,b) -> flatten {a,b}, E | E/reverse);
    L0 := apply(S#1, i -> i + S#0) | apply(S#3, i -> i + S#0+S#1+S#2);
    -- all5EltMinors := toList(S#0..<(#V-S#-1));
    -- additional4Elts := toList((#V-S#-1-S#-2-S#-3)..<(#V-S#-1));
    U24Minors := toList(0..<S#0);
    (n, r) := (#L0, 0);
    L := unique(L0 | H_L0);
    if #L =!= #L0 then r = 1;
    while n < #L and not isSubset(V, L) do ( (n, r) = (#L, r+1); L = unique(L | H_L); );
    (r, isSubset(V, L))	
)


TEST ///
M = specificMatroid "Q6"
assert(coveringNumber(M, 0) == (2, true))
assert(coveringNumber(M, 1) == (3, true))
assert(coveringNumber(M, 2) == (0, true))
///

------------------------------------------
-- Single-element represented extensions
------------------------------------------

vectorExtensions = method()
vectorExtensions Matrix := List => A -> (
    (m, n) := (numrows A, numcols A);
    k := ring A;
    vecs0 := { matrix{{0_k}}, matrix{{1_k}} };
    k0 := {matrix{{0_k}}} | apply(k.order - 1, e -> matrix{{k.PrimitiveElement^e}});
    vecs := vecs0;
    if debugLevel > 0 then << "vectorExtensions: finding extensions of " << m << " x " << n << " matrix..." << endl;
    for i to m-2 do vecs = flatten apply(vecs, v -> if v == 0 then apply(vecs0, a -> v || a) else apply(k0, a -> v || a));
    select(apply(vecs, v -> matroid(A | v)), isSimple)
)

extensions = method()
extensions (Matrix, List) := List => (A, pastureList) -> (
    mats := isoTypes vectorExtensions A;
    select(mats, M -> all(pastureList, P -> #morphisms(foundation M, P, FindOne => true) > 0))
)
extensions (Matroid, GaloisField, List) := List => (M, k, pastureList) -> (
    reps := representations(M, k); -- is FindOne sufficient here?
    isoTypes flatten apply(reps, A -> extensions(A, pastureList))
)

end--
------------------------------------------

-- specific matroids
F7 = specificMatroid "fano"
T8 = matroid (id_((ZZ/3)^4) | matrix table (4, 4, (i,j) -> if i == j then 0_(ZZ/3) else 1)) -- T8
C4 = matroid(toList(0..3), {{0,1}}, EntryMode => "nonbases")
C5 = matroid(toList(0..4), {{0,1,2}}, EntryMode => "nonbases")
P6 = matroid(toList(0..5), {{0,1,2}}, EntryMode => "nonbases")
Q6 = matroid(toList(0..5), {{0,1,2},{0,3,4}}, EntryMode => "nonbases")
R1 = matroid(toList(0..6), {{0,1,2,3}}, EntryMode =>"nonbases")
R2 = matroid(toList(0..6), {{0,1,2},{4,5,6}}, EntryMode => "nonbases")
R3 = matroid(toList(0..6), {{0,1,2}}, EntryMode => "nonbases")
Q8 = matroid(toList(0..<8), {{0,1,2,3},{0,1,4,5},{1,2,5,6},{2,3,6,7},{0,3,4,7},{4,5,6,7},{0,1,6,7},{1,2,4,7},{2,3,4,5},{0,3,5,6},{0,2,4,6}}, EntryMode => "nonbases")
L8 = matroid(toList(0..<8), {{0,1,2,3},{0,1,4,5},{1,2,5,6},{2,3,6,7},{0,3,4,7},{4,5,6,7},{0,7,2,5},{1,6,3,4}}, EntryMode => "nonbases")
Q8 = matroid(toList(0..<8), {{0,1,2,3},{0,1,4,5},{1,2,5,6},{2,3,6,7},{0,3,4,7},{4,5,6,7}, EntryMode => "nonbases")
M = matroid(toList(0..<7), {{0,1,2},{0,3,4},{1,3,5},{2,4,5},{2,3,6}}, EntryMode => "nonbases")
N = matroid(toList(0..<8), {{0,1,2,3},{0,1,4,5},{2,3,4,5},{0,1,6,7},{2,3,6,7},{4,5,6,7}}, EntryMode => "nonbases")
GF 4; D = matrix{{1,0,0,1,a,1,0,a+1,1},{0,1,0,a,a,1,a,1,a+1},{0,0,1,0,1,1,1,1,1}}
GF 4; D = matrix{{1,0,0,1,a+1,1,0,a,1},{0,1,0,a+1,a+1,1,a+1,1,a},{0,0,1,0,1,1,1,1,1}}
M = matroid (matrix{{7:1}} || transpose matrix {{-3,0},{-3/4,-1},{3/2,-2},{-3/4,1},{3/2,2},{0,0},{3,0}}) -- Example in paper

-- Example 3.4 in https://arxiv.org/pdf/math/0202294.pdf
NSC = {{0,5,6,7},{1,4,5,7},{2,3,5,7},{0,2,4,7},{0,1,4,8},{0,2,3,8},{0,1,3,7},{2,4,5,6},{1,3,5,6},{0,1,2,5}}
SC = select(subsets(9,5), S -> all(NSC, c -> not isSubset(c,S)));
N = matroid(NSC | SC, EntryMode => "circuits")
(isWellDefined N, betti ideal N)
elapsedTime (hexTypes foundation N, pairTypes foundation N)
FE = unique flatten flatten (foundation N).hexagons;
z = FE#(position(FE, e -> e == 0))
(#freePartPasture foundation N, #fundEltPartners (foundation N, z))
-- In particular, when 1 is a fundamental element in a foundation, partners of 1 need not span (F_M)*

V3 = pasture([x,y,z],"x+y,z-y,x*z+y^2");
V5 = pasture([x,y],"x+y, x*y-y,x^2*y-1,-y^2");
V7 = pasture([x,y],"-1,x+y,x^2+y^2");
V9 = pasture([x,y],"-1,-x-1,-y-1,-x*y-1");
-- Note: for wheels/whirls, computing foundations via Strategy => "hyperplanes" is faster, but the opposite is true for spikes/swirls

-- a class of non-orientable matroids (Bland--Las-Vergnas, Orientability of Matroids, Ex. 3.11, https://www.sciencedirect.com/science/article/pii/0095895678900801)
r = 6
C = apply(subsets(r, 2), p -> p | {p#0 + r, p#1 + r}) | apply(r, i -> delete(i, toList(0..<r)) | {i+r}) | {apply(r, i -> i+r)}
M = matroid(toList(0..<2*r), C | select(subsets(2*r, r+1), S -> not any(C, c -> isSubset(c, S))), EntryMode => "circuits")
elapsedTime morphisms(foundation M, specificPasture "sign")
-- for r = 6, M is representable over GF 5

-- Example 6.8.4 in Oxley
needsPackage "Cyclotomic"
reidMatroid := k -> matroid(
    K := cyclotomicField k;
    alpha := K_0;
    id_(K^3) | matrix{{1,1},{1-alpha,1},{1,0}} | matrix{apply(k-1, i -> ( f = sum(i+1, j -> alpha^j); matrix{{1,0},{1,1},{f,f}} ))}
)
areIsomorphic(reidMatroid(2), specificMatroid "nonfano")
M = reidMatroid(10)
elapsedTime F = foundation M
representations(M, GF 2^8)
representations(M, GF 3^5)
representations(M, GF 5)
representations(M, GF 5^2)
representations(M, GF 7)
-- reidMatroid(k) is representable over finite field iff order of field is 1 mod k (i.e. field contains primitive kth root of unity)

-- Nathan's spike
n = 4
X = entries id_((ZZ/2)^n) /(r -> transpose matrix{r})
Y = apply(n, i -> sum X - X#i)
E = toList apply(n, i -> X#i) | toList apply(n, i -> Y#i)
fringeBases = flatten flatten apply(subsets(n,2), p -> apply(toList(fold(apply(toList(0..<n) - set p, i -> set{X#i, Y#i}), (a,b) -> a ** b))/deepSplice/toList, s -> {s | {X#(p#0), Y#(p#0)}, s | {X#(p#1), Y#(p#1)}}));
fringeBases = apply(fringeBases, B -> sort apply(B, b -> position(E, e -> e == b)))
hypercube = toList((fold(apply(n, i -> set{i,i+n}), (a,b) -> a**b))/deepSplice/toList/sort)
areIsomorphic(matroid(toList(0..<2*n), fringeBases | hypercube), spike n \ set{0})
indepSet = {{0,1,2,3},{0,1,6,7},{2,3,4,5},{4,5,6,7}}
NS = matroid(toList(0..<2*n), fringeBases | (hypercube - set indepSet))
nonbases NS /toList == {{0,1,2,3},{0,1,4,5},{2,3,4,5},{0,2,4,6},{1,2,5,6},{0,3,4,7},{1,3,5,7},{0,1,6,7},{2,3,6,7},{4,5,6,7}}
NS = matroid(toList(0..<8), {{0,1,2,3},{0,1,4,5},{2,3,4,5},{0,2,4,6},{1,2,5,6},{0,3,4,7},{1,3,5,7},{0,1,6,7},{2,3,6,7},{4,5,6,7}}, EntryMode => "nonbases")
F = foundation NS
G = pasture([z], "z+z^2")
morphisms(G, G)
G = foundation specificMatroid "betsyRoss"
morphisms(F, G)

-- Non-trivial isomorphism check
F = foundation uniformMatroid_3 7
G = foundation uniformMatroid_4 7
F == G
elapsedTime areIsomorphic(F, G)

-- Isomorphism classes of foundations of matroids on 7 elements
r3 = select(allMatroids 7, M -> rank M <= 3);
elapsedTime foundr3 = r3/foundation;
numericalData = tally apply(foundr3, F -> (#freePartPasture F, #F.hexagons))
numericalClasses = apply(keys numericalData, k -> foundr3_(positions(foundr3, F -> k == (#freePartPasture F, #F.hexagons))));
elapsedTime isoTypes foundr3;

-- single element extensions
linearSubclasses = method()
linearSubclasses Matroid := List => M -> (
    r := rank M;
    hyp := hyperplanes M;
    corank2 := select(flats M, f -> rank_M f == r - 2);
    upSets := apply(corank2, f -> {f,select(hyp, h -> isSubset(f, h))});
    k := 0;
    validClasses := new MutableHashTable;
    H := hyp;
    for h in H do (
	c := set {};
	while (#H > 0) do (
	    c = c + set {h};
	    validClasses#c = 1;
	    H = H - set unique flatten select(upSets, u -> member(h, u));
	)
    )
)

isolatedHypSets = method()
isolatedHypSets (List, List) := Set => (hyp, upSets) -> (
    if #hyp == 0 then return set{};
    h0 := hyp#0;
    result := set{{h0}};
    while true do (
	numResults := #result;
	for l in keys result do (
	    result = result + set apply(hyp - set unique flatten select(upSets, u -> any(l, h -> member(h, u))) - set hyp_(toList(0..<max positions(hyp, H -> member(H,l)))), e -> append(l, e));
	);
    	if #result == numResults then break else print("isolatedHypSets: " | net(#result - numResults));
    );
    result + isolatedHypSets(hyp - set {h0}, upSets)
)

elapsedTime tally apply(subsets(28, 2), s -> (
includedIndices = s;
s0 = unique flatten upSets_includedIndices;
finishGen = false;
while finishGen == false do (
    currentSize = #includedIndices;
    for i in toList(0..<28) - set includedIndices do (
	if #(set(upSets#i) * set(s0)) > 1 then (
	    s0 = unique(s0 | upSets#i);
	    includedIndices = append(includedIndices, i);
	    break;
	);
    );
    if #includedIndices == currentSize then finishGen = true;
);
set includedIndices
))

-- TODO:
-- Q: when checking otherPairs, enough to consider torsion?
-- implement limits/colimits of pastures
-- determine if a pasture is a tensor product of specified pastures
-- compute symmetry quotients? Q: When is natural map Aut(M) -> Aut(F_M) surjective?
-- map from hyperplane presentation of foundation to basis presentation?
-- finish single element extensions via linear subclasses
-- Q: prefer type 1/2 pairs with coefficients of 1 when building fullRankSublattice? (Not entirely clear if this will actually lead to an improvement.)

restart
load "foundations.m2"
debugLevel = 1
