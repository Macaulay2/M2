
newPackage(
    "TriangularSets",
    Version => "0.1", 
    Date => "2 September 2017",
    Authors => {
      {Name => "Diego Cifuentes",
       Email => "diegcif@mit.edu",
       HomePage => "http://www.mit.edu/~diegcif"}
    },
    Headline => "triangular decompositions of ideals",
    Keywords => {"Commutative Algebra"},
    AuxiliaryFiles => true,
    PackageImports => {"Binomials","MapleInterface","Elimination"},
    PackageExports => {}
)

--  Copyright 2017 Diego Cifuentes.
--  You may redistribute this file under the terms of the GNU General
--  Public License as published by the Free Software Foundation,
--  either version 2 of the License, or any later version.

export { 
--Types
    "TriaSystem",
--Methods/Functions
    "triaSystem",
    "triangularize",
    "mvar",
    "initial",
    "isRegularChain",
    "isPrimeSimple",
    "isStronglyNormalized",
    "triangularizeBatch",
    "freeVariables",
    "minimalObjects",
    "checkTriangularize",
    "checkInterface",
    "ineqs",
    "gbList",
--Method options
--Symbols
    "TriangularDecompAlgorithm"
}

--##########################################################################--
-- GLOBAL VARIABLES 
--##########################################################################--


--##########################################################################--
-- SHORTCUTS
--##########################################################################--

-- list of generators
gbList = I -> first entries gens gb I

-- support of a list of polynomials
supportF = F -> if #F==0 then F else
    rsort toList sum for f in F list set support f;

uniqueFast = x -> keys set x;

--##########################################################################--
-- EXPORTED METHODS
--
--##########################################################################--

--###################################
-- Type definitions
--###################################

-- type representing a triangular system
TriaSystem = new Type of HashTable

net(TriaSystem) := Net => T -> (
    F := T.gens;
    netgens := if #F==0 then " "
        else if #F==1 then net first F
        else net F;
    H := T.ineqs;
    netineqs := if #H==0 then ""
        else if #H==1 then " / " | net first H
        else " / " | net H;
    return netgens | netineqs;
)

-- structure of a polynomial system
getStructure = (F,H) -> (
    nF := max(F / (f-> #terms f));
    if nF<=1 and #H==0 then return "Monomial";
    nH := max(H / (h-> #terms h));
    if nF<=2 and nH<=1 then return "Binomial";
    return "None";
)

selectTriaAlg = (alg,R,F,H) -> (
    if alg=!=null then return alg;
    struct := getStructure(F,H);
    if struct!="None" then return struct;
    if not checkInterface("Maple") then
        error "MapleInterface failed. Maple is needed unless ideal is binomial."; 
    if char R==0 and checkInterface("Epsilon") then return "Epsilon";
    return "Maple";
);

--###################################
-- Test Maple
--###################################

checkInterface = (alg) -> (
    mapleprogram := "returnvalue:=1:";
    epsilonprogram := "returnvalue:=SearchText('epsilon',cat(libname)):";
    test:= false;
    if alg=="Maple" then(
        try test = (callMaple(mapleprogram)==1)
    )else if alg=="Epsilon" then(
        try test = (callMaple(epsilonprogram)>0)
    )else
        error("unknown interface");
    return test;
)

-- ring with n variables
nVarsRing = (kk,n) -> (
    x := getSymbol "x";
    X := (x_0..x_(n-1));
    return kk(monoid[X,MonomialOrder=>Lex]);
)

checkTriangularize = (alg) -> (
    if alg=="Monomial" then return;
    if alg=="Binomial" then(
        checkHermite(); checkTriangularizeTorus(); );
    opts := TriangularDecompAlgorithm=>alg;
    R := nVarsRing(QQ,5);
    X := gens R;
    F:= null; TT:= null; T:=null;
    ---------------TEST0---------------
    F =  {X_0^3-X_0, X_0*X_2-X_2, X_2^2-X_2};
    TT = triangularize(ideal F,opts);
    assert(sum(degree \ TT) == 4);
    TT = triangularize(R,F,{X_0*X_2},opts);
    assert(#TT==1);
    T = first TT;
    assert(T.gens=={X_0-1,X_2-1} and #T.ineqs==0);
    ---------------TEST1---------------
    F = {X_1+3*X_3, 2*X_1-5*X_3, -X_1+4*X_3};
    TT = triangularize(R,F,{X_2},opts);
    assert(#TT==1);
    T = first TT;
    assert(#T.gens==2 and T.gens#1==X_3 and T.ineqs=={X_2});
    ---------------TEST2---------------
    F = {2*X_0^3 + 6*X_1*X_2, X_0*X_1 + 3*X_3, X_1*X_2^2 - 2*X_4^2};
    TT = triangularize(R,F,{},opts);
    assert(ideal F == intersect(saturate \ TT));
    ---------------TEST3---------------
    iszero := TT -> #TT==1 and #(TT_0).gens==0 and #(TT_0).ineqs==0;
    isone := TT -> #TT==0;
    assert(iszero triangularize(R,{0_R},{},opts) );
    assert(iszero triangularize(R,{},{},opts) );
    assert(isone triangularize(R,{1_R},{},opts) );
    assert(isone triangularize(R,{X_0^2,X_0+1},{},opts) );
)

checkHermite = () -> (
    A:=null; c:=null; H:=null; c2:=null;
    ---------------TEST0---------------
    A = matrix{{2,0},{1,0},{0,1}};
    c = {1,1,1};
    (H,c2) = myHermite(A, c, true, );
    assert(H == matrix{{1,0},{0,1},{0,0}});
    assert(c == c2);
    (H,c2) = myHermite(A, c, false, );
    assert(H == matrix{{1,0},{0,1},{0,0}});
    assert(c == c2);
    ---------------TEST1---------------
    A = matrix { {5,0,1,4}, {0,-1,-4,99}, {0,20,19,16}, {0,0,2,1}, {0,0,0,3} };
    c = {1,1,1,1,1};
    (H,c2) = myHermite(A, c, false, );
    assert(H_(0,0)==5 and H_(1,1)==1 and H_(2,2)==1 and H_(3,3)==3);
    assert(H_(1,0)==0 and H_(2,1)==0 and H_(3,2)==0 and H_(4,3)==0);
    assert(c == c2);
    (H,c2) = myHermite(A, c, true, );
    A = matrix { {5,0,1,4}, {0,-1,-4,99}, {0,20,19,16}, {0,0,2,1}, {0,0,0,3} };
    assert(H == matrix { {5,0,0,-1}, {0,1,0,-2}, {0,0,1,-1}, {0,0,0,3}, {0,0,0,0} });
    assert(c == c2);
)

checkTriangularizeTorus = () -> (
    R := nVarsRing(QQ,4);
    X := gens R;
    F:=null; T:=null; NZ:=null;
    ---------------TEST0---------------
    F =  {X_0^3-X_0, X_0*X_2-X_2, X_2^2-X_2};
    (T,NZ) = triangularizeBinomialTorus (F,{X_0,X_2},R,true,true);
    assert(T=={X_0-1,X_2-1});
    assert(member(X_2,NZ));
)

--###################################
-- Triangular systems
--###################################

triaSystem = method()
triaSystem(Ring,List,List) := TriaSystem => (R,F,H) -> (
    H = uniqueFast select(H, h -> h!=1);
    new TriaSystem from {
        symbol gens => rsort F,
        symbol ineqs => rsort H,
        symbol ring => R }
)
triaSystem(Ring,List) := TriaSystem => (R,F) -> 
    triaSystem(R,F,initial F)

gens(TriaSystem) := opts -> T -> T.gens
ineqs = method()
ineqs(TriaSystem) := T -> T.ineqs

codim(TriaSystem) := optsT -> (
    -- ignore first argument (options)
    T:= optsT_1;
    cdim:= #T.gens;
    return cdim;
)
dim(TriaSystem) := T -> numgens(T.ring) - codim T

degree(TriaSystem) := T -> product(for t in T.gens list degree(mvar t, t))

-- main variable of a polynomial
mvar = method()
mvar (RingElement) := f -> (
    I:= indices f;
    if #I==0 then return null;
    return (ring f)_(first I);
)

-- initial of a polynomial
-- output is monic
initial = method()
initial(RingElement,RingElement) := (x,f) -> (
    if isConstant f then return 1_(ring f);
    d := degree(x,f);
    c := leadCoefficient f;
    return f//(c*x^d);
)
initial(RingElement) := f -> initial(mvar f, f)
initial(List) := T -> for t in T list initial t
initial(TriaSystem) := T -> initial T.gens

-- pseudo remainder of f by g
-- TODO: f % g is slow when the exponents are large
pseudoRemainder(RingElement,RingElement,RingElement) := (x,f,g) -> (
    d1 := degree(x,f);
    d2 := degree(x,g);
    if d1<d2 then return f;
    h := initial(x,g);
    if not isConstant h then
        f = f * h^(d1-d2+1);
    return f % g;
)
pseudoRemainder(RingElement,TriaSystem) := (f,T) -> (
    if isConstant f then return f;
    for t in T.gens do(
        if f==0 then return f;
        x := mvar t;
        if mvar f < x then continue;
        f = pseudoRemainder(x,f,t);
    );
    return f;
)
RingElement % TriaSystem := (f,T) -> pseudoRemainder(f,T)

resultant(RingElement,TriaSystem) := opts -> (f,T) -> (
    if isConstant f then return f;
    for t in T.gens do(
        if f==0 then return f;
        x := mvar t;
        if mvar f < x then continue;
        f = resultant(f,t,x,opts);
    );
    return f;
)

freeVariables = method()
freeVariables(TriaSystem) := T -> (
    F:= T.gens;
    X:= gens T.ring;
    if #F==0 then return X;
    Xalg := for f in F list mvar f;
    return select(X, x-> not member(x,Xalg));
)

saturate(TriaSystem) := opts -> T -> (
    F:= T.gens;
    H:= T.ineqs;
    I:= ideal F;
    for h in H do I = saturate(I,h,opts);
    return I;
)

isRegularChain = method()
isRegularChain(TriaSystem) := T -> (
    for h in initial T do
        if resultant(h,T)==0 then return false;
    return true;
)

isStronglyNormalized = method()
isStronglyNormalized(TriaSystem) := T -> (
    F := T.gens;
    Xalg := select(for f in F list mvar f, m->m!=1);
    Xh := supportF initial F;
    if #(set Xh * set Xalg)>0 then return false;
    return true;
)

-- simple test to determine primality
isPrimeSimple = method()
isPrimeSimple(TriaSystem) := T -> (
    F:= T.gens;
    if not isPrime last F then return false;
    for i to #F-2 do
        if degree(mvar F_i, F_i)>1 then return false;
    return true;
)

--###################################
-- Triangular decompositions
--###################################

-- triangular decomposition
-- algorithms: Monomial, Binomial, Epsilon, Maple
triangularize = method(Options=> {TriangularDecompAlgorithm=>null});
triangularize(Ring,List,List) := opts -> (R,F,H) -> (
    if #F==0 then return {triaSystem(R,F,H)};
    alg:= selectTriaAlg(opts.TriangularDecompAlgorithm,R,F,H);
    TT:= if alg=="Monomial" then 
            triangularizeMonomial(F)
        else if alg=="Binomial" then
            reduceDecomposition triangularizeBinomial(F,H,R)
        else if alg=="Epsilon" then
            triangularizeEpsilon(F,H,R)
        else if alg=="Maple" then
            triangularizeMaple(F,H,R)
        else
            error("unknown algorithm");
    return TT;
)
triangularize(Ring,List) := opts -> (R,F) ->
    triangularize(R,F,{},opts)
triangularize(Ideal) := opts -> I -> 
    triangularize(ring I, I_*, {}, opts)
triangularize(MonomialIdeal) := opts -> I -> 
    triangularizeMonomial(I)

-- triangularize many systems simultaneously
triangularizeBatch = (alg,R,F,H) -> (
    TT:= if alg=="Epsilon" then
            triangularizeEpsilonBatch(R,F,H)
        else if alg=="Maple" then
            triangularizeMapleBatch(R,F,H)
        else
            error("unknown algorithm");
    return TT;
)

triangularizeMonomial = method();
triangularizeMonomial(MonomialIdeal) := (I) -> (
    TT := decompose I;
    return TT / ( T-> triaSystem(ring I,T_*,{}) );
)
triangularizeMonomial(List) := (F) -> 
    triangularizeMonomial(monomialIdeal F)

triangularizeMaple = (F,H,R) -> (
    F = hashTable{0=>F}; H = hashTable{0=>H};
    TT := triangularizeMapleBatch(R,F,H);
    return TT#0;
)

triangularizeMapleBatch = (R,F,H) -> (
    getOptions := (lazard,radicl,normalized) -> (
        sLaz:= if lazard then ",'output'='lazard'" else "";
        sRad:= if radicl then ",'radical'='yes'" else "";
        sNor:= if normalized===false then "" else ",'normalized'="|normalized;
        return sLaz | sNor | sRad;
    );
    getRing:= R -> (
        Q:= coefficientRing R;
        if char Q > 0 then
            if not Q#?order or Q.order!=char Q then 
                error("Field not implemented");
        sVars:= toString gens R;
        scharR:= toString char R;
        return "R:=PolynomialRing("|sVars|","|scharR|"): ";
    );
    getTria:= (i,Fi,Hi,sOpt) -> (
        sF:= toString Fi;
        sH:= if #H>0 then "," | toString Hi else "";
        sTria:= "dec:=Triangularize("|sF|sH|",R"|sOpt|"): ";
        sInfo:= "dec" | toString i | ":=[Info(dec,R)]: ";
        return sTria | sInfo;
    );
    getPrint:= (n) -> (
        sDec:= toString for i to n-1 list "dec"|toString i;
        return "returnvalue:=" | sDec | ": ";
    );
    processOutput:= (Hi,TTi) -> (
        if #Hi==0 then( --or not lazard
            TTi = first TTi; 
            TTi = TTi/(T -> triaSystem(R,T,initial T) ) ;
        )
        else 
            TTi = TTi / ( T -> triaSystem(R,T#0, (T#1|initial T#0)) );
        return TTi;
    );
    filter := f -> select(f, p->p!=0);
    F = applyPairs(F, (k,f) -> (k,filter f) );
    (TT,K) := initTT(R,F,H);
    n:= #K;
    sPackage:= "with(RegularChains): ";
    sRing:= getRing R;
    sOpt:= getOptions(true,false,false);
    sTria:= concatenate for i to n-1 list getTria(i,F#(K_i),H#(K_i),sOpt);
    sPrint:= getPrint(n);
    program:= sPackage | sRing | sTria | sPrint;
    use R;
    TTraw:= callMaple(program,"","placeholder1");
    for i to n-1 do TT#(K_i) = processOutput(H#(K_i),TTraw#i);
    return new HashTable from TT;
)

initTT = (R,F,H) -> (
    K := keys F;
    TT := new MutableHashTable from
        for k in K list k => null;
    P := partition(k->#(F#k)==0, K, {true,false});
    K0 := P#true; K1 := P#false;
    for k in K0 do
        TT#k = {triaSystem(R,F#k,H#k)};
    return (TT,K1);
)

triangularizeEpsilon = (F,H,R) -> (
    F = hashTable{0=>F}; H = hashTable{0=>H};
    TT := triangularizeEpsilonBatch(R,F,H);
    return TT#0;
)

triangularizeEpsilonBatch = (R,F,H) -> (
    getRing:= R -> (
        if char R > 0 then
            error("Epsilon does not support positive characteristic");
        sVars:= toString sort gens R;
        return "X:="|sVars|": ";
    );
    getTria:= (i,Fi,Hi) -> (
        if #Fi==0 then error("F must be nonempty");
        if #H>0 then Fi = {Fi,Hi};
        sF:= toString Fi;
        sTria:= "dec:=epsilon[RegSer]("|sF|",X): ";
        sInfo:= "dec" | toString i | ":=[dec]: ";
        return sTria | sInfo;
    );
    getPrint:= (n) -> (
        sDec:= toString for i to n-1 list "dec"|toString i;
        return "returnvalue:=" | sDec | ": ";
    );
    processOutput:= (TTi) -> (
        if #TTi==1 and #TTi#0==0 then TTi = {};
        TTi = TTi / ( T -> triaSystem(R,T#0, T#1) );
        return TTi;
    );
    filter := f -> select(f, p->p!=0);
    F = applyPairs(F, (k,f) -> (k,filter f) );
    (TT,K) := initTT(R,F,H);
    n:= #K;
    sRing:= getRing R;
    sTria:= concatenate for i to n-1 list getTria(i,F#(K_i),H#(K_i));
    sPrint:= getPrint(n);
    program:= sRing | sTria | sPrint;
    use R;
    TTraw:= callMaple(program,"","placeholder1");
    for i to n-1 do TT#(K_i) = processOutput(TTraw#i);
    return new HashTable from TT;
)

--###################################
-- Triangularize Binomials
--###################################

-- NZ: set of nonzero variables
-- TODO: handle the case that T0 = {1}
triangularizeBinomial = (F,NZ,R) -> (
    divideByVars := (f,X) -> product(set(support f) - X);
    triangularizeBinomial0 := (F,sF,Z,NZ) -> (
        --assume sF,Z are disjoint
        normalized:= true;
        (T0,NZ'):= triangularizeBinomialTorus(F,sF,R,normalized,true);
        T:= rsort (Z | T0);
        H:= minimalNonzero(T0, NZ | initialBinomial T0);
        return (triaSystem(R,T,H), NZ');
    );
    NZ = supportF NZ;
    queue := {(F,NZ)};
    TT := new MutableList;
    while #queue>0 do(
        (F,NZ) = first queue;
        queue = drop(queue,1);
        F = rsort gbList ideal F; --grevlex
        P:= partition(f-> #(terms f), F, {1,2});
        F':= P#2; Z:= P#1;
        sF:= support matrix(R, {F'});
        Z = Z / (f-> divideByVars(f,NZ));
        if member(1,Z) then continue;
        if #Z>0 then(
            D:= triangularizeMonomial(Z); 
            D = for T in D list T.gens;
            if #F'==0 then(
                D = for T in D list triaSystem(R,T,NZ);
                TT = join(TT, D );
                continue;
            );
            Z = D#0;
            needReduce:= #(set sF * set Z) > 0;
            if #D>1 or needReduce then (
                queue = queue | for T in D list (F'|T,NZ);
                continue;
            );
        );
        (T,NZ'):= triangularizeBinomial0(F',sF,Z,NZ);
        TT#(#TT) = T;
        queue = queue | for Xi in toList(NZ' - NZ) list (append(F,Xi), NZ);
    );
    return toList TT;
)

-- Input: T, triangular set of binomials
-- Input: NZ, set of nonzero vars (including initials of T)
-- Output: NZ' such that such that (T,NZ) = (T,NZ')
minimalNonzero = (T,NZ) -> (
    if #T==0 or #NZ==0 then return NZ;
    X := gens ring first T;
    nonzero := new MutableHashTable from 
        for x in X list x=>member(x,NZ);
    for t in T do(
        x:= mvar t;
        if not nonzero#x then continue;
        Y:= support last terms t;
        if member(x,Y) then continue;
        nonzero#x = false;
        for y in Y do nonzero#y = true;
    );
    NZ' := select(X, x -> nonzero#x);
    return NZ';
)

-- keeps only minimal sets
reduceDecomposition = TT -> (
    if #TT==0 then return TT;
    len:= (T -> #T.gens);
    contained:= (S,T) -> isSublist(S.gens,T.gens) and isSublist(S.ineqs,T.ineqs);
    cmp:= (T,T') -> (
        n:=len T; n':=len T';
        if n==n' then return 0;
        if n>n' then
            return if contained(T',T) then 1 else 0;
        return if contained(T,T') then -1 else 0;
    );
    TT = uniqueFast TT;
    minl:= minimalObjects(TT,cmp);
    return select(TT, T->minl#T);
)

-- minimal elements of a partial order
-- cmp(a,b) = 1 if a>=b
-- cmp(a,b) = -1 if a<b
-- cmp(a,b) = 0 if incomparable 
minimalObjects = (Os,cmp) -> (
    l := #Os;
    I := new MutableList from (0..<l);
    i := 0;
    while i<l do (
        j := i+1;
        if j>=l then break;
        while j<l do (
            rel := cmp(Os#(I#i),Os#(I#j));
            if rel==1 then (
                l = l-1;
                Ii:=I#i; I#i = I#l; I#l = Ii;
                break; )
            else if rel==-1 then (
                l = l-1;
                Ij:=I#j; I#j = I#l; I#l = Ij; )
            else(
                j = j+1;
                if j==l then i= i+1;
            );
        );
    );
    Il := toList take(I,l);
    return hashTable for i in 0..#I-1 list (Os#i,member(i,Il));
)

-- determines containment of sequences
-- maybe its easier to use isSubset(set S, set T)
isSublist = (S,T) -> (
    n:=#S; N:=#T;
    if n==0 then return true;
    if n>N then return false;
    i:= position( take(T,{0,N-n}), t -> t===S#0 );
    if i===null then return false;
    return isSublist(drop(S,1),drop(T,i+1));
)

-- initials of binomials (on the torus)
initialBinomial = F -> (
    initsupp := f -> drop(support leadMonomial f, 1);
    return flatten ( F / (f-> initsupp f) );
)

-- TODO: handle the case that T = {1}
-- triangularize F in K[X] on the torus
-- NZ: variables that we assume that are nonzero
triangularizeBinomialTorus = (F,X,R,normalized,getNZ) -> (
    if #F==0 then return ({},{});
    nonzero := new MutableHashTable from 
        hashTable for j to #X-1 list (j,false);
    (A,c) := binomial2matrix(F,X,nonzero);
    H:=null;
    (H,c) = myHermite(A, c, normalized, nonzero);
    s := select(toList(0..<#X), j->nonzero#j);
    NZ := X_s;
    H = entries H;
    T0 := for i to #H-1 list (
        vec2binomial(X, H_i, c_i) );
    if any(T0,f->f==1) then
        return if getNZ then ({1_R},{}) else {1_R};
    T := select(T0,f->f!=0);
    if not getNZ then return T;
    NZ = set NZ + set initialBinomial T;
    return (T, NZ);
)

-- matrix of exponents of a binomial system
binomial2matrix = (F,X,nonzero) -> (
    S := indices sum X;
    m := #F;
    c := new MutableList from 0..m-1;
    A := matrix for i in 0..m-1 list(
        (mons,coeffs) := coefficients F#i;
        coeffs = flatten entries coeffs;
        if #coeffs!=2 then error("not binomial");
        c#i = -coeffs_1//coeffs_0;
        mons = first entries mons;
        e0 := first exponents mons#0;
        e1 := first exponents mons#1;
        for j to #X-1 do(
            s:= S_j;
            if e0_s>0 and e1_s>0 then nonzero#j=true;
        );
        (e0-e1)_S
    );
    return (A, toList c);
)
-- f = x^e1 - c x^e2
vec2binomial = (x,e,c) -> (
    m1 := product for i to #x-1 list (x_i)^(max(e_i,0));
    m2 := product for i to #x-1 list (x_i)^(max(-e_i,0));
    f := m1 - c*m2;
    if isUnit f then f = 1_(ring f);
    return f;
);

-- reduces rows to echelon form (nonreduced)
-- negativepivot: whether the entries above pivots must be negative
-- keeps track of columns where cancellations occur
myHermite = (A,c,negativepivot,badcol) -> (
    m:= numRows A; I:= toList(0..<m);
    n:= numColumns A; J:= toList(0..<n);
    A = new MutableList from entries A;
    c = new MutableList from c;
    if badcol===null then
        badcol = new MutableHashTable from 
            hashTable for j in J list (j,false);
    column := j -> for i in I list (A#i)_j;
    assign := (i,Ai,ci,bcol) -> ( 
        A#i = Ai; 
        c#i = ci; 
        for j in bcol do badcol#j=true;
    );
    scaleRow := (i,j) -> 
        if (A#i)_j<0 then 
            assign(i, -(A#i), 1//c#i, {});
    swapRows := (i1,i2) -> (
        if i1!=i2 then(
            A1 := A#i1; c1 := c#i1;
            assign(i1, A#i2, c#i2, {});
            assign(i2, A1, c1, {});
        );
    );
    addRows := (i1,r1,i2,r2,j0) -> (
        A1 := r1*A#i1; 
        A2 := r2*A#i2;
        c12 := (c#i1)^r1*(c#i2)^r2;
        bcol:= select( drop(J,j0), 
            j -> not badcol#j and A1_j*A2_j<0 );
        return (A1+A2, c12, bcol);
    );
    reduceRows := (i1,i2,j) -> (
        x:=A#i1_j; y:=A#i2_j;
        A2:=null; c2:=null; b2:=null;
        if y%x==0 then(
            (A2,c2,b2)=addRows(i2,1,i1,-(y//x),j);
            assign(i2,A2,c2,b2);
        )else if x%y==0 then( 
            (A2,c2,b2)=addRows(i1,1,i2,-(x//y),j);
            assign(i1,A#i2,c#i2,{});
            assign(i2,A2,c2,b2);
        )else(
            (d,r,s) := gcdCoefficients(x,y);
            (A1,c1,b1):=addRows(i1,r,i2,s,j);
            (A2,c2,b2)=addRows(i1,y//d,i2,-x//d,j);
            assign(i1,A1,c1,b1);
            assign(i2,A2,c2,b2);
        );
    );
    reducePivot := (i1,i2,j) -> (
        x:=A#i1_j; y:=A#i2_j;
        if y==0 then return;
        q:= -(y//x);
        if y%x!=0 then q = q-1;
        (A2,c2,bcol):= addRows(i1,q,i2,1,j);
        if #bcol==0 or (y>0 and negativepivot) then
            assign(i2,A2,c2,bcol);
    );
    p := 0; --pivot
    Jp := {};
    for j in J do(
        aj := column j;
        Ij := select(drop(I,p), i-> aj_i!=0);
        if #Ij==0 then continue;
        Jp = append(Jp,j);
        swapRows(p,first Ij);
        for i in drop(Ij,1) do
            reduceRows(p,i,j);
        scaleRow(p,j);
        p = p + 1;
    );
    for p to #Jp-1 do
        for i in take(I,{0,p-1}) do
            reducePivot(p,i,Jp#p);
    A = matrix toList A;
    badcol = select(J, j->badcol#j);
    return (A,toList c);
)


RingMap TriaSystem := TriaSystem => (phi,T) -> (
    F := for f in T.gens list phi(f);
    H := for f in T.ineqs list phi(f);
    triaSystem(phi.target, F, H)
)

--##########################################################################--
-- Documentation and Tests
--##########################################################################--

beginDocumentation()

load "./TriangularSets/TriangularSetsDoc.m2";

TEST ///
load (TriangularSets#"source directory" | "./TriangularSets/test_triangular.m2")
///

