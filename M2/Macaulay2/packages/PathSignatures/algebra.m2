----------------------------------------------------------------------------------
--METHODS FOR TENSOR ALGEBRAS
----------------------------------------------------------------------------------


-- Diagonal matrix action on tensors
-- M is the 
matrixAction = method();
matrixAction (Matrix,  NCRing, NCRing) := NCRingElement => (M, A, B) -> (
    N :=transpose entries M;
    f := ncMap(B, A, apply(N, j->sum(length(j), i->j#i*(gens B)#i)));
    f
)

matrixAction (Matrix,  NCRingElement, NCRing) := NCRingElement => (M, p, B) -> (
    f := matrixAction(M,ring p, B);
    f(p)
)

-- Matrix * Tensor also computes the diagonal matrix action on a tensor, but creates the output nc ring automatically
Matrix * NCRingElement := (M, f) -> (
    n := length entries M;
    m := length entries transpose M;
    tf := length gens ring f;
    if(m != tf) then (error("a " | toString(n) | "x" | toString(m) | " matrix can not act on a tensor over " | toString(tf) | "-dimensional space.");)
    else (
    B := wordAlgebra(n, CoefficientRing => (coefficientRing ring f));
    matrixAction(M, f, B))
)

-- tensorParametrization takes a tensor T, constructs a ring R with one variable for each word appearing in T 
-- and creates the map that sends a variable to the coefficient of the corresponding word.
tensorParametrization = method(Options=>{CoefficientRing => null, VarWordTable => null})
tensorParametrization(NCRingElement) := opts -> (f) -> (
    t := terms f;
    lc := t / leadCoefficient;
    lm := t / leadMonomial;
    bR := coefficientRing (class f);
    bF := baseRing bR;
    if (opts.CoefficientRing =!= null ) then bF = opts.CoefficientRing;
    if(opts.VarWordTable === null) then (
        b := getSymbol("b");
        varis := apply(lm, i -> b_(wordString i));
        R := bF monoid(new Array from varis);
    ) else (
        vwtable := opts.VarWordTable;
        words := apply(lm, i-> value(wordString i));
        R = ring (product (keys vwtable));
        if(instance(R,QuotientRing)) then error("expected free polynomial ring.");
        scan(words, i-> (
                if (not isMember(i,values vwtable)) then (
                    printerr("warning: no variable associated to word " | toString(i));
                );
            ) 
        );
        lc = apply(gens R, i-> inner(f, (vwtable#i)_(ring f)));
    );
    map(bR,R,lc)
)

-- create the non commutative algebra over alphabet given by a list.
wordAlgebra = method(Options=>{CoefficientRing => QQ});
wordAlgebra (List) := opts -> (l) -> (
    Lt := getSymbol("Lt");
    myvars := apply(l,i-> (Lt_i));
    opts.CoefficientRing myvars
)

-- create the non commutative algebra over alphabet 1..z
wordAlgebra (ZZ) := opts -> (z) -> (
    wordAlgebra(toList(1..z), CoefficientRing => opts.CoefficientRing)
)

-- define shuffle products on words, use linExt to extend to NCRingElements. Define operator ** as shuffle product in NCAlgebra


--Intermediate operations for shuffle product of two words
shuffleHelper= method(); 
shuffleHelper(List,List,NCRing) := (w1,w2,R) -> (
    l1 := length(w1);
    l2 := length(w2);
    if(l1 == 0 and l2 == 0) then return 1_R;
    if(l1 == 0) then return (new Array from w2)_R;
    if(l2 == 0) then return (new Array from w1)_R;
    w1l := w1_{0..l1-2};
    w2l := w2_{0..l2-2};
    i := w1#-1;
    j := w2#-1;

    shuffleHelper(w1,w2l,R)*[j]_R + shuffleHelper(w1l,w2,R)*[i]_R
);

shuffleHelper(NCRingElement, List) := (f,w2) -> linExt(i->shuffleHelper(i,w2,ring f),f);

shuffleHelper(NCRingElement, NCRingElement) := (f,g) -> (
    if(ring f === ring g) then (
        return(linExt(i->shuffleHelper(f,i),g));)
    else (
        error "can not apply shuffle to polynomials from different rings";
    )
)

-- Exposed versions of shuffle

shuffle = method();
shuffle (NCRingElement, NCRingElement) := (a, b) -> shuffleHelper(a,b); 

NCRingElement ** NCRingElement := shuffle

NCRingElement ⧢ NCRingElement := shuffle

-- the antipode of the nc polynomial ring as a Hopf algebra
antipode NCRingElement := (f) -> (
    R := ring f;
    linExt(w -> (-1)^(length(w)) * (new Array from reverse(w))_R, f)
);



-- define halfshuffle on nc polynomials and words, then extend to nc pols via linExt
halfshuffleHelper = method();
halfshuffleHelper(NCRingElement, List) := (f,w) -> (
    wl := w_(toList(0..length(w)-2));
    wr := w_(-1);
    shuffleHelper(f,wl) * (ring f)_(wr-1)
)

halfshuffle = method();
halfshuffle (NCRingElement, NCRingElement) := (f,g) -> (
    if(ring f === ring g) then (
        if(degree f == 0 or degree g == 0) then error("can not apply halfshuffle to polynomials of degree zero.");
        return(linExt(i->halfshuffleHelper(f,i),g));)
    else (
        error "can not apply halfshuffle to polynomials from different rings";
    )
)

installMethod(symbol >>, NCRingElement, NCRingElement, (f,g)->halfshuffle(f,g))

-- the inner product on tensor space
innerHelper = method();
innerHelper(List, NCRingElement) := (l,f) -> (
    H := coefficientHTable f;
    mon := (new Array from l)_(ring f);
    if(H#?mon) then H#mon else 0
)

inner = method();
-- the first argument is to be viewed as an element of the dual space
inner(NCRingElement, NCRingElement) := (fv,f) -> (
    linExt(w->innerHelper(w,f),fv)
)

NCRingElement @ NCRingElement := inner



-- returns the word in an NC ring that corresponds to the signed volume under the signature
sgnVolTensor = method();
sgnVolTensor NCPolynomialRing := (R) -> (
    perms := permutations(toList(1..length(gens R)));
    (1/(length(gens R))!) * sum(perms,i-> sign(permutation i) * (new Array from i)_R)
);


-- adjointWord computes values of the half-shuffle homomorphism M_p of nc rings adjoint to polynomial maps of affine spaces under the signature.

-- Returns the image of a monomial under the map \varphi: R[x_1..x_d] \to T(R^d), x_i\maptso i, x_{i_1},...,x_{i_l}\mapsto x_{i_1}\shuffle .... \shuffle x_{i_l}
-- This function is then extended linearly in "phiMap"  

phiMapMon = method();
phiMapMon(List, NCPolynomialRing) := (l, A) -> (
    L := flatten apply(length(l), i -> toList((l#i : [i+1]_A)));
    fold(L, (i,j) -> i**j)
)

-- Returns the image of a polynomial under the map \varphi: R[x_1..x_d] \to T(R^d), x_i\maptso i, x_{i_1},...,x_{i_l}\mapsto x_{i_1}\shuffle .... \shuffle x_{i_l} 
-- Extends the previous function linearly.

phiMap = method();
phiMap(RingElement,NCPolynomialRing) := (p, A) -> (
    cA := coefficientRing A;
    sum(listForm p, i-> sub(i#1,cA) * phiMapMon(i#0,A))
)

adjointWordHelper = method();
adjointWordHelper (List, NCPolynomialRing, List) := (w, A, P) -> (

    w2 := {phiMap(P#(w#0 - 1),A)} | w_{1..length(w)-1};
    fold((i,j) -> i >> (phiMap(P#(j-1),A)), w2)
)

-- f is the input nc polynomial, A is the output nc ring and P is the polynomial transformation, given as a list of polynomials

adjointWord = method();
adjointWord (NCRingElement, NCPolynomialRing, List) := (f, A, P) -> (
    Raux:=ring product(P);
    d:=length(gens Raux);

    --Check that number of letters in the given NCRing is enough to compute the image of the word  
    if d>length(gens A) then (
        error("number of generators of the NCRing lower than dimension of the polynomial ring")
    );
    if not (length(P) == length(gens ring f)) then error("the polynomial transformation does not map to the space underlying the input word.");

    if not all(apply(P, p->part(0,p)), q->q==0) then (
        error("the image of 0 under the polynomial map is not 0");
    );
    if(f == 0_(ring f)) then return 0_A;

    linExt(w->adjointWordHelper(w,A,P), f)
)


-- Lyndon words and more.

-- Implementation of Duval's algorithm. Given d and k, nextLyndon(w,d,k) creates the next Lyndon word of length at most k in d letters after w in lexicographical order
nextLyndonWord = method();
nextLyndonWord(Array,ZZ,ZZ) := Array => (ar,d,k) -> (
    l := toList ar;
    nl := fold((ceiling(k/length(l))):l, (i,j)->i|j);
    if(length(nl)>k) then (nl = nl_{0..k-1});
    while(nl_(-1) == d and length(nl)>1) do (
        nl = nl_{0..length(nl)-2};
    );
    if(nl != {d}) then nl = nl + toList(((length(nl)-1):0) | (1:1));

    new Array from nl
);

-- lyndonWords(d,k) returns a list of all Lyndon words of length at most k in d letters

lyndonWords = method();
lyndonWords (ZZ,ZZ) := (d,k) -> (
    if(d <= 0) then error("d must be a positive integer in lyndonWords(d,k).");
    if(k <= 0) then error("k must be a positive integer in lyndonWords(d,k).");
    l:={[1]};
    while(l_(-1) != [d]) do (
        l = l | {nextLyndonWord(l_(-1),d,k)};
    );

    l
)

-- lie(a,b) returns the lie bracket of a and b

lie = (a,b) -> (a*b - b*a);

-- isLyndon(l) checks if l is a Lyndon word

isLyndon = method();
isLyndon Array := (w) -> (
    l := toList w;
    out := true;
    scan(1..length(l)-1, i->( if(out==true) then out = (l < l_{i..(length(l)-1)}) ) );

    out
);

-- lyndonFact(l) computes the standard decomposition of l

lyndonDecomposition = method();
lyndonDecomposition Array := (w) -> (
    i := length(w)-1;
    ls := apply(0..length(w)-2,i-> {new Array from w_{0..i},new Array from w_{i+1..length(w)-1}});
    cand := select(ls,i-> isLyndon(i_1));
    cand = cand_0;
    if(isLyndon(cand#0)) then return cand;

    lyndonDecomposition(cand#0) | {cand#1}
)


-- lieBasis(l, A) yields the basis element corresponding to the Lyndon word l in the free Lie algebra, realized in A

lieBasis = method();
lieBasis(Array, NCPolynomialRing) := (w,R) -> (
    if(length(w) == 0) then error("lieBasis expected a non-empty list as input.");
    if(length(w) == 1) then return R_(w_(-1) - 1);
    fact := apply(lyndonDecomposition(w),i-> lieBasis(i,R));
    lie(fact_0,fact_1)
)

lieBasis(List, NCPolynomialRing) := (l, R) -> lieBasis (new Array from l, R);



expTerm = (tl,l) -> (
    1/(length(l))!)*product(l,i->(tl_i)
)

-- Given a tensor p with constant term 0, tensorExp(p,k) returns the k-th level component of exp(p)
tensorExp = method();
tensorExp (NCRingElement, ZZ) := (p,k) -> (
    if(k==0) then return 1;
    if(length (select(terms p, j-> degree j == 0)) > 0) then error("tensorExp expects a nc polynomial with constant term 0.");
    s := {1} | toList apply(1..k, i-> sum(select(terms p, j->((degree j) == i))));
    comp := unique apply(compositions k, i->delete(0,i));
    t := sum(apply(comp, i-> expTerm(s,i)));

    t
)

logTerm = (tl,l) -> (
    1/(length(l)))*product(l,i->(tl_i)
)

-- Given a tensor p with constant term 1, tensorLog(p,k) returns the k-th level component of log(p)
tensorLog = method();
tensorLog (NCRingElement, ZZ) := (p,k) -> (
    if(select(terms p, j-> degree j == 0)!= {1}) then error("tensorLog expects a nc polynomial with constant term 1.");
    lp := - p + 1;
    s := {1} | toList apply(1..k, i-> sum(select(terms lp, j->((degree j) == i))));
    comp := unique apply(compositions k, i->delete(0,i));
    t := - sum(apply(comp, i-> logTerm(s,i)));

    t
)

lyndonShuffleHelper = method();
lyndonShuffleHelper(List,Ring,NCRing) := (l,R,A) -> ( -- R must be a suitable ring of Lyndon words
    w := new Array from l;
    var := hashTable apply(gens R,i-> (last baseName i, i));
    if isLyndon(w) then return var#w;
    dec := lyndonDecomposition(w);
    k := length(w);
    shuffle := fold(apply(dec, i-> i_A),(i,j)-> i**j);
    shmon := fold(apply(dec, i-> var#i),(i,j)-> i*j);
    coef := (product( apply(values tally dec, i-> i !)));
    rest := shuffle - coef * w_A;
    if rest == 0 then ( 
        return (1/coef * shmon);
    ) else (
        restpoly := lyndonShuffleHelper(rest,R);
        return(1/coef * (shmon - restpoly));
    )
)

lyndonShuffleHelper(NCRingElement,Ring) := (f,R) -> (
    linExt(i->lyndonShuffleHelper(i, R, ring f),f,CoefficientRing => R)
)

lyndonShuffle = method();
lyndonShuffle(NCRingElement) := (f) -> ( -- rewrites a tensor as a shuffle polynomial in lyndon words
    d := #(gens ring f);
    k := degree f;
    ly := getSymbol("ly");
    var := new Array from apply(lyndonWords(d,k), i-> ly_i);
    cR := coefficientRing (ring f);
    R := cR var;
    pol := lyndonShuffleHelper(f,R);
    polh := standardForm pol;
    polh = applyKeys(polh, i-> applyKeys(i,j-> last baseName R_j));
    polh
)


