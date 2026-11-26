newPackage(
	"AInfinity",
        Version => "0.3",
        Date => "November 25, 2026",
        Authors => {{Name => "David Eisenbud", 
                  Email => "de@msri.org", 
                  HomePage => "http://www.msri.org/~de"},
	          {Name => "Mike Stillman",
                  Email => "mike@math.cornell.edu",
                  HomePage => "http://pi.math.cornell.edu/~mike"},
        },
	PackageExports => {"Complexes","DGAlgebras","PushForward","PrimaryDecomposition","LocalRings"},
        AuxiliaryFiles => true,
        Headline => "A-infinity structures, duals, and dualizing complexes",
	Keywords => {"Commutative Algebra"}
	)

export {
    "aInfinity",
    "burkeResolution",
    "golodBetti",
    "picture",
    "displayBlocks",
    "extractBlocks",
    "mapComponents",
    "mapComponents1",
    "adjacentBurkeComponents",
    "hasMinimalMult",
    "isGolodAInf",
    "isLocallyMinimal",
    "isLocallyAcyclic",
    "burkeDifferential",
    "golodRanks",
    "totalBetti",
    "oppositeAInfinityAlgebra",
    "dualAInfinityModule",
    "dualAInfinityStructure",
    "dualizingComplexViaAInfinity",
    "SAInfAlg", "AInfAlg", "SAInfMod", "AInfMod",
    "sAInfAlg", "aInfAlg", "sAInfMod", "aInfMod",
    "sAInfAlgModel", "sAInfModModel", "checkStasheff", "burkeModule",
    "TensorOrder",
    --symbols
    "Bounded",
    "Check",
    "ShowRanks",
    "VerifyComplex"
    }

checkLengthLimit = value -> (
    if value =!= null and (not instance(value,ZZ) or value < 0) then
        error "LengthLimit must be null or a nonnegative integer";
    value
    )

checkOrder = value -> (
    if value =!= infinity and (not instance(value,ZZ) or value < 1) then
        error "Order must be infinity or a positive integer";
    value
    )
freeResolutionAfterPositiveRegrading = N -> (
    -- The A-infinity construction only uses homological degrees.  Compute the
    -- same ungraded resolution after assigning degree one to every ambient
    -- variable, then transport its matrices back.  This handles gradings with
    -- no positive heft and the inhomogeneous ZZ resolution path.  It is
    -- globally over the original polynomial ring.
    S := ring N;
    T := newRing(S, Degrees => toList(numgens S : 1), DegreeRank => 1);
    C := freeResolution coker substitute(presentation N,T);
    if length C == 0 then complex {substitute(C_0,S)}
    else complex apply(length C, i -> substitute(C.dd_(i+1),S))
    )

originMap = S -> map(coefficientRing S, S, toList(numgens S : 0))

checkNotLocalRing = R -> (
    if toString class R === "LocalRing" then
        error "rings constructed by localRing are not supported";
    S := try ring presentation R else null;
    if S =!= null and toString class S === "LocalRing" then
        error "rings constructed by localRing are not supported";
    R
    )

checkGlobalCoefficientSetup = R -> (
    checkNotLocalRing R;
    S := ring presentation R;
    kk := coefficientRing S;
    if not isField kk and kk =!= ZZ then
        error "global A-infinity computations currently require a coefficient field or ZZ";
    R
    )

-- Reduction modulo the variables is well defined on R = S/I only when
-- I is contained in the ideal of the variables.
vanishesAtOrigin = R -> isSubset(ideal presentation R,ideal vars ring presentation R)

checkVanishesAtOrigin = R -> (
    checkNotLocalRing R;
    if not vanishesAtOrigin R then
        error "this operation is interpreted at the origin; expected the defining ideal to be contained in the ideal of the variables";
    R
    )

checkOriginSetup = R -> (
    checkVanishesAtOrigin R;
    if not isField coefficientRing ring presentation R then
        error "expected the coefficient ring to be a field";
    R
    )

checkGolodSetup = R -> (
    checkOriginSetup R;
    S := ring presentation R;
    if not isSubset(ideal presentation R,(ideal vars S)^2) then
        error "Golod tests require a minimal presentation at the origin; expected the defining ideal to be contained in the square of the ideal of the variables";
    R
    )

ambientModule = M -> (
    R := ring M;
    S := ring presentation R;
    phi1 := substitute(presentation M, S);
    coker(phi1 | target phi1 ** presentation R)
    )

isLocallyMinimal = method()
isLocallyMinimal Complex := Boolean => C -> (
    S := checkVanishesAtOrigin ring C;
    red := originMap S;
    all(toList(min C + 1 .. max C), i -> red C.dd_i == 0)
    )

isLocallyAcyclic = method()
isLocallyAcyclic Complex := Boolean => C -> (
    checkVanishesAtOrigin ring C;
    if C.dd^2 != 0 then return false;
    lo := min C;
    hi := max C;
    if hi - lo <= 1 then return true;
    all(toList(lo+1..hi-1), i ->
        not isSubset(ann(HH_i C),ideal vars ring C))
    )

-- function to check whether the multiplication
-- 
minimalHigherProducts = (mR,n) -> (
   red := originMap(mR#"baseRing");
   mults := select(keys mR,k -> class k === List and #k >= 2 and #k <= n);
   all(mults,k -> red(mR#k) == 0)
   )

hasMinimalMultAtOrder = (R,n) -> (
   checkGolodSetup R;
   mR := aInfinity(R,Order=>n);
   if mR#?"SResolution" and not isLocallyMinimal mR#"SResolution" then
       error "the cached resolution of R is not minimal at the origin; minimality-dependent A-infinity tests would be meaningless";
   minimalHigherProducts(mR,n)
   )

hasMinimalMult = method()
hasMinimalMult Ideal := Boolean => I -> hasMinimalMult(quotient I, 2)
hasMinimalMult (Ideal, ZZ) := Boolean => (I,n) -> hasMinimalMult(quotient I, n)
hasMinimalMult (Ideal,InfiniteNumber) := Boolean => (I,n) -> hasMinimalMultAtOrder(quotient I,n)
hasMinimalMult Ring := Boolean => R -> hasMinimalMult(R,2)
hasMinimalMult (Ring,InfiniteNumber) := Boolean => (R,n) -> hasMinimalMultAtOrder(R,n)
hasMinimalMult (Ring,ZZ) := Boolean => (R,n) -> hasMinimalMultAtOrder(R,n)

-- Return the ambient ideal of a k-rational point of the quotient, whether
-- the caller describes it before or after passing to R.
rationalModelPoint = (R,p) -> (
    checkNotLocalRing R;
    I := ideal presentation R;
    S := ring I;
    if not instance(S,PolynomialRing) or not isField coefficientRing S then
        error "expected a quotient of a polynomial ring over a field";
    P := if ring p === S then p
         else if ring p === R then I + ideal lift(gens p,S)
         else error "expected a point ideal in the quotient or its ambient polynomial ring";
    if P == ideal(1_S) then error "expected a proper point ideal";
    if not isSubset(I,P) then error "the selected point is outside the quotient";
    -- When all variables reduce to constants, S/P is precisely k because
    -- P is proper. A positive-dimensional or non-rational ideal fails here.
    if any(gens S,x -> #(support(x % P)) != 0) then
        error "expected a closed point rational over the coefficient field";
    P
)

-- At a rational point the completed regular ambient ring is Q[[z_1,..,z_r]],
-- where z_1,..,z_r are independent linear parts, lifted to relations, and
-- Q is the minimal regular ambient ring. Because these parameters act as
-- zero on R, tensoring a minimal Q-resolution with their Koszul complex
-- gives P_R^S(t) = (1+t)^r P_R^Q(t).
intrinsicGolodByBetti = (R,mR,r) -> (
    A := mR#"SResolution";
    S := ring A;
    residue := originMap S;
    ranks := apply(length A+2,i -> rank mutableMatrix(residue A.dd_i));
    p := apply(length A+1,i -> rank A_i-ranks#i-ranks#(i+1));
    p = take(p,1+last positions(p,j -> j != 0));
    for j from 1 to r do (
        if #p < 2 then error "inconsistent ambient Betti polynomial";
        q := new MutableList from take(p,#p-1);
        for i from 1 to #q-1 do q#i = p#i-q#(i-1);
        if last p != last q or any(q,u -> u < 0) then
            error "inconsistent ambient Betti polynomial under elimination";
        p = toList q;
    );
    e := numgens S-r;
    -- Burke, Corollary 6.10: for M=k it suffices to compare through
    -- 1+max(pd_Q R, pd_Q k-1). Both projective dimensions are finite.
    bound := 1+max(#p-1,e-1);
    kR := coker vars R;
    mK := aInfinity(mR,kR);
    predicted := new MutableList from toList(bound+1:0);
    incomingRank := 0;
    for i from 0 to bound do (
        predicted#i = binomial(e,i) +
            sum(for j from 1 to min(#p-1,i-1) list p#j*predicted#(i-j-1));
        -- Build only through the next differential so a strict inequality
        -- in low degree terminates without constructing the rest of the bar.
        terms := burkeData(mR,mK,i+1);
        outgoing := burkeDifferentialFromData(mR,mK,terms,i+1);
        outgoingRank := rank mutableMatrix(residue outgoing);
        actual := rank terms#i-incomingRank-outgoingRank;
        if actual != predicted#i then return false;
        incomingRank = outgoingRank;
    );
    true
)

isGolodAInfAtOrigin = R -> (
    checkOriginSetup R;
    I := ideal presentation R;
    S := ring I;
    if isSubset(I,(ideal vars S)^2) then (
        mR := aInfinity R;
        if isLocallyMinimal mR#"SResolution" then return minimalHigherProducts(mR,infinity);
        return intrinsicGolodByBetti(R,mR,0);
    );

    -- Polynomial elimination of c*x+h (x absent from h, c an invertible
    -- scalar) preserves the origin since every relation vanishes there.
    J := try minimalPresentation I else null;
    if J =!= null then (
        T := ring J;
        if instance(T,PolynomialRing) and coefficientRing T === coefficientRing S
           and isSubset(J,(ideal vars T)^2) then return isGolodAInfAtOrigin(T/J);
    );

    residue := originMap S;
    linearRank := rank(residue jacobian I);
    -- The displayed generators bound the local number of generators from above.
    -- Independent linear parts remove that many parameters after completion.
    if numgens I-linearRank <= 1 then return true; -- regular or hypersurface
    intrinsicGolodByBetti(R,aInfinity R,linearRank)
)

isGolodAInfAtPoint = (R,p) -> (
    P := rationalModelPoint(R,p);
    S := ring P;
    coordinates := apply(gens S,x -> x % P);
    if all(coordinates,a -> a == 0) then return isGolodAInfAtOrigin R;
    -- f(x+a) defines the translate whose origin is the requested point a.
    translate := map(S,S,apply(gens S,coordinates,(x,a) -> x+a));
    isGolodAInfAtOrigin(S/(translate ideal presentation R))
)

isGolodAInf = method()
isGolodAInf Ideal := Boolean => I -> isGolodAInfAtOrigin quotient I
isGolodAInf Ring := Boolean => R -> isGolodAInfAtOrigin R
isGolodAInf(Ring,Ideal) := Boolean => (R,p) -> isGolodAInfAtPoint(R,p)
isGolodAInf(Ideal,Ideal) := Boolean => (I,p) -> isGolodAInfAtPoint(quotient I,p)

burkeResolution = method(Options=>{Check => false})

zeroOrSum = (targetModule,sourceModule,blocks) ->
    if #blocks == 0 then map(targetModule,sourceModule,0) else sum blocks

globalBurkeModuleResolution = (M,limit) -> (
    R := ring M;
    S := ring presentation R;
    RS := map(R,S);
    homogeneousInput := isHomogeneous R and isHomogeneous M;
    positiveRegrading := (homogeneousInput and heft S === null)
        or (not homogeneousInput and coefficientRing S === ZZ);
    Mres := if homogeneousInput and not positiveRegrading then
        freeResolution(pushForward(RS,M),LengthLimit=>limit)
    else (
        if not homogeneousInput then checkGlobalCoefficientSetup R;
        N := ambientModule M;
        if positiveRegrading then freeResolutionAfterPositiveRegrading N
        else freeResolution(N,LengthLimit=>limit)
        );
    (Mres,positiveRegrading))

burkeResolution(Module, ZZ) := Complex => o-> (M,len) ->(
    --put the map components together into a complex.
    if len < 0 then error "resolution length must be nonnegative";
    if not instance(o.Check,Boolean) then error "Check must be a Boolean";
    R := ring M;
    checkNotLocalRing R;
    if len == 0 then (
        (C0,regraded) := globalBurkeModuleResolution(M,0);
        V := R ** C0_0;
        return complex(if V == 0 then V else labeler({0},V)));
    mA := aInfinity R;
    mG := aInfinity(mA,M);
    data := burkeData(mA,mG,len);
    C := complex for t from 1 to len list burkeDifferentialFromData(mA,mG,data,t);
    F:=labeledTensorComplex(R,C);
    if o.Check == true then
        assert (0 == F.dd^2 and all(length F -1 , i-> 0 == HH_(i+1) F));
    F
    )

aInfinity = method(Options => {LengthLimit => null, Order => infinity, Check => true})
aInfinity Ideal := HashTable => o -> I -> aInfinity(quotient I,o)
aInfinity Ring := HashTable => o -> R -> (
    --R should be a factor ring of a polynomial ring S
    --An S-free resolution A of R^1 is added to the cache table of R,
    --along with tensor powers of the truncated shifted resolution
    --B = A_+[-1].
    --The HashTable returned contains the A-infinity structure on B
    --m: B^(**n) --> B through total degree LengthLimit, which defaults
    --to 1+max B; Order bounds the arity.
    --the maps m are given on each summand:
    --m#{u_1..u_p}: B_(u_1)**..**B_(u_p) -> B_(sum u-1)

checkNotLocalRing R;
m := new MutableHashTable;
m#"ring" = R;
checkLengthLimit o.LengthLimit;
checkOrder o.Order;
S := ring presentation R;
resolutionKey := "AInfinity.resolution";
tensorKey := "AInfinity.tensorPowers";
homogeneousRing := isHomogeneous R;
positiveRegrading := (homogeneousRing and heft S === null)
    or (not homogeneousRing and coefficientRing S === ZZ);
if not R.cache#?resolutionKey then (
    if not homogeneousRing then checkGlobalCoefficientSetup R;
    N := coker presentation R;
    R.cache#resolutionKey = if positiveRegrading
        then freeResolutionAfterPositiveRegrading N
        else freeResolution N;
    );
A := R.cache#resolutionKey;
if ring A =!= S then
    error "the cached AInfinity resolution has the wrong base ring";
if A.dd^2 != 0 then
    error "the cached AInfinity resolution is not a complex";

if length A == 0 then
    B := complex({labeler({2},S^0)},Base => 2)
else if length A == 1 then
    B = complex({labeler({2},A_1)},Base => 2)
else B = labeledTensorComplex complex(
        apply(length A-1, i -> 
	map(A_(i+1), A_(i+2), -A.dd_(i+2))), 
        Base => 2);
m#"resolution" = B;
m#"SResolution" = A;
m#"baseRing" = S;
m#"positiveRegrading" = positiveRegrading;

limit := 1+max B;
if o.LengthLimit =!= null then limit = min(limit,o.LengthLimit);
if o.Order === infinity then order := limit // 2 else order = min(o.Order, limit // 2);
-- Every operation m#k with sum k <= limit and #k <= order is recorded, and
-- no other higher operation is.  aInfinity(HashTable,Module) and the dual
-- constructions rely on this to detect truncated input.
m#"lengthLimit" = limit;
m#"order" = order;
m#"requestedOrder" = o.Order;

-- Publish the bound and its tensor powers together, only after construction
-- succeeds. An interrupt must leave either the previous complete entry or no
-- entry, never an advanced bound with missing/incomplete powers. Ignore the
-- old two-key cache, which may already have been left inconsistent by Ctrl+C.
tensorCache := if R.cache#?tensorKey then R.cache#tensorKey else null;
if tensorCache === null or tensorCache#0 < limit then (
    tensorCache = {limit, hashTable for t from 1 to limit//2 list
        t => labeledTensorComplex(toList(t:B), LengthLimit => limit)};
    R.cache#tensorKey = tensorCache;
    );
BB := tensorCache#1;

--m#{u_1}
apply(1+length B , i-> m#{i+2} = B.dd_(i+2));

--m#{u_1,u_2}
if order >= 2 then(
B2 := BB#2; -- labeledTensorComplex({B,B}, LengthLimit => limit);
A0 := complex {A_0};
d1 := map(A_0, B_2, A.dd_1); --the positive sign is a literal interpretation of Burke
d1d1 := hashTable for i from min B to max B2 -2 list 
       i+2 => (d1**id_(B_i))*(B2_(i+2))^[{2,i}] - (id_(B_i)**d1)*(B2_(i+2))^[{i,2}];

D := map(labeledTensorComplex{A0,B},B2,d1d1, Degree => -2);
assert (isComplexMap D);
m0 := nullHomotopy(D, FreeToExact => true);
for i from 4 to limit do(
    K := (componentsAndIndices B2_i)#1;
    for k in K do (
	k' := {k_0+k_1-1};
	m#k = map(target (B_(i-1))^[k'], source (B2_i)_[k],
	       (B_(i-1))^[k']*m0_i*(B2_i)_[k]
	        )
	)
    ));

--m#{u_1..u_t}
--note: limit == max B + 1; 
for t from 3 to order do(
    Bt := BB#t;
    con := concentration Bt;
    -- The cached tensor powers may extend beyond the present limit (they
    -- can come from an earlier call with a larger one); only degrees up to
    -- limit are constructed, so that the output does not depend on history.
    for i from con_0 to min(con_1,limit) do(
	K := (componentsAndIndices Bt_i)#1;
	for k in K do (
	    U := select(algebraMapComponents k, v -> #v_3>1);
    	    dm := - sum(
		for v in U list (
		k1 := k_{0..v_1-1};
		k2 := k_{v_1..v_2};
		k3 := k_{v_2+1..#k-1};
		fac1 := tensor(S,apply(k1, ell -> B_ell));
		fac3 := tensor(S,apply(k3, ell -> B_ell));
	        v_0 * m#(v_3)*(fac1 ** (m#k2) ** fac3)
		)
	    );
	    mk := dm//B.dd_(i-1);
	    if o.Check == true and B.dd_(i-1)*mk != dm then(
	        <<"i = "<<i<<" k = "<<k<<endl;
	        error"dm failed to lift in aInfinity(Ring)"
	        );
            m#k = map(B_(i-1), target Bt_i^[k], mk)
    )));
hashTable pairs  m
)

    
aInfinity(HashTable, Module) := HashTable => o -> (mR, M) -> (
    --R = ring M should be a factor ring of a polynomial ring S
    --mR = aInfinity R an A-infinity structure on a resolution A of R
    --M an R-module
    --The HashTable returned contains the A-infinity structure on 
    --an S-free resolution of M up to stage n.
    
m := new MutableHashTable;
m#"module" = M;
R := ring M;
checkNotLocalRing R;
if mR#?"ring" and mR#"ring" =!= R then
    error "the supplied A-infinity algebra belongs to a different quotient ring";
B := mR#"resolution";
S := ring presentation R;
if ring B =!= S then
    error "the supplied A-infinity algebra has the wrong polynomial base ring";
checkLengthLimit o.LengthLimit;
checkOrder o.Order;
A := if mR#?"SResolution" then mR#"SResolution" else freeResolution coker presentation R;

(Mres,positiveRegrading) := globalBurkeModuleResolution(M,infinity);
	
G := labeledTensorComplex{Mres};
m#"resolution" = G;
m#"SResolution" = Mres;
m#"baseRing" = S;
m#"positiveRegrading" = positiveRegrading;
limit := if o.LengthLimit =!= null then o.LengthLimit else 1+max G;
order := if length A == 0 then 1
    else if o.Order === infinity then 1 + limit // 2
    else min(o.Order,1 + limit // 2);
-- The module identities through total degree limit and arity order use the
-- algebra operations m#k with sum k <= limit and #k <= order-1.  Those with
-- sum k > 1+max B have zero target, so only the remaining ones are required.
-- A missing required operation would silently be treated as zero below.
if mR#?"lengthLimit" and mR#"lengthLimit" < min(limit,1+max B) then
    error "the supplied A-infinity algebra was constructed to too small a LengthLimit for this module structure";
if mR#?"order" and mR#"order" < min(order-1,(1+max B)//2) then
    error "the supplied A-infinity algebra was constructed to too small an Order for this module structure";
m#"lengthLimit" = limit;
m#"order" = order;
m#"requestedOrder" = o.Order;

--m#u, #u=1
  apply(length G , i-> m#{i+1} = G.dd_(i+1));    

BBG := hashTable for t from 1 to limit//2 list t => labeledTensorComplex(toList(t:B)|{G}, LengthLimit => limit);
m#"BBG" = BBG;
----m#u, #u=2
if order >= 2 and limit >= 2 then (
    BG := BBG#1; -- labeledTensorComplex({B,G}, LengthLimit => limit);
    A0 := complex {A_0};
    d1 := map(A_0, B_2, A.dd_1); -- the positive sign should in any case be the same as in aInfinity Ring.
    dG := hashTable for i from min G to min(limit-2, max G) list
                  i+2 => (d1**id_(G_i))*(BG_(i+2))^[{2,i}];
    D := map(A0**G, BG, dG, Degree => -2);
    (lo,hi) := concentration D;
    if o.Check == true and not all(toList(lo+1..hi), i ->
        (A0**G).dd_(i-2)*D_i == D_(i-1)*BG.dd_i)
    then error "the binary module homotopy is not a map of complexes";

    m0 := nullHomotopy(D, FreeToExact => true);
    for i from 2 to min(limit,1+(concentration G)_1) do(
        K := (componentsAndIndices BG_i)#1;
        for k in K do (
    	k' := {k_0+k_1-1};
    	m#k = map(G_(i-1), source (BG_i)_[k],m0_i*(BG_i)_[k])
    	)
        );
    );

--m#u, u = {u_1..u_t,u_(t+1)}, t>=2.

for t from 2 to order-1 do (
Bt := BBG#t; --labeledTensorComplex (toList(2:B)|{G}, LengthLimit => limit);
    con := concentration Bt;
    -- Bt can have terms of total degree above limit; those are not constructed.
    for i from con_0 to min(con_1,limit) do(
	K := (componentsAndIndices Bt_i)#1;
	for k in K do (
	    U := select(mapComponents k, v -> #v_3>1); -- the case #v3 = 1 would be d m#k
    	    mkTerms := for v in U list (
		k1 := k_{0..v_1-1};
		k2 := k_{v_1..v_2};
		k3 := k_{v_2+1..#k-2};
		k4 := last k; -- this would be k_(#k-1)
		fac1 := tensor(S,apply(k1, ell -> B_ell));
		fac3 := tensor(S,apply(k3, ell -> B_ell));
		fac4 := G_k4;
		if v_2 == #k-1 then (
		   mMap1 := if not m#?(v_3)
                       then map(
                           G_(sum v_3 - 1),
                           tensor(S,apply(drop(v_3,-1),ell -> B_ell)|{G_(last v_3)}),
                           0)
                       else m#(v_3)_[v_3];
                   mMap2 := if not m#?k2
                       then map(
                           G_(sum k2 - 1),
                           tensor(S,apply(drop(k2,-1),ell -> B_ell)|{G_(last k2)}),
                           0)
                       else m#(k2)_[k2];
		   v_0 * mMap1 * (fac1 ** mMap2)
		)
		else (
		   mRmap := if not mR#?k2
                       then map(B_(sum k2 - 1),tensor(S,apply(k2, ell -> B_ell)),0)
                       else mR#(k2)_[k2];
                   mMap := if not m#?(v_3)
                       then map(
                           G_(sum v_3 - 1),
                           tensor(S,apply(drop(v_3,-1),ell -> B_ell)|{G_(last v_3)}),
                           0)
                       else m#(v_3)_[v_3];
		   v_0 * mMap * (fac1 ** mRmap ** fac3 ** fac4)
	        )
	    );
	    dm := - sum mkTerms;
	    mk := dm//G.dd_(i-1);
	    if o.Check == true and G.dd_(i-1)*mk != dm then(
  	        <<"i = "<<i<<" k = "<<k<<endl;
	        error"dm failed to lift in aInfinity(Module)"
	        );
            m#k = map(G_(i-1), target Bt_i^[k], mk)
        )));
hashTable pairs  m)

aInfinity Module := HashTable => o -> M ->(
    mR := aInfinity(ring M, LengthLimit => o.LengthLimit, Order => o.Order, Check => o.Check);
    aInfinity(mR, M, o)
)


burkeData = method()
burkeData(HashTable,HashTable,ZZ) := List => (mA,mG,n) ->(
--F = burkeData(mA,mG,n) 
--produces the list of the free modules indexed 0..n in the Burke resolution,
--using the already-computed A-infinity algebra and module resolutions.
B := mA#"resolution";
G := mG#"resolution";
BB := {G}|apply(n//2, i->labeledTensorComplex(toList(i+1:B)|{G}, LengthLimit => n));
C := apply(n+1, i-> select(apply(BB,b-> b_i), c -> c != 0));
apply(C, c -> if #c == 0 then (ring B)^0 else burkeLabeledDirectSum c))

burkeData(Module,ZZ) := List => (M,n) ->(
--F = burkeData(M,n) 
--produces the list of the free modules indexed 0..n in the Burke resolution,
--in a form that things like F_5^[{3,2,0}] work (this is the projection).
--S = ring presentation ring M.
mA := aInfinity ring M;
mG := aInfinity(mA,M, LengthLimit => n);
burkeData(mA,mG,n))

mapComponents = method()
mapComponents List := List => u -> (
    --this serves for construction of the A-infinity structure on the resolution of a module 
    --AND for the construction of the differentials in the burke resolution. 
    --Only the last line differs from the program algebraMapComponents,
    --which has no distinguished last factor.
    
    --u = {u_0..u_n}; for i<n, u_i represents a free module in B, the truncated, shifted res of R^1; 
    --u_n represents a free module in G, the S-resolution of the R-module M.
    --output is a list whose elements have the form 
    --{sign, p,q,{v_0..v_m}} corresponding to
    --a map with
    --target =  {v_0..v_m} = {u_0..u_(p-1), v_p, u_(q+1)..u_n}
    --that collapses
    --u_p..u_q to a single index v_p = -1+sum(for i from p to q list u_i), 
    --where sign is (-1)^sum(apply p, i->u_i).
    --We require also v_0..v_(m-1)>=2 and v_m>=0; otherwise this is not 
    --the index of a module in the resolution.

    if #u == 0 or not all(u, i -> instance(i,ZZ)) then
        error "expected a nonempty list of integer Burke indices";
    if not all(drop(u,-1), i -> i >= 2) or last u < 0 then
        error "expected algebra indices at least 2 and a nonnegative final module index";

    sign := p-> (-1)^(sum apply(p, i->u_i)); 
    n := #u-1;
    L0 := apply(n+1, p-> {sign p, p,p,u_{0..p-1}|{u_p-1}|u_{p+1..n}});
    L1 := flatten apply(n, p -> for q from p+1 to n list {sign p, p,q,u_{0..p-1}|
	                         {-1+sum for i from p to q list u_i}|
		                 u_{q+1..n}});
    L := L0|L1;
    select(L, LL -> all (drop(LL_3,-1), p -> p >= 2) and last LL_3 >= 0)
    )

algebraMapComponents = method()
algebraMapComponents List := List => u -> (
    --this version is for the construction of the A-infinity algebra structure on a truncated,
    --shifted resolution of R = S/I. Only the last line differs from the program mapComponents,
    --which has a distinguished last factor with a different lower bound
    
    --u = {u_0..u_n}; u_i represents a free module in B, the truncated, shifted res of R^1; 
    --output is a list whose elements have the form 
    --{sign, p,q,{v_0..v_m}} corresponding to
    --a map with
    --target =  {v_0..v_m} = {u_0..u_(p-1), v_p, u_(q+1)..u_n}
    --that collapses
    --u_p..u_q to a single index v_p = -1+sum(for i from p to q list u_i), 
    --where sign is (-1)^sum(apply p, i->u_i).
    --We require also v_0..v_(m-1)>=2; otherwise this is not 
    --the index of a module to which we can apply m. 
    
    sign := p-> (-1)^(sum apply(p, i->u_i)); -- indices from 0 to p-1
    n := #u;
    L0 := apply(n, p-> {sign p, p, p, u_{0..p-1}|{u_p-1}|u_{p+1..n-1}});
    L1 := flatten apply(n, p -> for q from p+1 to n-1 list 
	      {sign p, p,q,
		  u_{0..p-1}|{-1+sum for i from p to q list u_i}|u_{q+1..n-1}});
    L := L0|L1;
    select(L, LL -> all (last LL, p -> p >= 2))
    )

burkeComponentOperation = (mA,mG,u,p,q) -> (
    B := mA#"resolution";
    S := ring B;
    G := mG#"resolution";
    n := #u - 1;
    left := tensor(S,for i from 0 to p-1 list B_(u_i));

    if q < n then (
        right := tensor(S,for i from q+1 to n-1 list B_(u_i));
        left ** mA#(u_{p..q}) ** right ** G_(u_n)
        )
    else left ** mG#(u_{p..q})
    )

burkeDifferentialListFromData = (mA,mG,F,t) -> (
    sourceLabels := (componentsAndIndices F_t)#1;
    targetLabels := (componentsAndIndices F_(t-1))#1;

    flatten for u in sourceLabels list (
        pieces := select(mapComponents u, v -> member(v#3,targetLabels));
        for v in pieces list (
            operation := burkeComponentOperation(mA,mG,u,v#1,v#2);
            v#0 * map(
                F_(t-1),
                F_t,
                (F_(t-1))_[v#3] * operation * (F_t)^[u]
                )
            )
        )
    )

burkeDifferentialFromData = (mA,mG,F,t) ->
    zeroOrSum(F_(t-1),F_t,burkeDifferentialListFromData(mA,mG,F,t))

burkeDifferentialList = method()
burkeDifferentialList (HashTable,HashTable, ZZ) := List => (mA,mG,t) -> (
    if t < 1 then error "Burke differential index must be positive";
    F := burkeData(mA,mG,t);
    burkeDifferentialListFromData(mA,mG,F,t)
)

burkeDifferential = method()
burkeDifferential (HashTable,HashTable, ZZ) := Matrix => (mA,mG,t) -> (
    if t < 1 then error "Burke differential index must be positive";
    burkeDifferentialFromData(mA,mG,burkeData(mA,mG,t),t)
    )

mapComponents(HashTable, HashTable, ZZ) := List =>(mA,mG,len) ->(
   --The output is a list D_1..D_len
   --where D_t is a list of  the matrices of maps 
   --F_t ->comp(u,F_t) -> comp(v, F_(t-1) -> F_(t-1)
   --where comp(u,F_t) is the component of F_t labeled u
   --and similarly for v,F_(t-1).
   --Thus sum D_t will be the map F.dd_t in the Burke resolution.
   if len < 0 then error "Burke differential length must be nonnegative";
   F := burkeData(mA,mG,len);
   for t from 1 to len list burkeDifferentialListFromData(mA,mG,F,t)
)

adjacentBurkeComponents = method()
adjacentBurkeComponents(HashTable, HashTable, ZZ) := List => (mA,mG,i) -> (
    if i < 0 then error "Betti index must be nonnegative";
    F := burkeData(mA,mG,i+1);
    incoming := if i == 0 then (
        -- The zeroth incoming differential is F_0 -> 0.
        {map((ring F_0)^0,F_0,0)}
        )
    else burkeDifferentialListFromData(mA,mG,F,i);
    outgoing := burkeDifferentialListFromData(mA,mG,F,i+1);
    {incoming,outgoing}
    )

-- Backward-compatible name from the upstream development version.
mapComponents1 = method()
mapComponents1(HashTable, HashTable, ZZ) := List => (mA,mG,i) ->
    adjacentBurkeComponents(mA,mG,i)

tensor(Ring, List) := {} >> o -> (R,L) -> (
    --note that A**B**C**..**D = (((A**B)**C)**..**D) = tensor(R,{A..D}).
    -- Retain the displayed factor order, which matters for chain complexes.
    if L === {} then return R^1;
    T := fold(L,(A,B) -> A ** B);
    if ring T === R then T else tensor(R,T)
    )


componentsAndIndices = (F) -> (
    if F == 0 then ({},{})
    else if not F.cache.?components then (
        -- F has no components
        ({F}, {null})
        )
    else if #F.cache.components == 1 then (
        if F.cache.?indices then 
            ({F}, F.cache.indices)
        else 
            ({F}, {null})
        )
    else (
        a := for i from 0 to #F.cache.components-1 list (
            (cs,ks) := componentsAndIndices(F.cache.components#i);
            -- A plain labeled direct sum stores labels on the parent, while
            -- nested Burke summands can carry their own labels.
            if #cs == 1 and ks === {null} and F.cache.?indices then
                (cs,{F.cache.indices#i})
            else (cs,ks)
            );
        (flatten(a/first), flatten(a/last))
        )
    )



golodRanks = method()
golodRanks (Module,ZZ) := List => (M,b) ->(
    --M is a module over a factor ring R = S/I, S a polynomial ring.
    --implements the rational function as power series. 
    --See Avramov, six lectures, p.50.
    --Output is the sequence of ranks, equal to those in the free resolution
    --if and only if M is a Golod module.
    if b < 0 then error "the bound must be nonnegative";
    R := ring M;
    checkGolodSetup R;
    p := presentation R;
    S := ring p;
    RS := map(R,S);
    homogeneousInput := isHomogeneous R and isHomogeneous M;
    (G,A) := if homogeneousInput and heft S =!= null then
        (freeResolution prune pushForward(RS,M), freeResolution coker p)
    else (
        -- Without a positive heft the resolutions need not be minimal.
        resolve := if homogeneousInput
            then freeResolutionAfterPositiveRegrading else freeResolution;
        (G0,A0) := (resolve ambientModule M, resolve coker p);
        if not isLocallyMinimal A0 or not isLocallyMinimal G0 then
            error "golodRanks requires S-free resolutions of R and M that are minimal at the origin";
        (G0,A0)
        );
    -- If h(t) = g(t)/(1-a(t)), compare coefficients in h=g+a*h.
    -- a_j = rank A_(j-1), j>=2. Compute only degrees 0..b,
    -- with integer coefficients and no auxiliary polynomial ring.
    gRanks := apply(1+length G,i -> rank G_i);
    aRanks := apply(length A,i -> rank A_(i+1));
    ranks := new MutableList from toList(b+1:0);
    for i from 0 to b do (
        ranks#i = if i < #gRanks then gRanks#i else 0;
        for j from 2 to min(i,#aRanks+1) do
            ranks#i = ranks#i + aRanks#(j-2)*ranks#(i-j);
        );
    toList ranks
    )

labeler = method()
--only direct sum modules can be labeled
labeler(Thing,Module) := (L,F) -> directSum(1:(L=>F));
--note that in labeling a direct sum, the labels must be applied to the modules
--And when the direct sum is formed.

label = method()
label(Module) := Thing => M-> (indices M)_0
label(List) := List => L-> apply(L, M ->(indices M)_0)

burkeLabeledDirectSum = method()
burkeLabeledDirectSum List := Module => L ->(
    --L is a list of labeled modules.
    ciL := apply(L, M -> componentsAndIndices M);
    directSum flatten apply(ciL, ci -> apply(#ci_0, i->(ci_1_i => ci_0_i)))
    )

-- A simultaneous construction of the tensor powers B^i and B^i ** G could
-- avoid recomputing common factors, but the present interface builds one
-- labeled tensor complex at a time.
labeledTensorComplex = method(Options => {LengthLimit => null})
labeledTensorComplex List := Complex => o -> L -> (
    --Input is L = {C_0..C_(p-1)}, a list of Complexes. 
    --Returns the tensor product D of the C_i. If LengthLimit => N is given, then
    --the result is computed only up to homological degree N.
    --Each term is labeled, so that the tensor product of {(C_i)_(u_i)} is labeled u = {..u_i..}
    --ComponentsAndIndices applied to D_i gives the correct list of indices, and
    --thus picture D works; but ALSO D_i_[u] and D_i^[u] return the inclusion
    --and projection correctly. (Note that this requires "double" labeling.)
    
    checkLengthLimit o.LengthLimit;
    if #L == 0 or not all(L,C -> class C === Complex) then
        error "expected a nonempty list of complexes";
    S := ring L_0;
    if #L == 1 then (
	B := L_0;
	top := if o.LengthLimit === null then max B else min(max B,o.LengthLimit);
	if top < min B then error "tensor complex has no terms in the requested range";
	F := for i from min B to top list labeler({i},B_i);
	if top == min B then
            return complex({first F},Base => min B);
	return complex(
            for i from min B to top - 1 list
                map(F_(i-min B),F_(i+1-min B),B.dd_(i+1)),
	    Base => min B)
        );
    p := #L;
    Min := apply(L, C->min C);
    Max := apply(L, C->max C);
    limit := if o.LengthLimit =!= null then min(o.LengthLimit,sum Max) else sum Max;
    -- Enumerate only degree tuples meeting the individual factor bounds.
    -- Varying the last index first preserves the order used by compositions.
    lower := apply(p,j -> sum(take(Min,j+1)));
    upper := apply(p,j -> sum(take(Max,j+1)));
    local degreeTuples;
    degreeTuples = (j,d) -> (
        if j == 0 then return {{d}};
        lo := max(Min_j,d-upper_(j-1));
        hi := min(Max_j,d-lower_(j-1));
        flatten for a from lo to hi list apply(degreeTuples(j-1,d-a),u -> append(u,a))
        );
    modules := apply(max(0,limit - sum Min + 1), i ->(
	    com := degreeTuples(p-1,i+sum Min);
	    apply(com, co -> 
		    (co => labeler(co, 
			       tensor(S,apply(p, pp->(L_pp)_(co_pp))))))
	));
--note the (necessary) double labeling.
    modules = select(modules, tt-> #tt != 0);

suitable := v-> if min v == 0 then position (v, vv -> vv == 1) else null;
     -- v is a list of ZZ. returns null unless v has the form
     -- {0...0,1,0..0}, in which case it returns the position of the 1.
    if #modules == 0 then error "tensor complex has no terms in the requested range";
    if #modules == 1 then return complex({map(S^0,directSum(1:(modules_0_0)),0)}, Base => sum Min -1);
    
    d := for i from 0 to #modules-2 list(	
        map(directSum modules#i,
            directSum modules#(i+1),
            matrix table( -- form a block matrix
                modules#i, -- rows of the table
                modules#(i+1), -- cols of the table
                (j,k) -> ( -- j,k each have the form (List => Module)
		    indtar := j#0;
		    indsrc := k#0;		    
                    tar := j#1;
                    src := k#1;
		    p := suitable(indsrc - indtar);
                    m := map(tar, src, 
			if p === null then 0 
			else(
			    sign := (-1)^(sum(indsrc_(toList(0..p-1))));
			    phi := sign*(
			    (tensor(S, apply(p, q -> L_q_(indtar_q))))
			    **
			    (L_p).dd_(indsrc_p)
			    **
                            tensor(S, apply(#L-p-1, q -> L_(p+q+1)_(indtar_(p+q+1)))))
			))
		    ))));
     (complex(d,Base => sum Min))
     )
	       
labeledTensorComplex Complex := Complex => o -> C -> labeledTensorComplex({C},o)

labeledTensorComplex(Ring, Complex) := Complex => o -> (R,C)->(
    --preserve the labels on C while tensoring each free module with R.
    --NOTE that C must be a labeled complex!
    
    c := for i from min C to max C list componentsAndIndices C_i;
    if #c_0_1 > 0 and first(c_0_1) === null then
        error "input complex must have a labeled first term";
    D := for j from 0 to max C - min C list
	if #c_j_0 == 0 then R^0
	else if #c_j_0 == 1 then
	    -- This avoids a directSum bug for one component.
	    labeler(c_j_1_0,R ** c_j_0_0)
	else directSum apply(#c_j_0,i -> labeler(c_j_1_i,R ** c_j_0_i));
    complex (for i from 1 to max C - min C list map(D_(i-1),D_i,R**C.dd_(i+min C)), Base => min C)
    )

isComplexMap = D -> (
    A := source D;
    B := target D;
    deg := degree D;
    isWellDefined D;
    (lo,hi) := concentration A;
    all(toList(lo+1..hi), i-> --make sure D is a map of complexes!
        (B.dd_(i+deg)*D_i  ==  D_(i-1)*A.dd_i)
	))

eagonSymbols = method()
eagonSymbols(ZZ,ZZ) := List => (n,i) ->(
    --symbol of the module Y^n_i, as a list of pairs, defined inductively from n-1,i+1 and n-1,0
    --assumes large number of vars and pd.
    if n === 0 then return {(i,{})};
    if i === 0 then return eagonSymbols(n-1,1);
    e' := eagonSymbols (n-1,0);
    e'1 := apply (e', L -> L_1|{i});
    eagonSymbols(n-1,i+1)|apply (#e', j-> (e'_j_0,e'1_j))
    )
golodBetti0 = (F,G,b) ->(
    --F,G finite free complexes (resolutions) over a ring S.
    --Compute the Betti table of what should be the Eagon resolution of 
    --the module resolved by G over the ring resolved by F
    --up to step b.
    symbs := apply(b+1, n -> eagonSymbols(n,0));
    mods := apply(symbs, s -> 
	directSum apply(#s, 
	    i -> G_(s_i_0) ** tensor(ring F,apply(s_i_1,j -> F_j))));
   if b == 0 then betti complex {mods_0}
   else betti complex apply(b,i->map(mods_i,mods_(i+1),0))
   )


golodBetti = method()
golodBetti (Module,ZZ) := BettiTally => (M,b) ->(
    --case where M is a module over a factor ring R = S/I,
    --MS is the same module over S
    --F = res I
    --K = res MS
    if b < 0 then error "the bound must be nonnegative";
    R := ring M;
    checkGolodSetup R;
    p := presentation R;
    S := ring p;
    phi1 := substitute(presentation M, S);
    phi := phi1 | target phi1 ** p;
    if isHomogeneous R and isHomogeneous phi and heft S =!= null then(
        MS := prune coker phi;
        K := res MS;
        F := res coker p)
    else (
        ranks := apply(golodRanks(M,b), r -> lift(r,ZZ));
        return if b == 0 then betti complex {S^(ranks_0)}
            else betti complex apply(b, i -> map(S^(ranks_i), S^(ranks_(i+1)), 0)));
    golodBetti0(F,K,b)
    )

totalBetti = method()
totalBetti(Module, ZZ) := ZZ => (M,i) ->(
    if i < 0 then error "Betti index must be nonnegative";
    R := ring M;
    checkOriginSetup R;
    if M == 0 then return 0;
    if i == 0 then (
        red := originMap R;
        return numgens prune(red ** M)
        );
    mA := aInfinity R;
    mG := aInfinity(mA,M);
    F := burkeData(mA,mG,i+1);
    differentials := for t from i to i+1 list burkeDifferentialFromData(mA,mG,F,t);
    residue := originMap(ring F_i);
    ranks := apply(differentials, d -> rank mutableMatrix(residue d));
    numgens(F_i) - sum ranks
    )


-- Tensor product B_{u1} ** ... ** B_{ut}; returns S^1 for the empty word.
tensorBWord = (B,u) -> tensor(ring B, apply(u, i -> B_i));

pairDegreeSum = (u,f) -> (
    if #u < 2 then 0 else sum for i from 0 to #u-2 list (
        sum for j from i+1 to #u-1 list f(u#i,u#j)
    )
);

reverseTensorSign = u -> (
    -- If the arity correction is c_r, reversal of an A-infinity
    -- composition forces c_r = c_s + c_(r-s+1).  The binary convention
    -- here has c_2 = 1, hence c_r = r-1 in every arity.
    pairDegreeSum(u,(a,b) -> a*b) + max(0,#u-1)
);

reverseTensorPermutation = (S,L) -> (
    src := tensor(S,L);
    tar := tensor(S,reverse L);
    if #L <= 1 then return map(tar,src,if numgens src == 0 then 0 else 1);

    firstFactor := first L;
    reversedRest := reverse drop(L,1);
    flip(firstFactor,tensor(S,reversedRest))
        * (firstFactor ** reverseTensorPermutation(S,drop(L,1)))
);

reverseTensorMap = (B,u) -> (
    (-1)^(reverseTensorSign u) * reverseTensorPermutation(ring B, apply(u, i -> B_i))
);

dualTensorSign = u -> (
    -- Additivity is forced by nested module operations.  With
    -- s(v)=v(v+1)/2+1, one also has
    -- reverseTensorSign(u)=sum s(u_i)+sum(u)+s(sum(u)-1) mod 2,
    -- so the dual module identities match the opposite algebra identities.
    if #u == 0 then 0
    else sum apply(u,v -> (v*(v+1))//2 + 1)
);

-- All nonempty words in {2,...,maxB} with total sum <= maxSum.
bWords = (maxB,maxSum) -> (
    if maxSum < 2 then {} else flatten for a from 2 to min(maxB,maxSum) list (
        {{a}} | apply(bWords(maxB,maxSum-a), w -> prepend(a,w))
    )
);

withinDualAInfinityLengthLimit = (key,limit) ->
    limit === null or sum(key) <= limit;

-- Partial dualization:
-- f : E ** H --> K
-- returns f^vee_last : E ** dual K --> dual H
--
-- This assumes the usual Macaulay2 tensor-basis ordering used by AInfinity:
-- columns of E ** H are grouped first by a basis vector of E, then by H.
partialDualLast = (f,E,H,K) -> (
    if f === 0 then map(dual H, E ** dual K, 0)
    else adjoint'(flip(dual H, K) * adjoint(f,E,H), dual K, dual H)
);

-- The dual of a left A-infinity module is naturally a left module over
-- the opposite A-infinity algebra in the suffix convention used by Burke.
--
-- LengthLimit follows the AInfinity convention: it bounds the total degree
-- sum(key) of higher operations.  Unary differentials are always retained.
oppositeAInfinityAlgebra = method(Options => {LengthLimit => null});
oppositeAInfinityAlgebra HashTable := opts -> mR -> (
    limit := checkLengthLimit opts.LengthLimit;
    B := mR#"resolution";
    -- Preserve the non-operation metadata, including the resolution, ring,
    -- and base ring.  The recorded limit is the smaller of the source limit
    -- and the requested one.
    pairs := apply(select(keys mR, key -> class key =!= List and key =!= "lengthLimit"),
        key -> key => mR#key);
    if mR#?"lengthLimit" then
        limit = if limit === null then mR#"lengthLimit" else min(limit,mR#"lengthLimit");
    if limit =!= null then pairs = append(pairs,"lengthLimit" => limit);

    for key in keys mR do if class key === List then (
        if #key == 1 then pairs = append(pairs, key => mR#key)
        else if withinDualAInfinityLengthLimit(key,limit) then (
            revkey := reverse key;
            if not mR#?revkey then
                error("oppositeAInfinityAlgebra: missing reversed operation " | toString revkey);
            rho := reverseTensorMap(B,key);
            f := mR#revkey;
            if source f != target rho then
                error "oppositeAInfinityAlgebra: source/target mismatch";
            if target f != B_(sum(key)-1) then
                error "oppositeAInfinityAlgebra: operation has the wrong target";
            pairs = append(pairs, key => f * rho);
        );
    );

    hashTable pairs
);

-- Given mA = A-infinity module structure on A, build the dual A-infinity
-- module structure on A^vee[p], where p = length A.  This is a module over
-- oppositeAInfinityAlgebra(mR), so B-words are reversed before dualizing.
--
-- This low-level helper constructs only the module operations.  The public
-- dualAInfinityStructure method below also packages the opposite algebra and
-- records the normalization shift.
--
-- LengthLimit bounds sum(key) for higher dual module operations.  It does not
-- truncate the underlying dual complex or its unary differential.
dualAInfinityModule = method(Options => {LengthLimit => null});
dualAInfinityModule(HashTable,HashTable) := opts -> (mR,mA) -> (
    limit := checkLengthLimit opts.LengthLimit;
    A := mA#"resolution";
    B := mR#"resolution";
    p := length A;
    maxB := max B;

    -- H_j = dual A_{p-j}, j = 0,...,p.
    -- Differential H_j --> H_{j-1} is dual(A_{p-j+1} --> A_{p-j}).
    -- Label H_j by {j}, as expected by AInfinity's Burke assembler.
    Hmodules := for j from 0 to p list directSum(1:({j} => dual A_(p-j)));
    dH := for j from 1 to p list
        map(Hmodules_(j-1), Hmodules_j, matrix dual(mA#{p-j+1}));
    H := if p == 0 then complex {first Hmodules} else complex dH;

    pairs := {
        "resolution" => H,
        -- AInfinity module structures carry the represented R-module under
        -- "module".  Retain it for compatibility and name its role explicitly.
        "module" => mA#"module",
        "sourceModule" => mA#"module"
    };
    -- The source "order" and "lengthLimit" describe the source operations,
    -- so they are not copied.
    for key in {"baseRing","positiveRegrading"} do
        if mA#?key then pairs = append(pairs,key => mA#key);
    if limit =!= null then pairs = append(pairs,"lengthLimit" => limit);

    -- Add the ordinary differentials.
    pairs = pairs | for j from 1 to p list {j} => dH#(j-1);

    -- Original structure map:
    --     mA#{u1,...,ut,i0} : B_u ** A_i0 --> A_i
    -- where i = sum(u)+i0-1.
    --
    -- Opposite dual structure map:
    --     mH#{u1,...,ut,j} : B_u ** dual A_i --> dual A_i0
    -- where j = p-i.
    for j from 0 to p do (
        for u in bWords(maxB, p-j+1) do (
            su := sum u;
            i := p - j;
            i0 := i - su + 1;
            if i0 >= 0 and i0 <= p
                and withinDualAInfinityLengthLimit(append(u,j),limit)
            then (
                revu := reverse u;
                keyA := append(revu,i0);
                Erev := tensorBWord(B,revu);
                expectedSource := Erev ** A_i0;
                f := if mA#?keyA then mA#keyA else (
                    -- A missing operation is harmless only when its source or
                    -- target is zero, in which case the typed zero is forced.
                    if numgens expectedSource != 0 and numgens A_i != 0 then
                        error("dualAInfinityModule: incomplete input; missing module operation " | toString keyA);
                    map(A_i,expectedSource,0)
                );
                if f =!= 0 and (
                    source f != expectedSource or target f != A_i
                ) then error(
                    "dualAInfinityModule: source/target mismatch for module operation "
                    | toString keyA
                );
                raw := partialDualLast(f,Erev,A_i0,A_i);
                -- The dual sign is additive under concatenation.  Use the
                -- unsigned permutation here: including reverseTensorMap's
                -- sign and then cancelling it obscures this formula.
                rho := reverseTensorPermutation(ring B,apply(u, v -> B_v));
                g0 := (-1)^(dualTensorSign u) * raw * (rho ** dual A_i);
                src := tensor(ring B,apply(u, v -> B_v) | {H_j});
                tar := H_(su+j-1);
                g := map(tar,src,matrix g0);
                pairs = append(pairs, append(u,j) => g);
            );
        );
    );

    hashTable pairs
);

checkAInfinityAlgebraForDual = (mR,limit) -> (
    checkLengthLimit limit;
    if not mR#?"resolution" then
        error "dualAInfinityStructure: the algebra structure has no resolution";
    if not mR#?"ring" then
        error "dualAInfinityStructure: the algebra structure has no ring";

    B := mR#"resolution";
    checkNotLocalRing ring B;
    if min B != 2 then
        error "dualAInfinityStructure: expected the shifted algebra resolution to start in degree 2";

    -- The complete finite structure has one operation for every word whose
    -- output can lie in B.  Requiring those keys prevents a truncated input
    -- from being silently interpreted as having zero higher operations.
    maxB := max B;
    for u in bWords(maxB,maxB+1) do (
        src := tensorBWord(B,u);
        tar := B_(sum(u)-1);
        if withinDualAInfinityLengthLimit(u,limit)
            and numgens src != 0 and numgens tar != 0 and not mR#?u
        then
            error("dualAInfinityStructure: incomplete input; missing algebra operation " | toString u);
    );

    mR
);

checkAInfinityModuleForDual = (mR,mA,limit) -> (
    checkAInfinityAlgebraForDual(mR,limit);
    if not mA#?"resolution" then
        error "dualAInfinityStructure: the module structure has no resolution";
    if not mA#?"module" then
        error "dualAInfinityStructure: the module structure has no represented module";

    A := mA#"resolution";
    B := mR#"resolution";
    if min A != 0 then
        error "dualAInfinityStructure: expected the module resolution to start in degree 0";
    if ring A =!= ring B then
        error "dualAInfinityStructure: algebra and module resolutions have different base rings";
    if ring(mA#"module") =!= mR#"ring" then
        error "dualAInfinityStructure: the represented module is not over the algebra's quotient ring";

    p := length A;
    if p >= 1 then for i from 1 to p do
        if numgens A_i != 0 and numgens A_(i-1) != 0 and not mA#?{i} then
            error("dualAInfinityStructure: incomplete input; missing module differential " | toString {i});

    mA
);

-- dualAInfinityStructure(mR,mA) returns the left A-infinity module structure
-- on A^vee[p] over the opposite algebra.  The returned hash table contains:
--   "algebra"            the opposite A-infinity algebra;
--   "resolution"         H_j = dual A_(p-j), indexed from 0 through p;
--   "normalizationShift" d-p, so H[-(d-p)] has normalized degrees d-i;
--   list keys             the dual A-infinity module operations.
--
-- The A-infinity indices deliberately remain nonnegative for compatibility
-- with AInfinity's Burke assembler.  normalizationShift records, but does
-- not apply, the later dualizing normalization.
--
-- Here LengthLimit has the AInfinity operation-degree meaning.  It is
-- unrelated to the normalized homological endpoint used by the option of the
-- same name on dualizingComplexViaAInfinity.
dualAInfinityStructure = method(Options => {LengthLimit => null});

dualAInfinityStructure(HashTable,HashTable) := opts -> (mR,mA) -> (
    limit := checkLengthLimit opts.LengthLimit;
    checkAInfinityModuleForDual(mR,mA,limit);
    mRop := oppositeAInfinityAlgebra(mR,LengthLimit=>limit);
    mDual0 := dualAInfinityModule(mR,mA,LengthLimit=>limit);
    -- The normalization uses the polynomial ring S, not ambient R: for a
    -- tower R = (S/I)/J, ambient R is S/I.
    pairs := apply(keys mDual0, key -> key => mDual0#key);
    pairs = pairs | {
        "algebra" => mRop,
        "ring" => mR#"ring",
        "normalizationShift" => dim ring mR#"resolution" - length mA#"resolution"
    };
    hashTable pairs
);

-- With one algebra input, construct and dualize its regular left module.
dualAInfinityStructure HashTable := opts -> mR -> (
    limit := checkLengthLimit opts.LengthLimit;
    -- Dual operations of small total degree can come from source-module
    -- operations near the top of its resolution.  Constructing the regular
    -- module therefore requires the complete finite source algebra even when
    -- the requested dual output is truncated.
    checkAInfinityAlgebraForDual(mR,null);
    R := mR#"ring";
    mA := aInfinity(mR,R^1);
    dualAInfinityStructure(mR,mA,LengthLimit=>limit)
);

-- With Bounded => true, the output is the bounded truncation
-- tau_{<=p} Burke(A^vee[p]), p = pdim_S R, shifted to normalized degrees;
-- its top term is generally a nonfree cokernel.
-- With Bounded => false, LengthLimit => n returns the free Burke model
-- through normalized homological degree n.
-- This is the ungraded normalization RHom_S(R,S)[dim S]; no internal
-- canonical-module twist is added for a graded presentation.
--
-- A finite free prefix generally has spurious homology in its terminal degree:
-- it records the dualizing model only through the requested prefix and is not,
-- by itself, a bounded dualizing complex.
dualizingComplexViaAInfinity = method(Options => {
    Bounded => true,
    LengthLimit => null,
    Check => true,
    VerifyComplex => true
});

checkDualizingOptions = opts -> (
    if opts.Bounded =!= true and opts.Bounded =!= false then
        error "Bounded must be a Boolean";
    if opts.LengthLimit =!= null
        and (not instance(opts.LengthLimit,ZZ) or opts.LengthLimit < 0)
    then error "LengthLimit must be null or a nonnegative homological degree";
    if opts.Bounded and opts.LengthLimit =!= null then
        error "LengthLimit is only available with Bounded => false";
    if not opts.Bounded and opts.LengthLimit === null then
        error "Bounded => false requires LengthLimit => n";
    if opts.Check =!= true and opts.Check =!= false then
        error "Check must be a Boolean";
    if opts.VerifyComplex =!= true and opts.VerifyComplex =!= false then
        error "VerifyComplex must be a Boolean";
);

checkDualizingAmbient = S -> (
    -- Broader coefficient rings are deferred until Macaulay2 supplies the
    -- needed resolution machinery.  They would also require a known
    -- normalized dualizing complex for the coefficient ring; S[dim S] is
    -- not valid for an arbitrary base.
    kk := coefficientRing S;
    if not isField kk and kk =!= ZZ then
        error "dualizingComplexViaAInfinity currently requires a polynomial ambient ring over a field or over ZZ";
    S
);

dualizingComplexViaAInfinity PolynomialRing := opts -> S -> (
    checkDualizingOptions opts;
    checkDualizingAmbient S;
    if not opts.Bounded and opts.LengthLimit < dim S then
        error("LengthLimit must be at least the first normalized homological degree " | toString dim S);
    (complex {S^1})[-dim S]
);

dualizingComplexViaAInfinity Ideal := opts -> I ->
    dualizingComplexViaAInfinity(quotient I,opts);

dualizingComplexViaAInfinity QuotientRing := opts -> R -> (
    checkDualizingOptions opts;
    S := checkDualizingAmbient ring presentation R;
    d := dim S;
    if 1_R == 0_R then return (complex {R^0})[-d];

    -- A-infinity algebra structure on A_+[1].
    mR := aInfinity(R, Check => opts.Check);

    -- A-infinity module structure on the S-free resolution A of R.
    -- Here R^1, viewed as an R-module, pushes forward to R as an S-module.
    mA := aInfinity(mR, R^1, Check => opts.Check);

    A := mA#"resolution";
    p := length A;

    -- Construct the packaged dual A-infinity structure.
    mH := dualAInfinityStructure(mR,mA);
    mRop := mH#"algebra";
    lo := mH#"normalizationShift";
    if not opts.Bounded and opts.LengthLimit < lo then
        error("LengthLimit must be at least the first normalized homological degree " | toString lo);

    -- In bounded mode, degree p+1 supplies the map used to form the top
    -- cokernel.  In free mode, LengthLimit is a normalized output degree, so
    -- subtract the normalization shift to obtain the Burke degree.
    -- The A-infinity data above are already complete for arbitrary prefixes:
    -- operations with larger total degree have zero target in the finite
    -- complexes B and H.
    burkeTopDegree := if opts.Bounded then p+1 else opts.LengthLimit-lo;
    F := burkeData(mRop,mH,burkeTopDegree);
    diffs := for t from 1 to burkeTopDegree list (
        dS := burkeDifferentialFromData(mRop,mH,F,t);
        map(R ** target dS, R ** source dS, substitute(matrix dS,R))
        );

    if opts.VerifyComplex and burkeTopDegree >= 2 then (
        for n from 1 to burkeTopDegree-1 do (
            if diffs#(n-1) * diffs#n != 0 then
                error("the Burke maps do not square to zero on normalized homological degree " | toString(n+1+lo));
        );
    );

    if not opts.Bounded then (
        if burkeTopDegree == 0 then
            return (complex {R ** (mH#"resolution")_0})[-lo];
        return (complex diffs)[-lo];
    );

    -- Since A^vee[p] has no homology above degree p, Burke(A^vee[p])
    -- is acyclic above p.  Replace the tail C_{p+1}->C_p by its cokernel.
    dtop := diffs#p;       -- C_{p+1} --> C_p
    if p == 0 then return (complex {coker dtop})[-lo];

    dp := diffs#(p-1);     -- C_p --> C_{p-1}
    top := coker dtop;

    -- The map C_p -> C_{p-1} descends to coker(dtop).
    dpbar := map(target dp, top, dp);

    boundedDiffs := (for n from 1 to p-1 list diffs#(n-1)) | {dpbar};

    (complex boundedDiffs)[-lo]
);
load "./AInfinity/labels.m2"
load "./AInfinity/structures.m2"
load "./AInfinity/models.m2"
load "./AInfinity/typed-burke.m2"
load "./AInfinity/blocks.m2"
load "./AInfinity/point-golod.m2"
load "./AInfinity/point-burke.m2"
load "./AInfinity/table-conversions.m2"

beginDocumentation()
load "./AInfinity/typed-docs.m2"
load "./AInfinity/merged-docs.m2"

doc ///
Key
  AInfinity
Headline
  A-infinity structures, dual modules, and dualizing complexes
Description
  Text
   Following Jesse Burke's paper "Higher Homotopies and Golod Rings",
   given a polynomial ring S and a factor ring R = S/I and an R-module X,
   we compute (finite) A-infinity algebra structure mR on an S-free resolution of R
   and the A-infinity mR-module structure on an S-free resolution of X, and use them to
   give a finite computation of the maps in an R-free resolution of X that we call the
   Burke resolution.
  Text
   The package also constructs opposite A-infinity algebras, dualizes left
   A-infinity modules to left modules over the opposite algebra, and uses this
   construction to compute normalized dualizing complexes.  Higher operations
   may be truncated by total operation degree without truncating the underlying
   free complex.
  Text
   Complete tables from @TO aInfinity@ can also be used with @TO sAInfAlg@ and
   @TO sAInfMod@. These constructors give typed structures whose identities can
   be checked with @TO checkStasheff@.
  Text
   Here is an example with the simplest Golod non-hypersurface in 3 variables
  Example
   S = ZZ/101[a,b,c]
   R = S/(ideal(a)*ideal(a,b,c))
   mR = aInfinity R;
   res coker presentation R
   mR#{2,2}
  Text
   Given a module X over R, Jesse Burke constructed a possibly non-minimal R-free resolution
   of any length from the finite data mR and mX:
  Example
   X = coker vars R
   A = betti burkeResolution(X,8)   
   B = betti res(X, LengthLimit => 8)
   A == B
Contributors
  Karl Schwede
SeeAlso
 aInfinity
 oppositeAInfinityAlgebra
 dualAInfinityStructure
 dualizingComplexViaAInfinity
///

doc///
Key
 aInfinity
 (aInfinity, Ideal)
 (aInfinity, Ring)
 (aInfinity, Module)
 (aInfinity, HashTable, Module)
 [aInfinity, LengthLimit]
 [aInfinity, Check]
 [aInfinity, Order]
Headline
 aInfinity algebra and module structures on free resolutions
Usage
 mR = aInfinity R
 mR = aInfinity I
 mX = aInfinity(mR, X)
 mX = aInfinity X
Inputs
 R:Ring
  of the form S/I, where S is a polynomial ring
 I:Ideal
  in a polynomial ring; this is shorthand for aInfinity(quotient I)
 mR:HashTable
  output of aInfinity R
 X:Module
  an R-module whose ambient free resolution receives the module structure
 LengthLimit => ZZ
  Construct A-infinity structure to specified nonnegative homological degree
 Check => Boolean
  Verifies that the lifts in the construction were successful
 Order => ZZ
  A positive integer or infinity (the default). Restrict the arity of A-infinity structures produced. For module structures, the module input is included in this arity.
Outputs
 mR:HashTable
  A-infinity algebra structure on res coker presentation R
 mX:HashTable
  A-infinity module structure over mR on res pushForward(map(R,S),X)
Description
 Text
   Given an S-free resolution of  R = S/I, set B = A_+[1] (so that B_m = A_(m-1) for m >= 2, B_i = 0 for i<2),
   and differentials have changed sign.
   
   The A-infinity algebra structure 
   is a sequence of degree -1 maps 
   
   mR#u: B_(u_1)**..**B_(u_t) -> B_(sum u -1), for sum u <= 2 + (pd_S R), and thus,
   since each u_i>= 2, for t <= 1 + (pd_S R)//2.
   
   where u is a List of integers \geq 2, such that
   
   mR#{v}: B_v -> B_(v-1) is the differential of B,
   
   mR#{v_1,v_2} is the multiplication (which is a homotopy B**B \to B lifting the degree -2 map
   d**1 - 1**d: B_2**B_2 \to B_1 (which induces 0 in homology)
       
   mR#u for n>2 is a homotopy for the negative of the sum of degree -2 maps of the form
   (+/-) mR(1**...** 1 ** mR ** 1 **..**),
   inserting m into each possible (consecutive) sub product, and i = 2...n-1.
   Here m_1 represents the differential both of B and of B^(**n).
   
   Given mR, a similar description holds for the A-infinity module structure mX on the
   S-free resolution of an R-module X. The convenience overload aInfinity X first
   constructs aInfinity(ring X), forwarding LengthLimit, Check, and Order to both
   the algebra and module constructions.
   
   With the optional argument LengthLimit => n, only those A-infinity maps are constructed that
   would be used to compute the resolution of a module of projective dimension n-1:
   the higher operations recorded are exactly those mR#u with sum u <= n (and at most
   the requested Order), independently of earlier computations.  The limit actually used
   is stored under the key "lengthLimit".  A module structure aInfinity(mR,X) refuses an
   algebra mR that was truncated below what the module structure requires.
 Example
   S = ZZ/101[a,b,c]
   R = S/(ideal(a)*ideal(a,b,c))
   mR = aInfinity R;
   keys mR
   res coker presentation R
   mR#"resolution"
   mR#{2,2}
   X = coker map(R^2,R^{2:-1},matrix{{a,b},{b,c}})
   mX = aInfinity(mR,X)
 Text
   Jesse Burke showed how to use mR,mX to make an R-free resolution
 Example
   betti burkeResolution(X,8)   
   betti res(X, LengthLimit =>8)
   Y = image presentation X
   burkeResolution(Y,8)
 Text
   The same construction works for positively graded rings with non-standard weights.
 Example
   S = ZZ/101[a,b,c, Degrees => {1,2,3}]
   R = S/(ideal(a^2)*ideal(a,b,c))
   M = coker vars R
   F = burkeResolution(M,4)
   isHomogeneous F
 Text
   If a homogeneous presentation has no positive heft (for example, because
   a variable has degree zero), the construction temporarily assigns
   degree one to every ambient variable, computes a globally exact free
   resolution, and transports its matrices back to S.  Only the auxiliary
   grading changes; the ring, module, and returned A-infinity maps do not.
 Example
   S = QQ[a,b, Degrees => {0,1}]
   R = S/((a^2-a)*ideal(a,b))
   mR = aInfinity R
   mR#"positiveRegrading"
   F = burkeResolution(R^1,3,Check => true)
   F.dd^2
SeeAlso
 burkeResolution
 oppositeAInfinityAlgebra
 dualAInfinityStructure
References
 Jesse Burke, Higher Homotopies and Golod Rings. arXiv:1508.03782v2, October 2015.
Caveat
  @TO aInfinity@ computes globally over the polynomial ambient ring. A Burke
  resolution at a rational point can be requested with @TT "burkeResolution(M,n,p)"@.
  Rings produced by @TT "localRing"@ are not supported.
  Inhomogeneous input currently requires a coefficient field or @TT "ZZ"@.
  Origin-local Betti numbers further require field coefficients and
  @TT "I subset (ideal vars S)"@. The presentation-specific methods
  @TO hasMinimalMult@, @TO golodRanks@, and @TO golodBetti@ require
  @TT "I subset (ideal vars S)^2"@. For the intrinsic property of a
  ring at a rational point use @TO isGolodAInf@.
///

doc ///
Key
 mapComponents
 (mapComponents,List)
 (mapComponents,HashTable,HashTable,ZZ)
 adjacentBurkeComponents
 (adjacentBurkeComponents,HashTable,HashTable,ZZ)
 mapComponents1
 (mapComponents1,HashTable,HashTable,ZZ)
Headline
 describe or construct the signed components of Burke differentials
Usage
 pieces = mapComponents u
 blocks = mapComponents(mR,mM,len)
 adjacent = adjacentBurkeComponents(mR,mM,i)
 adjacent = mapComponents1(mR,mM,i)
Inputs
 u:List
  a nonempty integer word: every algebra index is at least 2 and the final module index is nonnegative
 mR:HashTable
  an A-infinity algebra returned by aInfinity
 mM:HashTable
  an A-infinity module over mR
 len:ZZ
  the nonnegative index of the last differential to construct; zero returns an empty list
 i:ZZ
  a nonnegative homological degree
Outputs
 pieces:List
  signed consecutive collapses of u, each recorded as a sign, two positions, and a target label
 blocks:List
  for each degree from 1 through len, the list of embedded block maps whose sum is the Burke differential
 adjacent:List
  the component lists for the incoming and outgoing differentials at degree i
Description
 Text
  For a label u, this returns the signed combinatorial terms occurring in
  the A-infinity module identity.  Given mR and mM, it assembles those terms
  into the block maps of the Burke differentials.  All block modules are
  constructed from the supplied A-infinity data, so their base ring agrees
  with the base rings of mR and mM.
 Text
  @TT "mapComponents1(mR,mM,i)"@ is retained for compatibility with the
  upstream development version.  It is an alias for
  @TT "adjacentBurkeComponents(mR,mM,i)"@.  At i = 0, the incoming component
  list consists of the zero map from F_0 to the zero module.
 Example
  S = QQ[x]
  R = S/ideal(x^3)
  mR = aInfinity R
  mM = aInfinity(mR,coker vars R)
  mapComponents {2,1}
  blocks = mapComponents(mR,mM,2);
  sum(blocks#0) == burkeDifferential(mR,mM,1)
  adjacentBurkeComponents(mR,mM,1) == mapComponents1(mR,mM,1)
SeeAlso
 burkeDifferential
 burkeResolution
 picture
///

doc ///
Key
 burkeResolution
 (burkeResolution, Module, ZZ)
 [burkeResolution,Check]
Headline
 compute a resolution from A-infinity structures
Usage
 F = burkeResolution(M,len)
Inputs
 M:Module
  over a quotient ring R = S/I
 len:ZZ
  last homological degree to include, a nonnegative integer
 Check => Boolean
Outputs
 F:Complex
  free complex over R in homological degrees zero through len
Description
  Text
   This call returns the global Burke resolution of M through degree len.
   It uses the same construction for every choice made with @TT "setMaxIdeal"@.
   At length zero it returns only the free term F_0, with the same shifts and
   summand labels as in a longer prefix. The augmentation to M is understood.
   For a minimal regular local presentation and minimal ambient resolutions,
   the Burke resolution is minimal precisely when M is a Golod module.
   If the optional argument Check => true, the program checks that the
   differential squares to zero and that positive homology vanishes below
   the last computed degree. The default is Check => false.
  Example
   S = ZZ/101[x_1..x_4]
   I = x_1^2*ideal(vars S)
   R = S/I
   M = R^1/ideal(x_1..x_3)
   F = burkeResolution(M, 4, Check =>true)
  Text
   If one only wants a single differential, use burkeDifferential
   instead, but one must construct the Ainfinity structures separately.
   The matrix over the ambient ring induces the corresponding Burke differential over R.
  Example
   mR = aInfinity R
   mG = aInfinity(mR,M)
   burkeDifferential(mR,mG,5)
   burkeResolution(M, 5, Check =>true)
  Text
   the function golodBetti displays the Betti table of the resolution
   that would be constructed by burkeResolution, without actually making the construction.
  Example
   golodBetti (M,12)
   betti F
  Text
   The advantage of resolutions computed from A-infinity structures
   is the decomposition of the differential into blocks corresponding
   to tensor products of the modules in the finite resolutions. In the 
   following display, the symbol {u_1..u_n} denotes B_(u_1)**..**B_(u_(n-1))**G_(u_n),
   where G is the S-free resolution of M and B is the truncated shift of the S-free resolution
   A of R^1: that is, B_i = A_(i-1), i = 2...length A.
  Example
   picture F
  Text
   the functions displayBlocks and extractBlocks allow the examination of these submatrices.
  Example
   displayBlocks F.dd_2
   extractBlocks(F.dd_4, {{2,1}},{{3,1},{2,2}})
References
 Jesse Burke, Higher Homotopies and Golod Rings. arXiv:1508.03782v2, October 2015.
SeeAlso
 aInfinity
 burkeDifferential
 golodBetti
 picture
 extractBlocks
 displayBlocks
Subnodes
 Check
 picture
 displayBlocks
 extractBlocks
Caveat
  To resolve the stalk at a rational point, pass its ideal p as a third
  argument, as in @TO (burkeResolution,Module,ZZ,Ideal)@. Homogeneous input without a positive heft
  is temporarily regraded and transported back. Rings produced by
  @TT "localRing"@ are not supported.
///

doc ///
Key
 burkeDifferential
 (burkeDifferential, HashTable, HashTable, ZZ)
Headline
 construct one ambient Burke differential
Usage
 d = burkeDifferential(mR,mM,t)
Inputs
 mR:HashTable
  an A-infinity algebra returned by aInfinity R
 mM:HashTable
  a compatible A-infinity module over mR, with all operations needed at degree t
 t:ZZ
  a positive homological degree
Outputs
 d:Matrix
  the map F_t to F_(t-1) over the ambient polynomial ring S
Description
 Text
  The source and target are labeled free S-modules. Base changing the matrix
  to R gives the corresponding differential of burkeResolution(M,t).
  This method has no Check option; verification options belong to burkeResolution.
 Example
  S = QQ[x,y]
  R = S/ideal(x^2)
  M = coker vars R
  mR = aInfinity R
  mM = aInfinity(mR,M)
  d = burkeDifferential(mR,mM,2)
  ring d === S
  entries(R**d) == entries (burkeResolution(M,2)).dd_2
SeeAlso
 aInfinity
 burkeResolution
 mapComponents
 adjacentBurkeComponents
///

doc ///
Key
 Check
Headline
 Option for burkeResolution
Usage
 F := burkeResolution(M, len, Check => true)
Inputs
 M:Module
 len:ZZ
  last homological degree to include, a nonnegative integer
 Check:Boolean
Outputs
 F:Complex
Description
  Text
   With Check => true, burkeResolution checks @TT "F.dd^2 == 0"@ and
   verifies vanishing of positive homology below the final computed degree.
   The explicit point overload checks interior homology after localization at the
   specified point. Degrees zero and one have no interior positive homology to test.
   The default value is false.
  Example
   R = ZZ/101[a,b,c]/(ideal(a,b,c))^2
   M = coker vars R
   elapsedTime burkeResolution(M, 7, Check => false)
   elapsedTime burkeResolution(M, 7, Check => true)
Caveat
 The check can be expensive. It does not verify the augmentation
 @TT "HH_0(F) = M"@, and a finite prefix is not expected to be acyclic in
 its terminal degree.
SeeAlso
 burkeResolution
///

doc ///
Key
 picture
 (picture, Complex)
 (picture, Matrix)
 (picture, Module)
 ShowRanks
 [picture, ShowRanks]
Headline
 displays information about the blocks of a map or maps between direct sum modules
Usage
 picture F
 picture m
 picture M
Inputs
 F:Complex
 m:Matrix
  of map between labeled direct sum modules
 M:Module
  a labeled directSum module
Description
  Text
   The sources and targets of the differentials in F = burkeResolution(M,n), where
   M is an R = S/I-module, are direct sums
   whose summands are labeled, each by a List of ZZ corresponding
   to a tensor product of components of the S-free resolutions of R and M.
   If ShowRanks is true, then the rank of the corresponding summand is also
   displayed.
      
   The maps in the A-infinity structures are similarly labeled (each one has a source
   that has just one summand.)
   
   When applied to such a map, picture prints it as a table, with columns labeled with
   the symbols associated to the source and rows labeled with the symbols associated to the target.
   When applied to a complex, the output is a "netList" display of the pictures of each of the maps.
  Example
   R = ZZ/101[a,b,c,d]/ideal"a3,a2b2,b4,c4,d2"
   F = burkeResolution(coker vars R, 4)
   picture F.dd_3
   picture F
  Text
   The possible symbols in the table produced by picture are:
   
   . if the corresponding matrix is zero
   * if the corresponding matrix is nonzero
   (number) if the entries of the corresponding matrix contain a unit,
            the rank of the matrix tensored with the residue field is displayed
   id if the corresponding matrix is the identity matrix 
SeeAlso
 burkeResolution
 aInfinity
///


doc ///
    Key
     golodBetti
     (golodBetti, Module, ZZ)
   Headline
    list the ranks of the free modules in the resolution of a Golod module
   Usage
    B = golodBetti(M,b)    
   Inputs
    M:Module
     R-module
    b:ZZ
     nonnegative homological degree to which to carry the computation
   Outputs
    B:BettiTally
     This would be betti table of the free res of M over R, if M were a Golod module over R
   Description
    Text
     Let S be a polynomial ring and let R = S/I be minimally presented at
     the origin, so @TT "I subset (ideal vars S)^2"@. A module M over R is Golod if
     the resolution H of M has maximal betti numbers given the
     betti numbers of the S-free resolutions F of R and K of M. This resolution, H,
     has underlying graded module H = R**K**T(B), where B is the truncated resolution
     F_1 <- F_2... and T(B) is the tensor algebra.
     
     Since the component modules of H are given, the computation only requires the computation of
     the minimal S-free resolution of M, and then is purely numeric;
     the differentials in the R-free resolution of M are not computed.
     
     For inhomogeneous input, the computation is allowed only when the
     presentation of R and the S-free resolutions used for R and M are
     minimal at the origin. The returned table records the predicted total
     ranks; its internal-degree rows are auxiliary and have no invariant
     meaning for inhomogeneous input. Use @TO golodRanks@ when only the
     invariant rank sequence is wanted.
     
     In case M = coker vars R, the result is the Betti table of the Golod-Shamash-Eagon 
     resolution of the residue field.
     
     We say that M is a Golod module (over R) if the ranks of the free modules in a minimal R-free resolution
     of M are equal to the numbers produced by golodBetti. Theorems of Levin and Lescot assert that if
     R has a Golod module, then R is a Golod ring; and that if R is Golod, then the d-th syzygy
     of any R-module M is Golod for all d greater than or equal to the projective dimension
     of M as an S-module (more generally, the co-depth of M) (Avramov, 6 lectures, 5.3.2).
     
    Example
     S = ZZ/101[a,b,c]
     I = (ideal(a,b,c^2))^2
     F = res(S^1/I)
     R = S/I
     F = burkeResolution (coker vars R, 6)
     golodBetti(coker vars R,6)
     betti res (coker vars R, LengthLimit => 6)
     betti F
   SeeAlso
    burkeResolution
   Caveat
    Text
     The quotient must satisfy @TT "I subset (ideal vars S)^2"@; the method
     does not automatically minimize a nonminimal regular presentation. For
     inhomogeneous input only the column totals are invariant. The displayed
     internal-degree rows are auxiliary; use @TO golodRanks@ for totals.
///

doc ///
    Key
     golodRanks
     (golodRanks, Module, ZZ)
   Headline
    list the total ranks predicted for a Golod module
   Usage
    L = golodRanks(M,b)
   Inputs
    M:Module
    b:ZZ
     nonnegative homological degree to which to carry the computation
   Outputs
    L:List
   Description
    Text
     This is the rank-only version of @TO golodBetti@. It uses the S-free
     resolutions of R and M over S and returns the predicted total ranks
     through homological degree b.
     The quotient presentation must be minimal at the origin: its defining
     ideal must be contained in the square of the ideal of the variables.
     Homogeneous presentations without a positive heft use the same temporary
     positive regrading as @TO aInfinity@; the transported resolutions must
     still be minimal at the origin.
     For inhomogeneous input the S-free resolutions must likewise be minimal
     at the origin. The rank sequence is computed by an integer coefficient
     recurrence; no terms above the requested bound are expanded.
    Example
     S = QQ[x,y]
     R = S/ideal(x^2,y^2)
     M = coker vars R
     golodRanks(M,6)
     apply(7,i -> totalBetti(M,i))
   SeeAlso
    golodBetti
    burkeResolution
   Caveat
     This is an intrinsic Golod bound only for a minimal regular presentation
     at the origin, enforced by @TT "I subset (ideal vars S)^2"@. The method
     rejects rather than automatically minimizes any other presentation.
///

doc ///
   Key
    totalBetti
    (totalBetti, Module, ZZ)
   Headline
    compute one local total Betti number from Burke blocks
   Usage
    b = totalBetti(M,i)
   Inputs
    M:Module
    i:ZZ
     nonnegative homological degree
   Outputs
    b:ZZ
   Description
    Text
     Computes the i-th total Betti number by reducing the adjacent Burke
     differentials modulo the ideal of the variables and taking ranks over
     the coefficient field. For inhomogeneous input this is interpreted at
     the origin. This requires the defining
     ideal to be contained in the ideal of the variables; a minimal regular
     presentation is not required.
     In degree zero this returns the number of generators of
     @TT "M tensor_R k"@ at the origin.  The zero module has total Betti
     number zero in every degree.
    Example
     S = QQ[x,y]
     R = S/ideal(x^2,y^2)
     M = coker vars R
     apply(5,i -> totalBetti(M,i))
     totalBetti(R^0,0)
   SeeAlso
    burkeResolution
    golodRanks
   Caveat
     This is a local invariant at the origin, not a global Betti number. It
     requires a coefficient field and @TT "I subset (ideal vars S)"@, but it
     does not require the stronger minimal-presentation hypothesis used by
     the presentation-specific Golod bounds.
///

doc ///
Key
 displayBlocks
 (displayBlocks, Matrix)
Headline
 prints a matrix showing the source and target decomposition
Usage
 displayBlocks M
Inputs
 M:Matrix
  with source and target labeled direct sums of free modules
Description
  Text
   The maps produced by @TO burkeResolution@ and @TO aInfinity@ have direct sums of labeled modules
   as sources and targets; the label corresponds to the tensor factors.
   
   displayBlocks M shows this data
  Example
   R = ZZ/101[a,b,c]/(ideal(a,b,c^2))^2
   F = burkeResolution (coker vars R, 4)
   picture F  
   displayBlocks F.dd_3
SeeAlso
 picture
 burkeResolution
 extractBlocks
///

doc ///
Key
 extractBlocks
 (extractBlocks, Matrix, List)
 (extractBlocks, Matrix, List, List)
Headline
 displays components of a map in a labeled complex
Usage
 M = extractBlocks(f, sour)
 M = extractBlocks(f, tar, sour)
Inputs
 f:Matrix
  a map between labeled direct sums
 sour:List
  list of ZZ, the label of a summand of the source of f  
  OR list of lists of ZZ, specifying multiple summands
 tar:List
  same as sour, but for the target of f
Outputs
 M:Matrix
  the submatrix specified by tar and sour
Description
  Text
   The terms of the @TO burkeResolution@ resolution are direct sums of labeled modules.
   the function @TO picture@ shows the symbols associated to the summands, while
   the function extractBlocks provides the submatrix associated with the summands
   specified.
  Example
   R = ZZ/101[a,b,c,d]/ideal(a^2, b^2, c^3, d^4)
   M = R^1/ideal(a*b,c*d)
   F = burkeResolution(M,5)
   picture F.dd_3
   extractBlocks(F.dd_3, {2,1})
   extractBlocks(F.dd_3,{2,0}, {2,1})
   extractBlocks(F.dd_3,{2,0}, {{3,0},{2,1}})
SeeAlso
   burkeResolution
   picture
///

doc ///
Key
 isLocallyMinimal
 (isLocallyMinimal, Complex)
Headline
 Tests whether a complex is minimal at the origin
Usage
 h = isLocallyMinimal C
Inputs
 C : Complex
Outputs
 h : Boolean
Description
 Text
  Returns true when every differential of C becomes zero after reducing
  modulo the ideal generated by the variables.
 Example
  S = QQ[x,y]
  isLocallyMinimal complex koszul vars S
  isLocallyMinimal complex {id_(S^1)}
SeeAlso
 hasMinimalMult
///

doc ///
Key
 isLocallyAcyclic
 (isLocallyAcyclic, Complex)
Headline
 Tests whether strict interior homology vanishes at the origin
Usage
 h = isLocallyAcyclic C
Inputs
 C : Complex
Outputs
 h : Boolean
Description
 Text
  Returns true when the differential squares to zero and every homology
  module in a strict interior degree, min C < i < max C, vanishes after
  localizing at the ideal generated by the variables. This convention is
  unchanged by homological shifts: zero and negative interior degrees are
  tested too. Endpoint homology is not tested. A complex with at most two
  consecutive terms therefore passes whenever its differential squares to zero.
 Example
  S = QQ[x,y,z]
  R = S/ideal(x^2-y^3)
  M = coker vars R
  F = burkeResolution(M, 3, Check => true)
  isLocallyAcyclic F
SeeAlso
 isLocallyMinimal
 burkeResolution
///

doc ///
Key
 hasMinimalMult
 (hasMinimalMult,Ideal)
 (hasMinimalMult,Ideal,ZZ)
 (hasMinimalMult,Ideal,InfiniteNumber)
 (hasMinimalMult,Ring)
 (hasMinimalMult,Ring,InfiniteNumber)
 (hasMinimalMult,Ring,ZZ)
Headline
 Determines if the A-infinity multiplication is minimal
Usage
 hasMinimalMult R
 hasMinimalMult (R,n)
 hasMinimalMult I
 hasMinimalMult (I,n)
Inputs
 R : Ring
  the minimally presented quotient ring to test at the origin
 I : Ideal
  an ideal in a polynomial ring; this tests quotient I
 n : ZZ
  a positive integer or infinity; this bounds the operation arity, and defaults to 2
Outputs
 h : Boolean
   Whether or not the A-infinity multiplication is minimal to the specified order
Description
  Text
   This function computes the A-infinity multiplications up to n, and
   reduces them modulo the maximal ideal to determine if they are minimal.
   The quotient must be given by a minimal regular presentation at the
   origin: the defining ideal must lie in the square of the ideal generated
   by the ambient variables.
  Example
   S = QQ[x,y]
   R = S/ideal(x^2,y^2)
   hasMinimalMult(R,1)
   hasMinimalMult(R,2)
   isGolodAInf R
SeeAlso
   isGolodAInf
///

doc ///
Key
 isGolodAInf
 (isGolodAInf,Ideal)
 (isGolodAInf,Ring)
 (isGolodAInf,Ring,Ideal)
 (isGolodAInf,Ideal,Ideal)
Headline
 test whether a local ring at a rational point is Golod
Usage
 h = isGolodAInf R
 h = isGolodAInf I
 h = isGolodAInf(R,p)
 h = isGolodAInf(I,p)
Inputs
 R : Ring
  a polynomial ring or its quotient over a field
 I : Ideal
  the defining ideal of the quotient in its polynomial ambient ring
 p : Ideal
  a closed rational point in the ambient ring or its quotient, defaulting to the origin
Outputs
 h : Boolean
  whether the local ring of the quotient at the specified point is Golod
Description
  Text
   The intrinsic Golod criterion uses a minimal regular local presentation.
   The method moves the requested point to the origin by translation and
   eliminates polynomial variables that can be solved for. Once the
   defining ideal lies in the square of the ambient maximal ideal, it tests
   all higher algebra operations modulo that ideal. A separate local
   generator bound recognizes regular local rings and hypersurfaces when
   at most one equation remains.
   When the ambient resolution is nonminimal at the point or polynomial
   elimination leaves nonlinear equations with linear parts, the method
   uses the finite Betti-number criterion in Burke, Corollary 6.10.
  Example
   S=QQ[x,y]
   R=S/ideal((x-1)^2,(y+2)^2)
   isGolodAInf(R,ideal(S_0-1,S_1+2))
  Text
   Below is an example of an artinian ring R (based on an example of Roos
   and Katthan) which has minimal multiplications of order two, but 
   is not Golod.
  Example
   kk = ZZ/101
   S = kk[x,y,z,u]
   I = ideal(u^3, x*y^2, (x+y)*z^2, x^2*u+z*u^2, y^2*u+x*z*u, y^2*z+y*z^2)
   J = trim (I + (ideal vars S)^6)
   hasMinimalMult(quotient J, 2)
   isGolodAInf quotient J
SeeAlso
 hasMinimalMult
 aInfinity
Caveat
  The two-argument form computes with the given rational point independently
  of point choices in other computations. The Betti-number criterion can
  require larger free modules than the direct test on higher products.
///

doc ///
Key
 oppositeAInfinityAlgebra
 (oppositeAInfinityAlgebra, HashTable)
 [oppositeAInfinityAlgebra, LengthLimit]
Headline
 construct the opposite of an A-infinity algebra
Usage
 mRop = oppositeAInfinityAlgebra mR
 mRop = oppositeAInfinityAlgebra(mR, LengthLimit => n)
Inputs
 mR : HashTable
  an A-infinity algebra structure produced by @TO aInfinity@
 LengthLimit => ZZ
  retain higher operations whose key entries have total sum at most this nonnegative integer
Outputs
 mRop : HashTable
  the opposite A-infinity algebra structure on the same shifted resolution
Description
 Text
  If an operation of @TT "mR"@ has degree word
  @TT "u = {u_1,...,u_t}"@, the operation of the opposite algebra is obtained
  by reversing the word and the tensor factors, with sign exponent
  @TT "sum_(i<j) u_i*u_j + t - 1"@.  Applying this construction twice
  recovers the original operations, including their signs.
 Text
  @TT "LengthLimit"@ has the A-infinity operation-degree meaning: a higher
  list key is retained when the sum of its entries is at most the limit.
  Unary keys encode the differential and are always retained.  Thus
  @TT "LengthLimit => 0"@ retains the complete underlying complex but no
  higher operations.
 Example
  S = QQ[a,b,c]
  R = S/(ideal(a)*ideal(a,b,c))
  mR = aInfinity R
  mRop = oppositeAInfinityAlgebra(mR, LengthLimit => 4)
  sort select(keys mRop, k -> class k === List)
SeeAlso
 aInfinity
 dualAInfinityModule
 dualAInfinityStructure
///

doc ///
Key
 dualAInfinityModule
 (dualAInfinityModule, HashTable, HashTable)
 [dualAInfinityModule, LengthLimit]
Headline
 dualize the operations of an A-infinity module
Usage
 mDual = dualAInfinityModule(mR,mM)
 mDual = dualAInfinityModule(mR,mM,LengthLimit => n)
Inputs
 mR : HashTable
  an A-infinity algebra structure produced by @TO aInfinity@
 mM : HashTable
  a left A-infinity module structure over @TT "mR"@
 LengthLimit => ZZ
  retain higher dual operations whose key entries have total sum at most this nonnegative integer
Outputs
 mDual : HashTable
  the raw left A-infinity module structure on the shifted dual complex
Description
 Text
  Let @TT "A = mM#\"resolution\""@ have length @TT "p"@.  The underlying
  complex of the result is @TT "H_j = Hom_S(A_(p-j),S)"@ for
  @TT "0 <= j <= p"@.  It is the shifted dual @TT "A^vee[p]"@ and is a
  left module over @TO oppositeAInfinityAlgebra@ applied to @TT "mR"@.
 Text
  A source operation with key @TT "{u_t,...,u_1,i_0}"@ is partially
  dualized and its algebra tensor factors are reversed to give the operation
  with key @TT "{u_1,...,u_t,j}"@, where
  @TT "i_0 = p-j-sum(u)+1"@.  The sign exponent is the sum, over entries
  @TT "v"@ of @TT "u"@, of @TT "v*(v+1)/2+1"@.
 Text
  This is a low-level interface: the opposite algebra is not packaged in the
  result.  Use @TO dualAInfinityStructure@ for the preferred self-contained
  output.  @TT "LengthLimit"@ only truncates higher operation keys; every
  unary differential and the entire dual complex remain present.
 Example
  S = QQ[x,y,z]
  R = S/ideal(x^2)
  mR = aInfinity R
  M = coker vars R
  mM = aInfinity(mR,M)
  mDual = dualAInfinityModule(mR,mM,LengthLimit => 3)
  sort select(keys mDual,k -> class k === List)
  (mDual#"resolution").dd^2 == 0
SeeAlso
 aInfinity
 oppositeAInfinityAlgebra
 dualAInfinityStructure
///

doc ///
Key
 dualAInfinityStructure
 (dualAInfinityStructure, HashTable)
 (dualAInfinityStructure, HashTable, HashTable)
 [dualAInfinityStructure, LengthLimit]
Headline
 construct the dual A-infinity module over the opposite algebra
Usage
 mDual = dualAInfinityStructure(mR,mM)
 mDual = dualAInfinityStructure mR
 mDual = dualAInfinityStructure(mR,mM,LengthLimit => n)
Inputs
 mR : HashTable
  an A-infinity algebra structure produced by @TO aInfinity@
 mM : HashTable
  a left A-infinity module structure over @TT "mR"@
 LengthLimit => ZZ
  retain higher output operations whose key entries have total sum at most this nonnegative integer
Outputs
 mDual : HashTable
  a packaged dual A-infinity module structure
Description
 Text
  This is the preferred interface for dualizing A-infinity modules.  Its list
  keys are the operations on @TT "A^vee[p]"@.  The key @TT "\"algebra\""@
  contains the opposite A-infinity algebra, @TT "\"resolution\""@ contains
  the shifted dual complex, and @TT "\"normalizationShift\""@ contains
  @TT "dim(S)-p"@.  The latter is recorded for later dualizing-complex
  normalization but is not applied to the nonnegative A-infinity indices.
 Text
  The one-input form constructs and dualizes the regular module
  @TT "aInfinity(mR,(mR#\"ring\")^1)"@.  It requires a complete finite
  algebra structure, even when the dual output has a @TT "LengthLimit"@,
  because low-degree dual operations can arise from source-module operations
  near the top of the source resolution.
 Text
  More generally, a source module truncated at @TT "LengthLimit => n"@ need
  not contain the data required to dualize through the same numerical limit:
  source and dual resolution indices run in opposite directions.  The
  two-input method checks for exactly the source operations required by the
  requested dual output, and reports a missing required operation rather
  than silently treating it as zero.
 Example
  S = QQ[x,y,z]
  R = S/ideal(x^2)
  mR = aInfinity R
  mDual = dualAInfinityStructure(mR,LengthLimit => 3)
  mDual#?"algebra"
  mDual#"normalizationShift"
  sort select(keys mDual,k -> class k === List)
SeeAlso
 aInfinity
 oppositeAInfinityAlgebra
 dualAInfinityModule
 dualizingComplexViaAInfinity
///

doc ///
Key
 dualizingComplexViaAInfinity
 (dualizingComplexViaAInfinity, PolynomialRing)
 (dualizingComplexViaAInfinity, Ideal)
 (dualizingComplexViaAInfinity, QuotientRing)
 [dualizingComplexViaAInfinity, Bounded]
 [dualizingComplexViaAInfinity, LengthLimit]
 [dualizingComplexViaAInfinity, Check]
 [dualizingComplexViaAInfinity, VerifyComplex]
Headline
 construct a normalized dualizing complex using A-infinity structures
Usage
 D = dualizingComplexViaAInfinity R
 D = dualizingComplexViaAInfinity I
 F = dualizingComplexViaAInfinity(R, Bounded => false, LengthLimit => n)
Inputs
 R : Ring
  a polynomial ring or a quotient of a polynomial ring over a field or over @TT "ZZ"@
 I : Ideal
  an ideal in a supported polynomial ring; shorthand for applying the method to @TT "quotient I"@
 Bounded => Boolean
  whether to return the bounded model, which is the default
 LengthLimit => ZZ
  in free mode, a nonnegative final normalized homological degree, at least dim(S)-p where p is the length of the ambient resolution used
 Check => Boolean
  pass lift verification to the A-infinity construction
 VerifyComplex => Boolean
  verify that the assembled Burke differentials square to zero
Outputs
 D : Complex
  a normalized dualizing complex or a finite free prefix of one
Description
 Text
  In bounded mode this method constructs a model for
  @TT "RHom_S(R,S)[dim S]"@ and replaces the acyclic free tail by a terminal
  cokernel.  The terminal module is generally not free.  For a polynomial
  ring, the result is the one-term complex @TT "S[dim S]"@ in the package's
  homological indexing convention.
 Text
  With @TT "Bounded => false"@, @TT "LengthLimit => n"@ instead returns the
  free Burke model through normalized homological degree @TT "n"@.  This is
  distinct from @TT "LengthLimit"@ on the A-infinity transformation methods,
  where the option bounds the total degree of higher operation keys.  A
  finite free prefix can have spurious homology in its terminal degree and is
  not by itself a bounded dualizing complex.
 Text
  The normalization is ungraded: no internal canonical-module twist is
  added for a graded presentation.  The method currently requires a
  polynomial ambient ring over a field or over @TT "ZZ"@.  Rings produced
  by @TT "localRing"@ are not supported.
 Example
  S = QQ[x,y,z]
  R = S/ideal(x^2)
  D = dualizingComplexViaAInfinity R
  (min D,max D)
  D.dd^2 == 0
  F = dualizingComplexViaAInfinity(R,Bounded=>false,LengthLimit=>6)
  (min F,max F)
  all(toList(2..6),i -> isFreeModule F_i)
 Text
  Here is a small non-Cohen--Macaulay example.  The maximal ideal is an
  embedded associated prime, and the projective dimension over the
  three-variable polynomial ring is three although the quotient has
  dimension two.
 Example
  S = QQ[a,b,c]
  I = ideal(a)*ideal(a,b,c)
  associatedPrimes I
  R = S/I
  (dim R,pdim coker presentation R)
  D = dualizingComplexViaAInfinity R
  apply(toList(min D..max D),i -> prune HH_i D)
  presentation prune HH_0 D
  presentation prune HH_2 D
 Text
  Thus the dualizing complex has two nonzero homology modules (the
  cohomology of @TT "RHom"@ in this homological indexing):
  @TT "HH_0(D) = R/(a,b,c)"@ and @TT "HH_2(D) = R/(a)"@, up to the
  displayed internal degree shifts.  Its homology in degrees one and three
  is zero.  The separated zero-dimensional and two-dimensional pieces are
  exactly the behavior that disappears in the Cohen--Macaulay case.
 Text
  A reduced non-Cohen--Macaulay example is the union of a plane and a line
  meeting at the origin.  Its two minimal components have different
  dimensions, and both dimensions are visible in the dualizing homology.
 Example
  S = QQ[x,y,z]
  I = intersect(ideal(x),ideal(y,z))
  radical I == I
  minimalPrimes I
  apply(minimalPrimes I,P -> dim(S/P))
  R = S/I
  D = dualizingComplexViaAInfinity R
  apply(toList(min D..max D),i -> prune HH_i D)
  presentation prune HH_1 D
  presentation prune HH_2 D
 Text
  The presentations identify @TT "HH_1(D) = R/(y,z)"@, supported on the
  one-dimensional line, and @TT "HH_2(D) = R/(x)"@, supported on the
  two-dimensional plane, again up to internal degree shifts.  Thus this
  reduced ring has nonzero dualizing homology in two different degrees even
  though it has no embedded component.
SeeAlso
 dualAInfinityStructure
 aInfinity
Caveat
  The default bounded result is a dualizing complex. With
  @TT "Bounded => false"@ the result is only a finite free prefix and may
  have spurious terminal homology. Normalization is ungraded, so no internal
  canonical-module twist is supplied. The ambient polynomial ring must be
  over a field or @TT "ZZ"@, and rings produced by @TT "localRing"@ are not
  supported.
///

--Boundary cases: 1 variable, ring as module.

TEST ///
--ring and module have pdim 1
S = QQ[t]
R = S/t^3
M = R^1/t^2
F = burkeResolution(M, 5)
assert (0 == F.dd^2 and all(length F -1 , i-> 0 == HH_(i+1) F));

--resolution of the ring as module
M = R^1
F = burkeResolution(M,5) -- a nonminimal resolution!
assert (0 == F.dd^2 and all(length F -1 , i-> 0 == HH_(i+1) F));

--ring is Golod, module is pdim 1
S = ZZ/101[s,t,u,v]
R = S/(s*ideal(s,t,u,v))
M = R^1/s
F = burkeResolution(M,5, Check=>false)
assert (0 == F.dd^2 and all(length F -1 , i-> 0 == HH_(i+1) F));
///

TEST ///
debug needsPackage "AInfinity"
u = {2,2,3}
assert(mapComponents u =={{1, 2, 2, {2, 2, 2}}, {1, 0, 1, {3, 3}}, {1, 0, 2, {6}}, {1, 1, 2, {2, 4}}})
///

TEST ///
--case of rings and/or modules whose projective dimension over S is 1.
S = ZZ/101[a,b,c]
R = S/(ideal a)
mR = aInfinity R
assert (#select(keys mR, k -> class k === List) == 1)
use S
R = S/(ideal(a)*ideal(a,b,c))
Y = R^1/a
mR = aInfinity R
F = burkeResolution(Y,8)
assert(F.dd^2 == 0)
assert(all(7, i-> prune HH_(i+1)F == 0))
///

TEST ///
kk = ZZ/101
S = kk[a,b,c]
R = S/((ideal a^2)*ideal(a,b,c)) -- a simple 3 variable Golod ring
K = koszul vars R
M = coker K.dd_2
F = res(M, LengthLimit => 5)
N = coker F.dd_5 ;
mA = aInfinity R;
mG = aInfinity(mA,N);
assert(betti burkeResolution(N,5) == betti res (N, LengthLimit => 5))
assert(F.dd^2 == 0)
assert all(length F -1, i-> prune HH_(i+1)F == 0)
///

TEST ///
kk = ZZ/101
S = kk[x,y]
R = S/ideal"xy"
mR = aInfinity R
mG = aInfinity(mR,coker vars R,Check=>true)
N = coker vars R
F = burkeResolution(N, 5)
assert(F.dd^2 == 0)
assert all(length F -1, i-> prune HH_(i+1)F == 0)
assert(betti burkeResolution(N,5) == betti res (N, LengthLimit => 5))
///

TEST ///
-- multigraded example
kk = ZZ/101
S = kk[x,y,u,v,Degrees => {{1,0},{1,0},{0,1},{0,1}}]
R = S/ideal"xu-yv"
mR = aInfinity R
mG = aInfinity(mR,coker vars R,Check=>true)
N = coker vars R
F = burkeResolution(N, 6)
assert(isHomogeneous F)
assert(F.dd^2 == 0)
assert all(length F -1, i-> prune HH_(i+1)F == 0)
assert(betti burkeResolution(N,6) == betti res (N, LengthLimit => 6))
///

TEST ///
-- weighted positively graded Golod example
kk = ZZ/101
S = kk[a,b,c, Degrees => {1,2,3}]
R = S/(ideal(a^2)*ideal(a,b,c))
M = coker vars R
mR = aInfinity R
assert all(select(keys mR, k -> class k === List), k -> isHomogeneous mR#k)
assert hasMinimalMult R
F = burkeResolution(M, 6)
assert isHomogeneous F
assert(F.dd^2 == 0)
assert all(length F -1, i-> prune HH_(i+1)F == 0)
assert(betti F == betti res(M, LengthLimit => 6))
assert(golodBetti(M,6) == betti res(M, LengthLimit => 6))
///

TEST ///
-- weighted complete intersection: homogeneous but not Golod
kk = ZZ/101
S = kk[a,b,c, Degrees => {1,2,3}]
R = S/ideal(a^4+b^2, b^3+c^2)
mR = aInfinity(R, Order => 2)
assert all(select(keys mR, k -> class k === List), k -> isHomogeneous mR#k)
assert not hasMinimalMult(R,2)
F = burkeResolution(coker vars R, 4)
assert isHomogeneous F
assert(F.dd^2 == 0)
assert all(length F -1, i-> prune HH_(i+1)F == 0)
///

TEST ///
-- multigraded module with explicit twists
kk = ZZ/101
S = kk[x,y,u,v, Degrees => {{1,0},{1,0},{0,1},{0,1}}]
R = S/(x*ideal(x,y,u,v))
M = coker map(R^{{0,0}}, R^{{-1,0},{0,-1}}, matrix{{x,u}})
F = burkeResolution(M, 4)
assert isHomogeneous F
assert(F.dd^2 == 0)
assert all(length F -1, i-> prune HH_(i+1)F == 0)
assert(betti F == betti res(M, LengthLimit => 4))
///

TEST ///
-- globally exact inhomogeneous input, with local invariants at the origin
S = QQ[x,y,z]
R = S/ideal(x^2-y^3)
mR = aInfinity R
assert isLocallyMinimal mR#"SResolution"
M = coker vars R
F = burkeResolution(M, 3, Check => true)
assert(F.dd^2 == 0)
assert(totalBetti(M,1) == 3)
assert(totalBetti(M,2) == 4)
///

TEST ///
-- A quotient by zero has no algebra bars; Burke reduces to the ambient
-- free resolution and remains zero after its projective dimension.
S = QQ[x,y]
R = S/ideal(0_S)
M = coker vars R
mR = aInfinity R
assert(length mR#"SResolution" == 0)
assert(length mR#"resolution" == 0)
F = burkeResolution(M,4,Check => true)
assert(F.dd^2 == 0)
assert(apply(5,i -> rank F_i) == {1,2,1,0,0})
assert(golodRanks(M,0) == {1})
assert(golodRanks(M,4) == {1,2,1,0,0})
assert(apply(5,i -> totalBetti(M,i)) == {1,2,1,0,0})
///

TEST ///
-- Intrinsic Golodness survives polynomial elimination; relative bounds keep the input presentation.
S = QQ[x,y,z]
R = S/ideal(x-y^2,y-z^2)
M = coker vars R
assert(not isSubset(ideal R,(ideal vars S)^2))
assert(try (hasMinimalMult R; false) else true)
assert isGolodAInf R
assert(try (golodRanks(M,4); false) else true)
assert(try (golodBetti(M,4); false) else true)
assert(apply(5,i -> totalBetti(M,i)) == {1,1,0,0,0})
///

TEST ///
-- option bounds fail before construction
S = QQ[x,y]
R = S/ideal(x^2-y^3)
M = coker vars R
assert(try (aInfinity(R, LengthLimit => -1); false) else true)
assert(try (aInfinity(R, Order => 0); false) else true)
assert(try (aInfinity(M, LengthLimit => -1); false) else true)
assert(try (aInfinity(M, Order => -2); false) else true)
F0 = burkeResolution(M, 0, Check=>true)
assert(min F0 == 0 and max F0 == 0 and F0_0 == (burkeResolution(M,1))_0)
assert(try (burkeResolution(M, -1); false) else true)
assert(try (golodRanks(M, -1); false) else true)
assert(try (golodBetti(M, -1); false) else true)
assert(try (totalBetti(M, -1); false) else true)
///

TEST ///
-- Order caps algebra and module arity
debug needsPackage "AInfinity"
arityMax := h -> max apply(select(keys h, k -> class k === List), k -> #k)
S = ZZ/101[w,x,y,z]
R = S/ideal(w^2,x^2,y^2,z^2)
assert(arityMax aInfinity(R, Order => 1) <= 1)
assert(arityMax aInfinity(R, Order => 2) <= 2)
assert(arityMax aInfinity R > 2)
S = ZZ/101[a,b,c]
R = S/(a^2*ideal(a,b,c))
M = coker vars R
mR1 = aInfinity(R, Order => 1)
assert(try (aInfinity(mR1, M, Order => 3); false) else true)
assert(arityMax aInfinity(mR1, M, Order => 1) <= 1)
assert(arityMax aInfinity(M, Order => 2) <= 2)
assert(arityMax aInfinity(M, Order => 3) <= 3)
///

TEST ///
-- homogeneous global paths without a positive heft use a harmless regrading
S = QQ[a,b, Degrees => {0,1}]
R = S/ideal(a^2)
M = coker vars R
assert(isHomogeneous R)
assert(heft S === null)
mGlobal = aInfinity R
assert(mGlobal#"positiveRegrading")
mModule = aInfinity(mGlobal,M)
assert(mModule#"positiveRegrading")
Fglobal = burkeResolution(M, 2, Check => true)
assert(Fglobal.dd^2 == 0)
assert all(length Fglobal - 1, i -> HH_(i+1) Fglobal == 0)
assert(#golodRanks(M, 2) == 3)
Bglobal = golodBetti(M, 2)
assert(class Bglobal === BettiTally)
///

TEST ///
-- totalBetti includes degree zero
S = QQ[x,y,z]
R = S/ideal(x^2-y^3)
M = coker vars R
N = R^2
Z = R^0
torBetti := (M,i) -> numgens prune Tor_i(M, coker vars ring M)
assert(totalBetti(M,0) == torBetti(M,0))
assert(totalBetti(M,1) == torBetti(M,1))
assert(totalBetti(N,0) == 2)
assert(totalBetti(Z,0) == 0)
assert(totalBetti(Z,1) == 0)
///

TEST ///
-- the global construction supports translated inhomogeneous quotients
S = QQ[x,y]
R = S/ideal(x-1)
mR = aInfinity R
assert(ring(mR#"resolution") === S)
ok := try (totalBetti(R^1,0); false) else true
assert ok
///

TEST ///
-- inhomogeneous global resolutions over ZZ use the positive transport
S = ZZ[x]
R = S/ideal(x^2-x)
mR = aInfinity R
assert(mR#"positiveRegrading")
mG = aInfinity(mR,R^1)
assert(mG#"positiveRegrading")
F = burkeResolution(R^1,3,Check => true)
assert(F.dd^2 == 0)
assert all(length F - 1, i -> HH_(i+1) F == 0)
///

TEST ///
kk = ZZ/101
S = kk[x,y,z,u]
I = ideal(u^3, x*y^2, (x+y)*z^2, x^2*u+z*u^2, y^2*u+x*z*u, y^2*z+y*z^2) -- has the betti nums as in Roos
R = S/I
mR = aInfinity(R,Order => 2);
-- checking that limiting order worked
assert all(select(keys mR, k -> class k === List), l -> #l <= 2)
mR = aInfinity(R,Order => 3);
assert all(select(keys mR, k -> class k === List), l -> #l <= 3)
///

TEST ///
S = ZZ/101[a..d]
R = S/ideal"a3,b3,c3,d3"
mA = aInfinity R;
--tensorCommutativity(M,N): M**N --> N**M.
B = mA#"resolution"
--m{2,2} is skew-symmetric, mimicking A_1**A_1 -> A_2
assert (0==mA#{2,2}+mA#{2,2}*tensorCommutativity (B_2,B_2)) -- anti-commutative
assert (0==mA#{3,3}-mA#{3,3}*tensorCommutativity (B_3,B_3)) -- commutative
assert (0==mA#{3,2}+mA#{2,3}*tensorCommutativity (B_3,B_2)) -- mixed shifted degrees are skew-compatible
///


TEST ///
debug needsPackage"AInfinity"
S = ZZ/101[a,b,c]
K = complex koszul vars S
KK = labeledTensorComplex{K,K}
assert(indices KK_5 == {{3, 2}, {2, 3}})
R = S/ideal a
KKl = labeledTensorComplex(R,KK)
assert(indices KKl_1 =={0, 1})
///

TEST ///
debug needsPackage "AInfinity"
S = ZZ/101[a,b,c]
R = S/ideal"a3,b3,c3"
M = coker vars R
n = 3
BB = burkeData(M,n)
assert(BB_2 == S^{3:-2}++S^{3:-3})
assert (indices BB_2 == {{2}, {2, 0}})
assert (source (BB_2_[{2,0}]) == S^{3:-3})
///

TEST ///
debug needsPackage "AInfinity"
S = ZZ/101[a,b]
X = labeler(A,S^1)
Y = labeler(B,S^2)
C = labeledDirectSum {X,Y}
assert(componentsAndIndices C == ({S^1 , S^2 }, {A, B}))
assert(C^[A] == map(S^1,S^3,{{1,0,0}}))
assert(label components C == {A,B})
assert(indices C == {A,B})
///

TEST ///
debug needsPackage"AInfinity"
S = ZZ/101[a,b,c]
K' = complex koszul vars S

K = labeledTensorComplex K' -- exercise the single-complex overload
assert(label K_2 == {2})
assert(K_0_[{0}] == map(S^1,S^1,id_(S^1)))
K1 = labeledTensorComplex(K',LengthLimit => 1)
assert(max K1 == 1)

K2 = labeledTensorComplex({K',K'}, LengthLimit => 3)
assert(K2_1_[{1,0}] == map(S^{6:-1},S^{3:-1},id_(S^{3:-1})||0*id_(S^{3:-1})))
assert(max K2 == 3)
-- LengthLimit bounds the total degree for any number of factors.
K3 = labeledTensorComplex({K',K',K'}, LengthLimit => 4)
assert(max K3 == 4)
assert(indices K3_4 == indices (labeledTensorComplex{K',K',K'})_4)

R = S/ideal a
RK = labeledTensorComplex(R,K)
assert(label RK_2 == {2})
assert(RK_0_[{0}] == map(R^1,R^1,1))
///

TEST ///

kk = ZZ/101
S = kk[x,y,z]
I = ideal"x2,y2,z2"*(ideal vars S)
R =S/I
mA = aInfinity R
mG = aInfinity(mA,coker vars R)
F = burkeResolution(coker vars R,6)
assert (F.dd^2==0)
assert all(5, i-> prune HH_(i+1)F == 0)
///


TEST ///
--a long resolution with more vars, pd M small
S = ZZ/101[x_1..x_4]
I = x_1*ideal(vars S)
R = S/I
M = R^1/ideal(x_1..x_2)
F = burkeResolution(M, 8, Check =>false)
assert(F.dd^2 == 0)
assert all(length F - 1, i-> prune HH_(i+1)F == 0)
///

TEST ///
-- Opposite A-infinity algebra: operation cutoff and involutive signs.
S = QQ[a,b,c]
R = S/(ideal(a)*ideal(a,b,c))
mR = aInfinity R
mRop = oppositeAInfinityAlgebra(mR,LengthLimit=>4)
assert(sort select(keys mRop,k -> class k === List)
    == {{2},{2,2},{3},{4}})
assert(mRop#"lengthLimit" == 4)
doubleOpposite = oppositeAInfinityAlgebra(mRop,LengthLimit=>4)
retainedKeys = sort select(keys mRop,k -> class k === List)
assert all(retainedKeys,k -> mR#k == doubleOpposite#k)
differentialsOnly = oppositeAInfinityAlgebra(mR,LengthLimit=>0)
assert(sort select(keys differentialsOnly,k -> class k === List)
    == {{2},{3},{4}})
///

TEST ///
-- Raw and packaged dual module structures, including LengthLimit.
S = QQ[x,y,z]
R = S/ideal(x^2)
mR = aInfinity R
M = coker vars R
mM = aInfinity(mR,M)
fullDual = dualAInfinityModule(mR,mM)
rawDual = dualAInfinityModule(mR,mM,LengthLimit=>3)
packagedDual = dualAInfinityStructure(mR,mM,LengthLimit=>3)
expectedKeys = {{1},{2},{2,0},{2,1},{3}}
assert(sort select(keys rawDual,k -> class k === List) == expectedKeys)
assert(sort select(keys packagedDual,k -> class k === List) == expectedKeys)
assert all(expectedKeys,k -> rawDual#k == fullDual#k)
assert(rawDual#"lengthLimit" == 3)
assert((packagedDual#"algebra")#"lengthLimit" == 3)
A = mM#"resolution"
H = packagedDual#"resolution"
p = length A
assert((min H,max H) == (0,p))
assert(H.dd^2 == 0)
assert(packagedDual#"normalizationShift" == dim S-p)
assert all(toList(0..p),j -> numgens H_j == numgens A_(p-j))

-- The truncated data assemble to a complex after base change to R.
diffsS = mapComponents(packagedDual#"algebra",packagedDual,3)/sum
diffsR = apply(diffsS,d -> map(R ** target d,R ** source d,substitute(matrix d,R)))
assert((complex diffsR).dd^2 == 0)
///

TEST ///
-- One-input dualization agrees with explicitly dualizing the regular module.
S = QQ[a,b,c]
R = S/(ideal(a)*ideal(a,b,c))
mR = aInfinity R
mRegular = aInfinity(mR,R^1)
explicitDual = dualAInfinityStructure(mR,mRegular,LengthLimit=>4)
implicitDual = dualAInfinityStructure(mR,LengthLimit=>4)
dualKeys = sort select(keys explicitDual,k -> class k === List)
assert(dualKeys == sort select(keys implicitDual,k -> class k === List))
assert all(dualKeys,k -> explicitDual#k == implicitDual#k)
assert(explicitDual#"resolution" == implicitDual#"resolution")
///

TEST ///
-- Requested dual degree, rather than the same source cutoff, determines
-- which source module operations are required.
S = QQ[x,y,z]
R = S/ideal(x^2)
mR = aInfinity R
M = coker vars R
mM = aInfinity(mR,M)
missingHighSource = hashTable apply(
    select(keys mM,k -> k =!= {2,0}),
    k -> k => mM#k)
assert(try (dualAInfinityStructure(mR,missingHighSource); false) else true)
lowDual = dualAInfinityStructure(mR,missingHighSource,LengthLimit=>3)
assert(not lowDual#?{2,2})
missingLowSource = hashTable apply(
    select(keys mM,k -> k =!= {2,2}),
    k -> k => mM#k)
assert(try (
    dualAInfinityStructure(mR,missingLowSource,LengthLimit=>3);
    false
    ) else true)
assert(try (oppositeAInfinityAlgebra(mR,LengthLimit=>-1); false) else true)
assert(try (dualAInfinityModule(mR,mM,LengthLimit=>1/2); false) else true)
///

TEST ///
-- Exercise the public dualizing-complex modes.  A polynomial ring gives one
-- normalized term; a hypersurface is Cohen--Macaulay, so its bounded model
-- has exactly one nonzero homology module.
S = QQ[x,y,z]
G = dualizingComplexViaAInfinity S
assert((min G,max G) == (3,3))
assert(G.dd^2 == 0)
R = S/ideal(x^2)
D = dualizingComplexViaAInfinity R
assert((min D,max D) == (2,3))
assert(D.dd^2 == 0)
assert(prune HH_2 D != 0)
assert(prune HH_3 D == 0)
-- Free mode deliberately returns a prefix, not a bounded dualizing complex.
F = dualizingComplexViaAInfinity(
    R,
    Bounded=>false,
    LengthLimit=>6)
assert((min F,max F) == (2,6))
assert(F.dd^2 == 0)
assert all(toList(2..6),i -> isFreeModule F_i)
-- Ideal input delegates to the quotient-ring method.
DI = dualizingComplexViaAInfinity ideal(x^2)
assert(DI.dd^2 == 0)
-- These switches skip lift and d^2 verification but not construction.
Funchecked = dualizingComplexViaAInfinity(
    R,
    Bounded=>false,
    LengthLimit=>4,
    Check=>false,
    VerifyComplex=>false)
assert((min Funchecked,max Funchecked) == (2,4))
-- Free mode requires a final degree; bounded mode does not accept one.
assert(try (dualizingComplexViaAInfinity(R,Bounded=>false); false) else true)
assert(try (
    dualizingComplexViaAInfinity(R,LengthLimit=>4);
    false
    ) else true)
-- A free prefix cannot end before its first normalized degree.
assert(try (
    dualizingComplexViaAInfinity(
        R,
        Bounded=>false,
        LengthLimit=>1);
    false
    ) else true)
///

TEST ///
-- Mathematical test: the Stanley--Reisner union of a plane and a line.
-- From 0 -> R -> S/(x) ++ S/(y,z) -> k -> 0, the Ext sequence gives
-- HH_1(D) = R/(y,z) and HH_2(D) = R/(x), up to internal shifts.
S = QQ[x,y,z]
P = ideal x
Q = ideal(y,z)
R = S/intersect(P,Q)
assert(dim R == 2)
assert(dim S-pdim(coker presentation R) == 1)
D = dualizingComplexViaAInfinity R
assert(D.dd^2 == 0)
assert(select(toList(min D..max D),i -> prune HH_i D != 0) == {1,2})
H1 = prune HH_1 D
H2 = prune HH_2 D
assert((dim H1,dim H2) == (1,2))
assert isIsomorphic(H1,R^1/substitute(Q,R))
assert isIsomorphic(H2,R^1/substitute(P,R))
///

TEST ///
-- Mathematical test: two disjoint edges form a pure, disconnected
-- Stanley--Reisner complex.  Mayer--Vietoris gives a residue-field
-- deficiency module in degree 1 and the two facet rings in degree 2.
S = QQ[a,b,c,d]
P = ideal(a,b)
Q = ideal(c,d)
R = S/intersect(P,Q)
assert(dim R == 2)
assert(dim S-pdim(coker presentation R) == 1)
D = dualizingComplexViaAInfinity R
assert(D.dd^2 == 0)
assert(select(toList(min D..max D),i -> prune HH_i D != 0) == {1,2})
H1 = prune HH_1 D
H2 = prune HH_2 D
expectedH2 = (R^1/substitute(P,R)) ++ (R^1/substitute(Q,R))
assert((dim H1,dim H2) == (0,2))
assert isIsomorphic(H1,coker vars R)
assert isIsomorphic(H2,expectedH2)
///

TEST ///
-- Mathematical non-homogeneous test: a plane joined to the affine elliptic
-- curve y=0, z^2=x^3+x+1.  They meet in two points.  The elliptic component
-- rules out a positive cone structure; in these coordinates x*f also has x
-- as its unique lowest positive-weight term, while x is not in the ideal.
-- Mayer--Vietoris predicts curve-supported HH_1 and plane-supported HH_2.
S = QQ[x,y,z]
f = z^2-x^3-x-1
P = ideal x
Q = ideal(y,f)
I = intersect(P,Q)
R = S/I
assert(not isHomogeneous R)
assert(not isSubset(P,I))
assert((dim(S/(P+Q)),degree(S/(P+Q))) == (0,2))
assert(sort apply(minimalPrimes I,J -> dim(S/J)) == {1,2})
assert(dim R == 2)
assert(dim S-pdim(coker presentation R) == 1)
D = dualizingComplexViaAInfinity R
assert(D.dd^2 == 0)
assert(select(toList(min D..max D),i -> prune HH_i D != 0) == {1,2})
H1 = prune HH_1 D
H2 = prune HH_2 D
assert((dim H1,dim H2) == (1,2))
-- These homologies are cyclic with precisely the expected component support.
assert(numgens H1 == 1 and trim ann H1 == trim substitute(Q,R))
assert(numgens H2 == 1 and trim ann H2 == trim substitute(P,R))
///

TEST ///
-- The opposite algebra and the dual module satisfy the A-infinity identities
-- over S, including the epsilon terms that vanish after base change to R.
-- For this ring the dual module is not a module over the original algebra.
debug needsPackage "AInfinity"
strip = f -> map((ring f)^(numrows f),(ring f)^(numcols f),entries f)
kron = L -> fold(L,(a,b) -> a ** b)
idr = (S,r) -> id_(S^r)
residual = (mR,m,k,isModule) -> (
    B := mR#"resolution"; G := m#"resolution"; S := ring B;
    rk := (C,j) -> if j >= min C and j <= max C then numgens C_j else 0;
    r := j -> rk(B,j);
    wr := w -> if isModule then product(apply(drop(w,-1),r))*rk(G,last w) else product apply(w,r);
    tgt := if isModule then rk(G,sum k-2) else r(sum k-2);
    opM := (h,w,t) -> if h#?w then strip matrix h#w else map(S^t,S^(wr w),0);
    n := #k;
    comps := if isModule then mapComponents k else algebraMapComponents k;
    total := sum(comps, v -> (
        (sgn,p,q,w) := toSequence v;
        k2 := k_{p..q};
        pre := idr(S,product apply(k_{0..p-1},r));
        inner := if isModule and q == n-1 then kron{pre,opM(m,k2,rk(G,sum k2-1))}
            else if isModule then kron{pre,opM(mR,k2,r(sum k2-1)),
                idr(S,product apply(k_{q+1..n-2},r)),idr(S,rk(G,last k))}
            else kron{pre,opM(mR,k2,r(sum k2-1)),idr(S,product apply(k_{q+1..n-1},r))};
        sgn * opM(if isModule then m else mR,w,tgt) * inner));
    e := strip matrix (mR#"SResolution").dd_1;
    eps := if n != 2 then 0
        else if isModule then (if k_0 == 2 then e ** idr(S,rk(G,k_1)) else 0)
        else (if k_0 == 2 then e ** idr(S,r k_1) else 0)
            - (if k_1 == 2 then idr(S,r k_0) ** e else 0);
    total - eps == 0)
algebraOK = mR -> (B := mR#"resolution";
    all(select(bWords(max B,max B+2),w -> #w >= 2),k -> residual(mR,mR,k,false)))
moduleOK = (mR,m) -> (B := mR#"resolution"; G := m#"resolution";
    all(flatten for g from min G to max G list
        apply(bWords(max B,max G+2-g),w -> append(w,g)),k -> residual(mR,m,k,true)))
S = ZZ/101[x,y,z]
R = S/(ideal vars S)^2
mR = aInfinity R
assert algebraOK mR
for M in {R^1,coker vars R} do (
    mM := aInfinity(mR,M);
    assert moduleOK(mR,mM);
    mH := dualAInfinityStructure(mR,mM);
    assert algebraOK mH#"algebra";
    assert moduleOK(mH#"algebra",mH));
assert not moduleOK(mR,dualAInfinityStructure mR)
///

TEST ///
-- For a tower R = (S/I)/J the normalization uses S, not ambient R = S/I.
S = QQ[x,y,z]
T = S/ideal(x^2)
R = T/ideal(y^2)
use S
R' = S/ideal(x^2,y^2)
D = dualizingComplexViaAInfinity R
D' = dualizingComplexViaAInfinity R'
assert((min D,max D) == (min D',max D'))
assert((dualAInfinityStructure aInfinity R)#"normalizationShift" == 1)
assert(prune HH_1 D != 0 and prune HH_2 D == 0)
///

TEST ///
-- A truncated algebra does not depend on earlier cached computations, and
-- is rejected where the complete structure is required.
S = ZZ/101[a..e]
I = ideal(a^2,b^2,c^2,d^2,e^2,a*b*c,c*d*e)
R1 = S/I
R2 = S/I
fresh = aInfinity(R1, LengthLimit => 6)
full = aInfinity R2
afterFull = aInfinity(R2, LengthLimit => 6)
listKeys = h -> sort select(keys h, k -> class k === List)
assert(listKeys fresh == listKeys afterFull)
assert all(listKeys fresh, k -> sum k <= 6)
assert(fresh#"lengthLimit" == 6 and full#"lengthLimit" == 1 + max full#"resolution")
assert(try (dualAInfinityStructure afterFull; false) else true)
assert((oppositeAInfinityAlgebra(fresh, LengthLimit => 10))#"lengthLimit" == 6)
-- A module structure cannot be built over a too-short algebra.
assert(try (aInfinity(aInfinity(R1, LengthLimit => 3), R1^1); false) else true)
-- A module truncated at the same limit only needs the truncated algebra.
M = coker vars R1
mM = aInfinity(aInfinity(R1, LengthLimit => 4), M, LengthLimit => 4)
assert all(listKeys mM, k -> #k == 1 or sum k <= 4)
///

TEST ///
-- Origin-local predicates require the defining ideal to vanish at the origin.
S = QQ[x]
R = S/ideal(x-1)
C = complex {map(R^1,R^1,{{x}})}
assert(try (isLocallyMinimal C; false) else true)
assert(try (isLocallyAcyclic C; false) else true)
-- Quotient by the zero ideal: a single normalized term.
S = QQ[u,v]
D = dualizingComplexViaAInfinity(S/ideal(0_S))
assert((min D,max D) == (2,2))
F = dualizingComplexViaAInfinity(S/ideal(0_S),Bounded => false,LengthLimit => 4)
assert(F.dd^2 == 0)
///

-- Tests written before changing the implementation, from documented contracts
-- and independent mathematical expectations. Appended before end in the copy.

TEST ///
-- Degree zero includes the complete graded zeroth column, with its shifts.
S = QQ[x,y]
R = S/ideal(x^2,y^2)
assert(golodBetti(coker vars R,0) == betti complex {S^1})
assert(golodBetti(R^2,0) == betti complex {S^2})
assert(golodBetti(R^{-2},0) == betti complex {S^{-2}})
assert(golodBetti((coker vars R)**R^{-3},0) == betti complex {S^{-3}})
assert(golodBetti(R^0,0) == betti complex {S^0})
///

TEST ///
-- Inhomogeneous and zero-weight paths use auxiliary internal degrees.
S = QQ[u,v]
R = S/ideal(u^2-v^3)
assert(golodBetti(coker vars R,0) == betti complex {S^1})
assert(golodBetti(R^2,0) == betti complex {S^2})
assert(golodBetti(R^0,0) == betti complex {S^0})
S = QQ[a,b,Degrees=>{0,1}]
R = S/ideal(a^2)
assert(golodBetti(coker vars R,0) == betti complex {S^1})
///

TEST ///
-- Padded free resolutions have zero terms; their displays must remain usable.
S = QQ[x,y]
R = S/ideal(0_S)
F = burkeResolution(coker vars R,4,Check=>true)
assert(apply(5,i->rank F_i) == {1,2,1,0,0})
assert instance(picture F,Net)
assert instance(picture F.dd_3,Net)
assert instance(picture F_3,Net)
assert instance(displayBlocks F.dd_3,Net)
assert instance(displayBlocks F.dd_4,Net)
assert(toString picture F.dd_2 =!= toString picture(F.dd_2,ShowRanks=>true))
///

TEST ///
-- Every term may be zero, including over a ring with no variables.
S = QQ[]
R = S/ideal(0_S)
F = burkeResolution(R^0,2,Check=>true)
assert(F.dd^2 == 0)
assert instance(picture F,Net)
assert instance(picture F_0,Net)
assert instance(displayBlocks F.dd_1,Net)
assert(apply(4,i->totalBetti(R^0,i)) == {0,0,0,0})
///

TEST ///
-- A hand-specified labeled matrix gives independent block-selection oracles.
S = QQ[x,y]
src = directSum {{2}=>S^1,{2,0}=>S^2}
tar = directSum {{1}=>S^1,{3}=>S^1}
assert(indices src == {{2},{2,0}})
assert(indices tar == {{1},{3}})
f = map(tar,src,matrix{{1,x,y},{0,1,x}})
assert(entries extractBlocks(f,{2}) == {{1_S},{0_S}})
assert(entries extractBlocks(f,{{2,0},{2}}) == {{x,y,1_S},{1_S,x,0_S}})
assert(entries extractBlocks(f,{3},{2,0}) == {{1_S,x}})
assert(entries extractBlocks(f,{{3},{1}},{2}) == {{0_S},{1_S}})
assert instance(picture f,Net)
assert instance(displayBlocks f,Net)
assert(toString picture f =!= toString picture(f,ShowRanks=>true))
///

TEST ///
-- Check the public assembly interfaces, including the zeroth incoming map.
S = QQ[x,y]
R = S/ideal(x^2)
M = coker vars R
mR = aInfinity R
mM = aInfinity(mR,M)
F = burkeResolution(M,4)
for i from 1 to 3 do (
    pieces := adjacentBurkeComponents(mR,mM,i);
    assert(pieces == mapComponents1(mR,mM,i));
    assert(sum(pieces#0) == burkeDifferential(mR,mM,i));
    assert(sum(pieces#1) == burkeDifferential(mR,mM,i+1));
    d := burkeDifferential(mR,mM,i);
    assert(entries substitute(matrix d,R) == entries F.dd_i);
    assert(ring d === S));
z = adjacentBurkeComponents(mR,mM,0)
assert(#(z#0) == 1)
assert(numrows(first(z#0)) == 0 and numcols(first(z#0)) == rank F_0)
assert(mapComponents({0}) == {})
assert(mapComponents(mR,mM,0) == {})
///

TEST ///
-- Known Golod and non-Golod rings; a Boolean type check is insufficient.
for kk in {QQ,ZZ/2} do (
    S := kk[x,y,z];
    assert isGolodAInf(S/ideal(x^3));
    assert isGolodAInf((ideal vars S)^2);
    assert not isGolodAInf(S/ideal(x^2,y^2));
    assert(hasMinimalMult(S/ideal(x^2,y^2),1));
    assert not hasMinimalMult(S/ideal(x^2,y^2),2));
S = (ZZ/101)[x,y,z,u]
I = ideal(u^3,x*y^2,(x+y)*z^2,x^2*u+z*u^2,y^2*u+x*z*u,y^2*z+y*z^2)
J = trim(I+(ideal vars S)^6)
assert hasMinimalMult(J,2)
assert not isGolodAInf J
///

TEST ///
-- Exactness and minimality predicates need successful true AND false cases.
S = QQ[x,y]
K = complex koszul vars S
assert isLocallyAcyclic K
assert isLocallyMinimal K
C = complex {map(S^1,S^1,0),map(S^1,S^1,0)}
assert not isLocallyAcyclic C
C = complex {map(S^1,S^1,{{x}}),map(S^1,S^1,{{y}})}
assert not isLocallyAcyclic C
C = complex {id_(S^1)}
assert not isLocallyMinimal C
assert isLocallyAcyclic C
///

TEST ///
-- Independent rational-series oracles, without asking the implementation
-- to recompute its own expected answer.
S = QQ[x,y,z]
R = S/(ideal vars S)^2
assert(golodRanks(coker vars R,32) == apply(33,i->3^i))
S = QQ[x,y]
R = S/ideal(x^2,y^2)
assert(golodRanks(coker vars R,6) == {1,2,3,5,8,13,21})
assert(apply(7,i->totalBetti(coker vars R,i)) == {1,2,3,4,5,6,7})
R = S/ideal(0_S)
assert(golodRanks(coker vars R,8) == {1,2,1,0,0,0,0,0,0})
assert(golodRanks(R^0,8) == toList(9:0))
S = QQ[u,v]
R = S/ideal(u^2-v^3)
assert(golodRanks(coker vars R,8) == {1,2,2,2,2,2,2,2,2})
///

TEST ///
-- Mixed ranks and twists exercise adjoints beyond the residue-field fixture.
for kk in {QQ,ZZ/2} do (
    S := kk[x,y,z]; R := S/ideal(x^2,y^2);
    M := R^1 ++ ((coker vars R)**R^{-2});
    mR := aInfinity R; mM := aInfinity(mR,M);
    op := oppositeAInfinityAlgebra oppositeAInfinityAlgebra mR;
    ks := select(keys mR,k->class k === List);
    assert all(ks,k->op#k == mR#k);
    h := dualAInfinityStructure(mR,mM);
    h2 := dualAInfinityStructure(h#"algebra",h);
    assert(h2#"resolution" == mM#"resolution");
    assert all(select(keys mM,k->class k === List),k->h2#k == mM#k);
    h0 := dualAInfinityStructure(mR,mM,LengthLimit=>0);
    assert all(select(keys h0,k->class k === List),k->#k == 1);
    assert(h0#"resolution" == h#"resolution"));
///

TEST ///
-- Independent Ext oracle for a non-Cohen--Macaulay ring with two supports.
S = QQ[x,y,z]
R = S/(x*ideal(x,y,z))
D = dualizingComplexViaAInfinity R
assert(D.dd^2 == 0)
assert(select(toList(min D..max D),i->prune HH_i D != 0) == {0,2})
assert isIsomorphic(prune HH_0 D,coker vars R)
assert isIsomorphic(prune HH_2 D,R^1/ideal(R_0))
for i from min D to max D do (
    H := prune pushForward(map(R,S),HH_i D);
    E := prune Ext^(dim S-i)(coker presentation R,S^1);
    assert((H == 0 and E == 0) or isIsomorphic(H,E)));
///

TEST ///
-- Prefix endpoints and the distinction between bounded and free models.
S = QQ[x,y,z]
R = S/ideal(x^2)
F = dualizingComplexViaAInfinity(R,Bounded=>false,LengthLimit=>2)
assert((min F,max F) == (2,2))
assert isFreeModule F_2
F = dualizingComplexViaAInfinity(R,Bounded=>false,LengthLimit=>4)
assert((min F,max F) == (2,4))
assert(F.dd^2 == 0)
assert all(toList(2..4),i->isFreeModule F_i)
D = dualizingComplexViaAInfinity R
assert(prune HH_2 D != 0 and prune HH_3 D == 0)
assert(D == dualizingComplexViaAInfinity(R,Check=>false,VerifyComplex=>false))
///

TEST ///
-- The interior-degree convention is invariant under shifts.
S = QQ[x]
for b in {-3,-1,0,2} do (
    C := complex({map(S^1,S^1,0),map(S^1,S^1,0)},Base=>b);
    assert not isLocallyAcyclic C;
    E := complex({id_(S^1),map(S^1,S^0,0)},Base=>b);
    assert isLocallyAcyclic E);
///

TEST ///
-- Earlier review fixes: Ideal/infinity dispatch and oversized algebra limits.
S = QQ[x,y]
I = ideal(x^2,y^2)
assert(hasMinimalMult(I,infinity) == hasMinimalMult(S/I,infinity))
assert not hasMinimalMult(I,infinity)
assert hasMinimalMult(ideal(x^3),infinity)
R = S/I
full = aInfinity R
large = aInfinity(R,LengthLimit=>100,Check=>true)
ks = sort select(keys full,k->class k === List)
assert(ks == sort select(keys large,k->class k === List))
assert all(ks,k->full#k == large#k)
assert(large#"lengthLimit" == full#"lengthLimit")
///

TEST ///
-- Distinguish an intrinsic test from the bound for the input presentation.
S=QQ[x,y,z]
I=ideal(S_0-S_1^2,S_1-S_2^2)
assert isGolodAInf I
assert(try(hasMinimalMult I;false) else true)
assert isGolodAInf(I+ideal(S_2^3))
T=QQ[u,v,w]
J=ideal(T_0-T_1^2,T_1^2,T_2^2)
assert not isGolodAInf J
///

TEST ///
-- Rational points can be supplied in either the quotient or its ambient.
-- A selected-model computation elsewhere must not change this result.
S=QQ[x,y]
P=ideal(S_0-1,S_1+2)
I=ideal((S_0-1)^2,(S_1+2)^2)
R=S/I
assert not isGolodAInf(R,P)
assert not isGolodAInf(R,substitute(P,R))
assert not isGolodAInf(I,P)
assert(try (isGolodAInf R;false) else true)
assert(try (isGolodAInf(R,ideal vars S);false) else true)
assert(try (isGolodAInf(R,ideal(1_R));false) else true)
assert(try (isGolodAInf(S,ideal S_0);false) else true)
assert(try (isGolodAInf(S,ideal(S_0^2+1,S_1));false) else true)
T=QQ[a]
assert(try (isGolodAInf(R,ideal T_0);false) else true)
///

TEST ///
-- No global polynomial substitution can split this product into one field.
-- At either rational point the local ring is a field.
S=QQ[x]
I=ideal(S_0*(S_0-1))
R=S/I
assert isGolodAInf R
assert isGolodAInf(R,ideal(S_0-1))
assert isGolodAInf(R,ideal(R_0-1))
assert(try (isGolodAInf(R,ideal(S_0-2));false) else true)
-- The same argument is valid over a finite field.
T=ZZ/5[a,b]
assert not isGolodAInf(T/ideal((T_0-2)^2,(T_1+1)^2),ideal(T_0-2,T_1+1))
///

TEST ///
-- Two independent nonlinear local equations prevent polynomial elimination.
-- They become regular parameters after completion. Both answers occur.
S=QQ[x,y,z]
f=S_0+S_0^2
assert isGolodAInf(S/ideal(f,S_1^2))
assert not isGolodAInf(S/ideal(f,S_1^2,S_2^2))
assert isGolodAInf(S/(ideal f+(ideal(S_1,S_2))^2))
T=QQ[a,b,c,d]
L=ideal(T_0+T_0^2,T_1+T_1^2)
assert not isGolodAInf(T/(L+ideal(T_2^2,T_3^2)))
assert isGolodAInf(T/(L+(ideal(T_2,T_3))^2))
///

TEST ///
-- A disjoint closed component can make the GLOBAL free S-resolution
-- nonminimal at the origin, even for an ideal contained in the square.
S=QQ[x,y]
D=ideal(1-S_0,S_1)
H=S/(ideal(S_0^2)*D)
C=S/(ideal(S_0^2,S_1^2)*D)
assert not isLocallyMinimal((aInfinity H)#"SResolution")
assert not isLocallyMinimal((aInfinity C)#"SResolution")
assert isGolodAInf H
assert not isGolodAInf C
assert(try (hasMinimalMult(C,2);false) else true)
T=QQ[]
assert isGolodAInf T
assert isGolodAInf(T,ideal vars T)
///

load "./AInfinity/typed-tests.m2"
load "./AInfinity/regressions.m2"
load "./AInfinity/merged-tests.m2"

end
