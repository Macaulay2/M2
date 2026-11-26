-- shifted A-infinity algebra

-- Keys:
---- natural    Underlying graded module
---- ring       Ring associated to the A-infinity algebra
---- mm         HashTable -- multiplication maps
---- d          differential B_2 -> Q^1

SAInfAlg = new Type of HashTable

-- Constructors for shifted A-infinity algebras

sAInfAlg = method( TypicalValue => SAInfAlg,
                   Options => {LengthLimit => infinity,
                               TensorOrder => infinity} )

-- The bounds on identities describe unknown operations outside a computation.
-- In a complete structure an omitted component is the zero map.
knownBound := (X,k) -> if X#?cache and X.cache#?k then X.cache#k else infinity
setCheckBounds := (X,n,ell) -> (
    if n < infinity then X.cache#"AInfinityCheckOrder" = n;
    if ell < infinity then X.cache#"AInfinityCheckLimit" = ell;
    if n === infinity and ell === infinity then return X;
    -- Dense explicit module constructors use zero padding. A partial object
    -- may only retain padding for components whose values are actually known.
    shifted := class X === SAInfAlg or class X === SAInfMod;
    maps := select(pairs X#"mm",(k,f) -> #k == 1 or (#k <= n and
        sum k + (if shifted then -1 else #k-2) <= ell+1));
    if #maps == #(X#"mm") then return X;
    knownMaps := hashTable maps;
    new (class X) from applyPairs(X,(k,v) -> (k,if k === "mm" then knownMaps else v)))
inheritCheckBounds := (X,Y,arityShift,degreeShift) -> setCheckBounds(X,
    min(knownBound(X,"AInfinityCheckOrder"),knownBound(Y,"AInfinityCheckOrder")+arityShift),
    min(knownBound(X,"AInfinityCheckLimit"),knownBound(Y,"AInfinityCheckLimit")+degreeShift))
-- Copies and suspensions describe the same model and must retain both its
-- point of computation and any omitted terms of its ambient resolution.
inheritStructure := (X,Y,degreeShift) -> (
    X = inheritCheckBounds(X,Y,0,degreeShift);
    if Y#?cache then (
        for k in {"AInfinityPresentedRing","AInfinityModelPoint"} do
            if Y.cache#?k then X.cache#k = Y.cache#k;
        for k in {"AInfinityResolutionThrough","AInfinityLimit"} do
            if Y.cache#?k then X.cache#k = min(knownBound(X,k),Y.cache#k + degreeShift);
        if Y.cache#?"AInfinityOrder" then X.cache#"AInfinityOrder" =
            min(knownBound(X,"AInfinityOrder"),Y.cache#"AInfinityOrder");
        );
    X)
boundedOrder := (requested,maximum) -> (
    if requested === null or requested === infinity then return maximum;
    if not instance(requested,ZZ) or requested < 1 then error "TensorOrder must be a positive integer or infinity";
    min(requested,maximum))
boundedLength := (requested,maximum) -> (
    if requested === infinity then return maximum;
    if not instance(requested,ZZ) or requested < 0 then error "LengthLimit must be a nonnegative integer or infinity";
    min(requested,maximum))

checkComponents := (algebra,modulePart,mm,shifted,isModule) -> (
    for k in keys mm do (
        if not instance(k,List) or #k == 0 or not all(k,i -> instance(i,ZZ)) then
            error "expected a nonempty list of homological degrees";
        ak := if isModule then drop(k,-1) else k;
        if any(ak,i -> i < (if shifted then 2 else 1)) then error "invalid algebra input degree";
        factors := apply(ak,i -> algebra_i);
        if isModule then factors = append(factors,modulePart_(last k));
        j := sum k + (if shifted then -1 else #k-2);
        f := mm#k;
        if not instance(f,Matrix) or source f != tensor factors or target f != modulePart_j then
            error("operation has incompatible source or target at " | toString k);
        ))

typedAlgebra := (T,Q,C,mm,d,o) -> (
    shifted := T === SAInfAlg;
    loDegree := if shifted then 2 else 1;
    if ring C =!= Q or min C < loDegree then error "invalid underlying graded module";
    if source d != C_loDegree or target d != Q^1 then error "invalid augmentation map";
    checkComponents(C,C,mm,shifted,false);
    limit := boundedLength(o.LengthLimit,max C);
    maxOrder := (limit + (if shifted then 1 else 2)) // 2;
    order := boundedOrder(o.TensorOrder,maxOrder);
    -- Preserve the degree of the bottom term even after truncating it to zero.
    natural := if limit < loDegree then complex(Q^0,Base=>loDegree)
        else gradedModule naiveTruncation(C,,limit);
    if limit < loDegree then d = map(Q^1,natural_loDegree,0);
    ops := hashTable select(pairs mm,(k,f) -> #k <= order and max k <= limit
        and sum k + (if shifted then -1 else #k-2) <= limit);
    X := new T from hashTable {"natural"=>natural,"ring"=>Q,"mm"=>ops,"d"=>d,cache=>new CacheTable};
    X.cache#"AInfinityOrder" = order;
    X.cache#"AInfinityLimit" = limit;
    if limit < max C then X.cache#"AInfinityResolutionThrough" = limit;
    setCheckBounds(X,if order < maxOrder then order else infinity,
        if limit < max C then limit-1 else infinity))

sAInfAlg (Ring,Complex,HashTable,Matrix) := SAInfAlg => o -> (Q,C,mm,d) ->
    typedAlgebra(SAInfAlg,Q,C,mm,d,o)
sAInfAlg (SAInfAlg) := SAInfAlg => o -> B ->
    inheritStructure(sAInfAlg(B#"ring",B#"natural",B#"mm",B#"d",o),B,0)
-- View a ring as a shifted A-infinity algebra over itself
sAInfAlg (Ring) := SAInfAlg => o -> (Q) -> (
   zeroCx := (complex Q^0)[-2];

   return sAInfAlg(Q,zeroCx,hashTable {},map(Q^1, zeroCx_2,0),o);
)

-- View a quotient ring as a shifted A-infinity algebra over its ambient ring
sAInfAlg (QuotientRing) := SAInfAlg => o -> (R) -> (
   Bnat := complex(image presentation R)[-2];

   return sAInfAlg(ring Bnat,Bnat,hashTable {},inducedMap((ring Bnat)^1,Bnat_2),o);
)

-- A-infinity algebra

-- Keys:
---- natural    Underlying graded module
---- ring       Ring associated to the A-infinity algebra
---- mm         HashTable -- multiplication maps
---- d          the `curvature term'

AInfAlg = new Type of HashTable

-- Constructors for A-infinity algebras

aInfAlg = method( TypicalValue => AInfAlg,
                  Options => {LengthLimit => infinity,
                              TensorOrder => infinity} )

aInfAlg (Ring,Complex,HashTable,Matrix) := AInfAlg => o -> (Q,C,mm,d) ->
    typedAlgebra(AInfAlg,Q,C,mm,d,o)
aInfAlg (AInfAlg) := AInfAlg => o -> A ->
    inheritStructure(aInfAlg(A#"ring",A#"natural",A#"mm",A#"d",o),A,0)
-- View a ring as a A-infinity algebra over itself
aInfAlg (Ring) := AInfAlg => o -> (Q) -> (
   zeroCx := (complex Q^0)[-1];

   return aInfAlg(Q,zeroCx,hashTable {},map(Q^1, zeroCx_1,0),o);
)

-- View a quotient ring as an A-infinity algebra over its ambient ring
aInfAlg (QuotientRing) := AInfAlg => o -> (R) -> (
   Anat := complex(image presentation R)[-1];

   return aInfAlg(ring Anat,Anat,hashTable {},inducedMap((ring Anat)^1,Anat_1),o);
)

-- get the differentials from mm's given on generators

getExplicitDiffs = method( TypicalValue => List )

getExplicitDiffs (Ring,HashTable) := List => (Q,M) -> (
   if not all(keys M,k -> instance(k,List) and #k > 0 and all(k,
       p -> instance(p,List) and #p == 2 and all(p,i -> instance(i,ZZ)) and p#0 >= 1 and p#1 >= 0)) then
       error "expected keys made of {homological degree, zero-based generator index}";
   unary := select(keys M,k -> #k == 1);
   if #unary == 0 then error "specify the unary map on every basis element";
   degreesPresent := sort unique apply(unary,k -> k#0#0);
   curModule := Q^1;
   differentials := for i from 1 to last degreesPresent list (
       generators := sort apply(select(unary,k -> k#0#0 == i),k -> k#0#1);
       if generators != toList(0..#generators-1) then error "missing generator index in unary maps";
       columns := apply(generators,j -> M#{{i,j}});
       if not all(columns,c -> instance(c,List) and #c == numgens curModule) then
           error "wrong number of coordinates in unary map";
       f := if #columns == 0 or numgens curModule == 0 then map(curModule,Q^(#columns),0)
           else map(curModule,,transpose columns);
       curModule = source f;
       f);
   differentials
)

-- computes a single multiplication matrix corresponding to nList

stackExplicitMult = method( TypicalValue => Matrix )

stackExplicitMult (List,Complex,HashTable) := Matrix => (nList,F,M) -> (
   theseKeys := select(keys M, k -> (k / first) == nList);
   thisTarget := F_(sum nList + #nList - 2);
   thisSource := tensor apply(nList, i -> F_i);
   map(thisTarget,thisSource,0) + sum for k in theseKeys list (
       if any(k,p -> p#1 >= numgens F_(p#0)) then error "generator index out of range";
       if not instance(M#k,List) or #M#k != numgens thisTarget then error "wrong number of product coordinates";
       projection := tensor apply(k,p -> (F_(p#0))^{p#1});
       term := map(thisTarget,target projection,if #M#k == 0 then 0 else transpose {M#k});
       term * projection)
);

-- compute all the multiplications that are possible based on
-- the graded module given

stackExplicitMults = method( TypicalValue => HashTable )

stackExplicitMults (Complex,HashTable) := HashTable => (F,M) -> (
   presentMults := unique apply(select(keys M, k -> #k > 1), l -> l / first);
   mm := hashTable apply(presentMults, p -> (p,stackExplicitMult(p,F,M)));
   mm
)

aInfAlg (Ring,HashTable) := AInfAlg => o -> (Q,mm) -> (
   if #mm == 0 then return aInfAlg(Q,complex(Q^0,Base=>1),mm,map(Q^1,Q^0,0),o);
   diffs := getExplicitDiffs(Q,mm);
   if not all(#diffs-1, i -> source (diffs#i) === target (diffs#(i+1))) then
      error "Expected compatible first order maps.";
   --Anat := gradedModule apply(diffs, di -> source di);
   Anat := complex apply(diffs, di -> source di);
   Anat = Anat[-1];
   stackMs := stackExplicitMults(Anat,mm);
   -- all but the first differential
   stackMs = merge(stackMs, hashTable apply(#diffs-1, i -> ({i+2},diffs#(i+1))), (x,y) -> continue);

   return aInfAlg(Q,Anat,stackMs,diffs#0,o);
)

-- shifted A-infinity module

-- Keys:
---- natural    Underlying graded module (as a Complex with 0 diffl)
---- algebra    SAInfAlg over which this object is defined
---- mm         HashTable -- multiplication maps

SAInfMod = new Type of HashTable

-- Constructors for shifted A-infinity modules

sAInfMod = method( TypicalValue => SAInfMod,
                   Options => {TensorOrder => null} )

typedModule := (T,A,C,mm,o) -> (
    shifted := T === SAInfMod;
    if A#"ring" =!= ring C then error "incompatible base rings";
    checkComponents(A#"natural",C,mm,shifted,true);
    maxOrder := (max C - min C + 3) // 2;
    order := boundedOrder(o.TensorOrder,maxOrder);
    maps := hashTable select(pairs mm,(k,f) -> #k <= order);
    maps = if shifted then cleanupSModMults(A#"natural",C,maps,order) else cleanupModMults(A#"natural",C,maps,order);
    X := new T from hashTable {"natural"=>gradedModule C,"algebra"=>A,"mm"=>maps,cache=>new CacheTable};
    X.cache#"AInfinityOrder" = order;
    X.cache#"AInfinityLimit" = max C;
    X = setCheckBounds(X,if order < maxOrder then order else infinity,infinity);
    -- Only at most n-1 algebra inputs enter an n-input module identity.
    inheritCheckBounds(X,A,1,min C + (if shifted then 0 else 1)))

sAInfMod (SAInfAlg,Complex,HashTable) := SAInfMod => o -> (B,C,mm) ->
    typedModule(SAInfMod,B,C,mm,o)
sAInfMod (SAInfMod) := SAInfMod => o -> G ->
    inheritStructure(sAInfMod(G#"algebra",G#"natural",G#"mm",o),G,0)
-- View a shifted A-infinity algebra as a module over itself
sAInfMod (SAInfAlg) := SAInfMod => o -> (B) -> (
   -- first construct the shifted A-infinity module Σ B, then unshift
   Q := B#"ring";
   Gnat := (complex Q^1)[-1] ++ B#"natural"; -- this has labels coming from ++

   (lo,hi) := concentration Gnat;
   newmms := {({2},-B#"d")} |
               apply(toList(lo+1..hi),d -> (
                  Gd1 := Gnat_d ** Gnat_1;
                  ({d,1},map(Gnat_d,Gd1,(-1)^(d+1) * id_Gd1))
               ));

   oldmms := pairs B#"mm";

   -- make sure that no keys exist twice
   if B#"mm"#?{2} then oldmms = delete(({2},B#"mm"#{2}),oldmms);
   scan (toList (lo+1..hi),d -> (
      if B#"mm"#?{d,1} then oldmms = delete(({d,1},B#"mm"#{d,1}),oldmms);
   ));

   G := sAInfMod(B,Gnat,hashTable (newmms | oldmms),o);

   return inheritStructure(G[1],B,-1);
)

cleanupSModMults = method( TypicalValue => HashTable )

cleanupSModMults (Complex,Complex,HashTable,ZZ) := HashTable => (Bnat,Gnat,mm,tensorOrder) -> (

   shiftedComps := (n,ell) -> (
      -- returns a list with elements {(n-1):[2,maxBnat],[minGnat,maxGnat]}
      c := apply(compositions(n,ell-min Gnat), p -> p + (toList((n-1):0) | {min Gnat}));
      select(c,p -> (min drop(p,-1) >= 2 and max drop(p,-1) <= max Bnat and last p >= min Gnat and last p <= max Gnat))
   );

   zeroMap := p -> map(Gnat_(sum p - 1),tensor(apply(drop(p,-1), j -> Bnat_j) | {Gnat_(last p)}),0);

   -- assign to all keys missing in mm the zero map
   missingMm := new MutableHashTable from {};
   for n from 1 to tensorOrder do
      for ell from min Gnat to max Gnat do
         for degs in shiftedComps(n,ell+1) do
            if not mm#?degs then missingMm#degs = zeroMap degs;

   cleanMm := merge(mm,missingMm, (x,y) -> error "Internal error occurred.");

   return cleanMm;
)

-- shifted A-infinity module

-- Keys:
---- natural    Underlying graded module (as a Complex with 0 diffl)
---- algebra    AInfAlg over which this object is defined
---- mm         HashTable -- multiplication maps

AInfMod = new Type of HashTable

-- Constructors for A-infinity modules

aInfMod = method( TypicalValue => AInfMod,
                   Options => {TensorOrder => null} )

aInfMod (AInfAlg,Complex,HashTable) := AInfMod => o -> (A,C,mm) ->
    typedModule(AInfMod,A,C,mm,o)
aInfMod (AInfMod) := AInfMod => o -> F ->
    inheritStructure(aInfMod(F#"algebra",F#"natural",F#"mm",o),F,0)
-- View a A-infinity algebra as a module over itself
aInfMod (AInfAlg) := AInfMod => o -> (A) -> (
   -- Underlying module
   Q := A#"ring";
   Fnat := (complex Q^1) ++ A#"natural"; -- this has labels from ++

   (lo,hi) := concentration Fnat;
   newmms := {({1},A#"d")} |
              apply(toList(lo+1..hi),d -> (
                  Fd1 := Fnat_d ** Fnat_0;
                  ({d,0},map(Fnat_d,Fd1,id_Fd1))
               ));
   oldmms := pairs A#"mm";

   -- make sure that no keys exist twice
   if A#"mm"#?{1} then oldmms = delete(({1},A#"mm"#{1}),oldmms);
   scan (toList (lo+1..hi),d -> (
      if A#"mm"#?{d,0} then oldmms = delete(({d,0},A#"mm"#{d,0}),oldmms);
   ));

   return inheritStructure(aInfMod(A,Fnat,hashTable (newmms | oldmms),o),A,0);
)

cleanupModMults = method( TypicalValue => HashTable )

cleanupModMults (Complex,Complex,HashTable,ZZ) := HashTable => (Anat,Fnat,mm,tensorOrder) -> (

   comps := (n,ell) -> (
      -- returns a list with elements {(n-1):[minAnat,maxAnat],[minFnat,maxFnat]}
      c := apply(compositions(n,ell-min Fnat), c -> c + (toList((n-1):0) | {min Fnat}));
      select(c, p -> min drop(p,-1) >= 1 and max drop(p,-1) <= max Anat and last p >= min Fnat and last p <= max Fnat)
   );

   zeroMap := degs -> map(Fnat_(sum degs + #degs - 2),tensor(apply(drop(degs,-1), i -> Anat_i) | {Fnat_(last degs)}),0);

   -- assign to all keys missing in mm the zero map
   missingMs := new MutableHashTable from {};
   for n from 1 to tensorOrder do
      for ell from min Fnat to max Fnat do
         for degs in comps(n,ell-n+2) do
            if not mm#?degs then missingMs#degs = zeroMap degs;

   cleanMs := merge(mm,missingMs, (x,y) -> error "Internal error occurred.");

   cleanMs
)

-- Transformation between shifted and non-shifted A-infinity structures

sAInfAlg (AInfAlg) := SAInfAlg => o -> (A) -> (
   -- sign without evaluation: Σ μ_n = (-1)^n m_n Σ^n
   -- where m_n the shifted multiplication, and μ_n the unshifted
   -- then Σ μ_n Σ^(-n) = (-1)^(n + n(n-1)/2) m_n

   -- sign for evaluating Σ^(-n) (b_1, ..., b_n)
   signUnshiftedTensor := degs -> (-1)^(sum( apply(degs, reverse(0..#degs-1), (u,j) -> u*j) ));

   shiftedMm := hashTable apply(pairs A#"mm",(degs,v) -> (degs + toList(#degs:1), (-1)^(#degs + #degs*(#degs-1)/2) * signUnshiftedTensor(degs + toList(#degs:1)) * v));
   inheritStructure(sAInfAlg(A#"ring",(A#"natural")[-1],shiftedMm,A#"d",o),A,1)
)

aInfAlg (SAInfAlg) := AInfAlg => o -> (B) -> (
   -- sign without evaluation: Σ μ_n = (-1)^n m_n Σ^n
   -- where m_n the shifted multiplication, and μ_n the unshifted

   -- sign for evaluating Σ^n (a_1, ..., a_n)
   signShiftedTensor := degs -> (-1)^(sum( apply(degs, reverse(0..#degs-1), (u,j) -> u*j) ));

   unshiftedMm := hashTable apply(pairs B#"mm",(degs,v) -> (degs - toList(#degs:1), (-1)^(#degs) * signShiftedTensor(degs - toList(#degs:1)) * v));
   inheritStructure(aInfAlg(B#"ring",(B#"natural")[1],unshiftedMm,B#"d",o),B,-1)
)

sAInfMod (AInfMod) := SAInfMod => o -> (F) -> (
   -- sign without evaluation: μ_n = m_n (Σ**(n-1) ** id)
   -- where m_n the shifted multiplication, and μ_n the unshifted
   -- then μ_n  (Σ^(-1)**(n-1) ** id) = (-1)^((n-1)(n-2)/2) m_n

   -- sign for evaluating (Σ^(-1)^**(n-1) ** id) (b_1, ..., b_(n-1), g_n)
   signUnshiftedTensor := degs -> (-1)^(sum( apply(drop(degs,-1),reverse(0..#degs-2),(i,j) -> i*j)));

   shiftedMm := hashTable apply(pairs F#"mm",(degs,v) -> (degs + (toList(#degs-1:1) | {0}),(-1)^((#degs-1)*(#degs-2)/2) * signUnshiftedTensor(degs + (toList(#degs-1:1) | {0})) * v));

   return inheritStructure(sAInfMod(sAInfAlg F#"algebra",F#"natural",shiftedMm,o),F,0);
)

aInfMod (SAInfMod) := AInfMod => o -> (G) -> (
   -- sign without evaluation: μ_n = m_n (Σ**(n-1) ** id)
   -- where m_n the shifted multiplication, and μ_n the unshifted

   -- sign for evaluating (Σ^(-1)^**(n-1) ** id) (b_1, ..., b_(n-1), g_n)
   signShiftedTensor := degs -> (-1)^(sum( apply(drop(degs,-1),reverse(0..#degs-2),(i,j) -> i*j)));

   unshiftedMm := hashTable apply(pairs G#"mm",(degs,v) -> (degs - (toList(#degs-1:1) | {0}), signShiftedTensor(degs - (toList(#degs-1:1) | {0})) * v));

   return inheritStructure(aInfMod(aInfAlg G#"algebra",G#"natural",unshiftedMm,o),G,0);
)

-- Basic operations

-- Comparing (shifted) A-infinity structures

compareMults = method( TypicalValue => Boolean)

compareMults (HashTable,HashTable) := Boolean => (mm1,mm2) -> (
   mmKeys := unique join(keys mm1,keys mm2);

   for degs in mmKeys do (
      -- degs key in only one of the mm's, but zero in the other
      if (mm1#?degs and not mm2#?degs and mm1#degs != 0) then return false;
      if (mm2#?degs and not mm1#?degs and mm2#degs != 0) then return false;

      -- degs key in both
      if (mm1#?degs and mm2#?degs and mm1#degs != mm2#degs) then return false;
   );

   return true
)

SAInfAlg == SAInfAlg := Boolean => (B1,B2) -> (
   if any({"AInfinityCheckOrder","AInfinityCheckLimit"},k -> knownBound(B1,k) != knownBound(B2,k)) then return false;
   if not (B1#"natural" == B2#"natural") then return false;
   if not (B1#"d" == B2#"d") then return false;

   return compareMults(B1#"mm",B2#"mm");
)

AInfAlg == AInfAlg := Boolean => (A1,A2) -> (
   if any({"AInfinityCheckOrder","AInfinityCheckLimit"},k -> knownBound(A1,k) != knownBound(A2,k)) then return false;
   if not (A1#"natural" == A2#"natural") then return false;
   if not (A1#"d" == A2#"d") then return false;

   return compareMults(A1#"mm",A2#"mm");
)

SAInfMod == SAInfMod := Boolean => (G1,G2) -> (
   if any({"AInfinityCheckOrder","AInfinityCheckLimit"},k -> knownBound(G1,k) != knownBound(G2,k)) then return false;
   if not (G1#"natural" == G2#"natural") then return false;
   if not (G1#"algebra" == G2#"algebra") then return false;

   return compareMults(G1#"mm",G2#"mm");
)

AInfMod == AInfMod := Boolean => (F1,F2) -> (
   if any({"AInfinityCheckOrder","AInfinityCheckLimit"},k -> knownBound(F1,k) != knownBound(F2,k)) then return false;
   if not (F1#"natural" == F2#"natural") then return false;
   if not (F1#"algebra" == F2#"algebra") then return false;

   return compareMults(F1#"mm",F2#"mm");
)

-- Underlying complex of a (shifted) A-infinity structures

underlyingComplex := (X,loDegree,hiDegree,hasUnit,unitMap) -> (
    C := X#"natural";
    Q := ring C;
    moduleAt := i -> if hasUnit and i == loDegree then Q^1 else C_i;
    if loDegree == hiDegree then return complex(moduleAt loDegree,Base=>loDegree);
    complex(for i from loDegree+1 to hiDegree list (
        if hasUnit and i == loDegree+1 then unitMap
        else if X#"mm"#?{i} then X#"mm"#{i}
        else map(moduleAt(i-1),moduleAt i,0)),Base=>loDegree))
complex (SAInfAlg) := Complex => {} >> o -> B -> underlyingComplex(B,1,max B#"natural",true,-B#"d")
complex (AInfAlg) := Complex => {} >> o -> A -> underlyingComplex(A,0,max A#"natural",true,A#"d")
complex (SAInfMod) := Complex => {} >> o -> G -> underlyingComplex(G,min G#"natural",max G#"natural",false,null)
complex (AInfMod) := Complex => {} >> o -> F -> underlyingComplex(F,min F#"natural",max F#"natural",false,null)

-- Suspensions of (shifted) A-infinity modules

SAInfMod Array := SAInfMod => (G,L) -> (
   if #L != 1 or not instance(L#0,ZZ) then error "expected an integer shift";

   -- sign without evaluation: mk_n (id^(n-1) ** Σ^k) = (-1)^k Σ^k m_n
   -- where m_n the multiplication of G, and mk_n the multiplication of Σ^k G
   -- then mk_n = (-1)^k Σ m_n (id^(n-1) ** Σ^(-k))

   -- sign for evaluation (id^(n-1) ** Σ^(-k)) (b_1, ..., b_(n-1), g_n)
   signShiftedTensor := degs -> (-1)^(sum(drop(degs,-1))*L_0);

   suspendedMm := apply(pairs G#"mm", (degs,v) -> (degs + (toList(#degs-1:0) | {-L_0}),(-1)^(L_0) * signShiftedTensor(degs) * v));

   return inheritStructure(sAInfMod(G#"algebra",G#"natural" L,hashTable suspendedMm),G,-L_0);
)

AInfMod Array := AInfMod => (F,L) -> (
   if #L != 1 or not instance(L#0,ZZ) then error "expected an integer shift";

   -- μk_n (id^(n-1) ** Σ^k) = (-1)^(k*n) Σ^k μ_n
   -- where μ_n the multiplication of F and μk_n the multiplication of Σ^k F
   -- then μk_n = (-1)^(k*n) Σ^k μ_n (id^(n-1) ** Σ^(-k))

   -- sign for evaluation (id^(n-1) ** Σ^(-k)) (a_1, ..., a_(n-1), f_n)
   signShiftedTensor := degs -> (-1)^(sum(drop(degs,-1))*L_0);

   suspendedMm := apply(pairs F#"mm", (degs,v) -> (degs + (toList(#degs-1:0) | {-L_0}),(-1)^(L_0 * #degs) * signShiftedTensor(degs) * v));

   return inheritStructure(aInfMod(F#"algebra",F#"natural" L,hashTable suspendedMm),F,-L_0);
)

-- Summands of Stasheff identity

-- Sign for various (shifted) A-infinity structures
-- coming from evaluation and the formulas

signShiftedStasheff = method( TypicalValue => ZZ )

signShiftedStasheff (ZZ,ZZ,ZZ,List) := ZZ => (r,s,t,degs) -> (
   return (-1)^(sum apply(r, i -> degs_i));
)

signShiftedStasheff(ZZ,ZZ,List) := ZZ => (r,p,degs) -> (
   -- input
   --    p = r+s-1, degs
   --    where r+s+t = n
   return signShiftedStasheff(r,p-r+1,#degs-p-1,degs);
)

signStasheff = method( TypicalValue => ZZ )

signStasheff (ZZ,ZZ,ZZ,List) := ZZ => (r,s,t,degs) -> (
   return (-1)^(r + s*t + (s-2) * sum apply(r, i -> degs_i));
)

signStasheff (ZZ,ZZ,List) := ZZ => (r,p,degs) -> (
   return signStasheff(r,p-r+1,#degs-p-1,degs);
)

-- Indices of Stasheff summands restricted to certain degrees

sIndicesStasheff = method( TypicalValue => List )

sIndicesStasheff (List) := List => (degs) -> (
   -- input a list of degrees
   -- output: list with elements of the form {r,r+s-1,{v_0..v_(r+1+t)}}
   --         each entry corresponds to the summand (r,s,t) of the Stasheff identity
   --         where r+s+t = n := #degs, s >= 1, which corresponds to collapsing
   --         u_r..u_(r+s-1) to v_r in the new degree list:
   --         {v_0..v_(r+1+t)} = {u_0..u_(r-1), v_r, u_(r+s)..u_(n-1)}
   --         sign is (-1)^sum(apply r, i->deg_i).

   -- summands (r,s,t) with r+s+t = n and s >= 1
   comps := n -> select(compositions(3,n), t -> t_1 >= 1);

   n := #degs;

   return apply(comps(n), t -> {t_0, n-t_2-1, degs_{0..t_0-1} |
                               {sum(degs_{t_0..(t_0+t_1-1)})-1} |
                               degs_{(t_0+t_1)..n-1}});
)

indicesStasheff = method( TypicalValue => List )

indicesStasheff (List) := List => (degs) -> (
   -- input a list of degrees
   -- output: list with elements of the form {r,r+s-1,{v_0..v_(r+1+t)}}
   --         each entry corresponds to the summand (r,s,t) of the Stasheff identity
   --         where r+s+t = n := #degs, s >= 1, which corresponds to collapsing
   --         u_r..u_(r+s-1) to v_r in the new degree list:
   --         {v_0..v_(r+1+t)} = {u_0..u_(r-1), v_r, u_(r+s)..u_(n-1)}
   --         v_r = u_r+...+u_(r+s-1)

   -- summands (r,s,t) with r+s+t = n and s >= 1
   comps := n -> select(compositions(3,n), t -> t_1 >= 1);

   n := #degs;

   return apply(comps(n), t -> {t_0, n-t_2-1, degs_{0..t_0-1} |
                               {sum(degs_{t_0..(t_0+t_1-1)})+t_1-2} |
                               degs_{(t_0+t_1)..n-1}});
)

sAlgIndicesStasheff = method( TypicalValue => List )

sAlgIndicesStasheff (List) := List => (degs) -> (
   if sum degs - 2 < 2 then return {};

   return select(sIndicesStasheff degs, L -> all(last L, k -> k >= 2));
)

algIndicesStasheff = method( TypicalValue => List )

algIndicesStasheff (List) := List => (degs) -> (
   if sum degs + #degs - 3 < 1 then return {};

   return select(indicesStasheff degs, L -> all(last L, k -> k >= 1));
)

sModIndicesStasheff = method( TypicalValue => List )

sModIndicesStasheff (List) := List => (degs) -> (
   return select(sIndicesStasheff degs, L -> all(drop(last L,-1), k -> k >= 2));
)

modIndicesStasheff = method()

modIndicesStasheff (List) := List => (degs) -> (
   return select(indicesStasheff degs, L -> all(drop(last L,-1), k -> k >= 1));
)

-- Stasheff summands

sAlgSummandStasheff = method( TypicalValue => Matrix )

sAlgSummandStasheff (Complex,List,List,List) := Matrix => (Bnat,mBs,degs,L) -> (
   -- input:
   --    Bnat  graded module
   --    mBs = {HashTable,HashTable}
   --    degs  {u_1,...,u_n}
   --    L = {r,r+s-1,{v_1,...,v_(r+1+t)}}
   mB1 := mBs#0;
   mB2 := mBs#1;
   S := Bnat.ring;
   k1 := degs_{0..L_0-1};
   k2 := degs_{L_0..L_1};
   k3 := degs_{L_1+1..#degs-1};

   -- if L == {0,0,{2,2,2}} then error "err";

   -- Check whether mms exist, if not return 0
   if (not mB1#?(L_2) and not mB2#?(L_2)) or
      (not mB1#?k2 and not mB2#?k2) then
      return map(Bnat_(sum degs-2),tensor(apply(degs, u -> Bnat_u)),0);

   fac1 := tensor(S,apply(k1, ell -> Bnat_ell));
   fac3 := tensor(S,apply(k3, ell -> Bnat_ell));

   mBL3map := if mB1#?(L_2) then mB1#(L_2) else  mB2#(L_2);
   mBk2map := if mB1#?k2 then mB1#k2 else mB2#k2;

   return signShiftedStasheff(L_0,L_1,degs) * mBL3map * (fac1 ** mBk2map ** fac3);
)

sAlgSummandStasheff (Complex,HashTable,List,List) := Matrix =>  (Bnat,mB,degs,L) -> (
    sAlgSummandStasheff(Bnat,{mB,hashTable {}}, degs, L)
)

sAlgSummandStasheff (SAInfAlg,List,List) := Matrix => (B,degs,L) -> (
   sAlgSummandStasheff(B#"natural",B#"mm",degs,L);
)

algSummandStasheff = method( TypicalValue => Matrix )
algSummandStasheff (Complex,List,List,List) := Matrix => (Anat,mAs,degs,L) -> (
   -- input:
   --    Anat  graded module
   --    mAs = {HashTable,HashTable}
   --    degs  {u_1,...,u_n}
   --    L = {r,r+s-1,{v_1,...,v_(r+1+t)}}
   mA1 := mAs#0;
   mA2 := mAs#1;
   S := Anat.ring;
   k1 := degs_{0..L_0-1};
   k2 := degs_{L_0..L_1};
   k3 := degs_{L_1+1..#degs-1};

   -- Check whether mms exist, if not return 0
   if (not mA1#?(L_2) and not mA2#?(L_2)) or
      (not mA1#?k2 and not mA2#?k2) then
      return map(Anat_(sum degs+#degs-3),tensor(apply(degs, u -> Anat_u)),0);

   fac1 := tensor(S,apply(k1, ell -> Anat_ell));
   fac3 := tensor(S,apply(k3, ell -> Anat_ell));

   mAL3map := if mA1#?(L_2) then mA1#(L_2) else  mA2#(L_2);
   mAk2map := if mA1#?k2 then mA1#k2 else mA2#k2;

   return signStasheff(L_0,L_1,degs) * mAL3map * (fac1 ** mAk2map ** fac3);
)

algSummandStasheff (Complex,HashTable,List,List) := Matrix => (Anat,mA,degs,L) -> (
    algSummandStasheff(Anat,{mA,hashTable {}}, degs, L)
)

algSummandStasheff (AInfAlg,List,List) := Matrix => (A,degs,L) -> (
   algSummandStasheff(A#"natural",A#"mm",degs,L);
)

-- sModSummandStasheff = method( TypicalValue => Matrix )
-- inputs: (Complex,HashTable,Complex,HashTable,List,List)
-- FIXME: not a method since methods can only take four arguments
sModSummandStasheff = (Bnat,mB,Gnat,mGs,degs,L) -> (
   local mG1;
   local mG2;
   if member(List,ancestors class mGs) then (
      mG1 = mGs#0;
      mG2 = mGs#1;
   )
   else if member(HashTable, ancestors class mGs) then (
      mG1 = mGs;
      mG2 = hashTable {};
   );
   S := ring Bnat;
   k1 := degs_{0..L_0-1};
   k2 := degs_{L_0..L_1};
   k3 := degs_{L_1+1..#degs-2};
   k4 := last degs;

   -- if the desired component is not present in either input hash table for mG or the input hash table for mA
   -- then return the zero map.
   if (not mG1#?(L_2) and not mG2#?(L_2)) or
      (L_1 == #degs-1 and not mG1#?k2 and not mG2#?k2) or
      (L_1 != #degs-1 and not mB#?k2) then
      return map(Gnat_(sum degs-2),tensor(apply(drop(degs,-1),ell -> Bnat_ell) | {Gnat_(last degs)}),0);

   fac1 := tensor(S,apply(k1, ell -> Bnat_ell));
   local mMap2;
   local fac3;

   if L_1 == #degs-1 then
   (
      fac3 = id_(S^1);
      mMap2 = if mG1#?k2 then mG1#k2 else mG2#k2;
   )
   else
   (
      fac3 = tensor(S,apply(k3, ell -> Bnat_ell)) ** Gnat_k4;
      mMap2 = mB#k2;
   );

   mGv3Map := if mG1#?(L_2) then mG1#(L_2) else mG2#(L_2);
   return signShiftedStasheff(L_0,L_1,degs) * mGv3Map * (fac1 ** mMap2 ** fac3);
)

-- modSummandStasheff = method( TypicalValue => Matrix )
-- inputs: (Complex,HashTable,Complex,HashTable,List,List)
-- FIXME: not a method since methods can only take four arguments
modSummandStasheff := (Anat,mA,Fnat,mFs,degs,L) -> (
   local mF1;
   local mF2;
   if member(List,ancestors class mFs) then (
      mF1 = mFs#0;
      mF2 = mFs#1;
   )
   else if member(HashTable, ancestors class mFs) then (
      mF1 = mFs;
      mF2 = hashTable {};
   );
   S := ring Anat;
   k1 := degs_{0..L_0-1};
   k2 := degs_{L_0..L_1};
   k3 := degs_{L_1+1..#degs-2};
   k4 := last degs;

   -- if the desired component is not present in either input hash table for mG or the input hash table for mA
   -- then return the zero map.
   if (not mF1#?(L_2) and not mF2#?(L_2)) or
      (L_1 == #degs-1 and not mF1#?k2 and not mF2#?k2) or
      (L_1 != #degs-1 and not mA#?k2) then
      return map(Fnat_(sum degs+#degs-3),tensor(apply(drop(degs,-1),ell -> Anat_ell) | {Fnat_(last degs)}),0);

   fac1 := tensor(S,apply(k1, ell -> Anat_ell));
   local mMap2;
   local fac3;

   if L_1 == #degs-1 then
   (
      fac3 = id_(S^1);
      mMap2 = if mF1#?k2 then mF1#k2 else mF2#k2;
   )
   else
   (
      fac3 = tensor(S,apply(k3, ell -> Anat_ell)) ** Fnat_k4;
      mMap2 = mA#k2;
   );

   mFv3Map := if mF1#?(L_2) then mF1#(L_2) else mF2#(L_2);
   return signStasheff(L_0,L_1,degs) * mFv3Map * (fac1 ** mMap2 ** fac3);
)

-- Summand involving the Unit, aka the curvature term "d"

sAlgSummandStasheffUnit = method( TypicalValue => Matrix )

sAlgSummandStasheffUnit (Complex,List,List,Matrix) := (Bnat,mBs,degs,d) -> (
   -- input:
   --    Bnat  graded module
   --    mBs = {HashTable,HashTable}
   --    degs  {u_1,...,u_n}
   --    d     matrix
   mB1 := mBs#0;
   mB2 := mBs#1;

   if degs == {3} then (
       if mB1#?{3} then return d * mB1#{3};
       if mB2#?{3} then return d * mB2#{3};
   );

   zeroMap := map(Bnat_(sum degs - 2),tensor apply(degs, u -> Bnat_u),0);

   if #degs == 2 then
      return (if degs_0 == 2 then (- d ** id_(Bnat_(degs_1))) else zeroMap) +  (if degs_1 == 2 then (id_(Bnat_(degs_0)) ** d) else zeroMap);

   return zeroMap;
)

sAlgSummandStasheffUnit (Complex,HashTable,List,Matrix) := (Bnat,mB,degs,d) -> (
   return sAlgSummandStasheffUnit(Bnat,{mB,hashTable {}},degs,d);
)

sAlgSummandStasheffUnit (SAInfAlg,List) := (B,degs) -> (
   return sAlgSummandStasheffUnit(B#"natural",B#"mm", degs, B#"d");
)

algSummandStasheffUnit = method( TypicalValue => Matrix )
algSummandStasheffUnit (AInfAlg,List) := (A,degs) -> (
   if #degs == 1 then
      if degs_0 == 2 and A#"mm"#?{2} then
         return A#"d" * A#"mm"#{2};

   zeroMap := map(A#"natural"_(sum degs + #degs - 3),tensor apply(degs, u -> A#"natural"_u),0);

   if #degs == 2 then
      return (if degs_0 == 1 then (- A#"d" ** id_(A#"natural"_(degs_1))) else zeroMap) +  (if degs_1 == 1 then (- (-1)^(degs_0) * id_(A#"natural"_(degs_0)) ** A#"d") else zeroMap);

   return zeroMap;
)

sModSummandStasheffUnit = method( TypicalValue => Matrix )

sModSummandStasheffUnit (Complex,SAInfAlg,List) := (Gnat,B,degs) -> (
   -- input:
   --    Gnat  graded module
   --    degs  {u_1,...,u_n}
   --    d     matrix
   if #degs == 2 and degs_0 == 2 then
      return (- B#"d" ** id_(Gnat_(degs_1)));

   zeroMap := map(Gnat_(sum degs - 2),tensor(apply(drop(degs,-1), i -> B#"natural"_i) | {Gnat_(last degs)}),0);

   return zeroMap;
)

sModSummandStasheffUnit (SAInfMod,List) := (G,degs) -> (
   return sModSummandStasheffUnit(G#"natural",G#"algebra",degs);
)

modSummandStasheffUnit = method( TypicalValue => Matrix )
modSummandStasheffUnit (AInfMod,List) := (F,degs) -> (
   if #degs == 2 and degs_0 == 1 then
      return (- F#"algebra"#"d" ** id_(F#"natural"_(degs_1)));

   zeroMap := map(F#"natural"_(sum degs + #degs - 3),tensor(apply(drop(degs,-1), i -> F#"algebra"#"natural"_i) | {F#"natural"_(last degs)}),0);

   return zeroMap;
)

-- Check Stasheff identity

checkStasheff = method( TypicalValue => Boolean )
identityOrder := (X,n) -> (
    if n < 1 then error "expected a positive arity";
    if n > knownBound(X,"AInfinityCheckOrder") then error "arity exceeds the computed structure";
    n)
lastIdentityDegree := X -> min(max X#"natural",knownBound(X,"AInfinityCheckLimit"))
checkStasheff (SAInfAlg,ZZ) := Boolean => (B,n) -> (
   identityOrder(B,n);
   if n > (max B#"natural"+2)//2 then return true;
   if n == 1 and B#"mm"#?{3} and B#"d" * B#"mm"#{3} != 0 then return false;
   shiftedComps := (n,ell) -> select(compositions(n,ell),p -> min p >= 2 and max p <= max B#"natural");

   for ell from 2 to lastIdentityDegree B do (
      -- ell is the degree of the codomain of the Stasheff identity
      for degs in shiftedComps(n,ell+2) do (
         -- degs is the list of degrees of the domain of the Stasheff identity
         stasheffSummands := apply(sAlgIndicesStasheff degs, L -> sAlgSummandStasheff(B#"natural",B#"mm",degs,L));
         stasheffSum := sum(stasheffSummands) + sAlgSummandStasheffUnit(B,degs);
         if stasheffSum != 0 then return false;
      );
   );

   return true;
)

checkStasheff (SAInfAlg) := Boolean => (B) -> (
   tensorOrder := min((max B#"natural" + 2)//2,knownBound(B,"AInfinityCheckOrder"));
   for n from 1 to tensorOrder do (
      if not checkStasheff(B,n) then return false;
   );

   return true;
)

checkStasheff (AInfAlg,ZZ) := Boolean => (A,n) -> (
   identityOrder(A,n);
   if n > (max A#"natural"+3)//2 then return true;
   if n == 1 and A#"mm"#?{2} and A#"d" * A#"mm"#{2} != 0 then return false;
   comps := (n,ell) -> select(compositions(n,ell),p -> min p >= 1 and max p <= max A#"natural");

   for ell from 1 to lastIdentityDegree A do (
      -- ell is the degree of the codomain of the Stasheff identity
      for degs in comps(n,ell-n+3) do (
         -- degs is the list of degrees of the domain of the Stasheff identity
         stasheffSum := sum(apply(algIndicesStasheff degs, L -> algSummandStasheff(A#"natural",A#"mm",degs,L))) + algSummandStasheffUnit(A,degs);
         if stasheffSum != 0 then return false;
      );
   );

   return true;
)

checkStasheff (AInfAlg) := Boolean => (A) -> (
   for n from 1 to min((max A#"natural"+3)//2,knownBound(A,"AInfinityCheckOrder")) do (
      if not checkStasheff(A,n) then return false;
   );

   return true;
)

checkStasheff (SAInfMod,ZZ) := Boolean => (G,n) -> (
   identityOrder(G,n);
   if n > (max G#"natural"-min G#"natural"+4)//2 then return true;
   Bnat := G#"algebra"#"natural";
   Gnat := G#"natural";

   shiftedComps := (n,ell) -> (
      -- returns a list with elements {(n-1):[minBnat,maxBnat],[minGnat,maxGnat]}
      c := apply(compositions(n,ell-min Gnat), p -> p + (toList((n-1):0) | {min Gnat}));
      select(c,p -> (min drop(p,-1) >= 2 and max drop(p,-1) <= max Bnat and last p >= min Gnat and last p <= max Gnat))
   );

   for ell from min Gnat to lastIdentityDegree G do (
      -- ell is the degree of the codomain of the Stasheff identity

      for degs in shiftedComps(n,ell+2) do (
         -- degs is the list of degrees of the domain of the Stasheff identity
         stasheffSum := sum(apply(sModIndicesStasheff degs, L -> sModSummandStasheff(Bnat,G#"algebra"#"mm",Gnat,G#"mm",degs,L))) + sModSummandStasheffUnit(G,degs);
         if stasheffSum != 0 then return false;
      );
   );

   return true;
)

checkStasheff (SAInfMod) := Boolean => (G) -> (
   for n from 1 to min((max G#"natural" - min G#"natural" + 4)//2,knownBound(G,"AInfinityCheckOrder")) do (
      if not checkStasheff(G,n) then return false;
   );

   return true;
)

checkStasheff (AInfMod,ZZ) := Boolean => (F,n) -> (
   identityOrder(F,n);
   if n > (max F#"natural"-min F#"natural"+4)//2 then return true;
   Anat := F#"algebra"#"natural";
   Fnat := F#"natural";

   comps := (n,ell) -> (
      -- returns a list with elements {(n-1):[minAnat,maxAnat],[minFnat,maxFnat]}
      c := apply(compositions(n,ell-min Fnat), c -> c + (toList((n-1):0) | {min Fnat}));
      select(c, p -> min drop(p,-1) >= 1 and max drop(p,-1) <= max Anat and last p >= min Fnat and last p <= max Fnat)
   );

   for ell from min Fnat to lastIdentityDegree F do (
      -- ell is the degree of the codomain of the Stasheff identity

      for degs in comps(n,ell-n+3) do (
         -- degs is the list of degrees of the domain of the Stasheff identity
         stasheffSum := sum(apply(modIndicesStasheff degs, L -> modSummandStasheff(Anat,F#"algebra"#"mm",Fnat,F#"mm",degs,L))) + modSummandStasheffUnit(F,degs);
         if stasheffSum != 0 then return false;
      );
   );

   return true;
)

checkStasheff (AInfMod) := Boolean => (F) -> (
   for n from 1 to min((max F#"natural" - min F#"natural" + 4)//2,knownBound(F,"AInfinityCheckOrder")) do (
      if not checkStasheff(F,n) then return false;
   );

   return true;
)
