-- Get an A-infinity algebra model

-- LengthLimit bounds the target degree of higher operations. The unary
-- differential always records all terms of the given ambient resolution.
checkModelOptions = o -> (
   if o.LengthLimit =!= infinity and not(instance(o.LengthLimit,ZZ) and o.LengthLimit >= 0) then
      error "LengthLimit must be a nonnegative integer or infinity";
   if o.TensorOrder =!= infinity and not(instance(o.TensorOrder,ZZ) and o.TensorOrder >= 1) then
      error "TensorOrder must be a positive integer or infinity";
   if not instance(o.Check,Boolean) then error "Check must be a Boolean";
)

-- The point selected by setMaxIdeal controls localResolution. These matrices
-- resolve the input at that point; a local resolution need not be globally exact.
-- Cache by point, base ring and requested precision; never use the generic
-- resolution cache, which may contain a prefix or another ambient resolution.
ambientModelResolution = (owner,Q,homogeneous,limit,makeModule) -> (
   point := if homogeneous then null else (
      if not Q.?maxIdeal then
         error "inhomogeneous models use localResolution: select the ambient maximal ideal with setMaxIdeal first";
      Q.maxIdeal);
   key := ("AInfinity.modelResolution",Q,point,limit);
   if owner.cache#?key then return owner.cache#key;
   N := makeModule();
   -- Compute one degree beyond the requested cutoff so a truncated model
   -- cannot masquerade as one whose ambient resolution has actually ended.
   through := if limit === infinity then infinity else max(1,limit+1);
   -- Complexes requires a finite bound over a quotient. A finite relative
   -- resolution is usable without an option; stop if the syzygies persist.
   relative := instance(Q,QuotientRing) and limit === infinity;
   depth := if relative then max(dim Q,numgens Q)+2 else through+1;
   C := if homogeneous then freeResolution(N,LengthLimit=>depth)
        else (
           -- localResolution's own module cache does not record the point.
           -- A fresh presentation gives it an independent module cache.
           if instance(owner,Module) and not isSubset(ann N,point) then complex {Q^0}
           else (
              -- General local pruning can introduce denominators in the
              -- nullhomotopies. Keep polynomial lifts even when nonminimal.
              P := presentation N;
              localN := coker map(target P,source P,entries P);
              complex localResolution(localN,LengthLimit=>depth)));
   if relative and max C == depth then (
      K := ker C.dd_depth;
      if K != 0 and (homogeneous or isSubset(ann K,point)) then
         error "the relative resolution continues at the ambient dimension bound: specify a finite LengthLimit";
   );
   -- localResolution sometimes includes a trailing zero free module.
   partial := max C > through and C_(through+1) != 0;
   if max C > through then C = naiveTruncation(C,,through);
   result := (C,partial);
   owner.cache#key = result;
   result
)

checkModelComplex = C -> (
   if not all(toList(min C..max C), i->isFreeModule C_i) then
      error "expected a complex of free ambient modules";
   if C.dd^2 != 0 then error "the given differential does not square to zero";
)

-- Returns whether a supplied free complex is only a prefix. At a specified
-- local point, homology supported away from the point is immaterial.
checkSuppliedResolution = (C,o,point,lastSupported) -> (
   if not o.Check then return false;
   survives := H -> H != 0 and (point === null or isSubset(ann H,point));
   for i from min C+1 to max C-1 do
      if survives HH_i C then error("the supplied resolution has homology in degree ",toString i);
   if max C == min C or not survives(ker C.dd_(max C)) then return false;
   if o.LengthLimit > lastSupported then
      error "the supplied resolution stops with a nonzero syzygy: supply a longer resolution or a smaller LengthLimit";
   true
)

-- Restrict scalars along a quotient by lifting the module presentation and
-- imposing the relative ring relations on each ambient generator.
restrictModelModule = (Q,M) -> (
   if Q === ring M then return M;
   P := lift(presentation M,Q);
   coker(P | target P ** presentation(Q,ring M))
)

checkModelRings = (B,M) -> (
   Q := B#"ring";
   R := ring M;
   if R =!= Q and not isQuotientOf(Q,R) then
      error "expected the module coefficient ring to be a quotient of the ambient ring";
   if B.cache#?"AInfinityPresentedRing" then (
      A := B.cache#"AInfinityPresentedRing";
      if A =!= R and not isQuotientOf(A,R) and
         not isSubset(ideal promote(presentation(Q,A),R),ann M) then
         error "the defining ideal of the supplied algebra does not annihilate the module";
   )
)

-- Given an A-(n-1)-algebra structure, compute mm_n
getNextAlgMult = method( TypicalValue => HashTable,
                         Options => {LengthLimit => infinity,
                                     Check => true} )

getNextAlgMult (Complex,ZZ,MutableHashTable,Matrix) := HashTable => o -> (Bcx,n,mm,d) -> (

   limit := if o.LengthLimit === infinity then max Bcx else min(max Bcx, o.LengthLimit);

   nextMult := new MutableHashTable;

   shiftedComps := (n,ell) -> select(compositions(n,ell),p -> min p >= 2 and max p <= max Bcx);

   for ell from 2*n-2 to limit-1 do (
      -- min(Bcx**n) = 2*n, hence in degree ell < 2*n-2 the target is zero
      -- since deg(mm) = -1, the target is zero in degree ell > limit-1
      for degs in shiftedComps(n,ell+2) do (
         U := select(sAlgIndicesStasheff degs, v-> #v_2 > 1);
         dm := - sum for v in U list sAlgSummandStasheff(Bcx,{mm,nextMult},degs,v);

         if n == 2 then dm = dm - sAlgSummandStasheffUnit(Bcx,{mm,nextMult},degs,d);

         -- check whether it is possible to obtain a higher multiplication
         if o.Check == true then (
            if dm%Bcx.dd_(ell+1) != 0 then (
               error("algebra multiplication could not be lifted in input degrees ",toString degs)
            )
         );

         mk := dm//Bcx.dd_(ell+1);
         nextMult#degs = map(Bcx_(ell+1), tensor(apply(degs, u -> Bcx_u)), mk)
      );
   );

   nextMult
)

sAInfAlgModel = method( TypicalValue => SAInfAlg,
                        Options => {LengthLimit => infinity,
                                    TensorOrder => infinity,
                                    Check => true} )

algebraModelOnResolution = (Acx,o) -> (
   -- Put an A-infinity structure on Acx which is a resolution of a QuotientRing

   checkModelOptions o;
   checkModelComplex Acx;
   Q := ring Acx;
   if min Acx != 0 or Acx_0 != Q^1 then
      error "expected a cyclic quotient resolution in degrees >= 0 with degree-zero term Q^1";
   mm := new MutableHashTable;
   Bcx := if max Acx == 0 then complex({Q^0},Base=>2)
          else naiveTruncation(Acx,1,)[-1];
   limit := min(max Bcx, o.LengthLimit);
   maxOrder := max(1,(max Bcx+1)//2);
   tensorOrder := max(1,min(o.TensorOrder,(limit+1)//2));
   for i from 3 to max Bcx do mm#{i} = Bcx.dd_i;

   -- define d
   d := map(Acx_0, Bcx_2, Acx.dd_1); --the positive sign is a literal interpretation of Burke

   --mm#{u_1..u_n}
   for n from 2 to tensorOrder do (
      nextMult := getNextAlgMult(Bcx,n,mm,d,LengthLimit => limit,Check => o.Check);
      -- merge in the new multiplications
      apply(pairs nextMult, p -> mm#(p#0) = p#1);
   );

   -- create an SAInfAlg object
   B := new SAInfAlg from hashTable {
      "natural" => gradedModule Bcx,  -- strips the diff'l from B
      "ring" => Q,
      "mm" => hashTable pairs mm,
      "d" => d,
      cache => new CacheTable };

   B.cache#"AInfinityLimit" = limit;
   B.cache#"AInfinityOrder" = tensorOrder;

   if o.TensorOrder < maxOrder then B.cache#"AInfinityCheckOrder" = o.TensorOrder;
   if limit < max Bcx and maxOrder >= 2 then B.cache#"AInfinityCheckLimit" = limit-1;
   return B;
)

sAInfAlgModel Complex := SAInfAlg => o -> Acx -> (
   checkModelOptions o;
   checkModelComplex Acx;
   Q := ring Acx;
   if min Acx != 0 or Acx_0 != Q^1 then
      error "expected a cyclic quotient resolution in degrees >= 0 with degree-zero term Q^1";
   point := if not isHomogeneous Acx and Q.?maxIdeal then Q.maxIdeal else null;
   partial := checkSuppliedResolution(Acx,o,point,max Acx+1);
   B := algebraModelOnResolution(Acx,o);
   if partial then (
      B.cache#"AInfinityResolutionThrough" = max B#"natural";
      B.cache#"AInfinityCheckLimit" = o.LengthLimit-1;
   );
   if point =!= null then B.cache#"AInfinityModelPoint" = point;
   B
)

sAInfAlgModel (Ring,QuotientRing) := SAInfAlg => o -> (Q,R) -> (
   checkModelOptions o;
   if Q === R then return sAInfAlgModel(complex {Q^1},o);
   if not isQuotientOf(Q,R) then error "expected the second ring to be a quotient of the first";
   rels := presentation(Q,R);
   homogeneous := isHomogeneous rels;
   if not homogeneous and Q.?maxIdeal and not isSubset(ideal rels,Q.maxIdeal) then
      error "the selected maximal ideal does not contain the defining ideal of the quotient";
   (Acx,partial) := ambientModelResolution(R,Q,homogeneous,o.LengthLimit,()->coker rels);
   B := algebraModelOnResolution(Acx,o);
   if partial then (
      B.cache#"AInfinityResolutionThrough" = max B#"natural";
      B.cache#"AInfinityCheckLimit" = o.LengthLimit-1;
   );
   if not homogeneous then B.cache#"AInfinityModelPoint" = Q.maxIdeal;
   B.cache#"AInfinityPresentedRing" = R;
   B
)

sAInfAlgModel PolynomialRing := SAInfAlg => o -> Q -> sAInfAlgModel(complex {Q^1},o)

sAInfAlgModel QuotientRing := SAInfAlg => o -> R ->
   sAInfAlgModel(ring presentation R,R,o)

-- Get an A-infinity module model

-- Given an A-(n-1)-module structure, compute mm_n
getNextModMult = method( TypicalValue => HashTable,
                         Options => {LengthLimit => infinity,
                                     Check => true} )

getNextModMult (SAInfAlg,Complex,ZZ,MutableHashTable) := HashTable => o -> (B,Gcx,n,mG) -> (

   limit := if o.LengthLimit === infinity then max Gcx else min(max Gcx, o.LengthLimit);

   nextMult := new MutableHashTable;

   shiftedComps := (n,ell) -> (
      -- returns a list with elements {(n-1):[2,maxBcx],[minGcx,maxGcx]}
      c := apply(compositions(n,ell-min Gcx), p -> p + (toList((n-1):0) | {min Gcx}));
      select(c,p -> (min drop(p,-1) >= 2 and max drop(p,-1) <= max B#"natural" and last p >= min Gcx and last p <= max Gcx))
   );

   for ell from 2*(n-1) + min Gcx - 2 to limit-1 do (
      -- min(Bcx**(n-1) ** Gcx) = 2*(n-1)+min Gcx, hence in degree ell < .. the target is zero
      -- since deg(mm) = -1, the target is zero in degree ell > limit-1

      for degs in shiftedComps(n,ell+2) do (
         U := select(sModIndicesStasheff degs, v -> #v_2 > 1);
         dm := - sum for v in U list sModSummandStasheff(B#"natural",B#"mm",Gcx,{mG,nextMult},degs,v);

         if n == 2 then dm = dm - sModSummandStasheffUnit(Gcx,B,degs);

         if o.Check == true then (
            if dm%Gcx.dd_(ell+1) != 0 then (
               error("module multiplication could not be lifted in input degrees ",toString degs)
            )
         );

         mk := dm//Gcx.dd_(ell+1);
         nextMult#degs = map( Gcx_(ell+1),
                              tensor(apply(drop(degs,-1), u -> B#"natural"_u) | {Gcx_(last(degs))}),
                              mk);
      )
   );

   return nextMult;
)

sAInfModModel = method( TypicalValue => SAInfMod,
                        Options => {LengthLimit => infinity,
                                    TensorOrder => infinity,
                                    Check => true})

moduleModelOnResolution = (B,Gcx,o) -> (
   -- Put an A-infinity modules structure over B on Gcx
   -- Gcx is a free resolution of M over B#"ring"

   checkModelOptions o;
   checkModelComplex Gcx;
   Q := B#"ring";
   if Q =!= ring Gcx then error "expected the algebra and module complex to have the same ambient ring";
   mG := new MutableHashTable;
   limit := min(max Gcx,o.LengthLimit);
   bars := any(toList(min B#"natural"..max B#"natural"),i->B#"natural"_i != 0);
   maxOrder := if bars then (max Gcx+1-min Gcx)//2+1 else 1;
   tensorOrder := max(1,min(maxOrder,o.TensorOrder,(limit+1-min Gcx)//2+1));
   if tensorOrder >= 2 and B.cache#?"AInfinityResolutionThrough"
      and limit+1-min Gcx > B.cache#"AInfinityResolutionThrough" then
      error "the supplied algebra resolution ends before the required module inputs";
   -- An n-input module operation can use at most n-1 algebra inputs.
   -- Its inner algebra operation can land at most in degree limit-min Gcx.
   if B.cache#?"AInfinityCheckOrder" and tensorOrder-1 > B.cache#"AInfinityCheckOrder" then
      error "the supplied algebra has insufficient TensorOrder for this module model";
   if tensorOrder >= 3 and B.cache#?"AInfinityCheckLimit"
      and limit-min Gcx > B.cache#"AInfinityCheckLimit"+1 then
      error "the supplied algebra has insufficient LengthLimit for this module model";
   for i from min Gcx+1 to max Gcx do mG#{i} = Gcx.dd_i;

   --mG#{u_1..u_n}
   for n from 2 to tensorOrder do (
      nextMult := getNextModMult(B,Gcx,n,mG,LengthLimit => limit,Check => o.Check);
      -- merge in the new multiplications
      apply(pairs nextMult, p -> mG#(p#0) = p#1);
   );

   G := new SAInfMod from hashTable {
      "natural" => gradedModule Gcx,
      "algebra" => B,
      "mm" => hashTable pairs mG,
      cache => new CacheTable };

   G.cache#"AInfinityLimit" = limit;
   G.cache#"AInfinityOrder" = tensorOrder;

   if o.TensorOrder < maxOrder then G.cache#"AInfinityCheckOrder" = o.TensorOrder;
   if limit < max Gcx and maxOrder >= 2 then G.cache#"AInfinityCheckLimit" = limit-1;
   return G;
)

sAInfModModel (SAInfAlg,Module,Complex) := SAInfMod => o -> (B,M,Gcx) -> (
   checkModelOptions o;
   checkModelRings(B,M);
   checkModelComplex Gcx;
   Q := B#"ring";
   if Q =!= ring Gcx then error "expected the module complex over the algebra's ambient ring";
   point := if B.cache#?"AInfinityModelPoint" then B.cache#"AInfinityModelPoint"
            else if not isHomogeneous Gcx and Q.?maxIdeal then Q.maxIdeal else null;
   partial := checkSuppliedResolution(Gcx,o,point,max Gcx);
   if o.Check then (
      -- No augmentation map is supplied. Equality of the two modules proves
      -- the standard presentation case; otherwise detect definite mismatches
      -- via their annihilators and (local or graded) generator counts. The
      -- caller still identifies H_min(Gcx) with M for an arbitrary basis.
      H := HH_(min Gcx) Gcx;
      N := restrictModelModule(Q,M);
      if H != N then (
         a := ann H;
         b := ann N;
         sameAnn := if point === null then a == b else
                    not isSubset(a:b,point) and not isSubset(b:a,point);
         if not sameAnn then error "the supplied resolution has the wrong zeroth homology annihilator";
         if isField coefficientRing Q then (
            k := Q/(if point === null then ideal vars Q else point);
            if numgens prune(k**H) != numgens prune(k**N) then
               error "the supplied resolution has the wrong fiber dimension";
         );
      )
   );
   G := moduleModelOnResolution(B,Gcx,o);
   if partial then (
      G.cache#"AInfinityResolutionThrough" = max G#"natural";
      G.cache#"AInfinityCheckLimit" = o.LengthLimit-1;
   );
   if point =!= null then G.cache#"AInfinityModelPoint" = point;
   G
)

sAInfModModel (SAInfAlg,Module) := SAInfMod => o -> (B,M) -> (
   checkModelOptions o;
   R := ring M;
   Q := B#"ring";
   checkModelRings(B,M);
   homogeneous := isHomogeneous M and (R === Q or isHomogeneous presentation(Q,R));
   if B.cache#?"AInfinityModelPoint" and B.cache#"AInfinityModelPoint" != Q.maxIdeal then
      error "the supplied algebra was computed at a different maximal ideal";
   (Gcx,partial) := ambientModelResolution(M,Q,homogeneous,o.LengthLimit,()->restrictModelModule(Q,M));
   G := moduleModelOnResolution(B,Gcx,o);
   if not homogeneous then G.cache#"AInfinityModelPoint" = Q.maxIdeal;
   if partial then (
      G.cache#"AInfinityResolutionThrough" = max G#"natural";
      G.cache#"AInfinityCheckLimit" = o.LengthLimit-1;
   );
   G
)

sAInfModModel Module := SAInfMod => o -> M ->
   sAInfModModel(sAInfAlgModel(ring M,o),M,o)
