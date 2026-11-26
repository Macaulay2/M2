--
-- Homogeneous predicates refer to the graded maximal ideal. Local models
-- of an inhomogeneous quotient use the point selected in its ambient ring.
golodPoint = R -> (
   S := ring presentation R;
   if not isField coefficientRing S then error "Golod tests require a polynomial presentation over a field";
   local point;
   if isHomogeneous R then point = ideal vars S
   else (
      if not S.?maxIdeal then error "select a point in the ambient ring with setMaxIdeal";
      point = S.maxIdeal);
   if not isSubset(ideal presentation R, point) then error "the selected point is outside the quotient";
   point
)

hasMinimalMultAtPoint = method()
hasMinimalMultAtPoint Ideal := I -> hasMinimalMultAtPoint(quotient I, 2)
hasMinimalMultAtPoint (Ideal, InfiniteNumber) :=
hasMinimalMultAtPoint (Ideal, ZZ) := (I,n) -> hasMinimalMultAtPoint(quotient I, n)
hasMinimalMultAtPoint Ring := R -> hasMinimalMultAtPoint(R,2)
hasMinimalMultAtPoint (Ring,InfiniteNumber) :=
hasMinimalMultAtPoint (Ring,ZZ) := (R,n) -> (
   if n =!= infinity and (class n =!= ZZ or n < 1) then error "expected a positive arity or infinity";
   point := golodPoint R;
   S := ring point;
   if ideal presentation R == 0 then return true;
   red := if isHomogeneous R then map(coefficientRing S, S, toList(numgens S:0)) else S.residueMap;
   products := (sAInfAlgModel(R,TensorOrder=>n))#"mm";
   all(select(keys products, k -> #k >= 2 and #k <= n), k -> red(products#k) == 0)
)

-- In homological degree n the Burke terms are G_n together with
-- one copy of F_j tensored with the terms in degree n-j-1 for each j>0.
-- Count the summands by multidegree without building their free modules.
modelPointBettiTally = (F,G,b,graded) -> (
   S := ring F;
   emptyColumn := new BettiTally from {};
   column := V -> if graded then betti complex {V}
      else (rank V) * betti complex {S^1};
   algebra := apply(min(length F,max(0,b-1)), j -> column F_(j+1));
   terms := new MutableList from toList(b+1:emptyColumn);
   for n from 0 to b do (
      t := if n <= length G then column G_n else emptyColumn;
      for j from 1 to min(#algebra,n-1) do
         t = t ++ (algebra#(j-1) ** terms#(n-j-1));
      terms#n = t);
   result := emptyColumn;
   for n from 0 to b do result = result ++ ((terms#n)[-n]);
   new BettiTally from select(pairs result, (k,v) -> v != 0)
)

golodBettiAtPoint = method()
golodBettiAtPoint (Module,ZZ) := BettiTally => (M,b) -> (
   if b < 0 then error "expected a nonnegative homological degree";
   R := ring M;
   p := presentation R;
   S := ring p;
   m := substitute(presentation M,S);
   N := coker(m | target m ** p);
   if isHomogeneous R and isHomogeneous N then
      return modelPointBettiTally(res(coker p,LengthLimit=>b),res(prune N,LengthLimit=>b),b,true);
   if not S.?maxIdeal then error "select a point in the ambient ring with setMaxIdeal";
   if not isSubset(ideal p,S.maxIdeal) then error "the selected point is outside the quotient";
   modelPointBettiTally(localResolution(localPrune coker p,LengthLimit=>b),
      localResolution(localPrune N,LengthLimit=>b),b,false)
)

golodRanksAtPoint = method()
golodRanksAtPoint (Module,ZZ) := List => (M,b) -> (
   T := golodBettiAtPoint(M,b);
   ranks := new MutableList from toList(b+1:0);
   scan(pairs T, (k,v) -> ranks#(first k) = ranks#(first k) + v);
   toList ranks
)
