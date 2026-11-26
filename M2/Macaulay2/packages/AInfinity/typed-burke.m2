-- Burke resolution

burkeResolutionAtPoint = method( TypicalValue => Complex,
                          Options => {Check => false} )

checkBurkeExactness = (F,point) -> (
   if F.dd^2 != 0 then error "Burke differentials do not compose to zero";
   for i from min F+1 to max F-1 do (
       H := HH_i F;
       if H != 0 and (point === null or isSubset(ann H,point)) then
           error("nonzero Burke homology in degree " | toString i |
               if point === null then "" else " at the selected maximal ideal"));
   true)

burkeResolutionAtPoint (Module,ZZ) := Complex => o -> (M,len) -> (
   if len < 0 then error "expected a nonnegative resolution length";
   if not instance(o.Check,Boolean) then error "Check must be a Boolean";
   R := ring M;
   B := sAInfAlgModel R;
   G := sAInfModModel(B,M,LengthLimit => len);
   F := if len == 0 then complex(burkeModule(R,B,G,0),Base => 0)
       else complex apply(len,i -> burkeDifferential(R,B,G,i+1));
   point := if G#?cache and G.cache#?"AInfinityModelPoint" then G.cache#"AInfinityModelPoint"
           else if B#?cache and B.cache#?"AInfinityModelPoint" then B.cache#"AInfinityModelPoint" else null;
   if point =!= null then (point = substitute(point,R); F.cache#"AInfinityModelPoint" = point);
   if o.Check then checkBurkeExactness(F,point);
   F
)

-- Modules of the Burke resolution

burkeModule = method( Options => {} )

burkeModule (Ring,Complex,Complex,ZZ) := Module -> (R,Bnat,Gnat,d) -> (
   if ring Bnat =!= ring Gnat then error "expected complexes over the same ring";
   if d < min Gnat then return R^0;
   bars := select(toList(min Bnat..max Bnat),i -> Bnat_i != 0);
   if #bars == 0 then return if Gnat_d == 0 then R^0 else label({d},tensor(R,Gnat_d));
   if min bars < 2 then error "expected algebra bars in homological degrees at least two";
   lo := max(0,ceiling((d-max Gnat)/(max bars)));
   hi := floor((d-min Gnat)/(min bars));
   terms := flatten for k from lo to hi list (
       U := (labeledTensor(R,toList(k:Bnat) | {Gnat},d,d))_d;
       select(labelsAndComponents U,(w,N) -> N != 0));
   if #terms == 0 then R^0 else directSum apply(terms,(w,N) -> w => N)
)

burkeModule (Ring,SAInfAlg,SAInfMod,ZZ) := Module -> (R,B,G,d) -> (
   if B =!= G#"algebra" and B != G#"algebra" then error "module has a different A-infinity algebra";
   -- A length-limited resolution can omit entire modules as well as maps.
   -- Their absence from the displayed complex does not establish vanishing.
   if (G#?cache and G.cache#?"AInfinityResolutionThrough" and d > G.cache#"AInfinityResolutionThrough")
       or (B#?cache and B.cache#?"AInfinityResolutionThrough" and
           d-min G#"natural" > B.cache#"AInfinityResolutionThrough")
   then error "the A-infinity model is too short for the requested Burke term";
   burkeModule(R,B#"natural",G#"natural",d)
)

-- Rank of the modules of the Burke resolution

rankBurkeModule = method()

rankBurkeModule (Complex,Complex,ZZ) := ZZ => (Bnat,Gnat,d) -> (
   -- The bar words are either a bare module term or an algebra term followed by a shorter word.
   bars := select(toList(min Bnat..max Bnat),j -> Bnat_j != 0);
   if any(bars,j -> j < 2) then error "expected algebra bars in homological degrees at least two";
   a := hashTable apply(bars,j -> j => rank Bnat_j);
   ranks := new MutableHashTable;
   for i from min Gnat to d do
       ranks#i = rank Gnat_i + sum(select(bars,j -> i-j >= min Gnat),j -> a#j * ranks#(i-j));
   if d < min Gnat then 0 else ranks#d
)

-- Differentials of the Burke resolution

-- Share the public method table with the hash-table form in AInfinity.m2.

burkeDifferential (Ring,SAInfAlg,SAInfMod,ZZ) := Matrix => (R,B,G,t) -> (
   Bnat := B#"natural";
   Gnat := G#"natural";
   Q := B#"ring";
   src := burkeModule(R,B,G,t);
   tar := burkeModule(R,B,G,t-1);
   d := map(tar,src,0);
   if src == 0 or tar == 0 then return d;
   for u in indices src do (
       lastIndex := #u-1;
       for v in select(sModIndicesStasheff u,w -> member(w_2,indices tar)) do (
           p := v_0;
           q := v_1;
           key := u_{p..q};
           X := if q == lastIndex then G else B;
           ops := X#"mm";
           -- Constructors may explicitly populate a zero outside the known
           -- range. Check the bounds before interpreting even a stored block.
           if #key > 1 and X#?cache and (
               (X.cache#?"AInfinityCheckOrder" and #key > X.cache#"AInfinityCheckOrder")
               or (X.cache#?"AInfinityCheckLimit" and sum key-2 > X.cache#"AInfinityCheckLimit"))
           then error "the A-infinity model is too short for this Burke differential";
           if ops#?key then (
               left := tensor(Q,apply(take(u,p),i -> Bnat_i));
               block := left ** ops#key;
               if q < lastIndex then (
                   right := tensor(Q,apply(u_{q+1..lastIndex-1},i -> Bnat_i));
                   block = block ** right ** Gnat_(last u));
               if R =!= Q then block = tensor(R,block);
               d = d + signShiftedStasheff(p,q,u) * (tar)_[v_2] * block * (src)^[u];
               );
           ));
   d
)
