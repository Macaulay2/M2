-- Bernd Sturmfels and Josephine Yu  

needs "matrix1.m2"

--signOfPermu := P -> (
--     sign := 1;
--     scan(#P-1, a->
--     	  scan(a+1..#P-1, b ->
--	       if P#a > P#b then
--	       sign = -1 * sign
--	       else if P#a == P#b then
--	       sign = 0       
--	       ));
--     sign
--     )

signAndShuffle := (a,b) -> (
     ct := 0;
     i := 0; m := #a;
     j := 0; n := #b;
     sh := while a#?i or b#?j list (
	  t := if a#?i then a#i;
	  u := if b#?j then b#j;
     	  if t === u then return (0,);
	  if t === null then (j += 1; u)
	  else if u === null or t < u then (i += 1; t)
     	  else (ct = ct + m-i; j += 1; u));
     ((-1)^ct, toSequence sh));

signOfShuffle := (a,b) -> (
     ct := 0;
     i := 0; m := #a;
     j := 0; n := #b;
     while i<m and j<n do (
     	  if a#i == b#j then return 0;
     	  if a#i < b#j then i += 1
     	  else (ct = ct + m-i; j += 1));
     (-1)^ct);

fixupw = w -> if instance(w,String) then getSymbol w else w
     
Grassmannian = method(
     TypicalValue => Ideal, 
     Options => { 
	  CoefficientRing => ZZ, 
	  Variable => monoidDefaults.VariableBaseName	    -- this is a string, so use fixupw
	  });
Schubert = method(TypicalValue => Ideal, Options => options Grassmannian);

Grassmannian(ZZ,ZZ):= o -> (k,n) -> Schubert(k,n,n-k..n,o)
Grassmannian(ZZ,ZZ,PolynomialRing) := o -> (k,n,R) -> (
     I := Grassmannian(k,n,o);
     S := ring I;
     if numgens R < numgens S then error ("expected a ring with at least ",toString numgens S," generators");
     f := map(R,S,apply(numgens S, i -> R_i));
     f I)

Schubert(ZZ, ZZ, VisibleList) := o -> (k,n,sigma) -> (
     L := toSequence \ subsets(n+1,k+1);
     R := o.CoefficientRing (monoid [apply(L, i -> new IndexedVariable from {baseName fixupw o.Variable,unsequence i})]);
     vr := new HashTable from apply(#L, i -> L#i => R_i);
     higher := apply( select( L, s -> any(s, sigma, (a,b) -> a>b)), s -> vr#s );
     G := flatten for i from 0 to #L-1 list for j from i+1 to #L-1 list (
	  r := L#i;
	  s := L#j;
	  up := position(r, s, (a,b) -> a>b);
	  if up === null then continue;
	  snake := s_{0..up} | r_{up..k};
	  sum for s1 in subsets(snake,up+1) list (
	       s2 := snake - set s1;
	       (sgn1,c) := signAndShuffle(r_{0..up-1}, s2);
	       if sgn1 == 0 then continue;
	       (sgn2,d) := signAndShuffle(s1, s_{up+1..k});
	       if sgn2 == 0 then continue;
	       sgn := sgn1 * sgn2 * signOfShuffle(s1,s2);
	       sgn * vr#c * vr#d));
     g := generators forceGB matrix(R,{(G|higher)});
     forceGB g;
     ideal g)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
