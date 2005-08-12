signOfPermu := P -> (
     sign := 1;
     scan(#P-1, a->
     	  scan(a+1..#P-1, b ->
	       if P#a > P#b then
	       sign = -1 * sign
	       else if P#a == P#b then
	       sign = 0       
	       ));
     sign
     )
     
Grass = method(TypicalValue => Ideal);
Grass(ZZ,ZZ):= Ideal => (k,n) -> Schubert(k,n,toList(n-k..n))

Schubert = method(TypicalValue => Ideal);
Schubert(ZZ, ZZ, List) := Ideal => (kk,nn,sigma) -> (
     k := kk+1;
     n := nn+1;
     L := toSequence \ subsets(n,k);
     p := global p;
     vrs := apply(L, i -> new IndexedVariable from {p,i});
     M := monoid [vrs];
     R := ZZ M;
     vr := s -> (new IndexedVariable from {p,toSequence s})_R;
     T := flatten table(L, L, (i,j) -> {i,j});
     nonStandard := for t in T list (
	  if t#0 < t#1 then (
	       i := position(t#0,t#1,(a,b) -> a>b);
	       if i =!= null then {t#0, t#1, i} else continue)
	  else continue);
     shuffles := apply(nonStandard, t-> (
     	       snake := t#1_{0..t#2}|t#0_{t#2..k-1};
     	       apply(subsets(snake,t#2+1), s1-> (
	  		 s2 := sort toList((set snake) - set s1);
	  		 sgn := signOfPermu (s1 | s2);
	  		 {t#0_{0..t#2-1} | s2, s1 | t#1_{t#2+1..k-1}, sgn} ) ) ) );
     reorder := apply(shuffles, T ->
     	  select( 
	       apply(T, t -> {sort t#0, sort t#1, signOfPermu t#0 * signOfPermu t#1 * t#2} ), 
	       t -> t#2 != 0 ) );
     G := apply(reorder, T ->(
     	       quadric := 0; 
     	       apply(T, t-> (
	       	    	 temp := apply(t_{0,1}, vr) | t_{2};
	       	    	 quadric = quadric + temp#0 * temp#1 * temp#2;
	       	    	 )
	       	    );
     	       quadric ) );
     higher := apply(
	  select(
	       subsets(n,k),
	       s -> (
	       	    j := position(s, sigma, (a,b) -> a>b);
	       	    j =!= null ) ),
     	  vr);
     g := forceGB matrix{(G|higher)};
     g = generators g;
     forceGB g;
     ideal g
     )

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
