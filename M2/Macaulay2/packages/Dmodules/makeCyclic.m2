-- Copyright 1999-2002 by Anton Leykin and Harrison Tsai

--function
local ItimesF

--keys
local  howBig
howBig = symbol howBig
local  annV
annV = symbol annV
local degGen
degGen = symbol degGen

-------------------------------------------------
-- cancels out the n-th (differential) variable 
-------------------------------------------------
cancelOutDX := (h, m, n) -> (
     W := ring h;
     X := W_m;
     l := listForm h;
     s := max(l/(u->u#0#n)); -- the max degree of W_n in h
     --newH := sum(select(l, u -> u#0#n == s), u->(
	--       i := u#0;
	  --     i := take(i, n)|{0}|drop(i, n+1);
	    --   u#1*W_i
	      -- )); 
     apply(s+1, i->
	  ((-1)^i*binomial(s,i)*X^i, X^(s-i))
	  )     
     )
---------------------------------------------------
-- cancels out the m-th (non-differential) variable 
---------------------------------------------------
cancelOutX := (h, m, n) ->(
     W := ring h;
     DX := W_n;
     l := listForm h;
     s := max(l/(u->u#0#m)); -- the max degree of W_m in h
     --newH := sum(select(l, u -> u#0#n == s), u->(
	--       i := u#0;
	  --     i := take(i, n)|{0}|drop(i, n+1);
	    --   u#1*W_i
	      -- )); 
     apply(s+1, i->
	  ((-1)^(s-i)*binomial(s,i)*DX^i, DX^(s-i))
	  )     
     )
---------------------------------------------------
-- given presentations of  g \in DfD and h \in DgD 
-- computes the presentation of h \in DfD
---------------------------------------------------
composePres := (g, h) ->(
     flatten apply(g, u->( apply(h, v->(
	       		 (v#0 * u#0, u#1 * v#1)
	       		 ))))
     ) 
---------------------------------------------------
-- evaluates the presentation
---------------------------------------------------
valuePres := (p, h) -> (
     sum( p, u -> u#0 * h * u#1 )
     )

--------------------------------------------------------------------------
-- obtains the presentation of 1 in the two-sided DhD, where D = A_n(k) 
--------------------------------------------------------------------------
getOne := h -> (
     -- prep work
     W := ring h;
     createDpairs W;

     dpV := W.dpairVars;
     dpI := W.dpairInds;
     
     -- sanity check
     if (#(dpI#2) != 0) then
     error "expected no central variables in Weyl algebra";
     if not isField coefficientRing W then 
     error "expected field as the coefficient ring";
     
     t := h;
     pres := {(1_W,1_W)};
     scan(#(dpV#0), i->(
	       pres1 := cancelOutX(t, dpI#0#i, dpI#1#i);
	       t = valuePres(pres1, t);
	       pres2 := cancelOutDX(t, dpI#0#i, dpI#1#i);
	       t = valuePres(pres2, t);
	       pres = composePres(composePres(pres, pres1),pres2);
	       ));            
     l := leadCoefficient t;
     apply(pres, u->((1/l)*u#0, u#1))
     )

---------------------------------------------------------------------------
-- fed with <presentation>, returns the list of nonzero (mod I) right parts 
---------------------------------------------------------------------------
getUseful := (pres, I) -> (
     select( apply(pres, u->u#1%I), u->u!=0)
     )

 

---------------------------------------------------------------------------
-- computes ":" ideal
---------------------------------------------------------------------------
semicolonIdealIdeal := (I,J) -> (
     error "is not implemented yet"
     )
semicolonMatrixMatrix := (M, v) -> (
     R := ring M; 
     if R =!= ring v or rank target M != rank target v 
     then error "expected equal targets";
     ideal mingens ideal ker map(cokernel M , source v, v) 
     )
semicolonIdealRE := (I,f) -> (
     R := ring I;
     semicolonMatrixMatrix(gens I, matrix{{f}})
     )

------------------------------------------------------------------
-- multiplies an Ideal by a RingElement
-----------------------------------------------------------------
ItimesF = method()
ItimesF(Ideal, RingElement) := (I,f) -> (
     g := first entries gens I;
     g = g/(u->u*f);
     ideal g
     )
----------------------------------------------------------------------------
-- makes a cyclic module out of a module presented as a cokernel of a matrix 
----------------------------------------------------------------------------

tempV := local tempV

makeCyclic = method()
--makeCyclic Module := M -> makeCyclic relations M
makeCyclic Matrix := HashTable => M -> (
     F := target M;
     K := image M;
     n := numgens F;
     R := ring F;
     
     E := apply(toList(0..(n-1)), i ->
	  new HashTable from {
	       tempV => (t := matrix( (toList(i : {0_R})) 
			 | {{1_R}} | (toList((n-i-1):{0_R})) )), --(0,...,1,...,0)
	       annV => (ta := semicolonMatrixMatrix(M, t)),
	       howBig => numgens ta,
	       degGen => min apply(first entries gens ta, u->first degree u)
	       }
	  );
     
     pInfo(666, {"E=",E});
     E = select(E, e->e#annV != R);
     pInfo(666, {"E=",E});
          
     g := matrix( toList(n:{0_R}) ); --start with(0,0,...,0)
     Ag := ideal 1_R; -- annihilator of g 
     while #E > 0 do (
	  -- pick the "nicest" E_i
	  minDeg := min apply(E,u->u#degGen);
	  i := position(E, u->u#degGen == minDeg);
	  h := (E#i)#tempV;
	  Ah := (E#i)#annV;
	  E = drop(E, {i,i});
	  flg := true;
	  while flg
	  do ( -- while h is not in Dg
	       pInfo(666, {"g = ", g, " h = ", h, " h%g = ", h%g}); 
	       -- pick an element in Ah of the lowest degree
	       a := first select(1, first entries mingens Ah, 
		    u->first degree u == minDeg);
	       pInfo(3, {"picked ", a, " in Ah"}); 
	       unitPres := getOne a;
	       rr := apply(unitPres, u -> u#1);
	       r := select(1, rr, u -> Ag + ideal (a*u) == R);
	       if #r!=0
	       then flg = false 
	       else r = select(1, rr, u -> not isSubset(ideal (a*u), Ag));
	       r = first r;
	       -- D(h+rg) > Dg
	       g = h + r*g; 
	       Ag = semicolonMatrixMatrix(M, g); --ann(g)
	       );
	  );
     new HashTable from {Generator=>g, AnnG=>ideal mingens Ag}
     ) 


TEST ///
Dtrace 1
pInfo(1, "testing makeCyclic...")

-------------------------
-- R^3
-------------------------

x = symbol x; dx = symbol dx; 
R = QQ[x, dx, WeylAlgebra => {x=>dx}]
M = matrix {{dx, 0, 0}, {0, dx, 0}, {0, 0, dx}}
h = makeCyclic M
assert (entries h.Generator == entries matrix{{x^2},{x},{1}});
assert (listForm bFunction(h.AnnG,{1}) == listForm bFunction(cokernel M, {1}, {0,1,2}))
assert (holonomicRank h.AnnG == holonomicRank cokernel M)
assert (singLocus h.AnnG == singLocus cokernel M)


-------------------------
-- R^1 / dx^3  
-------------------------
use R
M = presentation image map( R^1/ideal dx^3, R^3, matrix{{1, x, x^2}} )
h = makeCyclic M 
b1 = bFunction(ideal dx^3, {1})
b2 = bFunction(cokernel M, {1}, {0,-1,-2})
assert (listForm b1 == listForm b2)
b2 = bFunction(cokernel M, {1}, {2,1,0})
b3 = bFunction(h.AnnG, {1})
assert (listForm b3 == listForm b2)
///

