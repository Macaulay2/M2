-* 
this seems to be a worksheet with experiments 
for the paper with Paco Castro 
*-

restart
DerLog = f -> image syz transpose (jacobian ideal f || matrix {{f}})
--DL = DerLog f
isFree = f -> (
     DL := DerLog f;
     n := numgens ring f;  
     if isHomogeneous DL then -- no need for fitting ideals
     rank source mingens DL == n
     else all(n, i->fittingIdeal(i,DL)==0) and fittingIdeal(n,DL)==1
     )
isSuffFree = f -> (
     --??? what is sufficiently free?
     DL := DerLog f;
     n := numgens ring f;  
     any((minors(n,mingens DL))_*, m-> m%f == 0 and (print (m//f); first degree (m//f)) == 0)
     )
isFreeAtOrigin = f -> (
     DL := DerLog f;
     n := numgens ring f;  
     any((minors(n,mingens DL))_*, m -> m%f == 0 and (m//f)%ideal gens ring f != 0)
     )
debug needsPackage "Dmodules"

-- Paco's kappa 
minOrderForAnnF1 = method(Options=>{OrderLimit=>infinity})
minOrderForAnnF1 RingElement := ZZ => o -> f -> (
     R := ring f;
     assert isPolynomialRing R;
     As := AnnFs sub(f,makeWA R); -- Annihilator way
     A := sub(As, {last gens ring As => -1});
     k := 0;
     while k<=o.OrderLimit do(
     	  if sub(kOrderAnnFa(k,f,-1), ring A) == A then return k;
	  k = k+1;	  
	  );
     return infinity -- if OrderLimit is reached    
     )

-- Yano's L(f)
yanoL = method(Options=>{Strategy=>Truncate})
yanoL RingElement := ZZ => o -> f -> (
     -- returns total order "ord" of the operator "a" if the monomial s^ord is present
     integralOrderOfS := a -> ( 
	  ord := max apply(listForm a, t->last t#0); -- order in "s" 
	  -- assume listForm does NOT screw up the order
	  for t in listForm a do (
	       m := t#0; 
	       if all(drop(m,-1),x->x==0) 
	       then return if last m == ord then ord else infinity;
	       );
	  infinity
	  );
     if o.Strategy === ViaAnnFs then (
      	  A := AnnFs f;
      	  Ds := newRing(ring A, Weights=>toList((numgens ring A - 1):0)|{1}); -- assumes "s" is last
          -- AR := ring A;
          --      createDpairs AR;
          --      Ds := (coefficientRing AR)[AR.dpairVars#2, AR.dpairVars#0, AR.dpairVars#1, 
 	  -- 	  WeylAlgebra=>apply(#AR.dpairVars#0, i->AR.dpairVars#0#i=>AR.dpairVars#1#i), MonomialOrder => Eliminate 1];
      	  g := flatten entries gens gb sub(A,Ds) / (a->leadTerm(1,a));
      	  min apply(g, integralOrderOfS)
      	  ) else if o.Strategy === Truncate then (
      	  k := 1;
      	  while true do (
	       A = kOrderAnnFs(k,f);
     	       Ds = newRing(ring A, Weights=>toList((numgens ring A - 1):0)|{1}); -- assumes "s" is last
	       g = flatten entries gens gb sub(A,Ds) / (a->leadTerm(1,a));
	       if any(g,a->integralOrderOfS a < infinity) then break else k = k+1;
	       );
      	  k
      	  ) else error "unknown Strategy" 
     )

-- annihilator of 1/f where f is a planar curve (with the singularity at 0)


tests = {
     (
     	  R = QQ[x_1,x_2]; f = x_1*x_2*(x_1+x_2)
     	  ),
     (
	  n = 3; R = QQ[x_1..x_n]; f = sum(n, i->x_(i+1))*(x_1 + x_2)*(x_1 + x_3)*(x_2 + x_3)*product(n, i->x_(i+1))
	  ),
     (
	  f = reiffen(4,5)
	  ),
     (
	  n = 2; R = QQ[x_{1,1}..x_{n,n}]; M = map(R^n,R^n,(i,j)->x_{i+1,j+1}); f = (det M)^2; assert not isFree f; f 
	  ),
     (
	  n = 5; R = QQ[x_1..x_n]; 
	  f = sum(n, i->x_(i+1))*product(n, i->x_(i+1)); --assert (not isSuffFree f and not isFreeAtOrigin f); 
	  f
	  )
     } 

end
TEST ///
debug needsPackage "Dmodules"
R = QQ[x_1,x_2]; f = x_1*x_2*(x_1+x_2)
assert(
     diffRatFun({1,0},f)
     ==2*x_1*x_2+x_2^2)
assert(
     diffRatFun({2,1},1/f) 
     == (-6*x_1^3-24*x_1^2*x_2-16*x_1*x_2^2-4*x_2^3)/(x_1^7*x_2^2+4*x_1^6*x_2^3+6*x_1^5*x_2^4+4*x_1^4*x_2^5+x_1^3*x_2^6)
     )
///

restart
load "h-arr.m2"
-- Reiffen curve: Ann^{(2)}=Ann
f = tests#2
minOrderForAnnF1 f

-- old experiments	   
degree (ideal jacobian ideal f + ideal f) -- Tjurina #
D = ring first A;
A = apply(A,A->sub(A,D));
Dd = newRing(D, Weights=>{0,0,1,1})
gbA = apply(A, A->flatten entries sub(gens gb A,Dd));
VerticalList apply(gbA, a->toString(a/leadMonomial))
R = (coefficientRing Dd)[take(gens Dd, numgens Dd//2)] 
m = ideal vars R;
fD = D^1/A#0
annA = VerticalList apply(ord-1, i->gens gb annihilator(A#(i+1)*(D^1/A#i)))
quotient(A#0,A#2)

-- annihilator does not work correctly!!!
gens gb annihilator(
     A#2*(D^1/A#0)
     )
gens gb annihilator(
     (ideal gens gb A#2)*(D^1/A#0)
     )
flatten entries sub(gens gb sub(A#2,Dd),Dd)/leadMonomial

