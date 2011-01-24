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
path = {".."}|path
needsPackage "Dmodules"

-- Paco's kappa 
minOrderForAnnF1 = method(Options=>{OrderLimit=>infinity})
minOrderForAnnF1 RingElement := ZZ => o -> f -> (
     R := ring f;
     assert isPolynomialRing R;
     As := AnnFs sub(f,makeWA R); -- Annihilator way
     A := sub(As, {last gens ring As => -1});
     k := 0;
     while k<=o.OrderLimit do(
     	  if sub(kOrderAnnF1(k,f), ring A) == A then return k;
	  k = k+1;	  
	  );
     return infinity -- if OrderLimit is reached    
     )

reiffen = (p,q)->(
     assert(p>=4 and q>=p+1);
     n := 2; 
     R := QQ[x_1..x_n]; 
     x_1^p+x_2^q+x_1*x_2^(q-1) 
     )
     

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

isFree f
k = 2
Ak = kOrderAnnFa(k,f,1) -- Paco's way
-- A = RatAnn sub(f,makeWA R) -- RatAnn calls b-function
As = AnnFs sub(f,makeWA R); -- Annihilator way
A = sub(As, {last gens ring As => -1})
As1 = sub(A,ring Ak);
As1 == Ak

-- Reiffen curve: Ann^{(2)}=Ann
f = tests#2
minOrderForAnnF1 f

-- Reiffen curve experiment:
a = -1
f = reiffen(4,5)
f = reiffen(7,10)
Dtrace 3
A1 = kOrderAnnFa(1,f,a)
A2 = kOrderAnnFa(2,f,a);
A3 = kOrderAnnFa(3,f,a);
A4 = kOrderAnnFa(4,f,a);
A5 = kOrderAnnFa(5,f,a);
degree charIdeal A1
degree charIdeal A2
degree charIdeal A3
degree charIdeal A4
degree charIdeal A5
degree (ideal jacobian ideal f + ideal f) -- Tjurina #
Dd = newRing(ring A1, Weights=>{0,0,1,1})
(sub(A1,Dd))_*/leadMonomial
(sub(A2,Dd))_*/leadMonomial
(sub(A3,Dd))_*/leadMonomial

flatten entries sub(gens gb sub(A3,Dd),Dd)/leadMonomial

-- build automorphisms
restart
load "h-arr.m2"
n = 2;
d = 1;
ind = apply(compositions(2*n+1,d), a->drop(a,-1))
as = flatten apply(n,i->apply(ind,j->a_(i,j)));
bs = flatten apply(n,i->apply(ind,j->b_(i,j)));
K = QQ[as|bs]
W = makeWA(K[x_1..x_n])
comm = sum(n, i1->(
	  f := sum(ind, j->sub(a_(i1,j),W)*W_j);
	  g := sum(ind, j->sub(b_(i1,j),W)*W_j);
	  sub(ideal last coefficients(f*g-g*f-1),K) + sum(i1, i2->(
	  	    f2 := sum(ind, j->sub(a_(i2,j),W)*W_j);
	  	    g2 := sum(ind, j->sub(b_(i2,j),W)*W_j);
	       	    sub(ideal last coefficients(f*g2-g2*f),K) + 
		    sub(ideal last coefficients(f*f2-f2*f),K) +
		    sub(ideal last coefficients(g*g2-g2*g),K) + 
		    sub(ideal last coefficients(g*f2-f2*g),K)
		    ))))
dim comm
transpose gens gb (comm + sub(ideal {
	       a_(0,{1,0,0,0})-b_(0,{0,1,0,0}), a_(0,{0,1,0,0})-b_(0,{1,0,0,0}),
	       --a_(0,{0,0,1,0})-b_(0,{0,0,0,1}), a_(0,{0,0,0,1})-2*b_(0,{0,0,1,0}),
	       a_(0,{1,0,0,0})-b_(0,{0,0,1,0}), a_(0,{0,1,0,0})-b_(0,{0,0,0,1}),
	       a_(1,{1,0,0,0})-b_(1,{0,1,0,0}), a_(1,{0,1,0,0})-b_(1,{1,0,0,0}),
	       --a_(1,{0,0,1,0})-b_(1,{0,0,0,1}), a_(1,{0,0,0,1})-2*b_(1,{0,0,1,0}),
	       b_(0,{0,0,0,0}), b_(1,{0,0,0,0}),	       
	       a_(0,{0,0,0,0}), a_(1,{0,0,0,0}),	       
	       a_(0,{1,0,0,0}), a_(1,{0,1,0,0}),	       
	       0
	       },K))
dim ideal oo
