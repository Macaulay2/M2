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
     	  if sub(kOrderAnnFa(k,f,-1), ring A) == A then return k;
	  k = k+1;	  
	  );
     return infinity -- if OrderLimit is reached    
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
scan(4..10, p-><<"kappa(reiffen("<<p<<","<<p+1<<")) = "<<minOrderForAnnF1 reiffen(p,p+1)<<endl)

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
f = reiffen(5,6)
f = reiffen(6,7)
f = reiffen(7,9)
f = reiffen(8,9) -- p-3 conjecture is not true
f = reiffen(8,10)
ord = 5 -- highest order
A = apply(ord,i->kOrderAnnFa(i+1,f,a));
degA = A/degree@@charIdeal
kappa = position(1..ord-1,i->degA#i==degA#(i-1))+1 

-- check
As = AnnFs sub(f,makeWA ring f); -- Annihilator way
A1 = sub(As, {last gens ring As => -1});
assert(sub(A1,ring A#(kappa-1))==A#(kappa-1))
	   
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
gens gb annihilator(
     A#2*(D^1/A#0)
     )
gens gb annihilator(
     (ideal gens gb A#2)*(D^1/A#0)
     )
flatten entries sub(gens gb sub(A#2,Dd),Dd)/leadMonomial

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
