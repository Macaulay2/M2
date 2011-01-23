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

tests = {
     (
     	  R = QQ[x_1,x_2]; f = x_1*x_2*(x_1+x_2)
     	  ),
     (
	  n = 3; R = QQ[x_1..x_n]; f = sum(n, i->x_(i+1))*(x_1 + x_2)*(x_1 + x_3)*(x_2 + x_3)*product(n, i->x_(i+1))
	  ),
     (
	  n = 2; R = QQ[x_1..x_n]; f=x_1^4+x_2^5+x_1*x_2^4
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
Ak = kOrderAnnF1(k,f) -- Paco's way
-- A = RatAnn sub(f,makeWA R) -- RatAnn calls b-function
As = AnnFs sub(f,makeWA R); -- Annihilator way
A = sub(As, {last gens ring As => -1})
As1 = sub(A,ring Ak);
As1 == Ak

-- Reiffen curve: Ann^{(2)}=Ann
f = tests#2
minOrderForAnnF1 f


