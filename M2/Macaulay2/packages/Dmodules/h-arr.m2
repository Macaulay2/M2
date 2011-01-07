restart
DerLog = f -> image syz transpose (jacobian ideal f || matrix {{f}})
--DL = DerLog f
isFree = f -> (
     DL := DerLog f;
     n := numgens ring f;  
     all(n, i->fittingIdeal(i,DL)==0) and fittingIdeal(n,DL)==1
     -- no need for fitting ideals, just check if there #mingens==n   
     )
isSuffFree = f -> (
     --??? what is sufficiently free?
     DL := DerLog f;
     n := numgens ring f;  
     #select(1, (minors(n,mingens DL))_*, m -> m != 0 and m%f == 0 and first degree m//f == 0) > 0
     )
isFreeAtOrigin = f -> (
     DL := DerLog f;
     n := numgens ring f;  
     #select(1, (minors(n,mingens DL))_*, m -> m != 0 and m%f == 0 and (m//f)%ideal gens ring f != 0) > 0
     )
needsPackage "Dmodules"
Ann1 = f -> (
     R := ring f;
     n := numgens R;
     D := makeWA R;
     f = sub(f,D);
     DL := DerLog f;
     prune ideal apply(entries transpose gens DL, a->(
	       delta := sum(n, i->a#i*D_(n+i));
	       delta + (delta*f - f*delta)//f -- delta + delta(f)/f 
	       ))
     )
end
restart
load "h-arr.m2"
R = QQ[x,y]; f = x*y*(x+y)
n = 3; R = QQ[x_1..x_n]; f = sum(n, i->x_(i+1))*(x_1 + x_2)*(x_1 + x_3)*(x_2 + x_3)*product(n, i->x_(i+1))
n = 2; R = QQ[x_1..x_n]; f=x_1^4+x_2^5+x_1*x_2^4;
n = 2; R = QQ[x_{1,1}..x_{n,n}]; M = map(R^n,R^n,(i,j)->x_{i+1,j+1}); assert not isFree f;  f = (det M)^2
n = 5; R = QQ[x_1..x_n]; f = sum(n, i->x_(i+1))*product(n, i->x_(i+1)); assert (not isSuffFree f and not isFreeAtOrigin f) 

isFree f
A1 = Ann1 f -- Paco's way
-- A = RatAnn sub(f,makeWA R) -- RatAnn calls b-function
As = AnnFs sub(f,makeWA R); -- Annihilator way
A = sub(As, {last gens ring As => -1})
sub(A1,ring A) == A



