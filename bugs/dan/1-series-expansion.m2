-- use this as the foundation for a general series expansion package:


recip1 = (n,wts,f) -> (
     -- n is a positive integer
     -- wts is a weight vector
     -- f is a polynomial of the form 1 plus terms of positive weight, which we verify
     -- we compute the terms of the expansion of 1/f of weight less than n
     if n <= 0 then error "expected a positive integer";
     if part(,0,wts,f) != 1 then error "expected a polynomial of the form 1 plus terms of positive weight";
     g := 1_(ring f);  -- g always has the form 1 plus terms weight 1,2,...,m-1
     m := 1;			   -- 1-f*g always has terms of wt m and higher
     tr := h -> part(,m-1,wts,h);
     while m < n do (
	  m = 2*m;
	  g = g + tr(g * (1 - tr(g * tr f)));
	  );
     if m === n then g else part(,n-1,wts,g))

recip2 = (n,wts,f) -> (
     -- n is a positive integer
     -- wts is a weight vector
     -- f is a polynomial of the form u plus terms of positive weight, where u is a constant unit, which we verify
     -- we compute the terms of the expansion of 1/f of weight less than n
     u := part(,0,wts,f);
     err := () -> error "expected a polynomial of the form u plus terms of positive weight, where u is a constant unit";
     if not isConstant u then err();
     u = lift(u,coefficientRing ring f);
     if not isUnit u then err();
     v := u^-1;
     v * recip1(n,wts,v * f))

recip = (n,wts,f) -> (
     -- n is a positive integer
     -- wts is a weight vector
     -- f is a polynomial of the form u plus terms of larger weight, where u is a unit times an invertible monomial, which we verify
     -- we compute the terms of the expansion of 1/f of weight less than n plus the weight of u
     (lo,hi) := weightRange(wts,f);
     u := part(,lo,wts,f);
     e := first exponents u;
     R := ring f;
     err := () -> error "expected a polynomial of the form u plus terms of larger weight, where u is a unit times an invertible monomial";
     if not all(e,i->i==0) and not (options R).Inverses then err();
     m := R_-e;
     k := coefficientRing R;
     u = lift(u*m,k);
     if not isUnit u then err();
     v := u^-1;
     v * m * recip1(n,wts,v * m * f))

fract = (n,wts,g,f) -> (
     -- n is a positive integer
     -- wts is a weight vector
     -- g is a polynomial
     -- f is a polynomial of the form u plus terms of larger weight, where u is a unit times an invertible monomial, which we verify
     -- we compute the terms of the expansion of g/f of weight less than n
     (lo,hi) := weightRange(wts,g);
     part(,n-1,wts,g * recip(n-lo,wts,f)))

end
load "series-expansion.m2"
R = degreesRing 1 ** QQ;
assert ( recip(10,{1},1-T) == 1+T+T^2+T^3+T^4+T^5+T^6+T^7+T^8+T^9 )
h = recip(10,{1},2-T)
assert( h == 1/2+(1/4)*T+(1/8)*T^2+(1/16)*T^3+(1/32)*T^4+(1/64)*T^5+(1/128)*T^6+(1/256)*T^7+(1/512)*T^8+(1/1024)*T^9 )
h*(2-T)
h = recip(10,{1},2-3*T+T^3)
assert( h == 1/2+(3/4)*T+(9/8)*T^2+(23/16)*T^3+(57/32)*T^4+(135/64)*T^5+(313/128)*T^6+(711/256)*T^7+(1593/512)*T^8+(3527/1024)*T^9 )
h*(2-3*T+T^3)
h = recip(10,{1},2*T-3*T^2+T^4)
assert( h == (1/2)*T^(-1)+3/4+(9/8)*T+(23/16)*T^2+(57/32)*T^3+(135/64)*T^4+(313/128)*T^5+(711/256)*T^6+(1593/512)*T^7+(3527/1024)*T^8 )
h*(2*T-3*T^2+T^4)
h = recip(10,{1},2*T^-1-3+T^2)
assert( h == (1/2)*T+(3/4)*T^2+(9/8)*T^3+(23/16)*T^4+(57/32)*T^5+(135/64)*T^6+(313/128)*T^7+(711/256)*T^8+(1593/512)*T^9+(3527/1024)*T^10 )
h * (2*T^-1-3+T^2)
h = fract(10,{1},T^3-T^4+T^8,2*T^-1-3+T^2)
assert( h == (1/2)*T^4+(1/4)*T^5+(3/8)*T^6+(5/16)*T^7+(11/32)*T^8+(53/64)*T^9 )
h*(2*T^-1-3+T^2)
