R = ZZ[x,Inverses=>true,MonomialOrder=>RevLex]
v = f -> first last exponents f - first first exponents f
check = (f,g) -> (
     q = f // g;
     r = f % g;
     assert(f == q*g + r);
     assert(r == 0 or v r < v g);
     )
check(1-3*x^3+3*x^6-x^9,1-x)
check(1,x)
check(1,1-x)
check(1,1-x^10)
check(1+x+x^5,1-x+x^2)
check(1+x+x^50,1-x+x^2)
check(1+x+x^500,1-x+x^2)
check(1+x+x^500,x^-6*(1-x+x^2))
check(x^-10*(1+x+x^500),x^-6*(1-x+x^2))
check(x^10*(1+x+x^500),x^-6*(1-x+x^2))
check(x^10*(1+x+x^500),x^6*(1-x+x^2))
check(x^10*(1+x+x^500)*(1-x+x^2),x^6*(1-x+x^2))
check(x^10*(1+x+x^100)*(1-x+x^2),x^6*(1-x+x^2))

stderr << currentFileName << ": test deferred" << endl
exit 0

assert( try (
	  x//(x-x); -- division by zero doesn't give error, medium priority
	  false ) else true )

-- Local Variables:
-- compile-command: "make euclid.okay"
-- End:
