--		Copyright 1993-1999 by Daniel R. Grayson

needs "methods.m2"
needs "remember.m2"

binomial(ZZ,ZZ) := ZZ => memoize (
     (n,i) -> (
	  if i < 0 then 0
	  else if i === 0 then 1
     	  else if n > 0 then (
     	       if i > n then 0
     	       else n! // (i! * (n-i)!)
		    -- binomial(n-1,i) + binomial(n-1,i-1)
	       )
	  else if n === 0 then 0
     	  else (-1)^i * binomial(-n+i-1,i)
	  )
     )

binomial(RingElement, ZZ):= (q,n)->(
     product(n, i-> q-i) * (promote(product(n, i->1+i),ring q))^(-1))

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
