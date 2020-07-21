-- Frank Schreyer

artinianGorensteinRing = (p,n,d,s) -> (
     -- p = characteristic
     -- n+1 = number of variables
     -- d = degree of the socle
     -- s = number of powers presenting the dual socle
     R := ZZ/p[vars(0 .. n)];
     if s <= n then error "expected s to be more than the number of variables";
     D := sum(n+1,i->R_i^d) + (
	  if s > n+1
	  then (sum(n+1,i->R_i))^d + sum(s-n-2,i->(random(1,R))^d)
	  else 0
     	  );				  -- a diff'l operator
     -- switch point of view and find which polynomials of each degree e,
     -- when regarded as diff'l operators, annihilate D
     e := 2;
     I := map(R^1,R^0,0);
     v := symmetricPower(d,vars R);
     while (
	  c := codim cokernel I;
	  c < n+1 
	  -- or c === n+1 and numgens source mingens image (v % I) > 1
	  ) do (
	  w := symmetricPower(e,vars R);
	  I = mingens image (I | w * (syz(diff(w,matrix {{D}})
			 -- ,{d-e} -- ??
			 )));
	  e = e+1;
	  );
     I)


     	  
p = 101

-- put an hour time limit later on each one

--scan(3 .. 20, n -> 
--     scan(2 .. 10, d -> 
--	  try scan(n+1 .. n + (binomial(n+d,n)) // n, 
--	       s -> (
--		    << "n = " << n << " d = " << d << " s = " << s << "\n";
--		    I := artinianGorensteinRing(p,n,d,s);
--		    gbTrace = 2;
--		    time C := resolution I;
--		    gbTrace = 0;
--		    betti C;
--		    )
--	       )
--	  else << "terminating inner loop\n"
--	  )
--     )
--
-- Frank is interested in the set of (d,n) such that computations of resolution I,
-- for the last s, can be done in less than time T, for various T, 1min, 1hr, 1day.
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test schreyer.out"
-- End:
