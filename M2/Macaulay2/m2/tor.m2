--		Copyright 1995 by Daniel R. Grayson

Tor = new ScriptedFunctor from {
     subscript => (
	  i -> new ScriptedFunctor from {
	       argument => (X -> (
	       	    	 (M,N) -> (
		    	      f := lookup(Tor,class i,class M,class N);
		    	      if f === null then error "no method available"
		    	      else f(i,M,N)
			      )
	       	    	 ) X
	       	    )
	       }
	  )
     }

Tor(ZZ, Module, Module) := Module => (i,M,N) -> (
     if ring M =!= ring N then error "expected the same ring";
     R := ring M;
     if not isCommutative R then error "'Tor' not implemented yet for noncommutative rings.";
     if i < 0 then R^0
     else if i === 0 then M ** N
     else (
	  C := resolution(M,LengthLimit=>i+1);
	  N = minimalPresentation N;
	  b := C.dd;
	  complete b;
	  if b#?i then (
	       if b#?(i+1) 
	       then homology(b_i ** N, b_(i+1) ** N)
	       else kernel (b_i ** N))
	  else (
	       if b#?(i+1) 
	       then error "internal error"
	       else C_i ** N)))


-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
