--		Copyright 1995 by Daniel R. Grayson

needs "gateway.m2" -- for ScriptedFunctor
needs "matrix1.m2"
needs "modules.m2"

TorOptions = new OptionTable from {
     Prune => true
     }


Tor = new ScriptedFunctor from {
     subscript => (
	  i -> new ScriptedFunctor from {
	       argument => (X -> (
	       	    	 TorOptions >> opts -> (M,N) -> (
		    	      f := lookup(Tor,class i,class M,class N);
		    	      if f === null then error "no method available"
		    	      else (f opts)(i,M,N)
			      )
	       	    	 ) X
	       	    )
	       }
	  )
     }

Tor(ZZ, Module, Module) := Module => opts -> (i,M,N) -> (
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


Tor(ZZ, Ideal, Matrix) := opts -> (i,J,f) -> Tor_i(module J,f,opts)
Tor(ZZ, Matrix, Ring) := opts -> (i,f,R) -> Tor_i(f,R^1,opts)
Tor(ZZ, Matrix, Ideal) := opts -> (i,f,J) -> Tor_i(f,module J,opts)
Tor(ZZ, Module, Ring) := opts -> (i,M,R) -> Tor_i(M,R^1,opts)
Tor(ZZ, Module, Ideal) := opts -> (i,M,J) -> Tor_i(M,module J,opts)
Tor(ZZ, Ideal, Ring) := opts -> (i,I,R) -> Tor_i(module I,R^1,opts)
Tor(ZZ, Ideal, Ideal) := opts -> (i,I,J) -> Tor_i(module I,module J,opts)
Tor(ZZ, Ideal, Module) := opts -> (i,I,N) -> Tor_i(module I,N,opts)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
