--		Copyright 1995 by Daniel R. Grayson

needs "gateway.m2" -- for ScriptedFunctor
needs "matrix1.m2"
needs "modules.m2"

-- TODO: should this be fixed for all Tor methods,
-- or should they each have their own options?
TorOptions = new OptionTable from {
    MinimalGenerators => true
     }

Tor = new ScriptedFunctor from {
    subscript => i -> new ScriptedFunctor from {
	-- Tor_i(F, G)
	argument => TorOptions >> opts -> X -> applyMethodWithOpts''(Tor, functorArgs(i, X), opts)
	},
    argument => TorOptions >> opts -> X -> applyMethodWithOpts''(Tor, X, opts)
    }

-- see packages/Complexes/Tor.m2 for Tor(ZZ, Module, Matrix) and Tor(ZZ, Matrix, Module)
Tor(ZZ, Module, Matrix) := Matrix => opts -> (i, J, f) -> notImplemented()
Tor(ZZ, Matrix, Module) := Matrix => opts -> (i, J, f) -> notImplemented()

Tor(ZZ, Ring,  Matrix) :=
Tor(ZZ, Ideal, Matrix) := opts -> (i,M,f) -> Tor_i(module M, f, opts)
Tor(ZZ, Matrix, Ring)  :=
Tor(ZZ, Matrix, Ideal) := opts -> (i,f,N) -> Tor_i(f, module N, opts)

-- TODO: Tor_i(R, S) should work as well
Tor(ZZ, Ring, Ring)   :=
Tor(ZZ, Ring, Ideal)  :=
Tor(ZZ, Ring, Module) :=
Tor(ZZ, Ideal, Ring)   :=
Tor(ZZ, Ideal, Ideal)  :=
Tor(ZZ, Ideal, Module) :=
Tor(ZZ, Module, Ring)   :=
Tor(ZZ, Module, Ideal)  := Module => opts -> (i,M,N) -> Tor_i(module M, module N, opts)
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

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
