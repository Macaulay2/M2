--		Copyright 1995 by Daniel R. Grayson

Ext = new ScriptedFunctor from {
     argument => (
	  (M,N) -> (
	       f := lookup(Ext,class M,class N);
	       if f === null then error "no method available"
	       else f(M,N)
	       )
	  ),	  
     superscript => (
	  i -> new ScriptedFunctor from {
	       argument => (X -> (
	       	    	 (M,N) -> (
		    	      f := lookup(Ext,class i,class M,class N);
		    	      if f === null then error "no method available"
		    	      else f(i,M,N)
		    	      )
	       	    	 ) X
	       	    )
	       }
	  )
     }

document { quote Ext,
     TT "Ext^i(M,N)", " -- compute the Ext module of two modules M, N.",
     PARA,
     "If M or N is an ideal or ring, it is regarded as a module in the 
     evident way.",
     SEEALSO("ScriptedFunctor")
     }

document { quote dd,
     TT "C.dd", " -- provides the differential map associated with a
     chain complex C.",
     SEEALSO "ChainComplex"
     }
	  
Ext(ZZ, Module, Module) := (i,M,N) -> (
     R := ring M;
     if R =!= ring N then error "expected modules over the same ring";
     if i < 0 then R^0
     else if i === 0 then Hom(M,N)
     else (
	  C := resolution(M,LengthLimit=>i+1);
	  b := C.dd;
	  complete b;
	  prune
	  if b#?i then (
	       if b#?(i+1) 
	       then homology(Hom(b_(i+1),N), Hom(b_i,N))
	       else cokernel Hom(b_i,N))
	  else (
	       if b#?(i+1) 
	       then kernel Hom(b_(i+1),N)
	       else Hom(C_i,N))))

Ext(ZZ, Matrix, Module) := (i,f,N) -> (
     R := ring f;
     if R =!= ring N then error "expected modules over the same ring";
     if i < 0 then R^0
     else if i === 0 then Hom(f,N)
     else (
	  g := resolution(f,LengthLimit=>i+1);
	  Es := Ext^i(source f, N);
	  Es':= target Es.pruningMap;	  -- Ext prunes everything, so get the original subquotient
	  Et := Ext^i(target f, N);
	  Et':= target Et.pruningMap;
	  Es.pruningMap^-1 * inducedMap(Es',Et',Hom(g_i,N)) * Et.pruningMap))

Ext(ZZ, Module, Matrix) := (i,N,f) -> (
     R := ring f;
     if R =!= ring N then error "expected modules over the same ring";
     if i < 0 then R^0
     else if i === 0 then Hom(N,f)
     else (
	  C := resolution(N,LengthLimit=>i+1);
	  Es := Ext^i(N, source f);
	  Es':= target Es.pruningMap;	  -- Ext prunes everything, so get the original subquotient
	  Et := Ext^i(N, target f);
	  Et':= target Et.pruningMap;
	  Et.pruningMap^-1 * inducedMap(Et',Es',Hom(C_i,f)) * Es.pruningMap))

Ext(ZZ, Matrix, Ring) := (i,f,R) -> Ext^i(f,R^1)
Ext(ZZ, Matrix, Ideal) := (i,f,J) -> Ext^i(f,module J)
Ext(ZZ, Module, Ring) := (i,M,R) -> Ext^i(M,R^1)
Ext(ZZ, Module, Ideal) := (i,M,J) -> Ext^i(M,module J)
Ext(ZZ, Ideal, Ring) := (i,I,R) -> Ext^i(module I,R^1)
Ext(ZZ, Ideal, Ideal) := (i,I,J) -> Ext^i(module I,module J)
Ext(ZZ, Ideal, Module) := (i,I,N) -> Ext^i(module I,N)
Ext(ZZ, Ring, Ring) := (i,S,R) -> Ext^i(S^1,R^1)
Ext(ZZ, Ring, Ideal) := (i,S,J) -> Ext^i(S^1,module J)
Ext(ZZ, Ring, Module) := (i,S,N) -> Ext^i(S^1,N)

TEST "
R = ZZ/101[a,b,c,d]
f = matrix {{c^3-b*d^2, b*c-a*d, b^3-a^2*c, a*c^2-b^2*d}}
M = cokernel f
assert( codim M === 2 )
assert( dim M === 2 )
E = Ext^2(M, R^1)
T = (degreesRing R)_0
p = poincare E
assert ( p == 3*T^(-3)-5*T^(-2)+1*T^(-1)+1 )
assert( dim E === 2 )
assert( dim Ext^1(M,R^1) === -1 )
-- assert ( poincare prune Ext^2(M,M) == (4T^-3 + 2T^-2 - 5T^-1 + 3) (1 - T)^2 )

F = Ext^3(M, R^1)
assert( dim F === 0 )
assert( degree F === 1 )

assert( Ext^4(M,R^1) == 0 )

k = cokernel vars R
N = cokernel matrix {{1_R}}
assert( dim Ext^2(N,k) === -1 )

g = vars R
P = (image g) / (image matrix {{a^2, b^2, c^2, d^2}})

assert( degree Hom(P,k) === 4 )
assert( degree P === 15 )
assert( dim P === 0 )
assert( pdim P === 4 )

assert( degree Ext^4(P,P) === 15 )

image g / image(g**g**g)
"

TEST "
eg1 = () -> (
  R = ZZ/101[a..d];
  m = matrix {{a*d - b*c, a^2*c - b^3, c^3 - b*d^2, a*c^2 - b^2*d}};
  C = resolution cokernel m;
  E2 = Ext^2(cokernel m, R)
  )
eg1()

eg2 = () -> (
  -- gbTrace 3;
  R = ZZ/101[a..f];
  m = matrix {{a*b*c - d*e*f, a*b*d - c*e*f, a*e*f - b*c*d}};
  C = resolution cokernel m;
  -- top m
  )
eg2()

eg3 = () -> (
  -- test newCoordinateSystem
  R = ZZ/101[a..f];
  m = matrix {{a-b+c, a-d-f}};
  newCoordinateSystem(R, m))
--eg3()
"

-- total ext over complete intersections

factorizations = (m) -> (
     -- m is a list of exponents for a monomial
     -- return a list of pairs of lists showing the factorizations
     -- e.g., if m is {1,1} we return
     --	    	 {  ( {1,0}, {0,1} ), ( {0,1}, {1,0} ), ( {1,1}, {0,0} ), ( {0,0}, {1,1} ) }
     if m === {} then { ({}, {}) }
     else (
	  i := m#-1;
	  splice apply(factorizations drop(m,-1), 
	       (n,o) -> apply (0 .. i, j -> (append(n,j), append(o,i-j))))))

makeAdjust := fudge -> v -> {- fudge * v#1 + v#0, - v#1}

Ext(Module,Module) := (N,M) -> (
     R := ring N;
     if R =!= ring M then error "expected modules over the same ring";
     p := R.relations;
     Q := ring p;
     I := ideal p;
     n := numgens Q;
     c := numgens I;
     if c =!= codim R then error "total Ext is available only for complete intersections";
     f := apply(c, i -> I_i);
     adjust := makeAdjust ((1 + max(first \ degree \ f)) // 2);
     toR := map(R,Q);
     N' := pushForward( toR, N );
     M' := pushForward( toR, M );
     E := resolution N';
     s := f / (g -> nullhomotopy (g*id_E));
     X := local X;
     T := k[X_0 .. X_(c-1), toSequence Q.syms, 
	  Degrees => {
	       apply(0 .. c-1,i -> adjust { - first degree f_i, -2}), 
	       n : adjust {1,0}
	       }];
     toT := map(T,Q,apply(toList(c .. c+n-1), i -> T_i));
     S := k[X_0 .. X_(c-1),Degrees=>{c:{2}}];    -- find another way to enumerate monomials
     mS := monoid S;
     use S;
     spots := E -> sort select(keys E, i -> class i === ZZ);
     DMT := T^(apply(spots E, i -> toSequence apply(degrees E_i, d -> adjust {first d,i})));
     Delta := new MutableHashTable;
     Delta#(exponents 1_mS) = -E.dd;
     scan(c, i -> Delta#(exponents mS_i) = s_i);
     scan(4 .. length E + 1, 
	  d -> if even d then (
	       scan( exponents \ leadMonomial \ first entries basis(d,S), 
		    m -> (
			 h := sum(factorizations m,
			      (n,o) -> if Delta#?n and Delta#?o then Delta#n * Delta#o else 0);
			 if h != 0 then (
			      Delta#m = nullhomotopy h;
			      )))));
     DT := map(DMT, DMT, transpose sum ( keys Delta, m -> T_m * toT sum Delta#m ), Degree => adjust {0,-1});
     D := DT ** toT M';
     ext := prune homology(D,D);
     ext#(global adjust) = adjust;
     ext
     )

TEST ///
     k = ZZ/101
     Q = k[x,y]
     I = ideal(x^3,y^5)
     R = Q/I
     k' = coker vars R
     N = cokernel random (R^3, R^{2:-2})
     M = cokernel random (R^3, R^{2:-2})
     E = Ext(N,M)
     adj = E.adjust
     scan(4, d -> (
	  bd := basis Ext^d(N,M);
	  assert(
	       tally splice apply(-10..10,i -> rank source basis(adj {i,-d},E) : {i}) ===
	       tally apply(rank source bd, i -> degree bd_i))))
///
