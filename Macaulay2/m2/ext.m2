--		Copyright 1995 by Daniel R. Grayson

Ext = new ScriptedFunctor from {
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
	  if b#?i then (
	       if b#?(i+1) 
	       then (
		    prune homology(Hom(b_(i+1),N), Hom(b_i,N))
		    )
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
