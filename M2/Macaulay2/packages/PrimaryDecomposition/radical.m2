-- Computing the radical of an ideal
-- Will be part of the PrimaryDecomposition package (most likely)

needsPackage "Elimination"
getMinimalPoly = method()
getMinimalPoly(Ideal,RingElement,RingElement) := (I,u,x) -> (
     ux := u*x;
     elimvars := select(gens ring I, y -> ux % y != 0);
     J := eliminate(I,elimvars);
     d := min apply(numgens J, i -> degree(x, J_i));
     --error "getminpoly";
     fs := select(1, flatten entries gens J, f -> degree(x,f) === d);
     fs#0
     )

getSeparablePart = method()
getSeparablePart(RingElement,RingElement,RingElement) := (f,u,x) -> (
     g := factors f;
     product select(g, g1 -> degree(x,g1) > 0))

-- MES: I grabbed this code from another file
zerodimRadical = (I) -> (
     -- assumption: dim I is 0
     if dim I != 0 then error "expected zero dimensional ideal";
     X := gens ring I;
     trim ideal gens gb(I + sum apply(#X, i -> eliminate(drop(X,{i,i}), I)))
     )

radical00 = method()     
radical00(Ideal,RingElement) := (I,u) -> (
     -- For each variable not in u, compute the 
     -- squarefree part (separable part)
     v := select(gens ring I, x -> u % x != 0);
     newelems := {};
     scan(v, x -> (
	       -- there are THREE problems here!
	       -- (a) use linear algebra
	       -- (b) char p
	       -- (c) f might not be the smallest eqn in var v_i.
	       --<< "about to get minpoly " << toString I << " u = " << toString u << " x = " << toString x << endl;
	       f := getMinimalPoly(I,u,x);
	       g := getSeparablePart(f,u,x);
	       if f != g then newelems = append(newelems,g)));
     --error "in radical00";
     if #newelems > 0 then I = I + ideal newelems;
     I
     )

rad = method()
rad Ideal := (I) -> (
     -- returns the radical of I
     R := ring I;
     radI := ideal(1_R);
     while I != 1 do (
	  u := independentSets(I,Limit=>1);
	  u = if #u === 0 then 1_R else first u;
	  J := radical00(I,u);
	  h := flatt(J,u);
	  h = (intersect values h)_0;
	  radJ := saturate(J,h);
	  radI = intersect(radI,radJ);
	  h = flatt(I,u);
	  h = (intersect values h)_0;
	  if h != 1 then 
	      h = product factors h;
	  --error "what h is being added?";
	  I = I + ideal(h);
	  );
     radI
     )
