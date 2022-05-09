-- Date: Fri, 06 Feb 2009 11:22:32 -0500 (EST)
-- From: Graham Denham <gdenham@uwo.ca>

needsPackage "HyperplaneArrangements"

logComplex = A -> (
     R := ring A; l := (dim R)-(dim coefficientRing R);
     n := #(toList A);
     V := R^l;
     Q := product toList A; J := jacobian ideal Q;
     Lambda := apply((l+1),i->exteriorPower(i,V));
     dQ := apply(l, i-> (
	       In := map(Lambda_i);      -- identity
	       wedgeProduct(1,i,V)*(J**In)));
     Omega := append(apply(l,i -> (      -- first make the modules
	       timesQ := map(Lambda_(i+1),Lambda_(i+1),Q);
	       genlist := gens trim ker map(dQ_i | timesQ, Degree => 2*n);
	       image map(R^(rank Lambda_i),,genlist^{0..(rank Lambda_i)-1}))),  -- toss redundant part
                       image map(R^{0},R^{n},matrix{{1_R}}));   
		       -- top guy has no constraints
-- now extend rings:
     k := coefficientRing R;
     a := symbol a;
     S := tensor(k[a_1..a_n],R,Degrees=>{(n+l):1});
     f := map(S,R);
-- make partial=omega_a on full exterior algebra, then restrict
     C := f coefficients A;
     D := f diagonalMatrix((toList A)/(i->Q//i));  -- 1/alpha times Q
     P := C*D*transpose (vars S)_{0..n-1};         -- Q.omega_a
     partial := apply(l, i-> (
	       In := map(f Lambda_i);      -- identity
	       wedgeProduct(1,i,f V)*(P**In)));
     OmegaS := Omega/f;
     genMatrices := OmegaS/gens;           -- cols are generators
-- now apply Q.differential to gens, write in terms of gens, and get rid of Q:
-- we get maps on enveloping free modules
     maps := apply(l, i-> (partial_i*genMatrices_i)//f Q//genMatrices_(i+1));
-- now induce maps on Omega:
     cplx := apply(l, i-> map(OmegaS_(i+1),OmegaS_i,maps_i));
     chainComplex reverse cplx);

R = QQ[x,y,z]
X3 = arrangement {x,y,z,x+y,x+z,y+z}

C = logComplex X3;
A = ring C;
(S,f) = selectVariables({6,7,8},A);
g = map(S,A,{1,2,3,4,5,6,x,y,z})
-- now we have a segmentation fault:
g(C)

