dMatrix = method()
dMatrix (List,ZZ) := (F,d) -> dMatrix(ideal F, d)
dMatrix (Ideal,ZZ) := (I, d) -> (
-- deflation matrix of order d     
     R := ring I;
     v := flatten entries vars R;
     n := #v;
     ind := toList((n:0)..(n:d)) / toList;
     ind = select(ind, i->sum(i)<=d and sum(i)>0);
     A := transpose diff(matrix apply(ind, j->{R_j}), gens I);
     scan(select(ind, i->sum(i)<d and sum(i)>0), i->(
	       A = A || transpose diff(matrix apply(ind, j->{R_j}), R_i*gens I);
	       ));
     A
     )
dIdeal = method()
dIdeal (Ideal, ZZ) := (I, d) -> (
-- deflation ideal of order d     
     R := ring I;
     v := gens R;
     n := #v;
     ind := toList((n:0)..(n:d)) / toList;
     ind = select(ind, i->sum(i)<=d and sum(i)>0);
     A := dMatrix(I,d);
     newvars := apply(ind, i->getSymbol("x"|concatenate(i/toString)));
     S := (coefficientRing R)[newvars,v]; 
     sub(I,S) + ideal(sub(A,S) * transpose (vars S)_{0..#ind-1})
     )	   
