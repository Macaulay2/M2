-- These are from minPres
checkpoly = (f)->(
     -- 1 Argument:  A polynomial.
     -- Return:      A list of the index of the first 
     --              (by index in the ring) variable that occurs 
     --              linearly in f and does not occur in any other 
     --              term of f and a polynomial with that term 
     --              eliminated.
     A := ring(f);
     p := first entries contract(vars A,f);
     i := position(p, g -> g != 0 and first degree g === 0);
     if i === null then
         {}
     else (
     	  v := A_i;
     	  c := f_v;
     	  result := {i,(-1)*(c^(-1)*(f-c*v))};
	  print result;
	  result
	  )
     )


finishMap = (L,xmap) -> (
     -- 2 Arguments:  A matrix and a new mutable list.
     -- Return:       a map from the ring corresponding to 
     --               entries in the matix to itself given by 
     --               entries in the matrix which have a linear
     --               term that does not occur elsewhere in the 
     --               polynomial. 
     A := ring L_0;
     count := #L;
     while count > 0 do (
	  --p := checkpoly(L_(count-1));
	  g := substitute(L_(count-1),matrix{toList xmap});
	  p := checkpoly(g);
	  if p =!= {} then (
	       xmap#(p#0) = p#1;
	       F1 := map(A,A,toList xmap);
	       << "F1 = " << F1 << endl;
	       F2 := map(A,A, F1 (F1.matrix));
	       << "F2 = " << F2 << endl;	       
	       xmap = new MutableList from first entries F2.matrix;);
	  count = count-1
	  );
     map(A,A,toList xmap)
     )

finishMap = (L,xmap) -> (
     -- 2 Arguments:  A matrix and a new mutable list.
     -- Return:       a map from the ring corresponding to 
     --               entries in the matix to itself given by 
     --               entries in the matrix which have a linear
     --               term that does not occur elsewhere in the 
     --               polynomial. 
     A := ring L_0;
     count := #L;
     while count > 0 do (
	  p := checkpoly(L_(count-1));
	  if p =!= {} then (
	       xmap#(p#0) = p#1;
	       F1 := map(A,A,toList xmap);
	       << "F1 = " << toString(F1) << endl;
	       F2 := map(A,A, F1 (F1.matrix));
	       << "F2 = " << toString(F2) << endl;	       
	       xmap = new MutableList from first entries F2.matrix;);
	  count = count-1
	  );
     map(A,A,toList xmap)
     )
end
restart
load "5-ataylor.m2"
S = ZZ/32003[w_0, w_1, x, y, z, MonomialSize => 16]
--S = ZZ/32003[w_0, w_1, x, y, z]
I = ideal (y^2-x*z, x*y-z^2, x^2-y*z, w_1*z-x, w_1*y-z, w_1*x-y, w_0*z-y, w_0*y-x, w_0*x-z, w_1^2-w_0, w_0*w_1-1, w_0^2-w_1)

F = finishMap(flatten entries generators I, new MutableList from first entries (vars S)); 


minimalPresentation I
I == intersect decompose I
C = decompose I

use ring I
I1 = trim substitute(I, {x => w_1*z})
I2 = trim substitute(I1, {y => w_0*z})
I3 = trim substitute(I2, {w_0 => w_1^2})

use ring I
I1 = trim substitute(I, {w_1 => w_0^2})
I2 = trim substitute(I1, {z => w_0*x})
I3 = trim substitute(I2, {x => w_0*y})

checkpoly(I_0)
checkpoly(I_1)
checkpoly(I_2)
checkpoly(I_3)
checkpoly(I_4)

R = ZZ/32003[x]
F = map(R,R,{x^4})
g = x
for i from 1 to 20 do g = F g
g

use S
F1 = map(S,S,{w_0^256, w_1^256, w_0^16*y, w_0*z, w_0^65*y})
F1 (w_0^256)

S = ZZ/32003[w_0, w_1, x, y, z, MonomialSize=>16]
F1 = map(S,S,{w_0^256, 1, 1, 1, 1})
F1 (w_0^256)
peek F1
peek S
