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

----------------------------------------
restart
load "5-ataylor.m2"
load "/Users/mike/local/my-projects/gins/reg4.m2"
R = ZZ/32003[a..d]
I = borel monomialIdeal(b^4, a^3*c)
J = ginFamily I;
T = ring J
forceGB gens J
((gens J) * substitute((syz gens I),T)) % J
(mons,cfs) = coefficients(oo,Variables=>{a,b,c,d});
L = first entries compress flatten cfs;
xm = new MutableList from first entries vars T

finishMap(L,xm);

P = select(L, f -> size f <= 4)
P = P/checkpoly
P/(x -> support x_1)
F = map(T,T,P/(x -> (T_(x#0) => x#1)))
F (L_1)
positions(L, f -> size f <= 4)
L_3
F L_3
F L_6
F L_8
Lsupp = L/support;
positions(Lsupp, x -> member(T_36,x))
positions(Lsupp, x -> member(T_39,x))
positions(Lsupp, x -> member(T_41,x))
L = apply(L,f -> F f);
tally(L/size)

P = select(L, f -> size f <= 5 and f != 0)
P = P/checkpoly
F = map(T,T,P/(x -> (T_(x#0) => x#1)))
L1 = apply(L,f -> F f);
L1a = apply(L1,f -> F f);
L1b = apply(L1a,f -> F f);
L1b == L1a
L1b/toString

tally(L1b/size)
P = select(L1b, f -> size f <= 8 and f != 0)
P = P/checkpoly
P = select(P, x -> x =!= {})
F = map(T,T,P/(x -> (T_(x#0) => x#1)))
L2 = apply(L1b,f -> F f);
L2a = apply(L2,f -> F f);
L2 == L2a

tally(L2a/size)
P = select(L2a, f -> size f <= 10 and f != 0)
P = P/checkpoly
P = select(P, x -> x =!= {})
F = map(T,T,P/(x -> (T_(x#0) => x#1)))
L3 = apply(L2a,f -> F f);
L3a = apply(L3,f -> F f);
L3b = apply(L3a,f -> F f);
L3b == L3a
L3c = apply(L3b,f -> F f);
L3c == L3b -- true....

tally(L3a/size)
P = select(L2a, f -> size f <= 10 and f != 0)
P = P/checkpoly
P = select(P, x -> x =!= {})
F = map(T,T,P/(x -> (T_(x#0) => x#1)))
L3 = apply(L2a,f -> F f);
L3a = apply(L3,f -> F f);
L3b = apply(L3a,f -> F f);
L3b == L3a
L3c = apply(L3b,f -> F f);
L3c == L3b -- true....
L3 = select(L3b, f -> f != 0);
#L3
L3_0

P = L3/checkpoly;
P = select(P, x -> x =!= {});
tally(P/(x -> size x#1))

P1 = select(P, x -> size x#1 <= 13)
F = map(T,T,P1/(x -> (T_(x#0) => x#1)))
L4 = apply(L3,f -> F f);
L4a = apply(L4, f -> F f);
L4b = apply(L4a, f -> F f);
L4c = apply(L4b, f -> F f);
L4d = apply(L4c, f -> F f);
L4d == L4c -- true...

L_1
part(1,L_1)
apply(L, f -> part(1,f))

I
N = prune Hom(I,(ring I)^1/I)
N = sheaf N
HH^0(N)

R = ZZ/32003[a..f]
syz vars R
M = oo * random(R^15, R^6)
J = ideal syz M
syz transpose M
res J
betti oo
J1 = substitute(J, {f => 0})
res J1
