decompose Ideal := (I) -> if I.cache.?decompose then I.cache.decompose else I.cache.decompose = (
     R := ring I;
     if isQuotientRing R then (
	  A := ultimate(ambient, R);
	  I = lift(I,A);
	  )
     else A = R;
     if not isPolynomialRing A then error "expected ideal in a polynomial ring or a quotient of one";
     if I == 0 then return {if A === R then I else ideal 0_R};
     ics := irreducibleCharacteristicSeries I;
     Psi := apply(ics#0, CS -> (
	       chk := topCoefficients CS;
	       chk = chk#1;  -- just keep the coefficients
	       chk = first entries chk;
	       iniCS := select(chk, i -> degree i =!= {0});
	       CS = ideal CS;
	       --<< "saturating " << CS << " with respect to " << iniCS << endl;
	       -- warning: over ZZ saturate does unexpected things.
	       scan(iniCS, a -> CS = saturate(CS, a, Strategy=>Eliminate));
--             scan(iniCS, a -> CS = saturate(CS, a));
	       --<< "result is " << CS << endl;
	       CS));
     Psi = new MutableList from Psi;
     p := #Psi;
     scan(0 .. p-1, i -> if Psi#i =!= null then 
	  scan(i+1 .. p-1, j -> 
	       if Psi#i =!= null and Psi#j =!= null then
	       if isSubset(Psi#i, Psi#j) then Psi#j = null else
	       if isSubset(Psi#j, Psi#i) then Psi#i = null));
     Psi = toList select(Psi,i -> i =!= null);
     components := apply(Psi, p -> ics#1 p);
     if A =!= R then (
	  components = apply(components, I -> ideal(generators I ** R));
	  );
     components
     )

end
restart
load "decompose2-test.m2"
     kk = QQ
     kk = ZZ/32003
     R = kk[x,y_1,y_2,z_1,z_2,a,b,c];     
     I = ideal(
	  x+2*y_1*z_1+3*a*y_1^2+5*y_1^4+2*c*y_1,
	  x+2*y_2*z_2+3*a*y_2^2+5*y_2^4+2*c*y_2,
	  2*z_2+6*a*y_2+20*y_2^3+2*c,
	  3*z_1^2+y_1^2+b,
	  3*z_2^2+y_2^2+b)

C = time irreducibleCharacteristicSeries I
topCoefficients C_0_0
use ring I
J = ideal C_0_0
f = b-c
time L1 = saturate(J,f^2);
time L2 = saturate(J,f);
assert(L1 == L2)
time L3 = saturate(J,f^2,Strategy=>Eliminate);
time L4 = saturate(J,f,Strategy=>Eliminate);
assert(L1 == L3)
assert(L1 == L4)

time intersect apply(flatten entries (topCoefficients C_0_0)_1, f -> ideal matrix(R,{{f}}))

gbTrace = 3
time decompose I;


gbTrace = 0
time L1 = saturate(J,f,Strategy=>Eliminate); -- seems OK
time L2 = saturate(J,f);  -- INCORRECT over QQ
assert(L1 == L2)

-- Fixed bug dealing with normal forms of syzygies over quotients over QQ
gbTrace=3
kk = QQ
R = kk[x,y_1,y_2,z_1,z_2,a,b,c];
J = ideal(3*z_2^2-3*a^2-b^2+c^2,
     15*b^4-20*b^3*c+5*c^4+3*z_1*b^2-6*z_1*b*c+3*z_1*c^2+2*z_2*c-2*a*c,
     -5*b^5+10*b^3*c^2-5*b*c^4+y_2*b^2+a*b^2-2*y_2*b*c-2*z_2*b*c+y_2*c^2+a*c^2,
     10*b^5*c-15*b^4*c^2+5*b^2*c^4+2*z_2*b^2*c-2*a*b^2*c+y_1*b^2-2*y_1*b*c+y_1*c^2,
     3*a^2+b^2+x)
S = R/J
m = matrix{{b-c}}
L = syz gb(m, Syzygies=>true) -- seems incorrect
L' = syz m
m * L

kk = ZZ/32003
R = kk[x,y_1,y_2,z_1,z_2,a,b,c];
J = ideal(3*z_2^2-3*a^2-b^2+c^2,
     15*b^4-20*b^3*c+5*c^4+3*z_1*b^2-6*z_1*b*c+3*z_1*c^2+2*z_2*c-2*a*c,
     -5*b^5+10*b^3*c^2-5*b*c^4+y_2*b^2+a*b^2-2*y_2*b*c-2*z_2*b*c+y_2*c^2+a*c^2,
     10*b^5*c-15*b^4*c^2+5*b^2*c^4+2*z_2*b^2*c-2*a*b^2*c+y_1*b^2-2*y_1*b*c+y_1*c^2,
     3*a^2+b^2+x)
S = R/J
m = matrix{{b-c}}
L1 = syz gb(m, Syzygies=>true)
L2 = value toString L
L1 % L2
L2 % L1
assert(L1 == L2)

--- compute the saturation
restart
kk = QQ
kk = ZZ/10037
--kk = ZZ/32003
R = kk[t, x, y_1, y_2, z_1, z_2, a, b, c, MonomialOrder=>Eliminate 1]
-- J is the same J as above.
J = ideal(3*z_2^2-3*a^2-b^2+c^2,
     15*b^4-20*b^3*c+5*c^4+3*z_1*b^2-6*z_1*b*c+3*z_1*c^2+2*z_2*c-2*a*c,
     -5*b^5+10*b^3*c^2-5*b*c^4+y_2*b^2+a*b^2-2*y_2*b*c-2*z_2*b*c+y_2*c^2+a*c^2,
     10*b^5*c-15*b^4*c^2+5*b^2*c^4+2*z_2*b^2*c-2*a*b^2*c+y_1*b^2-2*y_1*b*c+y_1*c^2)
     3*a^2+b^2+x)
f = b-c
L = ideal(t*f-1) + J
time gens gb L; -- in char p, 22 gens, using elim order, FAILS in char 0??
time L1 = ideal mingens ideal selectInSubring(1, gens gb L);
transpose gens L1

-- homogenize
kk = QQ
R = kk[t, x, y_1, y_2, z_1, z_2, a, b, c, h, MonomialOrder=>Eliminate 1]
J = ideal(3*z_2^2-3*a^2-b^2+c^2,
     15*b^4-20*b^3*c+5*c^4+3*z_1*b^2-6*z_1*b*c+3*z_1*c^2+2*z_2*c-2*a*c,
     -5*b^5+10*b^3*c^2-5*b*c^4+y_2*b^2+a*b^2-2*y_2*b*c-2*z_2*b*c+y_2*c^2+a*c^2,
     10*b^5*c-15*b^4*c^2+5*b^2*c^4+2*z_2*b^2*c-2*a*b^2*c+y_1*b^2-2*y_1*b*c+y_1*c^2,
     3*a^2+b^2+x)
f = b-c
L = ideal(t*f-1) + J
L = homogenize(L,h)
gbTrace=3
time gens gb L; -- very bad
transpose leadTerm sort gens gb L

-- a related bug (same bug?)
restart
kk = QQ
kk = ZZ/10037
kk = ZZ/32003
gbTrace=10
R = kk[t, x, y_1, y_2, z_1, z_2, a, b, c, MonomialOrder=>Eliminate 1]
-- J is the same J as above.
J = ideal(3*z_2^2-3*a^2-b^2+c^2,
     15*b^4-20*b^3*c+5*c^4+3*z_1*b^2-6*z_1*b*c+3*z_1*c^2+2*z_2*c-2*a*c,
     -5*b^5+10*b^3*c^2-5*b*c^4+y_2*b^2+a*b^2-2*y_2*b*c-2*z_2*b*c+y_2*c^2+a*c^2,
     10*b^5*c-15*b^4*c^2+5*b^2*c^4+2*z_2*b^2*c-2*a*b^2*c+y_1*b^2-2*y_1*b*c+y_1*c^2)
f = b-c
L = ideal(t*f-1) + J
gens gb L; -- in char p, 21 gens, using elim order, FAILS in char 0?? 
printWidth=70
transpose sort gens gb L
transpose selectInSubring(1, gens gb L) -- 13 gens

L1 = ideal mingens ideal selectInSubring(1, gens gb L); -- 8 gens (char p)
transpose gens L1
