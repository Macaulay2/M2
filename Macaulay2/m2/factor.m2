--		Copyright 1995 by Daniel R. Grayson

document { quote factor,
     TT "factor x", " -- factors x.",
     PARA,
     "The result is a ", TO "Product", " each of whose factors is a 
     ", TO "Power", " whose base is one of the factors found and whose
     exponent is an integer.",
     EXAMPLE "y = (2^15-4)/(2^15-5)",
     EXAMPLE "x = factor y",
     EXAMPLE "expand x",
     "We may ", TO "peek", " inside ", TT "x", " to a high depth to see
     its true structure as ", TO "Expression", ".",
     EXAMPLE "peek(x,100)",
     PARA,
     "For small integers factorization is done by trial division.  Eventually
     we will have code for large integers.  For multivariate polynomials the
     factorization is done with code of Michael Messollen (see 
     ", TO "Factorization and characteristic sets library", ").  For univariate
     polynomials the factorization is in turn done with code of 
     Gert-Martin Greuel and Ruediger Stobbe (see ", TO "Factory library", ").",
     EXAMPLE "R = ZZ/101[u]",
     EXAMPLE "factor (u^3-1)",
     "The constant term is provided as the last factor.",
     EXAMPLE "F = frac(ZZ/101[t])",
     EXAMPLE "factor ((t^3-1)/(t^3+1))",
     "The code for factoring in a fraction field is easy to read:",
     EXAMPLE "code(factor,F)"
     }

gcd(RingElement,RingElement) := (f,g) -> (
     R := ring f;
     if R =!= ring g then error "expected elements of the same ring";
     sendgg(ggPush f, ggPush g, ggfactor1);
     R.pop())

gcdCoefficients(RingElement,RingElement) := (f,g) -> (
     R := ring f;
     if R =!= ring g then error "expected elements of the same ring";
     sendgg(ggPush f, ggPush g, gggcdextended);
     q := R.pop();
     p := R.pop();
     sendgg(ggpop);
     {p,q})     

--- this is the way the engine's own gcd routine would get called
--- pgcd = (f, g) -> (sendgg(ggPush f, ggPush g, gggcd); new ring f)


pseudoRemainder = method()

pseudoRemainder(RingElement,RingElement) := (f,g) -> (
     R := ring f;
     if R =!= ring g then error "expected elements of the same ring";
     sendgg(ggPush f, ggPush g, ggfactor2);
     R.pop())

document { quote pseudoRemainder,
     TT "pseudoRemainder(f,g)", " -- computes the pseudo-remainder for
     f divided by g.",
     PARA,
     "This is an internal experimental routine."
     }

reorder := I -> (
     f := generators I;
     R := ring I;
     sendgg(ggPush f, ggfactor1);
     v := eePopIntarray();
     assert( #v == numgens R );
     v)

lcm2 := (x,y) -> x*y//gcd(x,y)
lcm := args -> (
     n := 1;
     scan(args, i -> n = lcm2(n,i));
     n)
commden := (f) -> (
     lcm apply(
	  first entries lift((coefficients f)#1, QQ),
	  denominator))

irreducibleCharacteristicSeries = method()
irreducibleCharacteristicSeries Ideal := I -> (
     f := generators I;
     R := ring I;
     if not isPolynomialRing R 
     then error "expected ideal in a polynomial ring";
     k := coefficientRing R;
     if k === QQ then (
	  error "factorization over QQ not implemented yet";
     	  f = matrix {
	       first entries f / (r -> r * commden r)
	       };
	  );
     re := reorder I;
     n := #re;
     x := quote x;
     f = substitute(f,apply(n,i -> R_(re#i) => R_i));
     sendgg(ggPush f, ggfactor2);
     ics := apply(eePopInt(), i -> apply(eePopInt(), j -> R.pop()));
     phi := map(R,R,apply(n,i->R_(re#i)));
     {ics,phi}
     )

document { quote irreducibleCharacteristicSeries,
     TT "irreducibleCharacteristicSeries I", " -- computes the irreducible
     characteristic series of ideal I.",
     PARA,
     "This is an internal routine used by ", TO "decompose", "."
     }

factor ZZ := (n,options) -> (
     new Product from apply(sort pairs factorInteger n, 
	  (p,i)-> new Power from {p,i}
	  )
     )
factor QQ := (r,options) -> factor numerator r / factor denominator r
erase quote factorInteger
-----------------------------------------------------------------------------
topCoefficients = method()
topCoefficients Matrix := f -> (
     R := ring f;
     sendgg(ggPush f, ggcoeffs);
     monoms := getMatrix R;
     coeffs := getMatrix R;
     {monoms, coeffs})

document { quote topCoefficients,
     TT "topCoefficients m", " -- for a matrix m, for each column, returns
     the coefficients of the highest power of the variable with the lowest
     index.",
     PARA,
     "Beware: the greatest variable is usually the first variable.",
     PARA,
     "The value returned is a list ", TT "{monoms, coeff}", ".
     Let x_i be the smallest index variable that occurs in the
     j-th column of ", TT "m", ". Then the j-th column of ", TT "coeff", "
     contains the (vector) coefficient of the highest power of this
     variable, and the j-th element of ", TT "monoms", " is the highest power
     x_i^n."
     }

decompose = method()
decompose(Ideal) := (I) -> if I.?decompose then I.decompose else I.decompose = (
     if not isPolynomialRing ring I
     then error "expected ideal in a polynomial ring";
     ics := irreducibleCharacteristicSeries I;
     Psi := apply(ics#0, CS -> (
	       CS = matrix {CS};
	       chk := topCoefficients CS;
	       chk = chk#1;		  -- just keep the coefficients
	       chk = first entries chk;
	       iniCS := select(chk, i -> degree i =!= {0});
	       CS = ideal CS;
	       scan(iniCS, a -> CS = saturate(CS, a));
	       CS));
     Psi = new MutableList from Psi;
     p := #Psi;
     scan(0 .. p-1, i -> if Psi#i =!= null then 
	  scan(i+1 .. p-1, j -> 
	       if Psi#i =!= null and Psi#j =!= null then
	       if isSubset(Psi#i, Psi#j) then Psi#j = null else
	       if isSubset(Psi#j, Psi#i) then Psi#i = null));
     Psi = toList select(Psi,i -> i =!= null);
     apply(Psi, p -> ics#1 p)
     )

document { quote decompose,
     TT "decompose I", " -- compute the ideals of the irreducible
     components of the subvariety defined by the ideal I.",
     PARA,
     "This code uses ", TO "irreducibleCharacteristicSeries", ".  See also
     ", TO "pseudoRemainder", ".",
     PARA,
     "At the moment, cases that involve factorization over extensions of
     prime fields are not handled, and a warning message is issued.  The
     message is: 'Factorisation over algebraic function field required!'.
     The user should take this as an indication that the factorization is
     not complete."
     }

TEST ///
R = ZZ[x,y,z]
f = (x - 2^32 * y) * (x + 2^33 * z - 77 * y)
d = factor f
assert( #d == 3 and expand d == f )
///

TEST ///
R=ZZ/32003[a..h]
I=ideal(-b*d^2*f*h^2+a*b*d*g*h^2,
	-a*b*d^2*e^2+c^2*g^2*h^2,
	-d^2*f*g^3+a*b*d*e^2*h)
dec = decompose I
assert(dec#-1 == ideal ( d*f-a*g,  g^4-b*e^2*h, a*d^2*g^2-c^2*h^3,
 	  a*b*d^2*e^2-c^2*g^2*h^2, a^2*b*d*e^2-c^2*f*g*h^2, 
	  a^3*b*e^2-c^2*f^2*h^2 ))
assert(dec == {
	  ideal(c,d),
	  ideal(h,d),
	  ideal(g,d),
	  ideal(h,f,a),
	  ideal(c,f,a),
	  ideal(g,f,a),
	  ideal(h,g,a),
	  ideal(c,f,b),
	  ideal(g,b),
	  ideal(h,f,b),
	  ideal(h,e,f),
	  ideal(g,e,f),
	  ideal(h,g,e),
	  ideal(-d*f+a*g,g^4-b*e^2*h,-d^3*f*g+c^2*h^3,-a^3*b*e^2+c^2*f^2*h^2,-a^2*b*d*e^2+c^2*f*g*h^2,-a*b*d^2*e^2+c^2*g^2*h^2)
	  }
     )
///

TEST ///
    -- permanents!
    R = ZZ/32003[r,s,t,u,v,w,x,y,z]
    I = ideal( r*v+s*u, r*w+t*u, s*w+t*v, r*y+s*x, r*z+t*x, s*z+t*y,
	u*y+v*x, u*z+w*x, v*z+w*y)
    time D = decompose I 
			-- used 130.74 seconds
			-- used 127.85 seconds
		        -- used 102.09 seconds
     			-- used 43.06 seconds (Messollen speed up!)
			-- used 41.93 seconds
			-- used 6.87 seconds, cygnus32
			-- 82 seconds in Singular
    assert(D == {
	      ideal(u,r,y,x,z,t*v+s*w),
	      ideal(z,x,y,w,u,v),
	      ideal(v,u,s,w,y,t*x+r*z),
	      ideal(v,s,y,x,z,t*u+r*w),
	      ideal(x,y,r,s,u,v),
	      ideal(v,u,r,w,x,t*y+s*z),
	      ideal(v,s,r,t,y,w*x+u*z),
	      ideal(z,x,y,t,r,s),
	      ideal(u,s,r,t,x,w*y+v*z),
	      ideal(z,x,w,t,r,u),
	      ideal(s,r,t,w,z,v*x+u*y),
	      ideal(v,u,t,w,z,s*x+r*y),
	      ideal(z,y,w,t,s,v),
	      ideal(w,t,r,s,u,v),
	      ideal(t,w,y,x,z,s*u+r*v)
	      })
///

TEST "
     R = ZZ/31991[x,y,z];
     ivd = decompose ideal (x^3-y^2,x^3-z^2,y^3-z^2);
     assert( #ivd ===  5 )
"

TEST "
     R = ZZ/31991[x,y,z]
     I = ideal (x,y)
     J = ideal (y-1,z-1)
     K = intersect(I,J)
     ivd = decompose K
     assert( #ivd == 2 )
"

TEST "
     -- from Wang's paper, example 8.1
     R = ZZ/5[x_1 .. x_4]
     I = ideal (
	       -3*x_3*x_4 + x_2^2 - 2*x_1 + 2,
	       -3*x_1^2*x_4 - 4*x_2*x_3 - 6*x_1*x_3 + 2*x_2^2 + 3*x_1*x_2,
	       -3*x_3^2*x_4 - x_1*x_4 + x_2^2*x_3 + x_2
	       )
     ivd = decompose I
     assert( #ivd === 2 )
"

TEST "
     -- This is a case where P1 factors.
     R = ZZ/109[x,y,z]
     I = ideal ((x-1)^2-(x-1)-3)
     J = ideal (y-1,z^2-z-3)
     P1 = ideal (x^2-x-3,y^2-y-3,z-13)
     P2 = ideal (x-13,y-55,z-12)
     K = intersect(I,J,P1,P2)
     ivd = decompose K
     "

TEST "
R = ZZ/31991[x,y]
assert( (x^2-10748*y*x+y^2)*(y^2+x^2)*(x^2+10748*y*x+y^2) == x^6 + y^6 )
assert ( # factor (x^6 + y^6) == 4 )
"
