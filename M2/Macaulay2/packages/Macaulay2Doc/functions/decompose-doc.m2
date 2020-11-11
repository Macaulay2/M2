--- status: Draft
--- author(s): Decker
--- notes: 

document { 
     Key => {minimalPrimes, decompose, (minimalPrimes, Ideal), (decompose, Ideal)-*,(minimalPrimes, MonomialIdeal), (decompose, MonomialIdeal)*-}, -- the latter two are in PrimaryDecomposition
     Headline => "minimal associated primes of an ideal",
     Usage => "minimalPrimes I\ndecompose I",
     Inputs => {"I" => Ideal,
	  },
     Outputs => {List => {"whose entries are the minimal associated primes of ", TT "I", " ."}
	  },
     TT "decompose", " is a synonym for ", TT "minimalPrimes", ".",
     PARA{},
     "This function computes the minimal associated primes
     of the ideal ", TT "I"," using characteristic sets. Geometrically, 
     it decomposes the algebraic set defined by ", TT "I.",
     PARA{},
     "If I is ", ofClass MonomialIdeal, ", then a more efficient algorithm
     is used.",
     PARA{},
     "Example. The homogenized equations
      of the affine twisted cubic curve define the union of the
      projective twisted cubic curve and a line at infinity:",
     EXAMPLE lines ///
	  R=QQ[w,x,y,z];
	  I=ideal(x^2-y*w,x^3-z*w^2)
	  minimalPrimes I
	  ///,
     "Note that the ideal is decomposed over the given field
     of coefficients and not over the extension field where
     the decomposition into absolutely irreducible factors
     occurs:",
      EXAMPLE lines ///
	  I = ideal(x^2+y^2)
	  minimalPrimes I
	  ///,
     PARA{},
     "For monomial ideals, the method used is essentially what is shown in the example.",
     EXAMPLE lines ///
	  I = monomialIdeal ideal"wxy,xz,yz"
	  minimalPrimes I
	  P = intersect(monomialIdeal(w,x,y),monomialIdeal(x,z),monomialIdeal(y,z))
	  minI = apply(flatten entries gens P, monomialIdeal @@ support)
	  ///,
     "It is sometimes useful to have the result P instead (where each generator
     encodes a single minimal prime.  This can be obtained directly, as in the
     following code.",
     EXAMPLE lines ///
	  dual radical I
	  P == oo
          ///,
     SeeAlso => {topComponents, removeLowestDimension, radical, irreducibleCharacteristicSeries,
	  (dual,MonomialIdeal)}
     }


TEST ///
R = ZZ[x,y,z]
f = (x - 2^32 * y) * (x + 2^33 * z - 77 * y)
d = factor f
assert( #d == 2 and value d == f )
///

TEST ///
R=ZZ/32003[a..h]
I=ideal(-b*d^2*f*h^2+a*b*d*g*h^2,
	-a*b*d^2*e^2+c^2*g^2*h^2,
	-d^2*f*g^3+a*b*d*e^2*h)
dec = minimalPrimes I
assert(dec#0 == ideal ( d*f-a*g,  g^4-b*e^2*h, a*d^2*g^2-c^2*h^3,
 	  a*b*d^2*e^2-c^2*g^2*h^2, a^2*b*d*e^2-c^2*f*g*h^2, 
	  a^3*b*e^2-c^2*f^2*h^2 ))
   E = {
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

    Ds = set apply(dec, I -> gens gb I)
    Es = set apply(E, I -> gens gb I)
    assert(Ds === Es)

///

TEST ///
    -- permanents!
    R = ZZ/32003[r,s,t,u,v,w,x,y,z]
    I = ideal( r*v+s*u, r*w+t*u, s*w+t*v, r*y+s*x, r*z+t*x, s*z+t*y,
	u*y+v*x, u*z+w*x, v*z+w*y)
    time D = minimalPrimes I 
			-- used 130.74 seconds
			-- used 127.85 seconds
		        -- used 102.09 seconds
     			-- used 43.06 seconds (Messollen speed up!)
			-- used 41.93 seconds
			-- used 6.87 seconds, cygnus32
     			-- used 5.19 seconds, linux
			-- 82 seconds in Singular
    E = {
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
	      }
    Ds = set apply(D, I -> gens gb I)
    Es = set apply(E, I -> gens gb I)
    assert(Ds === Es)
///

TEST "
     R = ZZ/31991[x,y,z];
     ivd = minimalPrimes ideal (x^3-y^2,x^3-z^2,y^3-z^2);
     assert( #ivd ===  5 )
"

TEST "
     R = ZZ/31991[x,y,z]
     I = ideal (x,y)
     J = ideal (y-1,z-1)
     K = intersect(I,J)
     ivd = minimalPrimes K
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
     ivd = minimalPrimes I
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
     ivd = minimalPrimes K
     "

TEST "
R = ZZ/31991[x,y]
assert( (x^2-10748*y*x+y^2)*(y^2+x^2)*(x^2+10748*y*x+y^2) == x^6 + y^6 )
assert ( # factor (x^6 + y^6) == 3 )
"

