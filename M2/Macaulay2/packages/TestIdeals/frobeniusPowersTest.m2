TEST /// -- fastExponentiation
R = ZZ/5[x,y];
-- some extreme cases
assert( fastExponentiation(0,0_R) == 1_R )
assert( fastExponentiation(409,0_R) == 0_R )
f = -2*x^5+x^3*y^2+x^2*y^3-2*x*y^4-2*y^5;
assert( fastExponentiation(0,f) == 1_R )
assert( fastExponentiation(1,f) == f )
-- less trivial case, in polynomial ring over prime field
time out1 = f^409;
time out2 = fastExponentiation(409,f);
assert( out1 == out2 )
-- quotient ring over prime field
R = R/ideal( f );
g = -x^10+x^9*y-x^8*y^2+2*x^7*y^3-x^6*y^4+x^4*y^6+2*x^3*y^7+2*x*y^9-2*y^10;
assert( fastExponentiation(0,g) == 1_R )
assert( fastExponentiation(1,g) == g )
time out1 = g^409;
time out2 = fastExponentiation(409,g);
assert( out1 == out2 )
-- quotient ring over a GaloisField
R = GF(25)[x,y]/ideal( x^10 + y^7 )
f = (-a+1)*x^4+(-2*a-2)*x^3*y-a*x^2*y^2+x*y^3+(2*a-2)*y^4-2*x^3+(a-2)*x^2*y+(-2*a+2)*x*y^2+(2*a+2)*y^3
assert( fastExponentiation(123,f) == f^123 )
-- polynomial ring over an infinite field
kk = frac( ZZ/3[t] )
R = kk[x,y,z]
f = -t^2*x^3*z+1/(t^2+1)*x*y+t^(-2)*y^2*z^3+x*z*y^2+1/(t-1)*y*z-t^3*x*z^2-y*t^(-1)+6*z*t-y+z*x^7-t^(-4)
assert( fastExponentiation(16,f) == f^16 )
-- quotient ring over an infinite field
R = R/ideal(f)
g = t^3*x^3*y*z+1/(t^2-1)*x*y*z^3+t^(-2)*x^5*y^2*z^3
assert( fastExponentiation(7,g) == g^7 )
///

TEST /// -- eth roots via frobenius and frobeniusPower
R = ZZ/5[x,y,z,w]
I = ideal(x^27*y^10+3*z^28+4*x^2*y^15*z^35,x^17*w^30+2*x^10*y^10*z^35,x*z^50)
assert(frobenius^(-1)(I) == ideal(x^5*y^2+4*y^3*z^7,z^5,x^3*w^6,x^2*y^2*z^7,z^10))
assert(frobenius(-1,I,FrobeniusRootStrategy => MonomialBasis) == ideal(x^5*y^2+4*y^3*z^7,z^5,x^3*w^6,x^2*y^2*z^7,z^10))
assert(frobeniusPower(1/5^2,I) == ideal(x,z,w))
assert(frobenius(-2,I,FrobeniusRootStrategy => MonomialBasis) == ideal(x,z,w))
assert(frobenius^(-3) I == ideal(1_R))
assert(frobeniusPower(1/5^3,I,FrobeniusRootStrategy => MonomialBasis) == ideal(1_R))
///

TEST ///  -- eth roots via frobenius and frobeniusPower
R = GF(27)[x,y,z]
--The ambient ring of GF(27) is ZZ[a]/(a^3-a+1).
I = ideal(a^2*x^18+(a-1)*x^14*y^7*z^4 +x^2*y^10*z^10,(a^2-a)*x^5*y^9*z^8-y^21)
--a^(1/3) = a + 1
--a^(1/9) = a - 1
assert(frobenius^(-1) I == ideal(x^6,a*x^4*y^2*z+y^3*z^3,x*y^3*z^2,y^7))
assert(frobenius(-1,I,FrobeniusRootStrategy => MonomialBasis) == ideal(x^6,a*x^4*y^2*z+y^3*z^3,x*y^3*z^2,y^7))
assert(frobeniusPower(1/3^2,I) == ideal(x,y))    
assert(frobeniusPower(1/3^2,I,FrobeniusRootStrategy => MonomialBasis) == ideal(x,y))
assert(frobenius^(-3) I == ideal(1_R))
assert(frobenius(-3,I,FrobeniusRootStrategy => MonomialBasis) == ideal(1_R))    
///


