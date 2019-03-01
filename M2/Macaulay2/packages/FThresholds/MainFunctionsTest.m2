--Below, we verify computations for a binomial from Example 4.3-4.5 in
--"F-pure thresholds of binomial hypersurfaces" by D. Hernandez.
--In each case, fpt is as computed in SpecialFThresholdTest.m2
--and nu's are read off from this.
--binomial test 1
TEST ///
ZZ/47[x,y];
f= x^7*y^2 + x^5*y^6;
assert(fpt(f)==3/16)
assert(nu(1,f)==8)
assert(nu(2,f)==414)
assert(nu(3,f)==19466)
assert(nu(3,f, UseSpecialAlgorithms => false)==19466)
assert(nu(3,f, UseSpecialAlgorithms => false, ComputePreviousNus=>false)==19466)
assert(nu(3,f, UseSpecialAlgorithms => false, Search=>BinaryRecursive)==19466)
assert(nu(3,f, UseSpecialAlgorithms => false, Search=>Linear)==19466)
assert(fptApproximation(2,f)=={0,8/47,414/2209})
///

--binomial test 2
TEST ///
ZZ/43[x,y];
f= x^7*y^2 + x^5*y^6;
assert(fpt(f)==8/43)
assert(nu(1,f)==7)
assert(nu(2,f)==343)
assert(nu(3,f)==14791)
assert(nu(3,f, UseSpecialAlgorithms => false)==14791)
assert(nu(3,f, UseSpecialAlgorithms => false, ComputePreviousNus=>false)==14791)
assert(nu(3,f, UseSpecialAlgorithms => false, Search=>BinaryRecursive)==14791)
assert(nu(3,f, UseSpecialAlgorithms => false, Search=>Linear)==14791)
assert(fptApproximation(2,f)=={0,7/43,343/43^2})
///

--binomial test 3
TEST ///
ZZ/37[x,y];
f= x^7*y^2 + x^5*y^6;
assert(nu(1,f)==6)
assert(nu(2,f)==256)
assert(nu(3,f)==9494)
assert(nu(3,f, UseSpecialAlgorithms => false)==9494)
assert(nu(3,f, UseSpecialAlgorithms => false, ComputePreviousNus=>false)==9494)
assert(nu(3,f, UseSpecialAlgorithms => false, Search=>BinaryRecursive)==9494)
assert(nu(3,f, UseSpecialAlgorithms => false, Search=>Linear)==9494)
assert(fptApproximation(2,f)=={0,6/37,256/1369})
///

--Below, our computations are based on results in the paper
--"Frobenius Powers of some monomial ideals" by Hernandez, Teixeira, Witt
TEST ///
ZZ/5[x,y];
M=ideal(x,y);
D=ideal(x^3,y^3);
f=x^3+y^3; -- generic element of D
g=x^3+x^2*y + x*y^2 + y^3 -- generic element of M^3
assert(criticalExponentApproximation(1,M^3,M)=={0,2/5})
assert(criticalExponentApproximation(2,M^3,M)=={0,2/5,14/25})
assert(criticalExponentApproximation(1,D,M)=={0,2/5})
assert(criticalExponentApproximation(2,D,M)=={0,2/5,14/25})
assert(fptApproximation(1,f)=={0,2/5})
assert(fptApproximation(2,f)=={0,2/5,14/25})
assert(fptApproximation(1,g)=={0,2/5})
assert(fptApproximation(2,g)=={0,2/5,14/25})
///

TEST ///
ZZ/7[x,y];
M=ideal(x,y);
D=ideal(x^4,y^4);
I=ideal(x^2,y);
f=x^4+y^4; -- generic element of D
g=x^4+x^3*y+x^2*y^2+x*y^3+y^4; -- generic element of M^4
assert(criticalExponentApproximation(1,M^4,I)=={0,4/7})
assert(criticalExponentApproximation(2,M^4,I)=={0,4/7,34/49})
assert(criticalExponentApproximation(1,D,I)=={0,4/7})
assert(criticalExponentApproximation(2,D,I)=={0,4/7,34/49})
assert(ftApproximation(1,f,I)=={0,4/7})
assert(ftApproximation(2,f,I)=={0,4/7,34/49})
assert(ftApproximation(1,g,I)=={0,4/7})
assert(ftApproximation(2,g,I)=={0,4/7,34/49})
///

--Here, we test F-pure threshold approximation computations for polynomials
TEST ///
ZZ/5[x,y,z];
f = 2*x^7*y^3*z^8+2*x^4*z^9+2*x*y^7*z^4;
assert( nu(6,f) == 2968 )
assert( nu(6,f,ComputePreviousNus => false) == 2968 )
assert( nu(6,f,Search => BinaryRecursive) == 2968 )
assert( nu(6,f,Search => Linear) == 2968 )
assert( nu(6,f,UseColonIdeals=>true,ContainmentTest => StandardPower) == 2968 )
assert( nu(6,ideal f,UseColonIdeals=>true,ContainmentTest => StandardPower,Search => BinaryRecursive) == 2968 )
assert( nu(6,ideal f,UseColonIdeals=>true,ContainmentTest => StandardPower,Search => Linear) == 2968 )
assert( nuList(6,f) == {0, 0, 4, 23, 118, 593, 2968} )
assert( nuList(6,f,UseColonIdeals => true) == {0, 0, 4, 23, 118, 593, 2968} )
//

TEST ///
ZZ/17[x,y,z,w];
F = -5*x*y^4*z^3-2*x^4*z^3*w+6*y*z^3*w^4+7*z*w^3-6*w^2;
assert( nu(2,F) == 220 )
assert( nu(2,F,ComputePreviousNus => false) == 220 )
assert( nu(2,F,Search => BinaryRecursive) == 220 )
assert( nu(2,F,Search => Linear) == 220 )
assert( nu(2,F,UseColonIdeals=>true,ContainmentTest => StandardPower) == 220 )
assert( nu(2,ideal F,UseColonIdeals=>true,ContainmentTest => StandardPower,Search => BinaryRecursive) == 220 )
assert( nu(2,ideal F,UseColonIdeals=>true,ContainmentTest => StandardPower,Search => Linear) == 220 )
assert( nu(3,F) == 3756 )
///

TEST ///
ZZ/3[x,y,z];
I=ideal(x^2+y^3,y^2+z^3,z^2+x^3);
assert( nu(3,I) == 39 )
assert( nu(3,I,ComputePreviousNus => false) == 39 )
assert( nu(3,I,Search => BinaryRecursive) == 39 )
assert( nu(3,I,Search => Linear) == 39 )
assert( nu(3,I,UseColonIdeals => true,ContainmentTest => FrobeniusRoot) == 39 )
assert( nuList(3,I) == {0, 3, 12, 39} )
assert( nuList(3,I,UseColonIdeals => true) == {0, 3, 12, 39} )
assert( nu(5,I,ComputePreviousNus => false, ContainmentTest => FrobeniusPower) == 242 )
assert( nu(5,I,UseColonIdeals => true, ContainmentTest => FrobeniusPower) == 242 )
assert( nuList(5,I, ContainmentTest => FrobeniusPower) == {0, 2, 8, 26, 80, 242} )
///

TEST ///
ZZ/13[x,y,z];
I=ideal(x^3+y^4,y^6+z^3,z^5+x^7);
assert( nu(1,I) == 11 )
assert( nu(1,I,Search => Linear) == 11 )
assert( nu(1,I,ComputePreviousNus => false) == 11 )
assert( nu(2,I, ContainmentTest => FrobeniusPower) == 154 )
///

TEST ///
ZZ/7[x,y,z];
I=ideal(x+y^2,y+z^2,z+x^2);
J=ideal(x^3,y^3,z^3);
time assert( nu(1,I,J) == 60 )
time assert( nu(1,I,J,ContainmentTest => FrobeniusRoot) == 60 )
time assert( nu(1,I,J,ContainmentTest => FrobeniusRoot,Search => Linear) == 60 )
time assert( nu(1,I,J, ContainmentTest => FrobeniusPower) == 48 )
time assert( mu(1,I,J) == 48 )
time assert( mu(1,I,J,ComputePreviousNus => false) == 48 )
time assert( nuList(1,I,J,ContainmentTest=> FrobeniusPower) == {6, 48} )
time assert( muList(1,I,J) == {6, 48} )
///

TEST ///
R = ZZ/11[x,y,z]/ideal(x*y-z^2);
f = sub(x, R);
assert(compareFPT(5/11, f) < 0);
assert(compareFPT(1/2, f) == 0);
assert(compareFPT(61/120, f) > 0);
///

TEST ///
T = ZZ/7[a,b];
S = ZZ/7[x,y,z,w];
f = map(T, S, {a^3, a^2*b, a*b^2, b^3});
I = ker f;
R = S/I;
g = sub(x, R);
assert(compareFPT(113/342, g) < 0);
assert(compareFPT(1/3, g) == 0);
assert(compareFPT(17/49, g) > 0);
///
