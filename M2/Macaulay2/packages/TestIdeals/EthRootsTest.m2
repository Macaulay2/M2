TEST /// -- test 0
R = ZZ/5[x,y,z,w]
I = ideal(x^27*y^10+3*z^28+4*x^2*y^15*z^35,x^17*w^30+2*x^10*y^10*z^35,x*z^50)
assert(frobeniusRoot(1,I) == ideal(x^5*y^2+4*y^3*z^7,z^5,x^3*w^6,x^2*y^2*z^7,z^10))
assert(frobeniusRoot(1,I,FrobeniusRootStrategy => MonomialBasis) == ideal(x^5*y^2+4*y^3*z^7,z^5,x^3*w^6,x^2*y^2*z^7,z^10))
assert(frobeniusRoot(2,I) == ideal(x,z,w))
assert(frobeniusRoot(2,I,FrobeniusRootStrategy => MonomialBasis) == ideal(x,z,w))
assert(frobeniusRoot(3,I) == ideal(1_R))
assert(frobeniusRoot(3,I,FrobeniusRootStrategy => MonomialBasis) == ideal(1_R))
///

TEST /// -- test 1
R = GF(27)[x,y,z]
--The ambient ring of GF(27) is ZZ[a]/(a^3-a+1).
I = ideal(a^2*x^18+(a-1)*x^14*y^7*z^4 +x^2*y^10*z^10,(a^2-a)*x^5*y^9*z^8-y^21)
--a^(1/3) = a + 1
--a^(1/9) = a - 1
assert(frobeniusRoot(1,I) == ideal(x^6,a*x^4*y^2*z+y^3*z^3,x*y^3*z^2,y^7))
assert(frobeniusRoot(1,I,FrobeniusRootStrategy => MonomialBasis) == ideal(x^6,a*x^4*y^2*z+y^3*z^3,x*y^3*z^2,y^7))
assert(frobeniusRoot(2,I) == ideal(x,y))    
assert(frobeniusRoot(2,I,FrobeniusRootStrategy => MonomialBasis) == ideal(x,y))
assert(frobeniusRoot(3,I) == ideal(1_R))
assert(frobeniusRoot(3,I,FrobeniusRootStrategy => MonomialBasis) == ideal(1_R))    
///

--TEST ///  -- test 2
--    kk = GF(5^4);
--    fg = (gens kk)#0;
--    assert( (getFieldGenRoot(6,5,5^4, kk))^(5^6) == fg)
--///

TEST /// -- test 3 (ascend ideal test)
    pp = 5;
    R = ZZ/pp[x,y,z];
    ff = x^3 + y^3 + z^3;
    cc = x;
    testIdeal1 = ascendIdeal(1, ff^(pp-1), ideal(cc)); --this should be the test ideal
    testIdeal2 = ascendIdeal(1, pp-1, ff, ideal(cc));
    testIdeal3 = ascendIdeal(1, {2, 2}, {ff, ff}, ideal(cc));
    mm = ideal(x,y,z);
    assert( (testIdeal1 == mm) and (testIdeal2 == mm) and (testIdeal3 == mm) )
///

TEST ///  --test 4 (ascend ideal test 2)
    pp = 13;
    R = ZZ/pp[x,y,z];
    ff = x^4 + y^4 + z^4;
    cc = x^3;
    testIdeal1 = ascendIdeal(1, ff^(pp-1), ideal(cc)); --this should be the test ideal
    testIdeal2 = ascendIdeal(1, pp-1, ff, ideal(cc)); --this should be the test ideal
    testIdeal3 = ascendIdeal(1, {5, 7}, {ff, ff}, ideal(cc));
    m2 = (ideal(x,y,z))^2;
    assert( (testIdeal1 == m2) and (testIdeal2 == m2) and (testIdeal3 == m2) )
///

TEST /// --test 5 (frobeniusRoots lists test 1)
    pp = 5;
    R = ZZ/pp[x,y,z];
    ff = x^5 + x*y^6 + y^3*z^4 + z^7;
    II = ideal(x^(2*pp)*x*y + y^(3*pp)*x^2*z, (x*y)^pp*x^3*y*z + (x*z)^pp*x^4*z);
    out1 = frobeniusRoot(1, ideal(ff^12)*II);
    out2 = frobeniusRoot(1, {12}, {ff}, II);
    out3 = frobeniusRoot(1, {12, 1}, {ff, II});
    assert( (out1 == out2) and (out1 == out3) )
///

TEST /// --test6 (compare frobeniusRoot vs frobeniusRootRingElements)
    pp = 5;
    R = ZZ/pp[x,y,z];
    ff = random(3, R) + random(5, R) + random(6, R);
    ak = 55+random(10);
    out1 = time frobeniusRoot(2, {ak}, {ff});
    out2 = time frobeniusRoot(2, ak, ff, FrobeniusRootStrategy=>MonomialBasis); 
    assert( out1 == out2 )
///



