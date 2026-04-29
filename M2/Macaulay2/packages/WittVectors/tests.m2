--TEST 0
TEST ///
witt(2,ZZ/3)
witt(2,ZZ/3[x])
witt(2,ZZ/5[x]/x^2)
witt(2,GF 9)
witt(2, (GF 9)[x])
witt(2, (GF 9)[x]/x^2)
assert( (try witt(2,ZZ/15)) == null)
assert( (try witt(2,QQ)) == null)
assert( (try witt(2,QQ[x])) == null)
print("1")
///

--TEST 1
TEST /// -- Check that the fSplittingHeight method gives back the correct number
    debug WittVectors
    for i from 1 to 4 do (
        I=ideal table2(i);
        j=i;
        assert(fSplittingHeight(I)==j)
    )
///

--TEST 2
TEST ///
    S = (ZZ/5)[x,y]
    I = ideal(x^2-y^3)
    R = S/I
    W2S = witt(2,S)
    W2R = witt(2,R)
    assert(char(W2S.overring) == 25)
    assert(char(W2R.overring) == 25)
///


--TEST 3
TEST ///
    S = (ZZ/5)[x_1,x_2,x_3,y_1,y_2,y_3]
    W3S = witt(3,S)
    w1 = witt{x_1,x_2,x_3}
    w2 = witt{y_1,y_2,y_3}
    assert(w1+w2 == witt{x_1+y_1, -x_1^4*y_1-2*x_1^3*y_1^2-2*x_1^2*y_1^3-x_1*y_1^4+x_2+y_2, -x_1^24*y_1-2*x_1^23*y_1^2-2*x_1^22*y_1^3-x_1^21*y_1^4-2*x_1^19*y_1^6+2*x_1^18*y_1^7+x_1^16*y_1^9-x_1^13*y_1^12-x_1^12*y_1^13+x_1^9*y_1^16+2*x_1^7*y_1^ 18-2*x_1^6*y_1^19-x_1^4*y_1^21-2*x_1^3*y_1^22-2*x_1^2*y_1^23-x_1*y_1^24-x_1^16*x_2*y_1^4+2*x_1^15*x_2*y_1^5-2*x_1^14*x_2*y_1^6+x_1^13*x_2*y_1^7-2*x_1^ 11*x_2*y_1^9-2*x_1^10*x_2*y_1^10-2*x_1^9*x_2*y_1^11+x_1^7*x_2*y_1^13-2*x_1^6*x_2*y_1^14+2*x_1^5*x_2*y_1^15-x_1^4*x_2*y_1^16-x_1^16*y_1^4*y_2+2*x_1^15*y _1^5*y_2-2*x_1^14*y_1^6*y_2+x_1^13*y_1^7*y_2-2*x_1^11*y_1^9*y_2-2*x_1^10*y_1^10*y_2-2*x_1^9*y_1^11*y_2+x_1^7*y_1^13*y_2-2*x_1^6*y_1^14*y_2+2*x_1^5*y_1^ 15*y_2-x_1^4*y_1^16*y_2+2*x_1^12*x_2^2*y_1^3+2*x_1^11*x_2^2*y_1^4+x_1^10*x_2^2*y_1^5+x_1^8*x_2^2*y_1^7+x_1^7*x_2^2*y_1^8+x_1^5*x_2^2*y_1^10+2*x_1^4*x_2 ^2*y_1^11+2*x_1^3*x_2^2*y_1^12-x_1^12*x_2*y_1^3*y_2-x_1^11*x_2*y_1^4*y_2+2*x_1^10*x_2*y_1^5*y_2+2*x_1^8*x_2*y_1^7*y_2+2*x_1^7*x_2*y_1^8*y_2+2*x_1^5*x_2 *y_1^10*y_2-x_1^4*x_2*y_1^11*y_2-x_1^3*x_2*y_1^12*y_2+2*x_1^12*y_1^3*y_2^2+2*x_1^11*y_1^4*y_2^2+x_1^10*y_1^5*y_2^2+x_1^8*y_1^7*y_2^2+x_1^7*y_1^8*y_2^2+ x_1^5*y_1^10*y_2^2+2*x_1^4*y_1^11*y_2^2+2*x_1^3*y_1^12*y_2^2-2*x_1^8*x_2^3*y_1^2+2*x_1^7*x_2^3*y_1^3-x_1^6*x_2^3*y_1^4-x_1^4*x_2^3*y_1^6+2*x_1^3*x_2^3* y_1^7-2*x_1^2*x_2^3*y_1^8-x_1^8*x_2^2*y_1^2*y_2+x_1^7*x_2^2*y_1^3*y_2+2*x_1^6*x_2^2*y_1^4*y_2+2*x_1^4*x_2^2*y_1^6*y_2+x_1^3*x_2^2*y_1^7*y_2-x_1^2*x_2^2 *y_1^8*y_2-x_1^8*x_2*y_1^2*y_2^2+x_1^7*x_2*y_1^3*y_2^2+2*x_1^6*x_2*y_1^4*y_2^2+2*x_1^4*x_2*y_1^6*y_2^2+x_1^3*x_2*y_1^7*y_2^2-x_1^2*x_2*y_1^8*y_2^2-2*x_ 1^8*y_1^2*y_2^3+2*x_1^7*y_1^3*y_2^3-x_1^6*y_1^4*y_2^3-x_1^4*y_1^6*y_2^3+2*x_1^3*y_1^7*y_2^3-2*x_1^2*y_1^8*y_2^3+x_1^4*x_2^4*y_1+2*x_1^3*x_2^4*y_1^2+2*x _1^2*x_2^4*y_1^3+x_1*x_2^4*y_1^4-x_1^4*x_2^3*y_1*y_2-2*x_1^3*x_2^3*y_1^2*y_2-2*x_1^2*x_2^3*y_1^3*y_2-x_1*x_2^3*y_1^4*y_2+x_1^4*x_2^2*y_1*y_2^2+2*x_1^3* x_2^2*y_1^2*y_2^2+2*x_1^2*x_2^2*y_1^3*y_2^2+x_1*x_2^2*y_1^4*y_2^2-x_1^4*x_2*y_1*y_2^3-2*x_1^3*x_2*y_1^2*y_2^3-2*x_1^2*x_2*y_1^3*y_2^3-x_1*x_2*y_1^4*y_2 ^3+x_1^4*y_1*y_2^4+2*x_1^3*y_1^2*y_2^4+2*x_1^2*y_1^3*y_2^4+x_1*y_1^4*y_2^4-x_2^4*y_2-2*x_2^3*y_2^2-2*x_2^2*y_2^3-x_2*y_2^4+x_3+y_3})
    assert(w1*w2 == witt{x_1*y_1, x_2*y_1^5+x_1^5*y_2, -x_1^5*x_2^4*y_1^20*y_2-2*x_1^10*x_2^3*y_1^15*y_2^2-2*x_1^15*x_2^2*y_1^10*y_2^3-x_1^20*x_2*y_1^5*y_2^4+x_3*y_1^25+x_1^25*y_3+x_2^5*y_2^5})
	///

--TEST 4
TEST ///
    S = (ZZ/3)[x,y,z,w]
    I = ideal(x^2+y^2+z^2+w^2, x*y+x*z+x*w+y*z+y*w+z*w)
    n = 3
    d = 3
    J = wittOverringIdeal(n, I)
    f = for i from 0 to n-1 list random(d, I)
    assert isSubset(ideal wittTupleToOverring witt f, J)
///

--TEST 5
TEST ///
    S = (ZZ/5)[x_1,x_2,x_3,y_1,y_2,y_3]
    W3S = witt(3,S)
    w1 = witt{x_1,x_2,x_3}
    assert(wittFrobenius(w1) == witt{x_1^5,x_2^5,x_3^5})
///

--TEST 6
TEST ///
    S = (ZZ/2)[x,y]
    f = x^2 + y^3
    I = ideal(f)
    J = findFrobeniusLiftConstraints(I)
    c=(entries vars ring J)#0#1
    use ring J
    assert(J == ideal(c*x^2*y+x^4))
///

--TEST 7
TEST ///
    S = (ZZ/2)[x,y]
    f = x^2 + y^3
    I = ideal(f)
    L = findFrobeniusLift(2,I)
    assert(L#1 == y^2)
///


--TEST 8
TEST ///
    S = (ZZ/3)[x,y]
    assert(wittOverringToTuple((wittOverring(2,S))_0^3) != 0)
    w = witt{x,x^2}
    R = S/x
    p = map(R,S)
    Wp = witt(2,p)
    Wp(w) == witt{0_R,0_R}
    w21 = witt(1,2,id_S)
    assert((try w21^2) == null)
    assert((try w21*w21) == null)
    use S
    f = map(S,S,{x^2,y})
    assert(wittOverringToTuple((wittOverring(2,S))_0^3) != 0)
    Wf = witt(1,2,f)
    explicit Wf
///


--TEST 9
TEST ///
R = GF(5)[x,y,z]
    f1 = random(3, R)
    f2 = random(2, R)
    f3 = random(4, R)
    tt = {f1, f2, f3}
    assert( witt tt == wittOverringToTuple wittTupleToOverring witt tt)
///

--TEST 10
TEST ///
    R = GF(7)[x,y,z]
    WR = witt(3, R)
    w = witt{x,y,z}
    t = truncate(2, WR)
    assert( t(w) == witt{x,y})
///

--TEST 11
TEST ///
R = ZZ/2
Rx = R[x]
B = Rx/(x^2)
R4 = GF 4
x=symbol x
Rx4 = R4[x]
B4 = Rx4/x^2


assert(R === unWitt witt(2,R))
assert(Rx === unWitt witt(2,Rx))
assert(B === unWitt witt(2,B))
assert(R4 === unWitt witt(2,R4))
assert(Rx4 === unWitt witt(2,Rx4))
assert(B4 === unWitt witt(2,B4))

assert(witt{1_R,0_R}+witt{0_R,1_R} == witt{1_R,1})
assert(witt{1_Rx,0_Rx}+witt{0_Rx,x_Rx} == witt{1_Rx,x_Rx})
assert(witt{1_B,0_B}+witt{0_B,x_B}  == witt{1_B,x_B})
assert(witt{a_R4+1,0_R4}+witt{0_R4,a_R4} == witt{a_R4+1,a_R4})
assert(witt{1_Rx4,0_Rx4}+witt{0_Rx4,x_Rx4} == witt{1_Rx4,x_Rx4})
assert(witt{1_B4,0_B4}+witt{0_B4,x_B4} == witt{1_B4,x_B4})

assert(numgens explicit witt(2,R) == 0)
assert(numgens explicit witt(2,Rx) == 2)
assert(numgens explicit witt(2,B) == 2)
assert(numgens explicit witt(2,R4) == 2)
assert(numgens explicit witt(2,Rx4) == 5)
assert(numgens explicit witt(2,B4) == 5)
///

--TEST 12
TEST ///
S = ZZ/2[x,y]
I = ideal(x*y)
J = ideal(0_S)
assert(dim createEquations(2,I,Homogeneous=>true) > 0)
assert(dim createEquations(2,I,Homogeneous=>true, PerturbationTerm=>{1}) < 0)
assert(dim createEquations(2,0_S,Homogeneous=>true) > 0)
///

--TEST 13
TEST ///
S = (GF 9) 
S'= makeCoefficientFieldPrime S
w = wittTupleToOverring(witt{0,a_S}) +wittTupleToOverring( witt{0,1_S})
wittTupleToOverring witt{0,a_S^2}
w = witt{a_S-1,0}
assert(length terms wittTupleToOverring witt{a_S-1,0}  == 4)
///

--TEST 14
TEST ///
debug WittVectors
S = (GF 9)[x,y]
S' = makeCoefficientFieldPrime S
W = witt(3,S)
W' = witt(3,S')
w1 = random(3, W) 
w2 = random(3, W)
w1'= witt(apply(w1.tuple,i->sub(i,S')))
w2'= witt(apply(w2.tuple,i->sub(i,S')))
w = w1+w2
w' = w1'+w2'
assert(witt(apply(w.tuple,i->sub(i,S'))) - (w1'+w2') == 0)
///

--TEST 15
TEST ///
S = GF 4[x]
E = explicit(witt(2,S))
assert((target E.cache.overringMap).cache.unWitt === S)
w = witt{1_S,x_S}
Ow = wittTupleToOverring w
assert((ring Ow).cache.unWitt === S)
Rw = wittTupleToRing w
assert(target (ring Rw).cache.overringMap === ring Ow)
///

--TEST 16
TEST ///
S = ZZ/2
isWellDefined (explicit witt(2,S)).cache.overringMap 
S = ZZ/2[x]
isWellDefined (explicit witt(2,S)).cache.overringMap 
S = ZZ/2[x]/x^2
isWellDefined (explicit witt(2,S)).cache.overringMap 
///
--TEST 17
TEST ///
S = ZZ/2
assert(wittRingToTuple wittTupleToRing witt{1_S,0_S} == 1)
assert(wittRingToTuple wittTupleToRing witt{1_S,0_S,0_S} == 1)
S = GF 4
assert(wittRingToTuple wittTupleToRing witt{1_S,0_S} == 1)
S = (ZZ/2)[x]
assert(wittRingToTuple wittTupleToRing witt{1_S,x_S} == witt{1_S,x_S})
S = GF 4[x]
Phi = (explicit(witt(2,S))).cache.overringMap
assert( Phi wittTupleToRing witt{1_S,x_S} == wittTupleToOverring witt{1_S,x_S})
assert(wittRingToTuple wittTupleToRing witt{1_S,x_S} == witt{1_S,x_S})
assert(wittOverringToTuple wittTupleToOverring witt{1_S,x_S} == witt{1_S,x_S})
assert(wittRingToTuple wittTupleToRing witt{1_S,x_S,x^2_S} == witt{1_S,x_S,x^2_S})
///

--TEST 18
TEST ///
R = GF(3)[x,y]
WR = witt(2, R)
for xx in -2..2 do(
    for yy in -2..2 do(
	assert( sub(xx, WR) + sub(yy, WR) == sub(xx + yy, WR) );
	assert( sub(xx, WR)*sub(yy, WR) == sub(xx*yy, WR) );
	)
    )
///

--TEST 19
TEST ///
R = GF(5)[x,y]
WR = witt(3, R)
w = random(2, WR)
assert(1_WR*w == w)
assert(0_WR*w == 0_WR)
///

--TEST 20
TEST ///
R = ZZ/2[x]
WR = witt(2, R)
w = random(2, WR)
assert(wittIdeal(w, sub(0, WR)) == wittIdeal(w))
///

--TEST 21
TEST ///
R = GF(2)[x]
WR = witt(2, R)
w = random(2, WR)
assert(ring wittTupleToOverring w === wittOverring(2,R))
EWR = explicit WR
Phi = EWR.cache.overringMap
assert(target Phi === wittOverring(2,R))
wittOverringToTuple(Phi wittTupleToRing witt{0_R,x_R} - wittTupleToOverring witt{0_R,x_R}) == 0
assert(ring first w === ring first wittRingToTuple wittTupleToRing w)
assert(wittIdeal(w, sub(0, WR)) == wittIdeal(w))
///

--TEST 22
TEST ///
R = ZZ/3[y,x]/(y^2)
WR = witt(4, R)
w1 = random(2, WR)
w2 = random(3, WR)
w3 = random(4, WR)
assert(2*w2 == w2 + w2)
assert(w2 - w2 == 0_WR)
assert(0_WR*w2 == 0_WR)
assert(1_WR*w3 == w3)
u= (w1 + w2) + w3 
v= w1 + (w2 + w3) 
assert((w1+w2)+w3 == w1+(w2+w3))
assert((w1*w2)*w3 - w1*(w2*w3)  == 0)
///

--TEST 23
TEST ///
R = GF(4)[z]
w = witt{(a+1)*z^4,0,0,0}
Rp = makeCoefficientFieldPrime R
wp = witt{(a_Rp+1)*z_Rp^4,0,0,0}
S = ambient Rp
wS = witt{(a_S+1)*z_S^4,0,0,0}
assert(w - w  == 0)
assert(wp - wp == 0)
assert(wS-wS == 0)
///

--TEST 24
TEST ///
R = GF(4)[x,y,z]
WR = witt(4, R)
w1 = random(1, WR)
w2 = random(1, WR)
w3 = random(3, WR)
assert(2*w2 == w2 + w2)
assert(w2 - w2 == 0_WR)
assert(1_WR*w3 == w3)
assert( (w1 + w2) + w3 == w1 + (w2 + w3) )
( (w1*w2)*w3 - w1*(w2*w3) )
///


--TEST 25
TEST ///
R = ZZ/2[x,y,z]/(x^3+y^3+z^3)
WR = witt(3, R)
w1 = random(2, WR)
w2 = random(3, WR)
w3 = random(4, WR)
assert(2*w2 == w2 + w2)
assert(w2 - w2 == 0_WR)
assert(0_WR*w2 == 0_WR)
assert(1_WR*w3 == w3)
assert( (w1 + w2) + w3 == w1 + (w2 + w3) )
///


--TEST 26
TEST ///
R=ZZ/3[x_0,x_1,x_2,x_3]
I=ideal(x_0^8*x_1+x_1^6*x_2+x_2^3+x_3^2*x_0)
assert(fSplittingHeight I == 8)
///


--TEST 27
TEST ///
R=ZZ/3[x_0,x_1,x_2,x_3]
I=ideal((x_0^4+x_1^4+x_2^4+x_3^4)^2)
assert(fSplittingHeight I == infinity)
///

