-- Test of saturation and colon ideals

R = ZZ/101[a..d]/(a^3-a^2*b)
gbTrace=3

assert(saturate(ideal(0_R),0_R,Strategy => Eliminate) == ideal(1_R)) -- ok now
assert(saturate(ideal(0_R),0_R,Strategy => Linear) == ideal(1_R)) -- ok
assert(saturate(ideal(0_R),0_R,Strategy => Bayer) == ideal(1_R)) -- ok
assert(saturate(ideal(0_R),0_R,Strategy => Iterate) == ideal(1_R)) -- ok

assert try saturate(ideal(0_R),a_R,Strategy => Eliminate) == ideal(a-b) else true -- ok error message
assert try saturate(ideal(0_R),a_R,Strategy => Linear) == ideal(a-b) else true -- ok error message
assert try saturate(ideal(0_R),a_R,Strategy => Bayer) == ideal(a-b) else true -- ok error message

assert(saturate(ideal(0_R),a_R,Strategy => Iterate) == ideal(a-b)) -- ok

A = ZZ/101[a..d]/(a^3-a^2*b)
R = A[x,y,z]/(x*a-y^3*a)

assert(saturate(ideal(0_R),0_R,Strategy => Eliminate) == ideal(1_R)) -- ok
assert(saturate(ideal(0_R),0_R,Strategy => Linear) == ideal(1_R)) -- ok
assert(saturate(ideal(0_R),0_R,Strategy => Bayer) == ideal(1_R)) -- ok
assert(saturate(ideal(0_R),0_R,Strategy => Iterate) == ideal(1_R)) -- ok

assert try saturate(ideal(0_R),a_R,Strategy => Eliminate) == ideal(x-y^3,a-b) else true -- ok error message
assert try saturate(ideal(0_R),a_R,Strategy => Linear) == ideal(x-y^3,a-b) else true -- ok error message
assert try saturate(ideal(0_R),a_R,Strategy => Bayer) == ideal(x-y^3,a-b) else true -- ok error message

assert(saturate(ideal(0_R),a_R,Strategy => Iterate) == ideal(x-y^3,a_R-b_R)) -- ok

A = ZZ/101[a..d]/(a^3-a^2*b)
R = A[x,y,z]/(x^3*a-y^3*a)

assert(saturate(ideal(0_R),0_R,Strategy => Eliminate) == ideal(1_R)) -- ok
assert(saturate(ideal(0_R),0_R,Strategy => Linear) == ideal(1_R)) -- ok
assert(saturate(ideal(0_R),0_R,Strategy => Bayer) == ideal(1_R)) -- ok
assert(saturate(ideal(0_R),0_R,Strategy => Iterate) == ideal(1_R)) -- ok

assert try saturate(ideal(0_R),a_R,Strategy => Eliminate) == ideal(x^3-y^3,a-b) else true -- ok error message
assert try saturate(ideal(0_R),a_R,Strategy => Linear) == ideal(x^3-y^3,a-b) else true -- ok error message
assert try saturate(ideal(0_R),a_R,Strategy => Bayer) == ideal(x^3-y^3,a-b) else true -- ok error message

assert(saturate(ideal(0_R),a_R,Strategy => Iterate) == ideal(x^3-y^3,a_R-b_R)) -- ok

debug Saturation
R = QQ[a..d, MonomialOrder=>{2,2}]; assert not isGRevLexRing R
R = QQ[a..d, MonomialOrder=>Eliminate 1]; assert not isGRevLexRing R
R = QQ[a..d, MonomialOrder=>{Weights=>{1,2,3,4},4}]; assert not isGRevLexRing R
R = QQ[a..d, MonomialOrder=>{Lex=>2,2}]; assert not isGRevLexRing R
R = QQ[a..d, MonomialOrder=>{Position=>Up}]; assert isGRevLexRing R
R = QQ[a..d, MonomialOrder=>{GRevLex => {1,2,3,4}}]; assert not isGRevLexRing R
