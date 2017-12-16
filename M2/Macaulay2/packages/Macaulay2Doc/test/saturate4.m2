-- Test of saturation and colon ideals

R = ZZ/101[a..d]/(a^3-a^2*b)
gbTrace=3

saturate(ideal(0_R),0_R,Strategy => Eliminate) == ideal(1_R) -- ok now
saturate(ideal(0_R),0_R,Strategy => Linear) == ideal(1_R) -- ok
saturate(ideal(0_R),0_R,Strategy => Bayer) == ideal(1_R) -- ok 
saturate(ideal(0_R),0_R,Strategy => Iterate) == ideal(1_R) -- ok

-*
saturate(ideal(0_R),a_R,Strategy => Eliminate) == ideal(a-b) -- ok error message
saturate(ideal(0_R),a_R,Strategy => Linear) == ideal(a-b) -- ok error message
saturate(ideal(0_R),a_R,Strategy => Bayer) == ideal(a-b) -- ok error message
*-

saturate(ideal(0_R),a_R,Strategy => Iterate) == ideal(a-b) -- ok


A = ZZ/101[a..d]/(a^3-a^2*b)
R = A[x,y,z]/(x*a-y^3*a)

saturate(ideal(0_R),0_R,Strategy => Eliminate) == ideal(1_R) -- ok
saturate(ideal(0_R),0_R,Strategy => Linear) == ideal(1_R) -- ok
saturate(ideal(0_R),0_R,Strategy => Bayer) == ideal(1_R) -- ok
saturate(ideal(0_R),0_R,Strategy => Iterate) == ideal(1_R) -- ok

-*
saturate(ideal(0_R),a_R,Strategy => Eliminate) == ideal(x-y^3,a-b) -- ok error message
saturate(ideal(0_R),a_R,Strategy => Linear) == ideal(x-y^3,a-b) -- ok error message
saturate(ideal(0_R),a_R,Strategy => Bayer) == ideal(x-y^3,a-b) -- ok error message
*-

saturate(ideal(0_R),a_R,Strategy => Iterate) == ideal(x-y^3,a_R-b_R) -- ok

A = ZZ/101[a..d]/(a^3-a^2*b)
R = A[x,y,z]/(x^3*a-y^3*a)

saturate(ideal(0_R),0_R,Strategy => Eliminate) == ideal(1_R) -- ok
saturate(ideal(0_R),0_R,Strategy => Linear) == ideal(1_R) -- ok
saturate(ideal(0_R),0_R,Strategy => Bayer) == ideal(1_R) -- ok
saturate(ideal(0_R),0_R,Strategy => Iterate) == ideal(1_R) -- ok

-*
saturate(ideal(0_R),a_R,Strategy => Eliminate) == ideal(x^3-y^3,a-b) -- ok error message
saturate(ideal(0_R),a_R,Strategy => Linear) == ideal(x^3-y^3,a-b) -- ok error message
saturate(ideal(0_R),a_R,Strategy => Bayer) == ideal(x^3-y^3,a-b) -- ok error message
*-
saturate(ideal(0_R),a_R,Strategy => Iterate) == ideal(x^3-y^3,a_R-b_R) -- ok

debug Core
R = QQ[a..d, MonomialOrder=>{2,2}]; assert not isGRevLexRing R
R = QQ[a..d, MonomialOrder=>Eliminate 1]; assert not isGRevLexRing R
R = QQ[a..d, MonomialOrder=>{Weights=>{1,2,3,4},4}]; assert not isGRevLexRing R
R = QQ[a..d, MonomialOrder=>{Lex=>2,2}]; assert not isGRevLexRing R
R = QQ[a..d, MonomialOrder=>{Position=>Up}]; assert isGRevLexRing R
R = QQ[a..d, MonomialOrder=>{GRevLex => {1,2,3,4}}]; assert not isGRevLexRing R
