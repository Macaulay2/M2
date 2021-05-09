R = QQ[x]
M = ideal x^3 / ideal x^7
N = ideal x^4 / ideal x^8
N' = ideal x^2 / ideal x^6
P = ideal x^4 / ideal x^6
assert not isSubquotient(N,P)
assert not isSubquotient(N',P)
assert isSubquotient(P,M)
assert not isSubquotient(image matrix {{x^4}},M)
assert isSubquotient(M, image matrix {{x^2}})
assert not isSubquotient(M, image matrix {{x^4}})
assert not isSubquotient(cokernel matrix {{x^4}},M)
assert not isSubquotient(M, cokernel matrix {{x^2}})
assert isSubquotient(M, cokernel matrix {{x^8}})

f = map(image matrix {{x^4}}, image matrix {{x^2}}, matrix {{x^3}})
assert( matrix{{x^5}} - matrix inducedMap(image matrix {{x^4}}, image matrix {{x^4}}, f, Verify => true) == 0 )
assert( matrix {{x}} - inducedMap(image matrix {{x^6}}, image matrix {{x^2}}, f, Verify => true) == 0 )
