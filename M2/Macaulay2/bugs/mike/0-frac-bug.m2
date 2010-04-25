k = GF( ZZ/17[a]/ideal(a^4-a^3+a^2-a+1) )
R = k[t]
1/t -- crashes, because simplify fails to check return code from rawGCDRingElement, which doesn't like k (yet)

-- Mike will use the new routine bool factoryGoodRing(const PolynomialRing *) at ring creation time to get around this