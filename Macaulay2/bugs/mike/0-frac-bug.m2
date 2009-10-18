k = GF( ZZ/17[a]/ideal(a^4-a^3+a^2-a+1) )
R = k[t]
1/t -- crashes, because simplify fails to check return code from rawGCDRingElement
