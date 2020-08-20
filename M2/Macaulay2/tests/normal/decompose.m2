R = ZZ/101[a..d]
I = ideal 0_R
assert ( minimalPrimes I == {I} )
assert ( minimalPrimes ideal 1_R == {} )

S = R / ((a+b)*(a^2+b))
I = ideal 0_S
assert (
     minimalPrimes I == {ideal(a^2+b), ideal(a+b)} 
     or
     minimalPrimes I == {ideal(a+b), ideal(a^2+b)} 
     )
assert( minimalPrimes ideal 1_S == {})

A = ZZ/101[a,b,c]
I = ideal (b^2 - 4*a*c)
irreducibleCharacteristicSeries I
minimalPrimes I
assert ( minimalPrimes I == { I } )

A = QQ[a,b,c]
I = ideal (b^2 - 4*a*c)
minimalPrimes I
assert ( minimalPrimes I == { I } )
J = a*I
minimalPrimes J
assert( 
     minimalPrimes J == {ideal a, ideal(b^2-4*a*c)}
     or 
     minimalPrimes J == {ideal(b^2-4*a*c), ideal a}
     )


f = 77 * a^5 + 64637 * a^3 + a - 111
g = 77 * a^8 + 646371 * a^3 + a - 111
h = 111/60 * f * g^2
s = factor h
assert( # s == 3 )
assert( value s == h )


h = symbol h

A = ZZ/103[a..e,h]
I = ideal ( a+b+c+d+e, a*b + b*c + c*d + d*e + e*a ,
     a*b*c + b*c*d + c*d*e + d*e*a + e*a*b,
     a*b*c*d + b*c*d*e + c*d*e*a + d*e*a*b + e*a*b*c,
     a*b*c*d*e - h^5
     )
--old status: This crashes in factory version 3.0.4 but not in 3.0.3
--old status: I've reported it to the Singular people
--old status: We may have to stick with the older version
time minimalPrimes I
assert ( 25 == # minimalPrimes I )

A = ZZ/101[a..e,h]
I = ideal ( a+b+c+d+e, a*b + b*c + c*d + d*e + e*a ,
     a*b*c + b*c*d + c*d*e + d*e*a + e*a*b,
     a*b*c*d + b*c*d*e + c*d*e*a + d*e*a*b + e*a*b*c,
     a*b*c*d*e - h^5
     )
time minimalPrimes I
assert ( 75 == # minimalPrimes I )

end

-- we don't have enough time to test this one

A = QQ[a..e]
I = ideal ( a+b+c+d+e, a*b + b*c + c*d + d*e + e*a ,
     a*b*c + b*c*d + c*d*e + d*e*a + e*a*b,
     a*b*c*d + b*c*d*e + c*d*e*a + d*e*a*b + e*a*b*c,
     a*b*c*d*e - 1
     )
time minimalPrimes I
assert ( 25 == # minimalPrimes I )


end

A = QQ[a..e,h]
I = ideal ( a+b+c+d+e, a*b + b*c + c*d + d*e + e*a ,
     a*b*c + b*c*d + c*d*e + d*e*a + e*a*b,
     a*b*c*d + b*c*d*e + c*d*e*a + d*e*a*b + e*a*b*c,
     a*b*c*d*e - h^5
     )
time minimalPrimes I
assert ( 25 == # minimalPrimes I )
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test decompose.out"
-- End:
