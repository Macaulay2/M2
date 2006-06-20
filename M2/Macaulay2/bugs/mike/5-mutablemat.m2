-- lifted from test/engine/raw-mutable.m2
error "the rest of this test file is not working yet, Mike will fix it"

det m
assert(rawToInteger (2 * rawMatrixEntry(p,0,0)) == det m)

rawMatrixColumnSwap(p,3,9); p
rawMatrixRowSwap(p,4,9); p
reduceRowsColsGcd(p,9,9); p
reduceRowsColsGcd(p,8,8); p

reduceGCD(p,9,9,8)
p
reduceGCD(p,9,9,7)
p
reduceRowsCols(p,8,8); p
p

gcdCoefficients(2001,1007)
gcd(2001,1007)


rawMatrixColumnChange(p,8,raw (-42_R),9,false)
p
m = map(R,p)
toString m
p = rawMutableMatrix(p, true)
p
rawMatrix
rawMinors(5,p,0)
rawMatrixEntry(oo,0,0)


R
reduceRowsCols(p,9,9)
///
     for r from 0 to nrows-1 do
         if r =!= i then (
	     a := 
     )
///
