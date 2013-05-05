debug Core
R = rawARingZZFlint()
a = 1_R
b = 2_R
rawRing a
a+b
a-b
a^3
a = 23746237846237846237846237846237846327846178623418764
a^4
-a
a*b
b*b
b^2
M = rawMutableMatrix(R, 3, 4, true)

R101 = rawARingZZpFlint(101)
select(2^36+1 .. 2^36 + 500, isPrime)
P = 68719476767
R = rawARingZZpFlint P
a = 10_R
a^(P-1)
a^(P^2-1)
a^(P^10)

select(2^64-500 .. 2^64-1, isPrime)
last oo
P = 18446744073709551557
-- P = 18,446,744,073,709,551,557
R = rawARingZZpFlint P
char R
random R
rawRandom R
R = ZZ/32003
random R

debug Core
R = ZZp(P, "Choose"=>"FLINT")
1_R + (-1_R)
M = 342342342342353242
N = 986908456830938608
M_R
N_R
M_R * N_R == (M*N)_R
M*N

R1 = ZZp(P, "Choose"=>"FFPACK") -- error, as it should be
R2 = ZZp(P, "Choose"=>"ARING") -- error, as it should be
R3 = ZZp(P) -- error, as it should be
R4 = ZZp(P, "Choose"=>"FLINT")
A = random(R4^4, R4^5)
B = random(R4^5, R4^4)
C = A*B
D = B*A
rank D
det oo
B*A
det oo

R1 = ZZp 101
R2 = ZZp 101
assert(R1 === R2)
R3 = ZZp(101, "Choose"=>"ARING")
R4 = ZZp(101, "Choose"=>"ARING")
assert(R1 =!= R3)
assert(R4 === R3)
