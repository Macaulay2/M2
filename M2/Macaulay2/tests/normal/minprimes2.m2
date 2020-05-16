-- from Issue https://github.com/Macaulay2/M2/issues/164
-- fixed 6 Nov 2014 MES.

-- original ring from post:
-- HCR = QQ(monoid[w_1, w_2, w_3, x_1, x_2, x_3, y_1, y_2, y_3, z_1, z_2, z_3, Degrees => {3:0, 3:1, 3:0, 3:1}, MonomialOrder => VerticalList{MonomialSize => 32, GRevLex => {6:1}, Position => Up, GRevLex => {6:1}}, DegreeRank => 1])
-- same ring:
HCR = QQ[w_1, w_2, w_3, x_1, x_2, x_3, y_1, y_2, y_3, z_1, z_2, z_3, Degrees => {3:0, 3:1, 3:0, 3:1}, MonomialOrder => {GRevLex => {6:1}, Position => Up, GRevLex => {6:1}}]
SI = ideal(-w_1*x_1+w_2*x_2, -w_1*x_1+w_3*x_3, -y_1*z_1+y_2*z_2, -y_1*z_1+y_3*z_3, w_1*x_1-y_1*z_1, w_3*x_3-y_3*z_3, w_2*x_2-y_2*z_2, w_1*w_2*w_3-y_1*y_2*y_3, x_1*x_2*x_3-z_1*z_2*z_3)
        
C = minimalPrimes(SI);
assert(#C == 37)
assert(all(C, i -> codim i === 6))

needsPackage "MinimalPrimes"
C = minprimes SI;
assert(#C == 37)
assert(all(C, i -> codim i === 6))

needsPackage "Binomials"
C = binomialMinimalPrimes(SI);
assert(#C == 37)
assert(all(C, i -> codim i === 6))
