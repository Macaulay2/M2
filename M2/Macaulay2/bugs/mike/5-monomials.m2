R = QQ[x,y,z]
M = matrix{{x^3,x*y^2,x^2-x},{x-1,x*y,y^3-1}}
mons = monomials M
coefficients(M, Monomials => mons)  

-- /capybara/share/Macaulay2/Macaulay2Core/m2/matrix2.m2:271:41:(0): expected matrices with the same number of rows

