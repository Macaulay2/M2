R = ZZ[x,y,z]
f = y^2 - y - x^3 + x^2
f = homogenize(f,z)
I = ideal diff f + ideal f
T = R/I
saturate I
X = Proj T
singularLocus X
T / saturate minors(codim T, jacobian presentation T)
T / (saturate minors(codim T, jacobian presentation T))
(saturate minors(codim T, jacobian presentation T))
codim T
describe T
T = R/f
codim T
saturate I
X = Proj T
singularLocus X
T = R/f
codim T
X = Proj T
singularLocus X
T / saturate minors(codim T, jacobian presentation T)
saturate minors(codim T, jacobian presentation T)
codim T
dim T
