I = Grassmannian(1,6, CoefficientRing => ZZ/101);
S = ring I
--gbTrace=10
elapsedTime C = res(I, FastNonminimal => true)

S = ZZ/101[a..d]
I = ideal(a^2-b^2, a*b)
gbTrace=10
elapsedTime C = res(I, FastNonminimal => true)
