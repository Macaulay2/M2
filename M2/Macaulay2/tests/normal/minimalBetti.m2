-- c.f. https://github.com/Macaulay2/M2/pull/2301#issuecomment-966735148
R = ZZ/101[a..d]
I = monomialCurveIdeal(R, {1,3,4})
r = regularity I -- behind the scenes, computes a free resolution of I
a = minimalBetti I

-- c.f. https://github.com/Macaulay2/M2/issues/2039
J = monomialIdeal leadTerm I
b = minimalBetti J

assert(a == b)
