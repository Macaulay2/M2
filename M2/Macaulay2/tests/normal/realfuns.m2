eps = 2.^-50
chk = (x,y) -> abs(x-y) < eps
assert chk ( integrate(sin,0,pi/2), 1 )
assert chk ( integrate(zeta,2,3), 1.3675256886839791457066699268939213778115038258418219887541718201423885236150082812747931 )
assert chk ( integrate(x -> exp(-x), 0, infinity), 1 )
assert chk ( integrate(x -> exp(-x^2), -infinity, infinity), sqrt(pi + 0))

